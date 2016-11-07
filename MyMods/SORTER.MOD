IMPLEMENTATION MODULE SORTER;

(*
  REVISION HISTORY
  ----------------
  23 Dec 91 -- Converted to M2 V 4.00.
*)
  IMPORT Break;
(*  IMPORT DebugPMD;*)
  IMPORT MED;
  FROM Terminal IMPORT ReadString;
  IMPORT Terminal;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;

(* 
  This sorting algorithm was written to use as few comparisons as possible,
to have as few steps btwn each comparison as possible, to take advantage of
natural sequencing, to preserve the order of equals (or even the reverse
order of equals), ie, to be stable, to use as little memory as possible (one
working array), and to be a modular, easily understood program written in
BASIC.  Unfortunately, the horrendous variable names in this program are from
this original BASIC listing of the program.
  The theory behind the algorithm may be described in a language of forests,
trees, branches, twigs and leaves.  There is a forest filled with trees of
different sizes.  Each tree is very orderly.  The trunk of a tree splits into
two branches of nearly the same size.  If one branch is larger than the
other, it is always the right-hand branch.  Similarly, each branch divides
into two more branches until the branches become twigs from which leaves grow. 
The leaves are the individual elements to be sorted.
  This program creates a butterfly merge to combine leaves into twigs, twigs
into branches, and branches into one final linked list starting at position
ELEMCNT+1.  In the end, LINKS(ELEMCNT+1) points to the first leaf,
LINKS(LINKS(ELEMCNT+1)) points to the second, LINKS(LINKS(LINKS(ELEMCNT+1)))
points to the third, and the last link points to itself.  
  Two things happen as the pgm jumps from twig to twig.  The leaves ahead of
the current record pointer get merged into a twig and the twigs and branches 
behind this pointer get merged into larger branches.  The butterfly merge
treats each merge the same way.  The heads of each twig sequence are kept at
positions ELEMCNT+1, ELEMCNT+2, ..., ELEMCNT+log2(ELEMCNT)+2 after the links
themselves, which are kept in positions 1,2,3,...,ELEMCNT of array links.
  The merge takes the last 2 sequences created in the list and combines them
into one.  One wing of the merge follows sequence 1 and the other follows
sequence 2.  The two are interwoven until the final link points to itself. 
Because the heads of each sequence are kept in the same array with the links
themselves, the merge is extraordinarily fast.  After each merge, the stack
of twig sequence heads has been reduced by one.
  Each time the current record pointer reaches a new twig, it generates new
sequences one item long to correspond to the leaves of that twig.  A two leaf
twig is produced by creating two one item sequences, each pointing to itself. 
Then these two leaves are merged once.  A three leaf twig is created from
three one item sequences merged twice.  A four leaf twig is merged from two
two leaf twigs: the first two leaf twig is created and merged once; then the
number of remaining merge passes is set to a negative number so that the
merge will be disabled until the second two leaf twig is created and merged
with the first.
  After each complete twig has been generated, merging continues until the
branches behind the current record pointer have been linked together.  Then
it jumps to the next twig, generates new leaves and lets the butterfly merge
fly again.
  A state machine was the simplest way to implement all the branching of the
FORTRAN code.  An arbitrary numbering from 1 to 6 is used in which the
variable STATE represents this state machine's indicator.  A second minor
state machine had to be introduced as well, which uses the variable MODE as
its indicator.  The commented out numbers are the statement labels from my
FORTRAN source code listing.  

*)

(* / D A T A : L *)
PROCEDURE TREESORT(I : ITEMPTR; L : LINKSPTR; LASTELEM : CARDINAL);

  VAR
    AK1,AK2,T2,T3,T4,SQNC1,      B1,B2     : LONGREAL;
    SEQHEAD,L0,L1,L2,STATE,MODE,ELEMIDX,T1 : CARDINAL;
    MERGES                                 : INTEGER;

  BEGIN
    AK1 := 0.;
    ELEMIDX := 0;
    MERGES := 0;
    T2 := 0.;
    T4 := 0.;
    SEQHEAD := LASTELEM + 1;
    L^[1] := 1;       (* That pesky zero origin arrays again *)
    L^[SEQHEAD] := 1;
    AK2 := 1.;
    SQNC1 := FLOAT(LASTELEM);
    WHILE SQNC1 >= 4. DO    (* Climb the tree *)
      AK2 := 2.*AK2;
      B2 := SQNC1/2.;
      SQNC1 := Int(B2);
      T4 := T4 + AK2*(B2-SQNC1);
    END(*WHILE*);
    T4 := AK2 - T4; (* T4 is the # of low order twigs *)
    B2 := AK2/2.;
    STATE := 1;
(* 13 *)
    WHILE (STATE > 1) OR (AK1 < AK2) DO (* Next twig.  Can only exit when *)
      CASE STATE OF                     (* STATE = 1 *)
      1:AK1 := AK1 + 1.;
        T1 := Round(AK1);
        B1 := B2;
        T3 := T2;
        WHILE NOT ODD(T1) DO
          INC(MERGES);
          T2 := T2 - B1;
          B1 := B1/2.;
          T1 := T1 DIV 2;
        END(*LOOP*);
(* Twig calculations *)
        T2 := T2 + B1;
        IF (SQNC1 = 2.) AND (T3 < T4) THEN 
          INC(MERGES);  (* 2 twig *)
        ELSIF (SQNC1 = 2.) OR (T3 < T4) THEN
(* 5 *)   INC(MERGES);     (* 3 twig *)
          INC(ELEMIDX);
(* Make a leaf *)
          L^[ELEMIDX] := ELEMIDX;
          L^[SEQHEAD] := ELEMIDX;
(* Next sequence head *)
          INC(SEQHEAD);
          INC(MERGES);  (* 2 twig *)
        ELSE  (* 4 twig.  Disengage # of merges *)
          MERGES := -1*MERGES;
        END(*IF*);
        STATE := 2;
      | 2:
(* 7 *) INC(ELEMIDX);
(* Make a leaf *)
        L1 := ELEMIDX;
        L^[ELEMIDX] := ELEMIDX;
        L^[SEQHEAD] := ELEMIDX;
(* L0 is head of older leaf *)
        L0 := SEQHEAD;
(* Head of most recent leaf *)
        INC(SEQHEAD);
        INC(ELEMIDX);
(* Make a leaf *)
        L2 := ELEMIDX;
        L^[ELEMIDX] := ELEMIDX;
        L^[SEQHEAD] := ELEMIDX;
        STATE := 4;
      | 4:
(* 9 *) IF I^[L1] <= I^[L2] THEN
          STATE := 5 (* switch to sequence 1 *)
        ELSE (* switch to sequence 2 *)
          L^[L0] := L2;
(* 8 *)   LOOP
            L0 := L2;
            L2 := L^[L0]; (* next leaf *)
            IF L2 = L0 THEN (* switch to sequence 1 *)
              L^[L0] := L1;
              STATE := 6;
              EXIT;
            END(*IF*);
            IF (I^[L1] <= I^[L2]) THEN EXIT END(*IF*);
          END(*LOOP*);
          IF STATE (* still *) = 4 THEN
            L^[L0] := L1;
            STATE := 5;
          END(*IF*);
        END(*IF*);
      | 5:
(* 11 *)L0 := L1;
        L1 := L^[L0];
        IF L1 <> L0 THEN
          STATE := 4;
        ELSE
          L^[L0] := L2;
          STATE := 6;
        END(*IF*);
      | 6:
(* 10 *)DEC(MERGES);
        IF MERGES > 0 THEN
(* 12 *)  DEC(SEQHEAD);  (* Head of latest branch or twig *)
          L0 := SEQHEAD - 1;    (* Head of older branch or twig *)
          L1 := L^[L0];      (* Head of sequence 1 *)
          L2 := L^[SEQHEAD]; (* Head of sequence 2 *)
          STATE := 4;
        ELSIF MERGES = 0 THEN
          STATE := 1
        ELSE (* MERGES < 0 *)
(* Make 2nd half of 4-twig by re-engaging the # of merges *)
          MERGES := -1*MERGES + 1;
          STATE := 2;
        END(*IF*);
      END(*STATE MACHINE CASE*);
    END(*WHILE*);
  END TREESORT;
(* / R E V E R T *)

  PROCEDURE QUICKSORT(VAR AI : ARRAY OF ITEM; LASTITEM : INDEX);
(*  VAR LASTITEM : INDEX;*)

    PROCEDURE SORT(L,R : INDEX);
    VAR
      I,J  : INDEX;
      X,W  : ITEM;

    BEGIN
      IF R-L < 10 THEN
        BinInsSort(AI,L,R);
      ELSE
        I := L;
        J := R;
        X := AI[(L+R) DIV 2];

        REPEAT
          WHILE AI[I] < X DO INC(I); END(*WHILE*);
          WHILE X < AI[J] DO DEC(J); END(*WHILE*);
          IF I <= J THEN
            W := AI[I];
            AI[I] := AI[J];
            AI[J] := W;
            INC(I);
            DEC(J);
          END(*IF*);
        UNTIL I > J;
        IF L < J THEN SORT(L,J) END(*IF*);
        IF I < R THEN SORT(I,R) END(*IF*);
      END(*IF*);
    END SORT;

  BEGIN  (* QUICKSORT *)
(*    LASTITEM := HIGH(AI);*)
(*
  Algorithm as implemented assumes that original array index starts at 1,
  hence the need to subtract 1 from LASTITEM.
*)
    SORT(0,LASTITEM-1);
  END QUICKSORT;

    PROCEDURE BinInsSort(VAR AI : ARRAY OF ITEM; Left,Right : INDEX);
    VAR
      I,J,M,L,R : INDEX;
      X         : ITEM;
    BEGIN
      FOR I := Left+1 TO Right DO
        X := AI[I];
        L := Left;
        R := I;
        WHILE L < R DO
          M := (L+R) DIV 2;
          IF AI[M] <= X THEN
            L := M + 1
          ELSE
            R := M
          END(*IF*);
        END(*WHILE*);
        FOR J := I TO R+1 BY -1 DO
          AI[J] := AI[J-1];
        END(*FOR*);
        AI[R] := X
      END(*FOR*);
    END BinInsSort;

  PROCEDURE BININSSORT(VAR AI : ARRAY OF ITEM; LASTITEM : INDEX);
(*
  VAR
    LASTITEM  : INDEX;
*)

  BEGIN
(*    LASTITEM := HIGH(AI);*)
(*
  Algorithm as implemented assumes that original array index starts at 1,
  hence the need to subtract 1 from LASTITEM.
*)
    BinInsSort(AI,0,LASTITEM-1);
  END BININSSORT;


END SORTER.
