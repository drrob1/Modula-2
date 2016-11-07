(*
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.CARDINAL_Text_IO; use Ada.CARDINAL_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
--  function Command_Name return string; -> obtain the name by which the program was invoked
-- function argument_count return CARDINAL; -> number of command-line arguments
--  function argument(n: CARDINAL) return string; -> obtain the n-th command-line argument
--  procedure Set_Exit_Status(M: CARDINAL); -> set return code to M
with Ada.Strings, Ada.Strings.Bounded, Ada.Strings.Unbounded;
--use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;
*)

MODULE SortStuff;

CONST
  maxLineLength = 80;
  maxEntries    = 10000;

TYPE
  STRING = ARRAY [1..maxLineLength] OF CHAR;
  WordEntryType = STRING;
  WordEntryPointerType = POINTER TO STRING;
  EntryListType = ARRAY [1..MaxEntries] OF WordEntryType;
  EntryPointerListType = ARRAY [1..MaxEntries] OF WordEntryPointerType;

VAR 
  InputFileName : STRING := "WordList.txt";
  OutputFileName: STRING := "WordListSrted.txt";

  c,ch : CHAR;

  NewEntry : WordEntryType;
  OutOfPlaceEntry : WordEntryPointerType;
  Buffer          : STRING;
  Length          : CARDINAL; 
  NumberOfEntries : CARDINAL;
  No_Exchanges    : BOOLEAN;
  InFile, OutFile : File_Type;

  EntryList        : EntryListType;
  EntryPointerList : EntryPointerListType;

  PROCEDURE BinInsSort(AI : VAR INOUT EntryListType; Left,Right : CARDINAL);
  VAR
      M,L,R : CARDINAL;
      X         : WordEntryType;
(*                                          i1,j1,m1,L1,R1 : CARDINAL; *)
      i1,j1 : CARDINAL;
    BEGIN
      Put_Line(" Binary insertion sort.");
(*                                          M := Left; *)
      FOR I := Left+1 .. Right-1 DO
        i1 := i; (* debugging sttmnt *)
        X := AI(I);
        L := Left;
        R := I;
        WHILE L < R DO
            M := (L+R) DIV 2;
         Put("WHILE LOOP: i,j,M,L,R= ");
         Put(I,6);
         Put(", M= ");
         Put(M,6);
         Put(L,6);
         Put(R,6);
         New_Line;
         Put("x =");
         Put(To_String(X));
         Put("    pause ... ");
--         Get_Immediate(c);

          IF AI(M) <= X THEN
            L := M + 1;
          ELSE
            R := M;
          END; (* IF AI(M) *)
        END; (* WHILE L<R *)
        FOR J := I .. R+1 BY -1 DO 
          j1 := j;                                  (* debugging sttmnt *)
          AI(J) := AI(J-1);
        END; (* FOR J in reverse *)
        AI(R) := X;
      END; (* FOR I in Left+1 *)
    END BinInsSort;

  PROCEDURE StraightInsSort(AI : VAR INOUT EntryListType; Left,Right : CARDINAL);
  VAR
      X  : WordEntryType;
      j  : CARDINAL;
      i1 : CARDINAL;
    BEGIN
    	Put_Line(" Straight insertion sort.");
      FOR I in Left .. Right-1 DO
        i1 := i; (*                                   debugging sttmnt *)
        X := AI(I);
        j := I;
        WHILE (j > 0) AND (x < AI(j-1)) DO
          AI(J) := AI(J-1);
          j := j -1;
          Put("WHILE LOOP: i,j,M,L,R= ");
          Put(I,6);
          Put(", M= ");
          New_Line;
          Put("x =");
          Put(To_String(X));
          Put("    pause ... ");
          Get_Immediate(c);
        END; (* WHILE j > 0 and x < AI(j-1) *)
        AI(j) := X;
      END; (*  FOR I *)
--   exception
--      when Constraint_Error =>
         --         print out i,j,l,m,r and probably x THEN pause beFORe exiting.
--         Put("Constraint_Error: i,j= ");
--         Put(I1,6);
--         Put(J,6);
--         New_Line;
--         Put("x =");
--        Put(To_String(X));
--         New_Line;
--         Put(" pause ... ");
--         Get_Immediate(c);
    END StraightInsSort;

   PROCEDURE qsort (AI : VAR INOUT EntryListType; Left,Right : CARDINAL);
   i,j : CARDINAL;
   w,x : WordEntryType;
   BEGIN
     Put_Line(" Quicksort.");
     i := Left;
     j := Right;
     x := AI( (Left+Right)/2 );
     REPEAT
       WHILE AI(i) < x DO
         i := i + 1;
       END; (* WHILE AI(i) *)
       WHILE x < AI(j) DO
         j := j - 1;
       END; (* WHILE x *)
       IF i <= j THEN
         w := AI(i);
         AI(i) := AI(j);
         AI(j) := w;
         i := i + 1;
         j := j - 1;
       END; (* IF i<=j *)
       IF i > j THEN EXIT END;
     UNTIL i > j; 
     IF Left < j THEN
         qsort(AI,Left,j);
     END IF;
     IF i < Right THEN
         qsort(AI,i,Right);
     END IF;
   END qsort;

PROCEDURE heapsort(RA : VAR INOUT EntryListType; Left,Right: CARDINAL);
VAR 
  L,IR,I,J : CARDINAL;
  RRA : WordEntryType;

BEGIN
  Put_Line(" Heapsort.");
  L := Right/2 + 1;
  IR := Right;
(*
--  The index L will be decremented from its initial value down to 'Left' during the heap
--  creation phase.  Once it reaches Left, the index IR will be decremented from its initial 
--  value down to 'Left' during the heap selection phase.
*)
  LOOP
   IF L > Left THEN (*  -- IF(L.GT.1)THEN in original Fortran code *)
     L := L -1;
     RRA := RA(L);
   ELSE
     RRA := RA(IR);
     RA(IR) := RA(Left); (*  -- RA(IR)=RA(1) in original Fortran code *)
     IR := IR -1;
     IF IR = Left THEN  (*   -- IF(IR.EQ.1)THEN in original Fortran code *)
       RA(Left) := RRA; (*   -- RA(1)=RRA in original Fortran code *)
       RETURN;
     END (* IF -- IR = Left, or IR.EQ.1 in the original Fortran code *)
   END; (* IF -- L > Left, or L.GT.1 in original Fortran code *)
   I=L;
   J=L + L;
   WHILE J <= IR DO  (* -- DO WHILE J.LE.IR is intent. Code was 20 IF(J.LE.IR)THEN *)
     IF J < IR THEN  (*   -- IF(J.LT.IR) THEN *)
       IF RA(J) < RA(J+1) THEN INC(J); END; (*  --  IF(RA(J).LT.RA(J+1)) J=J+1 *)
     END;
     IF RRA < RA(J) THEN (*  -- IF (RRA.LT.RA(J)) THEN *)
       RA(I) := RA(J);
       I := J;
       J := J + J;
     ELSE
       J := IR + 1;
     END; (* IF -- if(RRA.LT.RA(j)) *)
   END; (* WHILE -- do WHILE J.LE.IR, or GO TO 20 in the original Fortran code
             -- END IF FOR original 20 IF(J.LE.IR)THEN.  No longer needed *)
   RA(I) := RRA;
 END; (* top loop *)
END heapsort;


BEGIN
	Put_Line(" Works on bounded_strings, not pointers.");
  Open (InFile, In_File, InputFileName);
  CreateOrOpen (OutFile, Out_File, OutputFileName);
  -- read data into entry list
  WHILE NOT END_Of_File(InFile) AND (NumberOfEntries < MaxEntries) LOOP
    Get_Line(InFile,Buffer,Length);
    NewEntry := To_Bounded_String(To_Upper(Buffer(1..Length)));
    NumberOfEntries := NumberOfEntries + 1;
    EntryList(NumberOfEntries) := NewEntry;
--    EntryPointerList(NumberOfEntries) := new WordEntryType'(NewEntry);
  END LOOP;

-- sort entryList now using binary insertion sort
--  BinInsSort(EntryList,EntryList'First,EntryList'Last);
--   StraightInsSort(EntryList,EntryList'First,EntryList'Last);
--   qsort(EntryList,EntryList'First,EntryList'Last);
   heapsort(EntryList,EntryList'First,EntryList'Last);

   Put(" EntryPointerList'First=");
   Put(EntryList'First);
   Put(", EntryPointerList'Last=");
   Put(EntryList'Last);
   New_Line;

  -- write sorted data
  FOR I in 1 .. NumberOfEntries LOOP
    Put_Line(OutFile,To_String(EntryList(I)));
  END LOOP;
END SortStuff.
