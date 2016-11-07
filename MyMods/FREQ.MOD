MODULE FREQ;

(*
  REVISION HISTORY
  ----------------
*)
  FROM SYSTEM IMPORT ADR,DOSCALL
(*Error  : Unsupported SYSTEM item ==> 'DOSCALL' *);
  IMPORT Break;
  IMPORT DebugPMD;
  FROM Terminal IMPORT ReadString;
  IMPORT Terminal;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM LongIO IMPORT ReadLongInt, WriteLongInt;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;
  FROM UTILLIB IMPORT MAXCARD,CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT;
  FROM UL2 IMPORT COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF,GETFNM;
  FROM FIO IMPORT DRIVESEP,SUBDIRSEP,MYFILTYP,IOSTATE,FRESET,FCLOSE,
    FRDTXLN,FWRTXLN,FWRTX,RETBLKBUF,EXTRACTDRVPTH,FCLOSEDUP,FWRLN;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    GETTKNREAL,UNGETTKN;
(*  FROM SORTER IMPORT ITEMARRAY,LINKS,TREESORT,QUICKSORT,BININSSORT;*)
  

  CONST
    MAXLONGINT = 7FFFFFFFH;
    SP = ORD(' ');
    ORDAT = ORD('@');
    ORDCAPA = ORD('A');
    ORDCAPZ = ORD('Z');
    N  = 26;

TYPE
  INDEX = INTEGER;
  ITEM  = LONGINT;

  VAR
    LNCTR,C,ORDCAPCH                                : CARDINAL;
    INUN1                                           : MYFILTYP;
    PROMPT,NAMDFT,TYPDFT,BLANKBUF,INFNAM,INPUT,BUF  : BUFTYP;
    EOFFLG                                          : BOOLEAN;
  LETTERTABLE : ARRAY [1..N]  OF ITEM;
  LINKS     : ARRAY [1..2*N]  OF INDEX;
    REVLINKS                        : ARRAY [1..N] OF CARDINAL;

PROCEDURE TREESORT(LASTELEM : CARDINAL);

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
    LINKS[1] := 1;       (* That pesky zero origin arrays again *)
    LINKS[SEQHEAD] := 1;
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
          LINKS[ELEMIDX] := ELEMIDX;
          LINKS[SEQHEAD] := ELEMIDX;
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
        LINKS[ELEMIDX] := ELEMIDX;
        LINKS[SEQHEAD] := ELEMIDX;
(* L0 is head of older leaf *)
        L0 := SEQHEAD;
(* Head of most recent leaf *)
        INC(SEQHEAD);
        INC(ELEMIDX);
(* Make a leaf *)
        L2 := ELEMIDX;
        LINKS[ELEMIDX] := ELEMIDX;
        LINKS[SEQHEAD] := ELEMIDX;
        STATE := 4;
      | 4:
(* 9 *) IF LETTERTABLE[L1] <= LETTERTABLE[L2] THEN
          STATE := 5 (* switch to sequence 1 *)
        ELSE (* switch to sequence 2 *)
          LINKS[L0] := L2;
(* 8 *)   LOOP
            L0 := L2;
            L2 := LINKS[L0]; (* next leaf *)
            IF L2 = L0 THEN (* switch to sequence 1 *)
              LINKS[L0] := L1;
              STATE := 6;
              EXIT;
            END(*IF*);
            IF (LETTERTABLE[L1] <= LETTERTABLE[L2]) THEN EXIT END(*IF*);
          END(*LOOP*);
          IF STATE (* still *) = 4 THEN
            LINKS[L0] := L1;
            STATE := 5;
          END(*IF*);
        END(*IF*);
      | 5:
(* 11 *)L0 := L1;
        L1 := LINKS[L0];
        IF L1 <> L0 THEN
          STATE := 4;
        ELSE
          LINKS[L0] := L2;
          STATE := 6;
        END(*IF*);
      | 6:
(* 10 *)DEC(MERGES);
        IF MERGES > 0 THEN
(* 12 *)  DEC(SEQHEAD);  (* Head of latest branch or twig *)
          L0 := SEQHEAD - 1;    (* Head of older branch or twig *)
          L1 := LINKS[L0];      (* Head of sequence 1 *)
          L2 := LINKS[SEQHEAD]; (* Head of sequence 2 *)
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

  PROCEDURE PRINTSORTED;

  VAR
    C,I,J,K : CARDINAL;

  BEGIN
    TREESORT(N);

    WriteString(' TreeSorted Array: ');
    WriteLn;
    K := LINKS[N+1];
    FOR C := 1 TO N DO
      REVLINKS[C] := K;
      K := LINKS[K];
    END(*FOR*);
    FOR C := N TO 1 BY -1 DO
      Write(CHR(REVLINKS[C]+ORDAT));
      K := LINKS[K];
    END(*FOR*);
    WriteLn;

  END PRINTSORTED;

  BEGIN
    ASSIGN2BUF(' Enter Input File Name : ',PROMPT);
    ASSIGN2BUF('',NAMDFT);
    ASSIGN2BUF('.DOC',TYPDFT);
    GETFNM(PROMPT, NAMDFT, TYPDFT, INFNAM); 
    WriteString(' Input File Name : ');
    WriteString(INFNAM.CHARS);
    WriteLn;
    FRESET(INUN1,INFNAM,RD);

    LNCTR := 0;
    FOR C := 1 TO N DO LETTERTABLE[C] := 0; END(*FOR*);

    LOOP
      FRDTXLN(INUN1,BUF,EOFFLG);
      IF EOFFLG THEN EXIT END(*IF*);
      INC(LNCTR);
      C := 1;
      WHILE C <= BUF.COUNT DO     (* Will skip blank lines *)
        ORDCAPCH := ORD(CAP(BUF.CHARS[C]));
        INC(C);
        IF (ORDCAPCH >= ORDCAPA) AND (ORDCAPCH <= ORDCAPZ) THEN
          DEC(ORDCAPCH,ORDAT);
          LETTERTABLE[ORDCAPCH] := LETTERTABLE[ORDCAPCH] + 1;
          IF LETTERTABLE[ORDCAPCH] = MAXLONGINT THEN EXIT; END(*IF*);
        END(*IF*);
      END(*WHILE*);
    END(*LOOP*);

    FCLOSE(INUN1);

    WriteString(' Processed ');
    WriteCard(LNCTR,0);
    WriteString(' lines.');
    WriteLn;

    FOR C := 1 TO N DO
      Write(' ');
      Write(CHR(C+ORDAT));
      WriteString(' = ');
      WriteLongInt(LETTERTABLE[C],10);
      IF C MOD 5 = 0 THEN WriteLn END(*IF*);
    END(*FOR*);
    WriteLn;

    PRINTSORTED;

  END FREQ.
