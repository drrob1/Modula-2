IMPLEMENTATION MODULE LCARDLIB;
(*
  REVISION HISTORY
  ----------------
   6 Mar 05 -- Win32 version using SBM2 b 31.  But then I discovered I don't need this module.
                I'll move the one rtn I need to another module.  This module never re-compiled.
*)
(*
  IMPORT Break;  * Needed to more quickly trap ^C */
  IMPORT MED;    /* Needed to initialize the debugger */
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write,ReadString;
  FROM LongIO IMPORT ReadLongInt, WriteLongInt;
  FROM Storage IMPORT  Available, ALLOCATE, DEALLOCATE;
*)

  FROM SYSTEM  IMPORT  ADDRESS,ADR;
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT,
(*  FROM UL2 IMPORT *) COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF,GETFNM;

CONST ASCZERO = LONGORD('0');

PROCEDURE ADDCOMMAS(VAR INOUT STR : ARRAY OF CHAR);
VAR 
  C,PTR,NCOM,NDGTS,NULLPOSN : CARDINAL;
BEGIN
  NDGTS := STRLENFNT(STR);
  PTR := NDGTS - 1;  (* Zero origin array as a param *)
  NCOM := PTR DIV 3;
  NULLPOSN := NDGTS + NCOM;
  IF NULLPOSN > HIGH(STR) THEN
    WriteString(' Cannot add commas because string is too small.');
    WriteLn;
    RETURN;
  END(*IF*);
  WHILE NCOM > 0 DO
    FOR C := 1 TO 3 DO
      STR[PTR+NCOM] := STR[PTR];
      DEC(PTR);
    END(*FOR*);
    STR[PTR+NCOM] := ',';
    DEC(NCOM);
  END(*WHILE*);
  IF NULLPOSN < HIGH(STR) THEN STR[NULLPOSN] := 0C; END(*IF*);
END ADDCOMMAS;

PROCEDURE LCARD2STR(LC : LONGCARD; VAR STR : ARRAY OF CHAR);
VAR 
  DIGITS              : ARRAY [0..13] OF CHAR;
  C,PTR,NCOM,NULLPOSN : CARDINAL;
  NDGTS               : INTEGER;
BEGIN
  NDGTS := -1;
  REPEAT
    INC(NDGTS);
    DIGITS[NDGTS] := CHR(LC MOD 10 + ASCZERO);
    LC := LC DIV 10;
  UNTIL LC = 0;
  PTR := 0;
  WHILE NDGTS >= 0 DO 
    STR[PTR] := DIGITS[NDGTS]; 
    INC(PTR); 
    DEC(NDGTS); 
  END(*IF*);
(* Remember limitation of HIGH procedure *)
  IF PTR < HIGH(STR) THEN STR[PTR] := 0C; END(*IF*);
END LCARD2STR;

PROCEDURE WriteLongCard(N : LONGCARD; LEN : CARDINAL);
VAR 
  DIGITS,STRING : ARRAY [0..13] OF CHAR;
  NSPC,NDGTS,C  : INTEGER;
BEGIN
  LCARD2STR(N,STRING);
  NDGTS := STRLENFNT(STRING);
  IF (INTEGER(LEN) > NDGTS) AND (LEN <= 14) THEN
    NSPC := INTEGER(LEN) - NDGTS;
(* Null terminator is included in this copy operation *)
    COPYRIGHT(ADR(STRING[0]),ADR(STRING[NSPC]),NDGTS);
    FOR C := 0 TO NSPC-1 DO
      STRING[C] := ' ';
    END(*FOR*);
  END(*IF*);
  WriteString(STRING);
END WriteLongCard;
(*------------------------------------------------------------------*)
END LCARDLIB.