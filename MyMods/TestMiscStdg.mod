MODULE TestMiscStdg;
(*
REVISION HISTORY
----------------
 2 Feb 07 -- Modified testing for tokenptr to test problem found w/ LCHINBUF for CitiFilterTW
10 Oct 13 -- Testing gm2 routines.
15 Oct 13 -- Now testing new MiscStdInOutg routines.
*)
  FROM SYSTEM IMPORT ADR;
  IMPORT MiscStdInOutg, FpuIO;
  FROM MiscStdInOutg IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteCard,ADDCOMMAS,WriteLongReal,
LongRealToFixedString,
LongRealToFloatString,
LongRealToEngString,
LongRealToString ,
LongRealToStr, 
StringToLongReal, 
LongCardToString,
CardToString,
IntToString,
LongIntToString,
StringToCard,
StringToLongCard,
StringToInt,
StringToLongInt,
ReadAllChars,
FormatLongRealAsFloat;

(*
LongRealToFixedString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);
LongRealToFloatString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);
LongRealToEngString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);
LongRealToString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);
LongRealToStr (x: LONGREAL; VAR buffer: ARRAY OF CHAR);
StringToLongReal (s: ARRAY OF CHAR; VAR x: LONGREAL);
LongCardToString(LC: LONGCARD; VAR str: ARRAY OF CHAR);
CardToString(c: CARDINAL; VAR str: ARRAY OF CHAR);
IntToString(i: INTEGER; VAR str: ARRAY OF CHAR);
LongIntToString(LI: LONGINT; VAR str: ARRAY OF CHAR);
StringToCard(s: ARRAY OF CHAR; VAR c: CARDINAL);
StringToLongCard(s: ARRAY OF CHAR; VAR LC: LONGCARD);
StringToInt(s: ARRAY OF CHAR; VAR i: INTEGER);
StringToLongInt(s: ARRAY OF CHAR; VAR LI: LONGINT);
*)
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM LowLong IMPORT exponent;
  IMPORT LongMath, LMathLib0;
  FROM UTILLIBg IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT,MINCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM REALLIBg IMPORT CROPNSTR;
  FROM Environg IMPORT GetCommandLine;


  TYPE TSNAMEType = ARRAY [0..3] OF STR10TYP;

  CONST TSNAME = TSNAMEType {'DELIM','OP','DGT','ALLELSE'};

  VAR INBUF,TOKEN : BUFTYP;
(*
    TKNSTATE       : FSATYP;
    tpv1,tpv2,tpv3 : TKNPTRTYP;
*)
    RETCOD,C,posn,c1,c2,
    dotposn,eposn,Eposn    : CARDINAL;
    LC                     : LONGCARD;
    I, expnt               : INTEGER;
    L                      : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH                     : CHAR;
    s,s1,s2,s3,s4          : STRTYP;

PROCEDURE MyStringToLongInt(s: ARRAY OF CHAR; VAR LI: LONGINT);
VAR
  c, lenstr : CARDINAL;
  neg       : BOOLEAN;

BEGIN
  LI := 0;
  c := 0;
  lenstr := LENGTH(s);
  IF s[0] = '-' THEN
    neg := TRUE;
    INC(c);
  ELSE
    neg := FALSE;
  END; (* if negative number *)
  WHILE c < lenstr DO
    LI := 10 * LI + VAL(LONGINT,INT(ORD(s[c]) - ORD('0')));
    INC(c);
  END; (* conversion while loop *)
  IF neg THEN LI := -LI; END;
END MyStringToLongInt;

BEGIN

  LOOP
    WriteString(' Input line : ');
    ReadString(INBUF.CHARS);
    WriteLn;
    TRIM(INBUF);
    IF STRCMPFNT(INBUF.CHARS,'quit') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    eposn := LCHINBUFFNT(INBUF,"e");
    Eposn := LCHINBUFFNT(INBUF,'E');
    dotposn := LCHINBUFFNT(INBUF,'.');
    IF (eposn>0) OR (Eposn>0) OR (dotposn>0) THEN (* have real number *)
      StringToLongReal(INBUF.CHARS,R);
      WriteString('R=');
      WriteLongReal(R);
      LongRealToStr(R,s1);
      LongRealToString(R,s2,20);
      WriteString(', ToStr :');
      CROPNSTR(s1);
      WriteString(s1);
      WriteString(', ToString : ');
      CROPNSTR(s2);
      WriteString(s2);
      FormatLongRealAsFloat(R,s3,13);
      WriteString(', float: ');
      WriteString(s3);
(*             Cannot take ln of a neg number, and I want to test neg numbers also.
      r1 := LongMath.ln(R)/LongMath.ln(10.0);
      r2 := LMathLib0.ln(R)/LMathLib0.ln(10.0);
      WriteString(', LongMath ln: ');
      WriteLongReal(r1);
      WriteString(', LMathLib0 ln: ');
      WriteLongReal(r2);
*)
    ELSIF INBUF.CHARS[1] = '-' THEN (* have negative integer *)
(*      FpuIO.StrToLongInt(INBUF.CHARS,L); *)
      StringToLongInt(INBUF.CHARS,L);
      WriteString(' LongInt: ');
      LongIntToString(L,s1);
      IF L > 100000 THEN ADDCOMMAS(s1); END;
      WriteString(s1);
      MyStringToLongInt(INBUF.CHARS,L);
      WriteString(', My LongInt: ');
      LongIntToString(L,s1);
      IF L > 100000 THEN ADDCOMMAS(s1); END;
      WriteString(s1);
      IF ABS(L) < 2000000000 THEN 
        StringToInt(INBUF.CHARS,I);
        IntToString(I,s2);
        WriteString(', Int: ');
        WriteString(s2);
      END; (* if in range of an integer *)
    ELSE (* have positive whole number *)
      StringToLongCard(INBUF.CHARS,LC);
      LongCardToString(LC,s);
      IF LC > 100000 THEN ADDCOMMAS(s); END;
      WriteString(' LongCard: ');
      WriteString(s);
      IF LC < 4000000000 THEN
        StringToCard(INBUF.CHARS,c1);
        CardToString(c1,s1);
        IF c1 > 100000 THEN ADDCOMMAS(s1); END;
        WriteString(', Card: ');
        WriteString(s1);
      END; (* if in range of a cardinal *)
      IF LC < 2000000000 THEN
        StringToLongInt(INBUF.CHARS,L);
        WriteString(' LongInt: ');
        LongIntToString(L,s1);
        IF L > 100000 THEN ADDCOMMAS(s1); END;
        WriteString(s1);
        MyStringToLongInt(INBUF.CHARS,L);
        WriteString(', My LongInt: ');
        LongIntToString(L,s1);
        IF L > 100000 THEN ADDCOMMAS(s1); END;
        WriteString(s1);
        StringToInt(INBUF.CHARS,I);
        IntToString(I,s2);
        WriteString(', Int: ');
        WriteString(s2);
      END; (* if in range of a positive integer *)
    END; (* if real, negative or positive *)
    WriteLn;
  END; (* reading loop *)  
  WriteLn;
END TestMiscStdg.
