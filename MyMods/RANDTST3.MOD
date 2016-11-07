MODULE RANDTST3;
(*
  REVISION HISTORY
  ----------------
*)
  FROM SYSTEM IMPORT ADR;
  IMPORT Break;
  IMPORT DebugPMD;
  FROM Terminal IMPORT ReadString;
  IMPORT Terminal;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM LongIO IMPORT ReadLongInt, WriteLongInt;
  FROM RealConversions IMPORT RealToString, StringToReal;
  FROM RealInOut IMPORT ReadReal, WriteReal, Done;
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT;
  FROM UL2 IMPORT COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF,GETFNM;
(*
  FROM MYRANDOM IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL,
    RANDLONGINT;
*)
  FROM MYRAND3 IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL,
    RANDLONGINT;
  FROM TIMER IMPORT READTIMER;
(*
  FROM Conversions IMPORT ConvertCardinal, ConvertInteger;
  FROM MathLib0 IMPORT real,exp,ln;
  FROM Directories IMPORT Delete,DirResult;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;
  FROM TimeDate IMPORT Time,GetTime;
  FROM FIO IMPORT DRIVESEP,SUBDIRSEP,MYFILTYP,IOSTATE,FRESET,FCLOSE,
    FRDTXLN,FWRTXLN,FWRTX,RETBLKBUF,EXTRACTDRVPTH,FCLOSEDUP,FWRLN;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    GETTKNREAL,UNGETTKN;
  FROM TIMLIB IMPORT GETNEWDATE,TIME2MDY,MDY2TIME,MDY2STR,GREG2JUL,
    JUL2GREG,GETMDY;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR,FLOATLONG,TRUNCLONG;
  FROM Directories IMPORT Delete,DirResult;
*)

  CONST SEED = 6280;

  VAR
    BUF1,BUF2,BUF3 : BUFTYP;
    OK,STOP        : BOOLEAN;
    CH1,CH2,CH3    : CHAR;
    I1,I2,I3       : INTEGER;
    L1,L2,L3       : LONGINT;
    C1,C2,C3       : CARDINAL;
    R1,R2,R3       : LONGREAL;
    HRS,MINS,SECS  : CARDINAL;
    TOTALMSECS     : LONGINT;

BEGIN (******************************** MAIN ****************************)
  STOP := FALSE;
(*  RANDINIT(SEED); *)
  LOOP
(*    WriteString(' RandReal Call.'); WriteLn;*)
    FOR C1 := 1 TO 10 DO
      R1 := RANDREAL();
(*      RealToString(R1,12,25,BUF1.CHARS,OK);
      WriteString(BUF1.CHARS);
      IF NOT ODD(C1) THEN WriteLn; END(*IF*);*)
    END(*FOR*);
(*    WriteLn;
    WriteString(' RandCard(0) Call.'); WriteLn;*)
    FOR C1 := 1 TO 10 DO
      C2 := RANDCARD(0);
(*      WriteCard(C2,10);
      IF (C1 MOD 5) = 0 THEN WriteLn; END;*)
    END(*FOR*);
(*
  WriteString(' Hit any key to continue.');
  WriteLn;
  Read(CH1);
*)
(*    WriteLn;
    WriteString(' RandCard(100) Call.'); WriteLn;*)
    FOR C1 := 1 TO 10 DO
      C3 := RANDCARD(100);
(*      WriteCard(C3,10);
      IF (C1 MOD 5) = 0 THEN WriteLn; END;*)
    END(*FOR*);
(*    WriteLn;
    WriteString(' RandInt(0) Call.'); WriteLn;*)
    FOR I1 := 1 TO 10 DO
      I2 := RANDINT(0);
(*      WriteInt(I2,10);
      IF (I1 MOD 5) = 0 THEN WriteLn; END;*)
    END(*FOR*);
(*    WriteLn;
    WriteString(' RandInt(50) Call.'); WriteLn;*)
    FOR I1 := 1 TO 10 DO
      I3 := RANDINT(50);
(*      WriteInt(I3,10);
      IF (I1 MOD 5) = 0 THEN WriteLn; END;*)
    END(*FOR*);
(*
  WriteString(' Hit any key to continue.');
  WriteLn;
  Read(CH1);
*)
(*    WriteLn;
    WriteString(' RandLongInt(0) Call.'); WriteLn;*)
    FOR I1 := 1 TO 12 DO
      L2 := RANDLONGINT(0);
(*      WriteLongInt(L2,15);
      IF (I1 MOD 4) = 0 THEN WriteLn; END;*)
    END(*FOR*);
(*    WriteLn;
    WriteString(' RandLongInt(1.E5) Call.'); WriteLn;*)
    FOR I1 := 1 TO 12 DO
      L2 := RANDLONGINT(100000);   (* 1. E 5 *)
(*      WriteLongInt(L2,15);
      IF (I1 MOD 4) = 0 THEN WriteLn; END;*)
    END(*FOR*);
    READTIMER(HRS,MINS,SECS,TOTALMSECS);
    WriteString(' Total elapsed time is : ');
    WriteCard(HRS,0);
    WriteString(':');
    WriteCard(MINS,0);
    WriteString(':');
    WriteCard(SECS,0);
    WriteString('.  Total ms = ');
    WriteLongInt(TOTALMSECS,0);
    WriteLn;
    IF STOP THEN EXIT; END(*IF*);
    STOP := TRUE;
    RANDINIT(SEED);
  END(*LOOP*);
  RANDOMIZE;
(*  WriteString(' RandReal Call.'); WriteLn;*)
  FOR C1 := 1 TO 10 DO
    R1 := RANDREAL();
(*    RealToString(R1,12,25,BUF1.CHARS,OK);
    WriteString(BUF1.CHARS);
    IF NOT ODD(C1) THEN WriteLn; END(*IF*);*)
  END(*FOR*);
  READTIMER(HRS,MINS,SECS,TOTALMSECS);
  WriteString(' Total elapsed time is : ');
  WriteCard(HRS,0);
  WriteString(':');
  WriteCard(MINS,0);
  WriteString(':');
  WriteCard(SECS,0);
  WriteString('.  Total ms = ');
  WriteLongInt(TOTALMSECS,0);
  WriteLn;
  WriteLn;
END RANDTST3.
