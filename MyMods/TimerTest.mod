<*/NOWARN:F*>
MODULE TimerTest;
(*
  REVISION HISTORY
  ----------------
*)

(*
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK, CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
*)
  FROM MYRAND IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL;
IMPORT MMSYSTEM;

FROM ETIMER IMPORT READTIMER;
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR;
IMPORT WINUSER, WIN32, WINGDI, WINX;
IMPORT Storage;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT SysMets;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
(*
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    UNGETTKN,GETTKNREAL;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
*)
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
(****************************************************************************)
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;

  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

  CONST SEED = 6280;

  VAR
    BUF1,BUF2,BUF3 : BUFTYP;
    OK,STOP        : BOOLEAN;
    CH1,CH2,CH3    : CHAR;
    I1,I2,I3       : INTEGER;
    L1,L2,L3       : LONGINT;
    C1,C2,C3       : CARDINAL;
    R1,R2,R3       : LONGREAL;
    HRS,MINS,SECS,MSECS  : CARDINAL;
    TOTALMSECS     : LONGINT;
    s1,s2,s3,s4,s5,s6,s7,s8,s9 : STRTYP;


BEGIN (******************************** MAIN ****************************)
  STOP := FALSE;
  RANDINIT(SEED);
  LOOP
    WriteString(' RandReal Call.'); WriteLn;
    FOR C1 := 1 TO 10 DO
      R1 := RANDREAL();
      RealToStr(R1,s1);
      WriteString(s1);
      WriteString('  ');
      IF NOT ODD(C1) THEN WriteLn; END(*IF*);
    END(*FOR*);
    WriteLn;
    WriteString(' RandCard(0) Call.'); WriteLn;
    FOR C1 := 1 TO 10 DO
      C2 := RANDCARD(0);
      WriteCard(C2,0);
      IF (C1 MOD 5) = 0 THEN WriteLn; END;
    END(*FOR*);

  WriteString(' Hit <enter> to continue.');
  WriteLn;
(*  ReadChar(CH1); *)
  SkipLine;
    WriteLn;
    WriteString(' RandCard(100) Call.'); WriteLn;
    FOR C1 := 1 TO 10 DO
      C3 := RANDCARD(100);
      WriteCard(C3,0);
      IF (C1 MOD 5) = 0 THEN WriteLn; END;
    END(*FOR*);
    WriteLn;
    WriteString(' RandInt(0) Call.'); WriteLn;
    FOR I1 := 1 TO 10 DO
      I2 := RANDINT(0);
      WriteInt(I2,0);
      IF (I1 MOD 5) = 0 THEN WriteLn; END;
    END(*FOR*);
    WriteLn;
    WriteString(' RandInt(50) Call.'); WriteLn;
    FOR I1 := 1 TO 10 DO
      I3 := RANDINT(50);
      WriteInt(I3,0);
      IF (I1 MOD 5) = 0 THEN WriteLn; END;
    END(*FOR*);

  WriteString(' Hit <enter> to continue.');
  WriteLn;
(*  ReadChar(CH1); *)
  SkipLine;
(*
    WriteLn;
    WriteString(' RandLongInt(0) Call.'); WriteLn;
    FOR I1 := 1 TO 12 DO
      L2 := RANDLONGINT(0);
      WriteLongInt(L2,15);
      IF (I1 MOD 4) = 0 THEN WriteLn; END;
    END;
*)
(*    WriteLn;
    WriteString(' RandLongInt(1.E5) Call.'); WriteLn;
    FOR I1 := 1 TO 12 DO
      L2 := RANDLONGINT(100000);   (* 1. E 5 *)
      WriteLongInt(L2,15);
      IF (I1 MOD 4) = 0 THEN WriteLn; END;
    END;
*)

    READTIMER(HRS,MINS,SECS,MSECS);
    WriteString(' Total elapsed time is : ');
    WriteCard(HRS,0);
    WriteString(':');
    WriteCard(MINS,0);
    WriteString(':');
    WriteCard(SECS,0);
    WriteString('.  Total ms = ');
    WriteCard(MSECS,0);
(*    WriteLongInt(TOTALMSECS,0); *)
    WriteLn;

    IF STOP THEN EXIT; END(*IF*);
    STOP := TRUE;
    RANDINIT(SEED);
  END(*LOOP*);
  RANDOMIZE;
  WriteString(' RandReal Call.'); WriteLn;
  FOR C1 := 1 TO 10 DO
    R1 := RANDREAL();
    RealToStr(R1,s1);
    WriteString(s1);
    WriteString('  ');
    IF NOT ODD(C1) THEN WriteLn; END(*IF*);
  END(*FOR*);

  READTIMER(HRS,MINS,SECS,MSECS);
  WriteString(' Total elapsed time is : ');
  WriteCard(HRS,0);
  WriteString(':');
  WriteCard(MINS,0);
  WriteString(':');
  WriteCard(SECS,0);
  WriteString('.  Total ms = ');
  WriteCard(MSECS,0);
(*  WriteLongInt(TOTALMSECS,0); *)
  WriteLn;
  WriteLn;
  WriteString(' Hit <enter> to continue.');
  WriteLn;
(*  ReadChar(CH1); *)
  SkipLine;

END TimerTest.
