<*/NOWARN:F*>
MODULE CHI2_2;
(*
  REVISION HISTORY
  ----------------
  24 Oct 03 -- Now in SBM2 4.0 for Win32
  25 Oct 03 -- Added elapsed timer.
*)
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;

  FROM ETIMER IMPORT READTIMER;
(* *)
  FROM MYRANDOM IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL
    (*,RANDLONGINT*);
(* *)
(*
  FROM MYRAND IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL
    (* ,RANDLONGINT *);
*)
IMPORT MMSYSTEM;
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
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
(****************************************************************************)

  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;


  CONST
    NUMOFBINS = 100;
  VAR
    C,K,K2,LASTRW : CARDINAL;
    L,TOTALMSECS  : LONGINT;
    OK            : BOOLEAN;
    BUF1          : BUFTYP;
    RW            : INTEGER;
    RAND,CHI2     : LONGREAL;
    CHI2BIN       : ARRAY [0..NUMOFBINS-1] OF INTEGER;
(*
    CHI2BINL      : ARRAY [0..NUMOFBINS-1] OF LONGINT;
*)
    HRS,MINS,SECS,ms : CARDINAL;
    s1,s2,s3,s4,s5,s6,s7,s8,s9 : STRTYP;

BEGIN
  LOOP
    CHI2 := 0.;
    FOR C := 0 TO NUMOFBINS-1 DO CHI2BIN[C] := 0; END(*FOR*);
    RW := 0;
    FOR C := 1 TO NUMOFBINS*10 DO
      INC(CHI2BIN[RANDCARD(NUMOFBINS)]);
    END(*FOR*);
    READTIMER(HRS,MINS,SECS,ms);
    WriteString(' Total elapsed time is : ');
    WriteCard(HRS,0);
    WriteString(':');
    WriteCard(MINS,0);
    WriteString(':');
    WriteCard(SECS,0);
    WriteString('.  Total ms = ');
    WriteInt(ms,0);
    WriteLn;

(*                  Chi^2 = (1/n) <sum> {(O(i) - E(i)}^2/E(i) *)
    FOR C := 0 TO NUMOFBINS-1 DO
      CHI2 := CHI2 + LFLOAT((CHI2BIN[C]-10)*(CHI2BIN[C]-10))/10.;
    END(*FOR*);
    CHI2 := CHI2/LFLOAT(NUMOFBINS);
    WriteString(' The chi square bins are :');
    WriteLn;
    FOR C := 0 TO NUMOFBINS-1 DO
      WriteInt(CHI2BIN[C],6);
      IF (C MOD 10) = 9 THEN WriteLn; END(*IF*);
    END(*FOR*);
    WriteLn;
    WriteLn;
    RealToStr(CHI2,s1);
    WriteString(s1);
    WriteLn;
    ReadString(s2);
    IF (STRCMPFNT(s2,'exit') = 0) OR (STRCMPFNT(s2,'EXIT')= 0) THEN EXIT END;
    SkipLine;
  END;
END CHI2_2.
