<*/NOWARN:F*>
MODULE CHI2_1Term;
(*
  REVISION HISTORY
  ----------------
  24 Oct 03 -- Now in SBM2 4.0 for Win32
  25 Oct 03 -- Added elapsed timer.
  11 Nov 03 -- Playing w/ Terminal Module.
  21 May 06 -- Added Ran4.
*)
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
IMPORT MiscM2;
IMPORT RAN4;
  FROM Terminal IMPORT Read, WriteString, WriteLn, ReadChar, Write, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

  FROM ETIMER IMPORT READTIMER;
(*
  FROM MYRANDOM IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL,
    RANDLONGINT;
  FROM MYRAND2 IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL,
    RANDLONGINT;
  FROM Random IMPORT RandomInit,RandomCard,RandomInt,RandomReal,Randomize;
*)
  FROM MYRAND IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL
    (* ,RANDLONGINT *);
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
(*
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
*)

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
    ch               : CHAR;




BEGIN
  Reset;
  K := 1;
  BasicDialogs.PromptCard('4=ran4, else ran used',0,4,TRUE,K);

  LOOP
    CHI2 := 0.;
    FOR C := 0 TO NUMOFBINS-1 DO CHI2BIN[C] := 0; END(*FOR*);
    RW := 0;
    FOR C := 1 TO NUMOFBINS*10 DO
    	IF K = 4 THEN
        INC(CHI2BIN[RAN4.randcard(NUMOFBINS)]);
        s9 := 'Using ran4    ';
      ELSE
        INC(CHI2BIN[RANDCARD(NUMOFBINS)]);
        s9 := 'Using RAN     ';
      END; (*if*)
    END(*FOR*);
    WriteString(s9);
    READTIMER(HRS,MINS,SECS,ms);
    WriteLn;
    WriteString(' Total elapsed time is : ');
    WholeStr.CardToStr(HRS,s1); WriteString(s1);
    WriteString(':');
    WholeStr.CardToStr(MINS,s1); WriteString(s1);
    WriteString(':');
    WholeStr.CardToStr(SECS,s1); WriteString(s1);
    WriteString('.  Total ms = ');
    WholeStr.IntToStr(ms,s1); WriteString(s1);
    WriteLn;

(*                  Chi^2 = (1/n) <sum> {(O(i) - E(i)}^2/E(i) *)
    FOR C := 0 TO NUMOFBINS-1 DO
      CHI2 := CHI2 + LFLOAT((CHI2BIN[C]-10)*(CHI2BIN[C]-10))/10.;
    END(*FOR*);
    CHI2 := CHI2/LFLOAT(NUMOFBINS);
    WriteString(' The chi square bins are :');
    WriteLn;
    FOR C := 0 TO NUMOFBINS-1 DO
    	MiscM2.WriteRJCard(CHI2BIN[C],3);
(*      WholeStr.IntToStr(CHI2BIN[C],s2); WriteString(s2); WriteString('  '); *)
      IF (C MOD 10) = 9 THEN WriteLn; END(*IF*);
    END(*FOR*);
    WriteLn;
    WriteLn;
    RealToStr(CHI2,s1);
    WriteString(s1);
    WriteLn;
    Read(ch);
    IF (CAP(ch)= 'E') OR (CAP(ch)= 'Q') THEN EXIT END;
    Reset;
(*     SkipLine; *)
  END;
END CHI2_1Term.
