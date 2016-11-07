<*/NOWARN:F*>
IMPLEMENTATION MODULE ETIMER;
(*
  Elapsed Timer
  This is a very simple module that implements an event timer by reading
  the system time and comparing to that.  This routine will not work
  properly when the timer goes past midnight.  Perhaps I'll add this at
  some other point in time.

  REVISION HISTORY
  ================
  23 Dec 91 -- Converted to M2 V 4.00.  No longer need to do contortions for
                LongInts.
  24 Oct 03 -- Converted to SBM2 V 4.0 for Win32
*)

FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR;
IMPORT WINUSER, WIN32, WINGDI, WINX;
IMPORT Storage;
IMPORT Terminal, BasicDialogs,WholeStr;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT SysMets;
IMPORT Strings,MemUtils;
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
  IMPORT RConversions, LongStr, LongConv,Conversions;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
(****************************************************************************)
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;


CONST L60000 = 60000;  (* Number of msecs per min *)
      L60    = 60;
      L1000  = 1000;

VAR
  T0      : DateTime;

PROCEDURE STARTTIMER;
BEGIN
  GetClock(T0);
END STARTTIMER;

PROCEDURE READTIMER(VAR HRS,MINS,SECS,MSECS : CARDINAL);

VAR
  CurTime          : DateTime;
  EHRS,EMINS,ESECS,EJULIAN,tmsec : INTEGER; (* Elapsed time in the respective units *)

BEGIN
  GetClock(CurTime);
(*                  STACK[X] := LFLOAT(JULIAN(C1,C2,C3)); *)
  tmsec := (CurTime.hour-T0.hour)*3600000 + (CurTime.minute-T0.minute)*60000 +
       (CurTime.second-T0.second)*1000 + CurTime.fractions - T0.fractions;
  EJULIAN := JULIAN(CurTime.month,CurTime.day,CurTime.year) - JULIAN(T0.month,T0.day,T0.year);
  WHILE EJULIAN > 0 DO
    DEC(EJULIAN);
    tmsec := tmsec + 24*36000000;
  END;
  EHRS  := tmsec DIV 3600000;
  EMINS := (tmsec - EHRS*3600000) DIV 60000;
  ESECS := (tmsec - EHRS*3600000 - EMINS*60000) DIV 1000;
(* Just for fun, I'll use a different formula *)
  HRS   := EHRS;
  MINS  := (tmsec MOD 3600000) / 60000;
  SECS  := (tmsec MOD 60000)/1000;
  MSECS := tmsec MOD 1000;
END READTIMER;

BEGIN (* Initialize the timer call *)
  STARTTIMER;
END ETIMER.
