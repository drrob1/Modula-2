<*/NOWARN:F*>
IMPLEMENTATION MODULE HolMod;
(*
  REVISION HISTORY
  ----------------
  5 Apr 88 -- 1) Converted to M2 V3.03.
              2) Imported the REALLIB and CALLIB modules, and deleted their
                  code from here.  They were originally created here, but
                  it seems more appropriate to leave them in a central
                  module and import them.
  19 Mar 89 -- 1) Imported the newly created HPCALC to allow date arithmetic.
                  The rest of the code was modified to take advantage of this
                  new capability.
               2) Fixed a bug in the error reporting from the EASTER proc
                  so that a FOR index variable does not get assigned 0.
  30 Mar 89 -- 1) Fixed bug in the PR and HOL cmds that ignored 2 digit
                  years
               2) Added reminder to GREG help line to use quotes to force
                  the date to be taken as an ALLELSE TKNSTATE.
  26 Dec 90 -- 1) Utilized the GETTKNSTR procedure where appropriate.
               2) UL2 is used instead of UTILLIB.
               3) Added GETTKNEOL proc to deal with GREG & DOW cmds.
  25 Jul 93 -- 1) Dropping requirement of CALCCMD by passing cmdline thru
                   to GETRESULT.
               2) Allowed empty command line to quit after confirmation.
               3) Eliminated writing of trailing insignificant 0's from
                   arithmetic functions.
               4) Imported TKNRTNS instead of TOKENIZE.
               5) Eliminated need for GETTKNSTR by improving algorithm.
               6) Deleted GETTKNEOL proc as it is no longer used.  If
                   needed, it may be imported from TKNRTNS now.
  18 May 03 -- Conversion to Win32 using Stony Brook Modula-2 V 4
  20 Dec 04 -- Only deals w/ holidays (HPCalcTW does date arith) and uses Terminal w/o a msg loop.
  24 Jan 05 -- Added icon.
   8 May 07 -- Made HolMod.
  26 Dec 14 -- Added CalcMLK.
*)

  FROM SYSTEM IMPORT ADR, FUNC;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  IMPORT Strings;
  IMPORT Terminal, BasicDialogs;
  IMPORT MiscM2;
  FROM MiscM2 IMPORT WriteCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
    WriteReal, WriteLongReal, ReadString, ReadCard, ReadLongReal;
  FROM BasicDialogs IMPORT MessageTypes;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM WholeStr IMPORT ConvResults, StrToCard, CardToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM Environment IMPORT GetCommandLine;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,RMVCHR,ASSIGN2BUF, MRGBUFS, APPENDA2B, CONCATAB2C, INSERTAin2B;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  IMPORT RConversions, LongStr, LongConv;
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM REALLIB IMPORT AINT,ROUND,AMOD,PWRI,GETCROPNUM;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY,GREG2JUL,JUL2GREG,GETMDY;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM,FOPEN;

CONST NUMOFCMDS   = 20;
      HASHCONST   = 43;
      CALCCMD     = '=';
      HolidayIcon = '#100';

  VAR
    C,K              : CARDINAL;
    LJULDATE         : LONGINT;
    JULDATE          : LONGREAL;
    CMDNAM           : ARRAY [1..NUMOFCMDS] OF STR10TYP;
    HASHARRAY        : ARRAY [0..HASHCONST+10] OF CARDINAL;
    DAYNAMES         : ARRAY [1..7] OF STR10TYP;

PROCEDURE SUBTDAYS(C,Y : CARDINAL) :CARDINAL;
(*
******************************** SUBTDAYS ********************************
Subtract Days.
Computes how many days to subtract from the holiday depending on the year.
Days to Subtract = C + [5/4 Y] - [3/4 (1 + [Y/100])  ]) MOD 7
*)
BEGIN
  RETURN((C + 5*Y DIV 4 - 3*(1 + Y DIV 100) DIV 4) MOD 7);
END SUBTDAYS;

(*****************************************************************************************)
PROCEDURE CalcMLK(year : CARDINAL; VAR OUT day : CARDINAL);
(*
 Find the date of MLK day by finding which day back from Jan 21 is a Monday.
*)

VAR
  J : LONGINT;
  
BEGIN
  day := 21;
  J := JULIAN(1,day,year);
  WHILE (J MOD 7) <> 1 DO
    DEC(day);
    J := JULIAN(1,day,year);
  END;
END CalcMLK; 

PROCEDURE EASTER(YEAR : CARDINAL; VAR MM,DD : CARDINAL);
(*
************************************ EASTER ******************************
EASTER.
This routine computes the golden number for that year, then Easter Sunday is
the first Sunday following this date.  If the date is a Sunday, then Easter
is the following Sunday.
*)
VAR C,K,GOLDENNUM,IGNORE : CARDINAL;
    JULDATE              : LONGINT;

BEGIN
  IF (YEAR < 1900) OR (YEAR > 2500) THEN
    MM := 0;
    DD := 0;
    RETURN;
  END(*IF*);

  GOLDENNUM := (YEAR MOD 19) + 1;
  CASE GOLDENNUM OF
     1: (* APR 14 *)
      MM := 4;
      DD := 14;
  |  2: (* APR 3  *)
      MM := 4;
      DD := 3;
  |  3: (* MAR 23 *)
      MM := 3;
      DD := 23;
  |  4: (* APR 11 *)
      MM := 4;
      DD := 11;
  |  5: (* MAR 31 *)
      MM := 3;
      DD := 31;
  |  6: (* APR 18 *)
      MM := 4;
      DD := 18;
  |  7: (* APR 8  *)
      MM := 4;
      DD := 8;
  |  8: (* MAR 28 *)
      MM := 3;
      DD := 28;
  |  9: (* APR 16 *)
      MM := 4;
      DD := 16;
  | 10: (* APR 5  *)
      MM := 4;
      DD := 5;
  | 11: (* MAR 25 *)
      MM := 3;
      DD := 25;
  | 12: (* APR 13 *)
      MM := 4;
      DD := 13;
  | 13: (* APR 2  *)
      MM := 4;
      DD := 2;
  | 14: (* MAR 22 *)
      MM := 3;
      DD := 22;
  | 15: (* APR 10 *)
      MM := 4;
      DD := 10;
  | 16: (* MAR 30 *)
      MM := 3;
      DD := 30;
  | 17: (* APR 17 *)
      MM := 4;
      DD := 17;
  | 18: (* APR 7  *)
      MM := 4;
      DD := 7;
  | 19: (* MAR 27 *)
      MM := 3;
      DD := 27;
  END(*CASE*);
(*
  NOW FIND NEXT SUNDAY.
*)
  JULDATE := JULIAN(MM,DD,YEAR);
(*                                                                              old way GREG2JUL(MM,DD,YEAR,JULDATE); *)
  WHILE (JULDATE MOD 7) <> 0 DO
    INC(JULDATE);
  END (*WHILE*);
  GREGORIAN(JULDATE,MM,DD,IGNORE);
END EASTER;

PROCEDURE HOLCMD(YEAR,HOLNUM : CARDINAL; VAR HolDay : CARDINAL);

(*
***************************** HOLCMD ************************************
HOLIDAY COMMAND.
This procedure computes the holiday date for the input year and holiday
number.  If HOLNUM is out of range, proc returns 0.
*)

VAR MM,DD,DOW,C : CARDINAL;
    JULDATE : LONGREAL;

BEGIN
  CASE HOLNUM OF
    10: (* WB -- WASHINGTON'S BIRTHDAY *)
      HolDay := 21 - SUBTDAYS(2,YEAR-1);
  | 11: (* EA -- EASTER *)
      EASTER(YEAR,MM,DD);
  | 12: (* MD -- MOTHER'S DAY *)
      HolDay := 14 - SUBTDAYS(0,YEAR);
  | 13: (* MM -- MEMORIAL DAY *)
      HolDay := 31 - SUBTDAYS(2,YEAR);
  | 14: (* FD -- FATHER'S DAY *)
      HolDay := 21 - SUBTDAYS(3,YEAR);
  | 15: (* LD -- LABOR DAY *)
      HolDay := 7 - SUBTDAYS(3,YEAR);
  | 16: (* CD -- COLUMBUS DAY *)
      HolDay := 14 - SUBTDAYS(5,YEAR);
  | 17: (* ED -- ELECTION DAY *)
      HolDay := 8 - SUBTDAYS(1,YEAR);
  | 18: (* TG -- THANKSGIVING *)
      HolDay := 28 - SUBTDAYS(5,YEAR);
  ELSE (* INVALID HOLCMD ENTERED *)
    HolDay := 0;
  END(*CASE*);
  
END HOLCMD;

(************************ MAIN ***************************************)
BEGIN
  FOR C := 0 TO HASHCONST+10 DO HASHARRAY[C] := 0; END(*FOR*);
  HASHARRAY[19] := 1;
  HASHARRAY[20] := 2;
  HASHARRAY[35] := 3;
  HASHARRAY[12] := 4;
  HASHARRAY[39] := 5;
  HASHARRAY[29] := 6;
  HASHARRAY[21] := 7;
  HASHARRAY[38] := 8;
  HASHARRAY[33] := 9;
  HASHARRAY[24] := 10;
  HASHARRAY[5] := 11;
  HASHARRAY[16] := 12;
  HASHARRAY[25] := 13;
  HASHARRAY[9] := 14;
  HASHARRAY[15] := 15;
  HASHARRAY[6] := 16;
  HASHARRAY[8] := 17;
  HASHARRAY[26] := 18;
  HASHARRAY[2] := 19;

  CMDNAM[1] := 'DOW';
  CMDNAM[2] := 'JUL';
  CMDNAM[3] := 'GREG';
  CMDNAM[4] := 'HOL';
  CMDNAM[5] := 'HELP';
  CMDNAM[6] := 'H';
  CMDNAM[7] := '?';
  CMDNAM[8] := 'Q';
  CMDNAM[9] := 'PR';
  CMDNAM[10] := 'WB';
  CMDNAM[11] := 'EA';
  CMDNAM[12] := 'MD';
  CMDNAM[13] := 'MM';
  CMDNAM[14] := 'FD';
  CMDNAM[15] := 'LD';
  CMDNAM[16] := 'CD';
  CMDNAM[17] := 'ED';
  CMDNAM[18] := 'TG';
  CMDNAM[19] := 'ALL';

  DAYNAMES[1] := 'Sunday';
  DAYNAMES[2] := 'Monday';
  DAYNAMES[3] := 'Tuesday';
  DAYNAMES[4] := 'Wednesday';
  DAYNAMES[5] := 'Thursday';
  DAYNAMES[6] := 'Friday';
  DAYNAMES[7] := 'Saturday';

END HolMod.
