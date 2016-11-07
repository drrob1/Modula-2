MODULE TestHolidays;

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
  25 Dec 14 -- This test module is converted from cfnts.mod


  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
*)
  FROM SYSTEM IMPORT ADR;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM Environment IMPORT GetCommandLine;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  IMPORT RConversions, LongStr, LongConv;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM REALLIB IMPORT AINT,ROUND,AMOD,PWRI,GETCROPNUM;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;
  FROM HolidayCalc IMPORT HolType, GetHolidays;
  IMPORT MiscM2,ASCII;
  FROM MiscM2 IMPORT WriteString, WriteLn, WriteCard, WriteInt, PressAnyKey;


  CONST NUMOFCMDS   = 20;
        HASHCONST   = 43;
        FUDGEFACTOR = 5.0;
        CALCCMD     = '=';

  VAR
    FINI,GOOD                                           : BOOLEAN;
    C,K,RETCOD,RETCOD2,CMDNUM,MM,DD,YY,YEAR,DOW,HOLNUM,
    STRLEN,NON0POSN,NONBLPOSN                           : CARDINAL;
    I,J,SUM                                             : INTEGER;
    L                                                   : LONGINT;
    CH                                                  : CHAR;
    STPPRG,HAVEMATCH                                    : BOOLEAN;
    PROMPT,NAMDFT,TYPDFT,OUTFNAM,INBUF,OUTBUF,TOKEN     : BUFTYP;
    X,Y,Z,JULDATE,R                                     : LONGREAL;
    OUTSTR,STR1,STR2,STR3                               : STRTYP;
    TKNSTATE,CHRSTATE                                   : FSATYP;
    CMDNAM                          : ARRAY [1..NUMOFCMDS] OF STR10TYP;
    HASHARRAY                       : ARRAY [0..HASHCONST+10] OF CARDINAL;
    DAYNAMES                        : ARRAY [1..7] OF STR10TYP;
    Holidays : HolType;
    year, YEAR : CARDINAL;

PROCEDURE SUBTDAYS(C,Y : CARDINAL) :CARDINAL;
(*
******************************** SUBTDAYS ********************************
Computes how many days to subtract from the holiday depending on the year.
Days to Subtract = C + [5/4 Y] - [3/4 (1 + [Y/100])  ]) MOD 7
*)
BEGIN
  RETURN((C + 5*Y DIV 4 - 3*(1 + Y DIV 100) DIV 4) MOD 7);
END SUBTDAYS;

(************************ MAIN ***************************************)
BEGIN

(*  MLK,Pres,Easter,Mother,Memorial,Father,Labor,Columbus,Election,Thanksgiving : mdtype; *)
  FOR year := 2013 TO 2024 DO
    GetHolidays(year,Holidays);
    WriteCard(year);
    WriteString(" : MLK is Jan ");
    WriteCard(MLK.d);
    WriteString(", Pres Day is Feb ");
    WriteCard(Pres.d);
    WriteString(", Easter is ");
    WriteCard(Easter.m);
    WriteString("/");
    WriteCard(Easter.d);
    WriteString(", MomDay is May ");
    WriteCard(Mother.d);
    WriteString(", Memorial Day is May ");
    WriteCard(Memorial.d);
    WriteLn;
    WriteString(" DadDay is June ");
    WriteCard(Father.d);
    WriteString(", LaborDay is Sep ");
    WriteCard(Labor.d);
    WriteString(", ColumbusDay is Oct ");
    WriteCard(Columbus.d);
    WriteString(", ElectionDay is Nov ");
    WriteCard(Election.d);
    WriteString(", ThanksDay is Nov ");
    WriteCard(Thanksgiving.d);
    WriteLn;
    WriteLn;
  END;

  PressAnyKey;


END TestHolidays.
