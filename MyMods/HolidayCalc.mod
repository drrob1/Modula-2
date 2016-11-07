IMPLEMENTATION MODULE HolidayCalc;

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
  25 Dec 14 -- Converted to a module to get holiday dates by the calculator, using HolMod written for the Cal program.

  FROM Environment IMPORT GetCommandLine;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;
*)
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,SCANBACK,SCANFWD,COPYLEFT;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc,round;
  IMPORT RConversions, LongStr, LongConv;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM REALLIB IMPORT AINT,ROUND,AMOD,PWRI,GETCROPNUM;
  FROM HolMod IMPORT HOLCMD,EASTER,CalcMLK;





(*****************************************************************************************)
PROCEDURE GetHolidays(year : CARDINAL; VAR OUT Holidays : HolType);
        VAR
          m,d,c : CARDINAL;

BEGIN

  Holidays.MLK.m := 1;
  CalcMLK(year,Holidays.MLK.d);

  Holidays.Pres.m := 2;
  HOLCMD(year,10,Holidays.Pres.d);

  EASTER(year,Holidays.Easter.m,Holidays.Easter.d);

  Holidays.Mother.m := 5;
  HOLCMD(year,12,Holidays.Mother.d);

  Holidays.Memorial.m := 5;
  HOLCMD(year,13,Holidays.Memorial.d);

  Holidays.Father.m := 6;
  HOLCMD(year,14,Holidays.Father.d);

  Holidays.Labor.m := 9;
  HOLCMD(year,15,Holidays.Labor.d);

  Holidays.Columbus.m := 10;
  HOLCMD(year,16,Holidays.Columbus.d);

  Holidays.Election.m := 11;
  HOLCMD(year,17,Holidays.Election.d);

  Holidays.Thanksgiving.m := 11;
  HOLCMD(year,18,Holidays.Thanksgiving.d);

END GetHolidays;

(*****************************************************************************************)


(************************ MAIN ***************************************)

END HolidayCalc.
