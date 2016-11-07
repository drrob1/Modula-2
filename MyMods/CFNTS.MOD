MODULE CFNTS;
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
*)
  FROM SYSTEM IMPORT ADR;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
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
(*
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    UNGETTKN,GETTKNREAL;
*)
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM REALLIB IMPORT AINT,ROUND,AMOD,PWRI,GETCROPNUM;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;

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

PROCEDURE GETCMDNUM(INBUF : BUFTYP; VAR CMDNUM : CARDINAL);
(*
********************************** GETCMDNUM *************************
GET COMMAND NUMBER.
This routine parses the input command and tests against the HASHARRAY and
CMDNAM arrays, to return the CMDNUM.  If an invalid command or any error
from GETTKN occurs, then the returned CMDNUM value is zero.

INPUT FROM GBL VAR'S : HASHCONST, HASHARRAY, CMDNAM.
OUTPUT TO GBL VAR'S : TOKEN, TKNSTATE.
*)

VAR
    C,K,RETCOD,HASHIDX : CARDINAL;
    SUM                : INTEGER;

BEGIN
  INI1TKN(INBUF);
  GETTKN(TOKEN,TKNSTATE,L,RETCOD);
  SUM := L;
  CMDNUM := 0;
  IF TKNSTATE <> ALLELSE THEN 
    CMDNUM := 99;
    RETURN;
  ELSIF RETCOD <> 0 THEN
    WriteString(' Invalid Command.  So such Command as : ');
    WriteString(TOKEN.CHARS);
    WriteString('.  Try Again.');
    WriteLn;
    RETURN;
  END(*IF*);

  HASHIDX := ORD(SUM) MOD HASHCONST;
  CMDNUM := HASHARRAY[HASHIDX];
  WHILE (CMDNUM > 0) AND (STRCMPFNT(TOKEN.CHARS,CMDNAM[CMDNUM]) <> 0) DO
    INC(HASHIDX);
    CMDNUM := HASHARRAY[HASHIDX];
  END(*WHILE*);
END GETCMDNUM;

PROCEDURE EASTER(YEAR : CARDINAL; VAR MM,DD : CARDINAL);
(*
************************************ EASTER ******************************
EASTER.
This routine computes the golden number for that year, then Easter Sunday is
the first Sunday following this date.  If the date is a Sunday, then Easter
is the following Sunday.

*)
VAR C,K,GOLDENNUM,IGNORE : CARDINAL;
    JULDATE              : LONGREAL;

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
  GREG2JUL(MM,DD,YEAR,JULDATE);
  REPEAT
    JULDATE := JULDATE + 1.;
  UNTIL (Round(AMOD(JULDATE,7.)) + 1) = 1;
  JUL2GREG(JULDATE,MM,DD,IGNORE);
END EASTER;

PROCEDURE HOLCMD(YEAR : CARDINAL; HOLNUM : CARDINAL);
(*
***************************** HOLCMD ************************************
HOLIDAY COMMAND.
This procedure computes the holiday date for the input year and holiday
number.  It this prints the result to the terminal.  If the HOLNUM is out of
range, this routine does nothing and makes HOLNUM zero.

INPUT FROM GBL VAR'S : DAYNAMES.
*)
VAR MM,DD,DOW,C : CARDINAL;
    JULDATE : LONGREAL;

BEGIN
  CASE HOLNUM OF
    10: (* WB -- WASHINGTON'S BIRTHDAY *)
      DD := 21 - SUBTDAYS(2,YEAR-1);
      WriteString(" Washington's Birthday is ");
      GREG2JUL(2,DD,YEAR,JULDATE);
      DOW := Round(AMOD(JULDATE,7.)) + 1;
      WriteString(DAYNAMES[DOW]);
      WriteString(',');
      WriteString(' Feb ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
  | 11: (* EA -- EASTER *)
      EASTER(YEAR,MM,DD);
      IF (MM = 0) THEN 
(*        HOLNUM := 0; *)
        WriteString
            (' Invalid operation for Easter Procedure.  Probably bad year.');
        WriteLn;
        RETURN;
      END(*IF*);
      WriteString(' Easter Sunday is : ');
      WriteCard(MM,0);
      WriteString('/');
      WriteCard(DD,0);
      WriteString('/');
      WriteCard(YEAR,0);
      WriteLn;
  | 12: (* MD -- MOTHER'S DAY *)
      DD := 14 - SUBTDAYS(0,YEAR);
      WriteString(" Mother's Day is Sunday");
      WriteString(',');
      WriteString(' May ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
  | 13: (* MM -- MEMORIAL DAY *)
      DD := 31 - SUBTDAYS(2,YEAR);
      WriteString(' Memorial Day is Monday');
      WriteString(',');
      WriteString(' May ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
  | 14: (* FD -- FATHER'S DAY *)
      DD := 21 - SUBTDAYS(3,YEAR);
      WriteString(" Father's Day is Sunday");
      WriteString(',');
      WriteString(' June ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
  | 15: (* LD -- LABOR DAY *)
      DD := 7 - SUBTDAYS(3,YEAR);
      WriteString(' Labor Day is Monday');
      WriteString(',');
      WriteString(' September ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
  | 16: (* CD -- COLUMBUS DAY *)
      DD := 14 - SUBTDAYS(5,YEAR);
      WriteString(' Columbus Day is Monday');
      WriteString(',');
      WriteString(' October ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
  | 17: (* ED -- ELECTION DAY *)
      DD := 8 - SUBTDAYS(1,YEAR);
      WriteString(' Election Day is Tuesday');
      WriteString(',');
      WriteString(' Novenber ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
  | 18: (* TG -- THANKSGIVING *)
      DD := 28 - SUBTDAYS(5,YEAR);
      WriteString(' Thanksgiving Day is Thursday');
      WriteString(',');
      WriteString(' November ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
  ELSE (* INVALID HOLCMD ENTERED *)
    HOLNUM := 0;
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

  STPPRG := FALSE;

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
(* CMDNUM value of 99 is the new calculate cmd, now '=' *)

  DAYNAMES[1] := 'Sunday';
  DAYNAMES[2] := 'Monday';
  DAYNAMES[3] := 'Tuesday';
  DAYNAMES[4] := 'Wednesday';
  DAYNAMES[5] := 'Thursday';       
  DAYNAMES[6] := 'Friday';
  DAYNAMES[7] := 'Saturday';

  LOOP
    WriteString(' Enter Command or H for Help : ');
    ReadString(INBUF.CHARS);
    WriteLn;
    TRIM(INBUF);
    IF INBUF.COUNT = 0 THEN
      WriteString(' Do you wish to quit? : ');
      Read(CH);
      Write(CH);
      WriteLn;
      IF CAP(CH) = 'Y' THEN EXIT; END(*IF*);
    END(*IF*);
    GETCMDNUM(INBUF,CMDNUM);  (*initializes TOKENIZing *)
(*
   If command not found, force invocation of GETRESULT.  Error messages 
   will come from that procedure instead of from this one.
*)
    IF CMDNUM = 0 THEN CMDNUM := 99; END(*IF*);
    CASE CMDNUM OF
      1,3: (* DOW OR GREG COMMANDS *)
        IF DELIMCH = NULL THEN (* No more characters are on the input line *)
          WriteString(' Enter Date (MM/DD/YY) : ');
          ReadString(INBUF.CHARS);
          WriteLn;
          TRIM(INBUF);
        ELSE  (* Get all remaining characters *)
          RMVCHR(INBUF,1,TOKEN.COUNT);
        END(*IF*);
        GETMDY(INBUF,MM,DD,YY);
        IF YY = 0 THEN
          WriteString(' Invalid date.  Command Ignored.');
          WriteLn;
        ELSE  (* NO ERROR *)
          GREG2JUL(MM,DD,YY,JULDATE);
          WriteString(' Date Entered was : ');
          WriteCard(MM,0);
          WriteString('/');
          WriteCard(DD,0);
          WriteString('/');
          WriteCard(YY,0);
          WriteString('       which is a ');
          DOW := Round(AMOD(JULDATE,7.0)) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteLn;
          WriteString(' Julian Date is : ');
          RealToString(JULDATE,1,10,OUTSTR,FINI);
          WriteString(OUTSTR);
          WriteLn;
          PUSHX(JULDATE);
        END(*IF*);
    | 2: (* JUL COMMAND *)
        GETTKNREAL(INBUF,TKNSTATE,L,JULDATE,RETCOD);
        I := L;
        IF RETCOD > 0 THEN
          WriteString(' Enter Julian Date : ');
          ReadReal(JULDATE);
          WriteLn;
        ELSIF INBUF.CHARS[1] = 'X' THEN
          JULDATE := READX();
        END(*IF*);
        JUL2GREG(JULDATE,MM,DD,YY);
        IF YY = 0 THEN
          WriteString(' Julian Date Out of Range.  Command Ignored.');
          WriteLn;
        ELSE (* NO ERROR *)
          WriteString(' Gregorian Date is : ');
          WriteCard(MM,0);
          WriteString('/');
          WriteCard(DD,0);
          WriteString('/');
          WriteCard(YY,0);
          WriteString('       which is a ');
          DOW := Round(AMOD(JULDATE,7.0)) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteLn;
        END(*IF*);
    | 4: (* HOL COMMAND *)
        GETTKN(INBUF,TKNSTATE,L,RETCOD);
        I := L;
        IF RETCOD = 0 THEN
          YEAR := CARDINAL(I);
        ELSE
          WriteString(' Input Year : ');
          ReadCard(YEAR);
          WriteLn;
        END(*IF*);
        IF YEAR < 50 THEN 
          INC(YEAR,2000); 
        ELSIF YEAR < 100 THEN
          INC(YEAR,1900);
        END(*IF*);
        IF (YEAR >= 1900) OR (YEAR <= 2500) THEN
          LOOP (* UNTIL EITHER A QUIT OR A NULL IS ENTERED *)
            WriteString(' Enter 2 Letter Holiday Code or H for Help : ');
            ReadString(INBUF.CHARS);
            WriteLn;
            TRIM(INBUF);
            IF INBUF.COUNT < 1 THEN EXIT; END(*IF*);
            GETCMDNUM(INBUF,HOLNUM);
            IF HOLNUM = 8 THEN 
              EXIT;
            ELSIF (HOLNUM = 5) OR (HOLNUM = 6) OR (HOLNUM = 7) THEN
              WriteString(" WB -- Washington's Birthday");
              WriteLn;
              WriteString(' EA -- Easter');
              WriteLn;
              WriteString(" MD -- Mother's Day");
              WriteLn;
              WriteString(' MM -- Memorial Day');
              WriteLn;
              WriteString(" FD -- Father's Day");
              WriteLn;
              WriteString(' LD -- Labor Day');
              WriteLn;
              WriteString(' CD -- Columbus Day');
              WriteLn;
              WriteString(' ED -- Election Day');
              WriteLn;
              WriteString(' TG -- Thanksgiving Day');
              WriteLn;
              WriteString(' ALL - All of the above');
              WriteLn;
            ELSIF HOLNUM = 19 THEN
              FOR C := 10 TO 18 DO
                HOLCMD(YEAR,C);
              END(*FOR*);
            ELSE
              HOLCMD(YEAR,HOLNUM);
              IF HOLNUM = 0 THEN (* ERROR FROM HOLCMD ROUTINE *)
                WriteString(' Command must be a Holiday command.  Try Again.');
                WriteLn;
              END(*IF*);
            END(*IF*);
          END(*LOOP*);
        END(*IF*);
    | 5,6,7 : (* HELP LIST *)
        WriteString(" DOW -- Return Day Of Week for a Given Date");
        WriteLn;
        WriteString(' JUL -- Enter Julian Date and Return a Gregorian Date');
        WriteLn;
        WriteString(
        " GREG - Enter Gregorian Date and Return a Julian Date (need quote)");
        WriteLn;
        WriteString(' HOL -- Enter Holiday Code for Date of Holiday');
        WriteLn;
        WriteString(" HELP,H,? -- This List");
        WriteLn;
        WriteString(' PR -- Print Entire List of Holidays');
        WriteLn;
        WriteString(' =  -- Forces a noop calculation, but is itself invalid');
        WriteLn;
        WriteString(' Q  -- Quit Command');
        WriteLn;
    | 8 : (* QUIT COMMAND *)
        EXIT;
    | 9 : (* PR COMMAND *)
        GETTKN(INBUF,TKNSTATE,L,RETCOD);
        I := L;
        IF RETCOD = 0 THEN
          YEAR := CARDINAL(I);
        ELSE
          WriteString(' Input Year : ');
          ReadCard(YEAR);
          WriteLn;
        END(*IF*);
        IF YEAR < 50 THEN 
          INC(YEAR,2000); 
        ELSIF YEAR < 100 THEN
          INC(YEAR,1900);
        END(*IF*);
        IF (YEAR >= 1900) OR (YEAR <= 2500) THEN
          WriteString(" New Year's Day is a ");
          GREG2JUL(1,1,YEAR,JULDATE);
          DOW := Round(AMOD(JULDATE,7.)) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteString(' in ');
          WriteCard(YEAR,0);
          WriteLn;
          FOR C := 10 TO 14 DO HOLCMD(YEAR,C); END(*FOR*);
          WriteString(' Independence Day is a ');
          GREG2JUL(7,4,YEAR,JULDATE);
          DOW := Round(AMOD(JULDATE,7.)) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteString(' in ');
          WriteCard(YEAR,0);
          WriteLn;
          FOR C := 15 TO 18 DO HOLCMD(YEAR,C); END(*FOR*);
          WriteString(' Christmas Day is a ');
          GREG2JUL(12,25,YEAR,JULDATE);
          DOW := Round(AMOD(JULDATE,7.)) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteString(' in ');
          WriteCard(YEAR,0);
          WriteLn;
        END(*IF*);
    | 99 : (* Calculate Command *)
         R := GETRESULT(INBUF);
         GETCROPNUM(R,STR1);
         WriteString(STR1);
         WriteLn;
    ELSE  (* Actually should never get here now that cmdnum cannot be 0 *)
      WriteString(' Invalid Command.  Try Again.');
      WriteLn;
    END(*CASE*);
  END(*LOOP*);
END CFNTS.
