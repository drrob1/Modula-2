MODULE CFNTSW;
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
  23 May 03 -- Fixed bug when run two PR cmds in one execution of pgm, second file open fails.
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
    SCANBACK,SCANFWD,COPYLEFT,RMVCHR,ASSIGN2BUF;
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
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY,GREG2JUL,JUL2GREG,GETMDY;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;

CONST NUMOFCMDS   = 20;
      HASHCONST   = 43;
      CALCCMD     = '=';
TYPE
    ChanId      = IOChan.ChanId;
    FlagSet     = ChanConsts.FlagSet;
    OpenResults = ChanConsts.OpenResults;

  (* Accepted singleton values of FlagSet *)

CONST
  read  = FlagSet{ChanConsts.readFlag}; (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag};(* output operations are requested/available *)
  text  = FlagSet{ChanConsts.textFlag}; (* text operations are requested/available *)
  raw   = FlagSet{ChanConsts.rawFlag};  (* raw operations are requested/available *)
  echo  = FlagSet{ChanConsts.echoFlag}; (* echoing by interactive device on reading of characters from input stream requested/applies *)

  VAR
    FINI,GOOD,ok,OK,WR2FILE                             : BOOLEAN;
    C,K,RETCOD,RETCOD2,CMDNUM,MM,DD,YY,YEAR,DOW,HOLNUM,
    STRLEN,NON0POSN,NONBLPOSN                           : CARDINAL;
    I,J,SUM                                             : INTEGER;
    L,LJULDATE                                                   : LONGINT;
    CH                                                  : CHAR;
    STPPRG,HAVEMATCH                                    : BOOLEAN;
    PROMPT,NAMDFT,TYPDFT,OUTFNAM,INBUF,YRBUF,TOKEN     : BUFTYP;
    X,Y,Z,JULDATE,R                                     : LONGREAL;
    OUTSTR,STR1,STR2,STR3,YEARSTR                       : STRTYP;
    TKNSTATE,CHRSTATE                                   : FSATYP;
    CMDNAM                          : ARRAY [1..NUMOFCMDS] OF STR10TYP;
    HASHARRAY                       : ARRAY [0..HASHCONST+10] OF CARDINAL;
    DAYNAMES                        : ARRAY [1..7] OF STR10TYP;
    INUNT1,OUTUN1                   : MYFILTYP;
    inputline                       : ARRAY [0..255] OF CHAR;

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
(*  old way GREG2JUL(MM,DD,YEAR,JULDATE); *)
  WHILE (JULDATE MOD 7) <> 0 DO
    INC(JULDATE);
  END (*WHILE*);
  GREGORIAN(JULDATE,MM,DD,IGNORE);
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
      LJULDATE := JULIAN(2,DD,YEAR);
(*      GREG2JUL(2,DD,YEAR,JULDATE); *)
      DOW := (LJULDATE MOD 7) + 1;
      WriteString(DAYNAMES[DOW]);
      WriteString(',');
      WriteString(' Feb ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
      IF WR2FILE THEN
        FWRSTR(OUTUN1," Washington's Birthday is ");
        FWRSTR(OUTUN1,DAYNAMES[DOW]);
        FWRSTR(OUTUN1,',');
        FWRSTR(OUTUN1,' Feb ');
        ok := CardToStr(DD,STR1);
        FWRSTR(OUTUN1,STR1);
        FWRSTR(OUTUN1,', ');
        FWRTXLN(OUTUN1,YRBUF);
      END(*IF*);
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
      WriteCard(MM,2);
      WriteString('/');
      WriteCard(DD,2);
      WriteString('/');
      WriteCard(YEAR,0);
      WriteLn;
      IF WR2FILE THEN
        FWRSTR(OUTUN1," Easter Sunday is ");
        ok := CardToStr(MM,STR1);
        FWRSTR(OUTUN1,STR1);
        FWRSTR(OUTUN1,'/');
        ok := CardToStr(DD,STR2);
        FWRSTR(OUTUN1,STR2);
        FWRSTR(OUTUN1,'/');
        FWRTXLN(OUTUN1,YRBUF);
      END(*IF*);
  | 12: (* MD -- MOTHER'S DAY *)
      DD := 14 - SUBTDAYS(0,YEAR);
      WriteString(" Mother's Day is Sunday");
      WriteString(',');
      WriteString(' May ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
      IF WR2FILE THEN
        FWRSTR(OUTUN1," Mother's Day is Sunday, ");
        FWRSTR(OUTUN1,'May ');
        ok := CardToStr(DD,STR2);
        FWRSTR(OUTUN1,STR2);
        FWRSTR(OUTUN1,', ');
        FWRTXLN(OUTUN1,YRBUF);
      END(*IF*);
  | 13: (* MM -- MEMORIAL DAY *)
      DD := 31 - SUBTDAYS(2,YEAR);
      WriteString(' Memorial Day is Monday');
      WriteString(',');
      WriteString(' May ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
      IF WR2FILE THEN
        FWRSTR(OUTUN1," Memorial Day is Monday, ");
        FWRSTR(OUTUN1,'May ');
        ok := CardToStr(DD,STR2);
        FWRSTR(OUTUN1,STR2);
        FWRSTR(OUTUN1,', ');
        FWRTXLN(OUTUN1,YRBUF);
      END(*IF*);
  | 14: (* FD -- FATHER'S DAY *)
      DD := 21 - SUBTDAYS(3,YEAR);
      WriteString(" Father's Day is Sunday");
      WriteString(',');
      WriteString(' June ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
      IF WR2FILE THEN
        FWRSTR(OUTUN1," Father's Day is Sunday, ");
        FWRSTR(OUTUN1,'June ');
        ok := CardToStr(DD,STR2);
        FWRSTR(OUTUN1,STR2);
        FWRSTR(OUTUN1,', ');
        FWRTXLN(OUTUN1,YRBUF);
      END(*IF*);
  | 15: (* LD -- LABOR DAY *)
      DD := 7 - SUBTDAYS(3,YEAR);
      WriteString(' Labor Day is Monday');
      WriteString(',');
      WriteString(' September ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
      IF WR2FILE THEN
        FWRSTR(OUTUN1," Labor Day is Monday, ");
        FWRSTR(OUTUN1,'September ');
        ok := CardToStr(DD,STR2);
        FWRSTR(OUTUN1,STR2);
        FWRSTR(OUTUN1,', ');
        FWRTXLN(OUTUN1,YRBUF);
      END(*IF*);
  | 16: (* CD -- COLUMBUS DAY *)
      DD := 14 - SUBTDAYS(5,YEAR);
      WriteString(' Columbus Day is Monday');
      WriteString(',');
      WriteString(' October ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
      IF WR2FILE THEN
        FWRSTR(OUTUN1," Columbus Day is Monday, ");
        FWRSTR(OUTUN1,'October ');
        ok := CardToStr(DD,STR2);
        FWRSTR(OUTUN1,STR2);
        FWRSTR(OUTUN1,', ');
        FWRTXLN(OUTUN1,YRBUF);
      END(*IF*);
  | 17: (* ED -- ELECTION DAY *)
      DD := 8 - SUBTDAYS(1,YEAR);
      WriteString(' Election Day is Tuesday');
      WriteString(',');
      WriteString(' Novenber ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
      IF WR2FILE THEN
        FWRSTR(OUTUN1," Election Day is Tuesday, ");
        FWRSTR(OUTUN1,'November ');
        ok := CardToStr(DD,STR2);
        FWRSTR(OUTUN1,STR2);
        FWRSTR(OUTUN1,', ');
        FWRTXLN(OUTUN1,YRBUF);
      END(*IF*);
  | 18: (* TG -- THANKSGIVING *)
      DD := 28 - SUBTDAYS(5,YEAR);
      WriteString(' Thanksgiving Day is Thursday');
      WriteString(',');
      WriteString(' November ');
      WriteCard(DD,0);
      WriteString(', ');
      WriteCard(YEAR,0);
      WriteLn;
      IF WR2FILE THEN
        FWRSTR(OUTUN1," Thanksgiving Day is Thursday, ");
        FWRSTR(OUTUN1,'November ');
        ok := CardToStr(DD,STR2);
        FWRSTR(OUTUN1,STR2);
        FWRSTR(OUTUN1,', ');
        FWRTXLN(OUTUN1,YRBUF);
      END(*IF*);
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

  WR2FILE := FALSE;
  LOOP
    WriteString(' Enter Command or H for Help : ');
    ReadString(INBUF.CHARS);
    SkipLine;
    WriteLn;
    TRIM(INBUF);
    IF INBUF.COUNT = 0 THEN
      WriteString(' Do you wish to quit? : ');
      ReadChar(CH);
      WriteChar(CH);
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
          SkipLine;
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
          LJULDATE := JULIAN(MM,DD,YY);
          WriteString(' Date Entered was : ');
          WriteCard(MM,0);
          WriteString('/');
          WriteCard(DD,0);
          WriteString('/');
          WriteCard(YY,0);
          WriteString('       which is a ');
          DOW := (LJULDATE MOD 7) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteLn;
          WriteString(' Julian Date is : ');
(*
          RealToString(JULDATE,1,10,OUTSTR,FINI);
          WriteString(OUTSTR);
*)
          WriteCard(LJULDATE,0);
          WriteLn;
          PUSHX(LFLOAT(LJULDATE));
        END(*IF*);
    | 2: (* JUL COMMAND *)
        WriteString(' Enter Julian Date : ');
        ReadLongInt(LJULDATE);
        SkipLine;
        WriteLn;
        GREGORIAN(LJULDATE,MM,DD,YY);
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
          DOW := (LJULDATE MOD 7) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteLn;
        END(*IF*);
    | 4: (* HOL COMMAND *)
        GETTKN(INBUF,TKNSTATE,L,RETCOD);
        I := L;
        IF RETCOD = 0 THEN
          YEAR := ORD(I);
        ELSE
          WriteString(' Input Year : ');
          ReadCard(YEAR);
          SkipLine;
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
            SkipLine;
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
(*        I := L; *)
        IF RETCOD = 0 THEN
          YEAR := ORD(L);
        ELSE
          WriteString(' Input Year : ');
          ReadCard(YEAR);
          SkipLine;
          WriteLn;
        END(*IF*);
        IF YEAR < 50 THEN
          INC(YEAR,2000);
        ELSIF YEAR < 100 THEN
          INC(YEAR,1900);
        END(*IF*);
        IF NOT WR2FILE THEN  (* So as to not open a file when one is already open *)
          WR2FILE := TRUE;
          ASSIGN2BUF('ENTER OUTPUT FILE NAME : ',PROMPT);
          ASSIGN2BUF('cfntsout.TXT',NAMDFT);
          ASSIGN2BUF('.TXT',TYPDFT);
          GETFNM(PROMPT,NAMDFT,TYPDFT,OUTFNAM);
          WriteString('  OUTPUT FILE : ');
          WriteString(OUTFNAM.CHARS);
          WriteLn;
          FRESET(OUTUN1,OUTFNAM,WR);
        END (*IF*);

        IF (YEAR >= 1900) OR (YEAR <= 2500) THEN
          WriteString(" New Year's Day is a ");
(*          GREG2JUL(1,1,YEAR,JULDATE); *)
          LJULDATE := JULIAN(1,1,YEAR);
          DOW := (LJULDATE MOD 7) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteString(' in ');
          WriteCard(YEAR,0);
          WriteLn;
          FWRSTR(OUTUN1," New Year's Day is a ");
          FWRSTR(OUTUN1,DAYNAMES[DOW]);
          FWRSTR(OUTUN1,' in ');
          ok := CardToStr(YEAR,YEARSTR);
          ASSIGN2BUF(YEARSTR,YRBUF);
          FWRTXLN(OUTUN1,YRBUF);

          FOR C := 10 TO 14 DO HOLCMD(YEAR,C); END(*FOR*);
          WriteString(' Independence Day is a ');
(*          GREG2JUL(7,4,YEAR,JULDATE); *)
          LJULDATE := JULIAN(7,4,YEAR);
          DOW := (LJULDATE MOD 7) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteString(' in ');
          WriteCard(YEAR,0);
          WriteLn;
          FWRSTR(OUTUN1,' Independence Day is a ');
          FWRSTR(OUTUN1,DAYNAMES[DOW]);
          FWRSTR(OUTUN1,' in ');
          FWRTXLN(OUTUN1,YRBUF);

          FOR C := 15 TO 18 DO HOLCMD(YEAR,C); END(*FOR*);
          WriteString(' Christmas Day is a ');
(*          GREG2JUL(12,25,YEAR,JULDATE); *)
          LJULDATE := JULIAN(12,25,YEAR);
          DOW := (LJULDATE MOD 7) + 1;
          WriteString(DAYNAMES[DOW]);
          WriteString(' in ');
          WriteCard(YEAR,0);
          WriteLn;
          FWRSTR(OUTUN1,' Christmas Day is a ');
          FWRSTR(OUTUN1,DAYNAMES[DOW]);
          FWRSTR(OUTUN1,' in ');
          FWRTXLN(OUTUN1,YRBUF);
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
  IF WR2FILE THEN FCLOSE(OUTUN1); END;
END CFNTSW.
