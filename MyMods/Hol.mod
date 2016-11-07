<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:Holidays.RES*>
%ELSE
%END

MODULE Hol;
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
  24 Jan 05 -- Added icon
   8 May 07 -- Made HolMod and Hol.  Hol only writes 1 year to the screen.
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
  FROM HolMod IMPORT HOLCMD,EASTER;

CONST NUMOFCMDS   = 20;
      HASHCONST   = 43;
      CALCCMD     = '=';
      HolidayIcon = '#100';

  VAR
    FINI,GOOD,ok,OK,WR2FILE                             : BOOLEAN;
    C,K,RETCOD,RETCOD2,CMDNUM,MM,DD,YY,YEAR,DOW,HOLNUM,CurrentYear,HolDay,
    STRLEN,NON0POSN,NONBLPOSN                           : CARDINAL;
    I,J,SUM                                             : INTEGER;
    L,LJULDATE                                          : LONGINT;
    CH                                                  : CHAR;
    STPPRG,HAVEMATCH                                    : BOOLEAN;
    PROMPT,NAMDFT,TYPDFT,OUTFNAM,INBUF,YRBUF,TOKEN      : BUFTYP;
    X,Y,Z,JULDATE,R                                     : LONGREAL;
    OUTSTR,STR1,STR2,STR3,YEARSTR                       : STRTYP;
    TKNSTATE,CHRSTATE                                   : FSATYP;
    CMDNAM                          : ARRAY [1..NUMOFCMDS] OF STR10TYP;
    HASHARRAY                       : ARRAY [0..HASHCONST+10] OF CARDINAL;
    DAYNAMES                        : ARRAY [1..7] OF STR10TYP;
    INUNT1,OUTUN1                   : MYFILTYP;
    inputline                       : ARRAY [0..255] OF CHAR;
    tpv                             : TKNPTRTYP;
    YearConvRslt                    : ConvResults;

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

  LOOP
    GetCommandLine(inputline);
    IF LENGTH(inputline) > 0 THEN
      STPPRG := TRUE;
      WriteString(" inputline:");
      WriteString(inputline);
      WriteLn;
      
      StrToCard(inputline,YEAR,YearConvRslt);
      WriteString(' year=');
      WriteCard(YEAR);
      IF YearConvRslt <> strAllRight THEN
        WriteString(' Do you wish to quit? : ');
        Terminal.Read(CH);
        Terminal.Write(CH);
        WriteLn;
        IF CAP(CH) = 'Y' THEN EXIT; END(*IF*);
      END(*IF*);
    ELSE  (* length = 0 *)
      TIME2MDY(MM,DD,YEAR);
      ok := BasicDialogs.PromptCard('Year:',0,3000,TRUE,YEAR);  (*prompt,min,max,allowZero,response:boolean*)
      IF NOT ok THEN EXIT END;
    END(*if length*);
    IF YEAR < 50 THEN
      INC(YEAR,2000);
    ELSIF YEAR < 100 THEN
      INC(YEAR,1900);
    END(*IF*);
    CardToStr(YEAR,YEARSTR);

    IF (YEAR >= 1900) OR (YEAR <= 2500) THEN
      WriteString(" New Year's Day is a ");
      LJULDATE := JULIAN(1,1,YEAR);
      DOW := (LJULDATE MOD 7) + 1;
      WriteString(DAYNAMES[DOW]);
      WriteString(' in ');
      WriteCard(YEAR);
      WriteLn;

      HOLCMD(YEAR,10,DD);
      WriteString(" Washington's Birthday is ");
      LJULDATE := JULIAN(2,DD,YEAR);
      DOW := (LJULDATE MOD 7) + 1;
      WriteString(DAYNAMES[DOW]);
      WriteString(',');
      WriteString(' Feb ');
      WriteCard(DD);
      WriteString(', ');
      WriteCard(YEAR);
      WriteLn;
      
      EASTER(YEAR,MM,DD);
      WriteString(' Easter Sunday is : ');
      WriteCard(MM);
      WriteString('/');
      WriteCard(DD);
      WriteString('/');
      WriteCard(YEAR);
      WriteLn;

      HOLCMD(YEAR,12,DD);
      WriteString(" Mother's Day is Sunday");
      WriteString(',');
      WriteString(' May ');
      WriteCard(DD);
      WriteString(', ');
      WriteCard(YEAR);
      WriteLn;
      
      HOLCMD(YEAR,13,DD);
      WriteString(' Memorial Day is Monday');
      WriteString(',');
      WriteString(' May ');
      WriteCard(DD);
      WriteString(', ');
      WriteCard(YEAR);
      WriteLn;

      HOLCMD(YEAR,14,DD);
      WriteString(" Father's Day is Sunday");
      WriteString(',');
      WriteString(' June ');
      WriteCard(DD);
      WriteString(', ');
      WriteCard(YEAR);
      WriteLn;

      WriteString(' Independence Day is a ');
      LJULDATE := JULIAN(7,4,YEAR);
      DOW := (LJULDATE MOD 7) + 1;
      WriteString(DAYNAMES[DOW]);
      WriteString(' in ');
      WriteCard(YEAR);
      WriteLn;

      HOLCMD(YEAR,15,DD);
      WriteString(' Labor Day is Monday');
      WriteString(',');
      WriteString(' September ');
      WriteCard(DD);
      WriteString(', ');
      WriteCard(YEAR);
      WriteLn;

      HOLCMD(YEAR,16,DD);
      WriteString(' Columbus Day is Monday');
      WriteString(',');
      WriteString(' October ');
      WriteCard(DD);
      WriteString(', ');
      WriteCard(YEAR);
      WriteLn;

      HOLCMD(YEAR,17,DD);
      WriteString(' Election Day is Tuesday');
      WriteString(',');
      WriteString(' Novenber ');
      WriteCard(DD);
      WriteString(', ');
      WriteCard(YEAR);
      WriteLn;

      HOLCMD(YEAR,18,DD);
      WriteString(' Thanksgiving Day is Thursday');
      WriteString(',');
      WriteString(' November ');
      WriteCard(DD);
      WriteString(', ');
      WriteCard(YEAR);
      WriteLn;

      WriteString(' Christmas Day is a ');
      LJULDATE := JULIAN(12,25,YEAR);
      DOW := (LJULDATE MOD 7) + 1;
      WriteString(DAYNAMES[DOW]);
      WriteString(' in ');
      WriteCard(YEAR);
      WriteLn;

    END(* IF YEAR in range *);
  EXIT;
  END(*LOOP*);
  PressAnyKey;
END Hol.
