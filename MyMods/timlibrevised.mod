IMPLEMENTATION MODULE TIMLIBrevised;

(*
  REVISION HISTORY
  ----------------
  14 Apr 92 -- Created JULIAN and GREGORIAN procs, which are accurate beyond 3/1/2100.
  25 Jul 93 -- Changed GREG2JUL and JUL2GREG limits to those imposed by the
                algorithm, ie, only years <2100 are now allowed.
  25 Jul 94 -- Changed limits of allowed years to 1700 from 1900.
  10 Nov 02 -- Converted to SBM2 Win v4.
  17 May 03 -- First Win32 version.
  26 May 03 -- Adjusted algorithm for Julian fcn so year has a pivot.
  11 Oct 16 -- Conversion to gm2 was a bust.  I'm now backporting code from C++ and go.
                Added DateTimeType to the def module, and GetDateTime proc here.  First
                client is ShowTimer.mod.
  30 Dec 16 -- Added Milisecs to DatetimeType, and now assumes that CARDINAL is 32 bits wide.  Removed the long deprecated JUL2GREG and GREG2JUL.
                 Renamed to TIMLIBrevised.
  24 Apr 19 -- Started adding date and time strings to the DateTimeType.
   5 Aug 19 -- Adding DOW field, and Now, NOW procedures to copy Go.
   5 Dec 20 -- Added sleepsec and sleepmsec
*)

  FROM Strings IMPORT Assign;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM Environment IMPORT GetCommandLine;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETTKN,GETCHR,UNGETCHR;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  IMPORT IOChan, ChanConsts;
  IMPORT RConversions, LongStr, LongConv, LowLong;
  IMPORT SysClock,FormatDT,FormatString;
  IMPORT MiscStdInOut;


(*
CONST
    maxSecondParts      = 999; /*implementation dependent value*/
TYPE
    Month       = [1..12];
    Day         = [1..31];
    Hour        = [0..23];
    Min         = [0..59];
    Sec         = [0..59];
    Fraction    = [0..maxSecondParts];
    UTCDiff     = [-780..720];

    DateTime    =
        RECORD
        year            : CARDINAL;
        month           : Month;
        day             : Day;
        hour            : Hour;
        minute          : Min;
        second          : Sec;
        fractions       : Fraction; /* parts of a second */
        zone            : UTCDiff;  /* Time zone differential factor
                                       which is the number of minutes
                                       to add to local time to obtain
                                       UTC. */
        summerTimeFlag  : BOOLEAN;  /* interpretation depends on local
                                       usage */
        END;

//   This is declared in the def module.
  DateTimeType = RECORD
    M,D,Yr,Hr,Minutes,Seconds,Millisecs, DOW : CARDINAL;
    MonthStr,DayOfWeekStr : STR10TYP;
    DateStr,TimeStr,TimeWithSecondsStr : STRTYP;
    Julian : CARDINAL;
  END;
*)

(*
  {{{
*)
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

(*
  }}}
*)

VAR
  C,K,IDX,PTR,c,RETCOD                         : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG          : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  I,J                                          : INTEGER;
  inputline                                    : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;

TYPE
    ADIPMType = ARRAY [0..11] OF CARDINAL;

CONST
(*
  This is a typed constant that now represents the the last day of the previous month.
  The variable name is an acronym of Accumulated Days In Previous Months.
*)
    ADIPM = ADIPMType{0,31,59,90,120,151,181,212,243,273,304,334};

    FUDGEFACTOR = 5.0;

PROCEDURE TIME2MDY(VAR M,D,Y : CARDINAL);
(*
*********************************** TIME2MDY *************************
System Time To Month, Day, and Year Conversion.
This routine reads the system clock with the GetTime procedure and extracts
the components, putting them into their respective variables.
Used to be needed under Logitech, but not now w/ Stony Brook.
*)


VAR SYSTIME : DateTime;

BEGIN
  IF NOT CanGetClock() THEN
    M := 0;
    D := 0;
    Y := 0;
    RETURN;
  END(*IF*);

  GetClock(SYSTIME);
  WITH SYSTIME DO
    M := month;
    D := day;
    Y := year;
  END (*DO*);

END TIME2MDY;

PROCEDURE MDY2TIME(M,D,Y : CARDINAL);
(*
*********************************** MDY2TIME ************************
Month, Day and Year To System Date Conversion.
This routine sets the system clock with the SetTime procedure and
replaces the date with the M,D,Y parameters.  A fresh call to GetTime
procedure is made as to minimize the replacement of a new time for an
old time effect.

*)

VAR SYSTIME : DateTime;
    K       : CARDINAL;

BEGIN
  IF NOT CanSetClock() THEN RETURN END;
  GetClock(SYSTIME);
  WITH SYSTIME DO
    month := M;
    day := D;
    year := Y;
    SetClock(SYSTIME);
  END

END MDY2TIME;

PROCEDURE MDY2STR(M,D,Y : CARDINAL; VAR MDYSTR : ARRAY OF CHAR);
(*
***************************** MDY2STR *********************************
Month Day Year Cardinals To String.
This routine converts the Month, Day, and Year input cardinals to a string
format.

*)

CONST DATESEPCHAR = '/';

VAR MSTR,DSTR,YSTR : STR10TYP;
    C,STRPTR       : CARDINAL;
    OK             : BOOLEAN;

BEGIN
  OK := CardToStr(M,MSTR);
  OK := CardToStr(D,DSTR);
  OK := CardToStr(Y,YSTR);
  STRPTR := 0; (* NOTE THAT THIS IS A POST-INCREMENTED POINTER *)
  IF MSTR[0] <> BLANK THEN
    MDYSTR[STRPTR] := MSTR[0];
    INC(STRPTR);
  END(*IF*);
  MDYSTR[STRPTR] := MSTR[1];
  INC(STRPTR);
  MDYSTR[STRPTR] := DATESEPCHAR;
  INC(STRPTR);
  IF DSTR[0] <> BLANK THEN
    MDYSTR[STRPTR] := DSTR[0];
    INC(STRPTR);
  END(*IF*);
  MDYSTR[STRPTR] := DSTR[1];
  INC(STRPTR);
  MDYSTR[STRPTR] := DATESEPCHAR;
  INC(STRPTR);
  FOR C := 0 TO 3 DO
    MDYSTR[STRPTR] := YSTR[C];
    INC(STRPTR);
  END(*FOR*);
  MDYSTR[STRPTR] := NULL;    (* NULL TERMINATE STRING *)
END MDY2STR;

(*
{{{
new fields in the TimeDateType record/struct -- DateStr,TimeStr,TimeWithSecondsStr : STRTYP;
PROCEDURE DateTimeToString(dt : DateTime; VAR OUT date : ARRAY OF CHAR; VAR OUT time : ARRAY OF CHAR);
PROCEDURE FormatString(formatStr : ARRAY OF CHAR; VAR OUT destStr : ARRAY OF CHAR) : BOOLEAN;
destStr can be the same string as formatStr.
this procedure takes a variable number of parameters
to accomodate the contents of the format string
}}}
*)

PROCEDURE GetDateTime(VAR dt : DateTimeType) : DateTimeType;  (* C does this a lot, returning , not sure why *)
VAR
  DT : DateTimeType;
  SysTime : DateTime;
  AMpm : STRTYP;

BEGIN
  IF NOT CanGetClock() THEN
    WITH DT DO
      M := 0;
      D := 0;
      Yr := 0;
      Hr := 0;
      Minutes := 0;
      Seconds := 0;
      Millisecs := 0;
    END; (* WITH SysTime *)
    dt := DT;
    RETURN dt;
  END(*IF*);

  GetClock(SysTime);
  WITH DT DO
    M := SysTime.month;
    D := SysTime.day;
    Yr := SysTime.year;
    Hr := SysTime.hour;
    Minutes := SysTime.minute;
    Seconds := SysTime.second;
    Millisecs := SysTime.fractions;
    Julian := JULIAN(M,D,Yr);
    DOW := Julian MOD 7 + 1;
    Assign(MONTHNAMES[M],MonthStr);
    Assign(DAYNAMES[DOW],DayOfWeekStr);
    FormatDT.DateTimeToString(SysTime,DateStr,TimeStr);
    IF Hr >= 12 THEN
      Hr := Hr - 12;
      AMpm := "PM";
    ELSE
      AMpm := "AM";
    END;
    IF Hr < 1 THEN  (* So 0: Hours reads as 12 o'clock.  Must do this after assigning AM or PM, so 12 AM is not shown as 12 PM. *)
      Hr := 12;
    END;
    FormatString.FormatString("%c:%c:%c %s",TimeWithSecondsStr,Hr,Minutes,Seconds,AMpm);
  END (* With DT *);
  dt := DT;
  RETURN dt;
END GetDateTime;


PROCEDURE Now() : DateTimeType;
VAR D1 : DateTimeType;

BEGIN
  GetDateTime(D1);
  RETURN D1;
END Now;

PROCEDURE NOW() : DateTimeType;
VAR D1, D2 : DateTimeType;

BEGIN
  D1 := GetDateTime(D2);
  RETURN D1;
END NOW;

PROCEDURE GETMDY(INBUF : BUFTYP; VAR M,D,Y : CARDINAL);
(*
********************************** GETMDY *****************************
GET MONTH DAY YEAR.
This procedure parses the input string into the month, day and year.  Any
delimiters can be used because the parsing process uses a finite state
automata algorithm (part of the TOKENIZE module) to parse the input line.
If an error occurs, then Y is returned as zero.

*)
VAR TOKEN             : BUFTYP;
    TKNSTATE,CHRSTATE : FSATYP;
    C,RETCOD          : CARDINAL;
    ERRFLG            : BOOLEAN;
    SUM               : INTEGER;
    SUMARRAY          : ARRAY [1..3] OF INTEGER;
    CH                : CHAR;

BEGIN
  INI1TKN(INBUF);
  C := 1;
  LOOP
    GETTKN(TOKEN,TKNSTATE,SUM,RETCOD);
(*
  PARSE BY ONLY RETAINING THE DIGIT TOKENS (TKNSTATE=DGT) AND DISCARDING
  THE REST
*)
    ERRFLG := (SUM < 0) OR (RETCOD = 1) OR (TKNSTATE <> DGT);
    IF ERRFLG THEN EXIT; END(*IF*);
    SUMARRAY[C] := SUM;
    INC(C);
    IF C > 3 THEN EXIT; END(*IF*);
    REPEAT (* DISCARD CHARS UNTIL A DIGIT IS FOUND *)
      GETCHR(CH,CHRSTATE,RETCOD);
    UNTIL (CHRSTATE = DGT) OR (RETCOD <> 0);
    IF RETCOD <> 0 THEN EXIT; END(*IF*);
    UNGETCHR(RETCOD);
    IF RETCOD <> 0 THEN EXIT; END(*IF*);
  END(*LOOP*);

  IF ERRFLG OR (RETCOD <> 0) THEN
(* UNEXPECTED ERROR OCCURRED.  ABORT COMMAND *)
    WriteString(' Invalid Input.  Operation Ignored.');
    WriteLn;
    Y := 0;
    RETURN;
  END(*IF*);
  M := (SUMARRAY[1]);
  D := (SUMARRAY[2]);
  Y := (SUMARRAY[3]);
(*
  ADJUST FOR WHEN ONLY THE LAST 2 DIGITS OF THE YEAR WERE ENTERED, AND
  VALIDATE INPUT.
*)
  IF Y < 50 THEN
    INC(Y,2000);
  ELSIF Y < 100 THEN
    INC(Y,1900);
  ELSIF Y < 1900 THEN
(* ERROR *)
    Y := 0;
  ELSIF Y > 2500 THEN
(* ERROR *)
    Y := 0;
  END(*IF*);
  IF (M < 1) OR (M > 12) THEN
(* ERROR *)
    Y := 0;
  END(*IF*);
  IF (D < 1) OR (D > 31) THEN
(* ERROR *)
    Y := 0;
  END(*IF*);
END GETMDY;

PROCEDURE JULIAN(M,D,Y : CARDINAL) : CARDINAL;

VAR
  M0,D0,Y0,Juldate   : CARDINAL;

BEGIN
  IF Y < 30 THEN
    INC(Y,2000);
  ELSIF Y < 100 THEN
    INC(Y,1900);
  END(*IF*);
  IF (M < 1) OR (M > 12) OR (D < 1) OR (D > 31) OR (Y < 1700) OR (Y > 2500)
                                                                        THEN
(* Month, Day or Year is out of range *)
    Juldate := 0;
    RETURN(Juldate);
  END(*IF*);

  M0 := M - 1;
  Y0 := Y - 1;

  Juldate :=  Y0*365     (* Number of days in previous normal years *)
            + Y0 DIV 4   (* Number of possible leap days *)
            - Y0 DIV 100 (* Subtract all century years *)
            + Y0 DIV 400 (* Add back the true leap century years *)
            + ADIPM[M0] + D;
  IF ((( Y MOD 4 = 0) AND ( Y MOD 100 <> 0)) OR ( Y MOD 400 = 0)) AND
(*   123            3     3               32    2              21      *)
                                                                 (M > 2) THEN
    INC(Juldate);
  END(*IF*);
  RETURN Juldate;
END JULIAN;

PROCEDURE GREGORIAN(Juldate : CARDINAL; VAR M,D,Y : CARDINAL);

VAR Y0   : CARDINAL;
    M0   : [1..12];
    D0   : [1..31];
    L,JD : CARDINAL;

BEGIN
  Y0 := Juldate DIV 365;
  M0 := 1;
  D0 := 1;

  WHILE JULIAN(M0,D0,Y0) > Juldate DO DEC(Y0); END(*WHILE*);

  M0 := 12;
  WHILE JULIAN(M0,D0,Y0) > Juldate DO DEC(M0); END(*WHILE*);

  WHILE JULIAN(M0,D0,Y0) < Juldate DO INC(D0); END(*WHILE*);

  M := M0;
  D := D0;
  Y := Y0;
END GREGORIAN;

PROCEDURE Sleepmsec(t : CARDINAL);
  (* time is in milliseconds *)
  (*
    DateTimeType = RECORD
      M,D,Yr,Hr,Minutes,Seconds,Millisecs, DOW : CARDINAL;
      MonthStr,DayOfWeekStr : STR10TYP;
      DateStr,TimeStr,TimeWithSecondsStr : STRTYP;
      Julian : CARDINAL;
  END;
  *)

  VAR
    now, t0 : DateTimeType;
    msec, target, then : LONGCARD;

  BEGIN
    t0 := Now();
    msec := (t0.Hr * 3600 + t0.Minutes * 60 + t0.Seconds) * 1000 + t0.Millisecs;
    target := msec + VAL(LONGCARD,t);
    (*
    MiscStdInOut.WriteLn;
    MiscStdInOut.WriteString("msec= ");
    MiscStdInOut.WriteLongCard(msec);
    MiscStdInOut.WriteString(".    target= ");
    MiscStdInOut.WriteLongCard(target);
    MiscStdInOut.WriteLn;
    *)
    REPEAT
      now := NOW();
      then := (now.Hr*3600 + now.Minutes*60 + now.Seconds)*1000 + now.Millisecs;
      (*
      MiscStdInOut.WriteLn;
      MiscStdInOut.WriteString(" then= ");
      MiscStdInOut.WriteLongCard(then);
      MiscStdInOut.WriteLn;
      *)
    UNTIL then >= target;
END Sleepmsec;

PROCEDURE Sleepsec(t : CARDINAL);
  (* time is in seconds *)
  (*
    DateTimeType = RECORD
      M,D,Yr,Hr,Minutes,Seconds,Millisecs, DOW : CARDINAL;
      MonthStr,DayOfWeekStr : STR10TYP;
      DateStr,TimeStr,TimeWithSecondsStr : STRTYP;
      Julian : CARDINAL;
  END;
  *)

  VAR
    now, t0 : DateTimeType;
    sec, target, then : LONGCARD;

  BEGIN
    t0 := NOW();
    sec := t0.Hr * 3600 + t0.Minutes * 60 + t0.Seconds;
    IF t0.Millisecs >= 500 THEN
      INC(sec);
    END;
    target := sec + VAL(LONGCARD,t);
    (*
    MiscStdInOut.WriteLn;
    MiscStdInOut.WriteString("sec= ");
    MiscStdInOut.WriteLongCard(sec);
    MiscStdInOut.WriteString(".    target= ");
    MiscStdInOut.WriteLongCard(target);
    MiscStdInOut.WriteLn;
    *)
    REPEAT
      now := NOW();
      then := (now.Hr*3600 + now.Minutes*60 + now.Seconds);
      (*
      MiscStdInOut.WriteLn;
      MiscStdInOut.WriteString(" then= ");
      MiscStdInOut.WriteLongCard(then);
      MiscStdInOut.WriteLn;
        *)
    UNTIL then >= target;
END Sleepsec;





(* Old method of declaring and initializing the DAYNAMES array.
BEGIN (******************************** MAIN ****************************)
  DAYNAMES[1] := "Sunday";
  DAYNAMES[2] := "Monday";
  DAYNAMES[3] := "Tuesday";
  DAYNAMES[4] := "Wednesday";
  DAYNAMES[5] := "Thursday";
  DAYNAMES[6] := "Friday";
  DAYNAMES[7] := "Saturday";
*)
END TIMLIBrevised.
