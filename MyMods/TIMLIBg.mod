IMPLEMENTATION MODULE TIMLIBg;

(*
  REVISION HISTORY
  ----------------
  14 Apr 92 -- Created JULIAN and GREGORIAN procs, which are accurate beyond
                3/1/2100.
  25 Jul 93 -- Changed GREG2JUL and JUL2GREG limits to those imposed by the
                algorithm, ie, only years <2100 are now allowed.
  25 Jul 94 -- Changed limits of allowed years to 1700 from 1900.
  10 Nov 02 -- Converted to SBM2 Win v4.
  17 May 03 -- First Win32 version.
  26 May 03 -- Adjusted algorithm for Julian fcn so year has a pivot.
   6 Oct 13 -- Converted to gm2, and changed its name.
*)
  IMPORT ASCII, StdIO, LowLong;
  FROM WholeStr IMPORT CardToStr;
  FROM LowLong IMPORT round;
  FROM Environg IMPORT GetCommandLine;
  FROM UTILLIBg IMPORT NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM;
  FROM TKNRTNSg IMPORT FSATYP,DELIMCH,INI1TKN,GETTKN,GETCHR,UNGETCHR;
  FROM REALLIBg IMPORT AINT,AMOD,ROUND;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM MiscStdInOutg IMPORT WriteCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
    WriteLongReal, WriteLongRealFixed, ReadString, ReadCard, ReadLongReal;


VAR
  C,K,IDX,PTR,c,RETCOD                         : CARDINAL;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG          : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  I,J                                          : INTEGER;
  inputline                                    : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;

TYPE
    ADIPMType = ARRAY [0..11] OF INTEGER;
CONST
(*
  This is a typed constant that represents the difference btwn the last day
  of the previous month and 30, assuming each month was 30 days long.
  The variable name is an acronym of Accumulated Days In Previous Months.
*)
    ADIPM = ADIPMType{0,1,-1,0,0,1,1,2,3,3,4,4};
    FUDGEFACTOR = 5.0;

(* Old method of declaring and initializing the DAYNAMES array.  It is now in def module.
VAR
    DAYNAMES : ARRAY[1..7] OF STR10TYP;
*)

PROCEDURE GETNEWDATE(VAR M,D,Y : CARDINAL);
(*
****************************** GETNEWDATE *******************************
This subroutine does what its name says, gets the new system date by
allowing for date arithmetic relative to the previous system date.

*)
VAR HAPPY      : BOOLEAN;
    DOW,RETCOD : CARDINAL;
    JULDATE    : LONGREAL;
    ANS,TOKEN  : BUFTYP;
    INTVAL     : INTEGER;
    LI         : LONGINT;
    DATESTR    : STRTYP;
    TKNSTATE   : FSATYP;

BEGIN
  HAPPY := FALSE;
  TIME2MDY(M,D,Y);
  REPEAT (* UNTIL Happy *)
    MDY2STR(M,D,Y,DATESTR);
    GREG2JUL(M,D,Y,JULDATE);
    WriteString( ' Current Day and Date is ');
    DOW := ROUND(AMOD(JULDATE,7.0)) + 1;
    WriteString(DAYNAMES[DOW]);
    WriteString(', ');
    WriteString(DATESTR);
    WriteString('.  Enter relative day # or');
    WriteLn;
    WriteString('a complete date : ');
    ReadString(ANS.CHARS);
    WriteLn;
    TRIM(ANS);
    INI1TKN(ANS);
    GETTKN(TOKEN,TKNSTATE,LI,RETCOD);
    IF (TKNSTATE = ALLELSE) AND (TOKEN.CHARS[1] = 'N') THEN
      REPEAT
        WriteString(' Enter correct date for the study : ');
        ReadString(ANS.CHARS);
        TRIM(ANS);
        GETMDY(ANS,M,D,Y);
      UNTIL Y > 0;
    ELSIF TKNSTATE = DGT THEN
      IF DELIMCH = NULL THEN (* Have EOL *)
        GREG2JUL(M,D,Y,JULDATE);
        JULDATE := JULDATE + LFLOAT(LI);
        JUL2GREG(JULDATE,M,D,Y);
      ELSE (* more than 1 token is on the input line *)
        GETMDY(ANS,M,D,Y);
      END(*IF*);
    ELSE
      HAPPY := TRUE;
    END(*IF*);
  UNTIL HAPPY;
END GETNEWDATE;

PROCEDURE TIME2MDY(VAR M,D,Y : CARDINAL);
(*
*********************************** TIME2MDY *************************
System Time To Month, Day, and Year Conversion.
This routine reads the system clock with the GetTime procedure and extracts
the components, putting them into their respective variables.
Used to be needed under Logitech, but not now w/ Stony Brook.
*)

CONST DMASK = BITSET{0,1,2,3,4};
      MMASK = BITSET{5,6,7,8};
      YMASK = BITSET{9,10,11,12,13,14,15};

VAR SYSTIME : DateTime;
(*    C,K     : CARDINAL; *)

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
    INC(Y,1900); (* Seems that this function returns number of years since 1900 *) 
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
  CardToStr(M,MSTR);
  CardToStr(D,DSTR);
  CardToStr(Y,YSTR);
  STRPTR := 0; (* NOTE THAT THIS IS A POST-INCREMENTED POINTER *)
  IF MSTR[0] <> ASCII.sp THEN
    MDYSTR[STRPTR] := MSTR[0];
    INC(STRPTR);
  END(*IF*);
  MDYSTR[STRPTR] := MSTR[1];
  INC(STRPTR);
  MDYSTR[STRPTR] := DATESEPCHAR;
  INC(STRPTR);
  IF DSTR[0] <> ASCII.sp THEN
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

PROCEDURE GREG2JUL(M,D,Y : CARDINAL; VAR JULDATE : LONGREAL);
(*
******************************* GREG2JUL *********************************
Gregorian To Julian Date Conversion Routine.
This routine computes the Julian Date from the standard calendar date
format.  The formula is J := [365.25 Y0] + [30.6001 M0] + D + 1,720,982.
Y0 and M0 are the modified year and month, to account for the changes
imposed by leap years.
Reals must be used because the size and precision required to manipulate the
Julian dates exceeds that of cards or ints.  Longcards would obviate this
problem, but V2.0 of Logitech's Mod-2 compiler does not support them.

If the result returned is zero, one of the input params to the subroutine
was out of range.

INPUT FROM GLOBAL VAR'S : FUDGEFACTOR.

*)

VAR M0,D0,Y0 : LONGREAL;

BEGIN
  IF (M < 1) OR (M > 12) OR (D < 1) OR (D > 31) OR (Y < 1900) OR (Y >= 2100)
                                                                        THEN
(* MONTH, DAY OR YEAR IS OUT OF RANGE *)
    JULDATE := 0.0;
    RETURN;
  END(*IF*);

  D0 := FLOAT(D);
  IF M > 2 THEN
    M0 := FLOAT(M+1);
    Y0 := FLOAT(Y);
  ELSE
    M0 := FLOAT(M+13);
    Y0 := FLOAT(Y-1);
  END(*IF*);
  JULDATE := LowLong.round(365.25*Y0,0) + LowLong.round(30.6001*M0,0) + D0 + FUDGEFACTOR;
END GREG2JUL;

PROCEDURE JUL2GREG(JULDATE : LONGREAL; VAR M,D,Y : CARDINAL);
(*
************************************ JUL2GREG ***************************
Julian to Gregorian Date Conversion Routine.
This converts from the Julian date back to the conventional date format.

Year will be returned as zero if JULDATE was out of range.

INPUT FROM GLOBAL VAR'S : FUDGEFACTOR.
*)

VAR TEMP,M0,D0,Y0 : LONGREAL;
(*    C,K           : CARDINAL; *)

  PROCEDURE YRFUNCT() : LONGREAL;
  (*
  ***************************** YRFUNCT ********************************
  Year Function.
  This is similar to the statement function concept of Fortran.  It computes
  the year number from the Julian date number. There are no input params.

  GBL input from JUL2GREG Proc : TEMP.
  *)

  BEGIN
    RETURN(LowLong.round(TEMP/365.25,0));
  END YRFUNCT;

  PROCEDURE MONFUNCT(YR : LONGREAL) : LONGREAL;
  (*
  *************************************** MONFUNCT ***********************
  Month Function.
  This is a statement function that computes the month number from the
  julian date number using the year computed by the YRFUNCT.

  GBL input from JUL2GREG Proc : TEMP.
  *)

  BEGIN
    RETURN LowLong.round((TEMP - LowLong.round(365.25*YR,0))/30.6001,0);
  END MONFUNCT;

  PROCEDURE DAYFUNCT(YR,MON : LONGREAL) : LONGREAL;
  (*
  ******************************************** DAYFUNCT *****************
  Day Function.
  This statement function computes the day number from the julian date
  using the year and month computed by the other statement functions.

  GBL input from JUL2GREG Proc : TEMP.
  *)

  BEGIN
    RETURN(TEMP - LowLong.round(365.25*YR,0) - LowLong.round(30.6001*MON,0));
  END DAYFUNCT;

BEGIN
(*
  ERROR IF JULDATE IS BEFORE 1/1/1900 OR AFTER 12/31/2099.
*)
  IF (JULDATE < 694043.0) OR (JULDATE > 767092.0) THEN
    Y := 0;
    RETURN;
  END(*IF*);

  TEMP := JULDATE - FUDGEFACTOR;
  Y0 := YRFUNCT();
  LOOP
    M0 := MONFUNCT(Y0);
    LOOP
      D0 := DAYFUNCT(Y0,M0);
      IF D0 >= 1.0 THEN EXIT; END(*IF*);
      M0 := M0 - 1.0;
    END(*LOOP*);
    IF M0 >= 4.0 THEN EXIT; END(*IF*);
    Y0 := Y0 - 1.0;
  END(*LOOP*);

  IF M0 > 13.0 THEN
    M := ROUND(M0) - 13;
    Y := ROUND(Y0) + 1;
  ELSE
    M := ROUND(M0) - 1;
    Y := ROUND(Y0);
  END(*IF*);
  D := ROUND(D0);
END JUL2GREG;

PROCEDURE GETMDY(INBUF : BUFTYP; VAR M,D,Y : CARDINAL);
(*
********************************** GETMDY *****************************
GET MONTH DAY YEAR.
This procedure parses the input string into the month, day and year.  Any
delimiters can be used because the parsing process uses a finite state
automata algorithm (part of the TKNRTMSg module) to parse the input line.
If an error occurs, then Y is returned as zero.

*)
VAR TOKEN             : BUFTYP;
    TKNSTATE,CHRSTATE : FSATYP;
    C,RETCOD          : CARDINAL;
    ERRFLG            : BOOLEAN;
    SUM               : LONGINT;
    SUMARRAY          : ARRAY [1..3] OF LONGINT;
    CH                : CHAR;

BEGIN
  INI1TKN(INBUF);
  C := 1;
  LOOP
    GETTKN(TOKEN,TKNSTATE,SUM,RETCOD);
(*
  Parse by only retaining the digit tokens (TKNSTATE=DGT) and discarding the rest
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

PROCEDURE JULIAN(M,D,Y : CARDINAL) : LONGCARD;

VAR
  M0,D0,Y0   : LONGCARD;
  Juldate    : LONGCARD;

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

  M0 := (M - 1); (* CARDINAL is assignemtn compatible to LONGCARD *)
  Y0 := (Y - 1);

  Juldate :=  Y0*365     (* Number of days in previous normal years *)
            + Y0 DIV 4   (* Number of possible leap days *)
            - Y0 DIV 100 (* Subtract all century years *)
            + Y0 DIV 400 (* Add back the true leap century years *)
            + VAL(LONGCARD,ADIPM[M0]) + M0*30 + VAL(LONGCARD,D);
  IF ((( Y MOD 4 = 0) AND ( Y MOD 100 <> 0)) OR ( Y MOD 400 = 0)) AND
(*   123            3     3               32    2              21      *)
                                                                 (M > 2) THEN
    INC(Juldate);
  END(*IF*);
  RETURN Juldate;
END JULIAN;

PROCEDURE GREGORIAN(Juldate : LONGCARD; VAR M,D,Y : CARDINAL);

VAR Y0   : CARDINAL;
    M0   : [1..12];
    D0   : [1..31];
    L,JD : LONGCARD;

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

(* Old method of declaring and initializing the DAYNAMES array.
BEGIN (******************************** MAIN ****************************)
  DAYNAMES[1] := 'Sunday';
  DAYNAMES[2] := 'Monday';
  DAYNAMES[3] := 'Tuesday';
  DAYNAMES[4] := 'Wednesday';
  DAYNAMES[5] := 'Thursday';
  DAYNAMES[6] := 'Friday';
  DAYNAMES[7] := 'Saturday';
*)
END TIMLIBg.
