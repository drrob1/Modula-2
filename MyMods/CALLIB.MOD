IMPLEMENTATION MODULE CALLIB;

(*  Copyright (C) 1987  Robert Solomon MD.  All rights reserved. *)

  IMPORT Break;
  FROM Terminal IMPORT ReadString;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt;
  FROM UTILLIB IMPORT BLANK,STRTYP,BUFSIZ,BUFTYP,STR10TYP,
    ESC,NULL;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,
    GETCHR,UNGETCHR,GETTKN;
  FROM RealConversions IMPORT RealToString, StringToReal;
  FROM FloatingUtilities IMPORT Int, Round;
  FROM REALLIB IMPORT AMOD;

  CONST FUDGEFACTOR = 5.0;

  VAR 
    FINI                                 : BOOLEAN;
    C,K,RETCOD,RETCOD2,MM,DD,YY,YEAR,DOW : CARDINAL;
    I,J,SUM                              : INTEGER;
    INBUF,TOKEN                          : BUFTYP;
    JULDATE                              : LONGREAL;
    OUTSTR,STR1,STR2,STR3                : STRTYP;    
    TKNSTATE                             : FSATYP;
    DAYNAMES                             : ARRAY [1..7] OF STR10TYP;

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
  IF (M < 1) OR (M > 12) OR (D < 1) OR (D > 31) OR (Y < 1900) OR (Y > 2500)
                                                                        THEN
(* MONTH, DAY OR YEAR IS OUT OF RANGE *)
    JULDATE := 0.;
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
  JULDATE := Int(365.25*Y0) + Int(30.6001*M0) + D0 + FUDGEFACTOR;
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
    C,K           : CARDINAL;

  PROCEDURE YRFUNCT() : LONGREAL;
  (*
  ***************************** YRFUNCT ********************************
  Year Function.
  This is similar to the statement function concept of Fortran.  It computes
  the year number from the Julian date number. There are no input params.

  GBL input from JUL2GREG Proc : TEMP.
  *)

  BEGIN
    RETURN(Int(TEMP/365.25));
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
    RETURN(Int((TEMP - Int(365.25*YR))/30.6001));
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
    RETURN(TEMP - Int(365.25*YR) - Int(30.6001*MON));
  END DAYFUNCT;

BEGIN
(* 
  ERROR IF JULDATE IS BEFORE 1/1/1900 OR AFTER 12/31/2500.
*)
  IF (JULDATE < 694043.) OR (JULDATE > 913558.) THEN
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

  IF M0 > 13. THEN
    M := Round(M0) - 13;
    Y := Round(Y0) + 1;
  ELSE
    M := Round(M0) - 1;
    Y := Round(Y0);
  END(*IF*);
  D := Round(D0);
END JUL2GREG;

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
    C,K,RETCOD        : CARDINAL;
    ERRFLG            : BOOLEAN;
    SUM               : INTEGER;
    SUMARRAY          : ARRAY [1..3] OF INTEGER;
    CH                : CHAR;

BEGIN
  INI1TKN(INBUF);
  C := 1;
  RETCOD2 := 0;
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
  M := CARDINAL(SUMARRAY[1]);
  D := CARDINAL(SUMARRAY[2]);
  Y := CARDINAL(SUMARRAY[3]);
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

END CALLIB.

