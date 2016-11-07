IMPLEMENTATION MODULE REALLIBg;

(*  Copyright (C) 1987  Robert Solomon MD.  All rights reserved.
   REVISION HISTORY
   ----------------
  13 Oct 90 -- Added the FLOATLONG and TRUNCLONG procedures that are
                needed by MYRANDOM.
  22 Dec 91 -- Converted to M2 V 4.00, so deleted UL2 due to lack of
                CODE support.
  30 Jul 93 -- Added CROPNSTR and GETCROPNUM.
  10 Nov 02 -- Converted to SBM2 win v4
  17 May 03 -- First Win32 version.
  23 Dec 04 -- Removed references to TSLib.
   7 Oct 13 -- Converted to gm2, hence the name change.  And removal of FLOATLONG and TRUNCLONG.
  16 Oct 13 -- Adjusted CROPNSTR to return if it does not find a decimal point to indicate a real number.
*)
  FROM SYSTEM IMPORT ADR;
  IMPORT MiscStdInOutg,FpuIO;
  FROM MiscStdInOutg IMPORT ReadCard, WriteCard, ReadString, WriteString, WriteLn, LongRealToFixedString;
  IMPORT LongStr, ConvTypes, ASCII;
  IMPORT LongMath;
  FROM UTILLIBg IMPORT STRTYP,BUFSIZ,BUFTYP,STR10TYP,ESC,NULL,
    STRLENFNT,LCHINBUFFNT,TRIM,SCANFWD,SCANBACK,COPYLEFT;

CONST DECPT = '.';

PROCEDURE ROUND(R : LONGREAL) : CARDINAL;
(*
***************************************** ROUND ***************************
Does Just That.

*)
VAR R1 : LONGREAL;
BEGIN
  IF R > 0.0 THEN
    R1 := R + 0.5;
  ELSE 
    R1 := R - 0.5;
  END(*IF*);
  RETURN TRUNC(R1);
END ROUND;

PROCEDURE AINT(R : LONGREAL) : LONGREAL;
(*
****************************** AINT ***********************************
REAL INTEGERIZE FUNCTION.
This is equivalent to Fortran's AINT fnt, which just returns the integral
part of a number and removes the fractional part.  This is done by
converting to a string, searching for the decimal point, and then converting
the digits to the left of the decimal pt back to a real number.  It must round
or else face fact that conversion error will yield the wrong result.  Like
entering 321 converts to 320.99997.

*)

VAR R2   : LONGREAL;
    BUF  : BUFTYP;
    C,K  : CARDINAL;
    result : ConvTypes.ConvResults;

BEGIN
  R := R + 0.00005;
  K := 0;
  LongRealToFixedString(R,BUF.CHARS,15);
  BUF.COUNT := STRLENFNT(BUF.CHARS);
  C := LCHINBUFFNT(BUF,DECPT);
(*
  Null terminate real number string at the location of the decimal point and then convert back to a real number.
*)
  BUF.CHARS[C+1] := NULL;
  K := 0;
  LongStr.StrToReal(BUF.CHARS,R2,result);
  RETURN(R2);
END AINT;

PROCEDURE AMOD(ANUM,AMODULUS : LONGREAL) : LONGREAL;
(*
*************************************** AMOD **************************
Real Modulus
This is a modulus function for real numbers, that used to use the AINT in the
standard definition of the modulus fnt, A MOD B = A - [A/B]*B where []
represents the integerization function.  For integers the mod function is a
built in operator.
*)

VAR R      : LONGREAL;
    c1, c2 : LONGINT;

BEGIN
(*  R := ANUM - AINT(ANUM/AMODULUS)*AMODULUS; didn't work in all cases *)
  c1 := TRUNC(ANUM);
  c2 := TRUNC(AMODULUS);
  R := FLOAT(c1 REM c2);
  RETURN R;
(*  RETURN MATHLIB.Mod(ANUM,AMODULUS);  doesn't seem to work *)
END AMOD;

PROCEDURE PWRI(R:LONGREAL; I:INTEGER) : LONGREAL;
(*
************************** PWRI ***********************************
POWER OF I.
This is a power function with a real base and integer exponent.
it uses the optimized algorithm as discussed in PIM-2, V.  2.
*)

VAR Z : LONGREAL;
  NEGFLAG : BOOLEAN;

BEGIN
    Z := 1.0;
    NEGFLAG := I < 0;
    I := ABS(I);
    WHILE I > 0 DO
        IF ODD(I) THEN Z := Z*R; END(*IF*);
        R := R*R;
        I := I DIV 2;
    END(*WHILE*);
    IF NEGFLAG THEN Z := 1.0 / Z; END(*IF*);
    RETURN(Z);
END PWRI;

PROCEDURE PWRR(Y,X:LONGREAL) : LONGREAL;
(*
**************************** PWRR ************************************
POWER OF REAL NUMBERS.
THIS IS A POWER FUNCTION WITH A REAL BASE AND EXPONENT.  IT USES A
SIMPLE LOG AND ANTILOG FORMULA TO CALCULATE Y**X.
*)

VAR RESULT : LONGREAL;

BEGIN
  RESULT := LongMath.exp(X * LongMath.ln(Y));
  RETURN(RESULT);
END PWRR;

PROCEDURE CROPNSTR(VAR STR : ARRAY OF CHAR);
(*
******************************************* CROPNSTR ***********************
CROP Number STRing by truncating trailing insignificant zeros and leading blanks.
Remember, STR param is now passed as a zero origin string within this proc.
Must have a real number for this routine.  It will return if it does not find a 
decimal point.
*)

VAR
  STRLEN,NON0POSN,NONBLPOSN : CARDINAL;

BEGIN
  STRLEN := STRLENFNT(STR);
  IF SCANBACK(ADR(STR),STRLEN,'.',TRUE) = 0 THEN RETURN END;
(*
  Scanback for first nonzero char.  This is the new length of the string,
  which is now 1 too large, so the null is assigned without adding 1.  It
  doesn't matter for the SCANFWD search below.  NONBLPOSN is corrected by
  the -1 term.
*)
  NON0POSN := SCANBACK(ADR(STR),STRLEN,'0',FALSE);
  STR[NON0POSN] := NULL; (* Terminate string at 1-st insignificant 0 *)
  NONBLPOSN := SCANFWD(ADR(STR),NON0POSN,ASCII.sp,FALSE) - 1;
(*
  Remove the leading blanks by copying the non-blank string to the beginning, including the terminating null char.
*)
  COPYLEFT(ADR(STR[NONBLPOSN]),ADR(STR),NON0POSN-NONBLPOSN+1);
END CROPNSTR;

PROCEDURE GETCROPNUM(R : LONGREAL; VAR STR : ARRAY OF CHAR);
(*
********************************** GETCROPNUM *******************************
GET CROPped NUMber string from longreal input as a param.
*)


BEGIN
  FpuIO.LongRealToStr(R,18,13,STR);
  CROPNSTR(STR);
END GETCROPNUM;


PROCEDURE GETCROPNUMreal(r : REAL; VAR STR : ARRAY OF CHAR);
(*
********************************** GETCROPNUMreal *******************************
GET CROPped NUMber string from longreal input as a param.
*)


BEGIN
  FpuIO.RealToStr(r,18,13,STR);
  CROPNSTR(STR);
END GETCROPNUMreal;
END REALLIBg.
