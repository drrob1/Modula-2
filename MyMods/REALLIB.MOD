IMPLEMENTATION MODULE REALLIB;

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
*)
(*
  IMPORT Break;
  FROM Terminal IMPORT ReadString;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt;
  FROM FloatingUtilities IMPORT Int, Round;
  FROM LongMathLib0 IMPORT exp,ln;
  FROM RealConversions  IMPORT RealToString,    StringToReal;
  FROM NumberConversion IMPORT StringToLongInt, LongIntToString;
*)
  FROM SYSTEM IMPORT ADR;
  IMPORT RConversions, LongStr, LongConv, Conversions;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
(*  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr; *)
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  IMPORT LongMath, LowLong;  (* , MATHLIB; TSlib routine *)
  IMPORT IOChan, ChanConsts;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM UTILLIB IMPORT BLANK,STRTYP,BUFSIZ,BUFTYP,STR10TYP,ESC,NULL,
    STRLENFNT,LCHINBUFFNT,TRIM,SCANFWD,SCANBACK,COPYLEFT;

CONST DECPT = '.';

PROCEDURE ROUND(R : LONGREAL) : CARDINAL;
(*
***************************************** ROUND ***************************
Does Just That.

*)
VAR R1 : LONGREAL;
BEGIN
  IF R > 0. THEN R1 := R + 0.5;
  ELSE R1 := R - 0.5;
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
    FINI : BOOLEAN;

BEGIN
  R := R + 0.00005;
  K := 0;
  RealToStringFixed(R,15,14,BUF.CHARS,K,FINI);
  BUF.COUNT := STRLENFNT(BUF.CHARS);
  C := LCHINBUFFNT(BUF,DECPT);
(*
  Null terminate real number string at the location of the decimal point and
  then convert back to a real number.
*)
  BUF.CHARS[C+1] := NULL;
  K := 0;
  StringToReal(BUF.CHARS,K,R2,FINI);
  RETURN(R2);
END AINT;

PROCEDURE AMOD(ANUM,AMODULUS : LONGREAL) : LONGREAL;
(*
*************************************** AMOD **************************
Real Modulus
This is a modulus function for real numbers, that uses the AINT in the
standard definition of the modulus fnt, A MOD B = A - [A/B]*B where []
represents the integerization function.  For integers the mod function is a
built in operator.
*)

VAR R      : LONGREAL;
    c1, c2 : CARDINAL;

BEGIN
(*  R := ANUM - AINT(ANUM/AMODULUS)*AMODULUS; didn't work in all cases *)
  c1 := TRUNC(ANUM);
  c2 := TRUNC(AMODULUS);
  R := LFLOAT(c1 REM c2);
  RETURN R;
(*  RETURN MATHLIB.Mod(ANUM,AMODULUS);  doesn't seem to work *)
END AMOD;

PROCEDURE PWRI(R:LONGREAL; I:INTEGER) : LONGREAL;
(*
************************** PWRI ***********************************
POWER OF I.
This is a power function with a real base and integer exponent.
it uses the optimized algorithm as discussed in PIM-2, V. 2.
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
    IF NEGFLAG THEN Z := 1. / Z; END(*IF*);
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
CROP Number STRing by truncating trailing insignificant zeros and leading
blanks.
Remember, STR param is now passed as a zero origin string within this proc.
*)

VAR
  STRLEN,NON0POSN,NONBLPOSN : CARDINAL;

BEGIN
  STRLEN := STRLENFNT(STR);
(*
  Scanback for first nonzero char.  This is the new length of the string,
  which is now 1 too large, so the null is assigned without adding 1.  It
  doesn't matter for the SCANFWD search below.  NONBLPOSN is corrected by
  the -1 term.
*)
  NON0POSN := SCANBACK(ADR(STR),STRLEN,'0',FALSE);
  STR[NON0POSN] := NULL; (* Terminate string at 1-st insignificant 0 *)
  NONBLPOSN := SCANFWD(ADR(STR),NON0POSN,BLANK,FALSE) - 1;
(*
  Remove the leading blanks by copying the non-blank string to the
  beginning, including the terminating null char.
*)
  COPYLEFT(ADR(STR[NONBLPOSN]),ADR(STR),NON0POSN-NONBLPOSN+1);
END CROPNSTR;

PROCEDURE GETCROPNUM(R : LONGREAL; VAR STR : ARRAY OF CHAR);
(*
********************************** GETCROPNUM *******************************
GET CROPped NUMber string from longreal input as a param.
*)

VAR
  GOOD : BOOLEAN;
  pos  : CARDINAL;

BEGIN
  pos := 0;
  RealToStringFixed(R,15,14,STR,pos,GOOD);
  IF NOT GOOD THEN
    WriteString(' Error while converting real to string in GETCROPNUM,');
    WriteLn;
    WriteString(' probably because string was too small.');
    WriteLn;
  END(*IF*);
  CROPNSTR(STR);
END GETCROPNUM;

PROCEDURE FLOATLONG(L : LONGINT) : LONGREAL;
(*
********************************************************* FLOATLONG *********
Float a longint, ie, convert a long integer to a real.
*)

VAR R    : LONGREAL;
    BUF  : BUFTYP;
    FINI : BOOLEAN;
    pos  : CARDINAL;

BEGIN
    pos := 0;
    LongToString(L,40,BUF.CHARS,pos,FINI);
    pos := 0;
    StringToReal(BUF.CHARS,pos,R,FINI);
    RETURN R;
END FLOATLONG;

PROCEDURE TRUNCLONG(R : LONGREAL) : LONGINT;
(*
********************************************* TRUNCLONG ******************
Truncate a real into a long integer.  Uses the same algorithm that AINT
above does.
*)

VAR L    : LONGINT;
    BUF  : BUFTYP;
    C,K  : CARDINAL;
    FINI : BOOLEAN;

BEGIN
  K := 0;
  RealToStringFixed(R,15,14,BUF.CHARS,K,FINI);
  TRIM(BUF);
  C := LCHINBUFFNT(BUF,DECPT);
(*
  Null terminate real number string at the location of the decimal point and
  then convert to a longint.  Note that the decimal point itself is
  overwritten by the NULL.
*)
  BUF.CHARS[C] := NULL;
  K := 0;
  StringToLong(BUF.CHARS,K,L,FINI);
  RETURN(L);
END TRUNCLONG;

END REALLIB.
(*
PROCEDURE RealToString(num : LONGREAL;
                       digits : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Convert a real number into a string buf starting at position pos. *)
(* pos is left pointing at the next character after the converted number *)
(* done signifies the success of the conversion *)

PROCEDURE RealToStringFixed(num : LONGREAL;
                            digits, before : CARDINAL;
                            VAR OUT str : ARRAY OF CHAR;
                            VAR INOUT pos : CARDINAL;
                            VAR OUT done : BOOLEAN);
(* Convert a real number into a string buf starting at position pos using *)
(* fixed point notation with digits digits and before digits before the *)
(* decimal point *)
(* done signifies the success of the conversion *)

PROCEDURE StringToReal(str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT num : LONGREAL;
                       VAR OUT done : BOOLEAN);
(* Get a longreal number from a string starting at position pos. *)
(* pos is left pointing to the first character that is not part of *)
(* the number done signifies the success of the conversion *)
PROCEDURE StringToLong(str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT num : LONGINT;
                       VAR OUT done : BOOLEAN);
(* Get an longint number from a string buf starting at position pos. *)
(* pos is left pointing at the first character that is not part of the *)
(* number *)
(* done signifies the success of the conversion *)
(* Skips any leading spaces. *)

PROCEDURE StrToLong(buf : ARRAY OF CHAR; VAR OUT num : LONGINT) : BOOLEAN;
(* Convert a longint number from a string  *)
(* Skips any leading spaces. *)

PROCEDURE LongToString(num : LONGINT;
                       size : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Convert a longint number into the string starting at position *)
(* pos.  pos is left pointing to the character following the number *)
(* done signifies the success of the conversion *)

PROCEDURE LongToStr(num : LONGINT; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* Convert a longint to a string *)

*)
