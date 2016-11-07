IMPLEMENTATION MODULE MiscStdInOutg;
(*
  Revision History
  ----------------
  25 Sep 07 -- Made MiscM2 use std i/o and renamed module
   2 Apr 13 -- Defined SkipLine, which is needed for terminal input.
   3 Apr 13 -- Added WriteLongCard.  And ADDCOMMAS
   5 Oct 13 -- Converted to gm2.
  10 Oct 13 -- Added WriteLongInt.
  11 Oct 13 -- Added ReadAllChars, WriteLongRealFloat, LongRealToFloatString.
  12 Oct 13 -- Added WriteLongReal.
  13 Oct 13 -- Added StringToLongReal.
  14 Oct 13 -- Removed use of ISO routines.  Too many conflicts in gm2.


*)

        (********************************************************)
        (*                                                      *)
        (*          Miscellaneous utility procedures            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        31 July 1996                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (*   Shortcomings:                                      *)
        (*      1. The PressAnyKey procedure is failing to      *)
        (*         return until end-of-line is found.  As a     *)
        (*         workaround, I've changed it to require the   *)
        (*         <Enter> key to be pressed.                   *)
        (*      2. (fixed)                                      *)
        (*                                                      *)
        (*      The purpose of this module is to provide        *)
        (*      the non-portable part of a numerical            *)
        (*      analysis package - i.e. to separate out the     *)
        (*      library dependencies, so that most of the       *)
        (*      work in porting the software to another         *)
        (*      compiler or library lies in rewriting this      *)
        (*      (simple) module.                                *)
        (*                                                      *)
        (*      Many of the procedures here relate to output    *)
        (*      to a screen window.  For use in an environment  *)
        (*      which does not support screen windows, you      *)
        (*      simply have to replace the definition of        *)
        (*      type "Window" by a dummy definition, and let    *)
        (*      the implementation ignore the "Window"          *)
        (*      parameters.                                     *)
        (*                                                      *)
        (*      One catch with the present approach is that     *)
        (*      it requires the concept of the "current         *)
        (*      window".  Do not attempt to use this module     *)
        (*      in multitasking applications, because if        *)
        (*      more than one task is doing screen output       *)
        (*      then there is an ambiguity in what constitutes  *)
        (*      the current window.                             *)
        (*                                                      *)
        (*      This version is for use with the XDS compiler.  *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  ADR;


(*
                       IMPORT STextIO, SWholeIO, SRealIO, SLongIO, Strings, WholeStr, RealStr, LongStr;
                       FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
 *)
IMPORT DynamicStrings, StringConvert, FpuIO, StrIO, StdIO, NumberIO, ASCII, Strings;
FROM UTILLIBg IMPORT NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    MAXCARDFNT,SCANBACK,SCANFWD,COPYLEFT,COPYRIGHT,FILLCHAR,ASSIGN2BUF;
FROM Environg IMPORT GetCommandLine;
IMPORT REALLIBg;
FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

(************************************************************************)

(************************************************************************)
(*                      MISCELLANEOUS UTILITIES                         *)
(************************************************************************)

(************************************************************************)
(*                  NUMERIC-TO-STRING CONVERSION                        *)
(************************************************************************)

PROCEDURE LongRealToFixedString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);
    BEGIN
      LongRealToString(x,buffer,fieldsize);
    END LongRealToFixedString;

PROCEDURE LongRealToFloatString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);
BEGIN
  FormatLongRealAsFloat(x,buffer,fieldsize);
END LongRealToFloatString;

PROCEDURE LongRealToEngString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);
BEGIN
      LongRealToString(x,buffer,fieldsize);
END LongRealToEngString;

PROCEDURE LongRealToString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);
    (* Converts the number to a decimal character string in array       *)
    (* "buffer", right-justified in a field of fieldsize characters.    *)
    (* The format depends on the size of the number relative to the     *)
    (* size of the buffer.                                              *)
    (* Will write a zero if abs(number) is a very small number, here            *)
    (* if < 1.e-14, an arbitrary number I am using empirically by using         *)
    (* data from the SBM2 debugger.  And will write a 1 if number is very close *)

VAR
  fractionwidth : CARDINAL;

BEGIN
  IF ABS(x) < 1.0E-14 THEN
    x := 0.0;
  ELSIF ABS((ABS(x) - 1.0)) < 1.0E-13 THEN
    IF x > 0.0 THEN
      x := 1.0;
    ELSE
      x := -1.0;
    END;
  END;
  fractionwidth := MAXCARDFNT(fieldsize-5,5);
  FpuIO.LongRealToStr(x,fieldsize,fractionwidth,buffer);

END LongRealToString;

(*****************************************************************************)
PROCEDURE LongRealToStr (x: LONGREAL; VAR buffer: ARRAY OF CHAR );
VAR
  LongEnoughStr : ARRAY [0..20] OF CHAR;
  sigfig        : CARDINAL;

BEGIN
  IF ABS(x) < 1.0E-14 THEN
    x := 0.0;
  ELSIF ABS((ABS(x) - 1.0)) < 1.0E-13 THEN
    IF x > 0.0 THEN
      x := 1.0;
    ELSE
      x := -1.0;
    END;
  END;
  IF ABS(x) < 1.0E9 THEN
    sigfig := 20;
  ELSE                     (* force exponential notation *)
    sigfig := 8;
  END;
  LongRealToString(x,LongEnoughStr,sigfig);
  Strings.Assign(LongEnoughStr,buffer);
END LongRealToStr;

(*****************************************************************************)
PROCEDURE StringToLongReal (s: ARRAY OF CHAR; VAR x: LONGREAL);

BEGIN
  FpuIO.StrToLongReal(s, x);

END StringToLongReal;

(*****************************************************************************)

PROCEDURE LongCardToString(LC: LONGCARD; VAR str: ARRAY OF CHAR);
  VAR 
    dstr : DynamicStrings.String;

  BEGIN
    dstr := StringConvert.LongCardinalToString(LC,0,' ',10,FALSE);
    DynamicStrings.CopyOut(str,dstr);
  END LongCardToString;
(*****************************************************************************)
  
PROCEDURE CardToString(c: CARDINAL; VAR str: ARRAY OF CHAR);
BEGIN
  NumberIO.CardToStr(c,0,str);
END CardToString;
(*****************************************************************************)

PROCEDURE IntToString(i: INTEGER; VAR str: ARRAY OF CHAR);
BEGIN
  NumberIO.IntToStr(i,0,str);
END IntToString;
(*****************************************************************************)

PROCEDURE LongIntToString(LI: LONGINT; VAR str: ARRAY OF CHAR);
BEGIN
  FpuIO.LongIntToStr(LI,0,str);
END LongIntToString;
(*****************************************************************************)

PROCEDURE StringToCard(s: ARRAY OF CHAR; VAR c: CARDINAL);
BEGIN
  NumberIO.StrToCard(s,c);
END StringToCard;
(*****************************************************************************)

PROCEDURE StringToLongCard(s: ARRAY OF CHAR; VAR LC: LONGCARD);
VAR
  c, lenstr : CARDINAL;

BEGIN
  LC := 0;
  c := 0;
  lenstr := LENGTH(s);
  WHILE c < lenstr DO
    LC := 10 * LC + VAL(LONGINT,INT(ORD(s[c]) - ORD('0')));
    INC(c);
  END (* conversion while loop *)
END StringToLongCard;
(*****************************************************************************)

PROCEDURE StringToInt(s: ARRAY OF CHAR; VAR i: INTEGER);
BEGIN
  NumberIO.StrToInt(s,i);
END StringToInt;
(*****************************************************************************)

PROCEDURE StringToLongIntBad(s: ARRAY OF CHAR; VAR LI: LONGINT);
BEGIN
  FpuIO.StrToLongInt(s,LI);
END StringToLongIntBad;
(*****************************************************************************)

PROCEDURE StringToLongInt(s: ARRAY OF CHAR; VAR LI: LONGINT);
VAR
  c, lenstr : CARDINAL;
  neg       : BOOLEAN;

BEGIN
  LI := 0;
  c := 0;
  lenstr := LENGTH(s);
  IF s[0] = '-' THEN
    neg := TRUE;
    INC(c);
  ELSE
    neg := FALSE;
  END; (* if negative number *)
  WHILE c < lenstr DO
    LI := 10 * LI + VAL(LONGINT,INT(ORD(s[c]) - ORD('0')));
    INC(c);
  END; (* conversion while loop *)
  IF neg THEN LI := -LI; END;
END StringToLongInt;
(*****************************************************************************)

PROCEDURE GetExponent(R: LONGREAL; VAR n: INTEGER);

BEGIN
  n := 0;
  IF R < 0.0 THEN
     R := -R;
  END; (* if R is 0 or negative *)
  R := R + 5.0E-5*R;     (* help correct a conversion error for fractions *)
  IF (R = 0.0) OR (R = 1.0) THEN
    (* leave n at zero *)
  ELSIF R > 1.0 THEN
    WHILE R >= 10.0 DO
      R := R/10.0;
      INC (n);
    END; (* while *)
  ELSIF R < 1.0 THEN
    WHILE R < 1.0 DO
      R := R*10.0;
      DEC(n);
    END; (* while *)
  ELSE 
    (* should never get here, since all cases I can think of is already covered *)
    Error(' In GetExponent and R is out of range.  No idea why');
  END; (* if R *)
END GetExponent;

(*****************************************************************************)
PROCEDURE ADDCOMMAS(VAR STR : ARRAY OF CHAR);
VAR
  C,PTR,NCOM,NDGTS,NULLPOSN : CARDINAL;

BEGIN
(*  NDGTS := STRLENFNT(STR); *)
  
  NDGTS := LENGTH(STR);
  PTR := NDGTS - 1;  (* Zero origin array as a param *)
  NCOM := PTR DIV 3;
  NULLPOSN := NDGTS + NCOM;
  IF NULLPOSN > HIGH(STR) THEN
(*
    WriteString(' Cannot add commas because string is too small.');
    WriteLn;
*)
    RETURN;
  END(*IF*);
  WHILE NCOM > 0 DO
    FOR C := 1 TO 3 DO
      STR[PTR+NCOM] := STR[PTR];
      DEC(PTR);
    END(*FOR*);
    STR[PTR+NCOM] := ',';
    DEC(NCOM);
  END(*WHILE*);
  IF NULLPOSN < HIGH(STR) THEN STR[NULLPOSN] := 0C; END(*IF*);
END ADDCOMMAS;
(*****************************************************************************)


PROCEDURE FormatLongRealAsFloat(x: LONGREAL; VAR str: ARRAY OF CHAR; sigfig: CARDINAL);
VAR
  strlen,c,DecPtPosn : CARDINAL;
  exponent           : INTEGER;
  s,expstr           : ARRAY [0..127] OF CHAR;
  dot,e,plus         : Strings.String1;

BEGIN
  IF ABS(x) < 1.0E-14 THEN
    x := 0.0;
  ELSIF ABS(ABS(x) - 1.0) < 1.0E-13 THEN
    IF x > 0.0 THEN
      x := 1.0;
    ELSE
      x := -1.0;
    END;
  END;
  dot := '.';
  e := 'e';
  plus := '+';

  GetExponent(x,exponent);
  FpuIO.RealToStr(x,18,sigfig,s);
  REALLIBg.CROPNSTR(s);
  strlen := LENGTH(s);
  DecPtPosn := SCANBACK(ADR(s),strlen,'.',TRUE) -1;
  Strings.Delete(s,DecPtPosn,1);
  IF (x >= 1.0) OR (x = 0.0) THEN
    Strings.Insert(dot,1,s);
  ELSIF x > 0.0 THEN  (* must be a positive fraction < 1 *)
   (* Delete the leading 0's *)
    WHILE s[0] = '0' DO
      Strings.Delete(s,0,1);
    END; (* while have a leading 0 to delete *)
    Strings.Insert(dot,1,s);
  ELSIF x > -1.0 THEN  (* x is neg fraction btwn -1 and zero *)
    WHILE s[1] = '0' DO
      Strings.Delete(s,1,1);
    END; (* while have a leading 0 after minus sign *)
    Strings.Insert(dot,2,s);
  ELSE         (* x is more negative than a fraction *)
    Strings.Insert(dot,2,s);
  END; (* if pos, neg or a fraction matters as to where insert the decimal point *)
  IF exponent = 0 THEN
    Strings.Assign('     ',expstr);
  ELSIF exponent > 0 THEN
    IntToString(exponent,expstr);
    Strings.Append(e,s);
    Strings.Append(plus,s);
  ELSE (* exponent < 0 *)
    IntToString(exponent,expstr);
    Strings.Append(e,s);
  END; (* if exponent zero, positive, negative *)
  Strings.Append(expstr,s);
  Strings.Assign(s,str);  
END FormatLongRealAsFloat;


(************************************************************************)
(*                          SCREEN OUTPUT                               *)
(************************************************************************)

PROCEDURE CLS;
VAR c : CARDINAL;
BEGIN
  FOR c := 1 TO 25 DO
    WriteString('                                                                             ');
    WriteLn;
  END;
END CLS;
(************************************************************************)

PROCEDURE WriteChar(ch : CHAR);
BEGIN
  StdIO.Write(ch);
END WriteChar;


(************************************************************************)

PROCEDURE WriteString (s: ARRAY OF CHAR);

    BEGIN
      StrIO.WriteString (s);
    END WriteString;

(************************************************************************)

PROCEDURE WriteLn;

    BEGIN
      StrIO.WriteLn;
    END WriteLn;

(************************************************************************)

PROCEDURE PressAnyKey;

    (* "Press any key to continue". *)
    (* Bug: this is requiring "Enter" before the character can  *)
    (* be read.  I'm not yet sure how to solve this.            *)
    (* As a temporary work-around, we require the user to       *)
    (* press the <Enter> key rather than the <Any> key.         *)

    VAR dummyStr: ARRAY [0..10] OF CHAR;

    BEGIN
        StrIO.WriteLn;
        StrIO.WriteString ("Press <ENTER> to continue");
        StrIO.ReadString(dummyStr);
        StrIO.WriteLn;
    END PressAnyKey;

(************************************************************************)

PROCEDURE Error (message: ARRAY OF CHAR);

    BEGIN
        WriteString ("Error: ");  WriteString (message);
        PressAnyKey;
    END Error;

(************************************************************************)
PROCEDURE WriteLongInt(i: LONGINT);
BEGIN
  FpuIO.WriteLongInt(i,0);
END WriteLongInt;
(************************************************************************)
PROCEDURE WriteLongCard (n : LONGCARD);
  VAR 
    s : ARRAY [1..256] OF CHAR;
(*    dstr : DynamicStrings.String; *)

BEGIN
  LongCardToString(n,s);
  IF n > 1000000 THEN
    ADDCOMMAS(s);
  END;
(*
                            dstr := StringConvert.LongCardinalToString(n,0,' ',20,FALSE);
                            DynamicStrings.CopyOut(s,dstr);
                            IF n > 1000000 THEN
                              ADDCOMMAS(s);
                            END;
*)
    WriteString(s);
END WriteLongCard;
(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);
VAR 
  s : ARRAY [1..256] OF CHAR;
   
BEGIN
  NumberIO.CardToStr(N,0,s);
  IF N > 100000 THEN
    ADDCOMMAS(s);
  END;
  WriteString(s);
END WriteCard;

(************************************************************************)
PROCEDURE WriteInt (i: INTEGER);
    BEGIN
      NumberIO.WriteInt(i,0);
     
    END WriteInt;

(************************************************************************)

PROCEDURE WriteRJCard (number, fieldsize: CARDINAL);
    (* Like WriteCard, but the result is right justified in a field     *)
    (* of fieldsize characters.                                         *)

    VAR s : ARRAY [1..256] OF CHAR;
        c : CARDINAL;

    BEGIN
      CardToString(number,s);
      WHILE LENGTH(s) < fieldsize DO
        Strings.Insert(' ',0,s);
      END;
      WriteString(s);
    END WriteRJCard;

(************************************************************************)

PROCEDURE WriteReal (x: REAL;  places: CARDINAL);
    (* Writes x in a field "places" characters wide. *)

  VAR s : ARRAY [1..256] OF CHAR;
      c : CARDINAL;

BEGIN
  IF ABS(x) < 2.0E9 THEN
    c := MAXCARDFNT(places-4,5);
  ELSE
    places := 9;
    c := 5;   (* force exponential notation *)
  END;

  FpuIO.RealToStr(x,places,c,s);
  WriteString(s);
END WriteReal;

(************************************************************************)

PROCEDURE WriteLongRealFixed (x: LONGREAL;  fieldsize: CARDINAL);

    (* Writes x in a field "fieldsize" characters wide. *)
    (* Will now write a zero if abs(number) is a very small number, here *)
    (* if < 1.e-14, an arbitrary number I am using empirically by using  *)
    (* data from the debugger.  And will write a 1 if number is very close *)

VAR
  buffer: ARRAY [0..127] OF CHAR;

BEGIN
  LongRealToFixedString(x,buffer,fieldsize);
  WriteString (buffer);
END WriteLongRealFixed;

(************************************************************************)
PROCEDURE WriteLongRealFloat (x: LONGREAL;  fieldsize: CARDINAL);

    (* Writes x in a field "fieldsize" characters wide. *)
    (* Will now write a zero if abs(number) is a very small number, here *)
    (* if < 1.e-14, an arbitrary number I am using empirically by using  *)
    (* data from the debugger.  And will write a 1 if number is very close *)

VAR
  buffer: ARRAY [0..127] OF CHAR;

BEGIN
  LongRealToFloatString(x,buffer,fieldsize);
  WriteString (buffer);
END WriteLongRealFloat;
(************************************************************************)
PROCEDURE WriteLongRealEng (x: LONGREAL;  fieldsize: CARDINAL);

    (* Writes x in a field "fieldsize" characters wide. *)
    (* Will now write a zero if abs(number) is a very small number, here *)
    (* if < 1.e-14, an arbitrary number I am using empirically by using  *)
    (* data from the debugger.  And will write a 1 if number is very close *)

VAR
  buffer: ARRAY [0..127] OF CHAR;

BEGIN
  LongRealToEngString(x,buffer,fieldsize);
  WriteString (buffer);
END WriteLongRealEng;
(************************************************************************)
PROCEDURE WriteLongReal (x: LONGREAL);

    (* Will now write a zero if abs(number) is a very small number, here *)
    (* if < 1.e-14, an arbitrary number I am using empirically by using  *)
    (* data from the debugger.  And will write a 1 if number is very close *)

VAR
  buffer: ARRAY [0..127] OF CHAR;
  sigfig : CARDINAL;

BEGIN
  IF ABS(x) < 2.0E9 THEN
    sigfig := 15
  ELSE
    sigfig := 5
  END;
  LongRealToString(x,buffer,sigfig);
  WriteString (buffer);
END WriteLongReal;

(************************************************************************)

PROCEDURE RtJust(VAR str : ARRAY OF CHAR; width : CARDINAL);
VAR diff : INTEGER;
    len : CARDINAL;
BEGIN
  len := LENGTH(str);
  diff := INT(width) - INT(len);
  IF diff >=1 THEN       (* must include terminating null *)
    COPYRIGHT(ADR(str),ADR(str[diff]),len+1);
    FILLCHAR(ADR(str),diff,' ');
  END;
END RtJust;

(************************************************************************)
(*                         KEYBOARD INPUT                               *)
(************************************************************************)
PROCEDURE ReadChar(VAR ch : CHAR);
BEGIN
  StdIO.Read(ch);
END ReadChar;

(************************************************************************)

PROCEDURE ReadString(VAR s : ARRAY OF CHAR);
(* For a full proc like this, see MiscM2 *)

BEGIN
  StrIO.ReadString(s);
END ReadString;

(************************************************************************)
PROCEDURE SkipLine;
(*
  Removes successive items from the default input stream up to and including the next line mark or until the end of input is reached.
  The read result is set to the value allRight, or endOfInput. 
*)
BEGIN
(* do nothing now         STextIO.SkipLine;  *)
END SkipLine;
(************************************************************************)

PROCEDURE ReadAllChars(VAR s: ARRAY OF CHAR);
(*
  Basically a readstring routine that does not filter out the <esc> char.  
  Seems that the OS already processes backspace so I don't have to.
*)
VAR
  c,high : CARDINAL;
  ch     : CHAR;

BEGIN
  high := HIGH(s);
  c := 0;
  ch := '';
  
  WHILE (c <= high) AND (ch <> ASCII.EOL) DO
    ReadChar(ch);
    s[c] := ch;
    INC(c);
  END; (* while there are characters to get and can fit in the string *)

  DEC(c); (* overwrite EOL with nul *)
  IF c <= high THEN s[c] := ASCII.nul; END;
END ReadAllChars;
(************************************************************************)

PROCEDURE ReadCard (VAR N: CARDINAL);
    BEGIN
      NumberIO.ReadCard(N);
    END ReadCard;

(************************************************************************)

PROCEDURE ReadLongReal(VAR lr: LONGREAL);
    BEGIN
      FpuIO.ReadLongReal(lr);
    END ReadLongReal;



(************************************************************************)
PROCEDURE WriteCx (Z: LONGCOMPLEX);

    (* Writes Z in Cartesian form, with "places" characters allowed     *)
    (* for each of the real and imaginary parts.                        *)

    VAR j: CARDINAL;  impart: LONGREAL;

    BEGIN
        WriteLongReal (RE(Z));
        IF IM(Z) = 0.0 THEN
          WriteString('    ');
(*
                                    FOR j := 0 TO places+3 DO
                                      WriteString (" ");
                                    END (*FOR*);
*)
        ELSE
            impart := IM(Z);
            IF impart < 0.0 THEN
                WriteString (" - ");
                impart := -impart;
            ELSE
                WriteString (" + ");
            END (*IF*);
            WriteString ("j");
            WriteLongReal (impart);
        END (*IF*);
    END WriteCx;

(************************************************************************)
BEGIN
END MiscStdInOutg.
