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


IMPORT STextIO, SWholeIO, SRealIO, SLongIO, Strings, WholeStr, RealStr, LongStr, 
        DynamicStrings, StringConvert,FpuIO;
FROM UTILLIBg IMPORT NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    MAXCARDFNT,SCANBACK,SCANFWD,COPYLEFT,COPYRIGHT,FILLCHAR,ASSIGN2BUF;
IMPORT StrIO,NumberIO;
IMPORT ASCII;
FROM Environg IMPORT GetCommandLine;

FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

(************************************************************************)

(************************************************************************)
(*                      MISCELLANEOUS UTILITIES                         *)
(************************************************************************)

(************************************************************************)
(*                  NUMERIC-TO-STRING CONVERSION                        *)
(************************************************************************)

PROCEDURE LongRealToFixedString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);

    (* Converts the number to a decimal character string in array       *)
    (* buffer, right-justified in a field of fieldsize characters.      *)
    (* The format depends on the size of the number relative to the     *)
    (* size of the buffer.                                              *)

    (* Will now write a zero if abs(number) is a very small number, here *)
    (* if < 1.e-14, an arbitrary number I am using empirically by using  *)
    (* data from the SBM2 debugger.  And will write a 1 if number is very close *)

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
      LongStr.RealToFixed (x, fieldsize, buffer);
    END LongRealToFixedString;

(*****************************************************************************)
PROCEDURE LongRealToFloatString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);

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
  LongStr.RealToFloat(x,fieldsize,buffer);

END LongRealToFloatString;
(*****************************************************************************)
PROCEDURE LongRealToEngString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);

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
  LongStr.RealToEng(x,fieldsize+2,buffer);

END LongRealToEngString;
(*****************************************************************************)
PROCEDURE LongRealToString (x: LONGREAL; VAR buffer: ARRAY OF CHAR; fieldsize: CARDINAL);
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
  LongStr.RealToStr(x,LongEnoughStr);
  Strings.Assign(LongEnoughStr,buffer);

END LongRealToStr;

(*****************************************************************************)
PROCEDURE StringToLongReal (s: ARRAY OF CHAR; VAR x: LONGREAL);

BEGIN
  FpuIO.StrToLongReal(s, x);

END StringToLongReal;

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
  STextIO.WriteChar(ch);
END WriteChar;


(************************************************************************)

PROCEDURE WriteString (s: ARRAY OF CHAR);

    BEGIN
      STextIO.WriteString (s);
    END WriteString;

(************************************************************************)

PROCEDURE WriteLn;

    BEGIN
      STextIO.WriteLn;
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
(*  ok, this works.  Now to try with STEXTIO
        StrIO.WriteLn;
        StrIO.WriteString ("Press <ENTER> to continue");
        StrIO.ReadString(dummyStr);
        StrIO.WriteLn;
*)
        WriteLn;
        WriteString ("Press <ENTER> to continue");
        ReadString(dummyStr);
        WriteLn;
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
    dstr : DynamicStrings.String;

BEGIN
(*    LWholeStr.LongCardToStr(n,s); *)
    dstr := StringConvert.LongCardinalToString(n,0,' ',10,FALSE);
    DynamicStrings.CopyOut(s,dstr);
    ADDCOMMAS(s);
    WriteString(s);
END WriteLongCard;
(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);
VAR 
  s : ARRAY [1..256] OF CHAR;
   
    (* Writes a cardinal value. *)

BEGIN
  WholeStr.CardToStr(N,s);
  ADDCOMMAS(s);
  WriteString(s);
END WriteCard;

(************************************************************************)
PROCEDURE WriteInt (i: INTEGER);
(*
VAR s : STRTYP;
*)

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
      WholeStr.CardToStr(number,s);
      WHILE LENGTH(s) < fieldsize DO
        Strings.Insert(' ',0,s);
      END;
      WriteString(s);
    END WriteRJCard;

(************************************************************************)

PROCEDURE WriteReal (x: REAL;  places: CARDINAL);
    (* Writes x in a field "places" characters wide. *)

  VAR s : ARRAY [1..256] OF CHAR;


BEGIN
  IF (ABS(x) >= 1.0) OR (x = 0.0) THEN
    RealStr.RealToFixed(x,places,s);
  ELSE
    RealStr.RealToFloat(x,places,s);
  END;
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

BEGIN
  LongRealToString(x,buffer,15);
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
  STextIO.ReadChar(ch);
END ReadChar;

(************************************************************************)

PROCEDURE ReadString(VAR s : ARRAY OF CHAR);
(* For a full proc like this, see MiscM2 *)

BEGIN
  StrIO.ReadString(s);
(*
  STextIO.ReadRestLine(s);
  STextIO.ReadString(s);
  STextIO.SkipLine;
*)

END ReadString;

(************************************************************************)
PROCEDURE SkipLine;
(*
  Removes successive items from the default input stream up to and including the next line mark or until the end of input is reached.
  The read result is set to the value allRight, or endOfInput. 
*)
BEGIN
        STextIO.SkipLine;
END SkipLine;
(************************************************************************)

PROCEDURE ReadAllChars(VAR s: ARRAY OF CHAR);
(*
  Basically a readstring routine that does not filter out the <esc>
  char.  Seems that the OS already processes backspace so I don't have
  to.
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

  DEC(c); (* overright EOL with nul *)
  IF c <= high THEN s[c] := ASCII.nul; END;
END ReadAllChars;
(************************************************************************)

PROCEDURE ReadCard (VAR N: CARDINAL);
(*
VAR ch : CHAR;
    s  : STRTYP;
    mybuf,token : BUFTYP;
    tknstate : FSATYP;
    c,retcod  : CARDINAL;
    i  : LONGINT;
*)
    BEGIN
      NumberIO.ReadCard(N);
(*
      ReadString(s);
      ASSIGN2BUF(s,mybuf);
      IF mybuf.COUNT = 0 THEN
        N := 0;
        RETURN
      END;
      INI1TKN(mybuf);
      GETTKN(token,tknstate,i,retcod);
      N := i;
*)
    END ReadCard;

(************************************************************************)

PROCEDURE ReadLongReal(): LONGREAL;

VAR ch : CHAR;
(*
    s  : STRTYP;
    mybuf,token : BUFTYP;
    tknstate : FSATYP;
*)
    c,retcod  : CARDINAL;
    lr,result  : LONGREAL;
    i         : LONGINT;

    BEGIN
      FpuIO.ReadLongReal(lr);
      RETURN lr;
(*
   I'll use a library proc here as long as I have one.
      ReadString(s);
      ASSIGN2BUF(s,mybuf);
      IF mybuf.COUNT = 0 THEN RETURN 0.0 END;
      INI1TKN(mybuf);
      GETTKNREAL(token,tknstate,i,result,retcod);
      RETURN result;
*)
    END ReadLongReal;



(************************************************************************)
PROCEDURE WriteCx (Z: LONGCOMPLEX;  places: CARDINAL);

    (* Writes Z in Cartesian form, with "places" characters allowed     *)
    (* for each of the real and imaginary parts.                        *)

    VAR j: CARDINAL;  impart: LONGREAL;

    BEGIN
        WriteLongRealEng (RE(Z), places);
        IF IM(Z) = 0.0 THEN
            FOR j := 0 TO places+3 DO
                WriteString (" ");
            END (*FOR*);
        ELSE
            impart := IM(Z);
            IF impart < 0.0 THEN
                WriteString (" - ");
                impart := -impart;
            ELSE
                WriteString (" + ");
            END (*IF*);
            WriteString ("j");
            WriteLongRealEng (impart, places);
        END (*IF*);
    END WriteCx;

(************************************************************************)
BEGIN
END MiscStdInOutg.
