IMPLEMENTATION MODULE MiscStdInOut;
(*
  Revision History
  ----------------
  25 Sep 07 -- Made MiscM2 use std i/o and renamed module
   2 Apr 13 -- Defined SkipLine, which is needed for terminal input.
   3 Apr 13 -- Added WriteLongCard.  And ADDCOMMAS
   5 Jan 15 -- Removed references to the complex family.
  26 Apr 19 -- Changed cardinal conversion so that numbers < 10,000 do not show a comma.
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
    (* proc *)  ADR, MOVE, ADDADR;

IMPORT LongMath, STextIO, SWholeIO, SRealIO, SLongIO, Conversions,
        MemUtils, Strings, WholeStr, LWholeStr, RealStr, LongStr, LongConv;

FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, WinAttr,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, WindowTypes, SpecialKeys,
    GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, (* WriteString,*)
    WriteStringAt, WriteCellsAt, WriteCells, (* WriteLn,*) EraseToEOL, ChangeAttr,
    ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn,
    SetAutoScroll, WinShellToTextWindowMessage,
    MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect,
    GetBufferRect, EraseScreen, EraseRect, GetWinShellHandle, FindTextWindow,
    SetDisplayMode,GetDisplayMode,SetWindowEnable,
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage,
    IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars,
    SetScrollBarPos, SetWindowData, SetWindowDataNum, GetWindowData, GetWindowDataNum,
    GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos, CascadeWindow,
    SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo,
    SetScrollDisableWhenNone, SetActiveTabChild, SetTabChildPosition,
    GetForegroundWindow, SetForegroundWindow,
    SetTimer, KillTimer, DisplayHelp,
    Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;

FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,COPYRIGHT,FILLCHAR,ASSIGN2BUF;

IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT ASCII;
FROM Environment IMPORT GetCommandLine;
FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
FROM TIMLIBrevised IMPORT JULIAN,GREGORIAN,TIME2MDY;
(****************************************************************************)

  FROM Terminal IMPORT Read, (*WriteString, WriteLn, ReadChar, *) Write, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

(************************************************************************)

VAR
    (* The currently selected screen window. *)

    cw: Window;

(************************************************************************)
(*                      MATHEMATICAL FUNCTIONS                          *)
(************************************************************************)

PROCEDURE Exp (x: LONGREAL): LONGREAL;

    (* Exponential. *)

    BEGIN
        RETURN LongMath.exp(x);
    END Exp;

(************************************************************************)

PROCEDURE Log (x: LONGREAL): LONGREAL;

    (* Natural logarithm. *)

    BEGIN
        RETURN LongMath.ln(x);
    END Log;

(************************************************************************)

PROCEDURE Power (x, y: LONGREAL): LONGREAL;

    (* Computes x to the power of y. *)

    BEGIN
        RETURN LongMath.power(x,y);
    END Power;

(************************************************************************)

PROCEDURE Sin (x: LONGREAL): LONGREAL;

    (* Sine of x (radians). *)

    BEGIN
        RETURN LongMath.sin(x);
    END Sin;

(************************************************************************)

PROCEDURE Cos (x: LONGREAL): LONGREAL;

    (* Cosine of x (radians). *)

    BEGIN
        RETURN LongMath.cos(x);
    END Cos;

(************************************************************************)

PROCEDURE Sqrt (x: LONGREAL): LONGREAL;

    (* Square root. *)

    BEGIN
        RETURN LongMath.sqrt(x);
    END Sqrt;

(************************************************************************)

PROCEDURE ATan2 (x, y: LONGREAL): LONGREAL;

    (* Inverse tangent of y/x.  Result is in range -PI to PI. *)

    VAR result: LONGREAL;

    BEGIN
        IF x = 0.0 THEN
           IF y = 0.0 THEN RETURN 0.0
           ELSIF y < 0.0 THEN RETURN -0.5*PI
           ELSE RETURN 0.5*PI
           END (*IF*);
        ELSE
            result := LongMath.arctan (y/x);
            IF x < 0.0 THEN
                IF y >= 0.0 THEN result := PI - result
                ELSE result := result - PI
                END (*IF*);
            END (*IF*);
            RETURN result;
        END (*IF*);
    END ATan2;

(************************************************************************)
(*                      MISCELLANEOUS UTILITIES                         *)
(************************************************************************)

PROCEDURE BlockCopy (source, destination: ADDRESS;  bytecount: CARDINAL);

    (* Copies an array of bytes from the source address to the          *)
    (* destination address.                                             *)

    BEGIN
        MemUtils.MoveMem (destination, source, bytecount);
    END BlockCopy;

(************************************************************************)

PROCEDURE AddOffset (A: ADDRESS;  increment: CARDINAL): ADDRESS;

    (* Returns a pointer to the memory location whose physical address  *)
    (* is Physical(A)+increment.  It is assumed that the caller will    *)
    (* never try to run off the end of a segment.                       *)

    BEGIN
        RETURN ADDADR (A, increment);
    END AddOffset;

(************************************************************************)
(*                  NUMERIC-TO-STRING CONVERSION                        *)
(************************************************************************)

PROCEDURE LongRealToString (number: LONGREAL;
                                        VAR (*OUT*) buffer: ARRAY OF CHAR;
                                        fieldsize: CARDINAL);

    (* Converts the number to a decimal character string in array       *)
    (* "buffer", right-justified in a field of fieldsize characters.    *)
    (* The format depends on the size of the number relative to the     *)
    (* size of the buffer.                                              *)

    BEGIN
      IF (ABS(number) >= 1.) OR (number = 0.) THEN
        LongStr.RealToFixed (number, fieldsize, buffer);
      ELSE
        LongStr.RealToEng(number,fieldsize+2,buffer);
      END;
    END LongRealToString;

(*****************************************************************************)
PROCEDURE ADDCOMMAS(VAR INOUT STR : ARRAY OF CHAR);
VAR
  C,PTR,NCOM,NDGTS,NULLPOSN : CARDINAL;
BEGIN
  NDGTS := STRLENFNT(STR);
  PTR := NDGTS - 1;  (* Zero origin array as a param *)
  NCOM := PTR DIV 3;
  NULLPOSN := NDGTS + NCOM;
  IF NULLPOSN > HIGH(STR) THEN
(* {{{
    WriteString(' Cannot add commas because string is too small.');
    WriteLn;
}}} *)
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



PROCEDURE SelectWindow (w: Window);

    (* Specifies that all screen output, up until the next call to      *)
    (* SelectWindow, will be to window w.                               *)

    BEGIN
        cw := w;
    END SelectWindow;

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

    VAR dummyStr: STR10TYP;

    BEGIN
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
PROCEDURE WriteLongCard (n : LONGCARD);
    VAR s : STRTYP;

    BEGIN
      LWholeStr.LongCardToStr(n,s);
      IF n > 9999 THEN
        ADDCOMMAS(s);
      END;
      WriteString(s);
    END WriteLongCard;
(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);
VAR s : STRTYP;
    (* Writes a cardinal value. *)

    BEGIN
      WholeStr.CardToStr(N,s);
      IF N > 9999 THEN
        ADDCOMMAS(s);
      END;
      WriteString(s);
    END WriteCard;

(************************************************************************)
PROCEDURE WriteInt (i: INTEGER);
VAR s : STRTYP;
    (* Writes a cardinal value. *)

    BEGIN
      WholeStr.IntToStr(i,s);
      WriteString(s);
    END WriteInt;

(************************************************************************)

PROCEDURE WriteRJCard (number, fieldsize: CARDINAL);
    (* Like WriteCard, but the result is right justified in a field     *)
    (* of fieldsize characters.                                         *)

    VAR s : STRTYP;
        c : CARDINAL;

    BEGIN
(* {{{
        SWholeIO.WriteCard (number, fieldsize);
}}}*)
      WholeStr.CardToStr(number,s);
      WHILE LENGTH(s) < fieldsize DO
        Strings.Insert(' ',0,s);
      END;
      WriteString(s);
    END WriteRJCard;

(************************************************************************)

PROCEDURE WriteReal (x: REAL;  places: CARDINAL);
    (* Writes x in a field "places" characters wide. *)

    VAR s : STRTYP;
        c : CARDINAL;


    BEGIN
(*        SRealIO.WriteReal (x, places); *)
      IF (ABS(x) >= 1.) OR (x = 0.) THEN
        RealStr.RealToFixed(x,places,s);
      ELSE
        RealStr.RealToFloat(x,places,s);
      END;
      WriteString(s);
    END WriteReal;

(************************************************************************)

PROCEDURE WriteLongReal (x: LONGREAL;  places: CARDINAL);

    (* Writes x in a field "places" characters wide. *)
    (* Will now write a zero if abs(number) is a very small number, here *)
    (* if < 1.e-15, an arbitrary number I am using empirically by using  *)
    (* data from the debugger.  And will write a 1 if number is very close *)

    VAR buffer: ARRAY [0..127] OF CHAR;

    BEGIN
        (* I've scrapped my use of SLongIO here because it was giving wrong answers. *)
        (*SLongIO.WriteReal (x, places);*)
        IF ABS(x) < 1.E-14 THEN
          x := 0.

        ELSIF ABS((ABS(x) - 1.0)) < 1.E-13 THEN
          IF x > 0. THEN
            x := 1.0;
          ELSE
            x := -1.0;
          END;

        END;
        LongRealToString (x, buffer, places);
        WriteString (buffer);

    END WriteLongReal;

(************************************************************************)

PROCEDURE RtJust(VAR INOUT str : ARRAY OF CHAR; width : CARDINAL);
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


PROCEDURE ReadString(VAR OUT s : ARRAY OF CHAR);
(* For a full proc like this, see MiscM2 *)

BEGIN
  STextIO.ReadString(s);  (* I tried using ReadRestLine here, but this seemed to screw up subsequent attempts to read a string; Not sure why.   *)
  STextIO.SkipLine;
END ReadString;

PROCEDURE SkipLine;
(*
  Removes successive items from the default input stream up to and including the next line mark or until the end of input is reached.
  The read result is set to the value allRight, or endOfInput.  Not needed now that I am using
  STextIO.ReadRestLine in place of STextIO.ReadString.
*)
BEGIN
        STextIO.SkipLine;
END SkipLine;


PROCEDURE ReadCard (VAR (*OUT*) N: CARDINAL);
VAR ch : CHAR;
    s  : STRTYP;
    mybuf,token : BUFTYP;
    tknstate : FSATYP;
    c,retcod  : CARDINAL;
    i  : LONGINT;
    (* Reads a cardinal from the keyboard, echoing it to screen. *)

    BEGIN
      ReadString(s);
      ASSIGN2BUF(s,mybuf);
      IF mybuf.COUNT = 0 THEN
        N := 0;
        RETURN
      END;
      INI1TKN(mybuf);
      GETTKN(token,tknstate,i,retcod);
      N := i;

    END ReadCard;

(************************************************************************)

PROCEDURE ReadLongReal(): LONGREAL;

VAR ch : CHAR;
    s  : STRTYP;
    mybuf,token : BUFTYP;
    tknstate : FSATYP;
    c,retcod  : CARDINAL;
    r,result  : LONGREAL;
    i         : LONGINT;

    BEGIN
      ReadString(s);
      ASSIGN2BUF(s,mybuf);
      IF mybuf.COUNT = 0 THEN RETURN 0. END;
      INI1TKN(mybuf);
      GETTKNREAL(token,tknstate,i,result,retcod);
      RETURN result;
    END ReadLongReal;



(************************************************************************)

BEGIN
    cw := 0;
END MiscStdInOut.
