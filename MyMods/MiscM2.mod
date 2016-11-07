IMPLEMENTATION MODULE MiscM2;

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
(*
  REVISION HISTORY

   3 Sep 11 -- Changed behavior of ReadString so that if hit tab, a '*' appended to the string and the
                routine returns.
   3 Apr 13 -- Noticed that had already added WriteLongCard in the past here, but not to MiscStdInOut.
                ADDCOMMAS put here.
*)


FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  ADR, MOVE, ADDADR;

IMPORT LongMath, STextIO, SWholeIO, SRealIO, SLongIO, Conversions, MemUtils,
        Strings, WholeStr, LWholeStr, RealStr, LongStr, LongConv;

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
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
(****************************************************************************)

  FROM Terminal IMPORT Reset;
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
        LongStr.RealToFloat(number,fieldsize+2,buffer);
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
  Terminal.Reset;
  FOR c := 1 TO 25 DO
    WriteString('                                                                             ');
    WriteLn;
  END;
  Terminal.Reset;
END CLS;



PROCEDURE SelectWindow (w: Window);

    (* Specifies that all screen output, up until the next call to      *)
    (* SelectWindow, will be to window w.                               *)

    BEGIN
        cw := w;
    END SelectWindow;

(************************************************************************)

PROCEDURE WriteString (s: ARRAY OF CHAR);
    (* Writes s to the current window. *)

    BEGIN
      Terminal.WriteString(s);
    END WriteString;

(************************************************************************)

PROCEDURE WriteLn;
    (* Writes an end-of-line to the current window. *)

    BEGIN
      Terminal.WriteLn;
    END WriteLn;

(************************************************************************)
PROCEDURE WriteChar(ch : CHAR);
BEGIN
  Terminal.Write(ch);
END WriteChar;
(************************************************************************)

(************************************************************************)

PROCEDURE PressAnyKey;

    (* "Press any key to continue". *)
    (* Bug: this is requiring "Enter" before the character can  *)
    (* be read.  I'm not yet sure how to solve this.            *)
    (* As a temporary work-around, we require the user to       *)
    (* press the <Enter> key rather than the <Any> key.         *)

    VAR dummy: CHAR;

    BEGIN
        Terminal.WriteLn;
        Terminal.WriteString ("Press any key to continue");
(* Didn't work properly        REPEAT UNTIL Terminal.CharAvail(); *)
        dummy := Terminal.ReadChar();
        Terminal.WriteLn;
    END PressAnyKey;

(************************************************************************)

PROCEDURE Error (message: ARRAY OF CHAR);

    (* Puts a message to the screen. *)

    (* VAR w, save: Window; *)

    BEGIN
        (*
        save := cw;
        Windows.OpenWindow (w, Windows.black, Windows.green, 11, 14, 10, 69,
                                Windows.simpleframe, Windows.nodivider);
        SelectWindow (w);
        *)
        WriteString ("Error: ");  WriteString (message);
        PressAnyKey;
        (*
        Windows.CloseWindow (w);
        SelectWindow (save);
        *)
    END Error;

(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);
VAR s : STRTYP;
    (* Writes a cardinal value. *)

    BEGIN
      WholeStr.CardToStr(N,s);
      Terminal.WriteString(s);
(*        SWholeIO.WriteCard (N, 8); *)
    END WriteCard;

(************************************************************************)
PROCEDURE WriteInt (i: INTEGER);
VAR s : STRTYP;
    (* Writes a cardinal value. *)

    BEGIN
      WholeStr.IntToStr(i,s);
      Terminal.WriteString(s);
    END WriteInt;

(************************************************************************)
PROCEDURE WriteLongCard (lc: LONGCARD);
VAR s: STRTYP;

BEGIN
  LWholeStr.LongCardToStr(lc, s);
  ADDCOMMAS(s);
  Terminal.WriteString(s);
END WriteLongCard;


(************************************************************************)

PROCEDURE WriteRJCard (number, fieldsize: CARDINAL);

    (* Like WriteCard, but the result is right justified in a field     *)
    (* of fieldsize characters.                                         *)
VAR s : STRTYP;
    c : CARDINAL;
    BEGIN
(*        SWholeIO.WriteCard (number, fieldsize); *)
      WholeStr.CardToStr(number,s);
      WHILE LENGTH(s) < fieldsize DO
        Strings.Insert(' ',0,s);
      END;
      WriteString(s);
    END WriteRJCard;

(************************************************************************)

PROCEDURE WriteReal (x: REAL;  places: CARDINAL);
VAR s : STRTYP;
    c : CARDINAL;

    (* Writes x in a field "places" characters wide. *)

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
PROCEDURE Read() : CHAR;
                         (* I reversed the roles of these functions to be more consistent w/  expectations *)
BEGIN
  RETURN(Terminal.ReadChar());
END Read;
(************************************************************************)
PROCEDURE ReadChar(VAR OUT ch: CHAR);
                         (* I reversed the roles of these functions to be more consistent w/  expectations *)
BEGIN
  Terminal.Read(ch);
END ReadChar;
(************************************************************************)
PROCEDURE ReadString(VAR OUT s : ARRAY OF CHAR);
VAR ch : CHAR;

    mybuf,token : BUFTYP;
    tknstate : FSATYP;
    c,retcod  : CARDINAL;
    i  : LONGINT;
    (* Reads a string from the keyboard, echoing it to screen. *)
BEGIN
      s := '';
      c := 0;
      Terminal.Read(ch);
      WHILE ch <> Terminal.Enter DO
        Terminal.Write(ch);
        IF (ch = Terminal.BackSpace) OR (ch = Terminal.CursorLeft) THEN
          DEC(c);
          Strings.Delete(s,c,1);
          Terminal.Write(' ');
          Terminal.Write(ch);
        ELSIF ch = Terminal.Tab THEN
        	Strings.Append('*',s);
        	RETURN;
        ELSIF ch = Terminal.Escape THEN
        	s := '';
        	RETURN;
        ELSE
          Strings.Append(ch,s);
          INC(c);
        END(*if*);
      Terminal.Read(ch);
      END(*while*);
END ReadString;

PROCEDURE ReadCard (VAR (*OUT*) N: CARDINAL);
VAR ch : CHAR;
    s  : STRTYP;
    mybuf,token : BUFTYP;
    tknstate : FSATYP;
    c,retcod  : CARDINAL;
    i  : LONGINT;
    res : WholeStr.ConvResults;
    (* Reads a cardinal from the keyboard, echoing it to screen. *)

    BEGIN
      ReadString(s);
      WholeStr.StrToCard(s, c, res);
      N := c;
(*
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
PROCEDURE WriteCx (Z: LONGCOMPLEX;  places: CARDINAL);

    (* Writes Z in Cartesian form, with "places" characters allowed     *)
    (* for each of the real and imaginary parts.                        *)

    VAR j: CARDINAL;  impart: LONGREAL;

    BEGIN
        WriteLongReal (RE(Z), places);
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
            WriteLongReal (impart, places);
        END (*IF*);
    END WriteCx;

BEGIN
    cw := 0;
END MiscM2.
