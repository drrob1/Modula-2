(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA *)

IMPLEMENTATION MODULE CursesWindows ;


FROM SysStorage IMPORT ALLOCATE, DEALLOCATE ;
FROM ASCII IMPORT cr, lf, bs ;
FROM StrLib IMPORT StrLen, StrCopy ;
FROM NumberIO IMPORT WriteCard, WriteHex ;
FROM SYSTEM IMPORT TurnInterrupts ;
FROM COROUTINES IMPORT PROTECTION ;

FROM libc IMPORT printf ;
FROM M2RTS IMPORT InstallTerminationProcedure, Halt ;

IMPORT ncurses;
FROM ncurses IMPORT (* CONST *) Black,Red,Green,Yellow,Blue,Magenta,Cyan,White, 
  (* TYPE *) WINDOW,chtype,chstring,ATTRIBUTE,COLORPAIR,
  (* VAR *) LINES,COLS,TABSIZE,ESCDELAY,COLORS,COLOR_PAIRS,stdscr,curscr,newscr, ttytype,
  (* PROCs*) curses_version,assume_default_colors,use_default_colors,start_color,resizeterm,
             wresize,addch,addchnstr,addchstr,addnstr,addstr,has_colors,getch,wattr_on,wattr_off,wattr_set,wattr_get,
             baudrate,beep,bkgd,bkgdset,border,box,can_change_color,cbreak,clear,wclear,wrefresh,wmove,wcolor_set,
             initscr,savetty,resetty,delwin,endwin,isendwin,init_pair,noecho,nonl,getch,getstr,getnstr,nl,nocbreak,
             raw,noraw,redrawwin,refresh,wattr_on,trace,wgetch,wgetnstr,wgetstr,werase,chgat,wchgat,clearok,clrtobot,clrtoeol;


(*
  This is from Stony Brook Modula-2

FROM TextWindow IMPORT
    /* TYPES & CONSTS */
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, FontInfo, WinAttr, ClipboardFormat,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow, NormalChildWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo, WindowTypes,
    /* VARS */
    /* PROCS */
    ComposeAttribute, CreateWindow, SpecialKeys,
    GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, WriteString,
    WriteStringAt, WriteCellsAt, WriteCells, WriteLn, EraseToEOL, ChangeAttr,
    ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn,
    SetAutoScroll, WinShellToTextWindowMessage,
    MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect,
    GetBufferRect, EraseScreen, EraseRect, GetWinShellHandle, FindTextWindow,
    SetDisplayMode,GetDisplayMode,SetWindowEnable, (*  SetWindowTitle, *)
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage,
    IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars,
    SetScrollBarPos, SetWindowData, SetWindowDataNum, GetWindowData, GetWindowDataNum,
    GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos, CascadeWindow,
    SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo,
    SetScrollDisableWhenNone, SetActiveTabChild, SetTabChildPosition,
    GetForegroundWindow, SetForegroundWindow, SetWindowFont,
    SetTimer, KillTimer, DisplayHelp, SetWindowIcon,
    OpenClipboard, CloseClipboard, EmptyClipboard, ClipboardFormatAvailable,
    AllocClipboardMemory, UnlockClipboardMemory, SetClipboard, GetClipboard,
    Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;
*)


CONST
   A_REVERSE = 10 ;
   MaxWidth   = 78 ;
   MaxHeight  = 24 ;

(*
   The type Window is semi device specific. The parameters to control
   the window are NOT specific but the implementation of Window may
   change slightly.
*)

TYPE
   CursorType = (none, small, large) ;

   Window = POINTER TO window ;
   window = RECORD
               Xoffset : CARDINAL ;   (* x position of bottom left text  *)
               Yoffset : CARDINAL ;   (* y position of bottom right text *)
               Width   : CARDINAL ;   (* Width of text                   *)
               Height  : CARDINAL ;   (* Height of text                  *)
               Xcursor : CARDINAL ;   (* x position of cursor in window  *)
               Ycursor : CARDINAL ;   (* y position of cursor in window  *)
               BgCol   : CARDINAL ;
               FgCol   : CARDINAL ;
               Attrib  : COLORPAIR ;  (* Attribute made from Bg and Fg   *)
               Up      : Window ;     (* Pointer to upwards visability   *)
               Down    : Window ;     (* Pointer to downwards visability *)
               Cur     : CursorType ;
               Border  : BOOLEAN ;    (* Determines if a Border exists   *)
               Display : ARRAY [0..MaxWidth] , [0..MaxHeight] OF CHAR ;
               Title   : ARRAY [0..MaxWidth] OF CHAR ;
            END ;

VAR
   w   : WINDOW ;
   pair: CARDINAL ;
   Top       : Window ;      (* Top Window Pointer *)
   Default   : Window ;      (* The default window *)
   BorderCol: COLORPAIR ;   (* Color of the boarders *)


PROCEDURE Init ;
VAR
   r: INTEGER ;
BEGIN
   w := initscr() ;
   r := start_color();
   r := wclear(w) ;
   r := wrefresh(w) ;
   r := cbreak() ;
   r := noecho();
   r := nonl() ;
   pair := 1 ;
   IF NOT InstallTerminationProcedure(ResetScreen)
   THEN
      HALT
   END
END Init ;


(*
   ResetScreen - 
*)
PROCEDURE ResetScreen ;
VAR
   r: INTEGER ;
BEGIN
   IF NOT isendwin()
   THEN
      r := endwin()
   END
END ResetScreen ;


(*
   MoveTo - moves to position, x, y on the screen.
*)
PROCEDURE MoveTo (x, y: CARDINAL) ;
VAR
   r: INTEGER ;
   i: SHORTCARD ;
BEGIN
   r := wrefresh(w) ;
   r := wmove(w, y, x) ;
   r := wrefresh(w)
END MoveTo ;


(*
   CreateAttributeColor - returns a COLORPAIR created from two colors.
*)
PROCEDURE CreateAttributeColor (bg, fg: CARDINAL) : COLORPAIR ;
VAR
   cp: COLORPAIR ;
   r : INTEGER ;
BEGIN
   cp := pair ;
   INC(pair) ;
   r := init_pair(cp, VAL(SHORTCARD, fg), VAL(SHORTCARD, bg)) ;
   RETURN( cp )
END CreateAttributeColor ;


(*
   AddColorPairToChar - returns a ncurses chtype which is created
                        from a CHAR and COLORPAIR.
*)
PROCEDURE AddColorPairToChar (ch: CHAR; p: COLORPAIR) : chtype ;
VAR
   c: chtype ;
BEGIN
   c := VAL(CARDINAL, p) * 0100H + ORD(ch) ;
   RETURN( c )
END AddColorPairToChar ;


(*
   WriteChar - writes out a character.
*)
PROCEDURE WriteChar (c: chtype) ;
VAR
   r: INTEGER ;
BEGIN
   r := waddch(w, c) ;
   r := wrefresh(w) ;
END WriteChar ;

(*
   InitWindow - returns a Window handle. This Window is uninitialized.
*)
PROCEDURE InitWindow () : Window ;
VAR
   OldInterruptState: PROTECTION ;
   w                : Window ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   NEW(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( w )
END InitWindow ;


(*
   KillWindow - destroys a Window, w, and returns NIL.
*)
PROCEDURE KillWindow (w: Window) : Window ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   DISPOSE( w ) ;
   w := NIL ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( NIL )
END KillWindow ;


(*
   SetWindow - sets a Window, w, to contain background Color, bg,
               foreground Color, fg. The width, height are specified
               and border indicates whether the window has a Grey border.
               The Window, w, is returned.
*)
PROCEDURE SetWindow (w: Window; bg, fg: CARDINAL;
                     width, height, x, y: CARDINAL;
                     border: BOOLEAN) : Window ;
VAR
   i, j             : CARDINAL ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      Xoffset := x ;
      Yoffset := y ;
      Width := width ;
      Height := height ;
      BgCol := bg ;
      FgCol := fg ;
      Attrib := CreateAttributeColor(bg, fg) ;
      Border := border ;
      Xcursor := 0 ;
      Ycursor := 0 ;
      StrCopy( '', Title ) ;
      Cur := large ;
   END ;
   FOR i := 0 TO width DO
      FOR j := 0 TO height DO
         w^.Display[i, j] := ' ' ;
      END
   END ;
   AddWindow(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( w )
END SetWindow ;


(*
   AddWindow - adds a Window, w, to the display.
*)
PROCEDURE AddWindow (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   AddToList( Top, w ) ;
   RefreshWindow( w ) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END AddWindow ;


(*
   SubWindow - subtracts a Window, w, from the display.
*)
PROCEDURE SubWindow (w: Window) ;
VAR
   t                : Window ;
   x, y, j, l       : CARDINAL ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   t := w^.Down ;
   IF t=Top
   THEN
      t := NIL
   END ;
   x := WindowXoffset(w) ;
   y := WindowYoffset(w) ;
   l := WindowWidth(w) ;
   SubFromList( Top, w ) ;
   FOR j := 0 TO WindowHeight(w) DO
      WriteScreen( Top, x, y+j, l )
   END ;
   IF Top=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'Window error: SubWindow there are no more windows left')
   ELSE
(*    WHILE t#Top DO
         RefreshWindow( t ) ;
         t := t^.Down
      END
*) END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END SubWindow ;


(*
   NulWindow - returns TRUE if Window, w, is NIL.
               (Meaning it does not exist).
*)
PROCEDURE NulWindow (w: Window) : BOOLEAN ;
BEGIN
   RETURN( w=NIL )
END NulWindow ;


(*
   MoveCursor - moves the cursor of Window, w, to x, y.
*)
PROCEDURE MoveCursor (w: Window; x, y: CARDINAL) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      Xcursor := x ;
      Ycursor := y
   END ;
   UpdateCursor(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END MoveCursor ;


PROCEDURE LargeCursor (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   w^.Cur := large ;
   UpdateCursor(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END LargeCursor ;


PROCEDURE SmallCursor (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   w^.Cur := small ;
   UpdateCursor(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END SmallCursor ;


PROCEDURE NoCursor (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   w^.Cur := none ;
   UpdateCursor(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END NoCursor ;


PROCEDURE ClearWindow (w: Window) ;
VAR
   i, j             : CARDINAL ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      FOR j := 0 TO Height DO
         FOR i := 0 TO Width DO
            Display[i, j] := ' '
         END
      END
   END ;
   RefreshWindow(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END ClearWindow ;


(*
   MoveWindow - moves a Window, w, to position, x, y.
                The Window must have been removed from the display
                by SubWindow before this is called.
*)
PROCEDURE MoveWindow (w: Window; x, y: CARDINAL) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      Xoffset := x ;
      Yoffset := y ;
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END MoveWindow ;


(*
   DefaultWindow - returns the default window.
*)
PROCEDURE DefaultWindow () : Window ;
VAR
   OldInterruptState: PROTECTION ;
   w                : Window ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   w := Default ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( w )
END DefaultWindow ;


(*
   SetDefaultWindow - sets the default window.
*)
PROCEDURE SetDefaultWindow (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   Default := w ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END SetDefaultWindow ;


(*
   WriteChar - writes a character, ch, to Window, w.
*)
PROCEDURE WriteChar (w: Window; ch: CHAR) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   IF w#NIL
   THEN
      PerformWriteChar (w, ch)
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END WriteChar ;


PROCEDURE PerformWriteChar (w: Window; ch: CHAR) ;
BEGIN
   WITH w^ DO
      IF ch=cr
      THEN
         Xcursor := 0
      ELSIF ch=lf
      THEN
         IncYcursor(w)
      ELSIF ch=bs
      THEN
         IF Xcursor>0
         THEN
            DEC(Xcursor)
         ELSE
            IF Ycursor>0
            THEN
               DEC(Ycursor) ;
               Xcursor := Width
            END
         END
      ELSE
         Display[Xcursor, Ycursor] := ch ;
         RefreshChar(w, Xcursor, Ycursor) ;
         INC(Xcursor) ;
         IF Xcursor>Width
         THEN
            Xcursor := 0 ;
            IncYcursor(w)
         END
      END ;
      RefreshChar(w, Xcursor, Ycursor) ;
      UpdateCursor(w)
   END
END PerformWriteChar ;


PROCEDURE IncYcursor (w: Window) ;
BEGIN
   WITH w^ DO
      IF Ycursor<Height
      THEN
         INC(Ycursor)
      ELSE
         (* Ycursor = Height  therefore scroll up *)
         ScrollUp(w)
      END
   END
END IncYcursor ;


PROCEDURE ScrollUp (w: Window) ;
VAR
   i, j: CARDINAL ;
BEGIN
   WITH w^ DO
      IF Height>0
      THEN
         FOR j := 1 TO Height DO
            FOR i := 0 TO Width DO
               Display[i, j-1] := Display[i, j]
            END
         END ;
         FOR i := 0 TO Width DO
            Display[i, Height] := ' '
         END ;
         RefreshWindow(w)
      END
   END
END ScrollUp ;


(*
   WriteString - writes a string, a, to window, w.
*)
PROCEDURE WriteString (w: Window; a: ARRAY OF CHAR) ;
VAR
   i, j: CARDINAL ;
BEGIN
   j := StrLen(a) ;
   i := 0 ;
   WHILE i<=j DO
      WriteChar(w, a[i]) ;
      INC(i)
   END
END WriteString ;


(*
   WriteLn - places the cursor onto the beginning of a new line
             in Window, w.
*)
PROCEDURE WriteLn (w: Window) ;
BEGIN
   WriteChar(w, cr) ;
   WriteChar(w, lf)
END WriteLn ;


PROCEDURE ReadChar (w: Window; VAR ch: CHAR) ;
BEGIN
END ReadChar ;


(*
   ColorWindow - alters the foreground and background
                  Color of Window, w.
*)
PROCEDURE ColorWindow (w: Window; bg, fg: CARDINAL) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      FgCol := fg ;
      BgCol := bg ;
      Attrib := CreateAttributeColor(bg, fg)
   END ;
   RefreshWindow(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END ColorWindow ;


(*
   SizeWindow - not implemented.
*)
PROCEDURE SizeWindow (w: Window; width, height: CARDINAL) ;
BEGIN
END SizeWindow ;


(*
   PutOnTop - places Window, w, on top of the pile of Windows.
*)
PROCEDURE PutOnTop (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   Top := w ;
   RefreshWindow(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END PutOnTop ;


(*
   PutOnBottom - places Window, w, on the bottom of the pile
                 of Windows.
*)
PROCEDURE PutOnBottom (w: Window) ;
VAR
   j, l, x, y       : CARDINAL ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   SubFromList( Top, w ) ;
   AddToList( Top, w ) ;
   l := WindowWidth(w) ;
   y := WindowYoffset(w) ;
   x := WindowXoffset(w) ;
   FOR j := 0 TO WindowHeight(w) DO
      WriteScreen( Top, x, y+j, l )
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END PutOnBottom ;


(*
   SelectWindow - returns a Window which can be seen at screen
                  location, x, y.
                  If no Window is seen then NIL is returned.
*)
PROCEDURE SelectWindow (x, y: CARDINAL) : Window ;
VAR
   t                : Window ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   t := Top ;
   WHILE (t#NIL) AND (NOT InsideWindow( t, x, y )) DO
      t := t^.Down ;
      IF t=Top
      THEN
         t := NIL
      END
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( t )
END SelectWindow ;


(*
   TitleWindow - adds a title to a Window, w.
*)
PROCEDURE TitleWindow (w: Window; a: ARRAY OF CHAR) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   StrCopy(a, w^.Title) ;
   RefreshTitle(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END TitleWindow ;


(*
   Misc Utilities - Device Independant
*)

(*
   WindowHeight returns the height of the window, w.
   The height is determined by the height field and
   whether a border is arround the window.
   NOTE that the WindowHeight includes the border.
*)
PROCEDURE WindowHeight (w: Window) : CARDINAL ;
BEGIN
   WITH w^ DO
      IF Border
      THEN
         RETURN( Height+2 )
      ELSE
         RETURN( Height )
      END
   END
END WindowHeight ;


PROCEDURE WindowWidth (w: Window) : CARDINAL ;
BEGIN
   WITH w^ DO
      IF Border
      THEN
         RETURN( Width+2 )
      ELSE
         RETURN( Width )
      END
   END
END WindowWidth ;


PROCEDURE WindowXoffset (w: Window) : CARDINAL ;
BEGIN
   WITH w^ DO
      IF Border
      THEN
         RETURN( Xoffset-1 )
      ELSE
         RETURN( Xoffset )
      END
   END
END WindowXoffset ;


PROCEDURE WindowYoffset (w: Window) : CARDINAL ;
BEGIN
   WITH w^ DO
      IF Border
      THEN
         RETURN( Yoffset-1 )
      ELSE
         RETURN( Yoffset )
      END
   END
END WindowYoffset ;


PROCEDURE UpdateCursor (w: Window) ;
BEGIN
(*
   WITH w^ DO
      IF Cur=none
      THEN
         SetCursorType( 0, 0 )
      ELSIF Cur=small
      THEN
         SetCursorType( 6, 7 )
      ELSE
         SetCursorType( 0, 7 )
      END
   END
*)
END UpdateCursor ;


PROCEDURE RefreshWindow (w: Window) ;
VAR
   i: CARDINAL ;
BEGIN
   IF w^.Border
   THEN
      RefreshTitle( w ) ;
      RefreshBorder( w )
   END ;
   WITH w^ DO
      FOR i := 0 TO Height DO
         WriteWindow( w, 0, i, Width )
      END
   END
END RefreshWindow ;


PROCEDURE RefreshTitle (w: Window) ;
VAR
   i, j,
   x, y, y1: CARDINAL ;
BEGIN
   IF w^.Border
   THEN
      y1 := WindowWidth( w ) ;
      i := StrLen(w^.Title) ;
      IF y1<i
      THEN
         i := y1
      END ;
      x := WindowXoffset( w ) ;
      y := WindowYoffset( w ) ;
      IF i>0
      THEN
         FOR j := 1 TO i DO
            IF VisableAfter( w, x+j, y )
            THEN
               (* Graphics procedure to write ch at x, y *)
               (* need to send PageNo and Grey...        *)

               WriteCharAbs( w^.Title[j-1], BorderCol, x+j, y )
            END
         END
      END ;
      j := i+1 ;
      WHILE j<y1 DO
         IF VisableAfter( w, x+j, y )
         THEN
            WriteCharAbs( ' ', BorderCol, x+j, y ) ;
         END ;
         INC( j )
      END
   END
END RefreshTitle ;


(* x, y are absolute; returns TRUE if no Window ABOVE w masks x, y *)
(* from being displayed.                                           *)

PROCEDURE VisableAfter (w: Window; x, y: CARDINAL) : BOOLEAN ;
VAR
   t : Window ;
   ok: BOOLEAN ;
BEGIN
   t := w ;
   ok := TRUE ;
   WHILE ok AND (t#Top) DO
      t := t^.Up ;
      ok := NOT InsideWindow( t, x, y )
   END ;
   RETURN( ok )
END VisableAfter ;


(* RefreshBorder re-draws the boarder for window, w. Only draws  *)
(* three sides, Left, Right and Bottom since RefreshTitle draws   *)
(* the top Border. Also can only be called if the window, w, has *)
(* a Border.                                                     *)

PROCEDURE RefreshBorder (w: Window) ;
BEGIN
   WriteBorderVert( w, w, WindowXoffset(w), WindowYoffset(w),
                     WindowHeight(w) ) ;
   WriteBorderVert( w, w, WindowXoffset(w)+WindowWidth(w), WindowYoffset(w),
                     WindowHeight(w) ) ;
   WriteBorderHoriz( w, w, WindowXoffset(w), WindowYoffset(w)+WindowHeight(w),
                            WindowWidth(w) )
END RefreshBorder ;


(* WriteBorderHoriz draws a boarder.                             *)

PROCEDURE WriteBorderHoriz (w, wt: Window; x, y, l: CARDINAL) ;
VAR
   xo, yo,
   hi, wi: CARDINAL ;
BEGIN
   IF wt=Top
   THEN
      HorizLine( x, y, l )
   ELSE
      wt := wt^.Up ;
      (* Check to see if wt masks out w *)
      (* Ie if text is not interfered by Window of wt *)

      xo := WindowXoffset( wt ) ;
      yo := WindowYoffset( wt ) ;
      hi := WindowHeight( wt ) ;
      wi := WindowWidth( wt ) ;

      IF (y>yo+hi) OR (y<yo)
      THEN
         WriteBorderHoriz( w, wt, x, y, l )
      ELSIF (x>xo+wi) OR (x+l<xo)
      THEN
         WriteBorderHoriz( w, wt, x, y, l )
      ELSE
         (* wt is on top of x, y, l on w *)
         IF x<xo
         THEN
            WriteBorderHoriz( w, wt, x, y, xo-x )
         END ;
         IF x+l>xo+wi
         THEN
            WriteBorderHoriz( w, wt, xo+wi+1, y, x+l-(xo+wi+1) )
         END
      END
   END
END WriteBorderHoriz ;


(* WriteBorderVert draws a boarder. x, y are all Absolute to the *)
(* screen.                                                        *)

PROCEDURE WriteBorderVert (w, wt: Window; x, y, l: CARDINAL) ;
VAR
   xo, yo,
   hi, wi: CARDINAL ;
BEGIN
   IF wt=Top
   THEN
      VertLine( x, y, l )
   ELSE
      wt := wt^.Up ;
      (* Check to see if wt masks out w *)
      (* Ie if text is not interfered by Window of wt *)

      xo := WindowXoffset( wt ) ;
      yo := WindowYoffset( wt ) ;
      hi := WindowHeight( wt ) ;
      wi := WindowWidth( wt ) ;

      IF (x>xo+wi) OR (x<xo)
      THEN
         WriteBorderVert( w, wt, x, y, l )
      ELSIF (y>yo+hi) OR (y+l<yo)
      THEN
         WriteBorderVert( w, wt, x, y, l )
      ELSE
         (* wt is on top of x, y, l on w *)
         IF y<yo
         THEN
            WriteBorderVert( w, wt, x, y, yo-y )
         END ;
         IF y+l>yo+hi
         THEN
            WriteBorderVert( w, wt, x, yo+hi+1, y+l-(yo+hi+1) )
         END
      END
   END
END WriteBorderVert ;


PROCEDURE InsideWindow (w: Window; x, y: CARDINAL) : BOOLEAN ;
VAR
   xo, yo,
   hi, wi: CARDINAL ;
BEGIN
   xo := WindowXoffset( w ) ;
   yo := WindowYoffset( w ) ;
   hi := WindowHeight( w ) ;
   wi := WindowWidth( w ) ;
   RETURN( (xo<=x) AND (xo+wi>=x) AND
           (yo<=y) AND (yo+hi>=y) )
END InsideWindow ;


PROCEDURE RefreshChar (w: Window; x,y: CARDINAL) ;
VAR
   ok: BOOLEAN ;
   t : Window ;
BEGIN
   ok := TRUE ;
   t := w ;
   WHILE ok AND (t#Top) DO
      t := t^.Up ;
      ok := NOT InsideWindow( t, w^.Xoffset+x, w^.Yoffset+y )
   END ;
   IF ok
   THEN
      WriteCharAt( w, x, y )
   END
END RefreshChar ;


(*
   WriteScreen - refreshes the screen that is in position, x, y with,
                 l, characters. It starts with window, w, and looks
                 downward. If no window now covers the screen black
                 is written to the screen.
*)

PROCEDURE WriteScreen (w: Window; x, y, l: CARDINAL) ;
VAR
   i, j, k,
   xo, yo,
   wi, hi: CARDINAL ;
BEGIN
   IF w=NIL
   THEN
      FOR i := x TO x+l DO
         WriteCharAbs( ' ', Black, i, y )
      END
   ELSE
      xo := WindowXoffset(w) ;
      yo := WindowYoffset(w) ;
      hi := WindowHeight(w) ;
      wi := WindowWidth(w) ;
      (* Check for too high or too low, left and right *)
      IF (y>yo+hi) OR (y<yo) OR
         (x+l<xo) OR (x>xo+wi)
      THEN
         IF w^.Down=Top
         THEN
            WriteScreen( NIL, x, y, l )
         ELSE
            WriteScreen( w^.Down, x, y, l )
         END
      ELSE
         IF x<xo
         THEN
            j := xo ;
            IF w^.Down=Top
            THEN
               WriteScreen(NIL, x, y, j-x-1)
            ELSE
               WriteScreen(w^.Down, x, y, j-x-1)
            END
         ELSE
            j := x
         END ;
         IF x+l<xo+wi
         THEN
            k := x+l
         ELSE
            k := xo+wi ;
            IF w^.Down=Top
            THEN
               WriteScreen(NIL, k+1, y, x+l-k)
            ELSE
               WriteScreen(w^.Down, k+1, y, x+l-k)
            END
         END ;
         (* Check for Border *)
         IF w^.Border
         THEN
            IF (y=yo) OR (y=yo+hi)
            THEN
               HorizLine( j, y, k-j )
            ELSE
               IF xo=j
               THEN
                  WriteCharAbs(' ', BorderCol, j, y) ;
                  INC(j)
               END ;
               IF xo+wi=k
               THEN
                  WriteCharAbs(' ', BorderCol, k, y) ;
                  DEC(k)
               END ;
               IF k>=j
               THEN
                  WriteAt( w, j-w^.Xoffset, y-w^.Yoffset, k-j )
               END
            END
         ELSIF k>=j
         THEN
            WriteAt( w, j-w^.Xoffset, y-w^.Yoffset, k-j )
         END
      END
   END
END WriteScreen ;


(*
   WriteWindow - Updates the display screen, trying from window, w,
                 at x, y with l, characters. Only updates the screen
                 if any of these specified characters are not hidden
                 by above windows.
*)
PROCEDURE WriteWindow (w: Window; x, y, l: CARDINAL) ;
BEGIN
   WriteUp( w, w, x, y, l )
END WriteWindow ;


(*
   WriteUp - Checks above windows attempting to write characters
             if they are visable. Window, w, has l, characters at x, y
             which want to be shown. Window wt, has allowed x, y, l to be
             shown. Parameters x, y and l are all relative to w.
*)
PROCEDURE WriteUp (w, wt: Window; x, y, l: CARDINAL) ;
VAR
   xo, yo,
   hi, wi: CARDINAL ;
BEGIN
   IF wt=Top
   THEN
      WriteAt( w, x, y, l )
   ELSE
      wt := wt^.Up ;
      (* Check to see if wt masks out w *)
      (* Ie if text is not interfered by Window of wt *)

      xo := WindowXoffset( wt ) ;
      yo := WindowYoffset( wt ) ;
      hi := WindowHeight( wt ) ;
      wi := WindowWidth( wt ) ;

      IF (w^.Yoffset+y>yo+hi) OR
         (w^.Yoffset+y<yo)
      THEN
         WriteUp( w, wt, x, y, l )
      ELSIF (w^.Xoffset+x>xo+wi) OR
            (w^.Xoffset+x+l<xo)
      THEN
         WriteUp( w, wt, x, y, l )
      ELSE
         (* wt is on top of x, y, l on w *)
         IF w^.Xoffset+x<xo
         THEN
            WriteUp( w, wt, x, y, xo-(w^.Xoffset+x+1) )
         END ;
         IF w^.Xoffset+x+l>xo+wi
         THEN
            WriteUp( w, wt, xo+wi+1-w^.Xoffset, y, w^.Xoffset+x+l-(xo+wi+1) )
         END
      END
   END
END WriteUp ;


(*
   AddTolist - adds t, to list, Head. It places t at the end of the list.
*)
PROCEDURE AddToList (VAR Head: Window ; t: Window) ;
BEGIN
   IF Head=NIL
   THEN
      Head := t ;
      t^.Up := t ;
      t^.Down := t
   ELSE
      t^.Down := Head ;
      t^.Up := Head^.Up ;
      Head^.Up^.Down := t ;
      Head^.Up := t
   END
END AddToList ;


PROCEDURE SubFromList (VAR Head: Window ; t: Window) ;
BEGIN
   IF (t^.Up=t) AND (t=Head)
   THEN
      Head := NIL
   ELSE
      IF Head=t
      THEN
         Head := Head^.Down
      END ;
      t^.Up^.Down := t^.Down ;
      t^.Down^.Up := t^.Up
   END
END SubFromList ;


(*
   The following procedures may be device specific, certainly Window type specific.
*)


(*
   WriteAt - writes a windows display at x, y, onto the screen for l characters. The x, y are relative to the window.
             Disregarding window ordering.
*)

PROCEDURE WriteAt (w: Window; x, y, l: CARDINAL) ;
VAR
   i, xo, yo: CARDINAL ;
BEGIN
   WITH w^ DO
      xo := Xoffset ;
      yo := Yoffset ;
      FOR i := 0 TO l DO
         WriteCharAbs(Display[x+i, y], Attrib, xo+x+i, yo+y)
      END
   END
END WriteAt ;


PROCEDURE WriteCharAt (w: Window; x, y: CARDINAL) ;
BEGIN
   WITH w^ DO
      WriteCharAbs(Display[x, y], Attrib, Xoffset+x, Yoffset+y)
   END
END WriteCharAt ;


(*
   WriteCharAbs - the interface to the Color text subsystem.
*)

PROCEDURE WriteCharAbs (ch: CHAR; col: COLORPAIR; x, y: CARDINAL) ;
BEGIN
(*
   StrIO.WriteString('x  =') ; WriteCard(x, 4) ;
   StrIO.WriteString('y  =') ; WriteCard(y, 4) ;
   StrIO.WriteString('ch =') ; StdIO.Write(ch) ;
   StrIO.WriteString('col=') ; WriteHex(col, 4) ; StrIO.WriteLn ;
*)
   MoveTo(x, y) ;
   ColorText.WriteChar(AddColorPairToChar(ch, col))
END WriteCharAbs ;


PROCEDURE HorizLine (x, y, l: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   IF l>=0
   THEN
      (* TermCap.MoveCursor( x, y ) ; *)
      FOR i := 0 TO l DO
         WriteCharAbs(' ', BorderCol, x+i, y)
      END
   END
END HorizLine ;


PROCEDURE VertLine (x, y, l: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   IF l>=0
   THEN
      FOR i := 0 TO l DO
         WriteCharAbs(' ', BorderCol, x, y+i)
      END
   END
END VertLine ;


BEGIN
   Top := NIL ;
   Default := NIL ;
   BorderCol := CreateAttributeColor(White, Black);
   Init;
END CursesWindows.

(*
DEFINITION MODULE FOR "C" ncurses ;


FROM SYSTEM IMPORT ADDRESS ;

CONST
   Black   = 0 ;
   Red     = 1 ;
   Green   = 2 ;
   Yellow  = 3 ;
   Blue    = 4 ;
   Magenta = 5 ;
   Cyan    = 6 ;
   White   = 7 ;

TYPE
   WINDOW = ADDRESS ;
   chtype = LONGCARD ;
   chstring = ADDRESS ;
   ATTRIBUTE = chtype ;
   COLORPAIR = SHORTCARD ;

VAR
   LINES, COLS, TABSIZE,
   ESCDELAY,
   COLORS, COLOR_PAIRS   : INTEGER ;
   stdscr, curscr, newscr: WINDOW ;
   ttytype               : ADDRESS ;


PROCEDURE   curses_version ()                            : ADDRESS ;
PROCEDURE   assume_default_colors (a, b: INTEGER)        : INTEGER ;
PROCEDURE   use_default_colors ()                        : INTEGER ;
PROCEDURE   start_color ()                               : INTEGER ;
PROCEDURE   resizeterm (x: INTEGER; y: INTEGER)          : INTEGER ;
PROCEDURE   wresize (w: WINDOW; x: INTEGER; y: INTEGER)  : INTEGER ;
PROCEDURE   addch (ch: chtype)                           : INTEGER ;
PROCEDURE   addchnstr (s: chstring; n: INTEGER)		 : INTEGER ;
PROCEDURE   addchstr (s: chstring)                       : INTEGER ;
PROCEDURE   addnstr (s: chstring; n: INTEGER)            : INTEGER ;
PROCEDURE   addstr (s: chstring)                         : INTEGER ;
PROCEDURE   has_colors ()                                : BOOLEAN ;
PROCEDURE   getch ()                                     : INTEGER ;

PROCEDURE   wattr_on (w: WINDOW; attr_t, void: ADDRESS)  : INTEGER ;
PROCEDURE   wattr_off (w: WINDOW; attr_t, void: ADDRESS) : INTEGER ;
PROCEDURE   wattr_set (w: WINDOW; a: ATTRIBUTE; c: COLORPAIR; void: ADDRESS)  : INTEGER ;
PROCEDURE   wattr_get (w: WINDOW; VAR a: ATTRIBUTE; VAR c: COLORPAIR; void: ADDRESS) : INTEGER ;

PROCEDURE   baudrate ()                                  : INTEGER ;
PROCEDURE   beep  ()                                     : INTEGER ;
PROCEDURE   bkgd (ch: chtype)                            : INTEGER ;
PROCEDURE   bkgdset (ch: chtype) ;
PROCEDURE   border (a, b, c, d, e, f, g, h: chtype)      : INTEGER ;
PROCEDURE   box (w: WINDOW; a, b: chtype)                : INTEGER ;
PROCEDURE   can_change_color ()                          : BOOLEAN ;
PROCEDURE   cbreak ()                                    : INTEGER ;


PROCEDURE   clear ()                                     : INTEGER ;
PROCEDURE   wclear (w: WINDOW)				 : INTEGER ;
PROCEDURE   wrefresh (w: WINDOW)                         : INTEGER ;
PROCEDURE   wmove (w: WINDOW; y, x: INTEGER)             : INTEGER ;
PROCEDURE   waddch (w: WINDOW; ch: chtype)               : INTEGER ;
PROCEDURE   wcolor_set (w: WINDOW; p: SHORTCARD; nil: ADDRESS) : INTEGER ;



PROCEDURE   initscr ()                                   : WINDOW ;
PROCEDURE   savetty ()                                   : INTEGER ;
PROCEDURE   resetty ()                                   : INTEGER ;
PROCEDURE   delwin (w: WINDOW)                           : INTEGER ;
PROCEDURE   endwin ()                                    : INTEGER ;
PROCEDURE   isendwin ()                                  : BOOLEAN ;
PROCEDURE   init_pair (pair: COLORPAIR; fg, bg: SHORTCARD)                : INTEGER ;
PROCEDURE   noecho ()				         : INTEGER ;
PROCEDURE   nonl ()					 : INTEGER ;
PROCEDURE   getch(): INTEGER;
PROCEDURE   getstr(VAR s: ARRAY OF CHAR): INTEGER;
PROCEDURE   getnstr(VAR s: ARRAY OF CHAR; n: INTEGER): INTEGER;
PROCEDURE   nl ();					 : INTEGER ;
PROCEDURE   nocbreak ();				 : INTEGER ;
PROCEDURE   raw ();					 : INTEGER ;
PROCEDURE   noraw ();				 : INTEGER ;
PROCEDURE   redrawwin (w: WINDOW);			 : INTEGER ;
PROCEDURE   refresh ();				 : INTEGER ;
PROCEDURE   wattr_on (w: WINDOW; attr_t, void: ADDRESS)  : INTEGER ;
PROCEDURE   trace (d: INTEGER);
PROCEDURE wgetch (VAR w: WINDOW): INTEGER;
PROCEDURE wgetnstr(VAR w: WINDOW; VAR s: ARRAY OF CHAR; n: INTEGER): INTEGER;
PROCEDURE wgetstr(VAR w: WINDOW; VAR s: ARRAY OF CHAR): INTEGER;
PROCEDURE werase(VAR w: WINDOW): INTEGER;
PROCEDURE   chgat (VAR w: WINDOW; attr_t, nil: ADDRESS): INTEGER;
PROCEDURE   wchgat (VAR w: WINDOW; attr_t: ADDRESS, p: SHORTCARD; nil: ADDRESS); : INTEGER ;
PROCEDURE   clearok (VAR w: WINDOW; b: BOOLEAN ): INTEGER ;
PROCEDURE clrtobot () : INTEGER;
PROCEDURE clrtoeol (): INTEGER;

END ncurses.
*)
