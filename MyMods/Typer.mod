<*/NOWARN:F*>
(*--------------------------------------
   TYPER.C         ---  Typing Program
                   (c) Charles Petzold, 1996
   Typer.mod       ---  Translation to Stony Brook Modula-2
                   (c) Peter Stadler,   1997
   10 Sep 03 -- Fixed the bugs.  One I noticed and other Norman Black caught.
                 I found the Delete key problem, and Norman found Paint pblm in
                 that x coord did not include the cxChar factor as needed.
  --------------------------------------*)

MODULE Typer;
IMPORT SYSTEM;
IMPORT WINUSER;
IMPORT WIN32;
IMPORT WINGDI;
IMPORT WINX;
IMPORT Storage;

CONST
  szAppName = "Typer";
VAR
  hwnd        :  WIN32.HWND;
  msg         :  WINUSER.MSG;
  wc          :  WINUSER.WNDCLASSEX;
(*
#define BUFFER(x,y) *(pBuffer + y * cxBuffer + x)
*)
TYPE
  BUFFER = ARRAY[0..10000] OF CHAR;
VAR
  Buffer  : BUFFER;
  pBuffer : POINTER TO BUFFER = NIL;
  cxChar  : INTEGER;
  cyChar  : INTEGER;
  cxClient: INTEGER;
  cyClient: INTEGER;
  cxBuffer: INTEGER;
  cyBuffer: INTEGER;
  xCaret  : INTEGER;
  yCaret  : INTEGER;

<*/PUSH*>
%IF WIN32 %THEN
    <*/CALLS:WIN32SYSTEM*>
%ELSE
    <*/CALLS:WINSYSTEM*>
%END

(*++++*****************************************************************)
PROCEDURE MaxInt (a,b : INTEGER) : INTEGER;
(**********************************************************************)
BEGIN
  IF(a>b) THEN
    RETURN a;
  ELSE
    RETURN b;
  END;
END MaxInt;
(*++++*****************************************************************)
PROCEDURE MinInt (a,b : INTEGER) : INTEGER;
(**********************************************************************)
BEGIN
  IF(a>b) THEN
    RETURN b;
  ELSE
    RETURN a;
  END;
END MinInt;
(*++++*****************************************************************)
PROCEDURE  BUFFEROut (x,y : INTEGER) : CHAR;
(**********************************************************************)
(*
#define BUFFER(x,y) *(pBuffer + y * cxBuffer + x)
*)
VAR
  ch : CHAR;
BEGIN
  ch := Buffer[y*cxBuffer+x];
  RETURN ch;
END BUFFEROut;
(*++++*****************************************************************)
PROCEDURE  BUFFERIn (x,y : INTEGER; ch : CHAR);
(**********************************************************************)
BEGIN
  Buffer[y*cxBuffer+x] := ch;
END BUFFERIn;

<*/PUSH*>
%IF WIN32 %THEN
    <*/CALLS:WIN32SYSTEM*>
%ELSE
    <*/CALLS:WINSYSTEM*>
%END
(*++++*****************************************************************)
PROCEDURE  WndProc (hwnd        : WIN32.HWND;
                    iMsg        : WIN32.UINT;
                    wParam      : WIN32.WPARAM;
                    lParam      : WIN32.LPARAM) : WIN32.LRESULT [EXPORT];
(**********************************************************************)
VAR
  hdc         :  WIN32.HDC;
  x           :  INTEGER;
  y           :  INTEGER;
  i           :  INTEGER;
  ps          :  WINUSER.PAINTSTRUCT;
  tm          :  WINGDI.TEXTMETRIC;
BEGIN

  CASE (iMsg) OF
          | WINUSER.WM_CREATE :
               hdc := WINUSER.GetDC (hwnd);

               WINGDI.SelectObject (hdc, WINGDI.GetStockObject (WINGDI.SYSTEM_FIXED_FONT));
               WINGDI.GetTextMetrics (hdc, tm);
               cxChar := tm.tmAveCharWidth;
               cyChar := tm.tmHeight;

               WINUSER.ReleaseDC (hwnd, hdc);
               RETURN 0;

          | WINUSER.WM_SIZE :
                                   (* obtain window size in pixels             *)

               cxClient := WINUSER.LOWORD (lParam);
               cyClient := WINUSER.HIWORD (lParam);

                                   (* calculate window size in characters      *)

               cxBuffer := MaxInt (1, cxClient DIV cxChar);
               cyBuffer := MaxInt (1, cyClient DIV cyChar);

                                   (* allocate memory for buffer and clear it  *)

               IF (pBuffer # NIL) THEN
                    Storage.DEALLOCATE(pBuffer,10000);
               END;
               Storage.ALLOCATE(pBuffer,10000);
               (*
               IF ((pBuffer := (char * ) malloc (cxBuffer * cyBuffer)) = NIL) THEN
                    MessageBox (hwnd, "Window too large.  Cannot "
                                      "allocate enough memory.", "Typer",
                                      MB_ICONEXCLAMATION BOR MB_OK);
               ELSE
               *)   FOR y := 0 TO  cyBuffer-1 DO
                         FOR x := 0 TO cxBuffer-1 DO
                              BUFFERIn (x,y,' ');
                         END;
                    END;
(*
               END;
*)
                                   (* set caret to upper left corner           *)
               xCaret := 0;
               yCaret := 0;

               IF (hwnd = WINUSER.GetFocus ()) THEN
                    WINUSER.SetCaretPos (xCaret * cxChar, yCaret * cyChar);
               END;
               RETURN 0;

          | WINUSER.WM_SETFOCUS :
                                   (* create and show the caret                *)

               WINUSER.CreateCaret (hwnd, NIL, cxChar, cyChar);
               WINUSER.SetCaretPos (xCaret * cxChar, yCaret * cyChar);
               WINUSER.ShowCaret (hwnd);
               RETURN 0;

          | WINUSER.WM_KILLFOCUS :
                                   (* hide and destroy the caret               *)
               WINUSER.HideCaret (hwnd);
               WINUSER.DestroyCaret ();
               RETURN 0;

          | WINUSER.WM_KEYDOWN :
               CASE (wParam)                                                                                                                                                                           OF
                    | WINUSER.VK_HOME :
                         xCaret := 0;

                    | WINUSER.VK_END :
                         xCaret := cxBuffer - 1;

                    | WINUSER.VK_PRIOR :
                         yCaret := 0;

                    | WINUSER.VK_NEXT :
                         yCaret := cyBuffer - 1;

                    | WINUSER.VK_LEFT :
                         xCaret := MaxInt (xCaret - 1, 0);

                    | WINUSER.VK_RIGHT :
                         xCaret := MinInt (xCaret + 1, cxBuffer - 1);

                    | WINUSER.VK_UP :
                         yCaret := MaxInt (yCaret - 1, 0);

                    | WINUSER.VK_DOWN :
                         yCaret := MinInt (yCaret + 1, cyBuffer - 1);

                    | WINUSER.VK_DELETE :
                         FOR x := xCaret TO cxBuffer - 2 DO
                           BUFFERIn (x, yCaret,BUFFEROut (x + 1, yCaret));
                         END;
                         BUFFERIn (cxBuffer - 1, yCaret, ' ');

                         WINUSER.HideCaret (hwnd);
                         hdc := WINUSER.GetDC (hwnd);

                         WINGDI.SelectObject (hdc,
                              WINGDI.GetStockObject (WINGDI.SYSTEM_FIXED_FONT));
(* Buggy because BufferOut only gets one character at a time
                         WINGDI.TextOut (hdc, xCaret * cxChar, yCaret * cyChar,
                                  BUFFEROut (xCaret, yCaret),
                                  cxBuffer - xCaret);
*)
                         WINGDI.TextOut (hdc, xCaret * cxChar, yCaret * cyChar,
                               Buffer [yCaret*cxBuffer + xCaret], cxBuffer - xCaret);

                         WINUSER.ShowCaret (hwnd);
                         WINUSER.ReleaseDC (hwnd, hdc);
               ELSE
               END;
               WINUSER.SetCaretPos (xCaret * cxChar, yCaret * cyChar);
               RETURN 0;

          | WINUSER.WM_CHAR :
               FOR i := 0  TO VAL(INTEGER,WINUSER.LOWORD (lParam))-1 DO
                    CASE (wParam)                                                                                                                                                                      OF
                         | 8    :                    (* backspace           *)
                              IF (xCaret > 0) THEN
                                   DEC(xCaret);
                                   WINUSER.SendMessage (hwnd, WINUSER.WM_KEYDOWN,
                                                WINUSER.VK_DELETE, 1h);
                              END;

                         | 9    :                    (* tab                 *)
                              REPEAT
                                   WINUSER.SendMessage (hwnd, WINUSER.WM_CHAR, 0, 1h);
                              UNTIL (xCaret MOD 8 = 0);

                         | 10   :                    (* line feed           *)
                              INC(yCaret);
                              IF (yCaret = cyBuffer) THEN
                                   yCaret := 0;
                              END;

                         | 13   :                    (* carriage RETURN     *)
                              xCaret := 0;
                              INC(yCaret);
                              IF (yCaret = cyBuffer) THEN
                                   yCaret := 0;
                              END;

                         | 27   :                    (* escape              *)
                              FOR y := 0 TO cyBuffer-1 DO
                                   FOR x := 0 TO cxBuffer-1 DO
                                        BUFFERIn (x, y,' ');
                                   END;
                              END;

                              xCaret := 0;
                              yCaret := 0;

                              WINUSER.InvalidateRect (hwnd, WINX.NIL_RECT, FALSE);

                         ELSE                       (* character codes    *)
                              BUFFERIn (xCaret, yCaret, SYSTEM.CAST(CHAR,wParam));

                              WINUSER.HideCaret (hwnd);
                              hdc := WINUSER.GetDC (hwnd);

                              WINGDI.SelectObject (hdc,
                                   WINGDI.GetStockObject (WINGDI.SYSTEM_FIXED_FONT));

                              WINGDI.TextOut (hdc, xCaret * cxChar, yCaret * cyChar,
                                       BUFFEROut (xCaret, yCaret), 1);

                              WINUSER.ShowCaret (hwnd);
                              WINUSER.ReleaseDC (hwnd, hdc);
                              INC(xCaret);
                              IF (xCaret = cxBuffer) THEN
                                   xCaret := 0;
                                   INC(yCaret);
                                   IF (yCaret = cyBuffer) THEN
                                        yCaret := 0;
                                   END;
                              END;
                         END;
                    END;
               WINUSER.SetCaretPos (xCaret * cxChar, yCaret * cyChar);
               RETURN 0;

          | WINUSER.WM_PAINT :
               hdc := WINUSER.BeginPaint (hwnd, ps);
               WINGDI.SelectObject (hdc, WINGDI.GetStockObject (WINGDI.SYSTEM_FIXED_FONT));

               FOR y := 0 TO cyBuffer-1 DO
                  FOR x := 0 TO cxBuffer-1 DO
                    WINGDI.TextOut (hdc, x * cxChar, y * cyChar, BUFFEROut (x,y), 1);
                  END;
               END;
               WINUSER.EndPaint (hwnd, ps);
               RETURN 0;

          | WINUSER.WM_DESTROY :
               WINUSER.PostQuitMessage (0);
               RETURN 0;
     ELSE
          RETURN WINUSER.DefWindowProc (hwnd, iMsg, wParam, lParam);
     END;
END WndProc;
<*/POP*>
(*++++*****************************************************************)
PROCEDURE InitApplication () : BOOLEAN;
(**********************************************************************)
VAR
  rc  :  CARDINAL;

BEGIN
  wc.cbSize        := SIZE(WINUSER.WNDCLASSEX);
  wc.style         := WINUSER.CS_HREDRAW BOR WINUSER.CS_VREDRAW;
  wc.lpfnWndProc   := WndProc;
  wc.cbClsExtra    := 0;
  wc.cbWndExtra    := 0;
  wc.hInstance     := WINX.Instance;
  wc.hIcon         := WINUSER.LoadIcon (NIL, WINUSER.IDI_APPLICATION^);
  wc.hCursor       := WINUSER.LoadCursor (NIL, WINUSER.IDC_ARROW^);
  wc.hbrBackground := SYSTEM.CAST(WIN32.HBRUSH, WINGDI.GetStockObject (WINGDI.WHITE_BRUSH));
  wc.lpszMenuName  := NIL;
  wc.lpszClassName := SYSTEM.ADR(szAppName);
  wc.hIconSm       := WINUSER.LoadIcon (NIL, WINUSER.IDI_APPLICATION^);

  rc := WINUSER.RegisterClassEx (wc);
  RETURN rc#0;
END InitApplication;
(*++++*****************************************************************)
PROCEDURE InitMainWindow () : BOOLEAN;
(**********************************************************************)
BEGIN
  hwnd := WINUSER.CreateWindow
           (szAppName,                           (* window class name            *)
           "Typing Program: Translation to Stony Brook Modula-2",
                                                 (* window caption               *)
           WINUSER.WS_OVERLAPPEDWINDOW,          (* window style                 *)
           WINUSER.CW_USEDEFAULT,                (* initial x position           *)
           WINUSER.CW_USEDEFAULT,                (* initial y position           *)
           WINUSER.CW_USEDEFAULT,                (* initial x size               *)
           WINUSER.CW_USEDEFAULT,                (* initial y size               *)
           NIL,                                  (* parent window handle         *)
           NIL,                                  (* window menu handle           *)
           WINX.Instance,                        (* program instance handle      *)
           NIL);                                 (* creation parameters          *)

  IF hwnd = NIL THEN
    RETURN FALSE;
  END;
  WINUSER.ShowWindow (hwnd, WINUSER.SW_SHOWDEFAULT);
  WINUSER.UpdateWindow (hwnd);
  RETURN TRUE;
END InitMainWindow;
(*++++*****************************************************************)
BEGIN
  IF InitApplication()  AND  InitMainWindow() THEN
    WHILE (WINUSER.GetMessage(msg,NIL,0,0)) DO
      WINUSER.TranslateMessage(msg);
      WINUSER.DispatchMessage(msg);
    END;
  END;
END Typer.
