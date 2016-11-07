<*/NOWARN:F*>
MODULE SS3DancingMouse;
(*-----------------------------------------
REVISION HISTORY
================
22 Sep 04 -- Added check for screen saver running and only move mouse then.  And changed time interval
              to every second.  And changed random number system for mouse movement itself.
              And name change to ScreenSavingDancingMouse, abbreviated.
 4 Oct 04 -- Basic premise works.  Now I'll fine tune when mouse moving stops the screen saver.
 5 Oct 04 -- This new algorithm hangs at MIH & StM.  Will try another approach.
 6 Oct 04 -- SS2 works.  I'll now try timer procs.
 7 Oct 04 -- Timer procs work.  Now I'll fiddle w/ the time and mouse moving values used.
18 Jan 05 -- Will add an icon.
 1 Feb 05 -- Added Window Title text.
 3 Feb 05 -- For some reason the screen saving fcn doesn't work.  Will try a thing or 2.
 4 Feb 05 -- screen saving fcn fails w/ windows title used in timer proc.  Don't know y.
 7 Feb 05 -- Frank answered why.
-----------------------------------------*)

%IF WIN32 %THEN
    <*/Resource:ss3.RES*>
%ELSE
%END
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
IMPORT WINUSER;
IMPORT WINGDI;
IMPORT WIN32;
IMPORT WINX;
IMPORT SYSTEM;
FROM SYSTEM IMPORT FUNC, CAST, DWORD;
IMPORT SysMets;
IMPORT Terminal, BasicDialogs,FormatString;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT MMSYSTEM;
IMPORT WholeStr, LongStr, Strings;
FROM UTILLIB IMPORT STRTYP;
FROM MYRAND IMPORT RANDCARD, RANDINT;

CONST ID_TIMER  = 1;
      ID_TIMER2 = 2;
      Null_TimerProc = SYSTEM.CAST(WINUSER.TIMERPROC,NIL);

CONST szAppName = "SS3";  (* Screen Saving Dancing Mouse version 3 *)
      SS32Icon  = '#700';
VAR
   hwnd            :  WIN32.HWND;
   msg             :  WINUSER.MSG;
   wc              :  WINUSER.WNDCLASSEX;
   fFlipFlop       :  BOOLEAN = FALSE;
   bool,askflag    :  BOOLEAN;
   ch1             :  CHAR='N';
   BeepParam,FlashTime,Color,CountUpTimer,RapidTimer,MinutesLeft,SecondsLeft,HoursLeft,ColorVal,
     ColorValRed,ColorValBlue,ColorValGreen,ScreenSaving : CARDINAL;
   MinutesLeftStr,SecondsLeftStr,HoursLeftStr,WindowText,ScreenSavingStr : STRTYP;
   AlarmTime : CARDINAL = 1;
   cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth,
   iVscrollPos, iVscrollMax, iHscrollPos, iHscrollMax : INTEGER;
   mousemoveamt    : INTEGER;
   LongZero        : INTEGER64 = 0;
   boolp           : POINTER TO BOOLEAN;

<*/PUSH*>
%IF WIN32 %THEN
    <*/CALLS:WIN32SYSTEM*>
%ELSE
    <*/CALLS:WINSYSTEM*>
%END

(*++++*****************************************************************)
PROCEDURE TimerProc1 (hwnd        : WIN32.HWND;
(**********************************************************************)
          iMsg        : WIN32.UINT;
          iTimerID    : WIN32.UINT;
          dwTime      : WIN32.DWORD);
  VAR hBrush : WIN32.HBRUSH;
      hdc    : WIN32.HDC;
      rc     : WIN32.RECT;
      c      : CARDINAL;
BEGIN
(*     WINUSER.MessageBeep (0); *)

     WINUSER.GetClientRect (hwnd, rc);

     hdc := WINUSER.GetDC (hwnd);

(*     mousemoveamt := 5 - RANDINT(10); *)
(*
DWORD is in fact a CARDINAL, so you cannot assign negative numbers to such
type.

The solution is to use type casting, like:

mouse_event (MOUSEEVENTF_MOVE, CAST(DWORD,dx), CAST(DWORD,dy), 0, 0);
*)

     mousemoveamt := -5;
     WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVERRUNNING,c,boolp,c);
     IF boolp^ THEN
       INC(ScreenSaving);
       WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,CAST(DWORD, mousemoveamt),
                           CAST(DWORD, mousemoveamt), 0, 0);
     END;

     WINUSER.InvalidateRect (hwnd, WINX.NIL_RECT, FALSE);
     WINUSER.ReleaseDC (hwnd, hdc);
END TimerProc1;

(*++++*****************************************************************)
PROCEDURE TimerProc2 (hwnd        : WIN32.HWND;
(**********************************************************************)
          iMsg        : WIN32.UINT;
          iTimerID    : WIN32.UINT;
          dwTime      : WIN32.DWORD);
  VAR hBrush : WIN32.HBRUSH;
      hdc    : WIN32.HDC;
      rc     : WIN32.RECT;
BEGIN
(*     WINUSER.MessageBeep (0); *)

     WINUSER.GetClientRect (hwnd, rc);

     hdc := WINUSER.GetDC (hwnd);
     INC(CountUpTimer);
     HoursLeft := CountUpTimer/3600;
     MinutesLeft := (CountUpTimer MOD 3600) / 60;
     SecondsLeft := (CountUpTimer MOD 60);
     FormatString.FormatString("%3c:%c:%c",SecondsLeftStr,HoursLeft,MinutesLeft,SecondsLeft);
     WindowText  :=  SecondsLeftStr;
     FUNC WINUSER.SetWindowText(hwnd,WindowText);
     ColorValRed := RANDCARD(256);
     ColorValBlue := RANDCARD(256);
     ColorValGreen := RANDCARD(256);
     hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(ColorValRed,ColorValGreen,ColorValBlue));
     WINUSER.FillRect (hdc, rc, hBrush);


     WINUSER.InvalidateRect (hwnd, WINX.NIL_RECT, FALSE);
     WINUSER.ReleaseDC (hwnd, hdc);
END TimerProc2;

(*++++*****************************************************************)
PROCEDURE WndProc (hwnd        : WIN32.HWND;
(**********************************************************************)
          iMsg        : WIN32.UINT;
          wParam      : WIN32.WPARAM;
          lParam      : WIN32.LPARAM) : WIN32.LRESULT [EXPORT];
  VAR hBrush : WIN32.HBRUSH;
      hdc    : WIN32.HDC;
      ps     : WINUSER.PAINTSTRUCT;
      rc     : WIN32.RECT;
      tm     : WINGDI.TEXTMETRIC;
      i      : INTEGER;
      c      : CARDINAL;

BEGIN
     CASE (iMsg) OF
          | WINUSER.WM_CREATE :
              hdc := WINUSER.GetDC (hwnd);
              WINGDI.GetTextMetrics (hdc, tm);
              cxChar := tm.tmAveCharWidth;
              IF(tm.tmPitchAndFamily=1) THEN
                cxCaps := 3*cxChar/2;
              ELSE
                cxCaps := 2*cxChar/2;
              END;
              cyChar := tm.tmHeight + tm.tmExternalLeading;

              WINUSER.ReleaseDC (hwnd, hdc);

              iMaxWidth := 40 * cxChar + 22 * cxCaps;
              BeepParam := 0;
              FlashTime := 300; (* in ms    *)
              CountUpTimer := 0;
              RapidTimer := 0;
              Color := 0;
              SecondsLeftStr :=  '';
              WindowText :=  '';
              mousemoveamt := 5;
              NEW(boolp);
              RETURN 0;
(* replaced by timer procs
          | WINUSER.WM_TIMER :
               INC(Color);

               INC(RapidTimer);
               CountUpTimer := RapidTimer / (1000/FlashTime);
               HoursLeft := CountUpTimer/3600;
               MinutesLeft := (CountUpTimer MOD 3600) / 60;
               SecondsLeft := (CountUpTimer MOD 60);
               FormatString.FormatString("%2c:%2c:%2c",SecondsLeftStr,HoursLeft,MinutesLeft,SecondsLeft);

               mousemoveamt := 5 - RANDINT(10);
               WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVERRUNNING,c,boolp,c);
               IF boolp^ THEN
                 WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,mousemoveamt,mousemoveamt,0,0);
               END;

               WINUSER.InvalidateRect (hwnd, WINX.NIL_RECT, FALSE);

               RETURN 0;
*)
          | WINUSER.WM_CHAR :  (* what happens if a caret is never created? *)
              FOR i := 0  TO VAL(INTEGER,WINUSER.LOWORD (lParam))-1 DO
                CASE (wParam)                                                                                                                                                                      OF
                | 8    :                    (* backspace           *)
                | 9    :                    (* tab                 *)
                | 10   :                    (* line feed           *)
                | 13   :                    (* carriage RETURN     *)
                        CountUpTimer := 0;
                        MMSYSTEM.PlaySound ("tada.wav", NIL, MMSYSTEM.SND_FILENAME BOR MMSYSTEM.SND_ASYNC);
                | 27   :                    (* escape              *)
                        HALT;
                | 32   :                    (* space               *)
                        HALT;
                END;
              END;
              RETURN 0;

          | WINUSER.WM_PAINT :
(*
  PROCEDURE SetWindowText(hWnd : HWND; lpString : ARRAY OF ACHAR) : BOOL;
*)
               hdc := WINUSER.BeginPaint (hwnd, ps);
               WINUSER.GetClientRect (hwnd, rc);
               ColorValRed := RANDCARD(256);
               ColorValBlue := RANDCARD(256);
               ColorValGreen := RANDCARD(256);
               CASE Color OF
                 1: hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(ColorValRed,ColorValGreen,ColorValBlue));
               | 2: hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(ColorValRed,ColorValGreen,ColorValBlue));
               | 3: hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(ColorValRed,ColorValGreen,ColorValBlue));
               | 4: Color := 0;
                    hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(ColorValRed,ColorValGreen,ColorValBlue));
               ELSE
                    hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(0,0,0));
                    Color := 0;
               END;
               WINUSER.FillRect (hdc, rc, hBrush);
               WINGDI.TextOut(hdc,0,1*cyChar,SecondsLeftStr,LENGTH(SecondsLeftStr));
               WholeStr.CardToStr(ScreenSaving,ScreenSavingStr);
               WINGDI.TextOut(hdc,0,2*cyChar,ScreenSavingStr,LENGTH(ScreenSavingStr));
(*  This line prevented primary fcn of mouse movement to stop screen saver from working.
    Don't know why.
               FUNC WINUSER.SetWindowText(hwnd,WindowText);
*)
               WINUSER.EndPaint (hwnd, ps);
               WINGDI.DeleteObject (SYSTEM.CAST(WIN32.HGDIOBJ,hBrush));
               RETURN 0;

          | WINUSER.WM_DESTROY :
               WINUSER.KillTimer (hwnd, ID_TIMER);
               WINUSER.PostQuitMessage (0);
               DISPOSE(boolp);
               RETURN 0;
     ELSE
        RETURN WINUSER.DefWindowProc (hwnd, iMsg, wParam, lParam);
     END;
END WndProc;
<*/POP*>
(*++++*****************************************************************)
PROCEDURE InitApplication () : BOOLEAN;
(**********************************************************************)

BEGIN
  wc.cbSize        := SIZE(wc);
  wc.style         := WINUSER.CS_HREDRAW BOR WINUSER.CS_VREDRAW;
  wc.lpfnWndProc   := WndProc;
  wc.cbClsExtra    := 0;
  wc.cbWndExtra    := 0;
  wc.hInstance     := WINX.Instance;
(*  wc.hIcon         := WINUSER.LoadIcon (NIL, WINUSER.IDI_APPLICATION^);  *)
  wc.hIcon         := WINUSER.LoadIcon (WINX.Instance, SS32Icon);
  wc.hCursor       := WINUSER.LoadCursor (NIL, WINUSER.IDC_ARROW^);
  wc.hbrBackground := SYSTEM.CAST(WIN32.HBRUSH, WINGDI.GetStockObject (WINGDI.WHITE_BRUSH));
  wc.lpszMenuName  := NIL;
  wc.lpszClassName := SYSTEM.ADR(szAppName);
  wc.hIconSm       := WINUSER.LoadIcon (WINX.Instance, SS32Icon);
(*  wc.hIconSm       := WINUSER.LoadIcon (NIL,WINUSER.IDI_APPLICATION^); *)

  RETURN WINUSER.RegisterClassEx(wc)#0;
END InitApplication;

(*++++*****************************************************************)
PROCEDURE InitMainWindow () : BOOLEAN;
(**********************************************************************)
VAR p : WINUSER.TIMERPROC;

BEGIN
  hwnd := WINUSER.CreateWindow (szAppName,
                        "SS ver 3",
                        WINUSER.WS_OVERLAPPEDWINDOW,
                        WINUSER.CW_USEDEFAULT,
                        WINUSER.CW_USEDEFAULT,
                        (*WINUSER.CW_USEDEFAULT, *) 200,
                        (*WINUSER.CW_USEDEFAULT, *) 100,
                        NIL,
                        NIL,
                        WINX.Instance,
                        NIL);

  WHILE(WINUSER.SetTimer (hwnd, ID_TIMER, FlashTime,
                                   SYSTEM.CAST(WINUSER.TIMERPROC,SYSTEM.ADR(TimerProc1))  )=0) DO
      IF(WINUSER.IDCANCEL = WINUSER.MessageBox (hwnd,
                "Too many clocks or timers!",
                szAppName,
                WINUSER.MB_ICONEXCLAMATION BOR WINUSER.MB_RETRYCANCEL)) THEN
         RETURN FALSE;
      END;

  END;
  WHILE(WINUSER.SetTimer (hwnd, ID_TIMER2, 1000,
                                   SYSTEM.CAST(WINUSER.TIMERPROC,SYSTEM.ADR(TimerProc2))  )=0) DO
    IF(WINUSER.IDCANCEL = WINUSER.MessageBox (hwnd,
              "Too many clocks or timers!",
              szAppName,
              WINUSER.MB_ICONEXCLAMATION BOR WINUSER.MB_RETRYCANCEL)) THEN
       RETURN FALSE;
    END;
  END;

  WINUSER.ShowWindow (hwnd, WINUSER.SW_SHOWDEFAULT);
  WINUSER.UpdateWindow (hwnd);
  RETURN TRUE;
END InitMainWindow;


BEGIN
  askflag := TRUE;
  ScreenSaving := 0;
  IF InitApplication()  AND  InitMainWindow() THEN
    WHILE (WINUSER.GetMessage(msg,NIL,0,0)) DO
      WINUSER.TranslateMessage(msg);
      WINUSER.DispatchMessage(msg);
    END;
  END;
END SS3DancingMouse.
