<*/NOWARN:F*>
MODULE SSDancingMouse;
(*-----------------------------------------
REVISION HISTORY
================
22 Sep 04 -- Added check for screen saver running and only move mouse then.  And changed time interval
              to every second.  And changed random number system for mouse movement itself.
              And name change to ScreenSavingDancingMouse, abbreviated.
 4 Oct 04 -- Basic premise works.  Now I'll fine tune when mouse moving stops the screen saver.
-----------------------------------------*)

FROM Storage IMPORT ALLOCATE, DEALLOCATE;
IMPORT WINUSER;
IMPORT WINGDI;
IMPORT WIN32;
IMPORT WINX;
IMPORT SYSTEM;
IMPORT SysMets;
IMPORT Terminal, BasicDialogs,FormatString;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT MMSYSTEM;
IMPORT WholeStr, LongStr, Strings;
FROM UTILLIB IMPORT STRTYP;
FROM MYRAND IMPORT RANDCARD, RANDINT;

CONST ID_TIMER = 1;
      Null_TimerProc = SYSTEM.CAST(WINUSER.TIMERPROC,NIL);

CONST szAppName = "Screen Saving Dancing Mouse";
VAR
   hwnd            :  WIN32.HWND;
   msg             :  WINUSER.MSG;
   wc              :  WINUSER.WNDCLASSEX;
   fFlipFlop       :  BOOLEAN = FALSE;
   bool,askflag    :  BOOLEAN;
   ch1             :  CHAR='N';
   BeepParam,FlashTime,Color,CountUpTimer,MinutesLeft,SecondsLeft,HoursLeft,ColorVal,
     ColorValRed,ColorValBlue,ColorValGreen : CARDINAL;
   MinutesLeftStr,SecondsLeftStr,HoursLeftStr : STRTYP;
   AlarmTime : CARDINAL = 1;
   cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth,
   iVscrollPos, iVscrollMax, iHscrollPos, iHscrollMax : INTEGER;
   mousemoveamt    : INTEGER;
   boolp           : POINTER TO BOOLEAN;

<*/PUSH*>
%IF WIN32 %THEN
    <*/CALLS:WIN32SYSTEM*>
%ELSE
    <*/CALLS:WINSYSTEM*>
%END

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
               FlashTime := 1; (* in seconds *)
               CountUpTimer := 0;
               Color := 0;
               mousemoveamt := 0;
               NEW(boolp);
               RETURN 0;

          | WINUSER.WM_TIMER :
               fFlipFlop :=  NOT fFlipFlop;
               INC(Color);
               INC(CountUpTimer,FlashTime);
               HoursLeft := CountUpTimer/3600;
               MinutesLeft := (CountUpTimer MOD 3600) / 60;
               SecondsLeft := (CountUpTimer MOD 60);
               FormatString.FormatString("%2c:%2c:%2c",SecondsLeftStr,HoursLeft,MinutesLeft,SecondsLeft);

               mousemoveamt := 5 - RANDINT(10);
               WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVERRUNNING,c,boolp,c);
               WHILE boolp^ DO (* Keep looping until screen saver stops *)
                 WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,mousemoveamt,mousemoveamt,0,0);
                 WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVERRUNNING,c,boolp,c);
               END;

               WINUSER.InvalidateRect (hwnd, WINX.NIL_RECT, FALSE);

               RETURN 0;

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
  wc.hIcon         := WINUSER.LoadIcon (NIL, WINUSER.IDI_APPLICATION^);
  wc.hCursor       := WINUSER.LoadCursor (NIL, WINUSER.IDC_ARROW^);
  wc.hbrBackground := SYSTEM.CAST(WIN32.HBRUSH, WINGDI.GetStockObject (WINGDI.WHITE_BRUSH));
  wc.lpszMenuName  := NIL;
  wc.lpszClassName := SYSTEM.ADR(szAppName);
  wc.hIconSm       := WINUSER.LoadIcon (NIL,WINUSER.IDI_APPLICATION^);

  RETURN WINUSER.RegisterClassEx(wc)#0;
END InitApplication;

(*++++*****************************************************************)
PROCEDURE InitMainWindow () : BOOLEAN;
(**********************************************************************)
VAR p : WINUSER.TIMERPROC;

BEGIN
  hwnd := WINUSER.CreateWindow (szAppName,
                        "Screen Saving Dancing Mouse",
                        WINUSER.WS_OVERLAPPEDWINDOW,
                        WINUSER.CW_USEDEFAULT,
                        WINUSER.CW_USEDEFAULT,
                        (*WINUSER.CW_USEDEFAULT, *) 500,
                        (*WINUSER.CW_USEDEFAULT, *) 200,
                        NIL,
                        NIL,
                        WINX.Instance,
                        NIL);

  WHILE(WINUSER.SetTimer (hwnd, ID_TIMER, FlashTime*1000, Null_TimerProc)=0) DO
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
  IF InitApplication()  AND  InitMainWindow() THEN
    WHILE (WINUSER.GetMessage(msg,NIL,0,0)) DO
      WINUSER.TranslateMessage(msg);
      WINUSER.DispatchMessage(msg);
    END;
  END;
END SSDancingMouse.
