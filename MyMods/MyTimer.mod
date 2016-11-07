<*/NOWARN:F*>
MODULE MyTimer;
(*-----------------------------------------
REVISION HISTORY
================
23 Oct 03 -- Added intermediate time display.
  -----------------------------------------*)

IMPORT WINUSER;
IMPORT WINGDI;
IMPORT WIN32;
IMPORT WINX;
IMPORT SYSTEM;
IMPORT SysMets;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT MMSYSTEM;
IMPORT WholeStr, LongStr, Strings;
FROM UTILLIB IMPORT STRTYP;

CONST ID_TIMER = 1;
      Null_TimerProc = SYSTEM.CAST(WINUSER.TIMERPROC,NIL);

CONST szAppName = "My Timer";
VAR
   hwnd            :  WIN32.HWND;
   msg             :  WINUSER.MSG;
   wc              :  WINUSER.WNDCLASSEX;
   fFlipFlop       :  BOOLEAN = FALSE;
   bool            :  BOOLEAN;
   BeepParam, FlashTime, Color, CountDownTimer,MinutesLeft,SecondsLeft,HoursLeft :  CARDINAL;
   MinutesLeftStr,SecondsLeftStr,HoursLeftStr : STRTYP;
   AlarmTime : CARDINAL = 1;
  cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth,
  iVscrollPos, iVscrollMax, iHscrollPos, iHscrollMax : INTEGER;

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
               BasicDialogs.PromptCard('Alarm Time (min):',0,99,TRUE,AlarmTime);
               AlarmTime := AlarmTime * 60;
               CountDownTimer := AlarmTime;
               Color := 0;
               RETURN 0;

          | WINUSER.WM_TIMER :
               fFlipFlop :=  NOT fFlipFlop;
               INC(Color);
               DEC(CountDownTimer);
               IF CountDownTimer = 0 THEN
                 CountDownTimer := AlarmTime;
                 MMSYSTEM.PlaySound ("notify.wav", NIL, MMSYSTEM.SND_FILENAME BOR MMSYSTEM.SND_ASYNC);
                 BasicDialogs.MessageBox('Time is up.',MsgInfo);
               END;

               HoursLeft := CountDownTimer/3600;
               MinutesLeft := (CountDownTimer MOD 3600) / 60;
               SecondsLeft := (CountDownTimer MOD 60);
(*
               WholeStr.CardToStr(HoursLeft,HoursLeftStr);
               Strings.Insert('Hours = ',0,HoursLeftStr);
               WholeStr.CardToStr(MinutesLeft,MinutesLeftStr);
               Strings.Insert('Minutes = ',0,MinutesLeftStr);
               WholeStr.CardToStr(SecondsLeft,SecondsLeftStr);
               Strings.Insert('Seconds = ',0,SecondsLeftStr);
*)
               
               WholeStr.CardToStr(HoursLeft,HoursLeftStr);
               Strings.Append(':',HoursLeftStr);
               WholeStr.CardToStr(MinutesLeft,MinutesLeftStr);
               Strings.Append(':',MinutesLeftStr);
               WholeStr.CardToStr(SecondsLeft,SecondsLeftStr);
               Strings.Insert(MinutesLeftStr,0,SecondsLeftStr);
               Strings.Insert(HoursLeftStr,0,SecondsLeftStr);

               WINUSER.InvalidateRect (hwnd, WINX.NIL_RECT, FALSE);
               RETURN 0;

          | WINUSER.WM_PAINT :
               hdc := WINUSER.BeginPaint (hwnd, ps);

               WINUSER.GetClientRect (hwnd, rc);
               CASE Color OF
                 1: hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(255,0,0));
               | 2: hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(0,255,0));
               | 3: Color := 0;
                    hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(0,0,255));
               ELSE
                    hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(0,0,0));
                    Color := 0;
               END;
               WINUSER.FillRect (hdc, rc, hBrush);
(*
               WINGDI.TextOut(hdc,0,0,HoursLeftStr,LENGTH(HoursLeftStr));
               WINGDI.TextOut(hdc,0,1*cyChar,MinutesLeftStr,LENGTH(MinutesLeftStr));
*)
               WINGDI.TextOut(hdc,0,1*cyChar,SecondsLeftStr,LENGTH(SecondsLeftStr));
               WINUSER.EndPaint (hwnd, ps);
               WINGDI.DeleteObject (SYSTEM.CAST(WIN32.HGDIOBJ,hBrush));
               RETURN 0;

          | WINUSER.WM_DESTROY :
               WINUSER.KillTimer (hwnd, ID_TIMER);
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
                        "My Count Down Timer ",
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
  IF InitApplication()  AND  InitMainWindow() THEN
    WHILE (WINUSER.GetMessage(msg,NIL,0,0)) DO
      WINUSER.TranslateMessage(msg);
      WINUSER.DispatchMessage(msg);
    END;
  END;
END MyTimer.
