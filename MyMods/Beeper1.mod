<*/NOWARN:F*>
MODULE Beeper1;
(*-----------------------------------------
   BEEPER1.C       --- Timer Demo Program No. 1
                   (c) Charles Petzold, 1996
   Beeper1.mod     --- Translation to Stony Brook Modula-2
                   (c) Peter Stadler,   1997
  -----------------------------------------*)

IMPORT WINUSER;
IMPORT WINGDI;
IMPORT WIN32;
IMPORT WINX;
IMPORT SYSTEM;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;

CONST ID_TIMER = 1;
      Null_TimerProc = SYSTEM.CAST(WINUSER.TIMERPROC,NIL);
      
CONST szAppName = "Beeper1";
VAR
   hwnd            :  WIN32.HWND;
   msg             :  WINUSER.MSG;
   wc              :  WINUSER.WNDCLASSEX;
   fFlipFlop       :  BOOLEAN = FALSE;
   bool            :  BOOLEAN;
   BeepParam, FlashTime, Color :  CARDINAL;

<*/PUSH*>
%IF WIN32 %THEN
    <*/CALLS:WIN32SYSTEM*>
%ELSE
    <*/CALLS:WINSYSTEM*>
%END

PROCEDURE dummyTimerProc (hwnd        : WIN32.HWND;
(**********************************************************************)
          iMsg        : WIN32.UINT;
          iTimerID    : WIN32.UINT;
          dwTime      : WIN32.DWORD);
BEGIN
END dummyTimerProc;

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

BEGIN
     CASE (iMsg) OF
          | WINUSER.WM_CREATE :
               BeepParam := 0;
               bool := BasicDialogs.PromptCard('Beep Param:',0,100000,TRUE,BeepParam);
               FlashTime := 1;
               bool := BasicDialogs.PromptCard('Flash Time (sec):',0,999999,TRUE,FlashTime);
               Color := 0;
               RETURN 0;

          | WINUSER.WM_TIMER :
               WINUSER.MessageBeep (BeepParam);

               fFlipFlop :=  NOT fFlipFlop;
               INC(Color);
               WINUSER.InvalidateRect (hwnd, WINX.NIL_RECT, FALSE);

               RETURN 0;

          | WINUSER.WM_PAINT :
               hdc := WINUSER.BeginPaint (hwnd, ps);

               WINUSER.GetClientRect (hwnd, rc);
(*
               IF(fFlipFlop=TRUE) THEN
                 hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(255,0,0));
               ELSE
                 hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(0,0,255));
               END;
*)
               CASE Color OF
                 1: hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(255,0,0));
               | 2: hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(0,255,0));
               | 3: Color := 0;
                    hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(0,0,255));
               ELSE
                    hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(0,0,0));

               END;
               WINUSER.FillRect (hdc, rc, hBrush);
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
                        "Beeper1 Timer Demo: Translation to Stony Brook Modula-2",
                        WINUSER.WS_OVERLAPPEDWINDOW,
                        WINUSER.CW_USEDEFAULT,
                        WINUSER.CW_USEDEFAULT,
                        WINUSER.CW_USEDEFAULT,
                        WINUSER.CW_USEDEFAULT,
                        NIL,
                        NIL,
                        WINX.Instance,
                        NIL);
(*   p := dummyTimerProc; *)

(*  WHILE(WINUSER.SetTimer (hwnd, ID_TIMER, 1000, NIL)=0) DO *)
(*  WHILE(WINUSER.SetTimer (hwnd, ID_TIMER, 1000, SYSTEM.CAST(WINUSER.TIMERPROC,SYSTEM.ADR(dummyTimerProc)))=0) DO *)
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
END Beeper1.
