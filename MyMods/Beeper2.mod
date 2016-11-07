<*/NOWARN:F*>
MODULE Beeper2;
(*-----------------------------------------
   BEEPER2.C       --- Timer Demo Program No. 2
                   (c) Charles Petzold, 1996
   Beeper2.mod     --- Translation to Stony Brook Modula-2
                    (c) Peter Stadler,  1997
  -----------------------------------------*)

IMPORT WINUSER;
IMPORT WINGDI;
IMPORT WIN32;
IMPORT WINX;
IMPORT SYSTEM;

CONST ID_TIMER = 1;
CONST szAppName = "Beeper2";
      Null_TimerProc = SYSTEM.CAST(WINUSER.TIMERPROC,NIL);

VAR
   hwnd            :  WIN32.HWND;
   msg             :  WINUSER.MSG;
   wc              :  WINUSER.WNDCLASSEX;
   fFlipFlop       :  BOOLEAN = FALSE;

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
BEGIN
     CASE (iMsg) OF
          | WINUSER.WM_DESTROY :
               WINUSER.KillTimer (hwnd, ID_TIMER);
               WINUSER.PostQuitMessage (0);
               RETURN 0;
     ELSE
        RETURN WINUSER.DefWindowProc (hwnd, iMsg, wParam, lParam);
     END;
END WndProc;
<*/POP*>

<*/PUSH*>
%IF WIN32 %THEN
    <*/CALLS:WIN32SYSTEM*>
%ELSE
    <*/CALLS:WINSYSTEM*>
%END
(*++++*****************************************************************)
PROCEDURE TimerProc (hwnd        : WIN32.HWND;
(**********************************************************************)
          iMsg        : WIN32.UINT;
          iTimerID    : WIN32.UINT;
          dwTime      : WIN32.DWORD);
  VAR hBrush : WIN32.HBRUSH;
      hdc    : WIN32.HDC;
      rc     : WIN32.RECT;
BEGIN
     WINUSER.MessageBeep (0);
     fFlipFlop := NOT fFlipFlop;

     WINUSER.GetClientRect (hwnd, rc);

     hdc := WINUSER.GetDC (hwnd);
     IF(fFlipFlop=TRUE) THEN
       hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(255,0,0));
     ELSE
       hBrush := WINGDI.CreateSolidBrush (WINGDI.RGB(0,0,255));
     END;
     WINUSER.FillRect (hdc, rc, hBrush);
     WINUSER.ReleaseDC (hwnd, hdc);
     WINGDI.DeleteObject (SYSTEM.CAST(WIN32.HGDIOBJ,hBrush));
END TimerProc;
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
BEGIN
  hwnd := WINUSER.CreateWindow (szAppName,
                        "Beeper2 Timer Demo: Translation to Stony Brook Modula-2",
                        WINUSER.WS_OVERLAPPEDWINDOW,
                        WINUSER.CW_USEDEFAULT,
                        WINUSER.CW_USEDEFAULT,
                        WINUSER.CW_USEDEFAULT,
                        WINUSER.CW_USEDEFAULT,
                        NIL,
                        NIL,
                        WINX.Instance,
                        NIL);

(*  WHILE(WINUSER.SetTimer (hwnd, ID_TIMER, 1000, SYSTEM.ADR(TimerProc))=0) DO *)
  WHILE(WINUSER.SetTimer (hwnd, ID_TIMER, 1000, 
    SYSTEM.CAST(WINUSER.TIMERPROC,SYSTEM.ADR(TimerProc))  )=0) DO
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
END Beeper2.
