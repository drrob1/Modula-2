<*/NOWARN:F*>
MODULE SetExecState2;
(* Same as SetExecState but no longer resets the state upon exit *)
IMPORT SYSTEM;
IMPORT WIN32;
IMPORT WINUSER;
IMPORT WINGDI;
IMPORT WINX;
IMPORT MMSYSTEM;
IMPORT Strings;
IMPORT Conversions;
FROM Conversions IMPORT LongToStr,CardBaseToStr;
IMPORT FormatString;


CONST AppName = "SetExecState";
    ES_SYSTEM_REQUIRED  = 00000001H;
    ES_DISPLAY_REQUIRED = 00000002H;
    ES_USER_PRESENT     = 00000004H;
    ES_CONTINUOUS       = 80000000H;

VAR  hwnd       : WIN32.HWND;
     msg        : WINUSER.MSG;
     wc         : WINUSER.WNDCLASSEX;
     esPrevFlags : LONGCARD; (* WIN32.EXECUTION_STATE; *)
     cxChar, cxCaps, cyChar: INTEGER;

<*/PUSH*>
%IF WIN32 %THEN
    <*/CALLS:WIN32SYSTEM*>
%ELSE
    <*/CALLS:WINSYSTEM*>
%END
(******************************************************************************)
PROCEDURE WndProc (hwnd        : WIN32.HWND;
                   iMsg        : WIN32.UINT;
                   wParam      : WIN32.WPARAM;
                   lParam      : WIN32.LPARAM) : WIN32.LRESULT [EXPORT];
(******************************************************************************)

  VAR
     szBuffer :  ARRAY[0..10] OF CHAR;
     hdc      :  WIN32.HDC;
     ps       :  WINUSER.PAINTSTRUCT;
     rect     :  WIN32.RECT;
     hBrush   :  WIN32.HBRUSH;
     i        :  INTEGER;
     tm       :  WINGDI.TEXTMETRIC;
     Ok       :  BOOLEAN;
     cHi,cLo  :  CARDINAL;
     str30    :  ARRAY[0..30] OF CHAR;

BEGIN
  CASE iMsg OF
  | WINUSER.WM_CREATE:
    hdc := WINUSER.GetDC (hwnd);

    WINGDI.GetTextMetrics (hdc, tm);
    cxChar := tm.tmAveCharWidth;
    IF((tm.tmPitchAndFamily BAND 1)=1) THEN
      cxCaps := 3*cxChar/2;
    ELSE
      cxCaps := 2*cxChar/2;
    END;
    cyChar := tm.tmHeight + tm.tmExternalLeading;

    WINUSER.ReleaseDC (hwnd, hdc);
    (* MMSYSTEM.PlaySound ("hellowin.wav", NIL, MMSYSTEM.SND_FILENAME BOR MMSYSTEM.SND_ASYNC); *)
    RETURN 0;

  | WINUSER.WM_PAINT:
      hdc := WINUSER.BeginPaint (hwnd,ps);
      esPrevFlags  := WIN32.SetThreadExecutionState(ES_DISPLAY_REQUIRED BOR ES_CONTINUOUS);
      Ok := LongToStr(esPrevFlags,szBuffer);
      WINGDI.TextOut (hdc, cxChar, cyChar, "Prev Thread Exec State is ", 26);
      WINGDI.TextOut (hdc, cxChar * 26, cyChar, szBuffer, LENGTH(szBuffer));
(*      WINGDI.SetTextAlign (hdc, WINGDI.TA_RIGHT BOR WINGDI.TA_TOP); *)
(*      WINGDI.TextOut (hdc, cxChar + 26 * cxCaps, 2 * cyChar, szBuffer,
               WINUSER.wsprintf (szBuffer, "Prev Thread Exec State is   %d .", esPrevFlags)); *)

(*      WINGDI.SetTextAlign (hdc, WINGDI.TA_LEFT BOR WINGDI.TA_TOP); *)
      cHi := ORD(esPrevFlags SHR 16);
      cLo := esPrevFlags MOD 10000H;
      Ok := CardBaseToStr(esPrevFlags,16,str30);
      WINGDI.TextOut (hdc, cxChar, 2 * cyChar, str30, LENGTH(str30));


      WINUSER.EndPaint (hwnd, ps);
      RETURN 0;

  | WINUSER.WM_DESTROY:
(*      esPrevFlags  := WIN32.SetThreadExecutionState(ES_CONTINUOUS); *)

      WINUSER.PostQuitMessage(0);
      RETURN 0;
  ELSE
    RETURN WINUSER.DefWindowProc (hwnd, iMsg, wParam, lParam);
  END;
END WndProc;
<*/POP*>
(******************************************************************************)
PROCEDURE InitApplication () : BOOLEAN;
(******************************************************************************)
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
   wc.lpszClassName := SYSTEM.ADR (AppName);
   wc.hIconSm       := WINUSER.LoadIcon (NIL, WINUSER.IDI_APPLICATION^);

   RETURN WINUSER.RegisterClassEx (wc) <> 0;
END InitApplication;

PROCEDURE InitMainWindow() : BOOLEAN;
BEGIN
  hwnd := WINUSER.CreateWindow (AppName,                        (* window class name            *)
                                "SetExecState Pgm in Stony Brook Modula-2",
                                                                (* window caption               *)
                                WINUSER.WS_OVERLAPPEDWINDOW,    (* window style                 *)
                                WINUSER.CW_USEDEFAULT,          (* initial x position           *)
                                WINUSER.CW_USEDEFAULT,          (* initial y position           *)
                                WINUSER.CW_USEDEFAULT,          (* initial x size               *)
                                WINUSER.CW_USEDEFAULT,          (* initial y size               *)
                                NIL,                            (* parent window handle         *)
                                NIL,                            (* window menu handle           *)
                                wc.hInstance,                   (* program instance handle      *)
                                NIL);                           (* creation parameters          *)

  WINUSER.ShowWindow   (hwnd, WINUSER.SW_SHOWDEFAULT);
  WINUSER.UpdateWindow (hwnd);

  RETURN TRUE;
END InitMainWindow;


BEGIN
  IF InitApplication() AND InitMainWindow() THEN
    WHILE (WINUSER.GetMessage(msg,NIL,0,0)) DO
      WINUSER.TranslateMessage(msg);
      WINUSER.DispatchMessage(msg);
    END;
  END;
END SetExecState2.
