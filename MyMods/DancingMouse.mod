<*/NOWARN:F*>
MODULE DancingMouse;
(*-----------------------------------------
REVISION HISTORY
================
22 Sep 04 -- Added check for screen saver running and only move mouse then.  And changed time interval
              to every second.  And changed random number system for mouse movement itself.
-----------------------------------------*)

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

CONST szAppName = "Dancing Mouse";
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
  mousemoveamt     : INTEGER;

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
(*
               BasicDialogs.PromptCard('Alarm Time (min):',0,99,TRUE,AlarmTime);
               AlarmTime := AlarmTime * 60;
*)
               CountUpTimer := 0;
               Color := 0;
               mousemoveamt := 0;
               WINUSER.SendMessage(hwnd, WINUSER.WM_SETCURSOR, 0, 0);
               
               RETURN 0;

          | WINUSER.WM_TIMER :
               INC(Color);
               INC(CountUpTimer,FlashTime);

               HoursLeft := CountUpTimer/3600;
               MinutesLeft := (CountUpTimer MOD 3600) / 60;
               SecondsLeft := (CountUpTimer MOD 60);
               FormatString.FormatString("%2c:%2c:%2c",SecondsLeftStr,HoursLeft,MinutesLeft,SecondsLeft);
               
(*               WINUSER.SendMessage(hwnd, WINUSER.WM_SETCURSOR, 0, 0); *)
(*
               INC(mousemoveamt,20 - INT(RANDCARD(10)) );
               IF ODD(SecondsLeft) THEN
                 mousemoveamt := -1*mousemoveamt;
               END;
*)
               mousemoveamt := 5 - RANDINT(10);
               IF (SecondsLeft = 0)AND NOT ODD(MinutesLeft) THEN 
                 WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,mousemoveamt,mousemoveamt,0,0);
(* this works but isn't what I want to do    WINUSER.SetCursorPos(RANDCARD(100),RANDCARD(100));  *)
               END;
               WINUSER.InvalidateRect (hwnd, WINX.NIL_RECT, FALSE);

               RETURN 0;
(*
          | WINUSER.WM_SETCURSOR : 
               WINUSER.SetCursorPos(RANDCARD(100),RANDCARD(100));
          
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
                        "My Dancing Mouse ",
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
END DancingMouse.
(*
Subject: Re: Keeping computer awake
From: Frank Schoonjans <Frank.Schoonjans@ugent.be>
Newsgroups: comp.lang.modula2

Except for your method being very user-unfriendly, there are some other 
things wrong with your code.

1. When you process the WM_SETCURSOR notification message, you should return 
1.
2. You are setting the cursor at a random position for all (including 
possible system generated) WM_SETCURSOR notifications. You can expect 
unpleasant side effects indeed.
3. This method will not work anyway.
4. Try the following:

            | WINUSER.WM_TIMER :
                       WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,0,0,0,0);
                       RETURN 0;

Since the user cannot decrease the screen saver Wait time below 1 minute, I 
would set the timer time-out value at about 30000 milliseconds.

Frank.


Subject: Re: Keeping computer awake
From: Frank Schoonjans <frank.schoonjans@ugent.be>
Newsgroups: comp.lang.modula2

A friend suggested the following (exact) solution:

   WINUSER.SystemParametersInfo(SPI_GETSCREENSAVEACTIVE,...

and

   WINUSER.SystemParametersInfo(SPI_SETSCREENSAVEACTIVE,...

See:

http://msdn.microsoft.com/library/default.asp?url=/library/en-us/sysinfo/base/systemparametersinfo.asp
*)