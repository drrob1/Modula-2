Template for Windows Pgm

<*/NOWARN:F*>
(*----------------------------------------------------
Template for a Windows Program
  ----------------------------------------------------*)

MODULE template;

IMPORT SYSTEM;
IMPORT WINUSER, WIN32, WINGDI, WINX;
IMPORT SysMets;
IMPORT Strings;
IMPORT Lib;
IMPORT MATHLIB; (* Uses longreals.  Don't need to import LongMath *)
  FROM Environment IMPORT GetCommandLine;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRCMPFNT;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  IMPORT RConversions, LongStr, LongConv;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
(*
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    UNGETTKN,GETTKNREAL;
*)
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;

CONST
  szAppName = "template";
VAR
  hwnd        :  WIN32.HWND;
  msg         :  WINUSER.MSG;
  wc          :  WINUSER.WNDCLASSEX;
  cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth,
  iVscrollPos, iVscrollMax, iHscrollPos, iHscrollMax : INTEGER;


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
  szBuffer    :  ARRAY[0..10] OF CHAR;
  hdc         :  WIN32.HDC;
  i, x, y, iPaintBeg, iPaintEnd, iVscrollInc, iHscrollInc : INTEGER;
  ps          :  WINUSER.PAINTSTRUCT;
  tm          :  WINGDI.TEXTMETRIC;
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
      RETURN 0;

  | WINUSER.WM_SIZE :
       cxClient := WINUSER.LOWORD (lParam);
       cyClient := WINUSER.HIWORD (lParam);

       iVscrollMax := MaxInt (0, SysMets.NUMLINES + 2 - cyClient DIV cyChar);
       iVscrollPos := MinInt (iVscrollPos, iVscrollMax);

       WINUSER.SetScrollRange (hwnd, WINUSER.SB_VERT, 0, iVscrollMax, FALSE);
       WINUSER.SetScrollPos   (hwnd, WINUSER.SB_VERT, iVscrollPos, TRUE);

       iHscrollMax := MaxInt (0, 2 + (iMaxWidth - cxClient) DIV cxChar);
       iHscrollPos := MinInt (iHscrollPos, iHscrollMax);

       WINUSER.SetScrollRange (hwnd, WINUSER.SB_HORZ, 0, iHscrollMax, FALSE);
       WINUSER.SetScrollPos   (hwnd, WINUSER.SB_HORZ, iHscrollPos, TRUE);
       RETURN 0;

  | WINUSER.WM_VSCROLL :
      CASE (WINUSER.LOWORD (wParam)) OF
      | WINUSER.SB_TOP :
           iVscrollInc := -iVscrollPos;
      | WINUSER.SB_BOTTOM :
           iVscrollInc := iVscrollMax - iVscrollPos;
      | WINUSER.SB_LINEUP :
           iVscrollInc := -1;
      | WINUSER.SB_LINEDOWN :
           iVscrollInc := 1;
      | WINUSER.SB_PAGEUP :
           iVscrollInc := MinInt (-1, -cyClient / cyChar);
      | WINUSER.SB_PAGEDOWN :
           iVscrollInc := MaxInt (1, cyClient / cyChar);
      | WINUSER.SB_THUMBTRACK :
           iVscrollInc := VAL(INTEGER,WINUSER.HIWORD (wParam)) - iVscrollPos;
      ELSE
           iVscrollInc := 0;
      END;
      iVscrollInc := MaxInt (-iVscrollPos,
                    MinInt (iVscrollInc, iVscrollMax - iVscrollPos));

      IF (iVscrollInc <> 0) THEN
           INC(iVscrollPos,iVscrollInc);
           WINUSER.ScrollWindow (hwnd, 0, -cyChar * iVscrollInc, WINX.NIL_RECT, WINX.NIL_RECT);
           WINUSER.SetScrollPos (hwnd, WINUSER.SB_VERT, iVscrollPos, TRUE);
           WINUSER.UpdateWindow (hwnd);
      END;
      RETURN 0;

  | WINUSER.WM_HSCROLL :
       CASE (WINUSER.LOWORD (wParam)) OF
       | WINUSER.SB_LINEUP :
            iHscrollInc := -1;
       | WINUSER.SB_LINEDOWN :
            iHscrollInc := 1;
       | WINUSER.SB_PAGEUP :
            iHscrollInc := -8;
       | WINUSER.SB_PAGEDOWN :
            iHscrollInc := 8;
       | WINUSER.SB_THUMBPOSITION :
            iHscrollInc := VAL(INTEGER,WINUSER.HIWORD (wParam)) - iHscrollPos;
       ELSE
            iHscrollInc := 0;
       END;
       iHscrollInc := MaxInt (-iHscrollPos,
                      MinInt (iHscrollInc, iHscrollMax - iHscrollPos));

       IF (iHscrollInc <> 0) THEN
            INC(iHscrollPos,iHscrollInc);
            WINUSER.ScrollWindow (hwnd, -cxChar * iHscrollInc, 0, WINX.NIL_RECT, WINX.NIL_RECT);
            WINUSER.SetScrollPos (hwnd, WINUSER.SB_HORZ, iHscrollPos, TRUE);
       END;
       RETURN 0;

  | WINUSER.WM_PAINT :
       hdc := WINUSER.BeginPaint (hwnd, ps);

       iPaintBeg := MaxInt (0, iVscrollPos + ps.rcPaint.top DIV cyChar - 1);
       iPaintEnd := MinInt (SysMets.NUMLINES,
                        iVscrollPos + ps.rcPaint.bottom DIV cyChar);
(*I don't know why responsiveness is affected by where the x and y assignments are. *)
       x := cxChar * (1 - iHscrollPos);
       FOR i:= iPaintBeg TO iPaintEnd-1 DO
(* TextOut(hdc,x,y,pointer to a char string psText, num of chars in psText *)
            y := cyChar * (1 - iVscrollPos + iPaintBeg);
            WINGDI.TextOut (hdc, x, y,
                     SysMets.sysmetrics[i].szLabel,
                     Strings.Length(SysMets.sysmetrics[i].szLabel));

            WINGDI.TextOut (hdc, x + 22 * cxCaps, y,
                     SysMets.sysmetrics[i].szDesc,
                     Strings.Length(SysMets.sysmetrics[i].szDesc));

            WINGDI.SetTextAlign (hdc, WINGDI.TA_RIGHT + WINGDI.TA_TOP);

            WINGDI.TextOut (hdc, x + 22 * cxCaps + 40 * cxChar, y,
                     szBuffer,
            WINUSER.wsprintf (szBuffer, "%5d",
                                         WINUSER.GetSystemMetrics (SysMets.sysmetrics[i].iIndex)));

            WINGDI.SetTextAlign (hdc, WINGDI.TA_LEFT + WINGDI.TA_TOP);
            INC(y,cyChar);
       END;
       WINUSER.EndPaint (hwnd, ps);
       RETURN 0;
  | WINUSER.WM_CLOSE   : 
       WINUSER.DestroyWindow(hwnd);
           (* user click 'X' or hit Alt-F4 *)
           (* Good place to clean up, save files, close files, etc *)
       RETURN 0;
          
  | WINUSER.WM_DESTROY :
       WINUSER.PostQuitMessage (0);
       RETURN 0;
  ELSE
    RETURN WINUSER.DefWindowProc (hwnd, iMsg, wParam, lParam);
  END;
  RETURN WINUSER.DefWindowProc (hwnd, iMsg, wParam, lParam);
END WndProc;
<*/POP*>
(*++++*****************************************************************)
PROCEDURE InitApplication () : BOOLEAN;
(**********************************************************************)
BEGIN
  wc.cbSize        := SIZE(WINUSER.WNDCLASSEX);
  wc.style         := WINUSER.CS_HREDRAW + WINUSER.CS_VREDRAW;
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

  RETURN WINUSER.RegisterClassEx (wc)#0;
END InitApplication;
(*++++*****************************************************************)
PROCEDURE InitMainWindow () : BOOLEAN;
(**********************************************************************)
BEGIN
  hwnd := WINUSER.CreateWindow
           (szAppName,                           (* window class name            *)
           "Get System Metrics No. 3b: Translation to Stony Brook Modula-2",
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
END template.