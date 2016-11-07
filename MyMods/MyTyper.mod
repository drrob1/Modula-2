<*/NOWARN:F*>
(*--------------------------------------
  Will see if this works for a calculator interface.
  --------------------------------------*)

MODULE MyTyper;
IMPORT SYSTEM;
IMPORT WINUSER, WIN32, WINGDI, WINX;
IMPORT Storage;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT SysMets;
IMPORT Strings,MemUtils;
IMPORT Lib;
IMPORT MATHLIB; (* Uses longreals.  Don't need to import LongMath *)
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRCMPFNT,
    COPYLEFT,COPYRIGHT;
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
  szAppName = "MyTyper";
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
(*  Buffer  : BUFFER; *)
  pBuffer : POINTER TO BUFFER = NIL;
  cxChar  : INTEGER;
  cyChar  : INTEGER;
  cxClient: INTEGER;
  cyClient: INTEGER;
  cxBuffer: INTEGER;
  cyBuffer: INTEGER;
  xCaret  : INTEGER;
  yCaret  : INTEGER;
  ch1,ch2,ch3 :  CHAR;
  bool        :  BOOLEAN;
  c1,c2,c3    :  CARDINAL;
  str1,str2,str3,str4 : STRTYP;

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
  ch := pBuffer^[y*cxBuffer+x];
  RETURN ch;
END BUFFEROut;
(*++++*****************************************************************)
PROCEDURE  BUFFERIn (x,y : INTEGER; ch : CHAR);
(**********************************************************************)
BEGIN
  pBuffer^[y*cxBuffer+x] := ch;
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
  i,int       :  INTEGER;
  ps          :  WINUSER.PAINTSTRUCT;
  tm          :  WINGDI.TEXTMETRIC;
  OldfMode,u    :  WIN32.UINT;
BEGIN

  CASE (iMsg) OF
          | WINUSER.WM_CREATE :
               hdc := WINUSER.GetDC (hwnd);
               ch1 := 'a';
               ch2 := 0c;
               ch3 := 0c;
               str1 := '';
               str2 := '';
               str3 := '';
               str4 := '';


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
       bool := IntToStr(cxBuffer,str1);
       Strings.Concat('cxBuffer=',str1,str3);
       Strings.Append(', cyBuffer=',str3);
       bool := IntToStr(cyBuffer,str2);
       Strings.Append(str2,str3);

                                   (* allocate memory for buffer and clear it  *)
(*
               IF (pBuffer # NIL) THEN
                    Storage.DEALLOCATE(pBuffer,10000);
               END;
*)
               IF pBuffer = NIL THEN
                 Storage.ALLOCATE(pBuffer,10000);
                 FOR x := 0 TO 10000 DO pBuffer^[x] := ' ' END;
(*
                 FOR y := 0 TO  cyBuffer-1 DO
                   FOR x := 0 TO cxBuffer-1 DO
                     BUFFERIn (x,y,' ');
                   END;
                 END;
*)
                                   (* set caret to upper left corner           *)
                 xCaret := 0;
                 yCaret := 0;
               END;
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
                         int := yCaret*cxBuffer + xCaret;
(*
                         FOR x := xCaret TO cxBuffer - 2 DO
                              BUFFERIn (x, yCaret,BUFFEROut (x + 1, yCaret));
                         END;
*)
(*  This works. *)
                         MemUtils.MoveMem(pBuffer^[int],pBuffer^[int+1],cxBuffer-xCaret-1);
                         BUFFERIn (cxBuffer - 1, yCaret, ' ');
(* *)
(* This does too.
                         COPYLEFT(SYSTEM.ADR(pBuffer^[int+1]),SYSTEM.ADR(pBuffer^[int]),cxBuffer-xCaret-1);
                         BUFFERIn (cxBuffer - 1, yCaret, ' ');
 *)
                         WINUSER.HideCaret (hwnd);
                         hdc := WINUSER.GetDC (hwnd);

                         WINGDI.SelectObject (hdc,
                              WINGDI.GetStockObject (WINGDI.SYSTEM_FIXED_FONT));
(*
                         WINGDI.TextOut (hdc, xCaret*cxChar, yCaret*cyChar,
                                  pBuffer^ [yCaret*cxBuffer + xCaret],
                                  cxBuffer - xCaret);
*)
(* Buffer[y*cxBuffer+x]; Could not use BUFFEROut fcn here as it only returns 1 char
                         WINGDI.TextOut (hdc, xCaret*cxChar, yCaret * cyChar,
                                  BUFFEROut (xCaret, yCaret),
                                  cxBuffer - xCaret);
*)
                       FOR x := xCaret TO cxBuffer DO
                         WINGDI.TextOut (hdc, x * cxChar, yCaret * cyChar,
                                  BUFFEROut(x,yCaret), 1);
                       END;
(*
                       FOR x := 0 TO cxBuffer-1 DO
                         ch1 := BUFFEROut(x,yCaret);
                         WINGDI.TextOut (hdc, x, (*yCaret*) 5 * cyChar,
                                  ch1, 1);
                         str1[x+1] := ch1;  (* STRTYP is a one-origin string *)
                       END (*for*);
                       str1[cxBuffer] := NULL;
                       BasicDialogs.MessageBox(str1,MsgInfo);
*)
                       WINUSER.ShowCaret (hwnd);
                       WINUSER.ReleaseDC (hwnd, hdc);
                       WINUSER.UpdateWindow (hwnd);
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
(* Enough already.
       BasicDialogs.MessageBox('Hello from BasicDialogs MessageBox in WM_CHAR',MsgInfo);
       ch1 := BasicDialogs.YesNo('Hello from BasicDialogs YesNo','Y');
       ch2 := BasicDialogs.YesNoCancel('Hello from BasicDialogs YesNoCancel','N');
       ch3 := BasicDialogs.OkCancel('Hello from OkCancel','O',MsgInfo);
       bool := BasicDialogs.PromptString('From PromptString:',str1);
       bool := BasicDialogs.PromptPassword('From PromptPwd:',str2);
       bool := BasicDialogs.PromptCard('From PromptCard:',0,1000,TRUE,c1);
       bool := CardToStr(c1,str4);
       c3 := 0;
       bool := BasicDialogs.PromptOpenFile(str3,'',c3,'','','Test PromptOpenFile',FALSE);
*)

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
(* *)
               FOR y := 0 TO cyBuffer-1 DO  (* now this works! *)
                 WINGDI.TextOut (hdc, 0, y*cyChar,
                   pBuffer^ [y*cxBuffer], cxBuffer);  (* Buffer x-coord is zero *)
               END;
(* *)

(* partially helped
               WINGDI.SetTextAlign (hdc, WINGDI.TA_UPDATECP);
               FOR y := 0 TO cyBuffer-1 DO
                  FOR x := 0 TO cxBuffer-1 DO
                    WINGDI.TextOut (hdc, x, y * cyChar, BUFFEROut (x,y), 1);
                  END;
               END;
               WINGDI.SetTextAlign (hdc, WINGDI.TA_NOUPDATECP);
*)
(* don't understand things better now.
               BasicDialogs.MessageBox(str3,MsgInfo);
               WINGDI.TextOut(hdc,(cxBuffer-1)*cxChar,(cyBuffer-1)*cyChar,'==',2);
               BasicDialogs.MessageBox('Hello1 from BasicDialogs MessageBox in WM_Paint',MsgInfo);
               WINGDI.TextOut(hdc,0,0,'##',2);
               WINGDI.TextOut(hdc,0,50,'@@',2);
               BasicDialogs.MessageBox('Hello2 from BasicDialogs MessageBox in WM_Paint',MsgInfo);
               FOR y := 0 TO cyBuffer-1 DO
                  WINGDI.SetTextAlign (hdc, WINGDI.TA_UPDATECP);
                  FOR x := 0 TO cxBuffer-1 DO
                    WINGDI.TextOut (hdc, x, y * cyChar, BUFFEROut (x,y), 1);
                  END;
                  WINGDI.SetTextAlign (hdc, WINGDI.TA_NOUPDATECP);
                  WINGDI.TextOut(hdc,cxBuffer-1,cyBuffer-1,'++',2);
                  WINGDI.TextOut(hdc,(cxBuffer-1)*cxChar,(cyBuffer-1)*cyChar,'**',2);
                  WINGDI.TextOut(hdc,0,y*cyChar,BUFFEROut(x,y), 1);
               END;
               WINGDI.SetTextAlign (hdc, WINGDI.TA_NOUPDATECP); (*insert another for good measure *)
*)
(*
               y := 5;
               INC(y);
               WINGDI.TextOut (hdc, 0, y*cyChar,'Hello from Paint',16);
               INC(y);
               WINGDI.TextOut(hdc,0,y*cyChar,'Ch=',3);
               WINGDI.TextOut(hdc,cxChar*4,y*cyChar,ch1,1);
               INC(y);
               WINGDI.TextOut(hdc,0,y*cyChar,'Ch=',3);
               WINGDI.TextOut(hdc,cxChar*4,y*cyChar,ch2,1);
               INC(y);
               WINGDI.TextOut(hdc,0,y*cyChar,'Ch=',3);
               WINGDI.TextOut(hdc,cxChar*4,y*cyChar,ch3,1);
               INC(y);
               WINGDI.TextOut(hdc,0,y*cyChar,str1,LENGTH(str1));
               INC(y);
               WINGDI.TextOut(hdc,0,y*cyChar,str2,LENGTH(str2));
               INC(y);
               WINGDI.TextOut(hdc,0,y*cyChar,str4,LENGTH(str4));
               INC(y);
               WINGDI.TextOut(hdc,0,y*cyChar,str3,LENGTH(str3));
*)

               WINUSER.EndPaint (hwnd, ps);
               RETURN 0;

          | WINUSER.WM_CLOSE   : WINUSER.DestroyWindow(hwnd);
           (* user click 'X' or hit Alt-F4 *)
           (* Good place to clean up, save files, close files, etc *)
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
           "MyTyping Pgm modified for calculator interface.",
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
END MyTyper.
