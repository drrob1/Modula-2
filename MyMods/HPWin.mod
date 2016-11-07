<*/NOWARN:F*>
(*--------------------------------------
REVISION HISTORY
================
16 Oct 03 -- First full Windows version finished.
18 Oct 03 -- Changed how help cmd handled.
21 Oct 03 -- Changed how output strings are converted.  And added popups.
31 Oct 03 -- Added Prime & HCF.
 1 Nov 03 -- Fixed bug in var swap for hcf cmd.
 4 Nov 03 -- Made Prime use a LongCard.
11 Nov 03 -- Added DOW to here so it can display a day name.  And changed HELP display.  And added Terminal Module stuff.
  --------------------------------------*)

MODULE HPWin;
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR;
IMPORT WINUSER, WIN32, WINGDI, WINX;
IMPORT Storage;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT SysMets;
IMPORT Strings,MemUtils;
IMPORT Lib;
IMPORT MATHLIB; (* Part of TSlib that uses longreals. *)
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
  IMPORT RConversions, LongStr, LongConv,FormatString;
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
(****************************************************************************)

  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;

  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;

CONST
  szAppName = "HPWin";
  InputPrompt = 'Enter cmd or HELP : ';
  LastMod = '17 Nov 03';

VAR
  hwnd        :  WIN32.HWND;
  msg         :  WINUSER.MSG;
  wc          :  WINUSER.WNDCLASSEX;
(*
#define BUFFER(x,y) *(pBuffer + y * cxBuffer + x)
*)
TYPE
  BUFFER = ARRAY[0..10000] OF CHAR;
  OutStateTyp = (fix,float,gen);
  STR20TYP      = ARRAY [0..20] OF CHAR;

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
  sigfig,c1,c2,c3    :  CARDINAL;
  str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  longstr     : ARRAY [0..5120] OF CHAR;
  OutState : OutStateTyp = fix;
  InputPromptLen, LastModLen : CARDINAL;
  b           : BUFTYP;
  r           : LONGREAL;
  L           : LONGINT;
  LC          : LONGCARD;
  DAYNAMES    : ARRAY [0..6] OF STR10TYP;



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

(**********************************************************************)
PROCEDURE GetInputLine(VAR OUT str: ARRAY OF CHAR);
(*                                                                    *)
(**********************************************************************)
VAR c : CARDINAL;

BEGIN
  FOR c := InputPromptLen TO ORD(xCaret-1) DO
    str[c-InputPromptLen] := CAP(BUFFEROut(c,yCaret));
  END;
  str[xCaret-INT(InputPromptLen)] := NULL;
END GetInputLine;

(**********************************************************************)
PROCEDURE writestack(hdc : WIN32.HDC);
(* In:  sigfig                                                        *)
(**********************************************************************)
VAR strarray : ARRAY [1..8] OF STRTYP;
    stk      : ARRAY [1..8] OF LONGREAL;
    c,ignoreretcod,pos : CARDINAL;

BEGIN
  GETSTACK(stk,ignoreretcod);
(*  debugging code
  r := 1.0;
  FOR c := 1 TO STACKSIZE DO
        stk[c] := r;
        r := r + 50.;
  END;
*)
  IF OutState = fix THEN
    FOR c := 1 TO STACKSIZE DO
(*      pos := 0;                                            *)
(*      RealToStringFixed(stk[c],14,5,strarray[c],pos,bool); *)
      LongStr.RealToFixed(stk[c],sigfig,strarray[c]);
    END;
  ELSIF OutState = float THEN
    FOR c := 1 TO STACKSIZE DO
(*      pos := 0;                                     *)
(*      RealToString(stk[c],14,strarray[c],pos,bool); *)
      LongStr.RealToEng(stk[c],sigfig,strarray[c]);
    END;
  ELSE (* OutState = gen *)
    FOR c := 1 TO STACKSIZE DO
      LongStr.RealToStr(stk[c],strarray[c]);
    END;
  END;
  WINGDI.SelectObject (hdc, WINGDI.GetStockObject (WINGDI.SYSTEM_FIXED_FONT));
  WINGDI.TextOut (hdc, 0, INT(STACKSIZE)*cyChar,'X :                               ',34);
  WINGDI.TextOut (hdc, 0, INT(STACKSIZE-1)*cyChar,'Y :                               ',34);
  WINGDI.TextOut (hdc, 0, INT(STACKSIZE-2)*cyChar,'Z :                               ',34);
  WINGDI.TextOut (hdc, 0, INT(STACKSIZE-3)*cyChar,'T5:                               ',34);
  WINGDI.TextOut (hdc, 0, INT(STACKSIZE-4)*cyChar,'T4:                               ',34);
  WINGDI.TextOut (hdc, 0, INT(STACKSIZE-5)*cyChar,'T3:                               ',34);
  WINGDI.TextOut (hdc, 0, INT(STACKSIZE-6)*cyChar,'T2:                               ',34);
  WINGDI.TextOut (hdc, 0, INT(STACKSIZE-7)*cyChar,'T1:                               ',34);
  FOR c := 0 TO STACKSIZE-1 DO
    WINGDI.TextOut (hdc, 4*cxChar,
                          INT(STACKSIZE-c)*cyChar,strarray[c+1],LENGTH(strarray[c+1]));
  END;

END writestack;

(*****************************************************************)
PROCEDURE IsPrime(lc : LONGCARD) : BOOLEAN;
(*****************************************************************)
VAR t,m : LONGCARD;

BEGIN
  IF (lc = 0) OR (lc = 1) THEN RETURN FALSE
  ELSIF (lc = 2) OR (lc = 3) THEN RETURN TRUE
  ELSIF NOT ODD(lc) THEN RETURN FALSE
  END;

  m := TRUNC(sqrt(LFLOAT(lc)));
  t := 3;
  REPEAT
    IF (lc MOD t) = 0 THEN RETURN FALSE END;
    INC(t,2);
  UNTIL t > m;
  RETURN TRUE
END IsPrime;

PROCEDURE LongCard2HexStr(L : LONGCARD; VAR OutStr : ARRAY OF CHAR);
CONST  ASCZERO = ORD('0');
       ascA    = ORD('A');
VAR i,j,h  : CARDINAL;
    Str20  : STR20TYP;

BEGIN
  i := 0;
  REPEAT (* until L = 0 *)
    h := L MOD 16;
    IF (h <= 9) THEN Str20[i] := CHR(h + ASCZERO) ELSE Str20[i] := CHR(h -10 + ascA) END;
    INC(i);
    L := L DIV 16;
  UNTIL L = 0;
  j := 1;  (* first posn is a space to leave room for sign char *)
  OutStr[0] := ' ';
  REPEAT (* until i = 0 *)
    DEC(i);
    OutStr[j] := Str20[i];
    INC(j);
  UNTIL i = 0;
  OutStr[j] := 0C;
END LongCard2HexStr;

PROCEDURE LongInt2HexStr(L : LONGINT; VAR OutStr : ARRAY OF CHAR);
VAR
   IsNeg : BOOLEAN;
   LC    : LONGCARD;

BEGIN
    IF L < 0 THEN
      IsNeg := TRUE;
      LC := -L;
    ELSE
      IsNeg := FALSE;
      LC := L;
    END;
    LongCard2HexStr(LC,OutStr);
    IF IsNeg THEN
      OutStr[0] := '-';
    END;
END LongInt2HexStr;

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
               longstr := '';
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
(*
       bool := IntToStr(cxBuffer,str1);
       Strings.Concat('cxBuffer=',str1,longstr);
       Strings.Append(', cyBuffer=',longstr);
       bool := IntToStr(cyBuffer,str2);
       Strings.Append(str2,longstr);
*)
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
                 xCaret := InputPromptLen;
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
(* *)
                         WINGDI.TextOut (hdc, xCaret*cxChar, yCaret*cyChar,
                                  pBuffer^ [yCaret*cxBuffer + xCaret],
                                  cxBuffer - xCaret);
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

                             MemUtils.FillMemBYTE(
                                pBuffer^[yCaret*cxBuffer + xCaret],
                                          cxBuffer - xCaret,' ');
                              GetInputLine(str4);
                              ASSIGN2BUF(str4,b);
(*
                              BasicDialogs.MessageBox(str4,MsgInfo);
                              Strings.Concat('str4= ',str4,longstr);
                              BasicDialogs.MessageBox(longstr,MsgInfo);
                              Strings.Concat('b.CHARS= ',b.CHARS,str1);
                              Strings.Append(', COUNT=',str1);
                              CardToStr(b.COUNT,str2);
                              Strings.Append(str2,str1);
                              BasicDialogs.MessageBox(str1,MsgInfo);
                              CardToStr(b.COUNT,longstr);
                              BasicDialogs.MessageBox(longstr,MsgInfo);
*)
                              IF b.COUNT = 0 THEN HALT END;
                              IF STRCMPFNT(str4,'FIX') = 0 THEN
                                OutState := fix;
                                BasicDialogs.PromptCard('SigFig:',0,20,TRUE,sigfig);
                              ELSIF STRCMPFNT(str4,'FLOAT') = 0 THEN
                                OutState := float;
                                BasicDialogs.PromptCard('SigFig:',0,20,TRUE,sigfig);
                              ELSIF STRCMPFNT(str4,'GEN') = 0 THEN
                                OutState := gen;
(*                                BasicDialogs.PromptCard('SigFig:',0,20,TRUE,sigfig); *)
                              ELSIF STRCMPFNT(str4,'EXIT') = 0 THEN
                                HALT;
                              ELSIF STRCMPFNT(str4,'HEX') = 0 THEN
                               ASSIGN2BUF(' ',b);
                               r := GETRESULT(b);
                               IF (r > 0.) AND (r <= 2.0E9) THEN
                                 c2 := TRUNC(r);
                                 LongCard2HexStr(c2,str8);
                               ELSE
                                 L := VAL(LONGINT,r);
                                 LC := SYSTEM.CAST(LONGCARD,L);
                                 LongCard2HexStr(LC,str8);
                               END;
                               Strings.Concat('X = ',str8,str7);
                               Strings.Append(' hex',str7);
                               BasicDialogs.MessageBox(str7,MsgInfo);
                              ELSIF STRCMPFNT(str4,'PRIME') = 0 THEN
(* not needed.  I forgot abt READX
                               ASSIGN2BUF(' ',b);
                               r := GETRESULT(b);
*)

                               r := READX();
                               LC := VAL(LONGCARD,(ABS(r)));
                               bool := IsPrime(LC);
                               IF bool THEN
                                 FormatString.FormatString(' %l is prime.',str5,LC);
                               ELSE
                                 FormatString.FormatString(' %l is not prime.',str5,LC);
                               END;
                               BasicDialogs.MessageBox(str5,MsgInfo);

                              ELSIF STRCMPFNT(str4,'DOW') = 0 THEN
                                int := round(READX());
                                str3 := DAYNAMES[int MOD 7];
                                BasicDialogs.MessageBox(str3,MsgInfo);
                              ELSIF STRCMPFNT(str4,'HELP') = 0 THEN
(*
                                str0 :=
' This is an RPN style calculator as used by Hewlett Packard.';
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                   Strings.Append(' SQRT,SQR -- X = sqrt(X) or sqr(X) register.',str0);
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                   Strings.Append(
' STO,RCL  -- store/recall the X register to/from the memory register.',str0);
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                                BasicDialogs.MessageBox(str0,MsgInfo);
                                str0 :=
' SWAP,SWAPXY,<>,>< -- equivalent commands that swap the X and Y registers.';
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                   Strings.Append(
' LASTX -- put the value of the LASTX register back into the X register.',str0);
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                   Strings.Append(
' ROLLDN -- roll the stack down one register.  X goes to T1.',str0);
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                                BasicDialogs.MessageBox(str0,MsgInfo);
                  str0 :=
' EXP,LN -- evaluate exp(X) or ln(X) and put result back into X.';
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                  Strings.Append(
' ^   -- evaluate ABS(Y) to the X power, put result in X and pop stack 1 reg.',str0);
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                  Strings.Append(
' **  -- like "^" but rounds X before calling the PWRI function.',str0);
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                                BasicDialogs.MessageBox(str0,MsgInfo);
                  str0 :=
' INT, ROUND, FRAC -- guess.';
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                  Strings.Append(
' MOD -- evaluate Y MOD X, put result in X and pop stack 1 reg.',str0);
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                                BasicDialogs.MessageBox(str0,MsgInfo);
                  str0 :=
' SIN,COS,TAN,ARCTAN,ARCSIN,ARCCOS -- In radians.';
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                  Strings.Append(
' D2R, R2D -- perform degrees / radians conversion of the X register.',str0);
                                Strings.Append(ASCII.cr,str0);
                                Strings.Append(ASCII.lf,str0);
                                BasicDialogs.MessageBox(str0,MsgInfo);
                  longstr :=
' JUL -- Julian date num Z month, Y day, X year.  Pop stack x2.';
                                Strings.Append(ASCII.cr,longstr);
                                Strings.Append(ASCII.lf,longstr);
                  Strings.Append(
" TODAY- Julian date num of today's date.  Pop stack x2.",longstr);
                                Strings.Append(ASCII.cr,longstr);
                                Strings.Append(ASCII.lf,longstr);
                  Strings.Append(
' GREG-- Return Z month, Y day, X year of Julian date number in X.',longstr);
                                Strings.Append(ASCII.cr,longstr);
                                Strings.Append(ASCII.lf,longstr);
                  Strings.Append(
' DOW -- Return day number 1..7 of julian date number in X register.',longstr);
                                Strings.Append(ASCII.cr,longstr);
                                Strings.Append(ASCII.lf,longstr);
                  Strings.Append(
' HEX -- Round X register to a longcard and output it in hex format, but in console mode version only.',longstr);
                                BasicDialogs.MessageBox(longstr,MsgInfo);
*)
                              WINUSER.HideCaret (hwnd);
                              hdc := WINUSER.GetDC (hwnd);
                              WINGDI.SelectObject (hdc,
                                   WINGDI.GetStockObject (WINGDI.SYSTEM_FIXED_FONT));
                              Terminal.Reset;
                               longstr :=
' This is an RPN style calculator.';
                               WINGDI.TextOut(hdc,40*cxChar,8*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr := ' fix, float, gen -- output string format options.';
                               WINGDI.TextOut(hdc,0,9*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr := ' SQRT,SQR -- X = sqrt(X) or sqr(X) register.';
                               WINGDI.TextOut(hdc,0,10*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' STO,RCL  -- store/recall the X register to/from the memory register.';
                               WINGDI.TextOut(hdc,0,11*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' SWAP,SWAPXY,<>,>< -- equivalent commands that swap the X and Y registers.';
                               WINGDI.TextOut(hdc,0,12*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' LASTX -- put the value of the LASTX register back into the X register.';
                               WINGDI.TextOut(hdc,0,13*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' ROLLDN -- roll the stack down one register.  X goes to T1.';
                               WINGDI.TextOut(hdc,0,14*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
(*
                               longstr :=
' DUMP -- dump the stack to the screen.  But only in console mode version.  Not needed here.';
                               WINGDI.TextOut(hdc,0,17*cyChar,longstr,LENGTH(longstr));
*)
                               longstr :=
' EXP,LN -- evaluate exp(X) or ln(X) and put result back into X.';
                               WINGDI.TextOut(hdc,0,15*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' ^   -- evaluate ABS(Y) to the X power, put result in X and pop stack 1 reg.';
                               WINGDI.TextOut(hdc,0,16*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' **  -- like "^" but rounds X before calling the PWRI function.';
                               WINGDI.TextOut(hdc,0,17*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr := ' INT, ROUND, FRAC -- guess.';
                               WINGDI.TextOut(hdc,0,18*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' MOD -- evaluate Y MOD X, put result in X and pop stack 1 reg.';
                               WINGDI.TextOut(hdc,0,19*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' SIN,COS,TAN,ARCTAN,ARCSIN,ARCCOS -- In radians.';
                               WINGDI.TextOut(hdc,0,20*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' D2R, R2D -- perform degrees / radians conversion of the X register.';
                               WINGDI.TextOut(hdc,0,21*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' JUL -- Julian date num Z month, Y day, X year.  Pop stack x2.';
                               WINGDI.TextOut(hdc,0,22*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
" TODAY- Julian date num of today's date.  Pop stack x2.";
                               WINGDI.TextOut(hdc,0,23*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' GREG-- Return Z month, Y day, X year of Julian date number in X.';
                               WINGDI.TextOut(hdc,0,24*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' DOW -- Return day number 1..7 of julian date number in X register.';
                               WINGDI.TextOut(hdc,0,25*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' HEX -- Round X register to a longcard and output hex fmt.';
                               WINGDI.TextOut(hdc,0,26*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' PRIME -- Returns TRUE or FALSE of X reg.';
                               WINGDI.TextOut(hdc,0,27*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;
                               longstr :=
' HCF -- Pushs highest common factor of Y and X onto stack.';
                               WINGDI.TextOut(hdc,0,28*cyChar,longstr,LENGTH(longstr));
                               Terminal.WriteString(longstr); Terminal.WriteLn;

(*
                              c2 := Strings.Length(longstr);
                              c3 := LENGTH(longstr);
                              CardToStr(c2,str2);
                              CardToStr(c3,str3);
                              BasicDialogs.MessageBox(str2,MsgInfo);
                              BasicDialogs.MessageBox(str3,MsgInfo);
*)
                                WINUSER.ShowCaret (hwnd);
                                WINUSER.ReleaseDC (hwnd, hdc);

                              ELSE
(* Now must process the cmd, which is the whole reason for this pgm existing! *)
                                GETRESULT(b);
                              END;
                              WINUSER.HideCaret (hwnd);
                              hdc := WINUSER.GetDC (hwnd);
                              WINGDI.SelectObject (hdc,
                                   WINGDI.GetStockObject (WINGDI.SYSTEM_FIXED_FONT));
                              WINGDI.TextOut(hdc,xCaret*cxChar,0,
                                          pBuffer^[xCaret], cxBuffer-xCaret);
                              xCaret := InputPromptLen;
                              yCaret := 0;
                              writestack(hdc);
                              WINUSER.ShowCaret (hwnd);
                              WINUSER.ReleaseDC (hwnd, hdc);

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
       bool := BasicDialogs.PromptOpenFile(longstr,'',c3,'','','Test PromptOpenFile',FALSE);
*)

                         | 27   :                    (* escape              *)
                              FOR y := 0 TO cyBuffer-1 DO
                                   FOR x := 0 TO cxBuffer-1 DO
                                        BUFFERIn (x, y,' ');
                                   END;
                              END;

                              xCaret := InputPromptLen;
                              yCaret := 0;

                              WINUSER.InvalidateRect (hwnd, WINX.NIL_RECT, FALSE);

                         ELSE                       (* character codes    *)

                              BUFFERIn (xCaret, yCaret, SYSTEM.CAST(CHAR,wParam));

                              MemUtils.FillMemBYTE(
                                pBuffer^[yCaret*cxBuffer + xCaret+1],
                                          cxBuffer - xCaret-1,' ');

                              WINUSER.HideCaret (hwnd);
                              hdc := WINUSER.GetDC (hwnd);

                              WINGDI.SelectObject (hdc,
                                   WINGDI.GetStockObject (WINGDI.SYSTEM_FIXED_FONT));
(* Need to show rest of line which has been blanked out to not have prev entry bleed thru.
                              WINGDI.TextOut (hdc, xCaret * cxChar, yCaret * cyChar,
                                       BUFFEROut (xCaret, yCaret), 1);
*)
                              WINGDI.TextOut (hdc, xCaret*cxChar, yCaret*cyChar,
                                  pBuffer^ [yCaret*cxBuffer + xCaret],
                                  cxBuffer - xCaret);

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
               WINGDI.TextOut(hdc,0,0,InputPrompt,InputPromptLen);
               WINGDI.TextOut(hdc,INT(InputPromptLen)*cxChar,0,pBuffer^[InputPromptLen],
                                                cxBuffer-xCaret);
               WINGDI.TextOut(hdc,0,cyClient-cyChar,LastMod,LastModLen);
               writestack(hdc);
(* *)
(* *)
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
           "HPWin calculator interface (not console mode).",
                                                 (* window caption               *)
           WINUSER.WS_OVERLAPPEDWINDOW,          (* window style                 *)
           WINUSER.CW_USEDEFAULT,                (* initial x position           *)
           WINUSER.CW_USEDEFAULT,                (* initial y position           *)
           WINUSER.CW_USEDEFAULT,                (* initial x size      500         *)
           WINUSER.CW_USEDEFAULT,                (* initial y size      200         *)
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
  InputPromptLen := LENGTH(InputPrompt);
  LastModLen := LENGTH(LastMod);
  sigfig := 4;
  DAYNAMES[0] := 'Sunday';
  DAYNAMES[1] := 'Monday';
  DAYNAMES[2] := 'Tuesday';
  DAYNAMES[3] := 'Wednesday';
  DAYNAMES[4] := 'Thursday';
  DAYNAMES[5] := 'Friday';
  DAYNAMES[6] := 'Saturday';

  IF InitApplication()  AND  InitMainWindow() THEN
    WHILE (WINUSER.GetMessage(msg,NIL,0,0)) DO
      WINUSER.TranslateMessage(msg);
      WINUSER.DispatchMessage(msg);
    END;
  END;
END HPWin.
n                                                                             