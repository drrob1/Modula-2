MODULE AsciiHexTW;

IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS;
IMPORT WINUSER, WIN32, WINGDI, WINX;
FROM Strings IMPORT
    Append, Equal, Delete, Concat, Capitalize;
FROM ExStrings IMPORT
    AppendChar, EqualI;
FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, WinAttr, ClipboardFormat,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, WindowTypes, SpecialKeys,
    GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, WriteString,
    WriteStringAt, WriteCellsAt, WriteCells, WriteLn, EraseToEOL, ChangeAttr,
    ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn,
    SetAutoScroll, WinShellToTextWindowMessage,
    MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect,
    GetBufferRect, EraseScreen, EraseRect, GetWinShellHandle, FindTextWindow,
    SetDisplayMode,GetDisplayMode,SetWindowEnable,
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage,
    IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars,
    SetScrollBarPos, SetWindowData, SetWindowDataNum, GetWindowData, GetWindowDataNum,
    GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos, CascadeWindow,
    SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo, 
    SetScrollDisableWhenNone, SetActiveTabChild, SetTabChildPosition,
    GetForegroundWindow, SetForegroundWindow,
    SetTimer, KillTimer, DisplayHelp,
    OpenClipboard, CloseClipboard, EmptyClipboard, ClipboardFormatAvailable,
    AllocClipboardMemory, UnlockClipboardMemory, SetClipboard, GetClipboard,
    Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;
IMPORT Terminal, BasicDialogs, WinShell;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings, MemUtils;
IMPORT WholeStr, LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;

(****************************************************************************)
  IMPORT MiscM2;
  FROM Terminal IMPORT Read, (*WriteString, WriteLn, Write, *) ReadChar, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts, LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;


CONST  ordzero = ORD('0');
       ordA    = ORD('A');
       orda    = ORD('a');
       clipfmt = CLIPBOARD_ASCII;
       szAppName = "AsciiHexTW";
       InputPrompt = 'Enter string. Use terminal "H" for hex: ';
       LastMod = '26 Dec 03';



TYPE HexPair = RECORD uch,Lch : CHAR END;
     HexArray130 = ARRAY [1..30] OF HexPair;
     STR20TYP    = ARRAY [0..20] OF CHAR;
     pstrtyp     = POINTER TO STRTYP;  (* remember this is a 1-origin array *)

VAR s1,s2 : STRTYP;
    hex  : BOOLEAN;
    i,j,h,len,newlen,c1,c2,c3,temp  : CARDINAL;
    ch   : CHAR;
    ha   : HexArray130;
    pstr : POINTER TO STRTYP;
  cxChar  : INTEGER;
  cyChar  : INTEGER;
  cxClient: INTEGER;
  cyClient: INTEGER;
  cxBuffer: INTEGER;
  cyBuffer: INTEGER;
  cxScreen,cyScreen : COORDINATE;
  xCaret  : INTEGER;
  yCaret  : INTEGER;
  ch1,ch2,ch3 :  CHAR;
  bool,inputprocessed :  BOOLEAN;
  inputline,str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  longstr     : ARRAY [0..5120] OF CHAR;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf    : BUFTYP;
  pinstr      : pstrtyp;
  r           : LONGREAL;
  L           : LONGINT;
  LC          : LONGCARD;
  a           : ScreenAttribute;
  Win         : TextWindow;


PROCEDURE CleanHexString(VAR INOUT str: ARRAY OF CHAR);
(*********************************************************************)
VAR s                       : ARRAY [0..255] OF CHAR;
    i                       : INTEGER;
    c,upper,inner,outer,len : CARDINAL;

BEGIN
  upper := HIGH(str);
  str[upper] := NULL;  (* Sentinal NULL for my algorithm *)
  len := LENGTH(str);
  outer := 0;
  inner := 0;
  WHILE (outer <= len) DO
    IF (((CAP(str[outer])>='A') AND (CAP(str[outer])<='H')) OR
                                             ((str[outer]>='0') AND (str[outer]<='9')) ) THEN
    s[inner] := str[outer];
    INC(inner);
    END;
  INC(outer);
  END;
  s[inner] := NULL;
  FOR c := 0 TO inner DO
    str[c] := s[c];
  END;
(*  str[inner+1] := NULL; *)
END CleanHexString;

(********************************************************************************)
PROCEDURE GetCardDgt(ch:CHAR) : CARDINAL;
BEGIN
  IF ch <= '9' THEN
    RETURN ORD(ch) - ordzero;
  ELSIF ch <= 'F' THEN
    RETURN ORD(ch) - ordA + 10;
  ELSE
    RETURN ORD(ch) - orda + 10;
  END;
END GetCardDgt;

PROCEDURE GetHexDgt(n:CARDINAL) : CHAR;
BEGIN
  IF n > 9 THEN
    RETURN CHR(n+ordA-10)
  ELSE
    RETURN CHR(n+ordzero)
  END;
END GetHexDgt;

PROCEDURE DoIt(VAR INOUT s1,s2 : STRTYP);

VAR c1,c2 : CARDINAL;

BEGIN
        len := LENGTH(s1);
        IF CAP(s1[len]) = 'H' THEN   (* Hex -> Ascii *)
          CleanHexString(s1);
(*
          Terminal.WriteString(' Cleaned string is: ');
          Terminal.WriteString(s1);
          Terminal.WriteLn;
*)
          s1[len] := NULL;
          DEC(len);
          IF ODD(len) THEN 
            BasicDialogs.MessageBox(' Have an Odd string length.  Must be an error.',MsgInfo);
            s2 := '';
            RETURN;
          END;
          c1 := 1;
          newlen := len/2;
          FOR c2 := 1 TO newlen DO 
            ha[c2].uch := s1[c1];
            INC(c1);
            ha[c2].Lch := s1[c1];
            INC(c1);
          END;
          FOR c2 := 1 TO newlen DO
            s2[c2] := CHR(GetCardDgt(ha[c2].uch)*16 + GetCardDgt(ha[c2].Lch));
          END;
          s2[newlen+1] := NULL;
(*
          WriteString(' Output string is ');
          WriteCard(newlen);
          WriteString(' characters long, and is: ');
          WriteString(s2);
          WriteLn;
*)
        ELSE                          (* Ascii -> Hex *)
          FOR c1 := 1 TO len DO 
            temp := ORD(s1[c1]);
            ha[c1].uch := GetHexDgt(temp/16);
            ha[c1].Lch := GetHexDgt(temp MOD 16);
          END;
          c1 := 1;
          FOR c2 := 1 TO len DO 
            s2[c1] := ha[c2].uch;
            INC(c1);
            s2[c1] := ha[c2].Lch;
            INC(c1);
          END;
          s2[c1] := NULL;
        END;
END DoIt;

(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr         : Colors;
    x,y         : COORDINATE;
    i,int       :  INTEGER;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        RETURN OkayToClose;
    | TWM_CREATE: 
        ch1 := 'a';
        ch2 := 0c;
        ch3 := 0c;
        str1 := '';
        str2 := '';
        longstr := '';
        str4 := '';
        xCaret := InputPromptLen;
        yCaret := 0;
        inputline := '';
        SetScrollDisableWhenNone(tw,TRUE,TRUE);
    | TWM_SIZE:
        GetClientSize(tw,cxScreen,cyScreen);
        cxClient := msg.width;
        cyClient := msg.height;
        SnapWindowToFont(tw,TRUE);
        SetDisplayMode(tw,DisplayNormal);
        SetScrollRangeAllowed(tw,WA_VSCROLL,60);
        SetScrollBarPos(tw,WA_VSCROLL,0);
        SetScrollRangeAllowed(tw,WA_HSCROLL,100);
        SetScrollBarPos(tw,WA_HSCROLL,0);
        SetCaretType(tw,CtHalfBlock);
        MoveCaretTo(tw,xCaret,yCaret);
        MakeCaretVisible(tw);
        CaretOn(tw);
        SetWindowEnable(tw,TRUE);
        SetForegroundWindow(tw);
    | TWM_GAINFOCUS, TWM_ACTIVATEAPP :
        MoveCaretTo(tw,xCaret, yCaret);
        MakeCaretVisible(tw);
    |
    TWM_PAINT:
        WriteStringAt(tw,0,0,InputPrompt,a);
        WriteString(tw,inputline,a);
        EraseToEOL(tw,a);
        WriteStringAt(tw,cxClient-INT(LastModLen),0,LastMod,a);
        WriteLn(tw);
        WriteString(tw,' Input string is: ',a);
        WriteString(tw,inputline,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteString(tw,' Output string is: ',a);
        WriteString(tw,s2,a);
        EraseToEOL(tw,a);
        WriteLn(tw);        
        MoveCaretTo(tw,InputPromptLen,0);

    |
    TWM_KEY:
     IF inputprocessed THEN
      inputline := '';
      MoveCaretTo(tw,0,0);
      inputprocessed := FALSE;
      CaretOn(tw);
      RepaintScreen(tw);
     END;
     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN 
        CASE msg.k_ch OF 
          CHR(8) :                                     (* backspace      *)
            Strings.Delete(inputline,LENGTH(inputline)-1,1);
            x := Xpos(tw);
            WriteStringAt(tw,x-1,0,' ',a);
            MoveCaretTo(tw,x-1,0);
        | CHR(9) :                    (* tab                 *)

        | CHR(10):                    (* line feed           *)

        | CHR(13):                                    (* carriage RETURN *)
            inputprocessed := TRUE;
            CaretOff(tw);
            ASSIGN2BUF(inputline,inputbuf);
            IF inputbuf.COUNT = 0 THEN 
            (* FUNC *) CloseWindow(tw, CM_REQUEST);
            ELSIF STRCMPFNT(inputline,'FROMCLIP') = 0 THEN 
              IF NOT ClipboardFormatAvailable(clipfmt) THEN
                WriteStringAt(tw,0,0,'text format not available in clipboard.',a);
                RETURN DEFAULT_HANDLE;
              ELSIF NOT OpenClipboard(tw) THEN
                WriteString(tw,' OpenClipboard failed.',a);
                WriteLn(tw);
                RETURN DEFAULT_HANDLE;
              END;
              pinstr := GetClipboard(clipfmt);
              IF pinstr = NIL THEN
                WriteStringAt(tw,0,1,'unable to get clipboard.',a);
                RETURN DEFAULT_HANDLE;
              END;
              inputline := pinstr^;
              UnlockClipboardMemory;
              CloseClipboard(tw);
            END;
            DoIt(inputline,s2);
            WriteStringAt(tw,0,10,' Input string is: ',a);
            WriteString(tw,inputline,a);
            EraseToEOL(tw,a);
            WriteLn(tw);
            WriteString(tw,' Output string is: ',a);
            WriteString(tw,s2,a);
            EraseToEOL(tw,a);
            WriteLn(tw);
            ch := BasicDialogs.YesNo(' Put Output string in clipboard? ','N');
            IF CAP(ch) = 'Y' THEN
              IF NOT (OpenClipboard(tw) AND EmptyClipboard(tw)) THEN
                WriteString(tw,' Open and Empty Clipboard failed.',a);
                WriteLn(tw);
                CloseClipboard(tw);
                RETURN DEFAULT_HANDLE;
              END;
              pinstr := AllocClipboardMemory(BUFSIZ);
              IF pinstr = NIL THEN
                WriteString(tw,' Alloc clipboard memory failed.',a);
                WriteLn(tw);
                UnlockClipboardMemory;
                CloseClipboard(tw);
                RETURN DEFAULT_HANDLE;
              END;
              pinstr^ := s2;
              IF  NOT SetClipboard(clipfmt) THEN 
                WriteString(tw,' Setclipboard failed.',a); 
                WriteLn(tw);
              END;
              CloseClipboard(tw);
            END;


            RepaintScreen(tw);
        | CHR(27):             (* escape *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);

        ELSE (* CASE ch *)
          Strings.Append(msg.k_ch,inputline);
          WriteString(tw,msg.k_ch,a);

        END (* case ch *);
      ELSIF msg.k_special = KEY_PAGEUP THEN
      ELSIF msg.k_special = KEY_PAGEUP THEN
      ELSIF msg.k_special = KEY_PAGEDOWN THEN 
      ELSIF msg.k_special = KEY_HOME THEN
      ELSIF msg.k_special = KEY_END THEN
      ELSIF msg.k_special = KEY_RIGHTARROW THEN
      ELSIF msg.k_special = KEY_LEFTARROW THEN
      ELSIF msg.k_special = KEY_UPARROW THEN
      ELSIF msg.k_special = KEY_DOWNARROW THEN
      ELSIF msg.k_special = KEY_INSERT THEN
      ELSIF msg.k_special = KEY_DELETE THEN
        Strings.Delete(inputline,LENGTH(inputline)-1,1);
        x := Xpos(tw);
        WriteStringAt(tw,x-1,0,' ',a);
        MoveCaretTo(tw,x-1,0);
      ELSE (* msg.k_special *)
      END (*if*);
     END(* for *);
    ELSE (* case msg.msg *)
    END (* case msg.msg *);
    
    RETURN DEFAULT_HANDLE;
END WndProcTW;

PROCEDURE Start(param : ADDRESS);
BEGIN
    UNREFERENCED_PARAMETER(param);
    Win := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "Ascii To Hex for TextWindows Module", (* name : ARRAY OF CHAR *)
                        "",        (* menu : ARRAY OF CHAR *)
                        "",        (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        100,20, (* xSize, ySize : COORDINATE *)
                        180,100, (* xBuffer, yBuffer : COORDINATE *)
                        FALSE,  (* gutter : BOOLEAN *)
                        DefaultFontInfo, (* font : FontInfo *)
                        ComposeAttribute(Black, White, NormalFont), (* background : ScreenAttribute *)
                        ToplevelWindow,  (* windowType : WindowTypes *)
                        WndProcTW,
                        NormalWindow + AddScrollBars,    (* attribs : WinAttrSet *)
                        NIL); (* createParam : ADDRESS *)
    IF Win = NIL THEN
        WinShell.TerminateDispatchMessages(127);
    END;
    SetAutoScroll(Win,TRUE);
END Start;

(********************************** Main body ************************************)
BEGIN
  InputPromptLen := LENGTH(InputPrompt);
  LastModLen := LENGTH(LastMod);
  a := ComposeAttribute(Black, White, NormalFont);
  inputprocessed := FALSE;
  
  FUNC WinShell.DispatchMessages(Start, NIL);
END AsciiHexTW.
