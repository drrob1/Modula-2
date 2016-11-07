<*/NOWARN:F*>
(*--------------------------------------*)
MODULE ClipTest;
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
(*
FROM WinShell IMPORT
    Window, StartupDisplayMode, WinShellMsg, MessageRec, SetResourceFile,
    DispatchMessages, TerminateDispatchMessages,
    LoadString, CloseAllChildren, (*NormalWindow,*) AddStatusLine,
    SetStatusFormat, WriteStatusField, (*CreateWindow, WindowTypes,*) ClientTypes,
    ClientCreateData, TabPosition;
*)
IMPORT WinShell;
FROM DlgShell IMPORT
    NotifyType, NotifyResult, CONTROL, ControlType, DialogPositions,
    ModelessDialog, GetDialogParent;
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
  IMPORT RConversions, LongStr, LongConv,FormatString,WholeStr;
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
(*
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
*)
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;

  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;
(*
  FROM MiscM2 IMPORT Error, WriteString, WriteLn, WriteLongReal, Sqrt, CLS, PressAnyKey;
*)
FROM MYRANDOM IMPORT RANDCARD, RANDREAL;

CONST
  szAppName = "ClipTest";
  InputPrompt = 'Enter cmd or HELP : ';
  LastMod = '28 Nov 03';
(* ClipboardFormat     = (CLIPBOARD_ASCII, CLIPBOARD_UNICODE); *)
  clipfmt = CLIPBOARD_ASCII;

VAR
  hwnd        :  WIN32.HWND;
  msg         :  WINUSER.MSG;
  wc          :  WINUSER.WNDCLASSEX;

TYPE
  OutStateTyp = (fix,float,gen);
  STR20TYP      = ARRAY [0..20] OF CHAR;
  pbuftyp     = POINTER TO STRTYP;

VAR
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
  sigfig,c1,c2,c3    :  CARDINAL;
  inputline,str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  longstr     : ARRAY [0..5120] OF CHAR;
  OutState : OutStateTyp = fix;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf    : BUFTYP;
  pinbuf      : pbuftyp;
  r           : LONGREAL;
  L           : LONGINT;
  LC          : LONGCARD;
  DAYNAMES    : ARRAY [0..6] OF STR10TYP;
  a           : ScreenAttribute;
  Win         : TextWindow;


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
(*        PutStringAt(tw, 0, 0,'                                               ',a); *)
        WriteStringAt(tw,0,0,InputPrompt,a);
        WriteString(tw,inputline,a);
        EraseToEOL(tw,a);
        WriteStringAt(tw,cxClient-INT(LastModLen),0,LastMod,a);
        WriteLn(tw);
        MoveCaretTo(tw,InputPromptLen,0);

    | TWM_NOTIFYSCROLL:
(*
        writehelp(tw);
        RepaintScreen(tw);
*)
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
            bool := OpenClipboard(tw);
            IF OpenClipboard(tw) AND EmptyClipboard(tw) THEN
              WriteString(tw,' Open and Empty Clipboard succeeded.',a);
            ELSE
              WriteString(tw,' Open and Empty Clipboard failed.',a);
              CloseClipboard(tw);
              HALT;
            END;
            WriteLn(tw);
            pinbuf := AllocClipboardMemory(BUFSIZ);
            IF pinbuf <> NIL THEN
              WriteString(tw,' Getclipboard succeeded.',a);
            ELSE
              WriteString(tw,' Getclipboard failed.',a);
              UnlockClipboardMemory;
              CloseClipboard(tw);
              HALT;
            END;
            c2 := RANDCARD(1000);
            WholeStr.CardToStr(c2,str1);
            c3 := 1;
            WHILE str1[c3] # NULL DO
            	pinbuf^[c3] := str1[c3];
            	INC(c3);
            END;
            pinbuf^[c3] := NULL;
            IF  SetClipboard(clipfmt) THEN 
              WriteString(tw,' SetClipboard succeeded.',a);
            ELSE
              WriteString(tw,' Setclipboard failed.',a);
              CloseClipboard(tw);
              HALT;
            END;
            CloseClipboard(tw);

        | CHR(9) :                    (* tab                 *)
(*
PROCEDURE OpenClipboard(tw : TextWindow) : BOOLEAN;
PROCEDURE CloseClipboard(tw : TextWindow);
PROCEDURE EmptyClipboard(tw : TextWindow) : BOOLEAN;
PROCEDURE ClipboardFormatAvailable(fmt : ClipboardFormat) : BOOLEAN;
PROCEDURE AllocClipboardMemory(size : CARDINAL) : ADDRESS;
PROCEDURE UnlockClipboardMemory;
PROCEDURE SetClipboard(fmt : ClipboardFormat) : BOOLEAN;
PROCEDURE GetClipboard(fmt : ClipboardFormat) : ADDRESS;
*)
          WriteString(tw,' Will try to read the clipboard.',a);
          WriteLn(tw);
          bool := OpenClipboard(tw);
          IF bool THEN
            WriteString(tw,' OpenClipboard succeeded.',a);
          ELSE
            WriteString(tw,' OpenClipboard failed.',a);
            HALT;
          END;
          WriteLn(tw);
          pinbuf := GetClipboard(clipfmt);
          IF pinbuf <> NIL THEN
            WriteString(tw,' Getclipboard succeeded.',a);
          ELSE
            WriteString(tw,' Getclipboard failed.',a);
            UnlockClipboardMemory;
            CloseClipboard(tw);
            HALT;
          END;
          WriteLn(tw);
          c1 := 1;
          WHILE pinbuf^[c1] # NULL DO
            inputbuf.CHARS[c1] := pinbuf^[c1];
            INC(c1);
          END;
          UnlockClipboardMemory;
          CloseClipboard(tw);
          inputbuf.CHARS[c1] := NULL;
          WriteString(tw,inputbuf.CHARS,a);
          WriteLn(tw);

        | CHR(10):                    (* line feed           *)

        | CHR(13):                                    (* carriage RETURN *)
            inputprocessed := TRUE;
            CaretOff(tw);
            ASSIGN2BUF(inputline,inputbuf);
            IF inputbuf.COUNT = 0 THEN HALT END;
(*
            IF STRCMPFNT(inputline,'FIX') = 0 THEN
              OutState := fix;
              BasicDialogs.PromptCard('SigFig:',0,20,TRUE,sigfig);
            ELSIF STRCMPFNT(inputline,'FLOAT') = 0 THEN
              OutState := float;
              BasicDialogs.PromptCard('SigFig:',0,20,TRUE,sigfig);
            ELSIF STRCMPFNT(inputline,'GEN') = 0 THEN
              OutState := gen;
(*              BasicDialogs.PromptCard('SigFig:',0,20,TRUE,sigfig); *)
            ELSIF STRCMPFNT(inputline,'EXIT') = 0 THEN
              HALT;
            ELSIF STRCMPFNT(inputline,'HEX') = 0 THEN
              r := READX();
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
            ELSIF STRCMPFNT(inputline,'PRIME') = 0 THEN

              r := READX();
              LC := VAL(LONGCARD,(ABS(r)));
              bool := IsPrime(LC);
              IF bool THEN
                FormatString.FormatString(' %l is prime.',str5,LC);
              ELSE
                FormatString.FormatString(' %l is not prime.',str5,LC);
              END;
              BasicDialogs.MessageBox(str5,MsgInfo);

            ELSIF STRCMPFNT(inputline,'DOW') = 0 THEN
              int := round(READX());
              str3 := DAYNAMES[int MOD 7];
              BasicDialogs.MessageBox(str3,MsgInfo);
            ELSIF STRCMPFNT(inputline,'HELP') = 0 THEN writehelp(tw)


            ELSE
(* Now must process the cmd, which is the whole reason for this pgm existing! *)
              GETRESULT(inputbuf);
            END;
*)
            RepaintScreen(tw);
        | CHR(27):             (* escape *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);


        ELSE (* CASE ch *)
          Strings.Append(CAP(msg.k_ch),inputline);
          WriteString(tw,msg.k_ch,a);
(*          RepaintScreen(tw); *)
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
(*        RepaintScreen(tw); *)
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
(*
  PROCEDURE CreateWindow(parent : WinShell.Window;
                       name : ARRAY OF CHAR;
                       menu : ARRAY OF CHAR;
                       icon : ARRAY OF CHAR;
                       x, y : COORDINATE;
                       xSize, ySize : COORDINATE;
                       xBuffer, yBuffer : COORDINATE;
                       gutter : BOOLEAN;
                       font : FontInfo;
                       background : ScreenAttribute;
                       windowType : WindowTypes;
                       wndProc : TextWindowProcedure;
                       attribs : WinAttrSet;
                       createParam : ADDRESS) : TextWindow;
(* create a new window *)
(* parent = as WinShell  *)
(* name = as WinShell  *)
(* menu = the menu for the window. Can be "". *)
(* icon =  as WinShell *)
(* attribs = as WinShell *)
(* wndProc = the window procedure *)
(* createParam = an arbitrary value you can use to pass information
                 to the window procedure of the window. this value is
                 passed in the WSM_CREATE message. *)
(* font = the font to use for this window *)
(* background = the background color for this window *)
(* gutter = TRUE then the text window will always have a blank "gutter"
            on the left edge of the text window.
            FALSE the text will start at the left edge of the client area.
            *)
(* x, y = the initial screen coordinates for the window to be displayed
          if a parameter is -1 then the operating system will choose
          a default location for that coordinate.
          these positions are in pixels and are relative to the
          parent window client area origin for child windows
          or relative to the screen origin for all other windows. *)
(* xSize, ySize = the initial width and height in character cells
                  if -1 then a system default size will be used *)
(* xBuffer, yBuffer = the size of the screen buffer. the window can never
                      be larger than the screen buffer. if either xBuffer
                      or yBuffer is -1 the screen buffer is a variable size
                      and is sized to the number of cells the window client
                      area currently is capable displaying. *)
(* returns the window handle if successfull, otherwise NIL *)
*)
    Win := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "HPWin for TextWindows Module", (* name : ARRAY OF CHAR *)
                        "",        (* menu : ARRAY OF CHAR *)
                        "",        (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        50,20, (* xSize, ySize : COORDINATE *)
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
  a := ComposeAttribute(Black, White, NormalFont);
  inputprocessed := FALSE;

  FUNC WinShell.DispatchMessages(Start, NIL);
END ClipTest.
