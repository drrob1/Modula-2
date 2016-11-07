<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:SS6.RES*>
%ELSE
%END

(*--------------------------------------
REVISION HISTORY
================
 7 Feb 05 -- Frank answered my question about negative numbers and mouse_event.
25 Apr 05 -- Due to change in policy at CMC, I am planning to add a feature that every 10 min
              will wiggle mouse and return mouse cursor to starting point.  And write LastMod.
              Even though this wiggle mouse part may not be necessary, it's fun to write.
27 Apr 05 -- Changed name to SS5 and made mouse move just before screen saver setting.
29 Jan 06 -- Changed name to SS6 and added mouse click events
--------------------------------------*)

MODULE SS6;
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS, CAST, DWORD;
IMPORT WINUSER, WIN32, WINGDI, WINX;
IMPORT Strings, ExStrings, MemUtils;
FROM Strings IMPORT
    Append, Equal, Delete, Concat, Capitalize;
FROM ExStrings IMPORT
    AppendChar, EqualI;
FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, FontInfo, WinAttr, ClipboardFormat,
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
    SetDisplayMode,GetDisplayMode,SetWindowEnable, (*  SetWindowTitle, *)
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage,
    IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars,
    SetScrollBarPos, SetWindowData, SetWindowDataNum, GetWindowData, GetWindowDataNum,
    GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos, CascadeWindow,
    SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo,
    SetScrollDisableWhenNone, SetActiveTabChild, SetTabChildPosition,
    GetForegroundWindow, SetForegroundWindow, SetWindowFont,
    SetTimer, KillTimer, DisplayHelp, SetWindowIcon,
    OpenClipboard, CloseClipboard, EmptyClipboard, ClipboardFormatAvailable,
    AllocClipboardMemory, UnlockClipboardMemory, SetClipboard, GetClipboard,
    Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;
(*
FROM WinShell IMPORT
    Window, StartupDisplayMode, WinShellMsg, MessageRec, SetResourceFile,
    DispatchMessages, TerminateDispatchMessages,
    LoadString, CloseAllChildren, /*NormalWindow,*/ AddStatusLine,
    SetStatusFormat, WriteStatusField, /*CreateWindow, WindowTypes,*/ ClientTypes,
    ClientCreateData, TabPosition;
FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts /*drive path name extension*/, FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle,
    MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary,
    AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName,
    CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock,
    WriteBlock, ReadChar, WriteChar, PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength,
    GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList,
    FindInOSPathList, ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
*)
(* IMPORT Lib; Part of TSLib *)
(* IMPORT MATHLIB; Part of TSlib that uses longreals. *)
IMPORT WinShell;
FROM DlgShell IMPORT
    NotifyType, NotifyResult, CONTROL, ControlType, DialogPositions,
    ModelessDialog, GetDialogParent;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT LongMath;
IMPORT ASCII;
FROM Environment IMPORT GetCommandLine;
FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
IMPORT RConversions, LongStr, LongConv,FormatString;
FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
FROM LongStr IMPORT StrToReal,RealToFloat,RealToEng,RealToFixed,RealToStr,ConvResults;

FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SubStrCMPFNT,SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;
FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;

CONST
  szAppName = "SS6";  (* Screen Saving Dancing Mouse 4, now using text windows *)
  InputPrompt = 'Enter cmd or HELP : ';
  LastMod = '29 Jan 06';
  clipfmt = CLIPBOARD_ASCII;
  SS6Icon32 = '#100';
  SS6Icon16 = '#200';
  MouseTimer  = 1;
  ClockTimer = 2;
  MouseMovementAmt = -30;  (* Increased this constant so downclick and upclick are further apart *)
  TimeToWiggleMouse = 720; (* 720 sec is 12 min *)

TYPE
  STR20TYP    = ARRAY [0..20] OF CHAR;
  pstrtyp     = POINTER TO STRTYP;  (* remember this is a 1-origin array *)

VAR
  hwnd        :  WIN32.HWND;
  msg         :  WINUSER.MSG;
  wc          :  WINUSER.WNDCLASSEX;
  cxBuffer: INTEGER;
  cyBuffer: INTEGER;
  cxScreen,cyScreen,wxClient,wyClient : COORDINATE;
  xCaret  : INTEGER;
  yCaret  : INTEGER;
  ch2,ch3 :  CHAR;
  bool,inputprocessed,hpFileExists :  BOOLEAN;
  sigfig,c1,c2,c3,SSTimeOut        :  CARDINAL;
  inputline,HPFileName,Xstr,str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  longstr     : ARRAY [0..5120] OF CHAR;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf    : BUFTYP;
  pinstr      : pstrtyp;
  convres     : ConvResults;
  r           : LONGREAL;
  L           : LONGINT;
  LC          : LONGCARD;
  DAYNAMES    : ARRAY [0..6] OF STR10TYP;
  a,aBold     : ScreenAttribute;
  Win         : TextWindow;
  Reg         : ARRAY [0..35] OF LONGREAL;
  biggerFont  : FontInfo;
  ch1             :  CHAR='N';
  BeepParam,FlashTime,Color,CountUpTimer,RapidTimer,MinutesLeft,SecondsLeft,HoursLeft,ColorVal,
    ColorValRed,ColorValBlue,ColorValGreen,ScreenSaving : CARDINAL;
  MinutesLeftStr,SecondsLeftStr,HoursLeftStr,WindowText,ScreenSavingStr : STRTYP;
  AlarmTime : CARDINAL = 1;
  cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth,
  iVscrollPos, iVscrollMax, iHscrollPos, iHscrollMax                    : INTEGER;
  mousemoveamt    : INTEGER = -10;
  LongZero        : INTEGER64 = 0;
  boolp           : POINTER TO BOOLEAN;
  WiggleMouse     : INTEGER;
(*
    FontWeights         = (FwLight, FwNormal, FwDemiBold, FwBold, FwHeavy);

    CharacterSets       = (
                           LATIN1_CHARSET,/*ansi, iso*/
                           LATIN2_CHARSET,/*latin + eastern europe*/
                           SYMBOL_CHARSET,
                           ASCII_CHARSET,
                           DEFAULT_CHARSET
                          );
    FontInfo            =
        RECORD
        height          : INTEGER;/* positive = tenths of a point.
                                                100 = 10.0 points
                                                105 = 10.5 points.
                                     negative = device pixels
                                  */
        italic          : BOOLEAN;
        fixedPitch      : BOOLEAN;/* if TRUE then only fixed pitch */
                                  /* if FALSE then any pitch, including fixed */
        weight          : FontWeights;
        charSet         : CharacterSets;
        name            : ARRAY [0..63] OF CHAR;/* font family name.
                                                   GTK
                                                     may also optionally include
                                                     the font foundary in the name.
                                                     the foundary and font family name
                                                     are separated by the '-' character.
                                                */
        END;
*)


(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr    : Colors;
    x,y    : COORDINATE;
    i,int  : INTEGER;
    idx, c : CARDINAL;
    ans    : CHAR;
    bool   : BOOLEAN;

BEGIN
    c := 0;
    CASE msg.msg OF
    TWM_CLOSE:
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        KillTimer(tw, MouseTimer);
        KillTimer(tw, ClockTimer);
        DISPOSE(boolp);
        RETURN OkayToClose;
    | TWM_CREATE:
        FUNC SetWindowIcon(tw, SS6Icon16);
        ch1 := 'a';
        ch2 := 0c;
        ch3 := 0c;
        str1 := '';
        str2 := '';
        longstr := '';
        str4 := '';
        xCaret := 10;
        yCaret := 0;
        inputline := '';
        SetScrollDisableWhenNone(tw,TRUE,TRUE);
        bool := WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVETIMEOUT,c,ADR(SSTimeOut),c);
        DEC(SSTimeOut,5);  (* Give 5 sec leeway in case computer is busy or something like that *)
(*        SSTimeOut := 3;  For testing purposes only *)
        WiggleMouse := SSTimeOut;


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

    | TWM_PAINT:
        WriteStringAt(tw,0,0,SecondsLeftStr,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        CardToStr(ScreenSaving, str0);
        WriteString(tw,str0,a);
        EraseToEOL(tw,a);

    | TWM_TIMER:
(*
DWORD is typed as a CARDINAL here, so you cannot assign negative numbers easily.
The solution is to use type casting, like:
mouse_event (MOUSEEVENTF_MOVE, CAST(DWORD,dx), CAST(DWORD,dy), 0, 0);
*)
      IF msg.timerId = MouseTimer THEN
        WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVERRUNNING,c,boolp,c);

        IF boolp^ THEN
          INC(ScreenSaving);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,CAST(DWORD, mousemoveamt),
                              CAST(DWORD, mousemoveamt), 0, 0);
        ELSIF WiggleMouse <= 0 THEN
          WiggleMouse := SSTimeOut;
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE, CAST(DWORD, mousemoveamt),
                              CAST(DWORD, mousemoveamt), 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE, CAST(DWORD, -mousemoveamt),
                              CAST(DWORD, -mousemoveamt), 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
        END (*if boolp*);

      ELSIF msg.timerId = ClockTimer THEN
        DEC(WiggleMouse);
        INC(CountUpTimer);
        HoursLeft := CountUpTimer/3600;
        MinutesLeft := (CountUpTimer MOD 3600) / 60;
        SecondsLeft := (CountUpTimer MOD 60);
        FormatString.FormatString("%2c:%c:%c",SecondsLeftStr,HoursLeft,MinutesLeft,SecondsLeft);
        WindowText  :=  SecondsLeftStr;
        SetWindowTitle(tw, WindowText);
(*
        RepaintScreen(tw);
        or maybe just
*)
        WriteStringAt(tw,0,0,SecondsLeftStr,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        CardToStr(ScreenSaving, str0);
        WriteString(tw,str0,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        IntToStr(WiggleMouse, str0);
        WriteString(tw,' Wiggle Mouse in : ',a);
        WriteString(tw,str0,a);
        WriteString(tw,' seconds.',a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        WriteString(tw,LastMod,a);
        EraseToEOL(tw,a);
        WriteLn(tw);

      END (*if timerId *);
    |
    TWM_KEY:
     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN
        CASE msg.k_ch OF
          CHR(8) :                      (* backspace       *)
        | CHR(9) :                      (* tab             *)
        | CHR(10):                      (* line feed       *)
        | CHR(13):                      (* carriage RETURN *)
                    CloseWindow(tw,CM_REQUEST);
        | CHR(27):                               (* escape *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);

        | ' '    :  CountUpTimer := 0;
                    ScreenSaving := 0;
                    WiggleMouse := SSTimeOut;
        ELSE (* CASE ch *)
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
                        "SS6 Dancing Mouse TW", (* name : ARRAY OF CHAR *)
                        "",        (* menu : ARRAY OF CHAR *)
                        SS6Icon16, (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        32,7, (* xSize, ySize : COORDINATE *)
                        -1,-1, (* xBuffer, yBuffer : COORDINATE *)
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
    SetTimer(Win, MouseTimer, 300);
    SetTimer(Win, ClockTimer, 1000);
    NEW(boolp);
END Start;


(*++++*****************************************************************)

BEGIN

(* PROCEDURE SetWindowTitle(tw : TextWindow; title : ARRAY OF CHAR);                                 *)
(* PROCEDURE SetTimer(tw : TextWindow; timerId : CARDINAL; interval : CARDINAL);                     *)
(*
   create/reset a timer associated with the specified window
   timerId = a unique number to identify the timer.
   interval = the amount of time in milliseconds between WSM_TIMER messages
              this interval is only an approximate time
   calling SetTimer with the same timerId as a previous call but with
   a different interval has the effect of resetting the interval from
   the previous value to the new value
*)
(* PROCEDURE KillTimer(tw : TextWindow; timerId : CARDINAL);  InputPromptLen := LENGTH(InputPrompt); *)
  a := ComposeAttribute(Black, White, NormalFont);
  aBold := ComposeAttribute(Black, White, BoldFont);
  biggerFont := DefaultFontInfo;
  biggerFont.height := 140;
  SecondsLeftStr := '';
  WindowText :=  '';
  ScreenSaving := 0;
  CountUpTimer := 0;

  FUNC WinShell.DispatchMessages(Start, NIL);
END SS6.
