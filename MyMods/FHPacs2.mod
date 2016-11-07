<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:FHPacs.RES*>
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
30 Jan 06 -- Changed name to SS7 and moved mouse click events to within test for screensave on.
              I also determined that Jam-Flushing PACS computers allow me to turn off the screensaver.
              I decided to leave it on and only click mouse when screen saver is on.
 1 Feb 06 -- Changed name as I fiddle with its purpose tailored to the PACS system.  I want it to
              keep PACS from timing out, and change the screen power settings on that workstation.
              And I took out the check for screen saver.
 6 Feb 06 -- Added menu, fixed bug in sleep mode & WiggleMouse, and changed key purposes.
11 Feb 06 -- Added DaysLeft and associated logic.
19 Feb 06 -- Added an erasetoeol call.
26 Feb 06 -- erasetoeol call does not seem to be doing what I want w/ a trailing 's'.  So I
              padded a string w/ blanks.
 1 Mar 06 -- I think I found the erase2eol pblm, and I changed timer values to more closely match
              fast machines, and changed mouse move & click sequence.
             In changing this to version 2, I am trying to add the keyboard stuff.
20 Jul 06 -- Added key option whereby timer continues but computer does not affect mouse.  I'll use <home>.
              And reverse operation will be <end>.  Menu options are Groggy and Wake Up.
19 Oct 06 -- Added logic to include disk manipulations to prevent disks from falling asleep.  And
              changed away from sendinput because it didn't work as well (maybe I didn't do it right)
              and I included an <enter> keypress.
--------------------------------------*)

MODULE FHPacs2;
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
FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts (*drive path name extension*), FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle,
    MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary,
    AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName,
    CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock,
    WriteBlock, (* ReadChar, WriteChar, *) PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength,
    GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList,
    FindInOSPathList, ExpandFileSpec, FindFirst, (* FindNext, *) FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;

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
  szAppName = "FHPacs2";
  InputPromptLn1 = ' <tab> Sleep in 5; <sp> <bsp> Wake up; <del> emerg screen timer reset. ';
  InputPromptLn2 = ' <home> stop mouse movement; <end> resume mouse movements. ';
  LastMod = '24 Oct 06';
  clipfmt = CLIPBOARD_ASCII;
  FHIcon32 = '#100';
  FHIcon16 = '#200';
  MouseTimer  = 1;
  ClockTimer = 2;
  EmergencyScreenReset = 7200;
  VK_MENU              = 12h;    (* Alt Key *)
  VK_a                 = 41h;    (* a key *)
  VK_LMENU             = 0A4h;   (* L alt key *)
  VK_RMENU             = 0A5h;   (* R alt key *)
  VK_RETURN            = WINUSER.VK_RETURN;    (* enter or return key *)


TYPE
(*  STR20TYP    = ARRAY [0..20] OF CHAR; *)
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
  bool, inputprocessed, sleep, ok :  BOOLEAN;
  TimeOutReset,c1,c2,c3,SSTimeOut,PowerTimeOut : CARDINAL;
  inputline,HPFileName,Xstr,str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  longstr     : ARRAY [0..5120] OF CHAR;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf    : BUFTYP;
  pinstr      : pstrtyp;
  convres     : ConvResults;
  temparray   : ARRAY [0..10] OF CARDINAL;
  c           : CARDINAL;
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
    ColorValRed,ColorValBlue,ColorValGreen,ScreenSaving,Days,Hours : CARDINAL;
  MinutesLeftStr,SecondsLeftStr,HoursLeftStr,WindowText,ScreenSavingStr : STRTYP;
  AlarmTime : CARDINAL = 1;
  cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth,
  iVscrollPos, iVscrollMax, iHscrollPos, iHscrollMax                    : INTEGER;
  f               : File;
  mousemoveamt    : INTEGER = -20;
  LongZero        : INTEGER64 = 0;
  boolp           : POINTER TO BOOLEAN;
  WiggleMouse     : INTEGER;
   (* Alt-Dn, Alt-Up, A-Dn, A-Up *)
  InitInput,altdn,altup,adn,aup,mouseMove,mouseBack,mouseLclickDn,mouseLclickUp,InitMouse : WINUSER.INPUT;
  InputArray : ARRAY[0..9] OF WINUSER.INPUT;
  pinput     : WINUSER.PINPUT;
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
(*  Desciption of data structures for INPUT.
CONST
    KEYEVENTF_EXTENDEDKEY= 00001h;
    KEYEVENTF_KEYUP      = 00002h;

PROCEDURE keybd_event(bVk : BYTE;
                      bScan : BYTE;
                      dwFlags : DWORD;
                      dwExtraInfo : DWORD);


CONST
    MOUSEEVENTF_MOVE     = 00001h;
    MOUSEEVENTF_LEFTDOWN = 00002h;
    MOUSEEVENTF_LEFTUP   = 00004h;
    MOUSEEVENTF_RIGHTDOWN= 00008h;
    MOUSEEVENTF_RIGHTUP  = 00010h;
    MOUSEEVENTF_MIDDLEDOWN= 00020h;
    MOUSEEVENTF_MIDDLEUP = 00040h;
    MOUSEEVENTF_WHEEL       = 00800h; /* wheel button rolled */
    MOUSEEVENTF_ABSOLUTE = 08000h;

PROCEDURE mouse_event(dwFlags : DWORD;
                      dx : DWORD;
                      dy : DWORD;
                      cButtons : DWORD;
                      dwExtraInfo : DWORD);

TYPE
    /* NT only */
    MOUSEINPUT = RECORD
                   dx          : LONG;
                   dy          : LONG;
                   mouseData   : DWORD;
                   dwFlags     : DWORD;
                   time        : DWORD;
                   dwExtraInfo : DWORD;
                 END;
    PMOUSEINPUT = POINTER TO MOUSEINPUT;
    LPMOUSEINPUT = PMOUSEINPUT;

    /* NT only */
    KEYBDINPUT = RECORD
                   wVk         : WORD;
                   wScan       : WORD;
                   dwFlags     : DWORD;
                   time        : DWORD;
                   dwExtraInfo : DWORD;
                 END;
    PKEYBDINPUT = POINTER TO KEYBDINPUT;
    LPKEYBDINPUT = PKEYBDINPUT;

    /* NT only */
    INPUT = RECORD
              type        : DWORD;
              CASE : CARDINAL OF
                  0: mi : MOUSEINPUT;|
                  1: ki : KEYBDINPUT;|
                  2: hi : HARDWAREINPUT;
              ELSE
              END;
            END;
    PINPUT = POINTER TO INPUT;
    LPINPUT = PINPUT;

CONST
    INPUT_MOUSE     =0;
    INPUT_KEYBOARD  =1;
    INPUT_HARDWARE  =2;

/* NT only */
PROCEDURE SendInput(cInputs : UINT;     /* number of input in the array */
                    pInputs : INPUT;    /* array of inputs              */
                    cbSize : WINT) : UINT;    /* sizeof(INPUT)          */
*)

(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr    : Colors;
    x,y    : COORDINATE;
    i,int  : INTEGER;
    idx,c,k,SleepAmt : CARDINAL;
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
        bool := WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,PowerTimeOut,NIL,c);
        bool := WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,PowerTimeOut,NIL,c);
        RETURN OkayToClose;

    | TWM_CREATE:
        FUNC SetWindowIcon(tw, FHIcon16);
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
(*        DEC(SSTimeOut,5);  Give 5 sec leeway in case computer is busy or something like that *)
(*        SSTimeOut := 5;  For testing purposes only *)
        bool := WINUSER.SystemParametersInfo(WINUSER.SPI_GETPOWEROFFTIMEOUT,c,ADR(PowerTimeOut),c);
        TimeOutReset := PowerTimeOut - 1;
        WiggleMouse := TimeOutReset;
        sleep := FALSE;

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
        HoursLeft := CountUpTimer/3600;
        MinutesLeft := (CountUpTimer REM 3600) / 60;
        SecondsLeft := (CountUpTimer REM 60);
        Days := HoursLeft/24;
        Hours := HoursLeft REM 24;
        FormatString.FormatString("%2c:%c:%c",SecondsLeftStr,HoursLeft,MinutesLeft,SecondsLeft);
        MoveCaretTo(tw,0,0);
        CASE Days OF
          0: (* Do nothing *)
        | 1: CardToStr(Days,str0);
             CardToStr(Hours,str1);
             WriteString(tw,str0,a);
             WriteString(tw,' day ',a);
             IF Hours = 1 THEN
                 WriteString(tw,'1 hour',a);
             ELSE
                 WriteString(tw,str1,a);
                 WriteString(tw,' hours',a);
             END;
        ELSE
             CardToStr(Days,str0);
             CardToStr(Hours,str1);
             WriteString(tw,str0,a);
             WriteString(tw,' days ',a);
             IF Hours = 1 THEN
               WriteString(tw,'1 hour',a);
             ELSE
               WriteString(tw,str1,a);
               WriteString(tw,' hours',a);
             END;
        END; (* case Days *)
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        WriteString(tw,SecondsLeftStr,a);
        EraseToEOL(tw,a);

        WriteLn(tw);
        WriteLn(tw);

        WriteString(tw,InputPromptLn1,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteString(tw,InputPromptLn2,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        IF sleep THEN
          WriteString(tw,' Asleep.',a);
        ELSE
          WriteString(tw,' Awake.',a);
        END;
        EraseToEOL(tw,a);

        WriteLn(tw);
        IntToStr(WiggleMouse, str0);
        WindowText  :=  str0;
        SetWindowTitle(tw, WindowText);
        WriteString(tw,' Wiggle Mouse or Sleep in : ',a);
        WriteString(tw,str0,a);
        WriteString(tw,' seconds.',a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        WriteString(tw,LastMod,a);
        EraseToEOL(tw,a);
        WriteLn(tw);

    | TWM_MENU:
(*
  a menu item has been selected menuId = the menu resource id number for the menu item
  TWM_MENU:
       msg.menuId      : INTEGER;
       msg.accel       : BOOLEAN;
*)
         CASE msg.menuId OF
         0   : (* Sleep *)
               sleep := TRUE;
               SleepAmt := 5;
               WiggleMouse := SleepAmt +1; (* So timer does not display -1. *)
               bool :=
                 WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,SleepAmt,NIL,c);
               bool :=
                 WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,SleepAmt,NIL,c);
       | 5   : (* Reset WiggleMouse *)
              WiggleMouse := TimeOutReset;
              RepaintScreen(tw);
       | 10  : (* Reset Clock *)
              CountUpTimer := 0;
              RepaintScreen(tw);
       | 15  : (* Reset both WiggleMouse and Clock *)
              WiggleMouse  := TimeOutReset;
              CountUpTimer := 0;
              RepaintScreen(tw);
       | 17  : (* Groggy *)
              sleep := TRUE;  (* will stop mouse movement function only *)
              RepaintScreen(tw);
       | 18  : (* Wake Up *)
              sleep := FALSE;
              WiggleMouse := TimeOutReset;
              bool := WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,PowerTimeOut,NIL,c);
              bool := WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,PowerTimeOut,NIL,c);
              RepaintScreen(tw);

       | 20  : (* exit *)
              CloseWindow(tw,CM_REQUEST);
      ELSE (* do nothing but not an error *)
      END; (* case menuId *)

    | TWM_TIMER:
(*
DWORD is typed as a CARDINAL here, so you cannot assign negative numbers easily.
The solution is to use type casting, like:
mouse_event (MOUSEEVENTF_MOVE, CAST(DWORD,dx), CAST(DWORD,dy), 0, 0);
*)
      IF msg.timerId = MouseTimer THEN
        IF sleep THEN  (* Don't wiggle mouse *)
        ELSIF WiggleMouse <= 0 THEN
          WiggleMouse := TimeOutReset;

          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);  (* Do double click, I hope.*)
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,CAST(DWORD, mousemoveamt),
                              CAST(DWORD, mousemoveamt), 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE, CAST(DWORD, -mousemoveamt),
                              CAST(DWORD, -mousemoveamt), 0, 0);
(*
          WINUSER.keybd_event(VK_a,0,0,0);
          WINUSER.keybd_event(VK_a,0,WINUSER.KEYEVENTF_KEYUP,0);
*)
          WINUSER.keybd_event(VK_RETURN,0,0,0);
          WINUSER.keybd_event(VK_RETURN,0,WINUSER.KEYEVENTF_KEYUP,0);


(*          WINUSER.SendInput(10,pinput^,SIZE(WINUSER.INPUT));  *)

        CreateFile(f,'tempfile.txt');
        WriteBlock(f, ADR(temparray), SIZE(temparray) );
        CloseFile(f);

        ok := CopyFile('tempfile.txt','tmpfil.txt');
(*
        DeleteFile('tempfile.txt');
        DeleteFile('tmpfil.txt');
*)
        END (*if boolp*);

      ELSIF msg.timerId = ClockTimer THEN
        IF WiggleMouse > 0 THEN DEC(WiggleMouse); END;
        INC(CountUpTimer);
        RepaintScreen(tw);
(*
        HoursLeft := CountUpTimer/3600;
        MinutesLeft := (CountUpTimer MOD 3600) / 60;
        SecondsLeft := (CountUpTimer MOD 60);
        FormatString.FormatString("%2c:%c:%c",SecondsLeftStr,HoursLeft,MinutesLeft,SecondsLeft);
        WriteStringAt(tw,0,0,SecondsLeftStr,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteString(tw,InputPrompt,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        IntToStr(WiggleMouse, str0);
        WindowText  :=  str0;
        SetWindowTitle(tw, WindowText);
        WriteString(tw,' Wiggle Mouse or Sleep in : ',a);
        WriteString(tw,str0,a);
        WriteString(tw,' seconds.',a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        WriteString(tw,LastMod,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
*)
      END (*if timerId *);
    |
    TWM_KEY:
(*
TWM_KEY:
        k_count     : CARDINAL;
        k_special   : SpecialKeys;
        k_state     : KeyStateSet;
        k_ch        : CHAR;
*)
     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN
        CASE msg.k_ch OF
          CHR(8) :  (* backspace means wake up      *)
                    sleep := FALSE;
                    WiggleMouse := TimeOutReset;
                    bool :=
                      WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,PowerTimeOut,NIL,c);
                    bool :=
                      WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,PowerTimeOut,NIL,c);
                    RepaintScreen(tw);
        | CHR(9) :  (* tab means go to sleep               *)
                    sleep := TRUE;
                    SleepAmt := 5;
                    WiggleMouse := SleepAmt +1; (* So timer does not display -1. *)
                    bool :=
                      WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,SleepAmt,NIL,c);
                    bool :=
                      WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,SleepAmt,NIL,c);
                    RepaintScreen(tw);
        | CHR(10):                      (* line feed       *)
        | CHR(13):                      (* carriage RETURN *)
                    CloseWindow(tw,CM_REQUEST);
        | CHR(27):                               (* escape *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);

        | ' '    :  (* space wakes up and resets WiggleMouse *)
                    sleep := FALSE;
                    WiggleMouse := TimeOutReset;
                    bool :=
                      WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,PowerTimeOut,NIL,c);
                    bool :=
                      WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,PowerTimeOut,NIL,c);
                    RepaintScreen(tw);
        ELSE (* CASE ch *)
        END (* case ch *);
      ELSIF msg.k_special = KEY_PAGEUP THEN
      ELSIF msg.k_special = KEY_PAGEUP THEN
      ELSIF msg.k_special = KEY_PAGEDOWN THEN
      ELSIF msg.k_special = KEY_HOME THEN
        sleep := TRUE;  (* will stop mouse movement function only *)
        RepaintScreen(tw);
      ELSIF msg.k_special = KEY_END THEN
        sleep := FALSE; (* restore mouse movement function *)
        RepaintScreen(tw);
      ELSIF msg.k_special = KEY_RIGHTARROW THEN
      ELSIF msg.k_special = KEY_LEFTARROW THEN
      ELSIF msg.k_special = KEY_UPARROW THEN
      ELSIF msg.k_special = KEY_DOWNARROW THEN
      ELSIF msg.k_special = KEY_INSERT THEN
      ELSIF msg.k_special = KEY_DELETE THEN
        FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,EmergencyScreenReset,NIL,c);
        HALT;
      ELSE (* msg.k_special *)
(*
   KEY_MENU /*not all keyboards have this*/,
   KEY_RIGHTARROW, KEY_LEFTARROW, KEY_UPARROW, KEY_DOWNARROW,
   KEY_F1, KEY_F2, KEY_F3, KEY_F4, KEY_F5, KEY_F6,
   KEY_F7, KEY_F8, KEY_F9, KEY_F10, KEY_F11, KEY_F12
*)
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
                        "FH PACS", (* name : ARRAY OF CHAR *)
                        "#100",        (* menu : ARRAY OF CHAR *)
                        FHIcon16, (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        70,20, (* xSize, ySize : COORDINATE *)
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
    SetTimer(Win, MouseTimer,  750);
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
(*  CountUpTimer := 3600*24;  Testing *)
  sleep := FALSE;
(*  InitInput,altdn,altup,adn,aup,mouseMove,mouseBack,mouseLclickDn,mouseLclickUp,InitMouse : WINUSER.INPUT; *)
(*  Set up the move movement and double click variables *)
  InitMouse.type := WINUSER.INPUT_MOUSE;
  InitMouse.mi.dy := 0;
  InitMouse.mi.mouseData := 0;
  InitMouse.mi.time := 0;
  InitMouse.mi.dwExtraInfo := 0;
  
  mouseMove := InitMouse;
  mouseMove.mi.dx := mousemoveamt;
  mouseMove.mi.dwFlags := WINUSER.MOUSEEVENTF_MOVE;
  
  mouseBack := InitMouse;
  mouseBack.mi.dx := -mousemoveamt;
  mouseBack.mi.dwFlags := WINUSER.MOUSEEVENTF_MOVE;
  
  mouseLclickDn := InitMouse;
  mouseLclickDn.mi.dx := 0;
  mouseLclickDn.mi.dwFlags := WINUSER.MOUSEEVENTF_LEFTDOWN;
  
  mouseLclickUp := InitMouse;
  mouseLclickUp.mi.dx := 0;
  mouseLclickUp.mi.dwFlags := WINUSER.MOUSEEVENTF_LEFTUP;
  
(*  set up the Alt-A keystrokes *)
  InitInput.type := WINUSER.INPUT_KEYBOARD;
  InitInput.ki.wScan := 0;
  InitInput.ki.time := 0;
  InitInput.ki.dwExtraInfo := 0;
  altdn := InitInput;
  altdn.ki.wVk := VK_MENU;  (* alt key virtual key code *)
  altdn.ki.dwFlags := WINUSER.KEYEVENTF_EXTENDEDKEY;
  altup := InitInput;
  altup.ki.wVk := VK_MENU;
  altup.ki.dwFlags := WINUSER.KEYEVENTF_EXTENDEDKEY BOR WINUSER.KEYEVENTF_KEYUP;
  adn := InitInput;
  adn.ki.wVk := VK_a;
  adn.ki.dwFlags := 0;
  aup := InitInput;
  aup.ki.wVk := VK_a;
  aup.ki.dwFlags := WINUSER.KEYEVENTF_KEYUP;

  InputArray[0] := mouseLclickDn;
  InputArray[1] := mouseLclickUp;
  InputArray[2] := mouseLclickDn;
  InputArray[3] := mouseLclickUp;
  InputArray[4] := mouseMove;
  InputArray[5] := mouseBack;
  InputArray[6] := altdn;
  InputArray[7] := adn;
  InputArray[8] := aup;
  InputArray[9] := altup;
  pinput := CAST(WINUSER.PINPUT,ADR(InputArray));
  
  FOR c := 0 TO 10 DO temparray[c] := 0; END;
  
  FUNC WinShell.DispatchMessages(Start, NIL);
END FHPacs2.
