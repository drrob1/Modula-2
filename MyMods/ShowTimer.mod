<*/NOWARN:F*>

  %IF IA32 %THEN
    <*/Resource:ShowTimer.RES*>
  %ELSE
  %END
(*
  %IF WIN32 %THEN
    <*/Resource:ShowTimer.RES*>
  %ELSE
  %END
*)

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
              And I will try adding Alt-A key sequence to the mix.
20 Jul 06 -- Added key option whereby timer continues but computer does not affect mouse.  I'll use <home>.
              And reverse operation will be <end>.
18 Apr 08 -- Decided to make <space> a toggle.  F1 exits instead.  Made procedures MakeSleep and
              WakeUp.  And made exiting logic use EmergencyScreenReset if PowerTimeOut is too low.
30 Dec 08 -- Found out the hard way that emergency screen reset does not work when I need it.  I made
              the emergency procedure reset both poweroff and lowpower timers, not just 1.  And added
              a menu choice for Emergency Screen Reset.  And made it reset to 1200 and not 7200, or
              going from 2 hrs to 20 min to match the needs of Centricity before it autologs out.
20 Aug 11 -- Created this Epic module using FHPacs as its code base.  I have to take out the change power
              API calls as these are not allowed on our dictating pc's.  The mouse function remains.
              And I'll change the icon file.
15 Jan 13 -- Created this ShowTimer file using Epic as a model.  I can delete the power options as they
              will never be needed here.  I want this to be a timer display to be used for clickloop.btm
              And it will exit after the down timer hits 0.
              I don't remember why I have 2 timers, maybe just to see if I could.  I'll simplify this now.

The initialization gets the TimeOutReset from the commandline, or default=1200 (20 min).
Most of the work of this pgm is done in the PAINT section.  The menu and key sections have essentially been removed.

 5 Mar 13 -- Changed hitting <ESC> so that it sets the ERRORLEVEL to be tested for by the calling batch file.
              If this does not work, I'll have to use a flag file of some kind to signal the calling batch file to end.
              And I changed the IF wigglemouse so it exits when =1 instead of =0 to fix an off by one error.
              Turns out that I cannot change its ERRORLEVEL, so now to use a flag file I'll call st.flg.
 4 Oct 13 -- Will add back the key section so that it exits on <enter>
 7 Oct 13 -- And I want it to exit on <space>
25 Oct 13 -- Will now use a tokenizing routine to parse the commandline, so comments can appear on the line after the
              desired timout setting.
20 Dec 13 -- Will experiment with setting display mode to either hidden or minimized.
22 Dec 13 -- Added MyNormalWindow, trying to see if I can get it to allow minimization.
24 Dec 13 -- Found some minor typo bugs, like in CreateWindow call, and when I killed the timers.  And fixed why
              my code won't minimize.  I think because there was a DisplayNormal in the size msg.
25 Dec 13 -- Will not activate foreground, and will set minimize by default.
11 Apr 14 -- Found bug in that MyNormalWindow does not have the min and max buttons.  Will go back to NormalWindow, now that
              I found the source of why the windows would not minimize before.
11 Oct 16 -- Added the check for time of day, and will effectively escape out at 8pm, or 20:00.
              And I'll change the default timer to 900 sec.
13 Oct 16 -- Added a flag that can be turned off called ProcessExitTime.  And can set exit time.
              And removed the displayhidden key.  I'm surprised it was still active.
14 Oct 16 -- Realized that each launching of this pgm goes for ~15 min, so the only time limit that would work is from
              command line.  DISPOSE tpv done from GETTKN when RetCode=1.  So I only need to explicitly do that when
              GETTKN does not return RetCode = 1, ie, when there is a new HourOfDayExit on the command line.
              First param is the timer value, and 2nd param is the hour value.  Cannot set only hour value.
26 Jan 17 -- Reinserting code to wiggle the mouse pointer, as this functionality is still needed.
               Need 2 down counters to do this, one for mouse pointer, and one to
               determine when to exit.
30 Jan 17 -- Took out the menu option to hide ShowTimer.  Killing a hidden task requires the TaskManager.
               I searched and learned that I cannot stop a screen saver by a simulated mouse event.
               But a simulated keybd event can stop it.  So I coded that, also based on a search.
--------------------------------------*)

MODULE ShowTimer;
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS, CAST, DWORD;
IMPORT WINUSER, WIN32, WINGDI, WINX;
IMPORT Strings, ExStrings, MemUtils;
FROM Strings IMPORT Append, Equal, Delete, Concat, Capitalize;
FROM ExStrings IMPORT AppendChar, EqualI;
FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, FontInfo, WinAttr, WinAttrSet,
    ClipboardFormat, DisplayModes, ScreenAttribute, CaretTypes,
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
IMPORT FileFunc;
(*
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
*)
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
FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY, DateTimeType, GetDateTime;
FROM LongStr IMPORT StrToReal,RealToFloat,RealToEng,RealToFixed,RealToStr,ConvResults;

FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SubStrCMPFNT,SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;
FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
IMPORT MiscM2;
(* FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteLongCard,WriteInt,ReadString,ReadCard,Error; *)
FROM FilePicker IMPORT FileNamePicker;
FROM MyFIO2 IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
  IOSTATE,FRESET,FPURGE,FOPEN,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
  FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;
FROM TOKENPTR IMPORT FSATYP,DELIMCH,DELIMSTATE,INI1TKN,TKNPTRTYP,
  INI3TKN,GETCHR,UNGETCHR,GETTKN,GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;

CONST
  szAppName = "ShowTimer";
  InputPromptLn1 = " <esc> Exit and write the flag file to stop the click loop.";
  InputPromptLn2 = " <Enter> or <delete> Exit without writing the flag file. ";
  InputPromptLn3 = " <Ins> to change the set exit time from the default.";
  InputPromptLn4 = " <home> or <PgUp> resume exit at the set time";
  InputPromptLn5 = " <end> or <PgDn> stop exit at the set time. ";

  LastMod = "30 Jan 17";
  clipfmt = CLIPBOARD_ASCII;
  ShowTimerIcon32 = '#100';
  ShowTimerIcon16 = '#200';
  MainMenu        = '#100';
  MouseTimer  = 1;
  ClockTimer = 2;
  OneTimer = 1;
  MouseMovementAmt = -20;
  TimeToWiggleMouse = 720; (* 720 sec is 12 min *)
  EmergencyScreenReset = 900;
  DefaultHourOfDayExit = 20;               (* This corresponds to 8 pm *)
  FlagFileName = "st.flg";
  VK_MENU              = 12h;    (* Alt Key *)
  VK_a                 = 41h;    (* a key *)
  VK_LMENU             = 0A4h;    (* L alt key *)
  VK_RMENU             = 0A5h;    (* R alt key *)
  MyNormalWindow        = WinAttrSet{WA_VISIBLE, WA_TITLE, WA_RESIZABLE, WA_SYSMENU,
                                     WA_HSCROLL, WA_VSCROLL };


TYPE
  pstrtyp     = POINTER TO STRTYP;  (* remember this is a 1-origin array *)

(*
  DateTimeType = RECORD
    M,D,Yr,Hr,Minutes,Seconds : CARDINAL;
    MonthStr,DayOfWeekStr : STRTYP;
  END;
*)

VAR
  hwnd        :  WIN32.HWND;
  msg         :  WINUSER.MSG;
  wc          :  WINUSER.WNDCLASSEX;
  cxBuffer: INTEGER;
  cyBuffer: INTEGER;
  cxScreen,cyScreen,wxClient,wyClient : COORDINATE;
  xCaret, yCaret, I  : INTEGER;
  ch2,ch3 :  CHAR;
  bool, inputprocessed, sleep, ProcessExitTime :  BOOLEAN;
  TimeOutReset, c,c1,c2,c3,SSTimeOut,PowerTimeOut, SleepAmt, len, HourOfDayExit,RetCode : CARDINAL;
  inputline,HPFileName,Xstr,str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  longstr     : ARRAY [0..5120] OF CHAR;
  tokenstate  : FSATYP;
  tpv         : TKNPTRTYP = NIL;
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
  ch1         :  CHAR='N';
  BeepParam,FlashTime,Color,CountUpTimer,RapidTimer,MinutesLeft,SecondsLeft,HoursLeft,ColorVal,
    ColorValRed,ColorValBlue,ColorValGreen,ScreenSaving,Days,Hours : CARDINAL;
  MinutesLeftStr,SecondsLeftStr,HoursLeftStr,WindowText,ScreenSavingStr : STRTYP;
  AlarmTime   : CARDINAL = 1;
  cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth,
  iVscrollPos, iVscrollMax, iHscrollPos, iHscrollMax                    : INTEGER;
  mousemoveamt    : INTEGER = -20;
  LongZero        : INTEGER64 = 0;
  boolp           : POINTER TO BOOLEAN;
  DatTim          : DateTimeType;
  WiggleMouse,ExitCountDown : INTEGER;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN : BUFTYP;
  INUNT1,OUTUN1                      : MYFILTYP;
  OpenFileName,InName,OutName        : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                      : ARRAY [1..8*1024] OF CHAR;


PROCEDURE MakeSleep;
BEGIN
  sleep := TRUE;
  WiggleMouse := SleepAmt +1; (* So timer does not display -1. *)
END MakeSleep;

PROCEDURE WakeUp;
BEGIN
  sleep := FALSE;
  WiggleMouse := TimeOutReset;
END WakeUp;


(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr    : Colors;
    x,y    : COORDINATE;
    i,int  : INTEGER;
    idx,c,k: CARDINAL;
    ans    : CHAR;
    bool   : BOOLEAN;
    dt     : DateTimeType;  (* Also have DatTim declared globally *)

BEGIN
    c := 0;
    CASE msg.msg OF
    TWM_CLOSE:
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        KillTimer(tw, OneTimer);
                                                           (*        KillTimer(tw, ClockTimer); *)
        DISPOSE(boolp);
        RETURN OkayToClose;

    | TWM_CREATE:
        xCaret := 10;
        yCaret := 0;
        inputline := '';
        bool := WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVETIMEOUT,c,ADR(SSTimeOut),c);
                                                           (*        SSTimeOut := TimeOutReset; *)
                                                                (* WiggleMouse := TimeOutReset; *)
        IF SSTimeOut <= 5 THEN (* Will be 0 if screen saver is to be off *)
          WiggleMouse := TimeOutReset;
        ELSE
          DEC(SSTimeOut,2);  (* Give leeway in case computer is busy or something like that *)
          WiggleMouse := SSTimeOut;
        END; (* if Screen Saver set to off *)
        ExitCountDown := TimeOutReset;
        sleep := FALSE;
        SetDisplayMode(tw, DisplayMinimized);
        NEW(boolp);

    | TWM_SIZE:
        GetClientSize(tw,cxScreen,cyScreen);
        cxClient := msg.width;
        cyClient := msg.height;
        SnapWindowToFont(tw,TRUE);
        MoveCaretTo(tw,xCaret,yCaret);
        MakeCaretVisible(tw);
        CaretOn(tw);

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

        WriteString(tw,InputPromptLn3,a);
        EraseToEOL(tw,a);
        WriteLn(tw);

        WriteString(tw,InputPromptLn4,a);
        EraseToEOL(tw,a);
        WriteLn(tw);

        WriteString(tw,InputPromptLn5,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        WriteLn(tw);

        IF ProcessExitTime THEN
            WriteString(tw," Will Exit at ",a);
        ELSE
            WriteString(tw," Will Not Exit at ",a);
        END; (* If ProcessExitTime *)
        IntToStr(HourOfDayExit,str5);
        WriteString(tw,str5,a);
        WriteString(tw,":00 Hours.",a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        WriteLn(tw);
        IntToStr(WiggleMouse, str0);
        WriteString(tw,' Wiggle Mouse or Sleep in : ',a);
        WriteString(tw,str0,a);
        WriteString(tw,' seconds.',a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        IntToStr(ExitCountDown,str6);
        WriteString(tw," CountDown to Exit in ",a);
        WriteString(tw,str6,a);
        WriteString(tw," seconds.",a);
        EraseToEOL(tw,a);
        WindowText  :=  str6;
        SetWindowTitle(tw, WindowText);
        WriteLn(tw);
        WriteLn(tw);
        CardToStr(ScreenSaving, ScreenSavingStr);
        WriteString(tw," Number of times that the screen saver was interrupted is ",a);
        WriteString(tw,ScreenSavingStr,a);
        WriteString(tw,".",a);
        WriteLn(tw);
        IF boolp^ THEN
          WriteString(tw," ScreenSaver is on.",a);
        ELSE
          WriteString(tw," ScreenSaver is off.",a);
        END;
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
         0   : (* Hide *)
(* To experiment with either DisplayHidden or DisplayMinimized.  I'll disable it as it is not what I want anyway
              SetDisplayMode(tw, DisplayHidden);
*)
       | 10  : (* Reset Clock *)
              CountUpTimer := 0;
              RepaintScreen(tw);
       | 15  : (* Reset both WiggleMouse and Clock *)
              WiggleMouse  := TimeOutReset; (* same value as SSTimeOut *)
              CountUpTimer := 0;
              RepaintScreen(tw);
       | 17  : (* Stop processing of Exit Time *)
              ProcessExitTime := FALSE;
              RepaintScreen(tw);
       | 18  : (* Resume processing of Exit Time *)
              ProcessExitTime := TRUE;
              RepaintScreen(tw);
       | 19  : (* Emergency Timer Reset *)
(*                Not needed as this rtn does not alter the Power Timeout settings
              FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,EmergencyScreenReset,NIL,c);
              FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,EmergencyScreenReset,NIL,c);
              PowerTimeOut := EmergencyScreenReset;
              TimeOutReset := PowerTimeOut;
              WiggleMouse := TimeOutReset;
*)
       (* I intentionally do not close or HALT here, so I can see the results at next repainting *)

       | 20  : (* exit *)
              CloseWindow(tw,CM_REQUEST);
      ELSE (* do nothing but not an error *)
      END; (* case menuId *)

    | TWM_TIMER:

      WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVERRUNNING,c,boolp,c);

      IF boolp^ THEN     (* if screensaver running, stop it *)
        INC(ScreenSaving);
        (* WINUSER.keybd_event(bVK : BYTE; bScan : BYTE; dwflags : DWORD; dwExtraInfo : DWORD); *)
        WINUSER.keybd_event(WINUSER.VK_SPACE,0B9h,0,0);
      END; (* if screensaver running *)

(* Don't need IF timerid here since I now only use 1 timer *)

      IF WiggleMouse <= 0 THEN  (* This counter is usually ~5 min, or ~300 sec, at the hospital. *)
        WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,CAST(DWORD, mousemoveamt),
                                                     CAST(DWORD, mousemoveamt), 0, 0);
        WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,CAST(DWORD, -mousemoveamt),
                                                     CAST(DWORD, -mousemoveamt), 0, 0);
        WiggleMouse := SSTimeOut;

      ELSE
        DEC(WiggleMouse);
      END; (* if wigglemouse <= 0 *)

      IF ExitCountDown <= 0 THEN
        CloseWindow(tw,CM_REQUEST);
      ELSE
        DEC(ExitCountDown);
      END; (* if time to exit *)

      INC(CountUpTimer);


      dt := GetDateTime(DatTim); (* Both of these are out params, just like in C-ish.  Don't know why that's done, but it works. *)
      (* More elaborate than needed, to see if this works *)
      IF ProcessExitTime AND (dt.Hr >= HourOfDayExit) AND (DatTim.Minutes >= 0) AND (CountUpTimer > 10) THEN
          ASSIGN2BUF(FlagFileName,OUTFNAM);                      (* I copied the code from the <ESC> section below   *)
          FOPEN(OUTUN1,OUTFNAM,WR);
          IF OUTUN1.FILE.status <> 0 THEN
            str1 := " Error in opening/creating file ";
            Strings.Append(OUTFNAM.CHARS,str1);
            Strings.Append('--',str1);
            CASE FileFunc.TranslateFileError(OUTUN1.FILE) OF
              FileFunc.FileErrFileNotFound : Strings.Append("File not found.",str1);
            | FileFunc.FileErrDiskFull : Strings.Append("Disk Full",str1);
            ELSE
              Strings.Append("Unknown error occured.",str1);
            END(*CASE*);
            Strings.Append("    Program Terminated.",str1);
            BasicDialogs.MessageBox(str1,MsgInfo);
            HALT;
          END(*IF*);

          FWRSTR(OUTUN1,"escape key was hit.");
          FWRLN(OUTUN1);
          FCLOSE(OUTUN1);

          CloseWindow(tw, CM_REQUEST);
        END; (* IF time is or after 8 pm by default *)

      RepaintScreen(tw);

    | TWM_KEY:

     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN
        CASE msg.k_ch OF
          CHR(8) :  (* backspace  *)  (* Used to WakeUp; but now is ignored *)
                    RepaintScreen(tw);
        | CHR(9) :  (* tab  *)  (* Used to MakeSleep, but now will exit to make keystroke control easier. *)
                    CloseWindow(tw, CM_REQUEST);
        | CHR(10):                      (* line feed       *)
        | CHR(13):                      (* carriage RETURN *)
                    CloseWindow(tw, CM_REQUEST);
        | CHR(27):                               (* escape *)
            ASSIGN2BUF(FlagFileName,OUTFNAM);
            FOPEN(OUTUN1,OUTFNAM,WR);
            IF OUTUN1.FILE.status <> 0 THEN
              str1 := ' Error in opening/creating file ';
              Strings.Append(OUTFNAM.CHARS,str1);
              Strings.Append('--',str1);
              CASE FileFunc.TranslateFileError(OUTUN1.FILE) OF
                FileFunc.FileErrFileNotFound : Strings.Append('File not found.',str1);
              | FileFunc.FileErrDiskFull : Strings.Append('Disk Full',str1);
              ELSE
                Strings.Append('Unknown error occured.',str1);
              END(*CASE*);
              Strings.Append('    Program Terminated.',str1);
              BasicDialogs.MessageBox(str1,MsgInfo);
              HALT;
            END(*IF*);

            FWRSTR(OUTUN1,"escape key was hit.");
            FWRLN(OUTUN1);
            FCLOSE(OUTUN1);

 (* FUNC *) CloseWindow(tw, CM_REQUEST);

        | CHR(32):                      (* <space> used to close window, now does nothing *)
                                        (* CloseWindow(tw, CM_REQUEST); *)
(*        | 'h','H','i','I' :          make hidden, just to test.  To kill a hidden process requires the taskmanager *)
(*            SetDisplayMode(tw, DisplayHidden);         To experiment with either DisplayHidden or DisplayMinimized *)

        ELSE (* CASE ch *)
        END (* case ch *);
      ELSIF msg.k_special = KEY_PAGEUP THEN
        ProcessExitTime := TRUE;
      ELSIF msg.k_special = KEY_PAGEDOWN THEN
        ProcessExitTime := FALSE;
      ELSIF msg.k_special = KEY_HOME THEN
        ProcessExitTime := TRUE;
      ELSIF msg.k_special = KEY_END THEN
        ProcessExitTime := FALSE;
      ELSIF msg.k_special = KEY_RIGHTARROW THEN
      ELSIF msg.k_special = KEY_LEFTARROW THEN
      ELSIF msg.k_special = KEY_UPARROW THEN
      ELSIF msg.k_special = KEY_DOWNARROW THEN
      ELSIF msg.k_special = KEY_INSERT THEN
        BasicDialogs.PromptCard(" Enter new exit hour (24-hr): ",0,20,TRUE,HourOfDayExit);
      ELSIF (msg.k_special = KEY_DELETE) THEN
        CloseWindow(tw, CM_REQUEST);
      ELSIF msg.k_special = KEY_F1 THEN
        SetDisplayMode(tw, DisplayMinimized);  (* To experiment with either DisplayHidden or DisplayMinimized *)
      ELSIF msg.k_special = KEY_F2 THEN
        SetDisplayMode(tw, DisplayMinimized);  (* To experiment with either DisplayHidden or DisplayMinimized *)
      ELSIF (msg.k_special = KEY_F11) OR (msg.k_special = KEY_F12) THEN
        SetDisplayMode(tw, DisplayMinimized);  (* To experiment with either DisplayHidden or DisplayMinimized *)

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
    Win := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "ShowTimer", (* name : ARRAY OF CHAR *)
                        MainMenu,        (* menu : ARRAY OF CHAR *)
                        ShowTimerIcon16, (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        80,30, (* xSize, ySize : COORDINATE *)
                        -1,-1, (* xBuffer, yBuffer : COORDINATE *)
                        FALSE,  (* gutter : BOOLEAN *)
                        DefaultFontInfo, (* font : FontInfo *)
                        ComposeAttribute(Black, White, NormalFont), (* background : ScreenAttribute *)
                        ToplevelWindow,  (* windowType : WindowTypes *)
                        WndProcTW,
                        NormalWindow,    (* attribs : WinAttrSet *)
                        NIL); (* createParam : ADDRESS *)
    IF Win = NIL THEN
        WinShell.TerminateDispatchMessages(127);
    END;

    SetTimer(Win, OneTimer,  1000);
    (* NEW(boolp); doesn't seem to be working here. *)

(*    SetAutoScroll(Win,TRUE); Will comment this line out to see if I can minimize *)
(*    SetTimer(Win, MouseTimer,  750); *)
(*    SetTimer(Win, ClockTimer, 1000); *)
END Start;


(*++++*****************************************************************)

BEGIN
  a := ComposeAttribute(Black, White, NormalFont);
  aBold := ComposeAttribute(Black, White, BoldFont);
  biggerFont := DefaultFontInfo;
  biggerFont.height := 140;
  SecondsLeftStr := "";
  WindowText :=  "";
  ScreenSaving := 0;
  CountUpTimer := 0;
  sleep := FALSE;
  SleepAmt := 5;
  ch1 := 'a';
  ch2 := 0c;
  ch3 := 0c;
  str1 := '';
  str2 := '';
  longstr := '';
  str4 := '';
  TimeOutReset := EmergencyScreenReset;
  inputprocessed := FALSE;
  ProcessExitTime := TRUE;
  HourOfDayExit := DefaultHourOfDayExit;               (* This corresponds to 8 pm *)
  GetCommandLine(inputbuf.CHARS);
  TRIM(inputbuf);
  IF inputbuf.COUNT > 0 THEN
                                         (*  inputprocessed := StrToCard(inputline, TimeOutReset); *)
    INI1TKN(tpv,inputbuf);
    GETTKN(tpv,TOKEN,tokenstate,I,RetCode);  (* RetCode=0 is nl return; RetCode=1 is no more tokens on line *)
    IF tokenstate = DGT THEN
      inputprocessed := TRUE;
      TimeOutReset := I;
    END; (* if 1st tokenstate for the TimeOutReset *)
    GETTKN(tpv,TOKEN,tokenstate,I,RetCode);  (* RetCode=0 is nl return; RetCode=1 is no more tokens on line *)
    IF (RetCode = 0) AND (tokenstate = DGT) THEN
      HourOfDayExit := I;
    END; (* if 2nd tokenstate for the HourOfDayExit *)
    IF tpv # NIL THEN  (* This will catch when GETTKN does not DISPOSE of the ptr, ie, when there is a 2nd token on the line *)
      DISPOSE(tpv);
      tpv := NIL;
    END;
  END; (* if there was a valid command tail entry *)
  IF (NOT inputprocessed) OR (TimeOutReset > 60*60*24) THEN
    TimeOutReset := EmergencyScreenReset;
  END; (* if commandline had garbage or number is too large *)
  IF FileFunc.FileExists(FlagFileName) THEN
    FUNC FileFunc.DeleteFile(FlagFileName);
  END;

  PowerTimeOut := TimeOutReset;

  FUNC WinShell.DispatchMessages(Start, NIL);
END ShowTimer.

(* PROCEDURE SetWindowTitle(tw : TextWindow; title : ARRAY OF CHAR);              *)
(* PROCEDURE SetTimer(tw : TextWindow; timerId : CARDINAL; interval : CARDINAL);  *)
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

(*
Conversions PROCEDURE StringToInt(str : ARRAY OF CHAR;
                      VAR INOUT pos : CARDINAL;
                      VAR OUT num : INTEGER;
                      VAR OUT done : BOOLEAN);
*)
(* Get an integer number from a string starting at position pos. *)
(* pos is left pointing at the first character that is not part of the *)
(* number. done signifies the success of the conversion *)
(* Skips any leading spaces. *)

(*  Conversions PROCEDURE StrToInt(buf : ARRAY OF CHAR; VAR OUT num : INTEGER) : BOOLEAN; *)
(* Convert an integer number from a string  *)
(* Skips any leading spaces. *)
(*
PROCEDURE StringToCard(str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT num : CARDINAL;
                       VAR OUT done : BOOLEAN);
*)
(* Get a cardinal number from a string starting at position pos. *)
(* pos is left pointing at the first character that is not part of the *)
(* number *)
(* done signifies the success of the conversion *)
(* Skips any leading spaces. *)

(*  PROCEDURE StrToCard(buf : ARRAY OF CHAR; VAR OUT num : CARDINAL) : BOOLEAN; *)
(* Convert a string in buf to a cardinal *)
(* Skips any leading spaces. *)
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

(*
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx
  https://www.codeproject.com/articles/7305/keyboard-events-simulation-using-keybd-event-funct
  VK_SHIFT 0x10    left shift scan code AA, right shift scan code is B6
  VK_CONTROL 0x11  left control scan code 9D, right control scan code is 
  VK_MENU 0x12 but I don't have the scan code


*)