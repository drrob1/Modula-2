<*/NOWARN:F*>

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
30 Dec 08 -- Found out the hard way that emergency screen resent does not work when I need it.  I made
              the emergency procedure reset both poweroff and lowpower timers, not just 1.  And added
              a menu choice for Emergency Screen Reset.  And made it reset to 1200 and not 7200, or
              going from 2 hrs to 20 min to match the needs of Centricity before it autologs out.
 9 Dec 16 -- Will start fixing 2 problems.  (1) if screen timeout is 0, this pgm crashes, and
              (2) will have some way to input a time at which to automatically go to sleep or become groggy.
              And I took out code that was commented out long ago.
17 Dec 16 -- Added Will Sleep at time.
30 Jan 17 -- Fixed sequence to insert a Ctrl-a keybd_event, I found when searching about stopping an active screen saver.
               https://www.codeproject.com/articles/7305/keyboard-events-simulation-using-keybd-event-funct
31 Jan 17 -- Had to remove the keybd_event code because it choked the keyboard buffer.  I couldn't enter characters into PACS.               
--------------------------------------*)
<*/Resource:FHPacs.RES*>
MODULE FHPacs;
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

CONST
  LastMod = "31 Jan 17";
  szAppName = "FHPacs";
  InputPromptLn1 = " <tab> Sleep in 5; <bsp> Wake up; <del> or <F1> emerg screen timer reset and HALT. ";
  InputPromptLn2 = " <home> stop mouse movement; <end> resume mouse movements. ";
  InputPromptLn3 = " <sp> sleep toggle; <F9> Enter new hour of day to become groggy as 24-hr 2 digit number.";
  clipfmt = CLIPBOARD_ASCII;
  FHIcon32 = "#100";
  FHIcon16 = "#200";
  MouseTimer  = 1;
  ClockTimer = 2;
  EmergencyScreenReset = 1200;
  DefaultHourOfDayGroggy = 18;   (* This corresponds to 6 pm *)
  VK_MENU              = 12h;    (* Alt Key *)
  VK_a                 = 41h;    (* a key *)
  VK_LMENU             = 0A4h;    (* L alt key *)
  VK_RMENU             = 0A5h;    (* R alt key *)

TYPE
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
  bool, inputprocessed, sleep, HourOfDayGroggyOK :  BOOLEAN;
  TimeOutReset,c,c1,c2,c3,SSTimeOut,PowerTimeOut,HourOfDayGroggy,SleepAmt : CARDINAL;
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
    ColorValRed,ColorValBlue,ColorValGreen,ScreenSaving,Days,Hours : CARDINAL;
  MinutesLeftStr,SecondsLeftStr,HoursLeftStr,WindowText,ScreenSavingStr : STRTYP;
  AlarmTime : CARDINAL = 1;
  cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth,
  iVscrollPos, iVscrollMax, iHscrollPos, iHscrollMax                    : INTEGER;
  mousemoveamt    : INTEGER = -20;
  LongZero        : INTEGER64 = 0;
  boolp           : POINTER TO BOOLEAN;
  WiggleMouse     : INTEGER;

PROCEDURE maxcard(card1,card2 : CARDINAL) : CARDINAL;
BEGIN  (* No longer needed, but I left it here anyway *)
  IF card1 > card2 THEN
    RETURN(card1)
  ELSE
    RETURN(card2)
  END;
END maxcard;


PROCEDURE MakeSleep;
BEGIN
  sleep := TRUE;
  WiggleMouse := SleepAmt +1; (* So timer does not display -1. *)
  bool := WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,SleepAmt,NIL,c);
  bool := WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,SleepAmt,NIL,c);
END MakeSleep;

PROCEDURE WakeUp;
BEGIN
  sleep := FALSE;
  WiggleMouse := TimeOutReset;
  bool := WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,PowerTimeOut,NIL,c);
  bool := WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,PowerTimeOut,NIL,c);
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
    dt,DatTim : DateTimeType;
    GroggyTimeStr : STR10TYP;

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
        IF PowerTimeOut < 10 THEN
                k := EmergencyScreenReset
        ELSE
                k := PowerTimeOut
        END;
        FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,k,NIL,c);
        FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,k,NIL,c);
(*
                                                              FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,EmergencyScreenReset,NIL,c);
                                                              FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,EmergencyScreenReset,NIL,c);
                                                                   bool := WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,PowerTimeOut,NIL,c);
*)
        RETURN OkayToClose;

    | TWM_CREATE:
        FUNC SetWindowIcon(tw, FHIcon16);
        xCaret := 10;
        yCaret := 0;
        inputline := '';
        SetScrollDisableWhenNone(tw,TRUE,TRUE);
        FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVETIMEOUT,c,ADR(SSTimeOut),c);
        FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_GETPOWEROFFTIMEOUT,c,ADR(PowerTimeOut),c);
        IF PowerTimeOut > 0 THEN
          TimeOutReset := PowerTimeOut - 1;
        ELSE
          TimeOutReset := EmergencyScreenReset;  (* 1200 *)
        END (* IF *);
        WiggleMouse := TimeOutReset;
        sleep := FALSE;
        HourOfDayGroggy := DefaultHourOfDayGroggy;

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
        WriteString(tw,InputPromptLn3,a);
        EraseToEOL(tw,a);
        WriteLn(tw);
        IF sleep THEN
          WriteString(tw," Asleep.",a);
        ELSE
          WriteString(tw," Awake.  Will sleep at ",a);
          CardToStr(HourOfDayGroggy,GroggyTimeStr);
          WriteString(tw,GroggyTimeStr,a);
          WriteString(tw,":00 hours.",a);
        END;
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

    | TWM_MENU:
(*
  a menu item has been selected menuId = the menu resource id number for the menu item
  TWM_MENU:
       msg.menuId      : INTEGER;
       msg.accel       : BOOLEAN;
*)
         CASE msg.menuId OF
         0   : MakeSleep;

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

       | 17  : (* Groggy means stop mouse movement and double click function without changing the power settings *)
              sleep := TRUE;  (* make groggy *)
              RepaintScreen(tw);

       | 18  : WakeUp;
              RepaintScreen(tw);

       | 19  : (* Emergency Timer Reset *)
              FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,EmergencyScreenReset,NIL,c);
              FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,EmergencyScreenReset,NIL,c);
              PowerTimeOut := EmergencyScreenReset;
              TimeOutReset := PowerTimeOut;
              WiggleMouse := TimeOutReset;
              (* I intentionally do not close or HALT here, so I can see the results *)

       | 20  : (* exit *)
              CloseWindow(tw,CM_REQUEST);

       | 30  : (* Change hour to make groggy *)
               HourOfDayGroggyOK := BasicDialogs.PromptCard(" New Hour of Day to Exit:",16,23,FALSE,HourOfDayGroggy);
               IF NOT HourOfDayGroggyOK THEN
                 HourOfDayGroggy := DefaultHourOfDayGroggy;
               END (* if *);

      ELSE (* do nothing but not an error *)
      END; (* case menuId *)

    | TWM_TIMER:
(*
DWORD is typed as a CARDINAL here, so you cannot assign negative numbers easily.
The solution is to use type casting, like: mouse_event (MOUSEEVENTF_MOVE, CAST(DWORD,dx), CAST(DWORD,dy), 0, 0);
*)
      dt := GetDateTime(DatTim); (* Both of these are out params, just like in C-ish.  Don't know why that's done, but it works. *)
      IF (dt.Hr = HourOfDayGroggy) AND (DatTim.Minutes = 0) THEN
        sleep := TRUE;  (* will stop mouse movement and double click function without changing the power settings *)
      END (* if *);

      IF msg.timerId = MouseTimer THEN
        IF sleep THEN  (* Don't wiggle mouse *)
        ELSIF WiggleMouse <= 0 THEN
          WiggleMouse := TimeOutReset;
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);  (* Do double click, I hope.*)
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
          (* FOR k := 1 TO 1000 DO END; *)
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE,CAST(DWORD, mousemoveamt),
                              CAST(DWORD, mousemoveamt), 0, 0);
          WINUSER.mouse_event(WINUSER.MOUSEEVENTF_MOVE, CAST(DWORD, -mousemoveamt),
                              CAST(DWORD, -mousemoveamt), 0, 0);
(*  Turned out that these lines of code choked the keyboard buffer.  I couldn't enter characters into PACS.                              
          WINUSER.keybd_event(WINUSER.VK_CONTROL,9dh,0,0);  (* ctrl key press *)
          WINUSER.keybd_event(VK_a,9eh,0,0);        (* a key press *)
          WINUSER.keybd_event(VK_a,9eh,WINUSER.KEYEVENTF_KEYUP,0);  (* a key release *)
          WINUSER.keybd_event(WINUSER.VK_CONTROL,9dh,WINUSER.KEYEVENTF_EXTENDEDKEY BOR WINUSER.KEYEVENTF_KEYUP,0);  (* ctrl key release *)
*)          
        END (* if *);
      ELSIF msg.timerId = ClockTimer THEN
        IF WiggleMouse > 0 THEN DEC(WiggleMouse); END;
        INC(CountUpTimer);
        RepaintScreen(tw);
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
          CHR(8) :  (* backspace  *)
                    WakeUp;
                    RepaintScreen(tw);
        | CHR(9) :  (* tab  *)
                    MakeSleep;
                    RepaintScreen(tw);
        | CHR(10):                      (* line feed       *)
        | CHR(13):                      (* carriage RETURN *)
                    CloseWindow(tw,CM_REQUEST);
        | CHR(27):                               (* escape *)
               (* FUNC *) CloseWindow(tw, CM_REQUEST);

        | ' ' :  (* space  *)
                 IF sleep THEN
                   WakeUp;
                 ELSE
                   MakeSleep;
                 END(*if*);
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
      ELSIF (msg.k_special = KEY_DELETE) OR (msg.k_special = KEY_F1) THEN
        FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETLOWPOWERTIMEOUT,EmergencyScreenReset,NIL,c);
        FUNC WINUSER.SystemParametersInfo(WINUSER.SPI_SETPOWEROFFTIMEOUT,EmergencyScreenReset,NIL,c);
        HALT;
      ELSIF (msg.k_special = KEY_F8) OR (msg.k_special = KEY_F9) THEN
        HourOfDayGroggyOK := BasicDialogs.PromptCard(" New Hour of Day to Exit:",16,23,FALSE,HourOfDayGroggy);
        IF NOT HourOfDayGroggyOK THEN
          HourOfDayGroggy := DefaultHourOfDayGroggy;
        END (* if *);
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

(*
                              PROCEDURE PromptCard(prompt : ARRAY OF CHAR;min, max : CARDINAL;allowZero : BOOLEAN;VAR INOUT response : CARDINAL) : BOOLEAN;
                              open a modal dialog with a single edit field and a text label for the edit field contained in prompt.
                              returns TRUE if the user enters a valid number and presses OK otherwise FALSE
                              the number entered is contained in response
                              min and max provide range checking for the number entered.
                              if allowZero = TRUE then the number zero will be allowed even if outside
                              the allowed number range. This can be used to allow the user to enter a "null" value.
*)

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
                        90,20, (* xSize=Width, ySize=Height : COORDINATE *)
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

(*                                                                                    PROCEDURE SetWindowTitle(tw : TextWindow; title : ARRAY OF CHAR);  *)
(*                                                                        PROCEDURE SetTimer(tw : TextWindow; timerId : CARDINAL; interval : CARDINAL);  *)
(*
                                                                                              create/reset a timer associated with the specified window
                                                                                              timerId = a unique number to identify the timer.
                                                                                   interval = the amount of time in milliseconds between WSM_TIMER messages
                                                                                                this interval is only an approximate time
                                                                                         calling SetTimer with the same timerId as a previous call but with
                                                                                         a different interval has the effect of resetting the interval from
                                                                                           the previous value to the new value
*)
(*                                                     PROCEDURE KillTimer(tw : TextWindow; timerId : CARDINAL);  InputPromptLen := LENGTH(InputPrompt); *)
  a := ComposeAttribute(Black, White, NormalFont);
  aBold := ComposeAttribute(Black, White, BoldFont);
  biggerFont := DefaultFontInfo;
  biggerFont.height := 140;
  SecondsLeftStr := "";
  WindowText :=  "";
  ScreenSaving := 0;
  CountUpTimer := 0;
(*  CountUpTimer := 3600*24;  Testing *)
  sleep := FALSE;
  SleepAmt := 5;
  ch1 := 'a';
  ch2 := 0c;
  ch3 := 0c;
  str1 := "";
  str2 := "";
  longstr := "";
  str4 := "";

  FUNC WinShell.DispatchMessages(Start, NIL);
END FHPacs.
