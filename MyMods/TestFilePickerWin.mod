<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:TFPW.RES*>
%ELSE
%END

MODULE TestFilePickerWin;
(*
  REVISION HISTORY
  ----------------
  28 Nov 13 -- First use of FilePickerBase modules in a Windows GUI pgm.
*)


  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  IMPORT MiscM2,ASCII;
  IMPORT FileFunc;

  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts, (* drive path name extension, *) FileTypes, DeviceTypes,
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
    FindInOSPathList, ExpandFileSpec, FindFirst, (* FindNext, *) FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS;
(* IMPORT WINUSER, WIN32, WINGDI, WINX; *)
IMPORT Strings, MemUtils;
FROM Strings IMPORT FindNext, Append, Equal, Delete, Concat, Capitalize;
FROM ExStrings IMPORT
    AppendChar, EqualI;
FROM FormatString IMPORT FormatString;
FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, FontInfo, WinAttr, ClipboardFormat,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow, NormalChildWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo, WindowTypes,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, SpecialKeys,
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
IMPORT Terminal, BasicDialogs, DlgShell, WinShell;
FROM BasicDialogs IMPORT MessageBox,PromptString,MessageTypes;
IMPORT WholeStr, LongStr, LongConv;

  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,SubStrCMPFNT,stricmpfnt,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,
    RMVCHR,APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TIMLIB IMPORT GETMDY,JULIAN,TIME2MDY,MDY2STR;
  IMPORT SysClock;
  FROM SysClock IMPORT DateTime,GetClock;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FOPEN,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM,ChopIntoLines;
  FROM Environment IMPORT GetCommandLine;
  IMPORT MYRAND;
  FROM MYRAND IMPORT RANDCARD;
  FROM FilePickerBase IMPORT CountOfEntries, SetFilenamePattern, GetNextFilename, GetPrevFilename;
(* Errors are when no valid filenames were found with the given namestring pattern.  CountOfEntries will be 0.
   PROCEDURE GetNextFilename(VAR OUT ns,DirectoryEntry: NameString);
   PROCEDURE GetPrevFilename(VAR OUT ns,DirectoryEntry: NameString);
*)


CONST
  szAppName = "TestFilePickerWin";
  InputPrompt = '<enter> or <space> to select : ';
  LastMod = '28 Nov 13';
  CornerIcon32 = '#100';
  CornerIcon16 = '#200';
  Menu = '#100';
  MenuSep = '|';
  MaxNumOfTracks = 256;

VAR
  C,K,c,len,strlen,max                                        : CARDINAL;
  c32,d32,e32,f32,g32                                         : CARDINAL32;
  CH,ch                                                       : CHAR;
  haveValidTkn,bool,EOFFLG,OK,ok,done                         : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN : BUFTYP;
  dt                                            : DateTime;
  I,J                                           : INTEGER;
  ns, DirEntry, inputline                       : NameString;
  OutFileNameBuf                                : BUFTYP;
  tabchar                                       : Strings.String1 = ASCII.ht;
  cxChar,cyChar,cxClient,cyClient,cxBuffer,cyBuffer,xCaret,yCaret : INTEGER;
  cxScreen,cyScreen : COORDINATE;
  LastModLen  : CARDINAL;
  a           : ScreenAttribute;
  Win         : TextWindow;
(*************************************************************************************************)


(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr         : Colors;
    x,y         : COORDINATE;
    i,int       : INTEGER;
    cmdline     : ARRAY [0..255] OF CHAR;
    cmdbuf,tkn  : BUFTYP;
    ctr,c5      : CARDINAL;
    filter,s    : STRTYP;

BEGIN
    CASE msg.msg OF
    TWM_CLOSE:  (* Turns out that this winmsg is being executed twice before the pgm closes.  I have no idea why *)
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        WriteLn(tw);
        WriteLn(tw);
        RETURN OkayToClose;
    | TWM_CREATE:
        FUNC SetWindowIcon(tw, CornerIcon16);

        xCaret := 0;
        yCaret := 0;
        inputline := '';


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
    | TWM_PAINT:
        GetCommandLine(ns);
        len := LENGTH(ns);
        IF len = 0 THEN
          ok := PromptString(' Enter filename pattern: ',ns);
          IF NOT ok THEN CloseWindow(tw,CM_REQUEST); END;
        END;
        SetFilenamePattern(ns);
        max := CountOfEntries;
        IF max > 15 THEN max := 15 END;
          IF max > 1 THEN
            FOR ctr := 1 TO max DO
            GetNextFilename(ns,DirEntry);
            WriteString(tw,DirEntry,a);
            WriteLn(tw);
          END; (* for loop to get and display directory entries *)

          FOR ctr := 1 TO max DO
            GetPrevFilename(ns,DirEntry);
          END; (* for loop to get back to first directory entry *)
        END; (* for max not 0 or 1 *)
        MoveCaretTo(tw,0,22);
        EraseToEOL(tw,a);
        WriteStringAt(tw,0,22,DirEntry,a);
        WriteLn(tw);
        WriteLn(tw);
        WriteString(tw,' <enter> or <space> to select',a);

        INC(c32);
        FUNC FormatString(' Number of Paint msgs is: %c.',s,c32);
        WriteStringAt(tw,0,cyClient-2,s,a);
        WriteLn(tw);
        WriteStringAt(tw,0,cyClient-1,LastMod,a);

    | TWM_MENU:
(*
  a menu item has been selected menuId = the menu resource id number for the menu item
  TWM_MENU:
       msg.menuId      : INTEGER;
       msg.accel       : BOOLEAN;
*)
         CASE msg.menuId OF
           10 : (* Enter new file name pattern *)
               ok := PromptString(' Enter filename pattern: ',ns);
               IF NOT ok THEN CloseWindow(tw,CM_REQUEST); END;
               SetFilenamePattern(ns);
               max := CountOfEntries;
               EraseScreen(tw,a);
               IF max > 15 THEN max := 15 END;
                 IF max > 1 THEN
                   FOR ctr := 1 TO max DO
                     GetNextFilename(ns,DirEntry);
                     WriteString(tw,DirEntry,a);
                     WriteLn(tw);
                   END; (* for loop to get and display directory entries *)

                   FOR ctr := 1 TO max DO
                     GetPrevFilename(ns,DirEntry);
                   END; (* for loop to get back to first directory entry *)
                 END; (* for max not 0 or 1 *)
                 MoveCaretTo(tw,0,22);
                 EraseToEOL(tw,a);
                 WriteStringAt(tw,0,22,DirEntry,a);
                 WriteLn(tw);
                 WriteLn(tw);
                 WriteString(tw,' <enter> or <space> to select',a);
         | 20 : (* exit *)
               CloseWindow(tw,CM_REQUEST);
         | 30 : (* about *)
               BasicDialogs.MessageTitle := 'About';
               Strings.Assign('Last Modified and Compiled ',s);
               Strings.Append(LastMod,s);
               BasicDialogs.MessageBox(s, BasicDialogs.MsgInfo);
      ELSE (* do nothing but not an error *)
      END; (* case menuId *)


    |
    TWM_KEY:
     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN
        CASE msg.k_ch OF
          CHR(8) :                                     (* backspace       *)
            GetPrevFilename(ns,DirEntry);
            MoveCaretTo(tw,0,22);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,22,DirEntry,a);

        | CHR(9) :                                     (* tab             *)
            GetNextFilename(ns,DirEntry);
            MoveCaretTo(tw,0,22);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,22,DirEntry,a);

        | CHR(10):                                     (* line feed       *)

        | CHR(13):                                     (* carriage RETURN *)
            MoveCaretTo(tw,0,25);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,25,' Picked Filename is ',a);
            WriteString(tw,ns,a);
            MessageBox(ns,MsgInfo);
(*            CloseWindow(tw,CM_REQUEST); *)

        | CHR(27):                                     (* escape *)
            FUNC CloseWindow(tw, CM_REQUEST);
        | CHR(32):                                     (* space *)
            MoveCaretTo(tw,0,25);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,25,' Picked Filename is ',a);
            WriteString(tw,ns,a);
            MessageBox(ns,MsgInfo);
(*            CloseWindow(tw,CM_REQUEST); *)

        | 'A','a':
            BasicDialogs.MessageTitle := 'About';
            Strings.Assign('Last Modified and Compiled ',s);
            Strings.Append(LastMod,s);
            BasicDialogs.MessageBox(s, BasicDialogs.MsgInfo);

        ELSE (* CASE ch *)
        END (* case ch *);
      ELSIF msg.k_special = KEY_PAGEUP THEN
            GetPrevFilename(ns,DirEntry);
            MoveCaretTo(tw,0,22);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,22,DirEntry,a);

      ELSIF msg.k_special = KEY_PAGEDOWN THEN
            GetNextFilename(ns,DirEntry);
            MoveCaretTo(tw,0,22);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,22,DirEntry,a);

      ELSIF msg.k_special = KEY_HOME THEN
      ELSIF msg.k_special = KEY_END THEN
      ELSIF msg.k_special = KEY_RIGHTARROW THEN
            GetNextFilename(ns,DirEntry);
            MoveCaretTo(tw,0,22);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,22,DirEntry,a);

      ELSIF msg.k_special = KEY_LEFTARROW THEN
            GetPrevFilename(ns,DirEntry);
            MoveCaretTo(tw,0,22);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,22,DirEntry,a);

      ELSIF msg.k_special = KEY_UPARROW THEN
            GetPrevFilename(ns,DirEntry);
            MoveCaretTo(tw,0,22);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,22,DirEntry,a);

      ELSIF msg.k_special = KEY_DOWNARROW THEN
            GetNextFilename(ns,DirEntry);
            MoveCaretTo(tw,0,22);
            EraseToEOL(tw,a);
            WriteStringAt(tw,0,22,DirEntry,a);

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
    Win := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "VLC Shuffle", (* name : ARRAY OF CHAR *)
                        Menu,        (* menu : ARRAY OF CHAR *)
                        CornerIcon16,        (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        110,30, (* xSize, ySize : COORDINATE *)
                        250,100, (* xBuffer, yBuffer : COORDINATE *)
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
  LastModLen := LENGTH(LastMod);
  a := ComposeAttribute(Black, White, NormalFont);
  c32 := 0;
  haveValidTkn := FALSE;
  FUNC WinShell.DispatchMessages(Start, NIL);

END TestFilePickerWin.
