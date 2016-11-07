<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:WordList.RES*>
%ELSE
%END
MODULE MakeWordList;
(*
  REVISION HISTORY
  ----------------
  28 Sep 09 -- First version to help test an Ada sorting pgm.

*)


  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  IMPORT MiscM2,ASCII,MiscStdInOut;
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
(*
  FROM RealIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM TermFile IMPORT Open, IsTermFile, Close;
*)
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS;
(* IMPORT WINUSER, WIN32, WINGDI, WINX; *)
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
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings, MemUtils;
IMPORT WholeStr, LongStr, LongConv;

  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,GETTKNEOL,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE,GetHtmlCodeString;
  FROM TIMLIB IMPORT GETMDY, JULIAN;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FOPEN,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM,ChopIntoLines;
  FROM Environment IMPORT GetCommandLine;

CONST
  szAppName = "MakeWordList";
  InputPrompt = 'Enter cmd or HELP : ';
  LastMod = '28 Sep 09';
  WordListIcon = '#100';
  MenuSep = '|';
  MinWordLen = 4;

TYPE

VAR
  C,K,c,RETCOD,m,d,y,chknum                    : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,bool,EOFFLG,OK,ok,ZeroFlag,BankTranListEnd : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  tpv1,tpv2,tpv3                               : TKNPTRTYP;
  qfxtoken,GblOrg,GblAcctID,ledgerBalAmt,availBalAmt,BalAmtDateAsOf,outfilename,comment,acntid : STRTYP;
  I,J                                          : INTEGER;
  INFILE,OUTFILE                               : MYFILTYP;
  infile          : File;
  inputline,buf,infilename         : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;
  juldate1,juldate2,juldate3                   : LONGINT;
  outfilelabel : STRTYP;
  cxChar  : INTEGER;
  cyChar  : INTEGER;
  cxClient: INTEGER;
  cyClient: INTEGER;
  cxBuffer: INTEGER;
  cyBuffer: INTEGER;
  cxScreen,cyScreen : COORDINATE;
  xCaret  : INTEGER;
  yCaret  : INTEGER;
  LastModLen : CARDINAL;
  a           : ScreenAttribute;
  Win         : TextWindow;


PROCEDURE ProcessFile;

BEGIN
  LOOP
    FRDTXLN(INFILE,INBUF,BUFSIZ,EOFFLG);
    IF EOFFLG THEN EXIT END(*IF*);
    TRIM(INBUF);
    INI1TKN(tpv1,INBUF);
    LOOP
      GETTKN(tpv1,TOKEN,TKNSTATE,I,RETCOD);
      IF RETCOD > 0 THEN
(*
        WriteString("GETTKN's RETCOD is ");
        WriteCard(RETCOD);
        WriteLn;
*)
        EXIT;
      END(*IF retcod *);
      IF (TOKEN.COUNT >= MinWordLen) AND (TKNSTATE=ALLELSE) THEN
        FWRTXLN(OUTFILE,TOKEN);

      END(*IF Token.Count *);
    END(*LOOP to read and write tokens *);
  END(*LOOP to read and write lines *);
END ProcessFile;



(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr         : Colors;
    x,y         : COORDINATE;
    i,int       : INTEGER;
    cmdline     : ARRAY [0..255] OF CHAR;
    cmdbuf,tkn  : BUFTYP;
    tknstate    : FSATYP;
    retcod,c5   : CARDINAL;
    filter,s    : STRTYP;

BEGIN
    CASE msg.msg OF
    TWM_CLOSE:  (* Turns out that this winmsg is being executed twice before the pgm closes.  I have no idea why *)
(*
        BasicDialogs.MessageBox(outfilename,MsgInfo);

        BasicDialogs.MessageBox(outfilename,MsgInfo);

        FileFunc.CopyFile(OUTFNAM.CHARS,outfilename);
*)
        BasicDialogs.MessageBox(OUTFNAM.CHARS,MsgInfo);
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        RETURN OkayToClose;
    | TWM_CREATE:
        FUNC SetWindowIcon(tw, WordListIcon);

        xCaret := 0;
        yCaret := 0;
        inputline := '';
        juldate1 := 0;
        juldate2 := 0;
        juldate3 := 0;
        chknum := 0;

        INFNAM.CHARS := '';
        OUTFNAM.CHARS := '';

        filter := '';
        Strings.Append('txt Files',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('*.txt',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('qfx Files',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('*.qfx',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('qif Files',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('*.qif',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('comma Text',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('*.csv',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('All',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('*',filter);
        Strings.Append(MenuSep,filter);



(*
  PROCEDURE BasicDialogs.PromptOpenFile(VAR INOUT name : ARRAY OF CHAR;
                                            filters : ARRAY OF CHAR;
                                            VAR INOUT defFilter : CARDINAL;
                                            defDir : ARRAY OF CHAR;
                                            defExt : ARRAY OF CHAR;
                                            title : ARRAY OF CHAR;
                                            createable : BOOLEAN) : BOOLEAN;
 Opens an operating system common dialog for opening  a file
   filters specifies a list of file extension filters that are
   separated by semicolons.
   The format for filters is as follows.
   defDir = the default directory to start the dialog in
   an empty string "" means use the current directory.
   defExt = the default file extension to use if the user does not
   provide an extension. "" means no default extension.
   the extension should *not* have a leading '.' character.
   title = the caption text of the dialog. title can be empty "".
   in this case the default operating system title is used.
   If createable = TRUE then the file need not already exist, otherwise
   the file must exist for the dialog to return successful.
   RETURNs TRUE is successful and name will contain the file specification
   for the file the user has given.
*)
        c5 := 1;
        DlgShell.ConvertToNulls(MenuSep,filter);
        bool := BasicDialogs.PromptOpenFile(infilename,filter,c5,'','','Open the text file for word list',FALSE);
        BasicDialogs.MessageBox(infilename,MsgInfo);
        IF NOT bool THEN
          WriteString(tw,'Could not find file.  Does it exist?',a);
          HALT;
        END;

        IF NOT FileFunc.FileExists(infilename) THEN
          MiscM2.Error(' Could not find input file.  Does it exist?');
          HALT;
        END(*if*);
        INFNAM.CHARS := infilename;
        TRIM(INFNAM);

(*  BasicDialogs.MessageBox(INFNAM.CHARS,MsgInfo); *)
        FOPEN(INFILE,INFNAM,RD);

(* open output file *)
        outfilename := 'wordlist';
        Strings.Concat(outfilename,'.txt',OUTFNAM.CHARS);
        FUNC FileFunc.DeleteFile(OUTFNAM.CHARS);
        TRIM(OUTFNAM);
        FOPEN(OUTFILE,OUTFNAM,WR);

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
        ProcessFile;
        FCLOSE(INFILE);
        FCLOSE(OUTFILE);
        WriteLn(tw);
        WriteString(tw,OUTFNAM.CHARS,a);
        WriteString(tw,' file now closed.',a);
        WriteLn(tw);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        INC(c32);
        FUNC FormatString(' Number of Paint msgs is: %c.',buf,c32);
        WriteString(tw,buf,a);
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
         20  : (* exit *)
              CloseWindow(tw,CM_REQUEST);
      ELSE (* do nothing but not an error *)
      END; (* case menuId *)


    |
    TWM_KEY:
     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN
        CASE msg.k_ch OF
          CHR(8) :                                     (* backspace       *)
          FUNC CloseWindow(tw, CM_REQUEST);
        | CHR(9) :                                     (* tab             *)
          FUNC CloseWindow(tw, CM_REQUEST);

        | CHR(10):                                     (* line feed       *)

        | CHR(13):                                     (* carriage RETURN *)
          FUNC CloseWindow(tw, CM_REQUEST);

        | CHR(27):                                     (* escape *)
          FUNC CloseWindow(tw, CM_REQUEST);
        | CHR(32):                                     (* space *)
          FUNC CloseWindow(tw, CM_REQUEST);
        | 'A','a': (* About *)
             BasicDialogs.MessageTitle := 'About';
             Strings.Assign('Last Modified and Compiled ',s);
             Strings.Append(LastMod,s);
             BasicDialogs.MessageBox(s, BasicDialogs.MsgInfo);
        ELSE (* CASE ch *)
          FUNC CloseWindow(tw, CM_REQUEST);
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
    Win := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "Make a Word List File", (* name : ARRAY OF CHAR *)
                        "#100",        (* menu : ARRAY OF CHAR *)
                        "WordListIcon",        (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        110,20, (* xSize, ySize : COORDINATE *)
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
  infilename := '';
  FUNC WinShell.DispatchMessages(Start, NIL);

END MakeWordList.
