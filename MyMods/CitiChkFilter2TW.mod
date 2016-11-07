MODULE CitiChkFilter2TW;
(*
  REVISION HISTORY
  ----------------
  13 Mar 04 -- It does not seem to be always creating the output file.  
               It now copies the d/l file instead of renaming it.  And
               it echoes its output to the terminal.
  14 Mar 04 -- Made it a Text Window module
  21 Mar 04 -- Changed exit keys to remove <cr> and add <space>, <bs>, <tab>
  15 Apr 04 -- Decided to include <cr> again, as I fixed the pblm in Excel macros.
  31 Jul 04 -- Needed to change logic because Citibank now d/l null fields that I need.
*)
  IMPORT MiscM2;
  FROM MiscM2 IMPORT CLS, PressAnyKey, Error;
(*  
  FROM MiscM2 IMPORT CLS,WriteString,WriteLn,WriteCard,ReadCard,ReadString,
                     PressAnyKey,Error;
*)
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
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
    FindInOSPathList, ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
  FROM RealIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM TermFile IMPORT Open, IsTermFile, Close;

IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS;
(* IMPORT WINUSER, WIN32, WINGDI, WINX; *)
FROM Strings IMPORT
    Append, Equal, Delete, Concat, Capitalize;
FROM ExStrings IMPORT
    AppendChar, EqualI;
FROM FormatString IMPORT FormatString;
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
IMPORT ASCII;

  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE;
  FROM TIMLIB IMPORT GETMDY, JULIAN;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;
  FROM Environment IMPORT GetCommandLine;

CONST
  szAppName = "CitiChkFilter2TW";
  InputPrompt = 'Enter cmd or HELP : ';
  LastMod = '31 Jul 04';

VAR
  C,K,c,RETCOD,m,d,y,chknum                    : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG,OK,ok,ZeroFlag    : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
      tpv1,tpv2,tpv3 : TKNPTRTYP;

  I,J                                          : INTEGER;
  INUNT1,OUTUN1                                : MYFILTYP;
  inputline, buf                               : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;
  juldate1,juldate2,juldate3                   : LONGINT;
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
  
  
PROCEDURE ExtractLastNum(tkn : BUFTYP; VAR c: CARDINAL); 
(*
Needed for processing the Citibank csv file.  I will extract the last token.
If it is a number, c will = its value, else c=0.
*)

VAR tpv : TKNPTRTYP;
     i  : INTEGER;
    tknState : FSATYP;
    RETCOD : CARDINAL;

BEGIN
	INI1TKN(tpv,tkn);
	REPEAT
		GETTKN(tpv,tkn,tknState,i,RETCOD);
	UNTIL DELIMCH = NULL;

	IF tknState = DGT THEN
	  c := ABS(i);
	ELSE
		c := 0;
	END;
  IF tpv # NIL THEN DISPOSE(tpv); END;
END ExtractLastNum;

PROCEDURE ProcessFileLines(tw : TextWindow);
VAR I,J : INTEGER;
   tpv : TKNPTRTYP;
   buf : BUFTYP;
   c   : CARDINAL;

BEGIN
  LOOP (* to read multiple lines *)
    FRDTXLN(INUNT1,INBUF,250,EOFFLG);
    IF EOFFLG THEN EXIT END(*IF*);
    INI1TKN(tpv,INBUF);
    FOR J := 1 TO 2 DO (* just write first 2 tokens as we need to process the 3rd *)
      GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);
      IF RETCOD > 0 THEN EXIT END;
      FWRSTR(OUTUN1,'"');
      FWRTX(OUTUN1,TOKEN);
      FWRSTR(OUTUN1,'",');
      WriteString(tw,TOKEN.CHARS,a);
      WriteString(tw,',',a);
      
    END(*for*);
(* TOKEN is a date string and can be processed as such *)
    ok := StrToCard(TOKEN.CHARS[1..2],m);
    ok := StrToCard(TOKEN.CHARS[4..5],d);
    ok := StrToCard(TOKEN.CHARS[7..10],y);
    IF juldate1 = 0 THEN
      juldate1 := JULIAN(m,d,y);
    ELSE
      juldate2 := JULIAN(m,d,y);
    END(*IF*);


    GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);  (* Now a null field that needs to be transnum *)
    GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);  (* Trans name description *)
    ExtractLastNum(TOKEN,c);
    IF (c = 0) AND (juldate1 = juldate2) THEN
        ok := CardToStr(chknum,buf.CHARS);
        INC(chknum);
    ELSIF (juldate2 > 0) AND (juldate1 <> juldate2) THEN
      juldate1 := juldate2;
      juldate2 := 0;
      chknum := 0;
      ok := CardToStr(c,buf.CHARS);
    END(*if*);
    TRIM(buf);
    FWRSTR(OUTUN1,'"');
    FWRTX(OUTUN1,buf);   (* output transnum *)
    FWRSTR(OUTUN1,'"');

    FWRSTR(OUTUN1,'"');
    TRIM(TOKEN);
    FWRTX(OUTUN1,TOKEN); (* output trans name *)
    FWRSTR(OUTUN1,'"');
    
    WriteString(tw,buf.CHARS,a);
    WriteString(tw,TOKEN.CHARS,a);
    
    GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);  (* dollar amt *)
    FWRSTR(OUTUN1,'"');
    FWRTX(OUTUN1,TOKEN); (* output dollar amt *)
    FWRSTR(OUTUN1,'"');
    WriteString(tw,TOKEN.CHARS,a);

    FWRSTR(OUTUN1,'"Interest Checking"');
    
    GETTKNEOL(TOKEN,RETCOD);
    FWRTXLN(OUTUN1,TOKEN);
    WriteString(tw,TOKEN.CHARS,a);
    WriteLn(tw);
    
  END(*LOOP*);
END ProcessFileLines;
  
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

        xCaret := 0;
        yCaret := 0;
        inputline := '';
(*        SetScrollDisableWhenNone(tw,TRUE,TRUE); *)
        juldate1 := 0;
        juldate2 := 0;
        juldate3 := 0;
        chknum := 1;
        DeleteFile('CHK861.ASC');
        DeleteFile('CHK.ASC');
        ok := CopyFile('CHK_861.CSV','CHK861.ASC');
        IF NOT ok THEN
          Error(' Could not copy CHK_861.CSV file.  Does it exist?');
          HALT;
        END(*if*);
        ASSIGN2BUF('CHK861.ASC',INFNAM);
        ASSIGN2BUF('CHK.ASC',OUTFNAM);
        FRESET(INUNT1,INFNAM,RD);
        FRESET(OUTUN1,OUTFNAM,WR);        
        
        
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
        ProcessFileLines(tw);
        FCLOSE(INUNT1);
        FCLOSE(OUTUN1);
        WriteLn(tw);
        WriteString(tw,' CHK file now closed.',a);
        WriteLn(tw);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        INC(c32);
        FormatString(' Number of Paint msgs is: %c.',buf,c32);
        WriteString(tw,buf,a);
        WriteLn(tw);
        WriteStringAt(tw,0,cyClient-1,LastMod,a);

    |
    TWM_KEY:
     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN 
        CASE msg.k_ch OF 
          CHR(8) :                                     (* backspace       *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);
        | CHR(9) :                                     (* tab             *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);

        | CHR(10):                                     (* line feed       *)

        | CHR(13):                                     (* carriage RETURN *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);

        | CHR(27):                                     (* escape *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);
        | CHR(32):                                     (* space *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);
        
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
    Win := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "Citibank Check Filter for TextWindows Module", (* name : ARRAY OF CHAR *)
                        "",        (* menu : ARRAY OF CHAR *)
                        "",        (* icon : ARRAY OF CHAR *)
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
  FUNC WinShell.DispatchMessages(Start, NIL);  
  
END CitiChkFilter2TW.