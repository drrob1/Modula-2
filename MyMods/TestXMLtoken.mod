MODULE TestXMLtoken;
(*
  REVISION HISTORY
  ----------------
  25 Nov 13 -- First conception.  Mainly to write and test new behavior by ignoring tabs and spaces if the token is empty
  26 Nov 13 -- Added the ctrl chartype.  Not yet implemented.
  28 Nov 13 -- Testing now.
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
(*
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
*)
IMPORT Terminal, BasicDialogs, DlgShell, WinShell;
FROM Terminal IMPORT WriteString,WriteLn,Read,Write;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT WholeStr, LongStr, LongConv;

  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,stricmpfnt,SubStrCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,
    RMVCHR,APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TIMLIB IMPORT GETMDY,JULIAN,TIME2MDY,MDY2STR;
  IMPORT SysClock;
  FROM SysClock IMPORT DateTime,GetClock;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FOPEN,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM,ChopIntoLines;
  FROM Environment IMPORT GetCommandLine;
  FROM FilePicker IMPORT FileNamePicker;
  IMPORT MYRAND;
  FROM MYRAND IMPORT RANDCARD;

CONST
  InputPrompt = 'Enter test line : ';
  LastMod = '28 Nov 13';
  MenuSep = '|';

TYPE
  XMLtkntyp = (empty,string,openinghtml,closinghtml,othererror);
  XMLchrtyp = (eol,openangle,closeangle,slash,plain,ctrl);
  TrackType = RECORD
        location, title, creator, image, duration, extension : BUFTYP;
  END;
  XMLnameType = ARRAY [0..4] OF STR10TYP;

  CONST XMLname = XMLnameType {'empty','string','openinghtml','closinghtml','othererror'};


VAR
  C,K,c,RETCOD,strlen,indivTrackNum,LastTrackNum : CARDINAL;
  c32,d32,e32,f32,g32                            : CARDINAL32;
  CH,ch                                          : CHAR;
  haveValidTkn,bool,EOFFLG,OK,ok,done            : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN : BUFTYP;
  XMLtoken,datestr,peekXMLtoken,str1,str2,filter : STRTYP;
  XMLtokenstate,peekXMLtokenstate                : XMLtkntyp;
  dt                                             : DateTime;
  I,J                                           : INTEGER;
  INUNT1,OUTUN1                                 : MYFILTYP;
  infile                                        : File;
  inputline,buf,bl,deststring1,deststring2      : ARRAY [0..255] OF CHAR;
  timefmtstring,datetimefmtstring               : ARRAY [0..255] OF CHAR;
  infilename,outfilename                        : NameString;
  innameparts,outnameparts                      : FileNameParts;
  OutFileNameBuf                                : BUFTYP;
  InBuf, OutBuf                                 : ARRAY [1..8*1024] OF CHAR;

(************************************************************************************************************************)
PROCEDURE PeekXMLtoken(VAR INOUT f:File; VAR OUT XMLtoken:STRTYP; VAR OUT XMLtokenstate:XMLtkntyp; VAR OUT EOFFLG:BOOLEAN);
(*********************************************** GetXMLToken **********************************
This will use the FileFunc file operations as I do not want to read by lines.  I want this
as a character stream.  The only delimiters are angle brackets and EOL.  This is the only routine
where input characters are read and processed.

3/3/2011 2:30:50 PM
All references in this rtn to ReadChar(infile) is a bug.  It should be ReadChar(f) to reference
the param, not using the global infile.  Since all calls to GetXMLToken use infile as f, I
did not find this bug before.

11/22/2013 7:53:18 AM
Fixed the ReadChar(infile) -> ReadChar(f) bug.
*********************************************************************************************)
  VAR
    ch          : CHAR;
    chstate     : XMLchrtyp;

  BEGIN
      IF haveValidTkn THEN
        XMLtoken := peekXMLtoken;
        XMLtokenstate := peekXMLtokenstate;
        RETURN;
      ELSE
        peekXMLtoken := '';
        peekXMLtokenstate := empty;
        EOFFLG := FALSE;
        LOOP
          ch := PeekChar(f);
          IF f.status > 0 THEN
                XMLtoken := '';
                XMLtokenstate := othererror;
                RETURN;
          END; (* if file.status is error cond *)
          IF f.eof THEN
                chstate := eol;
                EOFFLG := TRUE;
                EXIT;
          ELSE (* ch is a valid character *)
                CASE ch OF  (* here is where the state of ch is determined.  Do not need a separate proc as unget is handled by PeekChar instead *)
                  EOL : chstate := eol;
                | '<' : chstate := openangle;
                | '>' : chstate := closeangle;
                | '/' : chstate := slash;
                ELSE
                  IF ORD(ch) <= 31 THEN (* since all case tags must be unique, cannot overlap w/ EOL so this is a separate IF *)
                        chstate := ctrl;
                  ELSE
                    chstate := plain;
                  END; (* if ctrl char *)
                END; (* case ch *)
          END; (* if file.eof *)

          CASE peekXMLtokenstate OF
            empty :
              CASE chstate OF
                plain,slash :
                        peekXMLtokenstate := string;
                        ch := ReadChar(f);
                        Strings.Append(ch,peekXMLtoken);
              | openangle :
                        ch := ReadChar(f);  (* Swallow ch *)
                        peekXMLtokenstate := openinghtml;
              | eol :
                        ch := ReadChar(f); (* Swallow eol *)
              | closeangle :
                        XMLtokenstate := othererror;
                        RETURN;
              | ctrl :
                        ch := ReadChar(f); (* Swallow any ascii control character not processed above like EOL *)
              END; (* case chstate *)
            | string :
              CASE chstate OF
                plain,slash,ctrl :
                        ch := ReadChar(f);
                        Strings.Append(ch,peekXMLtoken);
              | eol :
                        ch := ReadChar(f); (* Swallow EOL ch *)
                        EXIT;
              | openangle : (* openangle char is still avail for next loop iteration *)
                        EXIT;
              | closeangle :
                        MiscM2.Error(' In GetXMLToken.  String token got closeangle ch');
              END; (* case chstate *)
            | openinghtml :
              CASE chstate OF
                plain,openangle :
                  ch := ReadChar(f);
                  Strings.Append(ch,peekXMLtoken);
              | slash :
                  ch := ReadChar(f);
                  IF LENGTH(peekXMLtoken) = 0 THEN
                    peekXMLtokenstate := closinghtml
                  ELSE
                    Strings.Append(ch,peekXMLtoken);
                  END; (* if length =0 *)
              | closeangle,eol,ctrl :
                  ch := ReadChar(f); (* swallow ch *)
                  EXIT;
              END; (* case chstate *)
            | closinghtml :
              CASE chstate OF
                plain,slash,openangle :
                        ch := ReadChar(f);
                        Strings.Append(ch,peekXMLtoken);
              | closeangle,eol,ctrl :
                        ch := ReadChar(f); (* swallow ch *)
                        EXIT;
              END; (* case chstate *)
          ELSE
            MiscM2.Error(' In GetXMLtoken and tokenstate is in ELSE clause of CASE.');
            XMLtokenstate := othererror;
            RETURN;
          END (* case XMLtknstate *)
        END; (* loop *)
        haveValidTkn := TRUE;
        XMLtoken := peekXMLtoken;
        XMLtokenstate := peekXMLtokenstate;
      END; (* if have valid token *)
END PeekXMLtoken;
(*********************************************** NextXMLtoken **************************************************)

PROCEDURE NextXMLtoken;
BEGIN
  haveValidTkn := FALSE;
END NextXMLtoken;
(************************************************ GetXMLtoken *************************************************)

PROCEDURE GetXMLtoken(VAR INOUT f:File; VAR OUT XMLtoken:STRTYP; VAR OUT XMLtokenstate:XMLtkntyp; VAR OUT EOFFLG:BOOLEAN);
BEGIN
  NextXMLtoken;
  PeekXMLtoken(f, XMLtoken, XMLtokenstate, EOFFLG);
END GetXMLtoken;

(************************************ MAIN *************************************************************)

BEGIN
    Terminal.Reset;
    infilename := '';
(*    FileNamePicker(infilename); *)
    IF LENGTH(infilename) < 1 THEN   (* filenamepicker returned nothing, so will use BasicDialogs routine here *)
      filter := 'VLC Playlist Files';
      Strings.Append(MenuSep,filter);
      Strings.Append('*.xspf',filter);
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
  PROCEDURE BasicDialogs.PromptOpenFile(VAR INOUT name : ARRAY OF CHAR; filters : ARRAY OF CHAR; VAR INOUT defFilter : CARDINAL; defDir : ARRAY OF CHAR; defExt : ARRAY OF CHAR; title : ARRAY OF CHAR; createable : BOOLEAN) : BOOLEAN;
 Opens an operating system common dialog for opening  a file filters specifies a list of file extension filters that are separated by semicolons.
   The format for filters is as follows.
   defDir = the default directory to start the dialog in an empty string "" means use the current directory.
   defExt = the default file extension to use if the user does not provide an extension. "" means no default extension.  The extension should *not* have a leading '.' character.
   title = the caption text of the dialog. title can be empty "".  in this case the default operating system title is used.
   If createable = TRUE then the file need not already exist, otherwise the file must exist for the dialog to return successful.
   RETURNs TRUE if successful and name will contain the file specification
   for the file the user has given.
*)
          c := 1;
          DlgShell.ConvertToNulls(MenuSep,filter);
          bool := BasicDialogs.PromptOpenFile(infilename,filter,c,'','','Open VLC Playlist file',FALSE);
(*          BasicDialogs.MessageBox(infilename,MsgInfo); *)
          IF NOT bool THEN
            WriteString('Could not find file.  Does it exist?');
            HALT;
          END;
        END; (* if filenamepicker returned nothing then use PromptOpenFile *)

        IF NOT FileFunc.FileExists(infilename) THEN
          MiscM2.Error(' Could not find input file.  Does it exist?');
          HALT;
        END(*if*);

        OpenFile(infile,infilename,ReadOnlyDenyWrite);
        IF infile.status > 0 THEN
          WriteString(' Error in opening/creating file ');
          WriteString(inputline);
          WriteString('--');
          CASE TranslateFileError(infile) OF
            FileErrFileNotFound : WriteString('File not found.');
          | FileErrDiskFull : WriteString('Disk Full');
          ELSE
            WriteString('Nonspecific error occured.');
          END(*CASE*);
          WriteLn;
          WriteString(' Program Terminated.');
          WriteLn;
          HALT;
        END(*IF infile.status*);
        SetFileBuffer(infile,InBuf);
        WriteLn;

    PeekXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
    REPEAT
      WriteString(' token is: ');
      WriteString(XMLtoken);
      WriteLn;
      WriteString(' token state is: ');
      WriteString(XMLname[ORD(XMLtokenstate)]);
      WriteString(', get _N_ext token, _E_ or _Q_ ? ');
      Read(ch);
      Write(ch);
      WriteLn;
      IF CAP(ch)='N' THEN
        NextXMLtoken;
      END;
      PeekXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF (CAP(ch)='E') OR (CAP(ch)='Q') THEN
        EOFFLG := TRUE;
      END;
    UNTIL EOFFLG;
    WriteLn;
    WriteLn;
END TestXMLtoken.
