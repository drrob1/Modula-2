<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:VLCshuffle.RES*>
%ELSE
%END
MODULE vlcshuffle;
(*
  REVISION HISTORY
  ----------------
  21 Nov 13 -- First conception, based on qfx2xls.  Playlist file ext is xspf.
  25 Nov 13 -- First version that produced output.  But VLC won't yet understand new file.
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
FROM BasicDialogs IMPORT MessageTypes;
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
  FROM FilePicker IMPORT FileNamePicker;
  IMPORT MYRAND;
  FROM MYRAND IMPORT RANDCARD;

CONST
  szAppName = "vlcshuffle";
  InputPrompt = 'Enter cmd or HELP : ';
  LastMod = '21 Nov 13';
  CornerIcon32 = '#100';
  CornerIcon16 = '#200';
  MenuSep = '|';
  MaxNumOfTracks = 256;

TYPE
  XMLtkntyp = (empty,string,openinghtml,closinghtml,othererror);
  XMLchrtyp = (eol,openangle,closeangle,slash,plain);
  TrackType = RECORD
        location, title, creator, image, duration, extension : BUFTYP;
  END;


VAR
  C,K,c,RETCOD,strlen,indivTrackNum,LastTrackNum,shuffling    : CARDINAL;
  c32,d32,e32,f32,g32                                         : CARDINAL32;
  CH,ch                                                       : CHAR;
  haveValidTkn,bool,EOFFLG,OK,ok,done                         : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN : BUFTYP;
  trackArray                                     : ARRAY [1 .. MaxNumOfTracks] OF TrackType;
  NumArray                                       : ARRAY [1 .. MaxNumOfTracks] OF CARDINAL;
  XMLtoken,datestr,peekXMLtoken,str1,str2        : STRTYP;
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
  tabchar                                       : Strings.String1 = ASCII.ht;
  cxChar,cyChar,cxClient,cyClient,cxBuffer,cyBuffer,xCaret,yCaret : INTEGER;
  cxScreen,cyScreen : COORDINATE;
  LastModLen  : CARDINAL;
  a           : ScreenAttribute;
  Win         : TextWindow;
(*************************************************************************************************)
PROCEDURE MakeDateStr(VAR OUT dstr : ARRAY OF CHAR);
CONST
        DateSepChar = '-';

VAR
        m,d,y                  : CARDINAL;
        SYSTIME                : DateTime;
        MSTR,DSTR,YSTR,tempstr : STR10TYP;

BEGIN
  GetClock(SYSTIME);
  WITH SYSTIME DO
    OK := CardToStr(month,MSTR);
    OK := CardToStr(day,DSTR);
    OK := CardToStr(year,YSTR);
    Strings.Assign(MSTR,dstr);
    AppendChar(DateSepChar,dstr);
    Strings.Append(DSTR,dstr);
    AppendChar(DateSepChar,dstr);
    Strings.Append(YSTR,dstr);
    ok := CardToStr(hour,tempstr);
    AppendChar(DateSepChar,dstr);
    Strings.Append(tempstr,dstr);
    ok := CardToStr(minute,tempstr);
    AppendChar(DateSepChar,dstr);
    Strings.Append(tempstr,dstr);
    ok := CardToStr(second,tempstr);
    AppendChar(DateSepChar,dstr);
    Strings.Append(tempstr,dstr);
    ok := CardToStr(fractions,tempstr);
    AppendChar(DateSepChar,dstr);
    Strings.Append(tempstr,dstr);
  END; (* with *)
END MakeDateStr;

(**************************************************************************************************)
PROCEDURE Shuffle(ntimes:CARDINAL);
VAR C,K,c,shuffleswap: CARDINAL;

BEGIN (* SHUFFLE *)
  FOR c := 1 TO ntimes DO
(*
  Shuffle the array - pass *once* through the array, swapping each element
  with another, randomly chosen, element.
*)
    FOR C := LastTrackNum TO 2 BY -1 DO
(* swap element in ith place with any element at or below that place *)
      K := MYRAND.RANDCARD(C)+1;
      shuffleswap := NumArray[C];
      NumArray[C] := NumArray[K];
      NumArray[K] := shuffleswap;
    END(*FOR*);
  END(*FOR*);
END Shuffle;

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
                  chstate := plain;
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
                        MiscM2.Error(' In GetXMLToken.  Empty token got closeangle ch');
              END; (* case chstate *)
            | string :
              CASE chstate OF
                plain,slash :
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
              | closeangle,eol :
                  ch := ReadChar(f); (* swallow ch *)
                  EXIT;
              END; (* case chstate *)
            | closinghtml :
              CASE chstate OF
                plain,slash,openangle :
                        ch := ReadChar(f);
                        Strings.Append(ch,peekXMLtoken);
              | closeangle,eol :
                        ch := ReadChar(f); (* swallow ch *)
                        EXIT;
              END; (* case chstate *)
          ELSE
            MiscM2.Error(' In GetXMLtoken and tokenstate is in ELSE clause of CASE.');
          END (* case XMLtknstate *)
        END; (* loop *)
        haveValidTkn := TRUE;
        XMLtoken := peekXMLtoken;
        XMLtokenstate := peekXMLtokenstate;
      END; (* if have valid token *)
END PeekXMLtoken;

PROCEDURE NextXMLtoken;
BEGIN
  haveValidTkn := FALSE;
END NextXMLtoken;

PROCEDURE GetXMLtoken(VAR INOUT f:File; VAR OUT XMLtoken:STRTYP; VAR OUT XMLtokenstate:XMLtkntyp; VAR OUT EOFFLG:BOOLEAN);
BEGIN
  NextXMLtoken;
  PeekXMLtoken(f, XMLtoken, XMLtokenstate, EOFFLG);
END GetXMLtoken;


(**************************************************** GetTrack *********************************************)
PROCEDURE GetTrack(VAR OUT trk : TrackType);
(**************************************************** GetTrack *********************************************)

VAR I,J          : INTEGER;
    ch           : CHAR;
    c1,c2        : CARDINAL;
    str,s0,s1,s2 : STRTYP;

BEGIN
(* Must init record fields *)
  WITH trk DO
    location.CHARS := '';
    title.CHARS := '';
    creator.CHARS := '';
    image.CHARS := '';
    duration.CHARS := '';
    extension.CHARS := '';
  END; (* with *)

  LOOP
    GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
    IF EOFFLG THEN
      MiscM2.Error(' Trying to get XML record and got unexpected EOF condition.');
      RETURN;
    END;
    IF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'LOCATION') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        MiscM2.Error(' Trying to get location XML record and got unexpedted EOF condition or token is not a string.');
        RETURN;
      END;
      trk.location.CHARS := XMLtoken;
      TRIM(trk.location);
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);  (* retrieve the closinghtml for location *)

    ELSIF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'title') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        MiscM2.Error(' Trying to get title XML record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      trk.title.CHARS := XMLtoken;
      TRIM(trk.title);
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);  (* retrieve the closinghtml for title *)

    ELSIF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'creator') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        MiscM2.Error(' Trying to get creator XML record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      trk.creator.CHARS := XMLtoken;
      TRIM(trk.creator);
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);  (* retrieve the closinghtml for creator *)

    ELSIF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'image') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        MiscM2.Error(' Trying to get image XML record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      trk.image.CHARS := XMLtoken;
      TRIM(trk.image);
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);  (* retrieve the closinghtml for image *)

    ELSIF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'duration') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        MiscM2.Error(' Trying to get duration XML record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      trk.duration.CHARS := XMLtoken;
      TRIM(trk.duration);
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);  (* retrieve the closinghtml for duration *)

    ELSIF (XMLtokenstate = openinghtml) AND SubStrCMPFNT('extension',XMLtoken) THEN
(* this tag is more complicated than the others because it includes an application and a nested vlc:id tag *)
      trk.extension.CHARS := XMLtoken;
      TRIM(trk.extension);
(* retrieve and discard the vlc:id tag in its entirity *)
      REPEAT
        GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      UNTIL EOFFLG OR ((XMLtokenstate = closinghtml) AND (stricmpfnt(XMLtoken,'extension') = 0));

(* now should never get here because this tag is really part of the extension tag and it swollowed there *)
    ELSIF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'track') = 0) THEN
      MiscM2.Error(' in GetTrack and found an opening track tag');
    ELSIF (XMLtokenstate = closinghtml) AND (stricmpfnt(XMLtoken,'track') = 0) THEN
      EXIT;
    ELSIF (XMLtokenstate = closinghtml) AND (stricmpfnt(XMLtoken,'tracklist') = 0) THEN
        MiscM2.Error(' In GetTrack and came upon </tracklist>');
      EXIT;
    ELSE
(*      Have random white space here, typically either a space or a tab before an opening html tag.  Ignore it. *)
    END; (* if XMLtknstate = whatever *)
  END; (* loop for all contents of this track *)
END GetTrack;

(**************************************************** PutTrack *********************************************)
PROCEDURE PutTrack(n: CARDINAL);
(**************************************************** PutTrack *********************************************)
(* Input from global variable: indivTrackNum                                                               *)

VAR
  nstr    : STR10TYP;

BEGIN
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,'<track>');
  FWRLN(OUTUN1);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,' <location>');
  FWRTX(OUTUN1,trackArray[n].location);
  FWRSTR(OUTUN1,'</location>');
  FWRLN(OUTUN1);

  IF trackArray[n].title.COUNT > 0 THEN
        FWRSTR(OUTUN1,tabchar);
        FWRSTR(OUTUN1,tabchar);
        FWRSTR(OUTUN1,tabchar);
        FWRSTR(OUTUN1, ' <title>');
        FWRTX(OUTUN1,trackArray[n].title);
        FWRSTR(OUTUN1,'</title>');
        FWRLN(OUTUN1);
  END; (* if have a title tag *)
  IF trackArray[n].creator.COUNT > 0 THEN
        FWRSTR(OUTUN1,tabchar);
        FWRSTR(OUTUN1,tabchar);
        FWRSTR(OUTUN1,tabchar);
        FWRSTR(OUTUN1, ' <creator>');
        FWRTX(OUTUN1,trackArray[n].creator);
        FWRSTR(OUTUN1,'</creator>');
        FWRLN(OUTUN1);
  END; (* if have a creator tag *)
  IF trackArray[n].image.COUNT > 0 THEN
        FWRSTR(OUTUN1,tabchar);
        FWRSTR(OUTUN1,tabchar);
        FWRSTR(OUTUN1,tabchar);
        FWRSTR(OUTUN1, ' <image>');
        FWRTX(OUTUN1,trackArray[n].image);
        FWRSTR(OUTUN1,'</image>');
        FWRLN(OUTUN1);
  END; (* if have a image tag *)

  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,' <duration>');
  FWRTX(OUTUN1,trackArray[n].duration);
  FWRSTR(OUTUN1,'</duration>');
  FWRLN(OUTUN1);

  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,' <');
  FWRTX(OUTUN1,trackArray[n].extension);
  FWRSTR(OUTUN1,'> ');
  FWRLN(OUTUN1);

  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,'  <vlc:id>');
  ok := CardToStr(indivTrackNum,nstr);
  FWRSTR(OUTUN1,nstr);
  FWRSTR(OUTUN1,'</vlc:id>');
  FWRLN(OUTUN1);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,' </extension>');
  FWRLN(OUTUN1);

  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,'</track>');
  FWRLN(OUTUN1);
  INC(indivTrackNum);
END PutTrack;


(********************************************************************************************)
PROCEDURE ProcessXMLfile(tw : TextWindow);
(********************************************************************************************)

VAR
   buf        : BUFTYP;
   strlen,k,c : CARDINAL;

BEGIN
  strlen := ReadLine(infile,inputline);  (* this is the ?xml version line *)
  IF strlen >250 THEN
        MiscM2.Error(' first line of input file is longer than 250 chars.  This pgm may fail.');
  END;
  ASSIGN2BUF(inputline,INBUF);
  FWRTX(OUTUN1,INBUF);
  FWRLN(OUTUN1);

  strlen := ReadLine(infile,inputline);  (* this is the playlist xmlns= line *)
  IF strlen >250 THEN
        MiscM2.Error(' second line of input file is longer than 250 chars.  This pgm may fail.');
  END;
  ASSIGN2BUF(inputline,INBUF);
  FWRTX(OUTUN1,INBUF);
  FWRLN(OUTUN1);

  strlen := ReadLine(infile,inputline);  (* this is the title line *)
  IF strlen >250 THEN
        MiscM2.Error(' 3rd line of input file is longer than 250 chars.  This pgm may fail.');
  END;
  ASSIGN2BUF(inputline,INBUF);
  FWRTX(OUTUN1,INBUF);
  FWRLN(OUTUN1);

  LOOP (* ignoring white space until get the opening tracklist tag *)
    GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
    IF EOFFLG THEN
      MiscM2.Error(' Trying to get opening tracklist tag and got EOF condition.  Ending.');
      RETURN;
    END;
    IF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'tracklist') = 0) THEN
      EXIT;
    END; (* if have tracklist *)
  END; (* loop until get opening tracklist tag *)

  LOOP (* ignoring white space until get the opening track tag *)
    GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
    IF EOFFLG THEN
      MiscM2.Error(' Trying to get opening track tag and got EOF condition.  Ending.');
      RETURN;
    END;
    IF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'track') = 0) THEN
      EXIT;
    END; (* if have track *)
  END; (* loop until get the opening track tag *)

  LOOP (* to read the continuous stream of track tokens *)
    GetTrack(trackArray[indivTrackNum]);
(*
 This next token will either be a closing tracklist tag or an opening track tag.  If it is not
 a closing tracklist tag to end the loop, then we just swollowed the next opening track tag which
 is perfect for the GetTrack rtn anyway.
*)
    REPEAT (* to swollow garbage white space *)
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG THEN
        MiscM2.Error(' Trying to get another track tag and got EOF condition.  Ending.');
        RETURN;
      END;
    UNTIL (XMLtokenstate = closinghtml) OR (XMLtokenstate = openinghtml);
    IF (XMLtokenstate = closinghtml) AND (stricmpfnt(XMLtoken,'tracklist') = 0) THEN
        EXIT;
    END; (* if have closing tracklist *)
    INC(indivTrackNum);
  END; (* loop to read in tracks *)
  LastTrackNum := indivTrackNum;

  Strings.Assign('Last track number read is ',str1);
  ok := CardToStr(LastTrackNum,str2);
  Strings.Append(str2,str1);
  WriteString(tw,str1,a);
  WriteLn(tw);

(*  BasicDialogs.MessageBox(str1,MsgInfo); *)

(*
  need to shuffle here
*)
  FOR k:= 1 TO LastTrackNum DO
    NumArray[k] := k;
  END;

  SysClock.GetClock(dt);
  shuffling := dt.month + dt.day + dt.hour + dt.minute + dt.year + dt.second + dt.fractions;
  FOR k := 1 TO shuffling DO
    Shuffle(LastTrackNum);
  END; (* for shuffle *)


(* Finished shuffling *)
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,'<trackList>');
  FWRLN(OUTUN1);

  indivTrackNum := 0;

  FOR c := 1 TO LastTrackNum DO
        PutTrack(NumArray[c]);
  END;

(*
  FOR c := 1 TO LastTrackNum DO
        PutTrack(c);
  END;
*)
  FWRSTR(OUTUN1,tabchar);
  FWRSTR(OUTUN1,'</trackList>');
  FWRLN(OUTUN1);

  LOOP (* to read and write the rest of the lines *)
    strlen := ReadLine(infile,inputline); (* ignore the strlen because the closing tracklist tag is last on its line, so the next readline imm'ly finds EOL so strlen=0 *)
    IF infile.eof OR (infile.status > 0) THEN EXIT END;
    FWRSTR(OUTUN1,inputline);
    FWRLN(OUTUN1);
  END; (* final read and write loop *)


END ProcessXMLfile;



(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr         : Colors;
    x,y         : COORDINATE;
    i,int       : INTEGER;
    cmdline     : ARRAY [0..255] OF CHAR;
    cmdbuf,tkn  : BUFTYP;
    retcod,c5   : CARDINAL;
    filter,s    : STRTYP;

BEGIN
    CASE msg.msg OF
    TWM_CLOSE:  (* Turns out that this winmsg is being executed twice before the pgm closes.  I have no idea why *)
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        RemoveFileBuffer(infile);
        CloseFile(infile);
        FCLOSE(OUTUN1);
        WriteLn(tw);
        WriteString(tw,OUTFNAM.CHARS,a);
        WriteString(tw,' file now closed.',a);
        WriteLn(tw);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
(*        MiscM2.PressAnyKey; *)
        RETURN OkayToClose;
    | TWM_CREATE:
        FUNC SetWindowIcon(tw, CornerIcon16);

        xCaret := 0;
        yCaret := 0;
        inputline := '';

        INFNAM.CHARS := '';
        OUTFNAM.CHARS := '';

(* Will do filenamepicker first *)
        Terminal.Reset;
        c := 0;
        FileNamePicker(infilename);
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
  PROCEDURE BasicDialogs.PromptOpenFile(VAR INOUT name : ARRAY OF CHAR; filters : ARRAY OF CHAR; VAR INOUT defFilter : CARDINAL; defDir : ARRAY OF CHAR;
                                            defExt : ARRAY OF CHAR; title : ARRAY OF CHAR; createable : BOOLEAN) : BOOLEAN;
 Opens an operating system common dialog for opening  a file filters specifies a list of file extension filters that are separated by semicolons.
   The format for filters is as follows.
   defDir = the default directory to start the dialog in an empty string "" means use the current directory.
   defExt = the default file extension to use if the user does not provide an extension. "" means no default extension.
                                                                                             the extension should *not* have a leading '.' character.
   title = the caption text of the dialog. title can be empty "".  in this case the default operating system title is used.
   If createable = TRUE then the file need not already exist, otherwise the file must exist for the dialog to return successful.
   RETURNs TRUE if successful and name will contain the file specification
   for the file the user has given.
*)
          c5 := 1;
          DlgShell.ConvertToNulls(MenuSep,filter);
          bool := BasicDialogs.PromptOpenFile(infilename,filter,c5,'','','Open VLC Playlist file',FALSE);
          BasicDialogs.MessageBox(infilename,MsgInfo);
          IF NOT bool THEN
            WriteString(tw,'Could not find file.  Does it exist?',a);
            HALT;
          END;
        END; (* if filenamepicker returned nothing then use PromptOpenFile *)

        IF NOT FileFunc.FileExists(infilename) THEN
          MiscM2.Error(' Could not find input file.  Does it exist?');
          HALT;
        END(*if*);

        OpenFile(infile,infilename,ReadOnlyDenyWrite);
        IF infile.status > 0 THEN
          WriteString(tw,' Error in opening/creating file ',a);
          WriteString(tw,inputline,a);
          WriteString(tw,'--',a);
          CASE TranslateFileError(infile) OF
            FileErrFileNotFound : WriteString(tw,'File not found.',a);
          | FileErrDiskFull : WriteString(tw,'Disk Full',a);
          ELSE
            WriteString(tw,'Nonspecific error occured.',a);
          END(*CASE*);
          WriteLn(tw);
          WriteString(tw,' Program Terminated.',a);
          WriteLn(tw);
          HALT;
        END(*IF infile.status*);
        SetFileBuffer(infile,InBuf);


(*   Build outfilename *)
                                               (*
                                                 TYPE FileNameParts = RECORD
                                                      drive     : NameString;
                                                      path      : FileSpecString;
                                                      name      : NameString;     excluding the extension
                                                      extension : NameString;
                                                      END;
                                               *)
        ParseFileName(infilename, innameparts);
        outnameparts := innameparts;
        AppendChar('_',outnameparts.name);
        MakeDateStr(datestr);
        Strings.Append(datestr,outnameparts.name);
        Strings.Assign('.xspf',outnameparts.extension);
        AssembleParts(outnameparts, outfilename);
        FUNC FileFunc.DeleteFile(outfilename);
        ASSIGN2BUF(outfilename,OutFileNameBuf);
        TRIM(OutFileNameBuf);
        FOPEN(OUTUN1,OutFileNameBuf,WR);


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
        ProcessXMLfile(tw);

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
                        "VLC Shuffle", (* name : ARRAY OF CHAR *)
                        "",        (* menu : ARRAY OF CHAR *)
                        "CornerIcon16",        (* icon : ARRAY OF CHAR *)
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
  indivTrackNum  :=  1;
  haveValidTkn := FALSE;
  FUNC WinShell.DispatchMessages(Start, NIL);

END vlcshuffle.
