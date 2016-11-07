MODULE vlc;
(*
  REVISION HISTORY
  ----------------
  29 Nov 13 -- First version based on TestFilePickerBase, vlcshuffle and TestXMLtoken.
                This uses *.xspf as its default pattern.
  11 Jan 14 -- Discovered that if there is only 1 match to pattern, it fails.  And I will
                have new pattern option also display some potential matches.
  18 Jan 14 -- Add procedure to make sure pattern ends in .xspf.
  22 Sep 16 -- Increased capacity to 500 tracks, and now compiled w/ the amd64 flag.  As a consequence, I could not run the compiled and
                linked exe file in the VM, because that is the 32-bit version of win7.  But it does work fine on win10.
  26 Sep 16 -- Increased capacity to 2048 tracks, and will not allow reading in more than MaxNumOfTracks.  Should have coded that before.
*)

  IMPORT Terminal,MiscM2;
  FROM Terminal IMPORT CursorUp,CursorDown,PageUp,PageDown,CursorLeft,CursorRight,Escape,Tab,BackSpace,
    Enter,Position,Reset;
  FROM MiscM2 IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
    WriteReal, WriteLongReal, (* WriteChar, ReadChar,*) Read, ReadString, ReadCard, ReadLongReal;

  FROM FilePickerBase IMPORT CountOfEntries, SetFilenamePattern, GetNextFilename, GetPrevFilename;
(* Errors are when no valid filenames were found with the given namestring pattern.  CountOfEntries will be 0.
   PROCEDURE GetNextFilename(VAR OUT ns,DirectoryEntry: NameString);
   PROCEDURE GetPrevFilename(VAR OUT ns,DirectoryEntry: NameString);
*)
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT RConversions, LongStr, LongConv, WholeStr;
  FROM Conversions IMPORT CardToStr;
  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts (*drive path name extension*), FileTypes, DeviceTypes,
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
  IMPORT BasicDialogs;
  FROM BasicDialogs IMPORT MessageTypes;
  IMPORT Strings,MemUtils,ASCII;
  FROM Strings IMPORT (*FindNext,*) Append, Equal, Delete, Concat, Capitalize;
  FROM ExStrings IMPORT AppendChar, EqualI;
  IMPORT SYSTEM;
  FROM SYSTEM IMPORT FUNC, ADDRESS;
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
  MaxNumOfTracks = 2048;
  PlaylistPattern = '*.xspf';
  blankline =
  '                                                                             '; (* ~70 spaces *)
  sepline   =
  '-----------------------------------------------------------------------------';

TYPE
  XMLtkntyp = (empty,string,openinghtml,closinghtml,othererror);
  XMLchrtyp = (eol,openangle,closeangle,slash,plain,ctrl);
  TrackType = RECORD
        location, title, creator, image, duration, extension : BUFTYP;
  END;
  XMLnameType = ARRAY [0..4] OF STR10TYP;

VAR
  ctr,len,max : CARDINAL;
  ns,DirEntry : NameString;
  C,K,c,RETCOD,strlen,indivTrackNum,LastTrackNum,shuffling    : CARDINAL;
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
      WriteString(' Trying to get XML record and got unexpected EOF condition.');
      RETURN;
    END;
    IF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'LOCATION') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        WriteString(' Trying to get location XML record and got unexpedted EOF condition or token is not a string.');
        RETURN;
      END;
      trk.location.CHARS := XMLtoken;
      TRIM(trk.location);
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);  (* retrieve the closinghtml for location *)

    ELSIF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'title') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        WriteString(' Trying to get title XML record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      trk.title.CHARS := XMLtoken;
      TRIM(trk.title);
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);  (* retrieve the closinghtml for title *)

    ELSIF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'creator') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        WriteString(' Trying to get creator XML record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      trk.creator.CHARS := XMLtoken;
      TRIM(trk.creator);
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);  (* retrieve the closinghtml for creator *)

    ELSIF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'image') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        WriteString(' Trying to get image XML record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      trk.image.CHARS := XMLtoken;
      TRIM(trk.image);
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);  (* retrieve the closinghtml for image *)

    ELSIF (XMLtokenstate = openinghtml) AND (stricmpfnt(XMLtoken,'duration') = 0) THEN
      GetXMLtoken(infile,XMLtoken,XMLtokenstate,EOFFLG);
      IF EOFFLG OR (XMLtokenstate # string)  THEN
        WriteString(' Trying to get duration XML record and got unexpected EOF condition or token is not a string.');
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
      WriteString(' in GetTrack and found an opening track tag');
    ELSIF (XMLtokenstate = closinghtml) AND (stricmpfnt(XMLtoken,'track') = 0) THEN
      EXIT;
    ELSIF (XMLtokenstate = closinghtml) AND (stricmpfnt(XMLtoken,'tracklist') = 0) THEN
        WriteString(' In GetTrack and came upon </tracklist>');
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
PROCEDURE ProcessXMLfile;
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
      WriteString(' Trying to get opening tracklist tag and got EOF condition.  Ending.');
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
    IF indivTrackNum > MaxNumOfTracks THEN  (* Don't read lines that exceed capacity to hold and shuffle *)
    	EXIT;
    END; (* if have max num of tracks *)
  END; (* loop to read in tracks *)
  LastTrackNum := indivTrackNum;

  Strings.Assign('Last track number read is ',str1);
  ok := CardToStr(LastTrackNum,str2);
  Strings.Append(str2,str1);
  WriteString(str1);
  WriteLn;

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

PROCEDURE CheckPattern(VAR ns: NameString);
VAR
        FoundChar : BOOLEAN;

BEGIN
  FoundChar := FALSE;
  len := LENGTH(ns);
  IF len = 0 THEN
    ns := PlaylistPattern;
  ELSE
        c := 0;
        WHILE c < len DO
                IF (ns[c] = '*') OR (ns[c] = '.') THEN
                  FoundChar := TRUE;
        BREAK;
      ELSE
        INC(c);
      END; (* if found characters *)
    END; (* while c in range *)
    IF NOT FoundChar THEN
        Strings.Append(PlaylistPattern,ns);
    END; (* if not foundchar *)
  END; (* if len = 0 *)
END CheckPattern;


(***************************** MAIN **********************************)
(*
   PROCEDURE GetNextFilename(VAR OUT ns,DirectoryEntry: NameString);
   PROCEDURE GetPrevFilename(VAR OUT ns,DirectoryEntry: NameString);
*)
BEGIN
  WriteLn;
  GetCommandLine(ns);
  CheckPattern(ns);
  SetFilenamePattern(ns);

  max := CountOfEntries;
  IF max > 15 THEN max := 15 END;
  IF max > 1 THEN
    FOR ctr := 1 TO max DO
        GetNextFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    END; (* for loop to get and display directory entries *)

    FOR ctr := 1 TO max DO
        GetPrevFilename(ns,DirEntry);
    END; (* for loop to get back to first directory entry *)
  END; (* for max not 0 or 1 *)
  WriteLn;
  WriteLn;
  Position(0,20);
  WriteString(blankline);
  WriteLn;
  Position(0,20);
  WriteString(sepline);
  WriteLn;
  WriteString(DirEntry);
  WriteLn;
  LOOP
    IF CountOfEntries = 0 THEN
      WriteString(' No valid filenames found.  Need New pattern. ');
      WriteLn;
    END;
    WriteString( '<enter> or <space> to select, n for new pattern ');
    MiscM2.ReadChar(ch);

    CASE ch OF
        CursorUp:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetPrevFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | Enter :
        EXIT; (* ns and DirEntry are already set *)
    | CursorDown:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetNextFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | PageUp:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetPrevFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | PageDown:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetNextFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | CursorLeft:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetPrevFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | CursorRight:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetNextFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | Tab:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetNextFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | BackSpace:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetPrevFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | ' ':
        EXIT; (* ns and DirEntry are already set *)
    | 'n','N':
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        WriteString(blankline);
        Position(0,21);
        WriteString(' Enter new pattern: ');
        ReadString(ns);
        WriteLn;
        CheckPattern(ns);
        SetFilenamePattern(ns);
        Reset;
        max := CountOfEntries;
        IF max > 15 THEN max := 15 END;
        IF max > 0 THEN
          FOR ctr := 1 TO max DO
            GetNextFilename(ns,DirEntry);
            WriteString(DirEntry);
            WriteLn;
          END; (* for loop to get and display directory entries *)
          FOR ctr := 1 TO max DO
            GetPrevFilename(ns,DirEntry);
          END; (* for loop to get back to first directory entry *)
        END; (* for max not 0 or 1 *)
        WriteLn;
        WriteLn;
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        WriteString(DirEntry);
        WriteLn;

    | Escape:
        HALT;

    ELSE
        (* ignore the character.  *)
    END; (* case ch *)

  END; (* loop to read and process a char *)
  WriteLn;
  WriteLn;
  WriteString(' Picked File Name is ');
  WriteString(ns);
  WriteLn;

  infilename := ns;
  IF NOT FileExists(infilename) THEN
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


(*   Build outfilename *)
  ParseFileName(infilename, innameparts);
  outnameparts := innameparts;
  AppendChar('_',outnameparts.name);
  MakeDateStr(datestr);
  Strings.Append(datestr,outnameparts.name);
  Strings.Assign('.xspf',outnameparts.extension);
  AssembleParts(outnameparts, outfilename);
  FUNC DeleteFile(outfilename);
  ASSIGN2BUF(outfilename,OutFileNameBuf);
  TRIM(OutFileNameBuf);
  FOPEN(OUTUN1,OutFileNameBuf,WR);

  indivTrackNum  :=  1;
  haveValidTkn := FALSE;

  ProcessXMLfile;

  RemoveFileBuffer(infile);
  CloseFile(infile);
  FCLOSE(OUTUN1);
  PressAnyKey;
END vlc.
