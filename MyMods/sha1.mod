<*DEFINE (ConsoleMode,TRUE)*>
<*DEFINE (TokenPtr,TRUE)*>

MODULE SHA1;
(*
  REVISION HISTORY
  ----------------
   3 Apr 13 -- First version of module, using AddCr2 as a template.

*)
  IMPORT Terminal,MiscM2,SYSTEM;
  FROM SYSTEM IMPORT ADDRESS, ADR;
  FROM FilePicker IMPORT FileNamePicker;
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteLongCard,WriteInt,ReadString,ReadCard,
                         Error;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT RConversions, LongStr, LongConv, WholeStr;
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
  FROM Environment IMPORT GetCommandLine;
  IMPORT SHA1;
  FROM SHA1 IMPORT SHA1, Create, Destroy, Reset, HashBytes, GetString;

%IF ConsoleMode %THEN
    IMPORT MiscStdInOut, SIOResult;
    FROM MiscStdInOut IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, ReadString, SkipLine, ReadCard, ReadLongReal;
%ELSE
    IMPORT MiscM2;
    FROM MiscM2 IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, Read, ReadString, ReadCard, ReadLongReal;
%END
<*IF TokenPtr THEN*>
    FROM TOKENPTR IMPORT FSATYP,DELIMCH,DELIMSTATE,INI1TKN,TKNPTRTYP,
      INI3TKN,GETCHR,UNGETCHR,GETTKN,GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
<*ELSE*>
    FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
      INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
      GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
<*END*>
CONST
        K = 1024;

TYPE
  IOSTATE = (RD,WR,APND);

VAR
  C,IDX,PTR,c,RETCOD                           : CARDINAL;
  CH,ch                                        : CHAR;
  FLAG,FLAG2,FLAG3,ignoreboolean,EOFFLG,stop   : BOOLEAN;
  I,J,i,j                                      : INTEGER;
  InFile,OutFile                               : File;
  InName,OutName,OldInName                     : NameString;
  innameparts,outnameparts                     : FileNameParts;
  entry                                        : SearchEntry;
  inputline,OpenFileName,hashstr               : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..100*K] OF CHAR;
  a                                            : ADDRESS;
  filesize                                     : LONGCARD;
  hash                                         : SHA1;
  

(*

These file routines will read byte by byte without any buffering or making sure an entire
line is read in.  So these are simpler than the routines 
*)

PROCEDURE fopen(VAR INOUT F:File; FILNAM:ARRAY OF CHAR; RDWRSTATE:IOSTATE);
(*
************************ fopen **************************************
File Open.
RDWRSTATE IS EITHER RD FOR OPEN A FILE FOR READING, OR WR FOR OPEN A FILE FOR WRITING.
I wrote years before.
*)

  VAR 
    I,RETCOD : CARDINAL;
    EOFFLG   : BOOLEAN;
    fileError: CommonFileErrors;
    filelength : CARDINAL32;

  BEGIN
    CASE RDWRSTATE OF
      RD : OpenFile(F,FILNAM,ReadOnlyDenyWrite);
    | WR : CreateFile(F,FILNAM);  (*This proc truncates file before creation*)
    | APND : OpenCreateFile(F,FILNAM,ReadWriteDenyWrite);
    END(*CASE*);
    IF F.status <> 0 THEN
      WriteString(' Error in opening/creating file ');
      WriteString(FILNAM);
      WriteString('--');
      CASE TranslateFileError(F) OF
        FileErrFileNotFound : WriteString('File not found.');
      | FileErrDiskFull : WriteString('Disk Full');
      ELSE
        WriteString('Nonspecific error occured.');
      END(*CASE*);
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
      HALT;
    END(*IF F.status*);

    IF RDWRSTATE = APND THEN
      filelength := FileLength(F);
      MoveFilePos(F,filelength);
    END(*IF APND*);

  END fopen;

PROCEDURE fclose(VAR INOUT F:File);
BEGIN
  CloseFile(F);
END fclose;

PROCEDURE RdWrAllTx;
(*
****************************** RdAllTx *******************************
READ and WRITE ALL TEXT from file
This routine will read from the input file char-by-char and write the same way.
Then I don't have to worry about line lengths.

Will let the OS do the buffering.  It can probably do this better than I anyway.

I'll pass the infile and outfile globally; it now does not need to be params, in fact it doesn't need
any params at all.

*)
VAR
        C : CARDINAL;
        ch: CHAR;

  BEGIN
    IF InFile.eof OR (InFile.status <> 0) OR (OutFile.status <> 0) THEN RETURN; END(*IF*);
    filesize := 1;
    ReadLoop: LOOP
      ch := ReadChar(InFile);
      IF InFile.eof THEN RETURN; END;
      IF InFile.status <> 0 THEN
        WriteString(' Error in reading from infile.  Error code = ');
        WriteCard(InFile.status);
        WriteString('.  Read not done.');
        WriteLn;
        WriteString(' Program Terminated.');
        WriteLn;
        RETURN;
      END(*IF status error condition *);
      IF ch = EOL THEN
  (* dont need this BREAK statement anymore        BREAK ReadLoop *)
        WriteChar(OutFile, ASCII.cr);
        WriteChar(OutFile, ASCII.lf);
      ELSE
        WriteChar(OutFile, ch);
      END; (* if ch = eolmarker *)
      INC(filesize);

    END; (* ReadLoop *)
  END RdWrAllTx;

(* ************************* MAIN ***************************************************************)
BEGIN
<* IF NOT ConsoleMode THEN *>
  Terminal.Reset;
<*END*>
  c := 0;
  stop := FALSE;
  filesize := 0;
  FileNamePicker(InName);
  IF LENGTH(InName) < 1 THEN 
    FLAG := BasicDialogs.PromptOpenFile(InName,'',c,'','','Open input text file',FALSE);
    IF NOT FLAG THEN
      Error('Could not open file.  Does it exist?');
      HALT;
    END; (* if not flag for BasicDialogs promptopenfile *)
  END;  (* if length(inname) from filepicker *)

  fopen(InFile,InName,RD);
  a := ADR(InBuf);

  hash := Create();
  Reset(hash);
  REPEAT
    ReadBlock(InFile,InBuf,SIZE(InBuf));
    IF InFile.status > 0 THEN
      WriteString(' Error from file ReadBlock.  FileSize is .');
      WriteLongCard(filesize);
      WriteLn;
      WriteString(' Halting ');
      WriteLn;
      fclose(InFile);
      HALT;
    END; (* if infile.status error *)
    INC(filesize,InFile.count);

    HashBytes(hash,a,InFile.count);
  UNTIL InFile.eof;
  GetString(hash,hashstr);
  WriteString(' SHA1 hash string is : ');
  WriteString(hashstr);
  WriteLn;


  fclose(InFile);

<*IF NOT ConsoleMode THEN  *>
  PressAnyKey;
<* END *>

END SHA1.
(*
PROCEDURE RenameFile(fromFile, toFile : ARRAY OF CHAR) : BOOLEAN;
PROCEDURE DeleteFile(name : ARRAY OF CHAR) : BOOLEAN;
PROCEDURE FileExists(name : ARRAY OF CHAR) : BOOLEAN;
PROCEDURE CopyFile(source, dest : ARRAY OF CHAR) : BOOLEAN;
Type
  File = RECORD   And I will only list fields of importance to me.
    status : CARDINAL;  error code of last operation.  0 if successful.
    count  : CARDINAL;  used by the read and write procs
    eof  : BOOLEAN;
  END record
FileFunc PROCEDURE ReadBlock(VAR INOUT f: File; buf : ADDRESS; size : CARDINAL);  File.count contains actual amount read.  File.success =0 if successful
SHA1 PROCEDURE Create ():SHA1;
SHA1 PROCEDURE Reset(hash:SHA1);
SHA1 PROCEDURE HashBytes(hash:SHA1; data:ADDRESS; amount:CARDINAL);
SHA1 PROCEDURE GetString(hash:SHA1; VAR OUT str:ARRAY OF CHAR);  str is 40 characters long.
    

*)
