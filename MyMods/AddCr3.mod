MODULE AddCr3;
(*
  REVISION HISTORY
  ----------------
  5 Apr 88 -- Modified this file testing module for M2 V 3.03, and started
               using the new COPYDPIF Procedure in FIO.
  7 Apr 89 -- Using UL2 procs instead of the slower UTILLIB counterparts,
               and the updated procedure names (CONBUF renamed to
               ASSIGN2BUF).
  27 Dec 91 -- Converted to M2 V 4.00.
   2 Nov 02 -- Converted to M2 Win v4  Remember to use console mode.
   1 Aug 04 -- Testing FOPEN.
   8 Aug 04 -- Try to use FileFunc procs instead of my own.
   5 Mar 11 -- Testing MyFIO2, which rewrote FRDTXTLN incl'g the params.
   7 Mar 11 -- Made FTEST5a which really is using MyFIO2 routines.  Used to use FileFunc rtn's.
   9 Mar 11 -- Named this what it really does.  I have trouble remembering to/from DOS.
  12 Aug 11 -- Created AddCr.
  21 Aug 11 -- Made output ext to be .win to reflect the output file format.
               And now will experiment w/ file buffers thru SetFileBuffer to see if this speeds anything up.
   5 Sep 11 -- Completed coding FilePicker and will now use it here.  Since old way still works, this is V2.
   8 Sep 11 -- Now will rename old file to .old and rename new file to old file name.
  20 Oct 11 -- The 1st rename op will fail if '.old' file already exists.  I have to del this.
  24 Oct 11 -- Took out the use of the SaveAs dialog for .win.  Too many pop-ups.  It was good for debugging but not needed anymore.
  26 Oct 11 -- Took out the write message indicating the filename and bytes read.  It was more a debugging output anyway.
   8 Jan 12 -- Added capability to take command line input and use it to start the pattern searching.  And made it AddCr3.
*)
  IMPORT Terminal,MiscM2;
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
  FROM TermFile IMPORT Open, IsTermFile, Close;
  IMPORT BasicDialogs;
  FROM BasicDialogs IMPORT MessageTypes;
  IMPORT Strings,MemUtils,ASCII;
  FROM Environment IMPORT GetCommandLine;

CONST
        K = 1024;

TYPE
  IOSTATE = (RD,WR,APND);

VAR
  C,IDX,PTR,c,RETCOD                           : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,ignoreboolean,EOFFLG,stop   : BOOLEAN;
  I,J                                          : INTEGER;
  InFile,OutFile                               : File;
  InName,OutName,OldInName                     : NameString;
  innameparts,outnameparts                     : FileNameParts;
  entry                                        : SearchEntry;
  inputline,OpenFileName                       : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..10*K] OF CHAR;
  fs                                           : LONGCARD;



PROCEDURE fopen(VAR INOUT F:File; FILNAM:ARRAY OF CHAR; RDWRSTATE:IOSTATE);
(*
************************ fopen **************************************
File Open.
RDWRSTATE IS EITHER RD FOR OPEN A FILE FOR READING, OR WR FOR OPEN A FILE FOR WRITING.

*)

VAR I,RETCOD : CARDINAL;
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
  fs := 1;
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
    INC(fs);

  END; (* ReadLoop *)
END RdWrAllTx;

(* ************************* MAIN ***************************************************************)
BEGIN
  Terminal.Reset;
  c := 0;
  stop := FALSE;
(*
  FLAG := BasicDialogs.PromptOpenFile(InName,'',c,'','','Open input text file',FALSE);
  IF NOT FLAG THEN
    Error('Could not open file.  Does it exist?');
    HALT;
  END;
*)
  FileNamePicker(InName);
  IF LENGTH(InName) < 1 THEN
  	WriteString(' InName is null.  Halting.');
  	HALT;
  END;
  fopen(InFile,InName,RD);
  SetFileBuffer(InFile, InBuf);
  ParseFileName(InName, innameparts);

  outnameparts := innameparts;
  outnameparts.extension := '.win';
  AssembleParts(outnameparts, OutName);
(* This dialog is not needed anymore, and the interruption is too much.
  FLAG := BasicDialogs.PromptSaveAsFile(OutName,'',c,'','','Open output text file');
  IF NOT FLAG THEN
    Error('Could not open file.  ');
    HALT;
  END;
*)
  fopen(OutFile,OutName,WR);
  SetFileBuffer(OutFile,OutBuf);

  RdWrAllTx;

  fclose(InFile);
  fclose(OutFile);
(*
  WriteLn;
  WriteString(' Output filename: ');
  WriteLn;
  WriteString(OutName);
  WriteLn;
  WriteLn;
  WriteString(' Characters written: ');
  WriteLongCard(fs);
  WriteString('.');
  WriteLn;
*)
  innameparts.extension := '.old';
  AssembleParts(innameparts,OldInName);
  ignoreboolean := DeleteFile(OldInName);
  ignoreboolean := RenameFile(InName,OldInName);
  ignoreboolean := RenameFile(OutName,InName);
(*
  PressAnyKey;
*)
END AddCr3.
(*
PROCEDURE RenameFile(fromFile, toFile : ARRAY OF CHAR) : BOOLEAN;
PROCEDURE DeleteFile(name : ARRAY OF CHAR) : BOOLEAN;
PROCEDURE FileExists(name : ARRAY OF CHAR) : BOOLEAN;
PROCEDURE CopyFile(source, dest : ARRAY OF CHAR) : BOOLEAN;
*)
