MODULE AddCr;
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
*)
  IMPORT Terminal,MiscM2;
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
(*
  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENPTR IMPORT FSATYP,DELIMCH,INI3TKN,INI1TKN,GETTKN;
  FROM MyFIO2 IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FOPEN,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FWRLF,FAPPEND,COPYDPIF,GETFNM;
*)
  FROM Environment IMPORT GetCommandLine;

CONST
        K = 1024;

TYPE
  IOSTATE = (RD,WR,APND);

VAR
  C,IDX,PTR,c,RETCOD                           : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG,stop     : BOOLEAN;
  I,J                                          : INTEGER;
  InFile,OutFile                               : File;
  innameparts,outnameparts                     : FileNameParts;
  entry                                        : SearchEntry;
  inputline,OpenFileName,InName,OutName        : ARRAY [0..255] OF CHAR;
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
(*
  PROCEDURE PromptOpenFile(VAR INOUT name : ARRAY OF CHAR;
                         filters : ARRAY OF CHAR;
                         VAR INOUT defFilter : CARDINAL;
                         defDir : ARRAY OF CHAR;
                         defExt : ARRAY OF CHAR;
                         title : ARRAY OF CHAR;
                         createable : BOOLEAN) : BOOLEAN;
*)
(* Opens an operating system common dialog for opening  a file
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
  Terminal.Reset;
  c := 0;
  stop := FALSE;
  FLAG := BasicDialogs.PromptOpenFile(InName,'',c,'','','Open input text file',FALSE);
  IF NOT FLAG THEN
    Error('Could not open file.  Does it exist?');
    HALT;
  END;
(*  BasicDialogs.MessageBox(InName,MsgInfo); *)
  fopen(InFile,InName,RD);
  SetFileBuffer(InFile, InBuf);
  ParseFileName(InName, innameparts);

  outnameparts := innameparts;
  outnameparts.extension := '.win';
  AssembleParts(outnameparts, OutName);
  FLAG := BasicDialogs.PromptSaveAsFile(OutName,'',c,'','','Open output text file');
  IF NOT FLAG THEN
    Error('Could not open file.  ');
    HALT;
  END;
(*  BasicDialogs.MessageBox(OutName,MsgInfo); *)

  fopen(OutFile,OutName,WR);
  SetFileBuffer(OutFile,OutBuf);

  RdWrAllTx;

  fclose(InFile);
  fclose(OutFile);

  WriteString(' Output filename: ');
  WriteLn;
  WriteString(OutName);
  WriteLn;
  WriteLn;
  WriteString(' Characters written: ');
  WriteLongCard(fs);
  WriteString('.');
  WriteLn;
  PressAnyKey;

END AddCr.
