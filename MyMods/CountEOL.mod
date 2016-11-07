MODULE CountEOL;
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
  12 Mar 11 -- Used FTEST5 as a template for CountEOL
  12 Aug 11 -- Program does not crash when buffer > 4K which is the stack size.  
               Must be because all params are passed by reference and are not copied
               to the stack.
*)
  IMPORT Terminal,MiscM2,ASCII;
  FROM Terminal IMPORT WriteString,WriteLn;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SYSTEM IMPORT ADR;
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
  IMPORT Strings,MemUtils;
  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENPTR IMPORT FSATYP,DELIMCH,INI3TKN,INI1TKN,GETTKN;
(*
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FOPEN,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;
*)
  FROM Environment IMPORT GetCommandLine;

CONST
    IOBUFSIZ = 10 * 1024 * 1024;
    IOBUFSIZMAX = IOBUFSIZ + IOBUFSIZ + 1;
    EOFMARKER = 032C; (* CHR(26) OR CTRL-Z *)
    DRIVESEP  = ':';
    SUBDIRSEP = '\';

TYPE
  IOSTATE = (RD,WR,APND);
  IOBUFTYP = ARRAY [1..IOBUFSIZMAX] OF CHAR;
  MYFILTYP = RECORD
    FILE                      : File;
    IOBUF                     : POINTER TO IOBUFTYP;
    NXTPOSN,LASTPOSN,BYTESRED : CARDINAL;
    FILENAME                  : STRTYP;
    LineTermCH                : CHAR;
    iostate                   : IOSTATE;
  END(*RECORD*);


VAR
  C,K,IDX,PTR,c,RETCOD                         : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CRTotal,LFTotal                              : LONGCARD;
  CH,ch                                        : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG,stop     : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  I,J                                          : INTEGER;
  INUNT1,OUTUN1                                : MYFILTYP;
  inputline,OpenFileName,InName,OutName        : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;

PROCEDURE FileOpenForRead(VAR F:MYFILTYP; VAR FILNAM:BUFTYP);
(*
************************ FileOpenForRead **************************************

*)

VAR I,RETCOD : CARDINAL;
    EOFFLG   : BOOLEAN;
    fileError: CommonFileErrors;
    filelength : CARDINAL32;

BEGIN
  WITH F DO
(* ALLOCATE I/O BUFFER IN HEAP *)
(*    ALLOCATE(IOBUF,TSIZE(IOBUFTYP));*)
    NEW(IOBUF);
    iostate := RD;
    OpenFile(FILE,FILNAM.CHARS,ReadOnlyDenyWrite);
    IF FILE.status <> 0 THEN
      WriteString(' Error in opening/creating file ');
      WriteString(FILNAM.CHARS);
      WriteString('--');
      CASE TranslateFileError(FILE) OF
        FileErrFileNotFound : WriteString('File not found.');
      | FileErrDiskFull : WriteString('Disk Full');
      ELSE
        WriteString('Nonspecific error occured.');
      END(*CASE*);
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
      HALT;
    END(*IF*);
(*
  INITIALIZE I/O BUFFER AND ITS NXTPOSN.
*)
    IOBUF^[IOBUFSIZMAX] := CR;   (* Sentinal for text line searches *)
    FILENAME := FILNAM.CHARS;
    BYTESRED := 0;
    NXTPOSN  := 1;
    LASTPOSN := 1;
    F.LineTermCH := NULL;
  END(*WITH*);
END FileOpenForRead;


PROCEDURE FREAD(VAR INOUT F:MYFILTYP);
(*
****************************** FREAD *************************************
File READ.
This procedure will read the proper number of characters into the IOBUF.

*)
VAR CH : CHAR;
    C  : CARDINAL;

BEGIN
  WITH F DO
    IF FILE.eof THEN RETURN; END(*IF*);

    ReadBlock(FILE,ADR(IOBUF^[1]),IOBUFSIZ);
    BYTESRED := FILE.count;
    IF FILE.status <> 0 THEN
      WriteString(' Error in reading from file ');
      WriteString(FILENAME);
      WriteString('--');
      WriteString('Call error.  Either file was in Write mode, already closed,');
      WriteLn;
      WriteString('Nonspecific error.  Read not done.');
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
      HALT;
    END(*IF*);
(*
SUCCESSFUL READ.  CONTINUE.
  Will remove the End-Of-File MARKER if found.  Will no longer append it
  upon closure of the file by the FCLOSE PROCEDURE.  This test has
  been moved here so that the call to Again doesn't put it back if only one
  line delimiter is present.
    The variable BYTESRED will function also as a pointer to the
  end of the buffer, ie, the last character read.  LASTPOSN refers to the
  position of the last complete line.
*)
    IF LineTermCH = NULL THEN
    	C := BYTESRED;
    	REPEAT
    	  DEC(C);
    	UNTIL (C <= 0) OR (IOBUF^[C]=CR);
      IF C > 0 THEN
        LineTermCH := ASCII.cr;
      ELSE
        LineTermCH := ASCII.lf;
      END(*IF*);
    END(*IF*);
    NXTPOSN := 1;
    LASTPOSN := BYTESRED;
  END(*WITH*);
END FREAD;

PROCEDURE FCLOSE(VAR INOUT F : MYFILTYP);
(*
******************************* FCLOSE **************************************
*)

BEGIN
  WITH F DO
    CloseFile(FILE);
    DISPOSE(IOBUF);
  END(*WITH*);
END FCLOSE;

PROCEDURE PressAnyKey;

    (* "Press any key to continue". *)
    (* Bug: this is requiring "Enter" before the character can  *)
    (* be read.  I'm not yet sure how to solve this.            *)
    (* As a temporary work-around, we require the user to       *)
    (* press the <Enter> key rather than the <Any> key.         *)

    VAR dummy: CHAR;

BEGIN
  Terminal.WriteLn;
  Terminal.WriteString ("Press any key to continue");
  dummy := Terminal.ReadChar();
  Terminal.WriteLn;
END PressAnyKey;

PROCEDURE WriteCard (N: LONGCARD);
VAR s : STRTYP;
    BEGIN
      WholeStr.CardToStr(N,s);
      Terminal.WriteString(s);
(*        SWholeIO.WriteCard (N, 8); *)
    END WriteCard;


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
  FLAG := BasicDialogs.PromptOpenFile(InName,'',c,'','','Open input text file',FALSE);
  IF NOT FLAG THEN
    MiscM2.Error('Could not open file.  Does it exist?');
    HALT;
  END;
  ASSIGN2BUF(InName,INFNAM);
  FileOpenForRead(INUNT1,INFNAM);
  IF INUNT1.FILE.status <> 0 THEN
    WriteString(' Error in opening/creating file ');
    WriteString(InName);
    WriteString('--');
    CASE TranslateFileError(INUNT1.FILE) OF
      FileErrFileNotFound : WriteString('File not found.');
    | FileErrDiskFull : WriteString('Disk Full');
    ELSE
      WriteString('Nonspecific error occured.');
    END(*CASE*);
    WriteLn;
    WriteString(' Program Terminated.');
    WriteLn;
    HALT;
  END(*IF*);

  CRTotal := 0;
  LFTotal := 0;
  LOOP (* to read multiple lines *)
    FREAD(INUNT1);
    FOR C := 1 TO INUNT1.BYTESRED DO
      IF INUNT1.IOBUF^[C] = ASCII.cr THEN INC(CRTotal); END;
      IF INUNT1.IOBUF^[C] = ASCII.lf THEN INC(LFTotal); END;
    END;
    IF INUNT1.FILE.eof THEN EXIT END(*IF*);
  END(*LOOP*);
  FCLOSE(INUNT1);

  WriteLn;
  WriteString(' Number of CR is :');
  WriteCard(CRTotal);
  WriteString('.  Number of LF is :');
  WriteCard(LFTotal);
  WriteString('.');
  WriteLn;
  PressAnyKey;
END CountEOL.
