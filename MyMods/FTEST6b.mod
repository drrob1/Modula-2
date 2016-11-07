MODULE FTEST6b;
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
   9 Aug 04 -- Here the FileFunc buffers are used.
  11 Feb 09 -- Will read one char at a time, and changed module name to FTEST6a
               And now will use peek, and module is FTEST6b
*)
  IMPORT Terminal,MiscM2;
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,Error;
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
  IMPORT Strings,MemUtils;
  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF(*,GETFNM*);
  FROM TOKENPTR IMPORT FSATYP,DELIMCH,INI3TKN,INI1TKN,GETTKN;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FOPEN,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;
  FROM Environment IMPORT GetCommandLine;


VAR
  C,K,IDX,PTR,c,RETCOD                         : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH,ans,ch                                    : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG,stop     : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  I,J                                          : INTEGER;
  INUNT1,OUTUN1                                : File;
  inputline,OpenFileName,InName,OutName        : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;



BEGIN

  Terminal.Reset;
  c := 0;
  stop := FALSE;
  FLAG := BasicDialogs.PromptOpenFile(InName,'',c,'','','Open input text file',FALSE);
  IF NOT FLAG THEN
    Error('Could not open file.  Does it exist?');
    HALT;
  END;
  BasicDialogs.MessageBox(InName,MsgInfo);

  FLAG := BasicDialogs.PromptOpenFile(OutName,'',c,'','','Open output text file',TRUE);
  IF NOT FLAG THEN
    Error('Could not open file.  ');
    HALT;
  END;
  BasicDialogs.MessageBox(OutName,MsgInfo);

  OpenFile(INUNT1,InName,ReadOnlyDenyWrite);
  IF INUNT1.status <> 0 THEN
    WriteString(' Error in opening/creating file ');
    WriteString(InName);
    WriteString('--');
    CASE TranslateFileError(INUNT1) OF
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
  SetFileBuffer(INUNT1,InBuf);

  OpenCreateFile(OUTUN1,OutName,ReadWriteDenyWrite);
  IF OUTUN1.status <> 0 THEN
    WriteString(' Error in opening/creating file ');
    WriteString(OutName);
    WriteString('--');
    CASE TranslateFileError(OUTUN1) OF
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
  SetFileBuffer(OUTUN1,OutBuf);


  K := 0;
  LOOP (* to read multiple times *)
(*    c32 := ReadLine(INUNT1,inputline); *)
    CH := PeekChar(INUNT1);
    IF INUNT1.eof THEN EXIT END(*IF*);
(*
    WriteString(inputline);
    WriteLn;
    WriteLine(OUTUN1,inputline);
*)
    Terminal.Write(CH);
    WriteChar(OUTUN1,CH);
    ch := ReadChar(INUNT1);  (* intentionally place this call here so file pointer is incremented but CH is not used. and eof cond tested from Peek without Read being called.  *)
(*
    IF ((K MOD 10) = 0) AND NOT stop THEN
      WriteString(' Press any key to continue.');
      Terminal.Read(ans);
(*      Terminal.Write(ans); *)
      IF (CAP(ans) = 'E') OR (CAP(ans) = 'Q') THEN EXIT; END(*IF*);
      IF (CAP(ans) = 'S') THEN stop := TRUE END;
(*      WriteLn; *)
    END (* if K MOD 10 *);
*)
    INC(K);
  END(*LOOP*);
  RemoveFileBuffer(INUNT1);
  CloseFile(INUNT1);

  RemoveFileBuffer(OUTUN1);
  CloseFile(OUTUN1);
  WriteLn;
  WriteString(' Number of reads is :');
  WriteCard(K);
  WriteLn;
  PressAnyKey;
END FTEST6b.
