MODULE FTEST8;
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
  24 Aug 04 -- Test i/o of reals for HP calculator pgm.
   9 Feb 09 -- Test ChopIntoLines
*)
  IMPORT SYSTEM;
  FROM SYSTEM IMPORT ADR;
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
  IMPORT BasicDialogs,DlgShell;
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
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM,ChopIntoLines;
  FROM Environment IMPORT GetCommandLine;

CONST
  szAppName = "FTest8";
  InputPrompt = 'Enter cmd or HELP : ';
  LastMod = '9 Feb 09';
  CitiIcon = '#100';
  MenuSep = '|';


VAR
  C,K,IDX,PTR,c,RETCOD                         : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG,stop     : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  I,J                                          : INTEGER;
  INUNT1,OUTUN1                                : MYFILTYP;
  inputline,OpenFileName,InName,OutName        : STRTYP;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;
    i,int       : INTEGER;
    cmdline     : ARRAY [0..255] OF CHAR;
    cmdbuf,tkn  : BUFTYP;
    tknstate    : FSATYP;
    retcod,c5   : CARDINAL;
    filter      : STRTYP;


PROCEDURE doit;

VAR 
	filter : STRTYP;
	c : CARDINAL;
	INFNAM : BUFTYP;
	FLAG,stop : BOOLEAN;
BEGIN 
  Terminal.Reset;
        INFNAM.CHARS := '';
        OUTFNAM.CHARS := '';

        filter := 'qfx Files';
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

  c := 0;

        DlgShell.ConvertToNulls(MenuSep,filter);
        FLAG := BasicDialogs.PromptOpenFile(INFNAM.CHARS,filter,c,'','','Open transaction file',FALSE);
  IF NOT FLAG THEN
    Error('Could not open file.  Does it exist?');
    HALT;
  END;
        TRIM(INFNAM);

  stop := FALSE;
  BasicDialogs.MessageBox(INFNAM.CHARS,MsgInfo);
  FOPEN(INUNT1,INFNAM,RD);
  K := 0;
  REPEAT
        ChopIntoLines(INUNT1,TMPBUF,EOFFLG);
        WriteString(TMPBUF.CHARS);
        WriteLn;
        PressAnyKey;
        INC(K);

  UNTIL EOFFLG OR (K>20);
  WriteString('Final pause');
  WriteLn;
  PressAnyKey;
  FCLOSE(INUNT1);
	
END doit;


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
  doit;
END FTEST8.
