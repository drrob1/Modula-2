MODULE FTEST;
(*
  REVISION HISTORY
  ----------------
  5 Apr 88 -- Modified this file testing module for M2 V 3.03, and started
               using the new COPYDPIF Procedure in FIO.
  7 Apr 89 -- Using UL2 procs instead of the slower UTILLIB counterparts,
               and the updated procedure names (CONBUF renamed to
               ASSIGN2BUF).
  27 Dec 91 -- Converted to M2 V 4.00.
   2 Nov 02 -- Converted to M2 Win v4
   7 Mar 11 -- Modified to test MyFIO2.
*)
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts;
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
    FindInOSPathList, (* SupportsUTC,*) ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
  FROM RealIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM TermFile IMPORT Open, IsTermFile, Close;
  IMPORT Strings,FileFunc;

  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF(*,GETFNM*);
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI3TKN,INI1TKN,GETTKN;
  FROM MyFIO2 IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FOPEN,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;
  FROM Environment IMPORT GetCommandLine;

TYPE
    ChanId      = IOChan.ChanId;
    FlagSet     = ChanConsts.FlagSet;
    OpenResults = ChanConsts.OpenResults;

  (* Accepted singleton values of FlagSet *)

CONST
  read  = FlagSet{ChanConsts.readFlag}; (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag};(* output operations are requested/available *)
  text  = FlagSet{ChanConsts.textFlag}; (* text operations are requested/available *)
  raw   = FlagSet{ChanConsts.rawFlag};  (* raw operations are requested/available *)
  echo  = FlagSet{ChanConsts.echoFlag}; (* echoing by interactive device on reading of characters from input stream requested/applies *)

VAR
  C,K,IDX,PTR,c,RETCOD                         : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG          : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  I,J                                          : INTEGER;
  INUNT1,OUTUN1                                : MYFILTYP;
  inputline                                    : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;


PROCEDURE getcmdline;
VAR b : BUFTYP;
BEGIN
  GetCommandLine(inputline);
  c := LENGTH(inputline);

  IF c = 0 THEN
    ASSIGN2BUF('ENTER  INPUT FILE NAME : ',PROMPT);
    ASSIGN2BUF('FTEST.TXT',NAMDFT);
    ASSIGN2BUF('.TXT',TYPDFT);
    GETFNM(PROMPT,NAMDFT,TYPDFT,INFNAM);

    ASSIGN2BUF('ENTER OUTPUT FILE NAME : ',PROMPT);
    ASSIGN2BUF('WORDFILE.DOC',NAMDFT);
    ASSIGN2BUF('.DOC',TYPDFT);
    GETFNM(PROMPT, NAMDFT, TYPDFT, OUTFNAM);

  ELSE (* Command line not as general regarding presence of filename extensions. *)
    ASSIGN2BUF(inputline,INBUF);
    INI1TKN(INBUF);
    GETTKN(TOKEN,TKNSTATE,I,RETCOD);
    IF RETCOD > 0 THEN
      WriteString("GETTKN's RETCOD is ");
      WriteCard(RETCOD,0);
      WriteLn;
      HALT;
    ELSE
      b := TOKEN;
      ConstructFileName(b.CHARS,'.TXT',INFNAM.CHARS);
      TRIM(INFNAM);
    END;
    GETTKN(TOKEN,TKNSTATE,I,RETCOD);
    IF RETCOD = 0 THEN
      ConstructFileName(TOKEN.CHARS,'.DOC',OUTFNAM.CHARS);
      TRIM(OUTFNAM);
    ELSE
    	ConstructFileName(b.CHARS,'.DOC',OUTFNAM.CHARS);
    	TRIM(OUTFNAM);
(*
      WriteString("GETTKN's RETCOD is ");
      WriteCard(RETCOD,0);
      WriteLn;
*)
    END;
  END;
  WriteString('  FTEST INPUT FILE : ');
  WriteString(INFNAM.CHARS);
  WriteString(', INFNAM.COUNT = ');
  WriteCard(INFNAM.COUNT,0);
  WriteLn;
  WriteString(' OUTPUT FILE : ');
  WriteString(OUTFNAM.CHARS);
  WriteLn;
  WriteString('  -> Pause');
  ReadChar(CH);
  WriteLn;
  SkipLine;

END getcmdline;

BEGIN
	inputline := '';
  getcmdline;


  FOPEN(INUNT1,INFNAM,RD);
(*  COPYDPIF(INFNAM,OUTFNAM); *)

  FOPEN(OUTUN1,OUTFNAM,WR);
(*  FAPPEND(OUTUN1,OUTFNAM); *)



  LOOP (* to read multiple lines *)
    FRDTXLN(INUNT1,INBUF,0);
    IF INUNT1.FILE.eof THEN EXIT END(*IF*);
    WriteString(INBUF.CHARS);
    WriteLn;
    FWRTXLN(OUTUN1,INBUF);

    WriteString(' Press <enter> to continue.');
    ReadChar(CH);
    SkipLine;
    IF (CAP(CH) = 'E') OR (CAP(CH) = 'Q') THEN EXIT; END(*IF*);
    WriteLn;

  END(*LOOP*);
  FCLOSE(INUNT1);
(* needed for debugging.
  FPURGE(OUTUN1);
  c32 := GetFilePos(OUTUN1.FILE);
  d32 := FileLength(OUTUN1.FILE);
  GetFileSizes(OUTUN1.FILENAME,e32,f32);
  WriteString(' Filepos = ');
  WriteLongCard(c32,0);
  WriteString(', Filelen = ');
  WriteLongCard(d32,0);
  WriteString(' , FileSizes actual alloc =');
  WriteLongCard(e32,0);
  WriteLongCard(f32,0);
  WriteLn;
  SkipLine;
*)
  FCLOSE(OUTUN1);
END FTEST.
