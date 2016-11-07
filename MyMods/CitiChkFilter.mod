MODULE CitiChkFilter;
(*
  REVISION HISTORY
  ----------------
  13 Mar 04 -- It does not seem to be always creating the output file.  
               It now copies the d/l file instead of renaming it.  And
               it echoes its output to the terminal.
*)
  IMPORT MiscM2;
  FROM MiscM2 IMPORT CLS,WriteString,WriteLn,WriteCard,ReadCard,ReadString,
                     PressAnyKey,Error;
(*
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  IMPORT IOChan, ChanConsts;
*)
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
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
    FindInOSPathList, ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
  FROM RealIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM TermFile IMPORT Open, IsTermFile, Close;

  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF(*,GETFNM*);
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,DELIMSTATE,INI3TKN,INI1TKN,GETTKN,GETTKNSTR,GETTKNEOL;
  FROM TIMLIB IMPORT GETMDY, JULIAN;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;
  FROM Environment IMPORT GetCommandLine;

(*
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
*)

VAR
  C,K,c,RETCOD,m,d,y,chknum                    : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG,OK,ok,ZeroFlag    : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  I,J                                          : INTEGER;
  INUNT1,OUTUN1                                : MYFILTYP;
  inputline                                    : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;
  juldate1,juldate2,juldate3                   : LONGINT;


PROCEDURE getcmdline;
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

  ELSE
    ASSIGN2BUF(inputline,INBUF);
    INI1TKN(INBUF);
    GETTKN(TOKEN,TKNSTATE,I,RETCOD);
    IF RETCOD > 0 THEN
      WriteString("GETTKN's RETCOD is ");
      WriteCard(RETCOD);
      WriteLn;
      HALT;
    ELSE
      INFNAM := TOKEN;
      ConstructFileName(INFNAM.CHARS,'.TXT',INFNAM.CHARS);
      TRIM(INFNAM);
    END;
    GETTKN(TOKEN,TKNSTATE,I,RETCOD);
    IF RETCOD = 0 THEN
      OUTFNAM := TOKEN;
      ConstructFileName(OUTFNAM.CHARS,'.DOC',OUTFNAM.CHARS);
      TRIM(OUTFNAM);
    ELSE
      WriteString("GETTKN's RETCOD is ");
      WriteCard(RETCOD);
      WriteLn;
      HALT;
    END;
  END;
  WriteString('  FTEST INPUT FILE : ');
  WriteString(INFNAM.CHARS);
  WriteString(', INFNAM.COUNT = ');
  WriteCard(INFNAM.COUNT);
  WriteLn;
  WriteString(' OUTPUT FILE : ');
  WriteString(OUTFNAM.CHARS);
  WriteLn;
  PressAnyKey;
(*
  WriteString('  -> Pause');
  ReadChar(CH);
  SkipLine;
*)
  WriteLn;

END getcmdline;

BEGIN
(*
  getcmdline;
  ASSIGN2BUF('ENTER  INPUT FILE NAME : ',PROMPT);
  ASSIGN2BUF('CHK_861.CSV',NAMDFT);
  ASSIGN2BUF('.TXT',TYPDFT);
  GETFNM(PROMPT,NAMDFT,TYPDFT,INFNAM);
*)
  ZeroFlag := TRUE; (* decided to stop using *)
  juldate1 := 0;
  juldate2 := 0;
  juldate3 := 0;
  chknum := 1;
  ok := DeleteFile('CHK861.ASC');
  ok := DeleteFile('CHK.ASC');
  ok := CopyFile('CHK_861.CSV','CHK861.ASC');
  IF NOT ok THEN
    Error(' Could not copy CHK_861.CSV file.  Does it exist?');
    HALT;
  END(*if*);
  ASSIGN2BUF('CHK861.ASC',INFNAM);
  ASSIGN2BUF('CHK.ASC',OUTFNAM);


  FRESET(INUNT1,INFNAM,RD);
  FRESET(OUTUN1,OUTFNAM,WR);
(*  FAPPEND(OUTUN1,OUTFNAM); *)



  LOOP (* to read multiple lines *)
    FRDTXLN(INUNT1,INBUF,250,EOFFLG);
    IF EOFFLG THEN EXIT END(*IF*);
(*
    WriteString(INBUF.CHARS);
    WriteLn;
    PressAnyKey;
*)
    INI1TKN(INBUF);
    FOR J := 1 TO 2 DO (* just write first 2 tokens as we need to process the 3rd *)
      GETTKN(TOKEN,TKNSTATE,I,RETCOD);
      IF RETCOD > 0 THEN EXIT END;
      FWRSTR(OUTUN1,'"');
      FWRTX(OUTUN1,TOKEN);
      FWRSTR(OUTUN1,'",');
      WriteString(TOKEN.CHARS);
      WriteString(',');
      
    END(*for*);
(* TOKEN is a date string and can be processed as such *)
    ok := StrToCard(TOKEN.CHARS[1..2],m);
    ok := StrToCard(TOKEN.CHARS[4..5],d);
    ok := StrToCard(TOKEN.CHARS[7..10],y);
    IF juldate1 = 0 THEN
      juldate1 := JULIAN(m,d,y);
    ELSE
      juldate2 := JULIAN(m,d,y);
    END(*IF*);


    GETTKN(TOKEN,TKNSTATE,I,RETCOD);  (* I is ascii sum of char's in a string token *)
    ok := StrToInt(TOKEN.CHARS,I);          (* Now I is integer of transaction number *)
    IF NOT ok THEN
      WriteString(' Error from StrToInt for transaction number.');
      WriteLn;
      PressAnyKey;
(*
      WriteString(' Press any key to continue.');
      ReadChar(CH);
      SkipLine;
*)
      EXIT;
    END(*if*);
    IF (I = 0) AND (juldate1 = juldate2) THEN
        ok := CardToStr(chknum,TOKEN.CHARS);
        TRIM(TOKEN);
        INC(chknum);
    ELSIF (juldate2 > 0) AND (juldate1 <> juldate2) THEN
      juldate1 := juldate2;
      juldate2 := 0;
    END(*if*);
    FWRSTR(OUTUN1,'"');
    FWRTX(OUTUN1,TOKEN);
    FWRSTR(OUTUN1,'"');
    WriteString(TOKEN.CHARS);
    GETTKNEOL(TOKEN,RETCOD);
    FWRTXLN(OUTUN1,TOKEN);
    WriteString(TOKEN.CHARS);
    WriteLn;
    
(*
    WriteString(' Press any key to continue.');
    ReadChar(CH);
    SkipLine;
    IF (CAP(CH) = 'E') OR (CAP(CH) = 'Q') THEN EXIT; END(*IF*);
    WriteLn;
*)
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
  WriteString(' CHK file now closed.');
  PressAnyKey;
END CitiChkFilter.
