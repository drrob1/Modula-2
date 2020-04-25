MODULE IPConverter;
(*
  REVISION HISTORY
  ----------------
   2 Oct 12 -- Initial version, to convert the straight cardinal contained in IpToCountry.csv to the dotted format
                much more comnonly used.  Used FTEST5a and AddCr3 as templates.
   6 Oct 12 -- Added percentage indicator.
  25 Apr 20 -- Decided to make some notes, as I just tried to us this program and it started crashing.  The format of IpToCountry.csv is that it has many
                 initial comment lines.  It's own documentation says that any line that begins w/ either a # or white space is a comment.  But no actual
                 lines begin w/ white space.  Sample lines are below from the last file I d/l'd, dated 9/5/13.
                 "16777216","16777471","apnic","1313020800","AU","AUS","Australia"
                 "417824768","417857535","arin","1044921600","US","USA","United States"
                 "1008664576","1008730111","apnic","1082419200","CN","CHN","China"
                 "3758096128","3758096383","apnic","1313020800","AU","AUS","Australia"
                 "4278190080","4294967295","iana","410227200","ZZ","ZZZ","Reserved"




*)
  IMPORT Terminal,MiscM2;
  FROM FilePicker IMPORT FileNamePicker;
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteLongCard,WriteInt,ReadString,ReadCard,
                         Error;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  IMPORT Conversions, LongStr, LongConv, WholeStr;
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
  (*
  IMPORT BasicDialogs;
  FROM BasicDialogs IMPORT MessageTypes;
  *)
  IMPORT Strings,MemUtils,ASCII;
  FROM Environment IMPORT GetCommandLine;
  IMPORT MyFIO2,ExStrings;
  FROM ExStrings IMPORT AppendNum, AppendChar;
  FROM Strings IMPORT Assign, Append;
  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENPTR IMPORT FSATYP,DELIMCH,INI3TKN,INI1TKN,GETTKN,TKNPTRTYP,GETTKNEOL;
  FROM MyFIO2 IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,IOSTATE,FRESET,FPURGE,FOPEN,FCLOSE,
        FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;


CONST
        K = 1024;
        dot = '.';
TYPE
        Str20 = ARRAY [0..20] OF CHAR;

VAR
  C,IDX,PTR,c,RETCOD,k,ipcard,ipercent,
  filelength,filealloc,filectr,fl              : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  percent,flr                                  : REAL;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,ignoreboolean,EOFFLG,stop   : BOOLEAN;
  I,J                                          : INTEGER;
  InFile,OutFile                               : MYFILTYP;
  string                                       : Str20;
  InName,OutName,OldInName                     : NameString;
  innameparts,outnameparts                     : FileNameParts;
  entry                                        : SearchEntry;
  inputline,OpenFileName                       : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..10*K] OF CHAR;
  fs                                           : LONGCARD;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  tpv                                          : TKNPTRTYP;
  INUNT1,OUTUN1                                : MYFILTYP;



PROCEDURE Card2DottedQuartet(card : CARDINAL; VAR str : ARRAY OF CHAR);
(*
**************************************************************************** Card2DottedQuartet *******
*)
VAR
        part1, part2, part3, part4, i, j, c : CARDINAL;
        outstr : Str20;

BEGIN
        str := '';
  IF card = 0 THEN
  	str := '0';
  	RETURN
  END;

  c := card;
  part1 := c MOD 256;
  c := c DIV 256;
  part2 := c MOD 256;
  c := c DIV 256;
  part3 := c MOD 256;
  c := c DIV 256;
  part4 := c;

  outstr := '';
  AppendNum(part4,outstr);
  AppendChar(dot,outstr);
  AppendNum(part3,outstr);
  AppendChar(dot,outstr);
  AppendNum(part2,outstr);
  AppendChar(dot,outstr);
  AppendNum(part1,outstr);

  Assign(outstr,str);



END Card2DottedQuartet;

(* ************************* MAIN ***************************************************************)
BEGIN
  Terminal.Reset;
  FileNamePicker(InName);
(*  InName := 'IpToCountry.csv'; *)
  IF LENGTH(InName) < 1 THEN
        WriteString(' InName is null.  Halting.');
        HALT;
  END;

  ParseFileName(InName, innameparts);
  outnameparts := innameparts;
  outnameparts.extension := '.dat';
  AssembleParts(outnameparts, OutName);
  IF FileExists(OutName) THEN DeleteFile(OutName) END;

  GetFileSizes(InName,filelength,filealloc);
  filectr := 0;
  ASSIGN2BUF(InName,INFNAM);
  FOPEN(INUNT1,INFNAM,RD);
  IF INUNT1.FILE.status <> 0 THEN
    WriteString(' Error in opening/creating file ');
    WriteString(INFNAM.CHARS);
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
  fl := FileLength(INUNT1.FILE);
  flr := FLOAT(fl);

  ASSIGN2BUF(OutName,OUTFNAM);
  FOPEN(OUTUN1,OUTFNAM,WR);
  IF OUTUN1.FILE.status <> 0 THEN
    WriteString(' Error in opening/creating file ');
    WriteString(OUTFNAM.CHARS);
    WriteString('--');
    CASE TranslateFileError(OUTUN1.FILE) OF
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

  Terminal.Reset;
  WriteString(' File length, allocation and FL = ');
  WriteCard(filelength);
  WriteString(', ');
  WriteCard(filealloc);
  WriteString(', ');
  WriteCard(fl);
  WriteString('.');
  WriteLn;

  k := 0;
  LOOP (* to read lines *)
    FRDTXLN(INUNT1,INBUF,0);
    IF INUNT1.FILE.eof THEN EXIT END(*IF*);
    INC(k);
    INC(filectr,INBUF.COUNT);
    IF ((k MOD 5000) = 0) OR (k = 1000) THEN
      percent := FLOAT(filectr) / flr * 100.;
      ipercent := TRUNC(percent + 0.5);
      Terminal.Position(3,1);
      WriteCard(ipercent);
      WriteString(' %                    ');
    END; (* output only every 1000 lines *)
    INI1TKN(tpv,INBUF);
    IF INBUF.CHARS[1] <> '#' THEN
(* first token, the starting IP address field *)
      GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);
      ignoreboolean := Conversions.StrToCard(TOKEN.CHARS,ipcard);
      Card2DottedQuartet(ipcard,string);
      FWRSTR(OUTUN1,'"');
      FWRSTR(OUTUN1,string);
      FWRSTR(OUTUN1,'",');
(* 2nd token, the ending IP address field *)
      GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);
      ignoreboolean := Conversions.StrToCard(TOKEN.CHARS,ipcard);
      Card2DottedQuartet(ipcard,string);
      FWRSTR(OUTUN1,'"');
      FWRSTR(OUTUN1,string);
      FWRSTR(OUTUN1,'"');
    END; (* If line does not begin w/ comment character # *)
(* rest of the line, including comments and country codes *)
      GETTKNEOL(tpv,TOKEN,RETCOD);
      FWRSTR(OUTUN1,TOKEN.CHARS);
      FWRLN(OUTUN1);

    DISPOSE(tpv);
  END; (* read loop *)
  percent := FLOAT(filectr) / flr * 100.;
  ipercent := TRUNC(percent + 0.5);
  Terminal.Position(3,1);
  WriteCard(ipercent);
  WriteString(' %                    ');
  WriteLn;
  WriteString(' Number of bytes output is :');
  WriteCard(filectr);
  WriteString('.  Number of line output is :');
  WriteCard(k);
  WriteLn;

  FCLOSE(INUNT1);
  FCLOSE(OUTUN1);

  PressAnyKey;

(*
  fopen(InFile,InName,RD);
  SetFileBuffer(InFile, InBuf);

  fopen(OutFile,OutName,WR);
  SetFileBuffer(OutFile,OutBuf);

  RdWrAllTx;

  fclose(InFile);
  fclose(OutFile);
*)
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
  innameparts.extension := '.old';
  AssembleParts(innameparts,OldInName);
  ignoreboolean := DeleteFile(OldInName);
  ignoreboolean := RenameFile(InName,OldInName);
  ignoreboolean := RenameFile(OutName,InName);
*)
(*
  PressAnyKey;
*)
(*
    WriteString(' Enter card to convert : ');
    ReadCard(C);
    WriteLn;

    IF C=0 THEN
        WriteString(' Input cardinal is zero.  Exiting read loop. ');
        EXIT;
    END; /* if c=0 */
*)
(*
    WriteString(' Converted string is : ');
    WriteString(string);
    WriteLn;
*)

END IPConverter.
(*
FileFunc
  FileSpecString  = ARRAY [0..260] OF CHAR;
  NameString      = ARRAY [0..255] OF CHAR;

  PROCEDURE RenameFile(fromFile, toFile : ARRAY OF CHAR) : BOOLEAN;
  PROCEDURE DeleteFile(name : ARRAY OF CHAR) : BOOLEAN;
  PROCEDURE FileExists(name : ARRAY OF CHAR) : BOOLEAN;
  PROCEDURE CopyFile(source, dest : ARRAY OF CHAR) : BOOLEAN;

Strings
  PROCEDURE Assign(source : ARRAY OF CHAR; VAR OUT destination : ARRAY OF CHAR);
  PROCEDURE Append(source : ARRAY OF CHAR; VAR INOUT destination : ARRAY OF CHAR);

ExStrings
  PROCEDURE AppendChar(ch : CHAR; VAR INOUT str : ARRAY OF CHAR);
  PROCEDURE AppendNum(num : CARDINAL; VAR INOUT str : ARRAY OF CHAR);

Conversions
  PROCEDURE CardToString(num : CARDINAL;
                         size : CARDINAL;
                         VAR OUT str : ARRAY OF CHAR;
                         VAR INOUT pos : CARDINAL;
                         VAR OUT done : BOOLEAN);
  PROCEDURE CardToStr(num : CARDINAL; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
  PROCEDURE LongToString(num : LONGINT;
                         size : CARDINAL;
                         VAR OUT str : ARRAY OF CHAR;
                         VAR INOUT pos : CARDINAL;
                         VAR OUT done : BOOLEAN);
  PROCEDURE LongToStr(num : LONGINT; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
  PROCEDURE StringToCard(str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT num : CARDINAL;
                       VAR OUT done : BOOLEAN);
  PROCEDURE StrToCard(buf : ARRAY OF CHAR; VAR OUT num : CARDINAL) : BOOLEAN;

WholeStr
  PROCEDURE CardToStr(card : CARDINAL; VAR str : ARRAY OF CHAR);

LWholeStr
  PROCEDURE LongIntToStr(int : LONGINT; VAR str : ARRAY OF CHAR);
  PROCEDURE LongCardToStr(card : LONGCARD; VAR str : ARRAY OF CHAR);

FuleFunc
  PROCEDURE FileLength(VAR INOUT f : File) : CARDINAL32;
  PROCEDURE GetFileSizes(name : ARRAY OF CHAR; VAR actual, alloc : CARDINAL32);



*)
