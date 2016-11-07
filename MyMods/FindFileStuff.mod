(*
    SearchEntry = RECORD
            /* returned information for you */
            size        : CARDINAL32;
            attribute   : FileAttributeSet;
            dt          : DateTime;/*this is local time */
            %IF WIN32 %OR UNIX %THEN
            dtUTC       : DateTime;/*this is UTC time */
            %END
            name        : NameString;
            /* fields used for internal implementation */
            mayHave     : FileAttributeSet;
            mustHave    : FileAttributeSet;
        %IF PROTECT %THEN
            critic      : CriticalSection;
        %END
        %IF WIN32 %THEN
            findHandle  : ADDRESS;
            reserved0   : CARDINAL;
            reserved1   : CARDINAL;
        %ELSIF UNIX %THEN
            findHandle  : ADDRESS;
            pattern     : FileSpecString;
        %ELSIF FlashTek %OR DOS %OR WIN16 %THEN
            reserved    : ARRAY [0..20] OF CHAR;
        %ELSIF OS2 %THEN
            findHandle  : CARDINAL;
        %END
  END; /* SearchEntry record */
*)
(*
  the following record is used to contain the various components of a file specification
  The sizes of individual components are defined by the underlying file system for the device accessed.
  The sizes declared here are at least as big as necessary, but most likely larger
    FileNameParts =
        RECORD
          %IF DOS %OR FlashTek %OR PharLap %OR WIN16 %THEN
            drive       : ARRAY [0..127] OF CHAR;
          %ELSIF UNIX %THEN
            drive       : ARRAY [0..0] OF CHAR;/*N/A*/
          %ELSE
            drive       : NameString;
          %END
          path        : FileSpecString;
          name        : NameString;
          extension   : NameString;
        END;

   .drive only has meaning on Miscorosft platforms
       this is the device, it is either a logical drive letter or
       a UNC server name and share.
   .path = this is the path
   .name = the file name  excluding the file extension
   .extension = the file extension, this is everything  after the *last*
       '.' character. On Unix systems this may actually be the full file
       name since the convension there is to use a preceeding '.' to
       mark hidden files and/or directories.


    FileTypes           = (FileTypeUnknown,FileTypeDisk,FileTypeChar,FileTypePipe);
*)

(*
PROCEDURE FindFirst(path : ARRAY OF CHAR; mayHave : FileAttributeSet; mustHave : FileAttributeSet; VAR OUT entry : SearchEntry) : BOOLEAN;

 path specifies a device and directory where to search
 wildcards can be and generally are used in path.
 How wildcards are specified is filesystem dependent.
 Generally the '*' character matches any number of characters and a
 '?' character matches any single character.
 if this procedure succeeds(returns TRUE), you must call FindClose when
 you are done searching.
 Example of use
   This searches for all files, excluding file directories.
    IF FindFirst("*", StdAttributes, MustHaveNormalFile, entry) THEN
        REPEAT
            ...
        UNTIL NOT FindNext(entry);
        FindClose(entry);
    END;

  mayHave - returned files will only have attributes within this set, however they need not have all of the attributes.
  mustHave - all returned files will have ALL the specified Attribute(s)

  FindFirst combines mustHave into mayHave as a convenience.

  NormalFile is assumed for mayHave if Directory is not present.
*)
(*
PROCEDURE FindNext(VAR INOUT entry : SearchEntry) : BOOLEAN;
PROCEDURE FindClose(VAR INOUT entry : SearchEntry);

I need to create either a linked list of SearchEntry, sizes, etc, or a table of SearchEntry.
Since there are no linkedlist procedures already in the library, I'll use a table format.
Use readchar to read 1 char at a time to form the search pattern.  When <tab> is hit, then
append '*' to the pattern and use this for the search loop to build the table.  Then output
the table, possibly using a BasicDialogs procedure.  If I can, I'll leave the table up while
continued <tab> hits will cycle thru the table.
*)

MODULE FindFileStuff;
  IMPORT Terminal,MiscM2;
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteLongCard,WriteInt,ReadString,ReadCard,
                      CLS,Error;
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



CONST tablesize=100;
      pointertablesize=1000;
TYPE
  SearchEntryTableType = ARRAY [1..tablesize] OF SearchEntry;
  SearchEntryPtr = POINTER TO SearchEntry;
  SearchEntryTablePointerType = ARRAY [1..tablesize] OF SearchEntryPtr;

VAR
  SearchEntryTable,SearchEntryTableSorted : SearchEntryTableType;
  entry                                   : SearchEntry;
  entryptr                                : SearchEntryPtr;
  CountOfEntries, counter                 : CARDINAL;
  ns                                      : NameString;
  ch                                      : CHAR;

BEGIN
REPEAT
  CLS;
  WriteString(' Enter filename pattern: ');
  ReadString(ns);
  WriteLn;

  IF FindFirst(ns, StdAttributes, MustHaveNormalFile, entry) THEN
      CountOfEntries :=   0;
      REPEAT
        INC(CountOfEntries);
        SearchEntryTable[CountOfEntries] := entry;
      UNTIL NOT FindNext(entry) OR (CountOfEntries >= tablesize);
      FindClose(entry);
      SearchEntryTableSorted := SearchEntryTable;
  ELSE
        Error(' No valid file names found. ');
        HALT;
  END;

  counter := 1;
  REPEAT
    WriteString(SearchEntryTable[counter].name);
    WriteString('  ');
    WriteCard(SearchEntryTable[counter].size);
    WriteString('  ');
    WriteCard(SearchEntryTable[counter].dt.month);
    Terminal.Write('/');
    WriteCard(SearchEntryTable[counter].dt.day);
    Terminal.Write('/');
    WriteCard(SearchEntryTable[counter].dt.year);
    WriteString('  ');
    WriteCard(SearchEntryTable[counter].dt.hour);
    Terminal.Write(':');
    WriteCard(SearchEntryTable[counter].dt.minute);
    Terminal.Write(':');
    WriteCard(SearchEntryTable[counter].dt.second);
    Terminal.Write('.');
    WriteCard(SearchEntryTable[counter].dt.fractions);
    WriteLn;
    INC(counter);
    IF counter MOD 20 = 0 THEN PressAnyKey; END;
  UNTIL counter > CountOfEntries;
  ch  := BasicDialogs.YesNo(' Again? ','n');
UNTIL CAP(ch)='N';

END FindFileStuff.mod

