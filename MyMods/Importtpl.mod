  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;  
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts;
  FROM RealIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM TermFile IMPORT Open, IsTermFile, Close;


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

PROCEDURE Open(VAR cid : ChanId;
               flags : FlagSet;
               VAR res : OpenResults);
  (* Attempts to obtain and open a channel connected to the terminal.
     Without the raw flag, text is implied.
     Without the echo flag, line mode is requested, otherwise single
     character mode is requested.
     If successful, assigns to cid the identity of the opened channel, and
     assigns the value opened to res.
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel. *)

PROCEDURE IsTermFile(cid : ChanId) : BOOLEAN;
  (* Tests if the channel identified by cid is open to the terminal. *)

PROCEDURE Close(VAR cid : ChanId);
  (* If the channel identified by cid is not open to the terminal,
     the exception wrongDevice is raised; otherwise closes the channel
     and assigns the value identifying the invalid channel to cid. *)

  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETTKN,GETTKNSTR;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,DELIMSTATE,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
    GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
  FROM SysClock IMPORT DateTime;
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
    GetFileDateTimeUTC, SetFileDateTime, SetFileDateTimeUTC, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx, 
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList, 
    FindInOSPathList, SupportsUTC, ExpandFileSpec, FindFirst, FindNext, FindClose, 
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath, 
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
  FROM SYSTEM IMPORT REGISTERS,INT,ADDRESS,ADR,WORD;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ,CR,LF,MAXCARD,MAXINT,MININT,BUFTYP,ESC,
    NULL,BLANK,STRTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM ENVRTNS IMPORT OPNFIL;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRBL,FWRSTR,FWRLN,FCLOSEDUP,COPYDPIF,FAPPEND;
  FROM Str IMPORT Lows, Compare, Concat, Append, Copy, Slice, Pos, NextPos, CharPos, RCharPos, 
    Item,  ItemS, Prepend, Insert, Subst, Match, FindSubStr, IntToStr, CardToStr, RealToStr, 
    FixRealToStr, StrToInt, StrToCard, StrToReal, StrToC, StrToPas;
(* CONST Caps= Strings.Capitalize; Length= Strings.Length; Delete= Strings.Delete; *)
  FROM ExStrings IMPORT (* TYPE =Strings.CompareResults *) CompareI, EqualI, 
    FindNextI, FindPrevI, FindDiffI, AssignNullTerm, Lowercase, AnsiToUnicode,
    UnicodeToAnsi, Utf8Length, LengthUtf8, IsValidUtf8, UnicodeToUtf8, Utf8ToUnicode, 
    Utf8ToAnsi, FindAndReplace, FindAndReplaceI, AppendWithLengths, AppendChar, 
    AppendCharCond, AppendNum, AppendHex, GetNextItem, InList, AddItem, RemoveItem;
    
  FROM Strings IMPORT Assign, Extract, Delete, Insert, Replace, Append, Concat, 
    CanAssignAll, CanExtractAll, CanDeleteAll, CanInsertAll, CanReplaceAll, CanAppendAll,
    CanConcatAll, CompareResults (* TYPE = (less, equal, greater) *), Compare, 
    Equal, FindNext, FindPrev, FindDiff, Capitalize;
