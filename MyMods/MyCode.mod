MODULE MyCode;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
(*  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETTKN,GETTKNSTR; *)
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,DELIMSTATE,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR;
(*  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
    GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;  *)
  FROM SysClock IMPORT DateTime;
(*  Not Yet!
  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts (*drive path name extension*), FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle,
    MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary,
    AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName,
    CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock,
    WriteBlock, (*ReadChar, WriteChar,*) PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength,
    GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList,
    FindInOSPathList, SupportsUTC, ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
*)
  FROM SYSTEM IMPORT REGISTERS,ADDRESS,ADR,WORD;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ,CR,LF,MAXCARD,MAXINT,MININT,BUFTYP,ESC,
    NULL,BLANK,STRTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(* Not Yet!
  FROM ENVRTNS IMPORT OPNFIL;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRBL,FWRSTR,FWRLN,FCLOSEDUP,COPYDPIF,FAPPEND;
 *)
  FROM Str IMPORT Lows, Compare, Concat, Append, Copy, Slice, Pos, NextPos, CharPos, RCharPos,
    Item,  ItemS, Prepend, Insert, Subst, Match, FindSubStr, IntToStr, CardToStr, RealToStr,
    FixRealToStr, StrToInt, StrToCard, StrToReal, StrToC, StrToPas;
(* CONST Caps= Strings.Capitalize; Length= Strings.Length; Delete= Strings.Delete; *)
  FROM ExStrings IMPORT (* TYPE =Strings.CompareResults *) CompareI, EqualI,
    FindNextI, FindPrevI, FindDiffI, AssignNullTerm, Lowercase, AnsiToUnicode,
    UnicodeToAnsi, Utf8Length, LengthUtf8, IsValidUtf8, UnicodeToUtf8, Utf8ToUnicode,
    Utf8ToAnsi, FindAndReplace, FindAndReplaceI, AppendWithLengths, AppendChar,
    AppendCharCond, AppendNum, AppendHex, GetNextItem, InList, AddItem, RemoveItem;

  FROM Strings IMPORT Assign, Extract, Delete, (*Insert,*) Replace, (*Append, Concat, *)
    CanAssignAll, CanExtractAll, CanDeleteAll, CanInsertAll, CanReplaceAll, CanAppendAll,
    CanConcatAll, CompareResults (* TYPE = (less, equal, greater) *), (*Compare, *)
    Equal, (*FindNext, FindPrev, FindDiff,*) Capitalize;

  VAR
    b1, b2, b3, b4, b5, b6   : BUFTYP;
    c1, c2, c3, c4, c5, c6   : CARDINAL;
    i1, i2, i3, i4, i5, i6   : INTEGER;
    s1, s2, s3, s4, s5, s6   : STRTYP;
    ok1, ok2, ok3, ok4       : BOOLEAN;


BEGIN
LOOP

WriteString("Test My Code Module");  WriteLn;
WriteString("Enter string1: ");
ReadString(s1);
SkipLine;
IF STRCMPFNT(s1, "stop") =0 THEN EXIT END;
ASSIGN2BUF(s1,b1);
WriteString("Enter string2: ");
ReadString(s2);
SkipLine;
ASSIGN2BUF(s2,b2);
WriteString(" b1.count, b2.count: ");
WriteCard(b1.COUNT,0);
WriteString(", ");
WriteCard(b2.COUNT,0);
WriteLn;
COPYLEFT(ADR(b1.CHARS), ADR(s5), b1.COUNT);
COPYRIGHT(ADR(b2.CHARS), ADR(s6), b2.COUNT);
WriteString(" s5, s6: ");
WriteString(s5);
WriteString(",  ");
WriteString(s6);
WriteLn;

END (*Loop*)
END MyCode.
