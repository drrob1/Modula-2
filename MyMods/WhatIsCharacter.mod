<*DEFINE (ConsoleMode,FALSE)*>
(* Will test the FilePickerBase routines *)
MODULE WhatIsCharacter;

%IF ConsoleMode %THEN
    IMPORT MiscStdInOut, SIOResult;
    FROM MiscStdInOut IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, ReadString, SkipLine, ReadCard, ReadLongReal;
%ELSE
    IMPORT Terminal,MiscM2;
    FROM MiscM2 IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, Read, ReadString, ReadCard, ReadLongReal;
%END

(* Errors are when no valid filenames were found with the given namestring pattern.  CountOfEntries will be 0.
   PROCEDURE GetNextFilename(VAR OUT ns,DirectoryEntry: NameString);
   PROCEDURE GetPrevFilename(VAR OUT ns,DirectoryEntry: NameString);
*)
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
    WriteBlock,(* ReadChar, WriteChar,*) PeekChar, ReadLine, WriteLine, LockFileRegion,
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
  FROM Environment IMPORT GetCommandLine;


VAR
  ctr,len,max : CARDINAL;
  ns,DirEntry : NameString;
  ch          : CHAR;



(***************************** MAIN **********************************)
(*
   PROCEDURE GetNextFilename(VAR OUT ns,DirectoryEntry: NameString);
   PROCEDURE GetPrevFilename(VAR OUT ns,DirectoryEntry: NameString);
*)
BEGIN
  LOOP
    WriteLn;
    WriteString(' What Is Character? ');
    Terminal.Read(ch);
    WriteLn;
    IF CAP(ch)='Q' THEN EXIT END;
    WriteString(' Input character is: ');
    Terminal.Write(ch);
    WriteLn;
    WriteString(' Decimal value of input character is: ');
    WriteCard(ORD(ch));
    WriteLn;

<* IF ConsoleMode THEN *>
  WriteLn;
  WriteLn;
<* ELSE *>
  Terminal.Position(0,23);
<* END *>
  END;

  PressAnyKey;

END WhatIsCharacter.
