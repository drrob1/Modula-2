<*DEFINE (ConsoleMode,TRUE)*>

MODULE TestFilePicker;

%IF ConsoleMode %THEN
    FROM FilePickerConsole IMPORT FileNamePicker;
    IMPORT MiscStdInOut, SIOResult;
    FROM MiscStdInOut IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, ReadString, SkipLine, ReadCard, ReadLongReal;
%ELSE
    FROM FilePicker IMPORT FileNamePicker;
    IMPORT Terminal,MiscM2;
    FROM MiscM2 IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, Read, ReadString, ReadCard, ReadLongReal;
%END

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
  CountOfEntries, counter                 : CARDINAL;
  ns                                      : NameString;
  ch                                      : CHAR;



(***************************** MAIN **********************************)
BEGIN
  FileNamePicker(ns);

<* IF ConsoleMode THEN *>
  WriteLn;
  WriteLn;
  WriteLn;
  WriteLn;
<* ELSE *>
  Terminal.Position(0,23);
<* END *>
  WriteString(' Picked File Name is ');
  WriteString(ns);
  WriteString('                      ');
  WriteLn;
  PressAnyKey;

END TestFilePicker.