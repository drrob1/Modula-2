(* Will test the FilePickerBase routines *)
MODULE TestFilePicker2;

  IMPORT Terminal,MiscM2;
  FROM Terminal IMPORT CursorUp,CursorDown,PageUp,PageDown,CursorLeft,CursorRight,Escape,Tab,BackSpace,Enter;
  FROM MiscM2 IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
    WriteReal, WriteLongReal, WriteChar, ReadChar, Read, ReadString, ReadCard, ReadLongReal;

  FROM FilePickerBase IMPORT CountOfEntries, SetFilenamePattern, GetNextFilename, GetPrevFilename;
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
        WriteLn;
  GetCommandLine(ns);
  len := LENGTH(ns);
  IF len = 0 THEN
    WriteString(' Enter filename pattern: ');
    ReadString(ns);
    WriteLn;
  END;

  SetFilenamePattern(ns);

  max := CountOfEntries;
  IF max > 15 THEN max := 15 END;
  IF max > 1 THEN
    FOR ctr := 1 TO max DO
        GetNextFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    END; (* for loop to get and display directory entries *)

    FOR ctr := 1 TO max DO
        GetPrevFilename(ns,DirEntry);
    END; (* for loop to get back to first directory entry *)
  END; (* for max not 0 or 1 *)
  WriteLn;
  WriteString(DirEntry);
  WriteLn;
  LOOP
    IF CountOfEntries = 0 THEN
      Error(' No valid filenames found ');
      EXIT;
    END;
    WriteString( '<enter> or <space> to select');
    ReadChar(ch);
    WriteLn;
    CASE ch OF
      CursorUp:
        GetPrevFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    | Enter :
        EXIT; (* ns and DirEntry are already set *)
    | CursorDown:
        GetNextFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    | PageUp:
        GetPrevFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    | PageDown:
        GetNextFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    | CursorLeft:
        GetPrevFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    | CursorRight:
        GetNextFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    | Tab:
        GetNextFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    | BackSpace:
        GetPrevFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    | ' ':
        EXIT; (* ns and DirEntry are already set *)
    ELSE
        (* ignore the character.  *)
    END; (* case ch *)

  END; (* loop to read and process a char *)
  WriteLn;
  WriteLn;
  WriteLn;
  WriteLn;
  WriteString(' Picked File Name is ');
  WriteString(ns);
  WriteLn;
  PressAnyKey;
END TestFilePicker2.
