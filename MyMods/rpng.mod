(* (C) 1990-2013.  Robert W. Solomon.  All rights reserved.  *)
MODULE rpng;
(*
  This module uses the HPCALC module to simulate an RPN type calculator.
  REVISION HISTORY
  ----------------
   1 Dec 89 -- Changed prompt.
  24 Dec 91 -- Converted to M-2 V 4.00.  Changed params to GETRESULT.
  25 Jul 93 -- Output result without trailing insignificant zeros,
                imported UL2, and changed prompt again.
   3 Mar 96 -- Fixed bug in string display if real2str fails because
                number is too large (ie, Avogadro's Number).
  18 May 03 -- First Win32 version.  And changed name.
   1 Apr 13 -- Back to console mode pgm that will read from the cmdline.  Intended to be a quick and useful little utility.
                And will save/restore the stack to/from a file.
   2 May 13 -- Will use console mode flag for HPCALC, so it will write to console instead of the terminal module routines.
                And I now have the skipline included in MiscStdInOut so it is removed from here.
  15 Oct 13 -- Now writing for gm2 under linux.
*)
  FROM SYSTEM IMPORT ADR, ADDRESS;
  FROM MiscStdInOutg IMPORT StringToLongReal, LongRealToString, LongRealToStr, WriteLongReal, WriteReal,
    StringToInt, IntToString, StringToCard, CardToString,
    WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,Error;
  FROM Environg IMPORT GetCommandLine;
  FROM UTILLIBg IMPORT NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc;
  FROM TKNRTNSg IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM REALLIBg IMPORT AINT,ROUND,AMOD,PWRI,GETCROPNUM;
  FROM TIMLIBg IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM HPCALCg IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;
  IMPORT HPCALCg, MiscStdInOutg;
  IMPORT Strings,ASCII;
  FROM Strings IMPORT Append, Equal, Delete, Concat, Capitalize;
(*
  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts, FileTypes, DeviceTypes,
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
*)
  FROM FIO IMPORT File, OpenToRead, OpenToWrite, Close, EOF, EOLN, IsNoError, Exists, 
    SetPositionFromBeginning, SetPositionFromEnd, ReadChar,  WriteChar,  WriteLine, UnReadChar,
    ReadCardinal, WriteCardinal, ReadNBytes, WriteNBytes, FlushBuffer, GetFileName, StdIn, StdOut, StdErr
    (* , ReadString, WriteString *);


VAR
  C,c,K,STRLEN,NON0POSN,NONBLPOSN,RetCode : CARDINAL;
  R                                       : LONGREAL;
  INBUF                                   : BUFTYP;
  OKAY, StackFileExists                   : BOOLEAN;
  stk                                     : ARRAY [1..STACKSIZE] OF LONGREAL;
  StackFile                               : File;
  STR1,STR2,STR3,
  inputline,HPFileName,StackFileName,Xstr : STRTYP;


(*********************************************************************)

(*********************************************************************)

BEGIN (********************* MAIN ****************************************)
  StackFileName := 'RPNStack.sav';
  StackFileExists := Exists(StackFileName);
  IF StackFileExists THEN
    StackFile := OpenToRead(StackFileName);
    
    IF IsNoError(StackFile) THEN
      K := ReadNBytes(StackFile,SIZE(stk),ADR(stk));
      Close(StackFile);
      FOR c := STACKSIZE TO 1 BY -1 DO
        PUSHX(stk[c]);
      END; (* for *)
    END; (* if opened successfully *)
  END; (* if stackfileexists *)
  GetCommandLine(INBUF.CHARS);
  TRIM(INBUF);

  IF INBUF.COUNT < 1 THEN
    WriteString(' Enter calculation, HELP or Enter to exit: ');
    ReadString(INBUF.CHARS);
    TRIM(INBUF);
  END; (* if there is no command tail *)
  REPEAT (* Until finished with input *)
    R := GETRESULT(INBUF);
    WriteLn;
    WriteLn;
    WriteString(' Result = ');
    WriteLongReal(R);
    GETCROPNUM(R,Xstr);
    WriteString(',       ');
    WriteString(Xstr);
    WriteLn;
    WriteLn;
    WriteLn;
    WriteString(' Enter calculation, HELP or Enter to exit: ');
    ReadString(INBUF.CHARS);
    WriteLn;
    TRIM(INBUF);
  UNTIL INBUF.COUNT = 0;
  GETSTACK(stk,RetCode);
  StackFile :=   OpenToWrite(StackFileName);
  IF IsNoError(StackFile) THEN
    K := WriteNBytes(StackFile, SIZE(stk), ADR(stk));
    Close(StackFile);
  END; (* if no error for writing stack to file *)
END rpng.
