(*
  REVISION HISTORY
  ----------------
   4-Sep-11  First version.
   5-Sep-11  Added sort routine.
  18-Sep-11  Added fractional sec (which are really msec) to the sorted field.
   5-Oct-11  I think the sort function is failing if there is only 1 in the list.  So I'm only sorting a list > 5.
              On 2nd thought, I'll use a different algorithm for lists that are 2-5 long.
  25-Oct-11  Changed defaults so that it will by default only show 20 filenames.  And 2nd phase will start with a <tab>.
  18-Dec-11  Changed 1st prompt to accept more options, eliminated 2nd prompt, and added an <ESC> option.
  19-Dec-11  Changing display and select process so that the list remains on screen and default selection starts with
              1st displayed name.
   8-Jan-12  Allow a namestring to be in the command tail.  And all namestrings are appended w/ '*'
  19-Apr-13  Decided to only append '*' if the namestring does not already have '*'.  This allows me to specify an extension.
              And changed the m or p prompt to include a reference to the arrow keys.
  26-Nov-13  New name to not have the I/O as part of this module so it can be used in a GUI more easily.
              That is, it is only the base functionality.
*)
IMPLEMENTATION MODULE FilePickerBase;
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
  IMPORT BasicDialogs;
  FROM BasicDialogs IMPORT MessageTypes;
  IMPORT Strings,MemUtils,ASCII,ExStrings;
  FROM TIMLIB IMPORT GREG2JUL,JULIAN;
  FROM UTILLIB IMPORT SCANBACK;
  FROM SYSTEM IMPORT ADR;

CONST ArraySize=100;
      PointerArraySize=1000;
      DisplayFactor=15;
      WildCardChar = '*';

TYPE
(* When added sort rtn needed new types which include a julian field to sort.
  SearchEntryTableType = ARRAY [1..tablesize] OF SearchEntry;
  SearchEntryPtr = POINTER TO SearchEntry;
  SearchEntryTablePointerType = ARRAY [1..tablesize] OF SearchEntryPtr;
*)
  ExpandedEntryType = RECORD
                        ent    : SearchEntry;
                        julian : LONGREAL;
                      END;
  ExpandedEntryPointerType = POINTER TO ExpandedEntryType;
  ExpandedEntryPointerArrayType = ARRAY [1..PointerArraySize] OF ExpandedEntryPointerType;
(*        NameString      = ARRAY [0..255] OF CHAR;  Defined in FileFunc Module *)

VAR
  SearchEntryTable,SearchEntryTableHold : ExpandedEntryPointerArrayType;
  entry                                 : SearchEntry;
  entryptr                              : ExpandedEntryPointerType;
  counter,len                           : CARDINAL;
  s                                     : NameString;
  ch                                    : CHAR;
  ErrorFlag,ValidFilenamePatternSet     : BOOLEAN;
  m,d,y,hr,min,sec,frac,secinday        : CARDINAL;
  juldate,fsecinday                     : LONGREAL;
  jd                                    : LONGINT;
(*
  CountOfEntries : CARDINAL  is defined in the def module
*)


(************************************************************************************************)
(************************************************************************************************)
PROCEDURE revheapsort(VAR INOUT RA : ExpandedEntryPointerArrayType; Left,Right: CARDINAL);
VAR
  L,R,I,J : CARDINAL;
  RRA : ExpandedEntryPointerType;

BEGIN
  IF (Right-Left) < 2 THEN RETURN END;
  L := Right/2 + 1;
  R := Right;
(*
  The index L will be decremented from its initial value down to 'Left' during the heap
  creation phase.  Once it reaches Left, the index R will be decremented from its initial
  value down to 'Left' during the heap selection phase.
*)
  LOOP
   IF L > Left THEN (*  IF(L.GT.1)THEN in original Fortran code *)
     DEC(L);
     RRA := RA[L];
   ELSE
     RRA := RA[R];
     RA[R] := RA[Left]; (*  RA(IR)=RA(1) in original Fortran code *)
     DEC(R);
     IF R = Left THEN  (*   IF(IR.EQ.1)THEN in original Fortran code *)
       RA[Left] := RRA; (*   RA(1)=RRA in original Fortran code *)
       RETURN;
     END (* IF R = Left, or IR.EQ.1 in the original Fortran code *)
   END; (* IF L > Left, or L.GT.1 in original Fortran code *)
   I := L;
   J := L + L;
   WHILE J <= R DO  (* DO WHILE J.LE.IR is intent. Code was 20 IF(J.LE.IR)THEN *)
     IF J < R THEN  (*   IF(J.LT.IR) THEN *)
       IF (RA[J]^.julian > RA[J+1]^.julian) THEN INC(J); END; (* IF(RA(J).LT.RA(J+1)) J=J+1 *)
     END;
     IF (RRA^.julian > RA[J]^.julian) THEN (* IF (RRA.LT.RA(J)) THEN *)
       RA[I] := RA[J];
       I := J;
       J := J + J;
     ELSE
       J := R + 1;
     END; (* if(RRA.LT.RA(j)) *)
   END; (* WHILE J.LE.IR, or GO TO 20 in the original Fortran code
           END IF FOR original 20 IF(J.LE.IR)THEN.  No longer needed *)
   RA[I] := RRA;
 END; (* top loop *)
END revheapsort;

(************************************************************************************************)
PROCEDURE RevStraightInsSort(VAR INOUT RA : ExpandedEntryPointerArrayType; Left,Right: CARDINAL);
VAR
    X    : ExpandedEntryPointerType;
    j,I  : CARDINAL;
    i1   : CARDINAL;
BEGIN
  FOR I := Left+1 TO Right DO
(*      i1 := I;                                    debugging sttmnt *)
    X := RA[I];
    j := I;
    WHILE (j > 1) AND (X^.julian > RA[j-1]^.julian) DO
      RA[j] := RA[j-1];
      DEC(j);
    END; (* WHILE j > 1 and x < AI(j-1) *)
    RA[j] := X;
  END; (*  FOR I *)
END RevStraightInsSort;

(************************************************************************************************)
(************************************************************************************************)
PROCEDURE EntryTable2NameString(subscript : CARDINAL; VAR OUT s:NameString);
        VAR s0,s1 : NameString;
BEGIN
  s0 := '';
  Strings.Append(SearchEntryTable[subscript]^.ent.name,s0);
  Strings.Append('  ',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.size,s1);
  Strings.Append(s1,s0);
  Strings.Append('  ',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.dt.month,s1);
  Strings.Append(s1,s0);
  Strings.Append('/',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.dt.day,s1);
  Strings.Append(s1,s0);
  Strings.Append('/',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.dt.year,s1);
  Strings.Append(s1,s0);
  Strings.Append('  ',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.dt.hour,s1);
  Strings.Append(s1,s0);
  Strings.Append(':',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.dt.minute,s1);
  Strings.Append(s1,s0);
  Strings.Append(':',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.dt.second,s1);
  Strings.Append(s1,s0);
  Strings.Append('.',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.dt.fractions,s1);
  Strings.Append(s1,s0);
  s := s0;
END EntryTable2NameString;

(************************************************************************************************)
PROCEDURE SetFilenamePattern(ns: NameString);
(* If no valid file was found, then CountOfEntries will be zero.  That is the error flag *)
BEGIN
  CountOfEntries := 0;
  len := LENGTH(ns);

  IF SCANBACK(ADR(ns),len,WildCardChar,TRUE) = 0 THEN
    ExStrings.AppendChar('*',ns);
  END;  (* if ns does not contain a WildCardChar, append it *)
  IF FindFirst(ns, StdAttributes, MustHaveNormalFile, entry) THEN
    REPEAT
      INC(CountOfEntries);
      NEW(entryptr);
      SearchEntryTable[CountOfEntries] := entryptr;
      SearchEntryTable[CountOfEntries]^.ent := entry;
      m := entry.dt.month;
      d := entry.dt.day;
      y := entry.dt.year;
      hr := entry.dt.hour;
      min := entry.dt.minute;
      sec := entry.dt.second;
      frac := entry.dt.fractions;
      jd := JULIAN(m,d,y);
      secinday := 3600*hr + 60*min + sec;
      fsecinday := LFLOAT(secinday) + LFLOAT(frac)/1000.;
      juldate := LFLOAT(jd) + fsecinday/(24.*3600.);
      SearchEntryTable[CountOfEntries]^.julian := juldate;
    UNTIL NOT FindNext(entry) OR (CountOfEntries >= PointerArraySize);
    FindClose(entry);
    SearchEntryTableHold := SearchEntryTable;
  ELSE
   FindClose(entry);
   RETURN;
  END; (* if findfirst *)
(* reverse sort the entry table *)
  IF CountOfEntries > 5 THEN
    revheapsort(SearchEntryTable,1,CountOfEntries);
  ELSIF CountOfEntries > 1 THEN
    RevStraightInsSort(SearchEntryTable,1,CountOfEntries);
  END;
  ValidFilenamePatternSet := TRUE;
  counter := 0;
END SetFilenamePattern;

(************************************************************************************************)
PROCEDURE GetNextFilename(VAR OUT ns,DirectoryEntry: NameString);

BEGIN
  IF NOT ValidFilenamePatternSet THEN
    ns := '';
    DirectoryEntry := '';
    RETURN;
  END;

  IF counter < CountOfEntries THEN
    INC(counter);
  END;
(* If trying to go past the last table entry, keep returning the same last table entry *)
  ns := SearchEntryTable[counter]^.ent.name;
  EntryTable2NameString(counter,s);
  DirectoryEntry := s;
END GetNextFilename;
(************************************************************************************************)
PROCEDURE GetPrevFilename(VAR OUT ns,DirectoryEntry: NameString);
BEGIN
  IF NOT ValidFilenamePatternSet THEN
    ns := '';
    DirectoryEntry := '';
    RETURN;
  END;

  IF counter > 1 THEN
    DEC(counter);
  END;
(* If trying to go past the last table entry, keep returning the same last table entry *)
  ns := SearchEntryTable[counter]^.ent.name;
  EntryTable2NameString(counter,s);
  DirectoryEntry := s;
END GetPrevFilename;
(************************************** MAIN ********************************)
BEGIN
  ValidFilenamePatternSet := FALSE;

END FilePickerBase.
