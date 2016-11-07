<*DEFINE (ConsoleMode,TRUE)*>

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
  20-Apr-13  Created this variant that's in console mode.
*)
IMPLEMENTATION MODULE FilePickerConsole;

%IF ConsoleMode %THEN
    IMPORT MiscStdInOut, SIOResult;
    FROM MiscStdInOut IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, ReadString, SkipLine, ReadCard, ReadLongReal;
%ELSE
    IMPORT MiscM2,Terminal;
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
  IMPORT Strings,MemUtils,ASCII,ExStrings;
  FROM Environment IMPORT GetCommandLine;
  FROM TIMLIB IMPORT GREG2JUL,JULIAN;
  FROM UTILLIB IMPORT STRTYP,SCANBACK;
  FROM SYSTEM IMPORT ADR;

CONST
      PointerArraySize=1000;
      WildCardChar = '*';

TYPE
(* When added sort rtn needed new types which include a julian field to sort.
  SearchEntryTableType = ARRAY [1..tablesize] OF SearchEntry;
  SearchEntryPtr = POINTER TO SearchEntry;
  SearchEntryTablePointerType = ARRAY [1..tablesize] OF SearchEntryPtr;
*)
  ExpandedEntryType = RECORD
                        julian : LONGREAL;
                        ent    : SearchEntry;
                      END;
  ExpandedEntryPointerType = POINTER TO ExpandedEntryType;
  ExpandedEntryPointerArrayType = ARRAY [1..PointerArraySize] OF ExpandedEntryPointerType;
(*        NameString      = ARRAY [0..255] OF CHAR;  Defined in FileFunc Module *)

VAR
  SearchEntryTable,SearchEntryTableHold : ExpandedEntryPointerArrayType;
  entry                                   : SearchEntry;
  entryptr                                : ExpandedEntryPointerType;
  CountOfEntries, counter, holdctr, len   : CARDINAL;
  ns                                      : NameString;
  ch                                      : CHAR;
  inputline                               : STRTYP;
  AlreadyGotPattern                       : BOOLEAN;

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

PROCEDURE FileNamePicker(VAR OUT ns: NameString);
VAR
  ch                             : CHAR;
  s                              : NameString;
  m,d,y,hr,min,sec,frac,secinday : CARDINAL;
  juldate,fsecinday              : LONGREAL;
  jd                             : LONGINT;

  PROCEDURE GetPattern;
  BEGIN
    IF NOT AlreadyGotPattern THEN
      GetCommandLine(ns);
    ELSE
      WriteString(' Enter filename pattern: ');
      ReadString(ns);
      WriteLn;
    END; (* if get from commandline or prompt for it *)
    len := LENGTH(ns);
    IF len = 0 THEN 
      WriteString(' Enter filename pattern: ');
      ReadString(ns);
      WriteLn;
      len := LENGTH(ns);
    END; (* if len = 0, ask again once more.  If still empty, it will get a lone '*' *)
    IF SCANBACK(ADR(ns),len,WildCardChar,TRUE) = 0 THEN
      ExStrings.AppendChar('*',ns);
    END;  (* if ns does not contain a WildCardChar, append it *)
  END GetPattern;

  PROCEDURE LoadTable;
  BEGIN
    IF FindFirst(ns, StdAttributes, MustHaveNormalFile, entry) THEN
      CountOfEntries :=   0;
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
      Error(' No valid file names found. ');
      ns := '';
      FindClose(entry);
    END;
(* reverse sort the entry table *)
    IF CountOfEntries > 5 THEN
      revheapsort(SearchEntryTable,1,CountOfEntries);
    ELSIF CountOfEntries > 1 THEN
      RevStraightInsSort(SearchEntryTable,1,CountOfEntries);
    END;
  END LoadTable;

  PROCEDURE PrintPage;
  VAR
        i : CARDINAL;
        s : NameString;
  BEGIN
      IF counter <= CountOfEntries THEN
        i := counter;
        REPEAT
          WriteCard((i-counter+1) MOD 10);
          WriteString(") ");
          EntryTable2NameString(i, s);
          WriteString(s);
          WriteLn;
          INC(i);
        UNTIL (i-counter = 10) OR (i > CountOfEntries);
        WriteLn;
      END;
  END PrintPage;

VAR
(*    cmd : CHAR; *)
    i   : CARDINAL;
BEGIN
    GetPattern;
    AlreadyGotPattern := TRUE;

    LoadTable;

    counter := 1;
    LOOP
      PrintPage;
      WriteString("Enter number to select, (M)ore or (L)ess page, (P)attern, (Q)uit: ");
      ReadString(inputline);
      WriteLn;
      len := LENGTH(inputline);
      IF len > 0 THEN
        ch := CAP(inputline[1]);
      ELSE
        ch := '1';
      END; (* if inputline is empty or not.  To establish a default value of 1 *)
      CASE ch OF
      | "0" .. "9" :
            i := (ORD(ch)-ORD("0")+9) MOD 10 + counter;
            IF i <= CountOfEntries THEN
              Strings.Assign(SearchEntryTable[i]^.ent.name, ns);
              EXIT;
            ELSE
                Error('Invalid entry number.');
            END;
      | "M" :
            IF counter+10 < CountOfEntries THEN
                INC(counter, 10);
            ELSE
                Error('No more entries to display.');
            END;
      | "L" :
            IF counter > 1 THEN
                DEC(counter, 10);
            ELSE
                Error('No less entries to display.');
            END;
      | "P" :
            GetPattern;
            LoadTable();
            counter := 1;
      | "Q" :
            ns := "";
            EXIT;
      ELSE
            Error('Invalid command.');
      END; (* case on input character *)

    END;
END FileNamePicker;

BEGIN
  AlreadyGotPattern := FALSE;
END FilePickerConsole.

