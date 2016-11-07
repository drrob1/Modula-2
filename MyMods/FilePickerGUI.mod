<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:VLCshuffle.RES*>
%ELSE
%END
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
  26-Nov-13  New name to use a GUI window and not a terminal window.
*)
IMPLEMENTATION MODULE FilePickerGUI;
  IMPORT Terminal,MiscM2;
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
  IMPORT Strings,MemUtils,ASCII,ExStrings;
  FROM Environment IMPORT GetCommandLine;
  FROM TIMLIB IMPORT GREG2JUL,JULIAN;
  FROM UTILLIB IMPORT SCANBACK;
  FROM SYSTEM IMPORT ADR;
  FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, FontInfo, WinAttr, ClipboardFormat,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow, NormalChildWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo, WindowTypes,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, SpecialKeys,
    GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, WriteString,
    WriteStringAt, WriteCellsAt, WriteCells, WriteLn, EraseToEOL, ChangeAttr,
    ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn,
    SetAutoScroll, WinShellToTextWindowMessage,
    MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect,
    GetBufferRect, EraseScreen, EraseRect, GetWinShellHandle, FindTextWindow,
    SetDisplayMode,GetDisplayMode,SetWindowEnable, (*  SetWindowTitle, *)
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage,
    IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars,
    SetScrollBarPos, SetWindowData, SetWindowDataNum, GetWindowData, GetWindowDataNum,
    GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos, CascadeWindow,
    SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo,
    SetScrollDisableWhenNone, SetActiveTabChild, SetTabChildPosition,
    GetForegroundWindow, SetForegroundWindow, SetWindowFont,
    SetTimer, KillTimer, DisplayHelp, SetWindowIcon,
    OpenClipboard, CloseClipboard, EmptyClipboard, ClipboardFormatAvailable,
    AllocClipboardMemory, UnlockClipboardMemory, SetClipboard, GetClipboard,
    Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;

CONST ArraySize=100;
      PointerArraySize=1000;
      DisplayFactor=15;
      WildCardChar = '*';
  szAppName = "FilePickerGUI";
  InputPrompt = 'Enter or arrow keys : ';
  LastMod = '26 Nov 13';
  HalfCalcIcon = '#100';
  FullGreenIcon = '#200';
  HalfGreenIcon = '#300';

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
  entry                                   : SearchEntry;
  entryptr                                : ExpandedEntryPointerType;
  CountOfEntries, counter, holdctr, len   : CARDINAL;
  ns                                      : NameString;
  ch                                      : CHAR;
  inputprocessed                          : BOOLEAN = FALSE;
  hwnd        :  WIN32.HWND;
  msg         :  WINUSER.MSG;
  wc          :  WINUSER.WNDCLASSEX;
  cxChar      : INTEGER;
  cyChar      : INTEGER;
  cxClient    : INTEGER;
  cyClient    : INTEGER;
  cxBuffer    : INTEGER;
  cyBuffer    : INTEGER;
  cxScreen,cyScreen,wxClient,wyClient : COORDINATE;
  xCaret      : INTEGER;
  yCaret      : INTEGER;
  FPGUIWin    : TextWindow;
  biggerFont  : FontInfo;
  a,aBold     : ScreenAttribute;


(************************************************************************************************)
PROCEDURE heapsort(VAR INOUT RA : ExpandedEntryPointerArrayType; Left,Right: CARDINAL);
(*
  Favorite sort routine of "Numerical Recepies", and much faster than quicksort when I
  just tested these on 9/4/11.  Shocking!
  This compares contents and swaps pointers to be faster.  I have to remember to think
  about the sort operation as a comparison operation of dereferenced pointers, and a
  swap operation of pointers themselves.
*)
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
       IF (RA[J]^.julian < RA[J+1]^.julian) THEN INC(J); END; (* IF(RA(J).LT.RA(J+1)) J=J+1 *)
     END;
     IF (RRA^.julian < RA[J]^.julian) THEN (* IF (RRA.LT.RA(J)) THEN *)
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
END heapsort;

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

PROCEDURE revqsort (VAR INOUT RA : ExpandedEntryPointerArrayType; Left,Right: CARDINAL);
VAR
  i,j : CARDINAL;
  w,x : ExpandedEntryPointerType;
BEGIN
  i := Left;
  j := Right;
  x := RA[ (Left+Right) DIV 2 ];
  REPEAT
    WHILE (RA[i]^.julian > x^.julian) DO
      INC(i);
    END; (* WHILE AI[i] *)
    WHILE (x^.julian > RA[j]^.julian) DO
      DEC(j);
    END; (* WHILE x *)
    IF i <= j THEN
      w := RA[i];
      RA[i] := RA[j];
      RA[j] := w;
      INC(i);
      DEC(j);
    END; (* IF i<=j *)
  UNTIL i > j;
  IF Left < j THEN
      revqsort(RA,Left,j);
  END; (* IF Left < j *)
  IF i < Right THEN
      revqsort(RA,i,Right);
  END; (* If i < Right *)
END revqsort;

(************************************************************************************************)
PROCEDURE revQUICKSORT(VAR INOUT RA : ExpandedEntryPointerArrayType; Left,Right: CARDINAL);

    PROCEDURE revSORT(L,R : CARDINAL);
    VAR
      I,J  : CARDINAL;
      w,X  : ExpandedEntryPointerType;

    BEGIN
        I := L;
        J := R;
        X := RA[(L+R) DIV 2];

        REPEAT
          WHILE RA[I]^.julian > X^.julian DO INC(I); END;
          WHILE X^.julian > RA[J]^.julian DO DEC(J); END;
          IF I <= J THEN
            w := RA[I];
            RA[I] := RA[J];
            RA[J] := w;
            INC(I);
            DEC(J);
          END(*IF*);
        UNTIL I > J;
        IF L < J THEN revSORT(L,J) END(*IF*);
        IF I < R THEN revSORT(I,R) END(*IF*);
    END revSORT;

BEGIN  (* QUICKSORT body *)
  revSORT(Left,Right);
END revQUICKSORT;

(*++++*****************************************************************)
PROCEDURE FPGUIWndProcTW(tw:TextWindow; msg:TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr   : Colors;
    x,y   : COORDINATE;
    i,int : INTEGER;
    idx,c : CARDINAL;
    ans   : CHAR;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
(*        RETURN OkayToClose; *)
    | TWM_CREATE:
        SetScrollDisableWhenNone(tw,TRUE,TRUE);
    | TWM_SIZE:
        SnapWindowToFont(tw,TRUE);
        SetDisplayMode(tw,DisplayNormal);
(*
        SetDisplayMode(tw,DisplayVisible);
  DisplayModes        = (
                           DisplayNormal, /* will activate window */
                                          /* make it visible */
                                          /* reverses minimized or maximized */
                           DisplayVisible,/* makes window visible */
                                          /* will not activate window */
                           DisplayHidden,/* hides window */
                                         /* active window changes */
                           DisplayMinimized,/* minimizes window */
                                            /* does not change active window */
                           DisplayMaximized/* will activate window */
                                           /* maximizes window */
                          ) BIG;
*)
        SetScrollRangeAllowed(tw,WA_VSCROLL,60);
        SetScrollBarPos(tw,WA_VSCROLL,0);
        SetScrollRangeAllowed(tw,WA_HSCROLL,100);
        SetScrollBarPos(tw,WA_HSCROLL,0);
        SetCaretType(tw,CtHalfBlock);
(*        MoveCaretTo(tw,xCaret,yCaret); *)
        MakeCaretVisible(tw);
        CaretOn(tw);
        SetWindowEnable(tw,TRUE);
        SetForegroundWindow(tw);
    | TWM_GAINFOCUS, TWM_ACTIVATEAPP :
        MakeCaretVisible(tw);
    | TWM_PAINT:
    | TWM_NOTIFYSCROLL:
    | TWM_KEY:
        SetForegroundWindow(Win);
    ELSE (* case msg.msg *)
    END (* case msg.msg *);
    RETURN DEFAULT_HANDLE;
END FPGUIWndProcTW;

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
PROCEDURE FileNamePicker(VAR OUT ns:NameString);

(* I use both preincremented and postincremented pointers here *)

VAR
  ch                             : CHAR;
  s                              : NameString;
  m,d,y,hr,min,sec,frac,secinday : CARDINAL;
  juldate,fsecinday              : LONGREAL;
  jd                             : LONGINT;

BEGIN
  FPGUIWin := CreateWindow(NIL, (* parent : WinShell.Window *)
            "File Picker Window", (* name : ARRAY OF CHAR *)
            "",                   (* menu : ARRAY OF CHAR *)
            FullGreenIcon,            (* icon : ARRAY OF CHAR *)
             1, 1,
            (* -1,-1,  x,y= the initial screen coordinates for the window to be displayed *)
            99,27, (* xSize, ySize : COORDINATE *)
            500,200, (* xBuffer, yBuffer : COORDINATE *)
            FALSE,  (* gutter : BOOLEAN *)
            biggerFont, (* DefaultFontInfo, /* font : FontInfo */ *)
            ComposeAttribute(Black, White, NormalFont), (* background : ScreenAttribute *)
            ToplevelWindow,  (* windowType : WindowTypes *)
            FPGUIWndProcTW,
            NormalWindow + AddScrollBars,    (* attribs : WinAttrSet *)
            NIL); (* createParam : ADDRESS *)
  IF FPGUIWin = NIL THEN
    WinShell.TerminateDispatchMessages(127);
  END;
  SetAutoScroll(FPGUIWin,TRUE);
  SetForegroundWindow(FPGUIWin);
  WriteString(FPGUIWin,InputPrompt,biggerFont);
  RepaintScreen(FPGUIWin);
  ProcessInput(ns);
END FileNamePicker;


PROCEDURE ProcessInput(VAR OUT ns: NameString);
      REPEAT
        CLS;
        GetCommandLine(ns);
        len := LENGTH(ns);
        IF len = 0 THEN
          WriteString(' Enter filename pattern: ');
          ReadString(ns);
          WriteLn;
        END;
        len := LENGTH(ns);
        IF SCANBACK(ADR(ns),len,WildCardChar,TRUE) = 0 THEN
          ExStrings.AppendChar('*',ns);
        END;  (* if ns does not contain a WildCardChar, append it *)
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
(*                  revqsort(SearchEntryTable,1,CountOfEntries); *)
(*                  revQUICKSORT(SearchEntryTable,1,CountOfEntries); *)

        END;
        counter := 1;
        holdctr := counter-1;
        whileloop:
          WHILE counter <= CountOfEntries DO  (* if no valid filenames found, loop will not write anything *)
            EntryTable2NameString(counter,s);
            WriteString(s);
            WriteLn;
            IF (counter MOD DisplayFactor = 0) OR (counter > CountOfEntries) THEN
              WriteLn;
              Terminal.WriteString ("Press m for more loop, p for new pattern, arrows to cycle thru filenames.");
              ch := Terminal.ReadChar();
              ch := CAP(ch);
              Terminal.WriteLn;
              IF ch <> 'M' THEN
                BREAK whileloop
              ELSIF counter <= CountOfEntries THEN (* ch = M *)
                holdctr := counter;
                CLS
              ELSE (* counter > CountOfEntries AND ch = M *)
                WriteString(' No more entries to display. ');
                WriteLn;
              END; (* if ch <> M *)
            END; (* if counter mod DisplayFactor *)
            INC(counter);
          END; (* while counter <= CountOfEntries *)
(* Will pick up here after exit from whileloop *)

        IF ch <> 'P' THEN ch := 'N'; END;
      UNTIL ch='N';

      counter := holdctr;
      WriteLn;
      WriteString(' <tab>, <enter>, arrows or backspace: ');
      ch := Terminal.Tab;
      LOOP
        CASE ch OF
          Terminal.Tab,Terminal.CursorDown,Terminal.CursorRight,Terminal.PageDown,' ':
            IF counter < CountOfEntries THEN INC(counter) END;
            Terminal.Position(0,DisplayFactor+6);
                EntryTable2NameString(counter,s);
                WriteString(s);
                WriteString(' ');
(*                WriteLongReal(SearchEntryTable[counter]^.julian,8); *)
                WriteString('                                         ');
        | Terminal.Enter:
            IF counter < 1 THEN counter := 1 END;  (* default to 1st entry in table *)
            ns := SearchEntryTable[counter]^.ent.name;
            EXIT;
        | Terminal.BackSpace,Terminal.CursorUp,Terminal.CursorLeft,Terminal.PageUp:
            IF counter > 1 THEN DEC(counter) END;
            Terminal.Position(0,DisplayFactor+6);
                EntryTable2NameString(counter,s);
                WriteString(s);
                WriteString(' ');
(*                WriteLongReal(SearchEntryTable[counter]^.julian,8); *)
                WriteString('                                         ');
        | Terminal.Escape:
            ns := '';
            EXIT;
        | ELSE
                (* ignore *)
        END; (* case *)
        Terminal.Read(ch);
      END; (* Terminal Read Loop *)
END FileNamePicker;

(************************************** MAIN ********************************)
BEGIN
	a := ComposeAttribute(Black,White,NormalFont);
	aBold := ComposeAttribute(Black,White,BoldFont);
	biggerFont := DefaultFontInfo;
	biggerFont.height := 140;


END FilePickerGUI.