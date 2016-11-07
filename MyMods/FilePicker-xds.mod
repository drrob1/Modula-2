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
*)
IMPLEMENTATION MODULE FilePicker;

  FROM STextIO IMPORT WriteString,WriteLn,ReadString,ReadChar,SkipLine;
  FROM Storage IMPORT ALLOCATE;
  FROM FileSystem IMPORT Directory, DirEntry, FindFirst, FindNext, FindClose;
  FROM TIMLIB IMPORT JULIAN;
  IMPORT Strings, ProgramArgs, WholeStr, TextIO, SWholeIO;

(*
  PROCEDURE GREG2JUL(M,D,Y : CARDINAL; VAR JULDATE : LONGREAL);
  PROCEDURE JULIAN(M,D,Y : CARDINAL) : INTEGER;
*)

CONST (* ArraySize=100; *)
      PointerArraySize=1000;
      WildCardChar = '*';
      EmptyInput = 0C;
TYPE
(* When added sort rtn needed new types which include a julian field to sort.
  SearchEntryTableType = ARRAY [1..tablesize] OF DirEntry;
  SearchEntryPtr = POINTER TO DirEntry;
  SearchEntryTablePointerType = ARRAY [1..tablesize] OF SearchEntryPtr;
*)
  ExpandedEntryType = RECORD
                        julian : LONGREAL;
                        ent    : DirEntry;
                      END;
  ExpandedEntryPointerType = POINTER TO ExpandedEntryType;
  ExpandedEntryPointerArrayType = ARRAY [1..PointerArraySize] OF ExpandedEntryPointerType;
(*        NameString      = ARRAY [0..255] OF CHAR;  Defined in FileFunc Module *)

VAR
  SearchEntryTable : ExpandedEntryPointerArrayType;
  entry            : DirEntry;
  entryptr         : ExpandedEntryPointerType;
  CountOfEntries, counter : CARDINAL;

(*
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
*)

PROCEDURE revheapsort(VAR (*INOUT*) RA : ExpandedEntryPointerArrayType; Left,Right: CARDINAL);
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

PROCEDURE RevStraightInsSort(VAR (*INOUT*) RA : ExpandedEntryPointerArrayType; Left,Right: CARDINAL);
VAR
    X    : ExpandedEntryPointerType;
    j,I  : CARDINAL;
(*    i1   : CARDINAL; *)
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
(*
PROCEDURE revqsort (VAR (*INOUT*) RA : ExpandedEntryPointerArrayType; Left,Right: CARDINAL);
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
*)

PROCEDURE EntryTable2NameString(subscript : CARDINAL; VAR (*OUT*) s:NameString);
        VAR s0,s1 : NameString;
BEGIN
  s0 := '';
  Strings.Append(SearchEntryTable[subscript]^.ent.name,s0);
  Strings.Append('  ',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.size,s1);
  Strings.Append(s1,s0);
  Strings.Append('  ',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.time.month,s1);
  Strings.Append(s1,s0);
  Strings.Append('/',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.time.day,s1);
  Strings.Append(s1,s0);
  Strings.Append('/',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.time.year,s1);
  Strings.Append(s1,s0);
  Strings.Append('  ',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.time.hour,s1);
  Strings.Append(s1,s0);
  Strings.Append(':',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.time.minute,s1);
  Strings.Append(s1,s0);
  Strings.Append(':',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.time.second,s1);
  Strings.Append(s1,s0);
  Strings.Append('.',s0);
  WholeStr.CardToStr(SearchEntryTable[subscript]^.ent.time.fractions,s1);
  Strings.Append(s1,s0);
  s := s0;
END EntryTable2NameString;

PROCEDURE GetCommandLine(VAR cmd: ARRAY OF CHAR);
VAR
    arg: ProgramArgs.ChanId;
BEGIN
    cmd[0] := "";
    arg := ProgramArgs.ArgChan();
    IF ProgramArgs.IsArgPresent() THEN
        TextIO.ReadToken(arg, cmd);
	Strings.Append(WildCardChar,cmd);
    END;
END GetCommandLine;

PROCEDURE Error(msg: ARRAY OF CHAR);
BEGIN
    WriteString("**ERROR** ");
    WriteString(msg);
    WriteLn;
END Error;

PROCEDURE FileNamePicker(VAR ns: NameString);

    PROCEDURE LoadTable();
    VAR
        fsecinday : LONGREAL;
        jd        : INTEGER;
        secinday  : CARDINAL;
        dir       : Directory;

    BEGIN
        (*
        IF ns[LENGTH(ns)-1] <> WildCardChar THEN
          Strings.Append('*',ns);
        END;  (* if last char in ns is a WildCardChar *)
        *)

        IF FindFirst(dir, ns, entry) THEN
            CountOfEntries := 0;
            REPEAT
                IF NOT entry.dir THEN
                    NEW(entryptr);
                    entryptr^.ent := entry;
                    jd := JULIAN(entry.time.month, entry.time.day, entry.time.year);
                    secinday := 3600*entry.time.hour + 60*entry.time.minute + entry.time.second;
                    fsecinday := LFLOAT(secinday) + LFLOAT(entry.time.fractions)/1000.;
                    entryptr^.julian := LFLOAT(jd) + fsecinday/(24.*3600.);

                    INC(CountOfEntries);
                    SearchEntryTable[CountOfEntries] := entryptr;
                END;
            UNTIL NOT FindNext(dir, entry) OR (CountOfEntries >= PointerArraySize);
            FindClose(dir);

        (* reverse sort the entry table *)
            IF CountOfEntries > 5 THEN
                revheapsort(SearchEntryTable,1,CountOfEntries);
            ELSIF CountOfEntries > 1 THEN
                RevStraightInsSort(SearchEntryTable,1,CountOfEntries);
        (*        revqsort(SearchEntryTable,1,CountOfEntries); *)
        (*        revQUICKSORT(SearchEntryTable,1,CountOfEntries); *)

            END;
        ELSE
            Error('No valid file names found.');
        END;
    END LoadTable;

    PROCEDURE GetPattern;
    BEGIN
        WriteString('Enter filename pattern: ');
        ReadString(ns);
        SkipLine;
	Strings.Append(WildCardChar,ns);
        WriteLn;
    END GetPattern;

    PROCEDURE PrintPage;
    VAR
        i : CARDINAL;
        s : NameString;
    BEGIN
        IF counter <= CountOfEntries THEN
            i := counter;
            REPEAT
                SWholeIO.WriteCard((i-counter+1) MOD 10, 1);
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
    cmd : CHAR;
    i   : CARDINAL;
BEGIN
    GetCommandLine(ns);
    IF ns[0] = ""  THEN
        GetPattern;
    END;
    LoadTable();

    counter := 1;
    LOOP
        PrintPage;
        WriteString("Enter number to select, M)ore or L)ess page, P)attern, Q)uit: ");
        ReadChar(cmd);
        SkipLine;
        WriteLn;
(*        WriteString('cmd is '); SWholeIO.WriteCard(ORD(cmd),0); WriteLn; *)
        IF cmd=EmptyInput THEN cmd := '1' END;
	
        CASE CAP(cmd) OF
        | "0" .. "9" :
            i := (ORD(cmd)-ORD("0")+9) MOD 10 + counter;
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
        END;

    END;
END FileNamePicker;

END FilePicker.

