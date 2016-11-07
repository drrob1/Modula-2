(*
  Both BinInsSort and StraightInsSort had to be debugged because there was a mistake in the
  algorithm.  In the "FOR I := Left+1 TO Right DO", that was initially "Right-1" which meant that
  the last item in the list would be ignored and not be in its correct sorted position.
  I took a guess and changed this to how it reads now and I was right.

 18 Sep 11 -- Added timing capability.
 20 Sep 11 -- Added number input to select sort algorithm.
 22 Sep 11 -- Added code from my early days, ~ 91 ish.  This may take a while :-)
*)

MODULE SortPointerStuff;
  IMPORT Terminal,MiscM2,LongMath;
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteLongCard,WriteInt,ReadString,ReadCard,
                      CLS,Error,WriteReal;
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
  IMPORT Strings,MemUtils,ASCII;
(*
  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENPTR IMPORT FSATYP,DELIMCH,INI3TKN,INI1TKN,GETTKN;
  FROM MyFIO2 IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FOPEN,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FWRLF,FAPPEND,COPYDPIF,GETFNM;
*)
  FROM Environment IMPORT GetCommandLine;
  FROM SysClock IMPORT DateTime,CanGetClock,CanSetClock,GetClock;

CONST
  K = 1024;
  maxLineLength = 80;
  maxEntries    = 80000;
  maxEntriesTree = maxEntries+100;
  ln2 = 0.69314718;

TYPE
  IOSTATE = (RD,WR,APND);
  STRING = ARRAY [1..maxLineLength] OF CHAR;
  WordEntryType = RECORD
                        s      : STRING;
                        julian : LONGREAL;
                      END;
  WordPointerType = POINTER TO WordEntryType;
  WordArrayType = ARRAY [1..maxEntries] OF WordEntryType; (* for treesort *)
  WordPointerArrayType = ARRAY [1..maxEntries] OF WordPointerType;
  LinkArrayType = ARRAY [1..maxEntriesTree] OF CARDINAL;  (* for treesort *)
  LinkPointerArrayType = POINTER TO LinkArrayType;        (* for treesort *)
VAR
  InputFileName, OutputFileName: STRING;
  ch : CHAR;
  c,c1,ignoreCard  : CARDINAL;
  FLAG : BOOLEAN;
  wordentry : WordEntryType;
  OutOfPlaceWordPointer,NewWordPointer : WordPointerType;
  inputline       : STRING;
  Length          : CARDINAL;
  NumberOfEntries : CARDINAL;
  NoExchanges     : BOOLEAN;
  InitTime,FinalTime       : DateTime;
  InitSec,FinalSec,DiffSec : REAL;
  WordPointerArray,SortedWordPointerArray      : WordPointerArrayType;
  InFile,OutFile                               : File;
  innameparts,outnameparts                     : FileNameParts;
  searchentry                                  : SearchEntry;
  OpenFileName,InName,OutName                  : STRING;
  InBuf, OutBuf                                : ARRAY [1..30*K] OF CHAR;
  WA                                           : WordArrayType;
  LA                                           : LinkArrayType;


  PROCEDURE BinInsSort(VAR INOUT AI : WordPointerArrayType; Left,Right : CARDINAL);
  VAR
      I,J,M,L,R : CARDINAL;
      X         : WordPointerType;
(*                                          i1,j1,m1,L1,R1 : CARDINAL; *)
      i1,j1 : CARDINAL;

  BEGIN
      FOR I := Left+1 TO Right DO
        i1 := I; (* debugging sttmnt *)
        X := AI[I];
        L := Left;
        R := I;
        WHILE L < R DO
          M := (L+R) DIV 2;
          IF (Strings.Compare(AI[M]^.s, X^.s) = Strings.greater ) THEN
            R := M;
          ELSE
            L := M + 1;
          END; (* IF AI(M) > X *)
        END; (* WHILE L<R *)
        FOR J := I TO R+1 BY -1 DO
          j1 := J;                                  (* debugging sttmnt *)
          AI[J] := AI[J-1];
        END; (* FOR J in reverse *)
        AI[R] := X;
      END; (* FOR I in Left+1 *)
    END BinInsSort;


  PROCEDURE StraightInsSort(VAR INOUT AI : WordPointerArrayType; Left,Right : CARDINAL);
  VAR
      X    : WordPointerType;
      j,I  : CARDINAL;
      i1   : CARDINAL;
    BEGIN
      FOR I := Left+1 TO Right DO
        i1 := I; (*                                   debugging sttmnt *)
        X := AI[I];
        j := I;
        WHILE (j > 1) AND (Strings.Compare(X^.s, AI[j-1]^.s) = Strings.less) DO
          AI[j] := AI[j-1];
          DEC(j);
        END; (* WHILE j > 1 and x < AI(j-1) *)
        AI[j] := X;
      END; (*  FOR I *)
    END StraightInsSort;

   PROCEDURE qsort (VAR INOUT AI : WordPointerArrayType; Left,Right : CARDINAL);
   VAR
     i,j : CARDINAL;
     w,x : WordEntryType;
   BEGIN
     i := Left;
     j := Right;
     x := AI[ (Left+Right) DIV 2 ]^; (* Timing went down 500x when I replaced '/' with 'DIV' in this line. *)
     REPEAT  (* CompareResults = (less, equal, greater); *)
       WHILE Strings.Compare(AI[i]^.s, x.s) = Strings.less DO
         INC(i);
       END; (* WHILE AI[i] *)
       WHILE Strings.Compare(x.s, AI[j]^.s) = Strings.less DO
         DEC(j);
       END; (* WHILE x *)
       IF i <= j THEN
         OutOfPlaceWordPointer := AI[i];
         AI[i] := AI[j];
         AI[j] := OutOfPlaceWordPointer;
         INC(i);
         DEC(j);
       END; (* IF i<=j *)
     UNTIL i > j;
     IF Left < j THEN
         qsort(AI,Left,j);
     END; (* IF Left < j *)
     IF i < Right THEN
         qsort(AI,i,Right);
     END; (* If i < Right *)
   END qsort;

PROCEDURE qsort2 (VAR INOUT AI : WordPointerArrayType; Left,Right : CARDINAL);
(*
  qsort 2 allows for small lists to be heapsorted instead of all lists to be qsorted.
*)
  VAR
    i,j : CARDINAL;
    w,x : WordEntryType;
  BEGIN
  IF Right-Left < 10 THEN
    heapsort(AI,Left,Right);
  ELSE
     i := Left;
     j := Right;
     x := AI[ (Left+Right) DIV 2 ]^;
     REPEAT  (* CompareResults = (less, equal, greater); *)
       WHILE Strings.Compare(AI[i]^.s, x.s) = Strings.less DO
         INC(i);
       END; (* WHILE AI[i] *)
       WHILE Strings.Compare(x.s, AI[j]^.s) = Strings.less DO
         DEC(j);
       END; (* WHILE x *)
       IF i <= j THEN
         OutOfPlaceWordPointer := AI[i];
         AI[i] := AI[j];
         AI[j] := OutOfPlaceWordPointer;
         INC(i);
         DEC(j);
       END; (* IF i<=j *)
     UNTIL i > j;
     IF Left < j THEN
         qsort(AI,Left,j);
     END; (* IF Left < j *)
     IF i < Right THEN
         qsort(AI,i,Right);
     END; (* If i < Right *)
  END; (* if right - left < 10 *)
END qsort2;

PROCEDURE QUICKSORT(VAR INOUT AI : WordPointerArrayType; Left,Right : CARDINAL);

(* initial quicksort that I played with. *)

    PROCEDURE SORT(L,R : CARDINAL);
    VAR
      I,J  : CARDINAL;
      X    : WordEntryType;

    BEGIN
      IF R-L < 10 THEN
        BinInsSort(AI,L,R);
      ELSE
        I := L;
        J := R;
        X := AI[(L+R) DIV 2]^;

        REPEAT
          WHILE Strings.Compare(AI[I]^.s, X.s) = Strings.less DO INC(I); END;
          WHILE Strings.Compare(X.s, AI[J]^.s) = Strings.less DO DEC(J); END;
          IF I <= J THEN
            OutOfPlaceWordPointer := AI[I];
            AI[I] := AI[J];
            AI[J] := OutOfPlaceWordPointer;
            INC(I);
            DEC(J);
          END(*IF*);
        UNTIL I > J;
        IF L < J THEN SORT(L,J) END(*IF*);
        IF I < R THEN SORT(I,R) END(*IF*);
      END(*IF*);
    END SORT;

BEGIN  (* QUICKSORT body *)
  SORT(Left,Right);
END QUICKSORT;




(*
  Favorite sort routine of "Numerical Recepies"
*)
PROCEDURE heapsort(VAR INOUT RA : WordPointerArrayType; Left,Right: CARDINAL);
VAR
  L,R,I,J : CARDINAL;
  RRA : WordPointerType;

BEGIN
(*  Taking out this i/o operation to more accurately time this proc.
  WriteString(" Heapsort.");
  WriteLn;
*)
  L := Right DIV 2 + 1;
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
       IF Strings.Compare(RA[J]^.s, RA[J+1]^.s) = Strings.less THEN INC(J); END; (* IF(RA(J).LT.RA(J+1)) J=J+1 *)
     END;
     IF Strings.Compare(RRA^.s, RA[J]^.s) = Strings.less THEN (* IF (RRA.LT.RA(J)) THEN *)
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




(*
  I'm not going to debug this routine.  It requires copying params and passes the same param twice, once by value and once by
  reference.
*)
PROCEDURE mergesort(VAR INOUT RA : WordPointerArrayType; Left,Right: CARDINAL);

  VAR size : CARDINAL;

  PROCEDURE merge ( VAR INOUT x, z : WordPointerArrayType; low, middle, high : CARDINAL);
    VAR i, k, j, p : CARDINAL;
  BEGIN
    i := low;
    k := low;
    j := middle + 1;
    WHILE ( i <= middle ) AND ( j <= high ) DO
      IF (Strings.Compare(x[i]^.s,x[j]^.s) = Strings.less) OR Strings.Equal(x[i]^.s,x[j]^.s)  THEN  (* IF x[i]^ <= x[j]^ *)
        z[k] := x[i];
        INC(i)
      ELSE
        z[k] := x[j];
        INC(j)
      END (* if then *);
      INC(k)
    END (* while loop *);
    IF i > middle THEN
      FOR p := j TO high DO
        z[k] := x[p];
        INC(k)
      END (* for loop *);
    ELSE
      FOR p := i TO middle DO
        z[ k ] := x[ p ];
        INC( k )
      END (* for loop *);
    END (* if i > middle then *);
  END merge;

  PROCEDURE mpass ( VAR INOUT x, z : WordPointerArrayType; n, size : CARDINAL);
    VAR i, k : CARDINAL;
  BEGIN
    i := 0;
    WHILE  i  <= n - 2 * size DO
      merge(x, z, i, i+Right-1, i+2*Right-1);
      INC(i, 2*Right)
    END (* while loop *);
    IF i + size - 1 < n THEN
      merge(x, z, i, i+Right-1, n)
    ELSE
      FOR k := i TO n DO
        z[k] := x[k]
      END (* for loop *);
    END (* if i+size-1 < n then *)
  END mpass;

  PROCEDURE msort (VAR INOUT items : WordPointerArrayType; copy : WordPointerArrayType);
    VAR number, n  : INTEGER;
  BEGIN
    n := size;
    number := Left;
    WHILE number < n DO
      mpass(items, copy, n, number);
      INC(number, number);
      mpass(copy, items, n, number);
      INC(number, number);
    END; (* while loop *)
  END msort;

BEGIN
  msort( RA, RA )
END mergesort;


(* Doesn't work, and I don't know why *)
PROCEDURE HEAPSORT (VAR INOUT items : WordPointerArrayType; Right,Left : CARDINAL);

  VAR index, number : CARDINAL;

  PROCEDURE swap ( VAR INOUT p, q : WordPointerType );
  VAR temp : WordPointerType;
  BEGIN
    temp := p;
    p := q;
    q := temp
  END swap;


  PROCEDURE siftup (n : CARDINAL);
    VAR i, p : INTEGER;
        done : BOOLEAN;
  BEGIN
    i := INT(n);
    done := FALSE;
    WHILE ( i > INT(Right) ) AND NOT done DO
      p := i DIV 2;
      IF (Strings.Compare(items[i]^.s,items[p]^.s) = Strings.less) OR Strings.Equal(items[i]^.s,items[p]^.s) THEN  (* items[i] <= items [j] *)
        done := TRUE
      ELSE
        swap(items[i], items[p]);
        i := p
      END (* end if *)
    END (* end while *)
  END siftup;

  PROCEDURE siftdown( n : CARDINAL );
    VAR i, c : INTEGER;
        done : BOOLEAN;
  BEGIN
    i := INT(Right);
    c := INT(Right);
    done := FALSE;
    WHILE (c < INT(n)) AND NOT done DO
      c := 2 * i + 1;
      IF c <= INT(n) THEN
        IF ( c < INT(n) ) AND
                ((Strings.Compare(items[c]^.s,items[c+1]^.s)=Strings.less) OR Strings.Equal(items[c]^.s,items[c+1]^.s)) THEN
          INC(c)
        END (* if then *);
        IF (Strings.Compare(items[c]^.s,items[i]^.s)=Strings.less) OR Strings.Equal(items[c]^.s,items[i]^.s) THEN
          done := TRUE
        ELSE
          swap(items[c], items[i]);
          i := c
        END (* if then *);
      END (* if then *);
    END (* while *);
  END siftdown;

BEGIN
  FOR index := Right TO Left DO
    siftup( index )
  END (* for loop *);
  FOR index := Left TO Right BY -1 DO
    swap( items[Right], items[index] );
    siftdown( index - 1 )
  END (* for loop *)
END HEAPSORT;

PROCEDURE TREESORT(VAR words : WordArrayType; VAR links : LinkArrayType; N : CARDINAL);
(*
  This sorting algorithm was written to use as few comparisons as possible, to have as few steps btwn each comparison as possible,
to take advantage of natural sequencing, to preserve the order of equals (or even the reverse order of equals), ie, to be stable,
to use as little memory as possible (one working array), and to be a modular, easily understood program written in BASIC.
Unfortunately, the horrendous variable names in this program are from this original BASIC listing of the program.
  The theory behind the algorithm may be described in a language of forests, trees, branches, twigs and leaves.  There is a
forest filled with trees of different sizes.  Each tree is very orderly.  The trunk of a tree splits into two branches of
nearly the same size.  If one branch is larger than the other, it is always the right-hand branch.  Similarly, each branch divides
into two more branches until the branches become twigs from which leaves grow.  The leaves are the individual elements to be sorted.
  This program creates a butterfly merge to combine leaves into twigs, twigs into branches, and branches into one final linked list
starting at position N+1.  In the end, LINKS(N+1) points to the first leaf, LINKS(LINKS(N+1)) points to the second,
LINKS(LINKS(LINKS(N+1))) points to the third, and the last link points to itself.
  Two things happen as the pgm jumps from twig to twig.  The leaves ahead of the current record pointer get merged into a twig and
the twigs and branches behind this pointer get merged into larger branches.  The butterfly merge treats each merge the same way.
The heads of each twig sequence are kept at positions N+1, N+2, ..., N+log2(N)+2 after the links themselves,
which are kept in positions 1,2,3,...,N of array links.
  The merge takes the last 2 sequences created in the list and combines them into one.  One wing of the merge follows sequence 1
and the other follows sequence 2.  The two are interwoven until the final link points to itself.  Because the heads of each sequence
are kept in the same array with the links themselves, the merge is extraordinarily fast.  After each merge, the stack of twig
sequence heads has been reduced by one.
  Each time the current record pointer reaches a new twig, it generates new sequences one item long to correspond to the leaves
of that twig.  A two leaf twig is produced by creating two one item sequences, each pointing to itself. Then these two leaves
are merged once.  A three leaf twig is created from three one item sequences merged twice.  A four leaf twig is merged from two
two leaf twigs: the first two leaf twig is created and merged once; then the number of remaining merge passes is set to a negative
number so that the merge will be disabled until the second two leaf twig is created and merged with the first.
  After each complete twig has been generated, merging continues until the branches behind the current record pointer have been
linked together.  Then it jumps to the next twig, generates new leaves and lets the butterfly merge fly again.
  A state machine was the simplest way to implement all the branching of the FORTRAN code.  An arbitrary numbering from 1 to 6
is used in which the variable STATE represents this state machine's indicator.  A second minor state machine had to be introduced
as well, which uses the variable MODE as its indicator.

*)

  VAR
    AK1,AK2,T2,T3,T4,SQNC1,      B1,B2     : CARDINAL;
    SEQHEAD,L0,L1,L2,STATE,MODE,ELEMIDX,T1 : CARDINAL;
    MERGES                                 : INTEGER;

  BEGIN
(*
    IF (N+ln(N)/ln2+2) > HIGH(links) THEN
      Error(' Links array is too small for TREESORT.  Sort not done');
      RETURN;
    END;
*)
    AK1 := 0;
    ELEMIDX := 0;
    MERGES := 0;
    T2 := 0;
    T4 := 0;
    SEQHEAD := N + 1;
    links[1] := 1;
    links[SEQHEAD] := 1;
    AK2 := 1;
    SQNC1 := N;
    WHILE SQNC1 >= 4 DO    (* Climb the tree *)
      AK2 := 2*AK2;
      B2 := SQNC1/2;
      SQNC1 := B2;
      T4 := T4 + AK2*(B2-SQNC1);
    END(*WHILE*);
    T4 := AK2 - T4; (* T4 is the # of low order twigs *)
    B2 := AK2/2;
    STATE := 1;
    WHILE (STATE > 1) OR (AK1 < AK2) DO (* Next twig.  Can only exit when STATE = 1 *)
      CASE STATE OF
      1:
        INC(AK1);
        T1 := AK1;
        B1 := B2;
        T3 := T2;
        WHILE NOT ODD(T1) DO
          INC(MERGES);
          T2 := T2 - B1;
          B1 := B1/2;
          T1 := T1 DIV 2;
        END(*LOOP*);
(* Twig calculations *)
        T2 := T2 + B1;
        IF (SQNC1 = 2) AND (T3 < T4) THEN
          INC(MERGES);  (* 2 twig *)
        ELSIF (SQNC1 = 2) OR (T3 < T4) THEN
          INC(MERGES);     (* 3 twig *)
          INC(ELEMIDX);
(* Make a leaf *)
          links[ELEMIDX] := ELEMIDX;
          links[SEQHEAD] := ELEMIDX;
(* Next sequence head *)
          INC(SEQHEAD);
          INC(MERGES);  (* 2 twig *)
        ELSE  (* 4 twig.  Disengage # of merges *)
          MERGES := -1*MERGES;
        END(*IF*);
        STATE := 2;
      | 2:
        INC(ELEMIDX);
(* Make a leaf *)
        L1 := ELEMIDX;
        links[ELEMIDX] := ELEMIDX;
        links[SEQHEAD] := ELEMIDX;
(* L0 is head of older leaf *)
        L0 := SEQHEAD;
(* Head of most recent leaf *)
        INC(SEQHEAD);
        INC(ELEMIDX);
(* Make a leaf *)
        L2 := ELEMIDX;
        links[ELEMIDX] := ELEMIDX;
        links[SEQHEAD] := ELEMIDX;
        STATE := 4;
      | 4:
        IF (Strings.Compare(words[L1].s,words[L2].s) = Strings.less) OR
                                                    Strings.Equal(words[L1].s,words[L2].s) THEN  (* words[L1] <= words[L2] *)
          STATE := 5 (* switch to sequence 1 *)
        ELSE (* switch to sequence 2 *)
          links[L0] := L2;
          LOOP
            L0 := L2;
            L2 := links[L0]; (* next leaf *)
            IF L2 = L0 THEN (* switch to sequence 1 *)
              links[L0] := L1;
              STATE := 6;
              EXIT;
            END(*IF*);
            IF (Strings.Compare(words[L1].s,words[L2].s)=Strings.less) OR
                                Strings.Equal(words[L1].s,words[L2].s) THEN EXIT END(*IF*); (* words[L1] <= words[L2] *)
          END(*LOOP*);
          IF STATE (* still *) = 4 THEN
            links[L0] := L1;
            STATE := 5;
          END(*IF*);
        END(*IF*);
      | 5:
        L0 := L1;
        L1 := links[L0];
        IF L1 <> L0 THEN
          STATE := 4;
        ELSE
          links[L0] := L2;
          STATE := 6;
        END(*IF*);
      | 6:
        DEC(MERGES);
        IF MERGES > 0 THEN
          DEC(SEQHEAD);  (* Head of latest branch or twig *)
          L0 := SEQHEAD - 1;    (* Head of older branch or twig *)
          L1 := links[L0];      (* Head of sequence 1 *)
          L2 := links[SEQHEAD]; (* Head of sequence 2 *)
          STATE := 4;
        ELSIF MERGES = 0 THEN
          STATE := 1
        ELSE (* MERGES < 0 *)
(* Make 2nd half of 4-twig by re-engaging the # of merges *)
          MERGES := -1*MERGES + 1;
          STATE := 2;
        END(*IF*);
      END(*STATE MACHINE CASE*);
    END(*WHILE*);
  END TREESORT;

PROCEDURE fopen(VAR INOUT F:File; FILNAM:ARRAY OF CHAR; RDWRSTATE:IOSTATE);
(*
************************ fopen **************************************
File Open.
RDWRSTATE IS EITHER RD FOR OPEN A FILE FOR READING, OR WR FOR OPEN A FILE FOR WRITING.

*)

VAR I,RETCOD : CARDINAL;
    EOFFLG   : BOOLEAN;
    fileError: CommonFileErrors;
    filelength : CARDINAL32;

BEGIN
    CASE RDWRSTATE OF
      RD : OpenFile(F,FILNAM,ReadOnlyDenyWrite);
    | WR : CreateFile(F,FILNAM);  (*This proc truncates file before creation*)
    | APND : OpenCreateFile(F,FILNAM,ReadWriteDenyWrite);
    END(*CASE*);
    IF F.status <> 0 THEN
      WriteString(' Error in opening/creating file ');
      WriteString(FILNAM);
      WriteString('--');
      CASE TranslateFileError(F) OF
        FileErrFileNotFound : WriteString('File not found.');
      | FileErrDiskFull : WriteString('Disk Full');
      ELSE
        WriteString('Nonspecific error occured.');
      END(*CASE*);
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
      HALT;
    END(*IF F.status*);

    IF RDWRSTATE = APND THEN
      filelength := FileLength(F);
      MoveFilePos(F,filelength);
    END(*IF APND*);

END fopen;

PROCEDURE fclose(VAR INOUT F:File);
BEGIN
  CloseFile(F);
END fclose;


(* ************************* MAIN ***************************************************************)

BEGIN
  InName := "WordList.txt";
  OutputFileName := "WordListSrted.txt";
  NumberOfEntries := 0;

  c := 0;
  FLAG := BasicDialogs.PromptOpenFile(InName,'',c,'','','Open input text file',FALSE);
  IF NOT FLAG THEN
    Error('Could not open file.  Does it exist?');
    HALT;
  END;

  fopen(InFile,InName,RD);
  SetFileBuffer(InFile, InBuf);
  ParseFileName(InName, innameparts);

  outnameparts := innameparts;
  outnameparts.extension := '.sorted';
  AssembleParts(outnameparts, OutName);
  FLAG := BasicDialogs.PromptSaveAsFile(OutName,'',c,'','','Open output text file');
  IF NOT FLAG THEN
    Error('Could not open file.  ');
    HALT;
  END;

  fopen(OutFile,OutName,WR);
  SetFileBuffer(OutFile,OutBuf);

  LOOP  (* Read Loop *)
    ignoreCard := ReadLine(InFile,inputline);
    IF InFile.eof OR (NumberOfEntries >= maxEntries) THEN EXIT END;
    INC(NumberOfEntries);
    NEW(NewWordPointer);
    WordPointerArray[NumberOfEntries] := NewWordPointer;
    WordPointerArray[NumberOfEntries]^.s := inputline;
    WordPointerArray[NumberOfEntries]^.julian := 0.;
    WA[NumberOfEntries].s := inputline;  (* for treesort *)
    WA[NumberOfEntries].julian := 0.;    (* for treesort *)
  END; (* Loop for readline *)
  WriteString(' Number of Entries is ');
  WriteCard(NumberOfEntries);
  WriteLn;

  SortedWordPointerArray  := WordPointerArray;

  WriteString(' 1 -- heapsort');
  WriteLn;
  WriteString(' 2 -- Binary Insertion Sort');
  WriteLn;
  WriteString(' 3 -- Straight Insertion Sort');
  WriteLn;
  WriteString(' 4 -- qsort');
  WriteLn;
  WriteString(' 5 -- qsort2');
  WriteLn;
  WriteString(' 6 -- QUICKSORT');
  WriteLn;
  WriteString(' 7 -- TREESORT');
  WriteLn;
  WriteString(' enter number: ');
  Terminal.Read(ch);
  Terminal.Write(ch);
  WriteLn;
  GetClock(InitTime); (* Only timing the sort operation, not file or terminal I/O *)
  CASE ch OF                                                  (*        20,916 words | 75131 words *)
     '1' : heapsort(SortedWordPointerArray,1,NumberOfEntries); (*        0.015625 sec |  0.078125 sec *)
   | '2' : BinInsSort(SortedWordPointerArray,1,NumberOfEntries); (*      0.44141 sec  |  6.53906 sec *)
   | '3' : StraightInsSort(SortedWordPointerArray,1,NumberOfEntries); (* 1.53516 sec  | 70.83984 sec *)
   | '4' : qsort(SortedWordPointerArray,1,NumberOfEntries);  (*         12.82812 sec  |  0.062500 sec *)
   | '5' : qsort2(SortedWordPointerArray,1,NumberOfEntries); (*                       |  0.062500 sec *)
   | '6' : QUICKSORT(SortedWordPointerArray,1,NumberOfEntries);  (*                   |  0.062500 sec *)
   | '7' : TREESORT(WA,LA,NumberOfEntries);                    (*                     |  0.078125 sec *)
   ELSE
        WriteString(' Invalid entry.  Will do nothing.  Entry was: ');
        Terminal.Write(ch);
        WriteLn;

   END; (* case *)

   GetClock(FinalTime);


(* Write Loop *)
  IF ch = '7' THEN
  	c := LA[NumberOfEntries +1];
  	c1 := 0;
  	WHILE (c <= maxEntriesTree) AND ( c <> c1 ) DO
  		WriteLine(OutFile,WA[c].s);
  		c1 := c;
  		c := LA[c];
    END; (* while loop *)
  ELSE
    FOR c  := 1 TO NumberOfEntries DO
      WriteLine(OutFile,SortedWordPointerArray[c]^.s );
    END; (* output loop *)
  END; (* if ch=7 *)

  fclose(InFile);
  fclose(OutFile);
  PressAnyKey;

(*
  Will assume both times are on same day.  This will fail if testing is about midnight.
  And will assume the timing is < 1 hr.
*)
  WITH FinalTime DO
    FinalSec := FLOAT(3600*hour + 60*minute + second);
    FinalSec := FinalSec + FLOAT(fractions)/1000.;
  END; (* WITH FinalTime *)
  WITH InitTime DO
        InitSec := FLOAT(3600*hour + 60*minute + second);
        InitSec := InitSec + FLOAT(fractions)/1000.;
  END; (* WITH InitTime *)
  DiffSec := FinalSec - InitSec;
  WriteString(' Sort Time is ');
  WriteReal(DiffSec,5);
  WriteString(' seconds.');
  WriteLn;
  PressAnyKey;

END SortPointerStuff.
(*
MODULE SysClock
CONST
    maxSecondParts      = 999;  /*implementation dependent value*/
TYPE
  Month = [1..12]; Day = [1..31]; Hour = [0..23]; Min = [0..59]; Sec = [0..59]; Fraction = [0..maxSecondParts];
  UTCDiff     = [-780..720];

  DateTime = RECORD
    year : CARDINAL; month : Month; day : Day; hour : Hour; minute : Min; second : Sec;
        fractions       : Fraction; /* parts of a second */
        zone            : UTCDiff;  /* Time zone differential factor
                                       which is the number of minutes
                                       to add to local time to obtain
                                       UTC. */
        summerTimeFlag  : BOOLEAN;  /* interpretation depends on local usage */
        END;

/*
  NOTE: the zone and summerTimeFlag fields are supported on Win32 and Posix compliant Unix.
  summerTimeFlag can means daylight savings time or not
*/
PROCEDURE CanGetClock() : BOOLEAN;
PROCEDURE CanSetClock() : BOOLEAN;
PROCEDURE GetClock(VAR userData : DateTime);

DEFINITION MODULE SORTER;

EXPORT QUALIFIED MAXDIM,ITEMPTR,LINKSPTR,TREESORT,QUICKSORT,BININSSORT;

CONST
  MAXDIM = 10000;

TYPE
  INDEX      = INTEGER;
  ITEM       = CARDINAL;
  ITEMARRAY  = ARRAY [1..MAXDIM] OF ITEM;
  LINKSARRAY = ARRAY [1..MAXDIM+20]  OF INDEX;
  ITEMPTR    = POINTER TO ITEMARRAY;
  LINKSPTR   = POINTER TO LINKSARRAY;

PROCEDURE TREESORT(I : ITEMPTR; L : LINKSPTR; LASTELEM : CARDINAL);
(*
The LASTELEM input param is the actual last element used in its huge array.
This was the only way I could think of to allow this method to be in a
module of its own;
  This program creates a butterfly merge to combine leaves into twigs, twigs
into branches, and branches into one final linked list starting at position
LASTELEM+1.  In the end, LINKS(LASTELEM+1) points to the first leaf,
LINKS(LINKS(LASTELEM+1)) points to the second, LINKS(LINKS(LINKS(LASTELEM+1)))
points to the third, and the last link points to itself.
  The dimensions of LINKS needs to be >= LASTELEM + log2(LASTELEM) + 2, and
the procedure will check to make sure this condition is met.  If not, a
warning message is displayed on the terminal.

*)

PROCEDURE QUICKSORT(VAR AI : ARRAY OF ITEM; LASTITEM : INDEX);
(* Algorithm as implemented assumes that original array index starts at 1 *)

PROCEDURE BININSSORT(VAR AI : ARRAY OF ITEM; LASTITEM : INDEX);
(* Algorithm as implemented assumes that original array index starts at 1 *)

END SORTER.

IMPLEMENTATION MODULE SORTER;

(*
  REVISION HISTORY
  ----------------
  23 Dec 91 -- Converted to M2 V 4.00.
*)
  IMPORT Break;
(*  IMPORT DebugPMD;*)
  IMPORT MED;
  FROM Terminal IMPORT ReadString;
  IMPORT Terminal;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;

(*
  This sorting algorithm was written to use as few comparisons as possible,
to have as few steps btwn each comparison as possible, to take advantage of
natural sequencing, to preserve the order of equals (or even the reverse
order of equals), ie, to be stable, to use as little memory as possible (one
working array), and to be a modular, easily understood program written in
BASIC.  Unfortunately, the horrendous variable names in this program are from
this original BASIC listing of the program.
  The theory behind the algorithm may be described in a language of forests,
trees, branches, twigs and leaves.  There is a forest filled with trees of
different sizes.  Each tree is very orderly.  The trunk of a tree splits into
two branches of nearly the same size.  If one branch is larger than the
other, it is always the right-hand branch.  Similarly, each branch divides
into two more branches until the branches become twigs from which leaves grow.
The leaves are the individual elements to be sorted.
  This program creates a butterfly merge to combine leaves into twigs, twigs
into branches, and branches into one final linked list starting at position
ELEMCNT+1.  In the end, LINKS(ELEMCNT+1) points to the first leaf,
LINKS(LINKS(ELEMCNT+1)) points to the second, LINKS(LINKS(LINKS(ELEMCNT+1)))
points to the third, and the last link points to itself.
  Two things happen as the pgm jumps from twig to twig.  The leaves ahead of
the current record pointer get merged into a twig and the twigs and branches
behind this pointer get merged into larger branches.  The butterfly merge
treats each merge the same way.  The heads of each twig sequence are kept at
positions ELEMCNT+1, ELEMCNT+2, ..., ELEMCNT+log2(ELEMCNT)+2 after the links
themselves, which are kept in positions 1,2,3,...,ELEMCNT of array links.
  The merge takes the last 2 sequences created in the list and combines them
into one.  One wing of the merge follows sequence 1 and the other follows
sequence 2.  The two are interwoven until the final link points to itself.
Because the heads of each sequence are kept in the same array with the links
themselves, the merge is extraordinarily fast.  After each merge, the stack
of twig sequence heads has been reduced by one.
  Each time the current record pointer reaches a new twig, it generates new
sequences one item long to correspond to the leaves of that twig.  A two leaf
twig is produced by creating two one item sequences, each pointing to itself.
Then these two leaves are merged once.  A three leaf twig is created from
three one item sequences merged twice.  A four leaf twig is merged from two
two leaf twigs: the first two leaf twig is created and merged once; then the
number of remaining merge passes is set to a negative number so that the
merge will be disabled until the second two leaf twig is created and merged
with the first.
  After each complete twig has been generated, merging continues until the
branches behind the current record pointer have been linked together.  Then
it jumps to the next twig, generates new leaves and lets the butterfly merge
fly again.
  A state machine was the simplest way to implement all the branching of the
FORTRAN code.  An arbitrary numbering from 1 to 6 is used in which the
variable STATE represents this state machine's indicator.  A second minor
state machine had to be introduced as well, which uses the variable MODE as
its indicator.  The commented out numbers are the statement labels from my
FORTRAN source code listing.

*)

PROCEDURE TREESORT(I : ITEMPTR; L : LINKSPTR; LASTELEM : CARDINAL);

  VAR
    AK1,AK2,T2,T3,T4,SQNC1,      B1,B2     : LONGREAL;
    SEQHEAD,L0,L1,L2,STATE,MODE,ELEMIDX,T1 : CARDINAL;
    MERGES                                 : INTEGER;

  BEGIN
    AK1 := 0.;
    ELEMIDX := 0;
    MERGES := 0;
    T2 := 0.;
    T4 := 0.;
    SEQHEAD := LASTELEM + 1;
    L^[1] := 1;       (* That pesky zero origin arrays again *)
    L^[SEQHEAD] := 1;
    AK2 := 1.;
    SQNC1 := FLOAT(LASTELEM);
    WHILE SQNC1 >= 4. DO    (* Climb the tree *)
      AK2 := 2.*AK2;
      B2 := SQNC1/2.;
      SQNC1 := Int(B2);
      T4 := T4 + AK2*(B2-SQNC1);
    END(*WHILE*);
    T4 := AK2 - T4; (* T4 is the # of low order twigs *)
    B2 := AK2/2.;
    STATE := 1;
(* 13 *)
    WHILE (STATE > 1) OR (AK1 < AK2) DO (* Next twig.  Can only exit when *)
      CASE STATE OF                     (* STATE = 1 *)
      1:AK1 := AK1 + 1.;
        T1 := Round(AK1);
        B1 := B2;
        T3 := T2;
        WHILE NOT ODD(T1) DO
          INC(MERGES);
          T2 := T2 - B1;
          B1 := B1/2.;
          T1 := T1 DIV 2;
        END(*LOOP*);
(* Twig calculations *)
        T2 := T2 + B1;
        IF (SQNC1 = 2.) AND (T3 < T4) THEN
          INC(MERGES);  (* 2 twig *)
        ELSIF (SQNC1 = 2.) OR (T3 < T4) THEN
(* 5 *)   INC(MERGES);     (* 3 twig *)
          INC(ELEMIDX);
(* Make a leaf *)
          L^[ELEMIDX] := ELEMIDX;
          L^[SEQHEAD] := ELEMIDX;
(* Next sequence head *)
          INC(SEQHEAD);
          INC(MERGES);  (* 2 twig *)
        ELSE  (* 4 twig.  Disengage # of merges *)
          MERGES := -1*MERGES;
        END(*IF*);
        STATE := 2;
      | 2:
(* 7 *) INC(ELEMIDX);
(* Make a leaf *)
        L1 := ELEMIDX;
        L^[ELEMIDX] := ELEMIDX;
        L^[SEQHEAD] := ELEMIDX;
(* L0 is head of older leaf *)
        L0 := SEQHEAD;
(* Head of most recent leaf *)
        INC(SEQHEAD);
        INC(ELEMIDX);
(* Make a leaf *)
        L2 := ELEMIDX;
        L^[ELEMIDX] := ELEMIDX;
        L^[SEQHEAD] := ELEMIDX;
        STATE := 4;
      | 4:
(* 9 *) IF I^[L1] <= I^[L2] THEN
          STATE := 5 (* switch to sequence 1 *)
        ELSE (* switch to sequence 2 *)
          L^[L0] := L2;
(* 8 *)   LOOP
            L0 := L2;
            L2 := L^[L0]; (* next leaf *)
            IF L2 = L0 THEN (* switch to sequence 1 *)
              L^[L0] := L1;
              STATE := 6;
              EXIT;
            END(*IF*);
            IF (I^[L1] <= I^[L2]) THEN EXIT END(*IF*);
          END(*LOOP*);
          IF STATE (* still *) = 4 THEN
            L^[L0] := L1;
            STATE := 5;
          END(*IF*);
        END(*IF*);
      | 5:
(* 11 *)L0 := L1;
        L1 := L^[L0];
        IF L1 <> L0 THEN
          STATE := 4;
        ELSE
          L^[L0] := L2;
          STATE := 6;
        END(*IF*);
      | 6:
(* 10 *)DEC(MERGES);
        IF MERGES > 0 THEN
(* 12 *)  DEC(SEQHEAD);  (* Head of latest branch or twig *)
          L0 := SEQHEAD - 1;    (* Head of older branch or twig *)
          L1 := L^[L0];      (* Head of sequence 1 *)
          L2 := L^[SEQHEAD]; (* Head of sequence 2 *)
          STATE := 4;
        ELSIF MERGES = 0 THEN
          STATE := 1
        ELSE (* MERGES < 0 *)
(* Make 2nd half of 4-twig by re-engaging the # of merges *)
          MERGES := -1*MERGES + 1;
          STATE := 2;
        END(*IF*);
      END(*STATE MACHINE CASE*);
    END(*WHILE*);
  END TREESORT;
(* / R E V E R T *)

  PROCEDURE QUICKSORT(VAR AI : ARRAY OF ITEM; LASTITEM : INDEX);
(*  VAR LASTITEM : INDEX;*)

    PROCEDURE SORT(L,R : INDEX);
    VAR
      I,J  : INDEX;
      X,W  : ITEM;

    BEGIN
      IF R-L < 10 THEN
        BinInsSort(AI,L,R);
      ELSE
        I := L;
        J := R;
        X := AI[(L+R) DIV 2];

        REPEAT
          WHILE AI[I] < X DO INC(I); END(*WHILE*);
          WHILE X < AI[J] DO DEC(J); END(*WHILE*);
          IF I <= J THEN
            W := AI[I];
            AI[I] := AI[J];
            AI[J] := W;
            INC(I);
            DEC(J);
          END(*IF*);
        UNTIL I > J;
        IF L < J THEN SORT(L,J) END(*IF*);
        IF I < R THEN SORT(I,R) END(*IF*);
      END(*IF*);
    END SORT;

  BEGIN  (* QUICKSORT *)
(*    LASTITEM := HIGH(AI);*)
(*
  Algorithm as implemented assumes that original array index starts at 1,
  hence the need to subtract 1 from LASTITEM.
*)
    SORT(0,LASTITEM-1);
  END QUICKSORT;

    PROCEDURE BinInsSort(VAR AI : ARRAY OF ITEM; Left,Right : INDEX);
    VAR
      I,J,M,L,R : INDEX;
      X         : ITEM;
    BEGIN
      FOR I := Left+1 TO Right DO
        X := AI[I];
        L := Left;
        R := I;
        WHILE L < R DO
          M := (L+R) DIV 2;
          IF AI[M] <= X THEN
            L := M + 1
          ELSE
            R := M
          END(*IF*);
        END(*WHILE*);
        FOR J := I TO R+1 BY -1 DO
          AI[J] := AI[J-1];
        END(*FOR*);
        AI[R] := X
      END(*FOR*);
    END BinInsSort;

  PROCEDURE BININSSORT(VAR AI : ARRAY OF ITEM; LASTITEM : INDEX);
(*
  VAR
    LASTITEM  : INDEX;
*)

  BEGIN
(*    LASTITEM := HIGH(AI);*)
(*
  Algorithm as implemented assumes that original array index starts at 1,
  hence the need to subtract 1 from LASTITEM.
*)
    BinInsSort(AI,0,LASTITEM-1);
  END BININSSORT;


END SORTER.

Below is orig version OF Sorter that has other algorithms that I did NOT
debug when I converted this TO Logitech M2 V4.  Time TO DO so now.

IMPLEMENTATION MODULE SORTER;

(*
  REVISION HISTORY
  ----------------
*)
  IMPORT Break;
  IMPORT DebugPMD;
  FROM Terminal IMPORT ReadString;
  IMPORT Terminal;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM MathLib0 IMPORT real,exp,ln;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;

(*
  This sorting algorithm was written to use as few comparisons as possible,
to have as few steps btwn each comparison as possible, to take advantage of
natural sequencing, to preserve the order of equals (or even the reverse
order of equals), ie, to be stable, to use as little memory as possible (one
working array), and to be a modular, easily understood program written in
BASIC.  Unfortunately, the horrendous variable names in this program are from
this original BASIC listing of the program.
  The theory behind the algorithm may be described in a language of forests,
trees, branches, twigs and leaves.  There is a forest filled with trees of
different sizes.  Each tree is very orderly.  The trunk of a tree splits into
two branches of nearly the same size.  If one branch is larger than the
other, it is always the right-hand branch.  Similarly, each branch divides
into two more branches until the branches become twigs from which leaves grow.
The leaves are the individual elements to be sorted.
  This program creates a butterfly merge to combine leaves into twigs, twigs
into branches, and branches into one final linked list starting at position
ELEMCNT+1.  In the end, LINKS(ELEMCNT+1) points to the first leaf,
LINKS(LINKS(ELEMCNT+1)) points to the second, LINKS(LINKS(LINKS(ELEMCNT+1)))
points to the third, and the last link points to itself.
  Two things happen as the pgm jumps from twig to twig.  The leaves ahead of
the current record pointer get merged into a twig and the twigs and branches
behind this pointer get merged into larger branches.  The butterfly merge
treats each merge the same way.  The heads of each twig sequence are kept at
positions ELEMCNT+1, ELEMCNT+2, ..., ELEMCNT+log2(ELEMCNT)+2 after the links
themselves, which are kept in positions 1,2,3,...,ELEMCNT of array links.
  The merge takes the last 2 sequences created in the list and combines them
into one.  One wing of the merge follows sequence 1 and the other follows
sequence 2.  The two are interwoven until the final link points to itself.
Because the heads of each sequence are kept in the same array with the links
themselves, the merge is extraordinarily fast.  After each merge, the stack
of twig sequence heads has been reduced by one.
  Each time the current record pointer reaches a new twig, it generates new
sequences one item long to correspond to the leaves of that twig.  A two leaf
twig is produced by creating two one item sequences, each pointing to itself.
Then these two leaves are merged once.  A three leaf twig is created from
three one item sequences merged twice.  A four leaf twig is merged from two
two leaf twigs: the first two leaf twig is created and merged once; then the
number of remaining merge passes is set to a negative number so that the
merge will be disabled until the second two leaf twig is created and merged
with the first.
  After each complete twig has been generated, merging continues until the
branches behind the current record pointer have been linked together.  Then
it jumps to the next twig, generates new leaves and lets the butterfly merge
fly again.
  A state machine was the simplest way to implement all the branching of the
FORTRAN code.  An arbitrary numbering from 1 to 6 is used in which the
variable STATE represents this state machine's indicator.  A second minor
state machine had to be introduced as well, which uses the variable MODE as
its indicator.  The commented out numbers are the statement labels from my
FORTRAN source code listing.

*)

PROCEDURE TREESORT(LASTELEM : CARDINAL);

  VAR
    AK1,AK2,T2,T3,T4,SQNC1,      B1,B2     : LONGREAL;
    SEQHEAD,L0,L1,L2,STATE,MODE,ELEMIDX,T1 : CARDINAL;
    MERGES                                 : INTEGER;

  BEGIN
    AK1 := 0.;
    ELEMIDX := 0;
    MERGES := 0;
    T2 := 0.;
    T4 := 0.;
    SEQHEAD := LASTELEM + 1;
    LINKS[1] := 1;       (* That pesky zero origin arrays again *)
    LINKS[SEQHEAD] := 1;
    AK2 := 1.;
    SQNC1 := FLOAT(LASTELEM);
    WHILE SQNC1 >= 4. DO    (* Climb the tree *)
      AK2 := 2.*AK2;
      B2 := SQNC1/2.;
      SQNC1 := Int(B2);
      T4 := T4 + AK2*(B2-SQNC1);
    END(*WHILE*);
    T4 := AK2 - T4; (* T4 is the # of low order twigs *)
    B2 := AK2/2.;
    STATE := 1;
(* 13 *)
    WHILE (STATE > 1) OR (AK1 < AK2) DO (* Next twig.  Can only exit when *)
      CASE STATE OF                     (* STATE = 1 *)
      1:AK1 := AK1 + 1.;
        T1 := Round(AK1);
        B1 := B2;
        T3 := T2;
        WHILE NOT ODD(T1) DO
          INC(MERGES);
          T2 := T2 - B1;
          B1 := B1/2.;
          T1 := T1 DIV 2;
        END(*LOOP*);
(* Twig calculations *)
        T2 := T2 + B1;
        IF (SQNC1 = 2.) AND (T3 < T4) THEN
          INC(MERGES);  (* 2 twig *)
        ELSIF (SQNC1 = 2.) OR (T3 < T4) THEN
(* 5 *)   INC(MERGES);     (* 3 twig *)
          INC(ELEMIDX);
(* Make a leaf *)
          LINKS[ELEMIDX] := ELEMIDX;
          LINKS[SEQHEAD] := ELEMIDX;
(* Next sequence head *)
          INC(SEQHEAD);
          INC(MERGES);  (* 2 twig *)
        ELSE  (* 4 twig.  Disengage # of merges *)
          MERGES := -1*MERGES;
        END(*IF*);
        STATE := 2;
      | 2:
(* 7 *) INC(ELEMIDX);
(* Make a leaf *)
        L1 := ELEMIDX;
        LINKS[ELEMIDX] := ELEMIDX;
        LINKS[SEQHEAD] := ELEMIDX;
(* L0 is head of older leaf *)
        L0 := SEQHEAD;
(* Head of most recent leaf *)
        INC(SEQHEAD);
        INC(ELEMIDX);
(* Make a leaf *)
        L2 := ELEMIDX;
        LINKS[ELEMIDX] := ELEMIDX;
        LINKS[SEQHEAD] := ELEMIDX;
        STATE := 4;
      | 4:
(* 9 *) IF ITEMARRAY[L1] <= ITEMARRAY[L2] THEN
          STATE := 5 (* switch to sequence 1 *)
        ELSE (* switch to sequence 2 *)
          LINKS[L0] := L2;
(* 8 *)   LOOP
            L0 := L2;
            L2 := LINKS[L0]; (* next leaf *)
            IF L2 = L0 THEN (* switch to sequence 1 *)
              LINKS[L0] := L1;
              STATE := 6;
              EXIT;
            END(*IF*);
            IF (ITEMARRAY[L1] <= ITEMARRAY[L2]) THEN EXIT END(*IF*);
          END(*LOOP*);
          IF STATE (* still *) = 4 THEN
            LINKS[L0] := L1;
            STATE := 5;
          END(*IF*);
        END(*IF*);
      | 5:
(* 11 *)L0 := L1;
        L1 := LINKS[L0];
        IF L1 <> L0 THEN
          STATE := 4;
        ELSE
          LINKS[L0] := L2;
          STATE := 6;
        END(*IF*);
      | 6:
(* 10 *)DEC(MERGES);
        IF MERGES > 0 THEN
(* 12 *)  DEC(SEQHEAD);  (* Head of latest branch or twig *)
          L0 := SEQHEAD - 1;    (* Head of older branch or twig *)
          L1 := LINKS[L0];      (* Head of sequence 1 *)
          L2 := LINKS[SEQHEAD]; (* Head of sequence 2 *)
          STATE := 4;
        ELSIF MERGES = 0 THEN
          STATE := 1
        ELSE (* MERGES < 0 *)
(* Make 2nd half of 4-twig by re-engaging the # of merges *)
          MERGES := -1*MERGES + 1;
          STATE := 2;
        END(*IF*);
      END(*STATE MACHINE CASE*);
    END(*WHILE*);
  END TREESORT;

  PROCEDURE QUICKSORT(VAR AI : ARRAY OF ITEM);
  VAR LASTITEM : INDEX;

    PROCEDURE SORT(L,R : INDEX);
    VAR
      I,J  : INDEX;
      X,W  : ITEM;

    BEGIN
      IF R-L < 10 THEN
        BinInsSort(AI,L,R);
      ELSE
        I := L;
        J := R;
        X := AI[(L+R) DIV 2];

        REPEAT
          WHILE AI[I] < X DO INC(I); END(*WHILE*);
          WHILE X < AI[J] DO DEC(J); END(*WHILE*);
          IF I <= J THEN
            W := AI[I];
            AI[I] := AI[J];
            AI[J] := W;
            INC(I);
            DEC(J);
          END(*IF*);
        UNTIL I > J;
        IF L < J THEN SORT(L,J) END(*IF*);
        IF I < R THEN SORT(I,R) END(*IF*);
      END(*IF*);
    END SORT;

  BEGIN  (* QUICKSORT *)
    LASTITEM := HIGH(AI);
    SORT(0,LASTITEM);
  END QUICKSORT;

  PROCEDURE INDIRQUICKSORT(AI : ARRAY OF ITEM; VAR POINTERS : ARRAY OF INDEX);
  VAR LASTITEM : INDEX;
(*      C        : CARDINAL;*)

    PROCEDURE INDIRSORT(L,R : INDEX);
    VAR
      I,J,W  : INDEX;
      X      : ITEM;

    BEGIN
      IF R-L < 10 THEN
        IndirBinInsSort(AI,POINTERS,L,R);
      ELSE
        I := L;
        J := R;
        X := AI[POINTERS[(L+R) DIV 2]];

        REPEAT
          WHILE AI[POINTERS[I]] < X DO INC(I); END(*WHILE*);
          WHILE X < AI[POINTERS[J]] DO DEC(J); END(*WHILE*);
          IF I <= J THEN
            W := POINTERS[I];
            AI[I] := POINTERS[J];
            AI[J] := W;
            INC(I);
            DEC(J);
          END(*IF*);
        UNTIL I > J;
        IF L < J THEN INDIRSORT(L,J) END(*IF*);
        IF I < R THEN INDIRSORT(I,R) END(*IF*);
      END(*IF*);
    END INDIRSORT;

  BEGIN  (* QUICKSORT *)
    LASTITEM := HIGH(AI);
(*    FOR C := 0 TO LASTITEM DO POINTERS[C] := C; END(*FOR*);*)
    INDIRSORT(0,LASTITEM);
  END INDIRQUICKSORT;

    PROCEDURE BinInsSort(VAR AI : ARRAY OF ITEM; Left,Right : INDEX);
    VAR
      I,J,M,L,R : INDEX;
      X         : ITEM;
    BEGIN
      FOR I := Left+1 TO Right DO
        X := AI[I];
        L := Left;
        R := I;
        WHILE L < R DO
          M := (L+R) DIV 2;
          IF AI[M] <= X THEN
            L := M + 1
          ELSE
            R := M
          END(*IF*);
        END(*WHILE*);
        FOR J := I TO R+1 BY -1 DO
          AI[J] := AI[J-1];
        END(*FOR*);
        AI[R] := X
      END(*FOR*);
    END BinInsSort;

  PROCEDURE BININSSORT(VAR AI:ARRAY OF ITEM);
  VAR
    LASTITEM  : INDEX;

  BEGIN
    LASTITEM := HIGH(AI);
    BinInsSort(AI,0,LASTITEM);
  END BININSSORT;

    PROCEDURE IndirBinInsSort(VAR AI : ARRAY OF ITEM;
                          VAR POINTERS : ARRAY OF INDEX; Left,Right : INDEX);
    VAR
      I,J,M,L,R : INDEX;
      X         : ITEM;
    BEGIN
      FOR I := Left+1 TO Right DO
        X := AI[POINTERS[I]];
        L := Left;
        R := I;
        WHILE L < R DO
          M := (L+R) DIV 2;
          IF AI[POINTERS[M]] <= X THEN
            L := M + 1
          ELSE
            R := M
          END(*IF*);
        END(*WHILE*);
        FOR J := I TO R+1 BY -1 DO
          POINTERS[J] := POINTERS[J-1];
        END(*FOR*);
        POINTERS[R] := I;
      END(*FOR*);
    END IndirBinInsSort;

  PROCEDURE INDIRBININSSORT(AI:ARRAY OF ITEM; VAR POINTERS:ARRAY OF INDEX);
  VAR
    LASTITEM  : INDEX;
(*    C         : CARDINAL; *)

  BEGIN
    LASTITEM := HIGH(AI);
(*    FOR C := 0 TO LASTITEM DO POINTERS[C] := C END(*FOR*); *)
    IndirBinInsSort(AI,POINTERS,0,LASTITEM);
  END INDIRBININSSORT;

(* ************ From Sincovec et al ******************* *)

       PROCEDURE swap
               ( VAR p, q : ITEM                      (* in/out *) );
         VAR temp : ITEM;
       BEGIN
         temp := p;
         p := q;
         q := temp
       END swap;

       PROCEDURE mergesort
               ( VAR items : ARRAY OF ITEM            (* in/out *) );
(*                     size  : CARDINAL                 (* in *);*)
(*                     order : ordertype                (* in *) );*)

         VAR size : CARDINAL;

         PROCEDURE merge
                 ( VAR x, z  : ARRAY OF ITEM          (* in/out *);
                       low, middle, high : INTEGER    (* in *) );
           VAR i, k, j, p : INTEGER;
         BEGIN
           i := low;
           k := low;
           j := middle + 1;
           WHILE ( i <= middle ) AND ( j <= high ) DO
             IF x[ i ] <= x[ j ]
             THEN
               z[ k ] := x[ i ];
               INC( i )
             ELSE
               z[ k ] := x[ j ];
               INC( j )
             END (* if then *);
             INC( k )
           END (* while loop *);
           IF i > middle
           THEN
             FOR p := j TO high DO
               z[ k ] := x[ p ];
               INC( k )
             END (* for loop *);
           ELSE
             FOR p := i TO middle DO
               z[ k ] := x[ p ];
               INC( k )
             END (* for loop *);
           END (* if then *);
         END merge;

         PROCEDURE mpass
                   ( VAR x, z : ARRAY OF ITEM         (* in /out *);
                         n, size : INTEGER            (* in *) );
           VAR i, k : INTEGER;
         BEGIN
           i := 0;
           WHILE  i  <= n - 2 * size DO
             merge( x, z, i, i+size-1, i+2*size-1 );
             INC( i, 2*size )
           END (* while loop *);
           IF i + size - 1 < n
           THEN
             merge( x, z, i, i+size-1, n )
           ELSE
             FOR k := i TO n DO
               z[ k ] := x[ k ]
             END (* for loop *);
           END (* if then *)
         END mpass;

         PROCEDURE msort
                 ( VAR items : ARRAY OF ITEM          (* in/out *);
                       copy  : ARRAY OF ITEM          (* in *) );
           VAR number, n  : INTEGER;
         BEGIN
           n := size - 1;
           number := 1;
           WHILE number < n + 1 DO
             mpass( items, copy, n, number );
             INC( number, number );
             mpass( copy, items, n, number );
             INC( number, number )
           END (* while loop *)
         END msort;

       BEGIN
         size := HIGH(items) + 1;
         msort( items, items )
       END mergesort;



       PROCEDURE heapsort
               ( VAR items : ARRAY OF ITEM           (* in/out *) );
(*                     size  : CARDINAL                 (* in *);*)
(*                     order : ordertype               (* in *) );*)

       VAR index, number : INTEGER;
           size          : CARDINAL;

         PROCEDURE siftup
                 ( n : INTEGER                        (* in *) );
           VAR i, p : INTEGER;
               done : BOOLEAN;
         BEGIN
           i := n;
           done := FALSE;
           WHILE ( i > 0 ) AND NOT done DO
             p := (i - 1) DIV 2;
             IF items[ i ] <= items[ p ]
             THEN
               done := TRUE
             ELSE
               swap( items[ i ], items[ p ] );
               i := p
             END (* end if *)
           END (* end while *)
         END siftup;

         PROCEDURE siftdown
                 ( n : INTEGER                        (* in *) );
           VAR i, c : INTEGER;
               done : BOOLEAN;
         BEGIN
           i := 0;
           c := 0;
           done := FALSE;
           WHILE ( c < n ) AND NOT done DO
             c := 2 * i + 1;
             IF c <= n
             THEN
               IF ( c < n ) AND ( items[ c ] <= items[ c+1 ] )
               THEN
                 INC( c )
               END (* if then *);
               IF items[ c ] <= items[ i ]
               THEN
                 done := TRUE
               ELSE
                 swap( items[ c ], items[ i ] );
                 i := c
               END (* if then *);
             END (* if then *);
           END (* while *);
         END siftdown;

       BEGIN
         size := HIGH(items) + 1;
         number := size - 1;
         FOR index := 1 TO number DO
           siftup( index )
         END (* for loop *);
         FOR index := number TO 1 BY -1 DO
           swap( items[ 0 ], items[ index ] );
           siftdown( index - 1 )
         END (* for loop *)
       END heapsort;



END SORTER.
*)
