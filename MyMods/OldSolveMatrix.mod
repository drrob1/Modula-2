MODULE SolveMatrixFromFile;

        (********************************************************)
        (*                                                      *)
        (*              Test of Matrices module                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        15 August 1996                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)
(* 
  REVISION
  --------
   2 Mar 05 -- Added prompts to remind me of the file format
*)

FROM SYSTEM IMPORT FUNC;

FROM Mat IMPORT
    (* proc *)  Zero, Write, Add, Sub, Mul,
                Random, Solve, GaussJ, Invert, Eigenvalues;

FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, WinAttr,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, WindowTypes, SpecialKeys,
    GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, (* WriteString, *)
    WriteStringAt, WriteCellsAt, WriteCells, (* WriteLn,*) EraseToEOL, ChangeAttr,
    ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn,
    SetAutoScroll, WinShellToTextWindowMessage,
    MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect,
    GetBufferRect, EraseScreen, EraseRect, GetWinShellHandle, FindTextWindow,
    SetDisplayMode,GetDisplayMode,SetWindowEnable,
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage,
    IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars,
    SetScrollBarPos, SetWindowData, SetWindowDataNum, GetWindowData, GetWindowDataNum,
    GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos, CascadeWindow,
    SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo,
    SetScrollDisableWhenNone, SetActiveTabChild, SetTabChildPosition,
    GetForegroundWindow, SetForegroundWindow,
    SetTimer, KillTimer, DisplayHelp,
    Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;

FROM MiscM2 IMPORT WriteCx, SelectWindow, WriteString, WriteLn, PressAnyKey,
                   ReadCard, WriteCard, Error, CLS;

IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;

  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRBL,FWRSTR,FWRLN,FAPPEND;
(****************************************************************************)

  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
(*  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard; *)
(*  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine; *)
  FROM Terminal IMPORT Read, (*WriteString, WriteLn, Write, *) ReadChar, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

CONST MaxN = 5;

TYPE MaxRealArray = ARRAY [1..MaxN],[1..MaxN] OF LONGREAL;

VAR
  ch1,ch2,ch3 :  CHAR;
  bool,inputprocessed :  BOOLEAN;
  sigfig,c1,c2,c3,N   :  CARDINAL;
  inputline,OpenFileName,str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  longstr     : ARRAY [0..5120] OF CHAR;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf    : BUFTYP;
  r           : LONGREAL;
  L           : LONGINT;
  LC          : LONGCARD;
  InFile      : MYFILTYP;
  mybuf,token : BUFTYP;
  tknstate    : FSATYP;
  c,retcod,row,col    : CARDINAL;
  i           : LONGINT;
  ra1,ra2,ra3,ra4,ans : MaxRealArray;



(************************************************************************

PROCEDURE BasicTest;
    CONST Arows = 2;  Acols = 3;
          Brows = 3;  Bcols = 2;
    VAR A, D, E: ARRAY [1..Arows],[1..Acols] OF LONGREAL;
        B, C: ARRAY [1..Brows],[1..Bcols] OF LONGREAL;

    /* Checks some simple matrix operations. */


    BEGIN
        /*
        OpenWindow (w, yellow, blue, 1, 23, 10, 69, simpleframe, nodivider);
        SelectWindow (w);
        */
        Reset;
        WriteString ("TEST OF SIMPLE MATRIX OPERATIONS");
        WriteLn;  WriteLn;

        /* Give a value to the A matrix. */

        Random (A, Arows, Acols);
        WriteString ("Matrix A is");  WriteLn;
        Write (A, Arows, Acols, 5);

        /* Give a value to the B matrix. */

        Random (B, Brows, Bcols);
        WriteString ("Matrix B is");  WriteLn;
        Write (B, Brows, Bcols, 5);

        /* Try an addition (it will fail). */

        WriteString ("We can't compute A+B");  WriteLn;

        /* Try a multiplication (it should work). */

        Mul (A, B, Arows, Acols, Bcols, C);
        WriteString ("C = A*B is");  WriteLn;
        Write (C, Arows, Bcols, 5);

        /* Give a value to the D matrix. */

        Random (D, Arows, Acols);
        WriteString ("Matrix D is");  WriteLn;
        Write (D, Arows, Acols, 5);

        /* Try another addition (this one should work). */

        Add (A, D, Arows, Acols, E);
        WriteString ("E = A+D is");  WriteLn;
        Write (E, Arows, Acols, 5);

        PressAnyKey;

    END BasicTest;

************************************************************************

PROCEDURE SolveTest;

    /* Solution of a linear equation. */

    CONST Arows = 4;  Acols = 4;
          Brows = 4;  Bcols = 2;

    VAR A: ARRAY [1..Arows],[1..Acols] OF LONGREAL;
        B, C, D, X: ARRAY [1..Brows],[1..Bcols] OF LONGREAL;

    BEGIN
        Reset;
        WriteString ("SOLVING LINEAR ALGEBRAIC EQUATIONS");
        WriteLn;

        /* Give a value to the A matrix. */

        Random (A, Arows, Acols);
        WriteString ("Matrix A is");  WriteLn;
        Write (A, Arows, Acols, 4);

        /* Give a value to the B matrix. */

        Random (B, Brows, Bcols);
        WriteString ("Matrix B is");  WriteLn;
        Write (B, Brows, Bcols, 4);

        /* Solve the equation AX = B. */

        Solve (A, B, X, Arows, Bcols);
        /*GaussJ (A, B, X, Arows, Bcols);*/

        /* Write the solution. */

        WriteString ("The solution X to AX = B is");  WriteLn;
        Write (X, Brows, Bcols, 4);

        /* Check that the solution looks right. */

        Mul (A, X, Arows, Acols, Bcols, C);
        Sub (B, C, Brows, Bcols, D);
        WriteString ("As a check, AX-B evaluates to");  WriteLn;
        Write (D, Brows, Bcols, 4);

        PressAnyKey;

    END SolveTest;

************************************************************************

PROCEDURE SingularTest;

    /* Linear equation with singular coefficient matrix. */

    CONST Arows = 2;  Acols = 2;
          Brows = 2;  Bcols = 1;

    VAR A: ARRAY [1..Arows],[1..Acols] OF LONGREAL;
        B, X: ARRAY [1..Brows],[1..Bcols] OF LONGREAL;
     
    BEGIN
        /*
        OpenWindow (w, black, brown, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        */
        Reset;
        WriteString ("A SINGULAR PROBLEM");
        WriteLn;

        /* Give a value to the A matrix. */

        A[1,1] := 1.0;
        A[1,2] := 2.0;
        A[2,1] := 2.0;
        A[2,2] := 4.0;
        WriteString ("Matrix A is");  WriteLn;
        Write (A, Arows, Acols, 4);

        /* Give a value to the B matrix. */

        Random (B, Brows, Bcols);
        WriteString ("Matrix B is");  WriteLn;
        Write (B, Brows, Bcols, 4);

        /* Try to solve the equation AX = B. */

        Solve (A, B, X, Arows, Bcols);

        WriteString ("The equation AX = B could not be solved");  WriteLn;

        PressAnyKey;

    END SingularTest;

************************************************************************

PROCEDURE InversionTest;

    /* Inverting a matrix, also an eigenvalue calculation. */

    CONST N = 5;

    VAR A, B, X: ARRAY [1..N],[1..N] OF LONGREAL;
        W: ARRAY [1..N] OF LONGCOMPLEX;
        j: CARDINAL;

    BEGIN
        /*
        OpenWindow (w, yellow, brown, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        */
        Reset;
        WriteString ("INVERTING A SQUARE MATRIX");
        WriteLn;

        /* Give a value to the A matrix. */

        Random (A, N, N);
        WriteString ("Matrix A is");  WriteLn;
        Write (A, N, N, 4);

        /* Invert it. */

        Invert (A, X, N);

        /* Write the solution. */

        WriteLn;
        WriteString ("The inverse of A is");  WriteLn;
        Write (X, N, N, 4);

        /* Check that the solution looks right. */

        Mul (A, X, N, N, N, B);
        WriteLn;
        WriteString ("As a check, the product evaluates to");  WriteLn;
        Write (B, N, N, 4);

        PressAnyKey;
        Reset;

        WriteLn;  WriteString ("EIGENVALUES");  WriteLn;
        WriteString ("The eigenvalues of A are");  WriteLn;
        WriteString('This does not work at this time, so nevermind.');
        WriteLn;
/*
        Eigenvalues (A, W, N);
        FOR j := 1 TO N DO
            WriteString ("    ");  WriteCx (W[j], 5);  WriteLn;
        END /*FOR*/;

        PressAnyKey;
        WriteString ("The eigenvalues of its inverse are");  WriteLn;
        Eigenvalues (X, W, N);
        FOR j := 1 TO N DO
            WriteString ("    ");  WriteCx (W[j], 5);  WriteLn;
        END /*FOR*/;
*/
        PressAnyKey;

    END InversionTest;
*)
(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
(*
  PROCEDURE PromptOpenFile(VAR INOUT name : ARRAY OF CHAR;
                         filters : ARRAY OF CHAR;
                         VAR INOUT defFilter : CARDINAL;
                         defDir : ARRAY OF CHAR;
                         defExt : ARRAY OF CHAR;
                         title : ARRAY OF CHAR;
                         createable : BOOLEAN) : BOOLEAN;
*)
(* Opens an operating system common dialog for opening  a file
   filters specifies a list of file extension filters that are
   separated by semicolons.
   The format for filters is as follows.
   defDir = the default directory to start the dialog in
   an empty string "" means use the current directory.
   defExt = the default file extension to use if the user does not
   provide an extension. "" means no default extension.
   the extension should *not* have a leading '.' character.
   title = the caption text of the dialog. title can be empty "".
   in this case the default operating system title is used.
   If createable = TRUE then the file need not already exist, otherwise
   the file must exist for the dialog to return successful.
   RETURNs TRUE is successful and name will contain the file specification
   for the file the user has given.
*)
    Reset;
    c3 := 0;
    WriteString(' This program solves vector equation A X = B where A is coef matrix and is square.');
    WriteLn;
    WriteString(' Line 1 is N');
    WriteLn;
    WriteString(' Line 2..N is rows of coefficient matrix. ');
    WriteLn;
    WriteString(' Last line is B values.');
    WriteLn;
    PressAnyKey;
    bool := BasicDialogs.PromptOpenFile(OpenFileName,'',c3,'','','Open matrix values as a text file',FALSE);
    IF NOT bool THEN
      Error('Could not open file.  Does it exist?');
      HALT;
    END;
    BasicDialogs.MessageBox(OpenFileName,MsgInfo);
(*
    N := 3;
    FUNC BasicDialogs.PromptCard('Input number of unknowns',1,10,FALSE,N);
    WriteString('N=');
    WriteCard(N);
    WriteLn;
    WriteString('Test my own readcard proc.  Enter card :');
    ReadCard(c1);
    WriteLn;
    WriteString('My own readcard proc says card=');
    WriteCard(c1);
    WriteLn;
    PressAnyKey;
*)
    ASSIGN2BUF(OpenFileName,mybuf);
    FRESET(InFile,mybuf,RD);
    FRDTXLN(InFile,inputbuf,80,bool);
(*
    WriteString('First line of file ');
    WriteString(OpenFileName);
    WriteString(' is:');
    WriteLn;
    WriteString(inputbuf.CHARS);
    WriteLn;
*)
    INI1TKN(inputbuf);
    GETTKN(token,tknstate,i,retcod);
    N := i;
    WriteString('From file N = ');
    WriteCard(N);
    WriteLn;
    FOR row := 1 TO N DO
        FRDTXLN(InFile,inputbuf,80,bool);
(*
        WriteString('reading lines from file. row= ');
        WriteCard(row);
        WriteLn;
*)
        IF bool THEN
          Error('Reading from file and got an unexpected EOF.');
          WriteLn;
          HALT;
        END;
        INI1TKN(inputbuf);
        FOR col := 1 TO N DO
          GETTKNREAL(token,tknstate,i,r,retcod);
          IF retcod > 0 THEN
            Error('Parsing file for matrix loop.  GETTKNREAL Retcod > 0');
            WriteLn;
            HALT;
          END;
          ra1[row,col] := r;
        END (*for col*);
    END (*for row*);
(* Now need to read right hand size array *)
    FRDTXLN(InFile,inputbuf,80,bool);
(* Don't test here because this should be the last line of the file.
    IF NOT bool THEN
      Error('Reading from file and got an error.');
      WriteLn;
      HALT;
    END;
*)
    INI1TKN(inputbuf);
    FOR row := 1 TO N DO
      GETTKNREAL(token,tknstate,i,r,retcod);
      IF retcod > 0 THEN
        Error('Parsing file for matrix loop.  GETTKNREAL Retcod > 0');
        WriteLn;
        HALT;
      END;
      ra2[row,1] := r;
    END (*for col*);
    WriteString(' coef matrix is:');
    WriteLn;
    Write(ra1,N,N,3);
    WriteLn;
    WriteString(' Right hand side vector matrix is:');
    WriteLn;
    Write(ra2,N,1,3);
    WriteLn;

    PressAnyKey;

    CLS;

    Solve (ra1, ra2, ans, N, 1); 

    WriteString ("The solution X to AX = B is");  WriteLn;
    Write (ans, N, 1, 3);
    PressAnyKey;

(* Check that the solution looks right. *)

    Mul (ra1, ans, N, N, 1, ra3);
    Sub (ra3, ra2, N, 1, ra4);
    WriteString ("As a check, AX-B evaluates to");  WriteLn;
    Write (ra4, N, 1, 4);

    PressAnyKey;


(* I know this works.  Time to stop doing it.
    BasicTest;
    SolveTest;
    SingularTest;
    InversionTest;
*)
    FCLOSE(InFile);
END SolveMatrixFromFile.
(*
  PROCEDURE PromptSaveAsFile(VAR INOUT name : ARRAY OF CHAR;
                           filters : ARRAY OF CHAR;
                           VAR INOUT defFilter : CARDINAL;
                           defDir : ARRAY OF CHAR;
                           defExt : ARRAY OF CHAR;
                           title : ARRAY OF CHAR;
                           overwritePrompt : BOOLEAN) : BOOLEAN;
*)
(* As PromptOpenFile except this dialog is for saving a file.
   If overwritePrompt = TRUE then the user will be prompted when a file with
   the file name they enter already exists. They must answer yes to this
   dialog to continue and has this function return TRUE
*)
