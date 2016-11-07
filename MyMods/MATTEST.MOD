MODULE MatTest;

        (********************************************************)
        (*                                                      *)
        (*              Test of Matrices module                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        15 August 1996                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

(* IMPORT Cx; *)

FROM Mat IMPORT
    (* proc *)  Zero, Write, Add, Sub, Mul,
                Random, Solve, GaussJ, Invert, Eigenvalues;

(*
FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow;
*)

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

FROM MiscM2 IMPORT WriteCx, CLS, SelectWindow, WriteString, WriteLn, PressAnyKey;

IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
(****************************************************************************)

  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
(*  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine; *)
  FROM Terminal IMPORT Read, (*WriteString, WriteLn, Write, *) ReadChar, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

(************************************************************************)

PROCEDURE BasicTest;

    (* Checks some simple matrix operations. *)

    CONST Arows = 2;  Acols = 3;
          Brows = 3;  Bcols = 2;

    VAR A, D, E: ARRAY [1..Arows],[1..Acols] OF LONGREAL;
        B, C: ARRAY [1..Brows],[1..Bcols] OF LONGREAL;
        (*w: Window;*)

    BEGIN
        (*
        OpenWindow (w, yellow, blue, 1, 23, 10, 69, simpleframe, nodivider);
        SelectWindow (w);
        *)
        Reset;
        WriteString ("TEST OF SIMPLE MATRIX OPERATIONS");
        WriteLn;  WriteLn;

        (* Give a value to the A matrix. *)

        Random (A, Arows, Acols);
        WriteString ("Matrix A is");  WriteLn;
        Write (A, Arows, Acols, 5);

        (* Give a value to the B matrix. *)

        Random (B, Brows, Bcols);
        WriteString ("Matrix B is");  WriteLn;
        Write (B, Brows, Bcols, 5);

        (* Try an addition (it will fail). *)

        WriteString ("We can't compute A+B");  WriteLn;

        (* Try a multiplication (it should work). *)

        Mul (A, B, Arows, Acols, Bcols, C);
        WriteString ("C = A*B is");  WriteLn;
        Write (C, Arows, Bcols, 5);

        (* Give a value to the D matrix. *)

        Random (D, Arows, Acols);
        WriteString ("Matrix D is");  WriteLn;
        Write (D, Arows, Acols, 5);

        (* Try another addition (this one should work). *)

        Add (A, D, Arows, Acols, E);
        WriteString ("E = A+D is");  WriteLn;
        Write (E, Arows, Acols, 5);

        PressAnyKey;
        (*CloseWindow (w);*)

    END BasicTest;

(************************************************************************)

PROCEDURE SolveTest;

    (* Solution of a linear equation. *)

    CONST Arows = 4;  Acols = 4;
          Brows = 4;  Bcols = 2;

    VAR A: ARRAY [1..Arows],[1..Acols] OF LONGREAL;
        B, C, D, X: ARRAY [1..Brows],[1..Bcols] OF LONGREAL;
        (*w: Window;*)

    BEGIN
        (*
        OpenWindow (w, black, brown, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        *)
        Reset;
        WriteString ("SOLVING LINEAR ALGEBRAIC EQUATIONS");
        WriteLn;

        (* Give a value to the A matrix. *)

        Random (A, Arows, Acols);
        WriteString ("Matrix A is");  WriteLn;
        Write (A, Arows, Acols, 4);

        (* Give a value to the B matrix. *)

        Random (B, Brows, Bcols);
        WriteString ("Matrix B is");  WriteLn;
        Write (B, Brows, Bcols, 4);

        (* Solve the equation AX = B. *)

        Solve (A, B, X, Arows, Bcols);
        (*GaussJ (A, B, X, Arows, Bcols);*)

        (* Write the solution. *)

        WriteString ("The solution X to AX = B is");  WriteLn;
        Write (X, Brows, Bcols, 4);

        (* Check that the solution looks right. *)

        Mul (A, X, Arows, Acols, Bcols, C);
        Sub (B, C, Brows, Bcols, D);
        WriteString ("As a check, AX-B evaluates to");  WriteLn;
        Write (D, Brows, Bcols, 4);

        PressAnyKey;
        (*CloseWindow (w);*)

    END SolveTest;

(************************************************************************)

PROCEDURE SingularTest;

    (* Linear equation with singular coefficient matrix. *)

    CONST Arows = 2;  Acols = 2;
          Brows = 2;  Bcols = 1;

    VAR A: ARRAY [1..Arows],[1..Acols] OF LONGREAL;
        B, X: ARRAY [1..Brows],[1..Bcols] OF LONGREAL;
        (*w: Window;*)

    BEGIN
        (*
        OpenWindow (w, black, brown, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        *)
        Reset;
        WriteString ("A SINGULAR PROBLEM");
        WriteLn;

        (* Give a value to the A matrix. *)

        A[1,1] := 1.0;
        A[1,2] := 2.0;
        A[2,1] := 2.0;
        A[2,2] := 4.0;
        WriteString ("Matrix A is");  WriteLn;
        Write (A, Arows, Acols, 4);

        (* Give a value to the B matrix. *)

        Random (B, Brows, Bcols);
        WriteString ("Matrix B is");  WriteLn;
        Write (B, Brows, Bcols, 4);

        (* Try to solve the equation AX = B. *)

        Solve (A, B, X, Arows, Bcols);

        WriteString ("The equation AX = B could not be solved");  WriteLn;

        PressAnyKey;
        (*CloseWindow (w);*)

    END SingularTest;

(************************************************************************)

PROCEDURE InversionTest;

    (* Inverting a matrix, also an eigenvalue calculation. *)

    CONST N = 5;

    VAR A, B, X: ARRAY [1..N],[1..N] OF LONGREAL;
        W: ARRAY [1..N] OF LONGCOMPLEX;
        (*w: Window;*)  j: CARDINAL;

    BEGIN
        (*
        OpenWindow (w, yellow, brown, 0, 24, 0, 79, simpleframe, nodivider);
        SelectWindow (w);
        *)
        Reset;
        WriteString ("INVERTING A SQUARE MATRIX");
        WriteLn;

        (* Give a value to the A matrix. *)

        Random (A, N, N);
        WriteString ("Matrix A is");  WriteLn;
        Write (A, N, N, 4);

        (* Invert it. *)

        Invert (A, X, N);

        (* Write the solution. *)

        WriteLn;
        WriteString ("The inverse of A is");  WriteLn;
        Write (X, N, N, 4);

        (* Check that the solution looks right. *)

        Mul (A, X, N, N, N, B);
        WriteLn;
        WriteString ("As a check, the product evaluates to");  WriteLn;
        Write (B, N, N, 4);

        PressAnyKey;
        CLS;
        
        WriteLn;  WriteString ("EIGENVALUES");  WriteLn;
        WriteString ("The eigenvalues of A are");  WriteLn;
        Eigenvalues (A, W, N);
        FOR j := 1 TO N DO
            WriteString ("    ");  WriteCx (W[j], 5);  WriteLn;
        END (*FOR*);

        PressAnyKey;
        WriteString ("The eigenvalues of its inverse are");  WriteLn;
        Eigenvalues (X, W, N);
        FOR j := 1 TO N DO
            WriteString ("    ");  WriteCx (W[j], 5);  WriteLn;
        END (*FOR*);

        PressAnyKey;
        (*CloseWindow (w);*)

    END InversionTest;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
(*
    BasicTest;
    SolveTest;
    SingularTest;
*)
    InversionTest;
END MatTest.