MODULE MatTestM2A;

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
  REVISION HISTORY
  ================
  21 Dec 16 -- Removed all references to complex, which is only for the Eigenvalues anyway
  22 Dec 16 -- It compiled and worked without the Eigenvalue tests
  27 Dec 16 -- Changed params for VEC and MAT to not need array bounds as params like in C.  Modula-2 never needed that.  C does not have an array type.
  31 Dec 16 -- It finally works.
   6 Jan 17 -- Testing again w/ new randoms and Matrevised.
*)


FROM Matrevised IMPORT (* proc *)  Zero, Write, Add, Sub, Mul, Random, Solve, GaussJ, Invert, BelowSmallMakeZero;

FROM MiscStdInOut IMPORT CLS, WriteString, WriteLn, PressAnyKey;

IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
FROM Environment IMPORT GetCommandLine;

FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
FROM TIMLIBrevised IMPORT JULIAN,GREGORIAN,TIME2MDY;
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

    VAR
      A, D, E : ARRAY [1..Arows],[1..Acols] OF LONGREAL;
      B : ARRAY [1..Brows],[1..Bcols] OF LONGREAL;
      C : ARRAY [1..Arows],[1..Bcols] OF LONGREAL;
      success : BOOLEAN;

    BEGIN
        WriteString ("TEST OF SIMPLE MATRIX OPERATIONS");
        WriteLn;
        WriteLn;

        (* Give a value to the A matrix. *)
        Random (A);
        WriteString ("Matrix A is");  WriteLn;
        Write (A, 2);
        WriteLn;

        (* Give a value to the B matrix. *)
        Random (B);
        WriteString ("Matrix B is");  WriteLn;
        Write (B, 2);

        (* Try an addition (it will fail). *)

        success := Add(A,B,C);
        IF success THEN
          WriteString(" A + B succeeded.  It was not supposed to.");
          WriteLn;
        ELSE
          WriteString ("We can't compute A+B");  WriteLn;
        END (* if success *);
        WriteLn;
        WriteLn;

        (* Try a multiplication (it should work). *)
        success := Mul (A, B, C);
        IF success THEN
          WriteString ("C = A*B is");  WriteLn;
          Write (C, 2);
        ELSE
          WriteString(" C = A*B failed.  It was supposed to succeed.");
          WriteLn;
        END (* if success *);
        WriteLn;

        (* Give a value to the D matrix. *)
        Random (D);
        WriteString ("Matrix D is");  WriteLn;
        Write (D, 5);

        (* Try another addition (this one should work). *)
        success := Add (A, D, E);
        IF success THEN
          WriteString ("E = A+D is");  WriteLn;
          Write (E, 2);
        ELSE
          WriteString(" E = A+D failed and was supposed to succeed.");
          WriteLn;
        END (* if success *);
        WriteLn;

    END BasicTest;

(************************************************************************)

PROCEDURE SolveTest;

    (* Solution of a linear equation. *)

    CONST Arows = 4;  Acols = 4;
          Brows = 4;  Bcols = 2;

    VAR
      A: ARRAY [1..Arows],[1..Acols] OF LONGREAL;
      B, C, D, X, Y: ARRAY [1..Brows],[1..Bcols] OF LONGREAL;
      success : BOOLEAN;

    BEGIN
        WriteString ("SOLVING LINEAR ALGEBRAIC EQUATIONS");
        WriteLn;

        (* Give a value to the A matrix. *)
        Random (A);
        WriteString ("Matrix A is");  WriteLn;
        Write (A, 2);

        (* Give a value to the B matrix. *)
        Random (B);
        WriteString ("Matrix B is");  WriteLn;
        Write (B, 2);

        (* Solve the equation AX = B. *)
        success := Solve (A, B, X);
        IF success THEN
          (* Write the solution. *)
          WriteString ("The solution X to AX = B using Solve is");  WriteLn;
          Write (X, 6);
        ELSE
          WriteString(" Solution to Ax = B failed by Solve.  It was supposed to succeed.");
          WriteLn;
        END (* if success *);
        WriteLn;


        success := GaussJ (A, B, Y);
        IF success THEN
          (* Write the solution. *)
          WriteString ("The solution Y to AY = B using GaussJ is");  WriteLn;
          Write (X, 6);
        ELSE
          WriteString(" Solution to AY = B failed by GaussJ.  It was supposed to succeed.");
          WriteLn;
        END (* if success *);
        WriteLn;


        (* Check that the solution looks right. *)
        success := Mul (A, X, C);
        IF success THEN
          success := Sub (B, C, D);
          IF success THEN
            WriteString ("As a check, AX-B evaluates to");  WriteLn;
            Write (D, 5);
            WriteLn;
            BelowSmallMakeZero(D);
            WriteString(" AX-B is below after calling BelowSmallMakeZero ");
            WriteLn;
            Write(D,2);
            WriteLn;
          ELSE
            WriteString(" B-C failed but was supposed to succeed.");
            WriteLn;
          END;
        ELSE
          WriteString(" C = AX failed.  It was supposed to succeed.  End of SolveTest.");
          WriteLn;
        END;
        WriteLn;
    END SolveTest;

(************************************************************************)

PROCEDURE SingularTest;

    (* Linear equation with singular coefficient matrix. *)
    CONST Arows = 2;  Acols = 2;
          Brows = 2;  Bcols = 1;

    VAR A: ARRAY [1..Arows],[1..Acols] OF LONGREAL;
        B, X: ARRAY [1..Brows],[1..Bcols] OF LONGREAL;
        success : BOOLEAN;

    BEGIN
        WriteString ("A SINGULAR PROBLEM");
        WriteLn;

        (* Give a value to the A matrix. *)
        A[1,1] := 1.0;
        A[1,2] := 2.0;
        A[2,1] := 2.0;
        A[2,2] := 4.0;
        WriteString ("Matrix A is");  WriteLn;
        Write (A, 2);

        (* Give a value to the B matrix. *)
        Random (B);
        WriteString ("Matrix B is");  WriteLn;
        Write (B, 2);

        (* Try to solve the equation AX = B. *)
        success := Solve (A, B, X);
        IF NOT success THEN
          WriteString ("The equation AX = B could not be solved.  A is singular.");  WriteLn;
        ELSE
          WriteString(" The matrix is supposed to be singular, but solve succeeded.  It is");
          WriteLn;
          Write(X, 2);
          WriteLn;
        END;
        WriteLn;
    END SingularTest;

(************************************************************************)

PROCEDURE InversionTest;

    (* Inverting a matrix, without the eigenvalue calculation. *)

    CONST N = 5;

    VAR A, B, X: ARRAY [1..N],[1..N] OF LONGREAL;
        success : BOOLEAN;

    BEGIN
        WriteString ("INVERTING A SQUARE MATRIX");
        WriteLn;

        (* Give a value to the A matrix. *)
        Random (A);
        WriteString ("Matrix A is");  WriteLn;
        Write (A, 2);

        (* Invert it. *)

        success := Invert (A, X);

        (* Write the solution. *)
        IF NOT success THEN
          WriteLn;
          WriteString(" Inverting A was supposed to succeed.  It failed.");
          WriteLn;
          RETURN;
        END;

        WriteLn;
        WriteString ("The inverse of A is");  WriteLn;
        Write (X, 4);
        WriteLn;

        (* Check that the solution looks right. *)

        success := Mul (A, X, B);
        IF NOT success THEN
          WriteString(" Mul(A,X,B) was supposed to succeed.  It failed.");
          WriteLn;
        END;
        WriteLn;
        WriteString ("As a check, the raw product evaluates to");  WriteLn;
        Write (B, 6);



        BelowSmallMakeZero(B);
        WriteString(" Processed product evaluates to ");
        WriteLn;
        Write(B,2);
        WriteLn;
    END InversionTest;


(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN

    BasicTest;
    PressAnyKey;

    SolveTest;
    PressAnyKey;
    SingularTest;
    PressAnyKey;
    InversionTest;
    PressAnyKey;
END MatTestM2A.
