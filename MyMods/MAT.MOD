IMPLEMENTATION MODULE Mat;

        (********************************************************)
        (*                                                      *)
        (*                 Matrix arithmetic                    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        16 August 1996                  *)
        (*  Status:             Seems to be working             *)
        (*                                                      *)
        (*      Portability note: this module contains some     *)
        (*      open array operations which are a language      *)
        (*      extension in XDS Modula-2 but not part of       *)
        (*      ISO standard Modula-2.  So far I haven't worked *)
        (*      out how to do this in the standard language.    *)
        (*                                                      *)
        (********************************************************)

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Vec IMPORT
    (* type *)  VectorPtr,
    (* proc *)  NewVector, DisposeVector;

FROM LongComplexMath IMPORT
    (* proc *)  conj;

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
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, (*WriteString,*)
    WriteStringAt, WriteCellsAt, WriteCells, (*WriteLn,*) EraseToEOL, ChangeAttr,
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
    
FROM MiscM2 IMPORT Error, WriteString, WriteLn, WriteLongReal, Sqrt;

FROM MYRANDOM IMPORT RANDCARD, RANDREAL;
(*
FROM Rand IMPORT RANDOM;
*)

(************************************************************************)

CONST small = 1.0E-15;

TYPE
    subscript = [0..8191];
    Permutation = POINTER TO ARRAY subscript OF subscript;

(************************************************************************)
(*                  CREATING AND DESTROYING MATRICES                    *)
(************************************************************************)

PROCEDURE NewArray (N, M: CARDINAL): ArrayPtr;

    (* Creates an NxM matrix. *)

    VAR result: ArrayPtr;

    BEGIN
        NEW (result, N-1, M-1);
        RETURN result;
    END NewArray;

(************************************************************************)

PROCEDURE DisposeArray (VAR (*INOUT*) V: ArrayPtr;  N, M: CARDINAL);

    (* Deallocates an NxM matrix. *)

    BEGIN
        DISPOSE (V);
    END DisposeArray;

(************************************************************************)
(*                          COPYING A MATRIX                            *)
(************************************************************************)

PROCEDURE Copy (A: ARRAY OF ARRAY OF EltType;  r, c: CARDINAL;
                         VAR (*OUT*) B: ARRAY OF ARRAY OF EltType);

    (* Copies an rxc matrix A to B. *)

    VAR i, j: CARDINAL;

    BEGIN
        FOR i := 0 TO r-1 DO
            FOR j := 0 TO c-1 DO
                B[i,j] := A[i,j];
            END (*FOR*);
        END (*FOR*);
    END Copy;

(************************************************************************)
(*                        SPECIAL MATRICES                              *)
(************************************************************************)

PROCEDURE Zero (VAR (*INOUT*) M: ARRAY OF ARRAY OF EltType;  r, c: CARDINAL);

    (* Creates an r by c matrix with all zero entries. *)

    VAR i, j: CARDINAL;

    BEGIN
        FOR i := 0 TO r-1 DO
            FOR j := 0 TO c-1 DO
                M[i,j] := 0.0;
            END (*FOR*);
        END (*FOR*);
    END Zero;

(************************************************************************)

PROCEDURE Unit (VAR (*INOUT*) M: ARRAY OF ARRAY OF EltType;  N: CARDINAL);

    (* Creates an N by N unit matrix. *)

    VAR i: CARDINAL;

    BEGIN
        Zero (M, N, N);
        FOR i := 0 TO N-1 DO
            M[i,i] := 1.0;
        END (*FOR*);
    END Unit;

(************************************************************************)

PROCEDURE Random (VAR (*INOUT*) M: ARRAY OF ARRAY OF EltType;  r, c: CARDINAL);

    (* Creates an r by c matrix with random entries. *)

    VAR i, j: CARDINAL;

    BEGIN
        FOR i := 0 TO r-1 DO
            FOR j := 0 TO c-1 DO
                M[i,j] := VAL(EltType, FLOAT(RANDCARD(100)));
(*                M[i,j] := VAL(EltType, RANDREAL()); *)
            END (*FOR*);
        END (*FOR*);
    END Random;

(************************************************************************)
(*                  THE STANDARD ARITHMETIC OPERATIONS                  *)
(************************************************************************)

PROCEDURE Add (A, B: ARRAY OF ARRAY OF EltType;  r, c: CARDINAL;
                      VAR (*OUT*) C: ARRAY OF ARRAY OF EltType);

    (* Computes C := A + B.  All matrices are rxc. *)

    VAR i, j: CARDINAL;

    BEGIN
        FOR i := 0 TO r-1 DO
            FOR j := 0 TO c-1 DO
                C[i,j] := A[i,j] + B[i,j];
            END (*FOR*);
        END (*FOR*);
    END Add;

(************************************************************************)

PROCEDURE Sub (A, B: ARRAY OF ARRAY OF EltType;  r, c: CARDINAL;
                      VAR (*OUT*) C: ARRAY OF ARRAY OF EltType);

    (* Computes C := A - B.  All matrices are rxc.  *)

    VAR i, j: CARDINAL;

    BEGIN
        FOR i := 0 TO r-1 DO
            FOR j := 0 TO c-1 DO
                C[i,j] := A[i,j] - B[i,j];
            END (*FOR*);
        END (*FOR*);
    END Sub;

(************************************************************************)

PROCEDURE Mul (A, B: ARRAY OF ARRAY OF EltType;  r, c1, c2: CARDINAL;
                      VAR (*OUT*) C: ARRAY OF ARRAY OF EltType);

    (* Computes C := A*B, where A is rxc1 and B is c1xc2. *)

    VAR i, j, k: CARDINAL;  temp: EltType;

    BEGIN
        FOR i := 0 TO r-1 DO
            FOR j := 0 TO c2-1 DO
                temp := 0.0;
                FOR k := 0 TO c1-1 DO
                    temp := temp + A[i,k]*B[k,j];
                END (*FOR*);
                C[i,j] := temp;
            END (*FOR*);
        END (*FOR*);
    END Mul;

(************************************************************************)

PROCEDURE ScalarMul (A: EltType;  B: ARRAY OF ARRAY OF EltType;  r, c: CARDINAL;
                                  VAR (*OUT*) C: ARRAY OF ARRAY OF EltType);

    (* Computes C := A*B, where B is rxc. *)

    VAR i, j: CARDINAL;

    BEGIN
        FOR i := 0 TO r-1 DO
            FOR j := 0 TO c-1 DO
                C[i,j] := A * B[i,j];
            END (*FOR*);
        END (*FOR*);
    END ScalarMul;

(************************************************************************)
(*                      SOLVING LINEAR EQUATIONS                        *)
(************************************************************************)

PROCEDURE LUFactor (VAR (*INOUT*) A: ARRAY OF ARRAY OF EltType;  N: CARDINAL;
                          perm: Permutation;  VAR (*OUT*) oddswaps: BOOLEAN);

    (* LU decomposition of a square matrix.  We express A in the form   *)
    (* P*L*U, where P is a permutation matrix, L is lower triangular    *)
    (* with unit diagonal elements, and U is upper triangular.  This is *)
    (* an in-place computation, where on exit U occupies the upper      *)
    (* triangle of A, and L (not including its diagonal entries) is in  *)
    (* the lower triangle.  The permutation information is returned in  *)
    (* perm.  Output parameter oddswaps is TRUE iff an odd number of    *)
    (* row interchanges were done by the permutation.  (We need to know *)
    (* this only if we are going to go on to calculate a determinant.)  *)

    (* The precise relationship between the implied permutation matrix  *)
    (* P and the output parameter perm is somewhat obscure.  The vector *)
    (* perm^ is not simply a permutation of the subscripts [0..N-1]; it *)
    (* does, however, have the property that we can recreate P by       *)
    (* walking through perm^ from start to finish, in the order used    *)
    (* by procedure LUSolve.                                            *)

    VAR row, col, k, pivotrow: CARDINAL;
        sum, temp, maxval: LONGREAL;  VV: VectorPtr;

    BEGIN
        VV := NewVector (N);
        oddswaps := FALSE;

        (* Start by collecting (in VV), the maximum absolute value in   *)
        (* each row; we'll use this for pivoting decisions.             *)

        FOR row := 0 TO N-1 DO
            maxval := 0.0;
            FOR col := 0 TO N-1 DO
                IF ABS(A[row,col]) > maxval THEN
                    maxval := ABS(A[row,col]);
                END (*IF*);
            END (*FOR*);
            IF maxval = 0.0 THEN
                (* An all-zero row can never contribute pivot elements. *)
                VV^[row] := 0.0;
            ELSE
                VV^[row] := 1.0/maxval;
            END (*IF*);
        END (*FOR*);

        (* Crout's method: we work through one column at a time. *)

        FOR col := 0 TO N-1 DO

            (* Upper triangular component of this column - except for   *)
            (* the diagonal element, which we leave until after we've   *)
            (* selected a pivot from on or below the diagonal.          *)

            IF col > 0 THEN
                FOR row := 0 TO col-1 DO
                    sum := A[row,col];
                    IF row > 0 THEN
                        FOR k := 0 TO row-1 DO
                           sum := sum - A[row,k] * A[k,col];
                        END (*FOR*);
                    END (*IF*);
                    A[row,col] := sum;
                END (*FOR*);
            END (*IF*);

            (* Lower triangular component in this column.  The results  *)
            (* we get in this loop will not be correct until we've      *)
            (* divided by the pivot; but we work out the pivot location *)
            (* as we go, and come back later for this division.         *)

            maxval := 0.0;  pivotrow := col;
            FOR row := col TO N-1 DO
                sum := A[row,col];
                IF col > 0 THEN
                    FOR k := 0 TO col-1 DO
                       sum := sum - A[row,k] * A[k,col];
                    END (*FOR*);
                END (*IF*);
                A[row,col] := sum;
                temp := VV^[row] * ABS(sum);
                IF temp >= maxval THEN
                    maxval := temp;  pivotrow := row;
                END (*IF*);
            END (*FOR*);

            (* If pivot element was not already on the diagonal, do a   *)
            (* row swap.                                                *)

            IF pivotrow <> col THEN
                FOR k := 0 TO N-1 DO
                    temp := A[pivotrow,k];
                    A[pivotrow,k] := A[col,k];
                    A[col,k] := temp;
                END (*FOR*);
                oddswaps := NOT oddswaps;
                VV^[pivotrow] := VV^[col];

                (* We don't bother updating VV^[col] here, because      *)
                (* its value will never be used again.                  *)

            END (*IF*);
            perm^[col] := pivotrow;

            (* Finish off the calculation of the lower triangular part  *)
            (* for this column by scaling by the pivot A[col,col].      *)

            (* Remark: if the pivot is still zero at this stage, then   *)
            (* all the elements below it are also zero.  The LU         *)
            (* decomposition in this case is not unique - the original  *)
            (* matrix is singular, therefore U will also be singular -  *)
            (* but one solution is to leave all those elements zero.    *)

            temp := A[col,col];
            IF (col <> N-1) AND (temp <> 0.0) THEN
                temp := 1.0/temp;
                FOR row := col+1 TO N-1 DO
                    A[row,col] := temp*A[row,col];
                END (*FOR*);
            END (*IF*);

        END (*FOR*);

    END LUFactor;

(************************************************************************)

PROCEDURE LUSolve (LU: ARRAY OF ARRAY OF EltType;
                    VAR (*INOUT*) B: ARRAY OF ARRAY OF EltType;
                    N, M: CARDINAL;  perm: Permutation);

    (* Solves the equation P*L*U*X = B, where P is a permutation        *)
    (* matrix specified indirectly by perm; L is lower triangular; and  *)
    (* U is upper triangular.  The "Matrix" LU is not a genuine matrix, *)
    (* but rather a packed form of L and U as produced by procedure     *)
    (* LUfactor above.  On return B is replaced by the solution X.      *)
    (* Dimensions: left side is NxN, B is NxM.                          *)

    VAR i, j, k, ip: CARDINAL;  sum, scale: LONGREAL;

    BEGIN
        (* Pass 1: Solve the equation L*Y = B (at the same time sorting *)
        (* B in accordance with the specified permutation).  The        *)
        (* solution Y overwrites the original value of B.               *)

        (* Understanding how the permutations work is something of a    *)
        (* black art.  It helps to know that (a) ip>=i for all i, and   *)
        (* (b) in the summation over k below, we are accessing only     *)
        (* those elements of B that have already been sorted into the   *)
        (* correct order.                                               *)

        FOR i := 0 TO N-1 DO
            ip := perm^[i];
            FOR j := 0 TO M-1 DO
                sum := B[ip,j];
                B[ip,j] := B[i,j];
                IF i > 0 THEN
                    FOR k := 0 TO i-1 DO
                        sum := sum - LU[i,k] * B[k,j];
                    END (*FOR*);
                END (*IF*);
                B[i,j] := sum;
            END (*FOR*);
        END (*FOR*);

        (* Pass 2: solve the equation U*X = Y. *)

        FOR i := N-1 TO 0 BY -1 DO
            scale := LU[i,i];
            IF scale = 0.0 THEN
                Error ("Matrix is singular");
                RETURN;
            END (*IF*);
            FOR j := 0 TO M-1 DO
                sum := B[i,j];
                FOR k := i+1 TO N-1 DO
                    sum := sum - LU[i,k] * B[k,j];
                END (*FOR*);
                B[i,j] := sum/scale;
            END (*FOR*);
        END (*FOR*);

    END LUSolve;

(************************************************************************)

PROCEDURE Solve (A, B: ARRAY OF ARRAY OF EltType;
                    VAR (*OUT*) X: ARRAY OF ARRAY OF EltType;
                    N, M: CARDINAL);

    (* Solves the equation AX = B.  In the present version A must be    *)
    (* square and nonsingular.                                          *)
    (* Dimensions: A is NxN, B is NxM.                                  *)

    VAR LU, error, product: ArrayPtr;
        perm: Permutation;  s: BOOLEAN;

    BEGIN
        LU := NewArray (N, N);
        Copy (A, N, N, LU^);  Copy (B, N, M, X);
        ALLOCATE (perm, N*SIZE(subscript));
        LUFactor (LU^, N, perm, s);
        LUSolve (LU^, X, N, M, perm);

        (* For better accuracy, apply one step of iterative     *)
        (* improvement.  (Two or three steps might be better;   *)
        (* but they might even make things worse, because we're *)
        (* still stuck with the rounding errors in LUFactor.)   *)

        error := NewArray (N, M);
        product := NewArray (N, M);
        Mul (A, X, N, N, M, product^);
        Sub (B, product^, N, M, error^);
        LUSolve (LU^, error^, N, M, perm);
        Add (X, error^, N, M, X);
        DisposeArray (product, N, M);  DisposeArray (error, N, M);

        DEALLOCATE (perm, N*SIZE(subscript));
        DisposeArray (LU, N, N);
    END Solve;

(************************************************************************)

PROCEDURE GaussJ (A, B: ARRAY OF ARRAY OF EltType;
                     VAR (*OUT*) X: ARRAY OF ARRAY OF EltType;
                     N, M: CARDINAL);

    (* Solves the equation AX = B by Gauss-Jordan elimination.  In the  *)
    (* present version A must be square and nonsingular.                *)
    (* Dimensions: A is NxN, B is NxM.                                  *)

    VAR W: ArrayPtr;  i, j, k, prow: CARDINAL;
        pivot, scale, temp: EltType;

    BEGIN
        W := NewArray (N, N);  Copy (A, N, N, W^);  Copy (B, N, M, X);

        (* Remark: we are going to use elementary row operations to     *)
        (* turn W into a unit matrix.  However we don't bother to store *)
        (* the new 1.0 and 0.0 entries, because those entries will      *)
        (* never be fetched again.  We simply base our calculations on  *)
        (* the assumption that those values have been stored.           *)

        (* Pass 1: by elementary row operations, make W into an upper   *)
        (* triangular matrix.                                           *)

        prow := 0;
        FOR i := 0 TO N-1 DO
            pivot := 0.0;
            FOR j := i TO N-1 DO
                temp := W^[j,i];
                IF ABS(temp) > ABS(pivot) THEN
                    pivot := temp;  prow := j;
                END (*IF*);
            END (*FOR*);
            IF ABS(pivot) < small THEN
                Error ("Coefficient matrix is singular");
            END (*IF*);

            (* Swap rows i and prow. *)

            FOR j := i TO N-1 DO
                temp := W^[i,j];
                W^[i,j] := W^[prow,j];
                W^[prow,j] := temp;
            END (*FOR*);
            FOR j := 0 TO M-1 DO
                temp := X[i,j];
                X[i,j] := X[prow,j];
                X[prow,j] := temp;
            END (*FOR*);

            (* Scale the i'th row of both W and X. *)

            FOR j := i+1 TO N-1 DO
                W^[i,j] := W^[i,j]/pivot;
            END (*FOR*);
            FOR j := 0 TO M-1 DO
                X[i,j] := X[i,j]/pivot;
            END (*FOR*);

            (* Implicitly reduce the sub-column below W[i,i] to zero.   *)

            FOR k := i+1 TO N-1 DO
                scale := W^[k,i];
                FOR j := i+1 TO N-1 DO
                    W^[k,j] := W^[k,j] - scale*W^[i,j];
                END (*FOR*);
                FOR j := 0 TO M-1 DO
                    X[k,j] := X[k,j] - scale*X[i,j];
                END (*FOR*);
            END (*FOR*);

        END (*FOR*);

        (* Pass 2: reduce the above-diagonal elements of W to zero.     *)

        FOR i := N-1 TO 1 BY -1 DO

            (* Implicitly reduce the sub-column above W[i,i] to zero.   *)

            FOR k := 0 TO i-1 DO
                scale := W^[k,i];
                FOR j := 0 TO M-1 DO
                    X[k,j] := X[k,j] - scale*X[i,j];
                END (*FOR*);
            END (*FOR*);

        END (*FOR*);

        DisposeArray (W, N, N);

    END GaussJ;

(************************************************************************)

PROCEDURE Invert (A: ARRAY OF ARRAY OF EltType;
                           VAR (*INOUT*) X: ARRAY OF ARRAY OF EltType;
                           N: CARDINAL);

    (* Inverts an NxN nonsingular matrix. *)

    VAR I: ArrayPtr;

    BEGIN
        I := NewArray (N, N);  Unit (I^, N);
        Solve (A, I^, X, N, N);
        DisposeArray (I, N, N);
    END Invert;

(************************************************************************)
(*                         EIGENPROBLEMS                                *)
(************************************************************************)

PROCEDURE Balance (VAR INOUT A: ARRAY OF ARRAY OF EltType;  N: CARDINAL);

    (* Replaces A by a better-balanced matrix with the same eigenvalues.*)
    (* There is no effect on symmetrical matrices.  To minimize the     *)
    (* effect of rounding, we scale only by a restricted set of scaling *)
    (* factors derived from the machine's radix.                        *)

    CONST radix = 2.0;  radixsq = radix*radix;

    VAR row, j: CARDINAL;  c, r, f, g, s: LONGREAL;
        done: BOOLEAN;

    BEGIN
        REPEAT
            done := TRUE;
            FOR row := 0 TO N-1 DO
                c := 0.0;  r := 0.0;
                FOR j := 0 TO N-1 DO
                    IF j <> row THEN
                        c := c + ABS(A[j,row]);
                        r := r + ABS(A[row,j]);
                    END (*IF*);
                END (*FOR*);
                IF (c <> 0.0) AND (r <> 0.0) THEN
                    g := r/radix;  f := 1.0;  s := c + r;
                    WHILE c < g DO
                        f := f*radix;  c := c*radixsq;
                    END (*WHILE*);
                    g := r*radix;
                    WHILE c > g DO
                        f := f/radix;  c := c/radixsq;
                    END (*WHILE*);
                    IF (c+r)/f < 0.95*s THEN
                        done := FALSE;  g := 1.0/f;

                        (* Here is the actual transformation: a scaling *)
                        (* of this row and the corresponding column.    *)

                        FOR j := 0 TO N-1 DO
                            A[row,j] := g*A[row,j];
                        END (*FOR*);
                        FOR j := 0 TO N-1 DO
                            A[j,row] := f*A[j,row];
                        END (*FOR*);
                    END (*IF*);

                END (*IF c and r nonzero *);

            END (*FOR row := 0 TO N-1*);

        UNTIL done;

    END Balance;

(************************************************************************)

PROCEDURE Hessenberg (VAR INOUT A: ARRAY OF ARRAY OF EltType;  N: CARDINAL);

    (* Transforms an NxN matrix into upper Hessenberg form, i.e. all    *)
    (* entries below the diagonal zero except for the first subdiagonal.*)
    (* This is an "in-place" calculation, i.e. the answer replaces the  *)
    (* original matrix.                                                 *)

    CONST small = 1.0E-15;

    VAR pos, i, j, pivotrow: CARDINAL;
        pivot, temp: LONGREAL;  V: VectorPtr;

    BEGIN
        IF N <= 2 THEN RETURN END(*IF*);

        V := NewVector (N);
        FOR pos := 1 TO N-2 DO

            (* At this point in the calculation, A has the form         *)
            (*          A11     A12                                     *)
            (*          A21     A22                                     *)
            (* where A11 has (pos+1) rows and columns and is already in *)
            (* upper Hessenberg form; and A21 is zero except for its    *)
            (* last two columns.  This time around the loop, we are     *)
            (* going to transform A such that column (pos-1) of A21 is  *)
            (* reduced to zero.  The transformation will affect only    *)
            (* the last column of A11, therefore will not alter its     *)
            (* Hessenberg property.                                     *)

            (* Step 1: we need A[pos,pos-1] to be nonzero.  To keep     *)
            (* the calculations as well-conditioned as possible, we     *)
            (* allow for a preliminary row and column swap.             *)

            pivot := A[pos,pos-1];  pivotrow := pos;
            FOR i := pos+1 TO N-1 DO
                temp := A[i,pos-1];
                IF ABS(temp) > ABS(pivot) THEN
                    pivot := temp;  pivotrow := i;
                END (*IF*);
            END (*FOR*);

            IF ABS(pivot) < small THEN

                (* The pivot is essentially zero, so we already have    *)
                (* the desired property and no transformation is        *)
                (* necessary this time.  We simply replace all of the   *)
                (* "approximately zero" entries by 0.0.                 *)

                FOR i := pos TO N-1 DO
                    A[i,pos-1] := 0.0;
                END (*FOR*);

            ELSE

                IF pivotrow <> pos THEN

                    (* Swap rows pos and pivotrow, and then swap the    *)
                    (* corresponding columns.                           *)

                    FOR j := pos-1 TO N-1 DO
                        temp := A[pivotrow,j];
                        A[pivotrow,j] := A[pos,j];
                        A[pos,j] := temp;
                    END (*FOR*);
                    FOR i := 0 TO N-1 DO
                        temp := A[i,pivotrow];
                        A[i,pivotrow] := A[i,pos];
                        A[i,pos] := temp;
                    END (*FOR*);

                END (*IF*);

                (* Now we are going to replace A by T*A*Inverse(T),     *)
                (* where T is a unit matrix except for column pos.      *)
                (* That column is equal to a vector V, where V[i] = 0.0 *)
                (* for i < pos, and V[pos] = 1.0.  We don't bother      *)
                (* storing those fixed elements explicitly.             *)

                FOR i := pos+1 TO N-1 DO
                    V^[i] := -A[i,pos-1] / pivot;
                END (*FOR*);

                (* Premultiplication of A by T.  Because of the special *)
                (* structure of T, this affects only rows [pos+1..N].   *)
                (* We also know that some of the results will be zero.  *)

                FOR i := pos+1 TO N-1 DO
                    A[i,pos-1] := 0.0;
                    FOR j := pos TO N-1 DO
                        A[i,j] := A[i,j] + V^[i] * A[pos,j];
                    END (*FOR*);
                END (*FOR*);

                (* Postmultiplication by the inverse of T.  This affects*)
                (* only column pos.                                     *)

                FOR i := 0 TO N-1 DO
                    temp := 0.0;
                    FOR j := pos+1 TO N-1 DO
                        temp := temp + A[i,j] * V^[j];
                    END (*FOR*);
                    A[i,pos] := A[i,pos] - temp;
                END (*FOR*);

            END (*IF*);

        END (*FOR*);
        DisposeVector (V, N);

    END Hessenberg;

(************************************************************************)

PROCEDURE QR ( A: ARRAY OF ARRAY OF EltType;  VAR OUT W: ARRAY OF LONGCOMPLEX;
                                             N: CARDINAL);
(* I have no idea why I had to make the first param, the A matrix, a VAR param for this to work. *)
(* Could it be a compiler bug? *)
    (* Finds all the eigenvalues of an upper Hessenberg matrix.         *)
    (* On return W contains the eigenvalues.                            *)

    (* Source: this is an adaption of code from "Numerical Recipes"     *)
    (* by Press, Flannery, Teutolsky, and Vetterling.                   *)

    VAR last, m, j, k, L, its, i, imax: CARDINAL;
        z, y, x, w, v, u, shift, s, r, q, p, anorm: LONGREAL;

    (********************************************************************)

    BEGIN
        (* Compute matrix norm.  This looks wrong to me, but it seems   *)
        (* to be giving satisfactory results.                           *)

        anorm := ABS(A[0,0]);
        FOR i := 1 TO N-1 DO
            FOR j := i-1 TO N-1 DO
(* compiler flagged this line w/ an assign range error *)
            anorm := anorm + ABS(A[i,j]);
            END (*FOR*);
        END (*FOR*);

        last  := N-1;  shift := 0.0;
        its := 0;

        LOOP

            (* Find, if possible, an L such that A[L,L-1] is zero to    *)
            (* machine accuracy.  If we succeed then A is now block     *)
            (* diagonal, and we can work independently on the final     *)
            (* block (rows and columns L to last).                      *)

            L := last;
            LOOP
                IF L = 0 THEN EXIT(*LOOP*) END(*IF*);
                s := ABS(A[L-1,L-1]) + ABS(A[L,L]);
                IF s = 0.0 THEN s := anorm END(*IF*);
                IF ABS(A[L,L-1]) + s = s THEN
                    EXIT (*LOOP*);
                END (*IF*);
                DEC (L);
            END (*LOOP*);

            x := A[last,last];
            IF L = last THEN

                (* One eigenvalue found. *)

                W[last] := CMPLX (x+shift, 0.0);
                IF last > 0 THEN DEC (last);  its := 0;
                ELSE EXIT (*LOOP*);
                END (*IF*);

            ELSE
                y := A[last-1,last-1];
                w := A[last,last-1]*A[last-1,last];
                IF L = last-1 THEN

                    (* We're down to a 2x2 submatrix, so can work out   *)
                    (* the eigenvalues directly.                        *)

                    p := 0.5*(y-x);  q := p*p + w;
                    z := Sqrt(ABS(q));  x := x + shift;
                    IF q >= 0.0 THEN
                        IF p < 0.0 THEN z := p - z;
                        ELSE z := p + z;
                        END (*IF*);
                        W[last-1] := CMPLX (x+z, 0.0);
                        IF z = 0.0 THEN
                            W[last] := CMPLX (x, 0.0);
                        ELSE
                            W[last] := CMPLX (x - w/z, 0.0);
                        END (*IF*);
                    ELSE
                        W[last-1] := CMPLX (x+p, z);
                        W[last] := conj (W[last-1]);
                    END (*IF*);
                    IF last >= 2 THEN DEC (last, 2);  its := 0;
                    ELSE EXIT (*LOOP*)
                    END (*IF*);

                ELSE

                    IF its < 10 THEN
                        INC (its);
                    ELSE
                        (* If we're converging too slowly,      *)
                        (* modify the shift.                    *)

                        shift := shift + x;
                        FOR i := 0 TO last DO
                            A[i,i] := A[i,i] - x;
                        END (*FOR*);
                        s := ABS(A[last,last-1]) + ABS(A[last-1,last-2]);
                        x := 0.75*s;
                        y := x;  w := -0.4375*s*s;
                        its := 0;
                    END (*IF*);

                    (* We're now working on a sub-array [L..last] of    *)
                    (* size 3x3 or greater.  Our goal is to transform   *)
                    (* the matrix so as to reduce the magnitudes of     *)
                    (* the elements on the first sub-diagonal, so that  *)
                    (* after one or more iterations one of them will be *)
                    (* zero to within machine accuracy.                 *)

                    (* Shortcut: if we can find two consecutive         *)
                    (* subdiagonal elements whose product is small,     *)
                    (* we're even better off.                           *)

                    m := last-2;
                    LOOP
                        z := A[m,m];  r := x-z;  s := y-z;
                        p := (r*s-w)/A[m+1,m] + A[m,m+1];
                        q := A[m+1,m+1] - z - r - s;
                        r := A[m+2,m+1];
                        s := ABS(p) + ABS(q) + ABS(r);
                        p := p/s;  q := q/s;  r := r/s;
                        IF m = L THEN EXIT(*LOOP*) END(*IF*);
                        u := ABS(A[m,m-1]) * (ABS(q) + ABS(r));
                        v := ABS(p) * (ABS(A[m-1,m-1]) + ABS(z)
                                        + ABS(A[m+1,m+1]));
                        IF u+v = v THEN EXIT(*LOOP*) END(*IF*);
                        DEC (m);
                    END (*LOOP*);

                    A[m+2,m] := 0.0;
                    FOR i := m+3 TO last DO
                        A[i,i-2] := 0.0;
                        A[i,i-3] := 0.0;
                    END (*FOR*);

                    (* Apply row and column transformations that should *)
                    (* reduce the magnitudes of subdiagonal elements.   *)

                    FOR k := m TO last-1 DO
                        IF k <> m THEN
                            p := A[k,k-1];  q := A[k+1,k-1];
                            r := 0.0;
                            IF k <> last-1 THEN
                                r := A[k+2,k-1];
                            END (*IF*);
                            x := ABS(p) + ABS(q) + ABS(r);
                            IF x <> 0.0 THEN
                                p := p/x;  q := q/x;  r := r/x;
                            END (*IF*);
                        END (*IF*);
                        s := Sqrt(p*p + q*q + r*r);
                        IF p < 0.0 THEN s := -s END(*IF*);
                        IF s <> 0.0 THEN
                            IF k = m THEN
                                IF L <> m THEN
                                    A[k,k-1] := -A[k,k-1];
                                END(*IF*);
                            ELSE
                                A[k,k-1] := -s*x;
                            END (*IF*);
                            p := p + s;  x := p/s;  y := q/s;  z := r/s;
                            q := q/p;  r := r/p;

                            (* Row transformation. *)

                            FOR j := k TO last DO
(* access violation this line *)p := A[k,j] + q * A[k+1,j];
                                IF k <> last-1 THEN
                                    p := p + r * A[k+2,j];
                                    A[k+2,j] := A[k+2,j] - p*z;
                                END (*IF*);
                                A[k+1,j] := A[k+1,j] - p*y;
                                A[k,j] := A[k,j] - p*x;
                            END (*FOR*);

                            (* Column transformation. *)

                            imax := k+3;
                            IF last < imax THEN imax := last END(*IF*);
                            FOR i := L TO imax DO
                                p := x * A[i,k] + y * A[i,k+1];
                                IF k <> last-1 THEN
                                    p := p + z * A[i,k+2];
                                    A[i,k+2] := A[i,k+2] - p*r;
                                END (*IF*);
                                A[i,k+1] := A[i,k+1] - p*q;
                                A[i,k] := A[i,k] - p;
                            END (*FOR*);

                        END (*IF s <> 0.0 *);

                    END (*FOR k := m TO last-1 *);

                END (*IF test for 2x2 or bigger *);

            END (*IF test for 1x1 *);

        END (* main loop *);

    END QR;

(************************************************************************)

PROCEDURE Eigenvalues (A: ARRAY OF ARRAY OF EltType;
                          VAR OUT W: ARRAY OF LONGCOMPLEX;
                          N: CARDINAL);

    (* Finds all the eigenvalues of an NxN matrix.    *)
    (* This procedure does not modify A.              *)

    VAR Acopy: ArrayPtr;
    
    BEGIN
        IF N > 0 THEN
            Acopy := NewArray (N, N);
            Copy (A, N, N, Acopy^);
            Balance (Acopy^, N);
            Hessenberg (Acopy^, N);
            QR (Acopy^, W, N);
            DisposeArray (Acopy, N, N);
        END (*IF*);
    END Eigenvalues;

(************************************************************************)
(*                              OUTPUT                                  *)
(************************************************************************)

PROCEDURE Write (M: ARRAY OF ARRAY OF EltType;  r, c: CARDINAL;  places: CARDINAL);

    (* Writes the rxc matrix M to the screen, where each column *)
    (* occupies a field "places" characters wide.               *)

    VAR i, j: CARDINAL;

    BEGIN
        FOR i := 0 TO r-1 DO
            FOR j := 0 TO c-1 DO
                WriteString ("  ");
                WriteLongReal (M[i,j], places);
            END (*FOR*);
            WriteLn;
        END (*FOR*);
    END Write;

(************************************************************************)

END Mat.