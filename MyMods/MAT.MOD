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

(*
  REVISION HISTORY
  ======== =======
  28 Dec 16 -- Changed params to not need N or M, as Modula-2 never needed those.  And most of the functions here now return a boolean
                 to indicate success or failure.  Important point: HIGH() returns HIGH bound, not length of that dimension.
                 I wrote this after I wrote the Go version, so I confused HIGH() with len() from Go.  They are NOT the same!
   1 Jan 17 -- It finally works.  That took longer than I expected.
*)


FROM Storage IMPORT (* proc *)  ALLOCATE, DEALLOCATE;

FROM Vec IMPORT (* type *)  VectorPtr, (* proc *)  NewVector, DisposeVector;
(*
                                                                                             FROM LongComplexMath IMPORT (* proc *)  conj;
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

FROM Float IMPORT Round;

FROM MiscStdInOut IMPORT Error, WriteString, WriteLn, WriteLongReal, Sqrt;

FROM MyRand3revised IMPORT RANDCARD, RANDREAL;

(*
FROM MYRANDOM IMPORT RANDCARD, RANDREAL;
FROM Rand IMPORT RANDOM;
*)

(*  Defined in the def module
TYPE
    ArrayPtr = POINTER TO ARRAY OF ARRAY OF LONGREAL;
*)

(************************************************************************)

CONST small = 1.0E-10;
CONST verysmall = 1.0E-15;

TYPE
    subscript = [0..8191];
    Permutation = POINTER TO ARRAY subscript OF subscript;

(************************************************************************)
(*                  CREATING AND DESTROYING MATRICES                    *)
(************************************************************************)

PROCEDURE NewArray (N, M: CARDINAL): ArrayPtr;

    (* Creates an N x M matrix. *)
    (* But is N x M as upper bound, or number of elements?  Doesn't say.  *)
    (* I'll make it as an upper bound and see what happens                *)

    VAR result: ArrayPtr;

    BEGIN
        NEW (result, N-1, M-1);
        RETURN result;
    END NewArray;

(************************************************************************)

                                                                      (* PROCEDURE DisposeArray (VAR (*INOUT*) V: ArrayPtr;  N, M: CARDINAL); *)
PROCEDURE DisposeArray (VAR (*INOUT*) V: ArrayPtr);

    (* Deallocates an N x M matrix.  Since N,M are not used, I removed them from the param list *)

    BEGIN
        IF V <> NIL THEN
          DISPOSE (V);
          V := NIL;
        END (* if not NIL *);
    END DisposeArray;

(************************************************************************)
(*                          COPYING A MATRIX                            *)
(************************************************************************)

                                           (* PROCEDURE Copy (A: ARRAY OF ARRAY OF LONGREAL;  r, c: CARDINAL; VAR (*OUT*) B: ARRAY OF ARRAY OF LONGREAL); *)
PROCEDURE Copy (A: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) B: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;

    (* Copies an r x c matrix A to B. *)

    VAR i,j,rA,cA,rB,cB: CARDINAL;

    BEGIN
      rA := HIGH(A);
      cA := HIGH(A[0]);
      rB := HIGH(B);
      cB := HIGH(B[0]);

      IF (rA <> rB) OR (cA <> cB) THEN
        RETURN FALSE;
      END;

      FOR i := 0 TO rA DO
        FOR j := 0 TO cA DO
          B[i,j] := A[i,j];
        END (*FOR*);
      END (*FOR*);
      RETURN TRUE;
    END Copy;

(************************************************************************)
(*                        SPECIAL MATRICES                              *)
(************************************************************************)

                                                                     (* PROCEDURE Zero (VAR (*INOUT*) M: ARRAY OF ARRAY OF LONGREAL;  r, c: CARDINAL); *)
PROCEDURE Zero (VAR (*INOUT*) M: ARRAY OF ARRAY OF LONGREAL);


    (* Creates an r by c matrix with all zero entries. *)

    VAR i,j,r,c: CARDINAL;

    BEGIN
      r := HIGH(M);
      c := HIGH(M[0]);
      FOR i := 0 TO r DO
        FOR j := 0 TO c DO
          M[i,j] := 0.0;
        END (*FOR*);
      END (*FOR*);
    END Zero;

(************************************************************************)

                                                                       (* PROCEDURE Unit (VAR (*INOUT*) M: ARRAY OF ARRAY OF LONGREAL;  N: CARDINAL); *)
PROCEDURE Unit (VAR (*INOUT*) M: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;

    (* Creates an N by N unit matrix. *)

    VAR i,N: CARDINAL;

    BEGIN
      N := HIGH(M);
      IF N <> HIGH(M[0]) THEN
        RETURN FALSE;
      END;

      Zero(M);                                                                                      (* Zero (M, N, N); *)
      FOR i := 0 TO N DO
        M[i,i] := 1.0;
      END (*FOR*);
      RETURN TRUE;
    END Unit;

(************************************************************************)

                                                                   (* PROCEDURE Random (VAR (*INOUT*) M: ARRAY OF ARRAY OF LONGREAL;  r, c: CARDINAL); *)
PROCEDURE Random (VAR (*INOUT*) M: ARRAY OF ARRAY OF LONGREAL);

    (* Creates an r by c matrix with random entries. *)

    VAR
      i,j,r,c: CARDINAL;
      f : LONGREAL;

    BEGIN
        r := HIGH(M);
        c := HIGH(M[0]);
      FOR i := 0 TO r DO
        FOR j := 0 TO c DO
            f := RANDREAL();
            f := f*100.0;
            M[i,j] := Round(f);
                                                                  (*        M[i,j] := VAL(LONGREAL, FLOAT(RANDCARD(100))); *)
                                                                  (*        M[i,j] := VAL(LONGREAL, RANDREAL()); *)
        END (*FOR*);
      END (*FOR*);
    END Random;


PROCEDURE BelowSmallMakeZero(VAR (* InOut *) M : ARRAY OF ARRAY OF LONGREAL);

VAR
  r,c,N,m : CARDINAL;

BEGIN
  N := HIGH(M);
  m := HIGH(M[0]);

  FOR r := 0 TO N DO
    FOR c := 0 TO m DO
      IF M[r,c] < small THEN
        M[r,c] := 0.0
      END (* if < small *);
    END (* for c *);
  END (* for r *);
END BelowSmallMakeZero;



(************************************************************************)
(*                  THE STANDARD ARITHMETIC OPERATIONS                  *)
(************************************************************************)

                      (* PROCEDURE Add (A, B: ARRAY OF ARRAY OF LONGREAL;  r, c: CARDINAL; VAR (*OUT*) C: ARRAY OF ARRAY OF LONGREAL);*)
PROCEDURE Add (A, B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) C: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;

    (* Computes C := A + B.  All matrices are r x c. *)

    VAR i,j,r,c,r2,c2: CARDINAL;

    BEGIN
      r := HIGH(A);
      c := HIGH(A[0]);
      r2 := HIGH(B);
      c2 := HIGH(B[0]);
      IF (r <> r2) OR (c <> c2) THEN
        Zero(C);
        RETURN FALSE;
      END; (* if matrix dimensions don't match *)

      FOR i := 0 TO r DO
       FOR j := 0 TO c DO
         C[i,j] := A[i,j] + B[i,j];
       END (*FOR*);
      END (*FOR*);
      RETURN TRUE;
    END Add;

(************************************************************************)

                                      (* PROCEDURE Sub (A, B: ARRAY OF ARRAY OF LONGREAL;  r, c: CARDINAL; VAR (*OUT*) C: ARRAY OF ARRAY OF LONGREAL); *)
PROCEDURE Sub (A, B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) C: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;

    (* Computes C := A - B.  All matrices are r x c.  Returns TRUE on a successful operation, else FALSE *)

    VAR i,j,r,c,r2,c2: CARDINAL;

    BEGIN
      r := HIGH(A);
      c := HIGH(A[0]);
      r2 := HIGH(B);
      c2 := HIGH(B[0]);
      IF (r <> r2) OR (c <> c2) THEN
        Zero(C);
        RETURN FALSE;
      END; (* if matrix dimensions don't match *)

      FOR i := 0 TO r DO
        FOR j := 0 TO c DO
          C[i,j] := A[i,j] - B[i,j];
        END (*FOR*);
      END (*FOR*);
      RETURN TRUE;
    END Sub;

(************************************************************************)

                                (* PROCEDURE Mul (A, B: ARRAY OF ARRAY OF LONGREAL;  r, c1, c2: CARDINAL; VAR (*OUT*) C: ARRAY OF ARRAY OF LONGREAL); *)
PROCEDURE Mul (A, B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) C: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;

(*
  Computes C := A*B, where A is r x c1 and B is c1 x c2.
  Returns TRUE on a successful operation, else FALSE.
*)

    VAR
      i,j,k,rA,cA,rB,cB: CARDINAL;
      temp: LONGREAL;

    BEGIN
      rA := HIGH(A);     (* row A *)
      cA := HIGH(A[0]);  (* col A *)
      rB := HIGH(B);     (* row B *)
      cB := HIGH(B[0]);  (* col B *)
      IF (cA <> rB) THEN  (* matrix dimensions don't match.  Abort gracefully *)
        Zero(C);
        RETURN FALSE;
      END; (*if matrix dimensions don't match*)

      FOR i := 0 TO rA DO
       FOR j := 0 TO cB DO
         temp := 0.0;
         FOR k := 0 TO rB DO
           temp := temp + A[i,k]*B[k,j];
         END (*FOR*);
         C[i,j] := temp;
       END (*FOR*);
      END (*FOR*);
      RETURN TRUE;
    END Mul;

(************************************************************************)

                                                        (*  PROCEDURE ScalarMul (A: LONGREAL;  B: ARRAY OF ARRAY OF LONGREAL;  r, c: CARDINAL; VAR (*OUT*) C: ARRAY OF ARRAY OF LONGREAL); *)
PROCEDURE ScalarMul (A: LONGREAL;  B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) C: ARRAY OF ARRAY OF LONGREAL);

    (* Computes C := A*B, where B is r x c. *)

    VAR i,j,r,c: CARDINAL;

    BEGIN
      r := HIGH(B);
      c := HIGH(B[0]);
      FOR i := 0 TO r DO
        FOR j := 0 TO c DO
          C[i,j] := A * B[i,j];
        END (*FOR*);
      END (*FOR*);
    END ScalarMul;

(************************************************************************)
(*                      SOLVING LINEAR EQUATIONS                        *)
(************************************************************************)

                                         (* PROCEDURE LUFactor (VAR (*INOUT*) A: ARRAY OF ARRAY OF LONGREAL;  N: CARDINAL; perm: Permutation;  VAR (*OUT*) oddswaps: BOOLEAN); *)
PROCEDURE LUFactor (VAR (*INOUT*) A: ARRAY OF ARRAY OF LONGREAL;  perm: Permutation;  VAR (*OUT*) oddswaps: BOOLEAN) : BOOLEAN;

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

    VAR row, col, k, pivotrow, N : CARDINAL;
        sum, temp, maxval : LONGREAL;
        VV: VectorPtr;

    BEGIN
(*
  N is upper bound, not length.  This gives rise to many off by 1 errors.  So I'll just make N the length
  so I can exactly copy the code I have from before.
*)

      N := HIGH(A) + 1;  (* So now N is length, or number of elements, not the upper bound of A *)
      IF (N-1) <> HIGH(A[0]) THEN
        oddswaps := FALSE;
        RETURN FALSE;
      END; (* if A is not square *)

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

            (* Upper triangular component of this column -- except for  *)
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

        maxval := 0.0;
        pivotrow := col;
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
            maxval := temp;
            pivotrow := row;
          END (*IF*);
        END (*FOR row *);

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
      DisposeVector(VV);
      RETURN TRUE;
    END LUFactor;

(************************************************************************)

                                     (* PROCEDURE LUSolve (LU: ARRAY OF ARRAY OF LONGREAL; VAR (*INOUT*) B: ARRAY OF ARRAY OF LONGREAL; N, M: CARDINAL;  perm: Permutation); *)
PROCEDURE LUSolve (LU: ARRAY OF ARRAY OF LONGREAL; VAR (*INOUT*) B: ARRAY OF ARRAY OF LONGREAL; perm: Permutation) : BOOLEAN;

    (* Solves the equation P*L*U*X = B, where P is a permutation        *)
    (* matrix specified indirectly by perm; L is lower triangular; and  *)
    (* U is upper triangular.  The "Matrix" LU is not a genuine matrix, *)
    (* but rather a packed form of L and U as produced by procedure     *)
    (* LUfactor above.  On return B is replaced by the solution X.      *)
    (* Dimensions: left side is NxN, B is NxM.                          *)

    VAR
      i, j, k, ip, N, M : CARDINAL;
      sum, scale: LONGREAL;

    BEGIN
        (* Pass 1: Solve the equation L*Y = B (at the same time sorting *)
        (* B in accordance with the specified permutation).  The        *)
        (* solution Y overwrites the original value of B.               *)

        (* Understanding how the permutations work is something of a    *)
        (* black art.  It helps to know that (a) ip>=i for all i, and   *)
        (* (b) in the summation over k below, we are accessing only     *)
        (* those elements of B that have already been sorted into the   *)
        (* correct order.                                               *)

        N := HIGH(B) + 1;     (* To match orig code, N is number of elements, not high bound *)
        M := HIGH(B[0]) + 1;  (* Here too *)
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
                (* Error ("Matrix is singular"); I won't print a message, as the return flag indicates failure.  The client module with have to deal with the error. *)
                RETURN FALSE;
            END (*IF*);
            FOR j := 0 TO M-1 DO
                sum := B[i,j];
                FOR k := i+1 TO N-1 DO
                    sum := sum - LU[i,k] * B[k,j];
                END (*FOR*);
                B[i,j] := sum/scale;
            END (*FOR*);
        END (*FOR*);
        RETURN TRUE;
    END LUSolve;

(************************************************************************)

                                               (* PROCEDURE Solve (A, B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) X: ARRAY OF ARRAY OF LONGREAL; N, M: CARDINAL); *)
PROCEDURE Solve (A, B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) X: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;

    (* Solves the equation AX = B.  In the present version A must be    *)
    (* square and nonsingular.                                          *)
    (* Dimensions: A is N x N, B is N x M.                              *)

    VAR
      LU, error, product: ArrayPtr;
      perm: Permutation;
      s: BOOLEAN;
      N,M : CARDINAL;
      rA,cA,rB,cB,rX,cX : CARDINAL;
      bool : BOOLEAN;

    BEGIN
      rA := HIGH(A);
      cA := HIGH(A[0]);
      N := rA + 1;  (* N is number of elements, not the high bound *)
      rB := HIGH(B);
      cB := HIGH(B[0]);
      M := cB + 1;  (* M is number of elements, not the high bound *)
      rX := HIGH(X);
      cX := HIGH(X[0]);
      IF (rA <> cA) OR (cA <> rX) OR (rX <> rB) OR (cX <> cB) THEN (* matrix dim's don't match *)
          RETURN FALSE
      END; (* if Matrix dim's are not correct *)

      LU := NewArray (N, N);

      bool := Copy(A, LU^);                        (* Copy (A, N, N, LU^);  Copy (B, N, M, X); *)
      IF NOT bool THEN
        RETURN FALSE;
      END;
      bool := Copy(B,X);                           (*        Copy (B, N, M, X); *)
      IF NOT bool THEN
        RETURN FALSE;
      END;
      ALLOCATE (perm, N*SIZE(subscript));
      bool := LUFactor(LU^,perm,s);                (* LUFactor (LU^, N, perm, s); *)
      IF NOT bool THEN
        RETURN FALSE;
      END;
      bool := LUSolve(LU^,X,perm);                 (* LUSolve (LU^, X, N, M, perm); *)
      IF NOT bool THEN
        RETURN FALSE;
      END;

        (* For better accuracy, apply one step of iterative     *)
        (* improvement.  (Two or three steps might be better;   *)
        (* but they might even make things worse, because we're *)
        (* still stuck with the rounding errors in LUFactor.)   *)

        error := NewArray (N, M);
        product := NewArray (N, M);
        bool := Mul(A,X,product^);                   (*        Mul (A, X, N, N, M, product^); *)
        IF NOT bool THEN
            RETURN FALSE;
        END;
        bool := Sub(B,product^,error^);              (*        Sub (B, product^, N, M, error^); *)
        IF NOT bool THEN
            RETURN FALSE;
        END;
        bool := LUSolve(LU^,error^,perm);            (*        LUSolve (LU^, error^, N, M, perm); *)
        IF NOT bool THEN
            RETURN FALSE;
        END;
        bool := Add(X,error^,X);                     (*        Add (X, error^, N, M, X); *)
        IF NOT bool THEN
            RETURN FALSE;
        END;
        DisposeArray(product);      (*        DisposeArray (product, N, M);  DisposeArray (error, N, M); *)
        DisposeArray(error);

        DEALLOCATE (perm, N*SIZE(subscript));
        DisposeArray (LU);  (* This paniced.  I don't know why.  I'll try again *)
        RETURN TRUE;
    END Solve;

(************************************************************************)

                                   (* PROCEDURE GaussJ (A, B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) X: ARRAY OF ARRAY OF LONGREAL; N, M: CARDINAL); *)
PROCEDURE GaussJ (A, B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) X: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;

    (* Solves the equation AX = B by Gauss-Jordan elimination.  In the  *)
    (* present version A must be square and nonsingular.                *)
    (* Dimensions: A is NxN, B is NxM.                                  *)

    VAR W: ArrayPtr;
        i, j, k, prow, N, M, N2, x1, x2: CARDINAL;
        pivot, scale, temp: LONGREAL;
        bool: BOOLEAN;

    BEGIN
        N := HIGH(A) + 1;
        N2 := HIGH(A[0]) + 1;
        M := HIGH(B[0]) + 1;
        x1 := HIGH(X) + 1;
        x2 := HIGH(X[0]) + 1;
        IF (N <> N2) OR (M <> x2) OR (N <> x1) THEN (* matrix dim's don't match *)
          RETURN FALSE
        END; (* if Matrix dim's are not correct *)


        W := NewArray (N, N);
                                                               (*        Copy (A, N, N, W^);  Copy (B, N, M, X); *)
        Copy (A, W^);
        Copy (B, X);

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
                    pivot := temp;
                    prow := j;
                END (*IF*);
            END (*FOR*);
            IF ABS(pivot) < small THEN
                                                                  (* Error ("Coefficient matrix is singular"); *)
              RETURN FALSE;
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
                                                                                      (* DisposeArray (W, N, N); *)
        DisposeArray (W);
        RETURN TRUE;
    END GaussJ;

(************************************************************************)

                                          (* PROCEDURE Invert (A: ARRAY OF ARRAY OF LONGREAL;VAR (*INOUT*) X: ARRAY OF ARRAY OF LONGREAL; N: CARDINAL); *)
PROCEDURE Invert (A: ARRAY OF ARRAY OF LONGREAL; VAR (*INOUT*) X: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;

    (* Inverts an NxN nonsingular matrix. *)

    VAR
        I : ArrayPtr;
        bool : BOOLEAN;
        N : CARDINAL;

    BEGIN
        N := HIGH(A) + 1;
        I := NewArray(N, N);
        Unit(I^);                                    (* Unit (I^, N); *)
        bool := Solve (A, I^, X);
        DisposeArray(I);    (* DisposeArray (I, N, N); *)
        RETURN bool;
    END Invert;

(************************************************************************)
(*                              OUTPUT                                  *)
(************************************************************************)

                                                                               (* PROCEDURE Write (M: ARRAY OF ARRAY OF LONGREAL;  r, c: CARDINAL;  places: CARDINAL); *)
PROCEDURE Write (M: ARRAY OF ARRAY OF LONGREAL; places: CARDINAL);

    (* Writes the rxc matrix M to the screen, where each column *)
    (* occupies a field "places" characters wide.               *)

    VAR i, j, r, c : CARDINAL;

    BEGIN
      r := HIGH(M);
      c := HIGH(M[0]);
      FOR i := 0 TO r DO
        FOR j := 0 TO c DO
          WriteString ("  ");
          WriteLongReal (M[i,j], places);
        END (*FOR*);
        WriteLn;
      END (*FOR*);
    END Write;

(************************************************************************)
BEGIN
  (* empty module body *)
END Mat.
