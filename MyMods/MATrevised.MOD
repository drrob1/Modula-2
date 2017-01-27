IMPLEMENTATION MODULE Matrevised;

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
   6 Jan 17 -- I created this module, Matrevised, to see if I can fix all the N-1 references that I could not fix before.
*)


FROM Storage IMPORT (* proc *)  ALLOCATE, DEALLOCATE;

FROM Vec IMPORT (* type *)  VectorPtr, (* proc *)  NewVector, DisposeVector;

FROM Float IMPORT Round;

FROM MiscStdInOut IMPORT Error, WriteString, WriteLn, WriteLongReal, Sqrt;

FROM MyRand3revised IMPORT RANDCARD, RANDREAL;

FROM VLI IMPORT VLI, SetZero, SetValue, SetRandomSeed, GetRandomSmaller, Dispose;

(*	
VLI;
PROCEDURE Create() : VLI;      returns vli = 0
PROCEDURE Copy(vli : VLI) : VLI;   returns copy of vli
PROCEDURE Dispose(VAR INOUT vli : VLI);  disposes of vli, vli = NIL after this call
PROCEDURE Assign(vliSrc : VLI; VAR INOUT vliDst : VLI);  assign src to dst
PROCEDURE SetZero(vli : VLI);  sets vli = 0
PROCEDURE SetValue(vli : VLI; value : INTEGER64);  a simple way to load a "small" integer value
PROCEDURE SetRandomSeed(seed : CARDINAL); Initialize the random number generator with the given seed value.  For a given seed value the random number sequence will be identical.
   Use a value of zero, for a random seed value.

PROCEDURE GetRandom(bits : CARDINAL; VAR INOUT result : VLI);   returns random vli with cBits size
PROCEDURE GetRandomSmaller(vliA : VLI; VAR INOUT result : VLI);  returns random vli smaller than vliA
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
        NEW(result,N,M);                      (*  NEW (result, N-1, M-1); *)
        RETURN result;
    END NewArray;

(************************************************************************)

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

PROCEDURE Unit (VAR (*INOUT*) M: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;
    (* Creates an N by N unit matrix. *)

    VAR i,N: CARDINAL;

    BEGIN
      N := HIGH(M);
      IF N <> HIGH(M[0]) THEN
        RETURN FALSE;
      END;

      Zero(M);
      FOR i := 0 TO N DO
        M[i,i] := 1.0;
      END (*FOR*);
      RETURN TRUE;
    END Unit;

(************************************************************************)
(*	
VLI;
PROCEDURE Create() : VLI;      returns vli = 0
PROCEDURE Copy(vli : VLI) : VLI;   returns copy of vli
PROCEDURE Dispose(VAR INOUT vli : VLI);  disposes of vli, vli = NIL after this call
PROCEDURE Assign(vliSrc : VLI; VAR INOUT vliDst : VLI);  assign src to dst
PROCEDURE SetZero(vli : VLI);  sets vli = 0
PROCEDURE SetValue(vli : VLI; value : INTEGER64);  a simple way to load a "small" integer value
PROCEDURE SetRandomSeed(seed : CARDINAL); Initialize the random number generator with the given seed value.  For a given seed value the random number sequence will be identical.
   Use a value of zero, for a random seed value.

PROCEDURE GetRandom(bits : CARDINAL; VAR INOUT result : VLI);   returns random vli with cBits size
PROCEDURE GetRandomSmaller(vliA : VLI; VAR INOUT result : VLI);  returns random vli smaller than vliA
*)

PROCEDURE Random (VAR (*INOUT*) M: ARRAY OF ARRAY OF LONGREAL);
    (* Creates an r by c matrix with random entries. *)

    VAR
      i,j,r,c: CARDINAL;
      f : LONGREAL;
      v,limit : VLI;
      rand := CARDINAL;

    BEGIN
        r := HIGH(M);
        c := HIGH(M[0]);
        limit := Create();
        SetValue(limit,100);
        
      FOR i := 0 TO r DO
        FOR j := 0 TO c DO
        	GetRandomSmaller(limit,v);
        	rand := ORD(v);
          M[i,j] := LFLOAT(rand);
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

PROCEDURE Mul (A, B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) C: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;
(*
  Computes C := A*B, where A is r x c1 and B is c1 x c2.  Returns TRUE on a successful operation, else FALSE.
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
  That was before.  Now here in Matrevised, I'll keep N as an upper bound, and remove all of the N-1 references.
*)

      N := HIGH(A);  (* N was length, or number of elements, now it's the upper bound of A *)
      IF N <> HIGH(A[0]) THEN
        oddswaps := FALSE;
        RETURN FALSE;
      END; (* if A is not square *)

      VV := NewVector (N);
      oddswaps := FALSE;

        (* Start by collecting (in VV), the maximum absolute value in   *)
        (* each row; we'll use this for pivoting decisions.             *)

      FOR row := 0 TO N DO
        maxval := 0.0;
        FOR col := 0 TO N DO
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

      FOR col := 0 TO N DO

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
        FOR row := col TO N DO
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
          FOR k := 0 TO N DO
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
        IF (col <> N) AND (temp <> 0.0) THEN
          temp := 1.0/temp;
          FOR row := col+1 TO N DO
            A[row,col] := temp*A[row,col];
          END (*FOR*);
        END (*IF*);

      END (*FOR*);
      DisposeVector(VV);
      RETURN TRUE;
    END LUFactor;

(************************************************************************)

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

        N := HIGH(B);     (* To match orig code, N was number of elements, but now it's high bound again. *)
        M := HIGH(B[0]);  (* Here too *)
        FOR i := 0 TO N DO
            ip := perm^[i];
            FOR j := 0 TO M DO
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

        FOR i := N TO 0 BY -1 DO
            scale := LU[i,i];
            IF scale = 0.0 THEN
                (* Error ("Matrix is singular"); I won't print a message, as the return flag indicates failure.  The client module with have to deal with the error. *)
                RETURN FALSE;
            END (*IF*);
            FOR j := 0 TO M DO
                sum := B[i,j];
                FOR k := i+1 TO N DO
                    sum := sum - LU[i,k] * B[k,j];
                END (*FOR*);
                B[i,j] := sum/scale;
            END (*FOR*);
        END (*FOR*);
        RETURN TRUE;
    END LUSolve;

(************************************************************************)

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
      N := rA;  (* N was number of elements, now it's the high bound again *)
      rB := HIGH(B);
      cB := HIGH(B[0]);
      M := cB;  (* M was number of elements, now it's the high bound again *)
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

PROCEDURE GaussJ (A, B: ARRAY OF ARRAY OF LONGREAL; VAR (*OUT*) X: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;
    (* Solves the equation AX = B by Gauss-Jordan elimination.  In the  *)
    (* present version A must be square and nonsingular.                *)
    (* Dimensions: A is NxN, B is NxM.                                  *)

    VAR W: ArrayPtr;
        i, j, k, prow, N, M, N2, x1, x2: CARDINAL;
        pivot, scale, temp: LONGREAL;
        bool: BOOLEAN;

    BEGIN
        N := HIGH(A);
        N2 := HIGH(A[0]);
        M := HIGH(B[0]);
        x1 := HIGH(X);
        x2 := HIGH(X[0]);
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
        FOR i := 0 TO N DO
            pivot := 0.0;
            FOR j := i TO N DO
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

            FOR j := i TO N DO
                temp := W^[i,j];
                W^[i,j] := W^[prow,j];
                W^[prow,j] := temp;
            END (*FOR*);
            FOR j := 0 TO M DO
                temp := X[i,j];
                X[i,j] := X[prow,j];
                X[prow,j] := temp;
            END (*FOR*);

            (* Scale the i'th row of both W and X. *)

            FOR j := i+1 TO N DO
                W^[i,j] := W^[i,j]/pivot;
            END (*FOR*);
            FOR j := 0 TO M DO
                X[i,j] := X[i,j]/pivot;
            END (*FOR*);

            (* Implicitly reduce the sub-column below W[i,i] to zero.   *)

            FOR k := i+1 TO N DO
                scale := W^[k,i];
                FOR j := i+1 TO N DO
                    W^[k,j] := W^[k,j] - scale*W^[i,j];
                END (*FOR*);
                FOR j := 0 TO M DO
                    X[k,j] := X[k,j] - scale*X[i,j];
                END (*FOR*);
            END (*FOR*);

        END (*FOR*);

        (* Pass 2: reduce the above-diagonal elements of W to zero.     *)

        FOR i := N TO 1 BY -1 DO

            (* Implicitly reduce the sub-column above W[i,i] to zero.   *)

            FOR k := 0 TO i-1 DO
                scale := W^[k,i];
                FOR j := 0 TO M DO
                    X[k,j] := X[k,j] - scale*X[i,j];
                END (*FOR*);
            END (*FOR*);

        END (*FOR*);
                                                                                      (* DisposeArray (W, N, N); *)
        DisposeArray (W);
        RETURN TRUE;
    END GaussJ;

(************************************************************************)

PROCEDURE Invert (A: ARRAY OF ARRAY OF LONGREAL; VAR (*INOUT*) X: ARRAY OF ARRAY OF LONGREAL) : BOOLEAN;
    (* Inverts an NxN nonsingular matrix. *)

    VAR
        I : ArrayPtr;
        bool : BOOLEAN;
        N : CARDINAL;

    BEGIN
        N := HIGH(A);
        I := NewArray(N, N);
        Unit(I^);                                    (* Unit (I^, N); *)
        bool := Solve (A, I^, X);
        DisposeArray(I);                   (* DisposeArray (I, N, N); *)
        RETURN bool;
    END Invert;

(************************************************************************)
(*                              OUTPUT                                  *)
(************************************************************************)

PROCEDURE Write (M: ARRAY OF ARRAY OF LONGREAL; places: CARDINAL);
    (* Writes the r x c matrix M to the screen, where each column *)
    (* occupies a field "places" characters wide.                 *)

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
  SetRandomSeed(0);
END Matrevised.
