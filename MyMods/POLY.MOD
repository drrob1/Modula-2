IMPLEMENTATION MODULE Poly;

        (********************************************************)
        (*                                                      *)
        (*              Polynomial arithmetic                   *)
        (*      This version uses a vector representation       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        31 July 1996                    *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* proc *)  ADR;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LongComplexMath IMPORT
    (* proc *)  abs, conj, sqrt, scalarMult;

IMPORT MiscM2;
FROM MiscM2 IMPORT
    (* proc *)  PressAnyKey, WriteString, WriteLn, Error, Sqrt,
                LongRealToString;

(************************************************************************)

CONST small = 1.0E-12;

TYPE
    power = [0..8190];

    (* We represent a polynomial as a vector of coefficients, with      *)
    (* the zero-degree term coming first.                               *)

    CoeffPointer = POINTER TO ARRAY power OF CoeffType;
    Polynomial = POINTER TO
                    RECORD
                        degree: CARDINAL;
                        pcoeffs: CoeffPointer;
                    END (*RECORD*);

(************************************************************************)
(*
PROCEDURE Checkpoint (message: ARRAY OF CHAR);

    BEGIN
        WriteString (message);  WriteLn;
        PressAnyKey;
    END Checkpoint;
*)
(************************************************************************)
(*                CREATING AND DESTROYING POLYNOMIALS                   *)
(************************************************************************)

PROCEDURE Init (VAR (*OUT*) P: Polynomial);

    (* This should be the first operation performed on P, since this    *)
    (* module needs to keep track of which polynomials have already had *)
    (* space allocated for them.  It creates the zero polynomial.       *)

    BEGIN
        P := NIL;
    END Init;

(************************************************************************)

PROCEDURE Reduce (VAR (*INOUT*) P: Polynomial);

    (* If possible, reduces the degree of P by removing negligible      *)
    (* higher-order terms.                                              *)

    VAR N: power;  bytecount: CARDINAL;
        newcoeffs: CoeffPointer;

    BEGIN
        IF P <> NIL THEN
            N := P^.degree;
            WHILE (N > 0) AND (ABS(P^.pcoeffs^[N]) < small) DO
                DEC (N);
            END (*WHILE*);
            IF (N = 0) AND (ABS(P^.pcoeffs^[0]) < small) THEN
                DEALLOCATE (P^.pcoeffs, (P^.degree+1)*SIZE(CoeffType));
                DISPOSE (P);
            ELSIF N < P^.degree THEN
                bytecount := (N+1)*SIZE(CoeffType);
                ALLOCATE (newcoeffs, bytecount);
                MiscM2.BlockCopy (P^.pcoeffs, newcoeffs, bytecount);
                DEALLOCATE (P^.pcoeffs, (P^.degree+1)*SIZE(CoeffType));
                P^.pcoeffs := newcoeffs;
                P^.degree := N;
            END (*IF*);
        END (*IF*);
    END Reduce;

(************************************************************************)

PROCEDURE Make (N: power): Polynomial;

    (* Creates a polynomial of degree N.  The caller still has to       *)
    (* fill in the coefficient values.                                  *)

    VAR result: Polynomial;

    BEGIN
        NEW (result);
        WITH result^ DO
            degree := N;
            ALLOCATE (pcoeffs, (N+1)*SIZE(CoeffType));
        END (*WITH*);
        RETURN result;
    END Make;

(************************************************************************)

PROCEDURE Assign (VAR (*INOUT*) P: Polynomial;
                                coeffs: ARRAY OF CoeffType);

    (* Creates a polynomial with specified coefficients.  The previous  *)
    (* value, if any, is lost.  The coefficients are specified from     *)
    (* low to high degree; for example, the coefficient set specified   *)
    (* by the array (1.0, 2.0, 3.0) gives the second-degree polynomial  *)
    (* 1.0 + 2.0*x + 3.0*x^2.                                           *)

    BEGIN
        Destroy (P);
        P := Make (HIGH(coeffs));
        WITH P^ DO
            MiscM2.BlockCopy (ADR(coeffs), pcoeffs, (degree+1)*SIZE(CoeffType));
        END (*WITH*);
        Reduce (P);
    END Assign;

(************************************************************************)

PROCEDURE Destroy (VAR (*INOUT*) P: Polynomial);

    (* Deallocates the space occupied by P.  P is still considered to   *)
    (* exist, and its value is the zero polynomial.                     *)

    BEGIN
        IF P <> NIL THEN
            DEALLOCATE (P^.pcoeffs, (P^.degree+1)*SIZE(CoeffType));
            DISPOSE (P);
        END (*IF*);
    END Destroy;

(************************************************************************)
(*                      SOME INTERNAL UTILITIES                         *)
(************************************************************************)

PROCEDURE Copy (P: Polynomial): Polynomial;

    (* Creates a copy of a polynomial. *)

    VAR result: Polynomial;

    BEGIN
        result := NIL;
        IF P <> NIL THEN
            result := Make (P^.degree);
            WITH result^ DO
                MiscM2.BlockCopy (P^.pcoeffs, pcoeffs,
                                (degree+1)*SIZE(CoeffType));
            END (*WITH*);
        END (*IF*);
        RETURN result;
    END Copy;

(************************************************************************)
(*                        THE BASIC OPERATIONS                          *)
(************************************************************************)

PROCEDURE Degree (P: Polynomial): INTEGER;

    (* Returns the degree of P, i.e. the power of the most significant  *)
    (* term.  The degree of a constant is 0, but the degree of the      *)
    (* constant 0.0 is defined to be -1.                                *)

    BEGIN
        IF P = NIL THEN RETURN -1
        ELSE RETURN P^.degree
        END (*IF*);
    END Degree;

(************************************************************************)

PROCEDURE Add (A, B: Polynomial;  VAR (*INOUT*) C: Polynomial);

    (* Computes C := A + B.  Note that we don't copy the result to C    *)
    (* until the end of the computation, in case C is an alias for      *)
    (* one of A or B.                                                   *)

    VAR result: Polynomial;  j: power;

    BEGIN
        IF Degree (B) > Degree (A) THEN
            result := B;  B := A;  A := result;
        END (*IF*);
        result := Copy (A);
        IF B <> NIL THEN
            FOR j := 0 TO B^.degree DO
                result^.pcoeffs^[j] := result^.pcoeffs^[j] + B^.pcoeffs^[j];
            END (*FOR*);
        END (*IF*);
        Reduce (result);
        Destroy (C);  C := result;
    END Add;

(************************************************************************)

PROCEDURE Negate (P: Polynomial);

    (* P := -P.  This is an in-place operation, i.e. the original       *)
    (* value of P is overwritten.                                       *)

    VAR j: power;

    BEGIN
        IF P <> NIL THEN
            FOR j := 0 TO P^.degree DO
                P^.pcoeffs^[j] := -P^.pcoeffs^[j];
            END (*FOR*);
        END (*IF*);
    END Negate;

(************************************************************************)

PROCEDURE Sub (A, B: Polynomial;  VAR (*INOUT*) C: Polynomial);

    (* Computes C := A - B. *)

    VAR temp: Polynomial;

    BEGIN
        temp := Copy (B);  Negate (temp);
        Add (A, temp, C);
        Destroy (temp);
    END Sub;

(************************************************************************)

PROCEDURE Mul (A, B: Polynomial;  VAR (*INOUT*) C: Polynomial);

    (* Computes C := A*B. *)

    VAR result: Polynomial;  j, k, M, N, first, last: power;
        sum: CoeffType;

    BEGIN
        IF (A = NIL) OR (B = NIL) THEN
            result := NIL;
        ELSE
            M := A^.degree;  N := B^.degree;
            result := Make (M+N);
            FOR k := 0 TO M+N DO
                sum := 0.0;
                IF k < N THEN first := 0 ELSE first := k-N END(*IF*);
                IF k > M THEN last := M ELSE last := k END(*IF*);
                FOR j := first TO last DO
                    sum := sum + A^.pcoeffs^[j] * B^.pcoeffs^[k-j];
                END (*FOR*);
                result^.pcoeffs^[k] := sum;
            END (*FOR*);
        END (*IF*);
        Destroy (C);  C := result;
    END Mul;

(************************************************************************)

PROCEDURE Div (A, B: Polynomial;  VAR (*INOUT*) Q, R: Polynomial);

    (* Computes A/B.  On return the quotient is Q and the       *)
    (* remainder is R.                                          *)

    VAR quot, rem: Polynomial;  j, k: power;
        scale: CoeffType;

    BEGIN
        IF B = NIL THEN
            Error ("Division by zero");  RETURN;
        END (*IF*);
        rem := Copy(A);
        IF Degree(A) < Degree(B) THEN
            quot := NIL;
        ELSE
            quot := Make (A^.degree - B^.degree);
            FOR j := quot^.degree TO 0 BY -1 DO
                scale := rem^.pcoeffs^[j+B^.degree] / B^.pcoeffs^[B^.degree];
                quot^.pcoeffs^[j] := scale;
                FOR k := 0 TO B^.degree-1 DO
                    rem^.pcoeffs^[j+k] := rem^.pcoeffs^[j+k]
                                                - scale * B^.pcoeffs^[k];
                END (*FOR*);
                rem^.pcoeffs^[j+B^.degree] := 0.0;
            END (*FOR*);
        END (*IF*);
        Reduce (quot);  Reduce (rem);
        Destroy (Q);  Q := quot;
        Destroy (R);  R := rem;
    END Div;

(************************************************************************)
(*              EVALUATING A POLYNOMIAL AT A GIVEN POINT                *)
(************************************************************************)

PROCEDURE EvalR (P: Polynomial;  x: LONGREAL): LONGREAL;

    (* Returns P(x), for real x. *)

    VAR sum: LONGREAL;  j: power;

    BEGIN
        sum := 0.0;
        IF P <> NIL THEN
            FOR j := P^.degree TO 0 BY -1 DO
                sum := P^.pcoeffs^[j] + x * sum;
            END (*FOR*);
        END (*IF*);
        RETURN sum;
    END EvalR;

(************************************************************************)

PROCEDURE Eval (P: Polynomial;  x: LONGCOMPLEX): LONGCOMPLEX;

    (* Returns P(x), for complex x. *)

    VAR sum: LONGCOMPLEX;  j: power;

    BEGIN
        sum := CMPLX (0.0, 0.0);
        IF P <> NIL THEN
            FOR j := P^.degree TO 0 BY -1 DO
                sum := CMPLX (P^.pcoeffs^[j], 0.0) + x*sum;
            END (*FOR*);
        END (*IF*);
        RETURN sum;
    END Eval;

(************************************************************************)

PROCEDURE EvalD (P: Polynomial;  x: LONGCOMPLEX;
                                VAR (*OUT*) value, derivative: LONGCOMPLEX);

    (* Returns P(x) and the derivative of P at x. *)

    VAR j: power;  cj, jval: LONGCOMPLEX;

    BEGIN
        value := CMPLX (0.0, 0.0);
        derivative := value;
        IF P <> NIL THEN
            FOR j := P^.degree TO 1 BY -1 DO
                cj := CMPLX (P^.pcoeffs^[j], 0.0);
                jval := CMPLX (VAL(CoeffType, j), 0.0);
                value := cj + x*value;
                derivative := jval*cj + x*derivative;
            END (*FOR*);
            value := CMPLX (P^.pcoeffs^[0], 0.0) + x*value;
        END (*IF*);
    END EvalD;

(************************************************************************)
(*                      ROOTS OF POLYNOMIALS                            *)
(************************************************************************)

PROCEDURE Newton (P: Polynomial;  VAR (*INOUT*) root: LONGCOMPLEX);

    (* Improves an initial guess at a root of the equation P(x) = 0 by  *)
    (* Newton's method.  We assume that the input value of root is      *)
    (* close enough to make Newton's method appropriate.                *)

    VAR val, deriv, step: LONGCOMPLEX;

    BEGIN
        LOOP
            EvalD (P, root, val, deriv);
            IF abs(deriv) < small THEN
                (* We're not going to converge from here *)
                EXIT (*LOOP*);
            END (*IF*);
            step := val/deriv;
            IF abs(step) < small THEN
                (* We've converged. *)
                EXIT (*LOOP*);
            END (*IF*);
            root := root - step;
        END (*LOOP*);
    END Newton;

(************************************************************************)

PROCEDURE CxMueller (P: Polynomial;  x0, x1: LONGCOMPLEX;
                                        VAR (*INOUT*) root: LONGCOMPLEX);

    (* A version of Mu"ller's algorithm (see below) using complex       *)
    (* arithmetic.  The starting points are x0,x1,root, and the final   *)
    (* result is root.                                                  *)

    VAR y0, y1, y2: LONGCOMPLEX;
        a, b, prevstep, step, step1, step2, olddelta, delta: LONGCOMPLEX;

    BEGIN
        prevstep := x1 - x0;
        step := root - x1;
        y0 := Eval (P, x0);
        y1 := Eval (P, x1);
        olddelta := (y1-y0)/prevstep;
        LOOP
            y2 := Eval (P, root);
            delta := (y2-y1)/step;
            a := (delta-olddelta)/(prevstep+step);
            b := delta + step*a;
            prevstep := step;

            step := sqrt (b*b - scalarMult(4.0,a)*y2);
            step1 := b + step;
            step2 := b - step;
            IF abs(step1) >= abs(step2) THEN
                step := step1;
            ELSE
                step := step2;
            END (*IF*);
            IF abs(step) < small THEN
                EXIT (*LOOP*);
            END (*IF*);
            step := scalarMult(-2.0,y2) / step;
            IF abs(step) < small THEN
                EXIT (*LOOP*);
            END (*IF*);
            root := root + step;
            y1 := y2;  olddelta := delta;
        END (*LOOP*);
    END CxMueller;

(************************************************************************)

PROCEDURE Mueller (P: Polynomial;  VAR (*OUT*) root: LONGCOMPLEX);

    (* Finds one root of the equation P(x) = 0 by Mu"llers method.      *)

    (* Method: we fit a quadratic to three approximations x0, x1, x2.   *)
    (* (Initially these can be very poor approximations).  Then we      *)
    (* throw away x0 and take one root of the quadratic as the third    *)
    (* approximation.  We use real arithmetic for as long as possible,  *)
    (* and switch to complex arithmetic only when it's unavoidable.     *)

    CONST Initialx0 = -1.0;
          Initialx1 = 0.0;
          Initialx2 = 1.0;

    VAR x0, x1, x2, y0, y1, y2: LONGREAL;
        a, b, discr, prevstep, step, olddelta, delta: LONGREAL;

    BEGIN
        (* First eliminate a pathological case: a root at the origin.   *)

        IF ABS(P^.pcoeffs^[0]) < small THEN
            root := CMPLX (0.0, 0.0);  RETURN;
        END (*IF*);

        x0 := Initialx0;
        x1 := Initialx1;
        x2 := Initialx2;

        y1 := EvalR (P, Initialx1);

        (* Another troublesome case is where P(x) has the same value    *)
        (* at all three points.  The following loop is designed to      *)
        (* avoid that situation.                                        *)

        LOOP
            y0 := EvalR (P, x0);
            IF ABS(y1-y0) > small THEN EXIT(*LOOP*) END(*IF*);
            x0 := 2.0*x0;
        END (*LOOP*);

        step := Initialx2 - Initialx1;
        prevstep := Initialx1 - x0;
        olddelta := (y1-y0)/prevstep;

        (* Now for the main loop. *)

        LOOP
            y2 := EvalR (P, x2);
            delta := (y2-y1)/step;
            a := (delta - olddelta)/(prevstep+step);
            b := delta + step*a;
            prevstep := step;

            (* We've fitted the quadratic                               *)
            (*          y = a(x-x2)^2 + b(x-x2) + y2                    *)
            (* to the three sample points.  Now solve this for y = 0.   *)
            (* We choose the solution closest to x2.                    *)

            discr := b*b - 4.0*a*y2;
            IF discr < 0.0 THEN
                (* Need to go to a complex solution. *)
                EXIT (*LOOP*);
            END (*IF*);
            IF b >= 0.0 THEN
                step := b + Sqrt(discr);
            ELSE
                step := b - Sqrt(discr);
            END (*IF*);
            IF ABS(step) < small THEN
                (* Either we've converged (if y2 is small), or the      *)
                (* calculation is about to blow up.                     *)
                EXIT (*LOOP*);
            END (*IF*);
            step := -2.0*y2/step;
            IF ABS(step) < small THEN
                (* We've converged. *)
                EXIT (*LOOP*);
            END (*IF*);

            x1 := x2;  x2 := x2 + step;
            y1 := y2;  olddelta := delta;

        END (*LOOP*);

        IF discr >= 0.0 THEN

            (* We've found a real root. *)

            root := CMPLX (x2, 0.0);

        ELSE
            (* We have to continue the calculation using complex        *)
            (* arithmetic.                                              *)

            root := CMPLX(x2,0.0) - CMPLX(2.0*y2,0.0) / CMPLX(b, Sqrt(-discr));
            CxMueller (P, CMPLX(x1,0.0), CMPLX(x2,0.0), root);

        END (*IF*);

    END Mueller;

(************************************************************************)

PROCEDURE SolveQuadratic (P: Polynomial;
                                VAR (*OUT*) roots: ARRAY OF LONGCOMPLEX);

    (* Finds both solutions to the quadratic equation P(x) = 0.  This   *)
    (* procedure should be called only if P has degree 2.               *)

    VAR a, b, c, discr: LONGREAL;

    BEGIN
        a := P^.pcoeffs^[2];
        b := 0.5 * P^.pcoeffs^[1];
        c := P^.pcoeffs^[0];
        discr := b*b - a*c;
        IF discr < 0.0 THEN

            (* Complex roots. *)

            roots[0] := CMPLX (-b/a, Sqrt(-discr) / a);
            roots[1] := conj (roots[0]);

        ELSE

            (* A pair of real roots.  For best accuracy we compute the  *)
            (* larger root first, and get the other from the fact that  *)
            (* the product of the roots is c/a.                         *)

            IF b < 0.0 THEN
                discr := (-b + Sqrt(discr))/a;
            ELSE
                discr := (-b - Sqrt(discr))/a;
            END (*IF*);
            roots[0] := CMPLX (discr, 0.0);
            roots[1] := CMPLX (c/discr/a, 0.0);
        END (*IF*);

    END SolveQuadratic;

(************************************************************************)

PROCEDURE FindApproxRoots (P: Polynomial;
                                VAR (*OUT*) roots: ARRAY OF LONGCOMPLEX);

    (* Finds all (we hope) the solutions to P(x) = 0.  The results are  *)
    (* not guaranteed to be especially accurate; the caller should      *)
    (* probably refine the solutions.                                   *)
    (* Assumption: P <> NIL.                                            *)

    VAR factor, quot, rem: Polynomial;
        oneroot: LONGCOMPLEX;  re, im: LONGREAL;

    BEGIN
        IF P^.degree = 1 THEN
            roots[0] := CMPLX ( -P^.pcoeffs^[0]/P^.pcoeffs^[1], 0.0);
        ELSIF P^.degree = 2 THEN
            SolveQuadratic (P, roots);
        ELSE
            Init (quot);  Init (rem);

            (* Degree of P is greater than 2.  First find one root      *)
            (* by Mu"ller's method.                                     *)

            (* Checkpoint ("FindApproxRoots, before Mueller call"); *)

            Mueller (P, oneroot);

            (* Checkpoint ("FindApproxRoots, after Mueller call"); *)

            IF ABS(IM(oneroot)) < small THEN

                (* One real root found. *)

                oneroot := CMPLX (RE(oneroot), 0.0);
                roots[P^.degree-1] := oneroot;
                factor := Make(1);
                factor^.pcoeffs^[0] := -RE(oneroot);
                factor^.pcoeffs^[1] := 1.0;
            ELSE
                (* Complex conjugate pair found. *)

                roots[P^.degree-1] := oneroot;
                roots[P^.degree-2] := conj(oneroot);
                factor := Make(2);
                re := RE (oneroot);
                im := IM (oneroot);
                WITH factor^ DO
                    pcoeffs^[0] := re*re + im*im;
                    pcoeffs^[1] := -2.0*re;
                    pcoeffs^[2] := 1.0;
                END (*WITH*);
            END (*IF*);

            (* Checkpoint ("FindApproxRoots, before deflation"); *)

            (* Deflate the polynomial, and find the other roots. *)

            Div (P, factor, quot, rem);
            Destroy (factor);  Destroy (rem);
            FindApproxRoots (quot, roots);
            Destroy (quot);

        END (*IF*);

    END FindApproxRoots;

(************************************************************************)

PROCEDURE FindRoots (P: Polynomial;  VAR (*OUT*) roots: ARRAY OF LONGCOMPLEX);

    (* Finds all (we hope) the solutions to P(x) = 0. *)

    VAR j: power;

    BEGIN
        IF P <> NIL THEN

            FindApproxRoots (P, roots);

            (* The polynomial deflations can create a lot of    *)
            (* cumulative rounding error; so now we go back and *)
            (* improve the estimates, working directly from the *)
            (* original polynomial.                             *)

            FOR j := 0 TO P^.degree-1 DO
                Newton (P, roots[j]);
            END (*FOR*);

        END (*IF*);

    END FindRoots;

(************************************************************************)
(*                          SCREEN OUTPUT                               *)
(************************************************************************)

PROCEDURE Write (P: Polynomial;  places, linesize: CARDINAL);

    (* Writes P to the screen, where each coefficient is allowed to be  *)
    (* up to "places" characters wide, and "linesize" is the number of  *)
    (* characters allowed before we have to wrap onto a new line.       *)

    CONST Nul = CHR(0);  Space = ' ';

    TYPE subscript = [0..8191];
         WhichHalf = (upper, lower);

    VAR buffer: POINTER TO ARRAY WhichHalf,subscript OF CHAR;
        nextloc, checkpoint: subscript;

    (********************************************************************)

    PROCEDURE FlushBuffer;

        (* Writes out the buffer contents to the current line.  If the  *)
        (* buffer contains characters beyond the checkpoint, we put out *)
        (* everything up the checkpoint, and reshuffle the buffer       *)
        (* contents so that it's now loaded with what has to go out on  *)
        (* the next line.                                               *)

        VAR j: WhichHalf;  k: subscript;  temp: CHAR;

        BEGIN
            temp := "?";        (* to suppress a compiler warning *)
            FOR j := upper TO lower DO
                IF checkpoint < linesize THEN
                    temp := buffer^[j][checkpoint];
                    buffer^[j][checkpoint] := Nul;
                END (*IF*);
                WriteString (buffer^[j]);
                IF checkpoint < nextloc THEN
                    buffer^[j][0] := temp;
                    FOR k := 1 TO nextloc-checkpoint-1 DO
                        buffer^[j][k] := buffer^[j][k+checkpoint];
                    END (*FOR*);
                END (*IF*);
                WriteLn;
            END (*FOR*);
            DEC (nextloc, checkpoint);  checkpoint := 0;
        END FlushBuffer;

    (********************************************************************)

    PROCEDURE PutChar (hilo: WhichHalf;  ch: CHAR);

        (* Appends ch to either the upper or lower half of the buffer.  *)
        (* Corresponding positions in the other half are space-filled.  *)

        BEGIN
            IF nextloc >= linesize THEN
                FlushBuffer;
            END (*IF*);
            buffer^[hilo,nextloc] := ch;
            buffer^[VAL(WhichHalf,1-ORD(hilo)), nextloc] := Space;
            INC (nextloc);
        END PutChar;

    (********************************************************************)

    PROCEDURE PutString (hilo: WhichHalf;  str: ARRAY OF CHAR;  from: CARDINAL);

        (* Appends str[from..] to either the upper or lower half of the *)
        (* buffer.                                                      *)
        (* Corresponding positions in the other half are space-filled.  *)

        VAR j: subscript;

        BEGIN
            j := from;
            WHILE (j <= HIGH(str)) AND (str[j] <> Nul) DO
                PutChar (hilo, str[j]);  INC (j);
            END (*WHILE*);
        END PutString;

    (********************************************************************)

    PROCEDURE PutPower (value: power);

        (* Appends a number to the upper half of the buffer.    *)

        BEGIN
            IF value > 9 THEN
                PutPower (value DIV 10);
            END (*IF*);
            PutChar (upper, CHR (ORD("0") + value MOD 10));
        END PutPower;

    (********************************************************************)

    PROCEDURE PutCoeff (value: CoeffType);

        (* Appends a number to the lower half of the buffer.    *)

        VAR localbuff: ARRAY [0..79] OF CHAR;  start: CARDINAL;

        BEGIN
            LongRealToString (value, localbuff, places);
            start := 0;
            WHILE localbuff[start] = Space DO INC(start) END(*WHILE*);
            PutString (lower, localbuff, start);
        END PutCoeff;

    (********************************************************************)

    PROCEDURE PutTerm (coeff: CoeffType;  power: CARDINAL;  leading: BOOLEAN);

        (* Appends a term coeff*X^power to the buffer.  The final       *)
        (* parameter specifies whether to suppress a leading "+" sign.  *)

        BEGIN
            PutChar (lower, Space);
            IF coeff < 0.0 THEN
                PutChar (lower, '-');  coeff := -coeff;
            ELSIF NOT leading THEN
                PutChar (lower, '+');
            END (*IF*);
            PutChar (lower, Space);
            IF (power = 0) OR (ABS(coeff - 1.0) >= small) THEN
                PutCoeff (coeff);
            END (*IF*);
            IF power > 0 THEN
                PutChar (lower, "X");
                IF power > 1 THEN
                    PutPower (power);
                END (*IF*);
            END (*IF*);
            checkpoint := nextloc;
        END PutTerm;

    (********************************************************************)

    VAR j: power;

    BEGIN       (* Body of procedure Write *)
        ALLOCATE (buffer, 2*linesize);
        nextloc := 0;  checkpoint := 0;
        IF P = NIL THEN
            PutTerm (0.0, 0, TRUE);
        ELSE

            (* The leading coefficient requires special treatment,      *)
            (* so that we can avoid putting out a leading '+' sign.     *)

            PutTerm (P^.pcoeffs^[P^.degree], P^.degree, TRUE);
            FOR j := P^.degree-1 TO 0 BY -1 DO
                IF ABS(P^.pcoeffs^[j]) >= small THEN
                    PutTerm (P^.pcoeffs^[j], j, FALSE);
                END (*IF*);
            END (*FOR*);
        END (*IF*);

        (* Write out whatever is still left in the buffer. *)

        IF nextloc > 0 THEN
            FlushBuffer;
        END (*IF*);
        DEALLOCATE (buffer, 2*linesize);

    END Write;

(************************************************************************)

END Poly.

