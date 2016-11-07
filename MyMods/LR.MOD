IMPLEMENTATION MODULE LR;
(* Copyright (C) 1987  Robert Solomon MD.  All rights reserved.

  REVISION HISTORY
  ----------------
  22 Oct 88 -- 1) Added the GETCORR Procedure, but on testing the R**2 value
                  did not change with each iteration, casting doubt on the
                  validity of my weighting modification.
               2) Fixed omission in STDLR in that SUMWTXY was not computed.
  23 Feb 06 -- Converted to SBM2 for Win.  And since it is not allowed for this module to
                 write errors, slope and intercept will be set zero in case of an error.
*)
  IMPORT WholeStr, LongStr, LongMath;
  FROM LongMath IMPORT sqrt, exp, ln;

  CONST MAXELE = 100;
        ITERMAX = 20;
        TOLERANCE = 1.0E-4;  (* TOLERANCE FACTOR *)

PROCEDURE SQR(R : LONGREAL) : LONGREAL;
(*
*********************************** SQR ******************************
NO SURPRIZES HERE.
*)

BEGIN
  RETURN(R*R);
END SQR;

PROCEDURE DOLR(N:CARDINAL; X,Y:ARRAY OF LONGREAL; VAR Slope,Intercept:LONGREAL);
(*
********************************** DOLR *********************************
Do Linear Regression Routine.
  This routine does the actual linear regression using a weighted algorithm
that is described in Zanter, Jean-Paul, "Comparison of Linear Regression
Algortims," JPAM Jan/Feb '86, 5:1, p 14-22.  This algorithm is used instead
of the std one because the std one assumes that the errors on the independent
variable are negligible and the errors on the dependent variable are
constant.  This is definitely not the case in a clinical situation where,
for example, both the time of a blood sample and the counts per minute in
that blood sample, are subject to variances.
*)

VAR
  SUMWT (* SUM OF WEIGHTS *), SUMWTX (* SUM OF WEIGHTS * X *), SUMWTY,
  SUMWTX2 (* SUM OF WEIGHTS * X^2 *), SUMWTXY, SUMWTY2, SUMWTAX
  (* SUM OF WEIGHTS * AX *), SUMWTAXX (* SUM OF WEIGHTS * AX * X *),
  SUMWTAXY (* SUM OF WEIGHTS * AX * Y *),
  R2 (* R^2, OR THE COEFFICIENT OF DETERMINATION OF THE STD SOLN *),
  EXPECTEDX,EXPECTEDY,ERRORX,ERRORY2,DENOM,StDevS,StDevI   : LONGREAL;
(*
                                      1
  WEIGHT OF A POINT = ---------------------------------------
                      (error_on_y)**2 + (slope*error_on_x)**2

  AX -- a quantity defined by the author, used in the iterative solution of
        the system of equations used in computing the weights, and hence the
        errors on the data, and on the slopes and intercepts.  Like weights,
        it applies to each point.

     =  X - Weight * (Intercept + Slope*X - Y) * Slope * Error_on_X**2

It is clear that the weights and the AX quantity depend on the errors
on the data points.  My modification is a variation on the chi square
analysis to determine the errors on the data points.  This, too is
iterated.

                     (Observed value - Expected value)**2
 CHI SQUARE = sum of ------------------------------------
                             Expected value

*)
  WEIGHT,AX : ARRAY[0..MAXELE] OF LONGREAL;
  PREVSLOPE,PREVINTRCPT : LONGREAL;
  C,K,ITERCTR : CARDINAL;

(*
  The following procedures are local to this outer procedure, and all of the
  I/O for these are passed indirectly (globally).
*)

  PROCEDURE SIMPLESUMUP;
  (*
  **************************** SIMPLESUMUP ********************************
    Does the simple (unweighted) sums required of the std formula.  This is
  used as a first guess for the iterative solution performed by the
  other routines.
  *)
  VAR c : CARDINAL;
  BEGIN
    SUMWT   := FLOAT(N);
    SUMWTX  := 0.;
    SUMWTY  := 0.;
    SUMWTX2 := 0.;
    SUMWTXY := 0.;
    SUMWTY2 := 0.;

    FOR c := 0 TO N-1 DO
      SUMWTX  := SUMWTX  + X[c];
      SUMWTY  := SUMWTY  + Y[c];
      SUMWTXY := SUMWTXY + X[c]*Y[c];
      SUMWTX2 := SUMWTX2 + SQR(X[c]);
      SUMWTY2 := SUMWTY2 + SQR(Y[c]);
    END(*FOR*);
  END SIMPLESUMUP;

  PROCEDURE STDLR;
  (*
  ****************************** STDLR **********************************
  This routine does the standard, unweighted, computation of the slope and
  intercept, using the formulas that are built into many pocket calculators,
  including mine.  This computation serves as an initial guess for the
  iterative solution used by this program as described by Dr. Zanter.
  *)

  VAR SLOPENUMERATOR,SLOPEDENOMINATOR : LONGREAL;

  BEGIN
    SIMPLESUMUP;
    SLOPENUMERATOR := SUMWT*SUMWTXY - SUMWTX*SUMWTY;
    SLOPEDENOMINATOR := SUMWT*SUMWTX2 - SQR(SUMWTX);
    Slope := SLOPENUMERATOR/SLOPEDENOMINATOR;
    Intercept := (SUMWTY - Slope*SUMWTX)/SUMWT;
    R2 := SQR(SLOPENUMERATOR)/SLOPEDENOMINATOR/(SUMWT*SUMWTY2 - SQR(SUMWTY));
  END STDLR;

  PROCEDURE GETWTS;
  (*
  ************************ GETWTS ************************************
  GET WEIGHTS.
  This routine computes the weights and the AX quantities as given by the
  above formulas.

  *)

  VAR MINERROR : LONGREAL; (* MINIMUM ERROR ALLOWED. *)
          c        : CARDINAL;

  BEGIN
    FOR c := 0 TO N-1 DO
      EXPECTEDX := (Y[c] - Intercept) / Slope;
      EXPECTEDY := Slope * X[c] + Intercept;
      ERRORX := ABS(X[c] - EXPECTEDX)/sqrt(ABS(EXPECTEDX));
      MINERROR := TOLERANCE*ABS(EXPECTEDX);
      IF ERRORX < MINERROR THEN ERRORX := MINERROR; END(*IF*);
      ERRORY2 := SQR(Y[c] - EXPECTEDY)/ABS(EXPECTEDY);
      MINERROR := TOLERANCE*ABS(EXPECTEDY);
      IF ERRORY2 < MINERROR THEN ERRORY2 := MINERROR; END(*IF*);
      WEIGHT[c] := 1./(ERRORY2 + SQR(Slope*ERRORX));
      AX[c] := X[c] - WEIGHT[c]*(Slope*X[c] + Intercept - Y[c])*
                                                          Slope*SQR(ERRORX);
    END(*FOR*);
  END GETWTS;

  PROCEDURE WTSUMUP;
  (*
  ******************************** WTSUMUP *********************************
  Weighted Sum Up.
  This procedure sums the variables using the weights and AX quantaties as
  described (and computed) above.

  *)
  VAR W : LONGREAL;
          c : CARDINAL;

  BEGIN
    SUMWT    := 0.;
    SUMWTX   := 0.;
    SUMWTY   := 0.;
    SUMWTX2  := 0.;
    SUMWTXY  := 0.;
    SUMWTAX  := 0.;
    SUMWTAXX := 0.;
    SUMWTAXY := 0.;

    GETWTS;
    FOR c := 0 TO N-1 DO
      W := WEIGHT[c];
      SUMWT := SUMWT + W;
      SUMWTX := SUMWTX + W*X[c];
      SUMWTX2 := SUMWTX2 + W*SQR(X[c]);
      SUMWTY := SUMWTY + W*Y[c];
      SUMWTAX := SUMWTAX + W*AX[c];
      SUMWTAXX := SUMWTAXX + W*AX[c]*X[c];
      SUMWTAXY := SUMWTAXY + W*AX[c]*Y[c];
    END(*FOR*);
  END WTSUMUP;

  PROCEDURE WTLR;
  (*
  ******************************** WTLR ********************************
  Weighted Linear Regression.
  This procedure computes one iteration of the weighted*AX computation of
  the Slope and Intercept.

  *)
  BEGIN
    WTSUMUP;
    Slope := (SUMWTAX*SUMWTY - SUMWTAXY*SUMWT)/
                                          (SUMWTX*SUMWTAX - SUMWTAXX*SUMWT);
    Intercept := (SUMWTY - Slope*SUMWTX)/SUMWT;
  END WTLR;

  PROCEDURE GETCORR(VAR R2 : LONGREAL);
  (*
  ********************************* GETCORR ****************************
  Get Correlation Coefficient
  This uses a variant of the std formula, taking the weights into acnt.
  *)

  VAR W : LONGREAL;
          c : CARDINAL;

  BEGIN
    SUMWT    := 0.;
    SUMWTX   := 0.;
    SUMWTY   := 0.;
    SUMWTXY  := 0.;
    SUMWTX2  := 0.;
    SUMWTY2  := 0.;

    GETWTS;
    FOR c := 0 TO N-1 DO
      W := WEIGHT[c];
      SUMWT := SUMWT     + W;
      SUMWTX := SUMWTX   + W*X[c];
      SUMWTY := SUMWTY   + W*Y[c];
      SUMWTXY := SUMWTXY + W*X[c]*Y[c];
      SUMWTX2 := SUMWTX2 + W*SQR(X[c]);
      SUMWTY2 := SUMWTY2 + W*SQR(Y[c]);
    END(*FOR*);
    R2 := SQR(SUMWT*SUMWTXY - SUMWTX*SUMWTY)/
                (SUMWT*SUMWTX2 - SQR(SUMWTX))/(SUMWT*SUMWTY2 - SQR(SUMWTY));
  END GETCORR;

BEGIN (* BODY OF DOLR PROCEDURE *)
  STDLR;
  PREVSLOPE := Slope;
  PREVINTRCPT := Intercept;
(*
  GETCORR(R2);
  WriteString(' Slope = ');
  WriteReal(SLOPE,0);
  WriteString(';  Intercept = ');
  WriteReal(INTRCPT,0);
  WriteString(';  R**2 = ');
  WriteReal(R2,0);
  WriteLn;
*)

  LOOP
    FOR ITERCTR := 1 TO ITERMAX DO
      WTLR;
(*
      WriteString(' Slope = ');
      WriteReal(SLOPE,0);
      WriteString(';  Intercept = ');
      WriteReal(INTRCPT,0);
      WriteString(';  R**2 = ');
      WriteReal(R2,0);
      WriteLn;
      WriteString(' Weights =');
      FOR C := 0 TO N-1 DO
        WriteReal(WEIGHT[c],0);
        IF (C+1) MOD 6 = 0 THEN WriteLn; END(*IF*);
      END(*FOR*);
      WriteLn;
*)
      IF (ABS(Slope - PREVSLOPE) < TOLERANCE*ABS(Slope)) AND
                   (ABS(Intercept - PREVINTRCPT) < TOLERANCE*ABS(Intercept)) THEN
        EXIT;
      ELSE
        PREVSLOPE := Slope;
        PREVINTRCPT := Intercept;
      END(*IF*);
    END(*FOR*);
    EXIT;
  END(*LOOP*);
(*
  DENOM := SUMWT*SUMWTX2 - SQR(SUMWTX);
  StDevS := sqrt(ABS(SUMWT/DENOM));
  StDevI := sqrt(ABS(SUMWTX2/DENOM));
  WriteString(' St Dev on Slope is ');
  WriteReal(StDevS,0);
  WriteString(';  St Dev on Intercept is ');
  WriteReal(StDevI,0);
  WriteLn;
*)
END DOLR;

PROCEDURE SIMPLELR(N:CARDINAL; X,Y:ARRAY OF LONGREAL; VAR Slope,Intercept:LONGREAL);
(*
******************************* SIMPLELR *******************************
SIMPLE Linear Regression.
This routine is the entry point for client modules to use the linear
regression algorithm.  This separation allows for more complex
linearizations to be performed before the regression is computed.
*)
BEGIN
  IF (N > HIGH(X)+1) OR (N > HIGH(Y)+1) OR (N > MAXELE) OR (N < 3) THEN
    Slope := 0.;
    Intercept := 0.;
    RETURN;
  END;
(*
  IF (N > HIGH(X)+1) OR (N > HIGH(Y)+1) THEN
    WriteString(' *ERROR*  Value of N is greater than the size of the ');
    WriteString('X or Y arrays.');
    WriteLn;
    WriteString(' N = ');
    WriteCard(N,0);
    WriteString(';  Size of the X array is ');
    WriteCard(HIGH(X),0);
    WriteString(';  Size of the Y array is ');
    WriteCard(HIGH(Y),0);
    WriteString('.');
    WriteLn;
    WriteString(' Program results may be unreliable.');
    WriteLn;
    N := HIGH(X) + 1;
    IF N > HIGH(Y) THEN N := HIGH(Y) + 1; END/*IF*/;
  ELSIF N > MAXELE THEN
    WriteString(' Too Many Elements.  Maximum # is ');
    WriteCard(MAXELE,0);
    WriteString('.  Requested # of elements is ');
    WriteCard(N,0);
    WriteString(' Trailing elements truncated.');
    N := MAXELE;
  ELSIF N < 3 THEN
    WriteString(' Need at least 3 points.  Linear Regression not done.');
    WriteLn;
    SLOPE := 0.;
    INTRCPT := 0.;
    RETURN;
  END/*IF*/;
*)


  DOLR(N,X,Y,Slope,Intercept);
END SIMPLELR;

PROCEDURE SEMILOGLR(N:CARDINAL; X,Y:ARRAY OF LONGREAL; VAR Slope,Intercept:LONGREAL);
(*
******************************** SEMILOGLR *******************************
SemiLogarithmic Linear Regression.
This entry point first performs a log on the ordinate (Y values) before
calling the linear regression routine.

*)

VAR LOGY : ARRAY[0..MAXELE] OF LONGREAL;
    C    : CARDINAL;

BEGIN
  IF (N > HIGH(X)+1) OR (N > HIGH(Y)+1) OR (N > MAXELE) OR (N < 3) THEN
    Slope := 0.;
    Intercept := 0.;
    RETURN;
  END;
(*
  IF (N > HIGH(X)+1) OR (N > HIGH(Y)+1) THEN
    WriteString(' *ERROR*  Value of N is greater than the size of the ');
    WriteString('X or Y arrays.');
    WriteLn;
    WriteString(' N = ');
    WriteCard(N,0);
    WriteString(';  Size of the X array is ');
    WriteCard(HIGH(X),0);
    WriteString(';  Size of the Y array is ');
    WriteCard(HIGH(Y),0);
    WriteString('.');
    WriteLn;
    WriteString(' Program results may be unreliable.');
    WriteLn;
    N := HIGH(X) + 1;
    IF N > HIGH(Y) THEN N := HIGH(Y) + 1; END/*IF*/;
  ELSIF N > MAXELE THEN
    WriteString(' Too Many Elements.  Maximum # is ');
    WriteCard(MAXELE,0);
    WriteString('.  Requested # of elements is ');
    WriteCard(N,0);
    WriteString(' Trailing elements truncated.');
    N := MAXELE;
  ELSIF N < 3 THEN
    WriteString(' Need at least 3 points.  Linear Regression not done.');
    WriteLn;
    SLOPE := 0.;
    INTRCPT := 0.;
    RETURN;
  END/*IF*/;
*)
  FOR C := 0 TO N-1 DO
(*
    IF Y[c] < 0. THEN
      WriteString(' Cannot take the log of a negative number.  The Y');
      WriteString(' value of');
      WriteLn;
      WriteReal(Y[c],0);
      WriteString(' was made positive.  Results may be invalid.');
      WriteLn;
    END/*IF*/;
*)
    LOGY[C] := ln(ABS(Y[C]));
  END(*FOR*);
  DOLR(N,X,LOGY,Slope,Intercept);
  Intercept := exp(Intercept);
END SEMILOGLR;

END LR.
