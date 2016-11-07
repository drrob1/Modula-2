(* $ R - T - F - *)
IMPLEMENTATION MODULE MYRAND2;
(*
  Based on the RAN1 routine from Numerical Recipies.  3 linear congruential
  generators are combined here.  The first 2 generate the first and second
  half of the random integer I, and the 3rd shuffles them randomly.
      I(j) = aI(j-1) + c MOD m.  a is the multiplier, c is the increment, 
  m is the modulus.  To make this real R = I/m, which makes 0 <= R < 1. 
  Note that R will not equal 1 because 0 <= I < m, or 0 <= I <= m-1.  For
  this reason, bottom half of range is < .5, and top half is >= .5 .
*)

FROM TimeDate IMPORT Time, GetTime;
FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR,FLOATLONG,TRUNCLONG;
FROM InOut IMPORT WriteString,WriteCard,WriteLn;

CONST
  DIM        = 97;
  DIML       = VAL(LONGINT,DIM);
  MAXCARD    = 0FFFFH;
  MAXLONGINT = 07FFFFFFFH;

  A1     = 7141.;
  C1     = 54773.; 
  M1     = 259200.;

  A2     = 8121.;
  C2     = 28411.;
  M2     = 134456.;

  A3     = 4561.;
  C3     = 51349.;
  M3     = 243000.;

VAR
  RANDREALS      : ARRAY [0 .. DIM-1] OF REAL;
  RAN1,RAN2,RAN3 : REAL;
  J, K           : CARDINAL;
  RATIO1,RATIO2  : REAL;
  M3LONG         : LONGINT;  (* manually remove loop invariants *)
  DIMFLOAT       : REAL;

PROCEDURE Next (): REAL;
(*
  Internal procedure that actually gets the next random number.
*)
VAR R : REAL;

BEGIN
  RAN1 := AMOD(A1*RAN1 + C1,M1);
  RAN2 := AMOD(A2*RAN2 + C2,M2);
  RAN3 := AMOD(A3*RAN3 + C3,M3);
  J    := VAL(CARDINAL,TRUNCLONG(DIMFLOAT*RAN3) DIV M3LONG);
  IF (J > DIM) OR (J < 0) THEN
    WriteLn;
    WriteString(
' from Random Number Generator and index J is out of range.  J = ');
    WriteCard(J,0);
    WriteLn;
  END;
  R := RANDREALS[J];
  RANDREALS[J] := (RAN1 + RAN2*RATIO2)*RATIO1;  (* combine parts here *)
  RETURN R;
END Next;

PROCEDURE RANDINIT (seed : CARDINAL);
VAR
  I     : CARDINAL;
  DUMMY : REAL;

BEGIN
  RAN1 := AMOD(A1*FLOAT(seed) + C1,M1);  (* seed first generator *)
  RAN2 := AMOD(A2*RAN1 + C2,M2);  (* seed second generator *)
  RAN3 := AMOD(A3*RAN2 + C3,M3);  (* seed third generator *)
  FOR I := 0 TO DIM-1 DO   (* fill the table of random numbers *)
    RAN1 := AMOD(A1*RAN1 + C1,M1);
    RAN2 := AMOD(A2*RAN2 + C2,M2);
    RANDREALS[I] := (RAN1 + RAN2*RATIO2)*RATIO1;  (* combine parts here *)
  END;
(* 
  Exercise the generator to improve its randomness.  This is the inner
exercise loop.  The outer exercising loop occurs in RANDOMIZE.
*)
  FOR I := 0 TO 1999 DO
    DUMMY := Next ();
  END;
END RANDINIT;

PROCEDURE RANDCARD (bound : CARDINAL): CARDINAL;
BEGIN
  IF bound = 0 THEN 
    RETURN TRUNC(FLOAT(MAXCARD)*Next());
  ELSE
    RETURN TRUNC(FLOAT(bound)*Next());
  END;
END RANDCARD;

PROCEDURE RANDINT (bound : INTEGER): INTEGER;
BEGIN
  RETURN INTEGER (RANDCARD (CARDINAL (ABS (bound))));
END RANDINT;

PROCEDURE RANDREAL () : REAL;
BEGIN
  RETURN Next();
END RANDREAL;

PROCEDURE RANDLONGINT (bound : LONGINT): LONGINT;
BEGIN
  IF bound = 0L THEN 
    RETURN TRUNCLONG(FLOATLONG(MAXLONGINT)*Next());
  ELSE
    RETURN TRUNCLONG(FLOATLONG(bound)*Next());
  END;
END RANDLONGINT;

PROCEDURE RANDOMIZE;
VAR
  t    : Time;
  i, j : CARDINAL;
  dummy: REAL;
BEGIN
  GetTime(t);
  RANDINIT(t.millisec);
  j := t.minute;  (* minute is total elapsed minutes in day, not in hour *)
  FOR i := 0 TO j DO
    dummy := Next ();
  END;
END RANDOMIZE;


BEGIN
  RATIO1   := 1./M1;
  RATIO2   := 1./M2;
  DIMFLOAT := FLOAT(DIM);
  M3LONG   := TRUNCLONG(M3);
  RANDOMIZE;
END MYRAND2.
