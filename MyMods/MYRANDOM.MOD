IMPLEMENTATION MODULE MYRANDOM;
(*
  Based on the RAN1 routine from Numerical Recipies.  3 linear congruential
  generators are combined here.  The first 2 generate the first and second
  half of the random integer I, and the 3rd shuffles them randomly.
      I(j) = aI(j-1) + c MOD m.  a is the multiplier, c is the increment,
  m is the modulus.  To make this real R = I/m, which makes 0 <= R < 1.
  Note that R will not equal 1 because 0 <= I < m, or 0 <= I <= m-1.  For
  this reason, bottom half of range is < .5, and top half is >= .5 .
  
  REVISION HISTORY
  -------- -------
    6 Jan 17 -- Compiled by ADW.
  
*)
IMPORT SYSTEM;
FROM SysClock IMPORT DateTime, GetClock;
FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;

CONST
  DIM        = 97;
  DIML       = DIM;                         (*        VAL(LONGINT,DIM); *)
  MAXCARD    = 0FFFFFFFFH;
  MAXINT     = 07FFFFFFFH;

  A1     = 7141;
  C1     = 54773;
  M1     = 259200;

  A2     = 8121;
  C2     = 28411;
  M2     = 134456;

  A3     = 4561;
  C3     = 51349;
  M3     = 243000;

VAR
  RANDREALS      : ARRAY [0 .. DIM-1] OF LONGREAL;
  RAN1,RAN2,RAN3 : INTEGER;
  J, K           : CARDINAL;
  RATIO1,RATIO2  : LONGREAL;

PROCEDURE Next (): LONGREAL;
(*
  Internal procedure that actually gets the next random number.
*)
VAR R : LONGREAL;

BEGIN
  RAN1 := (A1*RAN1 + C1) MOD M1;
  RAN2 := (A2*RAN2 + C2) MOD M2;
  RAN3 := (A3*RAN3 + C3) MOD M3;
  J    := ORD(DIM*RAN3 DIV M3);

  IF (J > DIM) OR (J < 0) THEN
    BasicDialogs.MessageBox(" from Random Number Generator and index J is out of range.  J = ",MsgInfo);
(*
    WriteCard(J,0);
    WriteLn;
*)
  END;
  R := RANDREALS[J];
  RANDREALS[J] := (LFLOAT(RAN1) + LFLOAT(RAN2)*RATIO2)*RATIO1;
  RETURN R;
END Next;

PROCEDURE RANDINIT (seed : CARDINAL);
VAR
  I     : CARDINAL;
  DUMMY : LONGREAL;

BEGIN
  RAN1 := (A1*seed + C1) MOD M1;  (* seed first generator *)
  RAN2 := (A2*RAN1 + C2) MOD M2;  (* seed second generator *)
  RAN3 := (A3*RAN2 + C3) MOD M3;  (* seed third generator *)
  FOR I := 0 TO DIM-1 DO   (* fill the table of random numbers *)
    RAN1 := (A1*RAN1 + C1) MOD M1;
    RAN2 := (A2*RAN2 + C2) MOD M2;
    RANDREALS[I] := (LFLOAT(RAN1) + LFLOAT(RAN2)*RATIO2)*RATIO1;
  END;
(*
  Exercise the generator to improve its randomness.  This is the inner exercise loop.  The outer exercising loop occurs in RANDOMIZE.
*)
  FOR I := 0 TO 1999 DO
    DUMMY := Next ();
  END;
END RANDINIT;

PROCEDURE RANDCARD (bound : CARDINAL): CARDINAL;
BEGIN
  IF bound = 0 THEN
    RETURN TRUNC(LFLOAT(MAXCARD)*Next());
  ELSE
    RETURN TRUNC(LFLOAT(bound)*Next());
  END;
END RANDCARD;

PROCEDURE RANDINT (bound : INTEGER): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, RANDCARD (ORD (ABS (bound))));
END RANDINT;

PROCEDURE RANDREAL () : LONGREAL;
BEGIN
  RETURN Next();
END RANDREAL;

(*
PROCEDURE RANDLONGINT (bound : LONGINT): LONGINT;
BEGIN
  IF bound = 0L THEN
    RETURN TRUNCLONG(FLOATLONG(MAXLONGINT)*Next());
  ELSE
    RETURN TRUNCLONG(FLOATLONG(bound)*Next());
  END;
END RANDLONGINT;
*)
PROCEDURE RANDOMIZE;
VAR
  t    : DateTime;
  i, j : CARDINAL;
  dummy: LONGREAL;
BEGIN
  GetClock(t);
  RANDINIT(t.fractions);
(*  j := t.minute;  minute is total elapsed minutes in day, not in hour *)
  j := t.hour*60 + t.minute;
  FOR i := 0 TO j DO
    dummy := Next ();
  END;
END RANDOMIZE;


BEGIN
  RATIO1 := 1./LFLOAT(M1);
  RATIO2 := 1./LFLOAT(M2);
  RANDOMIZE;
END MYRANDOM.
