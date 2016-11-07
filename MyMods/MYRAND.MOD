<*/NOWARN:F*>
IMPLEMENTATION MODULE MYRAND;
(*  change def file to remove the longint random call
  Based on the RAN3 routine from Numerical Recipies.  This is not based on
the linear congruential method, but on a subtractive method, also described
by Knuth.
  To make this real R = I/m, which makes 0 <= R < 1. 
  Note that R will not equal 1 because 0 <= I < m, or 0 <= I <= m-1.  For
  this reason, bottom half of range is < .5, and top half is >= .5 .

  REVISION HISTORY
  ================
   3 Jan 92 -- Converted to M2 V 4.00.
  23 Oct 03 -- Converted to Win using SBM2 V 4
*)

IMPORT SYSTEM;
FROM SysClock IMPORT DateTime, GetClock;

CONST
  DIM        = 55; (* This value is special and should not be modified *)
  DIML       = VAL(LONGINT,DIM);         (* See Knuth for reason why   *)
  MAXCARD    = 0FFFFffffH;
  MAXINT     = 07FFFFFFFH;
(* If I remember correctly, there are compiler constants that represent the maxcard & maxlongint, etc *)
  BIG    = 1000000000;   (* 1.00 E 9 *)
  SEED   = 161803398;    (* 1.61 E 8 *)
  Z      = 0;


VAR
  RANARRAY   : ARRAY [1 .. DIM] OF INTEGER;
  J,K        : INTEGER;
  NEXT,NEXTP : CARDINAL;
  FAC        : LONGREAL;

PROCEDURE Next (): LONGREAL;
(*
  Internal procedure that actually gets the next random number.
*)
VAR R : LONGREAL;

BEGIN
  INC(NEXT);
  IF NEXT  > DIM THEN NEXT  := 1; END(*IF*); (* wrap NEXT  around 56 to 1 *)
  INC(NEXTP);
  IF NEXTP > DIM THEN NEXTP := 1; END(*IF*); (* wrap NEXTP around 56 to 1 *)
(*
  Generate a new random number subtractively.  Be sure that it is in range.
  Store it and output the derived uniform deviate.
*)
  J := RANARRAY[NEXT] - RANARRAY[NEXTP];
  IF J < Z THEN J := J + BIG; END(*IF*);
  RANARRAY[NEXT] := J;
  R := LFLOAT(J)*FAC;
  RETURN R;
END Next;

PROCEDURE RANDINIT (seed : CARDINAL);
VAR
  C,CC  : CARDINAL;
  DUMMY : LONGREAL;

BEGIN
(* Init'z using the input seed and large number SEED *)
  J := SEED - VAL(LONGINT,seed);
  J := J MOD BIG;
  RANARRAY[DIM] := J;
  K := 1;
  FOR C := 1 TO DIM-1 DO  (* Init'z the rest of the table in a slightly *)
    CC := 21*C MOD DIM;   (* random order, with numbers that are not    *)
    RANARRAY[CC] := K;    (* especially random.                         *)
    K := J - K;
    IF K < Z THEN K := K + BIG; END(*IF*);
    J := RANARRAY[CC];
  END(*FOR*);
  FOR CC := 1 TO 4 DO    (* Randomize by warming up the generator *)
    FOR C := 1 TO DIM DO
      RANARRAY[C] := RANARRAY[C] - RANARRAY[(C+30) MOD DIM + 1];
      IF RANARRAY[C] < Z THEN RANARRAY[C] := RANARRAY[C] + BIG; END(*IF*);
    END(*FOR*);
  END(*FOR*);
  NEXT  := 0;   (* Prepare indicies for first generated number *)
  NEXTP := 31;  (* This constant of 31 is special, see Knuth *)
(* 
  Exercise the generator to improve its randomness.  This is the inner
  exercise loop.  The outer exercising loop occurs in RANDOMIZE.
*)
  FOR C := 0 TO 1999 DO
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
  RETURN SYSTEM.CAST(INTEGER, (RANDCARD (ORD (ABS (bound)))));
END RANDINT;

PROCEDURE RANDREAL () : LONGREAL;
BEGIN
  RETURN Next();
END RANDREAL;

(*
PROCEDURE RANDLONGINT (bound : LONGINT): LONGINT;
BEGIN
  IF bound = 0 THEN 
    RETURN LONGTRUNC(LONGFLOAT(MAXLONGINT)*Next());
  ELSE
    RETURN LONGTRUNC(LONGFLOAT(bound)*Next());
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
(*  j := t.minute;   minute is total elapsed minutes in day, not in hour *)
  j := t.hour*60 + t.minute;
  FOR i := 0 TO j DO
    dummy := Next ();
  END;
END RANDOMIZE;


BEGIN
  FAC := 1./LFLOAT(BIG);
  RANDOMIZE;
END MYRAND.