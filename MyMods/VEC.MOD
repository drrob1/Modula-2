IMPLEMENTATION MODULE Vec;

        (********************************************************)
        (*                                                      *)
        (*                 Vector arithmetic                    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        15 August 1996                  *)
        (*  Status:             Seems to be working             *)
        (*                                                      *)
        (*      Portability note: this module contains some     *)
        (*      open array operations which are a language      *)
        (*      extension in XDS Modula-2 but not part of       *)
        (*      ISO standard Modula-2.  So far I haven't worked *)
        (*      out how to do this in the standard language.    *)
        (*                                                      *)
        (********************************************************)

(*  Data types defined in the def module
TYPE
    EltType = LONGREAL;
    VectorPtr = POINTER TO ARRAY OF EltType;
*)


FROM Storage IMPORT (* proc *)  ALLOCATE, DEALLOCATE;

FROM MiscM2 IMPORT (* proc *)  WriteLn, WriteString, WriteLongReal;

(************************************************************************)

(************************************************************************)
(*                  CREATING AND DESTROYING VECTORS                     *)
(************************************************************************)

PROCEDURE NewVector (N: CARDINAL): VectorPtr;

    (* Creates a vector of N elements, for 0 to N-1. *)

    VAR result: VectorPtr;

    BEGIN
        NEW (result, N-1);
        RETURN result;
    END NewVector;

(************************************************************************)

PROCEDURE DisposeVector (VAR (*INOUT*) V: VectorPtr);

    (* Deallocates a vector of N elements. *)
    (* But N is not used AFAIK.  So I'm taking it out of the param list. *)

    BEGIN
    	IF V <> NIL THEN
        DISPOSE (V);
        V := NIL
      END (*if*);
    END DisposeVector;

(************************************************************************)
(*                          COPYING A VECTOR                            *)
(************************************************************************)

(*                                                                PROCEDURE Copy (A: ARRAY OF EltType;  N: CARDINAL; VAR (*OUT*) B: ARRAY OF EltType); *)
PROCEDURE Copy (A: ARRAY OF EltType;  VAR (*OUT*) B: ARRAY OF EltType);

    (* Copies an N-element vector A to B. *)

    VAR i,N: CARDINAL;

    BEGIN
    	N  := HIGH(A);
      FOR i := 0 TO N-1 DO
        B[i] := A[i];
      END (*FOR*);
    END Copy;

(************************************************************************)
(*                          VECTOR ARITHMETIC                           *)
(************************************************************************)

(*                                                           PROCEDURE Add (A, B: ARRAY OF EltType;  elts: CARDINAL; VAR (*OUT*) C: ARRAY OF EltType); *)
PROCEDURE Add (A, B: ARRAY OF EltType; VAR (*OUT*) C: ARRAY OF EltType);

    (* Computes C := A + B.  All vectors have elts elements. *)

    VAR i,elts: CARDINAL;

    BEGIN
    	elts := HIGH(A);
      FOR i := 0 TO elts-1 DO
        C[i] := A[i] + B[i];
      END (*FOR*);
    END Add;

(************************************************************************)

PROCEDURE Sub (A, B: ARRAY OF EltType; VAR (*OUT*) C: ARRAY OF EltType);

    (* Computes C := A - B.  All vectors have elts elements.  *)

    VAR i,elts: CARDINAL;

    BEGIN
    	elts := HIGH(A);
      FOR i := 0 TO elts-1 DO
        C[i] := A[i] - B[i];
      END (*FOR*);
    END Sub;

(************************************************************************)

PROCEDURE Mul (A: ARRAY OF ARRAY OF EltType;  B: ARRAY OF EltType; VAR (*OUT*) C: ARRAY OF EltType);

    (* Computes C := A*B, where A is N1xN2 and B is N2x1. *)

    VAR i,j,N1,N2: CARDINAL;  sum: EltType;

    BEGIN
    	N1 := HIGH(A);
    	N2 := HIGH(B);
      FOR i := 0 TO N1-1 DO
        sum := 0.0;
        FOR j := 0 TO N2-1 DO
          sum := sum + A[i,j]*B[j];
        END (*FOR*);
        C[i] := sum;
      END (*FOR*);
    END Mul;

(************************************************************************)

PROCEDURE ScalarMul (A: EltType;  B: ARRAY OF EltType; VAR (*OUT*) C: ARRAY OF EltType);

    (* Computes C := A*B, where A is scalar and B has elts elements. *)

    VAR i,elts: CARDINAL;

    BEGIN
    	elts := HIGH(B);
      FOR i := 0 TO elts-1 DO
        C[i] := A * B[i];
      END (*FOR*);
    END ScalarMul;

(************************************************************************)
(*                              OUTPUT                                  *)
(************************************************************************)

PROCEDURE Write (V: ARRAY OF EltType;  N: CARDINAL;  places: CARDINAL);

(* Writes the N-element vector V to the screen, where each column occupies a field "places" characters wide. *)

    VAR i,N: CARDINAL;

    BEGIN
    	N := HIGH(V);
      FOR i := 0 TO N-1 DO
        WriteString ("  ");
        WriteLongReal (V[i], places-2);
      END (*FOR*);
      WriteLn;
    END Write;

(************************************************************************)

END Vec.

