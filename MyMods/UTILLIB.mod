IMPLEMENTATION MODULE UTILLIB;

(*
Copyright (C) 1987  Robert Solomon MD.  All rights reserved.
  THIS IS THE UTILITY FUNCTIONS MODULE, THAT CONTAINS ALL OF THE GENERALLY
  USEFUL SUBROUTINES.

  REVISION HISTORY
  ----------------
   9 Jun 87 -- Added STRCMPFNT.
  15 Jun 87 -- Added CHINBUFFNT, and MAXCARDFNT.
  30 Oct 87 -- Added TRIMFNT and cleaned up other comments and code.
  3  Mar 88 -- Module renamed to UTILLIB.  GETCNT deleted, and the TRIM
                Procedure was added.  Also, the CHINBUFFNT name was
                changed to the LCHINBUFFNT to mean the Last CH IN BUF FNT.
  26 Mar 88 -- Changed NAMSIZ const in GETFNM to 80 so that long paths are
                properly handled.
  31 Mar 88 -- Converted to M2 V3.03.
   1 Dec 89 -- Incorporated consistent code as written for the VAX.  Now
                this module and UL2 are entirely compatible.
  11 Nov 90 -- Utilized GETTKNSTR.
  22 Dec 91 -- Converted to M2 V 4.00 by duplicating UL2 procs using std
                code.
  20 Oct 02 -- Converted to M2 Win but using DOS switches.  Will keep it simple.
  28 Oct 02 -- Added TRIM to ASSIGN2BUF function.
   5 Nov 02 -- Removed looping from GETFNM.  Reorganized completely.
  17 May 03 -- First Windows version.
  19 May 03 -- Changed behavior of ASSIGN2BUF so it doesn't trim spaces.
  24 Aug 04 -- Added SubStrCMPFNT.
   2 Oct 07 -- Added BUF.LENGTH := BUF.COUNT line and now use std proc LENGTH in TRIM.
  23 Oct 13 -- Added stricmpfnt as a case insensitive comparison function.
  24 Mar 17 -- Started change to a string linked list, to backport what I've done w/ stringslice in Go.
  25 Mar 17 -- Finished the string linked list code.
  26 Mar 17 -- Finally removed GETFNM code that was moved to MyFIO long ago.
*)

  FROM SYSTEM IMPORT ADDRESS;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;

  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETTKN,GETTKNSTR;
  FROM Environment IMPORT GetCommandLine;

(*
    THE FOLLOWING ARE DECLARED IN THE DEFINITION MODULE.
CONST
    BUFSIZ    = 255;
    CR        = 15C; // CHR(13)
    LF        = 12C; // CHR(10)
    MAXCARD   = 0FFFFH;
    MAXINT    = 7FFFH;
    MININT    = 8000H;
    ESC       = 33C; // CHR(27)
    NULL      = 0C;
    BLANK     = ' ';
    CTRLCOD   = 31;


TYPE
  LONGBITSET = SET OF [0..31];

   THE TYPE HAS ONE MORE THAN BUFSIZ SO ARRAY SUBSCRIPT ERRORS ARE AVOIDED.
    STRTYP = ARRAY [1..BUFSIZ+1] OF CHAR;  // NOW COMPATIBLE WITH BUFTYP.CHARS
    BUFTYP = RECORD
               CHARS : STRTYP;   // ARRAY [1..BUFSIZ+1] OF CHAR;
               LENGTH,           // NUMBER OF CHARS IN BUF NOT INCL'G SPECIAL CHARS.
               COUNT : CARDINAL; // TOTAL NUMBER OF CHARS IN BUF
             END /*RECORD*/;
  STR10TYP = ARRAY [0..10] OF CHAR;
  STR20TYP = ARRAY [0..20] OF CHAR;

  StringItemPointerType = POINTER TO StringItemType;
  StringItemType    = RECORD
                            Prev : StringItemPointerType;
                            S    : BUFTYP;
                            Next : StringItemPointerType;
                      END;  // StringItemType record


  StringDoubleLinkedListType = RECORD
                  StartOfList, EndOfList, CurrentPlaceInList, PrevPlaceInList : StringPointerType;
                  len,NextPosition : CARDINAL;
  END;  // StringDoubleLinkedListtype record

  StringDoubleLinkedListPointerType = POINTER TO StringDoubleLinkedListType;

*)

TYPE
  CHRBUF   = ARRAY [0..32737] OF CHAR;
  CHRPTR   = POINTER TO CHRBUF;  (* Essentially a pointer to CHAR *)

VAR ROOTNAM,TOKEN,inputline : BUFTYP;
    TKNSTATE                : FSATYP;
    MOREFNM                 : BOOLEAN;  (* USED FOR GETFNM *)
    UPLOW                   : CARDINAL; (* Used by UPRCAS *)


PROCEDURE STRLENFNT(CHARRAY : ARRAY OF CHAR) : CARDINAL;
(*
******************************* STRLENFNT ***********************************
STRING LENGTH FUNCTION.
This function gets the string length by looking for the null in the
string.  It returns the actual number of chars, not the subscript of
the highest non-null element.  Remember that the HIGH standard
procedure returns the number of elements - 1 because it returns the
upper bound of a zero origin array.
Therefore, #OFELE := HIGH(CHARRAY) + 1; this condition is met
by returning a post incremented pointer.

*)

VAR C,HIGHELE : CARDINAL;

BEGIN
  C := 0;
  HIGHELE := HIGH(CHARRAY);
  WHILE (C <= HIGHELE) AND (CHARRAY[C] <> NULL) DO INC(C); END(*WHILE*);
  RETURN(C);
END STRLENFNT;

PROCEDURE STRCMPFNT(STR1,STR2 : ARRAY OF CHAR) : INTEGER;
(*
*********************************** STRCMPFNT *******************************
STRing CoMPare FuNcTion.
This function does a comparison of strings, returning:
  STR1 < STR2 means return a number < 0
  STR1 = STR2 means return a number = 0
  STR1 > STR2 means return a number > 0

If STR1 is a substring of STR2, then it is considered < it.  For two strings
to be =, their count of characters (lengths) also must be equal.

*)

VAR I,CNT1,CNT2,RESULT : INTEGER;

BEGIN
(*
  Because the strlenfnt returns the actual count of characters, and here we
  are using it to search a zero origin character array, the STRLENFNT will
  return a value that is 1 too high for our purposes.  The algorithm must
  compensate for this in the conditional tests of the main while loop below.
*)
  CNT1 := INT(STRLENFNT(STR1));
  CNT2 := INT(STRLENFNT(STR2));
  RESULT := 0;
  I := 0;
  WHILE (I < CNT1) AND (I < CNT2) AND (RESULT = 0) DO
    RESULT := INT(ORD(STR1[I])) - INT(ORD(STR2[I]));
    INC(I);
  END(*WHILE*);
(*
  Strings are only equal if their lengths are also equal.  Otherwise one
  is a substring of the other.  In this case, the shorter string is less
  than the longer one.
*)
  IF RESULT = 0 THEN RESULT := CNT1 - CNT2; END(*IF*);
  RETURN(RESULT);
END STRCMPFNT;

PROCEDURE stricmpfnt(STR1,STR2 : ARRAY OF CHAR) : INTEGER;
(*
*********************************** STRiCMPFNT *******************************
STRing insensitive CoMPare FuNcTion.  As in, case insensitive.
This function does a comparison of strings, returning:
  STR1 < STR2 means return a number < 0
  STR1 = STR2 means return a number = 0
  STR1 > STR2 means return a number > 0

If STR1 is a substring of STR2, then it is considered < it.  For two strings
to be =, their count of characters (lengths) also must be equal.

*)

VAR I,CNT1,CNT2,RESULT : INTEGER;

BEGIN
(*
  Because the strlenfnt returns the actual count of characters, and here we
  are using it to search a zero origin character array, the STRLENFNT will
  return a value that is 1 too high for our purposes.  The algorithm must
  compensate for this in the conditional tests of the main while loop below.
*)
  CNT1 := INT(STRLENFNT(STR1));
  CNT2 := INT(STRLENFNT(STR2));
  RESULT := 0;
  I := 0;
  WHILE (I < CNT1) AND (I < CNT2) AND (RESULT = 0) DO
    RESULT := INT(ORD(CAP(STR1[I]))) - INT(ORD(CAP(STR2[I])));
    INC(I);
  END(*WHILE*);
(*
  Strings are only equal if their lengths are also equal.  Otherwise one
  is a substring of the other.  In this case, the shorter string is less
  than the longer one.
*)
  IF RESULT = 0 THEN RESULT := CNT1 - CNT2; END(*IF*);
  RETURN(RESULT);
END stricmpfnt;

PROCEDURE SubStrCMPFNT(STR1,STR2 : ARRAY OF CHAR) : BOOLEAN;
(*
*********************************** SubStrCMPFNT *******************************
SubString  CoMPare FuNcTion.
This function does a comparison of strings, returning true if the first chars of
one string equals the other, ignoring string lenghts.  This is the difference
between this function and STRCMPFNT.

*)

VAR I,CNT1,CNT2,RESULT : INTEGER;

BEGIN
(*
  Because the strlenfnt returns the actual count of characters, and here we
  are using it to search a zero origin character array, the STRLENFNT will
  return a value that is 1 too high for our purposes.  The algorithm must
  compensate for this in the conditional tests of the main while loop below.
*)
  CNT1 := INT(STRLENFNT(STR1));
  CNT2 := INT(STRLENFNT(STR2));
  RESULT := 0;
  I := 0;
  WHILE (I < CNT1) AND (I < CNT2) AND (RESULT = 0) DO
    RESULT := INT(ORD(STR1[I])) - INT(ORD(STR2[I]));
    INC(I);
  END(*WHILE*);
  RETURN (RESULT = 0);
END SubStrCMPFNT;

PROCEDURE LCHINBUFFNT(BUF:BUFTYP; CH:CHAR) : CARDINAL;
(*
************************************ LCHINBUFFNT *****************************
LAST CHARACTER IN BUFFER FUNCTION.
This function searches the buffer input as its BUFTYP param for the character
input as its char param.  It searches from the end to the beginning, so will
find the last such character in the buffer.  It will return 0 if the
character was not found.  The number returned will be relative to a
one-origin string, as is the CHARS field of BUFTYP.
*)

VAR C : CARDINAL;

BEGIN
  C := BUF.COUNT;
  WHILE (C > 0) AND (BUF.CHARS[C] <> CH) DO DEC(C); END(*WHILE*);
  RETURN(C);
END LCHINBUFFNT;

PROCEDURE MAXCARDFNT(C1,C2 : CARDINAL) : CARDINAL;
(*
****************************** MAXCARDFNT **********************************
MAXIMUM CARDINAL FUNCTION.
This function will return the maximum value of the parameters passed to it.
That is, the higher of the two values will be returned.  For greater than
two params, nest them like in F(F(F(C1,C2),C3),C4).

*)
BEGIN
  IF C1 >= C2 THEN
    RETURN(C1);
  ELSE
    RETURN(C2);
  END(*IF*);
END MAXCARDFNT;

PROCEDURE MRGBUFS(VAR BUF1 : BUFTYP; BUF2 : BUFTYP);
(*
****************************** MRGBUFS ***********************************
MERGE BUFFERS.
This routine will concatenate the two buffers input to it by appending the
second to the first, sort of like BUF1 = BUF1 + BUF2.
*)

VAR C : CARDINAL;

BEGIN
  FOR C := 1 TO BUF2.COUNT + 1 (* INCLUDING NULL TERMINATOR *) DO
    BUF1.CHARS[C + BUF1.COUNT] := BUF2.CHARS[C];
  END(*FOR*);
  INC(BUF1.COUNT,BUF2.COUNT);
  INC(BUF1.LENGTH,BUF2.LENGTH);
END MRGBUFS;

PROCEDURE TRIMFNT(BUF : BUFTYP) : CARDINAL;
(*
****************************** TRIMFNT **************************
TRIM FUNCTION.

  This routine will trim the line passed to it by first looking for
  the null char and then scanning backward looking for the
  first non-blank character.  That is, trailing blanks are deleted,
  or trimmed.  A length of zero means a blank line
  was input.

  BUF -- Buffer containing line to search.

  INPUT FROM GBLVAR: BLANK
*)

VAR
  COL  : CARDINAL;

BEGIN
  COL := STRLENFNT(BUF.CHARS);
  WHILE (COL > 0 ) AND (BUF.CHARS[COL] = BLANK) DO DEC(COL); END(*WHILE*);
  RETURN(COL);
END TRIMFNT;

PROCEDURE TRIM(VAR BUF : BUFTYP);
(*
****************************** TRIM **************************
TRIM Procedure.

  This routine will trim the line passed to it by first looking for
  the null char and then scanning backward looking for the
  first non-blank character.  That is, trailing blanks are deleted,
  or trimmed.  A length of zero means a blank line
  was input.  This is the preferred call to achieve this goal.  The
  GETCNT procedure is still implemented for upward compatibility
  purposes.  In this procedure, the BUF.COUNT field is set, whereas in
  the TRIMFNT function, this field is not set but merely determined.
  Also, the NULL character is put in the proper position in the CHARS
  field to delete the trailing blanks altogether.

  BUF -- BUFFER CONTAINING LINE TO SEARCH.

  INPUT FROM GBLVAR: BLANK
*)

VAR
  COL  : CARDINAL;

BEGIN
(*  COL := STRLENFNT(BUF.CHARS); *)
  COL := LENGTH(BUF.CHARS);
  WHILE (COL > 0 ) AND (BUF.CHARS[COL] = BLANK) DO DEC(COL); END(*WHILE*);
  BUF.COUNT := COL;
  BUF.CHARS[COL+1] := NULL;  (* Null terminate string w/o trailing blanks *)
  BUF.LENGTH := BUF.COUNT;
  RETURN;
END TRIM;

PROCEDURE ISCTRL(CH : CHAR) : BOOLEAN;
(*
************************ ISCTRL *****************************
THIS FUNCTION WILL BE TRUE IF CHAR IS AN ASCII CONTROL CODE, IE,
IT HAS A VALUE <= 31, ELSE THIS FUNCTION WILL BE FALSE.
*)

BEGIN
    IF ORD(CH) <= CTRLCOD THEN
        RETURN(TRUE);
    ELSE
        RETURN(FALSE);
    END(*ISCTRL*);
END ISCTRL;

PROCEDURE ISDGT(CH : CHAR) : BOOLEAN;
(*
************************ ISDGT **********************
ISDIGIT FUNCTION.  FUNCTION WILL BE TRUE IF CHAR IS A DIGIT, FALSE OTHERWISE.
*)

CONST ZERO = '0';
      NINE = '9';

BEGIN
    IF (CH >= ZERO) AND (CH <= NINE) THEN
        RETURN(TRUE);
    ELSE
        RETURN(FALSE);
    END(*IF*);
END ISDGT;

PROCEDURE UPRCAS(CH:CHAR):CHAR;
(*
********************************** UPRCAS *************************
TO CONVERT A LOWER CASE CHAR TO AN UPPER CASE CHAR.
*)

BEGIN
    IF (CH >= 'a') AND (CH <= 'z') THEN
        RETURN(CHR(ORD(CH) - UPLOW));
    ELSE
        RETURN(CH);
    END(*IF*);
END UPRCAS;

PROCEDURE RMVCHR(VAR BUF : BUFTYP; STRTCL,RMVCNT : CARDINAL);
(*
*************************** RMVCHR *********************************
THE PURPOSE OF THIS ROUTINE IS TO REMOVE ANY NUMBER OF CHARS FROM THE
INPUT RECORD, BUF.

STRTCL--STARTING COLUMN OF CHARS TO BE REMOVED.
RMVCNT--COUNT OF CHAR'S TO BE REMOVED.
BUF   --ARRAY OF CHAR'S TO BE PROCESSED.
*)

VAR I,MOVPTR    : CARDINAL;
    FINI        : BOOLEAN;

BEGIN
    IF ((STRTCL+RMVCNT) > (BUF.COUNT+1)) OR (BUF.COUNT < 1) THEN
         WriteString(" IN RMVCHR; PARAMS DON'T MAKE SENSE: RMVCNT=");
         WriteCard(RMVCNT,0);
         WriteString(', STRTCL=');
         WriteCard(STRTCL,0);
         WriteString(', BUF.COUNT=');
         WriteCard(BUF.COUNT,0);
         WriteLn;
         WriteString(' BUF: ');
         WriteString(BUF.CHARS);
         RETURN;
    END(*IF*);  (*END ERROR TRAP *)
(*
  The separate test against BUF.COUNT + 1 is necessary to allow the last
char's in the buffer to be removed.  In this case, no char's need be
shifted, only BUF.COUNT need be decremented.
*)
    IF (STRTCL+RMVCNT) < (BUF.COUNT+1) THEN
        FOR MOVPTR := STRTCL+RMVCNT TO BUF.COUNT DO
           BUF.CHARS[MOVPTR-RMVCNT] := BUF.CHARS[MOVPTR];
        END(*FOR*);
    END(*IF*);
    DEC(BUF.COUNT,RMVCNT);
    BUF.CHARS[BUF.COUNT+1] := NULL;
END RMVCHR;

PROCEDURE APPENDA2B(BufA : BUFTYP; VAR BufB : BUFTYP);
(*
**************************** APPENDA2B *****************************
Append A buffer to B buffer.
This routine will append the first buffer to the second.
*)

VAR C : CARDINAL;

BEGIN
  IF BufA.COUNT+BufB.COUNT <= BUFSIZ THEN
    FOR C := 1 TO BufA.COUNT + 1 DO
      INC(BufB.COUNT);
      BufB.CHARS[BufB.COUNT] := BufA.CHARS[C];
    END(*FOR*);
(* Null terminator is included in this copy operation *)
    INC(BufB.LENGTH,BufA.LENGTH);
    DEC(BufB.COUNT);
  END(*IF*);
END APPENDA2B;

PROCEDURE CONCATAB2C(BufA,BufB : BUFTYP; VAR BufC : BUFTYP);
(*
****************** CONCATAB2C *******************************
Concatenate A and B buffers and store into C buffer.
*)

VAR C : CARDINAL;

BEGIN
  IF BufA.COUNT+BufB.COUNT <= BUFSIZ THEN
    BufC := BufA;
    FOR C := 1 TO BufB.COUNT + 1 (* including null terminator *) DO
      INC(BufC.COUNT);
      BufC.CHARS[BufC.COUNT] := BufB.CHARS[C];
    END(*FOR*);
    INC(BufC.LENGTH,BufB.LENGTH);
    DEC(BufC.COUNT);
  END(*IF*);
END CONCATAB2C;

PROCEDURE INSERTAin2B(SBUF : BUFTYP; VAR DBUF : BUFTYP; POSN : CARDINAL);
(*
************************ INSERTAin2B ************************************
INSERT A string into B string.
Inserts the first string into the 2-nd string at the POSN specified.
*)

VAR SUM,C,DBUFPTR : CARDINAL;

BEGIN
  SUM := SBUF.COUNT + DBUF.COUNT;
  IF (SUM < BUFSIZ) AND (POSN <= DBUF.COUNT+1) AND (POSN > 0) THEN
(*
  If POSN is greater than the size of the dest string, an appending
  operation is done.
     Make room for the source characters in dest string.  Null terminator
  is included in this copy operation.
*)
    DBUFPTR := DBUF.COUNT + 1;
    C := DBUFPTR + SBUF.COUNT;
    WHILE (DBUFPTR >= POSN) DO
      DBUF.CHARS[C] := DBUF.CHARS[DBUFPTR];
      DEC(C);
      DEC(DBUFPTR);
    END(*WHILE*);
(*
  Copy source string into the room just made in dest string.  Null
  terminator is NOT included in this copy operation.
*)
    FOR C := 1 TO SBUF.COUNT DO
      INC(DBUFPTR);
      DBUF.CHARS[DBUFPTR] := SBUF.CHARS[C];
    END(*FOR*);
  END(*IF*);
  DBUF.COUNT := SUM;
END INSERTAin2B;


PROCEDURE ASSIGN2BUF(CHARRAY : ARRAY OF CHAR; VAR BUF : BUFTYP);
(*
******************************** ASSIGN2BUF *********************************
ASSIGN string TO BUFfer.
THE PURPOSE OF THIS PROCEDURE IS TO DEVIATE FROM THE STANDARD DEFINITION OF
THE CHARACTER STRING IN THAT THE STANDARD REQUIRES STRING ARRAYS TO BEGIN AT
THE 0-TH ELEMENT, AND THIS IS NOT CONVENIENT.  TO ALLOW THE STRING ARRAYS TO
BEGIN AT THE 1-ST ELEMENT, CONVERSION PROCEDURES BETWEEN THE STANDARD AND
CREATED STRING TYPES IS NEEDED.
    THIS PROCEDURE CONVERTS FROM A COMPILER STD 0-TH ELEMENT CHAR ARRAY INTO
THE BUF TYPE USED BY THIS PROGRAM.

INPUT PARAMS:  CHARRAY--INPUT ARRAY OF CHARACTER IN STD FORMAT (BEGIN AT 0).

OUTPUT PARAMS: BUF--OUTPUT STRING IN BUFFER TYPE AS USED BY THIS PGM.
*)

VAR
   POSN : CARDINAL;

BEGIN
    BUF.COUNT := STRLENFNT(CHARRAY);
    FOR POSN := 1 TO BUF.COUNT DO
        BUF.CHARS[POSN] := CHARRAY[POSN-1];
    END(*FOR*);
(*
  APPEND NULL CHAR SO STRING IS NULL TERMINATED AS WELL
*)
    IF BUF.COUNT < BUFSIZ THEN BUF.CHARS[BUF.COUNT+1] := NULL; END(*IF*);
    BUF.LENGTH := BUF.COUNT;
(*    TRIM(BUF); *)
END ASSIGN2BUF;

PROCEDURE COPYLEFT(saddr,daddr : ADDRESS; Nbytes : CARDINAL);
(*
********************************** COPYLEFT *******************************
COPY forward and to the LEFT.
Copies from source to destination the number of bytes specified in Nbytes,
copying in a forward direction.  In overlapping situations, use this
procedure only to shift bytes to the LEFT.  Bypasses type, range & index
checking -- Use with care.
Assume Source & Destination to be of SIZE at least Nbytes.  That rules out
the possibility of running up against segment boundaries.
*)
VAR
  C   : CARDINAL;
  S,D : CHRPTR;

BEGIN
     IF Nbytes=0 THEN RETURN; END(*IF*);
(* Need separate test, else -1 which would be a no-no *)

     S := saddr;
     D := daddr;
     FOR C := 0 TO Nbytes-1 DO D^[C] := S^[C]; END(*FOR*);
(*
     C := 0;
     WHILE Nbytes > 0 DO
      D^[C] := S^[C];
      DEC(Nbytes);
      INC(C);
     END(*WHILE*);
*)
END COPYLEFT;

PROCEDURE COPYRIGHT(saddr,daddr : ADDRESS; Nbytes : CARDINAL);
(*
*********************************** COPYRIGHT ******************************
COPY backward and to the RIGHT.
Copies from source to destination the number of bytes specified in Nbytes,
copying in a backword direction.  In overlapping situations, use this
procedure only to shift bytes to the RIGHT.  Bypasses type, range &
index checking -- Use with care.
Assume Source & Destination to be of SIZE at least Nbytes.  That rules out
the possibility of running up against segment boundaries.
*)
VAR
  C   : CARDINAL;
  S,D : CHRPTR;

BEGIN
     IF Nbytes=0 THEN RETURN; END(*IF*);
(* Need separate test, else -1 which would be a no-no *)

     S := saddr;
     D := daddr;
     FOR C := Nbytes-1 TO 0 BY -1 DO D^[C] := S^[C]; END(*FOR*);
(*
     C := Nbytes-1;
     WHILE C > 0 DO
       D^[C] := S^[C];
       DEC(C);
     END(*WHILE*);
     D^[C] := S^[C];   (* C must = 0 now *)
*)
END COPYRIGHT;

PROCEDURE FILLCHAR(daddr : ADDRESS; Nbytes : CARDINAL; ch : CHAR);
(*
****************************** FILLCHAR *******************************
Fill Char.
Does just that.  Bypasses type, range & index checking -- Use with care.
Assume Destination to be of SIZE at least Nbytes.  That rules out the
possibility of running up against segment boundaries.
*)
VAR
  C : CARDINAL;
  D : CHRPTR;

BEGIN
     IF Nbytes=0 THEN RETURN; END(*IF*);
(* Need separate test, else -1 which would be a no-no *)

     D := daddr;
     FOR C := 0 TO Nbytes-1 DO D^[C] := ch; END(*FOR*);
(*
     C := 0;
     WHILE Nbytes > 0 DO
      D^[C] := ch;
      DEC(Nbytes);
      INC(C);
     END(*WHILE*);
*)
END FILLCHAR;

PROCEDURE SCANFWD(daddr : ADDRESS; Nbytes : CARDINAL; ch : CHAR;
                                               equal : BOOLEAN) : CARDINAL;
(*
********************************** SCANFWD *******************************
Scan Forward.
Scans forward Nbytes number of bytes for byte value of ch, returns number of
bytes scanned if successful, and Nbytes+1 if unsuccessful.  Stops at equality
if equal=TRUE, and at inequality if equal=FALSE.
Bypasses type, range & index checking -- Use with care.
Assume Destination to be of SIZE at least Nbytes.  That rules out the
possibility of running up against segment boundaries.
*)

VAR C         : CARDINAL;
    CONDITION : BOOLEAN;
    D         : CHRPTR;

BEGIN
  IF Nbytes=0 THEN RETURN 0; END(*IF*);
  D := daddr;
  C := 0;
  REPEAT
    CONDITION := D^[C] = ch;
    INC(C);
  UNTIL (C >= Nbytes) OR (CONDITION = equal);

  IF CONDITION = equal THEN     (* Scan succeeded *)
    RETURN(C);
  ELSE
    RETURN(C+1);                (* Scan failed    *)
  END(*IF*);
END SCANFWD;

PROCEDURE SCANBACK(daddr : ADDRESS; Nbytes : CARDINAL; ch : CHAR;
                                               equal : BOOLEAN) : CARDINAL;
(*
********************************** SCANBACK *******************************
Scan Backward.
Scans backward Nbytes number of bytes for byte value of ch, returns number of
bytes scanned if successful, and 0 if unsuccessful.  Stops at equality
if equal=TRUE, and at inequality if equal=FALSE.
Bypasses type, range & index checking -- Use with care.
Assume Destination to be of SIZE at least Nbytes.  That rules out the
possibility of running up against segment boundaries.
*)

VAR I         : INTEGER;
    C         : CARDINAL;
    CONDITION : BOOLEAN;
    D         : CHRPTR;

BEGIN
  IF Nbytes=0 THEN RETURN 0; END(*IF*);

  D := daddr;
  C := Nbytes;
  REPEAT
    DEC(C);                     (* Can't allow C to go neg *)
    CONDITION := D^[C] = ch;
  UNTIL (C <= 0) OR (CONDITION = equal);

  IF CONDITION = equal THEN     (* Scan succeeded *)
    RETURN(C+1);
  ELSE
    RETURN(C);                  (* Scan failed    *)
  END(*IF*);
END SCANBACK;

(* -------------------------------------------------------------------------- InitStringListPointerType -----------------------------------*)
PROCEDURE InitStringListPointerType() : StringDoubleLinkedListPointerType;
(*
PROCEDURE InitStringListPointerType(VAR sp : StringDoubleLinkedListPointerType) : StringDoubleLinkedListPointerType;
  Purpose of this routine is to init all the fields to either NIL or zero.
*)

VAR
  StringListP : StringDoubleLinkedListPointerType;

BEGIN
  NEW(StringListP);
  WITH StringListP^ DO
    StartOfList := NIL;
    EndOfList := NIL;
    CurrentPlaceInList := NIL;
    PrevPlaceInList := NIL;
    len := 0;
    NextPosition := 0;
  END; (* With StringListP *)

  RETURN(StringListP);
END InitStringListPointerType;


(* -------------------------------------------------------------------------- AppendStringToList -----------------------------------*)

PROCEDURE AppendStringToList(StringListP : StringDoubleLinkedListPointerType; strng : ARRAY OF CHAR);
  VAR
    StringP : StringItemPointerType;
    i : CARDINAL;

BEGIN
  IF StringListP = NIL THEN
    RETURN;
  END;

  NEW(StringP);

  StringP^.Prev := StringListP^.EndOfList;
  StringP^.Next := NIL;
  FOR i := 0 TO HIGH(strng) DO
    StringP^.S.CHARS[i+1] := strng[i];
  END; (* for strng *)
  StringP^.S.LENGTH := HIGH(strng) + 1;
  StringP^.S.COUNT := StringP^.S.LENGTH;

  IF StringListP^.StartOfList = NIL THEN   (* List is empty *)
    StringListP^.StartOfList := StringP;
    StringListP^.PrevPlaceInList := StringP;
    StringListP^.EndOfList := StringP;
  ELSE  (* List is not empty *)
    StringListP^.PrevPlaceInList := StringListP^.EndOfList;
    StringListP^.EndOfList^.Next := StringP;
  END; (* if list is empty *)

  StringListP^.EndOfList := StringP;
  StringListP^.CurrentPlaceInList := StringP;
  INC(StringListP^.len);

END AppendStringToList;


(* -------------------------------------------------------------------------- NextStringFromList -----------------------------------*)


PROCEDURE NextStringFromList(StringListP : StringDoubleLinkedListPointerType);
  VAR
    NextStringP : StringItemPointerType;
    c : CARDINAL;

BEGIN
  IF StringListP = NIL THEN
    RETURN;
  END;

  INC(StringListP^.NextPosition);
  NextStringP := StringListP^.CurrentPlaceInList^.Next;
  StringListP^.PrevPlaceInList := StringListP^.CurrentPlaceInList;
  StringListP^.CurrentPlaceInList := NextStringP;
  RETURN;
END NextStringFromList;

(* ------------------------------------------------------------------ GetNextStringFromList ---------------------- *)

PROCEDURE GetNextStringFromList(StringListP : StringDoubleLinkedListPointerType) : StringItemPointerType;
  VAR
    StringP : StringItemPointerType;

BEGIN
  IF StringListP = NIL THEN
    RETURN(NIL);
  END;

  StringP := StringListP^.CurrentPlaceInList;
  NextStringFromList(StringListP);
  RETURN(StringP);

END GetNextStringFromList;

(* -------------------------------------------------- CurrentPointerBeginning ---------------------------- *)

PROCEDURE CurrentPointerBeginning(StringListP : StringDoubleLinkedListPointerType);

BEGIN
  StringListP^.PrevPlaceInList := StringListP^.CurrentPlaceInList;
  StringListP^.CurrentPlaceInList := StringListP^.StartOfList;
END CurrentPointerBeginning;


(* -------------------------------------------------- CurrentPointerEnding ---------------------------- *)

PROCEDURE CurrentPointerEnding(StringListP : StringDoubleLinkedListPointerType);

BEGIN
  StringListP^.PrevPlaceInList := StringListP^.CurrentPlaceInList;
  StringListP^.CurrentPlaceInList := StringListP^.EndOfList;
END CurrentPointerEnding;

(* -------------------------------------------------- PrevStringFromList -------------------------------- *)

PROCEDURE PrevStringFromList(StringListP : StringDoubleLinkedListPointerType);
  VAR
    PrevStringP : StringItemPointerType;
    c : CARDINAL;

BEGIN
  IF StringListP = NIL THEN
    RETURN;
  END;

  PrevStringP := StringListP^.CurrentPlaceInList^.Prev;
  StringListP^.PrevPlaceInList := StringListP^.CurrentPlaceInList;
  StringListP^.CurrentPlaceInList := PrevStringP;
  RETURN;
END PrevStringFromList;

(* -------------------------------------------- GetPrevStringFromList ----------------------------------------- *)

PROCEDURE GetPrevStringFromList(StringListP : StringDoubleLinkedListPointerType) : StringItemPointerType;
  VAR
    PrevStringP : StringItemPointerType;
    c : CARDINAL;

BEGIN
  IF StringListP = NIL THEN
    RETURN(NIL);
  END;

  PrevStringP := StringListP^.CurrentPlaceInList;
  PrevStringFromList(StringListP);

  RETURN(PrevStringP);
END GetPrevStringFromList;

BEGIN (* ----------------------- Module Body ------------------------------------*)
  ROOTNAM.CHARS[1] := NULL;
  MOREFNM := FALSE;
  UPLOW := ORD('a') - ORD('A');
END UTILLIB.
