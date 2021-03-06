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
*)

  FROM SYSTEM IMPORT ADDRESS;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
(*  FROM Terminal IMPORT ReadString;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteInt,WriteCard; *)
(*  FROM RTSMain IMPORT Terminate, Status; *)
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETTKN,GETTKNSTR;
  FROM Environment IMPORT GetCommandLine;

(*
    THE FOLLOWING ARE DECLARED IN THE DEFINITION MODULE.
CONST
    BUFSIZ    = 255;
    CR        = 15C; (* CHR(13) *)
    LF        = 12C; (* CHR(10) *)
    MAXCARD   = 0FFFFH;
    MAXINT    = 7FFFH;
    MININT    = 8000H;
    ESC       = 33C; (* CHR(27) *)
    NULL      = 0C;
    BLANK     = ' ';
    CTRLCOD   = 31;


TYPE
(* THE TYPE HAS ONE MORE THAN BUFSIZ SO ARRAY SUBSCRIPT ERRORS ARE AVOIDED. *)
  STRTYP = ARRAY [1..BUFSIZ+1] OF CHAR;(* NOW COMPATIBLE WITH BUFTYP.CHARS *)
  BUFTYP = RECORD
    CHARS : STRTYP; (* ARRAY [1..BUFSIZ+1] OF CHAR; *)
    LENGTH,           (* NUMBER OF CHARS IN BUF NOT INCL'G SPECIAL CHARS.*)
    COUNT : CARDINAL; (* TOTAL NUMBER OF CHARS IN BUF *)
  END(*RECORD*);
  STR10TYP = ARRAY [0..10] OF CHAR;
  CHRPTR   = POINTER TO CHAR;
*)
TYPE
  CHRBUF   = ARRAY [0..32737] OF CHAR;
  CHRPTR   = POINTER TO CHRBUF;

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
  COL := STRLENFNT(BUF.CHARS);
  WHILE (COL > 0 ) AND (BUF.CHARS[COL] = BLANK) DO DEC(COL); END(*WHILE*);
  BUF.COUNT := COL;
  BUF.CHARS[COL+1] := NULL;  (* Null terminate string w/o trailing blanks *)
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
               CNT--COUNT OF CHARS IN INPUT ARRAY.
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
    TRIM(BUF);
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

PROCEDURE GETFNM(PROMPT,NAMDFT,TYPDFT : BUFTYP; VAR FILNAM : BUFTYP);
(*
********************************** GETFNM *******************************
GET FILE NAME SUBROUTINE.
THIS ROUTINE WILL GET A FILE NAME FROM THE TERMINAL, AND ALLOW DEFAULTS
FOR THE ENTIRE FILE NAME AND/OR THE EXTENSION.
PROMPT--THIS IS THE PROMPT TO BE PRINTED TO THE TERMINAL BEFORE THE
      FILENAME IS REQUESTED.  IT MUST HAVE A COLON, : , AS ITS LAST
      CHARACTER TO OPERATE NORMALLY.
NAMDFT--THIS IS THE NAME DEFAULT STRING THAT HAS THE FILE NAME THAT WILL
      BE USEDIF NO INPUT NAME IS ENTERED.  THIS MUST HAVE THE .EXT AS
      WELL AS THE NAME PART; THE TYPDFT WILL NOT BE APPENDED TO THE
      DEFAULT NAME.
TYPDFT--IS A 4 BYTE ARRAY THAT IS THE DEFAULT TYPE (EXTENSION) OF THE
      FILE.  IF A DOT IS NOT PRESENT IN THE INPUT CHARACTER STRING,
      THIS ARRAY WILL BE APPENDED TO THE INPUT STRING TO YIELD THE
      FULL FILENAME.
FILNAM--IS THE OUTPUT FILENAME THAT IS A COMBINATION OF THE INPUT
    STRING, THE TYPE DEFAULT, AND THE NAME DEFAULT, AS IS APPROPRIATE.

FUNCTION VALUE RETURNED WILL BE FALSE IF THE DEFAULT NAME WAS USED, I.E.,
NO NAME WAS INPUT FROM TERMINAL.  FUNCTION VALUE WILL BE TRUE OTHERWISE.
  IF A NAME IS INPUT WITHOUT AN EXTENSION, BEFORE THE DEFAULT EXTENSION IS
APPENDED, IT IS STORED IN THE ROOTNAM BUFFER.  WHEN NO STRING IS INPUT, THIS
NAME WITH THE DEFAULT EXTENSION APPENDED IS USED BEFORE THE FULL DEFAULT NAME
IS.

INPUT FROM GBL VAR'S:  NULL, BLANK.
*)

CONST
  DOT = '.';
  COLON = ':';
  NAMSIZ = 80;
  PROMPTSIZ = 30;
  TYPSIZ = 4;

VAR
  EOFFLG, ERRFLG, DOTFLG            : BOOLEAN;
  I, J, RETCOD, c                   : CARDINAL;
  DFTLEN, PROMPTLEN, STRLEN, NAMLEN : CARDINAL;
  TMPBUF                            : BUFTYP;
  SUM                               : INTEGER;

BEGIN
  DOTFLG := FALSE;
  NAMLEN := 0;

  REPEAT      (*UNTIL NOT ERRFLG*)
    ERRFLG := FALSE;
    IF MOREFNM THEN
      GETTKNSTR(TOKEN,SUM,RETCOD);
      IF RETCOD = 0 THEN
        MOREFNM := (DELIMCH = BLANK); (* re-iterate because may be *)
        FILNAM := TOKEN;              (* needed for TMPBUF FILNAM. *)
      ELSE  (* THIS BRANCH SHOULD NEVER BE NEEDED. *)
        FILNAM.COUNT := 0;
        MOREFNM := FALSE;
        WriteString('*Warning* From GETFNM, MOREFNM is true but RETCOD # 0.');
        WriteLn;
      END(*IF*);
    ELSE
(*    
  GetCommandLine(inputline);
  c := LENGTH(inputline);

  IF c = 0 THEN
    ASSIGN2BUF('ENTER  INPUT FILE NAME : ',PROMPT);
    ASSIGN2BUF('FTEST.TXT',NAMDFT);
    ASSIGN2BUF('.TXT',TYPDFT);
    GETFNM(PROMPT,NAMDFT,TYPDFT,INFNAM);

    ASSIGN2BUF('ENTER OUTPUT FILE NAME : ',PROMPT);
    ASSIGN2BUF('WORDFILE.DOC',NAMDFT);
    ASSIGN2BUF('.DOC',TYPDFT);
    GETFNM(PROMPT, NAMDFT, TYPDFT, OUTFNAM);

  ELSE
    ASSIGN2BUF(inputline,INBUF);
    INI1TKN(INBUF);
    GETTKN(TOKEN,TKNSTATE,I,RETCOD);
    IF RETCOD > 0 THEN
      WriteString("GETTKN's RETCOD is ");
      WriteCard(RETCOD,0);
      WriteLn;
      HALT;
    ELSE
      INFNAM := TOKEN;
    END;
    GETTKN(TOKEN,TKNSTATE,I,RETCOD);
    IF RETCOD = 0 THEN
      OUTFNAM := TOKEN;
    ELSE
      WriteString("GETTKN's RETCOD is ");
      WriteCard(RETCOD,0);
      WriteLn;
      HALT;
    END;
  END;
*)    
      WriteString(PROMPT.CHARS);
      FOR J := 1 TO BUFSIZ+1 DO
        TMPBUF.CHARS[J] := BLANK
      END(*FOR*);
      ReadString(TMPBUF.CHARS);
      WriteLn;
      SkipLine;
      TRIM(TMPBUF);
      INI1TKN(TMPBUF);
      GETTKNSTR(TOKEN,SUM,RETCOD);
      IF RETCOD = 0 THEN
        MOREFNM := (DELIMCH = BLANK);
        FILNAM := TOKEN;
      ELSE
        FILNAM.COUNT := 0;  (* JUST TO MAKE SURE, BUT SHOULD ALREADY BE 0. *)
        MOREFNM := FALSE;
     END(*IF*);
    END(*IF*);
    I := 1;
    WHILE I <= FILNAM.COUNT DO
      IF FILNAM.CHARS[I] = DOT THEN DOTFLG := TRUE; END(*IF*);
      INC(I);
    END(*WHILE*);
    FILNAM.LENGTH := I-1;

    IF FILNAM.LENGTH = 0 THEN
      IF ROOTNAM.CHARS[1] = 0C THEN
(* ROOTNAM IS NULL, SO CONTINUE WITH USING THE FULL DEFAULT NAME. *)
(* NULL STRING.  USE DEFAULT. *)

        I := 1;
        FILNAM.LENGTH := NAMDFT.COUNT;
        WHILE I <= FILNAM.LENGTH DO
          FILNAM.CHARS[I] := NAMDFT.CHARS[I];
          INC(I);
        END(*WHILE*);
        DOTFLG := TRUE;
      ELSE
        FILNAM := ROOTNAM;
        DOTFLG := FALSE;
      END(*IF*);
    END(*IF*);
    IF NOT DOTFLG THEN
      ROOTNAM := FILNAM;   (* STORE NAME WITHOUT EXTENSION. *)
      (* APPEND TYPE SINCE NO TYPE INPUT *)
      IF FILNAM.COUNT+TYPSIZ > NAMSIZ THEN
        WriteString('NAME TOO LONG AFTER APPENDING TYPE. TRY AGAIN.');
        WriteLn;
        ERRFLG := TRUE
      ELSE   (*NO ERROR*)
        FOR I := 1 TO TYPSIZ DO
            FILNAM.CHARS[FILNAM.LENGTH + I ] := TYPDFT.CHARS[I]
        END(*FOR*);
        INC(FILNAM.LENGTH, TYPSIZ);
      END(*IF*)
    END(*IF*);
  UNTIL NOT ERRFLG;
  FILNAM.COUNT := FILNAM.LENGTH;
(* NULL TERMINATE FILNAM STRING *)
  FILNAM.CHARS[FILNAM.COUNT + 1] := NULL;
END GETFNM;

BEGIN
  ROOTNAM.CHARS[1] := NULL;
  MOREFNM := FALSE;
  UPLOW := ORD('a') - ORD('A');
END UTILLIB.

