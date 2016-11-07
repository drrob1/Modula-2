(*V0=V34*)
(*V1=V40*)
IMPLEMENTATION MODULE UL2;

(*
Copyright (C) 1987  Robert Solomon MD.  All rights reserved.
  This is the utility functions module, that contains all of the generally
  useful subroutines.

  REVISION HISTORY
  ----------------
  9  Jun 87 -- Added STRCMPFNT.
  15 Jun 87 -- Added CHINBUFFNT, and MAXCARDFNT.
  30 Oct 87 -- Added TRIMFNT and cleaned up other comments and code.
  3  Mar 88 -- Module renamed to UTILLIB.  GETCNT deleted, and the TRIM 
                Procedure was added.  Also, the CHINBUFFNT name was 
                changed to the LCHINBUFFNT to mean the Last CH IN BUF FNT.
  26 Mar 88 -- Changed NAMSIZ const in GETFNM to 80 so that long paths are
                properly handled.
  31 Mar 88 -- Converted to M2 V3.03.
  1  Jul 88 -- 1) Added the low level CODE procedures, and made this the 
                UL2 (UTILLIB2) Procedure.
               2) Reversed the order of the LCHINBUFFNT and RMVCHR params.
               3) Added the APPENDA2B and the CONCATAB2C Procedures.
               4) Added the INSERTAin2B Procedure.
               5) Changed CONBUF name to ASSIGN2BUF;
   21 Nov 88 -- Amended the CODE procedures so that the direction flag is
                cleared if it is set.  This is to straighten out a pblm with
                the compiler just reported on BIX.
   11 Nov 90 -- Utilized GETTKNSTR.
   18 Jan 92 -- Converted to M-2 V 4.00.
    6 Nov 02 -- Converted to SBM2 V4 win.
*)
(*<V34
  FROM SYSTEM IMPORT
       ADR,ADDRESS,CODE,GETREG,SETREG,BYTE,WORD,AX,BX,CX,ES,DI,DS,SI;
  IMPORT MED;
  FROM BlockOps IMPORT BlockMoveForward,BlockMoveBackward,BlockMove,
    BlockPosition,BlockSet;
  IMPORT Break;
  FROM Terminal IMPORT ReadString;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteInt,WriteCard;
  FROM RTSMain IMPORT Terminate, Status;
  IMPORT UL3;
V34>*)
(*<V40*)
  FROM SYSTEM IMPORT ADR,ADDRESS,BYTE,WORD;
  FROM Strings IMPORT Length,Insert,Delete,Concat,CompareStr;
(*V40>*)
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM Environment IMPORT GetCommandLine;
  IMPORT UTILLIB;
  FROM UTILLIB IMPORT BUFSIZ,NULL,BLANK,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARD;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETTKN,GETTKNSTR;
  FROM MemUtils IMPORT FillMemBYTE, FillMemWORD, FillMemDWORD, ScanMemBYTE,
    ScanMemNeBYTE, ScanMemWORD, ScanMemNeWORD, ScanMemDWORD, ScanMemNeDWORD, MoveMem;
(*
    The following are declared in the UTILLIB DEFINITION MODULE.
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
(*
  The type has one more than BUFSIZ so array subscript errors are avoided.
*)
  STRTYP = ARRAY [1..BUFSIZ+1] OF CHAR;(* Now compatible with BUFTYP.CHARS *)
  BUFTYP = RECORD
    CHARS : STRTYP; (* ARRAY [1..BUFSIZ+1] OF CHAR; *)
    LENGTH,           (* Number of chars in BUF not incl'g special chars.*)
    COUNT : CARDINAL; (* Total number of chars in BUF *)
  END(*RECORD*);
  STR10TYP = ARRAY [0..10] OF CHAR;
*)

VAR ROOTNAM,TOKEN : BUFTYP;
    MOREFNM       : BOOLEAN;  (* Used for GETFNM *)
    TKNSTATE      : FSATYP;
(*
  Used for debugging this module.
    S1,S2,S3,S4,S5    : STR10TYP;
    B1,B2,B3,B4,B5    : BUFTYP;
    C1,C2,C3,C4,C5,C6 : CARDINAL;
    CH                : CHAR;
*)

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

BEGIN
     IF Nbytes=0 THEN RETURN; END(*IF*);

(*<V34
     SETREG(CX,Nbytes);
     SETREG(SI,saddr.OFFSET);
     SETREG(DI,daddr.OFFSET);
     SETREG(DS,saddr.SEGMENT);
     SETREG(ES,daddr.SEGMENT);
     CODE(0FCH,                 (* CLD       *)
          0F3H,0A4H);           (* REP MOVSB *)
V34>*)
(*<V40
     BlockMoveForward(daddr,saddr,Nbytes);
V40>*)
     MoveMem(saddr^,daddr^,Nbytes);
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

BEGIN
     IF Nbytes=0 THEN RETURN; END(*IF*);

(*<V34
     INC(saddr,Nbytes-1); INC(daddr,Nbytes-1);
     SETREG(CX,Nbytes);
     SETREG(SI,saddr.OFFSET);
     SETREG(DI,daddr.OFFSET);
     SETREG(DS,saddr.SEGMENT);
     SETREG(ES,daddr.SEGMENT);
     CODE(0FDH,                 (* STD       *)
          0F3H,0A4H);           (* REP MOVSB *)
     CODE(0FCH);                (* CLD       *)
V34>*)
(*<V40
    BlockMoveBackward(daddr,saddr,Nbytes);
V40>*)
    MoveMem(saddr^,daddr^,Nbytes);
END COPYRIGHT;

PROCEDURE FILLCHAR(daddr : ADDRESS; Nbytes : CARDINAL; ch : CHAR);
(*
****************************** FILLCHAR *******************************
Fill Char.
Does just that.  Bypasses type, range & index checking -- Use with care.
Assume Destination to be of SIZE at least Nbytes.  That rules out the
possibility of running up against segment boundaries.
*)

BEGIN
     IF Nbytes=0 THEN RETURN; END(*IF*);

(*<V34
     SETREG(AX,ch);
     SETREG(CX,Nbytes);
     SETREG(DI,daddr.OFFSET);
     SETREG(ES,daddr.SEGMENT);
     CODE(0FCH,                 (* CLD       *)
          0F3H,0AAH);           (* REP STOSB *)
V34>*)
(*<V40*)
(*     BlockSet(daddr,Nbytes,ADR(ch),1);*)
      UL3.FILLCHAR(daddr,Nbytes,ch);
(*V40>*)
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

VAR Nbytesleft,FLAGS,C : CARDINAL;
    ZEROFLAGSET        : BOOLEAN;

BEGIN
  IF Nbytes=0 THEN RETURN 0; END(*IF*);

(*<V34
  SETREG(CX,Nbytes);
  SETREG(AX,ch);
  SETREG(BX,equal);
  SETREG(DI,daddr.OFFSET);
  SETREG(ES,daddr.SEGMENT);
  CODE(                      (* inline code section *)
    0F7H,0C3H,001H,000H,  (*    TEST  BX,1  ; TEST FOR EQUALITY? *)
    074H,006H,            (*    JZ    $1    ; SKIP IF NOT        *)
    0FCH,                 (*    CLD         ; SCAN FORWARDS      *)
    0F2H,0AEH,            (*    REPNE SCASB ; INEQUALITY TEST    *)
    0E9H,003H,000H,       (*    JMP   $4    ; AND CONTINUE       *)
    0FCH,                 (* $1 CLD         ; SCAN FORWARDS      *)
    0F3H,0AEH,            (*    REPE  SCASB ; EQUALITY TEST      *)
    09CH,                 (* $4 PUSHF       ; NEED ZeroFlag      *)
    058H);                (*    POP   AX    ; LEAVE ZF IN AX     *)
  GETREG(CX,Nbytesleft);
  GETREG(AX,FLAGS);
  ZEROFLAGSET := 6 IN BITSET(FLAGS);
  IF ZEROFLAGSET = equal THEN   (* Scan succeeded *)
    RETURN(Nbytes - Nbytesleft);
  ELSE
    RETURN(Nbytes + 1);         (* Scan failed    *)
  END(*IF*);
V34>*)
(*<V40*)
(*
  IF equal THEN 
    C := BlockPosition(daddr,Nbytes,ADR(ch),1);
    IF C = MAXCARD THEN 
      C := Nbytes + 1; 
    ELSE
      INC(C); (* Picket fence pblm *)
    END(*IF*);
    RETURN(C);
  ELSE
    RETURN(UTILLIB.SCANFWD(daddr,Nbytes,ch,equal));
  END(*IF*);
*)
  RETURN(UL3.SCANFWD(daddr,Nbytes,ch,equal));
(*V40>*)
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

VAR Nbytesleft,FLAGS : CARDINAL;
    ZEROFLAGSET      : BOOLEAN;

BEGIN
  IF Nbytes=0 THEN RETURN 0; END(*IF*);

(*<V34
  INC(daddr,Nbytes-1);
  SETREG(CX,Nbytes);
  SETREG(AX,ch);
  SETREG(BX,equal);
  SETREG(DI,daddr.OFFSET);
  SETREG(ES,daddr.SEGMENT);

  CODE(                      (* inline code section *)
                          (* $2 NOP         ; SEARCH BACKWARDS   *)
    0F7H,0C3H,001H,000H,  (*    TEST  BX,1  ; TEST FOR EQUALITY? *)
    074H,006H,            (*    JZ    $3    ; SKIP IF NOT        *)
    0FDH,                 (*    STD         ; SCAN BACKWARDS     *)
    0F2H,0AEH,            (*    REPNE SCASB ; INEQUALITY TEST    *)
    0E9H,003H,000H,       (*    JMP   $4    ; JUMP TO FINISH.    *)
    0FDH,                 (* $3 STD         ; SCAN BACKWARDS     *)
    0F3H,0AEH,            (*    REPE  SCASB ; EQUALITY TEST      *)
    09CH,                 (* $4 PUSHF       ; NEED ZeroFlag      *)
    058H);                (*    POP   AX    ; LEAVE ZF IN AX     *)
  CODE(0FCH);             (*    CLD                              *)
  GETREG(CX,Nbytesleft);
  GETREG(AX,FLAGS);
  ZEROFLAGSET := 6 IN BITSET(FLAGS);
  IF ZEROFLAGSET = equal THEN   (* Scan succeeded *)
    RETURN(Nbytesleft+1);
  ELSE
    RETURN(0);                  (* Scan failed    *)
  END(*IF*);
V34>*)
(*<V40*)
  RETURN(UL3.SCANBACK(daddr,Nbytes,ch,equal));
(*V40>*)
END SCANBACK;

PROCEDURE FillWord(daddr : ADDRESS; Nwords : CARDINAL; val : WORD);
(*
***************************** FillWord ************************************
Fill Word.
Does just that.  Bypasses type, range & index checking -- Use with care.
Assume Destination to be of SIZE at least Nbytes.  That rules out the
possibility of running up against segment boundaries.
*)

BEGIN
  IF Nwords > 0 THEN
(*<V34
    SETREG(AX,val);
    SETREG(CX,Nwords);
    SETREG(DI,daddr.OFFSET);
    SETREG(ES,daddr.SEGMENT);
    CODE(0FCH,                 (* CLD       *)
         0F3H,0ABH);           (* REP STOSW *)
V34>*)
(*<V40*)
    BlockSet(daddr,Nwords*2,ADR(val),2);
(*V40>*)
  END(*IF*);
END FillWord;

PROCEDURE CopyWords(saddr,daddr : ADDRESS; Nwords : CARDINAL);
(*
*************************** CopyWord ***************************************
Does just that.  Bypasses type, range & index checking -- Use with care.
Assume Source & Destination to be of SIZE at least Nwords.  That rules out
the possibility of running up against segment boundaries.
*)

BEGIN
  IF Nwords > 0 THEN
(*<V34
    SETREG(CX,Nwords);
    SETREG(SI,saddr.OFFSET);
    SETREG(DS,saddr.SEGMENT);
    SETREG(DI,daddr.OFFSET);
    SETREG(ES,daddr.SEGMENT);
    CODE(0FCH,                 (* CLD       *)
         0F3H,0A5H);           (* REP MOVSW *)
V34>*)
(*<V40*)
    BlockMove(daddr,saddr,Nwords*2);
(*V40>*)
  END(*IF*);
END CopyWords;

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
by returning a post incremented pointer.  This is true under the version
3.4 proc; version 4.00 uses the Length proc from the Strings Module.

*)

VAR NUMOFELE : CARDINAL;

BEGIN
(*<V34
  NUMOFELE := HIGH(CHARRAY) + 1;
  RETURN(SCANFWD(ADR(CHARRAY),NUMOFELE,NULL,TRUE)-1);
V34>*)
(*<V40*)
  RETURN(Length(CHARRAY));
(*V40>*)
END STRLENFNT;

PROCEDURE STRCMPFNT(STR1,STR2 : ARRAY OF CHAR) : INTEGER;
(*
*********************************** STRCMPFNT *******************************
STRING COMPARE FUNCTION.
THIS FUNCTION DOES A COMPARISON OF STRINGS, RETURNING:
  STR1 < STR2 MEANS RETURN A NUMBER < 0
  STR1 = STR2 MEANS RETURN A NUMBER = 0
  STR1 > STR2 MEANS RETURN A NUMBER > 0

IF STR1 IS A SUBSTRING OF STR2, THEN IT IS CONSIDERED < IT.  FOR TWO STRINGS
TO BE =, THEIR COUNT OF CHARACTERS (LENGTHS) ALSO MUST BE EQUAL.

*)

VAR I,CNT1,CNT2,RESULT : INTEGER;

BEGIN
(*
  Because the STRLENFNT returns the actual count of characters, and here we
  are using it to search a zero origin character array, the STRLENFNT will
  return a value that is 1 too high for our purposes.  The algorithm must
  compensate for this in the conditional tests of the main while loop below.
*)
(*<V34
  CNT1 := INTEGER(STRLENFNT(STR1));
  CNT2 := INTEGER(STRLENFNT(STR2));
  RESULT := 0;
  I := 0;
  WHILE (I < CNT1) AND (I < CNT2) AND (RESULT = 0) DO
    RESULT := INTEGER(ORD(STR1[I])) - INTEGER(ORD(STR2[I]));
    INC(I);
  END(*WHILE*);
V34>*)
(*<V40*)
  RESULT := CompareStr(STR1,STR2);
(*V40>*)
(* 
  Strings are only equal if their lengths are also equal.  Otherwise one
  is a substring of the other.  In this case, the shorter string is less
  than the longer one.
*)
  IF RESULT = 0 THEN 
    RESULT := INTEGER(Length(STR1)) - INTEGER(Length(STR2)); 
  END(*IF*);
  RETURN(RESULT);
END STRCMPFNT;

PROCEDURE LCHINBUFFNT(BUF : BUFTYP; CH : CHAR) : CARDINAL;
(*
************************************ LCHINBUFFNT *****************************
LAST CHARACTER IN BUFFER FUNCTION.
THIS FUNCTION SEARCHES THE BUFFER INPUT AS ITS BUFTYP PARAM FOR THE CHARACTER
INPUT AS ITS CHAR PARAM.  IT SEARCHES FROM THE END TO THE BEGINNING, SO WILL
FIND THE LAST SUCH CHARACTER IN THE BUFFER.  IT WILL RETURN 0 IF THE CHARACTER
WAS NOT FOUND.  THE NUMBER RETURNED WILL BE RELATIVE TO A ONE-ORIGIN STRING,
AS IS THE CHARS FIELD OF BUFTYP.

*)

BEGIN
  RETURN(SCANBACK(ADR(BUF.CHARS),BUF.COUNT,CH,TRUE));
END LCHINBUFFNT;

PROCEDURE APPENDA2B(BufA : BUFTYP; VAR BufB : BUFTYP);
(*
****************************** APPENDA2B ********************************
Append A buffer to B buffer.
This routine will append the first buffer to the second.
*)

BEGIN
  IF BufA.COUNT+BufB.COUNT <= BUFSIZ THEN
(*<V34
    COPYLEFT(ADR(BufA.CHARS),ADR(BufB.CHARS[BufB.COUNT+1]),BufA.COUNT+1);
V34>*)
(*<V40*)
    BlockMoveForward(ADR(BufB.CHARS[BufB.COUNT+1]),ADR(BufA.CHARS),
                                                               BufA.COUNT+1);
(*V40>*)
(* Null terminator is included in this copy operation *)
    INC(BufB.COUNT,BufA.COUNT);
    INC(BufB.LENGTH,BufA.LENGTH);
  END(*IF*);
END APPENDA2B;

PROCEDURE CONCATAB2C(BufA,BufB : BUFTYP; VAR BufC : BUFTYP);
(*
****************************** APPENDAB2C ********************************
Concatenate A and to B buffers and store into C buffer.
*)

BEGIN
  IF BufA.COUNT+BufB.COUNT <= BUFSIZ THEN
(*<V34
    BufC := BufA;
    COPYLEFT(ADR(BufB.CHARS),ADR(BufC.CHARS[BufC.COUNT+1]),BufB.COUNT+1);
(* Null terminator is included in this copy operation *)
V34>*)
(*<V40*)
    Concat(BufA.CHARS,BufB.CHARS,BufC.CHARS);
(*V40>*)
    BufC.COUNT  := BufA.COUNT  + BufB.COUNT;
    BufC.LENGTH := BufA.LENGTH + BufB.LENGTH;
  END(*IF*);
END CONCATAB2C;

PROCEDURE MRGBUFS(VAR BufA : BUFTYP; BufB : BUFTYP);
(*
****************************** MRGBUFS ***********************************
MERGE BUFFERS.
This routine will concatenate the two buffers input to it by appending the
second to the first, sort of like BufA = BufA + BufB.
*)

BEGIN
  IF BufA.COUNT+BufB.COUNT <= BUFSIZ THEN
(*<V34
    COPYLEFT(ADR(BufB.CHARS),ADR(BufA.CHARS[BufA.COUNT+1]),BufB.COUNT+1);
V34>*)
(*<V40*)
    BlockMoveForward(ADR(BufA.CHARS[BufA.COUNT+1]),ADR(BufB.CHARS),
                                                               BufB.COUNT+1);
(*V40>*)
(* Null terminator is included in this copy operation *)
    INC(BufA.COUNT,BufB.COUNT);
    INC(BufA.LENGTH,BufB.LENGTH);
  END(*IF*);
END MRGBUFS;

PROCEDURE INSERTAin2B(SBUF : BUFTYP; VAR DBUF : BUFTYP; POSN : CARDINAL);
(*
********************************INSERTAin2B ******************************
INSERT A string into B string.
Inserts the first string into the 2-nd string at the POSN specified.
*)

VAR SUM : CARDINAL;

BEGIN
  SUM := POSN + SBUF.COUNT;
  IF (SUM <= BUFSIZ) AND (POSN <= DBUF.COUNT+1) AND (POSN > 0) THEN
(*<V34
(* Allow appending operation *)
    COPYRIGHT(ADR(DBUF.CHARS[POSN]),ADR(DBUF.CHARS[SUM]),DBUF.COUNT+2-POSN);
(* Null terminator is included in this copy operation *)
    COPYLEFT(ADR(SBUF.CHARS),ADR(DBUF.CHARS[POSN]),SBUF.COUNT);
(* Null terminator is NOT included in this copy operation *)
V34>*)
(*<V40*)
    Insert(SBUF.CHARS,DBUF.CHARS,POSN-1); (* Picket fence pblm for POSN *)
(*V40>*)
    INC(DBUF.COUNT,SBUF.COUNT);
    INC(DBUF.LENGTH,SBUF.LENGTH);
  END(*IF*);
END INSERTAin2B;

PROCEDURE TRIMFNT(BUF : BUFTYP) : CARDINAL;
(*
****************************** TRIMFNT **************************
TRIM FUNCTION.
  
  This routine will trim the line passed to it by first looking for
  the null char and then scanning backward looking for the
  first non-blank character.  That is, trailing blanks are deleted,
  or trimmed.  A length of zero means a blank line 
  was input.  This is the preferred call to achieve this goal.  The
  getcnt procedure is still implemented for upward compatibility
  purposes.

  BUF -- BUFFER CONTAINING LINE TO SEARCH.
  
  INPUT FROM GBLVAR: BLANK
*) 

VAR 
  C,COL  : CARDINAL; 

BEGIN 
  COL := STRLENFNT(BUF.CHARS);
  C := SCANBACK(ADR(BUF.CHARS),COL,BLANK,FALSE);
  RETURN(C);
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

BEGIN 
  BUF.COUNT := SCANBACK(ADR(BUF.CHARS),STRLENFNT(BUF.CHARS),BLANK,FALSE);
  BUF.LENGTH := BUF.COUNT;
  BUF.CHARS[BUF.COUNT+1] := NULL; (* Null terminate string w/o *)
  RETURN;                         (* trailing blanks.          *)
END TRIM; 

PROCEDURE RMVCHR(VAR BUF : BUFTYP; STRTCL,RMVCNT : CARDINAL);
(*
*************************** RMVCHR *********************************
THE PURPOSE OF THIS ROUTINE IS TO REMOVE ANY NUMBER OF CHARS FROM THE
INPUT RECORD, BUF.

RMVCNT--COUNT OF CHAR'S TO BE REMOVED.
STRTCL--STARTING COLUMN OF CHARS TO BE REMOVED.
BUF   --ARRAY OF CHAR'S TO BE PROCESSED.
*)

VAR CHARSUM : CARDINAL;

BEGIN
  CHARSUM := STRTCL + RMVCNT;
  IF (CHARSUM > (BUF.COUNT+1)) OR (BUF.COUNT < 1) THEN 
    WriteString(" IN RMVCHR; PARAMS DON'T MAKE SENSE: RMVCNT=");
    WriteInt(RMVCNT,0);
    WriteString(', STRTCL=');
    WriteInt(STRTCL,0);
    WriteString(', BUF.COUNT=');
    WriteCard(BUF.COUNT,0);
    WriteLn;
    WriteString(' BUF: ');
    WriteString(BUF.CHARS);
    WriteLn;
    RETURN;
  END(*IF*);  (*END ERROR TRAP *)
(*
  The separate test against BUF.COUNT + 1 is necessary to allow the last
  char's in the buffer to be removed.  In this case, no char's need be
  shifted, only BUF.COUNT need be decremented.
*)
  IF (CHARSUM <= (BUF.COUNT+1)) AND (STRTCL > 0) THEN
(*<V34
    COPYLEFT(ADR(BUF.CHARS[CHARSUM]),ADR(BUF.CHARS[STRTCL]),
                                                        BUF.COUNT+2-CHARSUM);
(* Null terminator is included in this copy operation *)
V34>*)
(*<V40*)
    Delete(BUF.CHARS,STRTCL-1,RMVCNT);  (* Picket fence pblm for STRTCL *)
(*V40>*)
  END(*IF*);
  DEC(BUF.COUNT,RMVCNT);
END RMVCHR;

PROCEDURE ASSIGN2BUF(CHARRAY : ARRAY OF CHAR; VAR BUF : BUFTYP);
(*
******************************* ASSIGN2BUF *********************************
ASSIGN string TO BUFFER.
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

BEGIN
  BUF.COUNT := STRLENFNT(CHARRAY);
  BUF.LENGTH := BUF.COUNT;
  COPYLEFT(ADR(CHARRAY),ADR(BUF.CHARS),BUF.COUNT);
(*
  Null terminator is NOT included in this copy operation because not every
  string is null terminated.
*)
  BUF.CHARS[BUF.COUNT+1] := NULL;
END ASSIGN2BUF;

PROCEDURE GETFNM(PROMPT,NAMDFT,TYPDFT : BUFTYP; VAR FILNAM : BUFTYP); 
(*
********************************** GETFNM *******************************
GET FILE NAME SUBROUTINE.
This routine will get a file name from the terminal, and allow defaults 
for the entire file name and/or the extension.

PROMPT--This is the prompt to be printed to the terminal before the 
      filename is requested.  In the Pascal version, it must have a 
      colon, : , as its last character to operate normally.
NAMDFT--This is the name default string that has the file name that will
      be used if no input name is entered.  This must have the .ext as 
      well as the name part; the typdft will not be appended to the 
      default name.
TYPDFT--is a 4 byte array that is the default type (extension) of the
      file.  If a dot is not present in the input character string,
      this array will be appended to the input string to yield the
      full filename.
FILNAM--is the output filename that is a combination of the input
    string, the type default, and the name default, as is appropriate.

Function value returned will be false if the default name was used, i.e.,
no name was input from terminal.  Function value will be true otherwise.
  If a name is input without an extension, before the default extension is
appended, it is stored in the ROOTNAM buffer.  When no string is input, this
name with the default extension appended is used before the full default name
is.

INPUT FROM GBL VAR'S:  NULL, BLANK.
*) 

CONST 
  DOT = '.'; 
  NAMSIZ = 80;

VAR 
  ERRFLG,DOTFLG : BOOLEAN; 
  C,I,J,RETCOD  : CARDINAL; 
  TMPBUF        : BUFTYP;
  SUM           : INTEGER;

BEGIN 
  REPEAT      (* UNTIL NOT ERRFLG *)
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
      WriteString(PROMPT.CHARS);
      ReadString(TMPBUF.CHARS); 
      WriteLn;
      TRIM(TMPBUF);
      INI1TKN(TMPBUF);
      GETTKNSTR(TOKEN,SUM,RETCOD);
      IF RETCOD = 0 THEN
        MOREFNM := (DELIMCH = BLANK);
        FILNAM := TOKEN;
      ELSE
        FILNAM.COUNT := 0;  (* Just to make sure, but should already be 0. *)
        MOREFNM := FALSE;
      END(*IF*);
    END(*IF*);
    C := SCANBACK(ADR(FILNAM.CHARS),FILNAM.COUNT,DOT,TRUE);
    DOTFLG := C > 0;
  
    IF FILNAM.COUNT = 0 THEN 
(* 
  NULL string.  Use default.
*)
      IF ROOTNAM.CHARS[1] = 0C THEN
(*
    ROOTNAM is NULL, so continue with using the full default name.
*)    
        FILNAM := NAMDFT;
        DOTFLG := TRUE;
      ELSE
        FILNAM := ROOTNAM;
        DOTFLG := FALSE;
      END(*IF*);
    END(*IF*);
    IF NOT DOTFLG THEN 
      ROOTNAM := FILNAM;   (* Store name without extension. *)
(*
  Append type since no type input.
*) 
      IF FILNAM.COUNT+TYPDFT.COUNT > NAMSIZ THEN 
        WriteString('Name too long after appending type.  Try again.');
        WriteLn;
        ERRFLG := TRUE
      ELSE   (* No error *)
        APPENDA2B(TYPDFT,FILNAM);
(* Null terminator is included in this copy operation *)
      END(*IF*);
    END(*IF*); 
  UNTIL NOT ERRFLG; 
END GETFNM; 

BEGIN
  ROOTNAM.CHARS[1] := NULL;
  ROOTNAM.COUNT := 0;
  MOREFNM := FALSE;
END UL2.
