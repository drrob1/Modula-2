IMPLEMENTATION MODULE TOKENIZE;
(*
Copyright (C) 1987  Robert Solomon MD.  All rights reserved.
  These routines collectively implement a very good facility to fetch,
  manipulate, and interpret tokens.

  REVISION HISTORY
  ----------------
  28 MAY 87 -- Added UNGETTKN capability and no longer exported GETCHR and
                 UNGETCHR.
  29 AUG 87 -- Restored exportation of GETCHR and UNGETCHR.
  3 Mar 88 -- Added the ASCZERO declaration and removed the function call
               from the DGT conversion loop.
  31 Mar 88 -- Converted to M2 V3.03.
  1 Sept 88 -- 1. Allowed quoted string to force ALLELSE state.
               2. Changed the method UNGETTKN uses to unget the token.
               3. Added the MULTSIGN and DIVSIGN OP states.
               4. Ran M2CHECK and deleted all unreferenced var's.
               5. Moved the NEGATV check for contigently making SUM < 0 out
                   of the LOOP and deleted the 5 previous statements for all
                   of the different states plus the end-of-line test.
  18 Mar 89 -- Added the GETTKNREAL Procedure.
  20 Mar 89 -- Changed GETOPCODE so that if a multicharacter op code is
                invalid, UNGETCHR is used to put the second char back.
   1 Dec 89 -- Made change in GETTKN that was demonstrated to be necessary
                when the code was ported to the VAX.
   2 Jan 90 -- Changed GETTKNREAL so that a real number may begin with a
                decimal pt.
   9 Nov 90 -- Added GETTKNSTR procedure and DELIMSTATE var.
  27 Dec 90 -- Added GETTKNEOL procedure, originally written for CFNTS.
  25 Jul 93 -- Fixed bug in GETCHR whereby CHRSTATE not set when at EOL,
                and adjusted algorithm of GETTKNSTR.
   6 Jun 95 -- Added FSAARRAY as way to assign FSATYP, and to easily
                modify the FSATYP assignments.
  20 Oct 02 -- Converted to M2 for win32, DOS mode.
  17 May 03 -- First Win32 version.
  30 Jun 03 -- Fixed real tokens so can now again begin w/ decpt, by always writing a leading 0.
  21 Jul 03 -- Fixed bug introduced by above step when a token has leading spaces
   4 Oct 03 -- Fixed bug when neg number is entered using unary minus.
*)

(*  From Obsolete Modules
  FROM Terminal IMPORT ReadString;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard;
  FROM RealConversions IMPORT RealToString, StringToReal;
*)
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
(*  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr; *)
  FROM UTILLIB IMPORT LF,BLANK,NULL,BUFTYP,ISDGT,MAXCARD,TRIM;
  FROM Conversions IMPORT StringToInt,StrToInt,IntToString,IntToStr,StringToCard,StrToCard, 
    CardToString,CardToStr,StrBaseToCard,CardBaseToStr,StringToLong,StrToLong,LongToString, 
    LongToStr,StrBaseToLong,LongBaseToStr;
  FROM RConversions IMPORT RealToString,RealToStringFixed,StringToReal;
(*
  str : ARRAY OF CHAR; VAR INOUT pos : CARDINAL; VAR OUT num : LONGREAL;
    VAR OUT done : BOOLEAN;
   Get a longreal number from a string starting at position pos.
   pos is left pointing to the first character that is not part of
   the number done signifies the success of the conversion.
*)
(*
  FROM Str IMPORT Lows,Compare,Concat,Append,Copy,Slice,Pos,NextPos,CharPos,RCharPos,
    Item,ItemS,Prepend,Insert,Subst,Match,FindSubStr,IntToStr,CardToStr,RealToStr,
    FixRealToStr, StrToInt, StrToCard, StrToReal, StrToC, StrToPas;
*)

  CONST
      TKNMAXSIZ = 80;
      DGTMAXSIZ = 5;
      OPMAXSIZ  = 2;
      POUNDSIGN = '#';  (* 35 *)
      PLUSSIGN  = '+';  (* 43 *)
      COMMA     = ',';  (* 44 *)
      MINUSSIGN = '-';  (* 45 *)
(*      COLON     = ':';  (* 58 *)   *)
      SEMCOL    = ';';  (* 59 *)
      LTSIGN    = '<';  (* 60 *)
      EQUALSIGN = '=';  (* 61 *)
      GTSIGN    = '>';  (* 62 *)
      MULTSIGN  = '*';
      DIVSIGN   = '/';
      ASCZERO   = ORD('0');   (* Modula-2 V 3.03 allows this as a const *)
      SQUOTE    = "'";
      DQUOTE    = '"';

(* NOW DECLARED IN DEFINITION MODULE
  TYPE
  (*
    FSA TYPE IS A TYPE TO DEFINE THE FINITE STATE AUTOMATA ON WHICH THIS
    ALGORITHM IS BASED.  IT IS AN ENUMERATION TYPE.
  *)
    FSATYP = (DELIM, OP, DGT, ALLELSE);

  VAR
    DELIMCH    : CHAR;    (* Exported delimiting character *)
    DELIMSTATE : FSATYP;  (* Exported delimiting character state *)
*)

VAR
(*  ASCZERO : CARDINAL;  No longer a variable, now a const *)
(*
  These variables are only used in tokenizer module.  They are declared here
  to make the variable static so to maintain their values between procedure
  calls.
*)
  CURPOSN,HOLDCURPOSN,PREVPOSN : CARDINAL;
(* Line buffers on which the tokenizing routines operate *)
  TKNBUF,HOLDTKNBUF            : BUFTYP;
  FSAARRAY                     : ARRAY [0..127] OF FSATYP;

PROCEDURE INI3TKN(BUF:BUFTYP);
(*
*************************** INI3TKN *******************************************
INITIALIZE TOKEN.
THE PURPOSE OF THE INITIALIZE TOKEN ROUTINE IS TO INITIALIZE THE
VARIABLES USED BY GETCHR TO BEGIN PROCESSING A NEW LINE.
THE BUFFER ON WHICH THE TOKENIZING RTNS OPERATE IS ALSO INITIALIZED.
CURPOSN IS INITIALIZED TO SKIP OVER THE @@A CHAR'S WHICH
ARE ALWAYS THE FIRST TWO CHAR'S ON THE LINE.

OUTPUT TO GBLVAR'S:  NONE.
*)

BEGIN
    CURPOSN  := 3;
    TKNBUF   := BUF;
    PREVPOSN := 0;
    InitFSAArray;
END INI3TKN;

PROCEDURE INI1TKN(BUF:BUFTYP);
(*
*************************** INI1TKN *******************************************
INITIALIZE TOKEN.
THE PURPOSE OF THE INITIALIZE TOKEN ROUTINE IS TO INITIALIZE THE
VARIABLES USED BY NXTCHR TO BEGIN PROCESSING A NEW LINE.
THE BUFFER ON WHICH THE TOKENIZING RTNS OPERATE IS ALSO INITIALIZED.
CURPOSN IS INITIALIZED TO START AT THE FIRST CHARACTER ON THE LINE.

OUTPUT TO GBLVAR'S:  NONE.
*)

BEGIN
    CURPOSN  := 1;
    TKNBUF   := BUF;
    PREVPOSN := 0;
    InitFSAArray;
END INI1TKN;

PROCEDURE STOTKNPOSN;
(*
****************************** STOTKNPOSN ***********************************
STORE TOKEN POSITION.
THIS ROUTINE WILL STORE THE VALUE OF THE CURPOSN INTO A HOLD VARIABLE FOR
LATER RECALL BY RCLTKNPOSN.  ITS MAIN USE IS BY THE GET ENVIRONMENT PROC'S
SO THAT THE ENVIRONMENT NEED BE PARSED ONLY ONCE.

*)
BEGIN
  IF (CURPOSN < 1) OR (CURPOSN > TKNBUF.COUNT) THEN
    WriteString(' *ERROR*  In STOTKNPOSN and CURPOSN out of range.  It is ');
    WriteCard(CURPOSN,0);
    WriteLn;
    WriteString(' Beginning of TKNBUF is : ');
    WriteString(TKNBUF.CHARS);
    WriteLn;
  END(*IF*);
  HOLDCURPOSN := CURPOSN;
  HOLDTKNBUF := TKNBUF;
END STOTKNPOSN;

PROCEDURE RCLTKNPOSN;
(*
****************************** RCLTKNPOSN **********************************
RECALL TOKEN POSITION.
THIS IS THE INVERSE OF THE STOTKNPOSN PROCEDURE.

*)
BEGIN
  IF (HOLDCURPOSN < 1) OR (HOLDTKNBUF.CHARS[1] = NULL) OR
   (HOLDCURPOSN > HOLDTKNBUF.COUNT) THEN
    WriteString('*ERROR* In RCLTKNPOSN and HOLDCURPOSN out of range.  It is ');
    WriteCard(HOLDCURPOSN,0);
    WriteLn;
    WriteString(' Beginning of TKNBUF is : ');
    WriteString(TKNBUF.CHARS);
    WriteLn;
  END(*IF*);
  CURPOSN := HOLDCURPOSN;
  TKNBUF  := HOLDTKNBUF;
END RCLTKNPOSN;

PROCEDURE GETCHR(VAR CH:CHAR; VAR CHRSTATE:FSATYP; VAR RETCOD:CARDINAL);
(*
**************************** GETCHR *******************************************
THIS IS THE GET CHARACTER ROUTINE.  ITS PURPOSE IS TO GET THE NEXT
CHARACTER FROM INBUF, DETERMINE ITS FSATYP (FINITE STATE AUTOMATA TYPE),
AND RETURN THE UPPER CASE VALUE OF CHAR.  RETCOD VALUE OF ONE MEANS AN END OF
LINE WAS FOUND, TWO MEANS AN ERROR OCCURRED.
NOTE: THE CURPOSN POINTER IS USED BEFORE IT'S INCREMENTED, UNLIKE MOST OF THE
OTHER POINTERS IN THIS PROGRAM.

CURPOSN -- CURRENT POSITION IN INBUF OF @@A LINE.
CH     -- THE CHARACTER TO BE RETURNED.
CHRSTATE-- FINITE STATE AUTOMATON VARIABLE INDICATING THE STATE OF THE
           CHARACTER READ.
RETCOD -- ZERO MEANS NORMAL RETURN.
          ONE MEANS END-OF-TKNBUF FOUND.  IN THIS CASE CH RETURNED IS NULL.

*)

BEGIN
  RETCOD := 0;
  IF CURPOSN > TKNBUF.COUNT THEN
    RETCOD := 1;
    CH := NULL;
    CHRSTATE := DELIM;
    RETURN;
  END(*IF*);
  CH := CAP(TKNBUF.CHARS[CURPOSN]);
(*
  Set state of character and assign to CHRSTATE.
*)
  IF ORD(CH) > 127 THEN
    CHRSTATE := ALLELSE;
  ELSE
    CHRSTATE := FSAARRAY[ORD(CH)];
  END(*IF*);
  INC(CURPOSN);
(*
  IF (CH = BLANK) OR (CH = NULL) OR (CH = SEMCOL) OR (CH = COMMA) THEN
    CHRSTATE := DELIM;
  ELSIF (CH = GTSIGN) OR (CH = LTSIGN) OR (CH = POUNDSIGN) OR
   (CH = EQUALSIGN) OR (CH = PLUSSIGN) OR (CH = MINUSSIGN) OR
   (CH = MULTSIGN) OR (CH = DIVSIGN) THEN
    CHRSTATE := OP;
  ELSIF ISDGT(CH) THEN
    CHRSTATE := DGT;
  ELSE
    CHRSTATE := ALLELSE; (* Alphabetic-mostly state, but allows ctrl chr's *)
  END(*IF*);
*)
END GETCHR;

PROCEDURE UNGETCHR(VAR RETCOD:CARDINAL);
(*
********************************* UNGETCHR ********************************
UNGETCHaracteR.
THIS IS THE ROUTINE THAT WILL ALLOW THE CHARACTER LAST READ TO BE READ
AGAIN BY DECREMENTING THE POINTER INTO TKNBUF, CURPOSN.

RETCOD = 0 MEANS NORMAL RETURN.
RETCOD = 1 MEANS ERROR, LIKE CAN'T UNGETCHR BECAUSE NO CHAR TO UNGET.

*)

BEGIN
  RETCOD := 0;
  IF (CURPOSN < 1) OR (CURPOSN > TKNBUF.COUNT+1) THEN
    WriteString(' *ERROR*  Cannot UNGETCHR because CURPOSN is out of range.');
    WriteString('  It is ');
    WriteCard(CURPOSN,0);
    WriteString('.');
    WriteLn;
    WriteString(' BEGINNING OF TKNBUF: ');
    WriteString(TKNBUF.CHARS);
    WriteLn;
    RETCOD := 1;
    RETURN;
  END(*IF*);
  DEC(CURPOSN);
END UNGETCHR;

PROCEDURE GETOPCODE(VAR TOKEN : BUFTYP; VAR OPCODE, RETCOD : CARDINAL);
(*
*************************** GETOPCODE ****************************************
GET OPCODE.
THIS ROUTINE RECEIVES A TOKEN OF FSATYP OP (MEANING IT IS AN OPERATOR)
AND ANALYZES IT TO DETERMINE AN OPCODE, WHICH IS A CARDINAL FROM 1..11.
THIS IS DONE AFTER THE NECESSARY VALIDITY CHECK OF THE INPUT TOKEN.
THE OPCODE ASSIGNMENTS FOR THE OP TOKENS ARE:
  < is 1                  <= is 2
  > is 3                  >= is 4
  = is 5   == is 5        <> is 6    # is 7
  + is 8                  += is 9
  - is 10                 -= is 11
  * is 12                 *= is 13
  / is 14                 /= is 15

THEIR MEANINGS ARE DERIVED FROM A COMBINATION FROM 3 COMPUTER LANGUAGES:
C, PASCAL, AND MODULA-2, AND SHOULD BE IMMEDIATELY APPARENT.

*)

VAR CH1,CH2 : CHAR;
    RETCOD2 : CARDINAL;
BEGIN
  OPCODE := 0;
  RETCOD := 0;
  CH1 := TOKEN.CHARS[1];
  CH2 := TOKEN.CHARS[2];
  IF (TOKEN.COUNT < 1) OR (TOKEN.COUNT > 2) THEN
    RETCOD := 1;
    RETURN;
  END(*IF*);

  CASE CH1 OF
      LTSIGN : OPCODE := 1;
  |   GTSIGN : OPCODE := 3;
  |   EQUALSIGN : OPCODE := 5;
  |   PLUSSIGN  : OPCODE := 8;
  |   MINUSSIGN : OPCODE := 10;
  |   POUNDSIGN : OPCODE := 7;
  |   MULTSIGN  : OPCODE := 12;
  |   DIVSIGN   : OPCODE := 14;
  ELSE
    RETCOD := 1;
    RETURN;
  END(*CASE*);
  IF TOKEN.COUNT = 1 THEN
    RETURN;
  ELSIF (CH2 = EQUALSIGN) AND (CH1 <> EQUALSIGN) AND (CH1 <> POUNDSIGN) THEN
    INC(OPCODE);
  ELSIF (CH1 = LTSIGN) AND (CH2 = GTSIGN) THEN
    OPCODE := 6;
  ELSE
    UNGETCHR(RETCOD2);
    IF RETCOD2 > 0 THEN
      WriteString(' In GETOPCODE and UNGETCHR failed.');
      WriteLn;
    END(*IF*);
    TOKEN.CHARS[TOKEN.COUNT] := NULL;
    TOKEN.COUNT := 1;
    DELIMCH := CH2;  (* This rtn is called after GETTKN defines DELIMCH *)
    DELIMSTATE := OP;
  END(*IF*);
END GETOPCODE;

PROCEDURE GETTKN(VAR TOKEN:BUFTYP; VAR TKNSTATE:FSATYP;
                 VAR SUM:INTEGER; VAR RETCOD2:CARDINAL);
(*
*************************** GETTKN **************************************

THIS IS THE GET NEXT TOKEN ROUTINE.  A TOKEN IS A STRING OF SYMBOLS THAT
REPRESENT A SIGNIFICANT QUANTITY, LIKE A CHAR STRING REPRESENTING A PARAM
NAME OR A DIGIT STRING REPRESENTING A NUMBER.  EACH TOKEN IS DELIMITED BY THE
DELIMITERS RECOGNIZED BY NXTCHR (COMMA, EQUALS SIGN, OR END-OF-TKNBUF COND).
THIS ROUTINE WILL DETERMINE THE LENGTH OF THE TOKEN AND WHETHER IT IS A
CHARACTER TOKEN OR A NUMBER TOKEN.

TOKEN -- BUFFER HOLDING THE CHARACTERS OF THIS TOKEN.
SUM   -- CONTAINS THE VALUE OF THE NUMBER IN A NUMBER TOKEN, HAS THE
          SUM OF THE ASCII VALUES FOR A CHARACTER TOKEN, OR HAS THE OPCODE
          VALUE FOR AN OP TOKEN.
RETCOD2--RETURN CODE.  0: Normal return; 1: No more tokens on line;
          2: Char token too long; 3: Number token with too many digits;
          4: Cannot UNGETCHR; 5: Currently unused;
          6: Error from GETOPCODE.

*)

VAR NEGATV         : BOOLEAN;
    ORDCH,C,RETCOD : CARDINAL;
    CH             : CHAR;
    CHRSTATE       : FSATYP;
    QUOCHR         : CHAR;   (* Holds the active quote char *)
    QUOFLG         : BOOLEAN;

BEGIN
  QUOCHR := NULL;
  QUOFLG := FALSE;
  PREVPOSN := CURPOSN;
  TKNSTATE := DELIM;
  RETCOD2 := 0;
  SUM := 0;
  TOKEN.COUNT := 0;
  NEGATV := FALSE;
  InitFSAArray;
  LOOP
    GETCHR(CH,CHRSTATE,RETCOD);
    IF RETCOD = 1 THEN
(*
  NO NEXT CHAR.  IF TKNSTATE IS DELIM, THEN GETTKN WAS CALLED WHEN THERE WERE
  NO MORE TOKENS ON LINE.  OTHERWISE IT MEANS THAT WE HAVE FETCHED THE LAST
  TOKEN ON THIS LINE.
*)
      IF TKNSTATE = DELIM THEN RETCOD2 := 1; END(*IF*);
      EXIT;
    END(*IF*);
    ORDCH := ORD(CH);
    IF QUOFLG AND (CH <> NULL) THEN CHRSTATE := ALLELSE; END(*IF*);
    CASE TKNSTATE OF
      DELIM : (* tokenstate *)
        CASE CHRSTATE OF
          DELIM : (* NULL char is a special delimiter because it will
                     immediately cause a return even if there is no token yet,
                     i.e., the token is only delimiters.  This is because of
                     the NULL char is the string terminater for general strings
                     and especially for environment strings, for which this
                     TOKENIZE module was originally written. *)
                   IF CH = NULL THEN EXIT; END(*IF*);
        | OP : TKNSTATE := OP;
               INC(TOKEN.COUNT);
               IF TOKEN.COUNT > TKNMAXSIZ THEN
                 WriteString("OPERATOR TOO LONG, FIRST 80 CHAR'S ARE:  ");
                 TOKEN.CHARS[TOKEN.COUNT] := NULL;
                 WriteString(TOKEN.CHARS);
                 WriteLn;
                 RETCOD2 := 2;
                 DEC(TOKEN.COUNT);
                 EXIT;
               END(*IF*);
               TOKEN.CHARS[TOKEN.COUNT] := CH;
        | DGT : TKNSTATE := DGT;
                INC(TOKEN.COUNT);
                SUM := INT(ORDCH - ASCZERO);
                TOKEN.CHARS[TOKEN.COUNT] := CH;
        | ALLELSE : TKNSTATE := ALLELSE;
                    QUOFLG := (CH = SQUOTE) OR (CH = DQUOTE);
                    IF QUOFLG THEN
                      QUOCHR := CH;
                    ELSE
                      INC(TOKEN.COUNT);
                      IF TOKEN.COUNT > TKNMAXSIZ THEN
                       WriteString(
                               "PARAM NAME TOO LONG, FIRST 80 CHAR'S ARE: ");
                       TOKEN.CHARS[TOKEN.COUNT] := NULL;
                       WriteString(TOKEN.CHARS);
                       WriteLn;
                       RETCOD2 := 2;
                       DEC(TOKEN.COUNT);
                       EXIT;
                      END(*IF*);
                      TOKEN.CHARS[TOKEN.COUNT] := CH;
                      SUM := INT(ORDCH);
                    END(*IF*);
        END(*CASE*);
    | OP : (* tokenstate *)
        CASE CHRSTATE OF
          DELIM : UNGETCHR(RETCOD);
                  IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                  EXIT;
        | OP : INC(TOKEN.COUNT);
               IF TOKEN.COUNT > OPMAXSIZ THEN
(*
                 WriteString("OPERATOR TOO LONG, FIRST 80 CHAR'S ARE: ");
                 TOKEN.CHARS[TOKEN.COUNT] := NULL;
                 WriteString(TOKEN.CHARS);
                 WriteLn;
                 RETCOD2 := 2;
*)
                 DEC(TOKEN.COUNT);
                 UNGETCHR(RETCOD);
                 IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                 EXIT;
               END(*IF*);
               TOKEN.CHARS[TOKEN.COUNT] := CH;
        | DGT : IF (TOKEN.CHARS[TOKEN.COUNT] = '+') OR
                   (TOKEN.CHARS[TOKEN.COUNT] = '-') THEN
                  IF TOKEN.COUNT = 1 THEN
                    IF TOKEN.CHARS[1] = '-' THEN NEGATV := TRUE; END(*IF*);
                    TKNSTATE := DGT;
(* OVERWRITE ARITHMETIC SIGN CHARACTER *)
                    TOKEN.CHARS[TOKEN.COUNT] := CH;
                    SUM := INT(ORDCH - ORD('0'));
                  ELSE   (* TOKEN.COUNT > 1 SO MUST FIRST RETURN OP *)
                    UNGETCHR(RETCOD); (* UNGET THIS DIGIT CHAR *)
                    UNGETCHR(RETCOD); (* THEN UNGET THE ARITH SIGN CHAR *)
                    DEC(TOKEN.COUNT); (* TKN NULL TERMINATED JUST BEFORE RET *)
                    CH := TOKEN.CHARS[TOKEN.COUNT+1]; (* SO DELIMCH CORRECTLY
                                                RETURNS THE ARITH SIGN CHAR *)
                    IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                    EXIT;
                  END(*IF*);
                ELSE
                  UNGETCHR(RETCOD);
                  IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                  EXIT;
                END(*IF*);
        | ALLELSE : UNGETCHR(RETCOD);
                    IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                    EXIT;
        END(*CASE*);
    | DGT : (* tokenstate *)
        CASE CHRSTATE OF
          DELIM : EXIT;
        | OP : UNGETCHR(RETCOD);
               IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
               EXIT;
        | DGT : INC(TOKEN.COUNT);
                IF (SUM > 214748364) OR ((SUM = 214748364) AND (ORDCH > ORD('7'))) THEN
                  RETCOD2 := 3;
                  DEC(TOKEN.COUNT);
                  EXIT;
                END(*IF*);
                TOKEN.CHARS[TOKEN.COUNT] := CH;
                SUM := 10 * SUM + INT(ORDCH - ORD('0'));
        | ALLELSE : UNGETCHR(RETCOD);
                    IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                    EXIT;
        END(*CASE*);
    | ALLELSE : (* tokenstate *)
        CASE CHRSTATE OF
          DELIM :
(*
  Always exit if get a NULL char as a delim.  A quoted string can only
  get here if CH is NULL.
*)
                  EXIT;
        | OP : UNGETCHR(RETCOD);
               IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
               EXIT;
        | DGT : INC(TOKEN.COUNT);
                IF TOKEN.COUNT > TKNMAXSIZ THEN
                  WriteString("PARAM NAME TOO LONG, FIRST 80 CHAR'S ARE: ");
                  TOKEN.CHARS[TOKEN.COUNT] := NULL;
                  WriteString(TOKEN.CHARS);
                  WriteLn;
                  RETCOD2 := 2;
                  DEC(TOKEN.COUNT);
                  EXIT;
                END(*IF*);
                TOKEN.CHARS[TOKEN.COUNT] := CH;
                IF (SUM < 3276) OR ((SUM = 3276) AND (ORDCH <= 7)) THEN
                  SUM := SUM + INT(ORDCH);
                END(*IF*);
        | ALLELSE : IF CH = QUOCHR THEN
                      QUOFLG := FALSE;
                      CHRSTATE := DELIM; (* So that DELIMSTATE will = delim *)
                      EXIT;
                    ELSE
                      INC(TOKEN.COUNT);
                      IF TOKEN.COUNT > TKNMAXSIZ THEN
                        WriteString(
                               "PARAM NAME TOO LONG, FIRST 80 CHAR'S ARE: ");
                        TOKEN.CHARS[TOKEN.COUNT] := NULL;
                        WriteString(TOKEN.CHARS);
                        WriteLn;
                        RETCOD2 := 2;
                        DEC(TOKEN.COUNT);
                        EXIT;
                      END(*IF*);
                      TOKEN.CHARS[TOKEN.COUNT] := CH;
                      IF (SUM < 3276) OR ((SUM = 3276) AND (ORDCH <= 7)) THEN
                        SUM := SUM + INT(ORDCH);
                      END(*IF*);
                    END(*IF*);
        END(*CASE*);
    END(*CASE*);
  END(*LOOP*);
  DELIMCH    := CH;
  DELIMSTATE := CHRSTATE;
  TOKEN.CHARS[TOKEN.COUNT+1] := NULL;
  IF (TKNSTATE = DGT) AND NEGATV THEN SUM := -SUM; END(*IF*);
(*
  For OP tokens, must return the opcode as the sum value.  Do this by
  calling GETOPCODE.
*)
  IF TKNSTATE = OP THEN
    GETOPCODE(TOKEN,C,RETCOD);
    SUM := INT(C);
    IF RETCOD <> 0 THEN RETCOD2 := 6; END(*IF*);
  END(*IF*);
  TOKEN.LENGTH := TOKEN.COUNT;
END GETTKN;

PROCEDURE GETTKNREAL(VAR TOKEN:BUFTYP; VAR TKNSTATE:FSATYP;
              VAR INTVAL:INTEGER; VAR REALVAL:LONGREAL; VAR RETCOD2:CARDINAL);
(*
************************ GETTKNREAL ***************************************
GET ToKeN REAL.
This behaves very similarly to GETTKN, only that all numbers are returned as
reals instead of integers.  Since it uses GETTKN to first see if there is a
dgt token, this rtn can be used instead of GETTKN if a real number token is
possible.  It returns the same codes as GETTKN, with an additional code of 7
if the real token is invalid (the StringToReal conversion failed).

RETCOD2--RETURN CODE.  0: Normal return; 1: No more tokens on line;
          2: Char token too long; 3: Number token with too many digits;
          4: Cannot UNGETCHR; 5: Currently unused;
          6: Error from GETOPCODE; (* No longer used *)
          7: real token invalid (the StringToReal conversion failed).
*)

VAR
    RETCOD,pos   : CARDINAL;
    CH           : CHAR;
    CHRSTATE     : FSATYP;
    TEMP         : BUFTYP;
    OK           : BOOLEAN;
BEGIN
  RETCOD2 := 0;
  REALVAL := 0.;
  GETTKN(TOKEN,TKNSTATE,INTVAL,RETCOD);
  IF (TKNSTATE = ALLELSE) AND (TOKEN.CHARS[1] = '.') AND (ORD(TOKEN.CHARS[2])
              >= ORD('0')) AND (ORD(TOKEN.CHARS[2]) <= ORD('9')) THEN
(*
  Likely have a real number beginning with a decimal point, so fall thru
  to the digit token without returning as if a non-digit token was fetched.
*)
    TKNSTATE := DGT;
  ELSIF TKNSTATE <> DGT THEN
    RETCOD2 := RETCOD;
    RETURN;
  END(*IF*);
(*
  Now must have a digit token.  Add leading 0 so can begin w/ decimal pt.  I think the libary now requires
  a leading digit for the conversion function to work.
*)
  UNGETTKN(RETCOD);
  IF RETCOD > 0 THEN
    WriteString('*Warning*  In GETTKNREAL and UNGETTKN failed.');
    WriteLn;
  END(*IF*);
  PREVPOSN := CURPOSN; (* So that this token can be ungotten as well *)
  TOKEN.COUNT := 1;
  TOKEN.CHARS[1] := '0';
  TOKEN.CHARS[2] := NULL;
  LOOP
    GETCHR(CH,CHRSTATE,RETCOD);
    IF RETCOD > 0 THEN EXIT; END(*IF*);
    CASE CHRSTATE OF
    DELIM : IF TOKEN.COUNT > 1 THEN EXIT END(*IF*); (* ignore leading delim *)
    | OP  : IF ((CH <> '+') AND (CH <> '-')) OR
              ((TOKEN.COUNT > 1) AND (TOKEN.CHARS[TOKEN.COUNT] <> 'E')) THEN
              UNGETCHR(RETCOD);
              EXIT;
            END(*IF*);
(*            INC(TOKEN.COUNT); overwrite leading 0 by not incr counter *)
            TOKEN.CHARS[TOKEN.COUNT] := CH;
    | DGT : INC(TOKEN.COUNT);
            TOKEN.CHARS[TOKEN.COUNT] := CH;
    | ALLELSE : IF (CH <> '.') AND (CH <> 'E') THEN
                  UNGETCHR(RETCOD);
                  EXIT;
                END(*IF*);
                INC(TOKEN.COUNT);
                TOKEN.CHARS[TOKEN.COUNT] := CH;
    END(*CASE*);
  END(*LOOP*);
  DELIMCH    := CH;
  DELIMSTATE := CHRSTATE;
  TOKEN.CHARS[TOKEN.COUNT+1] := NULL;
  TOKEN.LENGTH := TOKEN.COUNT;
  pos := 0;
  StringToReal(TOKEN.CHARS,pos,REALVAL,OK);
(*  REALVAL  :=  StrToReal(TOKEN.CHARS,OK);  *)
  IF NOT OK THEN RETCOD2 := 7; END(*IF*);
END GETTKNREAL;

PROCEDURE GETTKNSTR(VAR TOKEN:BUFTYP; VAR INTVAL:INTEGER;
                    VAR RETCOD2:CARDINAL);
(*
  GET ToKeN STRing.
  Will get tokens only stopping at a delimiter, unlike GETTKN, and all
  tokens are of state ALLELSE, so no state need be returned.  First will
  call GETTKN and only if a non all else token is returned or if
  DELIMSTATE is not a delimiter does this procedure need to do anything
  different.  Especially useful for getting filenames that begin with digits
  or have hyphens in the name.
    Same return codes as from GETTKN.  The quoted string option of GETTKN
  can be used to behave similarily, but this frees the user from having
  to remember to quote such strings himself.
*)

VAR
    RETCOD,C          : CARDINAL;
    CH                : CHAR;
    CHRSTATE,TKNSTATE : FSATYP;
    TEMP              : BUFTYP;
    OK                : BOOLEAN;
BEGIN
  FOR C := 48 TO 57 DO
    FSAARRAY[C] := ALLELSE;
  END(*FOR*);
  FSAARRAY[35] := ALLELSE;    (* poundsign *)
  FSAARRAY[42] := ALLELSE;    (* multsign *)
  FSAARRAY[43] := ALLELSE;    (* plussign *)
  FSAARRAY[45] := ALLELSE;    (* minussign *)
  FSAARRAY[47] := ALLELSE;    (* divsign *)
  FSAARRAY[60] := ALLELSE;      (* LTSIGN *)
  FSAARRAY[61] := ALLELSE;      (* EQUAL *)
  FSAARRAY[62] := ALLELSE;      (* GTSIGN *)
  GETTKN(TOKEN,TKNSTATE,INTVAL,RETCOD2);
(* Can't test RETCOD2 because number token error does not apply here *)
  IF (TKNSTATE=DELIM) OR ((TKNSTATE=ALLELSE) AND (DELIMSTATE=DELIM)) THEN
    RETURN;
  END(*IF*);
(*
  Now must do special function of this proc.
  Continue COUNT & CHARS fields as left off from GETTKN call.
  As of 6/95 this should always return a tknstate of allelse, so return.
*)
  RETURN;
  WITH TOKEN DO
    LOOP
      GETCHR(CH,CHRSTATE,RETCOD);
      IF (RETCOD > 0) OR ((CHRSTATE=DELIM) AND (COUNT > 0)) THEN
        EXIT;  (* Ignore leading delims *)
      END(*IF*);
      INC(COUNT);
      CHARS[COUNT] := CH;
      INC(INTVAL,ORD(CH));
    END(*LOOP*);
    DELIMCH        := CH;
    DELIMSTATE     := CHRSTATE;
    CHARS[COUNT+1] := NULL;
    LENGTH         := COUNT;
  END(*WITH*);
END GETTKNSTR;

PROCEDURE GETTKNEOL(VAR TOKEN : BUFTYP; VAR RETCOD : CARDINAL);
(*
GET ToKeN to EndOfLine.
This will build a token that consists of every character left on the line.
That is, it only stops at the end of line.
The TRIM procedure is used to set the COUNT and LENGTH fields.  This is
the only TOKENIZE procedure that uses it.
*)
VAR
  RETCOD2,C : CARDINAL;
  CH        : CHAR;
  CHRSTATE  : FSATYP;

BEGIN
  C      := 1;
  RETCOD := 0;
  PREVPOSN := CURPOSN; (* So this tkn can be ungotten as well *)
  WITH TOKEN DO
    LOOP
      GETCHR(CH,CHRSTATE,RETCOD2);
      IF RETCOD2 > 0 THEN EXIT; END(*IF*);  (* Only exit loop at EOL *)
      CHARS[C] := CH;
      INC(C);
    END(*LOOP*);
    CHARS[C] := NULL;  (*
                          Take advantage of the post incremented pointer.
                          Must do this assignment so that if a new string
                          put into this variable
                          is shorter than the previous string, TRIM will use
                          the correct (shorter) length.  Even the empty
                          string will be set correctly with the post incr ptr.
                       *)
    TRIM(TOKEN);
    IF COUNT = 0 THEN RETCOD := 1 END(*IF*);
  END(*WITH*);
END GETTKNEOL;

PROCEDURE UNGETTKN(VAR RETCOD : CARDINAL);
(*
************************************** UNGETTKN *****************************
UNGET TOKEN ROUTINE.
This routine will unget the last token fetched.  It does this by restoring
the previous value of POSN, held in PREVPOSN.  Only the last token fetched
can be ungotten, so PREVPOSN is reset after use.  If PREVPOSN contains this
as its value, then the unget operation will fail with a non-zero return
code.  if the operation is successful, then the return code will be zero.
*)

BEGIN
  RETCOD := 0;
  IF (CURPOSN <= PREVPOSN) OR (PREVPOSN < 1) THEN
    WriteString(' *ERROR*  Cannot UNGETTKN because CURPOSN is too small or ');
    WriteLn;
    WriteString('  there is no last token to UNGET.  CURPOSN = ');
    WriteCard(CURPOSN,0);
    WriteString(', PREVPOSN = ');
    WriteCard(PREVPOSN,0);
    WriteLn;
    WriteString(' BEGINNING OF TKNBUF: ');
    WriteString(TKNBUF.CHARS);
    WriteLn;
    RETCOD := 1;
    RETURN;
  END(*IF*);     (* End error trap *)

  CURPOSN  := PREVPOSN;
  PREVPOSN := 0;
END UNGETTKN;

PROCEDURE InitFSAArray;
VAR C : CARDINAL;
BEGIN
  FOR C := 1 TO 127 DO
    FSAARRAY[C] := ALLELSE;
  END(*FOR*);
  FOR C := 48 TO 57 DO
    FSAARRAY[C] := DGT;
  END(*FOR*);
  FSAARRAY[0] := DELIM;
  FSAARRAY[32] := DELIM; (* space *)
  FSAARRAY[44] := DELIM; (* comma *)
  FSAARRAY[59] := DELIM; (* semcol *)
  FSAARRAY[35] := OP;    (* poundsign *)
  FSAARRAY[42] := OP;    (* multsign *)
  FSAARRAY[43] := OP;    (* plussign *)
  FSAARRAY[45] := OP;    (* minussign *)
  FSAARRAY[47] := OP;    (* divsign *)
  FSAARRAY[60] := OP;      (* LTSIGN *)
  FSAARRAY[61] := OP;      (* EQUAL *)
  FSAARRAY[62] := OP;      (* GTSIGN *)
END InitFSAArray;

BEGIN  (* Initialize static variables that need it *)
  HOLDCURPOSN := 0;
  HOLDTKNBUF.CHARS[1] := NULL;
(*  ASCZERO := ORD('0');  no longer a var, now a const *)
  InitFSAArray;
END TOKENIZE.
