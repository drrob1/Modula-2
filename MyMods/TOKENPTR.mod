IMPLEMENTATION MODULE TOKENPTR;
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
  30 Jun 03 -- Fixed real tokens so can now again begin w/ decpt, by always writing a
                leading 0.
  21 Jul 03 -- Fixed bug introduced by above step when a token has leading spaces
   4 Oct 03 -- Fixed bug when neg number is entered using unary minus.
  14 Jul 04 -- Taken from TOKENIZE module and made to use pointers so it is re-entrant.
  15 Jul 04 -- GetTknReal not working properly when exp is neg.  So changed way leading
                zero handled.
   1 Aug 04 -- Had to increase tokensizemax for use w/ Citibank file.
   3 Mar 05 -- Changed GetTknReal case chrstate delim to be >0 instead of >1.  Should
                fix bug.
  22 Jul 06 -- Diverging code for these token modules.  This module will accept % at end of
                DGT token and return entered #/100.  Only for GetTknReal procedure.  This
                module will leave % state as ALLELSE.  Noticed that this module does not
                have the code to allow hex entry of numbers.  That's only in TKNRTNS.
   9 Feb 09 -- To GetOpCode I added recognition of </ to process HTML end operator and added
                GETHTMLKEYWORD(htmlkeyword);
  20 Oct 11 -- Added underscore '_' as unary minus.  FSA state is dgt.
   2 Apr 13 -- I think I fixed a memory like in that the init routines used NEW but I never called
                DISPOSE.  I now call DISPOSE in GETCHR after the last character has been read.
                Looks like I'm wrong.  There is a DISPOSE call in GETTKN.  I'll remove it
                from GETCHR.
   4 Apr 13 -- But in the testing here, I uncovered a bug in GETTKN in which a pointer was being
                accidentally incremented twice.  This bug was introduced when I added the _ processing.
                But it looks like it took me 1.5 yrs to discover it.
   5 Apr 13 -- Fixed 2 bugs in GETTKNSTR routine.  The 2nd one was that a RETURN never was deleted when
                I made the chages to accomodate the _ character.  Should have more thoroughly tested.
                These bugs were not in TNKRTNS.
   8 Apr 13 -- Changed to only get a new pointer in the INI routines if the current one is NIL.
  21 Apr 13 -- A pointer type has random data before the call to NEW.  I will initialize to NIL myself
                in the calling routine.
  14 Oct 16 -- I changed the NIL assignment line to be more direct.  Type problems have been fixed.
  25 Apr 20 -- Time to use LONGINT type instead of INTEGER.  A longint range is +/- 4.6E18, and longcard max is 9.2E18.
*)

  FROM SYSTEM IMPORT ADDRESS,ADR,WORD,CAST;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  IMPORT Terminal,MiscM2;
  FROM MiscM2 IMPORT ReadString,WriteCard;
  FROM Terminal IMPORT Read, WriteString, WriteLn, Write, ReadChar, Reset;
  IMPORT Strings, MemUtils;
  IMPORT WholeStr, LongStr, LongConv;
  IMPORT LongMath;
  IMPORT ASCII;
(*
{{{
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Str IMPORT Lows,Compare,Concat,Append,Copy,Slice,Pos,NextPos,CharPos,RCharPos,
    Item,ItemS,Prepend,Insert,Subst,Match,FindSubStr,IntToStr,CardToStr,RealToStr,
    FixRealToStr, StrToInt, StrToCard, StrToReal, StrToC, StrToPas;
}}}
*)
  FROM UTILLIB IMPORT LF,BLANK,NULL,STRTYP,BUFTYP,ISDGT,MAXCARD,TRIM,INSERTAin2B,ASSIGN2BUF;
  FROM Conversions IMPORT StringToInt,StrToInt,IntToString,IntToStr,StringToCard,StrToCard,
    CardToString,CardToStr,StrBaseToCard,CardBaseToStr,StringToLong,StrToLong,LongToString,
    LongToStr,StrBaseToLong,LongBaseToStr;
  FROM RConversions IMPORT RealToString,RealToStringFixed,StringToReal;
  FROM LongStr IMPORT StrToReal,RealToFloat,RealToEng,RealToFixed,RealToStr,ConvResults;

  CONST
      TKNMAXSIZ = 200;
      DGTMAXSIZ = 18;  (* LONGINT *)
      MaxLongIntVal = 4611686018427388000;  (* Close enough to max longint. *)
      NearMaxLongInt = 461168601842738800;  (* One order of magnitude less than MaxLongIntVal *)

      OPMAXSIZ  = 2;
      POUNDSIGN = '#';  (* 35 *)
      PLUSSIGN  = '+';  (* 43 *)
      COMMA     = ',';  (* 44 *)
      MINUSSIGN = '-';  (* 45 *)
      COLON     = ':';  (* 58 *)
      SEMCOL    = ';';  (* 59 *)
      LTSIGN    = '<';  (* 60 *)
      EQUALSIGN = '=';  (* 61 *)
      GTSIGN    = '>';  (* 62 *)
      MULTSIGN  = '*';
      DIVSIGN   = '/';
      SQUOTE    = "'";
      DQUOTE    = '"';
      DECPT     = '.';
      PERCNT    = '%';
      unaryminus= '_';

(* DECLARED IN DEFINITION MODULE
{{{
  TYPE

    FSA TYPE IS A TYPE TO DEFINE THE FINITE STATE AUTOMATA ON WHICH THIS
    ALGORITHM IS BASED.  IT IS AN ENUMERATION TYPE.

    FSATYP = (DELIM, OP, DGT, ALLELSE);
        TOKENTYP = RECORD
                TKNBUF, HOLDTKNBUF : BUFTYP;
                CURPOSN,HOLDCURPOSN,PREVPOSN : CARDINAL;
        END /* RECORD */ ;
        TKNPTRTYP = POINTER TO TOKENTYP;
VAR
    DELIMCH    : CHAR;    /* Exported delimiting character */
    DELIMSTATE : FSATYP;  /* Exported delimiting character state */
}}}
*)

  VAR
    FSAARRAY   : ARRAY [0..127] OF FSATYP;
    BufOfZero  : BUFTYP;
    ASCZERO    : LONGINT = INT('0');  (* 4/26/2020 1:45:13 PM.  Used to be a const, but could not force it to be a LONGINT. *)

PROCEDURE INI3TKN(VAR tpv : TKNPTRTYP; BUF:BUFTYP);
(*
*************************** INI3TKN *******************************************
INITIALIZE TOKEN.
THE PURPOSE OF THE INITIALIZE TOKEN ROUTINE IS TO INITIALIZE THE
VARIABLES USED BY GETCHR TO BEGIN PROCESSING A NEW LINE.
THE BUFFER ON WHICH THE TOKENIZING RTNS OPERATE IS ALSO INITIALIZED.
CURPOSN IS INITIALIZED TO SKIP OVER THE @@A CHAR'S WHICH
ARE ALWAYS THE FIRST TWO CHAR'S ON THE LINE.
tpv is a token pointer variable.
OUTPUT TO GBLVAR'S:  NONE.
*)

BEGIN
(*      IF tpv = CAST(TKNPTRTYP, NIL) THEN NEW(tpv); END;   Old syntax *)
  IF tpv = NIL THEN NEW(tpv); END;
  WITH tpv^ DO
    CURPOSN  := 3;
    TKNBUF   := BUF;
    PREVPOSN := 0;
    HOLDCURPOSN := 0;
    HOLDTKNBUF.CHARS[1] := NULL;
  END (*with*);
  InitFSAArray;
END INI3TKN;

PROCEDURE INI1TKN(VAR tpv : TKNPTRTYP; BUF:BUFTYP);
(*
*************************** INI1TKN *******************************************
INITIALIZE TOKEN.
THE PURPOSE OF THE INITIALIZE TOKEN ROUTINE IS TO INITIALIZE THE
VARIABLES USED BY NXTCHR TO BEGIN PROCESSING A NEW LINE.
THE BUFFER ON WHICH THE TOKENIZING RTNS OPERATE IS ALSO INITIALIZED.
CURPOSN IS INITIALIZED TO START AT THE FIRST CHARACTER ON THE LINE.
tpv is a token pointer variable.
OUTPUT TO GBLVAR'S:  NONE.
*)

BEGIN
(*   IF tpv = CAST(TKNPTRTYP, NIL) THEN   Old syntax *)
  IF tpv =  NIL THEN
    NEW(tpv);
  END;
  WITH tpv^ DO
    CURPOSN  := 1;
    TKNBUF   := BUF;
    PREVPOSN := 0;
    HOLDCURPOSN := 0;
    HOLDTKNBUF.CHARS[1] := NULL;
  END (*with*);
  InitFSAArray;
END INI1TKN;

PROCEDURE INITKN(VAR tpv : TKNPTRTYP; BUF : BUFTYP);
BEGIN
  INI1TKN(tpv, BUF);
END INITKN;

PROCEDURE NewToken(str : STRTYP) : TKNPTRTYP; (* Using a Go idiom *)
VAR
  tpv : TKNPTRTYP;
  BUF : BUFTYP;

BEGIN
  NEW(tpv); (* I don't see the need to test against a NIL pointer now.  I guess this is Go's influence on me now. *)
  ASSIGN2BUF(str, BUF);
  TRIM(BUF);

  WITH tpv^ DO
    CURPOSN  := 1;
    TKNBUF   := BUF;
    PREVPOSN := 0;
    HOLDCURPOSN := 0;
    HOLDTKNBUF.CHARS[1] := NULL;
  END (*with*);
  InitFSAArray;
  RETURN tpv;
END NewToken;

PROCEDURE STOTKNPOSN(VAR tpv : TKNPTRTYP);
(*
****************************** STOTKNPOSN ***********************************
STORE TOKEN POSITION.
THIS ROUTINE WILL STORE THE VALUE OF THE CURPOSN INTO A HOLD VARIABLE FOR
LATER RECALL BY RCLTKNPOSN.  ITS MAIN USE IS BY THE GET ENVIRONMENT PROC'S
SO THAT THE ENVIRONMENT NEED BE PARSED ONLY ONCE.

*)
BEGIN
    WITH tpv^ DO
    IF (CURPOSN < 1) OR (CURPOSN > TKNBUF.COUNT) THEN
      WriteString(' *ERROR*  In STOTKNPOSN and CURPOSN out of range.  It is ');
      WriteCard(CURPOSN);
      WriteLn;
      WriteString(' Beginning of TKNBUF is : ');
      WriteString(TKNBUF.CHARS);
      WriteLn;
    END(*IF*);
    HOLDCURPOSN := CURPOSN;
    HOLDTKNBUF := TKNBUF;
  END (*with*);
END STOTKNPOSN;

PROCEDURE RCLTKNPOSN(VAR tpv : TKNPTRTYP);
(*
****************************** RCLTKNPOSN **********************************
RECALL TOKEN POSITION.
THIS IS THE INVERSE OF THE STOTKNPOSN PROCEDURE.

*)
BEGIN
    WITH tpv^ DO
    IF (HOLDCURPOSN < 1) OR (HOLDTKNBUF.CHARS[1] = NULL) OR
     (HOLDCURPOSN > HOLDTKNBUF.COUNT) THEN
      WriteString('*ERROR* In RCLTKNPOSN and HOLDCURPOSN out of range.  It is ');
      WriteCard(HOLDCURPOSN);
      WriteLn;
      WriteString(' Beginning of TKNBUF is : ');
      WriteString(TKNBUF.CHARS);
      WriteLn;
    END(*IF*);
    CURPOSN := HOLDCURPOSN;
    TKNBUF  := HOLDTKNBUF;
  END (*with*);
END RCLTKNPOSN;

PROCEDURE GETCHR(VAR tpv:TKNPTRTYP; VAR CH:CHAR; VAR CHRSTATE:FSATYP; VAR RETCOD:CARDINAL);
(*
**************************** GETCHR *******************************************
THIS IS THE GET CHARACTER ROUTINE.  ITS PURPOSE IS TO GET THE NEXT
CHARACTER FROM INBUF, DETERMINE ITS FSATYP (FINITE STATE AUTOMATA TYPE),
AND RETURN THE UPPER CASE VALUE OF CHAR.  RETCOD VALUE OF ONE MEANS AN END OF
LINE WAS FOUND, TWO MEANS AN ERROR OCCURRED.
NOTE: THE CURPOSN POINTER IS USED BEFORE IT'S INCREMENTED, UNLIKE MOST OF THE
OTHER POINTERS IN THIS PROGRAM.
CURPOSN -- CURRENT POSITION IN INBUF OF @@A LINE.
CH      -- THE CHARACTER TO BE RETURNED.
CHRSTATE-- FINITE STATE AUTOMATON VARIABLE INDICATING THE STATE OF THE
           CHARACTER READ.
RETCOD  -- ZERO MEANS NORMAL RETURN.
           ONE MEANS END-OF-TKNBUF FOUND.  IN THIS CASE CH RETURNED IS NULL.
*)
BEGIN
    WITH tpv^ DO
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
  END (*with*);
END GETCHR;

PROCEDURE UNGETCHR(VAR tpv:TKNPTRTYP; VAR RETCOD:CARDINAL);
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
  WITH tpv^ DO
    IF (CURPOSN < 1) OR (CURPOSN > TKNBUF.COUNT+1) THEN
      WriteString(' *ERROR*  Cannot UNGETCHR because CURPOSN is out of range.');
      WriteString('  It is ');
      WriteCard(CURPOSN);
      WriteString('.');
      WriteLn;
      WriteString(' BEGINNING OF TKNBUF: ');
      WriteString(TKNBUF.CHARS);
      WriteLn;
      RETCOD := 1;
      RETURN;
    END(*IF*);
    DEC(CURPOSN);
  END(*with*);
END UNGETCHR;

PROCEDURE GETOPCODE(VAR tpv:TKNPTRTYP; VAR TOKEN : BUFTYP; VAR OPCODE, RETCOD : CARDINAL);
(*
*************************** GETOPCODE ****************************************
GET OPCODE.
THIS ROUTINE RECEIVES A TOKEN OF FSATYP OP (MEANING IT IS AN OPERATOR)
AND ANALYZES IT TO DETERMINE AN OPCODE, WHICH IS A CARDINAL FROM 1..15.
THIS IS DONE AFTER THE NECESSARY VALIDITY CHECK OF THE INPUT TOKEN.
THE OPCODE ASSIGNMENTS FOR THE OP TOKENS ARE:
  < is 1                  <= is 2
  > is 3                  >= is 4
  = is 5   == is 5        <> is 6    # is 7
  + is 8                  += is 9
  - is 10                 -= is 11
  * is 12                 *= is 13
  / is 14                 /= is 15
                          </ is 20

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
    ELSIF (CH1 = LTSIGN) AND (CH2 = DIVSIGN) THEN
        OPCODE := 20;
    ELSIF (CH2 = EQUALSIGN) AND (CH1 <> EQUALSIGN) AND (CH1 <> POUNDSIGN) THEN
      INC(OPCODE);
    ELSIF (CH1 = LTSIGN) AND (CH2 = GTSIGN) THEN
      OPCODE := 6;
    ELSE
      UNGETCHR(tpv,RETCOD2);
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

PROCEDURE GETTKN(VAR tpv : TKNPTRTYP; VAR TOKEN:BUFTYP; VAR TKNSTATE:FSATYP; VAR SUM:LONGINT; VAR RETCOD2:CARDINAL);
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
    C,RETCOD       : CARDINAL;
    ORDCH          : LONGINT;
    CH             : CHAR;
    CHRSTATE       : FSATYP;
    QUOCHR         : CHAR;   (* Holds the active quote char *)
    QUOFLG         : BOOLEAN;

BEGIN
  QUOCHR := NULL;
  QUOFLG := FALSE;
  WITH tpv^ DO
    PREVPOSN := CURPOSN;
    TKNSTATE := DELIM;
    RETCOD2 := 0;
    SUM := 0;
    TOKEN.COUNT := 0;
    NEGATV := FALSE;
    InitFSAArray;
    LOOP
      GETCHR(tpv,CH,CHRSTATE,RETCOD);
      IF RETCOD = 1 THEN
(*
  NO NEXT CHAR.  IF TKNSTATE IS DELIM, THEN GETTKN WAS CALLED WHEN THERE WERE
  NO MORE TOKENS ON LINE.  OTHERWISE IT MEANS THAT WE HAVE FETCHED THE LAST
  TOKEN ON THIS LINE.
*)
        IF TKNSTATE = DELIM THEN
          RETCOD2 := 1;
          DISPOSE(tpv);
          tpv := NIL;
(*          tpv := CAST(TKNPTRTYP, NIL);  Old syntax.  Changed 10/14/16 *)
        END(*IF*);
        EXIT;
      END(*IF*);
      ORDCH := INT(CH);
      IF QUOFLG AND (CH <> NULL) THEN CHRSTATE := ALLELSE; END(*IF*);
      CASE TKNSTATE OF
        DELIM : (* Tokenstate *)
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
                  IF CH = unaryminus THEN ORDCH := ASCZERO;  END;
                  INC(TOKEN.COUNT);
                  (* SUM := VAL(LONGINT,(ORDCH - ASCZERO));  LONG is not defined.  How odd *)
                  SUM := ORDCH - ASCZERO;
                  TOKEN.CHARS[TOKEN.COUNT] := CH;
          | ALLELSE : TKNSTATE := ALLELSE;
                      QUOFLG := (CH = SQUOTE) OR (CH = DQUOTE);
                      IF QUOFLG THEN
                        QUOCHR := CH;
                      ELSE
                        INC(TOKEN.COUNT);
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
                      SUM := INT(ORDCH);
                      END(*IF*);
          END(*CASE*);
      | OP : (* Tokenstate *)
          CASE CHRSTATE OF
            DELIM : UNGETCHR(tpv,RETCOD);
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
                   UNGETCHR(tpv,RETCOD);
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
                      (* SUM := INT(ORDCH - ORD('0')); *)
                      SUM := ORDCH - ASCZERO;
                    ELSE   (* TOKEN.COUNT > 1 SO MUST FIRST RETURN OP *)
                      UNGETCHR(tpv,RETCOD); (* UNGET THIS DIGIT CHAR *)
                      UNGETCHR(tpv,RETCOD); (* THEN UNGET THE ARITH SIGN CHAR *)
                      DEC(TOKEN.COUNT); (* TKN NULL TERMINATED JUST BEFORE RET *)
                      CH := TOKEN.CHARS[TOKEN.COUNT+1]; (* SO DELIMCH CORRECTLY RETURNS THE ARITH SIGN CHAR *)
                      IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                      EXIT;
                    END(*IF*);
                  ELSE
                    UNGETCHR(tpv,RETCOD);
                    IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                    EXIT;
                  END(*IF*);
          | ALLELSE : UNGETCHR(tpv,RETCOD);
                      IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                      EXIT;
          END(*CASE*);
      | DGT : (* Tokenstate *)
          CASE CHRSTATE OF
            DELIM : EXIT;
          | OP : UNGETCHR(tpv,RETCOD);
                 IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                 EXIT;
          | DGT :
                  IF TOKEN.CHARS[1] = unaryminus THEN
 (* Overwrite unaryminus char in this string by not incr pointer *)
                        NEGATV := TRUE;
                  ELSE
                    INC(TOKEN.COUNT);
                  END(*IF*);
                                 (*  BUG ?!              INC(TOKEN.COUNT);  leading to double incremented because of ELSE above this *)
                  (* IF (SUM > 214748364) OR ((SUM = 214748364) AND (ORDCH > ORD('7'))) THEN  Old version of this statement *)
                  IF SUM > NearMaxLongInt THEN   (* SUM is not made negative until the end of this proc. *)
                    RETCOD2 := 3;
                    DEC(TOKEN.COUNT);
                    EXIT;
                  END(*IF*);
                  TOKEN.CHARS[TOKEN.COUNT] := CH;
                  (*SUM := 10 * SUM + VAL(LONGINT,(ORDCH - ORD('0')));  I don't think I need VAL here *)
                  SUM := 10 * SUM + ORDCH - ASCZERO;
          | ALLELSE : UNGETCHR(tpv,RETCOD);
                      IF RETCOD <> 0 THEN RETCOD2 := 4; END(*IF*);
                      EXIT;
          END(*CASE*);
      | ALLELSE : (* Tokenstate *)
          CASE CHRSTATE OF
            DELIM :
(*
  Always exit if get a NULL char as a delim.  A quoted string can only get here if CH is NULL.
*)
                    EXIT;
          | OP : UNGETCHR(tpv,RETCOD);
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
                  (* IF (SUM < 3276) OR ((SUM = 3276) AND (ORDCH <= 7)) THEN looks like a holdover from 16 bit days. *)
                  IF SUM < MaxLongIntVal THEN
                    SUM := SUM + ORDCH;              (* VAL(LONGINT,(ORDCH)); *)
                  END(*IF*);
          | ALLELSE :
                  IF CH = QUOCHR THEN
                      QUOFLG := FALSE;
                      CHRSTATE := DELIM; (* So that DELIMSTATE will = delim *)
                      EXIT;
                  ELSE
                      INC(TOKEN.COUNT);
                      IF TOKEN.COUNT > TKNMAXSIZ THEN
                        (*  Commented out 4/25/2020, so that the calling module can handle the error w/o output from here, which is superfluous.
                        WriteString("PARAM NAME TOO LONG, FIRST 80 CHAR'S ARE: ");
                        TOKEN.CHARS[TOKEN.COUNT] := NULL;
                        WriteString(TOKEN.CHARS);
                        WriteLn;
                        *)
                        RETCOD2 := 2;
                        DEC(TOKEN.COUNT);
                        EXIT;
                      END(*IF*);
                      TOKEN.CHARS[TOKEN.COUNT] := CH;
                      (* IF (SUM < 3276) OR ((SUM = 3276) AND (ORDCH <= 7)) THEN   old version of this condition*)
                      IF SUM < MaxLongIntVal THEN
                        SUM := SUM + ORDCH;                           (* VAL(LONGINT,(ORDCH)); *)
                      END(*IF*);
                  END(*IF*);
          END(*CASE chrstate*);
      END(*CASE tokenstate*);
    END(*LOOP*);
    DELIMCH    := CH;
    DELIMSTATE := CHRSTATE;
    TOKEN.CHARS[TOKEN.COUNT+1] := NULL;
    IF (TKNSTATE = DGT) AND NEGATV THEN SUM := -SUM; END(*IF*);
(*
  For OP tokens, must return the opcode as the sum value.  Do this by calling GETOPCODE.
*)
    IF TKNSTATE = OP THEN
      GETOPCODE(tpv,TOKEN,C,RETCOD);
      SUM := INT(C);
      IF RETCOD <> 0 THEN RETCOD2 := 6; END(*IF*);
    END(*IF*);
    TOKEN.LENGTH := TOKEN.COUNT;
  END(*with*);
END GETTKN;

PROCEDURE GETTKNREAL(VAR tpv : TKNPTRTYP;  VAR TOKEN:BUFTYP; VAR TKNSTATE:FSATYP; VAR INTVAL:LONGINT; VAR REALVAL:LONGREAL;
                                                                                                      VAR RETCOD2:CARDINAL);
(*
************************ GETTKNREAL ***************************************
GET ToKeN REAL.
This behaves very similarly to GETTKN, only that all numbers are returned as
reals instead of integers.  Since it uses GETTKN to first see if there is a
dgt token, this rtn can be used instead of GETTKN if a real number token is
possible.  It returns the same codes as GETTKN, with an additional code of 7
if the real token is invalid (the StringToReal conversion failed).
     Added % operation.  The str2real conversion routine ignores the % sign so
I can leave it in the string and handle it after conversion.

RETCOD2--RETURN CODE.  0: Normal return; 1: No more tokens on line;
          2: Char token too long; 3: Number token with too many digits;
          4: Cannot UNGETCHR; 5: Currently unused;
          6: Error from GETOPCODE; /* No longer used */
          7: real token invalid (the StringToReal conversion failed).
*)

VAR
    RETCOD,pos   : CARDINAL;
    CH           : CHAR;
    CHRSTATE     : FSATYP;
    TEMP         : BUFTYP;
    OK,IsNeg     : BOOLEAN;
(*    convresults  : ConvResults; *)
BEGIN
  RETCOD2 := 0;
  REALVAL := 0.;
  WITH tpv^ DO
    GETTKN(tpv,TOKEN,TKNSTATE,INTVAL,RETCOD);
    IF (TKNSTATE = ALLELSE) AND (TOKEN.CHARS[1] = '.') AND (ORD(TOKEN.CHARS[2]) >= ORD('0')) AND (ORD(TOKEN.CHARS[2]) <= ORD('9')) THEN
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
    UNGETTKN(tpv,RETCOD);
    IF RETCOD > 0 THEN
      (*  Commented out 4/25/2020 12:30:38 PM so that the return code is all the error handling this rtn needs.  I don't want it to to I/O also.
          This block should have always returned as an error.  I just caught that now, at the time of rewriting the code for LONGINTs.
      WriteString('*Warning*  In GETTKNREAL and UNGETTKN failed.');
      WriteLn;
      *)
      RETCOD2 := RETCOD;
      RETURN;
    END(*IF*);
    PREVPOSN := CURPOSN; (* So that this token can be ungotten as well *)
    TOKEN.COUNT := 0;
(*    TOKEN.CHARS[1] := '0'; Now handled at bottom of proc *)
    TOKEN.CHARS[1] := NULL;
    LOOP
      GETCHR(tpv,CH,CHRSTATE,RETCOD);
      IF RETCOD > 0 THEN EXIT; END(*IF*);
      CASE CHRSTATE OF
      DELIM : IF TOKEN.COUNT > 0 THEN EXIT END(*IF*); (* ignore leading delim *)
      | OP  : IF ((CH <> '+') AND (CH <> '-')) OR
                                     ((TOKEN.COUNT > 1) AND (TOKEN.CHARS[TOKEN.COUNT] <> 'E')) THEN
                UNGETCHR(tpv,RETCOD);
                EXIT;
              END(*IF*);
              INC(TOKEN.COUNT);
              TOKEN.CHARS[TOKEN.COUNT] := CH;
      | DGT : INC(TOKEN.COUNT);
              IF CH = unaryminus THEN CH := MINUSSIGN END;
              TOKEN.CHARS[TOKEN.COUNT] := CH;
      | ALLELSE : IF (CH <> '.') AND (CH <> 'E') AND (CH <> '%') THEN
                    UNGETCHR(tpv,RETCOD);
                    EXIT;
                  END(*IF*);
                  INC(TOKEN.COUNT);
                  TOKEN.CHARS[TOKEN.COUNT] := CH;
      END(*CASE*);
    END(*LOOP*);
    DELIMCH    := CH;
    DELIMSTATE := CHRSTATE;
    IF TOKEN.CHARS[1] = DECPT THEN
      INSERTAin2B(BufOfZero,TOKEN,1);
    END;
    TOKEN.CHARS[TOKEN.COUNT+1] := NULL;
    TOKEN.LENGTH := TOKEN.COUNT;
    pos := 0;
    StringToReal(TOKEN.CHARS,pos,REALVAL,OK);
    IF NOT OK THEN RETCOD2 := 7; END(*IF*);
    IF TOKEN.CHARS[TOKEN.COUNT] = '%' THEN
        REALVAL := REALVAL / 100.;
    END;
  END(*with*);
END GETTKNREAL;

PROCEDURE GETTKNSTR(VAR tpv : TKNPTRTYP; VAR TOKEN:BUFTYP; VAR INTVAL:LONGINT; VAR RETCOD2:CARDINAL);
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
  WITH tpv^ DO
    GETTKN(tpv,TOKEN,TKNSTATE,INTVAL,RETCOD2);
    (* Can't test RETCOD2 because number token error does not apply here *)
    IF (TKNSTATE=DELIM) OR ((TKNSTATE=ALLELSE) AND (DELIMSTATE=DELIM)) THEN
      RETURN;
    END(*IF*);
(*
  Now must do special function of this proc.
  Continue COUNT & CHARS fields as left off from GETTKN call.
  As of 6/95 this should always return a tknstate of allelse, so return.
*)
    WITH TOKEN DO
      LOOP
        GETCHR(tpv,CH,CHRSTATE,RETCOD);
        IF (RETCOD > 0) OR ((CHRSTATE=DELIM) AND (COUNT > 0)) THEN
          EXIT;  (* Ignore leading delims *)
        END(*IF*);
        INC(COUNT);
        CHARS[COUNT] := CH;
        (*  INC(INTVAL,VAL(LONGINT,(CH)));  Don't need VAL here  *)
        INC(INTVAL,INT(CH));
      END(*LOOP*);
      DELIMCH        := CH;
      DELIMSTATE     := CHRSTATE;
      CHARS[COUNT+1] := NULL;
      LENGTH         := COUNT;
    END(*WITH TOKEN*);
  END(*with tpv*);
END GETTKNSTR;

PROCEDURE GETTKNEOL(VAR tpv : TKNPTRTYP; VAR TOKEN : BUFTYP; VAR RETCOD : CARDINAL);
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
  WITH tpv^ DO
    PREVPOSN := CURPOSN; (* So this tkn can be ungotten as well *)
    WITH TOKEN DO
      LOOP
        GETCHR(tpv,CH,CHRSTATE,RETCOD2);
        IF RETCOD2 > 0 THEN EXIT; END(*IF*);  (* Only exit loop at EOL *)
        CHARS[C] := CH;
        INC(C);
      END(*LOOP*);
      CHARS[C] := NULL;  (*
                          Take advantage of the post incremented pointer.
                          Must do this assignment so that if a new string put into this variable
                          is shorter than the previous string, TRIM will use
                          the correct (shorter) length.  Even the empty
                          string will be set correctly with the post incr ptr.
                       *)
      TRIM(TOKEN);
      IF COUNT = 0 THEN RETCOD := 1 END(*IF*);
    END(*WITH TOKEN*);
  END(*with tpv*)
END GETTKNEOL;

PROCEDURE GetHtmlCodeString(VAR tpv : TKNPTRTYP; VAR htmlcode:BUFTYP; VAR termcode:BOOLEAN; VAR RETCOD2:CARDINAL);
(*
********************************************************************************************************************
Return the string btwn angle brackets.  If a </ terminating code is found, then termcode is TRUE

RETCOD2--RETURN CODE.  0: Normal return; 1: No more tokens on line; 5: Error in processing htmlcode string
*)
VAR
    opcode,i          : LONGINT;
    ORDCH,C,RETCOD    : CARDINAL;
    CH                : CHAR;
    CHRSTATE,TKNSTATE : FSATYP;
    TOKEN             : BUFTYP;

BEGIN
  termcode := FALSE;
  RETCOD2 := 0;

  WITH tpv^ DO
(* Must have '<' with or without '/' *)
        REPEAT
      GETTKN(tpv,TOKEN,TKNSTATE,opcode,RETCOD);
    UNTIL (RETCOD>0) OR ((TKNSTATE=OP) AND (opcode=1)) OR ((TKNSTATE=OP) AND (opcode=20));
    IF RETCOD > 0 THEN
        RETCOD2 := 5;
        RETURN;
    END;
    IF opcode=20 THEN termcode := TRUE; END;
    GETTKN(tpv,TOKEN,TKNSTATE,i,RETCOD);  (* Should have the string I need *)
    htmlcode := TOKEN;
    IF TKNSTATE # ALLELSE THEN  (* error *)
        RETCOD2 := 5;
        RETURN;
    END; (* if tknstate # allelse *)
    GETTKN(tpv,TOKEN,TKNSTATE,opcode,RETCOD); (* swallow the terminating '>' *)
    IF (TKNSTATE # OP) OR (opcode # 3) THEN  (* error *)
        RETCOD2 := 5;
        RETURN;
    END;
  END(*with tpv*);
END GetHtmlCodeString;


PROCEDURE UNGETTKN(VAR tpv : TKNPTRTYP; VAR RETCOD : CARDINAL);
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
  WITH tpv^ DO
    IF (CURPOSN <= PREVPOSN) OR (PREVPOSN < 1) THEN
      WriteString(' *ERROR*  Cannot UNGETTKN because CURPOSN is too small or ');
      WriteLn;
      WriteString('  there is no last token to UNGET.  CURPOSN = ');
      WriteCard(CURPOSN);
      WriteString(', PREVPOSN = ');
      WriteCard(PREVPOSN);
      WriteLn;
      WriteString(' BEGINNING OF TKNBUF: ');
      WriteString(TKNBUF.CHARS);
      WriteLn;
      RETCOD := 1;
      RETURN;
    END(*IF*);     (* End error trap *)

    CURPOSN  := PREVPOSN;
    PREVPOSN := 0;
  END(*with*);
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
(*  FSAARRAY[59] := DELIM; semcol no longer a delim b/o html code str include semcol, like &amp; *)
  FSAARRAY[35] := OP;    (* poundsign *)
  FSAARRAY[42] := OP;    (* multsign *)
  FSAARRAY[43] := OP;    (* plussign *)
  FSAARRAY[45] := OP;    (* minussign *)
  FSAARRAY[47] := OP;    (* divsign *)
  FSAARRAY[60] := OP;      (* LTSIGN *)
  FSAARRAY[61] := OP;      (* EQUAL *)
  FSAARRAY[62] := OP;      (* GTSIGN *)
  FSAARRAY[95] := DGT;    (* underscore, now a unary minus *)
END InitFSAArray;

BEGIN  (* Initialize static variables that need it *)
  ASSIGN2BUF('0',BufOfZero);
  InitFSAArray;
END TOKENPTR.
