(* (C) 1990.  Robert W. Solomon.  All rights reserved.  *)

IMPLEMENTATION MODULE HPCALC;
(*
  This module simulates an HP type RPN calculator, but with a 8 reg stack
  instead of 4.  All operations occur at the bottom of the stack, as it
  does in HP calculators.

  REVISION HISTORY
  ----------------
   1 Dec 89 -- Added the help command.
  24 Dec 91 -- Converted to M-2 V 4.00.  Also changed the params to the
                GETRESULT proc to be more reasonable.
  21 Mar 93 -- Added exponentiation and MOD, INT, FRAC and ROUND, as
                well as used the UL2 procs again.
  25 Jul 93 -- Added JUL and GREG commands.
  18 May 03 -- Win32 version using Stony Brook Modula-2 v 4
  26 May 03 -- Allowed years to pivot for 2000 or 1900 in TIMLIB.
   4 Oct 03 -- Added LongCard2HexStr and its cmd.
  31 Oct 03 -- Added HCF cmd.
   1 Nov 03 -- Fixed the var swap bug in HCF rtn.
  27 Nov 04 -- Added pi as a cmd. 
  12 Mar 06 -- Made trig functions use arguments in degrees, changed stacksize to const, and moved stackregnames to def.
  22 Jul 06 -- Added % operator does xy/100 but does not drop stack.  Just like old HP 25.
  15 Apr 07 -- Added comma to push stack.  Removed it as a delim char and made it a ALLELSE.  And
                added ! ` and ~ as stack commands.  And updated i/o procedure use.
*)
(*
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
*)
  FROM MiscM2 IMPORT ReadString, WriteString, WriteLn, WriteReal;
  IMPORT IOChan, ChanConsts,LowLong,SYSTEM,SIOResult;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Environment IMPORT GetCommandLine;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRCMPFNT;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  IMPORT RConversions, LongStr, LongConv;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
(*
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    UNGETTKN,GETTKNREAL;
*)
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;

(*
   Declared in the Def Module.
TYPE  StackRegNames = (X,Y,Z,T5,T4,T3,T2,T1);
      RealStack     = ARRAY [X..T1] OF LONGREAL;

CONST STACKSIZE = ORD(T1) + 1;
*)
(* CONST PI        = 3.141592653589793;  now imported from LongMath *)

CONST
    SEMCOL = ';';
TYPE
	  STR20TYP      = ARRAY [0..20] OF CHAR;
VAR
    LASTX,MEMREG : LONGREAL;
    STACK        : RealStack;
    S            : StackRegNames;
    C1,C2,C3     : CARDINAL;
    str          : STR20TYP;
    bool         : BOOLEAN;
    delimSet     : CHARSETTYP;

PROCEDURE STACKUP;
VAR
    S : StackRegNames;
BEGIN
  FOR S := T2 TO X BY -1 DO
    STACK[VAL(StackRegNames,ORD(S)+1)] := STACK[S];
  END(*FOR*);
END STACKUP;

PROCEDURE STACKDN;
VAR
    S : StackRegNames;
BEGIN
  FOR S := Z TO T1 DO
    STACK[VAL(StackRegNames,ORD(S)-1)] := STACK[S];
  END(*FOR*);
END STACKDN;

PROCEDURE STACKROLLDN;
VAR
    TEMP : LONGREAL;
BEGIN
  TEMP := STACK[X];
  STACK[X] := STACK[Y];
  STACKDN;
  STACK[T1] := TEMP;
END STACKROLLDN;

PROCEDURE PUSHX(R : LONGREAL);
BEGIN
  STACKUP;
  STACK[X] := R;
END PUSHX;

PROCEDURE READX() : LONGREAL;
BEGIN
  RETURN(STACK[X]);
END READX;

PROCEDURE SWAPXY;
VAR TEMP : LONGREAL;
BEGIN
  TEMP := STACK[X];
  STACK[X] := STACK[Y];
  STACK[Y] := TEMP;
END SWAPXY;

PROCEDURE GETSTACK(VAR STK : ARRAY OF LONGREAL; VAR RETCOD : CARDINAL);
VAR
    S : StackRegNames;
BEGIN
  RETCOD := 0;
  IF HIGH(STK) + 1 < STACKSIZE THEN
(*
  Output param not large enough to hold entire stack.  Remember HIGH
  proc returns the highest subscript of a zero origin array.
*)
    RETCOD := 1;
    RETURN;
  END(*IF*);
  FOR S := X TO T1 DO
    STK[ORD(S)] := STACK[S];
  END(*FOR*);
END GETSTACK;

PROCEDURE DUMPSTACK;
VAR
    S      : StackRegNames;
    NUMSTR : ARRAY [1..80] OF CHAR;
    OK     : BOOLEAN;
    pos    : CARDINAL;
BEGIN
  WriteString('+--------------------+');
  WriteString('          ');
  WriteString('+--------------------+');
  WriteLn;
  WriteString('|  ');
  WriteReal(STACK[T1],15);
  WriteString('   |');
  WriteString('          ');
  WriteString('|  ');
  pos := 0;
  RealToStringFixed(STACK[T1],15,5,NUMSTR,pos,OK);
  WriteString(NUMSTR);
  WriteString('   |');
  WriteLn;
  FOR S := T2 TO X BY -1 DO
    WriteString('+--------------------+');
    WriteString('          ');
    WriteString('+--------------------+');
    WriteLn;
    WriteString('|  ');
    WriteReal(STACK[S],15);
    WriteString('   |');
    WriteString('          ');
    WriteString('|  ');
    pos := 0;
    RealToStringFixed(STACK[S],15,5,NUMSTR,pos,OK);
    WriteString(NUMSTR);
    WriteString('   |');
    WriteLn;
  END(*FOR*);
  WriteString('+--------------------+');
  WriteString('          ');
  WriteString('+--------------------+');
  WriteLn;
END DUMPSTACK;

PROCEDURE LongCard2HexStr(L : LONGCARD; VAR OutStr : ARRAY OF CHAR);
CONST  ASCZERO = ORD('0');
       ascA    = ORD('A');
VAR i,j,h  : CARDINAL;
    Str20  : STR20TYP;

BEGIN
  i := 0;
  REPEAT (* until L = 0 *)
    h := L MOD 16;
    IF (h <= 9) THEN Str20[i] := CHR(h + ASCZERO) ELSE Str20[i] := CHR(h -10 + ascA) END;
    INC(i);
    L := L DIV 16;
  UNTIL L = 0;
  j := 1;  (* first posn is a space to leave room for sign char *)
  OutStr[0] := ' ';
  REPEAT (* until i = 0 *)
    DEC(i);
    OutStr[j] := Str20[i];
    INC(j);
  UNTIL i = 0;
  OutStr[j] := 0C;
END LongCard2HexStr;

PROCEDURE LongInt2HexStr(L : LONGINT; VAR OutStr : ARRAY OF CHAR);
VAR
   IsNeg : BOOLEAN;
   LC    : LONGCARD;

BEGIN
    IF L < 0 THEN
      IsNeg := TRUE;
      LC := -L;
    ELSE
      IsNeg := FALSE;
      LC := L;
    END;
    LongCard2HexStr(LC,OutStr);
    IF IsNeg THEN
      OutStr[0] := '-';
    END;
END LongInt2HexStr;

(*****************************************************)
PROCEDURE HCF(a,b : CARDINAL) : CARDINAL;
(* a = bt + r, then hcf(a,b) = hcf(b,r)              *)
(*****************************************************)
VAR r : CARDINAL;

BEGIN
  IF a < b THEN
(*
    C1 := a;
    a := b;
    b := C1;
*)
    a := a BXOR b;
    b := a BXOR b;
    a := a BXOR b;
  END;
  REPEAT 
    r := a MOD b;
    a := b;
    b := r;
  UNTIL r = 0;
  RETURN a;
END HCF;


PROCEDURE GETRESULT(BUF : BUFTYP) : LONGREAL;
(*
************************* GETRESULT **********************************
Gets the result of the operations in the input line.  Parses and does the
operations.
*)
VAR
    RETCOD,c,c1,c2 : CARDINAL;
    I        : INTEGER;
(*    CH       : CHAR; *)
    TOKEN    : BUFTYP;
    TKNSTATE : FSATYP;
    R        : LONGREAL;
    L        : LONGINT;
    LC       : LONGCARD;
BEGIN
  TRIM(BUF);
  INI1TKN(BUF);
  LOOP  (* UNTIL FINISHED, I.E., RETCOD > 0 *)
    GETTKNREAL(TOKEN,TKNSTATE,L,R,RETCOD);
    I := L;
(*
    WriteString('After gettknreal & retcod =');
    WriteCard(RETCOD,0);
    WriteLn;
*)
    IF RETCOD > 0 THEN EXIT; END(*IF*);
    CASE TKNSTATE OF
      DGT : PUSHX(R);
    | OP  : IF (I = 6) OR (I = 20) THEN SWAPXY;
         ELSE
           LASTX := STACK[X];
           CASE I OF
              8 : (* Add *) STACK[X] := STACK[Y] + STACK[X];
           | 10 : (* Subt *) STACK[X] := STACK[Y] - STACK[X];
           | 12 : (* Mult *) STACK[X] := STACK[Y] * STACK[X];
           | 14 : (* Div *) STACK[X] := STACK[Y] / STACK[X];
           | 16 : (* ^   *) STACK[X] := exp(STACK[X]*ln(ABS(STACK[Y])));
           | 18 : (* **  *) STACK[X] := PWRI(STACK[Y],round(STACK[X]));
           | 22 : (* %   *) STACK[X] := STACK[Y] * STACK[X] / 100.;
           ELSE
             WriteString(TOKEN.CHARS);
             WriteString(' is an unrecognized operation.');
             STACKUP;
             WriteLn;
           END(*CASE*);
           IF I <> 22 THEN STACKDN; END; (* Do not move stack for % operator *)
         END(*IF*);
    | ALLELSE : IF STRCMPFNT(TOKEN.CHARS,'SQRT') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := sqrt(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'SQR') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := STACK[X] * STACK[X];
                ELSIF STRCMPFNT(TOKEN.CHARS,'HELP') = 0 THEN
                  WriteString(
' This is an RPN style calculator as popularized by Hewlett-Packard.');
                  WriteLn;
                  WriteString(
' SQRT,SQR -- X = sqrt(X) or sqr(X) register.');
                  WriteLn;
                  WriteString(
' STO,RCL  -- store/recall the X register to/from the memory register.');
                  WriteLn;
                  WriteString(
' SWAP,SWAPXY,<>,>< -- equivalent commands that swap the X and Y registers.');
                  WriteLn;
                  WriteString(
' LASTX -- put the value of the LASTX register back into the X register.');
                  WriteLn;
                  WriteString(
' ROLLDN -- roll the stack down one register.  X goes to T1.');
                  WriteLn;
                  WriteString(
' DUMP -- dump the stack to the screen.');
                  WriteLn;
                  WriteString(
' EXP,LN -- evaluate exp(X) or ln(X) and put result back into X.');
                  WriteLn;
                  WriteString(
' ^   -- evaluate ABS(Y) to the X power, put result in X and pop stack 1 reg.');
                  WriteLn;
                  WriteString(
' **  -- like "^" but rounds X before calling the PWRI function.');
                  WriteLn;
                  WriteString(
' INT, ROUND, FRAC -- do what their names suggest.');
                  WriteLn;
                  WriteString(
' MOD -- evaluate Y MOD X, put result in X and pop stack 1 reg.');
                  WriteLn;
                  WriteString(
' SIN,COS,TAN,ARCTAN,ARCSIN,ARCCOS -- take a wild guess.  In radians.');
                  WriteLn;
                  WriteString(
' D2R -- perform degrees to radians conversion of the X register.');
                  WriteLn;
                  WriteString(
' R2D -- perform radians to degrees conversion of the X register.');
                  WriteLn;
                  WriteString(
' JUL -- Return Julian date number of Z month, Y day, X year.  Pop stack x2.');
                  WriteLn;
                  WriteString(
" TODAY- Return Julian date number of today's date.  Pop stack x2.");
                  WriteLn;
                  WriteString(
' GREG-- Return Z month, Y day, X year of Julian date number in X.');
                  WriteLn;
                  WriteString(
' DOW -- Return day number 1..7 of julian date number in X register.');
                  WriteLn;
                  WriteString(
' HEX -- Round X register to a longcard and output it in hex format.');
                  WriteLn;
                ELSIF STRCMPFNT(TOKEN.CHARS,'STO') = 0 THEN
                  MEMREG := STACK[X];
                ELSIF STRCMPFNT(TOKEN.CHARS,'RCL') = 0 THEN
                  PUSHX(MEMREG);
                ELSIF STRCMPFNT(TOKEN.CHARS,'SWAP') = 0 THEN
                  SWAPXY;
                ELSIF STRCMPFNT(TOKEN.CHARS,'SWAPXY') = 0 THEN
                  SWAPXY;
                ELSIF STRCMPFNT(TOKEN.CHARS,'LASTX') = 0 THEN
                  PUSHX(LASTX);
                ELSIF STRCMPFNT(TOKEN.CHARS,'ROLLDN') = 0 THEN
                  STACKROLLDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,',') = 0 THEN 
                  STACKUP;
                ELSIF STRCMPFNT(TOKEN.CHARS,'!') = 0 THEN 
                  STACK[X] := STACK[Y];
                  STACKDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,'`') = 0 THEN 
                  SWAPXY;
                ELSIF STRCMPFNT(TOKEN.CHARS,'~') = 0 THEN 
                  SWAPXY;
                ELSIF STRCMPFNT(TOKEN.CHARS,'@') = 0 THEN 
                  PUSHX(LASTX);
                ELSIF STRCMPFNT(TOKEN.CHARS,'DUMP') = 0 THEN
                  DUMPSTACK;
                ELSIF STRCMPFNT(TOKEN.CHARS,'EXP') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := exp(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'LN') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := ln(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'Y^X') = 0 THEN
(* Now that ^ is an operator, this must be quoted to be parsed as such *)
                  LASTX := STACK[X];
                  STACK[X] := exp(STACK[X]*ln(ABS(STACK[Y])));
                  STACKDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,'INT') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := AINT(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'TRUNC') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := trunc(STACK[X],1);
                ELSIF STRCMPFNT(TOKEN.CHARS,'ROUND') = 0 THEN
                  LASTX := STACK[X];
(*                  STACK[X] := LowLong.round(STACK[X],1); doesn't work *)
                  IF STACK[X] >= 0. THEN           (* X is positive or zero *)
                    STACK[X] := AINT(STACK[X] + 0.5);
                  ELSE                             (* X is negative *)
                    STACK[X] := AINT(STACK[X] - 0.5);
                  END(*IF*);
                ELSIF STRCMPFNT(TOKEN.CHARS,'HEX') = 0 THEN  (* treat as positive *)
                  IF (STACK[X] > 0.) AND (STACK[X] <= 2.0E9) THEN
                    c := TRUNC(STACK[X]);
                    LongCard2HexStr(c,str);
                  ELSE
                    L := VAL(LONGINT,STACK[X]);
(*
                    R := AINT(STACK[X]);
                    c := 0;
                    RealToStringFixed(R,15,1,str,c,bool);
                    (*bool := *)StrToLong(str,L);
                    LongInt2HexStr(L,str);
*)
                    LC := SYSTEM.CAST(LONGCARD,L);
                    LongCard2HexStr(LC,str);
                  END;
                  WriteString(' Value of x reg in hex: ');
                  WriteString(str);
                  WriteLn;
                ELSIF STRCMPFNT(TOKEN.CHARS,'HCF') = 0 THEN
                  c1 := TRUNC(ABS(STACK[X]));
                  c2 := TRUNC(ABS(STACK[Y]));
                  c  := HCF(c2,c1);
                  STACKUP;
                  STACK[X] := LFLOAT(c);
                  
                ELSIF STRCMPFNT(TOKEN.CHARS,'FRAC') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := STACK[X] - AINT(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'MOD') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := AMOD(STACK[Y],STACK[X]);
                  STACKDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,'SIN') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := sin(STACK[X]*pi/180.);
                ELSIF STRCMPFNT(TOKEN.CHARS,'COS') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := cos(STACK[X]*pi/180.);
                ELSIF STRCMPFNT(TOKEN.CHARS,'TAN') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := sin(STACK[X]*pi/180.)/cos(STACK[X]*pi/180.);
                ELSIF STRCMPFNT(TOKEN.CHARS,'ARCTAN') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := arctan(STACK[X])*180./pi;
                ELSIF STRCMPFNT(TOKEN.CHARS,'ARCSIN') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := arctan(LASTX/sqrt(1.-(LASTX*LASTX)))*180./pi;
                ELSIF STRCMPFNT(TOKEN.CHARS,'ARCCOS') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := arctan(sqrt(1.-(LASTX*LASTX))/LASTX)*180./pi;
                ELSIF STRCMPFNT(TOKEN.CHARS,'D2R') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := STACK[X]*pi/180.;
                ELSIF STRCMPFNT(TOKEN.CHARS,'R2D') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := STACK[X]*180./pi;
                ELSIF STRCMPFNT(TOKEN.CHARS,'JUL') = 0 THEN
                  LASTX := STACK[X];
(* allow for 2000 years                 IF STACK[X] < 100. THEN STACK[X] := STACK[X] + 1900.; END; *)
                  STACK[X] := LFLOAT(JULIAN(round(STACK[Z]),
                                           round(STACK[Y]),round(STACK[X])));
                  STACKDN;
                  STACKDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,'TODAY') = 0 THEN
                  LASTX := STACK[X];
                  STACKUP;
                  TIME2MDY(C1,C2,C3);
                  STACK[X] := LFLOAT(JULIAN(C1,C2,C3));
                ELSIF STRCMPFNT(TOKEN.CHARS,'GREG') = 0 THEN
                  LASTX := STACK[X];
                  STACKUP;
                  STACKUP;
                  GREGORIAN(round(STACK[X]),C1,C2,C3);
                  STACK[Z] := LFLOAT(C1);
                  STACK[Y] := LFLOAT(C2);
                  STACK[X] := LFLOAT(C3);
                ELSIF STRCMPFNT(TOKEN.CHARS,'DOW') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := AMOD(LASTX,7.) + 1.;
                ELSIF STRCMPFNT(TOKEN.CHARS,'PI') = 0 THEN
                  PUSHX(pi);
                ELSE
                  WriteString(TOKEN.CHARS);
                  WriteString(' is an unrecognized command.');
                  WriteLn;
                END(*IF*);
    END(*CASE*);
  END(*LOOP*);
  RETURN(STACK[X]);
END GETRESULT;

BEGIN (********************* MAIN ****************************************)
  FOR S := X TO T1 DO STACK[S] := 0.; END(*FOR*);
(*  STACKSIZE  := ORD(T1) + 1; *)
  LASTX := 0.;
  MEMREG := 0.;
  delimSet := CHARSETTYP{BLANK,NULL,SEMCOL};  (* Note absence of COMMA *)
  NEWDELIMSET(delimSet);
END HPCALC.
