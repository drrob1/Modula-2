<*DEFINE (ConsoleMode,TRUE)*>

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
   1 Apr 13 -- Modified the DUMP routine so the characters are not printed on the same line as the numbers.  The
                alignment was all wrong anyway.  And added Dump2Console.
   2 May 13 -- Will use consolemode compilation flags.
   4 Oct 13 -- Reversed output in the stack dump operation to get columns to align.
  26 Dec 14 -- Added HOL command, fixed the help strings as I noticed errors when I wrote the cpp version,
                 and added a null ABOUT command here.
   5 Nov 15 -- Added vol, curt and recip commands.
  16 Apr 16 -- Added undo, redo commands.  These were first coded in C++ in ubuntu, and were fully debugged today.
  19 May 16 -- Fixed help to include the % operator, that was coded in 2006 but help was never updated back then.
   7 Jul 16 -- Added "UP" command.  I'm surprised that I never thought of this before.  Add PI to help list.  Change pivot for Jul command to current year.
  26 Mar 17 -- Backporting string slice equivalent I learned to do in Go.  And replaced "!" with "|" as stack down
                 command.  And will use LongStr conversion routines instead of RConversions.  This rtn no
                 longer does I/O.  I don''t want the Dump2Console procedure used anymore.
  27 Mar 17 -- FormatString used in DumpStack.
  28 Mar 17 -- Removed depracated RConversion code, reversed ^ and ** to match Go version, and fixed help text.
*)
  FROM SYSTEM IMPORT ADR;
  IMPORT STextIO;
  IMPORT IOChan, ChanConsts,LowLong,SYSTEM;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Environment IMPORT GetCommandLine;
  FROM FormatString IMPORT FormatString;
  FROM UTILLIB IMPORT BLANK,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,STR20TYP,BUFTYP,MAXCARDFNT,COPYLEFT,COPYRIGHT,
    FILLCHAR,SCANFWD,SCANBACK, STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,APPENDA2B,CONCATAB2C,
    INSERTAin2B,ASSIGN2BUF, StringItemPointerType,StringDoubleLinkedListPointerType,InitStringListPointerType,
    AppendStringToList,NextStringFromList,PrevStringFromList,CurrentPointerBeginning,CurrentPointerEnding,
    GetNextStringFromList,GetPrevStringFromList;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  IMPORT RConversions, LongStr, LongConv, Strings;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM HolidayCalc IMPORT HolType, GetHolidays;

%IF ConsoleMode %THEN
  IMPORT MiscStdInOut;
  FROM MiscStdInOut IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, ReadString, SkipLine, ReadCard, ReadLongReal;
%ELSE
  IMPORT MiscM2;
  FROM MiscM2 IMPORT WriteCard, WriteLongCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, Read, ReadString, ReadCard, ReadLongReal;
%END
(*
   Declared in the Def Module.
TYPE
  StackRegNames = (X,Y,Z,T5,T4,T3,T2,T1);
  RealStack     = ARRAY [X..T1] OF LONGREAL;

CONST
  STACKSIZE = ORD(T1) + 1;
    StackMatrixSize = 8;

VAR
  Holidays : HolType;

*)
(* CONST PI        = 3.141592653589793;  now imported from LongMath *)
TYPE
    StackRegNamesStringType = ARRAY [X .. T1] OF STR10TYP;
CONST
    SEMCOL = ';';
    Bottom = 1;
    Top = StackMatrixSize;
    LastCompiled = "28 Mar 2017";

VAR
    LASTX,MEMREG : LONGREAL;
    STACK        : RealStack;
    StackMatrix  : ARRAY [Bottom..Top] OF RealStack;
    S            : StackRegNames;
    C1,C2,C3     : CARDINAL;
    str          : STR20TYP;
    bool         : BOOLEAN;
    delimSet     : CHARSETTYP;
    StackRegNamesString : StackRegNamesStringType;
(*    StringListP  : StringDoubleLinkedListPOinterType;  Not sure if I want this here *)

(*****************************************************)
PROCEDURE STACKUP;
VAR
    S : StackRegNames;
BEGIN
  FOR S := T2 TO X BY -1 DO
    STACK[VAL(StackRegNames,ORD(S)+1)] := STACK[S];
  END(*FOR*);
END STACKUP;

(*****************************************************)
PROCEDURE STACKDN;
VAR
    S : StackRegNames;
BEGIN
  FOR S := Z TO T1 DO
    STACK[VAL(StackRegNames,ORD(S)-1)] := STACK[S];
  END(*FOR*);
END STACKDN;

(*****************************************************)
PROCEDURE STACKROLLDN;
VAR
    TEMP : LONGREAL;
BEGIN
  TEMP := STACK[X];
  STACK[X] := STACK[Y];
  STACKDN;
  STACK[T1] := TEMP;
END STACKROLLDN;

(*****************************************************)
PROCEDURE PUSHX(R : LONGREAL);
BEGIN
  STACKUP;
  STACK[X] := R;
END PUSHX;

PROCEDURE READX() : LONGREAL;
BEGIN
  RETURN(STACK[X]);
END READX;

(*****************************************************)
PROCEDURE SWAPXY;
VAR TEMP : LONGREAL;
BEGIN
  TEMP := STACK[X];
  STACK[X] := STACK[Y];
  STACK[Y] := TEMP;
END SWAPXY;

(*****************************************************)
PROCEDURE TruncateInsigZeros(VAR INOUT str : ARRAY OF CHAR);
VAR
  StrLen, Non0Posn, NonBlPosn : CARDINAL;

BEGIN
      StrLen := Strings.Length(str);
      (* Scanback for first nonzero char.  This is the new length of the string *)
      Non0Posn := SCANBACK(ADR(str),StrLen,'0',FALSE);
      str[Non0Posn] := NULL; (* Terminate string at 1-st insignificant 0 *)
      NonBlPosn := SCANFWD(ADR(str),Non0Posn,BLANK,FALSE) -1;
(*
   Remove the leading blanks by copying the non-blank string to the beginning, including the terminating null char.
*)
      COPYLEFT(ADR(str[NonBlPosn]),ADR(str),Non0Posn-NonBlPosn+1);
END TruncateInsigZeros;

(*****************************************************)
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

(*****************************************************)
PROCEDURE DUMPSTACK() : StringDoubleLinkedListPointerType;
VAR
    S : StackRegNames;
    NUMSTR,Xstr : ARRAY [1..80] OF CHAR;
    ok : BOOLEAN;
    pos : CARDINAL;
    StringListP : StringDoubleLinkedListPointerType;
    line : STRTYP;
BEGIN
  StringListP := InitStringListPointerType();
  Strings.Assign("----------------",line);
  Strings.Append("--+--",line);
  Strings.Append("------------------",line);
  AppendStringToList(StringListP,line);
  FOR S := T1 TO X BY -1 DO
    RealToFixed(STACK[S],6,Xstr);
    TruncateInsigZeros(Xstr);
    ok := FormatString("%-4s%-13s |  ",NUMSTR,StackRegNamesString[S],Xstr); (* NUMSTR receives the sprintf string *)
    Strings.Assign(NUMSTR,line);
    RealToStr(STACK[S],NUMSTR);
    TruncateInsigZeros(NUMSTR);
    Strings.Append(NUMSTR,line);
    AppendStringToList(StringListP,line);
  END(* FOR stack from top down *);
  Strings.Assign("----------------",line);
  Strings.Append("--+--",line);
  Strings.Append("------------------",line);
  AppendStringToList(StringListP,line);
  RETURN StringListP;
END DUMPSTACK;

(*****************************************************)
PROCEDURE Dump2Console;
(*
  I changed the looping so that the entire stack is output in the same loop, including the topmost element, T1.
  DNU since I added the string list code.
*)
VAR
    S      : StackRegNames;
    NUMSTR : ARRAY [1..80] OF CHAR;
    OK     : BOOLEAN;
    pos    : CARDINAL;
BEGIN
  FOR S := T1 TO X BY -1 DO
    STextIO.WriteString('------------');
    STextIO.WriteString('  ||  ');
    STextIO.WriteString('------------------');
    STextIO.WriteLn;
    pos := 0;
    RealToStringFixed(STACK[S],15,5,NUMSTR,pos,OK);
    STextIO.WriteString(NUMSTR);
    STextIO.WriteString('  ||  ');
    MiscStdInOut.WriteLongReal(STACK[S],10);
    STextIO.WriteLn;
  END(*FOR*);
  STextIO.WriteString('------------');
  STextIO.WriteString('  ||  ');
  STextIO.WriteString('------------------');
  STextIO.WriteLn;
END Dump2Console;

(*****************************************************)
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

(*****************************************************)
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
PROCEDURE StacksUp;
VAR
  i : CARDINAL;

BEGIN
    FOR i := StackMatrixSize-1 TO 1 BY -1 DO
    StackMatrix[i+1] := StackMatrix[i];
  END;
END StacksUp;
(*****************************************************)
PROCEDURE StacksDown;
VAR
    i : CARDINAL;

BEGIN
    FOR i := Bottom+1 TO Top DO
        StackMatrix[i-1] := StackMatrix[i];
    END;
END StacksDown;
(*****************************************************)
PROCEDURE PushStacks;
BEGIN
    StacksUp;
    StackMatrix[Bottom] := STACK;
END PushStacks;
(*****************************************************)
PROCEDURE RollDownStacks;     (* IE, undo *)
VAR
    TempStack : RealStack;

BEGIN
    TempStack := STACK;
    STACK := StackMatrix[Bottom];
    StacksDown;
    StackMatrix[Top] := TempStack;
END RollDownStacks;
(*****************************************************)
PROCEDURE RollUpStacks;      (* IE, redo *)
VAR
    TempStack : RealStack;

BEGIN
    TempStack := STACK;
    STACK := StackMatrix[Top];
    StacksUp;
    StackMatrix[Bottom] := TempStack;
END RollUpStacks;
(*****************************************************)

PROCEDURE HCF(a,b : CARDINAL) : CARDINAL;
(*     a = bt + r, then hcf(a,b) = hcf(b,r)          *)

VAR
  r : CARDINAL;

BEGIN
  IF a < b THEN
                                                                                      (* C1 := a; a := b; b := C1; *)
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


PROCEDURE GETRESULT(BUF : BUFTYP; VAR R : LONGREAL) : StringDoubleLinkedListPointerType;
(*
************************* GETRESULT **********************************
old signature was this: PROCEDURE GETRESULT(BUF : BUFTYP) : LONGREAL;
Gets the result of the operations in the input line.  Parses and does the
operations.  Returns R as a VAR param now, and returns StringListP which is either NIL or has a string list.
*)
VAR
    RETCOD,c,c1,c2,year : CARDINAL;
    I        : INTEGER;
    TOKEN    : BUFTYP;
    TKNSTATE : FSATYP;
                                                             (*   R        : LONGREAL; *)
    L        : LONGINT;
    LC       : LONGCARD;
    StringP  : StringItemPointerType;
    StringListP : StringDoubleLinkedListPointerType;
    s : STRTYP;

BEGIN
  TRIM(BUF);
  INI1TKN(BUF);
  StringListP := NIL;

  LOOP  (* UNTIL FINISHED, I.E., RETCOD > 0 *)
    GETTKNREAL(TOKEN,TKNSTATE,L,R,RETCOD);
    I := L;
    IF RETCOD > 0 THEN EXIT; END(*IF*);
    CASE TKNSTATE OF
      DGT :
            PUSHX(R);
            PushStacks;
    | OP  : IF (I = 6) OR (I = 20) THEN SWAPXY;
         ELSE
           LASTX := STACK[X];
           PushStacks;
           CASE I OF
              8 : (* Add *) STACK[X] := STACK[Y] + STACK[X];
           | 10 : (* Subt *) STACK[X] := STACK[Y] - STACK[X];
           | 12 : (* Mult *) STACK[X] := STACK[Y] * STACK[X];
           | 14 : (* Div *) STACK[X] := STACK[Y] / STACK[X];
           | 16 : (* ^   *) STACK[X] := PWRI(STACK[Y],round(STACK[X]));
           | 18 : (* **  *) STACK[X] := exp(STACK[X]*ln(ABS(STACK[Y])));
           | 22 : (* %   *) STACK[X] := STACK[Y] * STACK[X] / 100.;
           ELSE
             StringListP := InitStringListPointerType();
             Strings.Assign(TOKEN.CHARS,s);
             Strings.Append(" is an unrecognized operation.",s);
             AppendStringToList(StringListP,s);
             STACKUP;
           END(*CASE*);
           IF I <> 22 THEN STACKDN; END; (* Do not move stack for % operator *)
         END(*IF*);
    | ALLELSE : IF STRCMPFNT(TOKEN.CHARS,'SQRT') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := sqrt(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'D') = 0 THEN
                        Dump2Console;
                ELSIF STRCMPFNT(TOKEN.CHARS,'SQR') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := STACK[X] * STACK[X];
                ELSIF STRCMPFNT(TOKEN.CHARS,"RECIP") = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := 1.0 / STACK[X];
                ELSIF STRCMPFNT(TOKEN.CHARS,"CURT") = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := exp((1.0/3.0)*ln(STACK[X]));
                ELSIF STRCMPFNT(TOKEN.CHARS,"VOL") = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := exp(ln(2.0*STACK[X])*1.0/3.0);
                ELSIF STRCMPFNT(TOKEN.CHARS,"UNDO") = 0 THEN
                    RollDownStacks;
                ELSIF STRCMPFNT(TOKEN.CHARS,"REDO") = 0 THEN
                    RollUpStacks;
                ELSIF STRCMPFNT(TOKEN.CHARS,'HELP') = 0 THEN
                  StringListP := InitStringListPointerType();
                  AppendStringToList(StringListP," SQRT,SQR,CURT -- X = sqrt(X), sqr(X) or cube root X register.");
                  AppendStringToList(StringListP," RECIP -- reciprocal of X, ie, 1/X.");
                  AppendStringToList(StringListP," VOL -- Given a volume in X, estimate size of cavity assuming a sphere.");
                  AppendStringToList(StringListP," STO,RCL  -- store/recall the X register to/from the memory register.");
                  AppendStringToList(StringListP," `,~,SWAP,SWAPXY,<>,>< -- equivalent commands that swap the X and Y registers.");
                  AppendStringToList(StringListP," @, LASTX -- put the value of the LASTX register back into the X register.");
                  AppendStringToList(StringListP," DN,ROLLDN -- roll the stack down one register.  X goes to T1.");
                  AppendStringToList(StringListP," D -- Dump2console.   DUMP -- dump the stack to the terminal.");
                  AppendStringToList(StringListP," EXP,LN -- evaluate exp(X) or ln(X) and put result back into X.");
                  AppendStringToList(StringListP," ^   -- evaluate Y to the X power using PWRI func, put result in X.");
                  AppendStringToList(StringListP," **  -- evaluate ABS(Y) to the X power using exp() and ln().");
                  AppendStringToList(StringListP," %   -- does XY/100, places result in X.  Leaves Y alone.");
                  AppendStringToList(StringListP," INT, ROUND, FRAC -- do what their names suggest.");
                  AppendStringToList(StringListP," MOD -- evaluate Y MOD X, put result in X and pop stack 1 reg.");
                  AppendStringToList(StringListP," SIN,COS,TAN,ARCTAN,ARCSIN,ARCCOS -- In degrees.");
                  AppendStringToList(StringListP," D2R -- perform degrees to radians conversion of the X register.");
                  AppendStringToList(StringListP," R2D -- perform radians to degrees conversion of the X register.");
                  AppendStringToList(StringListP," PI -- goes into the X register.");
                  AppendStringToList(StringListP," R2D -- perform radians to degrees conversion of the X register.");
                  AppendStringToList(StringListP," TODAY- Return Julian date number of today's date.  Pop stack x2.");
                  AppendStringToList(StringListP," GREG-- Return Z month, Y day, X year of Julian date number in X.");
                  AppendStringToList(StringListP," DOW -- Return day number 1..7 of julian date number in X register.");
                  AppendStringToList(StringListP," HEX -- Round X register to a longcard and output it in hex format.");
                  AppendStringToList(StringListP," , or UP -- stack up.  | or DN -- stack down.");
                  AppendStringToList(StringListP," HCF -- Push HCF(Y,X) onto stack without removing Y or X.");
                  AppendStringToList(StringListP," HOL -- Compute the Holidays variable for year in X.");  (* exported by this module for client module to display *)
                  AppendStringToList(StringListP," undo, redo -- entire stack.  More comprehensive than lastx.");
                  Strings.Concat(" Last Compiled HPCALC.MOD ",LastCompiled,s);
                  AppendStringToList(StringListP,s);
                ELSIF STRCMPFNT(TOKEN.CHARS,'STO') = 0 THEN
                  MEMREG := STACK[X];
                ELSIF STRCMPFNT(TOKEN.CHARS,'RCL') = 0 THEN
                  PushStacks;
                  PUSHX(MEMREG);
                ELSIF STRCMPFNT(TOKEN.CHARS,'SWAP') = 0 THEN
                  PushStacks;
                  SWAPXY;
                ELSIF STRCMPFNT(TOKEN.CHARS,'SWAPXY') = 0 THEN
                  PushStacks;
                  SWAPXY;
                ELSIF STRCMPFNT(TOKEN.CHARS,'LASTX') = 0 THEN
                  PushStacks;
                  PUSHX(LASTX);
                ELSIF STRCMPFNT(TOKEN.CHARS,'ROLLDN') = 0 THEN
                  PushStacks;
                  STACKROLLDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,'DN') = 0 THEN
                  PushStacks;
                  STACKROLLDN;
                ELSIF (STRCMPFNT(TOKEN.CHARS,',') = 0) OR (STRCMPFNT(TOKEN.CHARS,'UP') = 0) THEN
                  PushStacks;
                  STACKUP;
                ELSIF STRCMPFNT(TOKEN.CHARS,'|') = 0 THEN
                  PushStacks;
                  STACK[X] := STACK[Y];
                  STACKDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,'`') = 0 THEN
                  PushStacks;
                  SWAPXY;
                ELSIF STRCMPFNT(TOKEN.CHARS,'~') = 0 THEN
                  PushStacks;
                  SWAPXY;
                ELSIF STRCMPFNT(TOKEN.CHARS,'@') = 0 THEN
                  PushStacks;
                  PUSHX(LASTX);
                ELSIF STRCMPFNT(TOKEN.CHARS,'DUMP') = 0 THEN
                  StringListP := DUMPSTACK();
                ELSIF STRCMPFNT(TOKEN.CHARS,'EXP') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := exp(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'LN') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := ln(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'Y^X') = 0 THEN
                (* Now that ^ is an operator, this must be quoted to be parsed as such *)
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := exp(STACK[X]*ln(ABS(STACK[Y])));
                  STACKDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,'INT') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := AINT(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'TRUNC') = 0 THEN
                  LASTX := STACK[X];
                  STACK[X] := trunc(STACK[X],1);
                ELSIF STRCMPFNT(TOKEN.CHARS,'ROUND') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
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
                    LC := SYSTEM.CAST(LONGCARD,L);
                    LongCard2HexStr(LC,str);
                  END;
                  StringListP := InitStringListPointerType();
                  Strings.Concat(" Value of X reg in hex: ",str,s);
                  AppendStringToList(StringListP,s);
                ELSIF STRCMPFNT(TOKEN.CHARS,'HCF') = 0 THEN
                  PushStacks;
                  c1 := TRUNC(ABS(STACK[X]));
                  c2 := TRUNC(ABS(STACK[Y]));
                  c  := HCF(c2,c1);
                  STACKUP;
                  STACK[X] := LFLOAT(c);
                ELSIF STRCMPFNT(TOKEN.CHARS,'P') = 0 THEN
                  (* do nothing but print RESULT= line again *)
                ELSIF STRCMPFNT(TOKEN.CHARS,'FRAC') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := STACK[X] - AINT(STACK[X]);
                ELSIF STRCMPFNT(TOKEN.CHARS,'MOD') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := AMOD(STACK[Y],STACK[X]);
                  STACKDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,'SIN') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := sin(STACK[X]*pi/180.);
                ELSIF STRCMPFNT(TOKEN.CHARS,'COS') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := cos(STACK[X]*pi/180.);
                ELSIF STRCMPFNT(TOKEN.CHARS,'TAN') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := sin(STACK[X]*pi/180.)/cos(STACK[X]*pi/180.);
                ELSIF STRCMPFNT(TOKEN.CHARS,'ARCTAN') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := arctan(STACK[X])*180./pi;
                ELSIF STRCMPFNT(TOKEN.CHARS,'ARCSIN') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := arctan(LASTX/sqrt(1.-(LASTX*LASTX)))*180./pi;
                ELSIF STRCMPFNT(TOKEN.CHARS,'ARCCOS') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := arctan(sqrt(1.-(LASTX*LASTX))/LASTX)*180./pi;
                ELSIF STRCMPFNT(TOKEN.CHARS,'D2R') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := STACK[X]*pi/180.;
                ELSIF STRCMPFNT(TOKEN.CHARS,'R2D') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := STACK[X]*180./pi;
                ELSIF STRCMPFNT(TOKEN.CHARS,'JUL') = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
(*     Will supercede code in JULIAN from TIMLIB here.  *)
                  TIME2MDY(C1,C2,C3);
                  IF STACK[X] <= LFLOAT (C3 MOD 100) THEN
                    STACK[X] := STACK[X] + 2000.;
                  ELSIF STACK[X] < 100. THEN
                    STACK[X] := STACK[X] + 1900.;
                  END;
                  STACK[X] := LFLOAT(JULIAN(round(STACK[Z]),round(STACK[Y]),round(STACK[X])));
                  STACKDN;
                  STACKDN;
                ELSIF STRCMPFNT(TOKEN.CHARS,"TODAY") = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACKUP;
                  TIME2MDY(C1,C2,C3);
                  STACK[X] := LFLOAT(JULIAN(C1,C2,C3));
                ELSIF STRCMPFNT(TOKEN.CHARS,"GREG") = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACKUP;
                  STACKUP;
                  GREGORIAN(round(STACK[X]),C1,C2,C3);
                  STACK[Z] := LFLOAT(C1);
                  STACK[Y] := LFLOAT(C2);
                  STACK[X] := LFLOAT(C3);
                ELSIF STRCMPFNT(TOKEN.CHARS,"DOW") = 0 THEN
                  LASTX := STACK[X];
                  PushStacks;
                  STACK[X] := AMOD(LASTX,7.) + 1.;
                ELSIF STRCMPFNT(TOKEN.CHARS,"PI") = 0 THEN
                  PushStacks;
                  PUSHX(pi);
                ELSIF STRCMPFNT(TOKEN.CHARS,"ABOUT") = 0 THEN
                  StringListP := InitStringListPointerType();
                  Strings.Concat(" Last compiled HPCALC.MOD ",LastCompiled,s);
                  AppendStringToList(StringListP,s);
                ELSIF STRCMPFNT(TOKEN.CHARS,"HOL") = 0 THEN
                  year := ROUND(STACK[X]);  (* Leave year on the stack so client modules can reference it.  *)
                  IF year < 40 THEN
                    INC(year,2000);
                  ELSIF year < 100 THEN
                    INC(year,1900);
                  END (* IF *);
                  IF (year >= 1900) AND (year <= 2100) THEN
                    GetHolidays(year,Holidays);
                    Holidays.valid := TRUE;
                  END (* IF *);
                ELSE
                  StringListP := InitStringListPointerType();
                  Strings.Concat(TOKEN.CHARS," is an unrecognized command.",s);
                  AppendStringToList(StringListP,s);
                END(* AllElse main IF statement *);
    END(*CASE*);
  END(*LOOP*);
  R := STACK[X];
  RETURN StringListP;                                                      (*  RETURN(STACK[X]); *)
END GETRESULT;

BEGIN (********************* MAIN ****************************************)
  FOR S := X TO T1 DO STACK[S] := 0.; END(*FOR*);
(*  STACKSIZE  := ORD(T1) + 1; *)
  LASTX := 0.;
  MEMREG := 0.;
  delimSet := CHARSETTYP{BLANK,NULL,SEMCOL};  (* Note absence of COMMA *)
  NEWDELIMSET(delimSet);
  Holidays.valid := FALSE;
  Strings.Assign(" X: ",StackRegNamesString[X]);
  Strings.Assign(" Y: ",StackRegNamesString[Y]);
  Strings.Assign(" Z: ",StackRegNamesString[Z]);
  Strings.Assign("T5: ",StackRegNamesString[T5]);
  Strings.Assign("T4: ",StackRegNamesString[T4]);
  Strings.Assign("T3: ",StackRegNamesString[T3]);
  Strings.Assign("T2: ",StackRegNamesString[T2]);
  Strings.Assign("T1: ",StackRegNamesString[T1]);

END HPCALC.
(*****************************************************)

(*****************************************************)


(*****************************************************)

