(* (C) 1990.  Robert W. Solomon.  All rights reserved.  *)
MODULE HPW;
(*
  This module uses the HPCALC module to simulate an RPN type calculator.
  REVISION HISTORY
  ----------------
   1 Dec 89 -- Changed prompt.
  24 Dec 91 -- Converted to M-2 V 4.00.  Changed params to GETRESULT.
  25 Jul 93 -- Output result without trailing insignificant zeros,
                imported UL2, and changed prompt again.
   3 Mar 96 -- Fixed bug in string display if real2str fails because
                number is too large (ie, Avogadro's Number).
  18 May 03 -- First Win32 version.  And changed name.
*)
  FROM SYSTEM IMPORT ADR;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM Environment IMPORT GetCommandLine;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT;
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
  FROM REALLIB IMPORT AINT,ROUND,AMOD,PWRI,GETCROPNUM;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;

VAR
    C,K,STRLEN,NON0POSN,NONBLPOSN : CARDINAL;
    R                             : LONGREAL;
    INBUF                         : BUFTYP;
    STR1,STR2,STR3                : STRTYP;
    OKAY                          : BOOLEAN;
    stk1                          : ARRAY [1..5] OF LONGREAL;
    stk2                          : ARRAY [1..8] OF LONGREAL;

BEGIN (********************* MAIN ****************************************)
(*
  GETSTACK(stk1,C);
  WriteString(' This number should be 1.  Number =');
  WriteCard(C,0);
  WriteLn;
*)
  LOOP (* Until finished with input *)
    WriteString(' Enter calculation, HELP or Enter to exit: ');
    ReadString(INBUF.CHARS);
    SkipLine;
    WriteLn;
    TRIM(INBUF);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
(* done in GETRESULT    INI1TKN(INBUF);*)
    R := GETRESULT(INBUF);
(*    DUMPSTACK; *)
    WriteString(' Result =');
    WriteReal(R,20);
    K := 0;
    RealToStringFixed(R,25,10,STR1,K,OKAY);
    IF OKAY THEN
      WriteString(',       ');
      STRLEN := STRLENFNT(STR1);
(* Scanback for first nonzero char.  This is the new length of the string *)
      NON0POSN := SCANBACK(ADR(STR1),STRLEN,'0',FALSE);
      STR1[NON0POSN+1] := NULL; (* Terminate string at 1-st insignificant 0 *)
      STR2 := STR1;
      NONBLPOSN := SCANFWD(ADR(STR1),NON0POSN,BLANK,FALSE);
(*
  Remove the leading blanks by copying the non-blank string to the beginning,
  including the terminating null char.
*)
      COPYLEFT(ADR(STR1[NONBLPOSN]),ADR(STR1),NON0POSN-NONBLPOSN+2);
      WriteString(STR1);
(*
      C := NON0POSN;
      WHILE C < STRLEN DO (* Blank out insig zeros, w/o changing len of str *)
        INC(C);
        STR2[C] := BLANK;
      END(*WHILE*);
      WriteString('.');
      WriteString(STR2);
      WriteString('.');
      GETCROPNUM(R,STR3);
      WriteString(STR3);
      WriteString('.');
*)
    END(*IF*);
    WriteLn;
  END(*LOOP*);
(*
  WriteLn;
  GETSTACK(stk2,C);
  WriteString(' This number should be 0.  Number =');
  WriteCard(C,0);
  WriteLn;
  WriteString(' Last but not least, the final stack values are:');
  WriteLn;
  FOR K := 1 TO STACKSIZE DO
    WriteString('  ');
    WriteReal(stk2[K],10);
    IF (K MOD 4) = 0 THEN WriteLn; END(*IF*);
  END(*FOR*);
  WriteLn;
*)
END HPW.
(*
DEFINITION MODULE SysClock;

(* Facilities for accessing a system clock that records the date and time of day *)

CONST
  maxSecondParts = <implementation-defined integral value>;

TYPE
  Month    = [1 .. 12];
  Day      = [1 .. 31];
  Hour     = [0 .. 23];
  Min      = [0 .. 59];
  Sec      = [0 .. 59];
  Fraction = [0 .. maxSecondParts];
  UTCDiff  = [-780 .. 720];
  DateTime =
    RECORD
      year:      CARDINAL;
      month:     Month;
      day:       Day;
      hour:      Hour;
      minute:    Min;
      second:    Sec;
      fractions: Fraction;      (* parts of a second *)
      zone:      UTCDiff;       (* Time zone differential factor which is the number
                                   of minutes to add to local time to obtain UTC. *)
      summerTimeFlag: BOOLEAN;  (* Interpretation of flag depends on local usage. *)
    END;

PROCEDURE CanGetClock (): BOOLEAN;
  (* Returns TRUE if a system clock can be read; FALSE otherwise *)

PROCEDURE CanSetClock (): BOOLEAN;
  (* Returns TRUE if a system clock can be set; FALSE otherwise *)

PROCEDURE IsValidDateTime (userData: DateTime): BOOLEAN;
  (* Returns TRUE if the value of userData represents a valid date and time; FALSE otherwise *)

PROCEDURE GetClock (VAR userData: DateTime);
  (* If possible, assigns system date and time of day to userData *)

PROCEDURE SetClock (userData: DateTime);
  (* If possible, sets the system clock to the values of userData *)

END SysClock.

PROCEDURE StringToInt(str : ARRAY OF CHAR;
                      VAR INOUT pos : CARDINAL;
                      VAR OUT num : INTEGER;
                      VAR OUT done : BOOLEAN);
(* Get an integer number from a string starting at position pos. *)
(* pos is left pointing at the first character that is not part of the *)
(* number. done signifies the success of the conversion *)
(* Skips any leading spaces. *)

PROCEDURE StrToInt(buf : ARRAY OF CHAR; VAR OUT num : INTEGER) : BOOLEAN;
(* Convert an integer number from a string  *)
(* Skips any leading spaces. *)

PROCEDURE IntToString(num : INTEGER;
                      size : CARDINAL;
                      VAR OUT str : ARRAY OF CHAR;
                      VAR INOUT pos : CARDINAL;
                      VAR OUT done : BOOLEAN);
(* Convert an integer number into a string starting at position *)
(* pos.  pos is left pointing to the character following the number *)
(* done signifies the success of the conversion *)

PROCEDURE IntToStr(num : INTEGER; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* Convert an integer number to a string  *)

PROCEDURE StringToCard(str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT num : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Get a cardinal number from a string starting at position pos. *)
(* pos is left pointing at the first character that is not part of the *)
(* number *)
(* done signifies the success of the conversion *)
(* Skips any leading spaces. *)

PROCEDURE StrToCard(buf : ARRAY OF CHAR; VAR OUT num : CARDINAL) : BOOLEAN;
(* Convert a string in buf to a cardinal *)
(* Skips any leading spaces. *)

PROCEDURE CardToString(num : CARDINAL;
                       size : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Convert a cardinal number into the string starting at position *)
(* pos.  pos is left pointing to the character following the number *)
(* done signifies the success of the conversion *)

PROCEDURE CardToStr(num : CARDINAL; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* Convert a cardinal to a string *)

PROCEDURE StrBaseToCard(str : ARRAY OF CHAR;
                        base : CARDINAL;
                        VAR OUT num : CARDINAL32) : BOOLEAN;
(* convert a string to a cardinal32 using the number base specified in base *)
(* (base >= 2) AND (base <= 16) *)
(* Skips any leading spaces. *)

PROCEDURE CardBaseToStr(num : CARDINAL32;
                        base : CARDINAL;
                        VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* convert a cardinal32 to a string using the number base specified in base *)
(* (base >= 2) AND (base <= 16) *)

PROCEDURE StringToLong(str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT num : LONGINT;
                       VAR OUT done : BOOLEAN);
(* Get an longint number from a string buf starting at position pos. *)
(* pos is left pointing at the first character that is not part of the *)
(* number *)
(* done signifies the success of the conversion *)
(* Skips any leading spaces. *)

PROCEDURE StrToLong(buf : ARRAY OF CHAR; VAR OUT num : LONGINT) : BOOLEAN;
(* Convert a longint number from a string  *)
(* Skips any leading spaces. *)

PROCEDURE LongToString(num : LONGINT;
                       size : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Convert a longint number into the string starting at position *)
(* pos.  pos is left pointing to the character following the number *)
(* done signifies the success of the conversion *)

PROCEDURE LongToStr(num : LONGINT; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* Convert a longint to a string *)

PROCEDURE StrBaseToLong(str : ARRAY OF CHAR;
                        base : CARDINAL;
                        VAR OUT num : LONGCARD) : BOOLEAN;
(* convert a string to a longcard using the number base specified in base *)
(* (base >= 2) AND (base <= 16) *)
(* Skips any leading spaces. *)

PROCEDURE LongBaseToStr(num : LONGCARD;
                        base : CARDINAL;
                        VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* convert a longcard to a string using the number base specified in base *)
(* (base >= 2) AND (base <= 16) *)

END Conversions.
PROCEDURE RealToString(num : LONGREAL;
                       digits : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Convert a real number into a string buf starting at position pos. *)
(* pos is left pointing at the next character after the converted number *)
(* done signifies the success of the conversion *)

PROCEDURE RealToStringFixed(num : LONGREAL;
                            digits, before : CARDINAL;
                            VAR OUT str : ARRAY OF CHAR;
                            VAR INOUT pos : CARDINAL;
                            VAR OUT done : BOOLEAN);
(* Convert a real number into a string buf starting at position pos using *)
(* fixed point notation with digits digits and before digits before the *)
(* decimal point *)
(* done signifies the success of the conversion *)
*)
