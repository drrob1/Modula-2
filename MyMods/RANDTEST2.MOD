<*/NOWARN:F*>
MODULE RANDTEST2;
(*
  REVISION HISTORY
  ----------------
  24 Oct 03 -- Converted to SBM2 V 4.0 for Win32
  20 May 06 -- Start testing Ran4 using pseudo DES algorithm.
*)

(*
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK, CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
*)
  FROM RAN4 IMPORT randcard,randint,randreal,psdes,ran4;
IMPORT MMSYSTEM;
IMPORT MiscM2;

(*
FROM TIMER IMPORT READTIMER;
*)
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR;
IMPORT WINUSER, WIN32, WINGDI, WINX;
IMPORT Storage;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT SysMets;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
(*
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    UNGETTKN,GETTKNREAL;
*)
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
(****************************************************************************)
(*
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
*)
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
(*
  IMPORT RConversions, LongStr, LongConv;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc;
*)

  CONST SEED = 6280;

  VAR
    BUF1,BUF2,BUF3 : BUFTYP;
    OK,STOP        : BOOLEAN;
    CH1,CH2,CH3    : CHAR;
    I1,I2,I3       : INTEGER;
    L1,L2,L3       : LONGINT;
    C1,C2,C3,c4    : CARDINAL;
    R1,R2,R3       : REAL;
    s1,s2,s3       : STR10TYP;
    HRS,MINS,SECS  : CARDINAL;
    TOTALMSECS     : LONGINT;

(*********************************************************************)
PROCEDURE Card2HexStr(L : CARDINAL; VAR OutStr : ARRAY OF CHAR);
(*********************************************************************)
CONST  ASCZERO = ORD('0');
       ascA    = ORD('A');
VAR i,j,h  : CARDINAL;
    Str10  : STR10TYP;

BEGIN
  i := 0;
  REPEAT (* until L = 0 *)
    h := L MOD 16;
    IF (h <= 9) THEN Str10[i] := CHR(h + ASCZERO) ELSE Str10[i] := CHR(h -10 + ascA) END;
    INC(i);
    L := L DIV 16;
  UNTIL L = 0;
  j := 1;  (* first posn is a space to leave room for sign char *)
  OutStr[0] := ' ';
  REPEAT (* until i = 0 *)
    DEC(i);
    OutStr[j] := Str10[i];
    INC(j);
  UNTIL i = 0;
  OutStr[j] := 0C;
END Card2HexStr;


BEGIN (******************************** MAIN ****************************)
  STOP := FALSE;
(* testing psdes and ran4
  C1 := 1;
  C2 := 1;
  psdes(C1,C2);
  MiscM2.WriteString(' psdes(1,1) =');
  Card2HexStr(C1,s1);
  MiscM2.WriteString(s1);
  MiscM2.WriteString(', ');
  Card2HexStr(C2,s2);
  MiscM2.WriteString(s2);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;

  C2 := 99;
  psdes(C1,C2);
  MiscM2.WriteString(' psdes(1,99) =');
  Card2HexStr(C1,s1);
  MiscM2.WriteString(s1);
  MiscM2.WriteString(', ');
  Card2HexStr(C2,s2);
  MiscM2.WriteString(s2);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;

  C1 := 99;
  psdes(C1,C2);
  MiscM2.WriteString(' psdes(99,99) =');
  Card2HexStr(C1,s1);
  MiscM2.WriteString(s1);
  MiscM2.WriteString(', ');
  Card2HexStr(C2,s2);
  MiscM2.WriteString(s2);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;

  C2 := 1;
  psdes(C1,C2);
  MiscM2.WriteString(' psdes(99,1) =');
  Card2HexStr(C1,s1);
  MiscM2.WriteString(s1);
  MiscM2.WriteString(', ');
  Card2HexStr(C2,s2);
  MiscM2.WriteString(s2);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;

  I1 := -1;
  R1 := ran4(I1);
  MiscM2.WriteString(' ran4(-1) =');
  MiscM2.WriteReal(R1,5);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;

  I3 := 99;
  R2 := ran4(I3);
  MiscM2.WriteString(' ran4(99) =');
  MiscM2.WriteReal(R2,5);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;

  I2 := -99;
  R3 := ran4(I2);
  MiscM2.WriteString(' ran4(-99) =');
  MiscM2.WriteReal(R3,5);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;

  R1 := ran4(I3);
  MiscM2.WriteString(' ran4(99) =');
  MiscM2.WriteReal(R1,5);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;

*)




  c4 := 0;
  LOOP
    MiscM2.WriteString(' RandReal Call.'); MiscM2.WriteLn;
    FOR C1 := 0 TO 9 DO
      R1 := randreal();
      MiscM2.WriteReal(R1,5);
      MiscM2.WriteString('  ');
      RealToStr(R1,s1);
      MiscM2.WriteString(s1);
      IF ODD(C1) THEN MiscM2.WriteLn; ELSE MiscM2.WriteString('  '); END(*IF*);
    END(*FOR*);
    MiscM2.WriteLn;
    MiscM2.PressAnyKey;

    MiscM2.WriteString(' RandCard(0) Call.'); MiscM2.WriteLn;
    FOR C1 := 1 TO 10 DO
      C2 := randcard(0);
      MiscM2.WriteCard(C2);
      IF (C1 MOD 5) = 0 THEN MiscM2.WriteLn; ELSE MiscM2.WriteString('  '); END;
    END(*FOR*);
    MiscM2.PressAnyKey;

    MiscM2.WriteString(' RandCard(100) Call.'); MiscM2.WriteLn;
    FOR C1 := 1 TO 10 DO
      C3 := randcard(100);
      MiscM2.WriteCard(C3);
      IF (C1 MOD 5) = 0 THEN MiscM2.WriteLn; ELSE MiscM2.WriteString('  '); END;
    END(*FOR*);
    MiscM2.PressAnyKey;

    MiscM2.WriteString(' RandInt(0) Call.'); MiscM2.WriteLn;
    FOR I1 := 1 TO 10 DO
      I2 := randint(0);
      MiscM2.WriteInt(I2);
      IF (I1 MOD 5) = 0 THEN MiscM2.WriteLn; ELSE MiscM2.WriteString('  '); END;
    END(*FOR*);
    MiscM2.PressAnyKey;

    MiscM2.WriteString(' RandInt(50) Call.'); MiscM2.WriteLn;
    FOR I1 := 1 TO 10 DO
      I3 := randint(50);
      MiscM2.WriteInt(I3);
      IF (I1 MOD 5) = 0 THEN MiscM2.WriteLn; ELSE MiscM2.WriteString('  '); END;
    END(*FOR*);
    MiscM2.PressAnyKey;

    MiscM2.WriteString(' More?');
    MiscM2.ReadString(s3);
    IF CAP(s3[0]) = 'N' THEN EXIT;
    ELSIF c4 >= 5 THEN
        EXIT;
    END(*IF*);
    INC(c4);
  END(*LOOP*);
END RANDTEST2.
