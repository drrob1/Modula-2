MODULE AsciiHex;

FROM MiscM2 IMPORT CLS, ReadString, ReadCard, WriteString, WriteLn, WriteCard, PressAnyKey,
                   Error;

IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;

(****************************************************************************)

  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt (*, ReadCard, WriteCard*);
  IMPORT STextIO;
(*  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine; *)
  FROM Terminal IMPORT Read, (*WriteString, WriteLn, Write, *) ReadChar, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;


CONST  ordzero = ORD('0');
       ordA    = ORD('A');
       orda    = ORD('a');

TYPE HexPair = RECORD uch,Lch : CHAR END;
     HexArray130 = ARRAY [1..30] OF HexPair;

VAR s1,s2 : STRTYP;
    hex  : BOOLEAN;
    i,j,h,len,newlen,c1,c2,temp  : CARDINAL;
    ch   : CHAR;
    ha   : HexArray130;


PROCEDURE CleanHexString(VAR INOUT str: ARRAY OF CHAR);
(*********************************************************************)
VAR s                       : ARRAY [0..255] OF CHAR;
    i                       : INTEGER;
    c,upper,inner,outer,len : CARDINAL;

BEGIN
  upper := HIGH(str);
  str[upper] := NULL;  (* Sentinal NULL for my algorithm *)
  len := LENGTH(str);
  outer := 0;
  inner := 0;
  WHILE (outer <= len) DO
    IF (((CAP(str[outer])>='A') AND (CAP(str[outer])<='H')) OR
                                             ((str[outer]>='0') AND (str[outer]<='9')) ) THEN
    s[inner] := str[outer];
    INC(inner);
    END;
  INC(outer);
  END;
  s[inner] := NULL;
  FOR c := 0 TO inner DO
    str[c] := s[c];
  END;
(*  str[inner+1] := NULL; *)
END CleanHexString;

(********************************************************************************)
PROCEDURE GetCardDgt(ch:CHAR) : CARDINAL;
BEGIN
  IF ch <= '9' THEN
    RETURN ORD(ch) - ordzero;
  ELSIF ch <= 'F' THEN
    RETURN ORD(ch) - ordA + 10;
  ELSE
    RETURN ORD(ch) - orda + 10;
  END;
END GetCardDgt;

PROCEDURE GetHexDgt(n:CARDINAL) : CHAR;
BEGIN
  IF n > 9 THEN
    RETURN CHR(n+ordA-10)
  ELSE
    RETURN CHR(n+ordzero)
  END;
END GetHexDgt;


(********************************** Main body ************************************)
BEGIN
  Terminal.Reset;
  LOOP
        Terminal.WriteString(' Enter string to clean: ');
        ReadString(s1);
        IF CAP(s1[1]) = 'Q' THEN EXIT END;
        Terminal.WriteLn;
        Terminal.WriteString(' string as is: ');
        Terminal.WriteString(s1);
        Terminal.WriteLn;
(*
        Terminal.WriteString(' Enter card to test GetHexDgt: ');
        STextIO.SkipLine;
        ReadCard(temp);
        Terminal.WriteLn;
        ch := GetHexDgt(temp);
        Terminal.WriteString(' HexDgt is: ');
        Terminal.Write(ch);
        Terminal.WriteLn;
        Terminal.WriteString(' Enter Hex dgt to test GetCardDgt: ');
        Terminal.Read(ch);
        Terminal.Write(ch);
        Terminal.WriteLn;
        temp := GetCardDgt(ch);
        Terminal.WriteString(' Cardinal is: ');
        WriteCard(temp);
        Terminal.WriteLn;
        PressAnyKey;
*)
        
        len := LENGTH(s1);
        IF CAP(s1[len]) = 'H' THEN   (* Hex -> Ascii *)
          CleanHexString(s1);
          Terminal.WriteString(' Cleaned string is: ');
          Terminal.WriteString(s1);
          Terminal.WriteLn;
          s1[len] := NULL;
          DEC(len);
          IF ODD(len) THEN 
            Error(' Have an Odd string length.  Must be an error.');
            EXIT
          END;
          c1 := 1;
          newlen := len/2;
          FOR c2 := 1 TO newlen DO 
            ha[c2].uch := s1[c1];
            INC(c1);
            ha[c2].Lch := s1[c1];
            INC(c1);
          END;
          FOR c2 := 1 TO newlen DO
            s2[c2] := CHR(GetCardDgt(ha[c2].uch)*16 + GetCardDgt(ha[c2].Lch));
          END;
          s2[newlen+1] := NULL;
          WriteString(' Output string is ');
          WriteCard(newlen);
          WriteString(' characters long, and is: ');
          WriteString(s2);
          WriteLn;
        ELSE                          (* Ascii -> Hex *)
          (* do nothing yet *)
        END;
  END (*loop*);
END AsciiHex.
