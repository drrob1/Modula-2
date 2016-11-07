MODULE TestClean;

FROM MiscM2 IMPORT CLS, ReadString, WriteString, WriteLn, PressAnyKey;

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
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
(*  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine; *)
  FROM Terminal IMPORT Read, (*WriteString, WriteLn, Write, *) ReadChar, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;


VAR s1,s2 : STRTYP;
    flag  : BOOLEAN;

PROCEDURE CleanRealString(VAR INOUT str: ARRAY OF CHAR);
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
    IF ((str[outer]='.') OR (CAP(str[outer])='E') OR ((str[outer]>='0') AND (str[outer]<='9')) ) THEN 
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
END CleanRealString;

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
        flag := BasicDialogs.PromptString(' Enter string to clean: ',s1);
        IF NOT flag THEN EXIT END;
*)
        CleanRealString(s1);
        Terminal.WriteString(' Cleaned string is: ');
        Terminal.WriteString(s1);
        Terminal.WriteLn;
(*        PressAnyKey; *)
  END (*loop*);
END TestClean.
