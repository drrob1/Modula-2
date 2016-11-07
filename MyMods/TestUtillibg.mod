MODULE TestUtillibg;
(*
REVISION HISTORY
----------------
 2 Feb 07 -- Modified testing for tokenptr to test problem found w/ LCHINBUF for CitiFilterTW
10 Oct 13 -- Testing gm2 routines.
*)
  FROM SYSTEM IMPORT ADR;
  FROM MiscStdInOutg IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteCard;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIBg IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT,MINCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(*
  FROM TOKENPTRg IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE;
*)
(*
  FROM TKNRTNSg IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
    GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  IMPORT RConversions, LongStr, LongConv;
*)
FROM Environg IMPORT GetCommandLine;


  TYPE TSNAMEType = ARRAY [0..3] OF STR10TYP;

  CONST TSNAME = TSNAMEType {'DELIM','OP','DGT','ALLELSE'};

  VAR INBUF,TOKEN : BUFTYP;
(*
    TKNSTATE       : FSATYP;
    tpv1,tpv2,tpv3 : TKNPTRTYP;
*)
    RETCOD,C,posn,c1,c2    : CARDINAL;
    I                      : INTEGER;
    L                      : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH                     : CHAR;

BEGIN

  LOOP
    WriteString(' Input line : ');
    ReadString(INBUF.CHARS);
    WriteLn;
(*
    WriteString(' length of input string is: ');
    WriteCard(STRLENFNT(INBUF.CHARS));
    PressAnyKey;
*)
    TRIM(INBUF);
    WriteString(' INBUF.COUNT is ');
    WriteCard(INBUF.COUNT);
    PressAnyKey;
    IF STRCMPFNT(INBUF.CHARS,'quit') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    posn := LCHINBUFFNT(INBUF,"e");
    WHILE posn > 0 DO
      WriteString(' posn=');
      WriteCard(posn);
      WriteString(', count=');
      WriteCard(INBUF.COUNT);
      WriteLn;
      
      RMVCHR(INBUF,posn,1);
      WriteString(' after rmvchr call:  ');
      WriteString(INBUF.CHARS);
      WriteString(', count=');
      WriteCard(INBUF.COUNT);
      WriteLn;
      posn := LCHINBUFFNT(INBUF,"'");
      PressAnyKey;
    END;
  END (* loop *);
  FILLCHAR(ADR(TOKEN.CHARS),20,'0');
  WriteString(' FillChar test with 0: ');
  WriteString(TOKEN.CHARS);
  WriteLn;

  WriteString(' MinCard and MaxCard tests.  Enter card 1: ');
  ReadCard(c1);
  WriteString(', Enter Card 2: ');
  ReadCard(c2);
  WriteLn;
  WriteString(' Max: ');
  WriteCard(MAXCARDFNT(c1,c2));
  WriteString(', Min: ');
  WriteCard(MINCARDFNT(c1,c2));
  WriteLn;
END TestUtillibg.
