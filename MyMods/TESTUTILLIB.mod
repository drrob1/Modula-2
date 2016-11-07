MODULE TESTUTILLIB;
(*
REVISION HISTORY
----------------
 2 Feb 07 -- Modified testing for tokenptr to test problem found w/ LCHINBUF for CitiFilterTW
*)
  IMPORT Terminal,MiscM2;  (* Need to include real i/o code into MiscM2 *)
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteLongReal;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  IMPORT RConversions, LongStr, LongConv;
  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,BLANK,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(* *)
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE;
(* *)
(*
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
    GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
*)


  TYPE TSNAMEType = ARRAY [0..3] OF STR10TYP;

  CONST TSNAME = TSNAMEType {'DELIM','OP','DGT','ALLELSE'};

  VAR INBUF,TOKEN : BUFTYP;
    TKNSTATE       : FSATYP;
    tpv1,tpv2,tpv3 : TKNPTRTYP;
    RETCOD,C,posn  : CARDINAL;
    I              : INTEGER;
    L              : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH             : CHAR;

BEGIN

  LOOP
    WriteString(' Input line : ');
    ReadString(INBUF.CHARS);
    WriteLn;
    TRIM(INBUF);
    IF STRCMPFNT(INBUF.CHARS,'quit') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    posn := LCHINBUFFNT(INBUF,"'");
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
END TESTUTILLIB.
