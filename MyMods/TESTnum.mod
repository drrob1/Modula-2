MODULE TESTnum;
  IMPORT Terminal,MiscM2;
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  IMPORT RConversions, LongStr, LongConv, WholeStr;
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
    RETCOD,C       : CARDINAL;
    str            : STRTYP;
    I              : INTEGER;
    L              : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH             : CHAR;

PROCEDURE ExtractLastNum(tkn : BUFTYP; VAR str : STRTYP; VAR c: CARDINAL); 
(*
Needed for processing the Citibank csv file.  I will extract the last token.
If it is a number, c will = its value, else c=0.
*)

VAR tpv : TKNPTRTYP;
     i  : INTEGER;
    tknState : FSATYP;
    RETCOD : CARDINAL;

BEGIN
	INI1TKN(tpv,tkn);
	REPEAT
		GETTKN(tpv,tkn,tknState,i,RETCOD);
	UNTIL DELIMCH = NULL;

	IF tknState = DGT THEN
	  c := ABS(i);
	  str := tkn.CHARS;
	ELSE
		c := 0;
		str := "0000";
	END;
  IF tpv # NIL THEN DISPOSE(tpv); END;
END ExtractLastNum;

BEGIN

  LOOP
    WriteString(' Input line : ');
    ReadString(INBUF.CHARS);
    WriteLn;
    TRIM(INBUF);
    IF STRCMPFNT(INBUF.CHARS,'QUIT') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    ExtractLastNum(INBUF,str,C);
      WriteString(' Str = ');
      WriteString(str);

      WriteString(', C=');
      WriteCard(C);

      WriteLn;
      WriteLn;
  END(*LOOP for lines *);
(*  IF tpv1 # NIL THEN DISPOSE(tpv1); END; *)
(*  DISPOSE(tpv1); *)
END TESTnum.
