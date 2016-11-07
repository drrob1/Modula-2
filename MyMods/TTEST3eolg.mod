MODULE TTEST3eol;
  IMPORT Terminal,MiscM2;
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteReal,WriteLongReal;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
(*  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard; *)
  IMPORT RConversions, LongStr, LongConv;
  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,BLANK,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(* *)
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,GETTKNEOL,DELIMCH,DELIMSTATE;
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
    IF STRCMPFNT(INBUF.CHARS,'QUIT') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    INI1TKN(tpv1,INBUF);
    LOOP
      GETTKNREAL(tpv1,TOKEN,TKNSTATE,I,R,RETCOD);
(*      GETTKN(tpv1,TOKEN,TKNSTATE,I,RETCOD);  R := 0.; *)

(*      GETTKNEOL(tpv1,TOKEN,RETCOD); *)
(*      GETTKNSTR(TOKEN,L,RETCOD); *)
      IF RETCOD > 0 THEN
        WriteString("GETTKN's RETCOD is ");
        WriteCard(RETCOD);
        WriteLn;
        EXIT;
      END(*IF*);
      WriteString(TOKEN.CHARS);

      WriteString(', TKNSTATE = ');
      WriteString(TSNAME[ORD(TKNSTATE)]);

      WriteString(', I=');
      WriteInt(I);
(*
      WriteString(', L=');
      WriteLongInt(L,0);
*)
      WriteString(', R=');
      WriteLongReal(R,15);



      WriteString(',  DELIMCH= ');
      Terminal.Write(DELIMCH);
      WriteString(', DELIMSTATE= ');
      WriteString(TSNAME[ORD(DELIMSTATE)]);
      WriteLn;
      WriteString(' Call UNGETTKN? ');
      Terminal.Read(CH);
      Terminal.Write(CH);
      WriteLn;
      IF CAP(CH) = 'Y' THEN UNGETTKN(tpv1,RETCOD); END(*IF*);
      IF RETCOD > 0 THEN
        WriteString(' Nonzero RETCOD from UNGETTKN.');
        WriteLn;
      END(*IF*);
    END(*LOOP for tokens*);
  END(*LOOP for lines *);
  IF tpv1 # NIL THEN DISPOSE(tpv1); END;
END TTEST3eol.
