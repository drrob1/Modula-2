MODULE TTEST2g;
  FROM MiscStdInOutg IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine,
    WriteInt,WriteCard,WriteLongInt,WriteLongCard,WriteLongRealFixed,WriteLongReal,WriteLongRealFloat;
  IMPORT LongStr, LongConv, StdIO, Strings;
  FROM UTILLIBg IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(*
  FROM TOKENIZEg IMPORT FSATYP,DELIMCH,DELIMSTATE,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR;
*)
(* *)
  FROM TKNRTNSg IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
    GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
(* *)

  TYPE TSNAMEType = ARRAY [0..3] OF STR10TYP;

  CONST TSNAME = TSNAMEType {'DELIM','OP','DGT','ALLELSE'};

  VAR
    INBUF,TOKEN : BUFTYP;
    TKNSTATE       : FSATYP;
(*    delimset,opset,dgtset : CHARSETTYP; *)
    RETCOD,C       : CARDINAL;
    I              : INTEGER;
    L              : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH             : CHAR;
(*    openres        : OpenResults; *)

BEGIN
(* Overrides the defaults
                                                opset := CHARSETTYP{};
                                                NEWOPSET(opset);
                                                delimset := CHARSETTYP{' ',0C};
                                                delimset := CHARSETTYP{};
                                                NEWDELIMSET(delimset);
                                                dgtset := CHARSETTYP{};
                                                NEWDGTSET(dgtset);
*)

  LOOP
    WriteString(' Input line : ');
    ReadString(INBUF.CHARS);
    WriteLn;
    TRIM(INBUF);
    IF STRCMPFNT(INBUF.CHARS,'QUIT') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    INI1TKN(INBUF);
    LOOP
(*     GETTKNREAL(TOKEN,TKNSTATE,L,R,RETCOD); *)
(*      GETTKN(TOKEN,TKNSTATE,L,RETCOD); R := 0.0; *)
(*      GETTKNSTR(TOKEN,I,RETCOD); *)
(*      GETTKNSTR(TOKEN,L,RETCOD); *)
      GETTKNEOL(TOKEN,RETCOD);
      IF RETCOD > 0 THEN
        WriteString("GETTKN's RETCOD is ");
        WriteCard(RETCOD);
        WriteLn;
        EXIT;
      END(*IF*);
      WriteString(TOKEN.CHARS);

      WriteString(', TKNSTATE = ');
      WriteString(TSNAME[ORD(TKNSTATE)]);
(*

                                                              WriteString(', I=');
                                                              WriteInt(I);
*)
      WriteString(', L=');
      WriteLongInt(L);

      WriteString(', R=');
      WriteLongReal(R);
      WriteString('   ');
      WriteLongRealFloat(R,15);


      WriteString(',  DELIMCH= ');
      WriteChar(DELIMCH);
      WriteString(', DELIMSTATE= ');
      WriteString(TSNAME[ORD(DELIMSTATE)]);
      WriteLn;
      WriteString(' Call UNGETTKN? ');
      StdIO.Read(CH);
(*                                                           StdIO.Write(CH); *)
      WriteLn;
      IF CAP(CH) = 'Y' THEN UNGETTKN(RETCOD); END(*IF*);
      IF RETCOD > 0 THEN
        WriteString(' Nonzero RETCOD from UNGETTKN.');
        WriteLn;
      END(*IF*);
    END(*tokenizing LOOP *);
  END(*getting input LOOP *);
END TTEST2g.
