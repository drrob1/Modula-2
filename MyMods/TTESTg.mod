MODULE TTESTg;
(*
  REVISION HISTORY
  ----------------
  10 Oct 13 -- converted to gm2.
*)
  IMPORT Strings, StdIO;

  IMPORT MiscStdInOutg;
  FROM MiscStdInOutg IMPORT WriteCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt, 
    WriteLongRealFloat, WriteLongRealEng, WriteLongRealFixed, ReadString, ReadCard, ReadLongReal;
(*
                       FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
                       FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
                       FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
                       IMPORT IOChan, ChanConsts;
                       IMPORT RConversions, LongStr, LongConv;
                       FROM SRealIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
*)
  FROM UTILLIBg IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,BLANK,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(*
                                             IMPORT TOKENPTRg;
*)
  FROM TOKENIZEg IMPORT FSATYP,DELIMCH,DELIMSTATE,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR;
(*
                                FROM TKNRTNSg IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
                                  INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
                                  GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
*)
  TYPE TSNAMEType = ARRAY [0..3] OF STR10TYP;

  CONST TSNAME = TSNAMEType {'DELIM','OP','DGT','ALLELSE'};

  VAR INBUF,TOKEN : BUFTYP;
    TKNSTATE       : FSATYP;
(*    delimset,opset,dgtset : CHARSETTYP; *)
    RETCOD,C       : CARDINAL;
    I              : INTEGER;
    L              : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH             : CHAR;
    s              : STRTYP;

BEGIN
(*
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
(*                           GETTKNREAL(TOKEN,TKNSTATE,I,R,RETCOD); *)
(*                           GETTKN(TOKEN,TKNSTATE,I,RETCOD); *)
    GETTKNSTR(TOKEN,I,RETCOD);
(*                           GETTKNSTR(TOKEN,L,RETCOD); *)
      IF RETCOD > 0 THEN
        WriteString("GETTKN's RETCOD is ");
        WriteCard(RETCOD);
        WriteLn;
        EXIT;
      END(*IF*);
      WriteString(TOKEN.CHARS);
(*
                             WriteString(', TKNSTATE = ');
                             WriteString(TSNAME[ORD(TKNSTATE)]);
*)
      WriteString(', I=');
      WriteInt(I);

(*
                            WriteString(', L=');
                            WriteLongInt(L,0);
*)
(*
                            WriteString(', fixed,eng,float R=');
      WriteLn;
      WriteString(' R= ');
      WriteLongRealFixed(R,15);
      WriteString(' : ');
      WriteLongRealEng(R,15);
      WriteString(' : ');
      WriteLongRealFloat(R,15);
      MiscStdInOutg.LongRealToString(R,s,14);
      WriteString(', ');
      WriteString(s);
      WriteString(', ');
      MiscStdInOutg.LongRealToStr(R,s);
      WriteString(s);
      WriteLn;
*)

      WriteString(',  DELIMCH= ');
      StdIO.Write(DELIMCH);
      WriteString(', DELIMSTATE= ');
      WriteString(TSNAME[ORD(DELIMSTATE)]);
      WriteLn;
      WriteString(' Call UNGETTKN? ');
      StdIO.Read(CH);
      StdIO.Write(CH); 
      WriteLn;
      IF CAP(CH) = 'Y' THEN UNGETTKN(RETCOD); END(*IF*);
      IF RETCOD > 0 THEN
        WriteString(' Nonzero RETCOD from UNGETTKN.');
        WriteLn;
      END(*IF*);
    END(*LOOP*);
  END(*LOOP*);
END TTESTg.
