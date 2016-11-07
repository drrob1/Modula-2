MODULE TestStrIOg;
(*
  REVISION HISTORY
  ----------------
  10 Oct 13 -- Testing terminal read char if it does what it should under gm2.
*)
  IMPORT Strings, StdIO;
  IMPORT MiscStdInOutg;
  FROM MiscStdInOutg IMPORT WriteCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt, 
    WriteLongRealEng, ReadString, ReadCard, ReadLongReal;
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

BEGIN
  LOOP
      StdIO.WriteString(' Enter a single character: ');
      StdIO.Read(CH);
      StdIO.Write(CH); 
      StdIO.WriteLn;
      StdIO.WriteString(' decimal value of entered char is: ');
      NumberIO.WriteCard(ORD(CH));
      WriteLn;
      IF CAP(CH) = 'Y' THEN EXIT; END(*IF*);
  END(*LOOP*);
END TestStrIOg.
