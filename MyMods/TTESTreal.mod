MODULE TTESTreal;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts;
  IMPORT RConversions, LongStr, LongConv;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
(*  FROM TermFile IMPORT Open, IsTermFile, Close; *)

  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,BLANK,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(* *)
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,DELIMSTATE,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR;
(* *)
(*
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
    GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
*)

TYPE
    ChanId      = IOChan.ChanId;
    FlagSet     = ChanConsts.FlagSet;
    OpenResults = ChanConsts.OpenResults;

  (* Accepted singleton values of FlagSet *)

CONST
  read  = FlagSet{ChanConsts.readFlag}; (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag};(* output operations are requested/available *)
  text  = FlagSet{ChanConsts.textFlag}; (* text operations are requested/available *)
  raw   = FlagSet{ChanConsts.rawFlag};  (* raw operations are requested/available *)
  echo  = FlagSet{ChanConsts.echoFlag}; (* echoing by interactive device on reading of characters from input stream requested/applies *)

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
    cid            : ChanId;
    flags          : FlagSet;
    openres        : OpenResults;

BEGIN
(*
  opset := CHARSETTYP{};
  NEWOPSET(opset);
(*  delimset := CHARSETTYP{' ',0C}; *)
  delimset := CHARSETTYP{};
(*  NEWDELIMSET(delimset); *)
  dgtset := CHARSETTYP{};
  NEWDGTSET(dgtset);
*)
(*  Not necessary, I think?
  flags := write + text;  (* For terminal writing of reals *)
  Open(cid,flags,openres);
*)

  LOOP
    WriteString(' Input line : ');
    ReadString(INBUF.CHARS);
    SkipLine;
    WriteLn;
    TRIM(INBUF);
    IF STRCMPFNT(INBUF.CHARS,'QUIT') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    INI1TKN(INBUF);
    LOOP
      GETTKNREAL(TOKEN,TKNSTATE,I,R,RETCOD);
(*      GETTKN(TOKEN,TKNSTATE,I,RETCOD); *)
(*      GETTKNSTR(TOKEN,I,RETCOD); *)
(*      GETTKNSTR(TOKEN,L,RETCOD); *)
      IF RETCOD > 0 THEN
        WriteString("GETTKN's RETCOD is ");
        WriteCard(RETCOD,0);
        WriteLn;
        EXIT;
      END(*IF*);
      WriteString(TOKEN.CHARS);

      WriteString(', TKNSTATE = ');
      WriteString(TSNAME[ORD(TKNSTATE)]);
      WriteString(', I=');
      WriteInt(I,0);

(*
      WriteString(', L=');
      WriteLongInt(L,0);
*)
(*
PROCEDURE WriteFloat(real : REAL; sigFigs : CARDINAL; width : CARDINAL);
  (* Writes the value of real to the default output channel in
     floating-point text form, with sigFigs significant figures, in a
     field of the given minimum width.  *)
PROCEDURE WriteReal(real : REAL; width : CARDINAL);
  (* Writes the value of real to the default output channel, as WriteFixed
     if the sign and magnitude can be shown in the given width, or
     otherwise as WriteFloat.  The number of places or significant digits
     depends on the given width.  *)
*)     
      WriteString(', R=');
      WriteReal(R,15);
      WriteString(' fixed, float: ');
      WriteFixed(R,0,0);
      WriteString('   ');
      WriteFloat(R,10,0);

      WriteString(',  DELIMCH= ');
      WriteChar(DELIMCH);
      WriteString(', DELIMSTATE= ');
      WriteString(TSNAME[ORD(DELIMSTATE)]);
      WriteLn;
      WriteString(' Call UNGETTKN? ');
      ReadChar(CH);
      SkipLine;
(*      WriteChar(CH); *)
      WriteLn;
      IF CAP(CH) = 'Y' THEN UNGETTKN(RETCOD); END(*IF*);
      IF RETCOD > 0 THEN
        WriteString(' Nonzero RETCOD from UNGETTKN.');
        WriteLn;
      END(*IF*);
    END(*LOOP*);
  END(*LOOP*);
(*  Close(cid); *)
END TTESTreal.