(*
  For this test as of 4/2/2013 7:36:06 PM I am using the compile time boolean tags to see how this goes
  And I'm using both acceptable syntaxes here, just because.
  Remember, to get console mode, in the SBM2 environment go to Linker Options, Win32, check Console App box.
  Before I compile this code, I need to enable more checks on the compiler to see if I've made a mistake that the
  compiler can catch if told to do so.

  And I have to remember to set the console mode to be Windows XP, which I just noticed is not the default.

  10/12/2013 10:45:22 AM  Debugging for gm2.  So removed all compile time boolean tags.
*)
MODULE TTESTptrg;
  IMPORT Strings, StdIO;
  IMPORT MiscStdInOutg, SIOResult;
  FROM REALLIBg IMPORT GETCROPNUM;
  FROM MiscStdInOutg IMPORT WriteCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
    WriteLongReal, WriteLongRealFixed, ReadString, ReadCard, ReadLongReal;
  FROM UTILLIBg IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  IMPORT TOKENPTRg;
  FROM TOKENPTRg IMPORT FSATYP,DELIMCH,DELIMSTATE,INI1TKN,TKNPTRTYP,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;

  TYPE TSNAMEType = ARRAY [0..3] OF STR10TYP;

  CONST TSNAME = TSNAMEType {'DELIM','OP','DGT','ALLELSE'};

  VAR
    INBUF,TOKEN    : BUFTYP;
    TKNSTATE       : FSATYP;
    tpv            : TKNPTRTYP;
    RETCOD,C       : CARDINAL;
    I              : INTEGER;
    L              : LONGINT;
    R,r1,r2,r3,r4,r5,r6  : LONGREAL;
    CH             : CHAR;
    str            : STRTYP;
    rr : SIOResult.ReadResults;

BEGIN

  LOOP
    WriteString(' Input line : ');
    ReadString(INBUF.CHARS);
    WriteLn;
(*
                                          WriteString(' Input line was : ');
                                          WriteString(INBUF.CHARS);
                                          WriteLn;
*)
    TRIM(INBUF);
    IF STRCMPFNT(INBUF.CHARS,'QUIT') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    INI1TKN(tpv,INBUF);

    LOOP

(*
                                                  GETTKNSTR(tpv,TOKEN,I,RETCOD);
                                                  GETTKNREAL(tpv,TOKEN,TKNSTATE,I,R,RETCOD);
                                                  GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD); 
*)
      GETTKNREAL(tpv,TOKEN,TKNSTATE,I,R,RETCOD);

      IF RETCOD > 0 THEN
        WriteString("GETTKN's RETCOD is ");
        WriteCard(RETCOD);
        WriteLn;
        EXIT;
      END(* IF retcod not zero *);
      WriteString(TOKEN.CHARS);

      WriteString(', TKNSTATE = ');
      WriteString(TSNAME[ORD(TKNSTATE)]);
      WriteString(', I=');
      WriteInt(I);

      WriteString(', R=');
      WriteLongReal(R);
      WriteString(', ');
      WriteLongRealFixed(R,20);
      WriteString(', ');
      GETCROPNUM(R,str);
      WriteString(str);
      WriteString(',  DELIMCH= ');
      StdIO.Write(DELIMCH);
      WriteString(', DELIMSTATE= ');
      WriteString(TSNAME[ORD(DELIMSTATE)]);
      WriteLn;
      WriteString(' Call UNGETTKN? ');
      StdIO.Read(CH);
(*      WriteLn; *)

      IF CAP(CH) = 'Y' THEN 
        UNGETTKN(tpv,RETCOD); 
      ELSE
        WriteLn; (* the <enter> from hitting Y is echoed, so only want to writeln when there is no prompt *)
      END(*IF*);

      IF RETCOD > 0 THEN
        WriteString(' Nonzero RETCOD from UNGETTKN.');
        WriteLn;
      END(* IF retcod not zero *);
    END(* LOOP to get tokens, ie, the inner loop *);
  END(* LOOP to get lines, ie, the outer loop *);

END TTESTptrg.
