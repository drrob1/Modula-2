(*
  For this test as of 4/2/2013 7:36:06 PM I am using the compile time boolean tags to see how this goes
  And I'm using both acceptable syntaxes here, just because.
  Remember, to get console mode, in the SBM2 environment go to Linker Options, Win32, check Console App box.
  Before I compile this code, I need to enable more checks on the compiler to see if I've made a mistake that the
  compiler can catch if told to do so.

  And I have to remember to set the console mode to be Windows XP, which I just noticed is not the default.

  25 Apr 20 -- Testing conversion to LONGINTs
*)
<*DEFINE (ConsoleMode,TRUE) *>
<*DEFINE (TokenPtr,TRUE) *>
<*DEFINE (TestReal,TRUE) *>
<*DEFINE (TestStr,FALSE) *>
<*DEFINE (TestSingleChar,FALSE) *>
MODULE TestTOKENPTR;
  IMPORT Strings;
  IMPORT Terminal, BasicDialogs, STextIO, SLWholeIO;
%IF ConsoleMode %THEN
    IMPORT MiscStdInOut, SIOResult;
    FROM MiscStdInOut IMPORT WriteCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, ReadString, SkipLine, ReadCard, ReadLongReal;
%ELSE
    IMPORT MiscM2;
    FROM MiscM2 IMPORT WriteCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
      WriteReal, WriteLongReal, WriteChar, ReadChar, Read, ReadString, ReadCard, ReadLongReal;
%END
  FROM BasicDialogs IMPORT MessageTypes;
  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,BLANK,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  IMPORT TOKENPTR, TOKENIZE, TKNRTNS;
<*IF TokenPtr THEN*>
    FROM TOKENPTR IMPORT FSATYP,DELIMCH,DELIMSTATE,INI1TKN,TKNPTRTYP,
      INI3TKN,GETCHR,UNGETCHR,GETTKN,GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;

<*ELSE*>
    FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
      INI3TKN,GETCHR,UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,
      GETTKNSTR,GETTKNEOL,UNGETTKN,GETTKNREAL;
<*END*>

  TYPE TSNAMEType = ARRAY [0..3] OF STR10TYP;

  CONST TSNAME = TSNAMEType {'DELIM','OP','DGT','ALLELSE'};

  VAR
    INBUF,TOKEN    : BUFTYP;
    TKNSTATE       : FSATYP;
<* IF TokenPtr THEN *>
    tpv            : TKNPTRTYP;
<* END *>
    RETCOD,C       : CARDINAL;
   (*                                 I              : INTEGER;  *)
    I              : LONGINT;

    L              : LONGINT;
%IF TestReal %THEN
    R,r1,r2,r3,r4,r5,r6  : LONGREAL;
%END
    CH             : CHAR;
    str            : STRTYP;
    %IF ConsoleMode %THEN
      rr : SIOResult.ReadResults;
    %END

BEGIN

  LOOP
    WriteString(' Input line : ');
    ReadString(INBUF.CHARS);
    WriteLn;
    %IF ConsoleMode %THEN
      WriteString(' Input line was : ');
      WriteString(INBUF.CHARS);
    %END
    WriteLn;
    TRIM(INBUF);
    IF STRCMPFNT(INBUF.CHARS,'QUIT') = 0 THEN EXIT; END(*IF*);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);

<* IF TokenPtr THEN *>
    (* INI1TKN(tpv,INBUF);  I have not needed to use the INI3TKN since I was playing w/ ANWORD, which was decades ago. *)
    TOKENPTR.INITKN(tpv,INBUF);
<* ELSE *>
    INI1TKN(INBUF);
<* END *>

    LOOP

<* IF TestStr AND TokenPtr THEN *>
      GETTKNSTR(tpv,TOKEN,i,RETCOD);
<* ELSIF TestReal AND TokenPtr THEN *>
      GETTKNREAL(tpv,TOKEN,TKNSTATE,L,R,RETCOD);
<* ELSIF TokenPtr THEN *>
      GETTKN(tpv,TOKEN,TKNSTATE,L,RETCOD);
<* ELSIF TestStr THEN*>
      GETTKNSTR(TOKEN,L,RETCOD);
<* ELSIF TestReal THEN*>
      GETTKNREAL(TOKEN,TKNSTATE,L,R,RETCOD);
<* ELSE*>
      GETTKN(TOKEN,TKNSTATE,L,RETCOD);
<* END *>

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
      SLWholeIO.WriteLongInt(L, 0);

      WriteString(',  DELIMCH= ');
      WriteChar(DELIMCH);
      WriteString(', DELIMSTATE= ');
      WriteString(TSNAME[ORD(DELIMSTATE)]);
      WriteLn;
      <* IF TestReal THEN *>
      WriteString(" R= ");
      WriteLongReal(R, 8);
      WriteLn;
      <* END *>
      WriteString(' Call UNGETTKN? ');
(*
      REPEAT
         STextIO.ReadChar(CH);       This code does not work, as it does not wait for input.  ReadString does wait for input.
      UNTIL rr = SIOResult.allRight;

This also does not work as it also does not wait for input on the 2nd+ calls, but it does for the 1st.  Will test to see if setting
correct console mode makes a difference.
      ReadChar(CH);
      WriteChar(CH);
*)
<* IF TestSingleChar AND ConsoleMode THEN*>
      ReadChar(CH);
      WriteChar(CH);
<* ELSE *>
      ReadString(str);
      WriteLn;
<* END *>

      CH := str[1];

<* IF TokenPtr THEN *>
      IF CAP(CH) = 'Y' THEN UNGETTKN(tpv,RETCOD); END(*IF*);
<* ELSE *>
      IF CAP(CH) = 'Y' THEN UNGETTKN(RETCOD); END(*IF*);
<* END *>

      IF RETCOD > 0 THEN
        WriteString(' Nonzero RETCOD from UNGETTKN.');
        WriteLn;
      END(* IF retcod not zero *);
    END(* LOOP to get tokens, ie, the inner loop *);
  END(* LOOP to get lines, ie, the outer loop *);

END TestTOKENPTR.
