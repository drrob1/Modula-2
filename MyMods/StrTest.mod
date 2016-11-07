MODULE StrTest;
(*
Created 2/14/2009 5:13 PM to test FindNext and see if it returns 0-origin vs 1-origin
*)
  IMPORT Terminal,MiscM2,String;
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteReal,WriteLongReal;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  IMPORT RConversions, LongStr, LongConv;
  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,BLANK,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
(* *)
  IMPORT TOKENPTR;
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
    pattern,line   : STRTYP;
    tpv1,tpv2,tpv3 : TKNPTRTYP;
    RETCOD,C,idx,pospat       : CARDINAL;
    I              : INTEGER;
    L              : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH             : CHAR;
    FOUND          : BOOLEAN;

BEGIN

  LOOP
    WriteString(' Input line : ');
    ReadString(line);
    WriteLn;
    IF STRCMPFNT(line,'QUIT') = 0 THEN EXIT; END(*IF*);
    IF LENGTH(line) = 0 THEN EXIT; END(*IF*);
    REPEAT
(*
PROCEDURE FindNext(pattern, stringToSearch : ARRAY OF CHAR; startIndex : CARDINAL; VAR OUT patternFound : BOOLEAN;
                   VAR OUT posOfPattern : CARDINAL);
*)
      WriteString(' pattern:');
      ReadString(pattern);
      WriteLn;
      Strings.FindNext(pattern,line,idx,FOUND,pospat);
      IF FOUND THEN
      	WriteString(' pattern found at position ');
      	WriteCard(pospat);
      	WriteLn;
      ELSE
      	WriteString(' pattern not found.');
      	WriteLn;
      END;
      WriteString(' Again? ');
      Terminal.Read(CH);
      Terminal.Write(CH);
      WriteLn;
    UNTIL CAP(CH) = 'Y';
  END(*LOOP for lines *);
END StrTest.
