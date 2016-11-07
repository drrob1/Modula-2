(*R-*) (* Range as in subrange and aritmetic checks *)
(*T-*) (* Testing for arrays, case and NIL pointer  *)
MODULE THP;
(*
  This module tests the HP type RPN calculator simulation.
*)
  IMPORT Break;
  FROM Terminal IMPORT ReadString;
  IMPORT Terminal;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct;
  FROM RealConversions IMPORT RealToString, StringToReal;
  FROM RealInOut IMPORT ReadReal, WriteReal, Done;
  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT,NULL;
  FROM UL2 IMPORT COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    UNGETTKN,GETTKNREAL;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;
(*
  FROM FIO IMPORT DRIVESEP,SUBDIRSEP,MYFILTYP,IOSTATE,FRESET,FCLOSE,
    FRDTXLN,FWRTXLN,FWRTX,RETBLKBUF,EXTRACTDRVPTH,FCLOSEDUP,FWRLN;
  FROM REALLIB IMPORT AINT,AMOD,ROUND;
  FROM TIMLIB IMPORT GETNEWDATE,TIME2MDY,MDY2TIME,MDY2STR,GREG2JUL,
    JUL2GREG,GETMDY;
*)

VAR 
    C,K   : CARDINAL;
    R     : REAL;
    INBUF : BUFTYP;
    stk1  : ARRAY [1..5] OF REAL;
    stk2  : ARRAY [1..STACKSIZE] OF REAL;

BEGIN (********************* MAIN ****************************************)
  GETSTACK(stk1,C);
  WriteString(' This number should be 1.  Number =');
  WriteCard(C,0);
  WriteLn;

  LOOP (* Until finished with input *)
    WriteString(' Enter input test string : ');
    ReadString(INBUF.CHARS);
    WriteLn;
    TRIM(INBUF);
    IF INBUF.COUNT = 0 THEN EXIT; END(*IF*);
    INI1TKN(INBUF);
    R := GETRESULT();
    DUMPSTACK;
    WriteString(' Result =');
    WriteReal(R,20);
    WriteLn;
  END(*LOOP*);
  WriteLn;
  WriteLn;
  GETSTACK(stk2,C);
  WriteString(' This number should be 0.  Number =');
  WriteCard(C,0);
  WriteLn;
  WriteString(' Last but not least, the final stack values are:');
  WriteLn;
  FOR K := 1 TO 6 DO WriteString('  '); WriteReal(stk2[K],10); END(*FOR*);
  WriteLn;
END THP.

