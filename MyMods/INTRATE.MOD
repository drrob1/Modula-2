MODULE INTRATE;
(*
  REVISION HISTORY
  ----------------
*)
  FROM SYSTEM IMPORT ADR,ADDRESS;
  IMPORT Break;
  IMPORT MED;
  FROM Terminal IMPORT ReadString;
  IMPORT Terminal;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM LongIO IMPORT ReadLongInt, WriteLongInt;
  FROM Conversions IMPORT ConvertCardinal, ConvertInteger;
  FROM RealConversions IMPORT RealToString, StringToReal;
  FROM RealInOut IMPORT ReadReal, WriteReal, Done;
  FROM LongMathLib0 IMPORT longreal,exp,ln;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT;
  FROM UL2 IMPORT COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF,GETFNM;
  FROM FIO IMPORT DRIVESEP,SUBDIRSEP,MYFILTYP,IOSTATE,FRESET,FCLOSE,
    FRDTXLN,FWRTXLN,FWRTX,RETBLKBUF,EXTRACTDRVPTH,FCLOSEDUP,FWRLN,
    FWRSTR;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    GETTKNREAL,UNGETTKN;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR,FLOATLONG,TRUNCLONG;
  FROM TIMLIB IMPORT GETNEWDATE,TIME2MDY,MDY2TIME,MDY2STR,GREG2JUL,
    JUL2GREG,GETMDY;
  FROM TIMER IMPORT READTIMER;

  VAR 
    FINI                                           : BOOLEAN;
    OUTUN1                                         : MYFILTYP;
    iHP,iMY,iNEXTHP,iNEXTMY,iANNUALHP,iANNUALMY    : LONGREAL;
    CH                                             : CHAR;
    FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLAG           : BOOLEAN;
    PROMPT,NAMDFT,TYPDFT,OUTFNAM                   : BUFTYP;
    ACUMINT,PV,PMT,PERINT,ANNUALINT,TOTPRICE,DNPMT : LONGREAL;
    NUMPER,loopctr                                 : CARDINAL;
    OUTSTR,TITLE,STR1,STR2,STR3,STR4               : STRTYP;

PROCEDURE HPMETHOD(i:LONGREAL) : LONGREAL;
(*
  Do one iteration of Newton's method to get next interest estimate 
  (EstIntHP), using HP's algebra.
  Uses the global PV, PMT, and NUMPER variables.
*)
VAR P,Q,EstIntHP,iPREV,NEXTTERM : LONGREAL;
  NEGn : INTEGER;
BEGIN
  iPREV := 1. + i;
  NEGn := -1 * INTEGER(NUMPER);
  P := i*PV/PMT - (1. - PWRI(iPREV,NEGn));
  Q := PWRI(iPREV,NEGn) * (Float(NUMPER)/(1.+1./i) + 1.);
  NEXTTERM := P/((Q-1.)/i);
  EstIntHP := i + NEXTTERM;
  RETURN(EstIntHP);
END HPMETHOD;

PROCEDURE MYMETHOD(i:LONGREAL) : LONGREAL;
(*
  Do one iteration of Newton's method to get next interest estimate
  (EstIntMy), using my algebra.
  Uses the global PV, PMT, and NUMPER variables.
*)
VAR EstIntMy,iPREV,NEXTTERM : LONGREAL;
  NEGn : INTEGER;

BEGIN
  iPREV := 1. + i;
  NEGn := -1 * INTEGER(NUMPER);
  NEXTTERM := ( (1. - PWRI(iPREV,NEGn)) - i*PV/PMT ) / 
    ( Float(NUMPER)*PWRI(iPREV,NEGn-1) + PWRI(iPREV,NEGn)/i - 1./i );
  EstIntMy := i - NEXTTERM;
  RETURN(EstIntMy);
END MYMETHOD;

PROCEDURE MY2ndMETHOD(i:LONGREAL) : LONGREAL;
(*
  Do one iteration of Newton's method to get next interest estimate
  (EstIntMy), using my calculus buy not my algebra.
  Uses the global PV, PMT, and NUMPER variables.
*)
VAR EstIntMy2nd,iPREV,NEXTTERM,FofX,dFofX : LONGREAL;
  NEGn : INTEGER;

BEGIN
  iPREV := 1. + i;
  NEGn := -1 * INTEGER(NUMPER);
  FofX := (1. - PWRI(iPREV,NEGn))/i - PV/PMT;
  dFofX  := (i*Float(NUMPER)*PWRI(iPREV,NEGn-1) - (1. - PWRI(iPREV,NEGn)))
             /(i*i);
  NEXTTERM := FofX / dFofX;
  EstIntMy2nd := i - NEXTTERM;
  RETURN(EstIntMy2nd);
END MY2ndMETHOD;

PROCEDURE PerIntRate(PV,PMT : LONGREAL; N : INTEGER) : LONGREAL;
(*
  Iterative solution to find the periodic interest rate, i, which is not
  necessarily the annual interest rate (unless the period is a year).

           PMT     PV
   i   =   ÄÄÄ  -  ÄÄÄÄÄÄÄÄÄ
    0      PV      N^2 * PMT

                 f(i)
   i   =   i  -  ÄÄÄÄÄ
    k+1     k    f'(i)

                    -n
           1 - (1+i)     PV
  f(i) =   ÄÄÄÄÄÄÄÄÄÄÄ - ÄÄÄ
               i         PMT

                    -n-1              -n
           i N (i+i)     - [ 1 - (1+i)
 f'(i) =   ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
                      2
                     i
*)

CONST
  TolFac = 1.E-12;  (* Tolerance Factor *)

VAR
  i,iPlus1,NextTerm,Tolerance : LONGREAL;
  NegN                        : INTEGER;

BEGIN
  i := PMT/PV - PV/Float(N)/Float(N)/PMT;
  NegN := -N;
  Tolerance := i * TolFac;
  REPEAT
    iPlus1   := i + 1.;
    NextTerm := ( (1. - PWRI(iPlus1,NegN)) - i*PV/PMT ) / 
           ( Float(N)*PWRI(iPlus1,NegN-1) + PWRI(iPlus1,NegN)/i - 1./i );
    i := i - NextTerm;
  UNTIL ABS(NextTerm) < Tolerance;
  RETURN(i);
END PerIntRate;

PROCEDURE BAL(N:CARDINAL) : LONGREAL;
(*
*************************** BAL ****************************************
BAL FUNCTION.
COMPUTES THE BALANCE OF THE PRINCIPLE OF THE MORTGAGE REMAINING AFTER
PAYMENT N.
*)

VAR RESULT : REAL;

BEGIN
  RESULT := PWRI(1.0 + PERINT,N) * 
       (((1.0/PWRI(1.0+PERINT,N) - 1.0)/PERINT) * PMT + PV);
  RETURN(RESULT);
END BAL;

  
BEGIN (* MAIN PROGRAM *)
(* 
  GET FILE NAMES FROM TERMINAL, USING THE GETFNM SUBROUTINE. 
*)
  ASSIGN2BUF('ENTER OUTPUT FILE NAME : ',PROMPT);
  ASSIGN2BUF('INTRATE.OUT',NAMDFT);
  ASSIGN2BUF('.OUT',TYPDFT);
  GETFNM(PROMPT, NAMDFT, TYPDFT, OUTFNAM); 
  
  FRESET(OUTUN1,OUTFNAM,WR);

  WriteString('OUTPUT FILE NAME : ');
  WriteString(OUTFNAM.CHARS);
  WriteLn;

  LOOP
    WriteString('INPUT NUMBER OF PERIODS (MONTHS) : ');
    ReadCard(NUMPER);
    WriteLn;
(*    NUMPER := 360;*)
    IF NUMPER = 0 THEN EXIT END(*IF*);

    WriteString('INPUT TITLE FOR THIS RUN OF IntRate : ');
    ReadString(TITLE);
    WriteLn;

    WriteString('  INPUT AMOUNT TO FINANCE : ');
    ReadReal(PV);
    IF NOT Done OR (PV = 0.) THEN EXIT END(*IF*);
    WriteLn;
    WriteString('INPUT ANNUAL INTEREST RATE AS A %-age : ');
    ReadReal(ANNUALINT);
    IF NOT Done THEN EXIT END(*IF*);
    WriteLn;
(*
    PV := 76800.;
    ANNUALINT := 6.95;
*)
    PERINT := ANNUALINT / 12.0 / 100.0;
    PMT := PV * PERINT/(1.0 - 1.0/PWRI(1.0+PERINT,NUMPER));
(*
  ACUMINT = BAL(K) - BAL(J-1) + PMT*FLOAT(K - J + 1);
*)
    ACUMINT := BAL(12) - BAL(0) + PMT*12.0;
    WriteLn;
    WriteLn;
    WriteString(TITLE);
    WriteLn;
    FWRLN(OUTUN1);
    FWRLN(OUTUN1);

    FWRSTR(OUTUN1,TITLE);
    FWRLN(OUTUN1);

    WriteString(' The amount to finance is $');
    FWRSTR(OUTUN1,' The amount to finance is $');
    RealToString(PV,2,11,OUTSTR,FINI);
    WriteString(OUTSTR);
    WriteLn;
    FWRSTR(OUTUN1,OUTSTR);
    FWRLN(OUTUN1);
    WriteString('Payment for ');
    WriteCard(NUMPER,0);
    WriteString(' month loan at ');
    FWRSTR(OUTUN1,'Payment for ');
    ConvertCardinal(NUMPER,4,OUTSTR);
    FWRSTR(OUTUN1,OUTSTR);
    FWRSTR(OUTUN1,' month loan at ');
    RealToString(ANNUALINT,2,7,OUTSTR,FINI);
    WriteString(OUTSTR);
    WriteString('% is : $');
    FWRSTR(OUTUN1,OUTSTR);
    FWRSTR(OUTUN1,'% is : $');
    RealToString(PMT+0.005,2,15,OUTSTR,FINI);
    WriteString(OUTSTR);
    WriteLn;
    FWRSTR(OUTUN1,OUTSTR);
    FWRLN(OUTUN1);
    iHP := PMT/PV - PV/Float(NUMPER)/Float(NUMPER)/PMT;
    iMY := iHP;
    WriteString(' Initial i = ');
    RealToString(iHP,15,25,OUTSTR,FINI);
    WriteString(OUTSTR);
    WriteLn;
    FWRSTR(OUTUN1,' Initial i = ');
    FWRSTR(OUTUN1,OUTSTR);
    FWRLN(OUTUN1);
(*
    WriteString(' Press any key to continue: ');
    Read(CH);
    WriteLn;
*)
    loopctr := 0;
    WriteString('    HP METHOD                     MY METHOD');
    WriteLn;
    WriteString('    =========                     =========');
    WriteLn;
    FWRSTR(OUTUN1,'   HP METHOD                     MY METHOD');
    FWRLN(OUTUN1);
    FWRSTR(OUTUN1,'   =========                     =========');
    FWRLN(OUTUN1);
    LOOP
      iNEXTHP := HPMETHOD(iHP);
      iNEXTMY := MYMETHOD(iMY);
(*      iNEXTMY := MY2ndMETHOD(iMY);*)
      iANNUALHP := iNEXTHP * 12.;
      iANNUALMY := iNEXTMY * 12.;
      RealToString(iNEXTHP,10,15,STR1,FINI);
      WriteString(STR1);
      RealToString(iANNUALHP,10,15,STR2,FINI);
      WriteString(STR2);
      RealToString(iNEXTMY,10,15,STR3,FINI);
      WriteString(STR3);
      RealToString(iANNUALMY,10,15,STR4,FINI);
      WriteString(STR4);
      WriteLn;
      FWRSTR(OUTUN1,STR1);
      FWRSTR(OUTUN1,STR2);
      FWRSTR(OUTUN1,STR3);
      FWRSTR(OUTUN1,STR4);
      FWRLN(OUTUN1);
      INC(loopctr);
      IF ((ABS(iHP-iNEXTHP) < 1.E-6*iHP) AND (ABS(iMY-iNEXTMY) < 1.E-6*iMY))
                                                       OR (loopctr > 40) THEN
        EXIT;
      END(*IF*);
      iHP := iNEXTHP;
      iMY := iNEXTMY;
    END(*Inner LOOP*);
    WriteString(' Periodic i is ');
    iMY := PerIntRate(PV,PMT,NUMPER);
    RealToString(iMY,10,15,STR1,FINI);
    WriteString(STR1);
    WriteString('.  Annual i is');
    RealToString(iMY*12.,10,15,STR2,FINI);
    WriteString(STR2);
    RealToString(iMY*1200.,2,6,STR3,FINI);
    WriteString(', ');
    WriteString(STR3);
    WriteString('%');
    WriteLn;
    FWRSTR(OUTUN1,' Periodic i is ');
    FWRSTR(OUTUN1,STR1);
    FWRSTR(OUTUN1,'.  Annual i is');
    FWRSTR(OUTUN1,STR2);
    FWRSTR(OUTUN1,', ');
    FWRSTR(OUTUN1,STR3);
    FWRSTR(OUTUN1,'%');
    FWRLN(OUTUN1);
  END(*Outer LOOP*);
  FCLOSE(OUTUN1);
END INTRATE.
