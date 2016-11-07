MODULE NEWJUL;
(*
  REVISION HISTORY
  ----------------
*)
  FROM SYSTEM IMPORT ADR;
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
  FROM Directories IMPORT Delete,DirResult;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;
  FROM TimeDate IMPORT Time,GetTime;
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT;
  FROM UL2 IMPORT COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF,GETFNM;
  FROM FIO IMPORT DRIVESEP,SUBDIRSEP,MYFILTYP,IOSTATE,FRESET,FCLOSE,
    FRDTXLN,FWRTXLN,FWRTX,RETBLKBUF,EXTRACTDRVPTH,FCLOSEDUP,FWRLN;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    GETTKNREAL,UNGETTKN;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR,FLOATLONG,TRUNCLONG;
  FROM TIMLIB IMPORT GETNEWDATE,TIME2MDY,MDY2TIME,MDY2STR,GREG2JUL,
    JUL2GREG,GETMDY,DAYNAMES,JULIAN,GREGORIAN;
  FROM TIMER IMPORT READTIMER;
  
  TYPE
(*    DAYNAMESType = ARRAY [1..7] OF STR10TYP; *)
    ADIPMType = ARRAY [0..11] OF INTEGER;
  CONST
(*
    DAYNAMES : DAYNAMESType = ['Sunday', 'Monday', 'Tuesday',  'Wednesday',
                               'Thursday', 'Friday', 'Saturday'];
*)
(*
  This is a typed constant that represents the difference btwn the last day
  of the previous month and 30, assuming each month was 30 days long.
  The variable name is an acronym of Accumulated Days In Previous Months.
*)
    ADIPM : ADIPMType = [0,1,-1,0,0,1,1,2,3,3,4,4];

  VAR 
    FINI,GOOD                                           : BOOLEAN;
    C,K,RETCOD,RETCOD2,CMDNUM,MM,DD,YY,YEAR,DOW,HOLNUM  : CARDINAL;
    M1,D1,Y1,M2,D2,Y2,M0,D0,Y0                          : CARDINAL;
    I,J,SUM                                             : INTEGER;
    CH                                                  : CHAR;
    STPPRG,HAVEMATCH                                    : BOOLEAN;
    PROMPT,NAMDFT,TYPDFT,OUTFNAM,INBUF,OUTBUF,TOKEN     : BUFTYP;
    X,Y,Z,J1,R,JD1                                      : LONGREAL;
    OUTSTR,STR1,STR2,STR3                               : STRTYP;    
    TKNSTATE,CHRSTATE                                   : FSATYP;
    J2,JD2                                              : LONGINT;
(*
PROCEDURE JULIAN(M,D,Y : CARDINAL) : LONGINT;

VAR
  M0,D0,Y0   : LONGINT;
  Juldate    : LONGINT;

BEGIN
  IF (M < 1) OR (M > 12) OR (D < 1) OR (D > 31) OR (Y < 1900) OR (Y > 2500)
                                                                        THEN
(* Month, Day or Year is out of range *)
    Juldate := 0;
    RETURN(Juldate);
  END(*IF*);

  M0 := LONG(M - 1);
  Y0 := LONG(Y - 1);

  Juldate :=  Y0*365     (* Number of days in previous normal years *)
            + Y0 DIV 4   (* Number of possible leap days *)
            - Y0 DIV 100 (* Subtract all century years *)
            + Y0 DIV 400 (* Add back the true leap century years *)
            + LONG(ADIPM[M0]) + M0*30 + LONG(D);
  IF ((( Y MOD 4 = 0) AND ( Y MOD 100 <> 0)) OR ( Y MOD 400 = 0)) AND
(*   123            3     3               32    2              21      *)
                                                                 (M > 2) THEN
    INC(Juldate);
  END(*IF*);
  RETURN Juldate;
END JULIAN;

PROCEDURE GREGORIAN(Juldate : LONGINT; VAR M,D,Y : CARDINAL);

VAR Y0   : CARDINAL;
    M0   : [1..12];
    D0   : [1..31];
    L,JD : LONGINT;

BEGIN
  Y0 := Juldate DIV 365;
  M0 := 1;
  D0 := 1;

  WHILE JULIAN(M0,D0,Y0) > Juldate DO DEC(Y0); END(*WHILE*);

  M0 := 12;
  WHILE JULIAN(M0,D0,Y0) > Juldate DO DEC(M0); END(*WHILE*);

  WHILE JULIAN(M0,D0,Y0) < Juldate DO INC(D0); END(*WHILE*);

  M := M0;
  D := D0;
  Y := Y0;
END GREGORIAN;
*)
BEGIN
  LOOP
    WriteString('Input M D Y : ');
    ReadString(INBUF.CHARS);
    WriteLn;
    TRIM(INBUF);
    GETMDY(INBUF,MM,DD,YY);
    IF (YY < 1600) THEN EXIT; END(*IF*);
    GREG2JUL(MM,DD,YY,J1);
    J2 := JULIAN(MM,DD,YY);
    WriteString('Julian date by traditional method : ');
    RealToString(J1,1,11,STR1,GOOD);
    WriteString(STR1);
    WriteString(';   New method : ');
    WriteLongInt(J2,0);
    WriteString('.');
    WriteLn;
    WriteString(' DOW by traditional method : ');
    DOW := Round(Frac(J1/7.)*7.)+1;
    WriteString(DAYNAMES[DOW]);
    WriteString(';   DOW by new method : ');
    K := J2 MOD 7 + 1;
    WriteString(DAYNAMES[K]);
    WriteLn;
    GREGORIAN(J2,M2,D2,Y2);
    WriteString('Attempted inversion yields ');
    WriteCard(M2,0);
    WriteString('/');
    WriteCard(D2,0);
    WriteString('/');
    WriteCard(Y2,0);
    WriteString(';  True inversion would be ');
    JUL2GREG(J1,M1,D1,Y1);
    WriteCard(M1,0);
    WriteString('/');
    WriteCard(D1,0);
    WriteString('/');
    WriteCard(Y1,0);
    WriteLn;
    WriteLn;
  END(*LOOP*);
  M0 := 3;
  D0 := 1;
  Y0 := 1900;
  GREG2JUL(M0,D0,Y0,JD1);
  J1 := JD1;
  JD2 := JULIAN(M0,D0,Y0);
  J2 := JD2;
  K := 0;
  C := 0;
  LOOP
    J1 := J1 + 1.;
    INC(J2);
    JUL2GREG(J1,M1,D1,Y1);
    GREGORIAN(J2,M2,D2,Y2);
    IF J2 MOD 1000 = 0 THEN
      WriteString('1000th M/D/Y = ');
      WriteCard(M2,0);
      WriteString('/');
      WriteCard(D2,0);
      WriteString('/');
      WriteCard(Y2,0);
      INC(K);
      IF K MOD 3 = 0 THEN WriteLn; ELSE WriteString('; '); END(*IF*);
    END(*IF*);
    IF (M1 <> M2) OR (D1 <> D2) OR (Y1 <> Y2) THEN
      WriteString(' The two functions now differ.  HP :');
      WriteCard(M1,0);
      WriteString('/');
      WriteCard(D1,0);
      WriteString('/');
      WriteCard(Y1,0);
      WriteString(';  New : ');
      WriteCard(M2,0);
      WriteString('/');
      WriteCard(D2,0);
      WriteString('/');
      WriteCard(Y2,0);
      WriteLn;
      INC(C);
(*      IF C MOD 2 = 0 THEN WriteLn; ELSE WriteString('; '); END(*IF*); *)
      IF C = 20 THEN EXIT; END(*IF*);
    END(*IF*);
  END(*LOOP*);
END NEWJUL.
