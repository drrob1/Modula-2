MODULE CALold;
(*  Copyright (C) 1987  Robert Solomon MD.  All rights reserved. *)
(*
  REVISION HISTORY
  ----------------
   6 Apr 88 -- 1) Converted to M2 V3.03.
               2) Response to 12 page question is now echoed to the terminal.
               3) Module name changed to CAL so not to conflict with the
                   Logitech's CALENDAR library module.
   4 Nov 90 -- Updated the UTILLIB references to UL2, and recompiled under
               V3.4.
  28 Oct 91 -- Added FSA parse and indiv month printing abilities.
   2 Nov 91 -- Fixed problem w/ Zeller's congruence when Las2dgts is small
                enough to make the expression evaluate to a negative value.
  20 Jan 92 -- First page now does not begin with a FF.
*)
  IMPORT Break;
  IMPORT Terminal;
  FROM Terminal IMPORT ReadString;
  FROM InOut IMPORT ReadCard,Read,WriteString,Write,WriteLn;
  FROM Conversions IMPORT ConvertCardinal, ConvertInteger;
  FROM FIO IMPORT DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRBL,FWRSTR,FWRLN,FCLOSEDUP,COPYDPIF,FAPPEND;
  FROM UTILLIB IMPORT BUFSIZ,BUFTYP,STR10TYP;
  FROM UL2 IMPORT COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF,GETFNM;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    GETTKNREAL,UNGETTKN;
  FROM TIMLIB IMPORT GETNEWDATE,TIME2MDY,MDY2TIME,MDY2STR,GREG2JUL,
    JUL2GREG,GETMDY;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;

  CONST
      LNSIZ      = 80;
      LNPPAG     = 60; (* Used by PRMONTH proc.  Top 6 lines used by 
                          Month and day names. *)
      FF         = 14C; (* CHR(12) *)
      CR         = 15C; (* CHR(13) *)
      ESC        = 33C; (* CHR(27) *)
      NULL       = 0C;
      BLANKCHR   = ' ';
      BOXCHRVERT = '|';
      BOXCHRHORZ = '_';

  TYPE 
    MNEnum  = (JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DCM);
    CALMODE = (FINI,CAL1,CAL12);
    LNTYP   = ARRAY [0..LNSIZ] OF CHAR;

  VAR
    OUTUN1                                          : MYFILTYP;
    PROMPT,NAMDFT,TYPDFT,BLANKBUF,OUTFNAM,INPUT,BUF : BUFTYP;
    MN, MN2, MN3                               : MNEnum; (*Month Number Vars*)
    ANS, CH, PAGCHR                                 : CHAR;
    MONTH                        : ARRAY [JAN..DCM],[1..6],[1..7] OF STR10TYP;
    YEAR,IYEAR,LAS2DI,FIR2DI,I,J,
                       K,DOW,W,JAN1DOW,FEBDAYS : CARDINAL;
    DIM, WIM                                   : ARRAY[JAN..DCM] OF CARDINAL;
    CALSTATE                                   : CALMODE;
    MONNAMSHORT                                : ARRAY[JAN..DCM] OF STR10TYP;
    MONNAMLONG                                 : ARRAY[JAN..DCM] OF LNTYP;
    DAYSNAMLONG, DAYSNAMSHORT, TOPLINE, BotLnWithMonth,
                    BOTMLINE, MIDLINE, ANYLINE : LNTYP;
    BLANKSTR2,YEARSTR                          : STR10TYP;
    RETCOD   : CARDINAL;
    INTVAL   : INTEGER;
    TOKEN    : BUFTYP;
    TKNSTATE : FSATYP;
    JULDATE  : LONGREAL;


PROCEDURE DAY2STR(DAY : CARDINAL; VAR STR : STR10TYP);
(*
********************* DAY2STR *****************************************
DAY TO STRING CONVERSION.
THIS ROUTINE WILL CONVERT THE 2 DIGIT DAY INTO A 2 CHAR STRING.
IF THE FIRST DIGIT IS ZERO, THEN THAT CHAR WILL BE BLANK.
*)

CONST
    ZERO = '0';

VAR
    TENSDGT, UNTSDGT : CARDINAL;

BEGIN
  TENSDGT := DAY DIV 10;
  UNTSDGT := DAY MOD 10;
  IF TENSDGT = 0 THEN
    STR[0] := BLANKCHR;
  ELSE
    STR[0] := CHR(TENSDGT + ORD(ZERO));
  END(*IF*);
  STR[1] := CHR(UNTSDGT + ORD(ZERO));
  STR[2] := NULL;
END DAY2STR;

PROCEDURE DATEASSIGN;
(*
********************* DATEASSIGN *******************************************
DATE ASSIGNMENT FOR MONTH.
THIS ROUTINE WILL ASSIGN THE DATES FOR AN ENTIRE MONTH.  IT WILL PUT THE CHAR
REPRESENTATIONS OF THE DATE IN THE FIRST 2 BYTES.  THE EXTRA BYTES CAN BE USED 
LATER FOR SEPCIAL PRINTER CONTROL CODES.

INPUT FROM GBL VAR'S : MN, DIM(MN), DOW
OUTPUT TO  GBL VAR'S : MONTH(MN,,), WIM(MN)
*)

VAR
    I,J,DATE,W : CARDINAL;
    DATESTR : STR10TYP;

BEGIN
  IF DOW <= 7 THEN
    W := 1;
  ELSE   (* DOW = 8 *)
    W := 0;
  END(*IF*);
  FOR DATE := 1 TO DIM[MN] DO
    IF DOW > 7 THEN
      INC(W);
      DOW := 1;
    END(*IF*);
    DAY2STR(DATE,DATESTR);
    MONTH[MN,W,DOW] := DATESTR;
    INC(DOW);
  END(*FOR*);
  WIM[MN] := W;  (* Return number of weeks in this month *)
END DATEASSIGN;

PROCEDURE PRMONTH(MN : MNEnum);

BEGIN
  FWRSTR(OUTUN1,PAGCHR);
  FWRBL(OUTUN1,35);  (* Line displacement for month name *)
  FWRSTR(OUTUN1,MONNAMSHORT[MN]);
  FWRSTR(OUTUN1,BLANKSTR2);
  FWRSTR(OUTUN1,YEARSTR);
  FWRLN(OUTUN1);
  FWRLN(OUTUN1);
  FWRSTR(OUTUN1,DAYSNAMLONG);
  FWRLN(OUTUN1);
  FWRSTR(OUTUN1,TOPLINE);
  FWRLN(OUTUN1);
  FOR W := 1 TO WIM[MN] DO
    FWRSTR(OUTUN1,BLANKSTR2);
    FWRSTR(OUTUN1,MONTH[MN,W,1]);
    FWRBL(OUTUN1,6);
    FOR I := 2 TO 6 DO
      FWRSTR(OUTUN1,BLANKSTR2);
      FWRSTR(OUTUN1,MONTH[MN,W,I]);
      FWRBL(OUTUN1,8);
    END(*FOR*);
    FWRSTR(OUTUN1,BLANKSTR2);
    FWRSTR(OUTUN1,MONTH[MN,W,7]);
    FWRSTR(OUTUN1,CR);
    FOR J := 1 TO LNPPAG DIV WIM[MN] - 1 DO
      FWRSTR(OUTUN1,ANYLINE);
      FWRLN(OUTUN1);
    END(*FOR*);
    IF W = WIM[MN] THEN
      BotLnWithMonth := BOTMLINE;
      BotLnWithMonth[34] := ' ';
      I := 35;
      FOR J := 0 TO STRLENFNT(MONNAMSHORT[MN])-1 DO
        BotLnWithMonth[I] := MONNAMSHORT[MN,J];
        INC(I);
      END(*FOR*);
      BotLnWithMonth[I] := ' ';
      INC(I);
      FOR J := 0 TO STRLENFNT(YEARSTR)-1 DO
        BotLnWithMonth[I] := YEARSTR[J];
        INC(I);
      END(*FOR*);
      BotLnWithMonth[I] := ' ';
      FWRSTR(OUTUN1,BotLnWithMonth);
    ELSE
      FWRSTR(OUTUN1,MIDLINE);
    END(*IF*);
    FWRLN(OUTUN1);
  END(*FOR*);
  PAGCHR := FF;
END PRMONTH;
  
(*
********************* MAIN *********************************************
*)
BEGIN
  BLANKSTR2 := '  ';
  WriteString('Calendar Printing Program.  Last Update 2 Nov 91.');
  WriteLn;
  ASSIGN2BUF(' Enter Output File Name : ',PROMPT);
  ASSIGN2BUF('CAL.OUT',NAMDFT);
  ASSIGN2BUF('.OUT',TYPDFT);
  GETFNM(PROMPT, NAMDFT, TYPDFT, OUTFNAM); 
  WriteString(' OUTPUT FILE : ');
  WriteString(OUTFNAM.CHARS);
  WriteLn;
  FRESET(OUTUN1,OUTFNAM,WR);

  MONNAMSHORT[JAN] := 'JANUARY';
  MONNAMSHORT[FEB] := 'FEBRUARY';
  MONNAMSHORT[MAR] := 'MARCH';
  MONNAMSHORT[APR] := 'APRIL';
  MONNAMSHORT[MAY] := 'MAY';
  MONNAMSHORT[JUN] := 'JUNE';
  MONNAMSHORT[JUL] := 'JULY';
  MONNAMSHORT[AUG] := 'AUGUST';
  MONNAMSHORT[SEP] := 'SEPTEMBER';
  MONNAMSHORT[OCT] := 'OCTOBER';
  MONNAMSHORT[NOV] := 'NOVEMBER';
  MONNAMSHORT[DCM] := 'DECEMBER';

  MONNAMLONG[JAN] := '    J A N U A R Y        ';  
  MONNAMLONG[FEB] := '   F E B R U A R Y       ';  
  MONNAMLONG[MAR] := '      M A R C H          ';  
  MONNAMLONG[APR] := '      A P R I L          ';  
  MONNAMLONG[MAY] := '        M A Y            ';  
  MONNAMLONG[JUN] := '       J U N E           ';  
  MONNAMLONG[JUL] := '       J U L Y           ';  
  MONNAMLONG[AUG] := '     A U G U S T         ';  
  MONNAMLONG[SEP] := '  S E P T E M B E R      ';  
  MONNAMLONG[OCT] := '    O C T O B E R        ';  
  MONNAMLONG[NOV] := '   N O V E M B E R       ';  
  MONNAMLONG[DCM] := '   D E C E M B E R       ';  

  DAYSNAMLONG := 
'SUNDAY    MONDAY      TUESDAY     WEDNESDAY   THURSDAY    FRIDAY      SATURDAY';

  DAYSNAMSHORT :=    '  S  M  T  W TH  F  S    ';

  TOPLINE := 
'컴컴컴컴컫컴컴컴컴컴컫컴컴컴컴컴컫컴컴컴컴컴컫컴컴컴컴컴컫컴컴컴컴컴컫컴컴컴컴'
             ;
  BOTMLINE := 
'컴컴컴컴컨컴컴컴컴컴컨컴컴컴컴컴컨컴컴컴컴컴컨컴컴컴컴컴컨컴컴컴컴컴컨컴컴컴컴'
             ;
  MIDLINE := 
'컴컴컴컴컵컴컴컴컴컴컵컴컴컴컴컴컵컴컴컴컴컴컵컴컴컴컴컴컵컴컴컴컴컴컵컴컴컴컴'
             ;
  ANYLINE  := 
'         �           �           �           �           �           �';

  DIM[JAN] := 31;
  DIM[MAR] := 31;
  DIM[APR] := 30;
  DIM[MAY] := 31;
  DIM[JUN] := 30;
  DIM[JUL] := 31;
  DIM[AUG] := 31;
  DIM[SEP] := 30;
  DIM[OCT] := 31;
  DIM[NOV] := 30;
  DIM[DCM] := 31;

  PAGCHR := BLANKCHR;

  LOOP
    FOR MN := JAN TO DCM DO
      FOR I := 1 TO 6 DO
        FOR J := 1 TO 7 DO
          MONTH[MN,I,J] := BLANKSTR2;
        END(*FOR*);
      END(*FOR*);
    END(*FOR*);
  
    IF DELIMCH = NULL THEN (* really do need another ReadString call *)
      WriteString('Input Year : ');
      ReadString(INPUT.CHARS);
      WriteLn;
      TRIM(INPUT);
      INI1TKN(INPUT);
    END(*IF*);
    MN := JAN;
    CALSTATE := FINI;
    GETTKN(TOKEN,TKNSTATE,INTVAL,RETCOD);
    YEAR := CARDINAL(INTVAL);
    IF (YEAR < 1600) OR (RETCOD > 0) THEN EXIT; END(*IF*);
(*
THIS PROGRAM USES A FORMULA TO CALCULATE THE DAY OF THE WEEK (DOW) THAT
JANUARY FIRST FALLS OUT ON, SO IT ASSUMS THATS THE MONTH IS JANUARY
AND THE DAY IS 1.  THESE ASSUMPTIONS ARE COMBINED INTO THE CONSTANT OF 29
BELOW.  
  THIS FORMULA CONSIDERS THE YEAR TO GO FROM MARCH TO FEBRUARY, SO IT CAN
EASILY HANDLE LEAP YEARS.  SO JANUARY IS MONTH 11, AND THE YEAR IS ONE LESS
THAN THE ACTUAL ONE FOR WHICH THE CALENDAR IS BEING CONSTRUCTED.
*)
(* Zeller's congruence fails when LAS2DI is < 5 or so.  It is no longer used.
    IYEAR := YEAR - 1;
    LAS2DI := IYEAR MOD 100;
    FIR2DI := IYEAR DIV 100;
    JAN1DOW := ((29 + LAS2DI + LAS2DI DIV 4 + FIR2DI DIV 4 - 2*FIR2DI) MOD 7)
               + 1;
*)
    GREG2JUL(1,1,YEAR,JULDATE);
    JAN1DOW := Round(Frac(JULDATE/7.)*7.)+1;
    DOW := JAN1DOW;

    IF ((YEAR MOD 4) = 0) AND ((YEAR MOD 100) <> 0) THEN
(* YEAR IS DIVISIBLE BY 4 AND NOT BY 100 *)
      FEBDAYS := 29;
    ELSIF (YEAR MOD 400) = 0 THEN
      FEBDAYS := 29;
    ELSE
(* HAVE EITHER A NON-LEAP YEAR OR A CENTURY YEAR *)
      FEBDAYS := 28;
    END(*IF*);
    DIM[FEB] := FEBDAYS;

    FOR MN := JAN TO DCM DO DATEASSIGN; END(*FOR*);

    ConvertCardinal(YEAR,4,YEARSTR);
    IF DELIMCH = NULL THEN  (* From the YEAR GETTKN call *)
      WriteString('Do you want a 12 page calendar? ');
      Read(ANS);
      Write(ANS);
      WriteLn;
      IF CAP(ANS) = 'Y' THEN 
        CALSTATE := CAL12;
      ELSE
        CALSTATE := CAL1;
      END(*IF*);
    ELSE
      LOOP
        GETTKN(TOKEN,TKNSTATE,INTVAL,RETCOD);
        IF RETCOD > 0 THEN 
          CALSTATE := FINI;
          EXIT;
        ELSIF INTVAL < 0 THEN 
          FOR MN := VAL(MNEnum,ORD(MN)+1) TO VAL(MNEnum,-INTVAL-1) DO
            PRMONTH(MN);
          END(*FOR*);
          CALSTATE := FINI;
(*          EXIT; Don't exit when hyphen operator is used *)
        ELSIF TKNSTATE = DGT THEN
          IF (INTVAL < 1) OR (INTVAL > 12) THEN
            WriteString(' Month number out of range.  Use 1 thru 12.');
            WriteLn;
            CALSTATE := FINI;
            EXIT;
          END(*IF*);
          CALSTATE := CAL12;
          MN := VAL(MNEnum,INTVAL - 1);
          PRMONTH(MN);
          CALSTATE := FINI;
        ELSIF TKNSTATE = ALLELSE THEN
          IF TOKEN.CHARS[1] = 'Y' THEN
            CALSTATE := CAL12;
            EXIT
          ELSIF TOKEN.CHARS[1] = 'N' THEN
            CALSTATE := CAL1;
            EXIT
          ELSE
            WriteString(' Invalid Input.  ');
            WriteString(TOKEN.CHARS);
            WriteString(' not allowed.');
            WriteLn;
            CALSTATE := FINI;
            EXIT;
          END(*IF*);
        ELSIF TKNSTATE = OP THEN
          IF INTVAL = 10 THEN (* minus sign as a hyphen entered here *)
            GETTKN(TOKEN,TKNSTATE,INTVAL,RETCOD);
            IF (RETCOD > 0) OR (TKNSTATE <> DGT) THEN EXIT END(*IF*);
            FOR MN := VAL(MNEnum,ORD(MN)+1) TO VAL(MNEnum,INTVAL-1) DO
              PRMONTH(MN);
              CALSTATE := FINI;
            END(*FOR*);
          ELSE
            WriteString(' Invalid Input.  ');
            WriteString(TOKEN.CHARS);
            WriteString(' not allowed.');
            WriteLn;
            CALSTATE := FINI;
            EXIT;
          END(*IF*);
        END(*IF*);  
      END(*LOOP*);
    END(*IF*);
    IF CALSTATE = CAL12 THEN
(* WRITE 12 PAGE CALENDAR, ONE MONTH PER PAGE *)
      FOR MN := JAN TO DCM DO
        PRMONTH(MN);
      END(*FOR*);
    ELSIF CALSTATE = CAL1 THEN
(* Write one page calendar *)
      FWRSTR(OUTUN1,PAGCHR);
      RETBLKBUF(35,BLANKBUF);  (* LINE DISPLACEMENT FOR YEAR *)
      FWRTX(OUTUN1,BLANKBUF);
      FWRSTR(OUTUN1,YEARSTR);
      FWRLN(OUTUN1);
      
      FOR MN  := JAN TO DCM BY 3 DO
        MN2 := VAL(MNEnum,(ORD(MN) + 1));
        MN3 := VAL(MNEnum,(ORD(MN) + 2));
        FWRLN(OUTUN1);
        FWRLN(OUTUN1);
        FWRLN(OUTUN1);
        FWRSTR(OUTUN1,MONNAMLONG[MN]);
        FWRSTR(OUTUN1,MONNAMLONG[MN2]);
        FWRSTR(OUTUN1,MONNAMLONG[MN3]);
        FWRLN(OUTUN1);
        FWRLN(OUTUN1);
        FWRLN(OUTUN1);
        FWRSTR(OUTUN1,DAYSNAMSHORT);
        FWRSTR(OUTUN1,DAYSNAMSHORT);
        FWRSTR(OUTUN1,DAYSNAMSHORT);
        FWRLN(OUTUN1);
        FOR W := 1 TO 6 DO
          FOR I := 1 TO 7 DO 
            FWRSTR(OUTUN1,BLANKCHR);
            FWRSTR(OUTUN1,MONTH[MN,W,I]); 
          END(*FOR I*);
          FWRSTR(OUTUN1,'    ');
          FOR I := 1 TO 7 DO 
            FWRSTR(OUTUN1,BLANKCHR);
            FWRSTR(OUTUN1,MONTH[MN2,W,I]); 
          END(*FOR I*);
          FWRSTR(OUTUN1,'    ');
          FOR I := 1 TO 7 DO 
            FWRSTR(OUTUN1,BLANKCHR);
            FWRSTR(OUTUN1,MONTH[MN3,W,I]); 
          END(*FOR I*);
          FWRLN(OUTUN1);
        END(*FOR W*);
      END(*FOR MN*);
      FWRLN(OUTUN1);
      FWRLN(OUTUN1);
      FWRBL(OUTUN1,35);
      FWRSTR(OUTUN1,YEARSTR);
      FWRLN(OUTUN1);
    END(*IF*);
  END(*LOOP*);
  FCLOSE(OUTUN1);
END CALold.
