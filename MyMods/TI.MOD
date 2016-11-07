MODULE TI;

(*
Copyright (C) 1995 Rob Solomon MD.  All rights reserved.
  REVISION HISTORY
  ----------------
*)
  FROM SYSTEM IMPORT ADR,ADDRESS;
  IMPORT Break;
  IMPORT MED;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;
  FROM Terminal IMPORT ReadString,KeyPressed;
  IMPORT Terminal;
  FROM TimeDate IMPORT Time,GetTime;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT;
  FROM UL2 IMPORT COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF,GETFNM;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,
    NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT GETNEWDATE,TIME2MDY,MDY2TIME,MDY2STR,GREG2JUL,
    JUL2GREG,GETMDY,JULIAN,GREGORIAN,DAYNAMES;
  FROM RTSTypes IMPORT Status;
  FROM RTSTerm IMPORT Terminate;

(*
  TYPE Time = RECORD day, minute, millisec: CARDINAL; END;
   - date and time of day

   'day' is : Bits 0..4 = day of month (1..31),
              Bits 5..8 = month of the year (1..12),
              Bits 9..15 = year - 1900.
   'minute' is hour * 60 + minutes.
   'millisec' is second * 1000 + milliseconds,
              starting with 0 at every minute.
*)

VAR 
    M,D,Y,HRS,Min,SEC : CARDINAL;   (* MIN conflicted with MIN fnt *)
    CH                : CHAR;
    PMFLG             : BOOLEAN;
    T,T0              : Time;
    DOW               : CARDINAL;
    JULDATE           : LONGINT;

BEGIN
  GetTime(T0);
  WriteString(
  ' Display (D)ate or (T)ime, (E)xit pgm, (U)pdate system date : ');
  WriteLn;
  LOOP
(*
    WriteString(
    ' Display (D)ate or (T)ime, (E)xit pgm, (U)pdate system date : ');
    REPEAT TIME2MDY(M,D,Y); UNTIL KeyPressed();
*)
    REPEAT
      GetTime(T);
      HRS := T.minute DIV 60;
      Min := T.minute MOD 60;
      SEC := T.millisec DIV 1000;
      IF HRS < 12 THEN
        PMFLG := FALSE;
        IF HRS = 0 THEN HRS := 12; END(*IF*);
      ELSE
        PMFLG := TRUE;
        IF HRS > 12 THEN DEC(HRS,12); END(*IF*);
      END(*IF*);
      IF SEC <> T0.millisec DIV 1000 THEN 
        T0 := T;
        Write(CR);
        WriteString(' Current Time is ');
        WriteCard(HRS,1);
        Write(':');
        Write(CHR(Min DIV 10 + ORD('0')));
        Write(CHR(Min MOD 10 + ORD('0')));
        Write(':');
        Write(CHR(SEC DIV 10 + ORD('0')));
        Write(CHR(SEC MOD 10 + ORD('0')));
        IF PMFLG THEN 
          WriteString(' pm.');
        ELSE
          WriteString(' am.');
        END;
      END(*IF*);
    UNTIL KeyPressed();
    Read(CH); (* Write(CH); *)
    WriteLn;
    WriteLn;
    CH := CAP(CH);
    CASE CH OF
      'D' : WriteString(' Current Day and Date is ');
            TIME2MDY(M,D,Y);
            JULDATE := JULIAN(M,D,Y);
            DOW := JULDATE MOD 7 + 1;
            WriteString(DAYNAMES[DOW]);
            WriteString(', ');
            WriteCard(M,1);
            Write('/');
            WriteCard(D,1);
            Write('/');
            WriteCard(Y MOD 100,2);
            Write('.');
            WriteLn;
            (* Continue looping *)
    | 'T' : GetTime(T);
            HRS := T.minute DIV 60;
            Min := T.minute MOD 60;
            SEC := T.millisec DIV 1000;
            IF HRS < 12 THEN
              PMFLG := FALSE;
              IF HRS = 0 THEN HRS := 12; END(*IF*);
            ELSE
              PMFLG := TRUE;
              IF HRS > 12 THEN DEC(HRS,12); END(*IF*);
            END(*IF*);
            WriteString(' Current Time is ');
            WriteCard(HRS,1);
            Write(':');
            Write(CHR(Min DIV 10 + ORD('0')));
            Write(CHR(Min MOD 10 + ORD('0')));
            Write(':');
            Write(CHR(SEC DIV 10 + ORD('0')));
            Write(CHR(SEC MOD 10 + ORD('0')));
            IF PMFLG THEN 
              WriteString(' pm.');
            ELSE
              WriteString(' am.');
            END;
            WriteLn;
            (* Continue looping *)
    | 'E','Q','X','S' : Terminate(Normal);
    | 'U' : EXIT;
    ELSE
      (* Any other input is ignored and loop is continued *)
    END(*CASE*);
  WriteLn;
  END(*LOOP*);
END TI.
