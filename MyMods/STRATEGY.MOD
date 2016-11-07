IMPLEMENTATION MODULE STRATEGY;
(*
  REVISION HISTORY
  ----------------
   6 Mar 05 -- Win32 version using SBM2 b 31.
*)
(*
  IMPORT Break;
  IMPORT MED;
  FROM Terminal IMPORT ReadString;
  IMPORT Terminal;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM LongIO IMPORT ReadLongInt, WriteLongInt;
  FROM RTSTypes IMPORT Status;
  FROM RTSTerm IMPORT Terminate;
  FROM Conversions IMPORT ConvertCardinal, ConvertInteger;
  FROM RealConversions IMPORT RealToString, StringToReal;
  FROM RealInOut IMPORT ReadReal, WriteReal, Done;
  FROM LongMathLib0 IMPORT longreal,exp,ln;
  FROM Directories IMPORT Delete,DirResult;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;
  FROM TimeDate IMPORT Time,GetTime;
  FROM Random IMPORT RandomInit,RandomCard,RandomInt,RandomReal,Randomize;
*)
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
  FROM SYSTEM IMPORT ADR,ADDRESS,FUNC;
  IMPORT WholeStr, LongStr, LongMath, Terminal, BasicDialogs, DlgShell, Strings;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  IMPORT RConversions, LongConv, LowLong;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT,
(*  FROM UL2 IMPORT *) COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM MyFIO IMPORT DRIVESEP,SUBDIRSEP,MYFILTYP,IOSTATE,FRESET,FOPEN,FCLOSE,
    FRDTXLN,FWRTXLN,FWRTX,RETBLKBUF,EXTRACTDRVPTH,FWRLN,
    FWRSTR,GETFNM;
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,DELIMCH,DELIMSTATE,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,GETTKNREAL,UNGETTKN;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR,FLOATLONG,TRUNCLONG,
    CROPNSTR,GETCROPNUM;
  FROM TIMLIB IMPORT GETNEWDATE,TIME2MDY,MDY2TIME,MDY2STR,GREG2JUL,
    JUL2GREG,GETMDY,JULIAN,GREGORIAN;
  FROM TIMER IMPORT READTIMER;
  FROM MYRAND IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL;
  FROM MiscM2 IMPORT WriteCx, SelectWindow, WriteString, WriteLn, PressAnyKey,
                   ReadCard, ReadString, ReadLongReal, WriteReal, WriteLongReal,
                   WriteCard, WriteInt, Error, CLS, RtJust;
(*  FROM LCARDLIB IMPORT ADDCOMMAS,LCARD2STR,WriteLongCard; *)

(* From Def Module
CONST
  OptionName : ARRAY [0..4] OF STR10TYP = ['S  ','H  ','D  ','SP ','SUR'];
TYPE
  OptionEnum = (S,H,D,SP,SUR); /* Stand, Hit, Double, Split, Surrender */
  OptionEnumRow = ARRAY [1..10] OF OptionEnum;

VAR
  Strategy : ARRAY [5..21] OF OptionEnumRow;
  SoftStrategy : ARRAY [2..11] OF OptionEnumRow;
  PairStrategy : ARRAY [1..10] OF OptionEnumRow;
  StratErrorFlag : BOOLEAN;
  OpenFileName,filter : STRTYP;
*)
CONST
  ORDZERO = ORD('0');
  MenuSep = '|';

VAR
  TempOptionRow : OptionEnumRow;
  C, ROW, RETCOD, forloop : CARDINAL;
  TOKEN,TKN,B,ROWBUF,ROWTOKEN,PROMPT,NAMDFT,TYPDFT,INFNAM : BUFTYP;

  TKNSTATE,ROWTKNSTATE : FSATYP;
  I,IROW : INTEGER;
  Option : OptionEnum;
  EOFFLAG : BOOLEAN;
  INUNT1 : MYFILTYP;
  CH : CHAR;

PROCEDURE MakeOption(TKN : BUFTYP) : OptionEnum;
BEGIN
  IF TKN.COUNT = 1 THEN
    CASE TKN.CHARS[1] OF
        'S': RETURN(S);
      | 'H': RETURN(H);
      | 'D': RETURN(D);
      ELSE
        StratErrorFlag := TRUE;
        RETURN(S);
    END(*CASE*);
  ELSIF (TKN.COUNT=2) AND (STRCMPFNT(TKN.CHARS[1..2],'SP')=0) THEN
    RETURN(SP);
  ELSIF (TKN.COUNT=3) AND (STRCMPFNT(TKN.CHARS[1..3],'SUR')=0) THEN
    RETURN(SUR);
  ELSE
    StratErrorFlag := TRUE;
    RETURN(S);
  END(*IF*);
END MakeOption;

PROCEDURE GETSTRATEGIES;

VAR tpv1 : TKNPTRTYP;
    c3 : CARDINAL ;
    bool : BOOLEAN ;

BEGIN
(*
  ASSIGN2BUF('Enter  input file name : ',PROMPT);
  ASSIGN2BUF('STRAT3b.DAT',NAMDFT);
  ASSIGN2BUF('.DAT',TYPDFT);
  GETFNM(PROMPT, NAMDFT, TYPDFT, INFNAM);
  FRESET(INUNT1,INFNAM,RD);
*)
  OpenFileName := '';

  filter := 'Data Files';
  Strings.Append(MenuSep,filter);
  Strings.Append('*.dat',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('Text Files',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('*.txt',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('All',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('*',filter);
  Strings.Append(MenuSep,filter);
  c3 := 1;
  DlgShell.ConvertToNulls(MenuSep,filter);
  bool := BasicDialogs.PromptOpenFile(OpenFileName,filter,c3,'','',
                                         'Open matrix values as a text file',FALSE);
  IF NOT bool THEN
    Error('Could not open file.  Does it exist?');
    HALT;
  END;
  INFNAM.CHARS := OpenFileName;
  TRIM(INFNAM);
  FOPEN(INUNT1,INFNAM,RD);
(*
  WriteString('  INPUT FILE : ');
  WriteString(INFNAM.CHARS);
  WriteLn;
*)
  LOOP
    FRDTXLN(INUNT1,ROWBUF,180,EOFFLAG);
    IF EOFFLAG THEN EXIT; END(*IF*);
    StratErrorFlag := ROWBUF.COUNT = 0;  (*Error is true if have a bl line *)
    IF NOT StratErrorFlag THEN
      INI1TKN(tpv1,ROWBUF);
      GETTKN(tpv1,ROWTOKEN,ROWTKNSTATE,IROW,RETCOD);
    END(*IF*);
    IF StratErrorFlag OR (RETCOD > 0) OR (TKNSTATE = OP) OR
                            ((TKNSTATE = ALLELSE) AND (TOKEN.COUNT > 3)) THEN
      StratErrorFlag := TRUE;
      WriteString(' In GetStrategies and have invalid token beginning line:');
      WriteLn;
      WriteString(ROWBUF.CHARS);
      WriteLn;
      PressAnyKey; (*WriteString(' Pause (Y/N): '); Read(CH); Write(CH); WriteLn; *)
    END(*IF*);
    ROW := IROW;
    C := 1;
    WHILE (C <= 10) AND NOT StratErrorFlag DO
      GETTKN(tpv1,TOKEN,TKNSTATE,I,RETCOD);
      IF RETCOD > 0 THEN
        StratErrorFlag := TRUE;
        WriteString(
        ' In GetStrategies and have error from GETTKN on line:');
        WriteLn;
        WriteString(TOKEN.CHARS);
        WriteLn;
        PressAnyKey; (* WriteString(' Pause (Y/N): '); Read(CH); Write(CH); WriteLn; *)
      ELSE
        TempOptionRow[C] := MakeOption(TOKEN);
        IF StratErrorFlag THEN
          WriteString(
          ' In GetStrategies and have error from MakeOption on line:');
          WriteLn;
          WriteString(TOKEN.CHARS);
          WriteLn;
          PressAnyKey; (* WriteString(' Pause (Y/N): '); Read(CH); Write(CH); WriteLn;*)
        END(*IF*);
        INC(C);
      END(*IF*);
    END(*WHILE*);
    IF (ROWTKNSTATE = DGT) AND NOT StratErrorFlag THEN
      IF (ROW >=5) AND (ROW <= 21) THEN
        Strategy[ROW] := TempOptionRow;
      ELSE CASE ROW OF
          22: PairStrategy[2] := TempOptionRow;
        | 33: PairStrategy[3] := TempOptionRow;
        | 44: PairStrategy[4] := TempOptionRow;
        | 55: PairStrategy[5] := TempOptionRow;
        | 66: PairStrategy[6] := TempOptionRow;
        | 77: PairStrategy[7] := TempOptionRow;
        | 88: PairStrategy[8] := TempOptionRow;
        | 99: PairStrategy[9] := TempOptionRow;
        | 1010: PairStrategy[10] := TempOptionRow;
        ELSE
          WriteString(' Invalid Row Value:');
          WriteCard(ROW);
          WriteLn;
          WriteString(ROWBUF.CHARS);
          WriteLn;
          StratErrorFlag := TRUE;
          PressAnyKey; (*WriteString(' Pause (Y/N): '); Read(CH); Write(CH); WriteLn; *)
        END(*CASE*);
      END(*IF*);
    ELSIF (TKNSTATE = ALLELSE) AND NOT StratErrorFlag THEN
      IF STRCMPFNT(ROWTOKEN.CHARS[1..2],'AA') = 0 THEN
        PairStrategy[1] := TempOptionRow;
      ELSIF ROWTOKEN.CHARS[1] = 'S' THEN (* Init SoftStrategy *)
        IF (ROWTOKEN.COUNT = 3) AND (STRCMPFNT(ROWTOKEN.CHARS[2..3],'10')=0)
                                                                         THEN
          SoftStrategy[10] := TempOptionRow;
        ELSIF (ROWTOKEN.COUNT = 3) AND (STRCMPFNT(ROWTOKEN.CHARS[2..3],'11')=0)
                                                                         THEN
          SoftStrategy[11] := TempOptionRow;
        ELSIF ROWTOKEN.COUNT = 2 THEN
          C := ORD(ROWTOKEN.CHARS[2]) - ORDZERO;
          SoftStrategy[C] := TempOptionRow;
        ELSE
          StratErrorFlag := TRUE;
          WriteString(
          ' In GetStrategies and have invalid token beginning line:');
          WriteLn;
          WriteString(ROWBUF.CHARS);
          WriteLn;
          PressAnyKey; (* WriteString(' Pause (Y/N): '); Read(CH); Write(CH); WriteLn; *)
        END(*IF*);
      END(*IF*);
    ELSE
      WriteString(' Invalid Row Designator Value:');
      WriteString(ROWTOKEN.CHARS);
      WriteLn;
      WriteString(ROWBUF.CHARS);
      WriteLn;
      StratErrorFlag := TRUE;
      PressAnyKey; (* WriteString(' Pause (Y/N): '); Read(CH); Write(CH); WriteLn; *)
    END(*IF*);
  END(*LOOP*);
  FCLOSE(INUNT1);
  IF tpv1 # NIL THEN DISPOSE(tpv1); END;
END GETSTRATEGIES;

PROCEDURE PUTSTRATEGIES;
VAR
  NSTR : ARRAY [1..4] OF CHAR;
  C1,C2 : CARDINAL;

BEGIN
  WriteString(' Strategy Matrix: ');
  WriteLn;
  FOR C1 := 5 TO 21 DO
    WholeStr.CardToStr(C1,NSTR);
(* Left justify str *)
    IF LENGTH(NSTR) = 1 THEN
      NSTR[2] := ' ';
      NSTR[3] := 0C;
    END(*IF*);
    WriteString(' Row ');
    WriteString(NSTR);
    WriteString(':');
    FOR C2 := 1 TO 10 DO
      WriteString(' ');
      WriteString(OptionName[ORD(Strategy[C1,C2])]);
    END(*FOR*);
    WriteLn;
  END(*FOR*);
  WriteString(' Hit any key to continue ');
  Terminal.Read(CH);
  Terminal.Write(CR);  (* Now overwrite the hit any key to continue msg *)
  WriteString(' SoftStrategy Matrix:    ');
  WriteLn;
  FOR C1 := 2 TO 11 DO
    WholeStr.CardToStr(C1,NSTR);
    IF LENGTH(NSTR) = 1 THEN (* Left justify str *)
      NSTR[2] := ' ';
      NSTR[3] := 0C;
    END(*IF*);
    WriteString(' Row S');
    WriteString(NSTR);
    WriteString(':');
    FOR C2 := 1 TO 10 DO
      WriteString(' ');
      WriteString(OptionName[ORD(SoftStrategy[C1,C2])]);
    END(*FOR*);
    WriteLn;
  END(*FOR*);
  WriteString(' Hit any key to continue ');
  Terminal.Read(CH);
  Terminal.Write(CR);  (* Now overwrite the hit any key to continue msg *)
  WriteString(' PairStrategy Matrix:    ');
  WriteLn;
  FOR C1 := 1 TO 10 DO
    WholeStr.CardToStr(C1,NSTR);
(* Duplicate number in string to indicate PairStrategy *)
    IF LENGTH(NSTR) = 1 THEN
      NSTR[2] := NSTR[1];
      NSTR[3] := ' ';
      NSTR[4] := ' ';
    ELSE
      NSTR[3] := NSTR[1];
      NSTR[4] := NSTR[2];
    END(*IF*);
    WriteString(' Row ');
    WriteString(NSTR);
    WriteString(':');
    FOR C2 := 1 TO 10 DO
      WriteString(' ');
      WriteString(OptionName[ORD(PairStrategy[C1,C2])]);
    END(*FOR*);
    WriteLn;
  END(*FOR*);
END PUTSTRATEGIES;

BEGIN  (* Initialization code for this module, init to all Stand *)
  FOR forloop := 1 TO 10 DO
    TempOptionRow[forloop] := S;
  END(*FOR*);
  FOR forloop := 5 TO 21 DO
    Strategy[forloop] := TempOptionRow;
  END(*FOR*);
  FOR forloop := 2 TO 11 DO
    SoftStrategy[forloop] := TempOptionRow;
  END(*FOR*);
  FOR forloop := 1 TO 10 DO
    PairStrategy[forloop] := TempOptionRow;
  END(*FOR*);
END STRATEGY.
