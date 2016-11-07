MODULE INTRATEW;
(*
  REVISION HISTORY
  ----------------
  10 Nov 02 -- Started to convertd to SBM2 4 win v4, but never completed task.
  20 May 03 -- First Win32 version, SBM2.
*)
  FROM SYSTEM IMPORT ADR,ADDRESS;
  IMPORT IOChan, ChanConsts;
  IMPORT RConversions, LongStr, LongConv, Str;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Environment IMPORT GetCommandLine;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;

  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM MyFIO IMPORT DRIVESEP,SUBDIRSEP,MYFILTYP,IOSTATE,FRESET,FCLOSE,FAPPEND,
    FRDTXLN,FWRTXLN,FWRTX,RETBLKBUF,EXTRACTDRVPTH,FWRLN,FWRSTR,GETFNM;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    GETTKNREAL,UNGETTKN;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR,CROPNSTR,FLOATLONG,TRUNCLONG;
  FROM TIMLIB IMPORT GETNEWDATE,TIME2MDY,MDY2TIME,MDY2STR,GREG2JUL,
    JUL2GREG,GETMDY;
  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts (*drive path name extension*), FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle,
    MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary,
    AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName,
    CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock,
    WriteBlock, (* ReadChar, WriteChar, *) PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength,
    GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList,
    FindInOSPathList, SupportsUTC, ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;

  VAR
    FINI                                           : BOOLEAN;
    OUTUN1                                         : MYFILTYP;
    iHP,iMY,iNEXTHP,iNEXTMY,iANNUALHP,iANNUALMY,iMY2,iNEXTMY2,iANNUALMY2 : LONGREAL;
    CH                                             : CHAR;
    FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLAG           : BOOLEAN;
    PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
      TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
    ACUMINT,PV,PMT,PERINT,ANNUALINT,TOTPRICE,DNPMT : LONGREAL;
    NUMPER,loopctr,pos                             : CARDINAL;
    OUTSTR,TITLE,STR1,STR2,STR3,STR4,STR5,STR6,pvstr,pmtstr,nperstr : STRTYP;
  C,K,IDX,PTR,c,RETCOD                         : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  TKNSTATE                                     : FSATYP;
  I,J                                          : INTEGER;
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

VAR
  inputline                                    : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;
    cid            : ChanId;
    flags          : FlagSet;
    openres        : OpenResults;


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
  NEGn := -1 * INT(NUMPER);
  P := i*PV/PMT - (1. - PWRI(iPREV,NEGn));
  Q := PWRI(iPREV,NEGn) * (LFLOAT(NUMPER)/(1.+1./i) + 1.);
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
  NEGn := -1 * INT(NUMPER);
  NEXTTERM := ( (1. - PWRI(iPREV,NEGn)) - i*PV/PMT ) /
    ( LFLOAT(NUMPER)*PWRI(iPREV,NEGn-1) + PWRI(iPREV,NEGn)/i - 1./i );
  EstIntMy := i - NEXTTERM;
  RETURN(EstIntMy);
END MYMETHOD;

PROCEDURE MY2ndMETHOD(i:LONGREAL) : LONGREAL;
(*
  Do one iteration of Newton's method to get next interest estimate
  (EstIntMy), using my calculus but not my algebra.
  Uses the global PV, PMT, and NUMPER variables.
*)
VAR EstIntMy2nd,iPREV,NEXTTERM,FofX,dFofX : LONGREAL;
  NEGn : INTEGER;

BEGIN
  iPREV := 1. + i;
  NEGn := -1 * INT(NUMPER);
  FofX := (1. - PWRI(iPREV,NEGn))/i - PV/PMT;
  dFofX  := (i*LFLOAT(NUMPER)*PWRI(iPREV,NEGn-1) - (1. - PWRI(iPREV,NEGn)))
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
   i   =   ---  -  ---------
    0      PV      N^2 * PMT

                 f(i)
   i   =   i  -  -----
    k+1     k    f'(i)

                    -n
           1 - (1+i)     PV
  f(i) =   ----------- - ---
               i         PMT

                    -n-1              -n
           i N (i+i)     - [ 1 - (1+i)
 f'(i) =   -----------------------------
                      2
                     i
*)

CONST
  TolFac = 1.E-12;  (* Tolerance Factor *)

VAR
  i,iPlus1,NextTerm,Tolerance : LONGREAL;
  NegN                        : INTEGER;

BEGIN
  i := PMT/PV - PV/LFLOAT(N)/LFLOAT(N)/PMT;
  NegN := -N;
  Tolerance := i * TolFac;
  REPEAT
    iPlus1   := i + 1.;
    NextTerm := ( (1. - PWRI(iPlus1,NegN)) - i*PV/PMT ) /
           ( LFLOAT(N)*PWRI(iPlus1,NegN-1) + PWRI(iPlus1,NegN)/i - 1./i );
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

PROCEDURE getcmdline;
BEGIN
  GetCommandLine(inputline);
  c := LENGTH(inputline);

  IF c = 0 THEN
(*
    ASSIGN2BUF('ENTER  INPUT FILE NAME : ',PROMPT);
    ASSIGN2BUF('FTEST.TXT',NAMDFT);
    ASSIGN2BUF('.TXT',TYPDFT);
    GETFNM(PROMPT,NAMDFT,TYPDFT,INFNAM);
*)
    ASSIGN2BUF('ENTER OUTPUT FILE NAME : ',PROMPT);
    ASSIGN2BUF('INTRATE.OUT',NAMDFT);
    ASSIGN2BUF('.OUT',TYPDFT);
    GETFNM(PROMPT, NAMDFT, TYPDFT, OUTFNAM);

  ELSE
    ASSIGN2BUF(inputline,INBUF);
    INI1TKN(INBUF);
    GETTKN(TOKEN,TKNSTATE,I,RETCOD);
    IF RETCOD > 0 THEN
      WriteString("GETTKN's RETCOD is ");
      WriteCard(RETCOD,0);
      WriteLn;
      HALT;
    ELSE
      OUTFNAM := TOKEN;
      ConstructFileName(INFNAM.CHARS,'.OUT',INFNAM.CHARS);
      TRIM(OUTFNAM);
    END;
  END;
  WriteString(' OUTPUT FILE : ');
  WriteString(OUTFNAM.CHARS);
  WriteLn;
(*
  WriteString('  -> Pause');
  ReadChar(CH);
  SkipLine;
  WriteLn;
*)
END getcmdline;

BEGIN  (******************************* Main Pgm **************************)
  getcmdline;

  FAPPEND(OUTUN1,OUTFNAM);
(*  not needed
  flags := write + text;  (* For terminal writing of reals *)
*)
  LOOP
    WriteString('INPUT NUMBER Years : ');
    ReadCard(K);
    SkipLine;
    WriteLn;
    NUMPER := K * 12;
(*    NUMPER := 360;*)
    IF NUMPER = 0 THEN EXIT END(*IF*);

    WriteString('INPUT TITLE FOR THIS RUN OF IntRate : ');
    ReadString(TITLE);
    SkipLine;
    WriteLn;

    WriteString('  INPUT AMOUNT TO FINANCE : ');
    ReadReal(PV);
    SkipLine;
    WriteLn;
    IF (* NOT Done OR*) (PV = 0.) THEN EXIT END(*IF*);
    WriteLn;
(*
    WriteString('INPUT ANNUAL INTEREST RATE AS A %-age : ');
    ReadReal(ANNUALINT);
*)
    WriteString(' Input Monthly Pmt : ');
    ReadReal(PMT);
    SkipLine;
    WriteLn;
(*    IF NOT Done THEN EXIT END(*IF*); *)
(*
    PV := 76800.;
    ANNUALINT := 6.95;

    PERINT := ANNUALINT / 12.0 / 100.0;
    PMT := PV * PERINT/(1.0 - 1.0/PWRI(1.0+PERINT,NUMPER));
*)
(*
  ACUMINT = BAL(K) - BAL(J-1) + PMT*FLOAT(K - J + 1);
*)
(*    ACUMINT := BAL(12) - BAL(0) + PMT*12.0; *)
    WriteLn;
    WriteLn;
    WriteString(TITLE);
    WriteLn;
    FWRSTR(OUTUN1,TITLE);
    FWRLN(OUTUN1);
    FWRLN(OUTUN1);
    FWRLN(OUTUN1);

    WriteString(' The amount to finance is $');
    FWRSTR(OUTUN1,' The amount to finance is $');
    pos := 0;
    RealToStringFixed(PV,11,2,pvstr,pos,FINI);
    CROPNSTR(pvstr);
    WriteString(pvstr);
    WriteLn;
    FWRSTR(OUTUN1,pvstr);
    FWRLN(OUTUN1);
(*    WriteString('Payment for '); *)
    WriteCard(NUMPER,0);
    WriteString(' month loan is $');
(*    FWRSTR(OUTUN1,'Payment for '); *)
    FINI := CardToStr(NUMPER,nperstr);
    FWRSTR(OUTUN1,nperstr);
    FWRSTR(OUTUN1,' month loan is $');
(*
    pos := 0;
    RealToStringFixed(ANNUALINT,7,2,OUTSTR,pos,FINI);
    WriteString(OUTSTR);
    WriteString('% is : $');
    FWRSTR(OUTUN1,OUTSTR);
    FWRSTR(OUTUN1,'% is : $');
*)
    pos := 0;
    RealToStringFixed(PMT+0.0005,6,4,pmtstr,pos,FINI);
    WriteString(pmtstr);
    CROPNSTR(pmtstr);
    WriteLn;
    FWRSTR(OUTUN1,pmtstr);
    FWRLN(OUTUN1);
    iHP := PMT/PV - PV/LFLOAT(NUMPER)/LFLOAT(NUMPER)/PMT;
    iMY := iHP;
    iMY2 := iMY;
    WriteString(' Initial periodic (month) i = ');
    pos := 0;
    RealToString(iHP,15,OUTSTR,pos,FINI);
    WriteString(OUTSTR);
    WriteLn;
    FWRSTR(OUTUN1,' Initial periodic (month) i = ');
    FWRSTR(OUTUN1,OUTSTR);
    FWRLN(OUTUN1);
(*
    WriteString(' Press any key to continue: ');
    Read(CH);
    WriteLn;
*)
    loopctr := 0;
    WriteString('    HP METHOD                        MY METHODs');
    WriteLn;
    WriteString('    =========                        =========');
    WriteLn;
    FWRSTR(OUTUN1,'   HP METHOD                        MY METHODs');
    FWRLN(OUTUN1);
    FWRSTR(OUTUN1,'   =========                        =========');
    FWRLN(OUTUN1);
    LOOP (* iteration loop to determine i *)
      iNEXTHP := HPMETHOD(iHP);
      iNEXTMY := MYMETHOD(iMY);
      iNEXTMY2 := MY2ndMETHOD(iMY2);
      iANNUALHP := iNEXTHP * 12.;
      iANNUALMY := iNEXTMY * 12.;
      iANNUALMY2 := iNEXTMY2 * 12.;
      pos := 0;
      RealToString(iNEXTHP,10,STR1,pos,FINI);
      WriteString(STR1);WriteString('  ');
      pos := 0;
      RealToStringFixed(iANNUALHP*100.,10,2,STR2,pos,FINI);
      WriteString(STR2);WriteString('  ');
      pos := 0;
      RealToString(iNEXTMY,10,STR3,pos,FINI);
      WriteString(STR3);WriteString('  ');
      pos := 0;
      RealToStringFixed(iANNUALMY*100.,10,2,STR4,pos,FINI);
      WriteString(STR4);WriteString('  ');
      WriteLn;
      pos := 0;
      RealToString(iNEXTMY2,10,STR5,pos,FINI);
      WriteString(STR5);WriteString('  ');
      pos := 0;
      RealToStringFixed(iANNUALMY2*100.,10,2,STR6,pos,FINI);
      WriteString(STR6);
      WriteLn;
      FWRSTR(OUTUN1,STR1);FWRSTR(OUTUN1,'  ');
      FWRSTR(OUTUN1,STR2);FWRSTR(OUTUN1,'  ');
      FWRSTR(OUTUN1,STR3);FWRSTR(OUTUN1,'  ');
      FWRSTR(OUTUN1,STR4);FWRSTR(OUTUN1,'  ');
      FWRSTR(OUTUN1,STR5);FWRSTR(OUTUN1,'  ');
      FWRSTR(OUTUN1,STR6);
      FWRLN(OUTUN1);
      INC(loopctr);
      IF ((ABS(iHP-iNEXTHP) < 1.E-6*iHP) AND (ABS(iMY-iNEXTMY) < 1.E-6*iMY))
                                                       OR (loopctr > 40) THEN
        EXIT;
      END(*IF*);
      iHP := iNEXTHP;
      iMY := iNEXTMY;
      iMY2 := iNEXTMY2;
    END(*iteration LOOP to determine i *);
    WriteLn;
    WriteString(' Periodic (Monthly) i by PerIntRate Proc is ');
    iMY := PerIntRate(PV,PMT,NUMPER);
    pos := 0;
    RealToStringFixed(iMY,15,2,STR1,pos,FINI);
    WriteString(STR1);
    WriteString('.  Annual i is ');
    pos := 0;
    RealToStringFixed(iMY*12.,15,2,STR2,pos,FINI);
    WriteString(STR2);
    pos := 0;
    RealToStringFixed(iMY*1200.,6,2,STR3,pos,FINI);
    CROPNSTR(STR3);
    WriteString(', ');
    WriteString(STR3);
    WriteString('%.');
    WriteLn;
    FWRLN(OUTUN1);
    FWRLN(OUTUN1);
    FWRSTR(OUTUN1,' Periodic (Monthly) i by PerIntRate Proc is ');
    FWRSTR(OUTUN1,STR1);
    FWRSTR(OUTUN1,'.  Annual i is ');
    FWRSTR(OUTUN1,STR2);
    FWRSTR(OUTUN1,', ');
    FWRSTR(OUTUN1,STR3);
    FWRSTR(OUTUN1,'%.');
    FWRLN(OUTUN1);
    FWRLN(OUTUN1);
    FWRLN(OUTUN1);
    FWRLN(OUTUN1);
  END(*Outer LOOP*);
  FCLOSE(OUTUN1);
END INTRATEW.
