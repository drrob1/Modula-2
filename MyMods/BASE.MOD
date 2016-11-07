MODULE BASE;

IMPORT ASCII,CardinalIO,Conversions,Directories,InOut,MathLib0,
  FloatingUtilities,NumberConversion,RealConversions,RealInOut,Terminal,
  Overlay,RTSMain,ENVRTNS,FIO,TOKENIZE,UTILLIB,CALLIB,REALLIB,TIMLIB,
  ALTRCHK,ANW1,ANWORDIO,LNRTNS,READRTN,SETUPRTN,WORDRTNS,WRPGRTNS,TIMER,
  UL2,DspFiles,InitLogitech,NdxBones,NdxFiles,NdxTypes,ScreenDisplay,
  ScreenInput,Scrn2Ndx,ScrnTypes,ScrnUtl1,StrConv,StrEdit,UserOps,
  WindowPrims,BigSets,Drectory,EnvironUtils,ErrorManager,FileIO,
  KbdInput,LowLevel,M2Strings,Numbers,PosUtils,StringIO;

VAR ERRORCODE : Overlay.ErrorCode;
    RTSSTATUS : RTSMain.Status;
    BUF,TOKEN : UTILLIB.BUFTYP;
    TKNSTATE  : TOKENIZE.FSATYP;
    RETCOD    : CARDINAL;
    I         : INTEGER;

BEGIN
  InitLogitech.AssignProcs();
  Terminal.ReadString(BUF.CHARS);
  UL2.TRIM(BUF);
  TOKENIZE.INI1TKN(BUF);
  LOOP
    TOKENIZE.GETTKN(TOKEN,TKNSTATE,I,RETCOD);
    IF (RETCOD = 0) AND (TKNSTATE = TOKENIZE.ALLELSE) THEN EXIT; END(*IF*);
    InOut.WriteString(' Invalid Test Application.  Please Reenter: ');
    Terminal.ReadString(BUF.CHARS);
    InOut.WriteLn;
    UL2.TRIM(BUF);
    TOKENIZE.INI1TKN(BUF);
  END(*LOOP*);
  Overlay.CallOverlay(BUF.CHARS,ERRORCODE,RTSSTATUS);
END BASE.

