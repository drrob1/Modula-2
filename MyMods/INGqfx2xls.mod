<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:qfx.RES*>
%ELSE
%END
MODULE INGqfx2xls;
(*
  REVISION HISTORY
  ----------------
  13 Mar 04 -- It does not seem to be always creating the output file.
               It now copies the d/l file instead of renaming it.  And
               it echoes its output to the terminal.
  14 Mar 04 -- Made it a Text Window module
  21 Mar 04 -- Changed exit keys to remove <cr> and add <space>, <bs>, <tab>
  15 Apr 04 -- Decided to include <cr> again, as I fixed the pblm in Excel macros.
  31 Jul 04 -- Needed to change logic because Citibank now d/l nulls in fields that I need.
   8 Aug 04 -- Copied to process MMA file as well.  And fixed a minor output bug.
   8 Nov 04 -- Citibank changed their file format again.  Don't need ExtractLastNum and the
                description is now 2 fields instead of 1.
  17 Jun 06 -- Opened a new citi acnt they call eSavings.  Need to include this into database.
                And changed initial value of chknum to zero, and any key will exit now.
  18 Jun 06 -- Now uses command line for file names.
  19 Jun 06 -- Fixed bug of always writing the same acnt name and made it output filename.
  27 Jan 07 -- Noticed that the fileformat changed slightly as of Oct or so.  I have to remove
                squotes from acnt#.  And added a menu option to exit.
  29 Jan 07 -- While at ISET 2007, I decided to change the method of removing the squote so that
                all squotes are removed, even if Citibank gets cute and puts more in.
   2 Oct 07 -- Now has ability to use .qif files, and needed a module name change for this.
                Also used menu pick instead of cmd line params.
  21 Feb 08 -- Had to make output file .txt so that Access on P5 could import the file.  Don't know y.
                And I copied the .txt file to .out file so I don't have to change anything on P4.
  24 Mar 08 -- HSBC uses short date format and squote delim for 2 dgt year.
                 And I changed output file format to be more straightforward, reordering fields.
   9 Feb 09 -- Now does .qfx files, hence module name change.  And will use <tab> as output delim, just because.
                And since it really is meant for Excel to import the text file, module name change to xls.
  21 Oct 09 -- Fixed problem of CLOSE message being processed twice.
  26 Oct 09 -- Looking to trap <INVSTMTMSGSRSV1> so know this is a stock file, not credit card file which would have <CREDITCARDMSGSRSV1>
  26 Oct 09 -- Created ING qfx2xls to deal the the investment msg server1.
*)


  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  IMPORT MiscM2,ASCII;
  IMPORT FileFunc;

  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts, (* drive path name extension, *) FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle,
    MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary,
    AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName,
    CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock,
    WriteBlock, ReadChar, WriteChar, PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength,
    GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList,
    FindInOSPathList, ExpandFileSpec, FindFirst, (* FindNext, *) FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
(*
  FROM RealIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM TermFile IMPORT Open, IsTermFile, Close;
*)
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS;
(* IMPORT WINUSER, WIN32, WINGDI, WINX; *)
FROM Strings IMPORT 
	FindNext,Append,Equal,Delete,Concat,Capitalize,Assign,Extract,Insert,Replace;
FROM ExStrings IMPORT
    AppendChar, EqualI;
FROM FormatString IMPORT FormatString;
FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, FontInfo, WinAttr, ClipboardFormat,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow, NormalChildWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo, WindowTypes,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, SpecialKeys,
    GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, WriteString,
    WriteStringAt, WriteCellsAt, WriteCells, WriteLn, EraseToEOL, ChangeAttr,
    ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn,
    SetAutoScroll, WinShellToTextWindowMessage,
    MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect,
    GetBufferRect, EraseScreen, EraseRect, GetWinShellHandle, FindTextWindow,
    SetDisplayMode,GetDisplayMode,SetWindowEnable, (*  SetWindowTitle, *)
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage,
    IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars,
    SetScrollBarPos, SetWindowData, SetWindowDataNum, GetWindowData, GetWindowDataNum,
    GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos, CascadeWindow,
    SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo,
    SetScrollDisableWhenNone, SetActiveTabChild, SetTabChildPosition,
    GetForegroundWindow, SetForegroundWindow, SetWindowFont,
    SetTimer, KillTimer, DisplayHelp, SetWindowIcon,
    OpenClipboard, CloseClipboard, EmptyClipboard, ClipboardFormatAvailable,
    AllocClipboardMemory, UnlockClipboardMemory, SetClipboard, GetClipboard,
    Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;
IMPORT Terminal, BasicDialogs, DlgShell, WinShell;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings, MemUtils;
IMPORT WholeStr, LongStr, LongConv;

  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,GETTKNEOL,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE,GetHtmlCodeString;
  FROM TIMLIB IMPORT GETMDY, JULIAN;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FOPEN,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM,ChopIntoLines;
  FROM Environment IMPORT GetCommandLine;

CONST
  szAppName = "INGqfx2xls";
  InputPrompt = 'Enter cmd or HELP : ';
  LastMod = '26 Jan 10';
  qfxIcon = '#100';
  MenuSep = '|';
  EleMax = 50;

TYPE
  csvORqifORqfxType = (csv,qif,qfx);
  qfxtkntyp = (empty,string,openinghtml,closinghtml,othererror);
  qfxchrtyp = (eol,openangle,closeangle,slash,plain);
  QFXRECTYP = RECORD
    uniqID,Orgstr,AcctIDstr,dateTradedstr,dateSettledstr,datePriceAsOfstr,FITIDstr,amtstr,namestr,tickerstr,memostr, : STRTYP;
    buy : BOOLEAN; (* look for <BUYTYPE>BUY *)
    m,d,y : CARDINAL;
    juldate : LONGINT;
    unitprice, commission, totaltrans, numUnits, mktval : LONGREAL;
  END;


VAR
  SecuritiesTable : ARRAY [1..EleMax] OF QFXRECTYP;
  TableCtr : CARDINAL; (* Will try this as a pre-incr counter *)
  AvailCash, MarginBalance, ShortBalance : LONGREAL;
  qfxtoken,GblOrg,GblUserID,ledgerBalAmt,availBalAmt,BalAmtDateAsOf,outfilename,
                            comment,acntid,dtasof,dtstart,dtend,fitid,dttrade,dtsettle,memo : STRTYP;
  qfxtokenstate     : qfxtkntyp;
  C,K,c,RETCOD,m,d,y,chknum                    : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,bool,EOFFLG,OK,ok,ZeroFlag,BankTranListEnd : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  tpv1,tpv2,tpv3                               : TKNPTRTYP;
  I,J                                          : INTEGER;
  INUNT1,OUTUN1                                : MYFILTYP;
  infile          : File;
  inputline,buf,infilename         : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;
  juldate1,juldate2,juldate3                   : LONGINT;
  csvqifqfxState : csvORqifORqfxType;
  outfilelabel : STRTYP;
  cxChar  : INTEGER;
  cyChar  : INTEGER;
  cxClient: INTEGER;
  cyClient: INTEGER;
  cxBuffer: INTEGER;
  cyBuffer: INTEGER;
  cxScreen,cyScreen : COORDINATE;
  xCaret  : INTEGER;
  yCaret  : INTEGER;
  LastModLen : CARDINAL;
  a           : ScreenAttribute;
  Win         : TextWindow;
  
  
(************************************************************************************************************************)
PROCEDURE DateFieldReformat(VAR datein,dateout : ARRAY OF CHAR);
(************************************************************************************************************************)
(*                                                                  01234567    0123456789
  This procedure changes the date as it is input in a qfx file from yyyymmdd -> mm/dd/yy.
*)

BEGIN
	dateout[0] := datein[4];
	dateout[1] := datein[5];
	dateout[2] := '/';
	dateout[3] := datein[6];
	dateout[4] := datein[7];
	dateout[5] := '/';
	dateout[6] := datein[2];
	dateout[7] := datein[3];
	dateout[8] := NULL;
	
END DateFieldReformat;
  
  
  


(************************************************************************************************************************)
PROCEDURE GetQfxToken(VAR INOUT f:File; VAR OUT qfxtoken:STRTYP; VAR OUT qfxtokenstate:qfxtkntyp; VAR OUT EOFFLG:BOOLEAN);
(*********************************************** GetQfxToken **********************************
This will use the included file operations as I do not want to read by lines.  I want this
as a character stream.  Delimiters are fixed at angle brackets and EOL.
*********************************************************************************************)
VAR
        qfxtkn : STRTYP;
        qfxtknstate : qfxtkntyp;
        ch       : CHAR;
        chstate  :qfxchrtyp;

BEGIN
        qfxtkn := '';
        qfxtknstate := empty;
        EOFFLG := FALSE;
        LOOP
          ch := PeekChar(f);
          IF f.status > 0 THEN
                qfxtoken := '';
                qfxtokenstate := othererror;
                RETURN;
          END; (* if file.status is error cond *)
          IF f.eof THEN
                chstate := eol;
                EOFFLG := TRUE;
                EXIT;
          ELSE
                CASE ch OF
              EOL : chstate := eol;
      | '<' : chstate := openangle;
      | '>' : chstate := closeangle;
      | '/' : chstate := slash;
      ELSE
        chstate := plain;
      END; (* case ch *)
          END; (* if file.eof *)
          CASE qfxtknstate OF
            empty :
              CASE chstate OF
                plain,slash :
                  qfxtknstate := string;
                        ch := ReadChar(infile);
                        Strings.Append(ch,qfxtkn);
              | openangle :
                        ch := ReadChar(infile);  (* Swallow ch *)
                        qfxtknstate := openinghtml;
              | eol :
                  ch := ReadChar(infile); (* Swallow eol *)
              | closeangle :
                        MiscM2.Error(' In GetQfxToken.  Empty token got closeangle ch');

        END; (* case chstate in empty *)
    | string :
        CASE chstate OF
          plain,slash :
                        ch := ReadChar(infile);
                  Strings.Append(ch,qfxtkn);
        | eol :
                        ch := ReadChar(infile); (* Swallow EOL ch *)
                  EXIT;
        | openangle : (* openangle char is still avail for next loop iteration *)
                  EXIT;
        | closeangle :
                  MiscM2.Error(' In GetQfxToken.  String token got closeangle ch');
        END; (* case chstate in string *)
    | openinghtml :
        CASE chstate OF
          plain,openangle :
                  ch := ReadChar(infile);
                  Strings.Append(ch,qfxtkn);
        | slash :
                  ch := ReadChar(infile);
                  IF LENGTH(qfxtkn) = 0 THEN
                    qfxtknstate := closinghtml
                  ELSE
                        Strings.Append(ch,qfxtkn);
                  END;
        | closeangle,eol :
                  ch := ReadChar(infile); (* swallow ch *)
                  EXIT;
        END; (* case chstate in openinghtml *)
    | closinghtml :
              CASE chstate OF
                plain,slash,openangle :
                        ch := ReadChar(infile);
                        Strings.Append(ch,qfxtkn);
              | closeangle,eol :
                        ch := ReadChar(infile); (* swallow ch *)
                        EXIT;
              END; (* case chstate in closinghtml *)
    ELSE
        MiscM2.Error(' In GetQfxToken and tokenstate is in ELSE clause of CASE.');
    END (* case qfxtknstate *)
  END; (* loop *)

  qfxtoken := qfxtkn;
  qfxtokenstate := qfxtknstate;
END GetQfxToken;




(********************************************* Build Securities Table ***********************************)
PROCEDURE BuildSecuritiesTable;
(********************************************* Build Securities Table ***********************************)
VAR i,j,I,J : INTEGER;
	  ch : CHAR;
	  str,s0,s1,s1 : STRTYP;
	  r : LONGREAL;
	  res: LongStr.ConvResult;

BEGIN
	REPEAT 
    GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
                 IF EOFFLG THEN MiscM2.Error(' Expected <STOCKINFO> .'); RETURN; END;
    IF STRCMPFNT(qfxtoken,'CREDITCARDMSGSRSV1') = 0 THEN
      MiscM2.Error(' This file contains credit card transactions, not investment transactions.  Try Again.');
      BankTranListEnd := TRUE;
      RETURN;
    END; (* if investment msg server 1 *)
  UNTIL STRCMPFNT(qfxtoken,'STOCKINFO') = 0;
	LOOP
    IF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'SECINFO') = 0) THEN
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* Should be <SECID> and I'm going to discard this *)
                 IF EOFFLG OR (STRCMPFNT(qfxtoken,'SECID') # 0)  THEN MiscM2.Error(' Trying to get qfx record and expected SECID but got unexpedted EOF condition.'); RETURN; END;
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* Should be <UNIQUEID> *)
                 IF EOFFLG OR (STRCMPFNT(qfxtoken,'UNIQUEID') # 0)  THEN MiscM2.Error(' Trying to get UNIQUEID openinghtml but got something unexpedted.'); RETURN; END;
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* Should be actual UNIQUEID string *)
                 IF EOFFLG OR (qfxtokenstate # string) THEN MiscM2.Error(' Trying to get UNIQUEID string but got unexpedted EOF condition or token is not a string.'); RETURN; END;
      SecuritiesTable[TableCtr].uniqID := qfxtoken;
      REPEAT
      	GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* Need SECNAME openinghtml *)
                 IF EOFFLG THEN MiscM2.Error(' Expected <SECNAME> .'); RETURN; END;
      UNTIL STRCMPFNT(qfxtoken,'SECNAME') = 0;
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* Need name of security *)
                 IF EOFFLG OR (qfxtokenstate # string) THEN MiscM2.Error(' Expected name of security.'); RETURN; END;
      SecuritiesTable[TableCtr].namestr := qfxtoken;
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* Should be <TICKER> *)
                 IF EOFFLG OR (STRCMPFNT(qfxtoken,'TICKER') # 0) THEN MiscM2.Error(' Expected <TICKER> .'); RETURN; END;
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* Should be TICKER string *)
                 IF EOFFLG OR (qfxtokenstate # string) THEN MiscM2.Error(' Expected ticker symbol.'); RETURN; END;
      SecuritiesTable[TableCtr].tickerstr := qfxtoken;
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* Should be <UNITPRICE> *)
                 IF EOFFLG OR (STRCMPFNT(qfxtoken,'UNITPRICE') # 0) THEN MiscM2.Error(' Expected <UNITPRICE> .'); RETURN; END;
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* should be UNITPRICE numerical data as a string *)
                 IF EOFFLG OR (qfxtokenstate # string) THEN MiscM2.Error(' Expected UNITPRICE data.'); RETURN; END;
      LongStr.StrToReal(qfxtoken,r,res);
                 IF res # strAllRight THEN MiscM2.Error (' Expected correct real number as a string but conversion failed.'); RETURN; END;
      SecuritiesTable[TableCtr].unitprice := r; (* RConversions.StringToReal would also do this *)
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG); (* Should be <DTASOF>20091026133524.430[-7:PDT] *)
                 IF EOFFLG OR (STRCMPFNT(qfxtoken,'DTASOF') # 0) THEN MiscM2.Error(' Expected <DTASOF> .'); RETURN; END;
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
                 IF EOFFLG OR (qfxtokenstate # string) THEN MiscM2.Error(' Expected date as of string.'); RETURN; END;
      DateFieldReformat(qfxtoken,SecuritiesTable[TableCtr].datePriceAsOfstr);
      REPEAT
      	GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* Need STOCKINFO closinghtml *)
                 IF EOFFLG THEN MiscM2.Error(' Expected </STOCKINFO> .'); RETURN; END;
      UNTIL (qfxtokenstate = closinghtml) AND (STRCMPFNT(qfxtoken,'STOCKINFO') = 0);

(* If get </SECLIST> then we are done.  Else we will get <STOCKINFO>  *)
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* should be closinghtml of SECINFO *)
                 IF EOFFLG THEN MiscM2.Error(' Expecting </SECLIST> or <STOCKINFO> .'); RETURN; END;
      IF STRCMPFNT(qfxtoken,'SECLIST') = 0 THEN EXIT; END;
      TableCtr := TableCtr + 1;
  END; (* LOOP *)
END BuildSecuritiesTable;

PROCEDURE AddInvestmentTransactionsToTable;
  	VAR i,j,I,J : INTEGER;
	  ch : CHAR;
	  str,s0,s1,s1 : STRTYP;
	  r : LONGREAL;
	  res: LongStr.ConvResult;

BEGIN
	REPEAT 
  GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
                 IF EOFFLG THEN MiscM2.Error(' Expected <INVSTMTRS> .'); RETURN; END;
  UNTIL STRCMPFNT(qfxtoken,'INVSTMTRS') = 0;
  GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
                 IF EOFFLG OR (STRCMPFNT(qfxtoken,'DTASOF') # 0) THEN MiscM2.Error(' Expected <DTASOF> .'); RETURN; END;
  GetQfxToken(infile,dtasof,qfxtokenstate,EOFFLG);
                 IF EOFFLG OR (qfxtokenstate # string) THEN MiscM2.Error(' Expected string of DTASOF .'); RETURN; END;
  REPEAT
  	GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
  	             IF EOFFLG THEN MiscM2.Error(' Expected <INVTRANSLIST> .'); RETURN; END;
  UNTIL STRCMPFNT(qfxtoken,'INVTRANSLIST') = 0;
  GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
                 IF EOFFLG OR (STRCMPFNT(qfxtoken,'DTSTART') # 0) THEN MiscM2.Error(' Expected <DTSTART> .'); RETURN; END;
  GetQfxToken(infile,dtstart,qfxtokenstate,EOFFLG);
                 IF EOFFLG OR (qfxtokenstate # string) THEN MiscM2.Error(' Expected date start string.'); RETURN; END;
  GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
                 IF EOFFLG OR (STRCMPFNT(qfxtoken,'DTEND') # 0) THEN MiscM2.Error(' Expected <DTEND> .'); RETURN; END;
  GetQfxToken(infile,dtend,qfxtokenstate,EOFFLG);
                 IF EOFFLG OR (qfxtokenstate # string) THEN MiscM2.Error(' Expected date start string.'); RETURN; END;
  REPEAT
  	GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
  	             IF EOFFLG THEN MiscM2.Error(' Expected <FITID> .'); RETURN; END;
  UNTIL (STRCMPFNT(qfxtoken,'FITID') = 0) AND (qfxtokenstate = openinghtml);
  GetQfxToken(infile,fitid,qfxtokenstate,EOFFLG);
                 IF EOFFLG OR (STRCMPFNT(qfxtokenstate # string) THEN MiscM2.Error(' Expected FITID string.'); RETURN; END;
  
  

  

END AddInvestmentTransactionsToTable;




(**************************************************** GetQFXRec *********************************************)
PROCEDURE GetQFXRec(VAR OUT qfxrec : QFXTYP);
(**************************************************** GetQFXRec *********************************************)
(*
  Uses the following global variables for output
  EOFFLG, BankTranListEnd
*)

VAR I,J : INTEGER;
    found : BOOLEAN;
    ch : CHAR;
    transnum,discardthis,squoteLocn,c1,c2,patternposn : CARDINAL;
    str,s0,s1,s2 : STRTYP;

BEGIN
(* Must init record fields *)
  WITH qfxrec DO
    Orgstr := GblOrg;
    AcctIDstr := GblAcctID;
    dateTradedstr := '';
    dateSettledstr := '';
    datePriceAsOfstr := '';
    FITIDstr := '';
    amtstr := '';
    namestr := '';
    tickerstr := '';
    memostr := '';
    uniqID := '';
    buy := FALSE;
    m := 0;
    d := 0;
    y := 0;
    juldate := 0;
    unitprice := 0.0;
    commission := 0.0;
    totaltrans := 0.0;
    numUnits := 0.0;
    mktval := 0.0;
  END; (* with *)
  BankTranListEnd := FALSE;

  LOOP
    GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
    IF EOFFLG THEN
      MiscM2.Error(' Trying to get qfx record and got unexpected EOF condition.');
      RETURN;
    END;
    IF STRCMPFNT(qfxtoken,'CREDITCARDMSGSRSV1') = 0 THEN
    	MiscM2.Error(' This file contains credit card transactions, not investment transactions.  Try Again.');
    	BankTranListEnd := TRUE;
    	RETURN;
    END; (* if investment msg server 1 *)
    IF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'DTPOSTED') = 0) THEN
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
      IF EOFFLG OR (qfxtokenstate # string)  THEN
        MiscM2.Error(' Trying to get qfx record and got unexpedted EOF condition or token is not a string.');
        RETURN;
      END;
(*      Strings.Extract(qfxtoken,0,8,qfxrec.datePostedstr); *)
      DateFieldReformat(qfxtoken,qfxrec.datePostedstr);
      ok := StrToCard(qfxtoken[5..6],qfxrec.m);
      ok := StrToCard(qfxtoken[7..8],qfxrec.d);
      ok := StrToCard(qfxtoken[1..4],qfxrec.y);
      qfxrec.juldate := JULIAN(qfxrec.m,qfxrec.d,qfxrec.y);
    ELSIF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'DTUSER') = 0) THEN
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
      IF EOFFLG OR (qfxtokenstate # string)  THEN
        MiscM2.Error(' Trying to get qfx record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
(*      Strings.Extract(qfxtoken,0,8,qfxrec.dateUserstr); *)
      DateFieldReformat(qfxtoken,qfxrec.dateUserstr);
    ELSIF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'TRNAMT') = 0) THEN
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
      IF EOFFLG OR (qfxtokenstate # string)  THEN
        MiscM2.Error(' Trying to get qfx record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      qfxrec.amtstr := qfxtoken;
    ELSIF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'FITID') = 0) THEN
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
      IF EOFFLG OR (qfxtokenstate # string)  THEN
        MiscM2.Error(' Trying to get qfx record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      qfxrec.FITIDstr := qfxtoken;
    ELSIF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'NAME') = 0) THEN
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
      IF EOFFLG OR (qfxtokenstate # string)  THEN
        MiscM2.Error(' Trying to get qfx record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      qfxrec.namestr := qfxtoken;
(*
  PROCEDURE FindNext(pattern, stringToSearch : ARRAY OF CHAR; startIndex : CARDINAL; VAR OUT patternFound : BOOLEAN;
                   VAR OUT posOfPattern : CARDINAL);
  PROCEDURE Delete(VAR INOUT stringVar : ARRAY OF CHAR; startIndex, numberToDelete: CARDINAL);
*)
      REPEAT
        c1 := 0;
        Strings.FindNext('amp;',qfxrec.namestr,c1,found,patternposn);
        IF found THEN
          Strings.Delete(qfxrec.namestr,patternposn,4);
        END;
      UNTIL NOT found;
    ELSIF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'MEMO') = 0) THEN
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
      IF EOFFLG OR (qfxtokenstate # string)  THEN
        MiscM2.Error(' Trying to get qfx record and got unexpected EOF condition or token is not a string.');
        RETURN;
      END;
      qfxrec.memostr := qfxtoken;
    ELSIF (qfxtokenstate = closinghtml) AND (STRCMPFNT(qfxtoken,'STMTTRN') = 0) THEN
    	RETURN;

    ELSIF (qfxtokenstate = closinghtml) AND (STRCMPFNT(qfxtoken,'BANKTRANLIST') = 0) THEN
      BankTranListEnd := TRUE;
      EXIT;
    END; (* if qfxtknstate = *)
  END; (* loop for record contents *)
END GetQFXRec;


(**********************************************************************************************)
PROCEDURE ProcessQFXFile(tw : TextWindow);
(* This file is first to get control from the windows procedure and actually start processing *)
(**********************************************************************************************)

VAR I,J : INTEGER;
   tpv : TKNPTRTYP;
   buf : BUFTYP;
   transnum,strlen,k,c1,patternposn : CARDINAL;
   qfxrec : QFXTYP;
   found : BOOLEAN;

BEGIN
(* Need to get header info for ORG and ACCTID *)
  BankTranListEnd := FALSE;
  LOOP
    GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
(*
        MiscM2.WriteString(' in ProcessQFXFile and trying to get header.  qfxtoken = ');
        MiscM2.WriteString(qfxtoken);
        MiscM2.WriteLn;
        MiscM2.WriteString(' and qfxtokenstate is ');
        MiscM2.WriteCard(ORD(qfxtokenstate));
        MiscM2.WriteLn;
        MiscM2.PressAnyKey;
*)
    IF EOFFLG THEN
      MiscM2.Error(' Trying to get header info and got EOF condition.');
      RETURN;
    END;
    IF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'ORG') = 0) THEN
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
      IF EOFFLG OR (qfxtokenstate # string)  THEN
        MiscM2.Error(' Trying to get ORG info and got EOF condition or token is not a string.');
        RETURN;
      END;
      GblOrg := qfxtoken;
(*      BasicDialogs.MessageBox(GblOrg,MsgInfo); *)
    ELSIF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'INTU.USERID') = 0) THEN
      GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
        IF EOFFLG OR (qfxtokenstate # string)  THEN
          MiscM2.Error(' Trying to get INTU.USERID and got EOF condition or token is not a string.');
          RETURN;
        END;
        GblUserID := qfxtoken;
(*        BasicDialogs.MessageBox(GblUserID,MsgInfo); *)
        EXIT;
    END; (* if qfxtknstate = *)
  END; (* loop for header info *)

(*   Build outfnam *)
  outfilename := '';
  strlen := LENGTH(GblOrg);
(*
  MiscM2.WriteString(' gblorg strlen =');
  MiscM2.WriteCard(strlen);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;
*)
  k := 1;
  REPEAT  (* assume 1st char is not a space or dot *)
    outfilename[k] := GblOrg[k];
    INC(k);
  UNTIL (k > strlen) OR (GblOrg[k] = ' ') OR (GblOrg[k] = '.');
  outfilename[k] := NULL;
(*
  MiscM2.WriteString(' outfilename=');
  MiscM2.WriteString(outfilename);
  MiscM2.WriteLn;
  MiscM2.PressAnyKey;
*)
  Strings.Concat(outfilename,'.txt',OUTFNAM.CHARS);
  FUNC FileFunc.DeleteFile(OUTFNAM.CHARS);
  TRIM(OUTFNAM);
  FOPEN(OUTUN1,OUTFNAM,WR);
  
(* Build a comment to include ORG name and acnt ID *)
  acntid := GblUserID;
  REPEAT
    c1 := 0;
    Strings.FindNext('X',acntid,c1,found,patternposn);
    IF found THEN
      Strings.Delete(acntid,patternposn,1);
    END;
  UNTIL NOT found;

  Strings.Concat(outfilename,acntid,comment);

  BuildSecuritiesTable;        (* done *)

(*  Reset input file so can start next pass thru file. *)
  RemoveFileBuffer(infile);
  CloseFile(infile);
  OpenFile(infile,infilename,ReadOnlyDenyWrite);
  SetFileBuffer(infile,InBuf);
  
  AddInvestmentTransactionsToTable;
  AddPositionsListToTable;
  
  LOOP (* to read multiple records, written when reading and writing indiv records.  Now need to build securities table *)
    GetQFXRec(GBLqfxRec);  (* the EOFFLG and BankTranListEnd globals would be set by GetQFXRec *)
    IF BankTranListEnd THEN
        EXIT
    ELSIF EOFFLG THEN
        RETURN
    END(*IF*);

    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,GBLqfxRec.datePostedstr);
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,ASCII.ht);

(* dollar amt *)
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,GBLqfxRec.amtstr);
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,ASCII.ht);

(* Trans name/description field *)
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,GBLqfxRec.namestr); (* output description field *)
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,ASCII.ht);

(* Trans Memo field.  Not used by Citibank, but HSBC does.  It will be written as a comment field *)

    IF LENGTH(GBLqfxRec.memostr) = 0 THEN
        GBLqfxRec.memostr := GBLqfxRec.FITIDstr;
    ELSE
     Strings.Append('  ',GBLqfxRec.FITIDstr);
     k := 0;
     Strings.Insert(GBLqfxRec.FITIDstr,k,GBLqfxRec.memostr);
    END;
    Strings.Append(': ',GBLqfxRec.memostr);
    Strings.Append(comment,GBLqfxRec.memostr);

    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,GBLqfxRec.memostr); (* output comment field *)
    FWRSTR(OUTUN1,'"');
    FWRLN(OUTUN1);

    WriteString(tw,'"',a);
    WriteString(tw,GBLqfxRec.datePostedstr,a);
    WriteString(tw,'","',a);
    WriteString(tw,GBLqfxRec.amtstr,a);
    WriteString(tw,'","',a);
    WriteString(tw,GBLqfxRec.namestr,a);
    WriteString(tw,'","',a);
    WriteString(tw,GBLqfxRec.memostr,a);
    WriteString(tw,'"',a);
    WriteLn(tw);

  END(*LOOP for multiple records*);

  (* Get Footer containing ledgerbal, balamt, dtasof.  Stop when come TO </OFX> *)

  ledgerBalAmt := '';
  availBalAmt := '';

  LOOP (* The only exit out of this loop is the EOFFLG condition because some qfx files, Citibank, have AVAILBAL and some not, HSBC. *)
        GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
        IF EOFFLG THEN
          (* MiscM2.Error(' Trying to get footer info and got EOF condition.'); *)
          EXIT;
        END;
        IF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'BALAMT') = 0) THEN
          GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
          IF EOFFLG OR (qfxtokenstate # string)  THEN
            (* MiscM2.Error(' Trying to get footer info and got EOF condition or token is not a string.');*)
            EXIT;
          END;
          ledgerBalAmt := qfxtoken;
        ELSIF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'DTASOF') = 0) THEN
          GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
          IF EOFFLG OR (qfxtokenstate # string)  THEN
            (* MiscM2.Error(' Trying to get footer info and got EOF condition or token is not a string.'); *)
            EXIT;
          END;
(*          Strings.Extract(qfxtoken,0,8,BalAmtDateAsOf); *)
          DateFieldReformat(qfxtoken,BalAmtDateAsOf);
        ELSIF (qfxtokenstate = openinghtml) AND (STRCMPFNT(qfxtoken,'AVAILBAL') = 0) THEN
          GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);  (* this token should be <balamt> to get and discard *)
          IF EOFFLG THEN EXIT END;
          GetQfxToken(infile,qfxtoken,qfxtokenstate,EOFFLG);
          IF EOFFLG OR (qfxtokenstate # string)  THEN
            EXIT;
          END;
          availBalAmt := qfxtoken;
        END; (* if qfxtknstate = *)
  END; (* loop for header info *)

(* Write out the ledgerBalAmt and availBalAmt as entries. *)
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,BalAmtDateAsOf);
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,ASCII.ht);

    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,ledgerBalAmt);
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,ASCII.ht);

    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,'Ledger Balance Amount'); (* output description field *)
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,ASCII.ht);

    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,comment); (* output comment field *)
    FWRSTR(OUTUN1,'"');

    FWRLN(OUTUN1);

    WriteString(tw,'"',a);
    WriteString(tw,BalAmtDateAsOf,a);
    WriteString(tw,'","',a);
    WriteString(tw,ledgerBalAmt,a);
    WriteString(tw,'","',a);
    WriteString(tw,'Ledger Balance',a);
    WriteString(tw,'","',a);
    WriteString(tw,comment,a);
    
    WriteString(tw,'"',a);
    WriteLn(tw);

    IF LENGTH(availBalAmt) > 0 THEN
        FWRSTR(OUTUN1,'"');
        FWRSTR(OUTUN1,BalAmtDateAsOf);
        FWRSTR(OUTUN1,'"');
      FWRSTR(OUTUN1,ASCII.ht);

      FWRSTR(OUTUN1,'"');
      FWRSTR(OUTUN1,availBalAmt);
      FWRSTR(OUTUN1,'"');
      FWRSTR(OUTUN1,ASCII.ht);

      FWRSTR(OUTUN1,'"');
      FWRSTR(OUTUN1,'Credit Amount Available'); (* output description field *)
      FWRSTR(OUTUN1,'"');
      FWRSTR(OUTUN1,ASCII.ht);
      
      FWRSTR(OUTUN1,'"');
      FWRSTR(OUTUN1,comment);
      FWRSTR(OUTUN1,'"');
      
      FWRLN(OUTUN1);

      WriteString(tw,'"',a);
      WriteString(tw,BalAmtDateAsOf,a);
      WriteString(tw,'","',a);
      WriteString(tw,availBalAmt,a);
      WriteString(tw,'","',a);
      WriteString(tw,'Credit Amount Available',a);
      WriteString(tw,'","',a);
      WriteString(tw,comment,a);

      WriteString(tw,'"',a);
      WriteLn(tw);
    END; (* if availBalAmt exists *)

END ProcessQFXFile;



(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr         : Colors;
    x,y         : COORDINATE;
    i,int       : INTEGER;
    cmdline     : ARRAY [0..255] OF CHAR;
    cmdbuf,tkn  : BUFTYP;
    tknstate    : FSATYP;
    retcod,c5   : CARDINAL;
    filter,s    : STRTYP;

BEGIN
    CASE msg.msg OF
    TWM_CLOSE:  (* Turns out that this winmsg is being executed twice before the pgm closes.  I have no idea why *)
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
            RETURN OkayToClose;  (* This statement fixed the executed twice pblm.  I figured out why and then how to fix it *)
        END;
(*        BasicDialogs.MessageBox(outfilename,MsgInfo); *)
        Strings.Append('.xls',outfilename);
(*
        BasicDialogs.MessageBox(outfilename,MsgInfo);
        BasicDialogs.MessageBox(OUTFNAM.CHARS,MsgInfo);
*)
        FileFunc.CopyFile(OUTFNAM.CHARS,outfilename);
        RETURN OkayToClose;
    | TWM_CREATE:
        FUNC SetWindowIcon(tw, qfxIcon);

        xCaret := 0;
        yCaret := 0;
        inputline := '';
        juldate1 := 0;
        juldate2 := 0;
        juldate3 := 0;
        chknum := 0;

        INFNAM.CHARS := '';
        OUTFNAM.CHARS := '';

        filter := 'qfx Files';
        Strings.Append(MenuSep,filter);
        Strings.Append('*.qfx',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('qif Files',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('*.qif',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('comma Text',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('*.csv',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('All',filter);
        Strings.Append(MenuSep,filter);
        Strings.Append('*',filter);
        Strings.Append(MenuSep,filter);

(*
  PROCEDURE BasicDialogs.PromptOpenFile(VAR INOUT name : ARRAY OF CHAR;
                                            filters : ARRAY OF CHAR;
                                            VAR INOUT defFilter : CARDINAL;
                                            defDir : ARRAY OF CHAR;
                                            defExt : ARRAY OF CHAR;
                                            title : ARRAY OF CHAR;
                                            createable : BOOLEAN) : BOOLEAN;
 Opens an operating system common dialog for opening  a file
   filters specifies a list of file extension filters that are
   separated by semicolons.
   The format for filters is as follows.
   defDir = the default directory to start the dialog in
   an empty string "" means use the current directory.
   defExt = the default file extension to use if the user does not
   provide an extension. "" means no default extension.
   the extension should *not* have a leading '.' character.
   title = the caption text of the dialog. title can be empty "".
   in this case the default operating system title is used.
   If createable = TRUE then the file need not already exist, otherwise
   the file must exist for the dialog to return successful.
   RETURNs TRUE is successful and name will contain the file specification
   for the file the user has given.
*)
        c5 := 1;
        DlgShell.ConvertToNulls(MenuSep,filter);
        bool := BasicDialogs.PromptOpenFile(infilename,filter,c5,'','','Open transaction file',FALSE);
(*        BasicDialogs.MessageBox(infilename,MsgInfo); *)
        IF NOT bool THEN
          WriteString(tw,'Could not find file.  Does it exist?',a);
          HALT;
        END;

        IF NOT FileFunc.FileExists(infilename) THEN
          MiscM2.Error(' Could not find input file.  Does it exist?');
          HALT;
        END(*if*);
        OpenFile(infile,infilename,ReadOnlyDenyWrite);
        IF infile.status > 0 THEN
          WriteString(tw,' Error in opening/creating file ',a);
          WriteString(tw,inputline,a);
          WriteString(tw,'--',a);
          CASE TranslateFileError(infile) OF
            FileErrFileNotFound : WriteString(tw,'File not found.',a);
          | FileErrDiskFull : WriteString(tw,'Disk Full',a);
          ELSE
            WriteString(tw,'Nonspecific error occured.',a);
          END(*CASE*);
          WriteLn(tw);
          WriteString(tw,' Program Terminated.',a);
          WriteLn(tw);
          HALT;
        END(*IF infile.status*);
        SetFileBuffer(infile,InBuf);

        C := LENGTH(infilename);
        DEC(C);
        buf[0] := CAP(infilename[C-2]);
        buf[1] := CAP(infilename[C-1]);
        buf[2] := CAP(infilename[C]);
        buf[3] := 0C;

        IF STRCMPFNT(buf,'QFX') = 0 THEN
                csvqifqfxState := qfx;
        ELSIF STRCMPFNT(buf,'QIF') = 0 THEN
          csvqifqfxState := qif;
        ELSE
          csvqifqfxState := csv
        END;

    | TWM_SIZE:
        GetClientSize(tw,cxScreen,cyScreen);
        cxClient := msg.width;
        cyClient := msg.height;
        SnapWindowToFont(tw,TRUE);
        SetDisplayMode(tw,DisplayNormal);
        SetScrollRangeAllowed(tw,WA_VSCROLL,60);
        SetScrollBarPos(tw,WA_VSCROLL,0);
        SetScrollRangeAllowed(tw,WA_HSCROLL,100);
        SetScrollBarPos(tw,WA_HSCROLL,0);
        SetCaretType(tw,CtHalfBlock);
        MoveCaretTo(tw,xCaret,yCaret);
        MakeCaretVisible(tw);
        CaretOn(tw);
        SetWindowEnable(tw,TRUE);
        SetForegroundWindow(tw);

    | TWM_GAINFOCUS, TWM_ACTIVATEAPP :
        MoveCaretTo(tw,xCaret, yCaret);
        MakeCaretVisible(tw);
    | TWM_PAINT:
        CASE csvqifqfxState OF
          qfx: ProcessQFXFile(tw);
        | qif: MiscM2.Error(' This pgm will only process qfx files.');
        | csv: MiscM2.Error(' This pgm will only process qfx files.');
        END (*case*);

        RemoveFileBuffer(infile);
        CloseFile(infile);
        FCLOSE(OUTUN1);
        WriteLn(tw);
        WriteString(tw,OUTFNAM.CHARS,a);
        WriteString(tw,' file now closed.',a);
        WriteLn(tw);
        EraseToEOL(tw,a);
        WriteLn(tw);
        WriteLn(tw);
        INC(c32);
        FUNC FormatString(' Number of Paint msgs is: %c.',buf,c32);
        WriteString(tw,buf,a);
        WriteLn(tw);
        WriteStringAt(tw,0,cyClient-1,LastMod,a);

    | TWM_MENU:
(*
  a menu item has been selected menuId = the menu resource id number for the menu item
  TWM_MENU:
       msg.menuId      : INTEGER;
       msg.accel       : BOOLEAN;
*)
         CASE msg.menuId OF
         20  : (* exit *)
              CloseWindow(tw,CM_REQUEST);
      ELSE (* do nothing but not an error *)
      END; (* case menuId *)


    |
    TWM_KEY:
     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN
        CASE msg.k_ch OF
          CHR(8) :                                     (* backspace       *)
          FUNC CloseWindow(tw, CM_REQUEST);
        | CHR(9) :                                     (* tab             *)
          FUNC CloseWindow(tw, CM_REQUEST);

        | CHR(10):                                     (* line feed       *)

        | CHR(13):                                     (* carriage RETURN *)
          FUNC CloseWindow(tw, CM_REQUEST);

        | CHR(27):                                     (* escape *)
          FUNC CloseWindow(tw, CM_REQUEST);
        | CHR(32):                                     (* space *)
          FUNC CloseWindow(tw, CM_REQUEST);
        | 'A','a': (* About *)
             BasicDialogs.MessageTitle := 'About';
             Strings.Assign('Last Modified and Compiled ',s);
             Strings.Append(LastMod,s);
             BasicDialogs.MessageBox(s, BasicDialogs.MsgInfo);
        ELSE (* CASE ch *)
          FUNC CloseWindow(tw, CM_REQUEST);
        END (* case ch *);
      ELSIF msg.k_special = KEY_PAGEUP THEN
      ELSIF msg.k_special = KEY_PAGEUP THEN
      ELSIF msg.k_special = KEY_PAGEDOWN THEN
      ELSIF msg.k_special = KEY_HOME THEN
      ELSIF msg.k_special = KEY_END THEN
      ELSIF msg.k_special = KEY_RIGHTARROW THEN
      ELSIF msg.k_special = KEY_LEFTARROW THEN
      ELSIF msg.k_special = KEY_UPARROW THEN
      ELSIF msg.k_special = KEY_DOWNARROW THEN
      ELSIF msg.k_special = KEY_INSERT THEN
      ELSIF msg.k_special = KEY_DELETE THEN

      ELSE (* msg.k_special *)
      END (*if*);
     END(* for *);
    ELSE (* case msg.msg *)
    END (* case msg.msg *);

    RETURN DEFAULT_HANDLE;
END WndProcTW;

PROCEDURE Start(param : ADDRESS);
BEGIN
    UNREFERENCED_PARAMETER(param);
    Win := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "qfx To text xls Converter", (* name : ARRAY OF CHAR *)
                        "#100",        (* menu : ARRAY OF CHAR *)
                        "qfxIcon",        (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        110,20, (* xSize, ySize : COORDINATE *)
                        250,100, (* xBuffer, yBuffer : COORDINATE *)
                        FALSE,  (* gutter : BOOLEAN *)
                        DefaultFontInfo, (* font : FontInfo *)
                        ComposeAttribute(Black, White, NormalFont), (* background : ScreenAttribute *)
                        ToplevelWindow,  (* windowType : WindowTypes *)
                        WndProcTW,
                        NormalWindow + AddScrollBars,    (* attribs : WinAttrSet *)
                        NIL); (* createParam : ADDRESS *)
    IF Win = NIL THEN
        WinShell.TerminateDispatchMessages(127);
    END;
    SetAutoScroll(Win,TRUE);
END Start;

(********************************** Main body ************************************)
BEGIN
  LastModLen := LENGTH(LastMod);
  a := ComposeAttribute(Black, White, NormalFont);
  c32 := 0;
  TableCtr := 0;
  AvailCash := 0.0;
  MarginBalance := 0.0;
  ShortBalance := 0.0;

  FUNC WinShell.DispatchMessages(Start, NIL);

END INGqfx2xls.
(*
<INVSTMTMSGSRSV1>
<CREDITCARDMSGSRSV1>
<DTASOF>20091026133524.150[-7:PDT]<CURDEF>USD
<INVACCTFROM><BROKERID>sharebuilder.com<ACCTID>0003065424</INVACCTFROM>
<INVTRANLIST>
<DTSTART>20091019050000.000[-7:PDT]<DTEND>20091026050000.000[-7:PDT]
<BUYSTOCK>
<INVBUY>
<INVTRAN><FITID>344560813<DTTRADE>20091026040000.000[-7:PDT]<DTSETTLE>20091029040000.000[-7:PDT]<MEMO>order posting</INVTRAN>
<SECID><UNIQUEID>17275R102<UNIQUEIDTYPE>CUSIP</SECID>
<UNITS>40<UNITPRICE>24<COMMISSION>9.95<TOTAL>-969.95<SUBACCTSEC>CASH<SUBACCTFUND>CASH</INVBUY>
<BUYTYPE>BUY
</BUYSTOCK>
</INVTRANLIST>
<INVPOSLIST>
<POSSTOCK><INVPOS><SECID><UNIQUEID>084423102<UNIQUEIDTYPE>CUSIP</SECID>
<HELDINACCT>CASH<POSTYPE>LONG<UNITS>40<UNITPRICE>24.78<MKTVAL>991.2<DTPRICEASOF>20091026133524.433[-7:PDT]</INVPOS></POSSTOCK>
<POSSTOCK><INVPOS><SECID><UNIQUEID>17275R102<UNIQUEIDTYPE>CUSIP</SECID>
<HELDINACCT>CASH<POSTYPE>LONG<UNITS>40<UNITPRICE>23.7<MKTVAL>948<DTPRICEASOF>20091026133524.433[-7:PDT]</INVPOS></POSSTOCK>
<POSSTOCK><INVPOS><SECID><UNIQUEID>219350105<UNIQUEIDTYPE>CUSIP</SECID>
<HELDINACCT>CASH<POSTYPE>LONG<UNITS>60<UNITPRICE>15.51<MKTVAL>930.6<DTPRICEASOF>20091026133524.433[-7:PDT]</INVPOS></POSSTOCK>
<POSSTOCK><INVPOS><SECID><UNIQUEID>92343V104<UNIQUEIDTYPE>CUSIP</SECID>
<HELDINACCT>CASH<POSTYPE>LONG<UNITS>100<UNITPRICE>28.64<MKTVAL>2864<DTPRICEASOF>20091026133524.433[-7:PDT]
</INVPOS></POSSTOCK></INVPOSLIST>
<INVBAL>
<AVAILCASH>65.2<MARGINBALANCE>0<SHORTBALANCE>0
</INVBAL></INVSTMTRS></INVSTMTTRNRS>
</INVSTMTMSGSRSV1>
<SECLISTMSGSRSV1>
<SECLIST>
<STOCKINFO><SECINFO><SECID><UNIQUEID>17275R102<UNIQUEIDTYPE>CUSIP</SECID>
<SECNAME>CISCO SYSTEMS INC<TICKER>CSCO<UNITPRICE>23.7<DTASOF>20091026133524.430[-7:PDT]</SECINFO>
<ASSETCLASS>OTHER</STOCKINFO>
<STOCKINFO><SECINFO><SECID><UNIQUEID>92343V104<UNIQUEIDTYPE>CUSIP</SECID>
<SECNAME>VERIZON COMMUNICATIONS<TICKER>VZ<UNITPRICE>28.64<DTASOF>20091026133524.453[-7:PDT]</SECINFO>
<ASSETCLASS>OTHER</STOCKINFO>
<STOCKINFO><SECINFO><SECID><UNIQUEID>219350105<UNIQUEIDTYPE>CUSIP</SECID>
<SECNAME>CORNING INC<TICKER>GLW<UNITPRICE>15.51<DTASOF>20091026133524.450[-7:PDT]</SECINFO><ASSETCLASS>OTHER</STOCKINFO>
<STOCKINFO><SECINFO><SECID><UNIQUEID>084423102<UNIQUEIDTYPE>CUSIP</SECID>
<SECNAME>BERKLEY W R CORP<TICKER>WRB<UNITPRICE>24.78<DTASOF>20091026133524.433[-7:PDT]</SECINFO>
<ASSETCLASS>OTHER</STOCKINFO>
</SECLIST>
</SECLISTMSGSRSV1>
</OFX>


*)
(*
  PROCEDURE StrToReal(str : ARRAY OF CHAR; VAR real : LONGREAL; VAR res : ConvResults);
  ConvResults = ConvTypes.ConvResults; /* strAllRight, strOutOfRange, strWrongFormat, strEmpty*/  
*)
