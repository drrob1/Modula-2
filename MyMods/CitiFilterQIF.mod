<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:Citi.RES*>
%ELSE
%END
MODULE CitiFilterQIF;
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
  17 Jun 08 -- Had to change the way checking, savings, unknown is assigned as Citibank changed its d/l'd filenames.
   4 Jun 17 -- Started coding the number fields changes because of Citibank.  And removed the test for "AUTHORIZED TRANSFER".
                 I don't remember why that's there.
*)


(*  Sample output file data
"CLEARED","09-28-2007","0","BROOKLYN HOSPITA PAYROLL","656.74","CHK.ASC","22326861","187"
"CLEARED","09-28-2007","11374","VERIZON WIRELESS (4) REFERENCE # 011374","-27.66","CHK.ASC","22326861","187"
"CLEARED","09-28-2007","11375","LIPA REFERENCE # 011375","-500.00","CHK.ASC","22326861","187"
"CLEARED","09-28-2007","2","CASH WITHDRAWAL AT CBC 00233 ","-1,000.00","CHK.ASC","22326861","187"
"CLEARED","09-25-2007","0","PAYPAL           TRANSFER","27.23","CHK.ASC","22326861","187"
"CLEARED","09-25-2007","04353","CHECK   4353 ","-15.00","CHK.ASC","22326861","187"
"CLEARED","09-24-2007","04357","CHECK   4357 ","-100.00","CHK.ASC","22326861","187"
*)

  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  IMPORT MiscM2;
  IMPORT FileFunc;
(*
  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet, SearchEntry, FileNameParts /*drive path name extension*/, FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle, MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary, AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName, CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock, WriteBlock, /* ReadChar, WriteChar, */ PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength, GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile, FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList, FindInOSPathList, ExpandFileSpec, FindFirst, /* FindNext, */ FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath, SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;

  FROM RealIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM TermFile IMPORT Open, IsTermFile, Close;
*)
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS;
(* IMPORT WINUSER, WIN32, WINGDI, WINX; *)
FROM Strings IMPORT FindNext, Append, Equal, Delete, Concat, Capitalize;
FROM ExStrings IMPORT
    AppendChar, EqualI;
FROM FormatString IMPORT FormatString;
FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure, NormalFont, BoldFont, ItalicFont, WinAttr, ClipboardFormat,
    DisplayModes, ScreenAttribute, CaretTypes, TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, WindowTypes, SpecialKeys, GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType, IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, WriteString,
    WriteStringAt, WriteCellsAt, WriteCells, WriteLn, EraseToEOL, ChangeAttr, ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn,
    SetAutoScroll, WinShellToTextWindowMessage, MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect,
    GetBufferRect, EraseScreen, EraseRect, GetWinShellHandle, FindTextWindow, SetDisplayMode,GetDisplayMode,SetWindowEnable,
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage, IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars,
    SetScrollBarPos, SetWindowData, SetWindowDataNum, GetWindowData, GetWindowDataNum, GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos, CascadeWindow,
    SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo, SetScrollDisableWhenNone, SetActiveTabChild, SetTabChildPosition,
    GetForegroundWindow, SetForegroundWindow, SetTimer, KillTimer, DisplayHelp, OpenClipboard, CloseClipboard, EmptyClipboard, ClipboardFormatAvailable,
    AllocClipboardMemory, UnlockClipboardMemory, SetClipboard, GetClipboard, Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;
IMPORT Terminal, BasicDialogs, DlgShell, WinShell;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings, MemUtils;
IMPORT WholeStr, LongStr, LongConv;
IMPORT ASCII;

  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP, BUFTYP,MAXCARDFNT, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR, APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,GETTKNEOL, UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE;
  FROM TIMLIBrevised IMPORT GETMDY, JULIAN;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP, IOSTATE,FOPEN,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,
  	FWRTXLN,RETBLKBUF,FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;
  FROM Environment IMPORT GetCommandLine;

CONST
  szAppName = "CitiFilterQIF";
  InputPrompt = "Enter cmd or HELP : ";
  LastMod = "4 Jun 17";
  CitiIcon = "#100";
  MenuSep = '|';

TYPE
  csvORqifType = (csv,qif);
  QIFTYP = RECORD
    datestr, numstr, Pstr, Mstr, amtstr : STRTYP;
    m,d,y,num : CARDINAL;
    juldate : CARDINAL;
  END;


VAR
  C,K,c,RETCOD,m,d,y,chknum                    : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,bool,EOFFLG,OK,ok,ZeroFlag,patfnd : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  tpv1,tpv2,tpv3                               : TKNPTRTYP;
  I,J                                          : INTEGER;
  INUNT1,OUTUN1                                : MYFILTYP;
  inputline, buf                               : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;
  juldate1,juldate2,juldate3                   : CARDINAL;
  csvqifState : csvORqifType;
  outfilelabel : STRTYP;
  GBLqif  : QIFTYP;
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
  patrnFound  : BOOLEAN;

(* ------------------------------------------------------------------------- ProcessCVSFile ------------------------- *)

PROCEDURE ProcessCSVFile(tw : TextWindow);
(* Sample input file data
"cleared","06-13-2006","","INTEREST","","20.34","","9964707466","461"
"cleared","06-12-2006","00212","TRANSFER FROM PREFERRED MONEY MARKET","Reference # 000212","40,000.00","","9964707466","461"
"cleared","05-26-2006","","AUTHORIZED TRANSFER","BROOKLYN HOSPITA PAYROLL","909.09","","22326861","187"
"cleared","05-26-2006","11185","DISCOVER CARD","Reference # 011185","-200.00","","22326861","187"
"cleared","05-26-2006","11186","KEYSPAN ENERGY DEL-L","Reference # 011186","-200.00","","22326861","187"
"cleared","05-26-2006","11187","LIPA","Reference # 011187","-300.00","","22326861","187"
"cleared","05-23-2006","04252","CHECK   4252","","-150.00","","22326861","187"
"cleared","05-22-2006","","DEPOSIT","","169.99","","22326861","187"
"cleared","06-13-2006","","INTEREST","","39.86","","45863483","170"
"cleared","06-12-2006","00212","TRANSFER TO PREFERRED MONEY MARKET","Reference # 000212","-40,000.00","","45863483","170"
*)


VAR I,J : INTEGER;
   tpv : TKNPTRTYP;
   buf : BUFTYP;
   transnum,discardthis,squoteLocn : CARDINAL;

BEGIN
  LOOP (* to read multiple lines *)
    FRDTXLN(INUNT1,INBUF,250,EOFFLG);
    IF EOFFLG THEN EXIT END(*IF*);
(*
  Remove squotes
*)
    squoteLocn := LCHINBUFFNT(INBUF, "'");
    WHILE squoteLocn > 0 DO
      RMVCHR(INBUF,squoteLocn,1);
      squoteLocn := LCHINBUFFNT(INBUF, "'");
    END;

    INI1TKN(tpv,INBUF);
    FOR J := 1 TO 2 DO (* just write first 2 tokens as we need to process the 3rd *)
      GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);
      IF RETCOD > 0 THEN EXIT END;
      FWRSTR(OUTUN1,'"');
      FWRTX(OUTUN1,TOKEN);
      FWRSTR(OUTUN1,'",');
      WriteString(tw,TOKEN.CHARS,a);
      WriteString(tw,',',a);
    END(*for*);

(* TOKEN is a date string and can be processed as such *)
    ok := StrToCard(TOKEN.CHARS[1..2],m);
    ok := StrToCard(TOKEN.CHARS[4..5],d);
    ok := StrToCard(TOKEN.CHARS[7..10],y);
    IF juldate1 = 0 THEN
      juldate1 := JULIAN(m,d,y);
    ELSE
      juldate2 := JULIAN(m,d,y);
    END(*IF*);
    IF  (juldate2 > 0) AND (juldate1 <> juldate2) THEN
      juldate1 := juldate2;
      juldate2 := 0;
      chknum := 0;
    END(*if*);

    GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);  (* Was a null field that needed to be transnum, *)
                  (* but the field format change corrected this and it is transnum again. *)
(*    ExtractLastNum(TOKEN,transnum);                                                     *)
(*    FindNext("CASH WITHDRAWAL AT",TOKEN.CHARS,0,ok,discardthis);                        *)
(*    IF (transnum = 0) OR ok THEN                                                        *)
    IF TOKEN.COUNT = 0 THEN
        ok := CardToStr(chknum,buf.CHARS);
        TRIM(buf);
        INC(chknum);
    ELSE
        buf := TOKEN;
    END;
    FWRSTR(OUTUN1,'"');
    FWRTX(OUTUN1,buf);   (* output transnum *)
    FWRSTR(OUTUN1,'",');

    GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);  (* Trans name description, part 1 *)
    FWRSTR(OUTUN1,'"');
   (*    TRIM(TOKEN);  Not needed after Citibank divided this field as they removed most extra spaces *)
    IF STRCMPFNT(TOKEN.CHARS,'AUTHORIZED TRANSFER') <> 0 THEN
        FWRTX(OUTUN1,TOKEN); (* output trans name part 1 only if not above phrase *)
      FWRSTR(OUTUN1," ");
    END;
    GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);  (* Trans name description, part 2 *)
    FWRTX(OUTUN1,TOKEN); (* output trans name part 2 *)
    FWRSTR(OUTUN1,'",');

    WriteString(tw,buf.CHARS,a);
    WriteString(tw,TOKEN.CHARS,a);  (* Will only output 2nd half of description, but it's enough *)

    GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);  (* dollar amt *)
    FWRSTR(OUTUN1,'"');
    FWRTX(OUTUN1,TOKEN); (* output dollar amt *)
    FWRSTR(OUTUN1,'",');
    WriteString(tw,TOKEN.CHARS,a);

    (* Back to output acnt name *)
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,outfilelabel);
    FWRSTR(OUTUN1,'"');
    GETTKN(tpv,TOKEN,TKNSTATE,I,RETCOD);   (* discard null field that used to say acnt name *)

    GETTKNEOL(tpv,TOKEN,RETCOD);
    FWRTXLN(OUTUN1,TOKEN);
    WriteString(tw,TOKEN.CHARS,a);
    WriteLn(tw);

  END(*LOOP*);
  IF tpv <> NIL THEN
    DISPOSE(tpv);
  END;
END ProcessCSVFile;

(* ------------------------------------------------------------------------- GetTransactionNumber ------------------------- *)
PROCEDURE GetTransactionNumber(s : STRTYP; VAR numstr : STRTYP) : CARDINAL;
  VAR
  	tpv : TKNPTRTYP = NIL;
  	token : BUFTYP;
  	tknstate : FSATYP;
  	transnum : INTEGER;
  	retcod : CARDINAL;
BEGIN
	(* I have to get a token until either EOL or token is a number > 10,000. *)
	
	(* tpv := NIL only if I have to. *)
	INI1TKN(tpv,s);
  numstr := "";	
	LOOP
	  GETTKN(tpv,token,tknstate,transnum,retcod);
	  IF (retcod > 0) THEN
	  	EXIT; (* exit this loop that is looking for a number *)
    ELSIF (tknstate = DGT) AND (transnum > 10000) THEN
    	numstr := token;
    	RETURN transnum;
    END;
	END; (* loop to fetch tokens for only one line *)	
	numstr := "";
	RETURN 0;
END GetTransactionNumber;

(* ------------------------------------------------------------------------- GetQIFRec ------------------------- *)

PROCEDURE GetQIFRec(VAR OUT qif : QIFTYP);
VAR I,J : INTEGER;
   tpv : TKNPTRTYP;
   buf : BUFTYP;
   ch : CHAR;
   transnum,discardthis,c1,c2 : CARDINAL;
   str,s0,s1,s2 : STRTYP;

BEGIN
(* Must init qif fields
  QIFTYP = RECORD
    datestr, numstr, Pstr, Mstr, amtstr : STRTYP;
    m,d,y,num : CARDINAL;
    juldate : LONGINT;  end;
*)
  WITH qif DO
    datestr := '';
    numstr := '';
    Pstr := '';
    Mstr := '';
    amtstr := '';
    m := 0;
    d := 0;
    y := 0;
    num := 0;
    juldate := 0;
  END; (* with *)

  LOOP
    FRDTXLN(INUNT1,INBUF,250,EOFFLG);
    IF EOFFLG THEN EXIT END(*IF*);
    ch := INBUF.CHARS[1];
    RMVCHR(INBUF,1,1);
    WITH qif DO
      CASE ch OF
        'D' : datestr := INBUF.CHARS;
              ok := StrToCard(INBUF.CHARS[1..2],qif.m);
              ok := StrToCard(INBUF.CHARS[4..5],qif.d);
              ok := StrToCard(INBUF.CHARS[7..10],qif.y);
              juldate := JULIAN(m,d,y);

      | 'N' : numstr := INBUF.CHARS;
              ok := StrToCard(numstr,num);

      | 'P' : TRIM(INBUF);
              Pstr := INBUF.CHARS;

      | 'M' : TRIM(INBUF);
              Mstr := INBUF.CHARS;
              transnum := GetTransactionNumber(Mstr,qif.numstr);
              IF transnum > 10000 THEN
              	qif.num := transnum;
              END;


      | 'T' : amtstr := INBUF.CHARS;
      | '!','C',' ' :  (* ignore the line and do nothing  *)
      | '^' : EXIT  (* loop that reads and processes lines *)
      ELSE
        (* ignore the line and do nothing *)
      END (* CASE ch *)
    END (* WITH qif *)
  END (* LOOP *)
END GetQIFRec;

(* ------------------------------------------------------------------------- ProcessQIFFile ------------------------- *)

PROCEDURE ProcessQIFFile(tw : TextWindow);
(*  Sample QIF input
!Type:Bank
D09-17-2007
N06273
PTRANSFER FROM CHECKING
MReference # 006273
T8,833.97
C*
^
D09-14-2007
N
PINTEREST
M
T285.38
C*
^

Sample output line
"CLEARED","09-28-2007","11374","VERIZON WIRELESS (4) REFERENCE # 011374","-27.66","CHK.ASC","22326861","187"
*)


VAR I,J : INTEGER;
   tpv : TKNPTRTYP;
   buf : BUFTYP;
   transnum : CARDINAL;
   qif : QIFTYP;

BEGIN
  LOOP (* to read multiple records *)
    GetQIFRec(qif);
    IF EOFFLG THEN EXIT END(*IF*); (* the EOFFLG would be set by GetQIFRec *)

    FWRSTR(OUTUN1,'"CLEARED","');
    FWRSTR(OUTUN1,qif.datestr);
    FWRSTR(OUTUN1,'",');

    IF juldate1 = 0 THEN
      juldate1 := qif.juldate;
    ELSE
      juldate2 := qif.juldate;
    END(*IF*);
    IF  (juldate2 > 0) AND (juldate1 <> juldate2) THEN
      juldate1 := juldate2;
      juldate2 := 0;
      chknum := 0;
    END(*if*);

    IF qif.num = 0 THEN
        ok := CardToStr(chknum,qif.numstr);
        qif.num := chknum;
        INC(chknum);
    END;
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,qif.numstr);   (* output transnum *)
    FWRSTR(OUTUN1,'",');

(* Trans name description, part 1, now Pstr *)
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,qif.Pstr); (* output trans name part 1 only if not above phrase *)
    FWRSTR(OUTUN1," ");
(* Trans name description, part 2, now Mstr *)
    FWRSTR(OUTUN1,qif.Mstr); (* output trans name part 2 *)
    FWRSTR(OUTUN1,'",');

(* dollar amt *)
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,qif.amtstr);
    FWRSTR(OUTUN1,'",');

(* output acnt name *)
    FWRSTR(OUTUN1,'"');
    FWRSTR(OUTUN1,outfilelabel);
    FWRSTR(OUTUN1,'",');

(* now output 2 empty fields, for historical reasons I can't remember anymore *)
    FWRSTR(OUTUN1,'"",""');
    FWRLN(OUTUN1);

    WriteString(tw,qif.datestr,a);
    WriteString(tw,',',a);
    WriteString(tw,qif.numstr,a);
    WriteString(tw,',',a);
    WriteString(tw,qif.Pstr,a);
    WriteString(tw,qif.Mstr,a);
    WriteString(tw,qif.amtstr,a);
    WriteLn(tw);

  END(*LOOP*);
END ProcessQIFFile;

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
    retcod,c5,idx,ignore : CARDINAL;
    filter      : STRTYP;

BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
        FileFunc.CopyFile(OUTFNAM.CHARS,'citifile.out');
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        RETURN OkayToClose;
    | TWM_CREATE:

        xCaret := 0;
        yCaret := 0;
        inputline := '';
        juldate1 := 0;
        juldate2 := 0;
        juldate3 := 0;
        chknum := 0;

        INFNAM.CHARS := '';
        OUTFNAM.CHARS := '';

        filter := 'QIF Files';
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
        bool := BasicDialogs.PromptOpenFile(INFNAM.CHARS,filter,c5,'','',
                                                              'Open transaction text file',FALSE);
        IF NOT bool THEN
          WriteString(tw,'Could not open file.  Does it exist?',a);
          HALT;
        END;
        TRIM(INFNAM);

(*  All will use same outfile name so the input file will be determined by the dialog box *)
        OUTFNAM.CHARS := 'citifile.txt';
        TRIM(OUTFNAM);

        FUNC FileFunc.DeleteFile(OUTFNAM.CHARS);
        IF NOT FileFunc.FileExists(INFNAM.CHARS) THEN
          MiscM2.Error(' Could not find input file.  Does it exist?');
          HALT;
        END(*if*);
        FOPEN(INUNT1,INFNAM,RD);
        FOPEN(OUTUN1,OUTFNAM,WR);

        buf[0] := CAP(INFNAM.CHARS[INFNAM.COUNT-2]);
        buf[1] := CAP(INFNAM.CHARS[INFNAM.COUNT-1]);
        buf[2] := CAP(INFNAM.CHARS[INFNAM.COUNT]);
        buf[3] := 0C;

        IF STRCMPFNT(buf,'QIF') = 0 THEN
          csvqifState := qif;
        ELSE
          csvqifState := csv
        END;


        idx := 0;
        outfilelabel :=  'unknown acnt';
        FindNext('861',INFNAM.CHARS,idx,patfnd,ignore);
        IF patfnd THEN outfilelabel := 'Checking'; END;
        FindNext('457',INFNAM.CHARS,idx,patfnd,ignore);
        IF patfnd THEN outfilelabel := 'Gold Savings'; END;
        FindNext('466',INFNAM.CHARS,idx,patfnd,ignore);
        IF patfnd THEN outfilelabel := 'Esavings'; END;
        FindNext('483',INFNAM.CHARS,idx,patfnd,ignore);
        IF patfnd THEN outfilelabel := 'old PMMA'; END;
(*        
        buf[0] := CAP(INFNAM.CHARS[INFNAM.COUNT-10]);
        buf[1] := CAP(INFNAM.CHARS[INFNAM.COUNT-9]);
        buf[2] := CAP(INFNAM.CHARS[INFNAM.COUNT-8]);
        buf[3] := CAP(INFNAM.CHARS[INFNAM.COUNT-7]);
        buf[4] := CAP(INFNAM.CHARS[INFNAM.COUNT-6]);
        buf[5] := CAP(INFNAM.CHARS[INFNAM.COUNT-5]);
        buf[6] := CAP(INFNAM.CHARS[INFNAM.COUNT-4]);
        buf[7] := 0C;
        IF STRCMPFNT(buf,'CHK_861') = 0 THEN
          outfilelabel := 'CHECKING';
        ELSIF STRCMPFNT(buf,'SAV_466') = 0 THEN
          outfilelabel := 'ESAVINGS';
        ELSIF STRCMPFNT(buf,'SAV_483') = 0 THEN
          outfilelabel := 'old PMMA';
        ELSE
          outfilelabel := 'unknown acnt';
        END;
*)
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
    |
    TWM_PAINT:
      CASE csvqifState OF
        qif: ProcessQIFFile(tw);
      | csv: ProcessCSVFile(tw)
      END;
        FCLOSE(INUNT1);
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
                        "Citibank Format Converter", (* name : ARRAY OF CHAR *)
                        "#100",        (* menu : ARRAY OF CHAR *)
                        "CitiIcon",        (* icon : ARRAY OF CHAR *)
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
  FUNC WinShell.DispatchMessages(Start, NIL);

END CitiFilterQIF.
