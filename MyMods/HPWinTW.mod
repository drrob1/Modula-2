<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:HPIcon.RES*>
(*    <*/Resource:HPIcon32.RES*> *)
%ELSE
%END

(*--------------------------------------
REVISION HISTORY
================
16 Oct 03 -- First full Windows version finished.
18 Oct 03 -- Changed how help cmd handled.
21 Oct 03 -- Changed how output strings are converted.  And added popups.
31 Oct 03 -- Added Prime & HCF.
 1 Nov 03 -- Fixed bug in var swap for hcf cmd.
 4 Nov 03 -- Made Prime use a LongCard.
11 Nov 03 -- Added DOW to here so it can display a day name.  And changed HELP display.  And added Terminal Module stuff.
18 Nov 03 -- Liked Terminal window for help, so will not use TextOut.
23 Nov 03 -- Changed to using TextWindows routines.
28 Nov 03 -- Fixed bug in handling backspace & Del string display.
22 Dec 03 -- Added ability to use clipboard routines.
24 Aug 04 -- Added all the storage registers and file i/o of same.
20 Nov 04 -- Made change in key logic so that '=' becomes '+', not requiring a shift press.
27 Nov 04 -- Did same for '*' by allowing ';' instead.  And added PI as a cmd 2 HPCALC.
28 Nov 04 -- Allowed comma for minus and squote for plus.
 8 Jan 05 -- Playing w/ icons.
17 Jan 05 -- Now playing w/ text characterists and positioning.
 1 Feb 05 -- Playing w/ Window Titles.
--------------------------------------*)

MODULE HPWinTW;
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS;
IMPORT WINUSER, WIN32, WINGDI, WINX;
IMPORT Strings, ExStrings, MemUtils;
FROM Strings IMPORT
    Append, Equal, Delete, Concat, Capitalize;
FROM ExStrings IMPORT
    AppendChar, EqualI;
FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, FontInfo, WinAttr, ClipboardFormat,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, WindowTypes, SpecialKeys,
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
(*
FROM WinShell IMPORT
    Window, StartupDisplayMode, WinShellMsg, MessageRec, SetResourceFile,
    DispatchMessages, TerminateDispatchMessages,
    LoadString, CloseAllChildren, /*NormalWindow,*/ AddStatusLine,
    SetStatusFormat, WriteStatusField, /*CreateWindow, WindowTypes,*/ ClientTypes,
    ClientCreateData, TabPosition;
*)
IMPORT WinShell;
FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts (*drive path name extension*), FileTypes, DeviceTypes,
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
    FindInOSPathList, ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
FROM DlgShell IMPORT
    NotifyType, NotifyResult, CONTROL, ControlType, DialogPositions,
    ModelessDialog, GetDialogParent;
IMPORT Storage;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
(* IMPORT Lib; Part of TSLib *)
(* IMPORT MATHLIB; Part of TSlib that uses longreals. *)
IMPORT LongMath;
IMPORT ASCII;
FROM Environment IMPORT GetCommandLine;
FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
IMPORT RConversions, LongStr, LongConv,FormatString;
FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
(*
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    UNGETTKN,GETTKNREAL;
*)
FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
(****************************************************************************)
(*
  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
*)
FROM LongStr IMPORT StrToReal,RealToFloat,RealToEng,RealToFixed,RealToStr,ConvResults;
IMPORT IOChan, ChanConsts,LowLong;
FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;

FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SubStrCMPFNT,SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;
FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;

CONST
  szAppName = "HPWinTW";
  InputPrompt = 'Enter cmd or HELP : ';
  LastMod = '17 Jan 05';
  clipfmt = CLIPBOARD_ASCII;
  HalfCalcIcon = '#100';
  FullGreenIcon = '#200';
  HalfGreenIcon = '#300';

TYPE
  OutStateTyp = (fix,float,gen);
  STR20TYP    = ARRAY [0..20] OF CHAR;
  pstrtyp     = POINTER TO STRTYP;  (* remember this is a 1-origin array *)

VAR
  hwnd        :  WIN32.HWND;
  msg         :  WINUSER.MSG;
  wc          :  WINUSER.WNDCLASSEX;
  cxChar  : INTEGER;
  cyChar  : INTEGER;
  cxClient: INTEGER;
  cyClient: INTEGER;
  cxBuffer: INTEGER;
  cyBuffer: INTEGER;
  cxScreen,cyScreen,wxClient,wyClient : COORDINATE;
  xCaret  : INTEGER;
  yCaret  : INTEGER;
  ch1,ch2,ch3 :  CHAR;
  bool,inputprocessed,hpFileExists :  BOOLEAN;
  sigfig,c1,c2,c3    :  CARDINAL;
  inputline,HPFileName,Xstr,str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  longstr     : ARRAY [0..5120] OF CHAR;
  OutState : OutStateTyp = fix;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf    : BUFTYP;
  pinstr      : pstrtyp;
  convres     : ConvResults;
  r           : LONGREAL;
  L           : LONGINT;
  LC          : LONGCARD;
  DAYNAMES    : ARRAY [0..6] OF STR10TYP;
  a,aBold     : ScreenAttribute;
  Win         : TextWindow;
  Reg         : ARRAY [0..35] OF LONGREAL;
  RegFile     : File;
  biggerFont  : FontInfo;

(*
    FontWeights         = (FwLight, FwNormal, FwDemiBold, FwBold, FwHeavy);

    CharacterSets       = (
                           LATIN1_CHARSET,(*ansi, iso*)
                           LATIN2_CHARSET,(*latin + eastern europe*)
                           SYMBOL_CHARSET,
                           ASCII_CHARSET,
                           DEFAULT_CHARSET
                          );
    FontInfo            =
        RECORD
        height          : INTEGER;/* positive = tenths of a point.
                                                100 = 10.0 points
                                                105 = 10.5 points.
                                     negative = device pixels
                                  */
        italic          : BOOLEAN;
        fixedPitch      : BOOLEAN;/* if TRUE then only fixed pitch */
                                  /* if FALSE then any pitch, including fixed */
        weight          : FontWeights;
        charSet         : CharacterSets;
        name            : ARRAY [0..63] OF CHAR;/* font family name.
                                                   GTK
                                                     may also optionally include
                                                     the font foundary in the name.
                                                     the foundary and font family name
                                                     are separated by the '-' character.
                                                */
        END;
*)

(**********************************************************************)
PROCEDURE writestack(tw : TextWindow);
(* In:  sigfig                                                        *)
(* Out: Xstr                                                          *)
(**********************************************************************)
VAR strarray : ARRAY [1..8] OF STRTYP;
    stk      : ARRAY [1..8] OF LONGREAL;
    c,ignoreretcod,pos : CARDINAL;

BEGIN
  GETSTACK(stk,ignoreretcod);
  IF OutState = fix THEN
    FOR c := 1 TO STACKSIZE DO
      LongStr.RealToFixed(stk[c],sigfig,strarray[c]);
    END;
  ELSIF OutState = float THEN
    FOR c := 1 TO STACKSIZE DO
      LongStr.RealToEng(stk[c],sigfig,strarray[c]);
    END;
  ELSE (* OutState = gen *)
    FOR c := 1 TO STACKSIZE DO
      LongStr.RealToStr(stk[c],strarray[c]);
    END;
  END;
  PaintOff(tw);

  WriteStringAt(tw,0,1,'                X :',aBold);
  WriteString(tw,strarray[STACKSIZE-7],aBold);
  EraseToEOL(tw,a); WriteLn(tw);
  Xstr :=  strarray[1];

  WriteString(tw, 'T1:',a);
  WriteString(tw,strarray[STACKSIZE],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,'T2:',a);
  WriteString(tw,strarray[STACKSIZE-1],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,'T3:',a);
  WriteString(tw,strarray[STACKSIZE-2],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,'T4:',a);
  WriteString(tw,strarray[STACKSIZE-3],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,'T5:',a);
  WriteString(tw,strarray[STACKSIZE-4],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,'Z :',a);
  WriteString(tw,strarray[STACKSIZE-5],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,'Y :',a);
  WriteString(tw,strarray[STACKSIZE-6],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,'X :',aBold);
  WriteString(tw,strarray[STACKSIZE-7],aBold);
  EraseToEOL(tw,a); WriteLn(tw);

  PaintOn(tw);
END writestack;

(****************************************************************)
PROCEDURE writehelp(tw : TextWindow);
BEGIN
     longstr := ' This is an RPN style calculator.';
     WriteStringAt(tw,0,10,longstr,a); WriteLn(tw);
     longstr := ' fix, float, gen -- output string format options.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := ' SQRT,SQR -- X = sqrt(X) or sqr(X) register.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' STO,RCL  -- store/recall the X register to/from the memory register.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
'    Now allows single letter or number to designate the register 0..35';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
'    Must be first cmd on line for this, else reg from a different module is used.';
     WriteString(tw,longstr,a); WriteLn(tw);

     longstr :=
' SWAP,SWAPXY,<>,>< -- equivalent commands that swap the X and Y registers.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' LASTX -- put the value of the LASTX register back into the X register.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' ROLLDN -- roll the stack down one register.  X goes to T1.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' EXP,LN -- evaluate exp(X) or ln(X) and put result back into X.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' ^   -- evaluate ABS(Y) to the X power, put result in X and pop stack 1 reg.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' **  -- like "^" but rounds X before calling the PWRI function.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := ' INT, ROUND, FRAC -- guess.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' MOD -- evaluate Y MOD X, put result in X and pop stack 1 reg.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' SIN,COS,TAN,ARCTAN,ARCSIN,ARCCOS -- In radians.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' D2R, R2D -- perform degrees / radians conversion of the X register.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' JUL -- Julian date num Z month, Y day, X year.  Pop stack x2.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
" TODAY- Julian date num of today's date.  Pop stack x2.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' GREG-- Return Z month, Y day, X year of Julian date number in X.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=
' DOW -- Return day number 1..7 of julian date number in X register.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=' HEX -- Round X register to a longcard and output hex fmt.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=' PRIME -- Returns TRUE or FALSE of X reg.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=' HCF -- Pushs highest common factor of Y and X onto stack.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := ' TOCLIP, FROMCLIP -- Send to/from clipboard.';
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " Allows substitutions: = for +, ; for *, ' for + and ',' for - .";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " tab key -- changes font size 10, 12, 14, 16, 18, 20 then back down";
     WriteString(tw,longstr,a); WriteLn(tw);
END writehelp;

(*****************************************************************)
PROCEDURE IsPrime(lc : LONGCARD) : BOOLEAN;
(*****************************************************************)
VAR t,m : LONGCARD;

BEGIN
  IF (lc = 0) OR (lc = 1) THEN RETURN FALSE
  ELSIF (lc = 2) OR (lc = 3) THEN RETURN TRUE
  ELSIF NOT ODD(lc) THEN RETURN FALSE
  END;

  m := TRUNC(sqrt(LFLOAT(lc)));
  t := 3;
  REPEAT
    IF (lc MOD t) = 0 THEN RETURN FALSE END;
    INC(t,2);
  UNTIL t > m;
  RETURN TRUE
END IsPrime;

(*++++*****************************************************************)
PROCEDURE LongCard2HexStr(L : LONGCARD; VAR OutStr : ARRAY OF CHAR);
(**********************************************************************)
CONST  ASCZERO = ORD('0');
       ascA    = ORD('A');
VAR i,j,h  : CARDINAL;
    Str20  : STR20TYP;

BEGIN
  i := 0;
  REPEAT (* until L = 0 *)
    h := L MOD 16;
    IF (h <= 9) THEN Str20[i] := CHR(h + ASCZERO) ELSE Str20[i] := CHR(h -10 + ascA) END;
    INC(i);
    L := L DIV 16;
  UNTIL L = 0;
  j := 1;  (* first posn is a space to leave room for sign char *)
  OutStr[0] := ' ';
  REPEAT (* until i = 0 *)
    DEC(i);
    OutStr[j] := Str20[i];
    INC(j);
  UNTIL i = 0;
  OutStr[j] := 0C;
END LongCard2HexStr;

(*++++*****************************************************************)
PROCEDURE LongInt2HexStr(L : LONGINT; VAR OutStr : ARRAY OF CHAR);
(**********************************************************************)
VAR
   IsNeg : BOOLEAN;
   LC    : LONGCARD;

BEGIN
    IF L < 0 THEN
      IsNeg := TRUE;
      LC := -L;
    ELSE
      IsNeg := FALSE;
      LC := L;
    END;
    LongCard2HexStr(LC,OutStr);
    IF IsNeg THEN
      OutStr[0] := '-';
    END;
END LongInt2HexStr;

(********************************************************************)
PROCEDURE GetRegIdx(char : CHAR) : CARDINAL;
(* Return 0..35 w/ A = 10 and Z = 35
*********************************************************************)
VAR
        ch : CHAR;
        idx : CARDINAL;

BEGIN
        ch := CAP(char);
        idx := 0;
        IF (ch >= '0') AND (ch <= '9') THEN
          idx := ORD(ch) - ORD('0');
        ELSIF (ch >= 'A') AND (ch <= 'Z') THEN
          idx  := ORD(ch) - ORD('A') + 10;
        END (*if*);
        RETURN idx;
END GetRegIdx;

(*********************************************************************)
PROCEDURE CleanRealString(VAR INOUT str: ARRAY OF CHAR);
(*********************************************************************)
VAR s                       : ARRAY [0..255] OF CHAR;
    i                       : INTEGER;
    c,upper,inner,outer,len : CARDINAL;

BEGIN
  upper := HIGH(str);
  str[upper] := NULL;  (* Sentinal NULL for my algorithm *)
  len := LENGTH(str);
  outer := 0;
  inner := 0;
  WHILE (outer <= len) DO
    IF ((str[outer]='.') OR (CAP(str[outer])='E') OR ((str[outer]>='0') AND (str[outer]<='9')) ) THEN
    s[inner] := str[outer];
    INC(inner);
    END;
  INC(outer);
  END;
  s[inner] := NULL;
  FOR c := 0 TO inner DO
    str[c] := s[c];
  END;
(*  str[inner+1] := NULL; *)
END CleanRealString;



(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr   : Colors;
    x,y   : COORDINATE;
    i,int : INTEGER;
    idx   : CARDINAL;
    ans   : CHAR;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        ans := BasicDialogs.YesNo(' Should RegFile be written?','Y');
        IF ans = 'Y' THEN
          OpenCreateFile(RegFile,HPFileName,ReadWriteDenyWrite);
          WriteBlock(RegFile, ADR(Reg), SIZE(Reg) );
          CloseFile(RegFile);
        END;


        RETURN OkayToClose;
    | TWM_CREATE:
        (* FUNC SetWindowIcon(tw, '#300'); *)
        ch1 := 'a';
        ch2 := 0c;
        ch3 := 0c;
        str1 := '';
        str2 := '';
        longstr := '';
        str4 := '';
        xCaret := InputPromptLen;
        yCaret := 0;
        inputline := '';
        SetScrollDisableWhenNone(tw,TRUE,TRUE);
        HPFileName := 'HPFil.sav';
        hpFileExists := FileExists(HPFileName);
        IF hpFileExists THEN
          OpenFile(RegFile,HPFileName,ReadOnlyDenyWrite);
          IF RegFile.status <> 0 THEN
            Terminal.WriteString(' Error in opening file ');
            Terminal.WriteString(HPFileName);
            Terminal.WriteString('--');
            CASE TranslateFileError(RegFile) OF
              FileErrFileNotFound : Terminal.WriteString('File not found.');
            | FileErrDiskFull : Terminal.WriteString('Disk Full');
            ELSE
              Terminal.WriteString('Nonspecific error occured.');
            END(*CASE*);
            Terminal.WriteLn;
            Terminal.WriteString(' Reg file read operation not done.');
            Terminal.WriteLn;
          END(*IF RegFile.status*);
          ReadBlock(RegFile, ADR(Reg), SIZE(Reg) );
          CloseFile(RegFile);
        END (* if hpfileexists *);


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
        WriteStringAt(tw,0,0,InputPrompt,a);
        WriteString(tw,inputline,a);
        EraseToEOL(tw,a);
(*        WriteStringAt(tw,cxClient-INT(LastModLen),10,LastMod,a); *)
        WriteStringAt(tw,cxScreen-INT(LastModLen),10,LastMod,a);
        WriteLn(tw);
        writestack(tw);
        SetWindowTitle(tw,Xstr);
        MoveCaretTo(tw,InputPromptLen,0);

    | TWM_NOTIFYSCROLL:
(*
        writehelp(tw);
        RepaintScreen(tw);
*)
    |
    TWM_KEY:
     IF inputprocessed THEN
      inputline := '';
      MoveCaretTo(tw,0,0);
      inputprocessed := FALSE;
      CaretOn(tw);
      RepaintScreen(tw);
     END;
     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN
        CASE msg.k_ch OF
          CHR(8) :                                     (* backspace      *)
            Strings.Delete(inputline,LENGTH(inputline)-1,1);
            x := Xpos(tw);
            WriteStringAt(tw,x-1,0,' ',a);
            MoveCaretTo(tw,x-1,0);
(*            RepaintScreen(tw); *)
        | CHR(9) :                    (* tab                 *)
            GetClientSize(tw,wxClient,wyClient);
            IF biggerFont.height <= 200 THEN
              INC(biggerFont.height, 20);
              SetClientSize(tw,wxClient+7,wyClient+2);
            ELSE
              biggerFont.height := 100;
              SetClientSize(tw,50,20);
            END;

            FUNC SetWindowFont(tw, biggerFont);
            GetClientSize(tw,cxScreen,cyScreen);
            CONTINUE
        | CHR(10):                    (* line feed       *)

        | CHR(13):                    (* carriage RETURN *)
            inputprocessed := TRUE;
            CaretOff(tw);
            ASSIGN2BUF(inputline,inputbuf);
            IF inputbuf.COUNT = 0 THEN CloseWindow(tw,CM_REQUEST) END;
            IF STRCMPFNT(inputline,'FIX') = 0 THEN
              OutState := fix;
              BasicDialogs.PromptCard('SigFig:',0,20,TRUE,sigfig);
            ELSIF STRCMPFNT(inputline,'FLOAT') = 0 THEN
              OutState := float;
              BasicDialogs.PromptCard('SigFig:',0,20,TRUE,sigfig);
            ELSIF STRCMPFNT(inputline,'GEN') = 0 THEN
              OutState := gen;
(*              BasicDialogs.PromptCard('SigFig:',0,20,TRUE,sigfig); *)
            ELSIF STRCMPFNT(inputline,'EXIT') = 0 THEN
              HALT;
            ELSIF SubStrCMPFNT(inputline,'STO') THEN
              idx := GetRegIdx(inputline[4]);
              Reg[idx] := READX();
            ELSIF SubStrCMPFNT(inputline,'RCL') THEN
              idx := GetRegIdx(inputline[4]);
              PUSHX(Reg[idx]);

(*
            ELSIF STRCMPFNT(inputline,'PI') = 0 THEN
              PUSHX(LongMath.pi);
*)
            ELSIF STRCMPFNT(inputline,'HEX') = 0 THEN
              r := READX();
              IF (r > 0.) AND (r <= 2.0E9) THEN
                c2 := TRUNC(r);
                LongCard2HexStr(c2,str8);
              ELSE
                L := VAL(LONGINT,r);
                LC := SYSTEM.CAST(LONGCARD,L);
                LongCard2HexStr(LC,str8);
              END;
              Strings.Concat('X = ',str8,str7);
              Strings.Append(' hex',str7);
              BasicDialogs.MessageBox(str7,MsgInfo);
            ELSIF STRCMPFNT(inputline,'PRIME') = 0 THEN

              r := READX();
              LC := VAL(LONGCARD,(ABS(r)));
              bool := IsPrime(LC);
              IF bool THEN
                FormatString.FormatString(' %l is prime.',str5,LC);
              ELSE
                FormatString.FormatString(' %l is not prime.',str5,LC);
              END;
              BasicDialogs.MessageBox(str5,MsgInfo);

            ELSIF STRCMPFNT(inputline,'DOW') = 0 THEN
              int := round(READX());
              str3 := DAYNAMES[int MOD 7];
              BasicDialogs.MessageBox(str3,MsgInfo);
            ELSIF STRCMPFNT(inputline,'TOCLIP') = 0 THEN
              r := READX();
              IF OutState = fix THEN
                LongStr.RealToFixed(r,sigfig,str9);
                    ELSIF OutState = float THEN
                LongStr.RealToEng(r,sigfig,str9);
                    ELSE (* OutState = gen *)
                LongStr.RealToStr(r,str9);
              END;
              IF NOT (OpenClipboard(tw) AND EmptyClipboard(tw)) THEN
                WriteString(tw,' Open and Empty Clipboard failed.',a);
                WriteLn(tw);
                CloseClipboard(tw);
                RETURN DEFAULT_HANDLE;
              END;
              pinstr := AllocClipboardMemory(BUFSIZ);
              IF pinstr = NIL THEN
                WriteString(tw,' Alloc clipboard memory failed.',a);
                WriteLn(tw);
                UnlockClipboardMemory;
                CloseClipboard(tw);
                RETURN DEFAULT_HANDLE;
              END;
              pinstr^ := str9;
              IF  NOT SetClipboard(clipfmt) THEN
                WriteString(tw,' Setclipboard failed.',a);
                WriteLn(tw);
              END;
              CloseClipboard(tw);
            ELSIF STRCMPFNT(inputline,'FROMCLIP') = 0 THEN
              IF NOT ClipboardFormatAvailable(clipfmt) THEN
                WriteStringAt(tw,0,0,'text format not available in clipboard.',a);
                RETURN DEFAULT_HANDLE;
              ELSIF NOT OpenClipboard(tw) THEN
                WriteString(tw,' OpenClipboard failed.',a);
                WriteLn(tw);
                RETURN DEFAULT_HANDLE;
              END;
              pinstr := GetClipboard(clipfmt);
              IF pinstr = NIL THEN
                WriteStringAt(tw,0,1,'unable to get clipboard.',a);
                RETURN DEFAULT_HANDLE;
              END;
              str8 := pinstr^;
              UnlockClipboardMemory;
              CloseClipboard(tw);
              CleanRealString(str8);
              LongStr.StrToReal(str8,r,convres);
              IF convres # strAllRight THEN
                WriteStringAt(tw,0,2,' string conversion to real failed.',a);
                WriteLn(tw);
                WriteString(tw,str8,a);
                EraseToEOL(tw,a);
                RETURN DEFAULT_HANDLE;
              END;
              PUSHX(r);

            ELSIF STRCMPFNT(inputline,'HELP') = 0 THEN
              writehelp(tw)


            ELSE
(* Now must process the cmd, which is the whole reason for this pgm existing! *)
              GETRESULT(inputbuf);
            END;
            RepaintScreen(tw);
        | CHR(27):             (* escape *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);


        ELSE (* CASE ch *)
          IF msg.k_ch = '=' THEN
            ch3 := '+';
          ELSIF msg.k_ch = ';' THEN
            ch3 := '*';
          ELSIF msg.k_ch = ',' THEN
            ch3 := '-';
          ELSIF msg.k_ch = "'" THEN
            ch3 := "+";
          ELSE
                ch3 := CAP(msg.k_ch);
          END (*if*);
          Strings.Append(ch3,inputline);
          WriteString(tw,msg.k_ch,a);
(*          RepaintScreen(tw);    works but lose cursor when typing.  I'll keep it out. *)
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
        Strings.Delete(inputline,LENGTH(inputline)-1,1);
        x := Xpos(tw);
        WriteStringAt(tw,x-1,0,' ',a);
        MoveCaretTo(tw,x-1,0);
(*        RepaintScreen(tw); *)
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
(*
  PROCEDURE CreateWindow(parent : WinShell.Window;
                       name : ARRAY OF CHAR;
                       menu : ARRAY OF CHAR;
                       icon : ARRAY OF CHAR;
                       x, y : COORDINATE;
                       xSize, ySize : COORDINATE;
                       xBuffer, yBuffer : COORDINATE;
                       gutter : BOOLEAN;
                       font : FontInfo;
                       background : ScreenAttribute;
                       windowType : WindowTypes;
                       wndProc : TextWindowProcedure;
                       attribs : WinAttrSet;
                       createParam : ADDRESS) : TextWindow;
(* create a new window *)
(* parent = as WinShell  *)
(* name = as WinShell  *)
(* menu = the menu for the window. Can be "". *)
(* icon =  as WinShell *)
(* attribs = as WinShell *)
(* wndProc = the window procedure *)
(* createParam = an arbitrary value you can use to pass information
                 to the window procedure of the window. this value is
                 passed in the WSM_CREATE message. *)
(* font = the font to use for this window *)
(* background = the background color for this window *)
(* gutter = TRUE then the text window will always have a blank "gutter"
            on the left edge of the text window.
            FALSE the text will start at the left edge of the client area.
            *)
(* x, y = the initial screen coordinates for the window to be displayed
          if a parameter is -1 then the operating system will choose
          a default location for that coordinate.
          these positions are in pixels and are relative to the
          parent window client area origin for child windows
          or relative to the screen origin for all other windows. *)
(* xSize, ySize = the initial width and height in character cells
                  if -1 then a system default size will be used *)
(* xBuffer, yBuffer = the size of the screen buffer. the window can never
                      be larger than the screen buffer. if either xBuffer
                      or yBuffer is -1 the screen buffer is a variable size
                      and is sized to the number of cells the window client
                      area currently is capable displaying. *)
(* returns the window handle if successfull, otherwise NIL *)
*)
    Win := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "HPWin for TextWindows Module", (* name : ARRAY OF CHAR *)
                        "",        (* menu : ARRAY OF CHAR *)
                        HalfCalcIcon, (* "#100", *)       (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        50,20, (* xSize, ySize : COORDINATE *)
                        500,200, (* xBuffer, yBuffer : COORDINATE *)
                        FALSE,  (* gutter : BOOLEAN *)
                        biggerFont, (* DefaultFontInfo, /* font : FontInfo */ *)
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


(*++++*****************************************************************)
BEGIN
  InputPromptLen := LENGTH(InputPrompt);
  LastModLen := LENGTH(LastMod);
  sigfig := 2;
  DAYNAMES[0] := 'Sunday';
  DAYNAMES[1] := 'Monday';
  DAYNAMES[2] := 'Tuesday';
  DAYNAMES[3] := 'Wednesday';
  DAYNAMES[4] := 'Thursday';
  DAYNAMES[5] := 'Friday';
  DAYNAMES[6] := 'Saturday';
  a := ComposeAttribute(Black, White, NormalFont);
  aBold := ComposeAttribute(Black, White, BoldFont);
  biggerFont := DefaultFontInfo;
  biggerFont.height := 140;

  inputprocessed := FALSE;

  FUNC WinShell.DispatchMessages(Start, NIL);
END HPWinTW.
