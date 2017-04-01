<*/NOWARN:F*>
<*/Resource:HPIcon.RES*>

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
 5 Feb 06 -- Playing w/ Menus and added RegAltered flag.
11 Mar 06 -- Saving and restoring stack
 7 Mar 07 -- Discovered an occasional bug in arith in which it seems operators act more than once.
              Changed how characters are written to display.  Hope this helps track down pblm.
              If not, I'll consider repainting screen to see if this helps track pblm down.
15 Apr 07 -- Still haven't a handle on this occasional arith bug.  But added arrow key and other
              stack functions.
25 Apr 07 -- Added terminal window output of typed lines, to be a record of what is typed at cmd line.
26 Apr 07 -- Playing with another window that I want to be a child window.
29 Apr 07 -- Now that I got this extra window thing down, I'm going to try it for the help window.
26 Jun 07 -- Never did help window.  Decided to try calling ProcessInput after a space.
 4 Jul 07 -- Made it not exit on space.
 9 Jul 07 -- Fixed bug in that processinput not called when hit up arrow.  And call processinput after
              unambiguous operators, which does not include '*' or '-'.
 4 Aug 07 -- Fixed minor bug when some terminating operators were not being written to childscreen.
29 Sep 07 -- Will now processinput on '=', and "'" for subtraction.
17 Sep 08 -- Found problem w/ processing cmd like RCL/, etc.  Fixed it.
30 Oct 08 -- Found that ' ' is appended to inputstring, so 'FIX' becomes 'FIX ' and is not recognized.
 1 Feb 09 -- Fixed bug in that menu help different than command help.
 8 Mar 09 -- Added code to immediately process subt operator, making squote redundant.  And I added a
             RETURN(DEFAULT_HANDLE) in that area to return control to Windows and what seems to be exiting
             the program.  I remember reading that the purpose of the windows procedure is so Windows
             has a procedure to call when it gets messages.  It seems to work.  Perhaps I should use this
             trick more widely.  Will it speed up the code?  Who knows if this even matters any more.
 9 Mar 09 -- Made it RETURN(USER_HANDLE);
 7 Jun 11 -- GetCommandLine and push whatever is there onto the stack upon starting.
             rpncalc in linux works this way and that pgm will exit after doing whatever the
             command line says.  If started w/o params, it does not exit imm'ly.
20 Oct 11 -- Noticed that the change that allowed '-' to operate imm'ly prevented negative exponents from
             being entered.  I'll use an underscore to represent the unary '-' negative sign.
             A side benefit to the GetCommandLine change is that neg exponents can be entered this way from
             the commandline.
11 Nov 14 -- Added a window to display the nonzero register contents.  And added a command to zero the registers.
26 Dec 14 -- Added HOL command, and fiddled w/ colors.
 6 Jan 15 -- Made help window the same size btwn menu and help command.
22 Jun 15 -- Made a lone space not exit (again).
28 Aug 15 -- Made filenames CONST, and changed the size of the main window.
29 Aug 15 -- Filenames will be in %userprofile, but this was harder than I first thought.  And will move file ops to the init section.
16 Apr 16 -- undo, redo added to HPCALC
 9 May 16 -- Fixed the output help text to include undo and redo.  That never was done before.  And had to incr helpwin number of rows to accomodate the extra line.
19 May 16 -- Fixed output help text for the % command, originally coded in 2006 but, oddly enough, the help was never updated.
20 May 16 -- Added F2 to F1 for help.  To match the cpp version.
 7 Jul 16 -- Added UP command, and added PI to help.
29 Mar 17 -- Backported HPCALC rtns to match Go.  Pass a linked list of strings, and operators to match Go.
               Removed code that was commented out long ago.
--------------------------------------*)

MODULE HPWinMenu;
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
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure, NormalFont, BoldFont, ItalicFont, FontInfo, WinAttr,
    ClipboardFormat, DisplayModes, ScreenAttribute, CaretTypes, TWMessageRec, ResponseType, CloseModes,
    CloseWindow, NormalWindow, NormalChildWindow, FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo,
    WindowTypes,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, SpecialKeys, GetClientSize, SetClientSize, SnapWindowToFont,
    SetScrollRangeAllowed, MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, WriteString,
    WriteStringAt, WriteCellsAt, WriteCells, WriteLn, EraseToEOL, ChangeAttr,
    ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn, SetAutoScroll, WinShellToTextWindowMessage,
    MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect, GetBufferRect, EraseScreen, EraseRect,
    GetWinShellHandle, FindTextWindow, SetDisplayMode,GetDisplayMode,SetWindowEnable, (*  SetWindowTitle, *)
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage,
    IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars, SetScrollBarPos, SetWindowData,
    SetWindowDataNum, GetWindowData, GetWindowDataNum, GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos,
    CascadeWindow, SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo, SetScrollDisableWhenNone,
    SetActiveTabChild, SetTabChildPosition, GetForegroundWindow, SetForegroundWindow, SetWindowFont,
    SetTimer, KillTimer, DisplayHelp, SetWindowIcon, OpenClipboard, CloseClipboard, EmptyClipboard,
    ClipboardFormatAvailable, AllocClipboardMemory, UnlockClipboardMemory, SetClipboard, GetClipboard,
    Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;
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
    NotifyType, NotifyResult, CONTROL, ControlType, DialogPositions, ModelessDialog, GetDialogParent;
IMPORT Storage;
IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT LongMath;
IMPORT ASCII;
FROM Environment IMPORT GetCommandLine,GetSymbol;
FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI;
IMPORT RConversions, LongStr, LongConv,FormatString;
FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
    StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
FROM TIMLIBrevised IMPORT JULIAN,GREGORIAN,TIME2MDY;
FROM LongStr IMPORT StrToReal,RealToFloat,RealToEng,RealToFixed,RealToStr,ConvResults;
IMPORT IOChan, ChanConsts,LowLong;
FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
(****************************************************************************)

FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,STR20TYP,BUFTYP,MAXCARDFNT,BLANK,NULL,COPYLEFT,COPYRIGHT,
  FILLCHAR,SCANFWD,SCANBACK, STRLENFNT,STRCMPFNT,LCHINBUFFNT,stricmpfnt,SubStrCMPFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
  APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF, StringItemPointerType,StringDoubleLinkedListPointerType,
  InitStringListPointerType,AppendStringToList,NextStringFromList,PrevStringFromList,CurrentPointerBeginning,
  CurrentPointerEnding,GetNextStringFromList,GetPrevStringFromList;
IMPORT UTILLIB;
FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT,RealStack,Holidays,PushStacks,
  RollDownStacks,RollUpStacks;
FROM HolidayCalc IMPORT HolType, GetHolidays;


CONST
  szAppName = "HPWinMenu";
  InputPrompt = "Enter cmd or HELP : ";
  LastMod = "29 Mar 17";
  clipfmt = CLIPBOARD_ASCII;
  HalfCalcIcon = '#100';
  FullGreenIcon = '#200';
  HalfGreenIcon = '#300';
  RegFileName = "HPReg.sav";
  StackFileName = "HPStack.sav";
  MainWindowHeight = 20;

TYPE
  OutStateTyp = (fix,float,gen);
(*                                                                           STR20TYP    = ARRAY [0..20] OF CHAR; *)
  pstrtyp     = POINTER TO STRTYP;  (* remember this is a 1-origin array *)
  charsetyp   = SET OF CHAR;

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
  bool,inputprocessed,hpFileExists,RegAltered,StackFileExists,UserprofileExists : BOOLEAN;
  sigfig,c,c1,c2,c3    :  CARDINAL;
  inputline,HPFileName,Xstr,str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  Path : FileSpecString;
  longstr     : ARRAY [0..5120] OF CHAR;
  OutState : OutStateTyp = fix;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf          : BUFTYP;
  pinstr            : pstrtyp;
  convres           : ConvResults;
  r                 : LONGREAL;
  L                 : LONGINT;
  LC                : LONGCARD;
  DAYNAMES          : ARRAY [0..6] OF STR10TYP;
  a,aBold,aInvertedRed,aInvertedLightRed,aInvertedCyan,aInvertedLightBlue,aInvertedLightCyan,aInverted,aInvertedYellow : ScreenAttribute;
  Win,WinChild,HelpWin,RegWin : TextWindow;
  winshellwindow    : WinShell.Window;
  Reg               : ARRAY [0..35] OF LONGREAL;
  RegFile,StackFile : File;
  biggerFont        : FontInfo;
  stk               : ARRAY [1..STACKSIZE] OF LONGREAL;
  opset             : charsetyp;
  slen              : CARDINAL;


(**********************************************************************)
PROCEDURE writestack(tw : TextWindow);
         (* gbl In:  sigfig, stk                                                   *)
         (* gbl Out: Xstr                                                          *)

VAR strarray : ARRAY [1..8] OF STRTYP;
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

  WriteStringAt(tw,0,1,"                X :",aBold);
  WriteString(tw,strarray[STACKSIZE-7],aInvertedYellow);
  EraseToEOL(tw,a); WriteLn(tw);
  Xstr :=  strarray[1];

  WriteString(tw,"T1:",a);
  WriteString(tw,strarray[STACKSIZE],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,"T2:",a);
  WriteString(tw,strarray[STACKSIZE-1],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,"T3:",a);
  WriteString(tw,strarray[STACKSIZE-2],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,"T4:",a);
  WriteString(tw,strarray[STACKSIZE-3],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,"T5:",a);
  WriteString(tw,strarray[STACKSIZE-4],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,"Z :",a);
  WriteString(tw,strarray[STACKSIZE-5],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,"Y :",a);
  WriteString(tw,strarray[STACKSIZE-6],a);
  EraseToEOL(tw,a); WriteLn(tw);

  WriteString(tw,"X :",aBold);
  WriteString(tw,strarray[STACKSIZE-7],aInverted);
  EraseToEOL(tw,a); WriteLn(tw);

  PaintOn(tw);
END writestack;

(****************************************************************)
PROCEDURE writehelp(tw : TextWindow);
BEGIN
     longstr := " This is an RPN style calculator written in Modula-2.";
     WriteStringAt(tw,0,0,longstr,a);
     longstr := " fix, float, gen -- output string format options.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " SQRT,SQR -- X = sqrt(X) or sqr(X) register.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " STO,RCL  -- store/recall the X register to/from the memory register.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := "    Now allows single letter or number to designate the register 0..35";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := "    Must be first cmd on line for this, else reg from a different module is used.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " ZEROREG -- Zeros all of the registers, but not the stack.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " SHOREG -- Shows the non-zero registers in its own window.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " SWAP,SWAPXY,<>,><,~,`, side arrow keys -- equivalent commands that swap the X and Y registers.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " LASTX,@ -- put the value of the LASTX register back into the X register.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " |, DN, ROLLDN -- roll the stack down one register.  X goes to T1.";
     WriteString(tw,longstr,a); WriteLn(tw);
     WriteString(tw," , or UP -- stack up operation.",a); WriteLn(tw);
     longstr := " Up or Down arrow, PageUp, Home, etc -- Stack movement commands done immediately.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " EXP,LN -- evaluate exp(X) or ln(X) and put result back into X.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " ^   -- Y to the X power using PWRI, put result in X and pop stack 1 reg.  Rounds X";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " **  -- ABS(Y) to the X power, put result in X and pop stack 1 reg, using exp and ln()";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " INT, ROUND, FRAC, PI -- all on X reg.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " MOD -- evaluate Y MOD X, put result in X and pop stack 1 reg.";
     WriteString(tw,longstr,a); WriteLn(tw);
     WriteString(tw," %   -- does XY/100, places result in X.  Leaves Y alone.",a); WriteLn(tw);
     longstr := " SIN,COS,TAN,ARCTAN,ARCSIN,ARCCOS -- Now in degrees.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " D2R, R2D -- perform degrees / radians conversion of the X register.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " JUL -- Julian date num Z month, Y day, X year.  Pop stack x2.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " TODAY- Julian date num of today's date.  Pop stack x2.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " GREG-- Return Z month, Y day, X year of Julian date number in X.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " DOW -- Return day number 1..7 of julian date number in X register.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=" HEX -- Round X register to a longcard and output hex fmt.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=" PRIME -- Returns TRUE or FALSE of X reg.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr :=" HCF -- Pushs highest common factor of Y and X onto stack.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " TOCLIP, FROMCLIP -- Send to/from clipboard.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " Allows substitutions: = for +, ; for *, ' for + and ',' for -, and _ is unary minus .";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " tab key -- changes font size 10, 12, 14, 16, 18, 20 then back down";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " HOL -- holiday days and dates for year in X ";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " CURT -- Cube Root number in X ";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " RECIP -- reciprocal of X, ie, 1/X.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " VOL -- Given a volume in X, estimate size of cavity assuming a sphere.";
     WriteString(tw,longstr,a); WriteLn(tw);
     longstr := " undo,redo -- works on entire stack as a whole.  More comprehensive than lastx";
     WriteString(tw,longstr,a); WriteLn(tw);
END writehelp;
(*****************************************************************)
PROCEDURE WriteReg(tw : TextWindow);
VAR
  i : CARDINAL;
  FirstNonZeroFlag,ok : BOOLEAN;
  str : STRTYP;
  ch : CHAR;

BEGIN
  EraseScreen(tw,a);
  FirstNonZeroFlag  := FALSE;
  str := " The following registers are not zero.";
  FOR i := 0 TO 35 DO
    IF Reg[i] = 0.0 THEN
      (* do nothing *)
    ELSE
      IF NOT FirstNonZeroFlag THEN
        WriteStringAt(tw,0,0,str,a);
        WriteLn(tw);
        WriteLn(tw);
        FirstNonZeroFlag := TRUE;
      END; (* if FirstNonZeroFlag *)
      WriteString(tw," Reg [",a);
                                                          (*      ok := CardToStr(i,str); *)
      ch := GetRegChar(i);
      str[1] := ch;
      str[2] := NULL;
      WriteString(tw,str,a);
      WriteString(tw,"] = ",a);
      CASE OutState OF
        fix: LongStr.RealToFixed(Reg[i],sigfig,str);
      | float: LongStr.RealToEng(Reg[i],sigfig,str);
      ELSE  (* outstate = gen *)
               LongStr.RealToStr(Reg[i],str);
      END; (* case *)
      WriteString(tw,str,a);
      WriteLn(tw);
    END; (* if reg = 0 *)
  END; (* for *)
END WriteReg;


(*****************************************************************)
PROCEDURE IsPrime(lc : LONGCARD) : BOOLEAN;

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
                                            (* Return 0..35 w/ A = 10 and Z = 35 *)
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
(********************************************************************)
PROCEDURE GetRegChar(idx : CARDINAL) : CHAR;
                                            (* Return '0'..'Z' with A = 10 and Z = 35 *)

VAR
  ch : CHAR;
  i : CARDINAL;

BEGIN
  IF (idx >= 0) AND (idx <= 9) THEN
    ch := CHR(idx + ORD('0'));
  ELSIF (idx >= 10) AND (idx <= 35) THEN
    ch  := CHR(idx - 10 + ORD('A'));
  ELSE
    ch  := NULL;
  END (*if*);
  RETURN ch;
END GetRegChar;

(*********************************************************************)
PROCEDURE CleanRealString(VAR INOUT str: ARRAY OF CHAR);

VAR s : ARRAY [0..255] OF CHAR;
    i : INTEGER;
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
PROCEDURE ProcessInput(tw : TextWindow);

                            (* Former code from <enter> is now here.  Needs no params as all var's are global to module. *)
VAR
  idx,c,C : CARDINAL;
  int     : INTEGER;
  ch1,ch2 : CHAR;
  ok      : BOOLEAN;
  str     : STRTYP;
  StringListP : StringDoubleLinkedListPointerType;
  StringP : StringItemPointerType;
  R : LONGREAL;

BEGIN
  LOOP (* dummy loop to allow EXIT on errors, as RETURN is only allowed in functions, not procedures. *)
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
   ELSIF STRCMPFNT(inputline,'EXIT') = 0 THEN
     HALT;
   ELSIF SubStrCMPFNT(inputline,'STO') THEN
     idx := GetRegIdx(inputline[4]);
     Reg[idx] := READX();
     RegAltered := TRUE;
(*                                                                                EraseScreen(RegWin,a); *)
     WriteReg(RegWin);
     IF inputbuf.COUNT >= 4 THEN  (* may have another command following this *)
         ch1 := inputline[4];
         ch2 := inputline[5];
       RMVCHR(inputbuf,1,3);
         IF ch1 IN opset THEN
         GETRESULT(inputbuf,R);
       ELSIF ch2 IN opset THEN
         RMVCHR(inputbuf,1,1);  (* remove ch1 *)
         GETRESULT(inputbuf,R);
       END;
     END;
   ELSIF SubStrCMPFNT(inputline,'RCL') THEN
     idx := GetRegIdx(inputline[4]);
     PUSHX(Reg[idx]);
     IF inputbuf.COUNT >= 4 THEN  (* may have another command following this *)
         ch1 := inputline[4];
         ch2 := inputline[5];
       RMVCHR(inputbuf,1,3);
         IF ch1 IN opset THEN
         GETRESULT(inputbuf,R);
       ELSIF ch2 IN opset THEN
         RMVCHR(inputbuf,1,1);
         GETRESULT(inputbuf,R);
       END;
     END;
   ELSIF STRCMPFNT(inputline,"ZEROREG") = 0 THEN
     FOR c := 0 TO 35 DO
       Reg[c] := 0.0;
     END; (* for c *)
   ELSIF STRCMPFNT(inputline,"SHOREG") = 0 THEN
     WriteReg(RegWin);
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
       EXIT;
     END;
     pinstr := AllocClipboardMemory(BUFSIZ);
     IF pinstr = NIL THEN
       WriteString(tw,' Alloc clipboard memory failed.',a);
       WriteLn(tw);
       UnlockClipboardMemory;
       CloseClipboard(tw);
       EXIT;
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
       EXIT;
     ELSIF NOT OpenClipboard(tw) THEN
       WriteString(tw,' OpenClipboard failed.',a);
       WriteLn(tw);
       EXIT;
     END;
     pinstr := GetClipboard(clipfmt);
     IF pinstr = NIL THEN
       WriteStringAt(tw,0,1,'unable to get clipboard.',a);
       EXIT;
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
       EXIT;
     END;
     PUSHX(r);
   ELSIF STRCMPFNT(inputline,"HOL") = 0 THEN
     GETRESULT(inputbuf,R);
     MoveCaretTo(tw,0,12);
     IF Holidays.valid THEN
       WriteString(tw," For year ",a);
       C := ROUND(READX());
       IF C < 40 THEN
         INC(C,2000);
       ELSIF C < 100 THEN
         INC(C,1900);
       END (* if *);
       ok := CardToStr(C,str);
       WriteString(tw,str,a);
       EraseToEOL(tw,a);
       WriteLn(tw);
       WriteString(tw,"New Years Day is a ",a);
       c := (JULIAN(1,1,C) MOD 7);
       WriteString(tw,DAYNAMES[c],a);
       WriteString(tw,", MLK Day is Jan ",a);
       ok := CardToStr(Holidays.MLK.d,str);
       WriteString(tw,str,a);
       WriteString(tw,", Pres Day is Feb ",a);
       ok := CardToStr(Holidays.Pres.d,str);
       WriteString(tw,str,a);
       WriteString(tw,", Easter is ",a);
       ok := CardToStr(Holidays.Easter.m,str);
       WriteString(tw,str,a);
       WriteString(tw,"/",a);
       ok := CardToStr(Holidays.Easter.d,str);
       WriteString(tw,str,a);
       EraseToEOL(tw,a);
       WriteLn(tw);
       WriteString(tw,"Mother's Day is May ",a);
       ok := CardToStr(Holidays.Mother.d,str);
       WriteString(tw,str,a);
       WriteString(tw,", Memorial Day is May ",a);
       ok := CardToStr(Holidays.Memorial.d,str);
       WriteString(tw,str,a);
       WriteString(tw,", Father's Day is June ",a);
       ok := CardToStr(Holidays.Father.d,str);
       WriteString(tw,str,a);
       EraseToEOL(tw,a);
       WriteLn(tw);
       WriteString(tw,"July 4 is a ",a);
       c := (JULIAN(7,4,C) MOD 7);
       WriteString(tw,DAYNAMES[c],a);
       WriteString(tw,", Labor Day is Sep ",a);
       ok := CardToStr(Holidays.Labor.d,str);
       WriteString(tw,str,a);
       WriteString(tw,", Columbus Day is Oct ",a);
        ok := CardToStr(Holidays.Columbus.d,str);
       WriteString(tw,str,a);
       WriteString(tw,", Election Day is Nov ",a);
       ok := CardToStr(Holidays.Election.d,str);
       WriteString(tw,str,a);
       EraseToEOL(tw,a);
       WriteLn(tw);
       WriteString(tw,"Veteran's Day is a ",a);
       c := (JULIAN(11,11,C) MOD 7);
       WriteString(tw,DAYNAMES[c],a);
       WriteString(tw,", Thanksgiving is Nov ",a);
       ok := CardToStr(Holidays.Thanksgiving.d,str);
       WriteString(tw,str,a);
       WriteString(tw,", Christmas Day is a ",a);
       c := (JULIAN(12,25,C) MOD 7);
       WriteString(tw,DAYNAMES[c],a);
       EraseToEOL(tw,a);
       WriteLn(tw);
       EraseToEOL(tw,a);
       WriteLn(tw);
       WriteLn(tw);
       WriteLn(tw);
       Holidays.valid := FALSE;
     END (* if Holidays.valid *);
   ELSIF STRCMPFNT(inputline,'HELP') = 0 THEN
         HelpWin := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "Help Window", (* name : ARRAY OF CHAR *)
                        "",                   (* menu : ARRAY OF CHAR *)
                        FullGreenIcon,            (* icon : ARRAY OF CHAR *)
                         1, 1,
                        (* -1,-1,  x,y= the initial screen coordinates for the window to be displayed *)
                        99,35, (* xSize, ySize : COORDINATE *)
                        500,200, (* xBuffer, yBuffer : COORDINATE *)
                        FALSE,  (* gutter : BOOLEAN *)
                        biggerFont, (* DefaultFontInfo, /* font : FontInfo */ *)
                        ComposeAttribute(Black, White, NormalFont), (* background : ScreenAttribute *)
                        ToplevelWindow,  (* windowType : WindowTypes *)
                        HelpWndProcTW,
                        NormalWindow + AddScrollBars,    (* attribs : WinAttrSet *)
                        NIL); (* createParam : ADDRESS *)
    IF HelpWin = NIL THEN
        WinShell.TerminateDispatchMessages(127);
    END;
    SetAutoScroll(HelpWin,TRUE);
    writehelp(HelpWin)
   ELSE  (* Now must process the cmd, which is the whole reason for this pgm existing! *)
     StringListP := GETRESULT(inputbuf,R);
     IF StringListP <> NIL THEN
       MoveCaretTo(tw,0,12);
       CurrentPointerBeginning(StringListP);
       FOR C := 1 TO StringListP^.len DO
         StringP := GetNextStringFromList(StringListP);
         WriteString(tw,StringP^.S.CHARS,a);
         EraseToEOL(tw,a);
         WriteLn(tw);
       END; (* FOR StringListP has strings to print *)
       C := StringListP^.len + 12;
       WHILE C < MainWindowHeight DO
         EraseToEOL(tw,a);
         WriteLn(tw);
         INC(C);
       END; (* While still have lines to be erased *)
       UTILLIB.DisposeStringListPointerType(StringListP);
     END; (* IF string list pointer is not NIL *)
   END;
   RepaintScreen(tw);
   EXIT;
  END; (* LOOP *)
END ProcessInput;

(*++++*****************************************************************)
PROCEDURE RegWndProcTW(tw:TextWindow; msg:TWMessageRec) : ResponseType;

VAR
    clr   : Colors;
    x,y   : COORDINATE;
    i,int : INTEGER;
    idx,c : CARDINAL;
    ans   : CHAR;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
(*                                           RETURN OkayToClose; *)
    | TWM_CREATE:
        SetScrollDisableWhenNone(tw,TRUE,TRUE);
    | TWM_SIZE:
        SnapWindowToFont(tw,TRUE);
        SetDisplayMode(tw,DisplayVisible);
(*
                    DisplayModes        = (
                      DisplayNormal,    // will activate window, make it visible and reverses minimized or maximized
                      DisplayVisible,   // makes window visible, will not activate window
                      DisplayHidden,    // hides window, active window changes
                      DisplayMinimized, // minimizes window, does not change active window
                      DisplayMaximized  // will activate window, maximizes window
                                           ) BIG;
*)
        SetScrollRangeAllowed(tw,WA_VSCROLL,60);
        SetScrollBarPos(tw,WA_VSCROLL,0);
        SetScrollRangeAllowed(tw,WA_HSCROLL,100);
        SetScrollBarPos(tw,WA_HSCROLL,0);
        SetCaretType(tw,CtHalfBlock);
(*                                                                                 MoveCaretTo(tw,xCaret,yCaret); *)
        MakeCaretVisible(tw);
        CaretOn(tw);
        SetWindowEnable(tw,TRUE);
(*                                                                SetForegroundWindow(tw); not for a child window *)
    | TWM_GAINFOCUS, TWM_ACTIVATEAPP :
        MakeCaretVisible(tw);
        RepaintScreen(tw);
    | TWM_PAINT:
                                                                         (*        WriteReg(RegWin); *)
    | TWM_NOTIFYSCROLL:
    | TWM_KEY:
        SetForegroundWindow(Win);
    ELSE (* case msg.msg *)
    END (* case msg.msg *);
    RETURN DEFAULT_HANDLE;
END RegWndProcTW;

(*++++*****************************************************************)
PROCEDURE HelpWndProcTW(tw:TextWindow; msg:TWMessageRec) : ResponseType;

VAR
    clr   : Colors;
    x,y   : COORDINATE;
    i,int : INTEGER;
    idx,c : CARDINAL;
    ans   : CHAR;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
(*        RETURN OkayToClose; *)
    | TWM_CREATE:
        SetScrollDisableWhenNone(tw,TRUE,TRUE);
    | TWM_SIZE:
        SnapWindowToFont(tw,TRUE);
        SetDisplayMode(tw,DisplayNormal);
(*
        SetDisplayMode(tw,DisplayVisible);
  DisplayModes        = (
                           DisplayNormal, /* will activate window */
                                          /* make it visible */
                                          /* reverses minimized or maximized */
                           DisplayVisible,/* makes window visible */
                                          /* will not activate window */
                           DisplayHidden,/* hides window */
                                         /* active window changes */
                           DisplayMinimized,/* minimizes window */
                                            /* does not change active window */
                           DisplayMaximized/* will activate window */
                                           /* maximizes window */
                          ) BIG;
*)
        SetScrollRangeAllowed(tw,WA_VSCROLL,60);
        SetScrollBarPos(tw,WA_VSCROLL,0);
        SetScrollRangeAllowed(tw,WA_HSCROLL,100);
        SetScrollBarPos(tw,WA_HSCROLL,0);
        SetCaretType(tw,CtHalfBlock);
(*        MoveCaretTo(tw,xCaret,yCaret); *)
        MakeCaretVisible(tw);
        CaretOn(tw);
        SetWindowEnable(tw,TRUE);
        SetForegroundWindow(tw);
    | TWM_GAINFOCUS, TWM_ACTIVATEAPP :
        MakeCaretVisible(tw);
    | TWM_PAINT:
    | TWM_NOTIFYSCROLL:
    | TWM_KEY:
        SetForegroundWindow(Win);
    ELSE (* case msg.msg *)
    END (* case msg.msg *);
    RETURN DEFAULT_HANDLE;
END HelpWndProcTW;

(*++++*****************************************************************)
PROCEDURE ChildWndProcTW(tw:TextWindow; msg:TWMessageRec) : ResponseType;

VAR
    clr   : Colors;
    x,y   : COORDINATE;
    i,int : INTEGER;
    idx,c : CARDINAL;
    ans   : CHAR;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
(*        RETURN OkayToClose; *)
    | TWM_CREATE:
        SetScrollDisableWhenNone(tw,TRUE,TRUE);
    | TWM_SIZE:
        SnapWindowToFont(tw,TRUE);
        SetDisplayMode(tw,DisplayVisible);
(*
        SetDisplayMode(tw,DisplayVisible);
  DisplayModes        = (
                           DisplayNormal, /* will activate window */
                                          /* make it visible */
                                          /* reverses minimized or maximized */
                           DisplayVisible,/* makes window visible */
                                          /* will not activate window */
                           DisplayHidden,/* hides window */
                                         /* active window changes */
                           DisplayMinimized,/* minimizes window */
                                            /* does not change active window */
                           DisplayMaximized/* will activate window */
                                           /* maximizes window */
                          ) BIG;
*)
        SetScrollRangeAllowed(tw,WA_VSCROLL,60);
        SetScrollBarPos(tw,WA_VSCROLL,0);
        SetScrollRangeAllowed(tw,WA_HSCROLL,100);
        SetScrollBarPos(tw,WA_HSCROLL,0);
        SetCaretType(tw,CtHalfBlock);
(*        MoveCaretTo(tw,xCaret,yCaret); *)
        MakeCaretVisible(tw);
        CaretOn(tw);
        SetWindowEnable(tw,TRUE);
(*        SetForegroundWindow(tw); not for a child window *)
    | TWM_GAINFOCUS, TWM_ACTIVATEAPP :
        MakeCaretVisible(tw);
    | TWM_PAINT:
    | TWM_NOTIFYSCROLL:
    | TWM_KEY:
        SetForegroundWindow(Win);
    ELSE (* case msg.msg *)
    END (* case msg.msg *);
    RETURN DEFAULT_HANDLE;
END ChildWndProcTW;

(*++++*****************************************************************)
PROCEDURE WndProcTW(tw:TextWindow; msg:TWMessageRec) : ResponseType;

VAR
    clr   : Colors;
    x,y   : COORDINATE;
    i,int : INTEGER;
    idx,c,len : CARDINAL;
    ans   : CHAR;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        IF RegAltered THEN
                ans := BasicDialogs.YesNo(' Should RegFile be written?','Y');
          IF ans = 'Y' THEN
            OpenCreateFile(RegFile,HPFileName,ReadWriteDenyWrite);
            WriteBlock(RegFile, ADR(Reg), SIZE(Reg) );
            CloseFile(RegFile);
          END; (* ans = Y *)
        END; (* RegAltered *)
        GETSTACK(stk,c);
        OpenCreateFile(StackFile,StackFileName,ReadWriteDenyWrite);
        WriteBlock(StackFile, ADR(stk), SIZE(stk) );
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
        GetCommandLine(inputline);
        len := LENGTH(inputline);
        IF len > 0 THEN
                ProcessInput(tw);
        END; (* if len > zero *)
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
    | TWM_MENU:
(*
  a menu item has been selected menuId = the menu resource id number for the menu item
  TWM_MENU:
       msg.menuId      : INTEGER;
       msg.accel       : BOOLEAN;
*)
         CASE msg.menuId OF
         0   : (* Fix 0 *)
               OutState := fix;
               sigfig := 0;
               RepaintScreen(tw);
       | 1   : (* Fix 2 *)
               OutState := fix;
               sigfig := 2;
               RepaintScreen(tw);
       | 2   : (* Fix 4 *)
               OutState := fix;
               sigfig := 4;
               RepaintScreen(tw);
       | 3   : (* Fix 6 *)
               OutState := fix;
               sigfig := 6;
               RepaintScreen(tw);
       | 4   : (* Float 4 *)
               OutState := float;
               sigfig := 4;
               RepaintScreen(tw);
       | 5   : (* Float 6 *)
               OutState := float;
               sigfig := 6;
               RepaintScreen(tw);
       | 6   : (* Float 8 *)
               OutState := float;
               sigfig := 8;
               RepaintScreen(tw);
       | 7   : (* Float 10 *)
               OutState := float;
               sigfig := 10;
               RepaintScreen(tw);
       | 8   : (* General *)
               OutState := gen;
               RepaintScreen(tw);
       | 9   : (* Write regfile *);
               IF RegAltered THEN
                 RegAltered := FALSE;
                 OpenCreateFile(RegFile,HPFileName,ReadWriteDenyWrite);
                 WriteBlock(RegFile, ADR(Reg), SIZE(Reg) );
                 CloseFile(RegFile);
               END; (* RegAltered *)
       | 10  : (* Conversion Hex out *)
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
       | 11  : (* Info Prime *)
              r := READX();
              LC := VAL(LONGCARD,(ABS(r)));
              bool := IsPrime(LC);
              IF bool THEN
                FormatString.FormatString(' %l is prime.',str5,LC);
              ELSE
                FormatString.FormatString(' %l is not prime.',str5,LC);
              END;
              BasicDialogs.MessageBox(str5,MsgInfo);
       | 12  : (* Info DOW Day of Week *)
              int := round(READX());
              str3 := DAYNAMES[int MOD 7];
              BasicDialogs.MessageBox(str3,MsgInfo);
       | 13  : (* copy To Clipboard *)
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
                RETURN USER_HANDLE;
              END;
              pinstr^ := str9;
              IF  NOT SetClipboard(clipfmt) THEN
                WriteString(tw,' Setclipboard failed.',a);
                WriteLn(tw);
              END;
              CloseClipboard(tw);
       | 14  : (* paste from clipboard *)
              IF NOT ClipboardFormatAvailable(clipfmt) THEN
                WriteStringAt(tw,0,0,'text format not available in clipboard.',a);
                RETURN USER_HANDLE;
              ELSIF NOT OpenClipboard(tw) THEN
                WriteString(tw,' OpenClipboard failed.',a);
                WriteLn(tw);
                RETURN USER_HANDLE;
              END;
              pinstr := GetClipboard(clipfmt);
              IF pinstr = NIL THEN
                WriteStringAt(tw,0,1,'unable to get clipboard.',a);
                RETURN USER_HANDLE;
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
                RETURN USER_HANDLE;
              END;
              PUSHX(r);
              RepaintScreen(tw);
       | 15  : (* Help *)
              HelpWin := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "Help Window", (* name : ARRAY OF CHAR *)
                        "",                   (* menu : ARRAY OF CHAR *)
                        FullGreenIcon,            (* icon : ARRAY OF CHAR *)
                         1, 1,        (* -1,-1,  x,y= the initial screen coordinates for the window to be displayed *)
                        99,30, (* xSize, ySize : COORDINATE  increased the Y value (# of rows) to match the help command *)
                        500,200, (* xBuffer, yBuffer : COORDINATE *)
                        FALSE,  (* gutter : BOOLEAN *)
                        biggerFont, (* DefaultFontInfo, /* font : FontInfo */ *)
                        ComposeAttribute(Black, White, NormalFont), (* background : ScreenAttribute *)
                        ToplevelWindow,  (* windowType : WindowTypes *)
                        HelpWndProcTW,
                        NormalWindow + AddScrollBars,    (* attribs : WinAttrSet *)
                        NIL); (* createParam : ADDRESS *)
              IF HelpWin = NIL THEN
                WinShell.TerminateDispatchMessages(127);
              END;
              SetAutoScroll(HelpWin,TRUE);
              writehelp(HelpWin)
      END; (* case menuId *)
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
        ch3 := ' ';
        CASE msg.k_ch OF
          CHR(8) :                    (* backspace      *)
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
        | CHR(10):                        (* line feed       *)
        | CHR(13),' ','+','/','%','=',"'",';':        (* carriage RETURN *)
(*  As child window works well enough, terminal window output not needed
          Terminal.WriteString(inputline);
          Terminal.WriteLn;
*)
          IF msg.k_ch = CHR(13) THEN (* do nothing *)
          ELSIF msg.k_ch = ' ' THEN
            IF LENGTH(inputline) = 0 THEN  (* sometimes discard space character *)
                Strings.Append(msg.k_ch,inputline);
            END; (*IF *)
          ELSIF msg.k_ch = '=' THEN
            Strings.Append('+',inputline)
          ELSIF msg.k_ch = "'" THEN
            Strings.Append('-',inputline)
          ELSIF msg.k_ch = ';' THEN
            Strings.Append('*',inputline)
          ELSE
            Strings.Append(msg.k_ch,inputline)
          END;
          WriteString(WinChild,inputline,a);
          WriteLn(WinChild);
          SetForegroundWindow(tw : TextWindow);
          ProcessInput(tw);
          RETURN USER_HANDLE;
        | CHR(27):                         (* escape *)
         (* FUNC *) CloseWindow(tw, CM_REQUEST);
        ELSE (* CASE ch *)
          IF msg.k_ch = '=' THEN           (* These specially mapped chars are now in a separate part     *)
            ch3 := '+';                    (*  of the case sttmnt and will not be processed here any more *)
          ELSIF msg.k_ch = ';' THEN
            ch3 := '*';
          ELSIF msg.k_ch = "'" THEN
            ch3 := "-";
          ELSE
            ch3 := CAP(msg.k_ch);
          END (*if*);
          Strings.Append(ch3,inputline);
          WriteString(tw,ch3,a);
          slen := LENGTH(inputline);
          IF (ch3 = '-') AND (slen >= 2) AND (CAP(inputline[slen-1]) <> "E") THEN  (* allow subt operator to be here, and not unary minus *)
            WriteString(WinChild,inputline,a);
            WriteLn(WinChild);
            SetForegroundWindow(tw : TextWindow);
            ProcessInput(tw);
            RETURN(USER_HANDLE);
          END; (* if subt operator found *)

(*          WriteString(tw,msg.k_ch,a);   *)
(*          RepaintScreen(tw);    works but lose cursor when typing.  I'll keep it out. *)
        END (* case ch *);
      ELSIF msg.k_special = KEY_PAGEUP THEN
        Strings.Append(' ',inputline); (* if don't append a blank, program may exit if there is no other input on this line *)
        ProcessInput(tw);
        PUSHX(READX());                          (* stackup *)
        RepaintScreen(tw);
        RETURN USER_HANDLE;
      ELSIF msg.k_special = KEY_PAGEDOWN THEN
        Strings.Append('!',inputline);           (* stackdn *)
        ProcessInput(tw);
      ELSIF msg.k_special = KEY_HOME THEN
        Strings.Append(' ',inputline);
        ProcessInput(tw);
        PUSHX(READX());                          (* stackup *)
        RepaintScreen(tw);
      ELSIF msg.k_special = KEY_END THEN
        Strings.Append('!',inputline);           (* stackdn *)
        ProcessInput(tw);
      ELSIF msg.k_special = KEY_RIGHTARROW THEN
        Strings.Append('~',inputline);           (* swap xy *)
        ProcessInput(tw);
        RETURN USER_HANDLE;
      ELSIF msg.k_special = KEY_LEFTARROW THEN
        Strings.Append('~',inputline);           (* swap xy *)
        ProcessInput(tw);
        RETURN USER_HANDLE;
      ELSIF msg.k_special = KEY_UPARROW THEN
        Strings.Append(' ',inputline);
        ProcessInput(tw);
        PUSHX(READX());                          (* stackup *)
        RepaintScreen(tw);
        RETURN USER_HANDLE;
      ELSIF msg.k_special = KEY_DOWNARROW THEN
        Strings.Append('!',inputline);           (* stackdn *)
        ProcessInput(tw);
        RETURN USER_HANDLE;
      ELSIF msg.k_special = KEY_INSERT THEN
      ELSIF msg.k_special = KEY_DELETE THEN
        Strings.Delete(inputline,LENGTH(inputline)-1,1);
        x := Xpos(tw);
        WriteStringAt(tw,x-1,0,' ',a);
        MoveCaretTo(tw,x-1,0);
      ELSIF (msg.k_special = KEY_F1) OR (msg.k_special = KEY_F2) THEN
        Strings.Append("HELP",inputline);
        ProcessInput(tw);
      ELSE (* msg.k_special *)
      END (*if*);
     END(* for *);
    ELSE (* case msg.msg *)
    END (* case msg.msg *);
    RETURN USER_HANDLE;
END WndProcTW;

PROCEDURE Start(param : ADDRESS);
BEGIN
    UNREFERENCED_PARAMETER(param);
(*
                       PROCEDURE CreateWindow(parent : WinShell.Window;  (* create a new window *) (* parent = as WinShell  *)
                         name : ARRAY OF CHAR;  (* name = as WinShell  *)
                         menu : ARRAY OF CHAR;  (* menu = the menu for the window. Can be "". *)
                         icon : ARRAY OF CHAR;  (* icon =  as WinShell *)
                         x, y : COORDINATE;  (* x, y = the initial screen coordinates for the window to be displayed.
                                               If a parameter is -1 then the operating system will choose a default location for that coordinate.
                                               These positions are in pixels and are relative to the parent window client area origin for child windows,
                                               or relative to the screen origin for all other windows. *)
                         xSize, ySize : COORDINATE; (* xSize, ySize = the initial width and height in character cells if -1 then a system default size will be used *)
                         xBuffer, yBuffer : COORDINATE;  (* xBuffer, yBuffer = the size of the screen buffer. the window can never be larger than the screen buffer.  If either xBuffer
                                                            or yBuffer is -1 the screen buffer is a variable size and is sized to the number of cells the window client
                                                            area currently is capable displaying. *)
                         gutter : BOOLEAN; (* gutter = TRUE then the text window will always have a blank "gutter" on the left edge of the text window.  FALSE the text will start at the left edge of the client area. *)
                         font : FontInfo; (* font = the font to use for this window *)
                         background : ScreenAttribute; (* background = the background color for this window *)
                         windowType : WindowTypes;
                         wndProc : TextWindowProcedure;  (* wndProc = the window procedure *)
                         attribs : WinAttrSet;  (* attribs = as WinShell *)
                         createParam : ADDRESS) : TextWindow;  (* createParam = an arbitrary value you can use to pass information to the window procedure of the window. this value is passed in the WSM_CREATE message. *)
                       (* returns the window handle if successfull, otherwise NIL *)

                       WindowTypes         = (TopLevel, MdiChild);
*)
    Win := CreateWindow(NIL, (* parent : WinShell.Window *)
                        "HPWin TextWindows Menu", (* name : ARRAY OF CHAR *)
                        "#100",                   (* menu : ARRAY OF CHAR *)
                        FullGreenIcon,            (* icon : ARRAY OF CHAR *)
                         1, 1, (* -1,-1,  x,y= the initial screen coordinates for the window to be displayed *)
                        90,MainWindowHeight, (* xSize, ySize : COORDINATE *)
                        800,200, (* xBuffer, yBuffer : COORDINATE *)
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
(*                                                  PROCEDURE GetWinShellHandle(tw : TextWindow) : WinShell.Window; *)
    winshellwindow := GetWinShellHandle(Win);
    WinChild := CreateWindow(winshellwindow, (* parent : WinShell.Window *)
                        '', (* name : ARRAY OF CHAR *)
                        '', (* menu : ARRAY OF CHAR *)
                        HalfCalcIcon, (* "#100", *)       (* icon : ARRAY OF CHAR *)
                        1050, 10, (* x, y = the initial screen coordinates for the window to be displayed.  If a parameter is -1 then the operating system will choose a default location for that coordinate.
                                 These positions are in pixels and are relative to the parent window client area origin for child windows, or relative to the screen origin for all other windows. *)
                        30,MainWindowHeight, (* xSize, ySize : COORDINATE *)
                        500,200, (* xBuffer, yBuffer : COORDINATE *)
                        FALSE,  (* gutter : BOOLEAN *)
                        biggerFont, (* DefaultFontInfo, /* font : FontInfo */ *)
                        ComposeAttribute(Black, White, NormalFont), (* background : ScreenAttribute *)
                        ToplevelWindow,  (* windowType : WindowTypes *) (* does not work if this is ChildWindow, I don't know why yet *)
                        ChildWndProcTW,
                        NormalChildWindow + AddScrollBars,    (* attribs : WinAttrSet *)
                        NIL); (* createParam : ADDRESS *)
    IF WinChild = NIL THEN
        WinShell.TerminateDispatchMessages(127);
    END;
    SetAutoScroll(WinChild,TRUE);

    RegWin := CreateWindow(winshellwindow, (* parent : WinShell.Window *)
                        '', (* name : ARRAY OF CHAR *)
                        '', (* menu : ARRAY OF CHAR *)
                        HalfCalcIcon, (* "#100", *)       (* icon : ARRAY OF CHAR *)
                        10, 500, (* x, y = the initial screen coordinates for the window to be displayed.  If a parameter is -1 then the operating system will choose a default location for that coordinate.
                                               These positions are in pixels and are relative to the parent window client area origin for child windows, or relative to the screen origin for all other windows. *)
                        60,25, (* xSize, ySize : COORDINATE *)
                        500,200, (* xBuffer, yBuffer : COORDINATE *)
                        FALSE,  (* gutter : BOOLEAN *)
                        biggerFont, (* DefaultFontInfo, /* font : FontInfo */ *)
                        ComposeAttribute(Black, White, NormalFont), (* background : ScreenAttribute *)
                        ToplevelWindow,  (* windowType : WindowTypes *) (* does not work if this is ChildWindow, I don't know why yet *)
                        RegWndProcTW,
                        NormalChildWindow + AddScrollBars,    (* attribs : WinAttrSet *)
                        NIL); (* createParam : ADDRESS *)
    IF RegWin = NIL THEN
        WinShell.TerminateDispatchMessages(127);
    END;
    SetAutoScroll(WinChild,TRUE);
    WriteReg(RegWin);
END Start;


(*++++*****************************************************************)(**********************************************************************)
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

  (* Allowed colors are Black, Blue, Green, Cyan, Red, Purple, Brown, DarkGray, LightGray, LightBlue, LightGreen, LightCyan, LightRed, Magenta, Yellow, White *)
  a := ComposeAttribute(Black, White, NormalFont);
  aBold := ComposeAttribute(Black, White, BoldFont);
  aInverted := ComposeAttribute(White, Black, NormalFont);
  aInvertedRed := ComposeAttribute(Red, Black, NormalFont);
  aInvertedLightRed := ComposeAttribute(LightRed, Black, NormalFont);
  aInvertedCyan := ComposeAttribute(Cyan, Black, NormalFont);
  aInvertedYellow := ComposeAttribute(Yellow, Black, NormalFont);
  aInvertedLightBlue := ComposeAttribute(LightBlue, Black, NormalFont);
  aInvertedLightCyan := ComposeAttribute(LightCyan, Black, NormalFont);
  biggerFont := DefaultFontInfo;
  biggerFont.height := 140;

  inputprocessed := FALSE;
  RegAltered := FALSE;
  opset    := charsetyp{'#','+','-','<','=','%','>','*','/','^'};

  UserprofileExists := GetSymbol("userprofile",Path);
  IF UserprofileExists THEN
    bool := SetDefaultPath(Path);  (* will ignore the result of this call, as it does not matter anyway. *)
  END;
  HPFileName := RegFileName;
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
    FOR c := 0 TO 35 DO
      IF ABS(Reg[c]) < 1.0E-20 THEN
        Reg[c] := 0.0;
      END; (* if *)
    END; (* for c  *)
  END (* if hpfileexists *);
  StackFileExists := FileExists(StackFileName);
  IF StackFileExists THEN
    OpenFile(StackFile,StackFileName,ReadOnlyDenyWrite);
    IF StackFile.status = 0 THEN
      ReadBlock(StackFile,ADR(stk), SIZE(stk) );
      CloseFile(StackFile);
      FOR c := STACKSIZE TO 1 BY -1 DO
        PUSHX(stk[c]);
      END; (* for *)
    END; (* if stackfile.status=0 *)
  END; (* if stackfileexists *)


  FUNC WinShell.DispatchMessages(Start, NIL);
END HPWinMenu.
