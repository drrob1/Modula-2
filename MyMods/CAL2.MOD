<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:Cal.RES*>
%ELSE
%END
MODULE CAL;
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
   2 May 04 -- Converted to Stony Brook M2 for Windows V4
  13 Jan 07 -- Converted to only print out short form to screen.
  14 Jan 07 -- Made it TextWindows and allowed some manipulations of month & yr output.
                And changed logic so it does not display prev month, but next 2 months.
   5 May 07 -- Made changes to highlight today in the display.  Not so easy!
  11 May 07 -- Added highlighting of holidays that I can easily compute.
*)
IMPORT SYSTEM;
FROM SYSTEM IMPORT ADR, FUNC, UNREFERENCED_PARAMETER, ADDRESS;
IMPORT WINUSER, WIN32, WINGDI, WINX, WinShell;
IMPORT Strings, ExStrings, MemUtils;
FROM Strings IMPORT
    Append, Equal, Delete, Concat, Capitalize;
FROM ExStrings IMPORT
    AppendChar, EqualI;
IMPORT Terminal, BasicDialogs;
(* FROM Terminal IMPORT Read, WriteString, WriteLn, Write, ReadChar, Reset; *)
FROM BasicDialogs IMPORT MessageTypes;
IMPORT MiscM2;
FROM MiscM2 IMPORT ReadString;
IMPORT WholeStr, LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,
    STRLENFNT,STRCMPFNT,SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF,FILLCHAR,
    TRIMFNT;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;

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
Colors      = (Black,Blue,Green,Cyan,Red,Purple,Brown,DarkGray,LightGray,LightBlue,LightGreen,LightCyan,
                LightRed,Magenta,Yellow,White);
FontStyles = (FsItalic, FsBold);
FontStyleSet = PACKEDSET OF FontStyles;
NormalFont  = FontStyleSet{};
BoldFont    = FontStyleSet{FsBold};
ItalicFont  = FontStyleSet{FsItalic};
*)

(****************************************************************************)
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts, LowLong;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  
  FROM HolMod IMPORT HOLCMD,EASTER;

  FROM MyFIO IMPORT DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRBL,FWRSTR,FWRLN,FAPPEND,GETFNM;
(*
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT GETNEWDATE,TIME2MDY,MDY2TIME,MDY2STR,GREG2JUL,
    JUL2GREG,GETMDY;
  FROM UL2 IMPORT COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF,GETFNM;
  FROM FloatingUtilities IMPORT Frac,Int,Round,Float,Trunc;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    GETTKNREAL,UNGETTKN;
*)

  CONST
      LNSIZ      = 80;
      LNPPAG     = 56; (* Used by PRMONTH proc.  Top 6 lines used by
                          Month and day names. *)
      FF         = 14C; (* CHR(12) *)
      CR         = 15C; (* CHR(13) *)
      ESC        = 33C; (* CHR(27) *)
(*      NULL       = 0C; *)
      BLANKCHR   = ' ';
      BLANK35    = '                       ';
      BOXCHRVERT = '|';
      BOXCHRHORZ = '_';
      szAppName = 'CAL';
      LastMod = '14 Jan 07';
      CalIcon32 = '#100';
      CalIcon16 = '#200';


  TYPE
    MNEnum    = (JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DCM);
    DayEnum   = (dummy,Sun,Mon,Tue,Wed,Thur,Fri,Sat);  (* need ORD(Sun) = 1 *);
    CALMODE   = (FINI,CAL1,CAL12);
    LNTYP     = ARRAY [0..LNSIZ] OF CHAR;
    MonthType = RECORD 
                  dayStr : STR10TYP;
                  sa     : ScreenAttribute;
                  dayNum : CARDINAL;
                END; (* record *)

  VAR
    OUTUN1                                          : MYFILTYP;
    PROMPT,NAMDFT,TYPDFT,BLANKBUF,OUTFNAM,INPUT,BUF : BUFTYP;
    MN, MN2, MN3                               : MNEnum; (*Month Number Vars*)
    ANS, CH, PAGCHR                                 : CHAR;
    MONTH : ARRAY [JAN..DCM],[1..6],[1..7] OF MonthType; (* subscripts are month, week, day *)
    YEAR,IYEAR,LAS2DI,FIR2DI,I,J,month,day,K,DOW,W,JAN1DOW,FEBDAYS : CARDINAL;
    DIM, WIM                                    : ARRAY[JAN..DCM] OF CARDINAL;
    CALSTATE                                    : CALMODE;
    MONNAMSHORT                                 : ARRAY[JAN..DCM] OF STR10TYP;
    MONNAMLONG                                  : ARRAY[JAN..DCM] OF LNTYP;
    DAYSNAMLONG, DAYSNAMSHORT, TOPLINE, BotLnWithMonth,BOTMLINE, MIDLINE, ANYLINE : LNTYP;
    BLANKSTR2,YEARSTR,FNameStr,BlankStr3       : STR10TYP;
    RETCOD   : CARDINAL;
    INTVAL,JULDATE   : INTEGER;
    TOKEN    : BUFTYP;
    a,aBold,aReverse,aRevBold  : ScreenAttribute;
    Win                        : TextWindow;
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
(*
    TKNSTATE : FSATYP;
*)

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

PROCEDURE HolidayAssign(mn : MNEnum);
(*
  This procedure will change the screen attribute for the holidays I can
  compute.  They are: New Years Day, Pres Day, Easter, Mom Day, Dad Day,
  July 4, Labor Day, Columbus Day, Election Day, Thanksgiving & Christmas
*)
VAR
    i,dow,StartWeek : CARDINAL;
    juldate : LONGINT;

BEGIN 
  CASE mn OF
     JAN: juldate := JULIAN(1,1,YEAR); 
          dow := juldate MOD 7 + 1;
          MONTH[mn,1,dow].sa := aReverse;
  |  FEB: HOLCMD(YEAR,10,DD);
          StartWeek := DD DIV 7 + 1;
          PresLoopLabel:
          FOR i := StartWeek TO WIM[mn] DO
            IF MONTH[mn,i,ORD(Mon)].dayNum = DD THEN 
              MONTH[mn,i,ORD(Mon)].sa := aReverse;
              BREAK PresLoopLabel;
            END; (* if MONTH *)
          END; (* for i *)
  |  MAR: EASTER(YEAR,MM,DD);
          IF MM = ORD(mn)+1 THEN 
            StartWeek := DD DIV 7 + 1;
            MarLoopLabel:
            FOR i := StartWeek TO WIM[mn] DO
              IF MONTH[mn,i,ORD(Sun)].dayNum = DD THEN 
                MONTH[mn,i,ORD(Sun)].sa := aReverse;
                BREAK MarLoopLabel;
              END; (* if MONTH *)
            END (* for i *)
          END; (* if MM *)
  |  APR: EASTER(YEAR,MM,DD);
          IF MM = ORD(mn)+1 THEN 
            StartWeek := DD DIV 7 + 1;
            AprLoopLabel:
            FOR i := StartWeek TO WIM[mn] DO
              IF MONTH[mn,i,ORD(Sun)].dayNum = DD THEN 
                MONTH[mn,i,ORD(Sun)].sa := aReverse;
                BREAK AprLoopLabel;
              END; (* if MONTH *)
            END (* for i *)
          END; (* if MM *)
  |  MAY: HOLCMD(YEAR,12,DD);
          StartWeek := DD DIV 7 + 1;
          MomLoopLabel:
          FOR i := StartWeek TO WIM[mn] DO
            IF MONTH[mn,i,ORD(Sun)].dayNum = DD THEN 
              MONTH[mn,i,ORD(Sun)].sa := aReverse;
              BREAK MomLoopLabel;
            END; (* if month *)
          END; (* for i *)
          
          HOLCMD(YEAR,13,DD);
          StartWeek := DD DIV 7 + 1;
          MemorialLoopLabel:
          FOR i := StartWeek TO WIM[mn] DO
            IF MONTH[mn,i,ORD(Mon)].dayNum = DD THEN 
              MONTH[mn,i,ORD(Mon)].sa := aReverse;
              BREAK MemorialLoopLabel;
            END; (* if month *)
          END; (* for i*)
          
  |  JUN: HOLCMD(YEAR,14,DD);
          StartWeek := DD DIV 7 + 1;
          DadLoopLabel:
          FOR i := StartWeek TO WIM[mn] DO
            IF MONTH[mn,i,ORD(Sun)].dayNum = DD THEN
              MONTH[mn,i,ORD(Sun)].sa := aReverse;
              BREAK DadLoopLabel;
            END; (* if month *)
          END; (* for i *)
  |  JUL: juldate := JULIAN(7,4,YEAR);
          dow := juldate MOD 7 + 1;
          July4LoopLabel:
          FOR i := 1 TO 2 DO
            IF MONTH[mn,i,dow].dayNum = DD THEN 
              MONTH[mn,i,dow].sa := aReverse;
              BREAK July4LoopLabel;
            END; (* if month *)
          END; (* for i *)
  |  AUG: 
  |  SEP: HOLCMD(YEAR,15,DD);
          StartWeek := DD DIV 7 + 1;
          SepLoopLabel:
          FOR i := StartWeek TO WIM[mn] DO
            IF MONTH[mn,i,ORD(Mon)].dayNum = DD THEN 
              MONTH[mn,i,ORD(Mon)].sa := aReverse;
              BREAK SepLoopLabel;
            END; (* if month *)
          END; (* for i *)
  |  OCT: HOLCMD(YEAR,16,DD);
          StartWeek := DD DIV 7 + 1;
          OctLoopLabel:
          FOR i := StartWeek TO WIM[mn] DO
            IF MONTH[mn,i,ORD(Mon)].dayNum = DD THEN 
              MONTH[mn,i,ORD(Mon)].sa := aReverse;
              BREAK OctLoopLabel;
            END; (* if month *)
          END; (* for i *)
  |  NOV: HOLCMD(YEAR,17,DD);
          StartWeek := DD DIV 7 + 1;
          ElecLoopLabel:
          FOR i := StartWeek TO WIM[mn] DO
            IF MONTH[mn,i,ORD(Tue)].dayNum = DD THEN 
              MONTH[mn,i,ORD(Tue)].sa := aReverse;
              BREAK ElecLoopLabel;
            END; (* if month *)
          END; (* for i *)
          
          HOLCMD(YEAR,18,DD);
          StartWeek := DD DIV 7 + 1;
          ThanksLoopLabel:
          FOR i := StartWeek TO WIM[wm] DO
            IF MONTH[mn,i,ORD(Thur)].dayNum = DD THEN 
              MONTH[mn,i,ORD(Thur)].sa := aReverse;
              BREAK ThanksLoopLabel;
            END; (* if month *)
          END; (* for i *)
  |  DCM: juldate := JULIAN(12,25,YEAR);
          dow := juldate MOD 7 + 1;
          XMasLoopLabel:
          FOR i := 4 TO 5 DO
            IF MONTH[mn,i,dow].dayNum = DD THEN 
              MONTH[mn,i,dow].sa := aReverse;
              BREAK XMasLoopLabel;
            END; (* if month *)
          END; (* for i *)
  ELSE
  END; (* case *)
END HolidayAssign;
  
PROCEDURE DATEASSIGN(MN : MNEnum);
(*
********************* DATEASSIGN *******************************************
DATE ASSIGNMENT FOR MONTH.
THIS ROUTINE WILL ASSIGN THE DATES FOR AN ENTIRE MONTH.  IT WILL PUT THE CHAR
REPRESENTATIONS OF THE DATE IN THE FIRST 2 BYTES.  THE EXTRA BYTES CAN BE USED
LATER FOR SEPCIAL PRINTER CONTROL CODES.

INPUT FROM GBL VAR'S : DIM(MN), DOW
OUTPUT TO  GBL VAR'S : MONTH(MN,,), WIM(MN)
*)

VAR
    I,J,DATE,W,TodayM,TodayD,TodayY,dow,StartWeek : CARDINAL;
    juldate : LONGINT;
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
    MONTH[MN,W,DOW].dayStr := DATESTR;
    MONTH[MN,W,DOW].sa := a;
    MONTH[MN,W,DOW].dayNum := DATE;
    INC(DOW);
  END(*FOR*);
  WIM[MN] := W;  (* Return number of weeks in this month *)
  
  TIME2MDY(TodayM,TodayD,TodayY);
  juldate := JULIAN(TodayM,TodayD,TodayY);
  IF (TodayM <> (ORD(MN)) +1) AND (TodayY <> YEAR)  THEN RETURN END;
  dow := juldate MOD 7 + 1;
  StartWeek := TodayD DIV 7 + 1;
  LoopLabel:
  FOR I := StartWeek TO W DO
    IF MONTH[MN,I,dow].dayNum = TodayD THEN 
      MONTH[MN,I,dow].sa := aReverse;
      BREAK LoopLabel;
    END; (*if month*)
  END; (* for I *)
  
END DATEASSIGN;

PROCEDURE DoMainRtn(tw : TextWindow);
VAR I,J,W : CARDINAL;
    mn : MNEnum;

BEGIN
    FOR mn := JAN TO DCM DO
      FOR I := 1 TO 6 DO
        FOR J := 1 TO 7 DO
          MONTH[mn,I,J].dayStr := BLANKSTR2;
          MONTH[mn,I,J].sa := a;
          MONTH[mn,I,J].dayNum := 0;
        END(*FOR*);
      END(*FOR*);
    END(*FOR*);

    CALSTATE := CAL1;
    IF YEAR < 1600 THEN RETURN END;

    JULDATE := JULIAN(1,1,YEAR);
    JAN1DOW := JULDATE MOD 7 +1;
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

    FOR mn := JAN TO DCM DO 
      DATEASSIGN(mn); 
      HolidayAssign(mn);
    END(*FOR*);

(* Write one page calendar *)
      WriteString(tw,BLANK35,a);
      WriteString(tw,YEARSTR,a);
      WriteLn(tw);
      WriteLn(tw);

(* off by 1 error as enum types are zero origin, month numbers are 1 origin *)
(*      month := 12;  debugging sttmnt only *)
      MN := JAN;
      IF (month >= 2) AND (month <= 10) THEN
        MN := VAL(MNEnum,ORD(month-1));
      ELSIF month = 12 THEN
        MN := OCT;
      END (*if month*);

      MN2  := VAL(MNEnum,(ORD(MN) + 1));
      MN3 := VAL(MNEnum,(ORD(MN2) + 1));
      WriteString(tw,MONNAMLONG[MN],a);
      WriteString(tw,MONNAMLONG[MN2],a);
      WriteString(tw,MONNAMLONG[MN3],a);
      WriteLn(tw);
      WriteLn(tw);
      WriteLn(tw);
      WriteString(tw,DAYSNAMSHORT,a);
      WriteString(tw,DAYSNAMSHORT,a);
      WriteString(tw,DAYSNAMSHORT,a);
      WriteLn(tw);
      FOR W := 1 TO 6 DO
        FOR I := 1 TO 7 DO
          WriteString(tw,BLANKCHR,a);
          WriteString(tw,MONTH[MN,W,I].dayStr,MONTH[MN,W,I].sa);
        END(*FOR I*);
        WriteString(tw,'    ',a);
        FOR I := 1 TO 7 DO
          WriteString(tw,BLANKCHR,a);
          WriteString(tw,MONTH[MN2,W,I].dayStr,MONTH[MN2,W,I].sa);
        END(*FOR I*);
        WriteString(tw,'    ',a);
        FOR I := 1 TO 7 DO
          WriteString(tw,BLANKCHR,a);
          WriteString(tw,MONTH[MN3,W,I].dayStr,MONTH[MN3,W,I].sa);
        END(*FOR I*);
        WriteLn(tw);
      END(*FOR W*);
    WriteLn(tw);
    WriteLn(tw);
    WriteString(tw,BLANK35,a);
    WriteString(tw,YEARSTR,a);
    WriteLn(tw);
  CALSTATE := FINI;
  MoveCaretTo(tw,xCaret,yCaret);
END DoMainRtn;

(*++++*****************************************************************)
PROCEDURE WndProcTW(tw : TextWindow; msg : TWMessageRec) : ResponseType;
(**********************************************************************)
VAR
    clr   : Colors;
    x,y   : COORDINATE;
    i,int : INTEGER;
    idx,c : CARDINAL;
    ans   : CHAR;
BEGIN
    CASE msg.msg OF
    TWM_CLOSE:
        IF msg.closeMode = CM_DICTATE THEN
            WinShell.TerminateDispatchMessages(0);
        END;
        RETURN OkayToClose;
    | TWM_CREATE:
        (* FUNC SetWindowIcon(tw, '#300'); *)
        xCaret := 0;
        yCaret := 0;
        SetScrollDisableWhenNone(tw,TRUE,TRUE);


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
        TIME2MDY(month,day,YEAR);
        WholeStr.CardToStr(YEAR,YEARSTR);

        DoMainRtn(tw);

    | TWM_NOTIFYSCROLL:
    | TWM_MENU:
(*
  a menu item has been selected menuId = the menu resource id number for the menu item
  TWM_MENU:
       msg.menuId      : INTEGER;
       msg.accel       : BOOLEAN;


      CASE msg.menuId OF
      END; /* case menuId */
*)

    |
    TWM_KEY:
     FOR i := 0  TO INT(msg.k_count-1) DO
      IF (msg.k_special = KEY_NOTSPECIAL) THEN
        CASE msg.k_ch OF
          CHR(8) :                    (* backspace      *)
        | CHR(9) :                    (* tab                 *)
        | CHR(10):                    (* line feed       *)
        | CHR(13):                    (* carriage RETURN *)
            CloseWindow(tw, CM_REQUEST);
        | CHR(27):                    (* escape *)
            CloseWindow(tw, CM_REQUEST);
        | ' ':                        (* space bar *)
            CloseWindow(tw, CM_REQUEST);
        | '1':
             month := 1;
             DoMainRtn(tw);
        | '2':
             month := 2;
             DoMainRtn(tw);
        | '3':
             month := 3;
             DoMainRtn(tw);
        | '4':
             month := 4;
             DoMainRtn(tw);
        | '5':
             month := 5;
             DoMainRtn(tw);
        | '6':
             month := 6;
             DoMainRtn(tw);
        | '7':
             month := 7;
             DoMainRtn(tw);
        | '8':
             month := 8;
             DoMainRtn(tw);
        | '9':
             month := 9;
             DoMainRtn(tw);
        | '0':
             month := 10;
             DoMainRtn(tw);
        ELSE (* CASE ch *)
            CloseWindow(tw, CM_REQUEST);
        END (* case ch *);
      ELSIF msg.k_special = KEY_PAGEUP THEN
            INC(YEAR);
            WholeStr.CardToStr(YEAR,YEARSTR);
            DoMainRtn(tw);
      ELSIF msg.k_special = KEY_PAGEDOWN THEN
            DEC(YEAR);
            WholeStr.CardToStr(YEAR,YEARSTR);
            DoMainRtn(tw);
      ELSIF msg.k_special = KEY_HOME THEN
      	    month := 1;
      	    DoMainRtn(tw);
      ELSIF msg.k_special = KEY_END THEN
      	    month := 10;
      	    DoMainRtn(tw);
      ELSIF msg.k_special = KEY_RIGHTARROW THEN
            IF month < 10 THEN INC(month); END;
            DoMainRtn(tw);
      ELSIF msg.k_special = KEY_LEFTARROW THEN
            IF month >= 2 THEN DEC(month); END;
            DoMainRtn(tw);
      ELSIF msg.k_special = KEY_UPARROW THEN
            INC(YEAR);
            WholeStr.CardToStr(YEAR,YEARSTR);
            DoMainRtn(tw);
      ELSIF msg.k_special = KEY_DOWNARROW THEN
            DEC(YEAR);
            WholeStr.CardToStr(YEAR,YEARSTR);
            DoMainRtn(tw);
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
                        "Calendar Screen Display for Windows", (* name : ARRAY OF CHAR *)
                        "",                   (* menu : ARRAY OF CHAR *)
                        CalIcon16,            (* icon : ARRAY OF CHAR *)
                        -1,-1, (* x,y= the initial screen coordinates for the window to be displayed *)
                        75,15, (* xSize, ySize : COORDINATE *)
                        500,500, (* xBuffer, yBuffer : COORDINATE *)
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


(*
********************* MAIN *********************************************
*)
BEGIN
  a := ComposeAttribute(Black, White, NormalFont);
  aBold := ComposeAttribute(Black, White, BoldFont);
  aReverse := ComposeAttribute(White, Blue, NormalFont);
  aRevBold := ComposeAttribute(White, Black, BoldFont);

  BLANKSTR2 := '  ';
  BlankStr3 := '| ';
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
'=============================================================================='
             ;
  BOTMLINE :=
'=============================================================================='
             ;
  MIDLINE :=
'---------+-----------+-----------+-----------+-----------+-----------+--------'
             ;
  ANYLINE  :=
'         |           |           |           |           |           |';

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

  FUNC WinShell.DispatchMessages(Start, NIL);

END CAL.
