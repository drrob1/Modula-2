MODULE CALScreen;
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
*)
IMPORT Terminal, BasicDialogs, WinShell;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings, MemUtils;
IMPORT WholeStr, LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,
    STRLENFNT,STRCMPFNT,SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF,FILLCHAR,
    TRIMFNT;
(*
FROM TextWindows IMPORT
    /* TYPES & CONSTS */
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, FontInfo, WinAttr, ClipboardFormat,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo,
    /* VARS */
    /* PROCS */
    ComposeAttribute, CreateWindow, WindowTypes, SpecialKeys,
    GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, WriteString,
    WriteStringAt, WriteCellsAt, WriteCells, WriteLn, EraseToEOL, ChangeAttr,
    ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn,
    SetAutoScroll, WinShellToTextWindowMessage,
    MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect,
    GetBufferRect, EraseScreen, EraseRect, GetWinShellHandle, FindTextWindow,
    SetDisplayMode,GetDisplayMode,SetWindowEnable, /*  SetWindowTitle, */
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
*)

(****************************************************************************)
  IMPORT MiscM2;
  FROM MiscM2 IMPORT ReadString;
  FROM Terminal IMPORT Read, WriteString, WriteLn, Write, ReadChar, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts, LowLong;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;

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
*)
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETCHR,UNGETCHR,GETTKN,
    GETTKNREAL,UNGETTKN;

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
    YEAR,IYEAR,LAS2DI,FIR2DI,I,J,month,day,
                       K,DOW,W,JAN1DOW,FEBDAYS : CARDINAL;
    DIM, WIM                                   : ARRAY[JAN..DCM] OF CARDINAL;
    CALSTATE                                   : CALMODE;
    MONNAMSHORT                                : ARRAY[JAN..DCM] OF STR10TYP;
    MONNAMLONG                                 : ARRAY[JAN..DCM] OF LNTYP;
    DAYSNAMLONG, DAYSNAMSHORT, TOPLINE, BotLnWithMonth,
                    BOTMLINE, MIDLINE, ANYLINE : LNTYP;
    BLANKSTR2,YEARSTR,FNameStr,BlankStr3       : STR10TYP;
    RETCOD   : CARDINAL;
    INTVAL,JULDATE   : INTEGER;
    TOKEN    : BUFTYP;
    TKNSTATE : FSATYP;


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

PROCEDURE DoMainRtn;
VAR I,J,W : CARDINAL;
    mn : MNEnum;

BEGIN
    FOR mn := JAN TO DCM DO
      FOR I := 1 TO 6 DO
        FOR J := 1 TO 7 DO
          MONTH[mn,I,J] := BLANKSTR2;
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

    FOR mn := JAN TO DCM DO DATEASSIGN(mn); END(*FOR*);


(* Write one page calendar *)
      WriteString(BLANK35);
      WriteString(YEARSTR);
      WriteLn;
      WriteLn;
      
(* off by 1 error as enum types are zero origin, month numbers are 1 origin *)      
(*      month := 12;  debugging sttmnt only *)
      MN2 := FEB;
      IF (month >= 3) AND (month <= 11) THEN
        MN2 := VAL(MNEnum,ORD(month-1));
      ELSIF month = 12 THEN 
        MN2 := NOV;
      END (*if month*);

      MN  := VAL(MNEnum,(ORD(MN2) - 1));
      MN3 := VAL(MNEnum,(ORD(MN2) + 1));
      WriteString(MONNAMLONG[MN]);
      WriteString(MONNAMLONG[MN2]);
      WriteString(MONNAMLONG[MN3]);
      WriteLn;
      WriteLn;
      WriteLn;
      WriteString(DAYSNAMSHORT);
      WriteString(DAYSNAMSHORT);
      WriteString(DAYSNAMSHORT);
      WriteLn;
      FOR W := 1 TO 6 DO
        FOR I := 1 TO 7 DO
          WriteString(BLANKCHR);
          WriteString(MONTH[MN,W,I]);
        END(*FOR I*);
        WriteString('    ');
        FOR I := 1 TO 7 DO
          WriteString(BLANKCHR);
          WriteString(MONTH[MN2,W,I]);
        END(*FOR I*);
        WriteString('    ');
        FOR I := 1 TO 7 DO
          WriteString(BLANKCHR);
          WriteString(MONTH[MN3,W,I]);
        END(*FOR I*);
        WriteLn;
      END(*FOR W*);
    WriteLn;
    WriteLn;
    WriteString(BLANK35);
    WriteString(YEARSTR);
    WriteLn;
  CALSTATE := FINI;
END DoMainRtn;


(*
********************* MAIN *********************************************
*)
BEGIN
  BLANKSTR2 := '  ';
  BlankStr3 := '| ';
  WriteString('Calendar Printing Program for Windows.  Last Update 14 Jan 07.');
  WriteLn;
  WriteLn;

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

  TIME2MDY(month,day,YEAR);
  WholeStr.CardToStr(YEAR,YEARSTR);
  DoMainRtn;
  MiscM2.PressAnyKey;
END CALScreen.
