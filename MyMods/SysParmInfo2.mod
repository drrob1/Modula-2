MODULE SysParmInfo2;
(*
  REVISION HISTORY
  ----------------
  31 Jan 06 -- Made this SysParmInfo2 and added power params.
*)

FROM SYSTEM IMPORT FUNC,ADR,ADDRESS;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM TextWindows IMPORT
    (* TYPES & CONSTS *)
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, WinAttr,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo,
    (* VARS *)
    (* PROCS *)
    ComposeAttribute, CreateWindow, WindowTypes, SpecialKeys,
    GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, (* WriteString, *)
    WriteStringAt, WriteCellsAt, WriteCells, (* WriteLn,*) EraseToEOL, ChangeAttr,
    ReadBufferString, RepaintRect, RepaintScreen, PaintOff, PaintOn,
    SetAutoScroll, WinShellToTextWindowMessage,
    MakeRowVisible, IsRectVisible, MakeRectVisible, GetVisibleRect,
    GetBufferRect, EraseScreen, EraseRect, GetWinShellHandle, FindTextWindow,
    SetDisplayMode,GetDisplayMode,SetWindowEnable,
    IsMinimized, IsMaximized, SetWindowTitle, SendUserMessage, PostUserMessage,
    IsUserMessageWaiting,AddVScrollBar, AddHScrollBar, AddScrollBars,
    SetScrollBarPos, SetWindowData, SetWindowDataNum, GetWindowData, GetWindowDataNum,
    GetWindowSize, SetWindowSize, GetWindowPos, SetWindowPos, CascadeWindow,
    SetWindowIsBusy, GetWindowDisplayInfo, SetWindowDisplayInfo,
    SetScrollDisableWhenNone, SetActiveTabChild, SetTabChildPosition,
    GetForegroundWindow, SetForegroundWindow,
    SetTimer, KillTimer, DisplayHelp,
    Xpos, Ypos, Xorg, Yorg, Xmax, Ymax;

FROM MiscM2 IMPORT WriteCx, SelectWindow, WriteString, WriteLn, PressAnyKey,
                   ReadCard, WriteCard, Error, CLS;

IMPORT Terminal, BasicDialogs;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv,WINUSER;
IMPORT LongMath;
IMPORT ASCII;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;

  FROM Environment IMPORT GetCommandLine;
  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRBL,FWRSTR,FWRLN,FAPPEND;
(****************************************************************************)

  FROM Terminal IMPORT Read, (*WriteString, WriteLn, Write, *) ReadChar, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

CONST 

TYPE boolptr = POINTER TO BOOLEAN;

VAR
  ch1,ch2,ch3 :  CHAR;
  bool,inputprocessed : BOOLEAN;
  boolp,boolp2,boolp3 : boolptr;
  sigfig,c1,c2,c3,N   : CARDINAL;
  inputline,OpenFileName,str1,str2,str3,str4,str5,str6,str7,str8,str9,str0 : STRTYP;
  longstr     : ARRAY [0..5120] OF CHAR;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf    : BUFTYP;
  r           : LONGREAL;
  L           : LONGINT;
  LC          : LONGCARD;
  InFile      : MYFILTYP;
  mybuf,token : BUFTYP;
  tknstate    : FSATYP;
  c,retcod,row,col,SSTimeOut,PowerTimeOut : CARDINAL;
  i           : LONGINT;

(*
PROCEDURE SystemParametersInfoW(uiAction : UINT;
                                uiParam : UINT;
                                pvParam : PVOID;
                                fWinIni : UINT) : BOOL;

*)

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    NEW(boolp);
    NEW(boolp2);
    Reset;
    bool := WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVETIMEOUT,c,ADR(SSTimeOut),c);
    WriteString(' Screen Saver TimeOut: ');
    WriteCard(SSTimeOut);
    WriteLn;
    bool := WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVEACTIVE,c,boolp,c);
    WriteString(' Screen Save Active enabled:');
    IF boolp^ THEN
      WriteString(' true.');
    ELSE
    	WriteString(' false.');
    END;
    WriteLn;
    WINUSER.SystemParametersInfo(WINUSER.SPI_SETSCREENSAVEACTIVE,ORD(FALSE),boolp,c);
    bool := WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVEACTIVE,c,boolp,c);
    WriteString(' After set to false.  Now Screen Save Active is:');
    IF boolp^ THEN
      WriteString(' true.');
    ELSE
    	WriteString(' false.');
    END;
    WriteLn;
    
    WINUSER.SystemParametersInfo(WINUSER.SPI_SETSCREENSAVEACTIVE,ORD(TRUE),boolp,c);
    bool := WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVEACTIVE,c,boolp,c);
    WriteString(' After set to true.  Now Screen Save Active is:');
    IF boolp^ THEN
      WriteString(' true.');
    ELSE
    	WriteString(' false.');
    END;
    WriteLn;


    PressAnyKey;

    WINUSER.SystemParametersInfo(WINUSER.SPI_GETSCREENSAVERRUNNING,c,boolp,c);
    WriteString(' Screen Saver is');
    IF boolp^ THEN
      WriteString(' running.');
    ELSE
    	WriteString(' not running.');
    END;
    WriteLn;
    PressAnyKey;

    WINUSER.SystemParametersInfo(WINUSER.SPI_GETPOWEROFFACTIVE,c,boolp2,c);
    WriteString(' Screen Power off is');
    IF boolp2^ THEN
    	WriteString(' enabled.');
    ELSE
    	WriteString(' disabled.');
    END;
    WriteLn;
    
    WINUSER.SystemParametersInfo(WINUSER.SPI_GETPOWEROFFTIMEOUT,c,ADR(PowerTimeOut),c);
    WriteString(' Power Off timeout is ');
    WriteCard(PowerTimeOut);
    WriteLn;
    
    WINUSER.SystemParametersInfo(WINUSER.SPI_GETLOWPOWERACTIVE,c,boolp2,c);
    WriteString(' Screen low power mode is');
    IF boolp2^ THEN
    	WriteString(' en');
    ELSE
    	WriteString(' dis');
    END;
    WriteString('abled.');
    WriteLn;
    
    WINUSER.SystemParametersInfo(WINUSER.SPI_GETLOWPOWERTIMEOUT,c,ADR(PowerTimeOut),c);
    WriteString(' Low Power timeout is ');
    WriteCard(PowerTimeOut);
    WriteLn;
    
    PressAnyKey;
    
    
    DISPOSE(boolp);
    DISPOSE(boolp2);
END SysParmInfo2.
