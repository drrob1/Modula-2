MODULE LRT;
(*  Copyright (C) 1987  Robert Solomon MD.  All rights reserved. *)
(*
   REVISION HISTORY
   ----------------
   23 Feb 06 -- Converted to SBM2 using Terminal mode.
*)

FROM SYSTEM IMPORT FUNC,ADR,ADDRESS;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM LR IMPORT SEMILOGLR, SIMPLELR;
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

FROM MiscM2 IMPORT WriteCx, SelectWindow, WriteString, WriteLn, PressAnyKey, WriteLongReal,
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
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;


  VAR N,N0,N1     : CARDINAL;
      M0,B0,M1,B1 : LONGREAL;
      X0,Y0,X1,Y1 : ARRAY [1..10] OF LONGREAL;

BEGIN
(*
******************************* MAIN **********************************
*)
  N0 := 10;
  X0[1] := 0.;                      Y0[1] := 5.9;
  X0[2] := 0.9;                     Y0[2] := 5.4;
  X0[3] := 1.8;                     Y0[3] := 4.4;
  X0[4] := 2.6;                     Y0[4] := 4.6;
  X0[5] := 3.3;                     Y0[5] := 3.5;
  X0[6] := 4.4;                     Y0[6] := 3.7;
  X0[7] := 5.2;                     Y0[7] := 2.8;
  X0[8] := 6.1;                     Y0[8] := 2.8;
  X0[9] := 6.5;                     Y0[9] := 2.4;
  X0[10] := 7.4;                    Y0[10] := 1.5;

  SIMPLELR(N0,X0,Y0,M0,B0);
  WriteString(' Test of the Simple Linear Regression Algorithm.');
  WriteLn;
  WriteString(' Slope = ');
  WriteLongReal(M0,20);
  WriteString(';  Intercept = ');
  WriteLongReal(B0,20);
  WriteLn;
  WriteLn;
  PressAnyKey;

  N1 := 3;
  X1[1] := 10.;                     Y1[1] := 898.;
  X1[2] := 20.;                     Y1[2] := 871.;
  X1[3] := 40.;                     Y1[3] := 794.;
  SEMILOGLR(N1,X1,Y1,M1,B1);
  WriteString(' Test of the Semi-Log Linear Regression Algorithm.');
  WriteLn;
  WriteString(' Slope = ');
  WriteLongReal(M1,20);
  WriteString(';  Intercept = ');
  WriteLongReal(B1,20);
  WriteLn;
  PressAnyKey;
  
  N0 := 9;
  X0[1] := 0.;                      Y0[1] := 39.82;
  X0[2] := 15.;                     Y0[2] := 45.34;
  X0[3] := 30.;                     Y0[3] := 34.38;
  X0[4] := 45.;                     Y0[4] := 28.93;
  X0[5] := 60.;                     Y0[5] := 30.17;
  X0[6] := 75.;                     Y0[6] := 19.84;
  X0[7] := 90.;                     Y0[7] := 16.02;
  X0[8] := 105.;                    Y0[8] := 13.26;
  X0[9] := 120.;                    Y0[9] := 9.86;

  SEMILOGLR(N0,X0,Y0,M0,B0);
  WriteString(' Semi-Log Linear Regression Algorithm using Gastric Emptying min & KCount data.');
  WriteLn;
  WriteString(' Slope = ');
  WriteLongReal(M0,20);
  WriteString(';  Intercept = ');
  WriteLongReal(B0,20);
  WriteLn;
  PressAnyKey;
  
  N0 := 8;
  X0[1] := 15.;                     Y0[1] := 45.34;
  X0[2] := 30.;                     Y0[2] := 34.38;
  X0[3] := 45.;                     Y0[3] := 28.93;
  X0[4] := 60.;                     Y0[4] := 30.17;
  X0[5] := 75.;                     Y0[5] := 19.84;
  X0[6] := 90.;                     Y0[6] := 16.02;
  X0[7] := 105.;                    Y0[7] := 13.26;
  X0[8] := 120.;                    Y0[8] := 9.86;

  SEMILOGLR(N0,X0,Y0,M0,B0);
  WriteString(' Semi-Log Linear Regression Algorithm using Gastric Emptying min & KCount data removing 1 point.');
  WriteLn;
  WriteString(' Slope = ');
  WriteLongReal(M0,20);
  WriteString(';  Intercept = ');
  WriteLongReal(B0,20);
  WriteLn;
  PressAnyKey;  
END LRT.