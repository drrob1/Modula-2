MODULE GastricEmptying;
(*
  REVISION
  --------
  24 Feb 06 -- Added logic to excluded lines w/o numbers.
  25 Sep 07 -- Decided to write to std out for output only
   3 Oct 08 -- Added decay correction for Tc-99m
  12 Sep 11 -- Added my new FilePicker module.
*)

FROM SYSTEM IMPORT FUNC;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM LR IMPORT SEMILOGLR, SIMPLELR;
IMPORT LongMath;
FROM LongMath IMPORT ln,exp;
IMPORT STextIO;
(* from Def Mod STextIO.  Remember to link as a console ap if use std i/o.
         FROM STextIO IMPORT WriteChar, WriteLn, WriteString;
         PROCEDURE WriteChar(ch : CHAR);
         PROCEDURE WriteLn;
         PROCEDURE WriteString(s : ARRAY OF CHAR);
*)
(*  No longer need this, as it is included in MiscStdInOut
    FROM STextIO IMPORT WriteString, WriteLn;
*)

FROM MiscStdInOut IMPORT WriteCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
    WriteReal, WriteLongReal, ReadString, ReadCard, ReadLongReal;

FROM Mat IMPORT
    (* proc *)  Zero, Write, Add, Sub, Mul,
                Random, Solve, GaussJ, Invert, Eigenvalues;
(*
FROM TextWindows IMPORT
    /* TYPES & CONSTS */
    TextWindow, Colors, TextWindowsMsg, TextWindowProcedure,
    NormalFont, BoldFont, ItalicFont, WinAttr,
    DisplayModes, ScreenAttribute, CaretTypes,
    TWMessageRec, ResponseType, CloseModes, CloseWindow, NormalWindow,
    FontWeights, DefaultFontInfo, COORDINATE, WindowDisplayInfo,
    /* VARS */
    /* PROCS */
    ComposeAttribute, CreateWindow, WindowTypes, SpecialKeys,
    GetClientSize, SetClientSize, SnapWindowToFont, SetScrollRangeAllowed,
    MoveCaretTo, GetCaretPos, CaretOn, CaretOff, ShowCaret, HideCaret, SetCaretType,
    IsCaretVisible, MakeCaretVisible, PutStringAt, PutAttrAt, /* WriteString, */
    WriteStringAt, WriteCellsAt, WriteCells, /* WriteLn,*/ EraseToEOL, ChangeAttr,
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
*)

IMPORT Terminal, BasicDialogs, DlgShell;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT ASCII;
FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;

FROM Environment IMPORT GetCommandLine;
FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE;
FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FOPEN,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRBL,FWRSTR,FWRLN,FAPPEND;
FROM FilePicker IMPORT FileNamePicker;
FROM FileFunc IMPORT NameString;
(****************************************************************************)

(*  FROM Terminal IMPORT Read, WriteString, WriteLn, Write, ReadChar, Reset; *)
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

CONST MaxN = 500;
	    MaxCol = 10;
      MenuSep = '|';
      StomIcon32 = '#100';
      StomIcon16 = '#200';

TYPE MaxRealArray = ARRAY [1..MaxN],[1..MaxCol] OF LONGREAL;
	   aRealArray   = ARRAY [1..MaxN] OF LONGREAL;

VAR
  ch1,ch2,ch3 :  CHAR;
  bool,inputprocessed :  BOOLEAN;
  sigfig,c1,c2,c3,N,M :  CARDINAL;
  inputline,OpenFileName,str1,str2,str3,str4,str5,str6,str7,str8,filter,str0 : STRTYP;
  ns, InFileName      : NameString;
  longstr     : ARRAY [0..5120] OF CHAR;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf    : BUFTYP;
  r           : LONGREAL;
  L           : LONGINT;
  LC          : LONGCARD;
  InFile      : MYFILTYP;
  mybuf,token : BUFTYP;
  tknstate    : FSATYP;
  c,retcod,row,col    : CARDINAL;
  i           : INTEGER;
  ra1,ra2,ra3,ra4,IM,ans : MaxRealArray;
  tpv1        : TKNPTRTYP;
  X,Y,DecayCorY : aRealArray;
  lambda1,intercept1,ln2,Thalf1,lambda2,intercept2,Thalf2 : LONGREAL;


(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
(*
  PROCEDURE PromptOpenFile(VAR INOUT name : ARRAY OF CHAR;
                         filters : ARRAY OF CHAR;
                         VAR INOUT defFilter : CARDINAL;
                         defDir : ARRAY OF CHAR;
                         defExt : ARRAY OF CHAR;
                         title : ARRAY OF CHAR;
                         createable : BOOLEAN) : BOOLEAN;
*)
(* Opens an operating system common dialog for opening  a file
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

  OpenFileName := '';

  FOR row := 1 TO MaxN DO
    FOR col := 1 TO MaxCol DO
      IM[row,col] := 0.;
    END
  END;

  FOR c := 1 TO MaxN DO
  	X[c] := 0.;
  	Y[c] := 0.;
  END;
  ln2 := ln(2.);
  longstr :=     'Determines T-1/2 of min vs kcounts using semilogLR.  N is';
  Strings.Append(ASCII.cr,longstr);
  Strings.Append(ASCII.lf,longstr);
  Strings.Append('determined by number of lines.',longstr);
  BasicDialogs.MessageBox(longstr,MsgInfo);

  filter := 'Text Files';
  Strings.Append(MenuSep,filter);
  Strings.Append('*.txt',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('All',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('*',filter);
  Strings.Append(MenuSep,filter);
  c3 := 1;
  DlgShell.ConvertToNulls(MenuSep,filter);
(*
  bool := BasicDialogs.PromptOpenFile(OpenFileName,filter,c3,'','',
                                       'Open min and kcount values as a text file',FALSE);
  IF NOT bool THEN
    WriteString('Could not open file.  Does it exist?');
    HALT;
  END;
*)
  FileNamePicker(InFileName);
  ASSIGN2BUF(InFileName,mybuf);
  FOPEN(InFile,mybuf,RD);
  N := 0;
  LOOP   (* read, count and process lines *)
    WHILE N < MaxN DO
        FRDTXLN(InFile,inputbuf,80,bool);
        IF bool THEN
          EXIT;
        END;
        INI1TKN(tpv1,inputbuf);
        INC(N);
        col := 1;
        REPEAT
          GETTKNREAL(tpv1,token,tknstate,i,r,retcod);
          IF (retcod = 0) AND (tknstate = DGT) THEN
            IM[N,col] := r;  (* IM is Input Matrix *)
            INC(col);
          END;
        UNTIL (retcod > 0) OR (col > MaxCol);
        IF col <= 2 THEN (* not enough numbers found on line, like if line is text only *)
        	DEC(N);
        END;
    END (*while N *);
  END; (* reading loop *)
(* Now need to create A and B matrices *)
  FOR c := 1 TO N DO
        X[c] := IM[c,1];
        Y[c] := IM[c,2];
  END;

  FOR c := 1 TO N DO 
  	DecayCorY[c] := Y[c]/(exp(-X[c]/360.6))  (* halflife Tc-99m in minutes *)
  END;

  WriteString(' N = ');
  WriteCard(N);
  WriteLn;
  WriteString(' X is time(min) and Y is kcounts :');
  WriteLn;
  FOR c := 1 TO N DO
  	WriteLongReal(X[c],5);
  	WriteString('         ');
  	WriteLongReal(Y[c],5);
  	WriteString('         ');
  	WriteLongReal(DecayCorY[c],5);
  	WriteLn;
  END;
  WriteLn;

(*  PressAnyKey; *)
(*  CLS;         *)
  SEMILOGLR(N,X,Y,lambda1,intercept1);
  Thalf1 := -ln2/lambda1;
  SEMILOGLR(N,X,DecayCorY,lambda2,intercept2);
  Thalf2 := -ln2/lambda2;
  WriteString(' Uncorrected T-1/2 is ');
  WriteLongReal(Thalf1,4);
  WriteString(' minutes.  Corrected T-1/2 is ');
  WriteLongReal(Thalf2,4);
  WriteString(' minutes.');
  WriteLn;
(*
  WriteString(' uncorrected T-1/2 is ');
  WriteLongReal(Thalf1,4);
  WriteString(' minutes, lambda is ');
  WriteLongReal(-lambda1,6);
  WriteString(' reciprocal minutes.');
  WriteLn;
  WriteString(' intercept is ');
  WriteLongReal(intercept1,6);
  WriteString(' kcounts.');
  WriteLn;
  WriteString(' Decay corrected T-1/2 is ');
  WriteLongReal(Thalf2,4);
  WriteString(' minutes, lambda is ');
  WriteLongReal(-lambda2,6);
  WriteString(' reciprocal minutes.');
  WriteLn;
  WriteString(' intercept is ');
  WriteLongReal(intercept2,6);
  WriteString(' kcounts.');
  WriteLn;
*)
  IF tpv1 # NIL THEN DISPOSE(tpv1); END;
  FCLOSE(InFile);
END GastricEmptying.
