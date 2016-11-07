MODULE SolveMatrixFromFile2;
(*
  REVISION
  --------
   2 Mar 05 -- Added prompts to remind me of the file format.
   3 Mar 05 -- Made version 2 write lines like eqn w/o =.
*)

FROM SYSTEM IMPORT FUNC;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM Mat IMPORT
    (* proc *)  Zero, Write, Add, Sub, Mul,
                Random, Solve, GaussJ, Invert, Eigenvalues;
(*
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
*)
FROM MiscM2 IMPORT WriteCx, SelectWindow, WriteString, WriteLn, PressAnyKey,
                   ReadCard, WriteCard, Error, CLS;

IMPORT Terminal, BasicDialogs, DlgShell;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
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
(****************************************************************************)

  FROM Terminal IMPORT Read, (*WriteString, WriteLn, Write, *) ReadChar, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

CONST MaxN = 6;
      MenuSep = '|';

TYPE MaxRealArray = ARRAY [1..MaxN],[1..MaxN] OF LONGREAL;

VAR
  ch1,ch2,ch3 :  CHAR;
  bool,inputprocessed :  BOOLEAN;
  sigfig,c1,c2,c3,N,M :  CARDINAL;
  inputline,OpenFileName,str1,str2,str3,str4,str5,str6,str7,str8,filter,str0 : STRTYP;
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
    Reset;
    OpenFileName := '';

    filter := 'Text Files';
    Strings.Append(MenuSep,filter);
    Strings.Append('*.txt',filter);
    Strings.Append(MenuSep,filter);
    Strings.Append('All',filter);
    Strings.Append(MenuSep,filter);
    Strings.Append('*',filter);
    Strings.Append(MenuSep,filter);
(*
    WriteString(' Solves vector equation A*X = B; A is a square coef matrix.');
    WriteLn;
    WriteString(' Line 1 is N');
    WriteLn;
    WriteString(' Line 2..N is rows of coefficient matrix w/ B values. ');
    WriteLn;
    PressAnyKey;
*)
    longstr :=     'Solves vector equation A*X = B; A is a square coef matrix.  Line 1 is';
    Strings.Append(ASCII.cr,longstr);
    Strings.Append(ASCII.lf,longstr);
    Strings.Append('N, lines 2..N are rows of coef matrix and B values on same line.',longstr);
    BasicDialogs.MessageBox(longstr,MsgInfo);
    c3 := 1;
    DlgShell.ConvertToNulls(MenuSep,filter);
    bool := BasicDialogs.PromptOpenFile(OpenFileName,filter,c3,'','',
                                         'Open matrix values as a text file',FALSE);
(*
    c3 := 0;
    bool := BasicDialogs.PromptOpenFile(OpenFileName,'',c3,'','',
                                         'Open matrix values as a text file',FALSE);
*)
    IF NOT bool THEN
      Error('Could not open file.  Does it exist?');
      HALT;
    END;
(*    BasicDialogs.MessageBox(OpenFileName,MsgInfo); *)
(*
    N := 3;
    FUNC BasicDialogs.PromptCard('Input number of unknowns',1,10,FALSE,N);
    WriteString('N=');
    WriteCard(N);
    WriteLn;
    WriteString('Test my own readcard proc.  Enter card :');
    ReadCard(c1);
    WriteLn;
    WriteString('My own readcard proc says card=');
    WriteCard(c1);
    WriteLn;
    PressAnyKey;
*)
    ASSIGN2BUF(OpenFileName,mybuf);
    FOPEN(InFile,mybuf,RD);
    FRDTXLN(InFile,inputbuf,80,bool);
(*
    WriteString('First line of file ');
    WriteString(OpenFileName);
    WriteString(' is:');
    WriteLn;
    WriteString(inputbuf.CHARS);
    WriteLn;
*)

    INI1TKN(tpv1,inputbuf);
    GETTKN(tpv1,token,tknstate,i,retcod);
    N := i;
    WriteString(' N = ');
    WriteCard(N);
    WriteLn;

    FOR row := 1 TO N DO
        FRDTXLN(InFile,inputbuf,80,bool);
        IF bool THEN
          Error('Reading from file and got an unexpected EOF.');
          WriteLn;
          HALT;
        END;
        INI1TKN(tpv1,inputbuf);
        FOR col := 1 TO N+1 DO
          GETTKNREAL(tpv1,token,tknstate,i,r,retcod);
          IF retcod > 0 THEN
            Error('Parsing file for matrix loop.  GETTKNREAL Retcod > 0');
            WriteLn;
            HALT;
          END;
          IM[row,col] := r;  (* IM is Input Matrix *)
        END (*for col*);
    END (*for row*);

(* Now need to create A and B matrices *)
  FOR row := 1 TO N DO
        FOR col := 1 TO N DO
                ra1 [row,col] := IM [row,col];
        END;
        ra2 [row,1] := IM [row, N+1];
  END;

    WriteString(' coef matrix is:');
    WriteLn;
    Write(ra1,N,N,3);
    WriteLn;
    WriteString(' Right hand side vector matrix is:');
    WriteLn;
    Write(ra2,N,1,3);
    WriteLn;

(*    PressAnyKey; *)

(*    CLS; *)

    Solve (ra1, ra2, ans, N, 1);

    WriteString ("The solution X to AX = B is");  WriteLn;
    Write (ans, N, 1, 3);
    PressAnyKey;

(* Check that the solution looks right. *)

    Mul (ra1, ans, N, N, 1, ra3);
    Sub (ra3, ra2, N, 1, ra4);
    WriteString ("As a check, AX-B evaluates to");  WriteLn;
    Write (ra4, N, 1, 4);

    PressAnyKey;
    IF tpv1 # NIL THEN DISPOSE(tpv1); END;
    FCLOSE(InFile);
END SolveMatrixFromFile2.
