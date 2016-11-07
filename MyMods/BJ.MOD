MODULE BJ;
(*
  REVISION HISTORY
  ----------------
  31 Oct 95 -- Changed outputing of Status and added a WriteLn before WRHAND.
  30 Nov 95 -- Changed it so cannot hit after split aces.
   8 Mar 05 -- Win32 version using SBM2 build 31.
*)
  FROM SYSTEM IMPORT ADR,ADDRESS,FUNC;
(*
  IMPORT RTSTypes;
  FROM RTSTerm IMPORT Terminate;
  IMPORT Break;
  IMPORT MED;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM LongIO IMPORT ReadLongInt, WriteLongInt;
  IMPORT Terminal;
  FROM Terminal IMPORT ReadString;
  FROM Random IMPORT RandomInit,RandomCard,RandomInt,RandomReal,Randomize;
*)
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
                   ReadCard, ReadString, ReadLongReal, WriteReal, WriteLongReal,
                   WriteCard, WriteRJCard, WriteInt, Error, CLS;
FROM MyFIO IMPORT DRIVESEP,SUBDIRSEP,MYFILTYP,IOSTATE,FRESET,FCLOSE,
    FRDTXLN,FWRTXLN,FWRTX,RETBLKBUF,EXTRACTDRVPTH,FWRLN,
    FWRSTR,GETFNM;

IMPORT Terminal, BasicDialogs, DlgShell;
FROM BasicDialogs IMPORT MessageTypes;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,MAXCARDFNT,
(*  FROM UL2 IMPORT *) COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
FROM MYRAND IMPORT RANDOMIZE,RANDINIT,RANDCARD,RANDINT,RANDREAL;
FROM STRATEGY IMPORT OptionEnum,OptionEnumRow,Strategy,SoftStrategy,
    PairStrategy,StratErrorFlag,OptionName,GETSTRATEGIES,PUTSTRATEGIES;
(* Imported from STRATEGY
CONST
  OptionName : ARRAY [0..4] OF STR10TYP = ['S  ','H  ','D  ','SP ','SUR'];
TYPE
  OptionEnum = (S,H,D,SP,SUR); /* Stand, Hit, Double, Split, Surrender */
  OptionEnumRow = ARRAY [1..10] OF OptionEnum;
VAR
  Strategy : ARRAY [5..21] OF OptionEnumRow;
  SoftStrategy : ARRAY [2..11] OF OptionEnumRow;
  PairStrategy : ARRAY [1..10] OF OptionEnumRow;
*)

CONST
  ORDZERO = ORD('0');
  ACE = 1;
  MaxNumOfPlayers = 7;
  MaxNumOfHands = 1000000;
  MaxNumOfRuns = 20;
  HandsPerPlayer = 7;
  NumOfDecks = 8;
  NumOfCards = 52*NumOfDecks;
  BoolName : ARRAY [0..1] OF STR10TYP = {' FALSE','  TRUE'};
  StatusName: ARRAY[0..8] OF STR10TYP = {'  LOST','  PUSHED','  WON',
    '  SURREND','  LOSTDBL','  WONDBL','  LOST2BJ','  PUSHEDBJ','  WONBJ'};

TYPE
  StatusEnum = (LOST,PUSHED,WON,SURREND,LOSTDBL,WONDBL,LOST2BJ,PUSHEDBJ,
                WONBJ);
  HANDTYP = RECORD
    CARD1,CARD2,TOTAL : CARDINAL;
    DOUBLED, SURRENDERED, BUSTED, BLACKJACK, SOFT : BOOLEAN;
    STATUS : StatusEnum;
  END (*RECORD*);

VAR
  DECK : ARRAY[1..NumOfCards] OF CARDINAL;
  ReSplitAcesFlag,LastHandWinLoss,TimeToShuffleFlag : BOOLEAN;
(*                                           AlreadySplitAces : BOOLEAN; *)
  PLAYER : ARRAY [1..MaxNumOfPlayers], [1..HandsPerPlayer] OF HANDTYP;
  DEALER : HANDTYP;
  SPLITS : ARRAY [1..MaxNumOfPlayers] OF CARDINAL;
  PREVSTATUS : ARRAY [1..MaxNumOfPlayers] OF StatusEnum;
  NumOfPlayers : CARDINAL;
  C,K,C1,C2,C3,CardCtr,swapped,P : CARDINAL;
  CH : CHAR;
  I,J : INTEGER;
  L,L1,L2,L3 : CARDINAL;
  Wins,Losses,Pushes,DBLWins,DBLLosses,BJWon,BJPushed,BJwDlrAce,Splits,
    Doubles,Surrenders,Busts,TotalHands   : CARDINAL;
  Score : INTEGER;
  WinsInARow,LossesInARow : CARDINAL;
  RUNS : ARRAY [1..MaxNumOfRuns] OF CARDINAL;

PROCEDURE ADDCOMMAS(VAR INOUT STR : ARRAY OF CHAR);
VAR
  C,PTR,NCOM,NDGTS,NULLPOSN : CARDINAL;
BEGIN
  NDGTS := STRLENFNT(STR);
  PTR := NDGTS - 1;  (* Zero origin array as a param *)
  NCOM := PTR DIV 3;
  NULLPOSN := NDGTS + NCOM;
  IF NULLPOSN > HIGH(STR) THEN
    WriteString(' Cannot add commas because string is too small.');
    WriteLn;
    RETURN;
  END(*IF*);
  WHILE NCOM > 0 DO
    FOR C := 1 TO 3 DO
      STR[PTR+NCOM] := STR[PTR];
      DEC(PTR);
    END(*FOR*);
    STR[PTR+NCOM] := ',';
    DEC(NCOM);
  END(*WHILE*);
  IF NULLPOSN < HIGH(STR) THEN STR[NULLPOSN] := 0C; END(*IF*);
END ADDCOMMAS;

PROCEDURE SHUFFLE(NTIMES:CARDINAL);
VAR C,K,c: CARDINAL;

BEGIN (* SHUFFLE *)
  FOR c := 1 TO NTIMES DO
(*
  Shuffle the array - pass *once* through the array, swapping each element
  with another, randomly chosen, element.
*)
    FOR C := NumOfCards TO 2 BY -1 DO
(* swap element in ith place with any element at or below that place *)
      K := RANDCARD(C)+1;
      swapped := DECK[C];
      DECK[C] := DECK[K];
      DECK[K] := swapped;
    END(*FOR*);
  END(*FOR*);
  CardCtr := 0;
END SHUFFLE;

PROCEDURE GETCARD() : CARDINAL;
BEGIN
  INC(CardCtr);
  RETURN DECK[CardCtr];
END GETCARD;

PROCEDURE DEAL;
VAR
  C1,C2,C : CARDINAL;

BEGIN
  FOR C1 := 1 TO NumOfPlayers DO
    FOR C2 := 1 TO HandsPerPlayer DO   (* Reinitialize hands per player *)
      WITH PLAYER[C1,C2] DO
        CARD1 := 0;
        CARD2 := 0;
        DOUBLED := FALSE;
        SURRENDERED := FALSE;
        BUSTED := FALSE;
        BLACKJACK := FALSE;
        SOFT := FALSE;
        STATUS := LOST;
      END(*WITH*);
    END(*FOR*);
    SPLITS[C1] := 1;
    PLAYER[C1,1].CARD1 := GETCARD();
  END(*FOR*);
  DEALER.CARD1 := GETCARD();
  DEALER.BUSTED := FALSE;
  DEALER.DOUBLED := FALSE;
  DEALER.SURRENDERED := FALSE;
  DEALER.BLACKJACK := FALSE;
  DEALER.SOFT := FALSE;
  DEALER.STATUS := LOST;
  FOR C := 1 TO NumOfPlayers DO
    PLAYER[C,1].CARD2 := GETCARD();
  END(*FOR*);
  DEALER.CARD2 := GETCARD();
END DEAL;

PROCEDURE DUMPHANDS(*P : CARDINAL*);
VAR
  P,C : CARDINAL;
BEGIN
  WriteString(
  '   Hand (card1,card2,total,  dbl,  sur, bust,   bj, soft, status) is: ');
  WriteLn;
  FOR P := 1 TO NumOfPlayers DO
(*
    WriteString(' For player [');
    WriteCard(P,0);
    WriteString(']:');
    WriteLn;
*)
    FOR C := 1 TO SPLITS[P] DO
      WriteString('P');
      WriteCard(P);
      WriteString(' Hand ');
      WriteCard(C);
      WriteString(':');
      WriteRJCard(PLAYER[P,C].CARD1,4);
      WriteRJCard(PLAYER[P,C].CARD2,6);
      WriteRJCard(PLAYER[P,C].TOTAL,6);
      WriteString(BoolName[ORD(PLAYER[P,C].DOUBLED)]);
      WriteString(BoolName[ORD(PLAYER[P,C].SURRENDERED)]);
      WriteString(BoolName[ORD(PLAYER[P,C].BUSTED)]);
      WriteString(BoolName[ORD(PLAYER[P,C].BLACKJACK)]);
      WriteString(BoolName[ORD(PLAYER[P,C].SOFT)]);
      WriteString(StatusName[ORD(PLAYER[P,C].STATUS)]);
      WriteLn;
    END(*FOR*);
  END(*FOR*);
  WriteString('   Dealer:');
  WriteRJCard(DEALER.CARD1,4);
  WriteRJCard(DEALER.CARD2,6);
  WriteRJCard(DEALER.TOTAL,6);
  WriteString(BoolName[ORD(DEALER.DOUBLED)]);
  WriteString(BoolName[ORD(DEALER.SURRENDERED)]);
  WriteString(BoolName[ORD(DEALER.BUSTED)]);
  WriteString(BoolName[ORD(DEALER.BLACKJACK)]);
  WriteString(BoolName[ORD(DEALER.SOFT)]);
  WriteLn;
END DUMPHANDS;

PROCEDURE WRHAND(HAND : HANDTYP; MSG : STRTYP);

BEGIN
  WriteString(MSG);
  WriteRJCard(HAND.CARD1,3);
  WriteRJCard(HAND.CARD2,3);
END WRHAND;

PROCEDURE HitMe(VAR h : HANDTYP);
VAR
  CARD : CARDINAL;
  Strat : OptionEnum;
BEGIN
  WITH h DO
    LOOP
      CARD := GETCARD();
      WriteRJCard(CARD,3);
      INC(TOTAL,CARD);
      SOFT := SOFT AND (TOTAL <= 11);
      IF TOTAL > 21 THEN
        BUSTED := TRUE;
        RETURN;
      ELSIF ((CARD = ACE) AND (TOTAL <= 11)) OR SOFT THEN
        SOFT := TRUE;
        Strat := SoftStrategy[TOTAL,DEALER.CARD1];
        IF Strat = S THEN
          INC(TOTAL,10);
          RETURN;
        ELSIF (Strat = H) OR (Strat = D) OR (Strat = SUR) THEN
          (* If going to take another hit, assume ACE = 1 *)
          IF TOTAL > 11 THEN SOFT := FALSE END(*IF*); (* Cannot make ACE = 11
                                                         anymore *)
        ELSE
          WriteString(' In SoftStrategy and got unexpected result.');
          WriteLn;
          DUMPHANDS(*P*);
          WriteString(' SoftStrategy(');
          WriteRJCard(TOTAL,2);
          WriteString(',');
          WriteRJCard(DEALER.CARD1,2);
          WriteString(')=');
          WriteString(OptionName[ORD(Strat)]);
          WriteLn;
        END(*IF*);
      ELSE  (* This card is not an Ace *)
(*
        IF SOFT THEN  (* Already pulled an Ace before *)
          Strat := SoftStrategy[TOTAL,DEALER.CARD1];
          IF Strat = S THEN
            INC(TOTAL,10);
            RETURN;
          ELSIF (Strat = H) OR (Strat = D) OR (Strat = SUR) THEN
            SOFT := TOTAL <= 11;
          ELSE
            WriteString(' In SoftStrategy and got unexpected result.');
            WriteLn;
            DUMPHANDS(*P*);
            WriteString(' SoftStrategy(');
            WriteRJCard(TOTAL,2);
            WriteString(',');
            WriteRJCard(DEALER.CARD1,2);
            WriteString(')=');
            WriteString(OptionName[ORD(Strat)]);
            WriteLn;
          END(*IF*);
        ELSE (* Not Soft *)
*)
          Strat := Strategy[TOTAL,DEALER.CARD1];
          IF Strat = S THEN
            RETURN;
          ELSIF (Strat = H) OR (Strat = D) OR (Strat = SUR) THEN
            (* Do nothing and loop around again *)
          ELSE
            WriteString(' In Strategy and got unexpected result.');
            WriteLn;
            DUMPHANDS(*P*);
            WriteString(' Strategy(');
            WriteRJCard(TOTAL,2);
            WriteString(',');
            WriteRJCard(DEALER.CARD1,2);
            WriteString(')=');
            WriteString(OptionName[ORD(Strat)]);
            WriteLn;
          END(*IF*);
(*      END(*IF*); *)
      END(*IF*);
    END(*LOOP*);
  END(*WITH*);
END HitMe;

PROCEDURE PLAYROUND;
VAR
  h,CARD,P : CARDINAL;
  AlreadySplitAces : BOOLEAN;

BEGIN
  IF ((DEALER.CARD1 = ACE) AND (DEALER.CARD2 = 10)) OR
                        ((DEALER.CARD2 = ACE) AND (DEALER.CARD1 = 10)) THEN
    DEALER.BLACKJACK := TRUE;
    DEALER.TOTAL := 21;
(*    check for player blackjack for statistics purposes only. *)
(*    RETURN; *)
  END(*IF*);
  AlreadySplitAces := FALSE;
  FOR P := 1 TO NumOfPlayers DO
    h := 1;
    REPEAT
      WITH PLAYER[P,h] DO
        WRHAND(DEALER,' DEALER START: ');
        WRHAND(PLAYER[P,h],' PLAYER: ');
        TOTAL := CARD1 + CARD2;
        IF (((CARD1 = ACE) AND (CARD2 = 10)) OR
              ((CARD2 = ACE) AND (CARD1 = 10))) AND NOT AlreadySplitAces THEN
          BLACKJACK := TRUE;
          TOTAL := 21;
          INC(h);
        ELSIF AlreadySplitAces THEN
          INC(TOTAL,10); (* When split aces, can't take more cards *)
          INC(h);
        ELSIF CARD1 = CARD2 THEN
          CASE PairStrategy[CARD1,DEALER.CARD1] OF
            S:  INC(h);
          | H:  HitMe(PLAYER[P,h]);
                INC(h);
          | D:  DOUBLED := TRUE;
                CARD := GETCARD();
                WriteRJCard(CARD,3);
                IF (CARD = ACE) AND (TOTAL < 11) THEN
                  INC(TOTAL,11);
                ELSE
                  INC(TOTAL,CARD);
                END(*IF*);
                BUSTED := TOTAL > 21;
                INC(h);
          | SP: IF (CARD1=ACE) AND AlreadySplitAces THEN
                  SOFT := TRUE;  (* Cannot come here now, left here if *)
                  HitMe(PLAYER[P,h]);   (* rules change.               *)
                  INC(h);
                ELSE
                  AlreadySplitAces := CARD1=ACE;
                  INC(SPLITS[P]);
                  INC(Splits);
                  PLAYER[P,SPLITS[P]].CARD1 := CARD2;
                  CARD2 := GETCARD();
                  PLAYER[P,SPLITS[P]].CARD2 := GETCARD();
(* Don't increment H pointer so will pass thru these split hands again *)
                END(*IF*);
          |SUR: SURRENDERED := TRUE;
                INC(h);
          END(*CASE*);
        ELSIF (CARD1 = ACE) OR (CARD2 = ACE) THEN (* Have soft hand *)
          CASE SoftStrategy[TOTAL,DEALER.CARD1] OF
            S:  INC(TOTAL,10);
                INC(h);
          | H:  SOFT := TRUE;
                HitMe(PLAYER[P,h]);
                INC(h);
          | D:  DOUBLED := TRUE;
                SOFT := TRUE;
                CARD := GETCARD();
                WriteRJCard(CARD,3);
                IF (CARD = ACE) AND (TOTAL < 11) THEN
                  INC(TOTAL,11);
                ELSE
                  INC(TOTAL,CARD);
                END(*IF*);
                IF SOFT AND (TOTAL <= 11) THEN
                  INC(TOTAL,10);
                END(*IF*);
                BUSTED := TOTAL > 21;
                INC(h);
          ELSE
            WriteString(' In SoftStrategy and got unexpected result.');
            WriteLn;
            DUMPHANDS(*P*);
            WriteString(' SoftStrategy(');
            WriteRJCard(CARD+1,2);
            WriteString(',');
            WriteRJCard(DEALER.CARD1,2);
            WriteString(')=');
            WriteString(OptionName[ORD(SoftStrategy[CARD+1,DEALER.CARD1])]);
            WriteLn;
          END(*CASE*);
        ELSE (* Have a regular hand *)
          CASE Strategy[TOTAL,DEALER.CARD1] OF
            S:  INC(h);
          | H:  HitMe(PLAYER[P,h]);
                INC(h);
          | D:  DOUBLED := TRUE;
                CARD := GETCARD();
                WriteRJCard(CARD,3);
                IF (CARD = ACE) AND (TOTAL < 11) THEN
                  INC(TOTAL,11);
                ELSE
                  INC(TOTAL,CARD);
                END(*IF*);
                BUSTED := TOTAL > 21;
                INC(h);
          |SUR: SURRENDERED := TRUE;
                INC(h);
          ELSE
            WriteString(' In Strategy and got unexpected result.');
            WriteLn;
            DUMPHANDS(*P*);
            WriteString(' Strategy(');
            WriteRJCard(TOTAL,2);
            WriteString(',');
            WriteRJCard(DEALER.CARD1,2);
            WriteString(')=');
            WriteString(OptionName[ORD(Strategy[TOTAL,DEALER.CARD1])]);
            WriteLn;
          END(*CASE*);
        END(*IF*);
      END(*WITH*);
    UNTIL h > SPLITS[P];
    WriteLn; (* For WRHAND lists *)
  END(*FOR*);
(* Test for Dealer BJ here if want to do it at the end *)
END PLAYROUND;

PROCEDURE SHOWDOWN;
VAR
  P,HA,CARD : CARDINAL;
BEGIN
  WITH DEALER DO
    IF NOT BLACKJACK THEN
      SOFT := (CARD1=ACE) OR (CARD2=ACE);
      TOTAL := CARD1 + CARD2;
      IF SOFT AND ((TOTAL+10) >= 17) AND ((TOTAL+10) <= 21) THEN
        INC(TOTAL,10);
      END(*IF*);
(*      WriteLn; *)
      WRHAND(DEALER,' DEALER SHOWDOWN: ');
      WHILE TOTAL < 17 DO
        CARD := GETCARD();
        WriteRJCard(CARD,3);
        IF NOT SOFT THEN
          SOFT := CARD=ACE;
        END(*IF*);
        INC(TOTAL,CARD);
        IF SOFT AND ((TOTAL+10) >= 17) AND ((TOTAL+10) <= 21) THEN
          INC(TOTAL,10);
        END(*IF*);
        SOFT := TOTAL <=11;  (* No longer a soft hand if Ace can't be 11 *)
      END(*WHILE*);
      BUSTED := TOTAL>21;
    END(*IF*);
  END(*WITH*);
  WriteLn;
  FOR P := 1 TO NumOfPlayers DO
    IF DEALER.BLACKJACK THEN
      IF PLAYER[P,1].BLACKJACK THEN
        PLAYER[P,1].STATUS := PUSHEDBJ;
        INC(TotalHands);
        INC(BJPushed);
      ELSE
        SPLITS[P] := 1; (* If already split, ignore them and only lose once *)
        PLAYER[P,1].STATUS := LOST2BJ;
        INC(TotalHands);
        INC(Losses);
      END(*IF*);
    ELSE (* Dealer doesn't have BJ *)
      IF PLAYER[P,1].BLACKJACK THEN
        PLAYER[P,1].STATUS := WONBJ;
        INC(TotalHands);
        INC(BJWon);
      ELSIF PLAYER[P,1].SURRENDERED THEN
        PLAYER[P,1].STATUS := SURREND;
        INC(TotalHands);
        INC(Surrenders);
      ELSE
        FOR HA := 1 TO SPLITS[P] DO
          INC(TotalHands);
          WITH PLAYER[P,HA] DO
            IF BUSTED THEN
              STATUS := LOST;
              INC(Losses);
              INC(Busts);
            ELSIF DEALER.BUSTED THEN
              STATUS := WON;
              INC(Wins);
            ELSIF TOTAL > DEALER.TOTAL THEN
              STATUS := WON;
              INC(Wins);
            ELSIF TOTAL = DEALER.TOTAL THEN
              STATUS := PUSHED;
              INC(Pushes);
            ELSE (* TOTAL < DEALER.TOTAL *)
              STATUS := LOST;
              INC(Losses);
            END(*IF*);
            IF DOUBLED THEN
              INC(Doubles);
              CASE STATUS OF
                WON: STATUS := WONDBL;
                     INC(DBLWins);    (* Also counted once in Wins *)
              | LOST: STATUS := LOSTDBL;
                     INC(DBLLosses);  (* Also counted once in Losses *)
              ELSE (* Ignore PUSHED, but mostly don't create an error cond *)
              END(*CASE*);
            END(*IF*);
(*
            WriteString(' STATUS: ');
            WriteString(StatusName[ORD(STATUS)]);
            WriteLn;
*)
          END(*WITH*);
(*
          WriteString(' Call DUMPHANDS (Y/N): ');
          Read(CH);
          Write(CH);
          WriteLn;
          IF CAP(CH) = 'Y' THEN DUMPHANDS(*P*); END(*IF*);
*)
        END(*FOR*);
      END(*IF*);
    END(*IF*);
    IF SPLITS[P] = 1 THEN  (* Hand not split *)
      PREVSTATUS[P] := PLAYER[P,1].STATUS;
    END(*IF*);
  END(*FOR*);
END SHOWDOWN;

BEGIN (* Main Pgm *)
  Wins := 0;
  Losses := 0;
  Busts := 0;
  BJWon := 0;
  BJPushed := 0;
  Splits := 0;
  Doubles := 0;
  TotalHands := 0;
  Pushes := 0;
  DBLWins := 0;
  DBLLosses := 0;
  Surrenders := 0;
  CardCtr := 1;
  NumOfPlayers := 1;
  FUNC BasicDialogs.PromptCard('Enter number of players (1-7):',1,7,FALSE,NumOfPlayers);
(*
  WriteString(' Enter number of players (1-7): ');
  ReadCard(NumOfPlayers);
  Read(CH);
  Write(CH);
  WriteLn;
  IF (CH <= '1') OR (CH > '9') THEN
    NumOfPlayers := 1;
  ELSE
    NumOfPlayers := ORD(CH) - ORDZERO;
  END(*IF*);
*)
  REPEAT      (* Outer card initializing loop *)
    FOR C := 1 TO 10 DO   (* Init Ace thru 10 *)
      DECK[CardCtr] := C;
      INC(CardCtr);
    END(*FOR*);
    FOR C := 1 TO 3 DO    (* Init pictures *)
      DECK[CardCtr] := 10;
      INC(CardCtr);
    END(*FOR*);
  UNTIL CardCtr > NumOfCards;
(*
  WriteString(' Before Shuffle:'); WriteLn;
  FOR C := 1 TO NumOfCards DO
      WriteRJCard(DECK[C],4);
      IF C MOD 19 = 0 THEN WriteLn; END(*IF*);
  END(*FOR*);
  WriteLn;
  WriteString(' Pause (Y/N): '); Read(CH); WriteLn;
*)
  RANDOMIZE;
  ReSplitAcesFlag := FALSE;

  REPEAT
    SHUFFLE(5);
(* output for testing purposes
    WriteLn; WriteString(' After Shuffle:'); WriteLn;
    FOR C := 1 TO NumOfCards DO
        WriteRJCard(DECK[C],4);
        IF C MOD 19 = 0 THEN WriteLn; END(*IF*);
    END(*FOR*);
    WriteLn;
    WriteString(' Shuffle again (Y/N): '); Read(CH); Write(CH); WriteLn;
*)
    CH := 'N';
  UNTIL CAP(CH) = 'N';
  GETSTRATEGIES;
  PUTSTRATEGIES;
  WriteString(' Matrices OK (Y/N): '); Terminal.Read(CH); Terminal.Write(CH); WriteLn;
  IF CAP(CH)='N' THEN HALT END(*IF*);
  LOOP
    REPEAT
      DEAL;
      PLAYROUND;
      SHOWDOWN;
(*
      WriteString(' Call DUMPHANDS (Y/N): ');
      Read(CH);
      Write(CH);
      WriteLn;
      IF CAP(CH) = 'Y' THEN DUMPHANDS(*P*); END(*IF*);
*)
      DUMPHANDS;
      WriteString(' Another hand? (Y/N): '); Terminal.Read(CH); Terminal.Write(CH); WriteLn;
    UNTIL (CAP(CH) = 'N') OR (CardCtr >= NumOfCards*3 DIV 4);
    WriteString(' Shuffle again (Y/N): '); Terminal.Read(CH); Terminal.Write(CH); WriteLn;
    IF CAP(CH) = 'N' THEN EXIT END(*IF*);
    SHUFFLE(1);
  END(*LOOP*);
  WriteLn;
  WriteString(' Wins: ');
  WriteCard(Wins);
  WriteString(', Losses: ');
  WriteCard(Losses);
  WriteString(', Pushes: ');
  WriteCard(Pushes);
  WriteString(', Doubles: ');
  WriteCard(Doubles);
  WriteString(', DBLWins: ');
  WriteCard(DBLWins);
  WriteString(', DBLLosses: ');
  WriteCard(DBLLosses);
  WriteLn;
  WriteString(' BJWon: ');
  WriteCard(BJWon);
  WriteString(', BJPushed: ');
  WriteCard(BJPushed);
  WriteString(', Splits: ');
  WriteCard(Splits);
  WriteString(', Busts: ');
  WriteCard(Busts);
  WriteString(', Surrenders: ');
  WriteCard(Surrenders);
  WriteString(', TotalHands: ');
  WriteCard(TotalHands);
  WriteLn;
  Score := INT(2*DBLWins) + INT(Wins) - INT(Losses) - INT(2*DBLLosses)  +  INT(BJWon*3 DIV 2)
            - INT(Surrenders DIV 2);
(*
  Score := DBLWins + Wins - Losses - DBLLosses;
  Score := Score + LONG(BJWon)*LONG(3) DIV LONG(2);
  INC(Score,BJWon*3 DIV 2);
*)
  WriteString(' Score: ');
  WriteInt(Score);
  WriteLn;
  PressAnyKey;

END BJ.
