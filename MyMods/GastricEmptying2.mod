MODULE GastricEmptying2;
(*
  REVISION
  --------
  24 Feb 06 -- Added logic to excluded lines w/o numbers.
  25 Sep 07 -- Decided to write to std out for output only
   3 Oct 08 -- Added decay correction for Tc-99m
  12 Sep 11 -- Added my new FilePicker module.
  26 Dec 14 -- Will change to the FilePickerBase module as I now find it easier to use.  Copied from vlc.mod.
                 But these use terminal mode.
  27 Dec 14 -- Removing unused modules so can compile in ADW.
  21 Jun 15 -- Will write out to file, and close all files before program closes.
*)

(*
  8/28/2015
  Normal values from source that I don't remember anymore.
  1 hr should have 90% of activity remaining in stomach = 6.6 hr halflife = 395 min halflife
  2 hr should have 60% of activity remaining in stomach = 2.7 hr halflife = 163 min halflife
  3 hr should have 30% of activity remaining in stomach = 1.7 hr halflife = 104 min halflife
  4 hr should have 10% of activity remaining in stomach = 1.2 hr halflife = 72 min halflife

*)


FROM SYSTEM IMPORT FUNC,CAST;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM LR IMPORT SEMILOGLR, SIMPLELR;
IMPORT WINUSER;
IMPORT LongMath;
FROM LongMath IMPORT ln,exp;
IMPORT STextIO;
IMPORT MiscM2;
FROM MiscM2 IMPORT WriteCard, CLS, WriteString, WriteLn, PressAnyKey, Error, WriteInt,
    WriteReal, WriteLongReal, ReadString, ReadCard, ReadLongReal;

IMPORT Terminal;
FROM Terminal IMPORT Position;
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
FROM FilePickerBase IMPORT CountOfEntries, SetFilenamePattern, GetNextFilename, GetPrevFilename;
FROM FileFunc IMPORT NameString,FileExists;
FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
(****************************************************************************)

(*
                                FROM Terminal IMPORT Read, WriteString, WriteLn, Write, ReadChar, Reset;
*)

CONST MaxN = 500;
      MaxCol = 10;
      SigFig = 2;
      GastricPattern = "gastric*.txt";
      MenuSep = '|';
      StomIcon32 = '#100';
      StomIcon16 = '#200';
      blankline =
      "                                                              ";
      sepline =
      "--------------------------------------------------------------";

TYPE MaxRealArray = ARRAY [1..MaxN],[1..MaxCol] OF LONGREAL;
           aRealArray   = ARRAY [1..MaxN] OF LONGREAL;

VAR
  ch,ch1,ch2,ch3 :  CHAR;
  bool,inputprocessed :  BOOLEAN;
  sigfig,c1,c2,c3,N,M,max,ctr,len :  CARDINAL;
  inputline,OpenFileName,str1,str2,str3,str4,str5,str6,str7,str8,filter,str0,
  Thalf1strFixed,Thalf2strFixed,Thalf1gen,Thalf2gen : STRTYP;
  ns, infilename, outfilename, DirEntry : NameString;
  longstr     : ARRAY [0..5120] OF CHAR;
  InputPromptLen, LastModLen : CARDINAL;
  inputbuf    : BUFTYP;
  mybuf,token : BUFTYP;
  r           : LONGREAL;
  L           : LONGINT;
  LC          : LONGCARD;
  InFile,OutFile : MYFILTYP;
  tknstate    : FSATYP;
  c,retcod,row,col    : CARDINAL;
  i           : INTEGER;
  ra1,ra2,ra3,ra4,IM,ans : MaxRealArray;
  tpv1        : TKNPTRTYP;
  X,Y,DecayCorY : aRealArray;
  lambda1,intercept1,ln2,Thalf1,lambda2,intercept2,Thalf2 : LONGREAL;

PROCEDURE CheckPattern(VAR ns: NameString);
VAR
  c,len : CARDINAL;
  FoundStar : BOOLEAN;

BEGIN
  FoundStar := FALSE;
  len := LENGTH(ns);
  c := len;
  IF len = 0 THEN
    ns := GastricPattern;
  ELSIF ns[0] = '*' THEN
    (* do nothing *)
  ELSE
    WHILE (c > 0) AND (ns[c] <> '*') DO
      DEC(c);
    END (* WHILE *);
    IF c = 0 THEN (* asterisk not found *)
      Strings.Append("*",ns);
    END (* if c=0 *);
  END (* if len=0 *);
END CheckPattern;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN

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
  tpv1 := CAST(TKNPTRTYP, NIL);

  WriteLn;
  GetCommandLine(ns);
  CheckPattern(ns);  (* if ns is empty, makes pattern gastric*.txt *)
  SetFilenamePattern(ns);

  max := CountOfEntries;
  IF max > 15 THEN max := 15 END;
  IF max > 1 THEN
    FOR ctr := 1 TO max DO
        GetNextFilename(ns,DirEntry);
        WriteString(DirEntry);
        WriteLn;
    END; (* for loop to get and display directory entries *)

    FOR ctr := 1 TO max DO
        GetPrevFilename(ns,DirEntry);
    END; (* for loop to get back to first directory entry *)
  END; (* for max not 0 or 1 *)
  WriteLn;
  WriteLn;
  Position(0,20);
  WriteString(blankline);
  WriteLn;
  Position(0,20);
  WriteString(sepline);
  WriteLn;
  WriteString(DirEntry);
  WriteLn;
  LOOP
    IF CountOfEntries = 0 THEN
      WriteString(' No valid filenames found.  Need New pattern. ');
      WriteLn;
    END;
    WriteString( '<enter> or <space> to select, n for new pattern ');
    MiscM2.ReadChar(ch);

    CASE ch OF
        Terminal.CursorUp:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetPrevFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | Terminal.Enter :
        EXIT; (* ns and DirEntry are already set *)
    | Terminal.CursorDown:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetNextFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | Terminal.PageUp:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetPrevFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | Terminal.PageDown:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetNextFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | Terminal.CursorLeft:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetPrevFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | Terminal.CursorRight:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetNextFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | Terminal.Tab:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetNextFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | Terminal.BackSpace:
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        GetPrevFilename(ns,DirEntry);
        WriteString(blankline);
        Position(0,21);
        WriteString(DirEntry);
        WriteLn;

    | ' ':
        EXIT; (* ns and DirEntry are already set *)
    | 'n','N':
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        WriteString(blankline);
        Position(0,21);
        WriteString(' Enter new pattern: ');
        ReadString(ns);
        WriteLn;
        CheckPattern(ns);
        SetFilenamePattern(ns);
        Terminal.Reset;
        max := CountOfEntries;
        IF max > 15 THEN max := 15 END;
        IF max > 0 THEN
          FOR ctr := 1 TO max DO
            GetNextFilename(ns,DirEntry);
            WriteString(DirEntry);
            WriteLn;
          END; (* for loop to get and display directory entries *)
          FOR ctr := 1 TO max DO
            GetPrevFilename(ns,DirEntry);
          END; (* for loop to get back to first directory entry *)
        END; (* for max not 0 or 1 *)
        WriteLn;
        WriteLn;
        Position(0,20);
        WriteString(blankline);
        WriteLn;
        Position(0,20);
        WriteString(sepline);
        WriteLn;
        WriteString(DirEntry);
        WriteLn;

    | Terminal.Escape:
        HALT;

    ELSE
        (* ignore the character.  *)
    END; (* case ch *)

  END; (* loop to read and process a char *)
  WriteLn;
  WriteLn;
  WriteString(' Picked File Name is ');
  WriteString(ns);
  WriteLn;

  infilename := ns;
  IF NOT FileExists(infilename) THEN
    MiscM2.Error(' Could not find input file.  Does it exist?');
    HALT;
  END(*if*);

  outfilename := infilename;
  Strings.Append(".out",outfilename);

  ASSIGN2BUF(infilename,mybuf);
  FOPEN(InFile,mybuf,RD);

  ASSIGN2BUF(outfilename,mybuf);
  FOPEN(OutFile,mybuf,APND);
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
  LongStr.RealToFixed(Thalf1,SigFig,Thalf1strFixed);
  LongStr.RealToStr(Thalf1,Thalf1gen);

  SEMILOGLR(N,X,DecayCorY,lambda2,intercept2);
  Thalf2 := -ln2/lambda2;
  LongStr.RealToFixed(Thalf2,SigFig,Thalf2strFixed);
  LongStr.RealToStr(Thalf2,Thalf2gen);


  WriteString(' Uncorrected T-1/2 is ');
  WriteString(Thalf1strFixed);
(*                                               WriteLongReal(Thalf1,4); *)
  WriteString(' minutes.  Corrected T-1/2 is ');
  WriteString(Thalf2strFixed);
(*                                               WriteLongReal(Thalf2,4); *)
  WriteString(' minutes.');
  WriteLn;

  FWRSTR(OutFile," Uncorrected T-1/2 is ");
  FWRSTR(OutFile,Thalf1strFixed);
  FWRSTR(OutFile," (");
  FWRSTR(OutFile,Thalf1gen);
  FWRSTR(OutFile," ) minutes.  Corrected T-1/2 is ");
  FWRSTR(OutFile,Thalf2strFixed);
  FWRSTR(OutFile," (");
  FWRSTR(OutFile,Thalf2gen);
  FWRSTR(OutFile," ) minutes.");
  FWRLN(OutFile);
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
  FCLOSE(OutFile);
  PressAnyKey;
END GastricEmptying2.
