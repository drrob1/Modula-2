MODULE SolveM2;
(*
  REVISION HISTORY
  -------- -------
   2 Mar 05 -- Added prompts to remind me of the file format.
   3 Mar 05 -- Made version 2 write lines like eqn w/o =.
   4 Mar 05 -- Don't need N as 1st line now.
  26 Feb 06 -- Will reject non-numeric entries and allows <tab> as delim.
  27 Dec 16 -- Started update to ADW Modula-2 and use updated MAT module.
*)

FROM SYSTEM IMPORT FUNC;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM Mat IMPORT ArrayPtr,NewArray,DisposeArray,Zero, Write, Add, Sub, Mul, Random, Solve, GaussJ, Invert,
  BelowSmallMakeZero;
FROM MiscStdInOut IMPORT WriteString, WriteLn, PressAnyKey, ReadCard, WriteCard, Error, CLS;

IMPORT Terminal, BasicDialogs, DlgShell;
FROM BasicDialogs IMPORT MessageTypes;
FROM Environment IMPORT GetCommandLine;
IMPORT Strings,MemUtils;
IMPORT WholeStr,LongStr, LongConv;
IMPORT LongMath;
IMPORT ASCII;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,ASSIGN2BUF;

  FROM REALLIB IMPORT AINT,AMOD,ROUND,PWRI,PWRR;
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,INI1TKN,GETCHR,
    UNGETCHR,GETTKN,UNGETTKN,GETTKNREAL,GETTKNSTR,DELIMCH,DELIMSTATE;
  FROM TIMLIBrevised IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FOPEN,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRBL,FWRSTR,FWRLN,FAPPEND;
(****************************************************************************)

  FROM Terminal IMPORT Read, (*WriteString, WriteLn, Write, *) ReadChar, Reset;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;

CONST MaxN = 9;

(* MaxRealArray is not square because the B column vector is in last column of IM *)
TYPE
    MaxRealArray = ARRAY [1..MaxN],[1..MaxN+1] OF LONGREAL;
    MatrixPtr = ArrayPtr;  (* import from Mat POINTER TO ARRAY OF ARRAY OF LONGREAL; *)

VAR
  ch1,ch2,ch3 :  CHAR;
  bool,inputprocessed,success :  BOOLEAN;
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
  ra1,ra2,ra3,ra4,ra5,IM : MaxRealArray;
  tpv1        : TKNPTRTYP;
  SolveAns,GaussJans,A,B,X,Y,check,subtrahend : MatrixPtr;


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
    FOR col := 1 TO MaxN DO
      IM[row,col] := 0.;
    END
  END;

  longstr :=     'Solves vector equation A*X = B; A is a square coef matrix.  N is';
  Strings.Append(ASCII.cr,longstr);
  Strings.Append(ASCII.lf,longstr);
  Strings.Append('determined by number of rows and B value is last on each line.',longstr);
(*
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
                            bool := BasicDialogs.PromptOpenFile(OpenFileName,filter,c3,'','',
                                       'Open matrix values as a text file',FALSE);
                                                    IF NOT bool THEN
                                                      Error('Could not open file.  Does it exist?');
                                                      HALT;
                                                    END;
*)


  GetCommandLine(OpenFileName);
  IF LENGTH(OpenFileName) = 0 THEN
    WriteString(" Usage: ");
    WriteString(longstr);
    HALT;
  END (* if commandline is empty *);



  ASSIGN2BUF(OpenFileName,mybuf);
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
        UNTIL (retcod > 0) OR (col > MaxN);
        IF col <= 3 THEN (* not enough numbers found on line, like if line is text only or null. *)
            DEC(N);
        END;
    END (*while N *);
  END; (* reading loop *)

(* Now need to create A and B matrices, which are 0-origin.  IM is 1-origin *)
  A := NewArray(N,N);
  B := NewArray(N,1);
  FOR row := 1 TO N DO
    FOR col := 1 TO N DO
      A^[row-1,col-1] := IM [row,col];
    END;
    B^[row-1,0] := IM [row, N+1];
  END;

  WriteString(' coef matrix, A, is:');
  WriteLn;
  Write(A^,5);
  WriteLn;
  WriteString(' Right hand side vector matrix, B, is:');
  WriteLn;
  Write(B^,5);
  WriteLn;

  X := NewArray(N,1);
  success := Solve (A^, B^, X^);

  IF success THEN
    WriteString ("The solution, X, using Solve AX = B is ");
    WriteLn;
    Write (X^, 6);
    WriteLn;
    PressAnyKey;
  ELSE
    WriteString(" Solve AX=B failed.");
    WriteLn;
  END (*if success *);

  Y := NewArray(N,1);
  success := GaussJ(A^,B^,Y^);
  IF success THEN
    WriteString(" The solution, Y, using GaussJ AX = B is ");
    WriteLn;
    Write(Y^,6);
    WriteLn;
  ELSE
    WriteString(" GaussJ AX=B failed.");
    WriteLn;
  END; (* if success *)

(* Check that the solution looks right. *)

  check := NewArray(N,1);
  success := Mul (A^, X^, check^ );
  IF success THEN
    subtrahend := NewArray(N,1);
    success := Sub (check^, B^, subtrahend^);
    IF success THEN
      WriteString ("As a check, raw AX-B evaluates to");
      WriteLn;
      Write (subtrahend^, 6);
      WriteLn;
      BelowSmallMakeZero(subtrahend^);
      WriteString(" Made Zero AX-B is ");
      Write(subtrahend^,2);
      WriteLn;
    ELSE
        WriteString(" AX-B subtraction operation failed.");
        WriteLn;
    END; (* if subtract *)
  ELSE
    WriteString(" AX multiplication operation failed.");
    WriteLn;
  END; (* if mul *)
  WriteLn;

  PressAnyKey;

  IF tpv1 # NIL THEN DISPOSE(tpv1); END;
  FCLOSE(InFile);
END SolveM2.
