MODULE SolveM2;
(*
  REVISION
  --------
   2 Mar 05 -- Added prompts to remind me of the file format.
   3 Mar 05 -- Made version 2 write lines like eqn w/o =.
   4 Mar 05 -- Don't need N as 1st line now.
  26 Feb 06 -- Will reject non-numeric entries and allows <tab> as delim.
  27 Dec 16 -- Started update to ADW Modula-2
*)

FROM SYSTEM IMPORT FUNC;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM Mat IMPORT Zero, Write, Add, Sub, Mul,Random, Solve, GaussJ, Invert;
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

(* MaxRealArray is not square because the B column vector is in last column of IM *)
TYPE MaxRealArray = ARRAY [1..MaxN],[1..MaxN+1] OF LONGREAL;

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
  FOR row := 1 TO MaxN DO
    FOR col := 1 TO MaxN DO
      IM[row,col] := 0.;
    END
  END;

  longstr :=     'Solves vector equation A*X = B; A is a square coef matrix.  N is';
  Strings.Append(ASCII.cr,longstr);
  Strings.Append(ASCII.lf,longstr);
  Strings.Append('determined by number of rows and B value is last on each line.',longstr);
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
END SolveM2.
