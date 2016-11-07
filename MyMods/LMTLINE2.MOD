MODULE LmtLine2;
(*  Limit Line.  Separate input file into 78 character pieces. *)
(*
  REVISION HISTORY
  ----------------
*)
  FROM SYSTEM IMPORT ADR,ADDRESS;
  IMPORT Break;
  IMPORT MED;
  IMPORT ASCII;
  FROM Terminal IMPORT ReadString;
  IMPORT Terminal;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteCard,WriteInt,
    WriteHex,WriteOct,Write;
  FROM Directories IMPORT Delete,DirResult;
  FROM UTILLIB IMPORT CR,LF,NULL,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,BUFTYP,
    MAXCARDFNT;
  FROM UL2 IMPORT COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,CopyWords,
    FillWord,STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF,GETFNM;
(*
  FROM FIO IMPORT DRIVESEP,SUBDIRSEP,MYFILTYP,IOSTATE,FRESET,FCLOSE,
    FRDTXLN,FWRTXLN,FWRTX,RETBLKBUF,EXTRACTDRVPTH,FCLOSEDUP,FWRLN,
    FWRSTR;
*)
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,DELIMSTATE,INI1TKN,
    INI3TKN,GETCHR,UNGETCHR,GETTKN,
    NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM TIMER IMPORT READTIMER;
  FROM FileSystem IMPORT File,Response,Command,Flag,FlagSet,Lookup,
    Close,Reset,Again,ReadChar,ReadByte,ReadNBytes,WriteChar,WriteByte,
    WriteNBytes,WriteWord,SetRead,SetWrite;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;

CONST
    NEWFIL    = FALSE;
    CRLF      = 0A0DH;  (* But remember byte swapping Intel design *)
    BaseLineLen = 75;
    ExtendedLineLen = 10;
    SP = ' ';

VAR
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,BUF : BUFTYP;
  INUNT1,OUTUN1                       : File;
  IOBUF                               : POINTER TO ARRAY [1..BUFSIZ] OF CHAR;
  EOFFLG                              : BOOLEAN;
  IGNORE                              : DirResult;
  ignore,BYTESRED,C                   : CARDINAL;
  CH                                  : CHAR;

BEGIN
(*
********************************* MAIN PROGRAM *************************
*)
  WriteString('Limit Line 2 -- Copyright (C) 1996  Robert Solomon MD.');
  WriteLn;
  WriteString(' All rights reserved.');
  WriteString('  Last Update 18 Apr 96.');
  WriteLn;  

(* Get file names from terminal, using the GETFNM subroutine. *)

  ASSIGN2BUF('Enter  input file name : ',PROMPT);
  ASSIGN2BUF('moondog.msg',NAMDFT);
  ASSIGN2BUF('.txt',TYPDFT);
  GETFNM(PROMPT, NAMDFT, TYPDFT, INFNAM);
  ASSIGN2BUF('Enter output file name : ',PROMPT);
  ASSIGN2BUF('MOONDOG.lmt',NAMDFT);
  ASSIGN2BUF('.lmt',TYPDFT);
  GETFNM(PROMPT, NAMDFT, TYPDFT, OUTFNAM); 
  
(*  NEW(IOBUF); *)
  Lookup(INUNT1,INFNAM.CHARS,NEWFIL);
  IF INUNT1.res = done THEN
(* Do nothing, but this won't be part of the ELSE clause below *)
  ELSIF INUNT1.res = notdone THEN
(*
  The response field of the file record would be notdone if file not found.
  If it is anything else, some other error occurred.
*)
    WriteString(' Error in Lookup for file ');
    WriteString(INFNAM.CHARS);
    WriteString('--');
    WriteString('File not found.');
    WriteLn;
    WriteString(' Program Terminated.');
    WriteLn;
    HALT;
  ELSE
    WriteString(' Error in Lookup for file ');
    WriteString(INFNAM.CHARS);
    WriteString('--');
    WriteString('Nonspecific error occured.');
    WriteString(' Program Terminated.');
    WriteLn;
    HALT;
  END(*IF*);
  Reset(INUNT1); (*RESET APPLIES TO READ'S AND WRITE'S, UNLIKE IN PASCAL. *)
(*
  Attempt to delete file before opening it.  This is to overcome the pblm
  that surfaced when ^Z was no longer written out when file was closed.
  That is, of trailing garbage from previous version of file that should be
  overwritten, isn't.  Only a pblm if new file is shorter than old one.
  Using Text mode will write ^z, so need this here.
  Delete(OUTFNAM.CHARS,IGNORE);
*)
  Lookup(OUTUN1,OUTFNAM.CHARS,TRUE);
  IF OUTUN1.res <> done THEN
    WriteString(' Error in Lookup for file ');
    WriteString(OUTFNAM.CHARS);
    WriteString('--');
    CASE OUTUN1.res OF
      notdone : WriteString('File not found.');
    ELSE
      WriteString('Nonspecific error occured.');
    END(*CASE*);
    WriteLn;
    WriteString(' Program Terminated.');
    WriteLn;
(*        Terminate(Normal);*)
    HALT;
  END(*IF*);
  Reset(OUTUN1); (* Reset applies to read's and write's, unlike in PASCAL *)

  WriteString('  INPUT FILE : '); 
  WriteString(INFNAM.CHARS); 
  WriteLn;
  WriteString(' OUTPUT FILE : ');
  WriteString(OUTFNAM.CHARS); 
  WriteLn;

  LOOP  (* Main file reading & writing loop *)
    C := 1;
    REPEAT
      ReadChar(INUNT1,CH);
      IF INUNT1.eof THEN EXIT END(*IF*);
      WriteChar(OUTUN1,CH);
      INC(C);
    UNTIL (C > BaseLineLen) OR (CH = ASCII.EOL);
    IF CH <> ASCII.EOL THEN
      (* Didn't find end of line so try to break at a space *)
      C := 1;
      REPEAT
        ReadChar(INUNT1,CH);
        IF INUNT1.eof THEN EXIT END(*IF*);
        IF CH <> SP THEN
          WriteChar(OUTUN1,CH);
          INC(C);
        END(*IF*);
      UNTIL (C > ExtendedLineLen) OR (CH = SP) OR (CH = ASCII.EOL);
      IF CH <> ASCII.EOL THEN WriteChar(OUTUN1,ASCII.EOL) END(*IF*);
    END(*IF*);
  END(*LOOP*);
(*  DISPOSE(IOBUF); *)
  Close(INUNT1);
  Close(OUTUN1);
END LmtLine2.

