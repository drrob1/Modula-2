(*V0=LTD*)
(*V1=FULL*)
IMPLEMENTATION MODULE FIO;

(*
Copyright (C) 1987  Robert Solomon MD.  All rights reserved.
  This module implements a simple fast input/output file system using
  dynamically allocated I/O buffers.  All support constants and code
  for this file system is also in this module.

  REVISION HISTORY
  ----------------
  15 JUNE 87 -- Added EXTRACTDRVPTH Procedure.
  20 AUGUST 87 -- Changed FRESET so path is search only when a file
                  is to be opened for reading.  Only 1 directory is searched
                  for an output file, which is then created if not found.
  3 Mar 88 -- The <CR> and <LF> line delimiter handling was altered.  Now
               if two delims are present at the end of a line, they must
               be different to both be handled as the same line.  Else the
               second occurrence of the delim is pushed back to the file so
               it is considered as a blank line.  Other operating systems
               may not use both the <CR> and <LF> as line delimiters.
  10 Mar 88 -- Added the FPURGE and FREAD procedures, exported them,
                and reorganized the call structure to more efficiently use
                the new procedures.
  14 Mar 88 -- Added the FAPPEND procedure to write to the end of a file w/o
                overwriting its previous contents.  ISCRLF procedure now
                returns TRUE for the EOFMARKER char.
  19 Mar 88 -- The FAPPEND function does not work as is.  Until I can get
                more time to figure out how to use the DUP or CDUP DOS
                functions to close a duplicate file handle, this will be
                deleted.
  21 Mar 88 -- Added the FCLOSEDUP procedure.
  26 Mar 88 -- Fixed the RDTXLN Procedure so that the EOFFLG is defined
                when the FREAD Procedure is *NOT* called.
  29 Mar 88 -- Added the COPYDPIF and FAPPEND Procedures that really work,
                and the FCLOSE Procedure no longer writes the EOFMARKER
                character to the file.
  31 Mar 88 -- Converted to M2 V3.03.
  12 Jul 88 -- Made changes to speed up routine, by using UL2 procs
               instead of the slower UTILLIB counterparts.
  1 Sept 88 -- 1. Added FWRBL Procedure.
               2. Fixed error message in RETBLKBUF.
  7 Apr 89  -- Made FCLOSE write out 0 bytes to truncate the file.
  17 Nov 89 -- Sped up disk access by searching backwards in IOBUF instead
                of forward char by char, and using the faster UL2 procs more
                often in the code.
  22 Dec 91 -- Converted to M2 V 4.00, which does not support CODE procs
                and completely changed the DOSCALL mechanism.  Also changed
                memory allocation calls to std procs of NEW and DISPOSE.
  24 Feb 94 -- Fixed bug in RDTXLN proc, involving very long lines that
                exceed BUFSIZ.
  20 Mar 96 -- Added LNMAX param to FRDTXLN.
  21 Mar 96 -- Fixed problem of nulls in file by not calling TRIM, which
                assumes null terminated strings and searches from the
                beginning for the null char.
  24 Mar 96 -- Fixed how FREAD handles its potential retained fragment,
                probably related to adding LNMAX param.
  16 Apr 96 -- Fixed pblm FRDTXLN with skipping bl lines when <CR><LF>
                not in original file.
*)
  IMPORT Break;
  IMPORT MED;
  FROM SYSTEM IMPORT REGISTERS,INT,ADDRESS,ADR;
  FROM Terminal IMPORT ReadString;
  FROM InOut IMPORT ReadCard,Read,WriteString,WriteLn,WriteInt,WriteCard;
(*  FROM RTSMain IMPORT Terminate, Status;*)
(*  FROM SYSTEM IMPORT SIZE,TSIZE,WORD,ADR,;*)
  FROM SYSTEM IMPORT WORD,ADR;
  FROM DiskFiles IMPORT GETFH;
  FROM FileSystem IMPORT File,Response,Command,Flag,FlagSet,Lookup,
    Close,Reset,Again,ReadChar,ReadByte,ReadNBytes,WriteChar,WriteByte,
    WriteNBytes,SetRead,SetWrite;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ,CR,LF,MAXCARD,MAXINT,MININT,BUFTYP,ESC,
    NULL,BLANK,STRTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM ENVRTNS IMPORT OPNFIL;
(*<FULL
  FROM DosCalls IMPORT DosCall_40h,DosCall_45h,DosCall_3Eh,DosCall_42h;
FULL>*)

(*
 NOW ALL THIS IS DECLARED IN THE DEFINITION MODULE, SO IT IS COMMENTED OUT.

CONST
    IOBUFSIZ = 10 * 1024;
    IOBUFSIZMAX = IOBUFSIZ + BUFSIZ + 1;
    EOFMARKER = 032C; (* CHR(26) OR CTRL-Z *)
    DRIVESEP  = ':';
    SUBDIRSEP = '\';

  IOBUFTYP = ARRAY [1..IOBUFSIZMAX] OF CHAR;
  MYFILTYP = RECORD
    FILE                      : File;
    IOBUF                     : POINTER TO IOBUFTYP;
    NXTPOSN,LASTPOSN,BYTESRED : CARDINAL;
    FILENAME                  : STRTYP;
    LineTermCH                : CHAR;
  END(*RECORD*);
  IOSTATE = (RD,WR);
*)

CONST WrFilorDev = 40H;  (* DOS function to write to a file or device *)
      DupFH      = 45H;
      CloseFH    = 3EH;
      MoveFilePtr= 42H;

VAR
  I               : INTEGER;
  BLANKBUF,TMPBUF : BUFTYP;

PROCEDURE RETBLKBUF(CNT : WORD; VAR BUF : BUFTYP);
(*
******************************* RETBLKBUF ***********************************
RETURN BLANK BUFFER.
THIS ROUTINE IS USED TO RETURN A BLANK BUFFER THAT IS USED TO INDENT
THE OUTPUT LINE BY AS MANY SPACES AS NEEDED.  ROUTINE IS NEEDED
PRIMARILY FOR WRRTN AND ANWORD'S MAIN BODY.

INPUT FROM GBLVAR'S : BLANK, BUFSIZ
*)

VAR C : CARDINAL;

BEGIN
  C := CARDINAL(CNT);  (* USE OF CARDINAL TYPE TRANSFER FUNCTION *)
  BUF := BLANKBUF;
  IF C > BUFSIZ THEN  (* Can't be < 0 because it is a Cardinal *)
    WriteString(' **ERROR** Range error from RETBLKBUF.  CNT = ');
    WriteCard(C,0);
    WriteString('.  CNT made to be zero.');
    WriteLn;
    C := 0;
  END(*IF*);
  BUF.COUNT := C;
END RETBLKBUF;

PROCEDURE FWRBL(VAR F : MYFILTYP; CNT : WORD);
(*
******************************* FWRBL ***********************************
Fast file WRite Blanks.
This routine is used to write CNT number of blanks to file F.

INPUT FROM GBLVAR'S : BLANK, BUFSIZ
*)

VAR C   : CARDINAL;
    BUF : BUFTYP;

BEGIN
  C := CARDINAL(CNT);  (* USE OF CARDINAL TYPE TRANSFER FUNCTION *)
  BUF := BLANKBUF;
  IF C > BUFSIZ THEN  (* Can't be < 0 because it is a Cardinal *)
    WriteString(' **ERROR** Range error from FWRBL.  CNT = ');
    WriteCard(C,0);
    WriteString('.  CNT made to be zero.');
    WriteLn;
    C := 0;
  END(*IF*);
  BUF.COUNT  := C;
  BUF.LENGTH := C;
  FWRTX(F,BUF);
END FWRBL;

PROCEDURE EXTRACTDRVPTH(BUF:BUFTYP; VAR DRVPATH:BUFTYP);
(*
****************************** EXTRACTDRVPTH *******************************
EXTRACT DRIVE AND PATH PROCEDURE.
THIS PROCEDURE EXTRACTS THE DRIVE AND PATH SPECIFICATION PRESENT IN BUF AND
TRANSFERS THEM TO THE DRVPATH PARAM.

*)
VAR C : CARDINAL;

BEGIN
  C := MAXCARDFNT(LCHINBUFFNT(BUF,DRIVESEP),LCHINBUFFNT(BUF,SUBDIRSEP));
  IF C = 0 THEN
    DRVPATH.COUNT  := 0;
    DRVPATH.LENGTH := 0;
    DRVPATH.CHARS[1] := NULL;
  ELSE
    DRVPATH := BUF;
    DRVPATH.COUNT  := C;
    DRVPATH.LENGTH := C;
    DRVPATH.CHARS[C+1] := NULL;
  END(*IF*);
END EXTRACTDRVPTH;

PROCEDURE FWRLN(VAR F : MYFILTYP);
(*
************************* FWRLN *********************************************
File WRite LiNe.
THIS ROUTINE WILL WRITE A <CR>, <LF> SEQUENCE BY CREATING AN EMPTY BUFFER
AND THEN CALLING FWRTXLN.

*)

VAR B : BUFTYP;

BEGIN
  WITH F DO
    IOBUF^[NXTPOSN] := CR;
    INC(NXTPOSN);
    IOBUF^[NXTPOSN] := LF;
    INC(NXTPOSN);
  END(*WITH*);
END FWRLN;

PROCEDURE ISCRLF(CH : CHAR) : BOOLEAN;
(*
************************* ISCRLF ***************************************
IS CR OR LF.
This is a simple function whose purpose is to make it easier to test whether
the character input as its parameter is either a <CR> or a <LF>.  Function
result is TRUE if this is the case.  This function also tests for EOFMARKER
so this function is TRUE in this case as well.

INPUT FROM GBL VAR'S : CR, LF, EOFMARKER.
*)
BEGIN
  RETURN((CH = CR) OR (CH = LF) OR (CH = EOFMARKER));
END ISCRLF;

PROCEDURE FRESET(VAR F:MYFILTYP; VAR FILNAM:BUFTYP; RDWRSTATE:IOSTATE);
(*
************************ FRESET **************************************
File RESET ROUTINE.
THIS PROCEDURE OPENS THE FILE FOR I/O, USING THE LOGITECH PROCEDURES LOOKUP
AND RESET.  FILNAM IS THE FILE NAME AS BUILT BY GETFNM.  UNLIKE THE PASCAL
PROCEDURE OF THE SAME NAME, THIS ROUTINES OPENS AND RESETS FILES FOR BOTH
READING AND WRITING.  RDWRSTATE IS EITHER RD FOR OPEN A FILE FOR READING,
OR WR FOR OPEN A FILE FOR WRITING.

*)

VAR I,RETCOD : CARDINAL;
    EOFFLG   : BOOLEAN;

BEGIN
  WITH F DO
(* ALLOCATE I/O BUFFER IN HEAP *)
(*    ALLOCATE(IOBUF,TSIZE(IOBUFTYP));*)
    NEW(IOBUF);

    IF RDWRSTATE = RD THEN
      OPNFIL(F,FILNAM,RETCOD);
      IF RETCOD <> 0 THEN
        WriteString(' Error in Lookup for file ');
        WriteString(FILNAM.CHARS);
        WriteString('--');
        IF RETCOD = 1 THEN
          WriteString('File not found.');
        ELSE  (* RETCOD MUST = 2 *)
          WriteString('Nonspecific error occured.');
        END(*IF*);
        WriteLn;
        WriteString(' Program Terminated.');
        WriteLn;
(*        Terminate(Normal);*)
        HALT;
      END(*IF*);
    ELSE (* RDWRSTATE MUST = WR *)
      Lookup(FILE,FILNAM.CHARS,TRUE);
      IF FILE.res <> done THEN
        WriteString(' Error in Lookup for file ');
        WriteString(FILNAM.CHARS);
        WriteString('--');
        CASE FILE.res OF
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
    END(*IF*);

    Reset(FILE); (*RESET APPLIES TO READ'S AND WRITE'S, UNLIKE IN PASCAL. *)
(*
  INITIALIZE I/O BUFFER AND ITS NXTPOSN.
*)
    IOBUF^[IOBUFSIZMAX] := CR;   (* Sentinal for text line searches *)
    FILENAME := FILNAM.CHARS;
    IF RDWRSTATE = RD THEN
      BYTESRED := 0;
      NXTPOSN  := 1;
      LASTPOSN := 1;
      F.LineTermCH := NULL;
      SetRead(FILE);
      FREAD(F,EOFFLG);   (* Ignore the EOFFLG result *)
    ELSE
      NXTPOSN := 1; (* START FWRTXLN OR FWRTX AT BEGINNING OF BUFFER. *)
      SetWrite(FILE);
    END(*IF*);
  END(*WITH*);
END FRESET;

PROCEDURE FREAD(VAR F:MYFILTYP; VAR EOFFLG:BOOLEAN);
(*
****************************** FREAD *************************************
File READ.
This procedure will read the proper number of characters into the IOBUF.

*)
VAR CH : CHAR;
    C  : CARDINAL;

BEGIN
  WITH F DO
    EOFFLG := FILE.eof;
    IF EOFFLG THEN RETURN; END(*IF*);
(*
  May have unprocessed line fragment that needs to be moved to the front of
  the buffer.  Then need to read from file to concatinate the fragments.
    If there are no LineTermCH chars, then the FRDTXLN SCANFWD call will
  find the sentinal and be 1 too large, hence C could potentially be 
  negative and NXTPOSN would be 2 more than BYTESRED.  So
  BYTESRED + 1 - NXTPOSN would be negative.  Hence a separate initialization
  for C if NXTPOSN > BYTESRED.
*)
    IF NXTPOSN > BYTESRED THEN
      C := 1;
    ELSE
      C := BYTESRED + 1 - NXTPOSN;
      COPYLEFT(ADR(IOBUF^[NXTPOSN]),ADR(IOBUF^[1]),C);
      INC(C);  (* Insert additional characters past the pending ones *)
    END(*IF*);
    ReadNBytes(FILE,ADR(IOBUF^[C]),IOBUFSIZ,BYTESRED);
    IF FILE.res <> done THEN
      WriteString(' Error in reading from file ');
      WriteString(FILENAME);
      WriteString('--');
      CASE FILE.res OF
         callerror : WriteString('Call error.  Either file was in Write ');
                     WriteLn;
                     WriteString('mode or already closed.');
      |  notdone : WriteString('Nonspecific error.  Read not done.');
      END(*CASE*);
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
(*      Terminate(Normal);*)
      HALT;
    END(*IF*);
(*
SUCCESSFUL READ.  CONTINUE.
  Will remove the End-Of-File MARKER if found.  Will no longer append it
  upon closure of the file by the FCLOSE PROCEDURE.  This test has
  been moved here so that the call to Again doesn't put it back if only one
  line delimiter is present.
    The variable BYTESRED will function also as a pointer to the
  end of the buffer, ie, the last character read.  LASTPOSN refers to the
  position of the last complete line.
*)
    INC(BYTESRED,C);  (* But this creates an off-by-one error *)
    DEC(BYTESRED);    (* So this fixes this *)
    IF LineTermCH = NULL THEN
      IF SCANBACK(IOBUF,BYTESRED,CR,TRUE) > 0 THEN
        LineTermCH := CR;
      ELSE
        LineTermCH := LF;
      END(*IF*);
    END(*IF*);
    IF IOBUF^[BYTESRED] = EOFMARKER THEN
      IOBUF^[BYTESRED] := LineTermCH;    (* Sentinal <CR> or <LF> *)
      DEC(BYTESRED);
    ELSE
      IOBUF^[BYTESRED+1] := LineTermCH;  (* Sentinal <CR> or <LF> *)
    END(*IF*);
    NXTPOSN := 1;
    LASTPOSN := BYTESRED;
    IF FILE.eof THEN RETURN; END(*IF*);
(*
  Possibility exists that a line has been split, so back up reading one
  character at a time until a <CR> or <LF> is found.

  WHILE NOT ISCRLF(IOBUF^[BYTESRED]) AND (LASTPOSN > 1) DO
    DEC(LASTPOSN);
  END(*WHILE*);
*)
    LASTPOSN := SCANBACK(ADR(IOBUF^[1]),BYTESRED,LineTermCH,TRUE);
    IF LASTPOSN = 0 THEN (* LineTermCH not found *)
      LASTPOSN := BYTESRED;
    END(*IF*);
  END(*WITH*);
END FREAD;

PROCEDURE FRDTXLN(VAR F:MYFILTYP; VAR BUF:BUFTYP; LNMAX:CARDINAL;
                                                         VAR EOFFLG:BOOLEAN);
(*
****************************** FRDTXLN *******************************
File READ TEXT LINE.
This routine functions like its slower counterpart, only it gets its
text line from the I/O buffer and not from the current input device.
Like its slower counterpart, this routine more closely approximates
the read of FORTRAN, with its reading an entire line, and testing for
the END-OF-FILE condition.

F  --THE FILE VARIABLE FOR THE FILE TO WHICH DATA IS TO BE WRITTEN.
BUF--THE BUFFER OF CHARS TO BE WRITTEN TO FILE F.
LNMAX-- Max amt of chars allowed per text line buffer.
EOFFLG--FLAG INDICATING WHETHER THE END OF FILE HAS BEEN FOUND.
*)
VAR C : CARDINAL;

BEGIN
  WITH F DO
    IF NXTPOSN > LASTPOSN THEN
(*
  I/O buffer is empty.  Read more data.
*)
      FREAD(F,EOFFLG);
      IF EOFFLG THEN RETURN; END(*IF*);
    ELSE
      EOFFLG := FALSE;
    END(*IF*);
    IF (LNMAX = 0) OR (LNMAX > BUFSIZ) THEN LNMAX := BUFSIZ END(*IF*);
    C := SCANFWD(ADR(IOBUF^[NXTPOSN]),LNMAX,LineTermCH,TRUE);
    COPYLEFT(ADR(IOBUF^[NXTPOSN]),ADR(BUF.CHARS),C);
(*
  Null terminate BUF so the other string fnts work properly.  This will
  replace the LineTermCH character, as well.  Note that the value of C
  is the position of LineTermCH, not of the last text char.
    If LineTermCH not found, SCANFWD will return BUFSIZ+1, still not the
  last text char.
*)
    WITH BUF DO
      CHARS[C] := NULL;
      COUNT := C - 1;
      LENGTH := COUNT;
    END(*WITH*);
    IF C > LNMAX THEN C := LNMAX END(*IF*); (* When LineTermCH not found *)
    INC(NXTPOSN,C);
(*
  INCREMENT PAST <CR><LF> CHARS.
  Some non-DOS systems use one or the other as a delimiter, not both.
  If two identical adjacent delims were found, we have one of those systems
  and two consecutive such characters means the second belongs to a
  blank line.  The second IF sttmnt includes a test for equality to catch
  this condition.  The std Modula-2 code did not include the <CR,LF> chars
  in the count, so it had to possible increment twice.  Now that one of
  these is included in the count, need to only increment once.
*)
    IF ISCRLF(IOBUF^[NXTPOSN]) AND (IOBUF^[NXTPOSN] <> LineTermCH) THEN
      INC(NXTPOSN);  (* Increment past <LF> if <CR> is LineTermCH *)
    END(*IF*);
  END(*WITH*);
END FRDTXLN;

PROCEDURE FPURGE(VAR F : MYFILTYP);
(*
**************************** FPURGE *************************************
File PURGE.
This routine will unconditionally write the IOBUF out to disk.

*)
VAR IGNORE : CARDINAL;

BEGIN
  WITH F DO
    WriteNBytes(FILE,IOBUF,NXTPOSN-1,IGNORE);  (* Ignore # bytes really  *)
    IF FILE.res <> done THEN                   (*               written. *)
      WriteString(' Error in writing to file ');
      WriteString(FILENAME);
      WriteString('--');
      CASE FILE.res OF
         callerror : WriteString('Call error.  Either file was in Read ');
                     WriteLn;
                     WriteString('mode or already closed.');
      |  eom : WriteString('Disk is full.  Insufficient disk space to ');
               WriteLn;
               WriteString('write to file.');
      |  notdone : WriteString('Nonspecific error.  Write not done.');
      END(*CASE*);
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
(*      Terminate(Normal);*)
    END(*IF*);
(*
SUCCESSFUL WRITE.  CONTINUE.
*)
    NXTPOSN := 1;
  END(*WITH*);
END FPURGE;

PROCEDURE FWRTX(VAR F : MYFILTYP; BUF : BUFTYP);
(*
****************************** FWRTX **************************************
File WRITE TEXT.
THIS ROUTINE FUNCTIONS LIKE ITS SLOWER COUNTERPART, ONLY IT PUTS ITS
TEXT LINE INTO THE I/O BUFFER AND NOT TO THE CURRENT OUTPUT DEVICE.
LIKE ITS SLOWER COUNTERPART, THIS ROUTINE MORE CLOSELY APPROXIMATES
THE WRITE OF FORTRAN, WITH ITS WRITING AN ENTIRE LINE, AND TESTING FOR
THE END OF FILE CONDITION.  ALSO, IT DIFFERS FROM FWRTXLN IN THAT THE
<CR> AND <LF> CHAR'S ARE NOT WRITTEN TO IOBUF.

F  --THE FILE VARIABLE FOR THE FILE TO WHICH DATA IS TO BE WRITTEN.
BUF--THE BUFFER OF CHARS TO BE WRITTEN TO FILE F.
*)

VAR
  I  : CARDINAL;

BEGIN
  WITH F DO
    IF NXTPOSN + BUF.COUNT > IOBUFSIZ THEN
(*
  This line will not fit in the I/O buffer, so first write buffer to disk.
*)
      FPURGE(F);
    END(*IF*);
(*
  Put string into I/O buffer.
*)
    COPYRIGHT(ADR(BUF.CHARS),ADR(IOBUF^[NXTPOSN]),BUF.COUNT);
    INC(NXTPOSN,BUF.COUNT);
(* Old way of moving char's, using pure (std) Modula-2.
    FOR I := 1 TO BUF.COUNT DO
      IOBUF^[NXTPOSN] := BUF.CHARS[I];
      INC(NXTPOSN);
    END(*FOR*);
*)
  END(*WITH*);
END FWRTX;

PROCEDURE FWRTXLN(VAR F : MYFILTYP; BUF : BUFTYP);
(*
****************************** FWRTXLN **************************************
File WRITE TEXT LINE.
THIS ROUTINE FUNCTIONS LIKE ITS SLOWER COUNTERPART, ONLY IT PUTS ITS
TEXT LINE INTO THE I/O BUFFER AND NOT TO THE CURRENT OUTPUT DEVICE.
LIKE ITS SLOWER COUNTERPART, THIS ROUTINE MORE CLOSELY APPROXIMATES
THE WRITE OF FORTRAN, WITH ITS WRITING AN ENTIRE LINE, AND TESTING FOR
THE END OF FILE CONDITION.

F  --THE FILE VARIABLE FOR THE FILE TO WHICH DATA IS TO BE WRITTEN.
BUF--THE BUFFER OF CHARS TO BE WRITTEN TO FILE F.
*)

VAR
  I,IGNORE : CARDINAL;

BEGIN
    FWRTX(F,BUF);
    FWRLN(F);
END FWRTXLN;

PROCEDURE FWRSTR(VAR F : MYFILTYP; CHARRAY : ARRAY OF CHAR);
(*
************************* FWRSTR ********************************************
File WRite STRing.
THE PURPOSE OF THIS ROUTINE IS TO ALLOW EASY OUTPUT OF STRINGS.  IT IS
IMPLEMENTED WITH CALLS TO ASSIGN2BUF AND FWRTX, BUT PUTTING IT
HERE SIMPLIFIES THE I/O FOR THE USER.

*)

VAR B : BUFTYP;

BEGIN
    ASSIGN2BUF(CHARRAY,B);
    FWRTX(F,B);
END FWRSTR;

PROCEDURE FCLOSE(F : MYFILTYP);
(*
******************************* FCLOSE **************************************
File CLOSE.
THIS ROUTINE WILL CLOSE THE FILE INPUT AS ITS FORMAL PARAMETER.  IF THE
BUFFER IS A WRITE BUFFER, THE BUFFER WILL BE WRITTEN BEFORE CLOSURE.
*)

VAR I,BYTESWRITTEN,ERR : CARDINAL;
    FH                 : WORD;
    r           	     : REGISTERS;
    a                  : ADDRESS;

BEGIN
  WITH F DO
    IF (wr IN FILE.flags) AND (NXTPOSN > 1) THEN
(*
  WRITE OUT BUFFER, without the EOFMARKER.

      IF NXTPOSN < IOBUFSIZMAX THEN
        IOBUF^[NXTPOSN] := EOFMARKER;
        INC(NXTPOSN);
      END(*IF*);
  Truncate file by directly writing 0 bytes to overcome the pblm
  that surfaced when ^Z was no longer written out when file was closed.
  That is, of trailing garbage from previous version of file that should be
  overwritten, isn't.  Only a pblm if new file is shorter than old one.
*)
      FPURGE(F);
      FH := GETFH(FILE);
      WITH r DO
        AH := WrFilorDev;
        BX := FH;
        CX := 0;
        a  := ADR(I);  (* Doesn't matter because writing 0 bytes *)
        DS := a.SEGMENT;
        DX := a.OFFSET;
        INT(21H, r);
        BYTESWRITTEN := AX;
        IF 0 IN FLAGS THEN
          ERR := AX;
        ELSE
          ERR := 0;
        END(*IF*);
      END(*WITH*);
(*<FULL
      DosCall_40h(FH,0,ADR(I),BYTESWRITTEN,ERR); (* Write file or device *)
FULL>*)
(*      DOSCALL(WrFilorDev,FH,0,ADR(I),BYTESWRITTEN,ERR);*)
    END(*IF*);
    Close(FILE);
(*    DEALLOCATE(IOBUF,TSIZE(IOBUFTYP));*)
    DISPOSE(IOBUF);
  END(*WITH*);
END FCLOSE;

PROCEDURE FCLOSEDUP(VAR F:MYFILTYP);
(*
******************************** FCLOSEDUP ******************************
File CLOSE DUPlicate file handle.
  This procedure will purge the I/O buffer, create a duplicate file handle
and then close this duplicate.  This will transfer the filesize, etc to
the directory.

*)

VAR FH,DUPFH : WORD; (* File Handle and its Duplicate *)
    ERRCOD   : CARDINAL;
    r        : REGISTERS;

BEGIN
  FPURGE(F);
  FH := GETFH(F.FILE);
  WITH r DO
    AH := CHR(DupFH);
    BX := FH;
    INT(21H, r);
    DUPFH := AX;
    IF 0 IN FLAGS THEN
      ERRCOD := AX;
    ELSE
      ERRCOD := 0;
    END(*IF*);
  END(*WITH*);
(*<FULL
  DosCall_45h(FH,DUPFH,ERRCOD); (* Duplicate a File Handle call *)
FULL>*)
  IF ERRCOD > 0 THEN
    WriteString(' Error while trying to duplicate a file handle.  Attempt ');
    WriteString('aborted.');
    WriteLn;
    RETURN;
  END(*IF*);
  WITH r DO
    AH := CloseFH;
    BX := DUPFH;
    INT(21H, r);
    IF 0 IN FLAGS THEN
      ERRCOD := AX;
    ELSE
      ERRCOD := 0;
    END(*IF*);
  END(*WITH*);
(*<FULL
  DosCall_3Eh(DUPFH,ERRCOD); (* Close a File Handle call *)
FULL>*)
  IF ERRCOD > 0 THEN
    WriteString(' Error while trying to close duplicate file handle.');
    WriteString('  Program terminated.');
(*    Terminate(Normal);*)
    HALT;
  END(*IF*);
  RETURN;
END FCLOSEDUP;

PROCEDURE COPYDPIF(INFNAM : BUFTYP; VAR OUTFNAM : BUFTYP);
(*
**************************** COPYDPIF ***********************************
COPY Drive and Path IF there is no drive or path already there.  That is,
this procedure will first check to see of the output file name has a drive
or path specification.  If so, then nothing else is done.  If not, then
the drive and path specification used for the input file will be copied to
the output file name.
*)

VAR DRVPATH,TMPBUF : BUFTYP;

BEGIN
  IF MAXCARDFNT(LCHINBUFFNT(OUTFNAM,DRIVESEP),LCHINBUFFNT(OUTFNAM,SUBDIRSEP))
                                                                     = 0 THEN
    EXTRACTDRVPTH(INFNAM,DRVPATH);
    TMPBUF := DRVPATH;
    MRGBUFS(TMPBUF,OUTFNAM);
    OUTFNAM := TMPBUF;
  END(*IF*);
END COPYDPIF;

PROCEDURE FAPPEND(VAR F:MYFILTYP; VAR FILNAM:BUFTYP);
(*
*********************** FAPPEND *****************************************
File APPEND.
This procedure will open the file given in FILNAM as an output file, and
whatever is written to this file is appended to the file, instead of
overwriting it.
*)
VAR FH                          : WORD; (* File Handle *)
    METHOD,FPTRHI,FPTRLO,ERRCOD : CARDINAL;
    r                           : REGISTERS;
 (* FPTR hi & lo are 32 bit file length *)

BEGIN
  FRESET(F,FILNAM,WR);
  FH := GETFH(F.FILE);
  METHOD := 2; (* 0=>Rel 2 beg, 1=>rel 2 cur posn, 2=>rel 2 EOF *)
(* Move File Pointer to a 32 bit offset of 0,0 relative to the EOF *)
  WITH r DO
    AH := MoveFilePtr;
    BX := FH;
    AL := CHR(METHOD);
    CX := 0;       (* InHigh *)
    DX := 0;       (* InLow  *)
    INT(21H, r);
    FPTRHI := DX;
    FPTRLO := AX;
    IF 0 IN FLAGS THEN
      ERRCOD := AX;
    ELSE
      ERRCOD := 0;
    END(*IF*);
  END(*WITH*);
(*<FULL
   DosCall_42h(FH,METHOD,0,0,FPTRHI,FPTRLO,ERRCOD);
FULL>*)
  IF ERRCOD > 0 THEN
    WriteString(' Error while trying to append to file ');
    WriteString(FILNAM.CHARS);
    WriteLn;
    WriteString('Program terminated.');
    WriteLn;
(*    Terminate(Normal);*)
    HALT;
  END(*IF*);
END FAPPEND;

BEGIN  (* INITIALIZE BLANKBUF FOR RETBLKBUF *)
  FILLCHAR(ADR(BLANKBUF.CHARS),BUFSIZ,BLANK);
(*  FOR I := 1 TO BUFSIZ DO BLANKBUF.CHARS[I] := BLANK; END(*FOR*); *)
  BLANKBUF.LENGTH := 0;
  BLANKBUF.COUNT  := 0;
END FIO.
