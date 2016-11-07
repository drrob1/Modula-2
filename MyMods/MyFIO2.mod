IMPLEMENTATION MODULE MyFIO2;

(*
Copyright (C) 1987-2011  Robert Solomon MD.  All rights reserved.
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
  20 Oct 02 -- Converted to M2 for win32, DOS mode.  Took out FCLOSEDUP.
   7 Dec 03 -- Had to modify the Import from FileFunc statement.
   2 May 04 -- Forgot to init MOREFNM variable.  Used Terminal procs
                instead of Std I/O.  Removed filename line too long logic.
   1 Aug 04 -- Changed param for WriteBlock because of compiler library change.
                Also decided to add FOPEN
  22 Dec 04 -- Changed GETFNM so it prompts w/ the default filename.
   9 Feb 09 -- Added ChopIntoLines routine as .qfx file is without <CR> or <LF>,
                and the correspond const of PracticalLineLmt
   3 Mar 11 -- Will change EOL processing so I can handle EOL more generally.  The problem case is
                when the file is now in <cr><lf> format and lines that are only <lf> terminated
                are inserted into a file.  EOL has to be checked during each line read, not once
                when file is opened.
                So far, I am only changing FRDTXLN to read char-by-char.  I'll leave the
                file writes alone and still allow them to be buffered.
               Re-wrote GETFNM which didn't work at all.
   8 Mar 11 -- Added FWRLF to allow me to write a fromdos routine.
  12 Aug 11 -- Finally fixed the buffer size.  When it >4K, it exceeds the stacksize and
               the program crashes when the processed file exceeds 4K.
   7 Apr 13 -- Added the boolean to FRESET to not halt on an error, allowing the status field
                to indicate the result.  I did not change FOPEN so most routines will not need
                to change the calling list.  I have to really want the new field to use it.
  23 Apr 13 -- Came across a bug in the FRDTXLN logic.  If the file has a single line and there is no EOL
                character, the routine used to detect an EOF condition and immediately exited.  Fixed.
*)

  FROM SysClock IMPORT DateTime;
  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts (*drive path name extension*), FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle,
    MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary,
    AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName,
    CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock,
    WriteBlock, (* ReadChar, *)WriteChar, PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength,
    GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList,
    FindInOSPathList, ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
  FROM SYSTEM IMPORT ADDRESS,ADR,WORD;
  IMPORT Terminal, MiscM2, FileFunc, Strings, STextIO;
  FROM MiscM2 IMPORT ReadString, WriteCard;
  FROM Terminal IMPORT Read, WriteString, WriteLn, ReadChar, Write, Reset;
(*  FROM STextIO IMPORT WriteString, WriteLn, ReadChar; *)
(*  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard; *)
  FROM Environment IMPORT GetCommandLine;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ,CR,LF,MAXCARD,MAXINT,MININT,BUFTYP,ESC,
    NULL,BLANK,STRTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETTKN,GETTKNSTR;

(*
 NOW ALL THIS IS DECLARED IN THE DEFINITION MODULE, SO IT IS COMMENTED OUT.

CONST
    IOBUFSIZ = 10 * 1024;
    IOBUFSIZMAX = IOBUFSIZ + BUFSIZ + 1;
    EOFMARKER = 032C; /* CHR(26) OR CTRL-Z */
    DRIVESEP  = ':';
    SUBDIRSEP = '\';
TYPE
  IOSTATE = (RD,WR,APND);
  IOBUFTYP = ARRAY [1..IOBUFSIZMAX] OF CHAR;
  MYFILTYP = RECORD
    FILE                      : File;
    IOBUF                     : POINTER TO IOBUFTYP;
    NXTPOSN,LASTPOSN,BYTESRED : CARDINAL;
    FILENAME                  : STRTYP;
    LineTermCH                : CHAR;
    iostate                   : IOSTATE;
  END/*RECORD*/;
*)

CONST
      PracticalLineLmt = 80;  (* Used by ChopIntoLines rtn. *)

VAR
  I                       : INTEGER;
  BLANKBUF,TMPBUF,ROOTNAM : BUFTYP;
(*  MOREFNM                 : BOOLEAN;   USED FOR GETFNM *)

PROCEDURE RETBLKBUF(CNT : WORD; VAR OUT BUF : BUFTYP);
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
  C := ORD(CNT);  (* USE OF CARDINAL TYPE TRANSFER FUNCTION *)
  BUF := BLANKBUF;
  IF C > BUFSIZ THEN  (* Can't be < 0 because it is a Cardinal *)
    WriteString(' **ERROR** Range error from RETBLKBUF.  CNT = ');
    WriteCard(C);
    WriteString('.  CNT made to be zero.');
    WriteLn;
    C := 0;
  END(*IF*);
  BUF.COUNT := C;
END RETBLKBUF;

PROCEDURE FWRBL(VAR INOUT F : MYFILTYP; CNT : WORD);
(*
******************************* FWRBL ***********************************
Fast file WRite Blanks.
This routine is used to write CNT number of blanks to file F.

INPUT FROM GBLVAR'S : BLANK, BUFSIZ
*)

VAR C   : CARDINAL;
    BUF : BUFTYP;

BEGIN
  C := ORD(CNT);  (* USE OF CARDINAL TYPE TRANSFER FUNCTION *)
  BUF := BLANKBUF;
  IF C > BUFSIZ THEN  (* Can't be < 0 because it is a Cardinal *)
    WriteString(' **ERROR** Range error from FWRBL.  CNT = ');
    WriteCard(C);
    WriteString('.  CNT made to be zero.');
    WriteLn;
    C := 0;
  END(*IF*);
  BUF.COUNT  := C;
  BUF.LENGTH := C;
  FWRTX(F,BUF);
END FWRBL;

PROCEDURE EXTRACTDRVPTH(BUF:BUFTYP; VAR OUT DRVPATH:BUFTYP);
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

PROCEDURE FWRLN(VAR INOUT F : MYFILTYP);
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

PROCEDURE FWRLF(VAR INOUT F : MYFILTYP);
(*
************************* FWRLF *********************************************
File WRite <lf>
THIS ROUTINE WILL WRITE A <LF> character, to convert from DOS to Linux.
*)

VAR B : BUFTYP;

BEGIN
  WITH F DO
    IOBUF^[NXTPOSN] := LF;
    INC(NXTPOSN);
  END(*WITH*);
END FWRLF;

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


PROCEDURE FRESET(VAR INOUT F:MYFILTYP; FILNAM:BUFTYP; RDWRSTATE:IOSTATE; HaltOnError:BOOLEAN);
(*
************************ FRESET **************************************
File RESET ROUTINE.
THIS PROCEDURE OPENS THE FILE FOR I/O, USING THE LOGITECH PROCEDURES LOOKUP
AND RESET.  FILNAM IS THE FILE NAME AS BUILT BY GETFNM.  UNLIKE THE PASCAL
PROCEDURE OF THE SAME NAME, THIS ROUTINES OPENS AND RESETS FILES FOR BOTH
READING AND WRITING.  RDWRSTATE IS EITHER RD FOR OPEN A FILE FOR READING,
OR WR FOR OPEN A FILE FOR WRITING.
     Stony Brook M2 doesn't use Lookup and Reset.  FileFunc module used instead.
New HaltOnError boolean added.  If false, then the F.status field needs to be checked to see
if any errors occurred.  If true, then the processing is the same as the old way.
*)

VAR I,RETCOD : CARDINAL;
    EOFFLG   : BOOLEAN;
    fileError: CommonFileErrors;
    filelength : CARDINAL32;

BEGIN
  WITH F DO
(* ALLOCATE I/O BUFFER IN HEAP *)
(*    ALLOCATE(IOBUF,TSIZE(IOBUFTYP));*)
    NEW(IOBUF);
    iostate := RDWRSTATE;

    CASE RDWRSTATE OF
      RD : OpenFile(FILE,FILNAM.CHARS,ReadOnlyDenyWrite);
    | WR : CreateFile(FILE,FILNAM.CHARS);  (*This proc truncates file before creation*)
    | APND : OpenCreateFile(FILE,FILNAM.CHARS,ReadWriteDenyWrite);
    END(*CASE*);
    IF HaltOnError AND (FILE.status <> 0) THEN
      WriteString(' Error in opening/creating file ');
      WriteString(FILNAM.CHARS);
      WriteString('--');
      CASE TranslateFileError(FILE) OF
        FileErrFileNotFound : WriteString('File not found.');
      | FileErrDiskFull : WriteString('Disk Full');
      ELSE
        WriteString('Nonspecific error occured.');
      END(*CASE*);
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
      HALT;
    END(*IF*);
    EOFFLG := FILE.eof;

(*  RESET no longer applies at all in SBM2.  Used to APPLY TO READ'S AND WRITE'S, UNLIKE IN PASCAL.
    Reset(FILE);
*)

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
(*      SetRead(FILE); *)
(*       FREAD(F,EOFFLG);   Ignore the EOFFLG result *)
    ELSE (* writing to new file or appending to old file *)
      NXTPOSN := 1; (* START FWRTXLN OR FWRTX AT BEGINNING OF BUFFER. *)
      IF RDWRSTATE = APND THEN
        filelength := FileLength(FILE);
(*        INC(filelength); *)
        MoveFilePos(FILE,filelength);
      END(*IF APND*);

    END(*IF RD*);
  END(*WITH*);
END FRESET;

PROCEDURE FOPEN(VAR INOUT F:MYFILTYP; FILNAM:BUFTYP; RDWRSTATE:IOSTATE);
BEGIN
        FRESET(F, FILNAM, RDWRSTATE, TRUE);
END FOPEN;


PROCEDURE FREAD(VAR F:MYFILTYP; VAR EOFFLG:BOOLEAN);
(*
****************************** FREAD *************************************
File READ.
This procedure will read the proper number of characters into the IOBUF.
I am not changing this proc because it is no longer used.  I now read 
character-by-character in FRDTXLN using ReadChar.
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
    ReadBlock(FILE,ADR(IOBUF^[C]),IOBUFSIZ);
    BYTESRED := FILE.count;
    IF FILE.status <> 0 THEN
      WriteString(' Error in reading from file ');
      WriteString(FILENAME);
      WriteString('--');
      WriteString('Call error.  Either file was in Write mode, already closed,');
      WriteLn;
      WriteString('Nonspecific error.  Read not done.');
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
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
  END/*WHILE*/;
*)
    LASTPOSN := SCANBACK(ADR(IOBUF^[1]),BYTESRED,LineTermCH,TRUE);
    IF LASTPOSN = 0 THEN (* LineTermCH not found *)
      LASTPOSN := BYTESRED;
    END(*IF*);
  END(*WITH*);
END FREAD;

PROCEDURE FRDTXLN(VAR INOUT F:MYFILTYP; VAR OUT BUF:BUFTYP; LNMAX:CARDINAL);
(*
****************************** FRDTXLN *******************************
File READ TEXT LINE.
This routine functions like its slower counterpart, only it gets its
text line from the I/O buffer and not from the current input device.
Like its slower counterpart, this routine more closely approximates
the read of FORTRAN, with its reading an entire line, and testing for
the END-OF-FILE condition.

Will now read character by character and let the OS do the buffering.

F  --THE FILE VARIABLE FOR THE FILE TO WHICH DATA IS TO BE WRITTEN.
BUF--THE BUFFER OF CHARS TO BE WRITTEN TO FILE F.
LNMAX-- Max amt of chars allowed per text line buffer.
EOFFLG--FLAG INDICATING WHETHER THE END OF FILE HAS BEEN FOUND.
*)
VAR
        C : CARDINAL;
        ch: CHAR;

BEGIN
  WITH F DO
    IF FILE.eof THEN RETURN; END(*IF*);
    BUF := BLANKBUF;
    IF (LNMAX = 0) OR (LNMAX >= BUFSIZ) THEN LNMAX := BUFSIZ END(*IF*);
    C := 1;
    ReadLoop: REPEAT
      ch := FileFunc.ReadChar(F.FILE);
      IF FILE.eof AND (C <= 1) THEN BREAK ReadLoop; END;
      IF FILE.status <> 0 THEN
        WriteString(' Error in reading from ');
        WriteString(FILENAME);
        WriteString(' file. Error code = ');
        WriteCard(FILE.status);
        WriteString('.  Read not done.');
        WriteLn;
        WriteString(' Program Terminated.');
        WriteLn;
        RETURN;
      END(*IF status error condition *);
      IF ch = EOL THEN
        BREAK ReadLoop
      ELSE
        Strings.Append(ch,BUF.CHARS);
        INC(C);
      END (* if ch = eolmarker *)

    UNTIL (C > LNMAX) OR F.FILE.eof;

    WITH BUF DO
      COUNT := C - 1;
      LENGTH := Strings.Length(CHARS); (* Just to see if COUNT = LENGTH during debugging *)
    END(* WITH BUF *);
  END(* WITH F *);
END FRDTXLN;



(*  Not fully written and I don't think I need it anymore since this was written as a kludge
    but I wrote a separate file reading rtn in qfx2xls.
PROCEDURE ChopIntoLines(VAR INOUT F:MYFILTYP; VAR OUT BUF:BUFTYP; VAR OUT EOFFLG:BOOLEAN);
(*
****************************** ChopIntoLines *******************************
File READ TEXT Chopped Into LINEs.
This routine functions like its faster counterpart FRDTXLN, only it gets its
text line 1 character at a time from the I/O buffer.
I had to create this because .qfx files do not have line breaks so I am artificially
breaking the line when '><' appears, that is, one HTML code string is adjacent to another.


F  --THE FILE VARIABLE FOR THE FILE TO WHICH DATA IS TO BE WRITTEN.
BUF--THE BUFFER OF CHARS TO BE WRITTEN TO FILE F.
LNMAX-- Max amt of chars allowed per text line buffer.  Not needed as a param for this rtn as it will always be the max.
EOFFLG--FLAG INDICATING WHETHER THE END OF FILE HAS BEEN FOUND.
*)
VAR
   LnPosn,LNMAX : CARDINAL;
   EOL          : BOOLEAN;
   ch1,ch2      : CHAR;

BEGIN
  LNMAX := 0;
  EOL := FALSE;
  IF EOFFLG THEN RETURN; END(*IF*);
  EOFFLG := FALSE;
  IF (LNMAX = 0) OR (LNMAX > BUFSIZ) THEN LNMAX := BUFSIZ END(*IF*);

  REPEAT
    I have not defined C here yet, but I decided to take this rtn out anyway.  See above comment.
  UNTIL (C >= LNMAX) OR  CHAR is EOLMARKER OR '>' OR EOF condition is TRUE;  (* This will not read '<' since it stops reading at '>' *)
    LnPosn := PracticalLineLmt;
    COPYLEFT(ADR(F.IOBUF^[F.NXTPOSN]),ADR(BUF.CHARS),LnPosn);

    INC(F.NXTPOSN,LnPosn);
    (* There is an off by 1 error on NXTPOSN probably introduced by 1-origin array nature of STRTYP *)

(*
Here is to deal w/ chopping up a line at >< point, or at <CR><LF>.  Some .qfx, like from Citibank, do not have <cr><lf>
*)
    ch1 := BUF.CHARS[LnPosn];
    ch2 := F.IOBUF^[F.NXTPOSN];
    LOOP
        IF (F.NXTPOSN >= F.LASTPOSN) THEN EXIT
        ELSIF (ch1 = '>') AND (ch2 = '<' ) THEN EXIT
      ELSIF (ch1 = F.LineTermCH) OR (ch2 = ASCII.lf) THEN EXIT
      END;
      INC(LnPosn);  (* This is a pre incremented pointer *)
      BUF.CHARS[LnPosn] := F.IOBUF^[F.NXTPOSN];
      INC(F.NXTPOSN);  (* Post incrementing this pointer corrects for the off by 1 error *)
      ch1 := BUF.CHARS[LnPosn];
      ch2 := F.IOBUF^[F.NXTPOSN];
    END; (* NXTPOSN,  LASTPOSN, ch1, ch2 *)

    BUF.CHARS[LnPosn+1] := NULL;
    BUF.COUNT := LnPosn;
    BUF.LENGTH := BUF.COUNT;
END ChopIntoLines;
*)
PROCEDURE FPURGE(VAR INOUT F : MYFILTYP);
(*
**************************** FPURGE *************************************
File PURGE.
This routine will unconditionally write the IOBUF out to disk.

*)
VAR IGNORE : CARDINAL;

BEGIN
  WITH F DO
    WriteBlock(FILE,IOBUF,NXTPOSN-1);
    IF FILE.status <> 0 THEN
      WriteString(' Error in writing to file ');
      WriteString(FILENAME);
      WriteString('--');
      CASE TranslateFileError(FILE) OF
        FileErrFileNotFound : WriteString('File not found.');
      | FileErrNotReady : WriteString('File not ready.');
      | FileErrDiskFull : WriteString('Disk full.');
      | FileErrWriteProtect : WriteString('Write protected.');
      ELSE
        WriteString('Nonspecific error occured.  Write not done.');
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

PROCEDURE FWRTX(VAR INOUT F : MYFILTYP; BUF : BUFTYP);
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

PROCEDURE FWRTXLN(VAR INOUT F : MYFILTYP; BUF : BUFTYP);
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

PROCEDURE FWRSTR(VAR INOUT F : MYFILTYP; CHARRAY : ARRAY OF CHAR);
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

PROCEDURE FCLOSE(VAR INOUT F : MYFILTYP);
(*
******************************* FCLOSE **************************************
File CLOSE.
THIS ROUTINE WILL CLOSE THE FILE INPUT AS ITS FORMAL PARAMETER.  IF THE
BUFFER IS A WRITE BUFFER, THE BUFFER WILL BE WRITTEN BEFORE CLOSURE.
*)

VAR I,BYTESWRITTEN,ERR : CARDINAL;
    FH                 : WORD;
    a                  : ADDRESS;

BEGIN
  WITH F DO
    IF (iostate <> RD) AND (NXTPOSN > 1) THEN
      FPURGE(F);
    END(*IF*);
    CloseFile(FILE);
    DISPOSE(IOBUF);
  END(*WITH*);
END FCLOSE;

PROCEDURE COPYDPIF(INFNAM : BUFTYP; VAR OUT OUTFNAM : BUFTYP);
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

PROCEDURE FAPPEND(VAR INOUT F:MYFILTYP; FILNAM:BUFTYP);
(*
*********************** FAPPEND *****************************************
File APPEND.
This procedure will open the file given in FILNAM as an output file, and
whatever is written to this file is appended to the file, instead of
overwriting it.
*)
VAR
  METHOD,FPTRHI,FPTRLO,ERRCOD : CARDINAL;
  FL                          : CARDINAL32;

BEGIN
  FRESET(F,FILNAM,APND,TRUE);
  WITH F DO
    FL := FileLength(FILE);
    IF FL=0 THEN RETURN END;  (* This means file didn't exist before and is same as simple write.*)
    SetFilePos(FILE,FL);
    IF FILE.status = 0 THEN RETURN END;
    CASE TranslateFileError(FILE) OF
      FileErrFileNotFound : WriteString('File not found.');
    ELSE
      WriteString('Nonspecific error occured after SetFilePos call.');
    END(*CASE*);
    WriteLn;
    HALT;
  END(*WITH*);
END FAPPEND;

PROCEDURE GETFNM(PROMPT,NAMDFT,TYPDFT : BUFTYP; VAR OUT FILNAM : BUFTYP);
(*
********************************** GETFNM *******************************
GET FILE NAME SUBROUTINE.
THIS ROUTINE WILL GET A FILE NAME FROM THE TERMINAL, AND ALLOW DEFAULTS
FOR THE ENTIRE FILE NAME AND/OR THE EXTENSION.
PROMPT--THIS IS THE PROMPT TO BE PRINTED TO THE TERMINAL BEFORE THE
      FILENAME IS REQUESTED.  IT MUST HAVE A COLON, : , AS ITS LAST
      CHARACTER TO OPERATE NORMALLY.
NAMDFT--THIS IS THE NAME DEFAULT STRING THAT HAS THE FILE NAME THAT WILL
      BE USEDIF NO INPUT NAME IS ENTERED.  THIS MUST HAVE THE .EXT AS
      WELL AS THE NAME PART; THE TYPDFT WILL NOT BE APPENDED TO THE
      DEFAULT NAME.
TYPDFT--IS A 4 BYTE ARRAY THAT IS THE DEFAULT TYPE (EXTENSION) OF THE
      FILE.  IF A DOT IS NOT PRESENT IN THE INPUT CHARACTER STRING,
      THIS ARRAY WILL BE APPENDED TO THE INPUT STRING TO YIELD THE
      FULL FILENAME.
FILNAM--IS THE OUTPUT FILENAME THAT IS A COMBINATION OF THE INPUT
    STRING, THE TYPE DEFAULT, AND THE NAME DEFAULT, AS IS APPROPRIATE.

FUNCTION VALUE RETURNED WILL BE FALSE IF THE DEFAULT NAME WAS USED, I.E.,
NO NAME WAS INPUT FROM TERMINAL.  FUNCTION VALUE WILL BE TRUE OTHERWISE.
  IF A NAME IS INPUT WITHOUT AN EXTENSION, BEFORE THE DEFAULT EXTENSION IS
APPENDED, IT IS STORED IN THE ROOTNAM BUFFER.  WHEN NO STRING IS INPUT, THIS
NAME WITH THE DEFAULT EXTENSION APPENDED IS USED BEFORE THE FULL DEFAULT NAME
IS.

INPUT FROM GBL VAR'S:  NULL, BLANK.

6 Mar 11
Doesn't work as intented anymore.  If I remember correctly, the complex logic is to deal w/ Logitech
library where a read would read 1st from the command line before taking input from a prompt.
That hasn't been true for 20 yrs or so, and only applied in DOS days.  I'll re-write it to be much simpler.
*)

CONST
  DOT = '.';
  COLON = ':';
  NAMSIZ = 80;
  PROMPTSIZ = 30;
  TYPSIZ = 4;

VAR
  ERRFLG, DOTFLG, NeedExt           : BOOLEAN;
  I, J, RETCOD, c                   : CARDINAL;
  DFTLEN, PROMPTLEN, STRLEN, NAMLEN : CARDINAL;
  B,TOKEN                           : BUFTYP;
  SUM                               : INTEGER;

BEGIN
  NeedExt := TRUE;
  NAMLEN := 0;

    ERRFLG := FALSE;
    WriteString('[ ');
    WriteString(NAMDFT.CHARS);
    WriteString(' ]  ');
    WriteString(PROMPT.CHARS);
    B := BLANKBUF;
    FILNAM := BLANKBUF;
    ReadString(B.CHARS);
    WriteLn;
    TRIM(B);

    I := 1;
    WHILE (I <= B.COUNT) AND NeedExt DO
      NeedExt := B.CHARS[I] <> DOT;
      INC(I);
    END;

    IF B.LENGTH = 0 THEN
      IF ROOTNAM.LENGTH = 0 THEN (* ROOTNAM IS NULL, SO CONTINUE WITH USING THE FULL DEFAULT NAME. *)
        FILNAM := NAMDFT;
        NeedExt := FALSE;  (* So don't append the ext to the filename *)
      ELSE (* ROOTNAM is not null so there must be a filename stem there *)
        FILNAM := ROOTNAM;
        NeedExt := TRUE; (* Need to append the ext to the filename, but this flag is not tested again. *)
        Strings.Append(TYPDFT.CHARS,FILNAM.CHARS);
        TRIM(FILNAM);
      END(*IF*);
    ELSIF NeedExt THEN (* TMPBUF is not null so there is something in there to deal with. *)
      ROOTNAM := B;   (* STORE NAME WITHOUT EXTENSION, overwriting if there was one there before. *)
      FILNAM := B;
      IF FILNAM.LENGTH+TYPSIZ > NAMSIZ THEN         (* APPEND TYPE SINCE NO TYPE INPUT *)
        WriteString('NAME TOO LONG AFTER APPENDING TYPE. TRY AGAIN.');
        WriteLn;
        ERRFLG := TRUE;
        RETURN;
      ELSE   (*NO ERROR*)
        Strings.Append(TYPDFT.CHARS,FILNAM.CHARS);
        TRIM(FILNAM);
      END(*IF*)
    ELSE (* TMPBUF not null and does not need an extension *)
        FILNAM := B;
    END(*IF*);

END GETFNM;

(**************************************** Main Module Body *****************)
BEGIN  (* INITIALIZE BLANKBUF FOR RETBLKBUF *)
  FILLCHAR(ADR(BLANKBUF.CHARS),BUFSIZ,BLANK);
(*  FOR I := 1 TO BUFSIZ DO BLANKBUF.CHARS[I] := BLANK; END(*FOR*); *)
  BLANKBUF.CHARS := '';
  BLANKBUF.LENGTH := 0;
  BLANKBUF.COUNT  := 0;
  ROOTNAM := BLANKBUF;
  TMPBUF := BLANKBUF;
END MyFIO2.
