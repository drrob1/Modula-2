IMPLEMENTATION MODULE MyFIO;

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
  20 Oct 02 -- Converted to M2 for win32, DOS mode.  Took out FCLOSEDUP.
   7 Dec 03 -- Had to modify the Import from FileFunc statement.
   2 May 04 -- Forgot to init MOREFNM variable.  Used Terminal procs
                instead of Std I/O.  Removed filename line too long logic.
   1 Aug 04 -- Changed param for WriteBlock because of compiler library change.
                Also decided to add FOPEN
  22 Dec 04 -- Changed GETFNM so it prompts w/ the default filename.
   9 Feb 09 -- Added ChopIntoLines routine as .qfx file is without <CR> or <LF>,
                and the correspond const of PracticalLineLmt
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
  IMPORT Terminal,MiscM2;
  FROM MiscM2 IMPORT ReadString,WriteCard;
  FROM Terminal IMPORT Read, WriteString, WriteLn, Write, ReadChar, Reset;
(*  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard; *)
  FROM Environment IMPORT GetCommandLine;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ,CR,LF,MAXCARD,MAXINT,MININT,BUFTYP,ESC,
    NULL,BLANK,STRTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI1TKN,GETTKN,GETTKNSTR;
(*  FROM ENVRTNS IMPORT OPNFIL; *)

(*
 NOW ALL THIS IS DECLARED IN THE DEFINITION MODULE, SO IT IS COMMENTED OUT.

CONST
    IOBUFSIZ = 10 * 1024;
    IOBUFSIZMAX = IOBUFSIZ + BUFSIZ + 1;
    EOFMARKER = 032C; /* CHR(26) OR CTRL-Z */
    DRIVESEP  = ':';
    SUBDIRSEP = '\';

  IOBUFTYP = ARRAY [1..IOBUFSIZMAX] OF CHAR;
  MYFILTYP = RECORD
    FILE                      : File;
    IOBUF                     : POINTER TO IOBUFTYP;
    NXTPOSN,LASTPOSN,BYTESRED : CARDINAL;
    FILENAME                  : STRTYP;
    LineTermCH                : CHAR;
    iostate                   : IOSTATE;
  END/*RECORD*/;
  IOSTATE = (RD,WR);
*)

CONST WrFilorDev  = 40H;  (* DOS function to write to a file or device *)
      DupFH       = 45H;
      CloseFH     = 3EH;
      MoveFilePtr = 42H;
      PracticalLineLmt = 80;  (* Used by ChopIntoLines rtn. *)

VAR
  I                       : INTEGER;
  BLANKBUF,TMPBUF,ROOTNAM : BUFTYP;
  MOREFNM                 : BOOLEAN;  (* USED FOR GETFNM *)

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
     Stony Brook M2 doesn't use Lookup and Reset.  FileFunc module used instead.

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
    IF FILE.status <> 0 THEN
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
      FREAD(F,EOFFLG);   (* Ignore the EOFFLG result *)
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

PROCEDURE FOPEN(VAR F:MYFILTYP; VAR FILNAM:BUFTYP; RDWRSTATE:IOSTATE);
BEGIN
        FRESET(F, FILNAM, RDWRSTATE);
END FOPEN;


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
    IF F.NXTPOSN > F.LASTPOSN THEN
(*
  I/O buffer is empty.  Read more data.
*)
      FREAD(F,EOFFLG);
      IF EOFFLG THEN RETURN; END(*IF*);
    ELSE
      EOFFLG := FALSE;
    END(*IF*);
    IF (LNMAX = 0) OR (LNMAX > BUFSIZ) THEN LNMAX := BUFSIZ END(*IF*);
    LnPosn := PracticalLineLmt;
    COPYLEFT(ADR(F.IOBUF^[F.NXTPOSN]),ADR(BUF.CHARS),LnPosn);

    INC(F.NXTPOSN,LnPosn);
    (* There is an off by 1 error on NXTPOSN probably introduced by 1-origin array nature of STRTYP *)

(*
Here is to deal w/ chopping up a line at >< point.  We are ignoring <CR><LF> as they do not exist in a .qfx file
*)
    ch1 := BUF.CHARS[LnPosn];
    ch2 := F.IOBUF^[F.NXTPOSN];
    WHILE (F.NXTPOSN < F.LASTPOSN) AND ( (ch1 # '>') OR (ch2 # '<' ) ) DO;
      INC(LnPosn);  (* This is a pre incremented pointer *)
      BUF.CHARS[LnPosn] := F.IOBUF^[F.NXTPOSN];
      ch1 := BUF.CHARS[LnPosn];
      ch2 := F.IOBUF^[F.NXTPOSN+1];
      INC(F.NXTPOSN);  (* Post incrementing this pointer corrects for the off by 1 error *)
    END; (* NXTPOSN,  LASTPOSN, ch1, ch2 *)

    BUF.CHARS[LnPosn+1] := NULL;
    BUF.COUNT := LnPosn;
    BUF.LENGTH := BUF.COUNT;
END ChopIntoLines;

PROCEDURE FPURGE(VAR F : MYFILTYP);
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

PROCEDURE FCLOSE(VAR F : MYFILTYP);
(*
******************************* FCLOSE **************************************
File CLOSE.
THIS ROUTINE WILL CLOSE THE FILE INPUT AS ITS FORMAL PARAMETER.  IF THE
BUFFER IS A WRITE BUFFER, THE BUFFER WILL BE WRITTEN BEFORE CLOSURE.
*)

VAR I,BYTESWRITTEN,ERR : CARDINAL;
    FH                 : WORD;
(*    r                        : REGISTERS; *)
    a                  : ADDRESS;

BEGIN
  WITH F DO
    IF (iostate <> RD) AND (NXTPOSN > 1) THEN
(*
  Either in Write or Append mode, and need to WRITE OUT BUFFER without the EOFMARKER.

      IF NXTPOSN < IOBUFSIZMAX THEN
        IOBUF^[NXTPOSN] := EOFMARKER;
        INC(NXTPOSN);
      END/*IF*/;
  Truncate file by directly writing 0 bytes to overcome the pblm
  that surfaced when ^Z was no longer written out when file was closed.
  That is, of trailing garbage from previous version of file that should be
  overwritten, isn't.  Only a pblm if new file is shorter than old one.
*)
      FPURGE(F);
(* not needed in SBM2
      FH := GETFH(FILE);
      WITH r DO
        AH := WrFilorDev;
        BX := FH;
        CX := 0;
        a  := ADR(I);  /* Doesn't matter because writing 0 bytes */
        DS := a.SEGMENT;
        DX := a.OFFSET;
        INT(21H, r);
        BYTESWRITTEN := AX;
        IF 0 IN FLAGS THEN
          ERR := AX;
        ELSE
          ERR := 0;
        END/*IF*/;
      END/*WITH*/;
*)
(*<FULL
      DosCall_40h(FH,0,ADR(I),BYTESWRITTEN,ERR); (* Write file or device *)
FULL>*)
(*      DOSCALL(WrFilorDev,FH,0,ADR(I),BYTESWRITTEN,ERR);*)
    END(*IF*);
    CloseFile(FILE);
(*    DEALLOCATE(IOBUF,TSIZE(IOBUFTYP));*)
    DISPOSE(IOBUF);
  END(*WITH*);
END FCLOSE;

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
    FL                          : CARDINAL32;
(*    r                           : REGISTERS; *)
 (* FPTR hi & lo are 32 bit file length *)

BEGIN
  FRESET(F,FILNAM,APND);
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
(*
  FH := GETFH(F.FILE);
  METHOD := 2; /* 0=>Rel 2 beg, 1=>rel 2 cur posn, 2=>rel 2 EOF */
/* Move File Pointer to a 32 bit offset of 0,0 relative to the EOF */
  WITH r DO
    AH := MoveFilePtr;
    BX := FH;
    AL := CHR(METHOD);
    CX := 0;       /* InHigh */
    DX := 0;       /* InLow  */
    INT(21H, r);
    FPTRHI := DX;
    FPTRLO := AX;
    IF 0 IN FLAGS THEN
      ERRCOD := AX;
    ELSE
      ERRCOD := 0;
    END/*IF*/;
*)
  END(*WITH*);
(*<FULL
   DosCall_42h(FH,METHOD,0,0,FPTRHI,FPTRLO,ERRCOD);
FULL>*)
(*
  IF ERRCOD > 0 THEN
    WriteString(' Error while trying to append to file ');
    WriteString(FILNAM.CHARS);
    WriteLn;
    WriteString('Program terminated.');
    WriteLn;
    HALT;
  END/*IF*/;
*)
END FAPPEND;

PROCEDURE GETFNM(PROMPT,NAMDFT,TYPDFT : BUFTYP; VAR FILNAM : BUFTYP);
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
*)

CONST
  DOT = '.';
  COLON = ':';
  NAMSIZ = 80;
  PROMPTSIZ = 30;
  TYPSIZ = 4;

VAR
  EOFFLG, ERRFLG, DOTFLG            : BOOLEAN;
  I, J, RETCOD, c                   : CARDINAL;
  DFTLEN, PROMPTLEN, STRLEN, NAMLEN : CARDINAL;
  TMPBUF,TOKEN                      : BUFTYP;
  SUM                               : INTEGER;

BEGIN
  DOTFLG := FALSE;
  NAMLEN := 0;

    ERRFLG := FALSE;
    WriteString('[ ');
    WriteString(NAMDFT.CHARS);
    WriteString(' ]  ');
    WriteString(PROMPT.CHARS);
    FOR J := 1 TO BUFSIZ+1 DO
      TMPBUF.CHARS[J] := BLANK
    END(*FOR*);
    ReadString(TMPBUF.CHARS);
    WriteLn;
    TRIM(TMPBUF);
    INI1TKN(TMPBUF);
    GETTKNSTR(TOKEN,SUM,RETCOD);
    IF RETCOD = 0 THEN
      MOREFNM := (DELIMCH = BLANK);
      FILNAM := TOKEN;
    ELSE
      FILNAM.COUNT := 0;  (* JUST TO MAKE SURE, BUT SHOULD ALREADY BE 0. *)
      MOREFNM := FALSE;
    END(*IF*);
    I := 1;
    WHILE (I <= FILNAM.COUNT) DO (* See if a dot was entered, and count strlen *)
      IF FILNAM.CHARS[I] = DOT THEN DOTFLG := TRUE; END(*IF*);
      INC(I);
    END(*WHILE*);
    FILNAM.LENGTH := I-1;

    IF FILNAM.LENGTH = 0 THEN
      IF ROOTNAM.CHARS[1] = 0C THEN
(* ROOTNAM IS NULL, SO CONTINUE WITH USING THE FULL DEFAULT NAME. *)
(* NULL STRING.  USE DEFAULT. *)

        I := 1;
        FILNAM.LENGTH := NAMDFT.COUNT;
        WHILE I <= FILNAM.LENGTH DO
          FILNAM.CHARS[I] := NAMDFT.CHARS[I];
          INC(I);
        END(*WHILE*);
        DOTFLG := TRUE;
      ELSE
        FILNAM := ROOTNAM;
        DOTFLG := FALSE;
      END(*IF*);
    END(*IF*);
    IF NOT DOTFLG THEN
      ROOTNAM := FILNAM;   (* STORE NAME WITHOUT EXTENSION. *)
      (* APPEND TYPE SINCE NO TYPE INPUT *)
      IF FILNAM.LENGTH+TYPSIZ > NAMSIZ THEN
        WriteString('NAME TOO LONG AFTER APPENDING TYPE. TRY AGAIN.');
        WriteLn;
        ERRFLG := TRUE;
        RETURN;
      ELSE   (*NO ERROR*)
        FOR I := 1 TO TYPSIZ DO
            FILNAM.CHARS[FILNAM.LENGTH + I ] := TYPDFT.CHARS[I]
        END(*FOR*);
        INC(FILNAM.LENGTH, TYPSIZ);
      END(*IF*)
    END(*IF*);
  FILNAM.COUNT := FILNAM.LENGTH;
(* NULL TERMINATE FILNAM STRING *)
  FILNAM.CHARS[FILNAM.COUNT + 1] := NULL;
END GETFNM;

(**************************************** Main Module Body *****************)
BEGIN  (* INITIALIZE BLANKBUF FOR RETBLKBUF *)
  FILLCHAR(ADR(BLANKBUF.CHARS),BUFSIZ,BLANK);
(*  FOR I := 1 TO BUFSIZ DO BLANKBUF.CHARS[I] := BLANK; END(*FOR*); *)
  BLANKBUF.LENGTH := 0;
  BLANKBUF.COUNT  := 0;
  MOREFNM := FALSE;
END MyFIO.
