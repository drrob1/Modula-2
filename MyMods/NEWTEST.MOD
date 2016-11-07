(*V0=LTD*)
(*V1=FULL*)
MODULE NEWTEST;

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
(*  FROM Storage IMPORT ALLOCATE, DEALLOCATE;*)
  FROM UTILLIB IMPORT BUFSIZ,CR,LF,MAXCARD,MAXINT,MININT,BUFTYP,ESC,
    NULL,BLANK,STRTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM ENVRTNS IMPORT OPNFIL;
(*<FULL
  FROM DosCalls IMPORT DosCall_40h,DosCall_45h,DosCall_3Eh,DosCall_42h;
FULL>*)


CONST
    IOBUFSIZ = 10 * 1024;
    IOBUFSIZMAX = IOBUFSIZ + BUFSIZ + 1;
    EOFMARKER = 032C; (* CHR(26) OR CTRL-Z *)
    DRIVESEP  = ':';
    SUBDIRSEP = '\';

TYPE
  IOBUFTYP = ARRAY [1..IOBUFSIZMAX] OF CHAR;
  IOBUFPTR = POINTER TO IOBUFTYP;
  MYFILTYP = RECORD
    FILE                      : File;
    IOBUF                     : POINTER TO IOBUFTYP;
    NXTPOSN,LASTPOSN,BYTESRED : CARDINAL;
    FILENAME                  : STRTYP;
    LineTermCH                : CHAR;
  END(*RECORD*);
  IOSTATE = (RD,WR);

CONST WrFilorDev = 40H;  (* DOS function to write to a file or device *)

VAR 
  I               : INTEGER;
  IOBUF           : IOBUFPTR;

BEGIN
  NEW(IOBUF);
  DISPOSE(IOBUF);
END NEWTEST.


