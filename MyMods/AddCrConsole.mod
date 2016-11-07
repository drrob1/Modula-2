MODULE AddCrConsole;
(*
  REVISION HISTORY
  ----------------
  5 Apr 88 -- Modified this file testing module for M2 V 3.03, and started
               using the new COPYDPIF Procedure in FIO.
  7 Apr 89 -- Using UL2 procs instead of the slower UTILLIB counterparts,
               and the updated procedure names (CONBUF renamed to
               ASSIGN2BUF).
  27 Dec 91 -- Converted to M2 V 4.00.
   2 Nov 02 -- Converted to M2 Win v4
   7 Mar 11 -- Modified to test MyFIO2.
   8 Mar 11 -- Now used to remove <cr> from a file to convert from dos to linux.
                Must remember that this is a console app, else it crashes w/ a device error.
  15 Aug 11 -- Changed the name to make it clearer that this uses console mode I/O procs.  And does
                not use MyFIO(2) procs.  These have been moved here because of the size limitations of BUFTYP.
  19 Aug 11 -- Now created AddCrConsole.  Cannot have module names containing special characters like - or _
  21 Aug 11 -- Made output ext .win to reflect the output file format.
*)

  FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
  FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
  FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT IOChan, ChanConsts;
  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts (*drive path name extension*), FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle,
    MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary,
    AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName,
    CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock,
    WriteBlock, (* ReadChar, WriteChar, *) PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength,
    GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList,
    FindInOSPathList, (* SupportsUTC,*) ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
  FROM RealIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM TermFile IMPORT Open, IsTermFile, Close;
  IMPORT Strings,FileFunc,ASCII;
(*
  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF;
  FROM TOKENIZE IMPORT FSATYP,DELIMCH,INI3TKN,INI1TKN,GETTKN;

  FROM MyFIO2 IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FOPEN,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FWRLF,FAPPEND,COPYDPIF,GETFNM;
*)
  FROM Environment IMPORT GetCommandLine;

TYPE
    ChanId      = IOChan.ChanId;
    FlagSet     = ChanConsts.FlagSet;
    OpenResults = ChanConsts.OpenResults;
TYPE
  IOSTATE = (RD,WR,APND);


CONST  (* Accepted singleton values of FlagSet *)
  read  = FlagSet{ChanConsts.readFlag}; (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag};(* output operations are requested/available *)
  text  = FlagSet{ChanConsts.textFlag}; (* text operations are requested/available *)
  raw   = FlagSet{ChanConsts.rawFlag};  (* raw operations are requested/available *)
  echo  = FlagSet{ChanConsts.echoFlag}; (* echoing by interactive device on reading of characters from input stream requested/applies *)

VAR
  C,K,IDX,PTR,c,RETCOD                         : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  fs                                           : LONGCARD;  (* file size *)
  CH                                           : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG          : BOOLEAN;
  I,J                                          : INTEGER;
  infile,outfile                               : File; (* FileFunc exported type *)
  innameparts,outnameparts                     : FileNameParts;
  inputline, inname, outname, InBuf, OutBuf    : ARRAY [0..255] OF CHAR;
  LongInputLine                                : ARRAY [1..8*1024] OF CHAR;


PROCEDURE getfilenames;

BEGIN
  GetCommandLine(inputline);
  c := LENGTH(inputline);
(*
  My strategy here is if there is no filename on the cmd line, it will ask for a name.  If blank pgm will halt.
  If not blank, it will use constructfilename to have a default .txt extension.  It will ask for 2nd filename.  If
  this is blank it will use the 1st filename and a default extension of .out.  So the 1st filename needs to be only a root name.
  If there is a name on the command line, pgm assumes this is a root name and uses that root for both input and output
  filenames, using extensions of .txt and .out, respectively.
*)

  IF c = 0 THEN
    WriteString('Enter In/Out File Name : ');
    ReadString(InBuf);
    SkipLine;
    IF LENGTH(InBuf) = 0 THEN HALT; END;
    ConstructFileName(InBuf,'.txt', inname);

    WriteString('Enter Out File Name : ');
    ReadString(OutBuf);
    SkipLine;
    IF LENGTH(OutBuf) = 0 THEN
      ConstructFileName(InBuf,'.win',outname);
    ELSE
        ConstructFileName(OutBuf,'.win',outname);
    END;

  ELSE
        ConstructFileName(inputline,'*.txt',inname);
        ConstructFileName(inputline,'*.win',outname);

  END;

  WriteString('  INPUT FILE : ');
  WriteString(inname);
  WriteLn;
  WriteString(' OUTPUT FILE : ');
  WriteString(outname);
  WriteLn;
(*
  WriteString('  -> Pause');
  ReadChar(CH);
  WriteLn;
  SkipLine;
*)
END getfilenames;

PROCEDURE fopen(VAR INOUT F:File; FILNAM:ARRAY OF CHAR; RDWRSTATE:IOSTATE);
(*
************************ fopen **************************************
File Open.
RDWRSTATE IS EITHER RD FOR OPEN A FILE FOR READING, OR WR FOR OPEN A FILE FOR WRITING.

*)

VAR I,RETCOD : CARDINAL;
    EOFFLG   : BOOLEAN;
    fileError: CommonFileErrors;
    filelength : CARDINAL32;

BEGIN
    CASE RDWRSTATE OF
      RD : OpenFile(F,FILNAM,ReadOnlyDenyWrite);
    | WR : CreateFile(F,FILNAM);  (*This proc truncates file before creation*)
    | APND : OpenCreateFile(F,FILNAM,ReadWriteDenyWrite);
    END(*CASE*);
    IF F.status <> 0 THEN
      WriteString(' Error in opening/creating file ');
      WriteString(FILNAM);
      WriteString('--');
      CASE TranslateFileError(F) OF
        FileErrFileNotFound : WriteString('File not found.');
      | FileErrDiskFull : WriteString('Disk Full');
      ELSE
        WriteString('Nonspecific error occured.');
      END(*CASE*);
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
      HALT;
    END(*IF F.status*);

    IF RDWRSTATE = APND THEN
      filelength := FileLength(F);
      MoveFilePos(F,filelength);
    END(*IF APND*);

END fopen;

PROCEDURE fclose(VAR INOUT F:File);
BEGIN
        FileFunc.CloseFile(F);
END fclose;




PROCEDURE RdWrAllTx;
(*
****************************** RdAllTx *******************************
READ and WRITE ALL TEXT from file
This routine will read from the input file char-by-char and write the same way.
Then I don't have to worry about line lengths.

Will let the OS do the buffering.  It can probably do this better than I anyway.

I'll pass the infile and outfile globally; it now does not need to be params, in fact it doesn't need
any params at all.

*)
VAR
        C : CARDINAL;
        ch: CHAR;

BEGIN
  IF infile.eof OR (infile.status <> 0) OR (outfile.status <> 0) THEN RETURN; END(*IF*);
  fs := 1;
  ReadLoop: LOOP
    ch := FileFunc.ReadChar(infile);
    IF infile.eof THEN RETURN; END;
    IF infile.status <> 0 THEN
      WriteString(' Error in reading from infile.  Error code = ');
      WriteCard(infile.status,0);
      WriteString('.  Read not done.');
      WriteLn;
      WriteString(' Program Terminated.');
      WriteLn;
      RETURN;
    END(*IF status error condition *);
    IF ch = EOL THEN
(* dont need this BREAK statement anymore        BREAK ReadLoop *)
      FileFunc.WriteChar(outfile, ASCII.cr);
      FileFunc.WriteChar(outfile, ASCII.lf);
    ELSE
      FileFunc.WriteChar(outfile, ch);
    END; (* if ch = eolmarker *)
    INC(fs);

  END; (* ReadLoop *)
END RdWrAllTx;

(* ************************* MAIN ***************************************************************)
BEGIN
  inputline := '';
  LongInputLine := '';
  getfilenames;


  fopen(infile,inname,RD);
  fopen(outfile,outname,WR);
  RdWrAllTx;


(*
  LOOP /* to read multiple lines */
    FRDTXLN(INUNT1,INBUF,0);
    IF INUNT1.FILE.eof THEN EXIT END;
    WriteString(INBUF.CHARS);
    WriteLn;
    FWRTX(OUTUN1,INBUF);
    FWRLF(OUTUN1);
/*
    WriteString(' Press <enter> to continue.');
    ReadChar(CH);
    SkipLine;
    IF (CAP(CH) = 'E') OR (CAP(CH) = 'Q') THEN EXIT; END;
    WriteLn;
*/
  END/*LOOP*/;
*)
  fclose(infile);
  fclose(outfile);
  WriteString(' Characters written: ');
  WriteLongCard(fs,0);
  WriteString('.');
  WriteLn;
END AddCrConsole.

(*
    FileNameParts =
        RECORD
            %IF DOS %OR FlashTek %OR PharLap %OR WIN16 %THEN
            drive       : ARRAY [0..127] OF CHAR;
            %ELSIF UNIX %THEN
            drive       : ARRAY [0..0] OF CHAR;
            %ELSE
            drive       : NameString;
            %END
            path        : FileSpecString;
            name        : NameString;
            extension   : NameString;
        END;
   .drive only has meaning on Microsoft platforms this is the device, it is either a logical drive letter or a UNC server name and share.
   .path = this is the path
   .name = the file name  excluding the file extension
   .extension = the file extension, this is everything  after the *last* '.' character.
                On Unix systems this may actually be the full file name since the convension there is to use a preceeding '.' to mark hidden files and/or directories.

PROCEDURE ParseFileName(pathname : ARRAY OF CHAR; VAR OUT parts : FileNameParts);
PROCEDURE AssembleParts(parts : FileNameParts; VAR OUT name : ARRAY OF CHAR);
                                                                    puts the file parts back together into a single string
PROCEDURE ConstructFileName(pri, def : ARRAY OF CHAR; VAR OUT res : ARRAY OF CHAR);

 pri = the primary file spec (device/path/name/extension)
 def = the default file spec (device/path/name/extension)
 the above can have all or none of various path components
 Anything pri does not have is supplied by def if it has the component.  This function uses ParseFileName
   to split pri and def into their respective components.
  Ex:
    ConstructFileName(FileName, '.mod', FileName);
      supplies a default extension. The leading . is necessary
    ConstructFileName(FileName, 'd:\dev\rtl\', FileName);
      supplies a default device and/or path. The trailing PathSepChar is necessary to signify that "rtl"
      is a directory name and not a file name.


*)
