MODULE FTEST6c;
(*
  REVISION HISTORY
  ----------------
  5 Apr 88 -- Modified this file testing module for M2 V 3.03, and started
               using the new COPYDPIF Procedure in FIO.
  7 Apr 89 -- Using UL2 procs instead of the slower UTILLIB counterparts,
               and the updated procedure names (CONBUF renamed to
               ASSIGN2BUF).
  27 Dec 91 -- Converted to M2 V 4.00.
   2 Nov 02 -- Converted to M2 Win v4  Remember to use console mode.
   1 Aug 04 -- Testing FOPEN.
   8 Aug 04 -- Try to use FileFunc procs instead of my own.
   9 Aug 04 -- Here the FileFunc buffers are used.
  11 Feb 09 -- Will read one char at a time, and changed module name to FTEST6a
               And now will use peek, and module is FTEST6b
  13 Feb 09 -- will use the file func's directly as a stream of characthers.  This pgm
                is to test GetQFXToken, and module is FTEST6c

*)
  IMPORT Terminal,MiscM2,DlgShell;
  FROM MiscM2 IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,Error;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  IMPORT RConversions, LongStr, LongConv, WholeStr;
  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts (*drive path name extension*), FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle,
    MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary,
    AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName,
    CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock,
    WriteBlock, ReadChar, WriteChar, PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength,
    GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList,
    FindInOSPathList, ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
  FROM TermFile IMPORT Open, IsTermFile, Close;
  IMPORT BasicDialogs;
  FROM BasicDialogs IMPORT MessageTypes;
  IMPORT Strings,MemUtils;
  FROM UTILLIB IMPORT NULL,CR,BUFSIZ,CTRLCOD,STRTYP,STR10TYP,
    BUFTYP,MAXCARDFNT,
    COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF(*,GETFNM*);
  FROM TOKENPTR IMPORT FSATYP,DELIMCH,INI3TKN,INI1TKN,GETTKN;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FOPEN,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM;
  FROM Environment IMPORT GetCommandLine;

CONST   MenuSep = '|';


TYPE 
  csvORqifORqfxType = (csv,qif,qfx);
  qfxEnumTyp = (org,acctid,banktranlist,stmttrn,dtposted,trnamt,fitID,name,memo,stmttrnend,
                banktranlistend,ledgerbal,balamt,dtasof,ofxend);
  qfxtkntyp = (empty,string,openinghtml,closinghtml,othererror);
  qfxchrtyp = (eol,openangle,closeangle,slash,plain);
  QIFTYP = RECORD
    datestr, numstr, Pstr, Mstr, amtstr : STRTYP;
    m,d,y,num : CARDINAL;
    juldate : LONGINT;
  END;


VAR
  C,K,IDX,PTR,c,RETCOD                         : CARDINAL;
  c32,d32,e32,f32,g32                          : CARDINAL32;
  CH,ans,ch                                    : CHAR;
  FLAG,FLAG2,FLAG3,FLAG4,FLAG5,EOFFLG,stop     : BOOLEAN;
  PROMPT,NAMDFT,TYPDFT,INFNAM,OUTFNAM,
  TMPBUF,NUMBUF,DRVPATH,INBUF,TOKEN            : BUFTYP;
  TKNSTATE                                     : FSATYP;
  I,J                                          : INTEGER;
  INUNT1,OUTUN1                                : File;
  inputline,OpenFileName,InName,OutName        : ARRAY [0..255] OF CHAR;
  InBuf, OutBuf                                : ARRAY [1..8*1024] OF CHAR;
  q,filter : STRTYP;
  qState : qfxtkntyp;

PROCEDURE GetQfxToken(VAR INOUT f:File; VAR OUT qfxtoken:STRTYP; VAR OUT qfxtokenstate:qfxtkntyp; VAR OUT EOFFLG:BOOLEAN);
(*********************************************** GetToken **********************************
This will use the included file operations as I do not want to read by lines.  I want this
as a character stream.  Delimiters are fixed at close angle bracket and EOL.
*********************************************************************************************)
VAR 
	qfxtkn : STRTYP;
	qfxtknstate : qfxtkntyp;
	ch       : CHAR;
	chstate  :qfxchrtyp;

BEGIN
	qfxtkn := '';
	qfxtknstate := empty;
	EOFFLG := FALSE;
	LOOP
	  ch := PeekChar(f);
	  IF f.status > 0 THEN
	  	qfxtoken := '';
	  	qfxtokenstate := othererror;
	  	RETURN;
	  END; (* if file.status is error cond *)
	  IF f.eof THEN
(* must clarify what happens if eof is reached here.  What is ch set to?  Need to make sure that the
   last token or char in file is not ignored *)
	  	chstate := eol;
	  	EOFFLG := TRUE;
	  	EXIT;
	  ELSE 
	  	CASE ch OF 
	      EOL : chstate := eol;
      | '<' : chstate := openangle;
      | '>' : chstate := closeangle;
      | '/' : chstate := slash;
      ELSE
      	chstate := plain;
      END; (* case ch *)
	  END; (* if file.eof *)
	  CASE qfxtknstate OF
	    empty : 
	      CASE chstate OF 
	        plain,slash :
                  qfxtknstate := string;
	                ch := ReadChar(INUNT1);
	                Strings.Append(ch,qfxtkn);
	      | openangle :
	                ch := ReadChar(INUNT1);  (* Swallow ch *)
	                qfxtknstate := openinghtml;
	      | eol :
                  ch := ReadChar(INUNT1); (* Swallow eol *)
	      | closeangle : 
	                MiscM2.Error(' In GetQfxToken.  Empty token got closeangle ch');

        END; (* case chstate in empty *)
    | string :
        CASE chstate OF
          plain,slash :
	                ch := ReadChar(INUNT1);
                  Strings.Append(ch,qfxtkn);
        | eol :
	                ch := ReadChar(INUNT1); (* Swallow EOL ch *)
                  EXIT;
        | openangle : (* openangle char is still avail for next loop iteration *)
                  EXIT;
        | closeangle : 
                  MiscM2.Error(' In GetQfxToken.  String token got closeangle ch');
        END; (* case chstate in string *)
    | openinghtml :
        CASE chstate OF
          plain,openangle :
                  ch := ReadChar(INUNT1);
                  Strings.Append(ch,qfxtkn);
        | slash :
                  ch := ReadChar(INUNT1);
                  IF LENGTH(qfxtkn) = 0 THEN 
                    qfxtknstate := closinghtml
                  ELSE 
                  	Strings.Append(ch,qfxtkn);
                  END;

        | closeangle,eol :
                  ch := ReadChar(INUNT1); (* swallow ch *)
                  EXIT;
        END; (* case chstate in openinghtml *)
    | closinghtml :
	      CASE chstate OF
	        plain,slash,openangle :
	                ch := ReadChar(INUNT1);
	                Strings.Append(ch,qfxtkn);
	      | closeangle,eol :
	                ch := ReadChar(INUNT1); (* swallow ch *)
	                EXIT;
	      END; (* case chstate in closinghtml *)
	  
	  
	  
    ELSE
    	MiscM2.Error(' In GetQfxToken and tokenstate is in ELSE clause of CASE.');
    END (* case qfxtknstate *)
  END; (* loop *)
    
  qfxtoken := qfxtkn;
  qfxtokenstate := qfxtknstate;
	
END GetQfxToken;	

(*********************************************** Main *********************************************)
BEGIN

  Terminal.Reset;
  stop := FALSE;

  filter := 'qfx Files';
  Strings.Append(MenuSep,filter);
  Strings.Append('*.qfx',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('qif Files',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('*.qif',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('comma Text',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('*.csv',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('All',filter);
  Strings.Append(MenuSep,filter);
  Strings.Append('*',filter);
  Strings.Append(MenuSep,filter);

  c := 1;
  DlgShell.ConvertToNulls(MenuSep,filter);
  FLAG := BasicDialogs.PromptOpenFile(OpenFileName,filter,c,'','','Open transaction text file',FALSE);
  IF NOT FLAG THEN
    Error('Could not open file.  Does it exist?');
    HALT;
  END;
  BasicDialogs.MessageBox(OpenFileName,MsgInfo);

  FLAG := BasicDialogs.PromptOpenFile(OutName,'',c,'','','Open output text file',TRUE);
  IF NOT FLAG THEN
    Error('Could not open file.  ');
    HALT;
  END;
  BasicDialogs.MessageBox(OutName,MsgInfo);

  OpenFile(INUNT1,OpenFileName,ReadOnlyDenyWrite);
  IF INUNT1.status <> 0 THEN
    WriteString(' Error in opening/creating file ');
    WriteString(InName);
    WriteString('--');
    CASE TranslateFileError(INUNT1) OF
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
  SetFileBuffer(INUNT1,InBuf);

  OpenCreateFile(OUTUN1,OutName,ReadWriteDenyWrite);
  IF OUTUN1.status <> 0 THEN
    WriteString(' Error in opening/creating file ');
    WriteString(OutName);
    WriteString('--');
    CASE TranslateFileError(OUTUN1) OF
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
  SetFileBuffer(OUTUN1,OutBuf);


  K := 0;
  LOOP (* to read multiple times *)
    GetQfxToken(INUNT1,q,qState,EOFFLG);
    IF EOFFLG THEN EXIT END(*IF*);
    WriteString(' qfxtoken=');
    WriteString(q);
    WriteString(', qfxtoken state is ');
    WriteCard(ORD(qState));
    WriteLn;
    WriteLine(OUTUN1,q);
    PressAnyKey;
    INC(K);
  END(*LOOP*);
  RemoveFileBuffer(INUNT1);
  CloseFile(INUNT1);

  RemoveFileBuffer(OUTUN1);
  CloseFile(OUTUN1);
  WriteLn;
  WriteString(' Number of reads is :');
  WriteCard(K);
  WriteLn;
  PressAnyKey;
END FTEST6c.
