<*/NOWARN:F*>
%IF WIN32 %THEN
    <*/Resource:Free.RES*>
%ELSE
%END

MODULE F;
(*
  REVISION HISTORY
  ----------------
 25 Jun 07 -- Initial Version.
*)

  FROM SYSTEM IMPORT ADR, FUNC;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  IMPORT Strings;
  IMPORT Terminal, BasicDialogs, STextIO;
(* from Def Mod STextIO.  Remember to link as a console ap if use std i/o.
  FROM STextIO IMPORT WriteChar, WriteLn, WriteString;

  PROCEDURE WriteChar(ch : CHAR);
  PROCEDURE WriteLn;
  PROCEDURE WriteString(s : ARRAY OF CHAR);
*)
  IMPORT MiscM2;
  FROM MiscM2 IMPORT WriteCard, CLS, (* WriteString, WriteLn, *) PressAnyKey, Error, WriteInt,
    WriteReal, WriteLongReal, ReadString, ReadCard, ReadLongReal;
  FROM STextIO IMPORT WriteString, WriteLn;
  FROM BasicDialogs IMPORT MessageTypes;
  FROM LongStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
  FROM WholeStr IMPORT ConvResults, StrToCard, CardToStr;
  IMPORT IOChan, ChanConsts,LowLong;
  FROM Environment IMPORT GetCommandLine;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,
    SCANBACK,SCANFWD,COPYLEFT,RMVCHR,ASSIGN2BUF, MRGBUFS, APPENDA2B, CONCATAB2C, INSERTAin2B;
  FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
  FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
  FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
  IMPORT RConversions, LongStr, LongConv, Conversions;
  FROM TOKENPTR IMPORT FSATYP,TKNPTRTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM REALLIB IMPORT AINT,ROUND,AMOD,PWRI,GETCROPNUM;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY,GREG2JUL,JUL2GREG,GETMDY;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT;
  FROM MyFIO IMPORT EOFMARKER,DRIVESEP,SUBDIRSEP,EXTRACTDRVPTH,MYFILTYP,
    IOSTATE,FRESET,FPURGE,FCLOSE,FREAD,FRDTXLN,FWRTX,FWRTXLN,RETBLKBUF,
    FWRSTR,FWRLN,FAPPEND,COPYDPIF,GETFNM,FOPEN;
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
(*
PROCEDURE GetDeviceFreeSpace(spec : ARRAY OF CHAR) : CARDINAL32;
 spec = file spec of any device, file or directory on the device
 returns the amount of free space on the device.
 the device can be a network share.
 if the device has more free space than MAX(CARDINAL32) then
 MAX(CARDINAL32) is returned

PROCEDURE GetDeviceFreeSpaceEx(spec : ARRAY OF CHAR;
                               VAR OUT allocGranularity : CARDINAL32) : LONGCARD;
 spec = file spec of any device, file or directory on the device
 returns the amount of free space on the device.
 the device can be a network share.
 allocGranularity contains the file allocation granularity of the device
*)


CONST
    FreeIcon = '#100';

  VAR
    FINI,GOOD,ok,OK,WR2FILE                             : BOOLEAN;
    C,free32,allocGranularity,GB,MB,KB,
    STRLEN,NON0POSN,NONBLPOSN                           : CARDINAL;
    I,J,SUM                                             : INTEGER;
    L,LJULDATE                                          : LONGINT;
    CH                                                  : CHAR;
    STPPRG,HAVEMATCH                                    : BOOLEAN;
    PROMPT,NAMDFT,TYPDFT,OUTFNAM,INBUF,YRBUF,TOKEN      : BUFTYP;
    X,Y,Z,JULDATE,R                                     : LONGREAL;
    OUTSTR,STR1,STR2,STR3,YEARSTR                       : STRTYP;
    TKNSTATE,CHRSTATE                                   : FSATYP;
    inputline,spec,s64,e64          : ARRAY [0..255] OF CHAR;
    tpv                             : TKNPTRTYP;
    YearConvRslt                    : ConvResults;
    free64                          : LONGCARD;
    GBstr,MBstr,KBstr               : STR10TYP;


PROCEDURE ADDCOMMAS(VAR INOUT STR : ARRAY OF CHAR);
VAR 
  C,PTR,NCOM,NDGTS,NULLPOSN : CARDINAL;
BEGIN
  NDGTS := STRLENFNT(STR);
  PTR := NDGTS - 1;  (* Zero origin array as a param *)
  NCOM := PTR DIV 3;
  NULLPOSN := NDGTS + NCOM;
  IF NULLPOSN > HIGH(STR) THEN
(*
    WriteString(' Cannot add commas because string is too small.');
    WriteLn;
*)
    RETURN;
  END(*IF*);
  WHILE NCOM > 0 DO
    FOR C := 1 TO 3 DO
      STR[PTR+NCOM] := STR[PTR];
      DEC(PTR);
    END(*FOR*);
    STR[PTR+NCOM] := ',';
    DEC(NCOM);
  END(*WHILE*);
  IF NULLPOSN < HIGH(STR) THEN STR[NULLPOSN] := 0C; END(*IF*);
END ADDCOMMAS;


(************************ MAIN ***************************************)
BEGIN

  LOOP
    GetCommandLine(inputline);
    IF LENGTH(inputline) > 0 THEN
      STPPRG := TRUE;
(*
      WriteString(" inputline:");
      WriteString(inputline);
      WriteLn;
*)
      spec[0] := CAP(inputline[0]);
      spec[1] := ':';
      spec[2] := NULL;

    ELSE  (* length = 0 *)
      FOR C := 0 TO 255 DO spec[C] := ' ' END;
      ok := BasicDialogs.PromptString('Device',spec);
      IF NOT ok THEN EXIT END;
      spec[1] := ':';
      spec[2] := NULL;
    END(*if length*);
(*
    WriteString(" spec=");
    WriteString(spec);
    WriteLn;
*)
    free32 := GetDeviceFreeSpace(spec);
    free64 := GetDeviceFreeSpaceEx(spec, allocGranularity);
(*
    WriteString(' free32 = ');
    WriteCard(free32);
    WriteLn;

    WriteString(' allocGranularity = ');
    WriteCard(allocGranularity);
    WriteLn;
    WriteString(' free64 = ');
*)
    KB := free64 DIV 1000;
    MB := KB DIV 1000;
    GB := MB DIV 1000;

    MB := MB MOD 1000;
    KB := KB MOD 1000;
    
    ok := Conversions.CardToStr(KB,KBstr);
    ok := Conversions.CardToStr(MB,MBstr);
    ok := Conversions.CardToStr(GB,GBstr);

    WriteLn;
    WriteString('  ');
    
    IF GB > 0 THEN 
      WriteString(GBstr);
      WriteString(' GB; ');
    ELSIF MB > 0 THEN 
      WriteString(MBstr);
      WriteString(' MB; ');
    ELSIF KB > 0 THEN 
      WriteString(KBstr);
      WriteString(' KB; ');
    END;  (* if *)


    ok := Conversions.LongBaseToStr(free64,10,s64);
    ADDCOMMAS(s64);
    WriteString(s64);
    WriteString(' bytes;    ');
    
    R := LFLOAT(free64);
    RealToEng(R,4,e64);
    WriteString(e64);
    WriteString(' bytes.');


    WriteLn;
    EXIT;
  END(*LOOP*);

(*  PressAnyKey;    Not needed for console ap *)
END F.
