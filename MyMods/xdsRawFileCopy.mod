MODULE xdsRawFileCopy;

IMPORT  SeqFile, STextIO, TextIO, SWholeIO, IOResult, WholeIO, Strings,
           RawIO, FileSys, FileName, ProgEnv;
TYPE
     StrTyp = ARRAY [0..255] OF CHAR;
     file = RECORD (* Based on SeqFile types and procedures *)
              handle : SeqFile.ChanId;
              flags  : SeqFile.FlagSet;
              name   : StrTyp;
              openRes: SeqFile.OpenResults;
              readRes: IOResult.ReadResults;
              eof    : BOOLEAN;
            END;
VAR
  src,dest             : file; (* handle, flags, name, openRes, readRes *)
  i,c,argnum           : CARDINAL;
  str1,str2            : StrTyp;
  buf                  : ARRAY [0..1000] OF CHAR;
  fnformat1,fnformat2  : FileName.Format;
  success              : BOOLEAN;
(*-------------------------------------------------------------------------------------------------------*)
PROCEDURE CopyFile(srcname,destname: ARRAY OF CHAR; VAR success:BOOLEAN);

VAR c : CARDINAL;
    done : BOOLEAN;
  
BEGIN
  Strings.Assign(srcname,src.name);
  Strings.Assign(destname,dest.name);
  src.flags := SeqFile.raw;
  dest.flags := src.flags;
  success := FALSE;
  IF FileSys.Exists(dest.name) THEN FileSys.Remove(dest.name,done) END;
  SeqFile.OpenRead(src.handle,src.name,src.flags,src.openRes);  (* read flag is implied *)
  IF src.openRes <> SeqFile.opened THEN RETURN END;
  STextIO.WriteString(' OpenRead successful.');

  SeqFile.OpenWrite(dest.handle,dest.name,dest.flags,dest.openRes);  (* write flag is implied *)
  IF dest.openRes <> SeqFile.opened THEN RETURN END;
  STextIO.WriteString(' OpenWrite successful.');
  STextIO.WriteLn;

  c := 0;
  LOOP
    RawIO.Read(src.handle,buf);
    src.readRes := IOResult.ReadResult(src.handle);
    src.eof := (src.readRes = IOResult.endOfInput);
    IF src.eof THEN EXIT END;
    INC(c);
    RawIO.Write(dest.handle,buf);
    dest.readRes := IOResult.ReadResult(dest.handle);
  END; 
  SeqFile.Close(src.handle);
  SeqFile.Close(dest.handle);

  STextIO.WriteString(' copy finished.');
  STextIO.WriteLn;
  STextIO.WriteString(' number of raw reads is:');
  SWholeIO.WriteCard(c,2);
  STextIO.WriteLn;
  success := TRUE;
END CopyFile;
  
(*---------------------------------------------------------------------*)
BEGIN
  argnum := ProgEnv.ArgNumber();
  IF argnum < 2 THEN 
    STextIO.WriteString('      Source File: ');
    STextIO.ReadToken(str1);
    STextIO.SkipLine;
    STextIO.WriteLn;
    STextIO.WriteString(' Destination File: ');
    STextIO.ReadToken(str2);
    STextIO.SkipLine;
    STextIO.WriteLn;
  ELSE
    ProgEnv.GetArg(1,str1);
    ProgEnv.GetArg(2,str2);
  END;
  FileName.GetFormat(str1,fnformat1);
  FileName.GetFormat(str2,fnformat2);
  IF fnformat1.ok AND fnformat2.ok THEN 
    CopyFile(str1,str2,success);
    IF NOT success THEN
      STextIO.WriteString(' CopyFile did not succeed.');
      STextIO.WriteLn;
    END;
  ELSE
    STextIO.WriteString(' Bad filenames, copy not done.');
    STextIO.WriteLn;
  END;
    
(*
TYPE
  ChanFlags = 
  ( readFlag,         input operations are requested/available
    writeFlag,        output operations are requested/available
    oldFlag,          a file may/must/did exist before the channel is opened
    textFlag,         text operations are requested/available
    rawFlag,          raw operations are requested/available
    interactiveFlag,  interactive use is requested/applies
    echoFlag);        echoing by interactive device on removal of characters from input stream requested/applies
  FlagSet = SET OF ChanFlags;
CONST                           Singleton values of FlagSet, to allow for example, read + write
  read  = FlagSet{readFlag};    input operations are requested/available
  write = FlagSet{writeFlag};   output operations are requested/available
  old   = FlagSet{oldFlag};     a file may/must/did exist before the channel is opened
  text  = FlagSet{textFlag};    text operations are requested/available
  raw   = FlagSet{rawFlag};     raw operations are requested/available
  interactive = FlagSet{interactiveFlag}; interactive use is requested/applies
  echo  = FlagSet{echoFlag};    echoing by interactive device on removal of characters from input stream requested/applies
TYPE
  OpenResults =               Possible results of open requests
    (opened,                  the open succeeded as requested
     wrongNameFormat,         given name is in the wrong format for the implementation
     wrongFlags,              given flags include a value that does not apply to the device
     tooManyOpen,             this device cannot support any more open channels
     outOfChans,              no more channels can be allocated
     wrongPermissions,        file or directory permissions do not allow request
     noRoomOnDevice,          storage limits on the device prevent the open
     noSuchFile,              a needed file does not exist
     fileExists,              a file of the given name already exists when a new one is required
     wrongFileType,           the file is of the wrong type to support the required operations
     noTextOperations,        text operations have been requested, but are not supported
     noRawOperations,         raw operations have been requested, but are not supported
     noMixedOperations,       text and raw operations have been requested, but they are not supported in combination
     alreadyOpen,      the source/destination is already open for operations not supported in combination with the requested operations
     otherProblem);    open failed for some other reason
TYPE
  ReadResults =
  ( notKnown,     no read result is set
    allRight,     data is as expected or as required
    outOfRange,   data cannot be represented
    wrongFormat,  data not in expected format
    endOfLine,    end of line seen before expected data
    endOfInput);  end of input seen before expected data

<*+ M2ADDTYPES *>
DEFINITION MODULE FileSys;
IMPORT SysClock;
PROCEDURE Exists(fname: ARRAY OF CHAR): BOOLEAN;  Returns TRUE, if file "fname" exists.
PROCEDURE ModifyTime(fname: ARRAY OF CHAR; VAR time: LONGCARD; VAR exists: BOOLEAN); Returns file modification time
PROCEDURE SetFileTime(fname: ARRAY OF CHAR; time: LONGCARD); Sets file modification time
PROCEDURE Rename(fname,newname: ARRAY OF CHAR; VAR done: BOOLEAN); Renames file "fname" to "newname"
PROCEDURE Remove(fname: ARRAY OF CHAR; VAR done: BOOLEAN); Removes file "fname"
PROCEDURE FullName(VAR full: ARRAY OF CHAR; name: ARRAY OF CHAR);
PROCEDURE GetCDNameLength(): CARDINAL;
PROCEDURE GetCDName(VAR s: ARRAY OF CHAR);
PROCEDURE SetCD(name: ARRAY OF CHAR): BOOLEAN;

TYPE
  Directory;
  Entry = RECORD
    fileSize: CARDINAL;
    creaTime: SysClock.DateTime;
    modfTime: SysClock.DateTime;
    nameSize: CARDINAL;
    isDir   : BOOLEAN;
    done    : BOOLEAN;
  END;

PROCEDURE OpenDir(VAR dir: Directory; name: ARRAY OF CHAR; VAR entry: Entry);
PROCEDURE NextDirEntry(dir: Directory; VAR entry: Entry);
PROCEDURE CloseDir(VAR dir: Directory);
PROCEDURE GetName(dir: Directory; VAR name: ARRAY OF CHAR);
PROCEDURE CreateDirectory(name: ARRAY OF CHAR): BOOLEAN;
PROCEDURE RemoveDirectory(name: ARRAY OF CHAR): BOOLEAN;

 FAT/UNC specific
PROCEDURE GetDrive(VAR drive: CHAR): BOOLEAN;
PROCEDURE SetDrive(drive: CHAR): BOOLEAN;
PROCEDURE GetDriveCDNameLength(drive: CHAR; VAR len: CARDINAL): BOOLEAN;
PROCEDURE GetDriveCDName(drive: CHAR; VAR dir: ARRAY OF CHAR): BOOLEAN;
PROCEDURE GetLabel(drive: CHAR; VAR label: ARRAY OF CHAR): BOOLEAN;
END FileSys.
======================================================================
DEFINITION MODULE FileName;
   File name procedures.
   File name consists of three parts:
        - path
        - name
        - extensions.
   The given procedures allow to parse file name and to construct
   file name from its parts.

TYPE
  Format = RECORD
    ok: BOOLEAN;                result
    dirPos, dirLen : CARDINAL;  directory position and length
    namePos,nameLen: CARDINAL;  name position and length
    extPos, extLen : CARDINAL;  extension position and length
  END;


PROCEDURE GetFormat(str: ARRAY OF CHAR; VAR f: Format);
 Returns the format of the string.  The values of *Pos, *Len fields are undefined if f.ok=FALSE.
 Get* procedures does not provide conversions (ie. no case chanegs or reductions).
PROCEDURE GetDir (fname: ARRAY OF CHAR; VAR dir: ARRAY OF CHAR);
PROCEDURE GetName(fname: ARRAY OF CHAR; VAR name: ARRAY OF CHAR);
PROCEDURE GetExt (fname: ARRAY OF CHAR; VAR ext: ARRAY OF CHAR);
PROCEDURE Get(fname: ARRAY OF CHAR; VAR dir,name,ext: ARRAY OF CHAR);


PROCEDURE Convert(str: ARRAY OF CHAR; VAR fname: ARRAY OF CHAR); Converts string (dir,name,ext) to file name according to the conventions of the underlying file system.
PROCEDURE ConvertExt(VAR ext: ARRAY OF CHAR);  Converts extension according to the conventions of the underlying file system.
PROCEDURE Length(dir,name,ext: CARDINAL): CARDINAL;  Returns estimated length, which is greater or equal then generated file name length after Create call.
PROCEDURE Create(dir,name,ext: ARRAY OF CHAR; VAR fname: ARRAY OF CHAR);

END FileName.

*)
END xdsRawFileCopy.