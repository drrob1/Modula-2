MODULE xdsRawFileIO;

IMPORT  SeqFile, STextIO, SWholeIO, IOResult,(*TextIO, WholeIO,*) RawIO;
TYPE file = RECORD (* Based on SeqFile types and procedures *)
              handle : SeqFile.ChanId;
              flags  : SeqFile.FlagSet;
              name   : ARRAY [0..255] OF CHAR;
              openRes: SeqFile.OpenResults;
              readRes: IOResult.ReadResults;
              eof    : BOOLEAN;
            END;
VAR
  out,in  : file; (* handle, flags, name, openRes, readRes, eof *)
  i,c     : CARDINAL;
(*  str     : ARRAY [0..255] OF CHAR; *)
  str5    : ARRAY [1..5] OF CHAR;
BEGIN
  out.flags := SeqFile.raw + SeqFile.old;
  out.name := 'rawseqfile.txt';
  str5 := 'Hello';
  SeqFile.OpenWrite(out.handle,out.name,out.flags,out.openRes);  (* write flag is implied *)
  IF out.openRes = SeqFile.opened THEN
    FOR i:=0 TO 9 DO
      RawIO.Write(out.handle,str5);
      out.readRes := IOResult.ReadResult(out.handle);
    END;
    SeqFile.Close(out.handle);
    STextIO.WriteString(' Open and write successful.');
    STextIO.WriteLn;
  ELSE
    STextIO.WriteString("Open error");
    STextIO.WriteLn;
    c := ORD(out.openRes);
    STextIO.WriteString(' OpenResult, as a cardinal, is: ');
    SWholeIO.WriteCard(c,10);
    STextIO.WriteLn;
  END;
  in.name := out.name;
  in.flags := out.flags;
  str5 := '';
  SeqFile.OpenRead(in.handle,in.name,in.flags,in.openRes);  (* read flag is implied *)
  IF in.openRes = SeqFile.opened THEN
    STextIO.WriteString(' Open for Reading successful.');
    STextIO.WriteLn;
    FOR i:=0 TO 9 DO
      RawIO.Read(in.handle,str5);
      in.readRes := IOResult.ReadResult(in.handle);
      in.eof := (in.readRes = IOResult.allRight);
      STextIO.WriteString(str5);
      STextIO.WriteLn;
    END;
    SeqFile.Close(in.handle);
    STextIO.WriteString(' Fixed Read finished.');
    STextIO.WriteLn;
  ELSE
    STextIO.WriteString("Open error");
    STextIO.WriteLn;
    c := ORD(in.openRes);
    STextIO.WriteString(' res Field result, as a cardinal, is: ');
    SWholeIO.WriteCard(c,10);
    STextIO.WriteLn;
  END;
  str5 := '';
  SeqFile.OpenRead(in.handle,in.name,in.flags,in.openRes);  (* read flag is implied *)
  IF in.openRes = SeqFile.opened THEN
    STextIO.WriteString(' 2nd open for Reading successful.');
    STextIO.WriteLn;
    c := 0;
    LOOP
      RawIO.Read(in.handle,str5);
      in.readRes := IOResult.ReadResult(in.handle);
      in.eof := (in.readRes = IOResult.endOfInput);
      IF in.eof THEN EXIT END;
      INC(c);
      STextIO.WriteString(str5);
      STextIO.WriteLn;
    END; 
    SeqFile.Close(in.handle);
    STextIO.WriteString(' 2nd Read finished.');
    STextIO.WriteLn;
    STextIO.WriteString(' number of lines is:');
    SWholeIO.WriteCard(c,10);
    STextIO.WriteLn;
  ELSE
    STextIO.WriteString("2nd Open status error");
    STextIO.WriteLn;
    c := ORD(in.openRes);
    STextIO.WriteString(' res Field result, as a cardinal, is: ');
    SWholeIO.WriteCard(c,10);
    STextIO.WriteLn;
  END;
(*
TYPE
  ChanFlags = 
  ( readFlag,        (* input operations are requested/available *)
    writeFlag,       (* output operations are requested/available *)
    oldFlag,         (* a file may/must/did exist before the channel is opened *)
    textFlag,        (* text operations are requested/available *)
    rawFlag,         (* raw operations are requested/available *)
    interactiveFlag, (* interactive use is requested/applies *)
    echoFlag);       (* echoing by interactive device on removal of characters from input stream requested/applies *)
  FlagSet = SET OF ChanFlags;
CONST                          (* Singleton values of FlagSet, to allow for example, read + write *)
  read  = FlagSet{readFlag};   (* input operations are requested/available *)
  write = FlagSet{writeFlag}; (* output operations are requested/available *)
  old   = FlagSet{oldFlag};     (* a file may/must/did exist before the channel is opened *)
  text  = FlagSet{textFlag};   (* text operations are requested/available *)
  raw   = FlagSet{rawFlag};     (* raw operations are requested/available *)
  interactive = FlagSet{interactiveFlag}; (* interactive use is requested/applies *)
  echo  = FlagSet{echoFlag};   (* echoing by interactive device on removal of characters from input stream requested/applies *)
TYPE
  OpenResults =        (* Possible results of open requests *)
    (opened,           (* the open succeeded as requested *)
     wrongNameFormat,  (* given name is in the wrong format for the implementation *)
     wrongFlags,       (* given flags include a value that does not apply to the device *)
     tooManyOpen,      (* this device cannot support any more open channels *)
     outOfChans,       (* no more channels can be allocated *)
     wrongPermissions, (* file or directory permissions do not allow request *)
     noRoomOnDevice,   (* storage limits on the device prevent the open *)
     noSuchFile,       (* a needed file does not exist *)
     fileExists,       (* a file of the given name already exists when a new one is required *)
     wrongFileType,    (* the file is of the wrong type to support the required operations *)
     noTextOperations, (* text operations have been requested, but are not supported *)
     noRawOperations,  (* raw operations have been requested, but are not supported *)
     noMixedOperations,(* text and raw operations have been requested, but they are not supported in combination *)
     alreadyOpen,      (* the source/destination is already open for operations not supported in combination with the requested operations *)
     otherProblem);    (* open failed for some other reason *)
TYPE
  ReadResults =
  ( notKnown,     (* no read result is set *)
    allRight,     (* data is as expected or as required *)
    outOfRange,   (* data cannot be represented *)
    wrongFormat,  (* data not in expected format *)
    endOfLine,    (* end of line seen before expected data *)
    endOfInput);  (* end of input seen before expected data *)
*)
  
END xdsRawFileIO.
