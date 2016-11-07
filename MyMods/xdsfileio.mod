MODULE xdsrndfileio

IMPORT  RndFile, STextIO, TextIO;

CONST flags = RndFile.text + RndFile.read + RndFile.write;

VAR
  file: RndFile.ChanId;
  res: RndFile.OpenResults;
  i  : CARDINAL;

BEGIN
  RndFile.OpenOld(file,"example.txt",flags,res);
  IF res = RndFile.opened THEN
    FOR i:=0 TO 9 DO
      TextIO.WriteString(cid,"Hello");
      TextIO.WriteLn(cid);
    END;
    RndFile.Close(file);
  ELSE
    STextIO.WriteString("Open error");
    STextIO.WriteLn;
  END;
END xdsfileio.
