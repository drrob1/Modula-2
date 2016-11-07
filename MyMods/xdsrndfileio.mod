MODULE xdsrndfileio;
IMPORT  RndFile, STextIO, TextIO, SWholeIO, IOResult (*, IOConsts *);
CONST flags = RndFile.text (* + RndFile.read + RndFile.write *);
VAR
  out,in : RndFile.ChanId;
  res    : RndFile.OpenResults;
  i,c    : CARDINAL;
  str    : ARRAY [0..255] OF CHAR;
  
BEGIN
  RndFile.OpenClean(out,"rndfile.txt",flags,res);  (* write flag is implied.  If not text, then it is raw *)
  IF res = RndFile.opened THEN
    FOR i:=0 TO 9 DO
      TextIO.WriteString(out,"Hello");
      TextIO.WriteLn(out);
    END;
    RndFile.Close(out);
    STextIO.WriteString(' Open and write successful.');
    STextIO.WriteLn;
  ELSE
    STextIO.WriteString("Open error");
    STextIO.WriteLn;
  END;
  RndFile.OpenOld(in,"rndfile.txt",flags,res);  (* read and old flags are implied *)
  IF res = RndFile.opened THEN
    STextIO.WriteString(' Open for Reading successful.');
    STextIO.WriteLn;
    FOR i:=0 TO 9 DO
      TextIO.ReadRestLine(in,str);
      TextIO.SkipLine(in);
      STextIO.WriteString(str);
      STextIO.WriteLn;
    END;
    RndFile.Close(in);
    STextIO.WriteString(' Fixed Read finished.');
    STextIO.WriteLn;
  ELSE
    STextIO.WriteString("Open error");
    STextIO.WriteLn;
    c := ORD(res);
    STextIO.WriteString(' res Field result, as a cardinal, is: ');
    SWholeIO.WriteCard(c,10);
    STextIO.WriteLn;
  END;
  RndFile.OpenOld(in,"rndfile.txt",flags,res);  (* read and old flags are implied *)
  IF res = RndFile.opened THEN
    STextIO.WriteString(' 2nd open for Reading successful.');
    STextIO.WriteLn;
    c := 0;
    LOOP
      TextIO.ReadRestLine(in,str);
      TextIO.SkipLine(in);
      IF  IOResult.ReadResult(in) <> IOResult.allRight THEN EXIT END;
      INC(c);
      STextIO.WriteString(str);
      STextIO.WriteLn;
      IF c>20 THEN EXIT END;
    END; 
    RndFile.Close(in);
    STextIO.WriteString(' 2nd Read finished.');
    STextIO.WriteLn;
    STextIO.WriteString(' number of lines is:');
    SWholeIO.WriteCard(c,10);
    STextIO.WriteLn;
  ELSE
    STextIO.WriteString("2nd Open status error");
    STextIO.WriteLn;
    c := ORD(res);
    STextIO.WriteString(' res Field result, as a cardinal, is: ');
    SWholeIO.WriteCard(c,10);
    STextIO.WriteLn;
  END;
    
END xdsrndfileio.
