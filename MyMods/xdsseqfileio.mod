(*
  ReadResults =
  ( notKnown,     (* no read result is set *)
    allRight,     (* data is as expected or as required *)
    outOfRange,   (* data cannot be represented *)
    wrongFormat,  (* data not in expected format *)
    endOfLine,    (* end of line seen before expected data *)
    endOfInput);  (* end of input seen before expected data *)
*)
MODULE xdsseqfileio;
IMPORT  SeqFile, STextIO, TextIO, SWholeIO, IOResult (*, IOConsts *);
CONST flags = SeqFile.text + SeqFile.old;
      rdflags = SeqFile.read + flags;
      wrflags = SeqFile.write + flags;
TYPE
  strtyp = ARRAY [0..255] OF CHAR;
  str15typ = ARRAY [0..15] OF CHAR;
VAR
  out,in  : SeqFile.ChanId;
  res     : SeqFile.OpenResults;
  i,c     : CARDINAL;
  str     : ARRAY [0..255] OF CHAR;
  rdreslt : IOResult.ReadResults;
  rdresultsArray : ARRAY [IOResult.notKnown..IOResult.endOfInput] OF
                    str15typ;
BEGIN
  rdresultsArray[IOResult.notKnown] := 'Not Known';
  rdresultsArray[IOResult.allRight] := 'All Right';
  rdresultsArray[IOResult.outOfRange] := 'Out of Range';
  rdresultsArray[IOResult.wrongFormat] := 'Wrong Format';
  rdresultsArray[IOResult.endOfLine] := 'End of Line';
  rdresultsArray[IOResult.endOfInput] := 'End of File';


  SeqFile.OpenWrite(out,"seqfile.txt",wrflags,res);  (* write flag is implied *)
  IF res = SeqFile.opened THEN
    FOR i:=0 TO 9 DO
      TextIO.WriteString(out,"Hello");
      TextIO.WriteLn(out);
    END;
    SeqFile.Close(out);
    STextIO.WriteString(' Open and write successful.');
    STextIO.WriteLn;
  ELSE
    STextIO.WriteString("Open error");
    STextIO.WriteLn;
    c := ORD(res);
    STextIO.WriteString(' res Field result, as a cardinal, is: ');
    SWholeIO.WriteCard(c,10);
    STextIO.WriteLn;
  END;
(*
  SeqFile.OpenRead(in,"seqfile.txt",flags,res);  (* read flag is implied *)
  IF res = SeqFile.opened THEN
    STextIO.WriteString(' Open for Reading successful.');
    STextIO.WriteLn;
    FOR i:=0 TO 9 DO
      TextIO.ReadString(in,str);
      TextIO.SkipLine(in);
      STextIO.WriteString(str);
      STextIO.WriteLn;
    END;
    SeqFile.Close(in);
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
*)
  SeqFile.OpenRead(in,"seqfile.txt",rdflags,res);  (* read flag is implied *)
  IF res = SeqFile.opened THEN
    STextIO.WriteString(' open for Reading successful.');
    STextIO.WriteLn;
    c := 0;
    LOOP
      TextIO.ReadRestLine(in,str);
      rdreslt := IOResult.ReadResult(in);
      TextIO.SkipLine(in);
      STextIO.WriteString(str);
      STextIO.WriteString(' IOResult = ');
  (*
      i:= ORD(rdreslt);
      SWholeIO.WriteCard(i,2);
  *)
      STextIO.WriteString(rdresultsArray[rdreslt]);
      STextIO.WriteLn;
      IF rdreslt <> IOResult.allRight THEN EXIT END;
      INC(c);
      IF c>20 THEN EXIT END;
    END; 
    SeqFile.Close(in);
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
(*
  ReadResults =
  ( notKnown,     (* no read result is set *)
    allRight,     (* data is as expected or as required *)
    outOfRange,   (* data cannot be represented *)
    wrongFormat,  (* data not in expected format *)
    endOfLine,    (* end of line seen before expected data *)
    endOfInput);  (* end of input seen before expected data *)
*)
END xdsseqfileio.
