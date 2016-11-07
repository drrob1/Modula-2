MODULE TestRealsg2;
(*
  REVISION HISTORY
  ----------------
  10 Oct 13 -- Testing conversions to eng and float under gm2.
*)

IMPORT LongStr, STextIO, SWholeIO;
FROM ASCII IMPORT EOL, nul, bs;

VAR 
  ch                           : CHAR;
  x,y,z                        : LONGREAL;
  FixedStr,FloatStr,EngStr,Str : ARRAY [0..25] OF CHAR;


BEGIN
  x := 100.0;
  y := 200.0;
  z := 0.1;

  STextIO.WriteString(' 100 : ');
  LongStr.RealToFloat(x,15,FloatStr);
  STextIO.WriteString(FloatStr);
  STextIO.WriteString(', ');
  LongStr.RealToEng(x,15,EngStr);
  STextIO.WriteString(EngStr);
  STextIO.WriteString(', ');
  LongStr.RealToFixed(x,15,FixedStr);
  STextIO.WriteString(FixedStr);
  STextIO.WriteString(', ');
  LongStr.RealToStr(x,Str);
  STextIO.WriteString(Str);
  STextIO.WriteLn;
  STextIO.WriteLn;  

  STextIO.WriteString(' 200 : ');
  LongStr.RealToFloat(y,15,FloatStr);
  STextIO.WriteString(FloatStr);
  STextIO.WriteString(', ');
  LongStr.RealToEng(y,15,EngStr);
  STextIO.WriteString(EngStr);
  STextIO.WriteString(', ');
  LongStr.RealToFixed(y,15,FixedStr);
  STextIO.WriteString(FixedStr);
  STextIO.WriteString(', ');
  LongStr.RealToStr(y,Str);
  STextIO.WriteString(Str);
  STextIO.WriteLn;
  STextIO.WriteLn;  
      
  STextIO.WriteString(' 0.1 : ');
  LongStr.RealToFloat(z,15,FloatStr);
  STextIO.WriteString(FloatStr);
  STextIO.WriteString(', ');
  LongStr.RealToEng(z,15,EngStr);
  STextIO.WriteString(EngStr);
  STextIO.WriteString(', ');
  LongStr.RealToFixed(z,15,FixedStr);
  STextIO.WriteString(FixedStr);
  STextIO.WriteString(', ');
  LongStr.RealToStr(z,Str);
  STextIO.WriteString(Str);
  STextIO.WriteLn;
  STextIO.WriteLn;  
      
  x := 0.01;
  y := 0.001;
  z := 0.0001;

  STextIO.WriteString(' .01 : ');
  LongStr.RealToFloat(x,15,FloatStr);
  STextIO.WriteString(FloatStr);
  STextIO.WriteString(', ');
  LongStr.RealToEng(x,15,EngStr);
  STextIO.WriteString(EngStr);
  STextIO.WriteString(', ');
  LongStr.RealToFixed(x,15,FixedStr);
  STextIO.WriteString(FixedStr);
  STextIO.WriteString(', ');
  LongStr.RealToStr(x,Str);
  STextIO.WriteString(Str);
  STextIO.WriteLn;
  STextIO.WriteLn;  
      
  STextIO.WriteString(' .001 : ');
  LongStr.RealToFloat(y,15,FloatStr);
  LongStr.RealToEng(y,15,EngStr);
  LongStr.RealToFixed(y,15,FixedStr);
  LongStr.RealToStr(y,Str);
  STextIO.WriteLn;
  STextIO.WriteLn;  
      
  STextIO.WriteString(' 0.0001 : ');
  LongStr.RealToFloat(z,15,FloatStr);
  STextIO.WriteString(FloatStr);
  STextIO.WriteString(', ');
  LongStr.RealToEng(z,15,EngStr);
  STextIO.WriteString(EngStr);
  STextIO.WriteString(', ');
  LongStr.RealToFixed(z,15,FixedStr);
  STextIO.WriteString(FixedStr);
  STextIO.WriteString(', ');
  LongStr.RealToStr(z,Str);
  STextIO.WriteString(Str);
  STextIO.WriteLn;
  STextIO.WriteLn;  

END TestRealsg2.
