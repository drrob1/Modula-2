MODULE TestLongIntConversion;
(*
REVISION HISTORY
----------------
 2 Feb 07 -- Modified testing for tokenptr to test problem found w/ LCHINBUF for CitiFilterTW
10 Oct 13 -- Testing gm2 routines.
15 Oct 13 -- Now testing new MiscStdInOutg routines.
15 Oct 13 -- Isolating bug in LongInt conversion.
*)
  IMPORT FpuIO,NumberIO,StrIO;
(*
  NumberIO.IntToStr(i,0,str);
  FpuIO.LongIntToStr(LI,0,str);
  NumberIO.StrToInt(s,i);
  FpuIO.StrToLongInt(s,LI);
*)
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM StrIO IMPORT WriteString, WriteLn, ReadString;

  VAR 
    C,c1,c2,strlen         : CARDINAL;
    LC                     : LONGCARD;
    I                      : INTEGER;
    LI                     : LONGINT;
    s,s1,s2,s3,s4          : ARRAY [0..50] OF CHAR;

BEGIN

  LOOP
    WriteString(' Input line : ');
    ReadString(s);
    strlen := LENGTH(s);
    IF strlen = 0 THEN EXIT; END(*IF*);
    NumberIO.StrToInt(s,I);
    WriteString(' Int: ');
    NumberIO.WriteInt(I,0);
    WriteString(', LongInt: ');
    FpuIO.StrToLongInt(s,LI);
    IF LI = 0 THEN
      WriteString(' 0 as conversion failed.');
    ELSE
      FpuIO.WriteLongInt(LI,0);
    END;  (* LI = 0 *)
    WriteLn;
    
  END; (* reading loop *)  
END TestLongIntConversion.
