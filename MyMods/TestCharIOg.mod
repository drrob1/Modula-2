MODULE TestCharIOg;
(*
  REVISION HISTORY
  ----------------
  10 Oct 13 -- Testing terminal read char if it does what it should under gm2.
*)

IMPORT StrIO, StdIO, NumberIO, IO;

VAR 
  CH             : CHAR;

BEGIN
  LOOP
      StrIO.WriteString(' Enter a single character for StdIO: ');
      StdIO.Read(CH);
      StdIO.Write(CH); 
      StrIO.WriteString('        decimal value of entered char is: ');
      NumberIO.WriteCard(ORD(CH),0);
      StrIO.WriteLn;
      IF CAP(CH) = 'Q' THEN EXIT; END(*IF*);
      
      StrIO.WriteString(' Enter a single character for IO: ');
      IO.Read(CH);
      IO.Write(CH); 
      StrIO.WriteString('        decimal value of entered char is: ');
      NumberIO.WriteCard(ORD(CH),0);
      StrIO.WriteLn;
      IF CAP(CH) = 'Q' THEN EXIT; END(*IF*);
  END(*LOOP*);
END TestCharIOg.
