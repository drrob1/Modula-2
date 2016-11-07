MODULE TestStrIOg;
(*
  REVISION HISTORY
  ----------------
  10 Oct 13 -- Testing terminal read char if it does what it should under gm2.
*)

IMPORT StrIO, StdIO, NumberIO, IO, MiscStdInOutg;
FROM ASCII IMPORT EOL, nul, bs;

VAR 
  CH             : CHAR;
  string         : ARRAY [1..127] OF CHAR;

PROCEDURE ReadAllChars(VAR s: ARRAY OF CHAR);
(*
  Basically a readstring routine that does not filter out the <esc>
  char.  Seems that the OS already processes backspace so I don't have
  to.
*)
VAR
  c,high : CARDINAL;
  ch     : CHAR;

BEGIN
  high := HIGH(s);
  c := 0;
  ch := '';
  
  WHILE (c <= high) AND (ch <> EOL) DO
    StdIO.Read(ch);
    s[c] := ch;
    INC(c);
  END; (* while there are characters to get and can fit in the string *)

  DEC(c); (* overright EOL with nul *)
  IF c <= high THEN s[c] := nul; END;
END ReadAllChars;
    
    

PROCEDURE WriteOutDecValues(s: ARRAY OF CHAR);
VAR
  len,c : CARDINAL;

  
BEGIN
  len := LENGTH(s);
  FOR c := 0 TO len DO
    NumberIO.WriteCard(ORD(s[c]),0);
    StrIO.WriteString('   ');
  END;
END WriteOutDecValues;

BEGIN
  LOOP
      StrIO.WriteString(' Enter something: ');
      MiscStdInOutg.ReadAllChars(string);
      StrIO.WriteString(string); 
      StrIO.WriteLn;
      StrIO.WriteString(' Decimal value of entered chars: ');
      WriteOutDecValues(string);
      StrIO.WriteLn;
      IF CAP(string[1]) = 'Q' THEN EXIT; END(*IF*);
      
  END(*LOOP*);
END TestStrIOg.
