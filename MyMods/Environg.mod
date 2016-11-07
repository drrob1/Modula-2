IMPLEMENTATION MODULE Environg;

FROM Args IMPORT GetArg, Narg;
FROM Strings IMPORT String1, Length, Assign, Delete, Insert, Replace, Append, Concat, Capitalize;
(* FROM UTILLIBg IMPORT STRTYP; *)
FROM MiscStdInOutg IMPORT WriteString, WriteLn, WriteCard, PressAnyKey;
IMPORT ASCII;

VAR
  c,k,n                   : CARDINAL;
  i,j                     : INTEGER;
  cmdlinefrag, argstr     : ARRAY [1..256] OF CHAR;
  null,blank              : String1;
  ok                      : BOOLEAN;

PROCEDURE GetCommandLine(VAR CommandLine : ARRAY OF CHAR);
(* 
  Gets the entire command line as I would get on Stony Brook under Windows.  Linux calls all only get individual arguments which is the
  unix standard as far back as the 70s K&R book on C.  I combine them all here.  GetArg(argstr,0) would get the program invocation name.
*)
BEGIN
  n := Narg();
  k := 1;
  null := '';
  blank := ' ';
  Assign(null,cmdlinefrag);
(*
  WriteString(' In GetCommandLine.  n = ');
  WriteCard(n);
  WriteLn;
  PressAnyKey;
*)
  WHILE k < n DO
    ok := GetArg(argstr,k);
    IF ok THEN
      Append(argstr,cmdlinefrag);
      Append(blank,cmdlinefrag);
      INC(k);
    ELSE (* not ok from the GetArg call *)
     WriteString(' In GetArg loop and it is not ok.  ArgN = ');
     WriteCard(k);
     WriteLn;
    END; (* if ok *)
  END; (* while k < n *)
  Assign(cmdlinefrag, CommandLine);
END GetCommandLine;

END Environg.
