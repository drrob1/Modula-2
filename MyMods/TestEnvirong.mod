MODULE TestEnvirong;
FROM MiscStdInOutg IMPORT ReadString, WriteString, WriteLn;
FROM Environg IMPORT GetCommandLine;
(* FROM UTILLIBg IMPORT STRTYP; *)

VAR
  commandline : ARRAY [1..256] OF CHAR;

BEGIN
  GetCommandLine(commandline);
  WriteString('Command line tail is: ');
  WriteString(commandline);
  WriteLn;

END TestEnvirong.
