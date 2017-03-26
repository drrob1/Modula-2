<*DEFINE (ConsoleMode,TRUE)*>
(* (C) 1990-2013.  Robert W. Solomon.  All rights reserved.  *)
MODULE RPN;
(*
  This module uses the HPCALC module to simulate an RPN type calculator.
  REVISION HISTORY
  ----------------
   1 Dec 89 -- Changed prompt.
  24 Dec 91 -- Converted to M-2 V 4.00.  Changed params to GETRESULT.
  25 Jul 93 -- Output result without trailing insignificant zeros,
                imported UL2, and changed prompt again.
   3 Mar 96 -- Fixed bug in string display if real2str fails because
                number is too large (ie, Avogadro''s Number).
  18 May 03 -- First Win32 version.  And changed name.
   1 Apr 13 -- Back to console mode pgm that will read from the cmdline.  Intended to be a quick and useful little utility.
                And will save/restore the stack to/from a file.
   2 May 13 -- Will use console mode flag for HPCALC, so it will write to console instead of the terminal module routines.
                And I now have the skipline included in MiscStdInOut so it is removed from here.
   4 Oct 13 -- Will use FormatString to get columns to align better.
  26 Dec 14 -- Added HOL functionality.  And added ABOUT command here.
   5 Jan 15 -- ABOUT command is no longer case sensitive.
   5 Nov 15 -- Recompiled because of changes to HPCALC.
  16 Apr 16 -- undo, redo commands in HPCALC.
   7 Jul 16 -- UP command in HPCALC.  PI added to help, also in HPCALC.
   8 Jul 16 -- Added line to always display the stack using Dump2Console, and added a startup message.
  26 Mar 17 -- HPCALC now outputs a string list instead of directly doing I/O.
*)
  FROM SYSTEM IMPORT ADR;
(*
                            FROM SLWholeIO IMPORT ReadLongInt,WriteLongInt,ReadLongCard,WriteLongCard;
                            FROM SWholeIO IMPORT ReadInt, WriteInt, ReadCard, WriteCard;
                            FROM STextIO IMPORT ReadString, WriteString, WriteLn, ReadChar, WriteChar, SkipLine;
                            FROM RealStr IMPORT StrToReal, RealToFloat, RealToEng, RealToFixed, RealToStr;
                            FROM SysClock IMPORT DateTime,GetClock,CanGetClock,CanSetClock,IsValidDateTime,SetClock;
                            FROM LowLong IMPORT sign,ulp,intpart,fractpart,trunc (*,round*) ;
                            FROM LongMath IMPORT sqrt,exp,ln,sin,cos,tan,arctan,arcsin,arccos,power,round,pi;
                            IMPORT RConversions, LongStr, LongConv, WholeStr (*, FormatString *) ;
                            FROM Conversions IMPORT StringToInt, StrToInt, IntToString, IntToStr, StringToCard,
                               StrToCard, CardToString, CardToStr, StringToLong, StrToLong, LongToString, LongToStr;
                            IMPORT Strings,MemUtils,ASCII;
                            FROM ExStrings IMPORT AppendChar, EqualI;
*)
  IMPORT STextIO, SWholeIO, SLWholeIO;
  FROM RConversions IMPORT RealToString, RealToStringFixed, StringToReal;
  FROM SLongIO IMPORT ReadReal, WriteFloat, WriteEng, WriteFixed, WriteReal;
  FROM Environment IMPORT GetCommandLine;
  FROM UTILLIB IMPORT BLANK,NULL,STRTYP,BUFSIZ,BUFTYP,STR10TYP,TRIM,STRLENFNT,STRCMPFNT,stricmpfnt,
    SCANBACK,SCANFWD,COPYLEFT;
  FROM TKNRTNS IMPORT FSATYP,CHARSETTYP,DELIMCH,INI1TKN,INI3TKN,GETCHR,
    UNGETCHR,GETTKN,NEWDELIMSET,NEWOPSET,NEWDGTSET,GETTKNSTR,GETTKNEOL,
    UNGETTKN,GETTKNREAL;
  FROM REALLIB IMPORT AINT,ROUND,AMOD,PWRI,GETCROPNUM;
  FROM TIMLIB IMPORT JULIAN,GREGORIAN,TIME2MDY;
  FROM HPCALC IMPORT STACKSIZE,PUSHX,READX,GETSTACK,DUMPSTACK,GETRESULT,RealStack,Holidays,PushStacks,
    RollDownStacks,RollUpStacks;
  IMPORT HPCALC, MiscStdInOut, Terminal;
  FROM MiscStdInOut IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,Error;
  FROM FormatString IMPORT FormatString;
  FROM FileFunc IMPORT EOL, FileSpecString, NameString, FileAttributes, FileAttributeSet,
    SearchEntry, FileNameParts (*drive path name extension*), FileTypes, DeviceTypes,
    AccessModes, FileUseInfo, FileUseInfoSet, CommonFileErrors, File, InvalidHandle,
    MustHaveNormalFile, MustHaveDirectory, MustHaveNothing, AllAttributes, StdAttributes,
    AddArchive, AddReadOnly, AddHidden, AddSystem, AddCompressed, AddTemporary,
    AddEncrypted, AddOffline, AddAlias, AddNormalFile, AddDirectory, OpenFile,
    OpenFileEx, CreateFile, CreateFileEx, GetTempFileDirectory, MakeTempFileName,
    CreateTempFile, CreateTempFileEx, OpenCreateFile, OpenCreateFileEx, FakeFileOpen,
    CloseFile, FileType, SetFileBuffer, RemoveFileBuffer, FlushBuffers, ReadBlock,
    WriteBlock, ReadChar, WriteChar, PeekChar, ReadLine, WriteLine, LockFileRegion,
    UnlockFileRegion, SetFilePos, GetFilePos, MoveFilePos, TruncateFile, FileLength,
    GetFileSizes, TranslateFileError, GetFileAttr, SetFileAttr, GetFileDateTime,
    SetFileDateTime, RenameFile, DeleteFile,
    FileExists, CopyFile, SetHandleCount, GetNextDir, ParseFileName, ParseFileNameEx,
    AssembleParts, ConstructFileName, ConstructFileNameEx, FindInPathList,
    FindInOSPathList, ExpandFileSpec, FindFirst, FindNext, FindClose,
    MakeDir, CreateDirTree, DeleteDir, DirExists, RenameDir, GetDefaultPath,
    SetDefaultPath, GetDeviceFreeSpace, GetDeviceFreeSpaceEx, GetDeviceType;
  IMPORT Strings;
  FROM Strings IMPORT Append, Equal, Delete, Concat, Capitalize;
  FROM HolidayCalc IMPORT HolType, GetHolidays;

CONST
  LastCompiled = "26 Mar 2017";

VAR
  C,c,K,STRLEN,NON0POSN,NONBLPOSN,RetCode : CARDINAL;
  R                                       : LONGREAL;
  INBUF                                   : BUFTYP;
  STR1,STR2,STR3                          : STRTYP;
  OKAY, StackFileExists,ok                : BOOLEAN;
  stk                                     : ARRAY [1..STACKSIZE] OF LONGREAL;
  StackFile                               : File;
  inputline,HPFileName,StackFileName,Xstr : STRTYP;
  DAYNAMES         : ARRAY [1..7] OF STR10TYP;

(*********************************************************************)
PROCEDURE CleanRealString(VAR INOUT str: ARRAY OF CHAR);
(*********************************************************************)
VAR s                       : ARRAY [0..255] OF CHAR;
    i                       : INTEGER;
    c,upper,inner,outer,len : CARDINAL;

BEGIN
  upper := HIGH(str);
  str[upper] := NULL;  (* Sentinal NULL for my algorithm *)
  len := LENGTH(str);
  outer := 0;
  inner := 0;
  WHILE (outer <= len) DO
    IF ((str[outer]='.') OR (CAP(str[outer])='E') OR ((str[outer]>='0') AND (str[outer]<='9')) ) THEN
    s[inner] := str[outer];
    INC(inner);
    END;
  INC(outer);
  END;
  s[inner] := NULL;
  FOR c := 0 TO inner DO
    str[c] := s[c];
  END;                                                                    (*  str[inner+1] := NULL; *)
END CleanRealString;

PROCEDURE TruncateInsigZeros(VAR INOUT str : ARRAY OF CHAR);
VAR
  StrLen, Non0Posn, NonBlPosn : CARDINAL;

BEGIN
      StrLen := Strings.Length(str);
(* Scanback for first nonzero char.  This is the new length of the string *)
      Non0Posn := SCANBACK(ADR(str),StrLen,'0',FALSE);
      str[Non0Posn] := NULL; (* Terminate string at 1-st insignificant 0 *)
      NonBlPosn := SCANFWD(ADR(str),Non0Posn,BLANK,FALSE) -1;
(*
   Remove the leading blanks by copying the non-blank string to the beginning, including the terminating null char.
*)
      COPYLEFT(ADR(str[NonBlPosn]),ADR(str),Non0Posn-NonBlPosn+1);
END TruncateInsigZeros;

PROCEDURE Dump2Console;
VAR
    S      : CARDINAL;
    NUMSTR : ARRAY [1..80] OF CHAR;
    OK     : BOOLEAN;
    pos    : CARDINAL;
BEGIN
  GETSTACK(stk,RetCode);
  STextIO.WriteString('------------');
  STextIO.WriteString('  ||  ');
  STextIO.WriteString('-------------------');
  STextIO.WriteLn;

  FOR S := STACKSIZE TO 1 BY -1 DO
    K := 0;
    RealToStringFixed(stk[S],15,5,Xstr,K,OKAY);
    IF OKAY THEN
      TruncateInsigZeros(Xstr);
    END;
    ok := FormatString('%-13s ||  ',NUMSTR,Xstr);
    IF ok THEN
      STextIO.WriteString(NUMSTR);
    END;
    MiscStdInOut.WriteLongReal(stk[S],10);
    STextIO.WriteLn;
    STextIO.WriteString('------------');
    STextIO.WriteString('  ||  ');
    STextIO.WriteString('-------------------');
    STextIO.WriteLn;
  END; (* for each element in stk *)
END Dump2Console;


BEGIN (********************* MAIN ****************************************)
  DAYNAMES[1] := "Sunday";
  DAYNAMES[2] := "Monday";
  DAYNAMES[3] := "Tuesday";
  DAYNAMES[4] := "Wednesday";
  DAYNAMES[5] := "Thursday";
  DAYNAMES[6] := "Friday";
  DAYNAMES[7] := "Saturday";

  StackFileName := 'RPNStack.sav';
  StackFileExists := FileExists(StackFileName);
  IF StackFileExists THEN
    OpenFile(StackFile,StackFileName,ReadOnlyDenyWrite);
    IF StackFile.status = 0 THEN
      ReadBlock(StackFile,ADR(stk), SIZE(stk) );
      CloseFile(StackFile);
      FOR c := STACKSIZE TO 1 BY -1 DO
        PUSHX(stk[c]);
      END; (* for *)
    END; (* if stackfile.status *)
  END; (* if stackfileexists *)
  PushStacks;

  WriteString(" HP RPN type calculator started.  Last compiled  ");
  WriteString(LastCompiled);
  WriteString(".");
  WriteLn;
  WriteLn;
  WriteLn;

  GetCommandLine(INBUF.CHARS);
  TRIM(INBUF);
  PushStacks;

  IF INBUF.COUNT <= 0 THEN
    WriteString(' Enter calculation, HELP or Enter to exit: ');
    ReadString(INBUF.CHARS);
                                                             (* This SkipLine call is in my M2MiscStdInOut library, so I removed it from here.    STextIO.SkipLine; *)
    TRIM(INBUF);
  END; (* if count <= zero *)
  REPEAT (* Until finished with input *)
    R := GETRESULT(INBUF);
    WriteLn;
    WriteLn;
    WriteString(' Result = ');
(*    WriteReal(R,15); *)
    K := 0;
    RealToStringFixed(R,20,10,Xstr,K,OKAY);
    IF OKAY THEN
      STRLEN := STRLENFNT(Xstr);
(* Scanback for first nonzero char.  This is the new length of the string *)
      NON0POSN := SCANBACK(ADR(Xstr),STRLEN,'0',FALSE);
      Xstr[NON0POSN+1] := NULL; (* Terminate string at 1-st insignificant 0 *)
      STR1 := Xstr;
      NONBLPOSN := SCANFWD(ADR(STR1),NON0POSN,BLANK,FALSE);
(*
   Remove the leading blanks by copying the non-blank string to the beginning, including the terminating null char.
*)
      COPYLEFT(ADR(STR1[NONBLPOSN]),ADR(STR1),NON0POSN-NONBLPOSN+2);
      ok := FormatString('%-20s||     ',STR2,STR1);
      WriteString(STR2);
      WriteReal(R,15);
    END (*IF RealToStringFixed succeeded *);
    WriteLn;
    Dump2Console;   (* Always displaying the stack.  I'll try this to see how much I like it *)
    WriteLn;
    
    IF Holidays.valid THEN
      WriteString(" For year ");
(*      R := READX(); Don't need this assignment as it's still in R from the GETRESULT above *)
      C := ROUND(R);
      IF C < 40 THEN
        INC(C,2000);
      ELSIF C < 100 THEN
        INC(C,1900);
      END (* if *);
      WriteCard(C);
      WriteLn;
      WriteString("New Years Day is a ");
      c := (JULIAN(1,1,C) MOD 7) + 1;
      WriteString(DAYNAMES[c]);
      WriteString(", MLK Day is Jan ");
      WriteCard(Holidays.MLK.d);
      WriteString(", Pres Day is Feb ");
      WriteCard(Holidays.Pres.d);
      WriteString(", Easter is ");
      WriteCard(Holidays.Easter.m);
      WriteString("/");
      WriteCard(Holidays.Easter.d);
      WriteString(", Mother's Day is May ");
      WriteCard(Holidays.Mother.d);
      WriteLn;
      WriteString("Memorial Day is May ");
      WriteCard(Holidays.Memorial.d);
      WriteString(", Father's Day is June ");
      WriteCard(Holidays.Father.d);
      WriteString(", July 4 is a ");
      c := (JULIAN(7,4,C) MOD 7) + 1;
      WriteString(DAYNAMES[c]);
      WriteString(", Labor Day is Sep ");
      WriteCard(Holidays.Labor.d);
      WriteString(", Columbus Day is Oct ");
      WriteCard(Holidays.Columbus.d);
      WriteLn;
      WriteString("Election Day is Nov ");
      WriteCard(Holidays.Election.d);
      WriteString(", Veteran's Day is a ");
      c := (JULIAN(11,11,C) MOD 7) + 1;
      WriteString(DAYNAMES[c]);
      WriteString(", Thanksgiving is Nov ");
      WriteCard(Holidays.Thanksgiving.d);
      WriteString(", Christmas Day is a ");
      c := (JULIAN(12,25,C) MOD 7) + 1;
      WriteString(DAYNAMES[c]);
      WriteLn;
      WriteLn;
      WriteLn;
      Holidays.valid := FALSE;
    ELSIF stricmpfnt(INBUF.CHARS,"ABOUT") = 0 THEN
      WriteString(" Last compiled ");
      WriteString(LastCompiled);
      WriteString(".");
      WriteLn;
      WriteLn;
      WriteLn;
    END (* IF *);

    WriteString(' Enter calculation, HELP, DUMP or Enter to exit: ');
    ReadString(INBUF.CHARS);
                                                             (*    STextIO.SkipLine; *)
    WriteLn;
    TRIM(INBUF);
    Capitalize(INBUF.CHARS);
    IF (STRCMPFNT(INBUF.CHARS,'D') = 0) OR (STRCMPFNT(INBUF.CHARS,'DUMP') = 0) THEN
      Dump2Console;
      INBUF.CHARS[1] := 'P';
      INBUF.CHARS[2] := '';
      TRIM(INBUF);
    END; (* if dump *)

  UNTIL INBUF.COUNT = 0;
  GETSTACK(stk,RetCode);
  OpenCreateFile(StackFile,StackFileName,ReadWriteDenyWrite);
  WriteBlock(StackFile, ADR(stk), SIZE(stk) );
  CloseFile(StackFile);
END RPN.

(* PROCEDURE GETSTACK(VAR STK : ARRAY OF LONGREAL; VAR RETCOD : CARDINAL);*)
(* StackRegNames = (X,Y,Z,T5,T4,T3,T2,T1);  *)
