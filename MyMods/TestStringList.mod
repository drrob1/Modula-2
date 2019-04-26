MODULE TestStringList;
(*
REVISION HISTORY
----------------
13 Oct 13 -- Testing gm2 routines.
11 Oct 16 -- gm2 was a bust.  I'm testing StonyBrook Modula-2 addition of GetDayTime, backporting C++ and go code I recently wrote, now named TestTimLib.
24 Mar 17 -- Now will use this code to test the new StringList code I've written now named TestStringList.
25 Mar 17 -- Finished TestStringList, and it works.
31 Mar 17 -- Now testing DisposeStringListPointerType;
 3 Apr 17 -- Testing the remove string stuff.
 9 Apr 18 -- Added NewStringList, in the Go idiom format.  And then opaque types w/ the needed getter routines.
*)


  FROM SYSTEM IMPORT ADR,ADDRESS,CAST;
  FROM MiscStdInOut IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard, WriteLongCard;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,STR20TYP,BUFTYP,MAXCARDFNT,NULL,COPYLEFT,COPYRIGHT,FILLCHAR,
    SCANFWD,SCANBACK, STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,APPENDA2B,CONCATAB2C,INSERTAin2B,
    ASSIGN2BUF, StringItemPointerType,StringDoubleLinkedListPointerType,InitStringListPointerType,NewStringList,
    AppendStringToList,NextStringFromList,PrevStringFromList,CurrentPointerBeginning,CurrentPointerEnding,
    GetNextStringFromList,GetPrevStringFromList;
  IMPORT MiscStdInOut,UTILLIB;
(*                 New String List Types from UTILLIB.def
  StringItemPointerType = POINTER TO StringItemType;
  StringItemType    = RECORD
                            Prev : StringItemPointerType;
                            S    : BUFTYP;
                            Next : StringItemPointerType;
                      END; // StringItemType Record


  StringDoubleLinkedListType = RECORD
                  StartOfList, EndOfList, CurrentPlaceInList, PrevPlaceInList : StringPointerType;
                  len : CARDINAL;
  END; // StringDoubleLinkedListType Recored

  StringDoubleLinkedListPointerType = POINTER TO StringDoubleLinkedListType;
*)

(*    removed 4/9/2018 5:42:08 PM as it's not used.
  FROM Environment IMPORT GetCommandLine;
  IMPORT TIMLIBrevised;
  FROM TIMLIBrevised IMPORT TIME2MDY, JULIAN, GREGORIAN, DateTimeType, GetDateTime;
  FROM SysClock IMPORT GetClock,DateTime;
*)

  CONST
    LastAltered = "9 Apr 2018";

  VAR
(*     removed 4/9/2018 5:44:35 PM
    INBUF,TOKEN               : BUFTYP;
    RETCOD,C,posn,c1,c2,M,D,Y : CARDINAL;
    I                         : INTEGER;
    L                         : LONGINT;
    R,r1,r2,r3,r4,r5,r6       : LONGREAL;
    CH                        : CHAR;
    LC                        : LONGCARD;
    dt                        : DateTime;
    dt1,dt2                   : DateTimeType;
*)
    c1 : CARDINAL;
    StringListP1 : StringDoubleLinkedListPointerType;
    s : STRTYP;
    b : BUFTYP;
    StringP : StringItemPointerType;

(******************************************************************************************************)

PROCEDURE AdrToHexStr(adr : ADDRESS; VAR INOUT OutStr : ARRAY OF CHAR);

CONST  ASCZERO = ORD('0');
       ascA    = ORD('A');
VAR i,j,h,M  : CARDINAL;
    Str20    : STR20TYP;

BEGIN
  i := 0;
  M := CAST(CARDINAL,adr);
  REPEAT (* until M = 0 *)
    h := M MOD 16;
    IF (h <= 9) THEN
      Str20[i] := CHR(h + ASCZERO)
    ELSE
      Str20[i] := CHR(h -10 + ascA) END;
    INC(i);
    M := M DIV 16;
  UNTIL M = 0;
  j := 1;  (* first posn is a space to leave room for sign char *)
  OutStr[0] := ' ';
  REPEAT (* until i = 0 *)
    DEC(i);
    OutStr[j] := Str20[i];
    INC(j);
  UNTIL i = 0;
  OutStr[j] := 0C;
END AdrToHexStr;

(*
  PROCEDURE PreviousNextFromStringItem(StringListP: StringDoubleLinkedListPointerType; VAR OUT previous, next : ADDRESS);
  PROCEDURE CurrentString(StringListP : StringDoubleLinkedListPointerType) : STRTYP;
  PROCEDURE CurrentStringBuffer(StringListP: StringDoubleLinkedListPointerType) : BUFTYP;
  PROCEDURE StringListLen(StringListP : StringDoubleLinkedListPointerType) : CARDINAL;
*)

(******************************************************************************************************)

PROCEDURE WriteStringItem(Prompt : ARRAY OF CHAR; StringP : StringItemPointerType);
  VAR
    s : STR20TYP;
    str : STRTYP;
    b   : BUFTYP;
    prev,next : ADDRESS;
BEGIN

  UTILLIB.PreviousNextFromStringItem(StringP, prev,next);
  str := UTILLIB.GetStringFromItem(StringP);
  b := UTILLIB.GetBuftypFromItem(StringP);

  WriteString(Prompt);

    AdrToHexStr(prev,s);
    WriteString(s);
    WriteString("; ");
(*    WriteString(S.CHARS); *)
    WriteString(str);
    WriteString("; ");
    AdrToHexStr(next,s);
    WriteString(s);
    WriteString("     Buftyp: ");
    WriteString(b.CHARS);
    WriteString("   count= ");
    MiscStdInOut.WriteCard(b.COUNT);
    WriteLn;

END WriteStringItem;


(********************************************** MAIN ****************************************************)

BEGIN

  WriteString(" TestStringList.  Last Altered ");
  WriteString(LastAltered);
  WriteString(".");
  WriteLn;

  (* StringListP1 := InitStringListPointerType();  *)
  StringListP1 := NewStringList();

  AppendStringToList(StringListP1," First String in this double linked list.");
  AppendStringToList(StringListP1," Second String in this double linked list.");
  AppendStringToList(StringListP1," Third String in this double linked list.");
  AppendStringToList(StringListP1," Fourth string in this double linked list.");

(* Display the list in forward direction *)
  CurrentPointerBeginning(StringListP1);

  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

(* Display the list in reverse direction *)
  CurrentPointerEnding(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetPrevStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;
  WriteLn;


  WriteString(" After removing the 4th string.");
  WriteLn;
  WriteLn;

(* Remove the last string, and then display in forward direction *)
  UTILLIB.RemoveLastStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" After removing the 3rd string.  Should leave 2 strings left.");
  WriteLn;
  WriteLn;

(* Remove the last string, and then display in forward direction *)
  UTILLIB.RemoveLastStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" After removing the 2nd string.  Should leave 1 string left.");
  WriteLn;
  WriteLn;

(* Remove the last string, and then display in forward direction *)

  UTILLIB.RemoveLastStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" And then there were none.");
  WriteLn;
  WriteLn;

(* Remove the last string, and then display in forward direction *)
  UTILLIB.RemoveLastStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" The length of the string by the len field is ");
  WriteCard(UTILLIB.StringListLen(StringListP1));
  WriteLn;

  AdrToHexStr(StringListP1,s);
  WriteString(" StringListP1 is ");
  WriteString(s);
  WriteLn;
  WriteLn;

  PressAnyKey;

  AppendStringToList(StringListP1,"First String");
  AppendStringToList(StringListP1,"Second String");
  AppendStringToList(StringListP1,"Third String");
  AppendStringToList(StringListP1,"Fourth string");

(* Display the list in forward direction *)
  CurrentPointerBeginning(StringListP1);

  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

(* Display the list in reverse direction *)
  CurrentPointerEnding(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetPrevStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;
  WriteLn;

  PressAnyKey;

  WriteString(" After removing the 1st string, leaving 3");
  WriteLn;
  WriteLn;

(* Remove the first string, and then display in forward direction *)
  UTILLIB.RemoveFirstStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" After removing the 2nd string, leaving 2.");
  WriteLn;
  WriteLn;

(* Remove the first string, and then display in forward direction *)
  UTILLIB.RemoveFirstStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" After removing the 3rd string, leaving 1.");
  WriteLn;
  WriteLn;

(* Remove the first string, and then display in forward direction *)
  UTILLIB.RemoveFirstStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" After removing the last remaining string.");
  WriteLn;
  WriteLn;

(* Remove the first string, and then display in forward direction *)
  UTILLIB.RemoveFirstStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;


  WriteString(" The length of the string by the len field is ");
  WriteCard(UTILLIB.StringListLen(StringListP1));
  WriteLn;

  AdrToHexStr(StringListP1,s);
  WriteString(" StringListP1 is ");
  WriteString(s);
  WriteLn;
  WriteLn;


  UTILLIB.DisposeStringListPointerType(StringListP1);

  AdrToHexStr(StringListP1,s);
  WriteString(" After DisposeStringListPointertype call.  StringListP1 is ");
  WriteString(s);
  WriteLn;
  WriteLn;

  PressAnyKey;

(*  StringListP1 := InitStringListPointerType(); *)
  StringListP1 := NewStringList();

  AppendStringToList(StringListP1," Fifth String in this double linked list.");
  AppendStringToList(StringListP1," Sixth String in this double linked list.");
  AppendStringToList(StringListP1," Seventh String in this double linked list.");
  AppendStringToList(StringListP1," Eighth string in this double linked list.");

(* Display the list in forward direction *)
  CurrentPointerBeginning(StringListP1);

  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

(* Display the list in reverse direction *)
  CurrentPointerEnding(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetPrevStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;
  WriteLn;


  WriteString(" The length of the string by the len field should be four, it is ");
  WriteCard(UTILLIB.StringListLen(StringListP1));
  WriteLn;

  AdrToHexStr(StringListP1,s);
  WriteString(" StringListP1 is ");
  WriteString(s);
  WriteLn;
  WriteLn;

  WriteString(" After removing the last string, leaving 3");
  WriteLn;
  WriteLn;

(* Remove the last string, and then display in forward direction *)
  UTILLIB.RemoveLastStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" After removing the 3rd string, leaving 2.");
  WriteLn;
  WriteLn;

(* Remove the last string, and then display in forward direction *)
  UTILLIB.RemoveLastStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" After removing the 2nd string, leaving 1.");
  WriteLn;
  WriteLn;

(* Remove the last string, and then display in forward direction *)
  UTILLIB.RemoveLastStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  WriteString(" After removing the last remaining string.");
  WriteLn;
  WriteLn;

(* Remove the last string, and then display in forward direction *)
  UTILLIB.RemoveLastStringFromList(StringListP1);
  CurrentPointerBeginning(StringListP1);
  FOR c1 := 1 TO UTILLIB.StringListLen(StringListP1) DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;


  WriteString(" The length of the string by the len field should be zero, and it is ");
  WriteCard(UTILLIB.StringListLen(StringListP1));
  WriteLn;

  AdrToHexStr(StringListP1,s);
  WriteString(" StringListP1 is ");
  WriteString(s);
  WriteLn;
  WriteLn;



  UTILLIB.DisposeStringListPointerType(StringListP1);

  AdrToHexStr(StringListP1,s);
  WriteString(" After Final DisposeStringListPointertype call.  StringListP1 is ");
  WriteString(s);
  WriteLn;
  WriteLn;
END TestStringList.
