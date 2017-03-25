MODULE TestStringList;
(*
REVISION HISTORY
----------------
13 Oct 13 -- Testing gm2 routines.
11 Oct 16 -- gm2 was a bust.  I'm testing StonyBrook Modula-2 addition of GetDayTime, backporting C++ and go code I recently wrote, now named TestTimLib.
24 Mar 17 -- Now will use this code to test the new StringList code I've written now named TestStringList.
*)


  FROM SYSTEM IMPORT ADR,ADDRESS,CAST;
  FROM MiscStdInOut IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteLongCard;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,STR20TYP,BUFTYP,MAXCARDFNT,NULL,COPYLEFT,COPYRIGHT,FILLCHAR,
    SCANFWD,SCANBACK, STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,APPENDA2B,CONCATAB2C,INSERTAin2B,
    ASSIGN2BUF, StringItemPointerType,StringDoubleLinkedListPointerType,InitStringListPointerType,
    AppendStringToList,NextStringFromList,PrevStringFromList,CurrentPointerBeginning,CurrentPointerEnding,
    GetNextStringFromList,GetPrevStringFromList;
  IMPORT UTILLIB;
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

  FROM Environment IMPORT GetCommandLine;
  IMPORT TIMLIB;
  FROM TIMLIB IMPORT TIME2MDY, JULIAN, GREGORIAN, DateTimeType, GetDateTime;
  FROM SysClock IMPORT GetClock,DateTime;
(* IMPORT NumberIO, CardinalIO, StdIO, StrIO; *)
(* FROM NumberIO IMPORT WriteCard,WriteInt; *)
(* FROM StrIO IMPORT WriteString,WriteLn; *)

  VAR
    INBUF,TOKEN : BUFTYP;
    RETCOD,C,posn,c1,c2,
    M,D,Y                  : CARDINAL;
    I                      : INTEGER;
    L                      : LONGINT;
    R,r1,r2,r3,r4,r5,r6    : LONGREAL;
    CH                     : CHAR;
    LC                     : LONGCARD;
    dt                     : DateTime;
    dt1,dt2                : DateTimeType;
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

PROCEDURE WriteStringItem(Prompt : ARRAY OF CHAR; StringP : StringItemPointerType);
  VAR
    s : STR20TYP;
BEGIN

  WriteString(Prompt);
  WITH StringP^ DO
    AdrToHexStr(Prev,s);
    WriteString(s);
    WriteString("; ");
    WriteString(S.CHARS);
    WriteString("; ");
    AdrToHexStr(Next,s);
    WriteString(s);
    WriteLn;
  END; (* with StringP deref *)
END WriteStringItem;


(**************************************************************************************************)

BEGIN

  StringListP1 := InitStringListPointerType();

  AppendStringToList(StringListP1," First String in this double linked list.");
  AppendStringToList(StringListP1," Second String in this double linked list.");
  AppendStringToList(StringListP1," Third String in this double linked list.");
  AppendStringToList(StringListP1," Fourth string in this double linked list.");

  CurrentPointerBeginning(StringListP1);

  FOR c1 := 1 TO StringListP1^.len DO
    StringP := GetNextStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)
  WriteLn;

  CurrentPointerEnding(StringListP1);
  FOR c1 := 1 TO StringListP1^.len DO
    StringP := GetPrevStringFromList(StringListP1);
    WriteCard(c1);
    WriteString(": ");
    WriteStringItem(" String Item: ",StringP);
  END; (* for range StringList len *)

  WriteLn;
  WriteLn;
END TestStringList.
