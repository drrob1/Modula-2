MODULE TestStringList;
(*
REVISION HISTORY
----------------
13 Oct 13 -- Testing gm2 routines.
11 Oct 16 -- gm2 was a bust.  I'm testing StonyBrook Modula-2 addition of GetDayTime, backporting C++ and go code I recently wrote.
24 Mar 17 -- Now will use this code to test the new StringList code I've written.
*)


  FROM SYSTEM IMPORT ADR,ADDRESS,CAST;
  FROM MiscStdInOut IMPORT WriteString,WriteLn,PressAnyKey,WriteCard,WriteInt,ReadString,ReadCard,
                     WriteLongCard;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM UTILLIB IMPORT BUFSIZ,CTRLCOD,STRTYP,STR10TYP,STR20TYP,BUFTYP,
    MAXCARDFNT,NULL, COPYLEFT,COPYRIGHT,FILLCHAR,SCANFWD,SCANBACK,
    STRLENFNT,STRCMPFNT,LCHINBUFFNT,MRGBUFS,TRIMFNT,TRIM,RMVCHR,
    APPENDA2B,CONCATAB2C,INSERTAin2B,ASSIGN2BUF, StringItemPointerType,
    StringDoubleLinkedListPointerType,InitStringListPointerType;
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
    StringListP1,StringListP2 : StringDoubleLinkedListPointerType;
    s : STRTYP;

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
    IF (h <= 9) THEN Str20[i] := CHR(h + ASCZERO) ELSE Str20[i] := CHR(h -10 + ascA) END;
    INC(i);
    M := M DIV 16;
  UNTIL L = 0;
  j := 1;  (* first posn is a space to leave room for sign char *)
  OutStr[0] := ' ';
  REPEAT (* until i = 0 *)
    DEC(i);
    OutStr[j] := Str20[i];
    INC(j);
  UNTIL i = 0;
  OutStr[j] := 0C;
END AdrToHexStr;

(**************************************************************************************************)

BEGIN
  GetClock(dt);
  WriteString(' From system clock: m d y hr min sec frac zone, DST ');
  WriteCard(dt.month);
  WriteString('  ');
  WriteCard(dt.day);
  WriteString('  ');
  WriteCard(dt.year);
  WriteString('  ');
  WriteCard(dt.hour);
  WriteString('  ');
  WriteCard(dt.minute);
  WriteString('  ');
  WriteCard(dt.second);
  WriteString('  ');
  WriteCard(dt.fractions);
  WriteString('  ');
  WriteInt(dt.zone);
  IF dt.summerTimeFlag THEN
    WriteString(' DST');
  ELSE
    WriteString(' std time');
  END;
  WriteLn;
  WriteLn;




  TIME2MDY(M,D,Y);
  WriteString(' From TIME2MDY:');
  WriteCard(M);
  WriteString(' / ');
  WriteCard(D);
  WriteString(' / ');
  WriteCard(Y);
  WriteLn;

  LC := JULIAN(M,D,Y);
  WriteString('Julian date number: ');
  WriteLongCard(LC);
  WriteLn;
  GREGORIAN(LC,M,D,Y);
  WriteString(' after gregorian: ');
  WriteCard(M);
  WriteString('/');
  WriteCard(D);
  WriteString('/');
  WriteCard(Y);
  WriteLn;

  dt2 := GetDateTime(dt1);

  WriteString(' dt2: m d y hr min sec frac zone, DST ');
  WriteCard(dt2.M);
  WriteString('  ');
  WriteCard(dt2.D);
  WriteString('  ');
  WriteCard(dt2.Yr);
  WriteString('  ');
  WriteCard(dt2.Hr);
  WriteString('  ');
  WriteCard(dt2.Minutes);
  WriteString('  ');
  WriteCard(dt2.Seconds);
  WriteString('  ');
  WriteString(dt2.MonthStr);
  WriteString('  ');
  WriteString(dt2.DayOfWeekStr);
  WriteLn;
  WriteLn;

  WriteString(' dt1: m d y hr min sec frac zone, DST ');
  WriteCard(dt1.M);
  WriteString('  ');
  WriteCard(dt1.D);
  WriteString('  ');
  WriteCard(dt1.Yr);
  WriteString('  ');
  WriteCard(dt1.Hr);
  WriteString('  ');
  WriteCard(dt1.Minutes);
  WriteString('  ');
  WriteCard(dt1.Seconds);
  WriteString('  ');
  WriteString(dt1.MonthStr);
  WriteString('  ');
  WriteString(dt1.DayOfWeekStr);
  WriteLn;
  WriteLn;



  StringListP1 := InitStringListPointerType(StringListP2);
  AdrToHexStr(StringListP1,s);
  WriteString(" Value of P1 is : ");
  WriteString(s);
  WriteString(".    Value of P2 is :");
  AdrToHexStr(StringListP2,s);
  WriteString(s);
  WriteLn;
  WriteLn;

  IF StringListP1 <> StringListP2 THEN
    WriteString(" String List P1 and P2 are not equal.  Will Halt now.");
    WriteLn;
    WriteLn;
    HALT;
  END; (* IF String list pointers are not equal *)

  WriteString(" String list pointers are equal. ");
  WriteLn;

  WriteString(" StartOfList: ");
  AdrToHexStr(StringListP1^.StartOfList,s);
  WriteString(s);
  WriteString(", EndOfList: ");
  AdrToHexStr(StringListP1^.EndOfList,s);
  WriteString(s);
  WriteString(", CurrentPlaceInList: ");
  AdrToHexStr(StringListP1^.CurrentPlaceInList,s);
  WriteString(s);
  WriteString(", PrevPlaceInList: ");
  AdrToHexStr(StringListP1^.PrevPlaceInList,s);
  WriteString(s);
  WriteString(", len: ");
  WriteCard(StringListP1^.len);
  WriteLn;

  AdrToHexStr(StringListP1^.CurrentPlaceInList^.Prev,s);
  WriteString(" StringListP1 Prev is : ");
  WriteString(s);
  WriteString(".  StringListP1 Next is : ");
  AdrToHexStr(StringListP1^.CurrentPlaceInList^.Next,s);
  WriteString(s);
  WriteString(".  Length of list is : ");
  WriteCard(StringListP1^.len);
  WriteLn;

  WriteString(" Length of the string in the list is : ");
  WriteCard(StringListP1^.CurrentPlaceInList^.S.LENGTH);
  WriteLn;


  WriteLn;
  WriteLn;

END TestStringList.
