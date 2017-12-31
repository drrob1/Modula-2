(*
  3/12/2015 11:06:12 AM
  From modula2.org tutorial.  This section deals with dynamic memory usage.  I will change it around so I better understand it.
  And I've made it a double linked list.

  3/13/2015 2:12:23 PM
  Will add output of the pointers so I can compare this with the prev and next field contents.  And I changed the name of variable AnEntry to AnEntryPointer.
*)

MODULE LinkList;

(* FROM Terminal2   IMPORT WriteString, WriteChar, WriteLn; *)
FROM MiscStdInOut IMPORT WriteString, WriteChar, WriteLn, WriteCard;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM SYSTEM  IMPORT (* TSIZE, *) ADDRESS;

TYPE  FullNamePointerType = POINTER TO FullName;
      FullName    = RECORD
                      Prev      : FullNamePointerType;
                      FirstName : ARRAY[0..20] OF CHAR;
                      Initial   : CHAR;
                      LastName  : ARRAY[0..20] OF CHAR;
                      Next      : FullNamePointerType;
                    END;
      STR20TYP = ARRAY [0..20] OF CHAR;

VAR   StartOfList,EndofList,CurrentPlaceInList,PrevPlaceInList,AnEntryPointer : FullNamePointerType;
      I : CARDINAL;
      s : STR20TYP;

(******************************************************************************************************)

PROCEDURE AdrToHexStr(adr : ADDRESS; VAR INOUT OutStr : ARRAY OF CHAR);

CONST  ASCZERO = ORD('0');
       ascA    = ORD('A');

TYPE  ADRTOCARD = RECORD
                    CASE :BOOLEAN OF
                      TRUE: A : ADDRESS;
                    | FALSE: C : CARDINAL;
                    END; (* CASE *)
                  END; (* RECORD *)

VAR i,j,h,L  : CARDINAL;
    Str20    : STR20TYP;
    adrcard : ADRTOCARD;

BEGIN
  i := 0;
  adrcard.A := adr;
  L := adrcard.C;
  REPEAT (* until L = 0 *)
    h := L MOD 16;
    IF (h <= 9) THEN Str20[i] := CHR(h + ASCZERO) ELSE Str20[i] := CHR(h -10 + ascA) END;
    INC(i);
    L := L DIV 16;
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

BEGIN   (* Main Program *)

  StartOfList := NIL;
  EndofList := NIL;
  CurrentPlaceInList := NIL;
  PrevPlaceInList := NIL;

                  (* Generate the first name in the list *)

   NEW(AnEntryPointer);
   StartOfList := AnEntryPointer;
   WriteString(" 1: ");
   AdrToHexStr(AnEntryPointer,s);
   WriteString(s);
   WITH AnEntryPointer^ DO
     Prev := NIL;
     FirstName := "John ";
     Initial   := 'Q';
     LastName  := " Doe";
     Next := NIL;
   END; (* WITH AnEntryPointer *)

                  (* Generate 2nd name in the list *)

   PrevPlaceInList := AnEntryPointer;
   NEW(AnEntryPointer);
   WriteString(", 2: ");
   AdrToHexStr(AnEntryPointer,s);
   WriteString(s);
   CurrentPlaceInList := AnEntryPointer;
   PrevPlaceInList^.Next := CurrentPlaceInList;

   WITH CurrentPlaceInList^ DO
     Prev := PrevPlaceInList;
     FirstName := "Mary ";
     Initial   := 'R';
     LastName  := " Johnson";
     Next      := NIL;
   END; (* With CurrentPlaceInList *)

                 (* Add 10 more names to complete the list *)

   FOR I := 1 TO 10 DO
     PrevPlaceInList := CurrentPlaceInList;
     NEW(AnEntryPointer);
     WriteString(", ");
     WriteCard(I+2);
     WriteString(":");
     AdrToHexStr(AnEntryPointer,s);
     WriteString(s);
     CurrentPlaceInList := AnEntryPointer;
     PrevPlaceInList^.Next := CurrentPlaceInList;
     CurrentPlaceInList^.Prev := PrevPlaceInList;
     CurrentPlaceInList^.FirstName := "Billy ";
     CurrentPlaceInList^.Initial   := CHR(I+64);   (* 65 is cap A *)
     CurrentPlaceInList^.LastName  := " Franklin";
     CurrentPlaceInList^.Next := NIL;
   END; (* FOR I *)
   EndofList := CurrentPlaceInList;
   WriteLn;
   WriteLn;

                        (* Display the list on the monitor in forward direction *)
   WriteString(" List in forward direction.");
   WriteLn;
   CurrentPlaceInList := StartOfList;
   WHILE CurrentPlaceInList # NIL DO

     AdrToHexStr(CurrentPlaceInList^.Prev,s);
     WriteString(s);

     WriteString(": ");
     WriteString(CurrentPlaceInList^.FirstName);
     WriteChar(CurrentPlaceInList^.Initial);
     WriteString(CurrentPlaceInList^.LastName);
     WriteString(" : ");

     AdrToHexStr(CurrentPlaceInList^.Next,s);
     WriteString(s);

     WriteLn;
     PrevPlaceInList := CurrentPlaceInList;
     CurrentPlaceInList := CurrentPlaceInList^.Next;
   END;

                        (* Display the list on the monitor in reverse direction *)
   WriteString(" List in reverse direction. ");
   WriteLn;
   CurrentPlaceInList := EndofList;
   WHILE CurrentPlaceInList # NIL DO

     AdrToHexStr(CurrentPlaceInList^.Prev,s);
     WriteString(s);

     WriteString(": ");
     WriteString(CurrentPlaceInList^.FirstName);
     WriteChar(CurrentPlaceInList^.Initial);
     WriteString(CurrentPlaceInList^.LastName);
     WriteString(" : ");

     AdrToHexStr(CurrentPlaceInList^.Next,s);
     WriteString(s);

     WriteLn;
     PrevPlaceInList := CurrentPlaceInList;   (* Does not seem to be needed here *)
     CurrentPlaceInList := CurrentPlaceInList^.Prev;
   END;


                             (* Deallocate is a good habit *)
   AnEntryPointer := StartOfList;
   (* Compile error, invalid factor for INC(), or operands of this operator are incorrect for "+" AnEntryPointer := AnEntryPointer + 4 *)
   WHILE AnEntryPointer # NIL DO
     CurrentPlaceInList := AnEntryPointer^.Next;
     DISPOSE(AnEntryPointer);
     AnEntryPointer := CurrentPlaceInList;
   END;

END LinkList.
