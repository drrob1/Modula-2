MODULE Lotto;
(*
  REVISION HISTORY
  ----------------
  18 May 09 -- Original version completed.
*)
  FROM SYSTEM IMPORT ADR,ADDRESS,FUNC;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  IMPORT TextWindows, Terminal, BasicDialogs, DlgShell;
  FROM BasicDialogs IMPORT MessageTypes;
  IMPORT Strings,MemUtils,WholeStr,LongStr,LongConv,LongMath,ASCII,SysClock,FormatString;
	IMPORT MiscM2,MyFIO,UTILLIB,MYRAND;
	
CONST
	MaxNum = 1000;
	
VAR
	Num,c,C,NumCtr : CARDINAL;  (* I'm not sure I even need NumCtr *)
	NumArray : ARRAY [1..MaxNum] OF CARDINAL;
	ch,CH : CHAR;
	dt : SysClock.DateTime;
	timefmtstring,datetimefmtstring,deststring1,deststring2,fnamestr,bl : ARRAY [0..255] OF CHAR;
	b : UTILLIB.BUFTYP;
	outfname : MyFIO.MYFILTYP;
		
PROCEDURE Shuffle(ntimes:CARDINAL);
VAR C,K,c,shuffleswap: CARDINAL;

BEGIN (* SHUFFLE *)
  FOR c := 1 TO ntimes DO
(*
  Shuffle the array - pass *once* through the array, swapping each element
  with another, randomly chosen, element.
*)
    FOR C := Num TO 2 BY -1 DO
(* swap element in ith place with any element at or below that place *)
      K := MYRAND.RANDCARD(C)+1;
      shuffleswap := NumArray[C];
      NumArray[C] := NumArray[K];
      NumArray[K] := shuffleswap;
    END(*FOR*);
  END(*FOR*);
END Shuffle;

PROCEDURE FWriteCard (VAR F:MyFIO.MYFILTYP; N:CARDINAL);
VAR s : ARRAY [0..255] OF CHAR;
	  b1 : UTILLIB.BUFTYP;

BEGIN
      WholeStr.CardToStr(N,s);
      UTILLIB.ASSIGN2BUF(s,b1);
      MyFIO.FWRTXLN(F,b1);
END FWriteCard;
	
BEGIN (* Main Pgm *)
	NumCtr := 1;
	Num := 0;
	bl := '                                                                       ';
	timefmtstring := '%2c:%2c:%2c';
	datetimefmtstring := '%c/%c/%c  at %c:%c:%c';
	fnamestr := 'Lotto.txt';
	REPEAT
  UNTIL BasicDialogs.PromptCard(' Enter number maximum or 0 to quit: ',5,MaxNum,TRUE,Num);
  IF Num = 0 THEN HALT END;
  FOR c := 1 TO Num DO
  	NumArray[c] := c;
  END;
  UTILLIB.ASSIGN2BUF(fnamestr,b);
  MyFIO.FAPPEND(outfname,b);
  Terminal.Reset;
  Terminal.WriteString(' Press any key for output.');
  Terminal.WriteLn;
  LOOP
  	REPEAT
  	  SysClock.GetClock(dt);
  	  deststring1 := bl;
  	  FormatString.FormatString(timefmtstring,deststring1,dt.hour,dt.minute,dt.second);
  	  Terminal.Position(0,2);
(* fine tune this number to stop the flickering effect of too rapid sequential screen writes *)
      IF dt.fractions < 50 THEN  
      	Terminal.WriteString(deststring1);
      END;
  	  Shuffle(Num);
  	UNTIL Terminal.CharAvail();
  	Terminal.Read(ch); (* discard *)
  	deststring2 := bl;
    FormatString.FormatString(datetimefmtstring,deststring2,dt.month,dt.day,dt.year,
                                                                      dt.hour,dt.minute,dt.second);
    MyFIO.FWRSTR(outfname,deststring2);
    MyFIO.FWRLN(outfname);
    Terminal.Reset;
  	FOR c := 1 TO Num DO
  		MiscM2.WriteCard(NumArray[c]);
  		Terminal.WriteString('  ');
    	IF c MOD 20 = 0 THEN Terminal.WriteLn END;
  		FWriteCard(outfname,NumArray[c]);  
    END;
    MyFIO.FWRLN(outfname);
  	Terminal.WriteLn;
	  Terminal.WriteString(' More?: ');
	  Terminal.Read(ch);
	  IF CAP(ch) <> 'Y' THEN EXIT END;
    Terminal.Reset;
    Terminal.WriteString(' Press any key for output.');
    Terminal.WriteLn;
	  
	END (* loop *);

  MyFIO.FCLOSE(outfname);
END Lotto.