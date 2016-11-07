MODULE pwrseris; 




*** ERROR ***
  ) expected
  [  found
*** ERROR ***
  ; expected
  ..  found

  PROCEDURE getPowerSpectrum(VAR data: array); 
*** ERROR ***
  BEGIN expected
  :  found


*** TRANSLATION STOPPED ... ***
 of real;
 var p_sp: array[l1..h1:s_int] of real;
 size: s_int);

  sInt 
var i: s_int;
begin


*** ... TRANSLATION RESUMED ***
  BEGIN 
    ?1i := 1; 
    WHILE ?1i < ?1size DO 

      ?1pSp[?1i] := (data[?1i]*data[?1i]+data[?1size-?1i+1]*data[?1size-?1i])/2; 
      ?1i := ?1i+1; 
      (*while*) 
    END; 
  END(*get_power_spectrum*)  getPowerSpectrum; 
 pwrseris.

****************************************************************************
NUMBER OF ? =  13

Please refer to the translator manual for detailed explanations and solutions

?0  = system problems
?1  = unknown identifier, undefined symbol or null operator
?2  = invalid Modula-2 constant string
?3  = warning - using string make sure to follow the Modula-2 rules 
?4  = Pascal statement not supported by Modula-2
?5  = function or procedure parameter
?6  = absolute variable declaration
?7  = external procedure or subprogram declaration
?8  = special in-line code statement not supported
?9  = to be changed in a function call
?10 = element expression in set can only be a constant expression
?11 = to be changed in a procedure call

****************************************************************************
NUMBER OF ERRORS =   3
