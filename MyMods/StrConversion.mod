DEFINITION MODULE WholeStr;
 
  (* Whole-number/string conversions *)
 
IMPORT
  ConvTypes;
 
TYPE
  ConvResults = ConvTypes.ConvResults; (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
 
(* the string form of a signed whole number is
     ["+" | "-"], decimal digit, {decimal digit}
*)
 
PROCEDURE StrToInt (str: ARRAY OF CHAR; VAR int: INTEGER; VAR res: ConvResults);
  (* Ignores any leading spaces in str. If the subsequent characters in str are in the
     format of a signed whole number, assigns a corresponding value to int. Assigns a
     value indicating the format of str to res.
  *)
 
PROCEDURE IntToStr (int: INTEGER; VAR str: ARRAY OF CHAR);
  (* Converts the value of int to string form and copies the possibly truncated result to str. *)
 
(* the string form of an unsigned whole number is
     decimal digit, {decimal digit}
*)
 
PROCEDURE StrToCard (str: ARRAY OF CHAR; VAR card: CARDINAL; VAR res: ConvResults);
  (* Ignores any leading spaces in str. If the subsequent characters in str are in the
     format of an unsigned whole number, assigns a corresponding value to card.
     Assigns a value indicating the format of str to res.
  *)
 
PROCEDURE CardToStr (card: CARDINAL; VAR str: ARRAY OF CHAR);
  (* Converts the value of card to string form and copies the possibly truncated result to str. *)
 
END WholeStr.

DEFINITION MODULE LongStr;
 
  (* LONGREAL/string conversions *)
 
IMPORT
  ConvTypes;
 
TYPE
  ConvResults = ConvTypes.ConvResults; (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
 
(* the string form of a signed fixed-point real number is
     ["+" | "-"], decimal digit, {decimal digit}, [".", {decimal digit}]
*)
 
(* the string form of a signed floating-point real number is
     signed fixed-point real number, "E"|"e", ["+" | "-"], decimal digit, {decimal digit}
*)
 
PROCEDURE StrToReal (str: ARRAY OF CHAR; VAR real: LONGREAL; VAR res: ConvResults);
  (* Ignores any leading spaces in str. If the subsequent characters in str are in the
     format of a signed real number, assigns a corresponding value to real.
     Assigns a value indicating the format of str to res.
  *)
 
PROCEDURE RealToFloat (real: LONGREAL; sigFigs: CARDINAL; VAR str: ARRAY OF CHAR);
  (* Converts the value of real to floating-point string form, with sigFigs significant
     figures, and copies the possibly truncated result to str.
  *)
 
PROCEDURE RealToEng (real: LONGREAL; sigFigs: CARDINAL; VAR str: ARRAY OF CHAR);
  (* Converts the value of real to floating-point string form, with sigFigs significant
     figures, and copies the possibly truncated result to str. The number is scaled with
     one to three digits in the whole number part and with an exponent that is a
     multiple of three.
  *)
 
PROCEDURE RealToFixed (real: LONGREAL; place: INTEGER; VAR str: ARRAY OF CHAR);
  (* Converts the value of real to fixed-point string form, rounded to the given place
     relative to the decimal point, and copies the possibly truncated result to str.
  *)
 
PROCEDURE RealToStr (real: LONGREAL; VAR str: ARRAY OF CHAR);
  (* Converts the value of real as RealToFixed if the sign and magnitude can be shown
     within the capacity of str, or otherwise as RealToFloat, and copies the possibly
     truncated result to str. The number of places or significant digits depend on the
     capacity of str.
  *)
 
END LongStr.

EFINITION MODULE RealStr;
 
  (* REAL/string conversions *)
 
IMPORT
  ConvTypes;
 
TYPE
  ConvResults = ConvTypes.ConvResults; (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
 
(* the string form of a signed fixed-point real number is
     ["+" | "-"], decimal digit, {decimal digit}, [".", {decimal digit}]
*)
 
(* the string form of a signed floating-point real number is
     signed fixed-point real number, "E"|"e", ["+" | "-"], decimal digit, {decimal digit}
*)
 
PROCEDURE StrToReal (str: ARRAY OF CHAR; VAR real: REAL; VAR res: ConvResults);
  (* Ignores any leading spaces in str. If the subsequent characters in str are in the
     format of a signed real number, assigns a corresponding value to real.
     Assigns a value indicating the format of str to res.
  *)
 
PROCEDURE RealToFloat (real: REAL; sigFigs: CARDINAL; VAR str: ARRAY OF CHAR);
  (* Converts the value of real to floating-point string form, with sigFigs significant
     figures, and copies the possibly truncated result to str.
  *)
 
PROCEDURE RealToEng (real: REAL; sigFigs: CARDINAL; VAR str: ARRAY OF CHAR);
  (* Converts the value of real to floating-point string form, with sigFigs significant
     figures, and copies the possibly truncated result to str. The number is scaled with
     one to three digits in the whole number part and with an exponent that is a
     multiple of three.
  *)
 
PROCEDURE RealToFixed (real: REAL; place: INTEGER; VAR str: ARRAY OF CHAR);
  (* Converts the value of real to fixed-point string form, rounded to the given place
     relative to the decimal point, and copies the possibly truncated result to str.
  *)
 
PROCEDURE RealToStr (real: REAL; VAR str: ARRAY OF CHAR);
  (* Converts the value of real as RealToFixed if the sign and magnitude can be shown
     within the capacity of str, or otherwise as RealToFloat, and copies the possibly
     truncated result to str. The number of places or significant digits depend on the
     capacity of str.
  *)
 
END RealStr.

DEFINITION MODULE LongMath;
 
  (* Mathematical functions for the type LONGREAL *)
 
CONST
  pi   = 3.1415926535897932384626433832795028841972;
  exp1 = 2.7182818284590452353602874713526624977572;
 
PROCEDURE sqrt (x: LONGREAL): LONGREAL;
  (* Returns the positive square root of x *)
 
PROCEDURE exp (x: LONGREAL): LONGREAL;
  (* Returns the exponential of x *)
 
PROCEDURE ln (x: LONGREAL): LONGREAL;
  (* Returns the natural logarithm of x *)
 
  (* The angle in all trigonometric functions is measured in radians *)
 
PROCEDURE sin (x: LONGREAL): LONGREAL;
  (* Returns the sine of x *)
 
PROCEDURE cos (x: LONGREAL): LONGREAL;
  (* Returns the cosine of x *)
 
PROCEDURE tan (x: LONGREAL): LONGREAL;
  (* Returns the tangent of x *)
 
PROCEDURE arcsin (x: LONGREAL): LONGREAL;
  (* Returns the arcsine of x, in the range [-pi/2, pi/2] *)
 
PROCEDURE arccos (x: LONGREAL): LONGREAL;
  (* Returns the arccosine of x, in the range [0, pi] *)
 
PROCEDURE arctan (x: LONGREAL): LONGREAL;
  (* Returns the arctangent of x, in the range [-pi/2, pi/2] *)
 
PROCEDURE power (base, exponent: LONGREAL): LONGREAL;
  (* Returns the value of the number base raised to the power exponent *)
 
PROCEDURE round (x: LONGREAL): INTEGER;
  (* Returns the value of x rounded to the nearest integer *)
 
PROCEDURE IsRMathException (): BOOLEAN;
  (* Returns TRUE if the current coroutine is in the exceptional execution state
     because of the raising of the LONGMATH exception; otherwise returns FALSE.
  *)
 
END LongMath.

DEFINITION MODULE SYSTEM;
 
  (* Gives access to system programming facilities that are probably non portable. *)
 
  (* The constants and types define underlying properties of storage *)
 
CONST
  BITSPERLOC    = <implementation-defined constant> ;
  LOCSPERWORD   = <implementation-defined constant> ;
 
TYPE
  LOC; (* A system basic type. Values are the uninterpreted contents of the smallest
          addressable unit of storage *)
  ADDRESS = POINTER TO LOC;
  WORD = ARRAY [0 .. LOCSPERWORD-1] OF LOC;
 
  (* BYTE and LOCSPERBYTE are provided if appropriate for machine *)
CONST
  LOCSPERBYTE = <implementation-defined constant> ;
 
TYPE
  BYTE = ARRAY [0 .. LOCSPERBYTE-1] OF LOC;
 
PROCEDURE ADDADR (addr: ADDRESS; offset: CARDINAL): ADDRESS;
  (* Returns address given by (addr + offset), or may raise an exception if this
     address is not valid.
  *)
 
PROCEDURE SUBADR (addr: ADDRESS; offset: CARDINAL): ADDRESS;
  (* Returns address given by (addr - offset), or may raise an exception if this address
     is not valid.
  *)
 
PROCEDURE DIFADR (addr1, addr2: ADDRESS): INTEGER;
  (* Returns the difference between addresses (addr1 - addr2), or may raise an exception
     if the arguments are invalid or if the address space is non-contiguous.
  *)
 
PROCEDURE MAKEADR (val: <some type>; ... ): ADDRESS;
  (* Returns an address constructed from a list of values whose types are
     implementation-defined, or may raise an exception if this address is not valid.
  *)
 
PROCEDURE ADR (VAR v: <anytype>): ADDRESS;
  (* Returns the address of variable v. *)
 
PROCEDURE ROTATE (val: <a packedset type>; num: INTEGER): <type of first parameter>;
  (* Returns a bit sequence obtained from val by rotating up or down (left or right) by
     the absolute value of num.  The direction is down if the sign of num is negative,
     otherwise the direction is up.
  *)
 
PROCEDURE SHIFT (val: <a packedset type>; num: INTEGER): <type of first parameter>;
  (* Returns a bit sequence obtained from val by shifting up or down (left or right) by
     the absolute value of num, introducing zeros as necessary.  The direction is down
     if the sign of num is negative, otherwise the direction is up.
  *)
 
PROCEDURE CAST (<targettype>; val: <anytype>): <targettype>;
  (* CAST is a type transfer function.  Given the expression denoted by val, it returns
     a value of the type <targettype>.  An invalid value for the target value or a
     physical address alignment problem may raise an exception.
  *)
 
PROCEDURE TSIZE (<type>; ... ): CARDINAL;
  (* Returns the number of LOCS used to store a value of the specified <type>.   The
     extra parameters, if present, are used to distinguish variants in a variant record.
  *)
 
END SYSTEM.

DEFINITION MODULE Strings;
 
  (* Facilities for manipulating strings *)
 
TYPE
  String1 = ARRAY [0..0] OF CHAR;
    (* String1 is provided for constructing a value of a single-character string type from a
       single character value in order to pass CHAR values to ARRAY OF CHAR parameters.
    *)
 
PROCEDURE Length (stringVal: ARRAY OF CHAR): CARDINAL;
  (* Returns the length of stringVal (the same value as would be returned by the
     standard function LENGTH).
  *)
 
 
(* The following seven procedures construct a string value, and attempt to assign it to a
   variable parameter.  They all have the property that if the length of the constructed string
   value exceeds the capacity of the variable parameter, a truncated value is assigned, while
   if the length of the constructed string value is less than the capacity of the variable
   parameter, a string terminator is appended before assignment is performed.
*)
 
PROCEDURE Assign (source: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
  (* Copies source to destination *)
 
PROCEDURE Extract (source: ARRAY OF CHAR; startPos, numberToExtract: CARDINAL;
                   VAR destination: ARRAY OF CHAR);
  (* Copies at most numberToExtract characters from source to destination, starting at position
     startPos in source.
  *)
 
PROCEDURE Delete (VAR stringVar: ARRAY OF CHAR; startPos, numberToDelete: CARDINAL);
  (* Deletes at most numberToDelete characters from stringVar, starting at position
     startPos.
  *)
 
PROCEDURE Insert (source: ARRAY OF CHAR; startPos: CARDINAL;
                  VAR destination: ARRAY OF CHAR);
  (* Inserts source into destination at position startPos *)
 
PROCEDURE Replace (source: ARRAY OF CHAR; startPos: CARDINAL;
                   VAR destination: ARRAY OF CHAR);
  (* Copies source into destination, starting at position startPos. Copying stops when
     all of source has been copied, or when the last character of the string value in
     destination has been replaced.
  *)
 
PROCEDURE Append (source: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
  (* Appends source to destination. *)
 
PROCEDURE Concat (source1, source2: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
  (* Concatenates source2 onto source1 and copies the result into destination. *)
 
(* The following predicates provide for pre-testing of the operation-completion
   conditions for the procedures above.
*)
 
PROCEDURE CanAssignAll (sourceLength: CARDINAL; VAR destination: ARRAY OF CHAR): BOOLEAN;
  (* Returns TRUE if a number of characters, indicated by sourceLength, will fit into
     destination; otherwise returns FALSE.
  *)
 
PROCEDURE CanExtractAll (sourceLength, startPos, numberToExtract: CARDINAL;
                         VAR destination: ARRAY OF CHAR): BOOLEAN;
  (* Returns TRUE if there are numberToExtract characters starting at startPos and
     within the sourceLength of some string, and if the capacity of destination is
     sufficient to hold numberToExtract characters; otherwise returns FALSE.
  *)
 
PROCEDURE CanDeleteAll (stringLength, startPos, numberToDelete: CARDINAL): BOOLEAN;
  (* Returns TRUE if there are numberToDelete characters starting at startPos and
     within the stringLength of some string; otherwise returns FALSE.
  *)
 
PROCEDURE CanInsertAll (sourceLength, startPos: CARDINAL;
                        VAR destination: ARRAY OF CHAR): BOOLEAN;
  (* Returns TRUE if there is room for the insertion of sourceLength characters from
     some string into destination starting at startPos; otherwise returns FALSE.
  *)
 
PROCEDURE CanReplaceAll (sourceLength, startPos: CARDINAL;
                         VAR destination: ARRAY OF CHAR): BOOLEAN;
  (* Returns TRUE if there is room for the replacement of sourceLength characters in
     destination starting at startPos; otherwise returns FALSE.
  *)
 
PROCEDURE CanAppendAll (sourceLength: CARDINAL; VAR destination: ARRAY OF CHAR): BOOLEAN;
  (* Returns TRUE if there is sufficient room in destination to append a string of
     length sourceLength to the string in destination; otherwise returns FALSE.
  *)
 
PROCEDURE CanConcatAll (source1Length, source2Length: CARDINAL;
                        VAR destination: ARRAY OF CHAR): BOOLEAN;
  (* Returns TRUE if there is sufficient room in destination for a two strings of
     lengths source1Length and source2Length; otherwise returns FALSE.
  *)
 
(* The following type and procedures provide for the comparison of string values, and for the
   location of substrings within strings.
*)
 
TYPE
  CompareResults = (less, equal, greater);
 
PROCEDURE Compare (stringVal1, stringVal2: ARRAY OF CHAR): CompareResults;
  (* Returns less, equal, or greater, according as stringVal1 is lexically less than,
     equal to, or greater than stringVal2.
  *)
 
PROCEDURE Equal (stringVal1, stringVal2: ARRAY OF CHAR): BOOLEAN;
  (* Returns Strings.Compare(stringVal1, stringVal2) = Strings.equal *)
 
PROCEDURE FindNext (pattern, stringToSearch: ARRAY OF CHAR; startPos: CARDINAL;
                    VAR patternFound: BOOLEAN; VAR posOfPattern: CARDINAL);
  (* Looks forward for next occurrence of pattern in stringToSearch, starting the search at
     position startPos. If startPos < LENGTH(stringToSearch) and pattern is found,
     patternFound is returned as TRUE, and posOfPattern contains the start position in
     stringToSearch of pattern. Otherwise patternFound is returned as FALSE, and posOfPattern
     is unchanged.
  *)
 
PROCEDURE FindPrev (pattern, stringToSearch: ARRAY OF CHAR; startPos: CARDINAL;
                    VAR patternFound: BOOLEAN; VAR posOfPattern: CARDINAL);
  (* Looks backward for the previous occurrence of pattern in stringToSearch and returns the
     position of the first character of the pattern if found. The search for the pattern
     begins at startPos. If pattern is found, patternFound is returned as TRUE, and
     posOfPattern contains the start position in stringToSearch of pattern in the range
     [0..startPos]. Otherwise patternFound is returned as FALSE, and posOfPattern is unchanged.
  *)
 
PROCEDURE FindDiff (stringVal1, stringVal2: ARRAY OF CHAR;
                    VAR differenceFound: BOOLEAN; VAR posOfDifference: CARDINAL);
  (* Compares the string values in stringVal1 and stringVal2 for differences. If they
     are equal, differenceFound is returned as FALSE, and TRUE otherwise. If
     differenceFound is TRUE, posOfDifference is set to the position of the first
     difference; otherwise posOfDifference is unchanged.
  *)
 
PROCEDURE Capitalize (VAR stringVar: ARRAY OF CHAR);
  (* Applies the function CAP to each character of the string value in stringVar. *)
 
END Strings.

DEFINITION MODULE SysClock;
 
(* Facilities for accessing a system clock that records the date and time of day *)
 
CONST
  maxSecondParts = <implementation-defined integral value>;
   
TYPE
  Month    = [1 .. 12];
  Day      = [1 .. 31];
  Hour     = [0 .. 23];
  Min      = [0 .. 59];
  Sec      = [0 .. 59];
  Fraction = [0 .. maxSecondParts];
  UTCDiff  = [-780 .. 720];
  DateTime =
    RECORD
      year:      CARDINAL;
      month:     Month;
      day:       Day;
      hour:      Hour;
      minute:    Min;
      second:    Sec;
      fractions: Fraction;      (* parts of a second *)
      zone:      UTCDiff;       (* Time zone differential factor which is the number
                                   of minutes to add to local time to obtain UTC. *)
      summerTimeFlag: BOOLEAN;  (* Interpretation of flag depends on local usage. *)
    END;
   
PROCEDURE CanGetClock (): BOOLEAN;
  (* Returns TRUE if a system clock can be read; FALSE otherwise *)
   
PROCEDURE CanSetClock (): BOOLEAN;
  (* Returns TRUE if a system clock can be set; FALSE otherwise *)
   
PROCEDURE IsValidDateTime (userData: DateTime): BOOLEAN;
  (* Returns TRUE if the value of userData represents a valid date and time; FALSE otherwise *)
   
PROCEDURE GetClock (VAR userData: DateTime);
  (* If possible, assigns system date and time of day to userData *)
   
PROCEDURE SetClock (userData: DateTime);
  (* If possible, sets the system clock to the values of userData *)
   
END SysClock.