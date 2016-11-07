This is a list of the standard (or built-in) procedures of Modula-2, Stony Brooks variety.

Standard procedures
The Modula-2 language includes a set of standard procedures that are built into the compiler and therefore are always available without importing them.
This chapter lists the standard procedures, their uses, allowed parameter types, and the type returned, if any.

ABS
PROCEDURE ABS(X : type) : type;
The absolute value function.  X can be any signed integer type, REAL, LONGREAL, COMPLEX or LONGCOMPLEX.  The result is the same type as the parameter.
The absolute value of a positive number is itself, and the absolute value of a negative number is the negative of the number.
The absolute value of a complex number is its magnitude. (Remember magnitude is defined as SQRT(real2 + imaginary2))

CAP
PROCEDURE CAP(ch : AnyCharType) : AnyCharType;
Converts ch to uppercase.  Characters other than lowercase letters in are left unchanged by this function.
For operating systems that support many languages this function returns correct results for the language the operating system has been setup to use.

CHR
PROCEDURE CHR(val : type) : CHAR;
Converts a numeric value to a value of type CHAR.  The value can be any signed or unsigned integer type.  The result is the character whose ordinal value is val.
If assignment checking is on, val is checked for a value in the range of type CHAR.

DEC
PROCEDURE DEC(VAR v : type);
PROCEDURE DEC(VAR v : type; amount : CARDINAL);
Decreases the value of variable v by one or by a specified amount. If an amount is not passed a value of one is assumed for amount. The variable can be of any signed or unsigned integer type, any character type, any boolean type or any enumeration type. The amount must be assignable to CARDINAL.

DISPOSE
PROCEDURE DISPOSE(VAR P : PointerType {;tag : tagtype});
Releases the storage allocated to a pointer by the NEW standard procedure or the ALLOCATE procedure.
The optional tag expressions are the tags of variant parts in a record that is being disposed.  Each tag expression must be a compile-time constant. The values of the tags must be identical to those given when the object was allocated.
When you dispose of dynamic arrays you do not supply the array HIGH bounds to DISPOSE as you did with NEW. The compiler will fetch these values automatically for you from the dynamic array data.
A call to DISPOSE is translated to a call to DEALLOCATE. In order to use DISPOSE, you must define the procedure DEALLOCATE.  Usually, you import DEALLOCATE from the ISO module Storage. You can, however, define the procedure yourself.

EXCL
PROCEDURE EXCL(SetVar : SetType; Element : ElementType);
Excludes an element from a SET or PACKEDSET variable. SetVar must be a variable of a set type and Element must be an expression resulting in a value of the element type of the set. The specified element is removed from the value of the set variable.

FLOAT
PROCEDURE FLOAT(num : AnyIntegerType) : REAL;
Converts the integer number num to REAL. num can be any signed or unsigned integer type.

LENGTH
PROCEDURE LENGTH(str : ARRAY OF AnyCharacterType) : CARDINAL;
Returns the length in characters of the passed string. The string passed to length can be an ANSI or Unicode string.

LFLOAT
PROCEDURE LFLOAT(num : AnyIntegerType) : LONGREAL;
Converts the integer number num to LONGREAL. num can be any signed or unsigned integer type.

HALT
PROCEDURE HALT;
PROCEDURE HALT(status : CARDINAL);
Halts the execution of a program.  Use the second form to return a status to the operating system.  This is useful to inform the program that executed the current program about the success or lack thereof of the execution.  If no status is given, 0 is the default value is returned.

HIGH
PROCEDURE HIGH(ArrayVar : ArrayType) : CARDINAL;
Returns the upper bound of an open array parameter variable.
In extended syntax mode you can pass any array variable to the HIGH function.
You can access all dimensions of an array with more than one dimension by passing the appropriate array dimension to HIGH. For example, to get the high bound of the second dimension of an array type with two or more dimensions you would subscript the array variable with one index, thus leaving the second dimension. The value used to index the array is not used since you are not accessing an array element but are informing the compiler which array HIGH dimension to retrieve. Typically you would use the value 0 for the subscript.
Example:
PROCEDURE test(arr : ARRAY OF ARRAY OF REAL);
.
.
.
   FOR i := 0 TO HIGH(arr) DO (* first dimension )
       FOR j := 0 TO HIGH(arr[0]) DO ( second dimension *)

INC
PROCEDURE INC(VAR v : type);
PROCEDURE INC(VAR v : type; amount : CARDINAL);
Increases the value of variable v by one or by a specified amount.  If an amount is not passed a value of one is assumed for amount. The variable can be of any signed or unsigned integer type, any character type, any boolean type or any enumeration type.  The amount must be assignable to CARDINAL.

INCL
PROCEDURE INCL(SetVar : SetType; Element : ElementType);
Includes an element in a SET or PACKEDSET variable.  SetVar must be a variable of a set type, and Element must be an expression resulting in a value of the element type of the set.  The specified element is added to the value of the set variable.

INT
PROCEDURE INT(Val : ValType) : INTEGER;
Converts Val to INTEGER.  Val can be any signed or unsigned integer type, any character type, a boolean type or any enumeration type.  For character, boolean and enumeration types, the returned value is the ordinal value of the parameter.

MAX
PROCEDURE MAX(ScalarType) : ScalarType;
Returns the maximum value of a scalar type.  The parameter is a type name, which must be a signed or unsigned integer type, a real type, a character type, a boolean type, a subrange, or an enumeration type.  The type returned by MAX is the same as the type of the parameter.

MIN
PROCEDURE MIN(ScalarType) : ScalarType;
Returns the minimum value of a scalar type.  The parameter is a type name, which must be signed or unsigned integer type, a real type, a character type, a boolean type, a subrange, or an enumeration type.  The type returned by MIN is the same as the type of the parameter.

NEW
PROCEDURE NEW(VAR P : PointerType {;tag : tagtype});
PROCEDURE NEW(VAR P : DynamicArrayType; highBounds : CARDINAL
            {; highBounds : CARDINAL});
The optional tag expressions are the tags of variant parts in a record that is being allocated.  Each tag expression must be a compile-time constant.  The values of the tags determine how much storage is allocated.
For dynamic arrays you must specify a high bound for each array dimension declared in the dynamic array declaration. Remember that you are specifying the high bound for the dimension, and the lower bound is assumed to be zero, and therefore the size of each dimension is the highbound+1.
Example:
PROCEDURE AllocateMatrix(VAR m : Matrix; order : CARDINAL);
BEGIN
   (* order is the number of element in each dimension )
   ( therefore the high bound is one less than this )
   ( square matrix *)
   NEW(Matrix, order-1, order-1);
END AllocateMatrix;
A call to NEW is translated to a call to ALLOCATE. In order to use NEW, you must define the procedure ALLOCATE. Usually, you import ALLOCATE from the ISO module Storage. You can, however, define the procedure yourself.

ODD
PROCEDURE ODD(Val : ValType) : BOOLEAN;
Returns TRUE if Val is ODD and FALSE otherwise.  Val can be any signed or unsigned integer type.

ORD
PROCEDURE ORD(Val : ValType) : CARDINAL;
Converts Val to CARDINAL.  Val can be INTEGER, LONGINT, LONGCARD, CHAR, BOOLEAN, or any enumeration type.  For CHAR, BOOLEAN and enumeration types, the returned value is the ordinal value of the parameter.

RE
PROCEDURE RE(Val : COMPLEX) : REAL;
PROCEDURE RE(Val : LONGCOMPLEX) : LONGREAL;
Returns the real part of a complex type.

IM
PROCEDURE IM(Val : COMPLEX) : REAL;
PROCEDURE IM(Val : LONGCOMPLEX) : LONGREAL;
Returns the imaginary part of a complex type.

CMPLX
PROCEDURE CMPLX(realPart, imaginaryPart : REAL) : COMPLEX;
PROCEDURE CMPLX(realPart, imaginaryPart : LONGREAL) : LONGCOMPLEX;
Returns a complex type where realPart and imaginaryPart compose the complex type. Note that this is the same syntax used to compose a complex constant. If realPart and imaginaryPart are constants then CMPLX returns a complex constant compatible with a complex types. If realPart or imaginaryPart are not compile time constants then CMPLX can only be used in a statement.

SIZE
PROCEDURE SIZE(item) : IntConst;
Returns the number of bytes of storage occupied by item.  item can be either a variable or a type name.  Variables can be fully qualified with subscripts or record field references. The type returned by this procedure is compatible with signed and unsigned integer types.

TRUNC
PROCEDURE TRUNC(RealVal : RealType) : CARDINAL;
Converts a REAL or LONGREAL value to CARDINAL by truncation; the fractional part of the real number is discarded.  The result is undefined if RealVal is out of the range of CARDINAL.

VAL
PROCEDURE VAL(ConvertToType; Value : AnyPervasiveType) : Type;
Converts a value from one type to another.  This function can convert  any predefined pervasive type to any other pervasive type. With the following exceptions.
You cannot convert an enumeration type to a real or complex type. Remember that boolean and character types are defined as enumeration types. 
You cannot convert a real type to a complex type. 
You cannot convert a complex type to a real type. 
The following are TRUE:
   INT(...)       = VAL(INTEGER, ...);
   ORD(...)       = VAL(CARDINAL, ...);
   FLOAT(...)     = VAL(REAL, ...);
   LFLOAT(...)    = VAL(LONGREAL, ...);
   TRUNC(...)     = VAL(CARDINAL, ...);


--------------------------------------------------------------------------------



PROCEDURE ABS(X : type) : type;
X can be INTEGER, LONGINT, REAL, or LONGREAL.

PROCEDURE CAP(CH : CHAR) : CHAR;

PROCEDURE CHR(val : type) : CHAR;
the value can be INTEGER, CARDINAL, LONGINT, or LONGCARD.

PROCEDURE DEC(VAR v : type);
PROCEDURE DEC(VAR v : type; amount : CARDINAL);
PROCEDURE INC(VAR v : type);
PROCEDURE INC(VAR v : type; amount : CARDINAL);
Changes the value of variable v by one or by a specified amount.  The
variable can be INTEGER, CARDINAL, LONGINT, LONGCARD, CHAR, BOOLEAN, 
ADDRESS, or any enumeration type.  The amount must be assignable to 
CARDINAL.

PROCEDURE DISPOSE(VAR P : pointertype {;tag : tagtype});
The values of the tags must be identical to those given when the object was
allocated.  
A call to DISPOSE is translated to a call to DEALLOCATE.  In order to
use it, you must define the procedure DEALLOCATE.  Usually this is
done by importing it from the module Storage or ShortStorage.  The
procedure can be user defined, however.

PROCEDURE EXCL(SetVar : SetType; Element : ElementType);
PROCEDURE INCL(SetVar : SetType; Element : ElementType);
Excludes or includes an element from a set variable.  SetVar must be a
variable of a set type and Element must be an expression resulting in
a value of the element type of the set.  The specified element is
removed from the value of the set variable.

PROCEDURE FLOAT(num : LONGINT) : REAL;
PROCEDURE LONGFLOAT(num : LONGINT) : LONGREAL;
num can be INTEGER, CARDINAL, or LONGINT.

PROCEDURE HALT;
PROCEDURE HALT(status : CARDINAL);
Halts the execution of a program.  Use the second from to return a status 
to DOS, which can be checked from within a batch file.  If no status is 
given, 0 is returned.

PROCEDURE HIGH(ArrayVar : ArrayType) : CARDINAL;

PROCEDURE LONG(Val : ValType) : LONGINT;
PROCEDURE LONG(R : REAL) : LONGREAL;
Val can be INTEGER, CARDINAL, LONGCARD, CHAR, BOOLEAN, or any
enumeration type.  In the case of CHAR, BOOLEAN and enumeration
types, the returned value is the ordinal value of the parameter.

PROCEDURE MAX(ScalarType) : ScalarType;
PROCEDURE MIN(ScalarType) : ScalarType;
Returns the maximum or minimum value of a scalar type.  The parameter
is a type name, which must be INTEGER, CARDINAL, LONGCARD, CHAR,
BOOLEAN, or an enumeration type.  The type returned is the same as
the type of the parameter.

PROCEDURE NEW(VAR P : pointertype {;tag : tagtype});
The tag expression must be a compile-time constant.
A call to NEW is translated to a call to ALLOCATE.  In order to
use it, you must define the procedure ALLOCATE.  Usually this is
done by importing it from the module Storage or ShortStorage.  The
procedure can be user defined, however.

PROCEDURE ODD(Val : ValType) : BOOLEAN;
Returns TRUE if Val is ODD, FALSE otherwise.  Val can be INTEGER,
CARDINAL, LONGINT, OR LONGCARD.

PROCEDURE ORD(Val : ValType) : CARDINAL;
PROCEDURE LONGORD(Val : ValType) : LONGCARD;
Val can be INTEGER, CARDINAL, LONGCARD, CHAR, BOOLEAN, or any
enumeration type.  In the case of CHAR, BOOLEAN and enumeration
types, the returned value is the ordinal value of the parameter.

PROCEDURE SHORT(Val : ValType) : INTEGER;
PROCEDURE SHORT(R : LONGREAL) : REAL;
Val can be INTEGER, CARDINAL, LONGCARD, CHAR, BOOLEAN, or any
enumeration type.  In the case of CHAR, BOOLEAN and enumeration
types, the returned value is the ordinal value of the parameter.

PROCEDURE SIZE(item) : IntConst;
Returns the number of bytes of storage occupied by item.  item can be
either a variable or a type name.  The type returned by this
procedure is compatible with INTEGER, CARDINAL LONGINT, and LONGCARD.

PROCEDURE TRUNC(RealVal : RealType) : CARDINAL;
PROCEDURE LONGTRUNC(RealVal : RealType) : LONGINT;
Converts REAL or LONGREAL value to respective target type by
truncation.  The result is undefined if RealVal is out of the range
of the target type.

PROCEDURE VAL(Type; Value : CARDINAL) : Type;
Converts a CARDINAL value to a scalar type.  Type must be INTEGER,
CARDINAL, LONGCARD, CHAR, BOOLEAN, or any enumeration type.
If assignment checking is on, this procedure checks to make sure
Value is between MIN(Type) and MAX(Type), inclusive.

SYSTEM module
ISO Modula-2 allows the SYSTEM module to be extended by an implementation. Unless otherwise noted, all items in SYSTEM are ISO Standard. Also note that while many symbol declarations are standard, the exact value of the declaration can vary from implementation to implementation.
ISO Modula-2 defines the type LOC as the minimum addressable piece of memory the machine can support. For all Stony Brook implementations this is 1 byte.

CONST
   BITSPERLOC    = 8; (*implementation defined *)
   LOCSPERBYTE = 1;(* implementation defined *)
   LOCSPERWORD = 2;(* implementation defined *)
   LOCSPERDWORD = 4;(* extension *)
   LOCSPERMACHINEWORD = 2 or 4;(* extension *)(* 2 in 16-bit mode, 4 in 32-bit mode *)

TYPE
   LOC = implementation defined = 1 byte of memory
   BYTE = LOC; (* this may not be true for other implementations *)
   WORD = ARRAY [0..LOCSPERWORD-1] OF LOC;
   DWORD = ARRAY [0..LOCSPERDWORD-1] OF LOC; (* extension *)
   MACHINEWORD = ARRAY [0..LOCSPERMACHINEWORD-1] OF LOC; (* extension *)
   NEARADDRESS = NEAR POINTER TO LOC; (* extension *)
   FARADDRESS = FAR POINTER TO LOC;  (* extension *)

The definition of type ADDRESS depends on the data memory model used for compiling. This can be different for each module you compile, thus the SYSTEM module adjusts to the module currently being compiled. Remember that 32-bit mode, and all non IA-32 processors, always use the Small data memory model.

IF DataModel = Small THEN
   ADDRESS = NEARADDRESS;
IF (DataModel = Medium) OR (DataModel = Large) THEN
   ADDRESS = FARADDRESS;

NEARADDRESS is the same size as the CARDINAL type.
FARADDRESS is twice the size of the NEARADDRESS type. FARADDRESS can also b
e accessed like a RECORD type to access the offset and segment components of a segmented address.
FARADDRESS =
  RECORD
      OFFSET     : CARDINAL;
      SEGMENT    : CARDINAL16;
  END;

FARADDRESS is not a normal RECORD type, and is not treated like a RECORD type by the compiler, except to allow you to access the offset and segment components of the address.

SYSTEM types compatibility rules

Types following types from the SYSTEM module have special compatibility rules when they are used in procedure parameter declarations, BYTE, WORD, DWORD and MACHINEWORD. When a formal parameter type is of one of these types then anything that is the same size as the type in question is parameter compatible with the type. This includes VAR parameters as well as value parameters. Thus a WORD parameter will accept any type that is the same size a WORD.

For open array parameters with a special system type as the root type of the open array, any type is compatible with the open array parameter if its SIZE is an even multiple of the system type size.

The above special compatibility rules allow you to write procedures that will accept all types. For example

PROCEDURE WriteBlock(f : File; data : ARRAY OF LOC);

A procedure like this will accept any type that can be declared, and inside the procedure you can determine the size of the type passed with the HIGH function. This procedure can accept anything because it uses the LOC type, BYTE could also have been used, and LOC is the smallest addressable unit. If the WORD type were used then a 3 byte type could not be passed to the procedure since 3 is not an even multiple of 2 which is the size of WORD.

NOTE: Stony Brook extends system type parameter compatibility to all assignment compatibility situations as well.

The ADDRESS type is fully assignment compatible with all POINTER types of the same size. Thus for example the ADDADR function in this module can accept any POINTER type. This includes VAR parameters.

ADDRESS Composition and Arithmetic

PROCEDURE ADDADR(addr : AddressType; offs : CARDINAL) : AddressType;
PROCEDURE SUBADR(addr : AddressType; offs : CARDINAL) : AddressType;
PROCEDURE DIFADR(left, right : AddressType) : INTEGER;


The AddressType in the above procedures will accept all pointer type in addition to type ADDRESS.

PROCEDURE MAKEADR(implementation defined) : ADDRESS;

Because of the segmented nature of the IA-32 processors we have two implementations of MAKEADR.

PROCEDURE MAKEADR(offs : CARDINAL) : NEARADDRESS;
PROCEDURE MAKEADR(seg, offs : CARDINAL) : FARADDRESS;

For all other processors the following is defined.

PROCEDURE MAKEADR(offs : CARDINAL) : ADDRESS;

If all parameters to MAKEADR are constants then MAKEADR results in a constant that can be used anywhere a compile time constant expression is required.

TSIZE Function

PROCEDURE TSIZE(TypeName [{,variant tag value}]) : size in LOCs;

TSIZE returns the size of the TypeName passed to it. If the type passed to TSIZE is a variant record, you can pass constant values for the tag fields of the variants and the returned size will be the size of the record with only the selected variants which may be different than the size of the record otherwise.

ADR Function

PROCEDURE ADR(VAR VarOrProc) : ADDRESS;

PROCEDURE NEARADR(VAR VarOrProc) : NEARADDRESS; (* extension *)

PROCEDURE FARADR(VAR VarOrProc) : FARADDRESS; (* extension *)

These functions return the address of any type of variable or procedure.

CAST Function

PROCEDURE CAST(TypeName, expression) : Expression of TypeName type;

Use this function to change the type of an expression to any other type. No checking of any kind is done for the correctness of the conversion. Type casts are a potentially non portable construct and as such the CAST function is imported from SYSTEM to signal such source code usage.

SHIFT and ROTATE Functions

PROCEDURE SHIFT(PackedSetType; shift : INTEGER) : PackedSetType;
PROCEDURE ROTATE(PackedSetType; shift : INTEGER) : PackedSetType;

If shift > 0 then the set is shifted from lower ordinal positions to higher ordinal positions. A left shift.

If shift < 0 then the set is shifted from higher ordinal positions to lower ordinal positions. A right shift.

Note that you can only pass a PACKEDSET type, and not a SET type, to SHIFT and ROTATE. Also Stony Brook Modula-2 restricts the PACKEDSET type size to have exactly the following number of elements.

8, 16, or an even multiple of (BITSPERLOC * LOCSPERMACHINEWORD) set elements.

The following variables, types and procedures in the SYSTEM module are all extended syntax.

Extended types
CARDINAL8, CARDINAL16, CARDINAL32, LONGCARD, SHORTCARD,
INTEGER8, INTEGER16, INTEGER32, LONGINT, SHORTINT,
BYTEBOOL, BOOL8, WORDBOOL, BOOL16, DWORDBOOL, BOOL32, ACHAR, UCHAR
These types are all pervasive types in the compiler so you do not need to import them from this module. However you may want to since it documents them as extended features that are being used.
The following types are compatible with the extended types defined in the Macintosh p1 compiler.
CARD8 = CARDINAL8;
CARD16 = CARDINAL16;
CARD32 = CARDINAL32;
INT8 = INTEGER8;
INT16 = INTEGER16
INT32 = INTEGER32
WORD8 = BYTE
WORD16 = WORD
WORD32 = DWORD
SET8 = PACKEDSET [0..7];
SET16 = PACKEDSET [0..15]
SET32 = PACKEDSET [0..31];

PSP Variable (extension)
VAR PSP : ADDRESS;
This is a pointer to the program segment prefix. This variable is available to DOS, 32-bit DOS extended and 16-bit Windows programs.

ENV Variable (extension)
VAR ENV : ADDRESS;
This is a pointer to the environment strings.

CMD Variable (extension)
VAR CMD : ADDRESS;
This is a pointer to the command line string for the program.

CPU Variable (extension)
VAR CPU : CARDINAL;
This variable identifies the specific CPU model the program is running on. The possible values are:
0 = 8086
1 = 80186
2 = 80286
3 = 80386
4 = 80486
5 = Pentium
6 = Pentium II/III

FPP Variable (extension)
VAR FPP : CARDINAL;
This variable identifies what if any floating point processor is installed. The possible values are:
0 = No FPU
1 = 8087
2 = 80287
3 = 80387 or greater

CPUCOUNT Variable (extension)
VAR CPUCOUNT : CARDINAL;
This variable gives the number of installed processors in the computer.

CPU_HYPERTHREAD Variable (extension)
VAR CPU_HYPERTHREAD : BOOLEAN;
This variable indicates if the processors in the computer are hyper-threaded.

CSALIAS Variable (extension)
VAR CSALIAS : CARDINAL16;
For 32-bit DOS extended systems CSALIAS gives you a variable that allows you to write to the code segment of the program without a processor fault occurring.

EXITCODE Variable (extension)
VAR EXITCODE : CARDINAL;
EXITCODE contains the value passed to the HALT procedure. Procedures installed into the termination chain may want to check the exit value of the program.

BuildNumber Variable (extension)
VAR BuildNumber : CARDINAL;
This variable contains the build number. This value is inserted by the linker via a linker option. The value is user defined.

DebuggerPresent Variable (extension)

VAR DebuggerPresent : BOOLEAN;
This variable is TRUE if the program is being debugged by the Stony Brook Debugger. Otherwise the value is FALSE.

OFFS Procedure (extension)
PROCEDURE OFFS(TypeOrVariableName.recordField{.recordField}) : ConstantValue;
OFFS returns a compile time constant value that is equal to the offset of the specified record field from the beginning of the record type.
Example:

TYPE
   RecType =
     RECORD
       x, y, z : CARDINAL;
     END;

CONST
   yOffset = OFFS(RecType.y);

VAR
   v : RecType;

BEGIN

   v.x := OFFS(v.z);

SYSHALT Procedure (extension)

PROCEDURE SYSHALT(exitVal : CARDINAL);

This procedure is exactly like the standard procedure HALT, except that if this procedure is called, then TERMINATION.HasHalted() = FALSE. Whereas if HALT is called then TERMINATION.HasHalted() = TRUE.

HALTPROCTYPE Type (extension)

TYPE
   HALTPROCTYPE = PROCEDURE(CARDINAL);

 

PROCEDURE HALTPROC(hp : HALTPROCTYPE) : HALTPROCTYPE;

Thus function procedure allows you to install a procedure that is called before the program is terminated by the operating system. The ISO standard termination code for static, i.e. IMPLEMENTATION, modules by default installs a procedure for this support. HALTPROC returns the currently installed procedure so that you can chain the system or override the currently installed system.

UNREFERENCED_PARAMETER Procedure (extension)

PROCEDURE UNREFERENCED_PARAMETER(AnyParameter);

The compiler generates a warning when a parameter of a procedure is not referenced within that procedure.  In some instances it is necessary to have an unreferenced parameter within a procedure.  The UNREFERENCED_PARAMETER procedure can be used to suppress warnings on unreferenced parameters.

SOURCEFILE Variable (extension)

This identifier represents a string literal whose value is the filename of the source file being compiled.

SOURCELINE Variable (extension)

This identifier represents a numeric constant whose value is the source line number where this instance of the identifier is used.

ASSERT Statement (extension)

This is a statement that checks a BOOLEAN expression for TRUE and if not raises an ASSERT exception. The assert exception will identify the module and line number of the failed ASSERT statement. ASSERT will generate this test code, if and only if, the version tag M2ASSERT is set, otherwise no code for the ASSERT statement is generated. The format of the ASSERT statement is:

ASSERT(BooleanExpression);

Example:

Assume the M2ASSERT version tag is set.

PROCEDURE foo(ptr : ADDRESS);
BEGIN
   ASSERT(ptr <> NIL);
   (* will never get here if ptr = NIL *)
   (* think of the ASSERT statement like this
   %IF M2ASSERT %THEN
IF ptr = NIL THEN
        RAISE(...);
END;
   %END
   *)
END foo;

ISASSERT function (extension)

PROCEDURE ISASSERT() : BOOLEAN;

This is a function that returns a BOOLEAN value. You can use this to trap ASSERT exceptions. It returns TRUE if the current exception is an ASSERT exception.

EXCEPTADR Procedure (extension)

PROCEDURE EXCEPTADR() : ADDRESS;

This is a function that returns the address where the current exception occurred. It will return NIL if there is no exception.

EXCEPT_INFO Procedure (extension)

PROCEDURE EXCEPT_INFOADR(VAR OUT addr : ADDRESS;

                         VAR OUT lineNumber : CARDINAL;

                         VAR OUT moduleName : ARRAY OF ACHAR);

This is a function that returns information about the current exception. The procedure will return NIL in addr if there is no exception. If the exception occurred because of a runtime check, then the lineNumber and moduleName parameters will return the location of the checking error. If the exception did not result from a runtime check, the lineNumber will return 0, and modulename will return an empty string.

SetUnhandledExceptionProc Procedure (extension)

PROCEDURE SetUnhandledExceptionProc(unhandled : PROC);

This procedure lets you set a procedure that will be called when an exception goes unhandled.

AttachDebugger procedure (extension, Win32 only) 

TYPE

   AttachDebuggerOpt =

   (

   DoNotAttach,    (* do not attach, the default *)

   AttachExternal,(* attach on EXCEPTIONS.sysException only. this will primarily be access violations *)

   AttachAll (* attach on all raised exceptions. this includes ALL Modula-2 exceptions *)

)   ;

PROCEDURE AttachDebugger(opt : AttachOptions);

Use this procedure to enable or disable just in time debugging support.

OutputDebugMessage procedure (extension)

PROCEDURE OutputDebugMessage(str : ARRAY OF CHAR);

This procedure actually does nothing except that our debugger for Win32 and Unix systems will trap calls to this procedure and display the passed parameter in the debug messages window.

EnableCallTrace procedure (extension, 32-bit only)

PROCEDURE EnableCallTrace;

By calling this procedure you enable the runtime system to perform a call trace when an exception of any kind is raised. If the exception goes unhandled then the call trace will be output to disk. If the exception is handled then the information is lost.

OutputCallTrace procedure (extension, 32-bit only)

PROCEDURE OutputCallTrace;

This procedure is only used with EnableCallTrace. If you want to handle an exception, but still want the call trace output to disk then you should call this procedure inside your exception handler. For example this allows you to handle all exceptions, hopefully terminating gracefully, and still getting a call trace which may help you debug the problem.

TrapAccessViolations procedure (extension, Unix systems only)

PROCEDURE TrapAccessViolations;

The procedure is used in shared objects to enable trapping access violations. Main programs automatically trap access violations. Access violations consist of the SIGSEGV, SIGBUS and SIGILL signals. These signals are global to an entire process, therefore our runtime system does assume it can possess these signals when running in a shared object. Trapping an access violation means the violation is converted to a native language exception.

Note that it is still possible for code to override our runtime system signal handlers for these signals. This can in main programs and shared objects. When this occurs the violation will not be trapped by our runtime system.

VA_START, VA_ARG procedures (extension)

PROCEDURE VA_START(VAR OUT addr : ADDRESS);

PROCEDURE VA_ARG(VAR INOUT addr : ADDRESS; TypeIdentifier) : ADDRESS;

This procedures are used to support accessing the parameters of a procedure which accepts a variable number of parameters (the VARIABLE procedure attribute). You use VA_START to get the address of the first variable argument. You then use VA_ARG for each argument passing the address VA_START initialized. The second parameter of the VA_ARG function is the type identifier of the argument you are about to read. The function returns a pointer to the data. The function also updates the parameter address, addr.

PROCEDURE example(format : ARRAY OF CHAR) [RightToleft, Leaves, Variable];
TYPE
Pointer = POINTER TO CARDINAL;(*all pointers are the same size*)
VAR
addr : ADDRESS;
ptrC : POINTER TO CARDINAL;
ptrCh : POINTER TO CHAR;
BEGIN
VA_START(addr);
   
ptrC := VA_ARG(addr, CARDINAL);
(* use ptrC *)
   
ptrCh := VA_ARG(addr, Pointer);
(* use ptrCh *)
END example;

CLONE procedure (extension)

PROCEDURE CLONE(VAR newObject : <some_class_type>;

                sourceObject : <some_class_type>);

This procedure creates an exact copy of the object referenced by sourceObject and stores a reference to the new object in the variable denoted by newObject.

A call to CLONE shall have two actual parameters. The first parameter shall be a variable designator of class type. The second parameter shall be an expression of a class type assignment compatible to the type of the first parameter.

If the type of the first parameter denotes an untraced class, the identifier "ALLOCATE" must be visible as required by the pervasive procedure "NEW".

Note: The formal type of the second parameter is the type of the first parameter.

If the first parameter to a call to CLONE is a variable of the traced class type, the use traced object storage to allocate storage for the object. For an untraced object, storage is allocated for the new object by a call to ALLOCATE. If storage cannot be allocated for the object, the value EMPTY shall be assigned to newObject, otherwise a reference to the new object is stored in newObject.

If storage allocation was successful, then the data in sourceObject is copied to newObject. If the first parameter to a call to CLONE is a variable of a traced class type, then the garbage collector shall be informed of a new traced variable in respect of the new value and then informed of a defunct traced variable in respect of the old value in sourceObject.

FUNC Keyword (extension)

This keyword allows you call a function procedure as a procedure, thus ignoring the result. No Warning will be generated in extended syntax mode, and no error generated in ISO mode.

Example:

BEGIN
   returnVal := MyFunction(param1, param2);
   FUNC MyFunction(param1, param2);
END;

FIXME Procedure (extension)

PROCEDURE FIXME(str : ARRAY OF CHAR);

This "procedure" allows you cause a warning to be generated in the source file at the source location with a string value passed to this function. No code is generated by the use of this procedure. This warning can never be suppressed with compiler options. You can use this feature to document something that needs addressing in your code and by having a warning generated you can use the mechanisms built into the development system for finding warnings errors in source files.

MOVE Procedure (extension)

PROCEDURE MOVE(VAR OUT destination  : ARRAY OF LOC; source : ARRAY OF LOC; amount : CARDINAL);

This procedure will generate an inline memory move. The two memory regions cannot overlap. The parameter amount is the number of bytes of memory to move.

FILL Procedure (extension)

PROCEDURE FILL(VAR OUT destination : ARRAY OF LOC; fillValue : FillType; amount : CARDINAL);

This procedure will generate an inline memory fill. You are only allowed to fill whole number types and small sets (up to the size of a processor register) with this procedure. The amount parameter is the number of iterations that the fillValue is written to memory.

SWAPENDIAN Procedure (extension)

PROCEDURE SWAPENDIAN(IntegerType) : SameIntegerType;

PROCEDURE SWAPENDIAN(VAR INOUT : IntegerOrRealType);

This is a function procedure that allows converting a number between little and big endian byte ordering format. This only has use for code that stores data on disk, or transmits data across a network, that is used on computers that have different natural data formats. This procedure can be called as a function or a procedure. When call as a function only integer types can be used. When called as a procedure you can pass both integer types and floating point types.

16-bit, 32-bit and 64-bit INTEGER and CARDINAL types can be used with this function. REAL and LONGREAL can also be used in the procedure syntax form.

Examples:

VAR
   card32 : CARDINAL32;
   card16 : CARDINAL16;

    r      : REAL;
BEGIN
   ...
   card32 := SWAPENDIAN(card32);
   card16 := SWAPENDIAN(card16);

    SWAPENDIAN(card32);

    SWAPENDIAN(r);

IA-32 processor note: This system function will generate the BSWAP instruction, if the target processor option is greater than or equal to 486, which is the default. The BSWAP instruction is only available on 486 and later processors. Normally you want the target processor to be greater than or equal to 486 and thus you may want to have two procedures if you absolutely must support 386 machines. One procedure that is compiled with the target processor at 386, and the other at the normal 486 processor and then at runtime choose which procedure to call using the CPU variable.

BIGENDIAN, LITTLEENDIAN Procedure (extension)

PROCEDURE BIGENDIAN(IntegerType) : SameIntegerType;

PROCEDURE BIGENDIAN(VAR INOUT : IntegerOrRealType);

PROCEDURE LITTLEENDIAN(IntegerType) : SameIntegerType;

PROCEDURE LITTLEENDIAN(VAR INOUT : IntegerOrRealType);

These functions are like SWAPEENDIAN except they always result in a specific endian format. They assume the input data is in the native format for the target processor and operating system. Therefore these procedures may, or may not, generate code. For example LITTLEENDIAN will never generate code when targeting an IA-32 processor. It will however generate code if the target is a SPARC processor.

LITTLEENDIAN is equivalent to

%IF BigEndian %THEN

   SWAPENDIAN(data);

%END

Multi-processing support functions (extension)

The compiler implements various intrinsic functions useful in multiprocessing. These functions generate inline machine code rather than a procedure call. These functions perform atomic operations in a multiprocessing environment. This means that the processor executing these functions has exclusive access to the data being operated on.

These functions are exported from the System module/package. The sample source code and runtime library have examples of using these functions.

Atomic compare and exchange

PROCEDURE ATOMIC_CMPXCHG(VAR INOUT data : SomeType; compare, source : SomeType) : SomeType;

where SomeType can be CARDINAL, INTEGER, DWORD or ADDRESS. Remember that pointers are compatible with ADDRESS parameter types.

This procedure compares the value in data with the value in compare and if equal, then the value in source is assigned to data. The function result is the value previously held in data. Only the variable data is atomically accessed. This function makes the new value written in data visible to other processors before returning.

Note: This intrinsic is not available for 386 processors.

Note: The return value may not be the same as the value in data, because another processor may have altered the value one processor cycle after this function alters the value.

Atomic exchange

PROCEDURE ATOMIC_XCHG(VAR INOUT data : SomeType; source : SomeType) : SomeType;

where SomeType can be CARDINAL, INTEGER, DWORD or ADDRESS. Remember that pointers are compatible with ADDRESS parameter types.

This procedure performs an atomic exchange of the value in data with the value in source. The function result is the value previously held in data. Only the variable data is atomically accessed. This function makes the new value written in data visible to other processors before returning. This operation may be used as a function or as a statement ignoring the return value.

Note: The return value may not be the same as the value in data, because another processor may have altered the value one processor cycle after this function alters the value.

Atomic add

PROCEDURE ATOMIC_ADD(VAR INOUT data : SomeType; constantValue : INTEGER) : SomeType;

where SomeType can be CARDINAL or INTEGER.

constantValue must be in the range

-128..127 for IA32

-4096..4095 for SPARC

This procedure adds the value in constantValue with the value in data. Positive numbers perform addition and negative numbers perform subtraction. The function result is the result of the addition/subtraction to data. Only the variable data is atomically accessed. This function makes the new value written in data visible to other processors before returning. This operation may be used as a function or as a statement ignoring the return value.

Note: This intrinsic is not available for 386 processors when used as a function.

Note: The return value may not be the same as the value in data, because another processor may have altered the value one processor cycle after this function alters the value.

Memory fence/barrier

PROCEDURE MEMORY_FENCE;

Note: This procedure is not necessary and does not perform any actions on IA-32 architecture processors (x86 processor family) since these processors support strong write ordering.

Note: You do not need to use a memory fence with any synchronization objects supported by the runtime library or the operating system, as they will take necessary actions.

This procedure generates a memory fence or barrier, and is necessary on processors that do not guarantee memory access order.

SPARC

PowerPC

IA-64

The procedure guarantees that all subsequent loads or stores will not access memory until after all previous loads and stores have accessed memory, as observed by other processors. The following pseudo code describes this further

1) <Acquire lock>

2) MEMORY_FENCE

3) critical section code

4) MEMORY_FENCE

5) <release lock>

The first memory fence stops the processor from looking ahead and pre-fetching any data used in the critical section. The second memory fence makes sure that any data written in the critical section is made visible to other processors before the write that releases the software lock. The memory fence is generally only used when implementing spinlocks.

Port Input/Output Procedures (extension, IA-32 only)

PROCEDURE INB(VAR OUT data : BYTE, port : CARDINAL);
PROCEDURE INW(VAR OUT data : WORD, port : CARDINAL);
PROCEDURE IND(VAR OUT data : DWORD, port : CARDINAL);(* 32-bit only *)

PROCEDURE OUTB(data : BYTE, port : CARDINAL);
PROCEDURE OUTW(data : WORD, port : CARDINAL);
PROCEDURE OUTD(data : DWORD, port : CARDINAL);(* 32-bit only *)

The above procedures are available on DOS, 32-bit DOS extended and 16-bit Windows systems, and they provide the ability to read and write to the I/O ports of the computer.

Enabling and Disabling of Interrupts (extension, IA-32 only)

PROCEDURE ENABLE;
PROCEDURE DISABLE;

The above procedures are available on DOS, 32-bit DOS extended and 16-bit Windows systems, and they provide the ability to enable and disable interrupts on the computer.

Processor Flags and Registers Types (extension, DOS operating systems only)

TYPE
    FLAGS = (
         CarryFlag,
         dummy1,
         ParityFlag,
         dummy2,
         AuxiliaryCarryFlag,
         dummy3,
         ZeroFlag,
         SignFlag,
         TrapFlag,
         InterruptFlag,
         DirectionFlag,
         OverflowFlag
            );

    FLAGSET = PACKEDSET OF FLAGS;

 

    (* 32-bit mode *)

 

    REGISTERS    =
    RECORD
        CASE : CARDINAL OF
        0:
            AL, AH, p1, p2    : CARDINAL8;
            CL, CH, p3, p4    : CARDINAL8;
            DL, DH, p5, p6    : CARDINAL8;
            BL, BH, p7, p8    : CARDINAL8;
        |
        1:
            AX, p9        : CARDINAL16;
            CX, p10       : CARDINAL16;
            DX, p11       : CARDINAL16;
            BX, p12       : CARDINAL16;
        |
        2:
            EAX           : INTEGER32;
            ECX           : INTEGER32;
            EDX           : INTEGER32;
            EBX           : INTEGER32;
        ELSE
        END;
        EBP, ESI, EDI    : INTEGER32;
        ES, DS        : CARDINAL16;
        CASE : BOOLEAN OF
        FALSE: FLAGS        : BITSET32;
        |
        TRUE: FLAGSET        : SET OF FLAGS;
        END;
    END;

 

    (* 16-bit mode *)

 

    REGISTERS    =
    RECORD
        CASE : CARDINAL OF
        0:
            AL, AH    : CARDINAL8;
            CL, CH    : CARDINAL8;
            DL, DH    : CARDINAL8;
            BL, BH    : CARDINAL8;
        |
        1:
            AX        : CARDINAL16;
            CX        : CARDINAL16;
            DX        : CARDINAL16;
            BX        : CARDINAL16;
        ELSE
        END;
        BP, SI, DI        : CARDINAL16;
        ES, DS        : CARDINAL16;
        CASE : BOOLEAN OF
        FALSE: FLAGS        : BITSET16;
        |
        TRUE: FLAGSET        : SET OF FLAGS;
        END;
    END;

INTR Procedure (extension, DOS operating systems only)

PROCEDURE INTR(IntNum : CARDINAL; VAR INOUT R : REGISTERS);

This procedure performs the specified software interrupt with the values from the REGISTERS data structure. The values returned from the interrupt are returned in the REGISTERS data structure.

The FLAGS, FLAGSET, REGISTERS and INTR symbols are only available for the DOS, 32-bit DOS extended and 16-bit Windows systems.
=================================================================
First understand that the Microsoft documentation is written for C 
programmers. Then understand that the C language is not a strongly typed 
language. NULL in the Windows declarations is the value 0. in WINX there are 
various constants that type NULL to various Windows pointer types, but in 
this case nothing for PVOID. You can use NIL in Modula-2. The value for NIL 
in Modula-2 is a numerical value of zero. So you can use NIL in this 
instance or you can add a NULL_PVOID to WINX with all the other typed 
declarations of NULL_ and use that. Your choice.

Norman 
=================================================================
Subject: ISO Modula-2
From: Norman Black <nospam@nospam.com>
Newsgroups: comp.lang.modula2
Date: 2/9/06

In Wirth M2 DIV and MOD were integer division with remainder even though 
"MOD" seems like modulus. In ISO M2 DIV and MOD are integer division with 
modulus. The modulus is *not* an integer remainder operation in the assumed 
sense. In ISO M2 / and REM are integer division with remainder. In my own 
code I have long since changed it all to / and REM even though nearly all 
integer types are CARDINAL just been in the habit of never using DIV and MOD 
since MOD is never what I want and it affects DIV.

The result of modulus is always positive and therefore affects the result 
that DIV will return. Note that for unsigned numbers DIV = / and MOD = REM. 
Therefore for CARDINAL there is no difference between / and DIV and REM and 
MOD. For INTEGER this is not the case and the compiler must generate 
different code for DIV and MOD. Note that processors implement integer 
division with remainder.

Norman 
=================================================================
> Even understanding the difference between MOD and REM,
> I had expected that
>
> WriteInt(-42 DIV 10, 5);
>     would give the same result as
> WriteInt((-42) DIV 10, 5);

Bad expectation given operator precedence rules. DIV has higher precedence 
than a unary sign operator. In the first case you have -(42 DIV 10) and the 
second you have (-42) DIV 10. So in one case you have a negative dividend 
and not the other. This only matters since DIV is not an integer division 
operator in the sense we most assume. (except for type CARDINAL).

Since DIV is not what most expect it is, it is best to never use it even for 
type CARDINAL. That way you cannot get into trouble with any false 
assumptions or operator precedence issues.

Norman
==============================================================================
Subject: Re: ISO Modula-2 "gotcha"
From: Martin Brown <|||newspam|||@nezumi.demon.co.uk>
Newsgroups: comp.lang.modula2
Date: 2/9/06

Tom Breeden wrote:

> Even understanding the difference between MOD and REM,
> I had expected that
> 
> WriteInt(-42 DIV 10, 5); 
>      would give the same result as
> WriteString("(-42) DIV 10"); 
> 
> Nope, not for ISO DIV.

Boggle! One of them prints out the string...
"(-42) DIV 10"

The old refrain "we know what you meant but we heard what you said" applies.

I am inclined to agree that it is a really dumb idea to alter the 
behaviour of existing operators in odd situations.

Where the intuitive interpretation of operator binding is potentially 
ambiguous it would be more in the spirit of the language to insist that 
the user place brackets to make his intention clear.

Given that: ISO  -X DIV Y  = -(X DIV Y) <> (-X) DIV Y  for some X,Y

We have a potential intermittent fault. It is a nasty behaviour to let 
this pass without requiring clarification in a language that strives to 
be unambiguous.

Regards,
Martin Brown

Yea, and all that because of the want for a modulus operator. Who cares 
about the modulus operator?

Norman 
===========================================================================
Subject: Re: Bit manipulation in ISO Modula-2
From: Martin Whitaker <news@removethis.martin-whitaker.me.uk>
Newsgroups: comp.lang.modula2

Robert wrote:
> Hello, can someone please tell me if bit manipulation operators such 
> as AND, NOT, OR, SHL, SHR, are part of the ISO Modula-2 standard?
> 
Yes. Use a packedset type (such as the predefined type BITSET) along
with the standard set operators and the SHIFT and ROTATE functions
imported from the SYSTEM module, e.g.

VAR a, b, c : BITSET;

a = b * c;               (* bitwise AND *)
a = b + c;               (* bitwise OR *)
a = b / c;               (* bitwise XOR *)
a = {} - b;              (* bitwise NOT *)
a = SYSTEM.SHIFT(b,1);   (* shift left by 1 *)
a = SYSTEM.SHIFT(b,-2);  (* shift right by 2 *)
a = SYSTEM.ROTATE(b,1);  (* rotate left by 1 *)
a = SYSTEM.ROTATE(b,-2); (* rotate right by 2 *)

You can declare your own bitset types, e.g.

TYPE Bitset8 = PACKEDSET OF (0..7);

although your compiler will probably place some restriction on
the maximum number of bits.

Martin
=============================================================
2/22/2009 4:02 PM
Data types corresponding FROM Modula-2 TO C
	
INTEGER          int
LONGINT          long long int
SHORTINT         short int
CARDINAL         unsigned int
LONGCARD         long long unsigned int
SHORTCARD        short unsigned int
BOOLEAN          int
REAL             double
LONGREAL         long double
SHORTREAL        FLOAT
CHAR             CHAR




