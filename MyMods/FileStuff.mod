    %IF WIN32 %OR OS2 %THEN
        FileSpecString  = ARRAY [0..260] OF CHAR;
        NameString      = ARRAY [0..255] OF CHAR;
    %ELSIF LINUX %OR SUNOS %THEN
        FileSpecString  = ARRAY [0..1023] OF CHAR;
        NameString      = ARRAY [0..255] OF CHAR;
    %ELSIF UNIX %THEN
        FileSpecString  = ARRAY [0..255] OF CHAR;(*may be larger*)
        NameString      = ARRAY [0..14] OF CHAR;(*may be larger*)
    %ELSE
        (*
         DOS has a smaller limit than the size of the following string
         network drivers can allow longer paths for servers
         *)
        FileSpecString  = ARRAY [0..255] OF CHAR;
        NameString      = ARRAY [0..12] OF CHAR;
    %END

    SearchEntry = RECORD
            (* returned information for you *)
            size        : CARDINAL32;
            attribute   : FileAttributeSet;
            dt          : DateTime;(*this is local time *)
            %IF WIN32 %OR UNIX %THEN
            dtUTC       : DateTime;(*this is UTC time *)
            %END
            name        : NameString;
            (* fields used for internal implementation *)
            mayHave     : FileAttributeSet;
            mustHave    : FileAttributeSet;
        %IF PROTECT %THEN
            critic      : CriticalSection;
        %END
        %IF WIN32 %THEN
            findHandle  : ADDRESS;
            reserved0   : CARDINAL;
            reserved1   : CARDINAL;
        %ELSIF UNIX %THEN
            findHandle  : ADDRESS;
            pattern     : FileSpecString;
        %ELSIF FlashTek %OR DOS %OR WIN16 %THEN
            reserved    : ARRAY [0..20] OF CHAR;
        %ELSIF OS2 %THEN
            findHandle  : CARDINAL;
        %END
  END; (* SearchEntry record *)

(*
  the following record is used to contain the various components of a file specification
  The sizes of individual components are defined by the underlying file system for the device accessed.  The sizes declared here are at least as big as necessary, but most likely larger *)
    FileNameParts =
        RECORD
          %IF DOS %OR FlashTek %OR PharLap %OR WIN16 %THEN
            drive       : ARRAY [0..127] OF CHAR;
          %ELSIF UNIX %THEN
            drive       : ARRAY [0..0] OF CHAR;(*N/A*)
          %ELSE
            drive       : NameString;
          %END
          path        : FileSpecString;
          name        : NameString;
          extension   : NameString;
        END;
(*
   .drive only has meaning on Miscorosft platforms
       this is the device, it is either a logical drive letter or
       a UNC server name and share.
   .path = this is the path
   .name = the file name  excluding the file extension
   .extension = the file extension, this is everything  after the *last*
       '.' character. On Unix systems this may actually be the full file
       name since the convension there is to use a preceeding '.' to
       mark hidden files and/or directories.
*)

    FileTypes           = (FileTypeUnknown,FileTypeDisk,FileTypeChar,FileTypePipe);

  (*
     These constants are used with the mayHave and mustHave search
     parameters of FindFirst

     Use one of these with mustHave to search for
       files only,
       directories only,
       files and directories
    *)
    MustHaveNormalFile  = FileAttributeSet{NormalFile};
    MustHaveDirectory   = FileAttributeSet{Directory};
    MustHaveNothing     = FileAttributeSet{};

    (* use the following in mayHave *)
    AllAttributes       = FileAttributeSet{MIN(FileAttributes)..MAX(FileAttributes)};
    StdAttributes       = AllAttributes - FileAttributeSet{Hidden, System};

(* these will allow the use of the set + operator to compose a search filter expression example: StdAttributes + AddHidden *)
    AddArchive          = FileAttributeSet{Archive};
    AddReadOnly         = FileAttributeSet{ReadOnly};
    AddHidden           = FileAttributeSet{Hidden};
    AddSystem           = FileAttributeSet{System};
    AddCompressed       = FileAttributeSet{Compressed};
    AddTemporary        = FileAttributeSet{Temporary};
    AddEncrypted        = FileAttributeSet{Encrypted};
    AddOffline          = FileAttributeSet{Offline};
    AddAlias            = FileAttributeSet{Alias};
    AddNormalFile       = FileAttributeSet{NormalFile};
    AddDirectory        = FileAttributeSet{Directory};

PROCEDURE FindFirst(path : ARRAY OF CHAR;
                    mayHave : FileAttributeSet;
                    mustHave : FileAttributeSet;
                    VAR OUT entry : SearchEntry) : BOOLEAN;
(*
 path specifies a device and directory where to search
 wildcards can be and generally are used in path.
 How wildcards are specified is filesystem dependent.
 Generally the '*' character matches any number of characters and a
 '?' character matches any single character.
 if this procedure succeeds(returns TRUE), you must call FindClose when
 you are done searching.
 Example of use
   This searches for all files, excluding file directories.
    IF FindFirst("*", StdAttributes, MustHaveNormalFile, entry) THEN
        REPEAT
            ...
        UNTIL NOT FindNext(entry);
        FindClose(entry);
    END;

  mayHave - returned files will only have attributes within this set, however they need not have all of the attributes.
  mustHave - all returned files will have ALL the specified Attribute(s)

  FindFirst combines mustHave into mayHave as a convenience.

  NormalFile is assumed for mayHave if Directory is not present.
*)

PROCEDURE FindNext(VAR INOUT entry : SearchEntry) : BOOLEAN;
(* find the next file in the search parameters given in FindFirst *)

PROCEDURE FindClose(VAR INOUT entry : SearchEntry);
(* terminates a FindFirst, FindNext sequence of operations *)


I need to create either a linked list of SearchEntry, sizes, etc, or a table of SearchEntry.
Since there are no linkedlist procedures already in the library, I'll use a table format.
Use readchar to read 1 char at a time to form the search pattern.  When <tab> is hit, then
append '*' to the pattern and use this for the search loop to build the table.  Then output
the table, possibly using a BasicDialogs procedure.  If I can, I'll leave the table up while
continued <tab> hits will cycle thru the table.
CONST tablesize=100;
      pointertablesize=1000;
TYPE
  SearchEntryTableType = ARRAY [1..tablesize] OF SearchEntry;
  SearchEntryPtr = POINTER TO SearchEntry;
  SearchEntryTablePointerType = ARRAY [1..tablesize] OF SearchEntryPtr;

VAR
  SearchEntryTable,SearchEntryTableSorted : SearchEntryTableType;
  entry                                   : SearchEntry;
  entryptr                                : SearchEntryPtr;
  CountOfEntries, counter                 : CARDINAL;

  inputline has the entered string until <tab> was hit
  append '*' to inputline
  IF FindFirst(inputline, StdAttributes, MustHaveNormalFile, entry) THEN
      CountOfEntries :=   1;
      REPEAT
        SearchEntryTable[CountOfEntries] := entry;
        INC(CountOfEntries);
      UNTIL NOT FindNext(entry) OR (counter > tablesize);
      FindClose(entry);
      SearchEntryTableSorted := SearchEntryTable;
  END;
  Since this is a postincremented pointer it will have the # of entries, not the last used index value.

always sort the table by rev date,  I think I want subsequent <tab>
to cycle thru the files in the active table.  Ill make the sorted table active.
Now to Display the sorted table.
Terminal.Reset;
counter := 0;
REPEAT
  Terminal.Write  .name, .dt, .size
  Terminal.WriteLn
  pause every 20 or so.
UNTIL counter >= CountOfEntries;



Thu 25 Aug 2011 02:26:15 PM EDT
Maybe do unsorted array of 20 or so.  When this works, do a table of pointers and sort that.

