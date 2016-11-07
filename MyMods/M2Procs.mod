DEFINITION MODULE FileFunc;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

<*/VALIDVER:PROTECT*>
(*<*/VERSION:PROTECT*>*)

(*
 FileFunc does not protect against multiple threads accessing the same
 File data structure. The PROTECT version tag enables this, however
 this still will not provide proper functionalitly for most applications
 since one thread may need to do multiple writes before it is done,
 another thread can then start reading.
 what an application really needs is a mutex semaphore to protect
 complete actions within their algorithm, not just single procedure calls
 hence the reason the protect version tag is defaulted off
*)

<*/NOPACK*>

FROM SYSTEM IMPORT
    BYTE, ADDRESS;

FROM SysClock IMPORT
    DateTime;

%IF PROTECT %THEN
FROM Threads IMPORT
    CriticalSection;
%END

CONST
    EOL                 = 36C;

    (* This is the character used to separate directory names and file names
       in a file specification.
    *)
    %IF UNIX %THEN
    PathSepChar         = '/';
    %ELSE
    PathSepChar         = '\';
    %END

TYPE
    (* these strings have room for the maximum data plus a null character *)

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

    FileAttributes =
        (
            ReadOnly,
            Hidden,
            System,
            Archive,

            Compressed,
            Encrypted,
            Temporary,
            Offline,     (* Indicates that the file data has been
                            physically moved to offline storage. *)

            Alias,       (* Unix link  *)
            OwnerRead,   (* "Unix" permissions *)
            OwnerWrite,
            OwnerExec,
            GroupRead,
            GroupWrite,
            GroupExec,
            OtherRead,
            OtherWrite,
            OtherExec,


            NormalFile,  (*regular files *)
            Directory    (*self explanatory. a Directory *)
        );
    FileAttributeSet    = PACKEDSET OF FileAttributes;
    (*
     none of the above attributes are guaranteed to exist on a given
     underlying file system except NormalFile and Directory.

     On Unix systems the Hidden attribute is set on files with a
     leading '.' character in the file name. This is the convension for
     "Hidden" files.
     You cannot set the Hidden attribute on Unix systems as this is
     a pseudo attribute.

     On Unix systems the ReadOnly attribute is set according to the
     file permissions and taking into account the file owner and
     the current user and group ids.
     You cannot set the ReadOnly attribute on Unix systems as this is
     a pseudo attribute. Use the permissions attributes.
    *)

    SearchEntry =
        RECORD
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
        END;

(*
     the following record is used to contain the various components
     of a file specification
     The sizes of individual components are defined by the underlying
     file system for the device accessed. The sizes declared here are
     at least as big as necessary, but most likely larger
     *)
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

    FileTypes           = (
                           FileTypeUnknown,
                           FileTypeDisk,
                           FileTypeChar,
                           FileTypePipe
                          );
    (*
     FileTypeChar is a data stream that cannot be repositioned
     and is text oriented. Usually this is accessing the console.
     This means input is comming from the keyboard, and output is going
     to the screen.
     FileTypePipe is a data stream that cannot be repositioned
     and the data format is undefined.
     FileTypeDisk should be obvious, some sort of floppy,harddisk,etc
     *)

    DeviceTypes         = (
                           DeviceUnknown,
                           DeviceFixedDisk,
                           DeviceRemovable,
                           DeviceRamdisk,
                           DeviceRemote
                          );

    AccessModes         = (
                           ReadOnlyDenyNone,
                           ReadOnlyDenyWrite,
                           ReadOnlyDenyAll,
                           ReadWriteDenyNone,
                           ReadWriteDenyWrite,
                           ReadWriteDenyAll,
                           WriteOnlyDenyNone,
                           WriteOnlyDenyAll
                          );
    (*
     These modes specify both an access mode and a file sharing mode
     ReadOnly, ReadWrite and WriteOnly are the access mode for your
     access to the file.
     DenyNone, DenyWrite, DenyAll are the sharing mode for the file
     DenyNone will allow everything to have full access to the file
     DenyWrite will allow others to have read access to the file
     DenyAll will allow nothing to access the file
     The most common modes used are ReadOnlyDenyWrite, ReadWriteDenyAll,
     and ReadWriteDenyNone.

     On Unix systems file open "sharing" does not exist. This module simulates
     this capability via record locking. There is no way to truely enforce
     file open sharing on Unix systems.
     *)

    FileUseInfo         = (
                           SequentialAccess,
                           RandomAccess,
                           WriteThrough,
                           TemporaryFile
                          );
    FileUseInfoSet      = PACKEDSET OF FileUseInfo;
    (*
     these flags can be used to give additional instructions to
     the operating system about how you will be using a file.
     the operating system can thus possibly optimize the file access
     given this additional information.
     these are only suggestions and an operating system may not support
     all or any of the suggestions.
     SequentialAccess = the file is accessed linearly, or at least mostly
                        in this manner. Under Win32 this flag has an
                        interesting effect on file caching. It will
                        cause the file to not be cached in the disk
                        cache except for any read ahead caching
                        the system does. Once you read past a certain
                        point the cached data is removed from the cache
                        which means that the second time you read the
                        file it will not be in the cache.
                        Thus for small/average files that are read
                        often it is best to not use this attribute.
     RandomAccess = the file is accessed in a random manner
     WriteThrough = Data written to the file should be immediately written
                    to the disk. The data can still be cached.
                    This flag only has meaning on operating systems that
                    implement write-back disk caches where the data
                    written will be written to disk at some later point
                    in time, thus allowing the system to optimize
                    access to the disk.
     TemporaryFile = the file is only temporary and will be deleted.
     *)

    CommonFileErrors    = (
                           FileErrSuccess,
                           FileErrFileNotFound,
                           FileErrPathNotFound,
                           FileErrNoHandles,
                           FileErrAccessDenied,
                           FileErrInvalidHandle,
                           FileErrNotReady,
                           FileErrWriteProtect,
                           FileErrSharingViolation,
                           FileErrLockViolation,
                           FileErrDiskFull,
                           FileErrBrokenPipe,
                           FileErrInterrupted,
                           FileErrUnknown
                          );
    (*
     these are common errors that can occur while accessing a file
     The File record contains an error code but this code is operating
     system dependent
     use TranslateFileError to convert this code to one of these
     errors
     an error code of zero however always means FileErrSuccess
     *)

    File =
        RECORD
            status      : CARDINAL;
            count       : CARDINAL;
            bp          : CARDINAL;
            be          : CARDINAL;
            size        : CARDINAL;
            start       : CARDINAL32;
            end         : CARDINAL32;
            buffer      : POINTER TO ARRAY [0..0] OF BYTE;(*any size allowed *)
            handle      : CARDINAL;
            userData    : ADDRESS;
            %IF PROTECT %THEN
            critic      : CriticalSection;
            %END
            peekedChar  : CHAR;
            mode        : AccessModes;
            valid       : BOOLEAN;
            dirty       : BOOLEAN;
            eof         : BOOLEAN;
            buffered    : BOOLEAN;
            canPosition : BOOLEAN;
            peeked      : BOOLEAN;
        END;
    (*
     the only user fields in the above File record are

        status          = the error state of the last operation
                          0 = Success
                              other errors vary by operating system
        count           = used by the read and write procedures
        handle          = the operating system file handle
        eof             = EndOfFile: will be set after a read operation
        userData        = you can use this field for anything you like

     you can access other fields but do so at your own risk. those fields may
     change in future implementations.
*)

CONST
    InvalidHandle       = MAX(CARDINAL);(*-1 for integers*)

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

    AllAttributes       = FileAttributeSet{MIN(FileAttributes)..
                                               MAX(FileAttributes)};
    StdAttributes       = AllAttributes -
                            FileAttributeSet{Hidden, System};

    (* these will allow the use of the set + operator to compose a search
     filter expression
     example: StdAttributes + AddHidden *)

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

    UnixPermissions     = FileAttributeSet{OwnerRead..OtherExec};
    UnixExecPerm        = FileAttributeSet{OwnerExec,GroupExec,OtherExec};
    UnixReadPerm        = FileAttributeSet{OwnerRead,GroupRead,OtherRead};
    UnixWritePerm       = FileAttributeSet{OwnerWrite,GroupWrite,OtherWrite};

PROCEDURE OpenFile(VAR OUT f : File;
                   spec : ARRAY OF CHAR;
                   mode : AccessModes);
(*
 open the file specified by spec, with the access mode specified by mode
 the file must currently exist
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
 the file position is at the beginning of the file
*)

PROCEDURE OpenFileEx(VAR OUT f : File;
                     spec : ARRAY OF CHAR;
                     mode : AccessModes;
                     useInfo : FileUseInfoSet);
(* as OpenFile, with additional information in useInfo *)

PROCEDURE CreateFile(VAR OUT f : File; spec : ARRAY OF CHAR);
(*
 create a file with the given file specification
 the mode of the file is ReadWriteDenyAll
 if the file already exists, then it is truncated to 0 bytes in size
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
 the file position is at the beginning of the file
*)

PROCEDURE CreateFileEx(VAR OUT f : File;
                       spec : ARRAY OF CHAR;
                       useInfo : FileUseInfoSet);
(* as CreateFile, with additional information in useInfo *)

PROCEDURE GetTempFileDirectory(VAR OUT spec : ARRAY OF CHAR);
(* return the system temportary file directory. *)

PROCEDURE MakeTempFileName(VAR INOUT spec : ARRAY OF CHAR);
(*
 creates a unique filename in the path given in spec.
 if spec = "", then the file is placed in the system temp directory.
 returns the resulting full file specification in spec.
*)

PROCEDURE CreateTempFile(VAR OUT f : File; VAR INOUT spec : ARRAY OF CHAR);
(*
 this is implemented as
     MakeTempFileName(spec);
     CreateFileEx(f, spec, FileUseInfoSet{TemporaryFile});
*)

PROCEDURE CreateTempFileEx(VAR OUT f : File;
                           VAR INOUT spec : ARRAY OF CHAR;
                           useInfo : FileUseInfoSet);
(*
 this is implemented as
     MakeTempFileName(spec);
     CreateFileEx(f, spec, useInfo+FileUseInfoSet{TemporaryFile});
*)

PROCEDURE OpenCreateFile(VAR OUT f : File;
                         spec : ARRAY OF CHAR;
                         mode : AccessModes);
(*
 open the file specified by spec, with the access mode specified by mode
 if the file does not exist it is created
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE OpenCreateFileEx(VAR OUT f : File;
                           spec : ARRAY OF CHAR;
                           mode : AccessModes;
                           useInfo : FileUseInfoSet);
(* as OpenCreateFile, with additional information in useInfo *)

PROCEDURE FakeFileOpen(VAR OUT f : File;
                       handle : CARDINAL;
                       mode : AccessModes);
(*
 initializes the File record with proper inital values
 handle = the operating system handle used to access the "file"
 mode = the access modes
 this call always succeeds
 *)

PROCEDURE CloseFile(VAR INOUT f : File);
(*
 closes the specified file
 if the file was buffered and the buffer is "dirty", then contents
 of the buffer are written to disk before closing the file
*)

PROCEDURE FileType(VAR INOUT f : File) : FileTypes;
(*
 returns the type of the file
 for DiskFile you can use the file position and size functions
*)

PROCEDURE SetFileBuffer(VAR INOUT f : File; VAR OUT buf : ARRAY OF BYTE);
(*
 attach a buffer to a file
 excellent for sequentially accessed files.
 size of buffer is taken from the size of the ARRAY OF BYTE parameter
 One a buffer is attached to a file all read/write calls go through
 the file buffer. The disk is only accessed when the buffer is full for
 writes or empty for reads.
 For example, your code could read one byte at a time from the file but
 the disk is accessed in chunks of the size of the buffer.
 1-64k are values to try. if you are writing files then larger is better.
 this is because a fast HD writes reasonably large blocks in the same time
 as smaller ones.
 the best rule for a buffer size is to time your application with
 some different buffer sizes to see what does best.
 NOTE: Do not forget to consider any disk caches the file system may
       have in testing file access performance.
*)

PROCEDURE RemoveFileBuffer(VAR INOUT f : File);
(* flushes buffer to disk if necessary, then removes file buffer *)

PROCEDURE FlushBuffers(VAR INOUT f : File; flushOs : BOOLEAN);
(*
 flushes any buffered data to disk
 if flushOS = TRUE then the file is flushed out of any
 OS buffers and or caches to disk, if the file system supports this
 capability. *)

PROCEDURE ReadBlock(VAR INOUT f : File;
                    VAR OUT buf : ARRAY OF BYTE;
                    size : CARDINAL);
(*
 the amount to be read is passed in the size parameter
 the data is read into buf
 the file record count field contains the actual amount read
 the file position is advanced by the actual amount read
 f.status = 0 signifies success, otherwise it contains the
 operating system error code  *)

PROCEDURE WriteBlock(VAR INOUT f : File;
                     buf : ARRAY OF BYTE;
                     size : CARDINAL);
(*
 the amount to be written is passed in the size parameter
 the data is written from buf
 the file record count field contains the actual amount written
 the file position is advanced by the actual amount written
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE ReadChar(VAR INOUT f : File) : CHAR;
(*
 reads a single character

 it returns the value EOL for the following character sequences
 Carriage Return. CHR(13)
 Line Feed. CHR(10)
 Carriage Return, Line Feed. CHR(13)CHR(10)

 the file position is advanced by the actual amount read

 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE WriteChar(VAR INOUT f : File; ch : CHAR);
(*
 writes a single char

 the value of the EOL is system dependent.
 for DOS and Windows => Carriage return, line feed pairs.
 for Unix => line feed.
 the file position is advanced by one character, except if
 EOL represents a multi character sequence. in this case the position
 is advanced by the number of characters in the sequence.

 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE PeekChar(VAR INOUT f : File) : CHAR;
(*
 same as ReadChar
 except that the file position is not advanced
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE ReadLine(VAR INOUT f : File; VAR OUT str : ARRAY OF CHAR) : CARDINAL;
(*
 a convenience call.
 this procedure reads a line of text from a file.
 the EOL marker is consumed by this read.
 if the line is longer than the passed string parameter
 then the procedure returns leaving the remainder of the line
 to be read on the next read.
 the return value is the number of characters in the line.
*)

PROCEDURE WriteLine(VAR INOUT f : File; str : ARRAY OF CHAR);
(*
 a convenience call
 implemented as
    IF str[0] <> '' THEN
        WriteBlock(f, str, LENGTH(str)*SIZE(CHAR));
    END;
    WriteChar(f, EOL);
*)

PROCEDURE LockFileRegion(VAR INOUT f : File; start, length : CARDINAL32);
(*
 gives the process exclusive access to the specified region
 of the file in question. You should unlock after the critical file
 operations have been completed
 multiple locking regions cannot overlap
 start = the starting offset in bytes of the region to be locked
 length = the length in bytes of the region to be locked
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE UnlockFileRegion(VAR INOUT f : File; start, length : CARDINAL32);
(*
 unlocks a previously locked region of the file
 start = the starting offset in bytes of the locked region
 length = the length in bytes of the region to be unlocked
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE SetFilePos(VAR INOUT f : File; pos : CARDINAL32);
(*
 pos is an absolute file position in bytes
 position zero is the beginning of the file
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE GetFilePos(VAR INOUT f : File) : CARDINAL32;
(*
 returns an absolute position
 position zero is the beginning of the file
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE MoveFilePos(VAR INOUT f : File; pos : INTEGER32);
(*
 moves the file position relative to the current position
 position zero is the beginning of the file
 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE TruncateFile(VAR INOUT f : File);
(*
 truncates the file size to the current file position
 example
    SetFilePos(f, newEnd);
    TruncateFile(f);

 f.status = 0 signifies success, otherwise it contains the
 operating system error code
*)

PROCEDURE FileLength(VAR INOUT f : File) : CARDINAL32;
(* size of the file in bytes *)

PROCEDURE GetFileSizes(name : ARRAY OF CHAR; VAR actual, alloc : CARDINAL32);
(*
 actual, alloc are the size of the file in bytes
 actual is the amount of data stored in the file
 alloc includes system allocation overhead
 such as sector/cluster granularity
*)

PROCEDURE TranslateFileError(f : File) : CommonFileErrors;
(*
 translates the OS dependent .status field for common errors
 only a small common subset of errors is translated
 if the error does not translate to to one of the errors in
 CommonFileErrors then FileErrUnknown is returned
*)

PROCEDURE GetFileAttr(name : ARRAY OF CHAR;
                      VAR OUT attr : FileAttributeSet) : BOOLEAN;
(*
 fetch the attributes of the file specified by name
 returns true if the operation succeeded
*)

PROCEDURE SetFileAttr(name : ARRAY OF CHAR;
                      attr : FileAttributeSet) : BOOLEAN;
(*
 set the attributes of the file specified by name with attr
 not all attributes can be set by this call, as this is system
 dependent. for example
 only the "owner" of a file may be allowed to make this change.
 returns true if the operation succeeded
*)

PROCEDURE GetFileDateTime(spec : ARRAY OF CHAR; VAR OUT dt : DateTime);
%IF WIN32 %OR UNIX %THEN
PROCEDURE GetFileDateTimeUTC(spec : ARRAY OF CHAR; VAR OUT dt : DateTime);
%END
(*
 fetch the date time of the file specified in spec
 the time is local time.
 if the file does not exist, dt.year = 0

 GetFileDateTimeUTC returns UTC time.
*)

PROCEDURE SetFileDateTime(spec : ARRAY OF CHAR; dt : DateTime) : BOOLEAN;
%IF WIN32 %OR UNIX %THEN
PROCEDURE SetFileDateTimeUTC(spec : ARRAY OF CHAR; dt : DateTime) : BOOLEAN;
%END
(*
 set the date time of the file specified in spec.
 the dt parameter is local time.
 sets "modified" time field
 also sets the "accessed" time field if applicable
 the system may not allow you to set the file date time.
 this is a system dependent feature.
 for example
 only the "owner" of a file may be allowed to make this change.

 SetFileDateTimeUTC assumes the dt parameter is UTC time.
*)

PROCEDURE RenameFile(fromFile, toFile : ARRAY OF CHAR) : BOOLEAN;
(*
 change the name of a file
 you can move a file from one directory to another on the same device
 with this function.
 the meaning of a device is somewhat dependent on the
 underlying file system
*)

PROCEDURE DeleteFile(name : ARRAY OF CHAR) : BOOLEAN;
(*
 deletes the named file from the disk
 returns TRUE if successful
*)

PROCEDURE FileExists(name : ARRAY OF CHAR) : BOOLEAN;
(*
 does the named file exists
 returns TRUE if successful
*)

PROCEDURE CopyFile(source, dest : ARRAY OF CHAR) : BOOLEAN;
(*
 copy a file from source to dest
 if dest already exists it will be overwritten
*)

PROCEDURE SetHandleCount(num : CARDINAL);
(*
 increases number of file handles available to a process
 The number of file handles specifies how many files can be open
 at any one point in time
 some systems have a limit and some do not.
 this call is then ignored on systems that do not have hard limits.
 if the current limit is greater than the value being set, then this call
 does not reduce the limit.
*)

PROCEDURE GetNextDir(list : ARRAY OF CHAR;
                     sepChars : ARRAY OF CHAR;
                     VAR INOUT i : CARDINAL;
                     VAR OUT item : ARRAY OF CHAR) : BOOLEAN;
(*
 list is a series of strings separated by a a given set of character(s)
 this list is a search path of directories
 sepChars = the character(s) that separate the individual elements
 the procedure starts search list at the position pointed to in "i"
 "i" is incremented past the returned string upon return
 item = the returned string extracted from list
 a PathSepChar characeter is appended to the returned path if this character
 is not already there
 returns TRUE if a value is returned in item, otherwise FALSE
 example of use

    i := 0;
    WHILE GetNextDir(path, i, dir) DO
        ConstructFileName(name, dir, spec);
         do something
    END;
*)

PROCEDURE ParseFileName(pathname : ARRAY OF CHAR;
                        VAR OUT parts : FileNameParts);
(* splits file spec into the FileNameParts record
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

PROCEDURE ParseFileNameEx(path : ARRAY OF CHAR;
                          VAR OUT parts : FileNameParts;
                          list : ARRAY OF CHAR;
                          sepChars : ARRAY OF CHAR);
(*
 like ParseFileName, but in addition
 an extension is only considered a file extension if it is
 contained within the list of extensions passed in the parameter list
 where each item within the list is separated by one of the characters
 is sepChars
 if the extension is not in the list it is then appended to name field
 this function obviously only makes sense on file systems where
 a filename can contain more than one . character
*)

PROCEDURE AssembleParts(parts : FileNameParts; VAR OUT name : ARRAY OF CHAR);
(* puts the file parts back together into a single string *)

PROCEDURE ConstructFileName(pri, def : ARRAY OF CHAR;
                            VAR OUT res : ARRAY OF CHAR);
(*
 pri = the primary file spec (device/path/name/extension)
 def = the default file spec (device/path/name/extension)
 the above can have all or none of various path components
 anything pri does not have is supplied by def if it has the component
 this function uses ParseFileName to split pri and def into their
 respective components

  ConstructFileName(FileName, '.mod', FileName);
   supplies a default extension. The leading . is necessary
  ConstructFileName(FileName, 'd:\dev\rtl\', FileName);
   supplies a default device and/or path. The trailing PathSepChar is necessary
   to signify that "rtl" is a directory name and not a file name.
*)

PROCEDURE ConstructFileNameEx(pri, def : ARRAY OF CHAR;
                              VAR OUT res : ARRAY OF CHAR;
                              list : ARRAY OF CHAR;
                              sepChars : ARRAY OF CHAR);
(* like ConstructFileName, however uses ParseFileNameEx to split
 the primary and default parts *)

PROCEDURE FindInPathList(fileName, pathList : ARRAY OF CHAR;
                         sepChars : ARRAY OF CHAR;
                         VAR OUT result : ARRAY OF CHAR;
                         searchCurrent : BOOLEAN) : BOOLEAN;
(*
 searches a given path list for a file given in fileName
 sepChars = the character(s) that separate the path elements
 if the function returns TRUE then the fileSpec is returned in result
 if searchCurrent = TRUE then the current directory is search first
 if searchCurrent = FALSE then the current directory is not searched first
*)

PROCEDURE FindInOSPathList(fileName : ARRAY OF CHAR;
                           VAR OUT result : ARRAY OF CHAR) : BOOLEAN;
(*
 searches the Operating system search path, "PATH", for a file
 if TRUE then the fileSpec is returned in result
 the current directory is always searched first
*)

PROCEDURE SupportsUTC(spec : ARRAY OF CHAR) : BOOLEAN;
(*
 returns TRUE if the filesystem containing the file/directory spec
 FULLY supports UTC file date times
*)

PROCEDURE ExpandFileSpec(VAR INOUT spec : ARRAY OF CHAR);
(*
 if passed a relative file spec, this procedure returns
 a complete unambiguous file spec which includes the device and path
 otherwise the passed file spec is returned unchanged.
*)

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


  mayHave - returned files will only have attributes within this set,
            however they need not have all of the attributes.
  mustHave - all returned files will have ALL the specified Attribute(s)

  FindFirst combines mustHave into mayHave as a convenience.

  NormalFile is assumed for mayHave if Directory is not present.
*)

PROCEDURE FindNext(VAR INOUT entry : SearchEntry) : BOOLEAN;
(* find the next file in the search parameters given in FindFirst *)

PROCEDURE FindClose(VAR INOUT entry : SearchEntry);
(* terminates a FindFirst, FindNext sequence of operations *)

PROCEDURE MakeDir(name : ARRAY OF CHAR) : BOOLEAN;
(*
 create a directory in the given location.
 name can contain a device and path in the specification
 all directories in the path up to the directory to be created must
 already exist.
 returns TRUE if successfull
*)

PROCEDURE CreateDirTree(fileSpec : ARRAY OF CHAR) : BOOLEAN;
(*
 makes sure every directory within the given path exists
 if it does not, the necessary directories are created
 returns TRUE if successfull
 this procedure accepts file specifications and ignores the filename
 this means that if you pass only a directory path a PatrhSepChar character
 should be at the end of the string.
 examples
 CreateDirTree("\sbm2\mod\FileFile.MOD");
 CreateDirTree("\sbm2\mod\");
*)

PROCEDURE DeleteDir(name : ARRAY OF CHAR) : BOOLEAN;
(*
 remove a directory
 returns TRUE if successfull
*)

PROCEDURE DirExists(name : ARRAY OF CHAR) : BOOLEAN;
(*
 does the specified directory exist
 returns TRUE if successfull
*)

PROCEDURE RenameDir(fromDir, toDir : ARRAY OF CHAR) : BOOLEAN;
(*
 rename a directory
 you can move a file from one directory to another on the same device
 with this function
 the meaning of a device is somewhat dependent on the underlying system
 returns TRUE if successfull
*)

PROCEDURE GetDefaultPath(VAR OUT path : ARRAY OF CHAR) : BOOLEAN;
(*
 gets the current default device and path
 the operating system uses the default path to open files when
 they are given a relative path
 for example if \sbm2\def\ is the default path then
 OpenFile(f, "FileFunc.DEF", ReadOnlyDenyWrite)
  is the same as
 OpenFile(f, "\sbm2\def\FileFunc.DEF", ReadOnlyDenyWrite)
*)

PROCEDURE SetDefaultPath(path : ARRAY OF CHAR) : BOOLEAN;
(* sets the current default device and path *)

PROCEDURE GetDeviceFreeSpace(spec : ARRAY OF CHAR) : CARDINAL32;
(*
 spec = file spec of any device, file or directory on the device
 returns the amount of free space on the device.
 the device can be a network share.
 if the device has more free space than MAX(CARDINAL32) then
 MAX(CARDINAL32) is returned
*)

PROCEDURE GetDeviceFreeSpaceEx(spec : ARRAY OF CHAR;
                               VAR OUT allocGranularity : CARDINAL32) : LONGCARD;
(*
 spec = file spec of any device, file or directory on the device
 returns the amount of free space on the device.
 the device can be a network share.
 allocGranularity contains the file allocation granularity of the device
*)

PROCEDURE GetDeviceType(spec : ARRAY OF CHAR) : DeviceTypes;
(*
 spec = file spec of any file or directory on the "device"
 DOS can only determine remote and removable
 Unix can only determine remote
*)

END FileFunc.
(***************************************************************************)
(*                                                                         *)
(*                           Copyright (C) 1996-2002                       *)
(*                         by Stony Brook Software                         *)
(*                                                                         *)
(*                           All rights reserved.                          *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE TextFileFunc;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

FROM FileFunc IMPORT
    File;

TYPE
    SeparatorSet        = PACKEDSET OF CHAR;

VAR
    Separators  : SeparatorSet; (* Default = SeparatorSet{' ',
                                                          FileFunc.EOL,
                                                          ASCII.ht,
                                                          ASCII.vt,
                                                          ASCII.ff};
                                *)

    PadChar     : CHAR;                 (* Default = ' ' *)
    ErrorChar   : CHAR;                 (* Default = '?' *)
    ChopField   : BOOLEAN;              (* Default = FALSE *)

(*
 it is recommended that any file using these functions be buffered
 by using FileFunc.SetFileBuffer. The best size of the buffer can only
 be determined via testing. Buffersizes smaller than 512 do not make
 much sense and sizes greater than 4-8k have diminishing returns
*)

PROCEDURE WriteString(VAR INOUT f : File; str : ARRAY OF CHAR);
(*
 writes the string, str, to the file f
*)

PROCEDURE WriteStringLn(VAR INOUT f : File; str : ARRAY OF CHAR);
(*
 this procedure is implemented as follows
        WriteString(f, str);
        WriteLn(f);
*)

PROCEDURE WriteField(VAR INOUT f : File;
                     str : ARRAY OF CHAR;
                     fieldLen : INTEGER);
 (*
 writes the string, str, to the file f
 fieldLen specifies the size of the field the string is written to
 if fieldLen > 0, str is right justified in the field
 if fieldLen < 0, str is left justified in the field
 if fieldLen = 0 then the full string str is always written to the file
 if the LENGTH of str is less than fieldLen then this function will
 use the global variable PadChar to justify the string in the field
 if the LENGTH of str is > fieldLen, and fieldLen <> 0, then the value
 of the Global variable ChopField determines the action taken
 if ChopField = TRUE then the field will be filled with ErrorChar and
    f.status = MAX(CARDINAL)
 if ChopField = FALSE then the string str will be truncated to the size
 of the field
*)

PROCEDURE WriteNumber(VAR INOUT f : File;
                      num : LONGINT;
                      fieldLen : INTEGER);
 (*
 the number num is converted to a string and written to the file f
 using the procedure WriteField
 See WriteField for the meaning of fieldLen
 if the numeric conversion fails f.status = MAX(CARDINAL)
*)

PROCEDURE WriteNumberBase(VAR INOUT f : File;
                          num : CARDINAL32;
                          base : CARDINAL;
                          fieldLen : INTEGER);
(*
 the number num is converted to a string in the number base base
 and is written to the file f using the procedure WriteField
 base must be >= 2 and <= 16
 See WriteField for the meaning of fieldLen
 if the numeric conversion fails f.status = MAX(CARDINAL)
*)

PROCEDURE WriteReal(VAR INOUT f : File;
                    num : LONGREAL;
                    sigFigs : CARDINAL;
                    eng : BOOLEAN;
                    fieldLen : INTEGER);
(*
 the real number num is converted to a string in scientific notation
 and written to the file f using the procedure WriteField
 sigFigs = the number of significant digits in the mantissa
 if eng = TRUE the the exponent will always be a multiple of 3 and the
 mantissa may thus be >= 10.0
 See WriteField for the meaning of fieldLen
 if the numeric conversion fails f.status = MAX(CARDINAL)
*)

PROCEDURE WriteRealFixed(VAR INOUT f : File;
                         num : LONGREAL;
                         place : CARDINAL;
                         fieldLen : INTEGER);
(*
 the real number num is converted to a string in decimal notation
 and written to the file f using the procedure WriteField
 place = the number of digits after the decimal place to output
 See WriteField for the meaning of fieldLen
 if the numeric conversion fails f.status = MAX(CARDINAL)
*)

PROCEDURE WriteBoolean(VAR INOUT f : File; bool : BOOLEAN; fieldLen : INTEGER);
(*
 writes a BOOLEAN value to the file f
 if bool = TRUE then a string value of "TRUE" is written
 otherwise "FALSE" is written
 See WriteField for the meaning of fieldLen
*)

PROCEDURE WriteLn(VAR INOUT f : File);
(*
 writes and end of line character(s) to the file f
 implemented as FileFunc.WriteChar(f, FileFunc.EOL)
*)

PROCEDURE ReadString(VAR INOUT f : File; VAR OUT str : ARRAY OF CHAR);
(*
 reads from the file f as many characters as will fit into the
 string str. IF the end of the current line is reached before the string
 is filled then reading will stop and the string will be null terminated
*)

PROCEDURE ReadField(VAR INOUT f : File; VAR OUT str : ARRAY OF CHAR);
(*
 reads from file f into the string str a string deliminated by the
 characters in the global variable Separators. If the string is not
 fully filled it will be null terminated.
 This function skips over separators before attempting to read the field
*)

PROCEDURE ReadNumber(VAR INOUT f : File) : LONGINT;
(*
 read an integer number from the file f by calling ReadField
 to read a field string and then doing a numeric conversion to an
 integer number. if the conversion fails f.status = MAX(CARDINAL)
 if an error occurs a value of 0 is always returned
*)

PROCEDURE ReadNumberValidate(VAR INOUT f : File;
                             min, max : LONGINT;
                             VAR OUT inRange : BOOLEAN) : LONGINT;
(*
 read an integer number from the file f by calling ReadField
 to read a field string and then doing a numeric conversion to an
 integer number. if the conversion fails f.status = MAX(CARDINAL)
 this function also validates the numeric range of the read number
 if an error occurs a value of 0 is always returned
 if the result within the range of min and max, inclusive then
 inRange = TRUE, otherwise inRange = FALSE
 in either case the number value will be returned
*)

PROCEDURE ReadNumberBase(VAR INOUT f : File; base : CARDINAL) : CARDINAL32;
(*
 read an integer number in the number base, base, from the file f by
 calling ReadField to read a field string and then doing a numeric
 conversion to an integer number.
 if the conversion fails f.status = MAX(CARDINAL)
 if an error occurs a value of 0 is always returned
 base must be >= 2 and <= 16
*)

PROCEDURE ReadReal(VAR INOUT f : File) : LONGREAL;
(*
 read a real number from the file f by calling ReadField to read a
 field string and then doing a numeric conversion to a real number
 the real number can be in decimal, scientific or engineering notation
 if the conversion fails f.status = MAX(CARDINAL)
 if an error occurs a value of 0.0 is always returned
*)

PROCEDURE ReadBoolean(VAR INOUT f : File) : BOOLEAN;
(*
 read a BOOLEAN value from the file f by calling ReadField to read a
 field string and then comparing the field string with the value "TRUE"
 If the field string is equal to "TRUE" then TRUE is returned, otherwise
 FALSE is returned
 The field string is compared case insensitively
*)

PROCEDURE ReadLn(VAR INOUT f : File);
(*
 skip all characters in the current line up to and including the
 end of the line
*)

END TextFileFunc.
DEFINITION MODULE RndFile;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
===========================================*)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END


IMPORT IOChan, ChanConsts, SYSTEM;

TYPE
    ChanId      = IOChan.ChanId;
    FlagSet     = ChanConsts.FlagSet;
    OpenResults = ChanConsts.OpenResults;

  (* Accepted singleton values of FlagSet *)

CONST
  read  = FlagSet{ChanConsts.readFlag}; (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag};(* output operations are requested/available *)
  old   = FlagSet{ChanConsts.oldFlag};  (* a file may/must/did exist before the channel is opened *)
  text  = FlagSet{ChanConsts.textFlag}; (* text operations are requested/available *)
  raw   = FlagSet{ChanConsts.rawFlag};  (* raw operations are requested/available *)

PROCEDURE OpenOld(VAR cid : ChanId;
                  name : ARRAY OF CHAR;
                  flags : FlagSet;
                  VAR res : OpenResults);
  (* Attempts to obtain and open a channel connected to a stored random
     access file of the given name. The old flag is implied; without the
     write flag, read is implied; without the text flag, raw is implied.
     If successful, assigns to cid the identity of the opened channel,
     assigns the value opened to res, and sets the read/write position to
     the start of the file. If a channel cannot be opened as required, the
     value of res indicates the reason, and cid identifies the invalid
     channel.
  *)

PROCEDURE OpenClean(VAR cid : ChanId;
                    name : ARRAY OF CHAR;
                    flags : FlagSet;
                    VAR res : OpenResults);
  (* Attempts to obtain and open a channel connected to a stored random
     access file of the given name. The write flag is implied;
     without the text flag, raw is implied.
     If successful, assigns to cid the identity of the opened channel,
     assigns the value opened to res, and truncates the file to zero length.
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel.
  *)

PROCEDURE IsRndFile(cid : ChanId) : BOOLEAN;
  (* Tests if the channel identified by cid is open to a random access file.
  *)

PROCEDURE IsRndFileException() : BOOLEAN;
  (* Returns TRUE if the current coroutine is in the exceptional execution
     state because of the raising of a RndFile exception;
     otherwise returns FALSE.
  *)

CONST
    FilePosSize = 4;

TYPE
    FilePos = ARRAY [1..FilePosSize] OF SYSTEM.LOC;

PROCEDURE StartPos(cid : ChanId) : FilePos;
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the position of
     the start of the file.
  *)

PROCEDURE CurrentPos(cid : ChanId) : FilePos;
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the position of
     the current read/write position.
  *)

PROCEDURE EndPos(cid : ChanId) : FilePos;
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the first
     position after which there have been no writes.
  *)

PROCEDURE NewPos(cid : ChanId;
                 chunks : INTEGER;
                 chunkSize : CARDINAL;
                 from : FilePos) : FilePos;
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the
     position (chunks * chunkSize) relative to the position given by from,
     or raises the exception posRange if the required position cannot be
     represented as a value of type FilePos.
  *)

PROCEDURE SetPos(cid : ChanId; pos : FilePos);
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise sets the read/write
     position to the value given by pos.
  *)

PROCEDURE Close(VAR cid : ChanId);
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise closes the channel, and
     assigns the value identifying the invalid channel to cid.
  *)

END RndFile.
DEFINITION MODULE SeqFile;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
===========================================*)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END


  (* Rewindable sequential files *)

IMPORT IOChan, ChanConsts;

TYPE
    ChanId      = IOChan.ChanId;
    FlagSet     = ChanConsts.FlagSet;
    OpenResults = ChanConsts.OpenResults;

  (* Accepted singleton values of FlagSet *)

CONST
  read  = FlagSet{ChanConsts.readFlag}; (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag};(* output operations are requested/available *)
  old   = FlagSet{ChanConsts.oldFlag};  (* a file may/must/did exist before the channel is opened *)
  text  = FlagSet{ChanConsts.textFlag}; (* text operations are requested/available *)
  raw   = FlagSet{ChanConsts.rawFlag};  (* raw operations are requested/available *)

PROCEDURE OpenWrite(VAR cid : ChanId;
                    name : ARRAY OF CHAR;
                    flags : FlagSet;
                    VAR res : OpenResults);
  (* Attempts to obtain and open a channel connected to a stored rewindable
     file of the given name. The write flag is implied; without the raw flag,
     text is implied. If successful, assigns to cid the identity of the
     opened channel, assigns the value opened to res, and selects output mode,
     with the write position at the start of the file
     (i.e. the file is of zero length).
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel.
  *)

PROCEDURE OpenAppend(VAR cid : ChanId;
                     name : ARRAY OF CHAR;
                     flags : FlagSet;
                     VAR res : OpenResults);
  (* Attempts to obtain and open a channel connected to a stored rewindable
     file of the given name.
     The write and old flags are implied; without the raw flag, text is
     implied. If successful, assigns to cid the identity of the opened
     channel, assigns the value opened to res, and selects output mode,
     with the write position corresponding to the length of the file.
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel.
  *)

PROCEDURE OpenRead(VAR cid : ChanId;
                   name : ARRAY OF CHAR;
                   flags : FlagSet;
                   VAR res : OpenResults);
  (* Attempts to obtain and open a channel connected to a stored rewindable
     file of the given name. The read and old flags are implied; without the
     raw flag, text is implied.If successful, assigns to cid the identity
     of the opened channel, assigns the value opened to res, and selects
     input mode, with the read position corresponding to the start of the
     file. If a channel cannot be opened as required, the value of res
     indicates the reason, and cid identifies the invalid channel.
  *)

PROCEDURE IsSeqFile(cid : ChanId) : BOOLEAN;
  (* Tests if the channel identified by cid is open to a rewindable
     sequential file. *)

PROCEDURE Reread(cid : ChanId);
  (* If the channel identified by cid is not open to a rewindable sequential
     file, the exception wrongDevice is raised; otherwise attempts to set
     the read position to the start of the file, and to select input mode.
     If the operation cannot be performed (perhaps because of insufficient
     permissions) neither input mode nor output mode is selected.
  *)

PROCEDURE Rewrite(cid : ChanId);
  (* If the channel identified by cid is not open to a rewindable sequential
     file, the exception wrongDevice is raised; otherwise, attempts to
     truncate the file to zero length, and to select output mode.
     If the operation cannot be performed (perhaps because of insufficient
     permissions) neither input mode nor output mode is selected.
  *)

PROCEDURE Close(VAR cid : ChanId);
  (* If the channel identified by cid is not open to a rewindable sequential
     file, the exception wrongDevice is raised; otherwise closes the channel,
     and assigns the value identifying the invalid channel to cid.
  *)

END SeqFile.
DEFINITION MODULE StreamFile;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

  (* Independent sequential data streams *)

IMPORT IOChan, ChanConsts;

TYPE
    ChanId      = IOChan.ChanId;
    FlagSet     = ChanConsts.FlagSet;
    OpenResults = ChanConsts.OpenResults;

  (* Accepted singleton values of FlagSet *)

CONST
  read  = FlagSet{ChanConsts.readFlag}; (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag};(* output operations are requested/available *)
  old   = FlagSet{ChanConsts.oldFlag};  (* a file may/must/did exist before the channel is opened *)
  text  = FlagSet{ChanConsts.textFlag}; (* text operations are requested/available *)
  raw   = FlagSet{ChanConsts.rawFlag};  (* raw operations are requested/available *)

PROCEDURE Open(VAR cid : ChanId;
               name : ARRAY OF CHAR;
               flags : FlagSet;
               VAR res : OpenResults);
  (* Attempts to obtain and open a channel connected to a sequential
     stream of the given name. The read flag implies old; without the raw
     flag, text is implied. If successful, assigns to cid the identity of
     the opened channel, and assigns the value opened to res. If a channel
     cannot be opened as required, the value of res indicates the reason,
     and cid identifies the invalid channel.   *)

PROCEDURE IsStreamFile(cid : ChanId) : BOOLEAN;
  (* Tests if the channel identified by cid is open to a sequential stream. *)

PROCEDURE Close(VAR cid : ChanId);
  (* If the channel identified by cid is not open to a sequential stream,
     the exception wrongDevice is raised; otherwise closes the channel, and
     assigns the value identifying the invalid channel to cid.   *)

END StreamFile.
DEFINITION MODULE TextIO;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END


  (* Input and output of character and string types over specified channels.
     The read result is of the type IOConsts.ReadResults.
  *)

IMPORT IOChan;

  (* The following procedures do not read past line marks *)

PROCEDURE ReadChar(cid : IOChan.ChanId; VAR ch : CHAR);
  (* If possible, removes a character from the input stream cid and
     assigns the corresponding value to ch.  The read result is set to
     the value allRight, endOfLine, or endOfInput.
  *)

PROCEDURE ReadRestLine(cid : IOChan.ChanId; VAR s : ARRAY OF CHAR);
  (* Removes any remaining characters from the input stream cid before
     the next line mark, copying to s as many as can be accommodated as
     a string value. The read result is set to the value allRight,
     outOfRange, endOfLine, or endOfInput.  *)

PROCEDURE ReadString(cid : IOChan.ChanId; VAR s : ARRAY OF CHAR);
  (* Removes only those characters from the input stream cid before the
     next line mark that can be accommodated in s as a string value, and
     copies them to s.  The read result is set to the value allRight,
     endOfLine, or endOfInput.  *)

PROCEDURE ReadToken(cid : IOChan.ChanId; VAR s : ARRAY OF CHAR);
  (* Skips leading spaces, and then removes characters from the input
     stream cid before the next space or line mark, copying to s as
     many as can be accommodated as a string value. The read result is
     set to the value allRight, outOfRange, endOfLine, or end OfInput. *)

  (* The following procedure reads past the next line mark *)

PROCEDURE SkipLine(cid : IOChan.ChanId);
  (* Removes successive items from the input stream cid up to and
     including the next line  mark, or until the end of input is reached.
     The read result is set to the value allRight, or endOfInput.  *)

  (* Output procedures *)

PROCEDURE WriteChar(cid : IOChan.ChanId; ch : CHAR);
  (* Writes the value of ch to the output stream cid. *)

PROCEDURE WriteLn(cid : IOChan.ChanId);
  (* Writes a line mark to the output stream cid. *)

PROCEDURE WriteString(cid : IOChan.ChanId; s : ARRAY OF CHAR);
  (* Writes the string value in s to the output stream cid. *)

END TextIO.
DEFINITION MODULE SIOResult;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
===========================================*)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END


(* Read results for the default input channel *)

IMPORT IOConsts;

TYPE
    ReadResults = IOConsts.ReadResults;

(*
  TYPE
    ReadResults =    (* This type is used to classify the result of an input operation *)
    (
      notKnown,      (* no data read result is set *)
      allRight,      (* data is as expected or as required *)
      outOfRange,    (* data cannot be represented *)
      wrongFormat,  (* data not in expected format *)
      endOfLine,    (* end of line seen before expected data *)
      endOfInput    (* end of input seen before expected data *)
    );
*)

PROCEDURE ReadResult() : ReadResults;

(* Returns the result for the last read operation on the default input
   channel *)

END SIOResult.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1996                                  *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE SLWholeIO;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END


(* Input and output of whole numbers in decimal text form over default
   channels. The read result is of the type IOConsts.ReadResults. *)

(* The text form of a signed whole number is
   ["+" | "-"], decimal digit, {decimal digit}

   The text form of an unsigned whole number is
   decimal digit, {decimal digit}
*)

PROCEDURE ReadLongInt(VAR int : LONGINT);
(* Skips leading spaces, and removes any remaining characters from the
   default input channel that form part of a signed whole number.  The
   value of this number is assigned to int. The read result is set to
   the value allRight, outOfRange, wrongFormat, endOfLine, or endOfInput. *)

PROCEDURE WriteLongInt(int : LONGINT; width : CARDINAL);
  (* Writes the value of int to the default output channel in text form,
     in a field of the given minimum width.A width of zero(0) is special
 and means a single space character will always be output before the number *)

PROCEDURE ReadLongCard(VAR card : LONGCARD);
  (* Skips leading spaces, and removes any remaining characters from the
     default input channel that form part of an unsigned whole number.
     The value of this  number is assigned to card.  The read result is
     set to the value allRight, outOfRange, wrongFormat, endOfLine,
     or endOfInput. *)

PROCEDURE WriteLongCard(card : LONGCARD; width : CARDINAL);
  (* Writes the value of card to the default output channel in text form,
     in a field of the given minimum width.A width of zero(0) is special
 and means a single space character will always be output before the number *)

END SLWholeIO.
DEFINITION MODULE STextIO;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs  1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman

          Implementation  1993
                by R. Sutcliffe
       (Portions coded by G. Tischer)
        Trinity Western University
7600 Glover Rd., Langley, BC Canada V3A 6H4
         e-mail: rsutc@twu.ca
    Last modification date 1993 10 20
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END


  (* Input and output of character and string types over default channels. The read result is of the type IOConsts.ReadResults. *)

  (* The following procedures do not read past line marks *)

PROCEDURE ReadChar(VAR ch : CHAR);
  (* If possible, removes a character from the default input stream,
     and assigns the corresponding value to ch.  The read result is set
     to allRight, endOfLine or endOfInput. *)

PROCEDURE ReadRestLine(VAR s : ARRAY OF CHAR);
  (* Removes any remaining characters from the default input stream before
     the next line mark, copying to s as many as can be accommodated as a
     string value.  The read result is set to the value allRight,
     outOfRange, endOfLine, or endOfInput. *)

PROCEDURE ReadString(VAR s : ARRAY OF CHAR);
  (* Removes only those characters from the default input stream before the
     next line mark that can be accommodated in s as a string value, and
     copies them to s.  The read result is set to the value allRight,
     endOfLine, or endOfInput. *)

PROCEDURE ReadToken(VAR s : ARRAY OF CHAR);
  (* Skips leading spaces, and then removes characters from the default
     input stream before the next space or line mark, copying to s as
     many as can be accommodated as a string value.  The read result is
     set to the value allRight, outOfRange, endOfLine, or endOfInput. *)

  (* The following procedure reads past the next line mark *)

PROCEDURE SkipLine;
  (* Removes successive items from the default input stream up to and
     including the next line mark or until the end of input is reached.
     The read result is set to the value allRight, or endOfInput. *)


  (* Output procedures *)

PROCEDURE WriteChar(ch : CHAR);
  (* Writes the value of ch to the default output stream. *)

PROCEDURE WriteLn;
  (* Writes a line mark to the default output stream. *)

PROCEDURE WriteString(s : ARRAY OF CHAR);
  (* Writes the string value of s to the default output stream. *)

END STextIO.
DEFINITION MODULE LongIO;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END


  (* Input and output of real numbers in decimal text form over specified
channels. The read result is of the type IOConsts.ReadResults. *)

IMPORT IOChan;

  (* The text form of a signed fixed-point real number is
       ["+" | "-"], decimal digit, {decimal digit},
       [".", {decimal digit}]

     The text form of a signed floating-point real number is
       signed fixed-point real number,
       "E", ["+" | "-"], decimal digit, {decimal digit}
  *)

PROCEDURE ReadReal(cid : IOChan.ChanId; VAR real : LONGREAL);
  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of a signed fixed or floating point number.
     The value of this number is assigned to real.
     The read result is set to the value allRight, outOfRange, wrongFormat,
     endOfLine, or  endOfInput. *)

PROCEDURE WriteFloat(cid : IOChan.ChanId;
                     real : LONGREAL;
                     sigFigs : CARDINAL;
                     width : CARDINAL);
  (* Writes the value of real to cid in floating-point text form,
     with sigFigs significant figures, in a field of the given minimum width.
  *)
(* if width = 0 then a single space character will always be output *)
(* before the number. *)

PROCEDURE WriteEng(cid : IOChan.ChanId;
                   real : LONGREAL;
                   sigFigs : CARDINAL;
                   width : CARDINAL);
  (* As for WriteFloat, except that the number is scaled with one to three
     digits in the whole number part, and with an exponent that is a
     multiple of three. *)
(* if width = 0 then a single space character will always be output *)
(* before the number. *)

PROCEDURE WriteFixed(cid : IOChan.ChanId;
                     real : LONGREAL;
                     place : INTEGER;
                     width : CARDINAL);
  (* Writes the value of real to cid in fixed-point text form, rounded to
     the given place relative to the decimal point, in a field of the given
     minimum width. *)
(* if width = 0 then a single space character will always be output *)
(* before the number. *)

PROCEDURE WriteReal(cid : IOChan.ChanId;
                    real : LONGREAL;
                    width : CARDINAL);
  (* Writes the value of real to cid, as WriteFixed if the sign and
     magnitude can be shown in the given width, or otherwise as WriteFloat.
     The number of places or significant digits depends on the given width.
  *)
(* if width = 0 then a single space character will always be output *)
(* before the number. *)

END LongIO.
DEFINITION MODULE LWholeIO;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

  (* Input and output of whole numbers in decimal text form over specified
channels. The read result is of the type IOConsts.ReadResults.
  *)

IMPORT IOChan;

  (* The text form of a signed whole number is
       ["+" | "-"], decimal digit, {decimal digit}

     The text form of an unsigned whole number is
       decimal digit, {decimal digit}
  *)

PROCEDURE ReadLongInt(cid : IOChan.ChanId; VAR int : LONGINT);
  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of a signed whole number.  The value of this number
     is assigned to int.  The read result is set to the value allRight,
     outOfRange, wrongFormat, endOfLine, or endOfInput. *)

PROCEDURE WriteLongInt(cid : IOChan.ChanId; int : LONGINT; width : CARDINAL);
  (* Writes the value of int to cid in text form, in a field of the given
     minimum width. A width of zero(0) is special and means a single
     space character will always be output before the number *)

PROCEDURE ReadLongCard(cid : IOChan.ChanId; VAR card : LONGCARD);
  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of an unsigned whole number.  The value of this number
     is assigned to card.  The read result is set to the value allRight,
     outOfRange, wrongFormat, endOfLine, or endOfInput. *)

PROCEDURE WriteLongCard(cid : IOChan.ChanId; card : LONGCARD; width : CARDINAL);
  (* Writes the value of card to cid in text form, in a field of the given
     minimum width.A width of zero(0) is special and means a single
     space character will always be output before the number *)

END LWholeIO.
DEFINITION MODULE RealIO;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
===========================================*)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END


  (* Input and output of real numbers in decimal text form over specified
channels. The read result is of the type IOConsts.ReadResults. *)

IMPORT IOChan;

  (* The text form of a signed fixed-point real number is
       ["+" | "-"], decimal digit, {decimal digit},
       [".", {decimal digit}]

     The text form of a signed floating-point real number is
       signed fixed-point real number,
       "E", ["+" | "-"], decimal digit, {decimal digit}
  *)

PROCEDURE ReadReal(cid : IOChan.ChanId; VAR real : REAL);
  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of a signed fixed or floating point number.
     The value of this number is assigned to real.
     The read result is set to the value allRight, outOfRange, wrongFormat,
     endOfLine, or  endOfInput. *)

PROCEDURE WriteFloat(cid : IOChan.ChanId;
                     real : REAL;
                     sigFigs : CARDINAL;
                     width : CARDINAL);
  (* Writes the value of real to cid in floating-point text form,
     with sigFigs significant figures, in a field of the given minimum width.
  *)
(* if width = 0 then a single space character will always be output *)
(* before the number. *)

PROCEDURE WriteEng(cid : IOChan.ChanId;
                   real : REAL;
                   sigFigs : CARDINAL;
                   width : CARDINAL);
  (* As for WriteFloat, except that the number is scaled with one to three
     digits in the whole number part, and with an exponent that is a
     multiple of three. *)
(* if width = 0 then a single space character will always be output *)
(* before the number. *)

PROCEDURE WriteFixed(cid : IOChan.ChanId;
                     real : REAL;
                     place : INTEGER;
                     width : CARDINAL);
  (* Writes the value of real to cid in fixed-point text form, rounded to
     the given place relative to the decimal point, in a field of the given
     minimum width. *)
(* if width = 0 then a single space character will always be output *)
(* before the number. *)

PROCEDURE WriteReal(cid : IOChan.ChanId;
                    real : REAL;
                    width : CARDINAL);
  (* Writes the value of real to cid, as WriteFixed if the sign and
     magnitude can be shown in the given width, or otherwise as WriteFloat.
     The number of places or significant digits depends on the given width.
  *)
(* if width = 0 then a single space character will always be output *)
(* before the number. *)

END RealIO.
DEFINITION MODULE SWholeIO;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs  1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

(* Input and output of whole numbers in decimal text form over default
   channels. The read result is of the type IOConsts.ReadResults. *)

(* The text form of a signed whole number is
   ["+" | "-"], decimal digit, {decimal digit}

   The text form of an unsigned whole number is
   decimal digit, {decimal digit}
*)

PROCEDURE ReadInt(VAR int : INTEGER);
(* Skips leading spaces, and removes any remaining characters from the
   default input channel that form part of a signed whole number.  The
   value of this number is assigned to int. The read result is set to
   the value allRight, outOfRange, wrongFormat, endOfLine, or endOfInput. *)

PROCEDURE WriteInt(int : INTEGER; width : CARDINAL);
(* Writes the value of int to the default output channel in text form,
 in a field of the given minimum width. A width of zero(0) is special
 and means a single space character will always be output before the number *)

PROCEDURE ReadCard(VAR card : CARDINAL);
  (* Skips leading spaces, and removes any remaining characters from the
     default input channel that form part of an unsigned whole number.
     The value of this  number is assigned to card.  The read result is
     set to the value allRight, outOfRange, wrongFormat, endOfLine,
     or endOfInput. *)

PROCEDURE WriteCard(card : CARDINAL; width : CARDINAL);
(* Writes the value of card to the default output channel in text form,
 in a field of the given minimum width. A width of zero(0) is special
 and means a single space character will always be output before the number *)

END SWholeIO.
DEFINITION MODULE TermFile;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs  1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

  (* Access to the terminal device *)

  (* Channels opened by this module are connected to a single terminal device;
     typed characters are distributed between channels according to the
     sequence of read requests. *)

IMPORT IOChan, ChanConsts;

TYPE
    ChanId      = IOChan.ChanId;
    FlagSet     = ChanConsts.FlagSet;
    OpenResults = ChanConsts.OpenResults;

  (* Accepted singleton values of FlagSet *)

CONST
  read  = FlagSet{ChanConsts.readFlag}; (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag};(* output operations are requested/available *)
  text  = FlagSet{ChanConsts.textFlag}; (* text operations are requested/available *)
  raw   = FlagSet{ChanConsts.rawFlag};  (* raw operations are requested/available *)
  echo  = FlagSet{ChanConsts.echoFlag}; (* echoing by interactive device on reading of characters from input stream requested/applies *)

PROCEDURE Open(VAR cid : ChanId;
               flags : FlagSet;
               VAR res : OpenResults);
  (* Attempts to obtain and open a channel connected to the terminal.
     Without the raw flag, text is implied.
     Without the echo flag, line mode is requested, otherwise single
     character mode is requested.
     If successful, assigns to cid the identity of the opened channel, and
     assigns the value opened to res.
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel. *)

PROCEDURE IsTermFile(cid : ChanId) : BOOLEAN;
  (* Tests if the channel identified by cid is open to the terminal. *)

PROCEDURE Close(VAR cid : ChanId);
  (* If the channel identified by cid is not open to the terminal,
     the exception wrongDevice is raised; otherwise closes the channel
     and assigns the value identifying the invalid channel to cid. *)

END TermFile.
(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 1988-2002                          *)
(*                         by Stony Brook Software                         *)
(*                                                                         *)
(*                           All rights reserved.                          *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE Terminal;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

(* a simple text output *)
(* the size of the display is generally 80x25, but may be different *)

CONST
    (* these character codes are used for some non ASCII characters *)
    (* supported by this terminal module *)

    CursorUp    = CHR(1);
    CursorDown  = CHR(2);
    PageUp      = CHR(3);
    PageDown    = CHR(4);
    CursorLeft  = CHR(5);
    CursorRight = CHR(6);

    Escape      = CHR(27);
    Tab         = CHR(9);
    BackSpace   = CHR(8);
    Bell        = CHR(7);
    Enter       = CHR(13);
    LineFeed    = CHR(10);

PROCEDURE Write(ch : CHAR);
(* write a single character to the currsor cursor position *)

PROCEDURE WriteString(str : ARRAY OF CHAR);
(* write a string to the cursor position *)

PROCEDURE WriteLn;
(* set the cursor X position to 0, far left *)
(* and increment the Y positon, down *)
(* if the Y position is greater that the terminal window size *)
(* then the terminal window data is scrolled and a blank line *)
(* is displayed at the bottom of the terminal window *)
(* and the Y position of the cursor is at the bottom of the terminal *)
(* window *)

PROCEDURE Position(X, Y : CARDINAL);
(* set the cursor position *)

PROCEDURE CharAvail() : BOOLEAN;
(* is there a character keystroke available for input *)

PROCEDURE Read(VAR OUT ch : CHAR);
(* read a character keystroke *)

PROCEDURE ReadChar() : CHAR;
(* same as Read, but in function form *)

PROCEDURE Reset;
(* clears the screen and places the cursor at 0,0 *)

END Terminal.
DEFINITION MODULE WholeIO;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

  (* Input and output of whole numbers in decimal text form over specified
channels. The read result is of the type IOConsts.ReadResults.
  *)

IMPORT IOChan;

  (* The text form of a signed whole number is
       ["+" | "-"], decimal digit, {decimal digit}

     The text form of an unsigned whole number is
       decimal digit, {decimal digit}
  *)

PROCEDURE ReadInt(cid : IOChan.ChanId; VAR int : INTEGER);
  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of a signed whole number.  The value of this number
     is assigned to int.  The read result is set to the value allRight,
     outOfRange, wrongFormat, endOfLine, or endOfInput. *)

PROCEDURE WriteInt(cid : IOChan.ChanId; int : INTEGER; width : CARDINAL);
  (* Writes the value of int to cid in text form, in a field of the given
     minimum width.  A width of zero(0) is special and means a single
     space character will always be output before the number *)

PROCEDURE ReadCard(cid : IOChan.ChanId; VAR card : CARDINAL);
  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of an unsigned whole number.  The value of this number
     is assigned to card.  The read result is set to the value allRight,
     outOfRange, wrongFormat, endOfLine, or endOfInput. *)

PROCEDURE WriteCard(cid : IOChan.ChanId; card : CARDINAL; width : CARDINAL);
  (* Writes the value of card to cid in text form, in a field of the given
     minimum width. A width of zero(0) is special and means a single
     space character will always be output before the number *)


END WholeIO.
DEFINITION MODULE FIO;

FROM SYSTEM IMPORT
    BYTE;

IMPORT Str;

CONST
    DiskFull		= 0F0h;

    StandardInput	= 1;
    StandardOutput	= 2;
    ErrorOutput		= 3;
    AuxDevice		= 4;
    PrinterDevice	= 5;

%IF DOS %OR FlashTek %OR WIN16 %THEN
    MaxOpenFiles	= 20;
%ELSE
    MaxOpenFiles	= 100;
%END
    BufferOverhead	= 0;

TYPE
    File		= CARDINAL;
    CHARSET		= Str.CHARSET;

    PathTail		= ARRAY [0..12] OF CHAR;
    FileAttr		= SET OF (
				  readonly,
				  hidden,
				  system,
				  volume,
				  directory,
				  archive
				 );

    <*/PUSH/PACK/NOWARN:A*>
    DirEntry		=
	RECORD
	rsvd		: ARRAY [0..20] OF CARDINAL8;
	attr		: FileAttr;
	time		: CARDINAL16;
	date		: CARDINAL16;
	size		: CARDINAL32;
	name		: PathTail;
	END;
    <*/POP*>

VAR
    RunTimeError	: PROCEDURE(CARDINAL32, CARDINAL, ARRAY OF CHAR);

    IOcheck		: BOOLEAN;
    EOF			: BOOLEAN;
    Separators		: Str.CHARSET;
    OK			: BOOLEAN;
    ChopOff		: BOOLEAN;
    Eng			: BOOLEAN;
    PrefixChar		: CHAR;
    PostfixChar		: CHAR;

PROCEDURE Append(name : ARRAY OF CHAR) : File;

PROCEDURE AssignBuffer(f : File; VAR buf : ARRAY OF BYTE);

PROCEDURE ChDir(name : ARRAY OF CHAR);

PROCEDURE Close(f : File);

PROCEDURE Create(name : ARRAY OF CHAR) : File;

PROCEDURE Erase(name : ARRAY OF CHAR);

PROCEDURE Exists(name : ARRAY OF CHAR) : BOOLEAN;

PROCEDURE Flush(f : File);

PROCEDURE GetCurrentDate() : CARDINAL32;

PROCEDURE GetDir(drive : SHORTCARD; VAR name : ARRAY OF CHAR);

PROCEDURE GetDrive() : SHORTCARD;

PROCEDURE GetFileDate(f : File) : CARDINAL32;

PROCEDURE GetPos(f : File) : CARDINAL32;

PROCEDURE IOresult() : CARDINAL;

PROCEDURE MkDir(name : ARRAY OF CHAR);

PROCEDURE Open(name : ARRAY OF CHAR) : File;

PROCEDURE OpenRead(name : ARRAY OF CHAR) : File;

PROCEDURE RdBin(f : File;
		VAR buf : ARRAY OF BYTE;
		amount : CARDINAL) : CARDINAL;


PROCEDURE RdItem(f : File; VAR str : ARRAY OF CHAR);

PROCEDURE RdChar(f : File) : CHAR;

PROCEDURE RdBool(f : File) : BOOLEAN;

PROCEDURE RdShtInt(f : File) : SHORTINT;

PROCEDURE RdInt(f : File) : INTEGER;

PROCEDURE RdLngInt(f : File) : LONGINT;

PROCEDURE RdShtCard(f : File) : SHORTCARD;

PROCEDURE RdCard(f : File) : CARDINAL;

PROCEDURE RdLngCard(f : File) : CARDINAL32;

PROCEDURE RdShtHex(f : File) : SHORTCARD;

PROCEDURE RdHex(f : File) : CARDINAL;

PROCEDURE RdLngHex(f : File) : CARDINAL32;

PROCEDURE RdReal(f : File) : REAL;

PROCEDURE RdLngReal(f : File) : LONGREAL;

PROCEDURE RdStr(f : File; VAR str : ARRAY OF CHAR);

PROCEDURE ReadFirstEntry(path : ARRAY OF CHAR;
			 attr : FileAttr;
			 VAR entry : DirEntry) : BOOLEAN;

PROCEDURE ReadNextEntry(VAR entry : DirEntry) : BOOLEAN;

PROCEDURE Rename(fromName, toName : ARRAY OF CHAR);

PROCEDURE RmDir(name : ARRAY OF CHAR);

PROCEDURE Seek(f : File; pos : CARDINAL32);

PROCEDURE SetDrive(drive : SHORTCARD);

PROCEDURE SetFileDate(f : File; dt : CARDINAL32);

PROCEDURE Size(f : File) : CARDINAL32;

PROCEDURE Truncate(f : File);

PROCEDURE WrBin(f : File; buf : ARRAY OF BYTE; amount : CARDINAL);

PROCEDURE WrCharRep(f : File; ch : CHAR; amount : CARDINAL);

PROCEDURE WrLn(f : File);

PROCEDURE WrChar(f : File; ch : CHAR);

PROCEDURE WrBool(f : File; bool : BOOLEAN; len : INTEGER);

PROCEDURE WrShtInt(f : File; num : SHORTINT; len : INTEGER);

PROCEDURE WrInt(f : File; num : INTEGER; len : INTEGER);

PROCEDURE WrLngInt(f : File; num : LONGINT; len : INTEGER);

PROCEDURE WrShtCard(f : File; num : SHORTCARD; len : INTEGER);

PROCEDURE WrCard(f : File; num : CARDINAL; len : INTEGER);

PROCEDURE WrLngCard(f : File; num : CARDINAL32; len : INTEGER);

PROCEDURE WrShtHex(f : File; num : SHORTCARD; len : INTEGER);

PROCEDURE WrHex(f : File; num : CARDINAL; len : INTEGER);

PROCEDURE WrLngHex(f : File; num : CARDINAL32; len : INTEGER);

PROCEDURE WrReal(f : File;
		 num : REAL;
		 precision : CARDINAL;
		 len : INTEGER);

PROCEDURE WrLngReal(f : File;
		    num : LONGREAL;
		    precision : CARDINAL;
		    len : INTEGER);

PROCEDURE WrFixReal(f : File;
		    num : REAL;
		    precision : CARDINAL;
		    len : INTEGER);

PROCEDURE WrFixLngReal(f : File;
		       num : LONGREAL;
		       precision : CARDINAL;
		       len : INTEGER);

PROCEDURE WrStr(f : File; str : ARRAY OF CHAR);

PROCEDURE WrStrAdj(f : File; str : ARRAY OF CHAR; len : INTEGER);

END FIO.
DEFINITION MODULE FIOR;

TYPE
    ExtStr	= ARRAY [0..2] OF CHAR;

PROCEDURE AddExtension(VAR spec : ARRAY OF CHAR; ext : ARRAY OF CHAR);

PROCEDURE ChangeExtension(VAR spec : ARRAY OF CHAR; ext : ARRAY OF CHAR);

PROCEDURE RemoveExtension(VAR spec : ARRAY OF CHAR);

PROCEDURE ExpandPath(path : ARRAY OF CHAR; VAR fullPath : ARRAY OF CHAR);

PROCEDURE IsExtension(spec : ARRAY OF CHAR; ext : ExtStr) : BOOLEAN;

PROCEDURE MakePath(VAR path : ARRAY OF CHAR;
		       head, tail : ARRAY OF CHAR);

PROCEDURE SplitPath(path : ARRAY OF CHAR;
		    VAR head, tail : ARRAY OF CHAR);

END FIOR.
DEFINITION MODULE IO;

IMPORT Str;

CONST
    MaxRdLength		= 256;

TYPE
    WrStrType		= PROCEDURE(ARRAY OF CHAR);
    RdStrType		= PROCEDURE(VAR ARRAY OF CHAR);
    CHARSET		= Str.CHARSET;

VAR
    RdStrRedirect	: RdStrType;
    WrStrRedirect	: WrStrType;
    InputRedirected	: BOOLEAN;
    OutputRedirected	: BOOLEAN;

    RdLnOnWr		: BOOLEAN;
    Prompt		: BOOLEAN;

    Separators		: CHARSET;
    OK			: BOOLEAN;
    ChopOff		: BOOLEAN;
    Eng			: BOOLEAN;
    PrefixChar		: CHAR;
    SuffixChar		: CHAR;

PROCEDURE RedirectInput(fileName : ARRAY OF CHAR);

PROCEDURE RedirectOutput(fileName : ARRAY OF CHAR);

PROCEDURE RdStr(VAR str : ARRAY OF CHAR);

PROCEDURE EndOfRd(skip : BOOLEAN) : BOOLEAN;

PROCEDURE RdLn;

PROCEDURE RdChar() : CHAR;

PROCEDURE RdBool() : BOOLEAN;

PROCEDURE RdShtInt() : SHORTINT;

PROCEDURE RdInt() : INTEGER;

PROCEDURE RdLngInt() : LONGINT;

PROCEDURE RdShtCard() : SHORTCARD;

PROCEDURE RdCard() : CARDINAL;

PROCEDURE RdLngCard() : CARDINAL32;

PROCEDURE RdShtHex() : SHORTCARD;

PROCEDURE RdHex() : CARDINAL;

PROCEDURE RdLngHex() : CARDINAL32;

PROCEDURE RdReal() : REAL;

PROCEDURE RdLngReal() : LONGREAL;

PROCEDURE RdItem(VAR str : ARRAY OF CHAR);

PROCEDURE WrStr(str : ARRAY OF CHAR);

PROCEDURE WrStrAdj(str : ARRAY OF CHAR; len : INTEGER);

PROCEDURE WrLn;

PROCEDURE WrChar(ch : CHAR);

PROCEDURE WrBool(bool : BOOLEAN; len : INTEGER);

PROCEDURE WrShtInt(num : SHORTINT; len : INTEGER);

PROCEDURE WrInt(num : INTEGER; len : INTEGER);

PROCEDURE WrLngInt(num : LONGINT; len : INTEGER);

PROCEDURE WrShtCard(num : SHORTCARD; len : INTEGER);

PROCEDURE WrCard(num : CARDINAL; len : INTEGER);

PROCEDURE WrLngCard(num : CARDINAL32; len : INTEGER);

PROCEDURE WrShtHex(num : SHORTCARD; len : INTEGER);

PROCEDURE WrHex(num : CARDINAL; len : INTEGER);

PROCEDURE WrLngHex(num : CARDINAL32; len : INTEGER);

PROCEDURE WrReal(num : REAL; precision : CARDINAL; len : INTEGER);

PROCEDURE WrLngReal(num : LONGREAL; precision : CARDINAL; len : INTEGER);

PROCEDURE WrFixReal(num : REAL; precision : CARDINAL; len : INTEGER);

PROCEDURE WrFixLngReal(num : LONGREAL; precision : CARDINAL; len : INTEGER);

PROCEDURE WrCharRep(ch : CHAR; count : CARDINAL);

END IO.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1987-2002                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

DEFINITION MODULE SYSTEMC;

%IF SPARC %AND Bits32 %THEN
<*/PROCESSOR:V8/NOAPPREGS*>
(* limit the registers compiler "helper" procedures can use.
   these procedures have no symbol file.
   to be compatible with any possible user options we choose the
   least common denoiminator
   *)
%END

FROM SYSTEM IMPORT
    ADDRESS;

%IF WIN32 %OR OS2 %OR UNIX %THEN
PROCEDURE STACKGROWTH(pages : CARDINAL)
                            %IF IA32 %THEN
                            [ALTERS()];
                            %ELSE
                            ;
                            %END
%END

PROCEDURE MOVE(SOURCE, DEST : ADDRESS; COUNT : CARDINAL)
                                %IF IA32 %THEN
                                [PASS(,,AX), ALTERS(AX)];
                                %ELSE
                                [LeftToRight];
                                %END

PROCEDURE MOVEBYTES(SOURCE : ADDRESS; SRC_LEN : CARDINAL;
                    DEST : ADDRESS; DEST_LEN : CARDINAL)
                                %IF IA32 %THEN
                                [PASS(,DX,,AX), ALTERS(AX,DX)];
                                %ELSE
                                [LeftToRight];
                                %END

PROCEDURE COPYPAR3S(source, dest : ADDRESS; amount : CARDINAL) [LeftToRight];

PROCEDURE COPYOPENPARAMS;

%IF IA32 %AND Bits16 %THEN
PROCEDURE COPYPAR3;

PROCEDURE COPYOPENPARAM;
%END

%IF %NOT IA32 %THEN
(*
PROCEDURE LOADUA_WORDS;

PROCEDURE LOADUA_WORDU;
*)

PROCEDURE LOADUA_DWORDS;

PROCEDURE LOADUA_DWORDU;

PROCEDURE LOADUA_LONG;

PROCEDURE LOADUA_REAL4;

PROCEDURE LOADUA_REAL8;

PROCEDURE LOADUA_COMPLEX4;

PROCEDURE LOADUA_COMPLEX8;

(*
PROCEDURE STOREUA_WORD;
*)

PROCEDURE STOREUA_DWORD;

PROCEDURE STOREUA_LONG;

PROCEDURE STOREUA_REAL4;

PROCEDURE STOREUA_REAL8;

PROCEDURE STOREUA_COMPLEX4;

PROCEDURE STOREUA_COMPLEX8;

PROCEDURE CALLUA;
%END

PROCEDURE LMUL(RIGHT, LEFT : LONGINT) : LONGINT
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

PROCEDURE LMULCHECK(RIGHT, LEFT : LONGINT) : LONGINT
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

PROCEDURE LDIVIS(RIGHT, LEFT : LONGINT) : LONGINT
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

PROCEDURE LDIVIU(RIGHT, LEFT : LONGCARD) : LONGCARD
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

PROCEDURE LREMU(RIGHT, LEFT : LONGCARD) : LONGCARD
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

PROCEDURE LREMS(RIGHT, LEFT : LONGINT) : LONGINT
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

PROCEDURE LDIVS(RIGHT, LEFT : LONGINT) : LONGINT
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

PROCEDURE LMODS(RIGHT, LEFT : LONGINT) : LONGINT
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

PROCEDURE DIVS(RIGHT, LEFT : INTEGER) : INTEGER
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

PROCEDURE MODS(RIGHT, LEFT : INTEGER) : INTEGER
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END

%IF SPARC %THEN
PROCEDURE FLOAT32(i : INTEGER) : LONGREAL;
PROCEDURE FLOAT64(li : LONGINT) : LONGREAL;
%END

PROCEDURE TTRUNC ["SYSTEMC_TRUNC"] ();
(*
PROCEDURE TROUND ["SYSTEMC_ROUND"] ();
PROCEDURE TINT ["SYSTEMC_INT"] ();
PROCEDURE TFRAC ["SYSTEMC_FRAC"] ();
*)

%IF IA32 %THEN
PROCEDURE MULC;
PROCEDURE CMPC;
%END
PROCEDURE DIVC;

VAR
    CapTable    : ARRAY [MIN(ACHAR)..MAX(ACHAR)] OF ACHAR;

PROCEDURE InitCapTable;

PROCEDURE CAP(ch : ACHAR) : ACHAR
                %IF IA32 %THEN
                [PASS(AX)];
                %ELSE
                ;
                %END

PROCEDURE UCAP(ch : UCHAR) : UCHAR
                %IF IA32 %THEN
                [PASS(AX)];
                %ELSE
                ;
                %END

END SYSTEMC.
(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 1994-2002                          *)
(*                      by Stony Brook Software                            *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE StdHandles;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

(* the meanings the the various functions should be obvious by their names *)

PROCEDURE HaveStdInput() : BOOLEAN;

PROCEDURE StdInputHandle() : CARDINAL;

PROCEDURE HaveStdOutput() : BOOLEAN;

PROCEDURE StdOutputHandle() : CARDINAL;

PROCEDURE HaveStdError() : BOOLEAN;

PROCEDURE StdErrorHandle() : CARDINAL;
(* generally the same as the standard output handle *)

PROCEDURE IsConsole(h : CARDINAL) : BOOLEAN;
(* is the handle refering to a command console *)
(* for input handles this generally means the keyboard *)
(* for output handles this means the display *)

END StdHandles.
DEFINITION MODULE SetExStorageDebugMode;

END SetExStorageDebugMode.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1987-2002                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE RunProg;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

TYPE
    ExecHandle;

    ExecFlags = (ExecAsync,
                 ExecDetached,
                 ExecMinimized,
                 ExecMaximized,
                 ExecHidden,
                 ExecNormalPriority,
                 ExecIdlePriority,
                 ExecHighPriority);
    ExecFlagSet = SET OF ExecFlags;

CONST
    SyncExec    = ExecFlagSet{};
    AsyncExec   = ExecFlagSet{ExecAsync};

PROCEDURE RunProgram(name, command, defaultPath : ARRAY OF CHAR;
                     flags : ExecFlagSet;
                     VAR OUT status : CARDINAL) : BOOLEAN;
(*
   Run the program given by "name" and passing the command line
   specified by "command".  The exit status of the program
   is returned in status
   defaultPath is the default device and directory for the application.
   if defaultPath = "" then the curreent default path is used.
   this function returns TRUE if the program was executed.
   The program is executed synchronous by default, meaning the program
   being executed must terminate before this call will return.
   status has no meaning unless the program is executed synchronous.

   on Unix systems when ExecAsync is used the executed process is
   orphaned by this call. Use RunProgramEx to not orphan the process.
   the process is orphaned because Unix want's the parent
   to "wait" on the child.

   All ExecFlags are optional behavior if the targeted operating
   system supports that option. If the system does not support an option
   then it is ignored.
   ExecAsync => If this flag is given then RunProgram will return after
                the specified program starts executing.
   ExecDetached => the process will not have standard input/output. All
                   standard output will be ignored. The process will receive
                   an error on standard input reads.
                   The program can create windows if it desires
                   ExecDetached implies ExecAsync
                   The program is run in the background
   ExecMinimized => The application will be informed to open its window
                    in the minimized state.
   ExecMaximized => The application will be informed to open its window
                    in the maximized state.
   ExecHidden    => The application will be informed to open its window hidden.
   Exec...Priority => Normal is assumed if none of these flags are used.
                      determines the process execution priority
*)

PROCEDURE RunProgramEx(name, command, defaultPath : ARRAY OF CHAR;
                       flags : ExecFlagSet;
                       VAR OUT handle : ExecHandle) : BOOLEAN;
(*
   this procedure is always used with ExecAsync
   if this flag is not specified it will return FALSE,
   otherwise this procedure operates just as RunProgram.
   the parameters are as RunProgram.
   the procedure returns in the parameter 'handle'
   a value which you can later use to retrieve the exit status
   of the program.
   when you are done with the program you should call TerminateProgram
   to free up resources allocated internally to track the executed program
   TerminateProgram also terminates the program if it is still running.
*)

PROCEDURE GetProgramExitStatus(handle : ExecHandle) : CARDINAL;
(*
   retrieve the exit value of the program
   if the program has not terminated MAX(CARDINAL) will be returned
*)

PROCEDURE TerminateProgram(VAR INOUT handle : ExecHandle);
(*
   this terminates the program you executed and releases resources
   allocated to track the executed program
   the value of handle is invalid(NIL) after this call
*)

PROCEDURE PerformCommand(com : ARRAY OF CHAR;
                         flags : ExecFlagSet;
                         VAR OUT status : CARDINAL) : BOOLEAN;
(*
   Run the command shell to perform the command, the com parameter.
   status is the exit status of the command shell
   not of a program that it runs
   returns TRUE if the command shell was executed
   status is the return result of the command interpreter.
*)

END RunProg.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1987-2002                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE RConversions;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

PROCEDURE RealToString(num : LONGREAL;
                       digits : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Convert a real number into a string buf starting at position pos. *)
(* pos is left pointing at the next character after the converted number *)
(* done signifies the success of the conversion *)

PROCEDURE RealToStringFixed(num : LONGREAL;
                            digits, before : CARDINAL;
                            VAR OUT str : ARRAY OF CHAR;
                            VAR INOUT pos : CARDINAL;
                            VAR OUT done : BOOLEAN);
(* Convert a real number into a string buf starting at position pos using *)
(* fixed point notation with digits digits and before digits before the *)
(* decimal point *)
(* done signifies the success of the conversion *)

PROCEDURE StringToReal(str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT num : LONGREAL;
                       VAR OUT done : BOOLEAN);
(* Get a longreal number from a string starting at position pos. *)
(* pos is left pointing to the first character that is not part of *)
(* the number done signifies the success of the conversion *)

END RConversions.
(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 1995-2002                          *)
(*                      by Stony Brook Software                            *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE MemUtils;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

<*/NOHIGH*>(* none of these calls care about this, so why pass usused data *)

FROM SYSTEM IMPORT
    LOC, BYTE, WORD, DWORD;

PROCEDURE FillMemBYTE(VAR OUT dest : ARRAY OF LOC;
                      numBytes : CARDINAL;
                      db : BYTE);
(* fill the memory location specified in dest *)
(* BYTE = 8bits *)
(* numBytes is the number of bytes to fill *)
(* db is the byte to use to fill dest *)

PROCEDURE FillMemWORD(VAR OUT dest : ARRAY OF LOC;
                      numWords : CARDINAL;
                      dw : WORD);
(* fill the memory location specified in dest *)
(* WORD = 16bits *)
(* numWords is the number of words to fill *)
(* dw is the word to use to fill dest *)

PROCEDURE FillMemDWORD(VAR OUT dest : ARRAY OF LOC;
                       numDwords : CARDINAL;
                       dd : DWORD);
(* fill the memory location specified in dest *)
(* DWORD = 32bits *)
(* numDwords is the number of dwords to fill *)
(* dd is the dword to use to fill dest *)

PROCEDURE ScanMemBYTE(dest : ARRAY OF LOC;
                      numBytes : CARDINAL;
                      db : BYTE) : CARDINAL;
(* scan the memory location specified in dest *)
(* BYTE = 8bits *)
(* numBytes is the number of bytes at most to scan *)
(* db is the byte to use to scan dest *)
(* the return value will be the first location where the byte db *)
(* was found while scanning. If the first location matches a value of zero *)
(* is returned. If the value was not found then numBytes will be returned *)

PROCEDURE ScanMemNeBYTE(dest : ARRAY OF LOC;
                        numBytes : CARDINAL;
                        db : BYTE) : CARDINAL;
(* like ScanMemBYTE but looks for the first location not equal to db *)

PROCEDURE ScanMemWORD(dest : ARRAY OF LOC;
                      numWords : CARDINAL;
                      dw : WORD) : CARDINAL;
(* scan the memory location specified in dest *)
(* WORD = 16bits *)
(* numWords is the number of words at most to scan *)
(* dw is the word to use to scan dest *)
(* the return value will be the first location where the word dw *)
(* was found while scanning. If the first location matches a value of zero *)
(* is returned. If the value was not found then numWords will be returned *)

PROCEDURE ScanMemNeWORD(dest : ARRAY OF LOC;
                        numWords : CARDINAL;
                        dw : WORD) : CARDINAL;
(* like ScanMemWORD but looks for the first location not equal to dw *)

PROCEDURE ScanMemDWORD(dest : ARRAY OF LOC;
                       numDwords : CARDINAL;
                       dd : DWORD) : CARDINAL;
(* scan the memory location specified in dest *)
(* DWORD = 32bits *)
(* numDwords is the number of dwords at most to scan *)
(* dd is the dword to use to scan dest *)
(* the return value will be the first location where the dword dd *)
(* was found while scanning. If the first location matches a value of zero *)
(* is returned. If the value was not found then numDwords will be returned *)

PROCEDURE ScanMemNeDWORD(dest : ARRAY OF LOC;
                         numDwords : CARDINAL;
                         dd : DWORD) : CARDINAL;
(* like ScanMemDWORD but looks for the first location not equal to dd *)

PROCEDURE MoveMem(VAR OUT dest : ARRAY OF LOC;
                  src : ARRAY OF LOC;
                  numBytes : CARDINAL);
(* this procedure correctly handles overlapping memory regions *)
(* by performing the memory move top down when necessary *)
(* numBytes is the number of bytes to move *)
(* BYTE = 8bits *)

END MemUtils.
(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 1995-2002                          *)
(*                      by Stony Brook Software                            *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE FormatString;

FROM SYSTEM IMPORT
    ADDRESS;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL*>
%END
<*/COPYATTRIBUTES*>

PROCEDURE FormatString(formatStr : ARRAY OF CHAR;
                       VAR OUT destStr : ARRAY OF CHAR) : BOOLEAN
                         [RightToLeft, LEAVES, VARIABLE];

(* destStr can be the same string as formatStr. *)
(* this procedure takes a variable number of parameters *)
(* to accomodate the contents of the format string *)

(* note that when you pass a numeric constant you are *)
(* passing a longint, since in Stony Brook compilers all constants *)
(* are longint size until typed by an assignment or expression. *)
(* there is no formal parameter for a variable parameter to type check the constant to. *)
(* use ORD or INT if you need a CARDINAL or INTEGER sized constant *)
(* passed as one of the variable parameters. *)

(* the following character combinations allow simple entry of some *)
(* control characters into a format string *)
(* control characters are prefixed by a backslash character *)
(* \n = new line
        on Unix systems this is a single linefeed character CHR(10).
        on other systems this outputs two characters CHR(13)CHR(10)
*)
(* \t = horizontal tab *)
(* \v = vertical tab *)
(* \f = form feed *)
(* \x000 = character in hexidecimal form *)
(* if you want a backslash character in the output string then you need *)
(* to use a double backslash character sequence \\ character sequence. *)
(* if an unknown control specifier follows a backslash it is simply output *)
(* as itself, remember that the \ preceeding the char is not output *)

(*
  Format specifications, discussed below, always begin with a percent
  sign (%). If a percent sign is followed by a character that has no
  meaning as a format field, the function immediately returns FALSE,
  unless the next character is also a percent sign. In this case a
  single percent sign is placed into the output string.
  The format-control string is read from left to right. When the first
  format specification (if any) is encountered, it causes the value of
  the first argument after the format-control string to be converted and
  copied to the output string according to the format specification.
  The second format specification causes the second argument to be
  converted and copied, and so on. If there are more arguments than
  format specifications, the extra arguments are ignored. If there are
  not enough arguments for all of the format specifications, the results
  are undefined.

  The function will return FALSE if the output string cannot fully contain
  the results of the formatting.
  An illegal format field will cause a return of FALSE.
  A hex control character that is too large will cause a return of FALSE.
  A field width that is too large will cause a return of FALSE.
  Otherwise the function returns TRUE.

  A format specification has the following form:
    %[-]['][width]type

  Each field is a single character or a number signifying a particular
  format option. The type characters that appear after the last optional
  format field determine whether the associated argument is interpreted
  as a string, or a number. The simplest format specification contains
  only the percent sign and a type character (for example, %s).
  The optional fields control other aspects of the formatting.
  Following are the optional and required fields and their meanings:

  Field         Meaning
  -             Pad the output to the right to fill the field width, thus
                justifying output to the left.
                If this field is omitted, the output is padded to the left,
                thus justifying it to the right.

  '             The single quote character is followed by a character
                which will be used as the padding character if padding
                of the field is necessary. The default padding character
                is a blank space.

  width         A field width is specified by a cardinal constant or
                an asterisk. An asterisk specifies the field width is a
                variable and is the next parameter in the format parameters.
                The parameter type is CARDINAL.

                Width must be <= 256.

                Copy the specified minimum number of characters to the
                output string. The width field is a cardinal and the
                default is zero.

                The width specification never causes a value to be
                truncated; if the number of characters in the output value
                is greater than the specified width, or if the width field
                is not present, all characters of the value are printed.

                Pad characters will be output as necessary to fill the
                field width if the value does not fully occupy the
                specified width.

  type          Output the corresponding argument as a string, or a number.

                This field can be any of the following character sequences:
                The output type specifiers are not case sensitive.

        c       the type is CARDINAL, output is in decimal
        h       the type is CARDINAL, output is in hexadecimal
        i       the type is INTEGER, output is in decimal
        l       the type is LONGINT, output is in decimal
        s       the type is a null terminated ARRAY OF CHAR.
                The terminating null character is mandatory.
*)

PROCEDURE FormatStringEx(formatStr : ARRAY OF CHAR;
                         VAR OUT destStr : ARRAY OF CHAR;
                         args : ADDRESS) : BOOLEAN;
(*
  as FormatString except the base address of the "variable" arguments is
  passed in the args parameter.

  this function can be useful within a procedure that accepts a variable number of
  arguments and still wants to call FormatString with those variable arguments.
  in this case do the following.

  PROCEDURE MyProc(...
  VAR
      addr : ADDRESS;
  BEGIN
      VA_START(addr);
      ...
      ok := FormatStringEx(format, dest, addr);
  END MyProc;
*)

END FormatString.
(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 1994-2002                          *)
(*                      by Stony Brook Software                            *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE FormatDT;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

FROM SysClock IMPORT
    DateTime;

TYPE
    DateFormats         = (MonthDayYear, DayMonthYear, YearMonthDay);

(* the following variables contain the formatting information *)
(* used in converting between strings and the DateTime record *)

VAR
    LeadingDayZero      : BOOLEAN;
    LeadingMonthZero    : BOOLEAN;
    LeadingTimeZero     : BOOLEAN;

    Time24Hour          : BOOLEAN;

    TimeSep             : ARRAY [0..15] OF CHAR;
    DateSep             : ARRAY [0..15] OF CHAR;

    FullYear            : BOOLEAN;

    AmStr               : ARRAY [0..15] OF CHAR;
    PmStr               : ARRAY [0..15] OF CHAR;

    DateFormat          : DateFormats;

PROCEDURE GetSystemFormatInfo;
(* this procedure retrieves the formatting information from the system *)
(* and stores it in the above variables *)
(* This procedure is called by the module initialization code. *)
(* It is here in case you need to reload the system information later *)

PROCEDURE DateTimeToString(dt : DateTime;
                           VAR OUT date : ARRAY OF CHAR;
                           VAR OUT time : ARRAY OF CHAR);
(* this function converts the DateTime to string format using *)
(* the current formatting information *)

PROCEDURE StringToDateTime(dateStr : ARRAY OF CHAR;
                           timeStr : ARRAY OF CHAR;
                           VAR OUT dt : DateTime) : BOOLEAN;
(* the function converts the strings to a DateTime record using the *)
(* current formatting information *)

END FormatDT.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1992-2002                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)


(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1994-2002                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE ExStrings;
(* this module adds additional functionality to the ISO Strings module. *)
(* ISO does not allow the Strings module to be extended, hence the *)
(* existence of this module. *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END
<*/COPYATTRIBUTES*>

IMPORT Strings;

TYPE
    CompareResults      = Strings.CompareResults;

(* case insensitive versions of ISO Strings functions *)

PROCEDURE CompareI(stringVal1, stringVal2 : ARRAY OF CHAR)
                                    : CompareResults [Invariant];

PROCEDURE EqualI(stringVal1, stringVal2 : ARRAY OF CHAR) : BOOLEAN [Invariant];

PROCEDURE FindNextI(pattern, stringToSearch : ARRAY OF CHAR;
                    startIndex : CARDINAL;
                    VAR OUT patternFound : BOOLEAN;
                    VAR OUT posOfPattern : CARDINAL);

PROCEDURE FindPrevI(pattern, stringToSearch : ARRAY OF CHAR;
                    startIndex : CARDINAL;
                    VAR OUT patternFound : BOOLEAN;
                    VAR OUT posOfPattern : CARDINAL);

PROCEDURE FindDiffI(stringVal1, stringVal2 : ARRAY OF CHAR;
                    VAR OUT differenceFound : BOOLEAN;
                    VAR OUT posOfDifference : CARDINAL);

(* Misc other usefull stuff *)
(* follows ISO parameter convensions *)

PROCEDURE AssignNullTerm(source : ARRAY OF CHAR; VAR OUT destination : ARRAY OF CHAR);
(* copies source to destination.
   the destination will always be null terminated.
*)

PROCEDURE NullTerminate(VAR str : ARRAY OF CHAR);
(* this will make sure the string is null terminated by placing *)
(* a null character in the high bound of the string *)
(* harmless if the string is already null terminated, and will truncate *)
(* the last character if it is not null terminated *)

PROCEDURE Lowercase(VAR INOUT stringVar : ARRAY OF CHAR);
(* the reverse of Strings.Capitalize *)

PROCEDURE AnsiToUnicode(strA : ARRAY OF ACHAR; VAR OUT strU : ARRAY OF UCHAR);
(* convert an Ansi(8-bit) string to a Unicode string *)

PROCEDURE UnicodeToAnsi(strU : ARRAY OF UCHAR;
                        VAR OUT strA : ARRAY OF ACHAR;
                        replaceChar : ACHAR);
(* convert an Unicode string to a Ansi(8-bit) string
   some Unicode characters may be "lost" and replaced with the
   replaceChar character.
   for example converting Kanji to Ansi will not work.
*)

PROCEDURE Utf8Length(strU : ARRAY OF UCHAR) : CARDINAL;
(*
  returns the number of character ACHAR characters needed to translate the UCHAR
  unicode string to UTF-8 format.
  the length returned does not include a null terminator character byte.
*)

PROCEDURE LengthUtf8(strA : ARRAY OF ACHAR) : CARDINAL;
(*
   return the length in characters, not bytes, of a UTF-8 sequence.
   Using LENGTH or Strings.Length will return the number of byte characters.
*)

PROCEDURE IsValidUtf8(strA : ARRAY OF ACHAR) : BOOLEAN;
(*
  returns true if the character sequence is a valid UTF-8 sequence.

  what this procedure is checking for are "over long" character encodings and
  truncated characters at the end of a string.
  UTF-8 encoding allows only the shortest possible encoding to exist in a
  valid UTF-8 character string.
*)

PROCEDURE UnicodeToUtf8(strU : ARRAY OF UCHAR; VAR OUT strA : ARRAY OF ACHAR);
(*
  convert a two byte Unicode character to a multibyte UTF-8 sequence.
  the ACHAR character string will be 1-3x the number of UCHAR characters.
  so a 3x ACHAR string will *always* be able to accept a conversion of
  a UCHAR character string.
*)

PROCEDURE Utf8ToUnicode(strA : ARRAY OF ACHAR;
                        VAR OUT strU : ARRAY OF UCHAR;
                        replaceChar : UCHAR);
(*
  convert a multibyte UTF-8 sequence to a two byte unicode character array.
  if the UTF-8 sequence contains a character that cannot be contained within
  a UCHAR unicode character (> 65535), then replaceChar will be inserted into
  the destination string for the unsupported character.
*)

PROCEDURE Utf8ToAnsi(utf8 : ARRAY OF ACHAR;
                     VAR OUT strA : ARRAY OF ACHAR;
                     replaceChar : ACHAR);
(*
  convert a UTF-8 sequence to an ACHAR character sequence.
  for characters > MAX(ACHAR) then replaceChar is inserted for the
  out of range character.

  you can pass the same string to the utf8 and strA parameters.
*)

PROCEDURE FindAndReplace(find, replace : ARRAY OF CHAR;
                         VAR INOUT str : ARRAY OF CHAR) : BOOLEAN;
(*
  find the string find in the string str and if found then replace
  the string find in str with replace.
  the string comparison is case sensitive.
  returns TRUE if find was found and a replace occurred, otherwise false.
*)

PROCEDURE FindAndReplaceI(find, replace : ARRAY OF CHAR;
                          VAR INOUT str : ARRAY OF CHAR) : BOOLEAN;
(*
  find the string find in the string str and if found then replace
  the string find in str with replace.
  the string comparison is NOT case sensitive.
  returns TRUE if find was found and a replace occurred, otherwise false.
*)

PROCEDURE AppendWithLengths(source : ARRAY OF CHAR;
                            srcLen : CARDINAL;
                            VAR INOUT destination : ARRAY OF CHAR;
                            destLen : CARDINAL) : CARDINAL;
(* source is appended to destination *)
(* this is a very specialied Append procedure that can be used *)
(* where you are appending many different strings to a single string *)
(* this function will be faster than using Append since it is your job *)
(* to keep track of the string lengths *)
(* the result is the length of the result which is placed in destination *)

PROCEDURE AppendChar(ch : CHAR; VAR INOUT str : ARRAY OF CHAR);
(* appends the character, ch, to the end of the string str *)
(* if there is no space left in the string nothing happens *)

PROCEDURE AppendCharCond(ch : CHAR; VAR INOUT str : ARRAY OF CHAR);
(* same as AppendChar, except it will not append the character *)
(* if the character already exists at the end of the string *)

PROCEDURE AppendNum(num : CARDINAL; VAR INOUT str : ARRAY OF CHAR);
(* append a decimal number to the end of the string *)
(* the number may be truncated if not enough space exists in the string *)

PROCEDURE AppendHex(num, digits : CARDINAL; VAR INOUT str : ARRAY OF CHAR);
(* append a hex number with at most digits characters *)
(* to the end of the string *)
(* the number may be truncated if not enough space exists in the string *)

(* the following procedure are used for parsing a string *)
(* with a set of characters that are separators of individual items *)
(* within the main string *)
(* individual items should not be greater than 128 characters *)

PROCEDURE GetNextItem(list : ARRAY OF CHAR;
                      VAR INOUT i : CARDINAL;
                      VAR OUT item : ARRAY OF CHAR;
                      sep : ARRAY OF CHAR) : BOOLEAN;
(* list = the main string contains the various items *)
(* i = the index position to start scanning list for next item *)
(*     upon return i will have been advanced beyond the item returned *)
(* item = the item extracted from the list *)
(* sep = characters that are considered as separators, *)
(* separating the individual items within the list *)
(* example of use *)
(*
    i := 0;
    WHILE GetNextItem(list, i, item, ";,") DO
        (* do something *)
    END;
*)

PROCEDURE InList(item, list : ARRAY OF CHAR; sep : ARRAY OF CHAR) : BOOLEAN;
(* item = the item to scan for *)
(* list = the list if items *)
(* sep = a string contains characters that are considered as separators *)
(* separating the items within the list *)
(* returns TRUE is item is in list *)

PROCEDURE AddItem(item : ARRAY OF CHAR;
                  VAR INOUT list : ARRAY OF CHAR;
                  ch : CHAR);
(* add the item to the list using the character ch *)
(* as the separator character *)

PROCEDURE RemoveItem(item : ARRAY OF CHAR;
                     VAR INOUT list : ARRAY OF CHAR;
                     sep : ARRAY OF CHAR);
(* removes an item from the list if it exists in the list *)
(* item = the item to remove *)
(* list = the list if items *)
(* sep = a string contains characters that are considered as separators *)
(* separating the items within the list *)

END ExStrings.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1992-2002                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
(*
    This module is a superset of the ISO defined Storage module.
    The ISO Storage module is implemented with calls to this module.
    ISO does not allow Storage to be extended, hence the existence of
    the module.

    The module maintains a free list of blocks that are available for
    allocation. When there is not a big enough block to satisfy the
    allocation request, the heap then goes to its source of memory,
    usually the operating system, for a new block of memory. It would be silly
    to request 50 bytes from the source so the heap allocates chunks from
    the source and then splits these chunks down as allocation requests are made.

    To minimize waste of memory (fragmentation) you may want to change the chunk size
    to a multiple of some size.
    The default chunk size is 64k. (32k for 16-bit DOS)
    The heap manager does have some overhead per block of memory allocated.
    You must take this into account when you set multiples of block sizes.
    This overhead size is BlockOverhead, a value exported from this module.

    A heap has two options to handle allocations larger than the chunk size.
    1. Allocate the memory and add it to the internal heap memory list.
    2. Allocate memory directly from the memory source, and deallocate the
            memory directly back to the memory source on deallocation.

    See the SetChunkSize APIs for further information.


    -----

    Multiple heaps.
    This module allows multiple heaps. A heap is a list of memory blocks
    that are available for allocation. You can think of each heap as a
    "different" storage module.
    All operations in the storage module operate on the current heap, or
    the "Ex" operations operate on the heap parameter passed.

    To create a new heap use AllocHeap. You can then use this heap
    handle with the various "Ex" APIs or use it with UseHeap or PushHeap
    to set the heap as the current heap.

    This module always initializes a default heap. So you need not
    bother with this if you do not need multiple heaps.
*)
DEFINITION MODULE ExStorage;

<*/NOPACK*>

<*/VALIDVERSION:ALIGN8*>
%IF %NOT Bits16 %THEN
    %IF %NOT IA32 %THEN
        <*/VERSION:ALIGN8*>
    %END
%END

FROM SYSTEM IMPORT
    ADDRESS, CAST;

TYPE
    StorageExceptions   =
                          (
                           (* ISO defined exceptions *)
                           nilDeallocation,
                           pointerToUnallocatedStorage,
                           wrongStorageToUnallocate,

                           (* Stony Brook extended exceptions *)

                           outOfStorage,
                           tooManyFreeBlocks,
                           deallocateToWrongHeap,
                           memoryOverwrite,
                           heapStackOverflow,
                           heapStackUnderflow,
                           heapCorrupt
                          );

    AllocStrategy       = (FirstFit, BestFit);
    CombineStrategy     = (NormalCombine, FullCombine, NoCombine);
    SplitStrategy       = (SplitTopDown, SplitBottomUp);

    HeapErrorCodes      = (ReturnNIL, DoException, TryAgain);
    HeapErrorProc       = PROCEDURE((*amount:*)CARDINAL) : HeapErrorCodes;

    MemorySourceAlloc   = PROCEDURE((*amount:*)CARDINAL,
                                    (*userData:*)ADDRESS) : ADDRESS(*memory addr*);
    MemorySourceDealloc = PROCEDURE((*addr:*)ADDRESS,
                                    (*amount:*)CARDINAL,
                                    (*userData:*)ADDRESS);
    MemorySourceAvail   = PROCEDURE((*userData:*)ADDRESS) : CARDINAL32;

CONST
%IF ALIGN8 %THEN
    BlockOverhead       = 8;
%ELSE
    BlockOverhead       = SIZE(CARDINAL);
%END
(* BlockOverhead is also the alignment which all allocated memory returns *)

    HeapErrorNIL        = CAST(HeapErrorProc, 1);
    HeapErrorException  = CAST(HeapErrorProc, 2);
    (*
      you can use these to set the heap error handler to an
      internal procedure that simply returns the specificed result code.
    *)

TYPE
    HeapInfoPointer;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

PROCEDURE ALLOCATE(VAR OUT addr : ADDRESS; amount : CARDINAL);
PROCEDURE AllocateEx(VAR OUT addr : ADDRESS;
                     amount : CARDINAL;
                     heap : HeapInfoPointer);
(*
 allocate memory from the current heap
 addr = the address of the memory block to deallocate
 amount = the size of the memory block in bytes
 on return addr will contain the address of the memory block or
 a value of NIL if there was not enough memory available to
 satisfy the allocation request. This behaviour can be modified by
 installing a new HeapErrorProc.
 This call can fail if SetHeapMax or FreezeHeap are used to limit the
 size of the heap and no block large enough exists in the heap for
 the allocation.
 HeapErrorProc is always called when not enough memory is available
 to satisfy the request. The HeapErrorProc can return one of three
 possible actions for the heap manager to take.
 HeapErrorCodes       = (ReturnNIL, DoException, TryAgain);
 ReturnNIL does just that. This is the default.
 DoException will have ALLOCATE raise an outOfStorage exception
 TryAgain will have the system try to allocate the requested amount again
*)

PROCEDURE DEALLOCATE(VAR INOUT addr : ADDRESS; amount : CARDINAL);
PROCEDURE DeallocateEx(VAR INOUT addr : ADDRESS;
                       amount : CARDINAL;
                       heap : HeapInfoPointer);
(*
 deallocate memory to the current heap
 addr = the address of the memory block to deallocate
 amount = the size of the memory block in bytes
 On return of this procedure addr will contain the value NIL.
 If NIL is passed to addr then a nilDeallocation exception is raised
 If the heap is in debug mode the following actions are also performed.
 The size of the decallocation is checked to make sure it agrees with
 the size the block was allocated. A wrongStorageToUnallocate exception
 is raised if the size is not correct
 If the memory is not allocated memory, such as deallocating twice,
 a pointerToUnallocatedStorage exception is raised.
 A check for memory overwrite is done and a memoryOverwrite exception is
 raised if one is found.
 A check is made to make sure the memory block was allocated from the
 current heap. A deallocateToWrongHeap exception is raised if the memory
 did not come from the current heap.
*)

PROCEDURE IsStorageException() : BOOLEAN;
(* Return TRUE if the current exception came from this module *)

PROCEDURE StorageException() : StorageExceptions;
(*
 If IsStorageException() = TRUE, then this function returns the
 specific exception that was raised
*)

PROCEDURE ReALLOCATE(VAR INOUT addr : ADDRESS; amount : CARDINAL);
PROCEDURE ReallocateEx(VAR INOUT addr : ADDRESS;
                       amount : CARDINAL;
                       heap : HeapInfoPointer);
(*
 a convienence call
 allocates a memory block of size amount,
 copies the old memory block to the new memory block
 and then deallocates the old memory block.
 addr can be NIL for this call. in this case it simply
 acts like a call to allocate.
*)

PROCEDURE AllocateAligned(VAR OUT addr, alignedAddr : ADDRESS;
                          amount, align : CARDINAL);
PROCEDURE AllocateAlignedEx(VAR OUT addr, alignedAddr : ADDRESS;
                            amount, align : CARDINAL;
                            heap : HeapInfoPointer);
(*
 as ALLOCATE except...
 allocate a memory block and return an address aligned on an even
 multiple address of amount align
 addr = the returned memory block allocated. use this address when
          you deallocate the memory. also use amount+align for the
          amount of memory to deallocate
 alignedAddr = the aligned memory address. use this address for your
                 pointer.
 amount = the amount of memory to allocate.
 align = the alignment multiple.
*)

PROCEDURE Available(amount : CARDINAL) : BOOLEAN;
PROCEDURE AvailableEx(amount : CARDINAL; heap : HeapInfoPointer) : BOOLEAN;
(*
 can a block of the specified size be allocated from the current heap
 TRUE is returned if the amount is available.
 This call can fail if SetHeapMax or FreezeHeap are used to limit the
 size of the heap and no block large enough exists in the heap for
 the allocation.
*)

PROCEDURE MaxAvailable() : CARDINAL;
PROCEDURE MaxAvailableEx(heap : HeapInfoPointer) : CARDINAL;
(*
 largest single continuous block available to be allocated.
 if the heap is not frozen the memory source will be checked for the
 largest block available.
 SetHeapMax can also limit this value.
*)

PROCEDURE MemAvailable() : CARDINAL32;
PROCEDURE MemAvailableEx(heap : HeapInfoPointer) : CARDINAL32;
(*
 total memory in the heap free list, and available from the memory source,
 if the heap has not been frozen. SetHeapMax can also limit this value.
*)

PROCEDURE MemoryInUse() : CARDINAL32;
PROCEDURE MemoryInUseEx(heap : HeapInfoPointer) : CARDINAL32;
(*
 amount of memory currently allocated in the heap
 this value includes any overhead needed by the heap manager

 IF (heap = NIL) THEN
     the value returned is the sum of MemoryInUse for all
     currently active heaps.
*)

PROCEDURE MaxMemoryUsed() : CARDINAL32;
PROCEDURE MaxMemoryUsedEx(heap : HeapInfoPointer) : CARDINAL32;
(*
 the largest amount of memory ever allocated in the heap
 this value includes any overhead needed by the heap manager.

 IF (heap = NIL) THEN
     the value returned is the sum of MaxMemoryUsed for all
     currently active heaps.
*)

PROCEDURE ResetMaxMemoryUsed;
PROCEDURE ResetMaxMemoryUsedEx(heap : HeapInfoPointer);
(*
  resets the internal counter for the maximum amount of memory used
  to zero.

 IF (heap = NIL) THEN
     all currently active heaps will have their counter reset.
*)

PROCEDURE HeapMemory() : CARDINAL32;
PROCEDURE HeapMemoryEx(heap : HeapInfoPointer) : CARDINAL32;
(*
 amount of memory the heap currently owns
 this takes into account heap granularity and overhead.

 IF (heap = NIL) THEN
     the value returned is the sum of HeapMemory for all
     currently active heaps.
*)

PROCEDURE FreezeHeap(yes : BOOLEAN);
PROCEDURE FreezeHeapEx(yes : BOOLEAN; heap : HeapInfoPointer);
(*
 freeze the current heap to it present size.
 no more allocations will be made from the memory source.
*)

PROCEDURE CombineHeap;
PROCEDURE CombineHeapEx(heap : HeapInfoPointer);
(*
 make a pass through the entire free list attempting to combine
 all blocks that can be combined
*)

PROCEDURE FreeHeap;
PROCEDURE FreeHeapEx(heap : HeapInfoPointer);
(* return all memory blocks back to the memory source. *)

PROCEDURE ClearHeap;
PROCEDURE ClearHeapEx(heap : HeapInfoPointer);
(*
 an instant DEALLOCATE everything that is currently allocated in the heap
 while not returning memory to the memory source.
*)

PROCEDURE SetMemorySource(alloc : MemorySourceAlloc;
                          dealloc : MemorySourceDealloc;
                          avail : MemorySourceAvail;
                          userData : ADDRESS);
PROCEDURE SetMemorySourceEx(alloc : MemorySourceAlloc;
                            dealloc : MemorySourceDealloc;
                            avail : MemorySourceAvail;
                            userData : ADDRESS;
                            heap : HeapInfoPointer);
(*
 a heaps memory source defaults to the operating system.
 with this function you can redirect these allocations to another source of memory.

 userData = arbitrary data which will be passed to your callback procedures.
 for MemorySourceAlloc, NIL should be returned to indicate failure.
*)

PROCEDURE SetHeapError(err : HeapErrorProc);
PROCEDURE SetHeapErrorEx(err : HeapErrorProc; heap : HeapInfoPointer);
(*
 The HeapErrorProc is called when ALLOCATE cannot satisfy the allocation
 request. This allows you to perform some action(s) if possible and gain
 the memory to satisfy the ALLOCATE request
 The HeapErrorProc returns a value to indicate what the heap manager
 should do
 HeapErrorCodes = (ReturnNIL, DoException, TryAgain);
 ReturnNIL does just that
 DoException will have ALLOCATE raise an outOfStorage exception
 TryAgain will have the system try to allocate the requested amount again
*)

PROCEDURE GetHeapError() : HeapErrorProc;
PROCEDURE GetHeapErrorEx(heap : HeapInfoPointer) : HeapErrorProc;
(* return the current heap error procedure *)

PROCEDURE GetHeapMax() : CARDINAL32;
PROCEDURE GetHeapMaxEx(heap : HeapInfoPointer) : CARDINAL32;
(* get maximum amount of memory a heap can contain *)

PROCEDURE SetHeapMax(max : CARDINAL32);
PROCEDURE SetHeapMaxEx(max : CARDINAL32; heap : HeapInfoPointer);
(* set maximum amount of memory a heap can contain *)

PROCEDURE GetChunkSize() : CARDINAL;
PROCEDURE GetChunkSizeEx(heap : HeapInfoPointer) : CARDINAL;
(*
 get the block size allocated from the source of memory for the heap
 when the heap needs more memory to satisfy an ALLOCATE request
*)

PROCEDURE SetChunkSize(max : CARDINAL; direct : BOOLEAN);
PROCEDURE SetChunkSizeEx(max : CARDINAL; direct : BOOLEAN; heap : HeapInfoPointer);
(*
 set the block size allocated from the source of memory for the heap
 when the heap needs more memory to satisfy an ALLOCATE request.
 if direct = FALSE then all allocation requests will become a part of the memory
     freelist managed by the heap. the memory will remain a part of the heap
     until the heap is freed.
 if direct = TRUE then allocation requests larger than the chunk size will
     be allocated directly from the memory source, and when deallocated it will
     be deallocated back to the memory source. the memory allocation will never
     be placed into the heap memory freelist. this can help reduce memory fragmentation
     but direct allocations are slower than allocations from the
     internal memory freelist.
*)

PROCEDURE GetMinAllocSize() : CARDINAL;
PROCEDURE GetMinAllocSizeEx(heap : HeapInfoPointer) : CARDINAL;
(* get the minimum block size that can be allocated *)

PROCEDURE SetMinAllocSize(min : CARDINAL);
PROCEDURE SetMinAllocSizeEx(min : CARDINAL; heap : HeapInfoPointer);
(*
 set the minimum block size that can be allocated
 usefull when using SetCombine(NoCombine) to get a uniform block size
 without playing with your data structure definitions and/or the amount
 value passed to ALLOCATE.
*)

PROCEDURE GetDebug() : BOOLEAN;
PROCEDURE GetDebugEx(heap : HeapInfoPointer) : BOOLEAN;
(* get the heap debug state *)

PROCEDURE SetDebug(debug : BOOLEAN);
PROCEDURE SetDebugEx(debug : BOOLEAN; heap : HeapInfoPointer);
(*
 debug = FALSE is the default
 it debug = TRUE then the heap is checked for consistency and
 fills all blocks with NIL before allocation and deallocation
 it can also detect many memory overwrite situations.
 it can also help detect memory leaks.
 Setting the debug mode can only be done when the heap is empty,
 otherwise this call has no effect.
 You can use the SetExStorageDebugMode module to easily set the debug mode
 to TRUE for the default heap.
 Just import the module as the first module imported in your main program.
 This will set the debug mode for the default heap. This works even if the
 initialization code in one or more modules allocates memory because it would be
 the first module initialization code called that uses the ExStorage module.
*)

PROCEDURE ScanForMemoryOverwrites;
PROCEDURE ScanForMemoryOverwritesEx(heap : HeapInfoPointer);
(*
 If SetDebug(TRUE) then you can call this procedure to scan all blocks
 deallocated and allocated to look for memory overwrites
 If one is found then a memoryOverwrite exception will be raised
 IF SetDebug(FALSE) then this call returns immediately without performing
 any checks
*)

PROCEDURE ScanForMemoryOverwritesOnApiCalls(yes : BOOLEAN);
PROCEDURE ScanForMemoryOverwritesOnApiCallsEx(yes : BOOLEAN;
                                              heap : HeapInfoPointer);
(*
 if SetDebug(TRUE) and (yes = TRUE) then all calls to
 ALLOCATE, DEALLOCATE, Available
 will first scan for memory overwrites before performing their
 normal action(s)
 if SetDebug(TRUE) and (yes = FALSE) then memory overwrites are only
 checked in the DEALLOCATE procedure and then only the deallocated block
 is checked.
*)

TYPE
    CheckLeakCallbackProc = PROCEDURE((*allocId*)CARDINAL32, (*size*) CARDINAL);
    StopLeakCallbackProc = PROCEDURE(VAR INOUT (*allocId*)CARDINAL32);
     (*allocId = a unique number identifying a specific allocation *)

PROCEDURE CheckForMemoryLeaks(callback : CheckLeakCallbackProc);
PROCEDURE CheckForMemoryLeaksEx(callback : CheckLeakCallbackProc; heap : HeapInfoPointer);
(*
 if SetDebug(TRUE) then call this function when you believe you have
 deallocated all memory in your application. The callback procedure
 will be called for each memory block that has not been deallocated
*)

PROCEDURE SetMemoryStopLeakCallback(callback : StopLeakCallbackProc;
                                    allocId : CARDINAL32);
PROCEDURE SetMemoryStopLeakCallbackEx(callback : StopLeakCallbackProc;
                                      allocId : CARDINAL32;
                                      heap : HeapInfoPointer);
(*
    if SetDebug(TRUE) then
    once you have detected one or more memory leaks via CheckForMemoryLeaks,
    and you can precisely repeat the execution of your program,
    you can use this call to have a procedure of yours called when the memory
    block with the given allocId is allocated.
    You can set a breakpoint in this procedure to stop when one of your leaked
    memory blocks is allocated. You can now debug your code to determine why the
    memory allocation leaked.
    You can assign a value to allocId to look for another memory leak. The allocId
    you assign should be higher than the current one.
*)

PROCEDURE GetCombine() : CombineStrategy;
PROCEDURE GetCombineEx(heap : HeapInfoPointer) : CombineStrategy;
(* get the heap combine state *)

PROCEDURE SetCombine(comb : CombineStrategy);
PROCEDURE SetCombineEx(comb : CombineStrategy; heap : HeapInfoPointer);
(*
 NormalCombine = the current heap block will be fully combined where
                 where possible before checking its suitability for
                 allocation.
                 This gives best performance, while still reducing
                 the fragmentation of heap memory
                 This is the default value
 FullCombine   = All the free blocks in the heap will be combined where
                 possible before doing any allocation
                 If you are having problems with heap fragmentation with
                 NormalCombine, then this option will help.
                 How much help cannot be determined.
                 This will slow down allocations, more so in FirstFit
                 than in BestFit strategy
 NoCombine     = No heap block combining will be done. This will slow
                 down heap allocation unless your blocks are all the same
                 size. See SetMinAllocSize for more info
*)

PROCEDURE GetStrategy() : AllocStrategy;
PROCEDURE GetStrategyEx(heap : HeapInfoPointer) : AllocStrategy;
(* get the current allocation strategy *)

PROCEDURE SetStrategy(s : AllocStrategy);
PROCEDURE SetStrategyEx(s : AllocStrategy; heap : HeapInfoPointer);
(*
 there are two allocation strategies available
 FirstFit = default and fastest by far, the first block that is greater
            than or equal to the requested allocation will be used
            This is the default value
 BestFit =  the block that best fits the requested allocation will
            be used. The entire free list will be searched until an
            exact match is found. If not found then the next block size
            >= the requested size will be used
*)

PROCEDURE SetSplitStrategy(split : SplitStrategy);
PROCEDURE SetSplitStrategyEx(split : SplitStrategy;
                             heap : HeapInfoPointer);
(*
 there are two block splitting strategies
 SplitTopDown = the default and the fastest mechanism.
 depending on how data structures such as lists are created
 a one of the strategies may cause the structures to be arranged such
 that processor memory cache is used much more efficiently and therefore
 increase execution performance.
 for best effect you will generally have to use multiple heaps, one
 per structure, to force linear adjacent memory allocation.
*)

PROCEDURE SetThreadSafe(yes : BOOLEAN);
PROCEDURE SetThreadSafeEx(yes : BOOLEAN; heap : HeapInfoPointer);
(*
  if yes = TRUE then the heap is thread safe. meaning multiple threads
  can allocate from the heap and not corrupt the heap.
  being thread safe incurs a small overhead.

  single threaded code can safely turn off thread protection.

  multi-threaded code can turn off thread safety if the application controls
  heap access such that only a single thread is allowed to access the heap
  at any point in time.

  thread safe, yes = TRUE, is the default when a heap is initialized.
*)

PROCEDURE AllocHeap() : HeapInfoPointer;
PROCEDURE AllocHeapEx(defaultError : HeapErrorCodes) : HeapInfoPointer;
(*
 returns a handle to a heap if successfull. otherwise returns NIL.
 defaultError can be ReturnNIL or DoException
*)

PROCEDURE DeallocHeap(heap : HeapInfoPointer);
(* releases a previously allocated heap.
   all heap memory is released (FreeHeap) before the heap is deallocated.
*)

PROCEDURE GetHeap() : HeapInfoPointer;
(* return the current heap pointer *)

PROCEDURE GetDefaultHeap() : HeapInfoPointer;
(* return the default heap pointer *)

PROCEDURE UseHeap(heap : HeapInfoPointer) : HeapInfoPointer;
(* set the current heap, and return the previous heap *)

PROCEDURE PushHeap(heap : HeapInfoPointer);
(*
 push the previous heap onto an internal stack and set the passed heap
 as the current heap.
 other threads will be blocked from AllocHeap, DeallocHeap,
 UseHeap, PushHeap, PopHeap, ClearHeapStack until PopHeap is called.
*)

PROCEDURE PopHeap;
(* set the current heap to the last heap pushed onto the stack *)

PROCEDURE ClearHeapStack;
(*
 clear all heaps off of the internal stack
 can be usefull in exception handlers to do some clean up
*)

PROCEDURE LockHeap(heap : HeapInfoPointer);
(*
 this procedure "locks" the heap such that only the current thread
 can access the heap.
 this may be called recursively. you must call Unlock once for each
 call to lock.
*)

PROCEDURE UnlockHeap(heap : HeapInfoPointer);
(* this call reverses a previous LockHeap call. *)

PROCEDURE AllocSystemMemory(VAR OUT addr : ADDRESS; amount : CARDINAL);
PROCEDURE DeallocSystemMemory(VAR INOUT addr : ADDRESS; amount : CARDINAL);
(*
  this calls are not an integral part of this modules functions.
  they are simply here to provide a convenient and portable way
  of directly allocating operating system memory.

  for alloc, addr = NIL if the call fails.
*)

END ExStorage.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1987-2002                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE Environment;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

PROCEDURE GetSymbol(name : ARRAY OF CHAR;
                    VAR OUT result : ARRAY OF CHAR) : BOOLEAN;
(* Get the value of an enviroment symbol.  name is the name of the symbol
   to get, result is the returned definition, function value is TRUE if the
   symbol was found.
   name should not be longer than 128 characters.
   returns FALSE if the symbol is not found and the value in result will
   be unchanged.
   returns TRUE if the symbol was found and the entire contents of the
   symbol fit in the output variable result.
   if the symbol was found and result is not large enough FALSE is returned
   and the value in result will be the truncated value of the symbol.
*)

PROCEDURE GetCommandLine(VAR OUT commandLine : ARRAY OF CHAR);
(* Get the command line with which the program was run. *)

PROCEDURE GetProgramName(VAR OUT name : ARRAY OF CHAR);
(* Get the name of the currently running program.
   under DOS, DOS extended, Win16, Win32 this will be a fully qualified
   path and name of the running executable program.
*)
(* On Linux 2.2 and later kernels a fully qualified path is returned. *)
(* On other Unix systems this value does not have any real meaning
   other than the name of the program executed.
*)

END Environment.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1989-2002                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

DEFINITION MODULE Conversions;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

PROCEDURE StringToInt(str : ARRAY OF CHAR;
                      VAR INOUT pos : CARDINAL;
                      VAR OUT num : INTEGER;
                      VAR OUT done : BOOLEAN);
(* Get an integer number from a string starting at position pos. *)
(* pos is left pointing at the first character that is not part of the *)
(* number. done signifies the success of the conversion *)
(* Skips any leading spaces. *)

PROCEDURE StrToInt(buf : ARRAY OF CHAR; VAR OUT num : INTEGER) : BOOLEAN;
(* Convert an integer number from a string  *)
(* Skips any leading spaces. *)

PROCEDURE IntToString(num : INTEGER;
                      size : CARDINAL;
                      VAR OUT str : ARRAY OF CHAR;
                      VAR INOUT pos : CARDINAL;
                      VAR OUT done : BOOLEAN);
(* Convert an integer number into a string starting at position *)
(* pos.  pos is left pointing to the character following the number *)
(* done signifies the success of the conversion *)

PROCEDURE IntToStr(num : INTEGER; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* Convert an integer number to a string  *)

PROCEDURE StringToCard(str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT num : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Get a cardinal number from a string starting at position pos. *)
(* pos is left pointing at the first character that is not part of the *)
(* number *)
(* done signifies the success of the conversion *)
(* Skips any leading spaces. *)

PROCEDURE StrToCard(buf : ARRAY OF CHAR; VAR OUT num : CARDINAL) : BOOLEAN;
(* Convert a string in buf to a cardinal *)
(* Skips any leading spaces. *)

PROCEDURE CardToString(num : CARDINAL;
                       size : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Convert a cardinal number into the string starting at position *)
(* pos.  pos is left pointing to the character following the number *)
(* done signifies the success of the conversion *)

PROCEDURE CardToStr(num : CARDINAL; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* Convert a cardinal to a string *)

PROCEDURE StrBaseToCard(str : ARRAY OF CHAR;
                        base : CARDINAL;
                        VAR OUT num : CARDINAL32) : BOOLEAN;
(* convert a string to a cardinal32 using the number base specified in base *)
(* (base >= 2) AND (base <= 16) *)
(* Skips any leading spaces. *)

PROCEDURE CardBaseToStr(num : CARDINAL32;
                        base : CARDINAL;
                        VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* convert a cardinal32 to a string using the number base specified in base *)
(* (base >= 2) AND (base <= 16) *)

PROCEDURE StringToLong(str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT num : LONGINT;
                       VAR OUT done : BOOLEAN);
(* Get an longint number from a string buf starting at position pos. *)
(* pos is left pointing at the first character that is not part of the *)
(* number *)
(* done signifies the success of the conversion *)
(* Skips any leading spaces. *)

PROCEDURE StrToLong(buf : ARRAY OF CHAR; VAR OUT num : LONGINT) : BOOLEAN;
(* Convert a longint number from a string  *)
(* Skips any leading spaces. *)

PROCEDURE LongToString(num : LONGINT;
                       size : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT pos : CARDINAL;
                       VAR OUT done : BOOLEAN);
(* Convert a longint number into the string starting at position *)
(* pos.  pos is left pointing to the character following the number *)
(* done signifies the success of the conversion *)

PROCEDURE LongToStr(num : LONGINT; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* Convert a longint to a string *)

PROCEDURE StrBaseToLong(str : ARRAY OF CHAR;
                        base : CARDINAL;
                        VAR OUT num : LONGCARD) : BOOLEAN;
(* convert a string to a longcard using the number base specified in base *)
(* (base >= 2) AND (base <= 16) *)
(* Skips any leading spaces. *)

PROCEDURE LongBaseToStr(num : LONGCARD;
                        base : CARDINAL;
                        VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
(* convert a longcard to a string using the number base specified in base *)
(* (base >= 2) AND (base <= 16) *)

END Conversions.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1998-2002                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE BitVectors;
(* bit vectors are similar to Modula-2 SETs, however they are dynamically *)
(* sized *)

<*/VALIDVERSION:PROTECT*>
%IF WIN32 %OR UNIX %THEN
<*/VERSION:PROTECT*>
%END
(* make sure this version tag is set the same in the DEF and MOD *)

FROM ExStorage IMPORT
    HeapInfoPointer;

CONST
    BitVectorBits       = SIZE(CARDINAL)*8;

TYPE
    BitVectorElement    = PACKEDSET OF [0..BitVectorBits-1];

    (* this is defined in the DEF only to support the inline macro procedures
       FastSetBit and FastClearBit.
       your should ignore this and use the provided APIs.
    *)
    BitVectorRecord =
        RECORD
        %IF PROTECT %THEN
        lock            : CARDINAL;
        pad             : ARRAY [1..32] OF CARDINAL8;
        (* separate the lock and the data by a cache line *)
        %END
        lowBit          : CARDINAL;
        highBit         : CARDINAL;
        numElements     : CARDINAL;
        heap            : HeapInfoPointer;
        bits            : ARRAY [0..0] OF BitVectorElement;
        (* the above array is allocated to its appropriate size *)
        (* depending on the range of the BitVector *)
        END;
    BitVector           = POINTER TO BitVectorRecord;

CONST
    EmptyElement        = BitVectorElement{};
    FullElement         = BitVectorElement{0..BitVectorBits-1};

PROCEDURE NewVector(VAR OUT v : BitVector; lowBit, highBit : CARDINAL);
(* allocate a new bit vector with a lower bound of lowBit and upper bound *)
(* of highBit. *)
(* v = NIL if the operation does not succeed *)
(* a bit outside the allocated range of a bit vector is considered clear, *)
(* meaning the bit is not set, and obviously never can be set *)
(* the bits in the new vector default to clear *)

PROCEDURE DisposeVector(VAR INOUT v : BitVector);
(* deallocate a previously allocated bit vector *)
(* v = NIL after this procedure *)

PROCEDURE ReallocVector(VAR INOUT v : BitVector; newHighBit : CARDINAL);
(* change the upper bound of the previously allocated bit vector v *)
(* to the new bound in newHighBit. the lower bound is unchanged *)
(* the value v will change as a result of this call *)
(* if the high bound is increased then the new bits will default to clear *)

PROCEDURE SetBit(b : CARDINAL; v : BitVector);
(* set the bit value in b in the bit vector v *)
(* if b is outside the valid range of v it is ignored and this procedure *)
(* performs no action *)

PROCEDURE ClearBit(b : CARDINAL; v : BitVector);
(* clear the bit value in b in the bit vector v *)
(* if b is outside the valid range of v it is ignored and this procedure *)
(* performs no action *)

PROCEDURE FastSetBit(b : CARDINAL; v : BitVector); MACRO;
(* as SetBit, except this gets generated inline.
   it is not thread safe and it does not range check the set bit.
*)
BEGIN
    b := b - v^.lowBit;
    INCL(v^.bits[b / BitVectorBits], b REM BitVectorBits);
END FastSetBit;

PROCEDURE FastClearBit(b : CARDINAL; v : BitVector); MACRO;
(* as ClearBit, except this gets generated inline.
   it is not thread safe and it does not range check the set bit.
*)
BEGIN
    b := b - v^.lowBit;
    EXCL(v^.bits[b / BitVectorBits], b REM BitVectorBits);
END FastClearBit;

PROCEDURE SetBits(bottom, top : CARDINAL; v : BitVector);
(* set the range of bits from bottom..top in the bit vector v *)
(* if any bit is outside the valid range of v it is ignored *)
(* any bit within the valid range of v will be set *)

PROCEDURE ClearBits(bottom, top : CARDINAL; v : BitVector);
(* clear the range of bits from bottom..top in the bit vector v *)
(* if any bit is outside the valid range of v it is ignored *)
(* any bit within the valid range of v will be cleared *)

PROCEDURE BitIsSet(b : CARDINAL; v : BitVector) : BOOLEAN;
(* returns TRUE if the bit value b is set in the bit vector v, *)
(* otherwise FALSE is returned *)
(* if the bit value b is outside the range of v then FALSE is returned *)

PROCEDURE AnyBitSet(bottom, top : CARDINAL; v : BitVector) : BOOLEAN;
(* returns TRUE if any bit in the range bottom..top is set *)
(* in the bit vector v, otherwise FALSE *)

PROCEDURE AnyBitClear(bottom, top : CARDINAL; v : BitVector) : BOOLEAN;
(* returns TRUE if any bit in the range bottom..top is clear *)
(* in the bit vector v, otherwise FALSE *)
(* if any bit from bottom..top is outside the range of v *)
(* then TRUE is returned since these bits are defined as always clear *)

PROCEDURE FindSetBits(startBit, numBits : CARDINAL;
                      v : BitVector;
                      VAR OUT bottom : CARDINAL) : BOOLEAN;
(* this scans the BitVector for a group of set bits *)
(* the search begins at the bit specified by startBit. *)
(* returns TRUE if a group of bits was found and then bottom contains *)
(* the first bit position in the found group. *)

PROCEDURE FindClearBits(startBit, numBits : CARDINAL;
                        v : BitVector;
                        VAR OUT bottom : CARDINAL) : BOOLEAN;
(* as FindSetBits, except looks for clear bits *)

PROCEDURE Empty(v : BitVector) : BOOLEAN;
(* returns TRUE if all bits in the bit vector v are clear *)

PROCEDURE And(v1, v2 : BitVector);
(* perform a boolean "and" operation on the two bit vectors *)
(* v2 is the destination of this operation *)
(* v2 := v2 "and" v1 *)
(* if v1 is smaller than v2 then these bits are assumed clear for this *)
(* "and" operation *)
(* if v1 is larger than v2 the extra bits are ignored *)
(* maximum performance comes when two bit vectors have the same lower bound *)

PROCEDURE Or(v1, v2 : BitVector);
(* perform a boolean "or" operation on the two bit vectors *)
(* v2 is the destination of this operation *)
(* v2 := v2 "or" v1 *)
(* if v1 is smaller than v2 then these bits are assumed clear for this *)
(* "or" operation *)
(* if v1 is larger than v2 the extra bits are ignored *)
(* maximum performance comes when two bit vectors have the same lower bound *)

PROCEDURE Minus(v1, v2 : BitVector);
(* remove any bit values set in bit vector v1 from bit vector v2 *)
(* v2 is the destination of this operation *)
(* v2 := v2 "-" v1 *)
(* if v1 is smaller than v2 then these bits are assumed clear for this *)
(* operation *)
(* if v1 is larger than v2 the extra bits are ignored *)
(* maximum performance comes when two bit vectors have the same lower bound *)

PROCEDURE BitsInCommon(v1, v2 : BitVector) : BOOLEAN;
(* returns TRUE if any bit value in the two bit vectors *)
(* is set in both bit vectors, otherwise FALSE is returned *)
(* maximum performance comes when two bit vectors have the same lower bound *)

PROCEDURE Equal(v1, v2 : BitVector) : BOOLEAN;
(* returns TRUE if the two bit vectors contain the same set and clear bits *)
(* otherwise FALSE is returned *)
(* maximum performance comes when two bit vectors have the same lower bound *)

PROCEDURE Copy(v1, v2 : BitVector);
(* copy the bits in bit vector v1 into bit vector v2 *)
(* if v1 is smaller than v2 then these bits are cleared in v2 *)
(* if v1 is larger than v2 then these extra bits are ignored *)
(* maximum performance comes when two bit vectors have the same lower bound *)

PROCEDURE Duplicate(v1 : BitVector; VAR OUT v2 : BitVector);
(* allocate a new bit vector in v2 with the same range a bit vector v1 *)
(* and copy the bits from v1 into v2 *)

PROCEDURE Zap(val : BitVectorElement; v : BitVector);
(* assign all bit vector storage elements in v with the value val *)
(* this procedure is usefull to assign all clear or set bits in a bit vector *)
(* using the constants EmptyElement or FullElement *)

END BitVectors.
(***************************************************************************)
(*                                                                         *)
(*                           Copyright (C) 1989-2002                       *)
(*                         by Stony Brook Software                         *)
(*                                                                         *)
(*                           All rights reserved.                          *)
(*                                                                         *)
(***************************************************************************)

DEFINITION MODULE ASCII;

CONST
    nul = CHR(0);     soh = CHR(1);    stx = CHR(2);    etx = CHR(3);
    eot = CHR(4);     enq = CHR(5);    ack = CHR(6);    bel = CHR(7);
    bs  = CHR(8);     ht  = CHR(9);    lf  = CHR(10);   vt  = CHR(11);
    ff  = CHR(12);    cr  = CHR(13);   so  = CHR(14);   si  = CHR(15);
    dle = CHR(16);    dc1 = CHR(17);   dc2 = CHR(18);   dc3 = CHR(19);
    dc4 = CHR(20);    nak = CHR(21);   syn = CHR(22);   etb = CHR(23);
    can = CHR(24);    em  = CHR(25);   sub = CHR(26);   esc = CHR(27);
    fs  = CHR(28);    gs  = CHR(29);   rs  = CHR(30);   us  = CHR(31);
    space = CHR(32);  del = CHR(127);

END ASCII.
DEFINITION MODULE WholeStr;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs  1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

  (* Whole-number/string conversions *)

IMPORT
    ConvTypes;

TYPE
    ConvResults = ConvTypes.ConvResults;
    (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

(* the string form of a signed whole number is
     ["+" | "-"], decimal digit, {decimal digit}
*)

PROCEDURE StrToInt(str : ARRAY OF CHAR;
                   VAR int : INTEGER;
                   VAR res : ConvResults);
  (* Ignores any leading spaces in str. If the subsequent characters in
     str are in the format of a signed whole number, assigns a corresponding
     value to int. Assigns a value indicating the format of str to res.  *)

PROCEDURE IntToStr(int : INTEGER; VAR str : ARRAY OF CHAR);
  (* Converts the value of int to string form and copies the possibly
     truncated result to str. *)

(* the string form of an unsigned whole number is
     decimal digit, {decimal digit} *)

PROCEDURE StrToCard(str : ARRAY OF CHAR;
                    VAR card : CARDINAL;
                    VAR res : ConvResults);
  (* Ignores any leading spaces in str. If the subsequent characters in
     str are in the format of an unsigned whole number, assigns a
     corresponding value to card. Assigns a value indicating the format
     of str to res.  *)

PROCEDURE CardToStr(card : CARDINAL; VAR str : ARRAY OF CHAR);
  (* Converts the value of card to string form and copies the possibly
     truncated result to str. *)

END WholeStr.
DEFINITION MODULE CharClass;


(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)


%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

  (* Classification of values of the type CHAR *)

PROCEDURE IsNumeric(ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch is classified as a numeric character *)

PROCEDURE IsLetter(ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch is classified as a letter *)

PROCEDURE IsUpper(ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch is classified as an upper case letter *)

PROCEDURE IsLower(ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch is classified as a lower case letter *)

PROCEDURE IsControl(ch: CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch represents a control function *)

PROCEDURE IsWhiteSpace(ch : CHAR): BOOLEAN;
  (* Returns TRUE if and only if ch represents a space character or a format effector *)

END CharClass.
DEFINITION MODULE LongConv;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

(* Low-level LONGREAL/string conversions *)

IMPORT
    ConvTypes;

TYPE
    ConvResults = ConvTypes.ConvResults;
                (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

(* the string form of a signed fixed-point real number is
     ["+" | "-"], decimal digit, {decimal digit}, [".", {decimal digit}]
*)

(* the string form of a signed floating-point real number is
   signed fixed-point real number,
   "E",
   ["+" | "-"], decimal digit, {decimal digit}
*)

PROCEDURE ScanReal(inputCh : CHAR;
                   VAR chClass : ConvTypes.ScanClass;
                   VAR nextState : ConvTypes.ScanState);
(* Represents the start state of a finite state scanner for real
   numbers - assigns class of inputCh to chClass and a procedure
   representing the next state to nextState.
*)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

PROCEDURE FormatReal(str : ARRAY OF CHAR) : ConvResults;
(* Returns the format of the string value for conversion to REAL *)

PROCEDURE ValueReal(str : ARRAY OF CHAR) : LONGREAL;
(* Returns the value corresponding to the real number string
   value str if str is well-formed; otherwise raises the RealConv exception.
*)

PROCEDURE LengthFloatReal(real : LONGREAL; sigFigs : CARDINAL) : CARDINAL;
(* Returns the number of characters in the floating-point
   string representation of real with sigFigs significant figures
*)

PROCEDURE LengthEngReal(real : LONGREAL; sigFigs : CARDINAL) : CARDINAL;
(* Returns the number of characters in the floating-point engineering
   string representation of real with sigFigs significant figures
*)

PROCEDURE LengthFixedReal(real : LONGREAL; place : INTEGER) : CARDINAL;
(* Returns the number of characters in the fixed-point
   string representation of real rounded to the given place relative to
   the decimal point
*)

PROCEDURE IsRConvException() : BOOLEAN;
(* Returns TRUE if the current coroutine is in the exceptional execution
   state because of the raising of an exception in a routine from this
   module; otherwise returns FALSE
*)

END LongConv.

DEFINITION MODULE LongStr;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

  (* LONGREAL/string conversions *)

IMPORT ConvTypes;

TYPE
    ConvResults = ConvTypes.ConvResults;
                  (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

(* the string form of a signed fixed-point real number is
     ["+" | "-"], decimal digit, {decimal digit}, [".", {decimal digit}]
*)

(* the string form of a signed floating-point real number is
   signed fixed-point real number,
   "E",
   ["+" | "-"], decimal digit, {decimal digit}
*)

PROCEDURE StrToReal(str : ARRAY OF CHAR;
                    VAR real : LONGREAL;
                    VAR res : ConvResults);
  (* Ignores any leading spaces in str.
     If the subsequent characters in str are in the format of a signed real
     number, assigns a corresponding value to real.
     Assigns a value indicating the format of str to res.
  *)

PROCEDURE RealToFloat(real : LONGREAL;
                      sigFigs : CARDINAL;
                      VAR str : ARRAY OF CHAR);
  (* Converts the value of real to floating-point string form, with sigFigs
     significant figures, and copies the possibly truncated result to str.
  *)

PROCEDURE RealToEng(real : LONGREAL;
                    sigFigs : CARDINAL;
                    VAR str : ARRAY OF CHAR);
  (* Converts the value of real to floating-point string form, with sigFigs
     significant figures, and copies the possibly truncated result to str.
     The number is scaled with one to three digits in the whole number part
     and with an exponent that is a multiple of three. *)

PROCEDURE RealToFixed(real : LONGREAL;
                      place : INTEGER;
                      VAR str : ARRAY OF CHAR);
  (* Converts the value of real to fixed-point string form, rounded to the
     given place relative to the decimal point, and copies the possibly
     truncated result to str. *)
(* if place is negative then the decimal point is suppressed and only *)
(* the whole part of the number will be output *)

PROCEDURE RealToStr(real : LONGREAL; VAR str : ARRAY OF CHAR);
  (* Converts the value of real as RealToFixed if the sign and magnitude
     can be shown within the capacity of str, or otherwise as RealToFloat,
     and copies the possibly truncated result to str. The number of places
     or significant digits depend on the capacity of str. *)

END LongStr.
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1996                                  *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 1996                                  *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
DEFINITION MODULE LWholeStr;

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

  (* Whole-number/string conversions *)

IMPORT
    ConvTypes;

TYPE
    ConvResults = ConvTypes.ConvResults;
    (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

(* the string form of a signed whole number is
     ["+" | "-"], decimal digit, {decimal digit}
*)

PROCEDURE StrToLongInt(str : ARRAY OF CHAR;
                       VAR int : LONGINT;
                       VAR res : ConvResults);
  (* Ignores any leading spaces in str. If the subsequent characters in
     str are in the format of a signed whole number, assigns a corresponding
     value to int. Assigns a value indicating the format of str to res.  *)

PROCEDURE LongIntToStr(int : LONGINT; VAR str : ARRAY OF CHAR);
  (* Converts the value of int to string form and copies the possibly
     truncated result to str. *)

(* the string form of an unsigned whole number is
     decimal digit, {decimal digit} *)

PROCEDURE StrToLongCard(str : ARRAY OF CHAR;
                        VAR card : LONGCARD;
                        VAR res : ConvResults);
  (* Ignores any leading spaces in str. If the subsequent characters in
     str are in the format of an unsigned whole number, assigns a
     corresponding value to card. Assigns a value indicating the format
     of str to res.  *)

PROCEDURE LongCardToStr(card : LONGCARD; VAR str : ARRAY OF CHAR);
  (* Converts the value of card to string form and copies the possibly
     truncated result to str. *)

END LWholeStr.
DEFINITION MODULE ProgramArgs;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs  1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman

=========================================== *)

(* access to program arguments *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

IMPORT IOChan;

TYPE
    ChanId      = IOChan.ChanId;

PROCEDURE ArgChan() : ChanId;
(* returns the value that identifies a channel for reading program arguments *)

PROCEDURE IsArgPresent() : BOOLEAN;
(* tests if there is a current argument to read from; If not,
   read <= IOChan.CurrentFlags() will be FALSE, and attempting to read
   from the argument channel will raise the exception notAvailable
*)

PROCEDURE NextArg();
(* if there is another argument, causes subsequent input from the
   argument device to come from the start of the next argument. Otherwise
   there is no argument to read from, and a call to IsArgPresent will
   return FALSE
*)

END ProgramArgs.
DEFINITION MODULE IOConsts;


(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)



(* Types and constants for input/output modules *)

TYPE
    (* This type is used to classify the result of an input operation *)
    ReadResults =
    (
     notKnown,          (* no data read result is set *)
     allRight,          (* data is as expected or as required *)
     outOfRange,        (* data cannot be represented *)
     wrongFormat,       (* data not in expected format *)
     endOfLine,         (* end of line seen before expected data *)
     endOfInput         (* end of input seen before expected data *)
    );

END IOConsts.
DEFINITION MODULE RealConv;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

(* Low-level REAL/string conversions *)

IMPORT
    ConvTypes;

TYPE
    ConvResults = ConvTypes.ConvResults;
                (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

(* the string form of a signed fixed-point real number is
     ["+" | "-"], decimal digit, {decimal digit}, [".", {decimal digit}]
*)

(* the string form of a signed floating-point real number is
   signed fixed-point real number,
   "E",
   ["+" | "-"], decimal digit, {decimal digit}
*)

PROCEDURE ScanReal(inputCh : CHAR;
                   VAR chClass : ConvTypes.ScanClass;
                   VAR nextState : ConvTypes.ScanState);
(* Represents the start state of a finite state scanner for real
   numbers - assigns class of inputCh to chClass and a procedure
   representing the next state to nextState.
*)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

PROCEDURE FormatReal(str : ARRAY OF CHAR) : ConvResults;
(* Returns the format of the string value for conversion to REAL *)

PROCEDURE ValueReal(str : ARRAY OF CHAR) : REAL;
(* Returns the value corresponding to the real number string
   value str if str is well-formed; otherwise raises the RealConv exception.
*)

PROCEDURE LengthFloatReal(real : REAL; sigFigs : CARDINAL) : CARDINAL;
(* Returns the number of characters in the floating-point
   string representation of real with sigFigs significant figures
*)

PROCEDURE LengthEngReal(real : REAL; sigFigs : CARDINAL) : CARDINAL;
(* Returns the number of characters in the floating-point engineering
   string representation of real with sigFigs significant figures
*)

PROCEDURE LengthFixedReal(real : REAL; place : INTEGER) : CARDINAL;
(* Returns the number of characters in the fixed-point
   string representation of real rounded to the given place relative to
   the decimal point
*)

PROCEDURE IsRConvException() : BOOLEAN;
(* Returns TRUE if the current coroutine is in the exceptional execution
   state because of the raising of an exception in a routine from this
   module; otherwise returns FALSE
*)

END RealConv.
DEFINITION MODULE RealStr;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END


  (* REAL/string conversions *)

IMPORT ConvTypes;

TYPE
    ConvResults = ConvTypes.ConvResults;
                  (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

(* the string form of a signed fixed-point real number is
     ["+" | "-"], decimal digit, {decimal digit}, [".", {decimal digit}]
*)

(* the string form of a signed floating-point real number is
   signed fixed-point real number,
   "E",
   ["+" | "-"], decimal digit, {decimal digit}
*)

PROCEDURE StrToReal(str : ARRAY OF CHAR;
                    VAR real : REAL;
                    VAR res : ConvResults);
  (* Ignores any leading spaces in str.
     If the subsequent characters in str are in the format of a signed real
     number, assigns a corresponding value to real.
     Assigns a value indicating the format of str to res.
  *)

PROCEDURE RealToFloat(real : REAL;
                      sigFigs : CARDINAL;
                      VAR str : ARRAY OF CHAR);
  (* Converts the value of real to floating-point string form, with sigFigs
     significant figures, and copies the possibly truncated result to str.
  *)

PROCEDURE RealToEng(real : REAL;
                    sigFigs : CARDINAL;
                    VAR str : ARRAY OF CHAR);
  (* Converts the value of real to floating-point string form, with sigFigs
     significant figures, and copies the possibly truncated result to str.
     The number is scaled with one to three digits in the whole number part
     and with an exponent that is a multiple of three. *)

PROCEDURE RealToFixed(real : REAL;
                      place : INTEGER;
                      VAR str : ARRAY OF CHAR);
  (* Converts the value of real to fixed-point string form, rounded to the
     given place relative to the decimal point, and copies the possibly
     truncated result to str. *)
(* if place is negative then the decimal point is suppressed and only *)
(* the whole part of the number will be output *)

PROCEDURE RealToStr(real : REAL; VAR str : ARRAY OF CHAR);
  (* Converts the value of real as RealToFixed if the sign and magnitude
     can be shown within the capacity of str, or otherwise as RealToFloat,
     and copies the possibly truncated result to str. The number of places
     or significant digits depend on the capacity of str. *)

END RealStr.

DEFINITION MODULE Storage;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman

===========================================*)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

(* Facilities for dynamically allocating and deallocating storage *)

IMPORT SYSTEM;

TYPE
    StorageExceptions   =
                          (
                           nilDeallocation,
                           pointerToUnallocatedStorage,
                           wrongStorageToUnallocate
                          );

PROCEDURE ALLOCATE(VAR addr : SYSTEM.ADDRESS; amount : CARDINAL);
(* Allocates storage for a variable of size amount and assigns the
   address of this variable to addr. If there is insufficient unallocated
   storage to do this, the value NIL is assigned to addr.
*)

PROCEDURE DEALLOCATE(VAR addr : SYSTEM.ADDRESS; amount : CARDINAL);
(* Deallocates the amount locations allocated by ALLOCATE for the storage
   of the variable addressed by addr and assigns the value NIL to addr
*)

PROCEDURE StorageException() : StorageExceptions;
(* Returns TRUE if the current coroutine is in the exceptional execution
   state because of the raising of an exception from StorageExceptions;
   otherwise returns FALSE.
*)

PROCEDURE IsStorageException() : BOOLEAN;
(* IF the current coroutine is in the exceptional execution state because
   of the raising of an exception from StorageExceptions, returns the
   corresponding enumeration value, and otherwise raises an exception.
*)

END Storage.


(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)
DEFINITION MODULE Strings;

<*/VALIDVER:ASM*>
%IF Bits16 %THEN
<*/VERSION:ASM*>
%END

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL*>
%END
<*/COPYATTRIBUTES*>

TYPE
    String1 = ARRAY [0..0] OF CHAR;

%IF ASM %AND Bits16 %THEN
    PROCEDURE Length(stringVal : ARRAY OF CHAR) : CARDINAL
                    [PASS(CX, ES:BX), ALTERS(AX,BX,CX,DX,ES), Invariant];
%ELSE
    PROCEDURE Length(stringVal : ARRAY OF CHAR) : CARDINAL [Invariant];
%END

(* the following seven procedures construct a string value, and attempt *)
(* to assign it to a variable parameter. The all have the property that *)
(* if the length of the constructed string value exceeds the capacity *)
(* of the variable parameter, a truncated value is assigned, while *)
(* if the length of the constructed string value is less than the capacity *)
(* of the variable parameter, a string terminator is appended before *)
(* assignment is performed. *)

PROCEDURE Assign(source : ARRAY OF CHAR; VAR OUT destination : ARRAY OF CHAR);
(* copies source to destination *)

PROCEDURE Extract(source : ARRAY OF CHAR;
                  startIndex, numberToExtract : CARDINAL;
                  VAR OUT destination : ARRAY OF CHAR);
(* copies at most numberToExtract characters from source to destination, *)
(* starting at position startIndex in source *)

PROCEDURE Delete(VAR INOUT stringVar : ARRAY OF CHAR;
                 startIndex, numberToDelete: CARDINAL);
(* deletes at most numberToDelete characters from stringVar, starting at *)
(* position startIndex *)

PROCEDURE Insert(source : ARRAY OF CHAR;
                 startIndex : CARDINAL;
                 VAR INOUT destination : ARRAY OF CHAR);
(* inserts source into destination at position startIndex *)

PROCEDURE Replace(source : ARRAY OF CHAR;
                  startIndex : CARDINAL;
                  VAR INOUT destination : ARRAY OF CHAR);
(* copies source into destination, starting at position startIndex. *)
(* Copying stops when all of source has been copied, or when the last *)
(* character of the string in destination has been replaced. *)

PROCEDURE Append(source : ARRAY OF CHAR; VAR INOUT destination : ARRAY OF CHAR);
(* appends source to destination *)

PROCEDURE Concat(source1, source2 : ARRAY OF CHAR;
                 VAR OUT destination : ARRAY OF CHAR);
(* concatenates source2 to source1 and copies the result into destination *)

(* the following predicates provide for pre-testing of the *)
(* operation-completion conditions for the procedures above. *)

PROCEDURE CanAssignAll(sourceLength : CARDINAL;
                       VAR OUT destination : ARRAY OF CHAR) : BOOLEAN;
(* retuns TRUE if a number of characters, indicated by sourceLength, *)
(* will fit into destination; otherwise returns FALSE *)

PROCEDURE CanExtractAll(sourceLength, startIndex, numberToExtract : CARDINAL;
                        VAR OUT destination : ARRAY OF CHAR) : BOOLEAN;
(* returns TRUE if there are numberToExtract characters starting at *)
(* startIndex and within the sourceLength of some string, and if the *)
(* capacity of destination is sufficient to hold numberToExtract characters; *)
(* otherwise returns FALSE *)

PROCEDURE CanDeleteAll(stringLength,
                       startIndex,
                       numberToDelete : CARDINAL) : BOOLEAN;
(* returns TRUE if there are numberToDelete characters starting at *)
(* startIndex and within the stringLength of some string; *)
(* otherwise returns FALSE *)

PROCEDURE CanInsertAll(sourceLength, startIndex : CARDINAL;
                       VAR INOUT destination : ARRAY OF CHAR) : BOOLEAN;
(* returns TRUE if there is room for insertion of sourceLength *)
(* characters from some string into destination starting at startIndex; *)
(* otherwise returns FALSE *)

PROCEDURE CanReplaceAll(sourceLength, startIndex : CARDINAL;
                        VAR INOUT destination : ARRAY OF CHAR) : BOOLEAN;
(* returns TRUE if there is room for the replacement of sourecLength *)
(* characters in destination starting at startIndex; *)
(* otherwise returns FALSE *)

PROCEDURE CanAppendAll(sourceLength : CARDINAL;
                       VAR INOUT destination : ARRAY OF CHAR) : BOOLEAN;
(* returns TRUE if there is sufficient room in destination to append *)
(* a string of length sourceLength to the string in destination; *)
(* otherwise returns FALSE *)

PROCEDURE CanConcatAll(source1Length, source2Length : CARDINAL;
                       VAR OUT destination: ARRAY OF CHAR) : BOOLEAN;
(* returns TRUE if there is sufficient room in destination for two strings *)
(* of length source1Length and source2Length; otherwise returns FALSE *)

TYPE
    CompareResults = (less, equal, greater);

%IF ASM %AND Bits16 %THEN

PROCEDURE Compare(stringVal1, stringVal2 : ARRAY OF CHAR) : CompareResults
    [PASS(CX, ES:DI, DX, BX:SI), ALTERS(AX,BX,CX,DX,SI,DI,ES), Invariant];

PROCEDURE Equal(stringVal1, stringVal2 : ARRAY OF CHAR) : BOOLEAN
    [PASS(CX, ES:DI, DX, BX:SI), ALTERS(AX,BX,CX,DX,SI,DI,ES), Invariant];

%ELSE

PROCEDURE Compare(stringVal1, stringVal2 : ARRAY OF CHAR)
                                        : CompareResults [Invariant];
(* returns less, equal or greater accordinly if stringVal1 is lexically *)
(* less than, equal to, or greater than stringVal2 *)

PROCEDURE Equal(stringVal1, stringVal2 : ARRAY OF CHAR)
                                        : BOOLEAN [Invariant];
(* returns Compare(stringVal1, stringVal2) = equal *)

%END

PROCEDURE FindNext(pattern, stringToSearch : ARRAY OF CHAR;
                   startIndex : CARDINAL;
                   VAR OUT patternFound : BOOLEAN;
                   VAR OUT posOfPattern : CARDINAL);
(* looks forward for the next occurance of pattern in stringToSearch, *)
(* starting the search at position startIndex. *)
(* If startIndex < LENGTH(stringToSearch) and pattern is found, patternFound *)
(* is returned as TRUE, and posOfPattern contains the start position in *)
(* stringToSearch of pattern. Otherwise patternFound is returned as FALSE, *)
(* and posOfPattern is unchanged. *)

PROCEDURE FindPrev(pattern, stringToSearch : ARRAY OF CHAR;
                   startIndex : CARDINAL;
                   VAR OUT patternFound : BOOLEAN;
                   VAR OUT posOfPattern : CARDINAL);
(* looks backward for the previous occurence of pattern in stringToSearch *)
(* and returns the position of the first character of the pattern if found. *)
(* The search for the pattern begins at startIndex. If pattern is found, *)
(* patternFound is returned as TRUE, and posOfPattern contains the start *)
(* position in stringToSearch of pattern in the range [0..startIndex]. *)
(* otherwise patternFound is returned FALSE, and posOfPattern is unchanged. *)

PROCEDURE FindDiff(stringVal1, stringVal2 : ARRAY OF CHAR;
                   VAR OUT differenceFound : BOOLEAN;
                   VAR OUT posOfDifference : CARDINAL);
(* compares the string values in stringVal1 and stringVal2 for differences. *)
(* If they are equal, differenceFound is returned as FALSE, and TRUE *)
(* otherwise. If differenceFound is TRUE, posOfDifference is set to the *)
(* position of the first difference; otherwise posOfDifference is unchanged. *)

PROCEDURE Capitalize(VAR INOUT stringVar : ARRAY OF CHAR);
(* applies the function CAP to each character of the string in stringVar *)

END Strings.


DEFINITION MODULE WholeConv;
(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman
=========================================== *)

(* Low-level whole-number.string conversions *)

IMPORT
    ConvTypes;

TYPE
    ConvResults = ConvTypes.ConvResults;
                (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

PROCEDURE ScanInt(inputCh : CHAR;
                  VAR chClass : ConvTypes.ScanClass;
                  VAR nextState : ConvTypes.ScanState);
(* Represents the start state of a finite state scanner for signed whole
   numbers - assigns class of inputCh to chClass and a procedure
   representing the next state to nextState.
*)

PROCEDURE ScanCard(inputCh : CHAR;
                   VAR chClass : ConvTypes.ScanClass;
                   VAR nextState : ConvTypes.ScanState);
(* Represents the start state of a finite state scanner for signed whole
   numbers - assigns class of inputCh to chClass and a procedure
   representing the next state to nextState.
*)

%IF DLL %THEN
<*/EXPORTALL/PROPAGATEEXCEPTIONALL/COPYATTRIBUTES*>
%END

PROCEDURE FormatInt(str : ARRAY OF CHAR) : ConvResults;
(* Returns the format of the string value for conversion to INTEGER *)

PROCEDURE ValueInt(str : ARRAY OF CHAR) : INTEGER;
(* Returns the value corresponding to the signed whole number string
   value str if str is well-formed; otherwise raises the WholeConv exception.
*)

PROCEDURE LengthInt(int : INTEGER) : CARDINAL;
(* Returns the number of characters in the string representation of int *)

PROCEDURE FormatCard(str : ARRAY OF CHAR) : ConvResults;
(* Returns the format of the string value for conversion to CARDINAL *)

PROCEDURE ValueCard(str : ARRAY OF CHAR) : CARDINAL;
(* Returns the value corresponding to the signed whole number string
   value str if str is well-formed; otherwise raises the WholeConv exception.
*)

PROCEDURE LengthCard(card : CARDINAL) : CARDINAL;
(* Returns the number of characters in the string representation of card *)

PROCEDURE IsWholeConvException() : BOOLEAN;
(* Returns TRUE if the current coroutine is in the exceptional execution
   state because of the raising of an exception in a routine from this
   module; otherwise returns FALSE
*)

END WholeConv.

DEFINITION MODULE Str;

IMPORT Strings;

TYPE
    CHARSET	= PACKEDSET OF CHAR;
    PosLen =
	RECORD
        Pos	: CARDINAL;
	Len	: CARDINAL;
	END;


CONST
    Caps	= Strings.Capitalize;
    Length	= Strings.Length;
    Delete	= Strings.Delete;
(*
PROCEDURE Caps(VAR str : ARRAY OF CHAR);
PROCEDURE Length(str : ARRAY OF CHAR) : CARDINAL;
PROCEDURE Delete(VAR str : ARRAY OF CHAR; pos, len : CARDINAL);
*)

PROCEDURE Lows(VAR str : ARRAY OF CHAR);

PROCEDURE Compare(s1, s2 : ARRAY OF CHAR) : INTEGER;

PROCEDURE Concat(VAR dest : ARRAY OF CHAR;
		 str1 : ARRAY OF CHAR;
		 str2 : ARRAY OF CHAR);

PROCEDURE Append(VAR dest : ARRAY OF CHAR; str : ARRAY OF CHAR);

PROCEDURE Copy(VAR dest : ARRAY OF CHAR; str : ARRAY OF CHAR);

PROCEDURE Slice(VAR dest : ARRAY OF CHAR;
		str : ARRAY OF CHAR;
		pos, len : CARDINAL);

PROCEDURE Pos(str, subStr : ARRAY OF CHAR) : CARDINAL;

PROCEDURE NextPos(str, subStr : ARRAY OF CHAR; pos : CARDINAL) : CARDINAL;

PROCEDURE CharPos(str : ARRAY OF CHAR; ch : CHAR) : CARDINAL;

PROCEDURE RCharPos(str : ARRAY OF CHAR; ch : CHAR) : CARDINAL;

PROCEDURE Item(VAR dest : ARRAY OF CHAR;
	       str : ARRAY OF CHAR;
	       sep : CHARSET;
	       n : CARDINAL);

PROCEDURE ItemS(VAR dest : ARRAY OF CHAR;
		str : ARRAY OF CHAR;
		sep : ARRAY OF CHAR;
		n : CARDINAL);

PROCEDURE Prepend(VAR dest : ARRAY OF CHAR; str : ARRAY OF CHAR);

PROCEDURE Insert(VAR dest : ARRAY OF CHAR;
		str : ARRAY OF CHAR;
		pos : CARDINAL);

PROCEDURE Subst(VAR dest : ARRAY OF CHAR;
		src : ARRAY OF CHAR;
		new : ARRAY OF CHAR);

PROCEDURE Match(src, pattern : ARRAY OF CHAR) : BOOLEAN;

PROCEDURE FindSubStr(src, pattern : ARRAY OF CHAR;
		     VAR posList : ARRAY OF PosLen) : BOOLEAN;


PROCEDURE IntToStr(num : LONGINT;
		   VAR str : ARRAY OF CHAR;
		   base : CARDINAL;
		   VAR ok : BOOLEAN);

PROCEDURE CardToStr(num : CARDINAL32;
		    VAR str : ARRAY OF CHAR;
		    base : CARDINAL;
                    VAR ok : BOOLEAN);

PROCEDURE RealToStr(num : LONGREAL;
                    precision : CARDINAL;
		    eng : BOOLEAN;
		    VAR dest : ARRAY OF CHAR;
		    VAR ok : BOOLEAN);

PROCEDURE FixRealToStr(num : LONGREAL;
                       precision : CARDINAL;
		       VAR str : ARRAY OF CHAR;
		       VAR ok : BOOLEAN);

PROCEDURE StrToInt(str : ARRAY OF CHAR;
		   base : CARDINAL;
		   VAR ok : BOOLEAN) : LONGINT;

PROCEDURE StrToCard(str : ARRAY OF CHAR;
		    base : CARDINAL;
                    VAR ok : BOOLEAN) : CARDINAL32;

PROCEDURE StrToReal(str : ARRAY OF CHAR; VAR ok : BOOLEAN ) : LONGREAL;

PROCEDURE StrToC(src : ARRAY OF CHAR; VAR dest : ARRAY OF CHAR) : BOOLEAN;

PROCEDURE StrToPas(src : ARRAY OF CHAR; VAR dest : ARRAY OF CHAR) : BOOLEAN;

END Str.

DEFINITION MODULE Lib;

FROM SYSTEM IMPORT
    ADDRESS, BYTE, WORD;

%IF DOS %OR FlashTek %THEN
FROM SYSTEM IMPORT
    REGISTERS;
%END

VAR
    RunTimeError        : PROCEDURE(CARDINAL32, CARDINAL, ARRAY OF CHAR);

PROCEDURE AddAddr(a : ADDRESS; incr : CARDINAL) : ADDRESS;

PROCEDURE SubAddr(a : ADDRESS; incr : CARDINAL) : ADDRESS;

PROCEDURE IncAddr(VAR a : ADDRESS; incr : CARDINAL);

PROCEDURE DecAddr(VAR a : ADDRESS; incr : CARDINAL);

PROCEDURE Compare(src, dest : ADDRESS; len : CARDINAL) : CARDINAL;

TYPE
    CpuKind     =
        (
         cpu_Unknown,
         cpu_V20,
         cpu_V30,
         cpu_8088,
         cpu_8086,
         cpu_80188,
         cpu_80186,
         cpu_80286,
         cpu_80386,
         cpu_80486,
         cpu_Pentium
        );
    FpuKind     =
        (
         fpu_none,
         fpu_8087,
         fpu_80287,
         fpu_80387
        );
    CpuRec      =
        RECORD
            cpu : CpuKind;
            fpu : FpuKind;
        END;

PROCEDURE CpuId(VAR id : CpuRec);


PROCEDURE Delay(amount : CARDINAL);

%IF DOS %OR FlashTek %THEN
PROCEDURE Dos(VAR R : REGISTERS);
%END

TYPE
    ExecEnvPtr  = POINTER TO ARRAY [0..0] OF ADDRESS;

PROCEDURE Exec(command, params : ARRAY OF CHAR; env : ExecEnvPtr) : CARDINAL;

PROCEDURE ExecCmd(command : ARRAY OF CHAR) : CARDINAL;

PROCEDURE EnvironmentFind(name : ARRAY OF CHAR;
                          VAR result : ARRAY OF CHAR);

PROCEDURE Fill(dest : ADDRESS; count : CARDINAL; db : BYTE);

TYPE
    DayType     = (
                   Sunday,
                   Monday,
                   Tuesday,
                   Wednesday,
                   Thursday,
                   Friday,
                   Saturday);

PROCEDURE GetDate(VAR year, month, day : CARDINAL;
                  VAR dayOfWeek : DayType);

PROCEDURE GetTime(VAR hrs, mins, secs, hsecs : CARDINAL);

PROCEDURE SetDate(year, month, day : CARDINAL; dayOfWeek : DayType);

PROCEDURE SetTime(hrs, mins, secs, hsecs : CARDINAL);

PROCEDURE HashString(str : ARRAY OF CHAR; range : CARDINAL) : CARDINAL;

%IF DOS %OR FlashTek %THEN
PROCEDURE Intr(VAR R : REGISTERS; intNum : CARDINAL);
%END

PROCEDURE Move(src, dest : ADDRESS; count : CARDINAL);

PROCEDURE ParamCount() : CARDINAL;

PROCEDURE ParamStr(VAR str : ARRAY OF CHAR; n : CARDINAL);

TYPE
    CompareProc = PROCEDURE(CARDINAL, CARDINAL) : BOOLEAN;
    SwapProc    = PROCEDURE(CARDINAL, CARDINAL);

PROCEDURE QSort(numItems : CARDINAL;
                less : CompareProc;
                swap : SwapProc);

PROCEDURE HSort(numItems : CARDINAL;
                less : CompareProc;
                swap : SwapProc);

PROCEDURE RAND() : REAL;

PROCEDURE RANDOM(range : CARDINAL) : CARDINAL;

PROCEDURE RANDOMIZE;

PROCEDURE ScanL(dest : ADDRESS; count : CARDINAL; db : BYTE) : CARDINAL;

PROCEDURE ScanNeL(dest : ADDRESS; count : CARDINAL; db : BYTE) : CARDINAL;

PROCEDURE ScanNeR(dest : ADDRESS; count : CARDINAL; db : BYTE) : CARDINAL;

PROCEDURE ScanR(dest : ADDRESS; count : CARDINAL; db : BYTE) : CARDINAL;

PROCEDURE SetReturnCode(code : CARDINAL);

PROCEDURE Terminate(p : PROC; VAR old : PROC);

PROCEDURE WordFill(dest : ADDRESS; cound : CARDINAL; dw : WORD);

PROCEDURE WordMove(src, dest : ADDRESS; count : CARDINAL);

END Lib.
DEFINITION MODULE MATHLIB;

IMPORT LongMath;

CONST
    M_E		= 2.718281828459045240;      (* e *)
    M_Log2E	= 1.442695040888963410;      (* log2 of e *)
    M_Log10E	= 0.434294481903251828;      (* log10 of e *)
    M_Ln2	= 0.693147180559945309;      (* ln of 2.0 *)
    M_Ln10	= 2.302585092994045680;      (* ln of 10.0 *)
    M_Pi	= 3.141592653589793240;      (* pi *)
    M_PiBy2	= 1.570796326794896620;      (* pi / 2.0 *)
    M_PiBy4	= 0.785398163397448310;      (* pi / 4.0 *)
    M_1ByPy	= 0.318309886183790672;      (* 1.0 / pi *)
    M_2ByPy	= 0.636619772367581343;      (* 2.0 / pi *)
    M_1BySqrtPi	= 0.564189583547756287;      (* 1.0 / sqrt(pi) *)
    M_2BySqrtPi	= 1.128379167095512570;      (* 2.0 / sqrt(pi) *)
    M_Sqrt2	= 1.414213562373095050;      (* sqrt(2.0) *)
    M_Sqrt2By2	= 0.707106781186547524;      (* sqrt(2.0) / 2.0 *)

CONST
    Sin    = LongMath.sin;
    Cos    = LongMath.cos;
    Tan    = LongMath.tan;
    ASin   = LongMath.arcsin;
    ACos   = LongMath.arccos;
    ATan   = LongMath.arctan;
    Sqrt   = LongMath.sqrt;
    Exp    = LongMath.exp;
    Log    = LongMath.ln;
    Pow    = LongMath.power;
(*
PROCEDURE Sin(num : LONGREAL) : LONGREAL;
PROCEDURE Cos(num : LONGREAL) : LONGREAL;
PROCEDURE Tan(num : LONGREAL) : LONGREAL;
PROCEDURE ASin(num : LONGREAL) : LONGREAL;
PROCEDURE ACos(num : LONGREAL) : LONGREAL;
PROCEDURE ATan(num : LONGREAL) : LONGREAL;
PROCEDURE Sqrt(num : LONGREAL) : LONGREAL;
PROCEDURE Exp(num : LONGREAL) : LONGREAL;
PROCEDURE Log(num : LONGREAL) : LONGREAL;
PROCEDURE Pow(X, Y : LONGREAL) : LONGREAL;
*)

PROCEDURE SinH(num : LONGREAL) : LONGREAL;

PROCEDURE CosH(num : LONGREAL) : LONGREAL;

PROCEDURE TanH(num : LONGREAL) : LONGREAL;

PROCEDURE ATan2(X, Y : LONGREAL) : LONGREAL;

PROCEDURE Log10(num : LONGREAL) : LONGREAL;

PROCEDURE Mod(X, Y : LONGREAL) : LONGREAL;

PROCEDURE Rexp(VAR i : INTEGER; num : LONGREAL) : LONGREAL;

END MATHLIB.
