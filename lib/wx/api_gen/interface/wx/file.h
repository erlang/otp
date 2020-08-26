/////////////////////////////////////////////////////////////////////////////
// Name:        file.h
// Purpose:     interface of wxTempFile, wxFile
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxTempFile

    wxTempFile provides a relatively safe way to replace the contents of the
    existing file. The name is explained by the fact that it may be also used as
    just a temporary file if you don't replace the old file contents.

    Usually, when a program replaces the contents of some file it first opens it for
    writing, thus losing all of the old data and then starts recreating it.
    This approach is not very safe because during the regeneration of the file bad
    things may happen: the program may find that there is an internal error preventing
    it from completing file generation, the user may interrupt it (especially if file
    generation takes long time) and, finally, any other external interrupts (power
    supply failure or a disk error) will leave you without either the original file
    or the new one.

    wxTempFile addresses this problem by creating a temporary file which is meant to
    replace the original file - but only after it is fully written. So, if the user
    interrupts the program during the file generation, the old file won't be lost.
    Also, if the program discovers itself that it doesn't want to replace the old
    file there is no problem - in fact, wxTempFile will @b not replace the old
    file by default, you should explicitly call wxTempFile::Commit() to do it.
    Calling wxTempFile::Discard() explicitly discards any modifications: it
    closes and deletes the temporary file and leaves the original file unchanged.
    If you call neither Commit() nor Discard(), the destructor will
    call Discard() automatically.

    To summarize: if you want to replace another file, create an instance of
    wxTempFile passing the name of the file to be replaced to the constructor.
    (You may also use default constructor and pass the file name to wxTempFile::Open.)
    Then you can write to wxTempFile using wxFile-like functions and later call
    wxTempFile::Commit() to replace the old file (and close this one) or call
    wxTempFile::Discard() to cancel the modifications.

    @library{wxbase}
    @category{file}
*/
class wxTempFile
{
public:
    /**
        Default constructor doesn't do anything.

        Call Open() later.
     */
    wxTempFile();

    /**
        Associates wxTempFile with the file to be replaced and opens it.

        @warning
        You should use IsOpened() to verify that the constructor succeeded.
    */
    explicit wxTempFile(const wxString& strName);

    /**
        Destructor calls Discard() if temporary file is still open.
    */
    ~wxTempFile();

    /**
        Validate changes: deletes the old file of name m_strName and renames the new
        file to the old name. Returns @true if both actions succeeded.

        If @false is returned it may unfortunately mean two quite different things:
        either that the old file couldn't be deleted or that the new file
        couldn't be renamed to the old name.
    */
    bool Commit();

    /**
        Discard changes: the old file contents are not changed, the temporary
        file is deleted.
    */
    void Discard();

    /**
        Flush the data written to the file to disk.

        This simply calls wxFile::Flush() for the underlying file and may be
        necessary with file systems such as XFS and Ext4 under Linux. Calling
        this function may however have serious performance implications and
        also is not necessary with many other file systems so it is not done by
        default -- but you can call it before calling Commit() to absolutely
        ensure that the data was indeed written to the disk correctly.
     */
    bool Flush();

    /**
        Returns @true if the file was successfully opened.
    */
    bool IsOpened() const;

    /**
        Returns the length of the file.

        Returns ::wxInvalidOffset if the length couldn't be determined.

        Please also note that there is @e no guarantee that reading that many
        bytes from the file will always succeed. While this is true for regular
        files (unless the file size has been changed by another process in
        between Length() and Read() calls), some special files, such as most
        files under @c /sys or @c /proc directories under Linux, don't actually
        contain as much data as their size indicates.
    */
    wxFileOffset Length() const;

    /**
        Open the temporary file, returns @true on success, @false if an error
        occurred.
        @a strName is the name of file to be replaced. The temporary file is always
        created in the directory where @a strName is. In particular, if @a strName
        doesn't include the path, it is created in the current directory and the
        program should have write access to it for the function to succeed.
    */
    bool Open(const wxString& strName);

    /**
        Seeks to the specified position.
    */
    wxFileOffset Seek(wxFileOffset ofs,
                      wxSeekMode mode = wxFromStart);

    /**
        Returns the current position or ::wxInvalidOffset if file is not opened or
        if another error occurred.
    */
    wxFileOffset Tell() const;

    /**
        Write to the file, return @true on success, @false on failure.
        The second argument is only meaningful in Unicode build of wxWidgets when
        @a conv is used to convert @a str to multibyte representation.
    */
    bool Write(const wxString& str,
               const wxMBConv& conv = wxConvUTF8);
};



/**
    @class wxFile

    A wxFile performs raw file I/O. This is a very small class designed to
    minimize the overhead of using it - in fact, there is hardly any overhead at
    all, but using it brings you automatic error checking and hides differences
    between platforms and compilers. wxFile also automatically closes the file in
    its destructor so you won't forget to do so.
    wxFile is a wrapper around @c file descriptor. - see also wxFFile for a
    wrapper around @c FILE structure.

    ::wxFileOffset is used by the wxFile functions which require offsets as
    parameter or return them. If the platform supports it, wxFileOffset is a
    typedef for a native 64 bit integer, otherwise a 32 bit integer is used for
    ::wxFileOffset.

    @library{wxbase}
    @category{file}
*/
class wxFile
{
public:

    /**
        The OpenMode enumeration defines the different modes for opening a file with wxFile.
        It is also used with wxFile::Access function.
    */
    enum OpenMode {

        /** Open file for reading or test if it can be opened for reading with Access() */
        read,

        /** Open file for writing deleting the contents of the file if it already exists
            or test if it can be opened for writing with Access(). */
        write,

        /** Open file for reading and writing; cannot be used with Access() */
        read_write,

        /** Open file for appending: the file is opened for writing, but the old contents
            of the file are not erased and the file pointer is initially placed at the end
            of the file; cannot be used with Access().

            This is the same as OpenMode::write if the file doesn't exist.
        */
        write_append,

        /**
            Open the file securely for writing (Uses O_EXCL | O_CREAT).
            Will fail if the file already exists, else create and open it atomically.
            Useful for opening temporary files without being vulnerable to race exploits.
        */
        write_excl
    };

    /**
       Standard file descriptors
    */
    enum { fd_invalid = -1, fd_stdin, fd_stdout, fd_stderr };

    /**
       Default constructor.
    */
    wxFile();

    /**
        Opens a file with a filename.

        @param filename
            The filename.
        @param mode
            The mode in which to open the file.

        @warning
        You should use IsOpened() to verify that the constructor succeeded.
    */
    wxFile(const wxString& filename,
           wxFile::OpenMode mode = wxFile::read);

    /**
        Associates the file with the given file descriptor, which has already been
        opened. See Attach() for the list of predefined descriptors.

        @param fd
            An existing file descriptor.
    */
    wxFile(int fd);

    /**
        Destructor will close the file.
        @note This destructor is not virtual so you should not use wxFile polymorphically.
    */
    ~wxFile();

    /**
        Returns the error code for the last unsuccessful operation.

        The error code is system-dependent and corresponds to the value of the
        standard @c errno variable when the last error occurred.

        Notice that only simple accessors such as IsOpened() and Eof() (and
        this method itself) don't modify the last error value, all other
        methods can potentially change it if an error occurs, including the
        const ones such as Tell() or Length().

        @since 2.9.2

        @see ClearLastError()
    */
    int GetLastError() const;

    /**
        Resets the error code.

        GetLastError() will return 0 until the next error occurs.

        @since 2.9.2
    */
    void ClearLastError();

    /**
        This function verifies if we may access the given file in specified mode.
        Only values of @c wxFile::read or @c wxFile::write really make sense here.
    */
    static bool Access(const wxString& name, wxFile::OpenMode mode);

    /**
        Attaches an existing file descriptor to the wxFile object.
        Examples of predefined file descriptors are 0, 1 and 2 which correspond to
        stdin, stdout and stderr (and have symbolic names of @c wxFile::fd_stdin,
        @c wxFile::fd_stdout and @c wxFile::fd_stderr).

        The descriptor should be already opened and it will be closed by wxFile
        object.
    */
    void Attach(int fd);

    /**
        Closes the file.
    */
    bool Close();

    /**
        Creates a file for writing.

        If the file already exists, setting @b overwrite to @true will ensure
        it is overwritten.

        @a access may be an OR combination of the ::wxPosixPermissions enumeration
        values.
    */
    bool Create(const wxString& filename,
                bool overwrite = false,
                int access = wxS_DEFAULT);

    /**
        Get back a file descriptor from wxFile object - the caller is responsible for
        closing the file if this descriptor is opened.
        IsOpened() will return @false after call to Detach().

        @return The file descriptor (this is new since wxWidgets 3.0.0, in the
        previous versions this method didn't return anything).
    */
    int Detach();

    /**
        Returns @true if the end of the file has been reached.
        Note that the behaviour of the file pointer-based class wxFFile is
        different as wxFFile::Eof() will return @true here only if an
        attempt has been made to read @b past the last byte of the file, while
        wxFile::Eof() will return @true even before such attempt is made if the
        file pointer is at the last position in the file.

        Note also that this function doesn't work on unseekable file descriptors
        (examples include pipes, terminals and sockets under Unix) and an attempt to
        use it will result in an error message.

        So, to read the entire file into memory, you should write a loop which uses
        Read() repeatedly and tests its return condition instead of using Eof()
        as this will not work for special files under Unix.
    */
    bool Eof() const;

    /**
        Returns @true if the given name specifies an existing regular file
        (not a directory or a link).
    */
    static bool Exists(const wxString& filename);

    /**
        Flushes the file descriptor.

        Note that Flush() is not implemented on some Windows compilers due to a
        missing fsync function, which reduces the usefulness of this function
        (it can still be called but it will do nothing on unsupported compilers).
    */
    bool Flush();

    /**
        Returns the type of the file.
    */
    wxFileKind GetKind() const;

    /**
        Returns @true if the file has been opened.
    */
    bool IsOpened() const;

    /**
        Returns the length of the file.
    */
    wxFileOffset Length() const;

    /**
        Opens the file, returning @true if successful.

        @param filename
            The filename.
        @param mode
            The mode in which to open the file.
        @param access
            An OR-combination of ::wxPosixPermissions enumeration values.
    */
    bool Open(const wxString& filename, wxFile::OpenMode mode = wxFile::read,
              int access = wxS_DEFAULT);

    /**
        Reads from the file into a memory buffer.

        @param buffer
           Buffer to write in
        @param count
           Bytes to read

        @return The number of bytes read, or the symbol ::wxInvalidOffset.
    */
    ssize_t Read(void* buffer, size_t count);

    /**
        Reads the entire contents of the file into a string.

        Since wxWidgets 3.1.1 this method also works for unseekable files.

        @param str
            Non-@NULL pointer to a string to read data into.
        @param conv
            Conversion object to use in Unicode build; by default supposes
            that file contents is encoded in UTF-8 but falls back to the
            current locale encoding (or Latin-1 if it is UTF-8 too) if it is
            not.

        @return @true if file was read successfully, @false otherwise.

        @since 2.9.5
    */
    bool ReadAll(wxString* str, const wxMBConv& conv = wxConvAuto());

    /**
        Seeks to the specified position.

        @param ofs
            Offset to seek to.
        @param mode
            One of wxFromStart, wxFromEnd, wxFromCurrent.

        @return The actual offset position achieved, or ::wxInvalidOffset on
                failure.
    */
    wxFileOffset Seek(wxFileOffset ofs,
                      wxSeekMode mode = wxFromStart);

    /**
        Moves the file pointer to the specified number of bytes relative to the
        end of the file. For example, @c SeekEnd(-5) would position the pointer 5
        bytes before the end.

        @param ofs
            Number of bytes before the end of the file.

        @return The actual offset position achieved, or ::wxInvalidOffset on
                failure.
    */
    wxFileOffset SeekEnd(wxFileOffset ofs = 0);

    /**
        Returns the current position or ::wxInvalidOffset if file is not opened or
        if another error occurred.
    */
    wxFileOffset Tell() const;

    /**
       Write data to the file (descriptor).

       @param buffer
          Buffer from which to read data
       @param count
         Number of bytes to write

       @return The number of bytes written.
    */
    size_t Write(const void *buffer, size_t count);

    /**
        Writes the contents of the string to the file, returns @true on success.
        The second argument is only meaningful in Unicode build of wxWidgets when
        @a conv is used to convert @a s to a multibyte representation.

        Note that this method only works with @c NUL-terminated strings, if you want
        to write data with embedded @c NULs to the file you should use the other
        Write() overload.
    */
    bool Write(const wxString& s, const wxMBConv& conv = wxConvAuto());

    /**
        Returns the file descriptor associated with the file.
    */
    int fd() const;
};

