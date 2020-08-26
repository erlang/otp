/////////////////////////////////////////////////////////////////////////////
// Name:        ffile.h
// Purpose:     interface of wxTempFFile, wxFFile
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxTempFFile

    wxTempFFile provides a relatively safe way to replace the contents of the
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

    wxTempFFile addresses this problem by creating a temporary file which is meant to
    replace the original file - but only after it is fully written. So, if the user
    interrupts the program during the file generation, the old file won't be lost.
    Also, if the program discovers itself that it doesn't want to replace the old
    file there is no problem - in fact, wxTempFFile will @b not replace the old
    file by default, you should explicitly call wxTempFFile::Commit() to do it.
    Calling wxTempFFile::Discard() explicitly discards any modifications: it
    closes and deletes the temporary file and leaves the original file unchanged.
    If you call neither Commit() nor Discard(), the destructor will
    call Discard() automatically.

    To summarize: if you want to replace another file, create an instance of
    wxTempFFile passing the name of the file to be replaced to the constructor.
    (You may also use default constructor and pass the file name to wxTempFFile::Open.)
    Then you can write to wxTempFFile using wxFFile-like functions and later call
    wxTempFFile::Commit() to replace the old file (and close this one) or call
    wxTempFFile::Discard() to cancel the modifications.

    @since 3.1.4

    @library{wxbase}
    @category{file}
*/
class wxTempFFile
{
public:
    /**
        Default constructor doesn't do anything.

        Call Open() later.
     */
    wxTempFFile();

    /**
        Associates wxTempFFile with the file to be replaced and opens it.

        @warning
        You should use IsOpened() to verify that the constructor succeeded.
    */
    explicit wxTempFFile(const wxString& strName);

    /**
        Destructor calls Discard() if temporary file is still open.
    */
    ~wxTempFFile();

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

        This simply calls wxFFile::Flush() for the underlying file and may be
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
        Seeks to the specified position and returns @true on success.
    */
    bool Seek(wxFileOffset ofs, wxSeekMode mode = wxFromStart);

    /**
        Returns the current position.
    */
    wxFileOffset Tell() const;

    /**
        Writes the contents of the string to the file, returns @true on success.

        The second argument is only meaningful in Unicode build of wxWidgets when
        @a conv is used to convert @a str to multibyte representation.
    */
    bool Write(const wxString& str, const wxMBConv& conv = wxMBConvUTF8());
};



/**
    @class wxFFile

    wxFFile implements buffered file I/O.

    This is a very small class designed to minimize the overhead of using it - in fact,
    there is hardly any overhead at all, but using it brings you automatic error checking
    and hides differences between platforms and compilers.

    It wraps inside it a @c FILE * handle used by standard C IO library (also known as @c stdio).

    @library{wxbase}
    @category{file}

    @see wxFFile::IsOpened
*/
class wxFFile
{
public:
    wxFFile();

    /**
        Opens a file with the given file pointer, which has already been opened.

        @param fp
            An existing file descriptor, such as stderr.
    */
    wxFFile(FILE* fp);

    /**
        Opens a file with the given mode.
        As there is no way to return whether the operation was successful or not from
        the constructor you should test the return value of IsOpened() to check that it
        didn't fail.

        @param filename
            The filename.
        @param mode
            The mode in which to open the file using standard C strings.
            Note that you should use "b" flag if you use binary files under Windows
            or the results might be unexpected due to automatic newline conversion done
            for the text files.
    */
    wxFFile(const wxString& filename, const wxString& mode = "r");


    /**
        Destructor will close the file.

        @note it is not virtual so you should @e not derive from wxFFile!
    */
    ~wxFFile();

    /**
        Attaches an existing file pointer to the wxFFile object.

        The descriptor should be already opened and it will be closed by wxFFile object.
    */
    void Attach(FILE* fp, const wxString& name = wxEmptyString);

    /**
        Closes the file and returns @true on success.
    */
    bool Close();

    /**
        Get back a file pointer from wxFFile object -- the caller is responsible for
        closing the file if this descriptor is opened.

        IsOpened() will return @false after call to Detach().

        @return The FILE pointer (this is new since wxWidgets 3.0.0, in the
        previous versions this method didn't return anything).
    */
    FILE* Detach();

    /**
        Returns @true if an attempt has been made to read @e past
        the end of the file.

        Note that the behaviour of the file descriptor based class  wxFile is different as
        wxFile::Eof() will return @true here as soon as the last byte of the file has been read.

        Also note that this method may only be called for opened files. Otherwise it asserts and returns false.

        @see IsOpened()
    */
    bool Eof() const;

    /**
        Returns @true if an error has occurred on this file, similar to the standard
        @c ferror() function.

        Please note that this method may only be called for opened files. Otherwise it asserts and returns false.

        @see IsOpened()
    */
    bool Error() const;

    /**
        Flushes the file and returns @true on success.
    */
    bool Flush();

    /**
        Returns the type of the file.

        @see wxFileKind
    */
    wxFileKind GetKind() const;

    /**
        Returns the file name.

        This is the name that was specified when the object was constructed or
        to the last call to Open(). Notice that it may be empty if Attach() was
        called without specifying the name.
     */
    const wxString& GetName() const;

    /**
        Returns @true if the file is opened.

        Most of the methods of this class may only be used for an opened file.
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
    */
    bool Open(const wxString& filename, const wxString& mode = "r");

    /**
        Reads the specified number of bytes into a buffer, returning the actual number read.

        @param buffer
            A buffer to receive the data.
        @param count
            The number of bytes to read.

        @return The number of bytes read.
    */
    size_t Read(void* buffer, size_t count);

    /**
        Reads the entire contents of the file into a string.

        @param str
            String to read data into.
        @param conv
            Conversion object to use in Unicode build; by default supposes
            that file contents is encoded in UTF-8.

        @return @true if file was read successfully, @false otherwise.
    */
    bool ReadAll(wxString* str, const wxMBConv& conv = wxConvAuto());

    /**
        Seeks to the specified position and returns @true on success.

        @param ofs
            Offset to seek to.
        @param mode
            One of wxFromStart, wxFromEnd, wxFromCurrent.
    */
    bool Seek(wxFileOffset ofs, wxSeekMode mode = wxFromStart);

    /**
        Moves the file pointer to the specified number of bytes before the end of the
        file and returns @true on success.

        @param ofs
            Number of bytes before the end of the file.
    */
    bool SeekEnd(wxFileOffset ofs = 0);

    /**
        Returns the current position.
    */
    wxFileOffset Tell() const;

    /**
        Writes the contents of the string to the file, returns @true on success.

        The second argument is only meaningful in Unicode build of wxWidgets when
        @a conv is used to convert @a str to multibyte representation.
    */
    bool Write(const wxString& str, const wxMBConv& conv = wxConvAuto());

    /**
        Writes the specified number of bytes from a buffer.

        @param buffer
            A buffer containing the data.
        @param count
            The number of bytes to write.

        @return The number of bytes written.
    */
    size_t Write(const void* buffer, size_t count);

    /**
        Returns the file pointer associated with the file.
    */
    FILE* fp() const;
};

