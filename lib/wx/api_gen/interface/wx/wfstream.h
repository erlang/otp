/////////////////////////////////////////////////////////////////////////////
// Name:        wfstream.h
// Purpose:     interface of wxTempFileOutputStream
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxTempFileOutputStream

    wxTempFileOutputStream is an output stream based on wxTempFile.
    It provides a relatively safe way to replace the contents of the
    existing file.

    @library{wxbase}
    @category{streams}

    @see wxTempFile
*/
class wxTempFileOutputStream : public wxOutputStream
{
public:
    /**
        Associates wxTempFileOutputStream with the file to be replaced and opens it.

        @warning
        You should use wxStreamBase::IsOk() to verify if the constructor succeeded.

        Call Commit() or wxOutputStream::Close() to replace the old file and close
        this one. Calling Discard() (or allowing the destructor to do it) will
        discard the changes.
    */
    wxTempFileOutputStream(const wxString& fileName);

    /**
        Validate changes: deletes the old file of the given name and renames the new
        file to the old name. Returns @true if both actions succeeded.

        If @false is returned it may unfortunately mean two quite different things: either that
        either the old file couldn't be deleted or that the new file couldn't be renamed
        to the old name.
    */
    virtual bool Commit();

    /**
        Discard changes: the old file contents are not changed, the temporary file is
        deleted.
    */
    virtual void Discard();
};



/**
    @class wxTempFFileOutputStream

    wxTempFFileOutputStream is an output stream based on wxTempFFile.
    It provides a relatively safe way to replace the contents of the
    existing file.

    @since 3.1.4

    @library{wxbase}
    @category{streams}

    @see wxTempFFile
*/
class wxTempFFileOutputStream : public wxOutputStream
{
public:
    /**
        Associates wxTempFFileOutputStream with the file to be replaced and opens it.

        @warning
        You should use wxStreamBase::IsOk() to verify if the constructor succeeded.

        Call Commit() or wxOutputStream::Close() to replace the old file and close
        this one. Calling Discard() (or allowing the destructor to do it) will
        discard the changes.
    */
    wxTempFFileOutputStream(const wxString& fileName);

    /**
        Validate changes: deletes the old file of the given name and renames the new
        file to the old name. Returns @true if both actions succeeded.

        If @false is returned it may unfortunately mean two quite different things: either that
        either the old file couldn't be deleted or that the new file couldn't be renamed
        to the old name.
    */
    virtual bool Commit();

    /**
        Discard changes: the old file contents are not changed, the temporary file is
        deleted.
    */
    virtual void Discard();
};



/**
    @class wxFFileOutputStream

    This class represents data written to a file.
    There are actually two such groups of classes: this one is based on wxFFile
    whereas wxFileOutputStream is based in the wxFile class.

    Note that wxOutputStream::SeekO() can seek beyond the end of the stream
    (file) and will thus not return ::wxInvalidOffset for that.

    @library{wxbase}
    @category{streams}

    @see wxBufferedOutputStream, wxFFileInputStream, wxFileOutputStream, wxFileInputStream
*/
class wxFFileOutputStream : public wxOutputStream
{
public:
    /**
        Open the given file @a filename with mode @a mode.

        @warning
        You should use wxStreamBase::IsOk() to verify if the constructor succeeded.
    */
    wxFFileOutputStream(const wxString& filename,
                        const wxString& mode = "wb");

    /**
        Initializes a file stream in write-only mode using the file I/O object file.
    */
    wxFFileOutputStream(wxFFile& file);

    /**
        Initializes a file stream in write-only mode using the file descriptor fp.
    */
    wxFFileOutputStream(FILE* fp);

    /**
        Destructor.
    */
    virtual ~wxFFileOutputStream();

    /**
        Returns @true if the stream is initialized and ready.
    */
    bool IsOk() const;

    /**
        Returns the underlying file object.
        @since 2.9.5
    */
    wxFFile* GetFile() const;
};



/**
    @class wxFileOutputStream

    This class represents data written to a file.
    There are actually two such groups of classes: this one is based on wxFile
    whereas wxFFileOutputStream is based in the wxFFile class.

    Note that wxOutputStream::SeekO() can seek beyond the end of the stream
    (file) and will thus not return ::wxInvalidOffset for that.

    @library{wxbase}
    @category{streams}

    @see wxBufferedOutputStream, wxFileInputStream, wxFFileOutputStream, wxFFileInputStream
*/
class wxFileOutputStream : public wxOutputStream
{
public:
    /**
        Creates a new file with @a ofileName name and initializes the stream in write-only mode.

        @warning
        You should use wxStreamBase::IsOk() to verify if the constructor succeeded.
    */
    wxFileOutputStream(const wxString& ofileName);

    /**
        Initializes a file stream in write-only mode using the file I/O object file.
    */
    wxFileOutputStream(wxFile& file);

    /**
        Initializes a file stream in write-only mode using the file descriptor @e fd.
    */
    wxFileOutputStream(int fd);

    /**
        Destructor.
    */
    virtual ~wxFileOutputStream();

    /**
        Returns @true if the stream is initialized and ready.
    */
    bool IsOk() const;

    /**
        Returns the underlying file object.
        @since 2.9.5
    */
    wxFile* GetFile() const;
};



/**
    @class wxFileInputStream

    This class represents data read in from a file.
    There are actually two such groups of classes: this one is based on wxFile
    whereas wxFFileInputStream is based in the wxFFile class.

    Note that wxInputStream::SeekI() can seek beyond the end of the stream (file)
    and will thus not return ::wxInvalidOffset for that.

    @library{wxbase}
    @category{streams}

    @see wxBufferedInputStream, wxFileOutputStream, wxFFileOutputStream
*/
class wxFileInputStream : public wxInputStream
{
public:
    /**
        Opens the specified file using its @a ifileName name in read-only mode.

        @warning
        You should use wxStreamBase::IsOk() to verify if the constructor succeeded.
    */
    wxFileInputStream(const wxString& ifileName);

    /**
        Initializes a file stream in read-only mode using the file I/O object file.
    */
    wxFileInputStream(wxFile& file);

    /**
        Initializes a file stream in read-only mode using the specified file descriptor.
    */
    wxFileInputStream(int fd);

    /**
        Destructor.
    */
    virtual ~wxFileInputStream();

    /**
        Returns @true if the stream is initialized and ready.
    */
    bool IsOk() const;

    /**
        Returns the underlying file object.
        @since 2.9.5
    */
    wxFile* GetFile() const;
};



/**
    @class wxFFileInputStream

    This class represents data read in from a file.
    There are actually two such groups of classes: this one is based on wxFFile
    whereas wxFileInputStream is based in the wxFile class.

    Note that wxInputStream::SeekI() can seek beyond the end of the stream (file)
    and will thus not return ::wxInvalidOffset for that.

    @library{wxbase}
    @category{streams}

    @see wxBufferedInputStream, wxFFileOutputStream, wxFileOutputStream
*/
class wxFFileInputStream : public wxInputStream
{
public:
    /**
        Opens the specified file using its @a filename name using the specified @a mode.

        @warning
        You should use wxStreamBase::IsOk() to verify if the constructor succeeded.
    */
    wxFFileInputStream(const wxString& filename,
                       const wxString& mode = "rb");

    /**
        Initializes a file stream in read-only mode using the file I/O object file.
    */
    wxFFileInputStream(wxFFile& file);

    /**
        Initializes a file stream in read-only mode using the specified file pointer @a fp.
    */
    wxFFileInputStream(FILE* fp);

    /**
        Destructor.
    */
    virtual ~wxFFileInputStream();

    /**
        Returns @true if the stream is initialized and ready.
    */
    bool IsOk() const;

    /**
        Returns the underlying file object.
        @since 2.9.5
    */
    wxFFile* GetFile() const;
};



/**
    @class wxFFileStream

    This stream allows both reading from and writing to a file using buffered
    STDIO functions.

    @library{wxbase}
    @category{streams}

    @see wxFFileInputStream, wxFFileOutputStream, wxFileStream
*/
class wxFFileStream : public wxFFileInputStream,
                      public wxFFileOutputStream
{
public:
    /**
        Initializes a new file stream in the given @a mode using the specified
        @a iofileName name.

        @warning
        You should use wxStreamBase::IsOk() to verify if the constructor succeeded.
    */
    wxFFileStream(const wxString& iofileName, const wxString& mode = "w+b");

    /**
        Returns @true if the stream is initialized and ready.

        This method returns @true if the stream can be both read from and
        written to.
    */
    bool IsOk() const;
};



/**
    @class wxFileStream

    This class represents data that can be both read from and written to a file.
    There are actually two such groups of classes: this one is based on wxFile
    whereas wxFFileStream is based in the wxFFile class.

    @library{wxbase}
    @category{streams}

    @see wxFileInputStream, wxFileOutputStream, wxFFileStream
*/
class wxFileStream : public wxFileOutputStream,
                     public wxFileInputStream
{
public:
    /**
        Initializes a new file stream in read-write mode using the specified
        @a iofileName name.

        @warning
        You should use IsOk() to verify if the constructor succeeded.
    */
    wxFileStream(const wxString& iofileName);

    /**
        Returns @true if the stream is initialized and ready.

        This method returns @true if the stream can be both read from and
        written to.
    */
    bool IsOk() const;
};

