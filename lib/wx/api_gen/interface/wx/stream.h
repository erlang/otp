/////////////////////////////////////////////////////////////////////////////
// Name:        stream.h
// Purpose:     interface of wxStreamBase and its derived classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    These enumeration values are returned by various functions in the context
    of wxStream classes.
*/
enum wxStreamError
{
    wxSTREAM_NO_ERROR = 0,      //!< No error occurred.
    wxSTREAM_EOF,               //!< EOF reached in Read() or similar.
    wxSTREAM_WRITE_ERROR,       //!< generic write error on the last write call.
    wxSTREAM_READ_ERROR         //!< generic read error on the last read call.
};

/**
    @class wxStreamBase

    This class is the base class of most stream related classes in wxWidgets.
    It must not be used directly.

    @library{wxbase}
    @category{streams}

    @see wxStreamBuffer
*/
class wxStreamBase
{
public:
    /**
        Creates a dummy stream object. It doesn't do anything.
    */
    wxStreamBase();

    /**
        Destructor.
    */
    virtual ~wxStreamBase();

    /**
        This function returns the last error.
    */
    wxStreamError GetLastError() const;

    /**
        Returns the length of the stream in bytes. If the length cannot be
        determined (this is always the case for socket streams for example),
        returns ::wxInvalidOffset.

        @since 2.5.4
    */
    virtual wxFileOffset GetLength() const;

    /**
        This function returns the size of the stream.
        For example, for a file it is the size of the file.

        @warning
        There are streams which do not have size by definition, such as socket
        streams. In that cases, GetSize() returns 0 so you should always test its
        return value.
    */
    virtual size_t GetSize() const;

    /**
        Returns @true if no error occurred on the stream.

        @see GetLastError()
    */
    virtual bool IsOk() const;

    /**
        Returns @true if the stream supports seeking to arbitrary offsets.
    */
    virtual bool IsSeekable() const;

    /**
        Resets the stream state.

        By default, resets the stream to good state, i.e. clears any errors.
        Since wxWidgets 2.9.3 can be also used to explicitly set the state to
        the specified error (the @a error argument didn't exist in the previous
        versions).

        @see GetLastError()
     */
    void Reset(wxStreamError error = wxSTREAM_NO_ERROR);

    /**
        Returns the opposite of IsOk().
        You can use this function to test the validity of the stream as if
        it was a pointer:

        @code
            bool DoSomething(wxInputStream& stream)
            {
                wxInt32 data;
                if (!stream.Read(&data, 4))
                    return false;
                ...
            }
        @endcode
    */
    bool operator!() const;

protected:

    /**
        Internal function.
        It is called when the stream needs to change the current position.

        @param pos
            Offset to seek to.
        @param mode
            One of the ::wxSeekMode enumeration values.

        @return The new stream position or ::wxInvalidOffset on error.
    */
    virtual wxFileOffset OnSysSeek(wxFileOffset pos, wxSeekMode mode);

    /**
        Internal function.
        It is called when the stream needs to know the real position.

        @return The current stream position.
    */
    virtual wxFileOffset OnSysTell() const;
};

/**
    @class wxStreamBuffer

    wxStreamBuffer is a cache manager for wxStreamBase: it manages a stream buffer
    linked to a stream.

    Each stream always has one autoinitialized stream buffer, but you may
    attach more of them to the same stream.

    @library{wxbase}
    @category{streams}

    @see wxStreamBase, @ref overview_stream
*/
class wxStreamBuffer
{
public:
    /** BufMode flags */
    enum BufMode
    {
        read,
        write,
        read_write
    };

    /**
        Constructor, creates a new stream buffer using @a stream as a parent stream
        and mode as the IO mode.

        @param stream
            The parent stream.
        @param mode
            Can be: wxStreamBuffer::read, wxStreamBuffer::write, wxStreamBuffer::read_write.

        One stream can have many stream buffers but only one is used internally
        to pass IO call (e.g. wxInputStream::Read() -> wxStreamBuffer::Read()),
        but you can call directly wxStreamBuffer::Read without any problems.
        Note that all errors and messages linked to the stream are stored in the
        stream, not the stream buffers:

        @code
        streambuffer.Read(...);
        streambuffer2.Read(...);
            // This call erases previous error messages set by 'streambuffer'
            // assuming that both instances are stream buffers for the same stream
        @endcode

        @see SetBufferIO()
    */
    wxStreamBuffer(wxStreamBase& stream, BufMode mode);

    /**
        Constructor for an input buffer of the specified size.

        Using it is equivalent to using the constructor above with read mode
        and calling SetBufferIO() but is more convenient.

        @since 2.9.0

        @param bufsize
            The size of buffer in bytes.
        @param stream
            The associated input stream, the buffer will be used in read mode.
     */
    wxStreamBuffer(size_t bufsize, wxInputStream& stream);

    /**
        Constructor for an output buffer of the specified size.

        Using it is equivalent to using the constructor above with write mode
        and calling SetBufferIO() but is more convenient.

        @since 2.9.0

        @param bufsize
            The size of buffer in bytes.
        @param stream
            The associated output stream, the buffer will be used in write mode.
     */
    wxStreamBuffer(size_t bufsize, wxOutputStream& stream);

    /**
        Constructor; creates a new empty stream buffer which won't flush any data
        to a stream. mode specifies the type of the buffer (read, write, read_write).

        This stream buffer has the advantage to be stream independent and to work
        only on memory buffers but it is still compatible with the rest of the
        wxStream classes. You can write, read to this special stream and it will
        grow (if it is allowed by the user) its internal buffer.
        Briefly, it has all functionality of a "normal" stream.

        @warning
        The "read_write" mode doesn't currently work for standalone stream buffers.

        @see SetBufferIO()
    */
    wxStreamBuffer(BufMode mode);

    /**
        Copy constructor.

        This method initializes the stream buffer with the data of the specified
        stream buffer. The new stream buffer has the same attributes, size, position
        and they share the same buffer. This will cause problems if the stream to
        which the stream buffer belong is destroyed and the newly cloned stream
        buffer continues to be used, trying to call functions in the (destroyed)
        stream. It is advised to use this feature only in very local area of the
        program.
    */
    wxStreamBuffer(const wxStreamBuffer& buffer);

    /**
        Destructor.
        It finalizes all IO calls and frees all internal buffers if necessary.
    */
    ~wxStreamBuffer();

    /**
        Fill the IO buffer.
    */
    bool FillBuffer();

    /**
        Toggles the fixed flag. Usually this flag is toggled at the same time as
        @e flushable. This flag allows (when it has the @false value) or forbids
        (when it has the @true value) the stream buffer to resize dynamically the
        IO buffer.

        @see SetBufferIO()
    */
    void Fixed(bool fixed);

    /**
        Flushes the IO buffer.
    */
    bool FlushBuffer();

    /**
        Toggles the flushable flag.
        If @a flushable is disabled, no data are sent to the parent stream.
    */
    void Flushable(bool flushable);

    /**
        Returns a pointer on the end of the stream buffer.
    */
    void* GetBufferEnd() const;

    /**
        Returns a pointer on the current position of the stream buffer.
    */
    void* GetBufferPos() const;

    /**
        Returns the size of the buffer.
    */
    size_t GetBufferSize() const;

    /**
        Returns a pointer on the start of the stream buffer.
    */
    void* GetBufferStart() const;

    /**
        Gets a single char from the stream buffer. It acts like the Read() call.

        @warning
        You aren't directly notified if an error occurred during the IO call.

        @see Read()
    */
    virtual char GetChar();

    /**
        Returns the amount of available data in the buffer.
    */
    size_t GetDataLeft();

    /**
        Returns the current position (counted in bytes) in the stream buffer.
    */
    size_t GetIntPosition() const;

    /**
        Returns the amount of bytes read during the last IO call to the parent stream.
    */
    size_t GetLastAccess() const;

    /**
        Puts a single char to the stream buffer.

        @warning
        You aren't directly notified if an error occurred during the IO call.

        @see Read()
    */
    virtual void PutChar(char c);

    /**
        Reads a block of the specified size and stores the data in buffer.
        This function tries to read from the buffer first and if more data has
        been requested, reads more data from the associated stream and updates
        the buffer accordingly until all requested data is read.

        @return It returns the size of the data read. If the returned size is
                different of the specified size, an error has occurred and
                should be tested using GetLastError().
    */
    virtual size_t Read(void* buffer, size_t size);

    /**
        Copies data to @a buffer.
        The function returns when @a buffer is full or when there isn't
        any more data in the current buffer.

        @see Write()
    */
    size_t Read(wxStreamBuffer* buffer);

    /**
        Resets to the initial state variables concerning the buffer.
    */
    void ResetBuffer();

    /**
        Changes the current position.
        Parameter @a mode may be one of the following:

        - @b wxFromStart: The position is counted from the start of the stream.
        - @b wxFromCurrent: The position is counted from the current position of the stream.
        - @b wxFromEnd: The position is counted from the end of the stream.

        @return Upon successful completion, it returns the new offset as
                measured in bytes from the beginning of the stream.
                Otherwise, it returns ::wxInvalidOffset.
    */
    virtual wxFileOffset Seek(wxFileOffset pos, wxSeekMode mode);

    /**
        Specifies which pointers to use for stream buffering.
        You need to pass a pointer on the start of the buffer end and another
        on the end. The object will use this buffer to cache stream data.
        It may be used also as a source/destination buffer when you create an
        empty stream buffer (See wxStreamBuffer::wxStreamBuffer).

        @remarks
        When you use this function, you will have to destroy the IO buffers
        yourself after the stream buffer is destroyed or don't use it anymore.
        In the case you use it with an empty buffer, the stream buffer will not
        resize it when it is full.

        @see wxStreamBuffer(), Fixed(), Flushable()
    */
    void SetBufferIO(void* start, void* end, bool takeOwnership = false);

    /**
        Destroys or invalidates the previous IO buffer and allocates a new one of the
        specified size.

        @warning
        All previous pointers aren't valid anymore.

        @remarks
        The created IO buffer is growable by the object.

        @see Fixed(), Flushable()
    */
    void SetBufferIO(size_t bufsize);

    /**
        Sets the current position (in bytes) in the stream buffer.

        @warning
        Since it is a very low-level function, there is no check on the position:
        specifying an invalid position can induce unexpected results.
    */
    void SetIntPosition(size_t pos);

    /**
        Returns the parent stream of the stream buffer.
        @deprecated use GetStream() instead
    */
    wxStreamBase* Stream();

    /**
        Gets the current position in the stream. This position is calculated from
        the @e real position in the stream and from the internal buffer position: so
        it gives you the position in the @e real stream counted from the start of
        the stream.

        @return Returns the current position in the stream if possible,
                ::wxInvalidOffset in the other case.
    */
    virtual wxFileOffset Tell() const;

    /**
        Truncates the buffer to the current position.

        @note Truncate() cannot be used to enlarge the buffer. This is
              usually not needed since the buffer expands automatically.
    */
    void Truncate();

    /**
        Writes a block of the specified size using data of buffer.
        The data are cached in a buffer before being sent in one block to the stream.
    */
    virtual size_t Write(const void* buffer, size_t size);

    /**
        See Read().
    */
    size_t Write(wxStreamBuffer* buffer);
};



/**
    @class wxOutputStream

    wxOutputStream is an abstract base class which may not be used directly.
    It is the base class of all streams which provide a Write() function,
    i.e. which can be used to output data (e.g. to a file, to a socket, etc).

    If you want to create your own output stream, you'll need to derive from this
    class and implement the protected OnSysWrite() function only.

    @library{wxbase}
    @category{streams}
*/
class wxOutputStream : public wxStreamBase
{
public:
    /**
        Creates a dummy wxOutputStream object.
    */
    wxOutputStream();

    /**
        Destructor.
    */
    virtual ~wxOutputStream();

    /**
        Closes the stream, returning @false if an error occurs.
        The stream is closed implicitly in the destructor if Close() is not
        called explicitly.

        If this stream wraps another stream or some other resource such
        as a file, then the underlying resource is closed too if it is owned
        by this stream, or left open otherwise.
    */
    virtual bool Close();

    /**
        Returns the number of bytes written during the last Write().
        It may return 0 even if there is no error on the stream if it is
        only temporarily impossible to write to it.
    */
    virtual size_t LastWrite() const;

    /**
        Puts the specified character in the output queue and increments the
        stream position.
    */
    void PutC(char c);

    /**
        Changes the stream current position.

        @param pos
            Offset to seek to.
        @param mode
            One of wxFromStart, wxFromEnd, wxFromCurrent.

        @return The new stream position or ::wxInvalidOffset on error.
    */
    virtual wxFileOffset SeekO(wxFileOffset pos, wxSeekMode mode = wxFromStart);

    /**
        Returns the current stream position.
    */
    virtual wxFileOffset TellO() const;

    /**
        Writes up to the specified amount of bytes using the data of buffer.
        Note that not all data can always be written so you must check the number
        of bytes really written to the stream using LastWrite() when this function
        returns.

        In some cases (for example a write end of a pipe which is currently full)
        it is even possible that there is no errors and zero bytes have been written.
        This function returns a reference on the current object, so the user can
        test any states of the stream right away.
    */
    virtual wxOutputStream& Write(const void* buffer, size_t size);

    /**
        Reads data from the specified input stream and stores them
        in the current stream. The data is read until an error is raised
        by one of the two streams.
    */
    wxOutputStream& Write(wxInputStream& stream_in);

    /**
        Writes exactly the specified number of bytes from the buffer.

        Returns @true if exactly @a size bytes were written. Otherwise, returns
        @false and LastWrite() should be used to retrieve the exact amount of
        the data written if necessary.

        This method uses repeated calls to Write() (which may return writing
        only part of the data) if necessary.

        @since 2.9.5
    */
    bool WriteAll(const void* buffer, size_t size);

protected:
    /**
        Internal function. It is called when the stream wants to write data of the
        specified size @a bufsize into the given @a buffer.

        It should return the size that was actually wrote (which maybe zero if
        @a bufsize is zero or if an error occurred; in this last case the internal
        variable @c m_lasterror should be appropriately set).
    */
    size_t OnSysWrite(const void* buffer, size_t bufsize);
};


/**
    @class wxInputStream

    wxInputStream is an abstract base class which may not be used directly.
    It is the base class of all streams which provide a Read() function,
    i.e. which can be used to read data from a source (e.g. a file, a socket, etc).

    If you want to create your own input stream, you'll need to derive from this
    class and implement the protected OnSysRead() function only.

    @library{wxbase}
    @category{streams}
*/
class wxInputStream : public wxStreamBase
{
public:
    /**
        Creates a dummy input stream.
    */
    wxInputStream();

    /**
        Destructor.
    */
    virtual ~wxInputStream();

    /**
        Returns @true if some data is available in the stream right now, so that
        calling Read() wouldn't block.
    */
    virtual bool CanRead() const;

    /**
        Returns @true after an attempt has been made to read past the end of the
        stream.
    */
    virtual bool Eof() const;

    /**
        Returns the first character in the input queue and removes it,
        blocking until it appears if necessary.

        On success returns a value between 0 - 255; on end of file returns @c wxEOF.
    */
    int GetC();

    /**
        Returns the last number of bytes read.
    */
    virtual size_t LastRead() const;

    /**
        Returns the first character in the input queue without removing it.
    */
    virtual char Peek();

    /**
        Reads the specified amount of bytes and stores the data in buffer.
        To check if the call was successful you must use LastRead() to check
        if this call did actually read @a size bytes (if it didn't, GetLastError()
        should return a meaningful value).

        @warning
        The buffer absolutely needs to have at least the specified size.

        @return This function returns a reference on the current object, so the
                user can test any states of the stream right away.
    */
    virtual wxInputStream& Read(void* buffer, size_t size);

    /**
        Reads data from the input queue and stores it in the specified output stream.
        The data is read until an error is raised by one of the two streams.

        @return This function returns a reference on the current object, so the
                user can test any states of the stream right away.
    */
    wxInputStream& Read(wxOutputStream& stream_out);

    /**
        Reads exactly the specified number of bytes into the buffer.

        Returns @true only if the entire amount of data was read, otherwise
        @false is returned and the number of bytes really read can be retrieved
        using LastRead(), as with Read().

        This method uses repeated calls to Read() (which may return after
        reading less than the requested number of bytes) if necessary.

        @warning
        The buffer absolutely needs to have at least the specified size.

        @since 2.9.5
    */
    bool ReadAll(void* buffer, size_t size);

    /**
        Changes the stream current position.

        This operation in general is possible only for seekable streams
        (see wxStreamBase::IsSeekable()); non-seekable streams support only
        seeking positive amounts in mode @c wxFromCurrent (this is implemented
        by reading data and simply discarding it).

        @param pos
            Offset to seek to.
        @param mode
            One of wxFromStart, wxFromEnd, wxFromCurrent.

        @return The new stream position or ::wxInvalidOffset on error.
    */
    virtual wxFileOffset SeekI(wxFileOffset pos, wxSeekMode mode = wxFromStart);

    /**
        Returns the current stream position or ::wxInvalidOffset if it's not
        available (e.g. socket streams do not have a size nor a current stream
        position).
    */
    virtual wxFileOffset TellI() const;

    /**
        This function is only useful in read mode.
        It is the manager of the "Write-Back" buffer. This buffer acts like a
        temporary buffer where data which has to be read during the next read IO
        call are put. This is useful when you get a big block of data which you
        didn't want to read: you can replace them at the top of the input queue
        by this way.

        Be very careful about this call in connection with calling SeekI() on
        the same stream. Any call to SeekI() will invalidate any previous call
        to this method (otherwise you could SeekI() to one position, "unread" a
        few bytes there, SeekI() to another position and data would be either
        lost or corrupted).

        @return Returns the amount of bytes saved in the Write-Back buffer.
    */
    size_t Ungetch(const void* buffer, size_t size);

    /**
        This function acts like the previous one except that it takes only one
        character: it is sometimes shorter to use than the generic function.
    */
    bool Ungetch(char c);

protected:

    /**
        Internal function. It is called when the stream wants to read data of the
        specified size @a bufsize and wants it to be placed inside @a buffer.

        It should return the size that was actually read or zero if EOF has been
        reached or an error occurred (in this last case the internal @c m_lasterror
        variable should be set accordingly as well).
    */
    size_t OnSysRead(void* buffer, size_t bufsize) = 0;
};




/**
    @class wxCountingOutputStream

    wxCountingOutputStream is a specialized output stream which does not write any
    data anywhere, instead it counts how many bytes would get written if this were a
    normal stream. This can sometimes be useful or required if some data gets
    serialized to a stream or compressed by using stream compression and thus the
    final size of the stream cannot be known other than pretending to write the stream.
    One case where the resulting size would have to be known is if the data has
    to be written to a piece of memory and the memory has to be allocated before
    writing to it (which is probably always the case when writing to a memory stream).

    @library{wxbase}
    @category{streams}
*/
class wxCountingOutputStream : public wxOutputStream
{
public:
    /**
        Creates a wxCountingOutputStream object.
    */
    wxCountingOutputStream();

    /**
        Destructor.
    */
    virtual ~wxCountingOutputStream();

    /**
        Returns the current length of the stream.

        This is the amount of data written to the stream so far, in bytes.
    */
    virtual wxFileOffset GetLength() const;
};


/**
    @class wxBufferedInputStream

    This stream acts as a cache. It caches the bytes read from the specified
    input stream (see wxFilterInputStream).
    It uses wxStreamBuffer and sets the default in-buffer size to 1024 bytes.
    This class may not be used without some other stream to read the data
    from (such as a file stream or a memory stream).

    @library{wxbase}
    @category{streams}

    @see wxStreamBuffer, wxInputStream, wxBufferedOutputStream
*/
class wxBufferedInputStream : public wxFilterInputStream
{
public:
    /**
        Constructor using the provided buffer or default.

        @param stream
            The associated low-level stream.
        @param buffer
            The buffer to use if non-@NULL. Notice that the ownership of this
            buffer is taken by the stream, i.e. it will delete it. If this
            parameter is @NULL a default 1KB buffer is used.
    */
    wxBufferedInputStream(wxInputStream& stream,
                          wxStreamBuffer *buffer = NULL);

    /**
        Constructor allowing to specify the size of the buffer.

        This is just a more convenient alternative to creating a wxStreamBuffer
        of the given size and using the other overloaded constructor of this
        class.

        @param stream
            The associated low-level stream.
        @param bufsize
            The size of the buffer, in bytes.

        @since 2.9.0
     */
    wxBufferedInputStream(wxInputStream& stream, size_t bufsize);

    /**
        Destructor.
    */
    virtual ~wxBufferedInputStream();
};




/**
    Enumeration values used by wxFilterClassFactory.
*/
enum wxStreamProtocolType
{
    wxSTREAM_PROTOCOL,  //!< wxFileSystem protocol (should be only one).
    wxSTREAM_MIMETYPE,  //!< MIME types the stream handles.
    wxSTREAM_ENCODING,  //!< The HTTP Content-Encodings the stream handles.
    wxSTREAM_FILEEXT    //!< File extensions the stream handles.
};

/**
    @class wxFilterClassFactory

    Allows the creation of filter streams to handle compression formats such
    as gzip and bzip2.

    For example, given a filename you can search for a factory that will
    handle it and create a stream to decompress it:

    @code
        factory = wxFilterClassFactory::Find(filename, wxSTREAM_FILEEXT);
        if (factory)
            stream = factory->NewStream(new wxFFileInputStream(filename));
    @endcode

    wxFilterClassFactory::Find can also search for a factory by MIME type,
    HTTP encoding or by wxFileSystem protocol.
    The available factories can be enumerated using wxFilterClassFactory::GetFirst()
    and wxFilterClassFactory::GetNext().

    @library{wxbase}
    @category{streams}

    @see wxFilterInputStream, wxFilterOutputStream, wxArchiveClassFactory,
        @ref overview_archive
*/
class wxFilterClassFactory : public wxObject
{
public:
    /**
        Returns @true if this factory can handle the given protocol, MIME type, HTTP
        encoding or file extension.

        When using @c wxSTREAM_FILEEXT for the second parameter, the first parameter
        can be a complete filename rather than just an extension.
    */
    bool CanHandle(const wxString& protocol,
                   wxStreamProtocolType type = wxSTREAM_PROTOCOL) const;

    /**
        A static member that finds a factory that can handle a given protocol, MIME
        type, HTTP encoding or file extension. Returns a pointer to the class
        factory if found, or @NULL otherwise.
        It does not give away ownership of the factory.

        When using @c wxSTREAM_FILEEXT for the second parameter, the first parameter
        can be a complete filename rather than just an extension.
    */
    static const wxFilterClassFactory* Find(const wxString& protocol,
                                            wxStreamProtocolType type = wxSTREAM_PROTOCOL);

    //@{
    /**
        GetFirst and GetNext can be used to enumerate the available factories.
        For example, to list them:

        @code
        wxString list;
        const wxFilterClassFactory *factory = wxFilterClassFactory::GetFirst();

        while (factory) {
            list << factory->GetProtocol() << wxT("\n");
            factory = factory->GetNext();
        }
        @endcode

        GetFirst()/GetNext() return a pointer to a factory or @NULL if no more
        are available. They do not give away ownership of the factory.
    */
    static const wxFilterClassFactory* GetFirst();
    const wxFilterClassFactory* GetNext() const;
    //@}

    /**
        Returns the wxFileSystem protocol supported by this factory.
        Equivalent to @code wxString(*GetProtocols()) @endcode.
    */
    wxString GetProtocol() const;

    /**
        Returns the protocols, MIME types, HTTP encodings or file extensions
        supported by this factory, as an array of null terminated strings.
        It does not give away ownership of the array or strings.

        For example, to list the file extensions a factory supports:

        @code
        wxString list;
        const wxChar *const *p;

        for (p = factory->GetProtocols(wxSTREAM_FILEEXT); *p; p++)
            list << *p << wxT("\n");
        @endcode
    */
    virtual const wxChar * const* GetProtocols(wxStreamProtocolType type = wxSTREAM_PROTOCOL) const = 0;

    //@{
    /**
        Create a new input or output stream to decompress or compress a given stream.

        If the parent stream is passed as a pointer then the new filter stream
        takes ownership of it. If it is passed by reference then it does not.
    */
    virtual wxFilterInputStream*  NewStream(wxInputStream& stream) const = 0;
    virtual wxFilterOutputStream* NewStream(wxOutputStream& stream) const = 0;
    virtual wxFilterInputStream*  NewStream(wxInputStream* stream) const = 0;
    virtual wxFilterOutputStream* NewStream(wxOutputStream* stream) const = 0;
    //@}

    /**
        Remove the file extension of @a location if it is one of the file
        extensions handled by this factory.
    */
    wxString PopExtension(const wxString& location) const;

    /**
        Adds this class factory to the list returned  by GetFirst()/GetNext().

        It is not necessary to do this to use the filter streams. It is usually
        used when implementing streams, typically the implementation will
        add a static instance of its factory class.

        It can also be used to change the order of a factory already in the list,
        bringing it to the front. This isn't a thread safe operation so can't be
        done when other threads are running that will be using the list.

        The list does not take ownership of the factory.
    */
    void PushFront();

    /**
        Removes this class factory from the list returned by GetFirst()/GetNext().
        Removing from the list isn't a thread safe operation so can't be done
        when other threads are running that will be using the list.

        The list does not own the factories, so removing a factory does not delete it.
    */
    void Remove();
};



/**
    @class wxFilterOutputStream

    A filter stream has the capability of a normal stream but it can be placed
    on top of another stream. So, for example, it can compress, encrypt the data
    which are passed to it and write them to another stream.

    @note
    The use of this class is exactly the same as of wxOutputStream.
    Only a constructor differs and it is documented below.

    @library{wxbase}
    @category{streams}

    @see wxFilterClassFactory, wxFilterInputStream
*/
class wxFilterOutputStream : public wxOutputStream
{
public:
    //@{
    /**
        Initializes a "filter" stream.

        If the parent stream is passed as a pointer then the new filter stream
        takes ownership of it. If it is passed by reference then it does not.
    */
    wxFilterOutputStream(wxOutputStream& stream);
    wxFilterOutputStream(wxOutputStream* stream);
    //@}
};



/**
    @class wxFilterInputStream

    A filter stream has the capability of a normal stream but it can be placed on
    top of another stream. So, for example, it can uncompress or decrypt the data which
    are read from another stream and pass it to the requester.

    @note
    The interface of this class is the same as that of wxInputStream.
    Only a constructor differs and it is documented below.

    @library{wxbase}
    @category{streams}

    @see wxFilterClassFactory, wxFilterOutputStream
*/
class wxFilterInputStream : public wxInputStream
{
public:
    //@{
    /**
        Initializes a "filter" stream.

        If the parent stream is passed as a pointer then the new filter stream
        takes ownership of it. If it is passed by reference then it does not.
    */
    wxFilterInputStream(wxInputStream& stream);
    wxFilterInputStream(wxInputStream* stream);
    //@}
};



/**
    @class wxBufferedOutputStream

    This stream acts as a cache. It caches the bytes to be written to the specified
    output stream (See wxFilterOutputStream). The data is only written when the
    cache is full, when the buffered stream is destroyed or when calling SeekO().

    This class may not be used without some other stream to write the data
    to (such as a file stream or a memory stream).

    @library{wxbase}
    @category{streams}

    @see wxStreamBuffer, wxOutputStream
*/
class wxBufferedOutputStream : public wxFilterOutputStream
{
public:
    /**
        Constructor using the provided buffer or default.

        @param stream
            The associated low-level stream.
        @param buffer
            The buffer to use if non-@NULL. Notice that the ownership of this
            buffer is taken by the stream, i.e. it will delete it. If this
            parameter is @NULL a default 1KB buffer is used.
    */
    wxBufferedOutputStream(wxOutputStream& stream,
                           wxStreamBuffer *buffer = NULL);

    /**
        Constructor allowing to specify the size of the buffer.

        This is just a more convenient alternative to creating a wxStreamBuffer
        of the given size and using the other overloaded constructor of this
        class.

        @param stream
            The associated low-level stream.
        @param bufsize
            The size of the buffer, in bytes.

        @since 2.9.0
     */
    wxBufferedOutputStream(wxOutputStream& stream, size_t bufsize);

    /**
        Destructor. Calls Sync() and destroys the internal buffer.
    */
    virtual ~wxBufferedOutputStream();

    /**
        Calls Sync() and changes the stream position.
    */
    virtual wxFileOffset SeekO(wxFileOffset pos, wxSeekMode mode = wxFromStart);

    /**
        Flushes the buffer and calls Sync() on the parent stream.
    */
    virtual void Sync();
};


/**
    @class wxWrapperInputStream

    A wrapper input stream is a kind of filter stream which forwards all the
    operations to its base stream. This is useful to build utility classes such
    as wxFSInputStream.

    @note
    The interface of this class is the same as that of wxInputStream.
    Only a constructor differs and it is documented below.

    @library{wxbase}
    @category{streams}

    @see wxFSInputStream, wxFilterInputStream
    @since 2.9.4
*/
class wxWrapperInputStream : public wxFilterInputStream
{
public:
    //@{
    /**
        Initializes a wrapper stream.

        If the parent stream is passed as a pointer then the new wrapper stream
        takes ownership of it. If it is passed by reference then it does not.
    */
    wxWrapperInputStream(wxInputStream& stream);
    wxWrapperInputStream(wxInputStream* stream);
    //@}

protected:
    /**
        Default constructor, use InitParentStream() to finish initialization.

        This constructor can be used by the derived classes from their own
        constructors when the parent stream can't be specified immediately.
        The derived class must call InitParentStream() later to do it.
     */
    wxWrapperInputStream();

    //@{
    /**
        Set up the wrapped stream for an object initialized using the default
        constructor.

        The ownership logic is the same as for the non-default constructor,
        i.e. this object takes ownership of the stream if it's passed by
        pointer but not if it's passed by reference.
     */
    void InitParentStream(wxInputStream& stream);
    void InitParentStream(wxInputStream* stream);
    //@}
};
