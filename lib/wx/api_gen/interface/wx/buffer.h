/////////////////////////////////////////////////////////////////////////////
// Name:        buffer.h
// Purpose:     interface of wxMemoryBuffer
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    wxScopedCharTypeBuffer<T> is a template class for storing characters.

    Data are stored in reference-counted buffer. In other words, making a copy
    of wxScopedCharTypeBuffer<T> will @em not make another copy of the stored
    string data, it will still point to the same location in memory.

    wxScopedCharTypeBuffer<T> supports two storage modes: owned and non-owned.
    "Owned" data buffer (created with CreateOwned() or wxCharTypeBuffer<T>
    derived class) owns the data and frees them when the last buffer pointing
    to them is destroyed.

    "Non-owned" buffer (created with CreateNonOwned()), on the other hand,
    references data owned by somebody else -- typical use is by
    wxString::mb_str() or wxString::wc_str(), which may return non-owned buffer
    pointing to wxString's internal store.

    Because of this, the validity of data stored in wxScopedCharTypeBuffer<T>
    is limited by the lifetime of the "parent" object that created the
    buffer (e.g. the wxString on which mb_str() was called).

    If you need to preserve the data for longer, assign it to
    wxCharTypeBuffer<T> instead of wxScopedCharTypeBuffer<T>. On the other
    hand, use wxScopedCharTypeBuffer<T> if the buffer is to be destroyed before
    the "parent" object -- typical use would be creating it on the stack and
    destroying when it goes out of scope (hence the class' name).

    @tparam T
        The type of the characters stored in this class.

    @since 2.9.0

    @nolibrary
    @category{data}
*/
template <typename T>
class wxScopedCharTypeBuffer
{
public:
    /// Stored characters type.
    typedef T CharType;

    /// Default constructor, creates NULL buffer.
    wxScopedCharTypeBuffer();

    /**
        Creates non-owned buffer from string data @a str.

        The buffer's destructor will not destroy @a str. The returned buffer's
        data is valid only as long as @a str is valid.

        @param str String data.
        @param len If specified, length of the string, otherwise the string
                   is considered to be NUL-terminated.
     */
    static const wxScopedCharTypeBuffer CreateNonOwned(const CharType *str, size_t len = wxNO_LEN);

    /**
        Creates owned buffer from @a str and takes ownership of it.

        The buffer's destructor will free @a str when its reference count
        reaches zero (initial count is 1).

        @param str String data.
        @param len If specified, length of the string, otherwise the string
                   is considered to be NUL-terminated.
     */
    static const wxScopedCharTypeBuffer CreateOwned(CharType *str, size_t len = wxNO_LEN);

    /**
        Copy constructor.

        Increases reference count on the data, does @em not make wxStrdup()
        copy of the data.
     */
    wxScopedCharTypeBuffer(const wxScopedCharTypeBuffer& src);

    /// Assignment operator behaves in the same way as the copy constructor.
    wxScopedCharTypeBuffer& operator=(const wxScopedCharTypeBuffer& src);

    /**
        Destructor. Frees stored data if it is in "owned" mode and data's
        reference count reaches zero.
     */
    ~wxScopedCharTypeBuffer();

    /**
        Returns the internal pointer and resets the buffer.

        It's the caller responsibility to deallocate the returned pointer using
        @c free() function.

        Notice that this method is dangerous because it can only be called on a
        non-shared owning buffer. Calling it on any other kind of buffer object
        will result in a crash after the pointer is freed, so avoid using it
        unless absolutely necessary and you are absolutely certain that the
        buffer is not shared.
     */
    CharType* release() const;

    /// Resets the buffer to NULL, freeing the data if necessary.
    void reset();

    /// Returns pointer to the stored data.
    CharType *data();

    /// Returns const pointer to the stored data.
    const CharType *data() const;

    /// Returns length of the string stored.
    size_t length() const;

    /// Implicit conversion to C string.
    operator const CharType *() const;

    /// Random access to the stored C string.
    CharType operator[](size_t n) const;
};

/// Scoped char buffer.
typedef wxScopedCharTypeBuffer<char> wxScopedCharBuffer;

/// Scoped wchar_t buffer.
typedef wxScopedCharTypeBuffer<wchar_t> wxScopedWCharBuffer;

/**
    wxCharTypeBuffer<T> is a template class for storing characters.

    The difference from wxScopedCharTypeBuffer<T> is that this class
    doesn't have non-owned mode and the data stored in it are valid for
    as long as the buffer instance exists. Other than that, this class'
    behaviour is the same as wxScopedCharTypeBuffer<T>'s -- in particular,
    the data are reference-counted and copying the buffer is cheap.

    wxScopedCharTypeBuffer<T> buffers can be converted into wxCharTypeBuffer<T>.

    @tparam T
        The type of the characters stored in this class.

    @since 2.9.0

    @nolibrary
    @category{data}
*/
template <typename T>
class wxCharTypeBuffer : public wxScopedCharTypeBuffer<T>
{
public:
    /**
        Creates (owned) buffer from @a str and takes ownership of it.

        @param str String data.
        @param len If specified, length of the string, otherwise the string
                   is considered to be NUL-terminated.

        @see wxScopedCharTypeBuffer<T>::CreateOwned()
     */
    wxCharTypeBuffer(const CharType *str = NULL, size_t len = wxNO_LEN);


    /**
        Creates (owned) buffer of size @a len.

        @see wxScopedCharTypeBuffer<T>::CreateOwned()
     */
    wxCharTypeBuffer(size_t len);

    /**
        Copy constructor.

        Increases reference count on the data, does @em not make wxStrdup()
        copy of the data.
     */
    wxCharTypeBuffer(const wxCharTypeBuffer& src);

    /**
        Makes a copy of scoped buffer @a src.

        If @a src is a non-owned buffer, a copy of its data is made using
        wxStrdup(). If @a src is an owned buffer, this constructor behaves
        in the usual way (reference count on buffer data is incremented).
     */
    wxCharTypeBuffer(const wxScopedCharTypeBuffer<T>& src);

    /**
        Assigns @a str to this buffer and takes ownership of it (i.e.\ the
        buffer becomes "owned").
     */
    wxCharTypeBuffer& operator=(const CharType *str);

    /// Assignment operator behaves in the same way as the copy constructor.
    wxCharTypeBuffer& operator=(const wxCharTypeBuffer& src);

    /**
        Assigns a scoped buffer to this buffer.

        If @a src is a non-owned buffer, a copy of its data is made using
        wxStrdup(). If @a src is an owned buffer, the assignment behaves
        in the usual way (reference count on buffer data is incremented).
     */
    wxCharTypeBuffer& operator=(const wxScopedCharTypeBuffer<T>& src);

    /**
        Extends the buffer to have size @a len.

        Can only be called on buffers that don't share data with another
        buffer (i.e. reference count of the data is 1).

        @see shrink()
     */
    bool extend(size_t len);

    /**
        Shrinks the buffer to have size @a len and NUL-terminates the string
        at this length.

        Can only be called on buffers that don't share data with another
        buffer (i.e. reference count of the data is 1).

        @param len Length to shrink to. Must not be larger than current length.

        @note The string is not reallocated to take less memory.

        @since 2.9.0

        @see extend()
     */
    bool shrink(size_t len);
};

/**
    This is a specialization of wxCharTypeBuffer<T> for @c char type.

    @nolibrary
    @category{data}
*/
class wxCharBuffer : public wxCharTypeBuffer<char>
{
public:
    typedef wxCharTypeBuffer<char> wxCharTypeBufferBase;
    typedef wxScopedCharTypeBuffer<char> wxScopedCharTypeBufferBase;

    wxCharBuffer(const wxCharTypeBufferBase& buf);
    wxCharBuffer(const wxScopedCharTypeBufferBase& buf);
    wxCharBuffer(const CharType *str = NULL);
    wxCharBuffer(size_t len);
    wxCharBuffer(const wxCStrData& cstr);
};

/**
    This is a specialization of wxCharTypeBuffer<T> for @c wchar_t type.

    @nolibrary
    @category{data}
*/
class wxWCharBuffer : public wxCharTypeBuffer<wchar_t>
{
public:
    typedef wxCharTypeBuffer<wchar_t> wxCharTypeBufferBase;
    typedef wxScopedCharTypeBuffer<wchar_t> wxScopedCharTypeBufferBase;

    wxWCharBuffer(const wxCharTypeBufferBase& buf);
    wxWCharBuffer(const wxScopedCharTypeBufferBase& buf);
    wxWCharBuffer(const CharType *str = NULL);
    wxWCharBuffer(size_t len);
    wxWCharBuffer(const wxCStrData& cstr);
};

/**
    @class wxMemoryBuffer

    A @b wxMemoryBuffer is a useful data structure for storing arbitrary sized
    blocks of memory. wxMemoryBuffer guarantees deletion of the memory block when
    the object is destroyed.

    @library{wxbase}
    @category{data}
*/
class wxMemoryBuffer
{
public:
    /**
        Copy constructor, refcounting is used for performance, but wxMemoryBuffer
        is not a copy-on-write structure so changes made to one buffer effect all
        copies made from it.

        @see @ref overview_refcount
    */
    wxMemoryBuffer(const wxMemoryBuffer& src);

    /**
        Create a new buffer.

        @param size
            size of the new buffer, 1KiB by default.
    */
    wxMemoryBuffer(size_t size = 1024);

    /**
        Append a single byte to the buffer.

        @param data
            New byte to append to the buffer.
    */
    void AppendByte(char data);

    /**
        Single call to append a data block to the buffer.

        @param data
            Pointer to block to append to the buffer.
        @param len
            Length of data to append.
    */
    void AppendData(const void *data, size_t len);

    /**
        Clear the buffer contents.

        The buffer won't contain any data after this method is called.

        @see IsEmpty()

        @since 2.9.4
     */
    void Clear();

    /**
        Ensure that the buffer is big enough and return a pointer to the start
        of the empty space in the buffer. This pointer can be used to directly
        write data into the buffer, this new data will be appended to the
        existing data.

        @param sizeNeeded
            Amount of extra space required in the buffer for
            the append operation
    */
    void* GetAppendBuf(size_t sizeNeeded);

    /**
        Returns the size of the buffer.
    */
    size_t GetBufSize() const;

    /**
        Return a pointer to the data in the buffer.
    */
    void* GetData() const;

    /**
        Returns the length of the valid data in the buffer.
    */
    size_t GetDataLen() const;

    /**
        Ensure the buffer is big enough and return a pointer to the
        buffer which can be used to directly write into the buffer
        up to @a sizeNeeded bytes.
    */
    void* GetWriteBuf(size_t sizeNeeded);

    /**
        Returns true if the buffer contains no data.

        @see Clear()

        @since 2.9.4
     */
    bool IsEmpty() const;

    /**
        Ensures the buffer has at least @a size bytes available.
    */
    void SetBufSize(size_t size);

    /**
        Sets the length of the data stored in the buffer.
        Mainly useful for truncating existing data.

        @param size
            New length of the valid data in the buffer. This is
            distinct from the allocated size
    */
    void SetDataLen(size_t size);

    /**
        Update the length after completing a direct append, which
        you must have used GetAppendBuf() to initialise.

        @param sizeUsed
            This is the amount of new data that has been
            appended.
    */
    void UngetAppendBuf(size_t sizeUsed);

    /**
        Update the buffer after completing a direct write, which
        you must have used GetWriteBuf() to initialise.

        @param sizeUsed
            The amount of data written in to buffer
            by the direct write
    */
    void UngetWriteBuf(size_t sizeUsed);
};

