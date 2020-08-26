/////////////////////////////////////////////////////////////////////////////
// Name:        mstream.h
// Purpose:     interface of wxMemoryOutputStream, wxMemoryInputStream
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxMemoryOutputStream

    This class allows using all methods taking a wxOutputStream reference to write
    to in-memory data.

    Example:
    @code
        wxMemoryOutputStream stream;
        if (!my_wxImage.SaveFile(stream))
            return;

        // now we can access the saved image bytes:
        wxStreamBuffer* theBuffer = stream.GetOutputStreamBuffer();
        unsigned char byte;
        if (theBuffer->Read(byte, 1) != 1)
            return;

        // ... do something with 'byte'...

        // remember that ~wxMemoryOutputStream will destroy the internal
        // buffer since we didn't provide our own when constructing it
    @endcode

    @library{wxbase}
    @category{streams}

    @see wxStreamBuffer
*/
class wxMemoryOutputStream : public wxOutputStream
{
public:
    /**
        If @a data is @NULL, then it will initialize a new empty buffer which will
        grow if required.

        @warning
        If the buffer is created by wxMemoryOutputStream, it will be destroyed
        at the destruction of the stream.
    */
    wxMemoryOutputStream(void* data = NULL, size_t length = 0);

    /**
        Destructor.

        If the buffer wasn't provided by the user, it will be deleted here.
    */
    virtual ~wxMemoryOutputStream();

    /**
        Allows you to transfer data from the internal buffer of wxMemoryOutputStream
        to an external buffer. @a len specifies the size of the buffer.
    */
    size_t CopyTo(void* buffer, size_t len) const;

    /**
        Returns the pointer to the stream object used as an internal buffer
        for this stream.
    */
    wxStreamBuffer* GetOutputStreamBuffer() const;
};



/**
    @class wxMemoryInputStream

    This class allows using all methods taking a wxInputStream reference to read
    in-memory data.

    Example:
    @code
        // we've got a block of memory containing a BMP image and we want
        // to use wxImage to load it:
        wxMemoryInputStream stream(my_memory_block, size);

        wxImage theBitmap;
        if (!theBitmap.LoadFile(stream, wxBITMAP_TYPE_BMP))
            return;

        // we can now safely delete the original memory buffer as the data
        // has been decoded by wxImage:
        delete [] my_memory_block;
    @endcode

    @library{wxbase}
    @category{streams}

    @see wxStreamBuffer, wxMemoryOutputStream
*/
class wxMemoryInputStream : public wxInputStream
{
public:
    /**
        Initializes a new read-only memory stream which will use the specified
        buffer data of length len. The stream does not take ownership of the buffer,
        i.e. the buffer will not be deleted in its destructor.
    */
    wxMemoryInputStream(const void* data, size_t len);

    /**
        Creates a new read-only memory stream, initializing it with the data from
        the given output stream @a stream.
    */
    wxMemoryInputStream(const wxMemoryOutputStream& stream);

    /**
        Creates a new read-only memory stream, initializing it with the
        data from the given input stream @a stream.

        The @a len argument specifies the amount of data to read from the
        @a stream. Setting it to ::wxInvalidOffset means that the @a stream
        is to be read entirely (i.e. till the EOF is reached).
    */
    wxMemoryInputStream(wxInputStream& stream,
                        wxFileOffset len = wxInvalidOffset);

    /**
        Destructor. Does NOT free the buffer provided in the ctor.
    */
    virtual ~wxMemoryInputStream();

    /**
        Returns the pointer to the stream object used as an internal buffer
        for that stream.
    */
    wxStreamBuffer* GetInputStreamBuffer() const;
};

