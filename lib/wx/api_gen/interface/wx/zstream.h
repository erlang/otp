/////////////////////////////////////////////////////////////////////////////
// Name:        zstream.h
// Purpose:     interface of wxZlibOutputStream
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/// Compression level
enum wxZlibCompressionLevels {
    wxZ_DEFAULT_COMPRESSION = -1,
    wxZ_NO_COMPRESSION = 0,
    wxZ_BEST_SPEED = 1,
    wxZ_BEST_COMPRESSION = 9
};

/// Flags
enum wxZLibFlags {
    wxZLIB_NO_HEADER = 0,    //!< raw deflate stream, no header or checksum
    wxZLIB_ZLIB = 1,         //!< zlib header and checksum
    wxZLIB_GZIP = 2,         //!< gzip header and checksum, requires zlib 1.2.1+
    wxZLIB_AUTO = 3          //!< autodetect header zlib or gzip
};


/**
    @class wxZlibOutputStream

    This stream compresses all data written to it.

    The compressed output can be in zlib or gzip format.
    Note that writing the gzip format requires zlib version 1.2.1 or greater
    (the builtin version does support gzip format).

    The stream is not seekable, wxOutputStream::SeekO() returns
    ::wxInvalidOffset.

    @library{wxbase}
    @category{archive,streams}

    @see wxOutputStream, wxZlibInputStream
*/
class wxZlibOutputStream : public wxFilterOutputStream
{
public:
    //@{
    /**
        Creates a new write-only compressed stream.

        @a level means level of compression. It is number between 0 and 9
        (including these values) where 0 means no compression and 9 best but
        slowest compression. -1 is default value (currently equivalent to 6).

        If the parent stream is passed as a pointer then the new filter stream
        takes ownership of it. If it is passed by reference then it does not.

        The @a flags wxZLIB_ZLIB and wxZLIB_GZIP specify whether the output data
        will be in zlib or gzip format. wxZLIB_ZLIB is the default.

        If @a flags is wxZLIB_NO_HEADER, then a raw deflate stream is output
        without either zlib or gzip headers. This is a lower level mode, which
        is not usually used directly. It can be used to embed a raw deflate
        stream in a higher level protocol.

        The values of the ::wxZlibCompressionLevels and ::wxZLibFlags
        enumerations can be used.
    */
    wxZlibOutputStream(wxOutputStream& stream, int level = -1,
                       int flags = wxZLIB_ZLIB);
    wxZlibOutputStream(wxOutputStream* stream, int level = -1,
                       int flags = wxZLIB_ZLIB);
    //@}

    /**
        Returns @true if zlib library in use can handle gzip compressed data.
    */
    static bool CanHandleGZip();

    //@{
    /**
        Sets the dictionary to the specified chunk of data. This can improve
        compression rate but note that the dictionary has to be the same when
        you deflate the data as when you inflate the data, otherwise you
        will inflate corrupted data.

        Returns @true if the dictionary was successfully set.
    */
    bool SetDictionary(const char *data, size_t datalen);
    bool SetDictionary(const wxMemoryBuffer &buf);
    //@}
};



/**
    @class wxZlibInputStream

    This filter stream decompresses a stream that is in zlib or gzip format.
    Note that reading the gzip format requires zlib version 1.2.1 or greater,
    (the builtin version does support gzip format).

    The stream is not seekable, wxInputStream::SeekI returns ::wxInvalidOffset.
    Also wxStreamBase::GetSize() is not supported, it always returns 0.

    @library{wxbase}
    @category{archive,streams}

    @see wxInputStream, wxZlibOutputStream.
*/
class wxZlibInputStream : public wxFilterInputStream
{
public:
    //@{
    /**
        If the parent stream is passed as a pointer then the new filter stream
        takes ownership of it. If it is passed by reference then it does not.

        The @a flags wxZLIB_ZLIB and wxZLIB_GZIP specify whether the input data
        is in zlib or gzip format. If wxZLIB_AUTO is used, then zlib will
        autodetect the stream type, this is the default.

        If @a flags is wxZLIB_NO_HEADER, then the data is assumed to be a raw
        deflate stream without either zlib or gzip headers. This is a lower level
        mode, which is not usually used directly. It can be used to read a raw
        deflate stream embedded in a higher level protocol.

        The values of the ::wxZLibFlags enumeration can be used.
    */
    wxZlibInputStream(wxInputStream& stream, int flags = wxZLIB_AUTO);
    wxZlibInputStream(wxInputStream* stream, int flags = wxZLIB_AUTO);
    //@}

    /**
        Returns @true if zlib library in use can handle gzip compressed data.
    */
    static bool CanHandleGZip();

    //@{
    /**
        Sets the dictionary to the specified chunk of data. This can improve
        compression rate but note that the dictionary has to be the same when
        you deflate the data as when you inflate the data, otherwise you
        will inflate corrupted data.

        Returns @true if the dictionary was successfully set.
    */
    bool SetDictionary(const char *data, size_t datalen);
    bool SetDictionary(const wxMemoryBuffer &buf);
    //@}
};

