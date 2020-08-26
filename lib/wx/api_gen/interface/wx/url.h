/////////////////////////////////////////////////////////////////////////////
// Name:        url.h
// Purpose:     interface of wxURL
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Error types returned from wxURL::GetError().
*/
enum wxURLError {
    wxURL_NOERR = 0,    ///< No error.
    wxURL_SNTXERR,      ///< Syntax error in the URL string.
    wxURL_NOPROTO,      ///< Found no protocol which can get this URL.
    wxURL_NOHOST,       ///< A host name is required for this protocol.
    wxURL_NOPATH,       ///< A path is required for this protocol.
    wxURL_CONNERR,      ///< Connection error.
    wxURL_PROTOERR      ///< An error occurred during negotiation.
};

/**
    @class wxURL

    wxURL is a specialization of wxURI for parsing URLs. Please look at wxURI
    documentation for more info about the functions you can use to retrieve the
    various parts of the URL (scheme, server, port, etc).

    Supports standard assignment operators, copy constructors, and comparison
    operators.

    @library{wxnet}
    @category{net}

    @see wxSocketBase, wxProtocol
*/
class wxURL : public wxURI
{
public:
    /**
        Constructs a URL object from the string. The URL must be valid
        according to RFC 1738. In particular, file URLs must be of the format
        @c "file://hostname/path/to/file", otherwise GetError() will return a
        value different from ::wxURL_NOERR.

        It is valid to leave out the hostname but slashes must remain in place,
        in other words, a file URL without a hostname must contain three
        consecutive slashes (e.g. @c "file:///somepath/myfile").

        @param url
            Url string to parse.
    */
    wxURL(const wxString& url = wxEmptyString);

    /**
        Destroys the URL object.
    */
    virtual ~wxURL();

    /**
        Returns the last error. This error refers to the URL parsing or to the
        protocol. It can be one of ::wxURLError.
    */
    wxURLError GetError() const;

    /**
        Creates a new input stream on the specified URL. You can use all but
        seek functionality of wxStream. Seek isn't available on all streams.
        For example, HTTP or FTP streams don't deal with it.

        Note that this method is somewhat deprecated, all future wxWidgets
        applications should use wxFileSystem instead.

        Example:

        @code
        wxURL url("http://a.host/a.dir/a.file");
        if (url.GetError() == wxURL_NOERR)
        {
            wxInputStream *in_stream;

            in_stream = url.GetInputStream();
            // Then, you can use all IO calls of in_stream (See wxStream)
        }
        @endcode

        @return Returns the initialized stream. You will have to delete it
                 yourself.

        @see wxInputStream
    */
    wxInputStream* GetInputStream();

    /**
        Returns a reference to the protocol which will be used to get the URL.
    */
    wxProtocol& GetProtocol();

    /**
        Returns @true if this object is correctly initialized, i.e.\ if
        GetError() returns ::wxURL_NOERR.
    */
    bool IsOk() const;

    /**
        Sets the default proxy server to use to get the URL. The string
        specifies the proxy like this: @c "<hostname>:<port number>".

        @param url_proxy
            Specifies the proxy to use.

        @see SetProxy()
    */
    static void SetDefaultProxy(const wxString& url_proxy);

    /**
        Sets the proxy to use for this URL.

        @see SetDefaultProxy()
    */
    void SetProxy(const wxString& url_proxy);

    /**
        Initializes this object with the given URL and returns ::wxURL_NOERR if
        it's valid (see GetError() for more info).
    */
    wxURLError SetURL(const wxString& url);
};

