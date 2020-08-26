/////////////////////////////////////////////////////////////////////////////
// Name:        protocol/http.h
// Purpose:     interface of wxHTTP
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxHTTP

    wxHTTP can be used to establish a connection to an HTTP server.

    wxHTTP can thus be used to create a (basic) HTTP @b client.

    @library{wxnet}
    @category{net}

    @see wxSocketBase, wxURL
*/
class wxHTTP : public wxProtocol
{
public:
    /**
        Default constructor.
    */
    wxHTTP();

    /**
        Destructor will close the connection if connected.
    */
    virtual ~wxHTTP();

    //@{
    /**
        Connect to the HTTP server.

        By default, connection is made to the port 80 of the specified @a host.
        You may connect to a non-default port by specifying it explicitly using
        the second overload.

        Currently wxHTTP only supports IPv4.

        For the overload taking wxSockAddress, the @a wait argument is ignored.
     */
    virtual bool Connect(const wxString& host);
    virtual bool Connect(const wxString& host, unsigned short port);
    virtual bool Connect(const wxSockAddress& addr, bool wait);
    //@}

    /**
        Returns the data attached with a field whose name is specified by @a header.
        If the field doesn't exist, it will return an empty string and not a @NULL string.

        @note
        The header is not case-sensitive, i.e. "CONTENT-TYPE" and "content-type"
        represent the same header.
    */
    wxString GetHeader(const wxString& header) const;

    /**
        Creates a new input stream on the specified path.

        Notice that this stream is unseekable, i.e. SeekI() and TellI() methods
        shouldn't be used.

        Note that you can still know the size of the file you are getting using
        wxStreamBase::GetSize(). However there is a limitation: in HTTP protocol,
        the size is not always specified so sometimes @c (size_t)-1 can returned to
        indicate that the size is unknown.
        In such case, you may want to use wxInputStream::LastRead() method in a loop
        to get the total size.

        @return Returns the initialized stream. You must delete it yourself
                 once you don't use it anymore and this must be done before
                 the wxHTTP object itself is destroyed. The destructor
                 closes the network connection. The next time you will
                 try to get a file the network connection will have to
                 be reestablished, but you don't have to take care of
                 this since wxHTTP reestablishes it automatically.

        @see wxInputStream
    */
    virtual wxInputStream* GetInputStream(const wxString& path);

    /**
        Returns the HTTP response code returned by the server.

        Please refer to RFC 2616 for the list of responses.
    */
    int GetResponse() const;

    /**
        Set HTTP method.

        Set <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html">common</a>
        or expanded HTTP method.

        Overrides GET or POST methods that is used by default.

        @param method
            HTTP method name, e.g. "GET".

        @since 3.0

        @see SetPostBuffer(), SetPostText()
    */
    void SetMethod(const wxString& method);

    /**
        It sets data of a field to be sent during the next request to the HTTP server.

        The field name is specified by @a header and the content by @a h_data.
        This is a low level function and it assumes that you know what you are doing.
    */
    void SetHeader(const wxString& header, const wxString& h_data);

    /**
        Returns the value of a cookie.
    */

    wxString GetCookie(const wxString& cookie) const;

    /**
        Returns @true if there were cookies.
    */
    bool HasCookies() const;

    /**
        Set the binary data to be posted to the server.

        If a non-empty buffer is passed to this method, the next request will
        be an HTTP @c POST instead of the default HTTP @c GET and the given @a
        data will be posted as the body of this request.

        For textual data a more convenient SetPostText() can be used instead.

        @param contentType
            The value of HTTP "Content-Type" header, e.g. "image/png".
        @param data
            The data to post.
        @return
            @true if any data was passed in or @false if the buffer was empty.

        @since 2.9.4
     */
    bool SetPostBuffer(const wxString& contentType, const wxMemoryBuffer& data);

    /**
        Set the text to be posted to the server.

        After a successful call to this method, the request will use HTTP @c
        POST instead of the default @c GET when it's executed.

        Use SetPostBuffer() if you need to post non-textual data.

        @param contentType
            The value of HTTP "Content-Type" header, e.g. "text/html;
            charset=UTF-8".
        @param data
            The data to post.
        @param conv
            The conversion to use to convert @a data contents to a byte stream.
            Its value should be consistent with the charset parameter specified
            in @a contentType.
        @return
            @true if string was non-empty and was successfully converted using
            the given @a conv or @false otherwise (in this case this request
            won't be @c POST'ed correctly).

        @since 2.9.4
     */
    bool SetPostText(const wxString& contentType,
                     const wxString& data,
                     const wxMBConv& conv = wxConvUTF8);
};

