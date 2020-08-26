/////////////////////////////////////////////////////////////////////////////
// Name:        uri.h
// Purpose:     interface of wxURI
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Host type of URI returned from wxURI::GetHostType().
*/
enum wxURIHostType
{
    wxURI_REGNAME,      ///< Host is a normal register name (@c "www.mysite.com").
    wxURI_IPV4ADDRESS,  ///< Host is a version 4 ip address (@c "192.168.1.100").
    wxURI_IPV6ADDRESS,  ///< Host is a version 6 ip address (@c "[aa:aa:aa:aa::aa:aa]:5050").
    wxURI_IPVFUTURE     ///< Host is a future ip address, wxURI is unsure what kind.
};

/**
    @class wxURI

    wxURI is used to extract information from a URI (Uniform Resource Identifier).

    For information about URIs, see RFC 3986 (http://www.ietf.org/rfc/rfc3986.txt).

    In short, a URL is a URI. In other words, URL is a subset of a URI - all
    acceptable URLs are also acceptable URIs.

    wxURI automatically escapes invalid characters in a string, so there is no
    chance of wxURI "failing" on construction/creation.

    wxURI supports copy construction and standard assignment operators. wxURI
    can also be inherited from to provide further functionality.

    To obtain individual components you can use one of the GetXXX() methods.
    However, you should check HasXXX() before calling a get method, which
    determines whether or not the component referred to by the method is
    defined according to RFC 2396. Consider an undefined component equivalent
    to a @NULL C string.

    Example:
    @code
        // protocol will hold the http protocol (i.e. "http")
        wxString protocol;
        wxURI myuri("http://mysite.com");
        if( myuri.HasScheme() )
            protocol = myuri.GetScheme();
    @endcode

    @note On URIs with a "file" scheme wxURI does not parse the userinfo,
          server, or port portion. This is to keep compatibility with
          wxFileSystem, the old wxURL, and older url specifications.

    @library{wxbase}
    @category{net}

    @see wxURL
*/
class wxURI : public wxObject
{
public:
    /**
        Creates an empty URI.
    */
    wxURI();

    /**
        Constructor for quick creation.

        @param uri
            URI (Uniform Resource Identifier) to initialize with.
    */
    wxURI(const wxString& uri);

    /**
        Copies this URI from another URI.

        @param uri
            URI (Uniform Resource Identifier) to initialize with.
    */
    wxURI(const wxURI& uri);

    /**
        Builds the URI from its individual components and adds proper
        separators.

        If the URI is not a reference or is not resolved, the URI that is
        returned is the same one passed to the constructor or Create().
    */
    wxString BuildURI() const;

    /**
        Builds the URI from its individual components, adds proper separators,
        and returns escape sequences to normal characters.

        @note It is preferred to call this over Unescape(BuildURI()) since
              BuildUnescapedURI() performs some optimizations over the plain
              method.
    */
    wxString BuildUnescapedURI() const;

    /**
        Creates this URI from the @a uri string.

        Returns @true if this instance was correctly initialized.

        @param uri
            String to initialize from.
    */
    bool Create(const wxString& uri);

    /**
        Obtains the fragment of this URI.

        The fragment of a URI is the last value of the URI, and is the value
        after a "#" character after the path of the URI.

        @c "http://mysite.com/mypath#<fragment>"
    */
    const wxString& GetFragment() const;

    /**
        Obtains the host type of this URI, which is one of wxURIHostType.
    */
    wxURIHostType GetHostType() const;

    /**
        Returns the password part of the userinfo component of this URI. Note
        that this is explicitly depreciated by RFC 1396 and should generally be
        avoided if possible.

        @c "http://<user>:<password>@mysite.com/mypath"
    */
    wxString GetPassword() const;

    /**
        Returns the (normalized) path of the URI.

        The path component of a URI comes directly after the scheme component
        if followed by zero or one slashes ('/'), or after the server/port
        component.

        Absolute paths include the leading '/' character.

        @c "http://mysite.com<path>"
    */
    const wxString& GetPath() const;

    /**
        Returns a string representation of the URI's port.

        The Port of a URI is a value after the server, and must come after a
        colon (:).

        @c "http://mysite.com:<port>"

        @note You can easily get the numeric value of the port by using
              wxAtoi() or wxString::Format().
    */
    const wxString& GetPort() const;

    /**
        Returns the Query component of the URI.

        The query component is what is commonly passed to a cgi application,
        and must come after the path component, and after a '?' character.

        @c "http://mysite.com/mypath?<query>"
    */
    const wxString& GetQuery() const;

    /**
        Returns the Scheme component of the URI.

        The first part of the URI.

        @c "<scheme>://mysite.com"
    */
    const wxString& GetScheme() const;

    /**
        Returns the Server component of the URI.

        The server of the URI can be a server name or a type of IP address. See
        GetHostType() for the possible values for the host type of the server
        component.

        @c "http://<server>/mypath"
    */
    const wxString& GetServer() const;

    /**
        Returns the username part of the userinfo component of this URI. Note
        that this is explicitly depreciated by RFC 1396 and should generally be
        avoided if possible.

        @c "http://<user>:<password>@mysite.com/mypath"
    */
    wxString GetUser() const;

    /**
        Returns the UserInfo component of the URI.

        The component of a URI before the server component that is postfixed by
        a '@' character.

        @c "http://<userinfo>@mysite.com/mypath"
    */
    const wxString& GetUserInfo() const;

    /**
        Returns @true if the Fragment component of the URI exists.
    */
    bool HasFragment() const;

    /**
        Returns @true if the Path component of the URI exists.
    */
    bool HasPath() const;

    /**
        Returns @true if the Port component of the URI exists.
    */
    bool HasPort() const;

    /**
        Returns @true if the Query component of the URI exists.
    */
    bool HasQuery() const;

    /**
        Returns @true if the Scheme component of the URI exists.
    */
    bool HasScheme() const;

    /**
        Returns @true if the Server component of the URI exists.
    */
    bool HasServer() const;

    /**
        Returns @true if the User component of the URI exists.
    */
    bool HasUserInfo() const;

    /**
        Returns @true if a valid [absolute] URI, otherwise this URI is a URI
        reference and not a full URI, and this function returns @false.
    */
    bool IsReference() const;

    /**
        Inherits this URI from a base URI - components that do not exist in
        this URI are copied from the base, and if this URI's path is not an
        absolute path (prefixed by a '/'), then this URI's path is merged with
        the base's path.

        For instance, resolving "../mydir" from "http://mysite.com/john/doe"
        results in the scheme (http) and server ("mysite.com") being copied
        into this URI, since they do not exist. In addition, since the path of
        this URI is not absolute (does not begin with '/'), the path of the
        base's is merged with this URI's path, resulting in the URI
        "http://mysite.com/john/mydir".

        @param base
            Base URI to inherit from. Must be a full URI and not a reference.
        @param flags
            Currently either wxURI_STRICT or 0, in non-strict mode some
            compatibility layers are enabled to allow loopholes from RFCs prior
            to 2396.
    */
    void Resolve(const wxURI& base, int flags = wxURI_STRICT);

    /**
        Translates all escape sequences (normal characters and returns the result.

        If you want to unescape an entire wxURI, use BuildUnescapedURI()
        instead, as it performs some optimizations over this method.

        @param uri
            String with escaped characters to convert.
    */
    static wxString Unescape(const wxString& uri);

    /**
        Compares this URI to another URI, and returns @true if this URI equals
        @a uricomp, otherwise it returns @false.

        @param uricomp
            URI to compare to.
    */
    bool operator==(const wxURI& uricomp) const;
};

