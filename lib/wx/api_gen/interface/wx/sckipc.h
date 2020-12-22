/////////////////////////////////////////////////////////////////////////////
// Name:        sckipc.h
// Purpose:     interface of wxTCPServer
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    See wxTCPConnection.
*/
enum wxIPCFormat
{
  wxIPC_INVALID =          0,
  wxIPC_TEXT =             1,  /* CF_TEXT */
  wxIPC_BITMAP =           2,  /* CF_BITMAP */
  wxIPC_METAFILE =         3,  /* CF_METAFILEPICT */
  wxIPC_SYLK =             4,
  wxIPC_DIF =              5,
  wxIPC_TIFF =             6,
  wxIPC_OEMTEXT =          7,  /* CF_OEMTEXT */
  wxIPC_DIB =              8,  /* CF_DIB */
  wxIPC_PALETTE =          9,
  wxIPC_PENDATA =          10,
  wxIPC_RIFF =             11,
  wxIPC_WAVE =             12,
  wxIPC_UTF16TEXT =        13, /* CF_UNICODE */
  wxIPC_ENHMETAFILE =      14,
  wxIPC_FILENAME =         15, /* CF_HDROP */
  wxIPC_LOCALE =           16,
  wxIPC_UTF8TEXT =         17,
  wxIPC_UTF32TEXT =        18,
  wxIPC_UNICODETEXT,
  wxIPC_PRIVATE =          20
};


/**
    @class wxTCPServer

    A wxTCPServer object represents the server part of a client-server conversation.
    It emulates a DDE-style protocol, but uses TCP/IP which is available on most
    platforms.

    A DDE-based implementation for Windows is available using wxDDEServer.

    @library{wxnet}
    @category{net}

    @see wxTCPClient, wxTCPConnection, @ref overview_ipc
*/
class wxTCPServer : public wxObject
{
public:
    /**
        Constructs a server object.
    */
    wxTCPServer();

    /**
        Registers the server using the given service name.

        Under Unix, the string must contain an integer id which is used as an
        Internet port number. @false is returned if the call failed
        (for example, the port number is already in use).
    */
    virtual bool Create(const wxString& service);

    /**
        When a client calls @b MakeConnection, the server receives the
        message and this member is called.

        The application should derive a member to intercept this message and
        return a connection object of either the standard wxTCPConnection type,
        or of a user-derived type.
        If the topic is "STDIO", the application may wish to refuse the connection.
        Under Unix, when a server is created the OnAcceptConnection message is
        always sent for standard input and output.
    */
    virtual wxConnectionBase* OnAcceptConnection(const wxString& topic);
};



/**
    @class wxTCPClient

    A wxTCPClient object represents the client part of a client-server conversation.
    It emulates a DDE-style protocol, but uses TCP/IP which is available on most
    platforms.

    A DDE-based implementation for Windows is available using wxDDEClient.

    To create a client which can communicate with a suitable server, you need
    to derive a class from wxTCPConnection and another from wxTCPClient.
    The custom wxTCPConnection class will intercept communications in
    a 'conversation' with a server, and the custom wxTCPServer is required
    so that a user-overridden wxTCPClient::OnMakeConnection() member can return
    a wxTCPConnection of the required class, when a connection is made.

    @library{wxnet}
    @category{net}

    @see wxTCPServer, wxTCPConnection, @ref overview_ipc
*/
class wxTCPClient : public wxObject
{
public:
    /**
        Constructs a client object.
    */
    wxTCPClient();

    /**
        Tries to make a connection with a server specified by the host
        (a machine name under Unix), service name (must contain an integer
        port number under Unix), and a topic string.

        If the server allows a connection, a wxTCPConnection object will be returned.

        The type of wxTCPConnection returned can be altered by overriding
        the OnMakeConnection() member to return your own derived connection object.
    */
    virtual wxConnectionBase* MakeConnection(const wxString& host,
                                             const wxString& service,
                                             const wxString& topic);

    /**
        The type of wxTCPConnection returned from a MakeConnection() call can
        be altered by deriving the @b OnMakeConnection member to return your
        own derived connection object. By default, a wxTCPConnection
        object is returned.

        The advantage of deriving your own connection class is that it will
        enable you to intercept messages initiated by the server, such
        as wxTCPConnection::OnAdvise(). You may also want to store
        application-specific data in instances of the new class.
    */
    virtual wxConnectionBase* OnMakeConnection();

    /**
        Returns @true if this is a valid host name, @false otherwise.
    */
    virtual bool ValidHost(const wxString& host);
};



/**
    @class wxTCPConnection

    A wxTCPClient object represents the connection between a client and a server.
    It emulates a DDE-style protocol, but uses TCP/IP which is available on most
    platforms.

    A DDE-based implementation for Windows is available using wxDDEConnection.

    A wxTCPConnection object can be created by making a connection using a
    wxTCPClient object, or by the acceptance of a connection by a wxTCPServer object.
    The bulk of a conversation is controlled by calling members in a
    @b wxTCPConnection object or by overriding its members.

    An application should normally derive a new connection class from
    wxTCPConnection, in order to override the communication event handlers
    to do something interesting.

    @library{wxnet}
    @category{net}

    @see wxTCPClient, wxTCPServer, @ref overview_ipc
*/
class wxTCPConnection : public wxObject
{
public:
    //@{
    /**
        Constructs a connection object.

        If no user-defined connection object is to be derived from wxTCPConnection,
        then the constructor should not be called directly, since the default
        connection object will be provided on requesting (or accepting) a connection.

        However, if the user defines his or her own derived connection object,
        the wxTCPServer::OnAcceptConnection and/or wxTCPClient::OnMakeConnection
        members should be replaced by functions which construct the new connection object.

        If the arguments of the wxTCPConnection constructor are void, then a default
        buffer is associated with the connection. Otherwise, the programmer must
        provide a buffer and size of the buffer for the connection object to use in
        transactions.
    */
    wxTCPConnection();
    wxTCPConnection(void* buffer, size_t size);
    //@}

    //@{
    /**
        Called by the server application to advise the client of a change in
        the data associated with the given item.

        Causes the client connection's OnAdvise() member to be called.

        Returns @true if successful.
    */
    bool Advise(const wxString& item, const void* data, size_t size,
                wxIPCFormat format = wxIPC_PRIVATE);
    bool Advise(const wxString& item, const char* data);
    bool Advise(const wxString& item, const wchar_t* data);
    bool Advise(const wxString& item, const wxString data);
    //@}

    /**
        Called by the client or server application to disconnect from the other
        program; it causes the OnDisconnect() message to be sent to the
        corresponding connection object in the other program.

        The default behaviour of @b OnDisconnect is to delete the
        connection, but the calling application must explicitly delete its
        side of the connection having called @b Disconnect.

        Returns @true if successful.
    */
    virtual bool Disconnect();

    //@{
    /**
        Called by the client application to execute a command on the server.
        Can also be used to transfer arbitrary data to the server (similar
        to Poke() in that respect). Causes the server connection's OnExecute()
        member to be called.

        Returns @true if successful.
    */
    bool Execute(const void* data, size_t size,
                 wxIPCFormat format = wxIPC_PRIVATE);
    bool Execute(const char* data);
    bool Execute(const wchar_t* data);
    bool Execute(const wxString data);
    //@}

    /**
        Message sent to the client application when the server notifies it of a
        change in the data associated with the given item.
    */
    virtual bool OnAdvise(const wxString& topic,
                          const wxString& item,
                          const void* data,
                          size_t size,
                          wxIPCFormat format);

    /**
        Message sent to the client or server application when the other
        application notifies it to delete the connection.
        Default behaviour is to delete the connection object.
    */
    virtual bool OnDisconnect();

    /**
        Message sent to the server application when the client notifies it to
        execute the given data.
        Note that there is no item associated with this message.
    */
    virtual bool OnExecute(const wxString& topic, const void* data,
                           size_t size,
                           wxIPCFormat format);

    /**
        Message sent to the server application when the client notifies it to
        accept the given data.
    */
    virtual bool OnPoke(const wxString& topic, const wxString& item,
                        const void* data,
                        size_t size,
                        wxIPCFormat format);

    /**
        Message sent to the server application when the client calls Request().

        The server should respond by returning a character string from @b OnRequest,
        or @NULL to indicate no data.
    */
    virtual const void* OnRequest(const wxString& topic,
                                  const wxString& item,
                                  size_t* size,
                                  wxIPCFormat format);

    /**
        Message sent to the server application by the client, when the client
        wishes to start an 'advise loop' for the given topic and item.
        The server can refuse to participate by returning @false.
    */
    virtual bool OnStartAdvise(const wxString& topic,
                               const wxString& item);

    /**
        Message sent to the server application by the client, when the client
        wishes to stop an 'advise loop' for the given topic and item.
        The server can refuse to stop the advise loop by returning @false, although
        this doesn't have much meaning in practice.
    */
    virtual bool OnStopAdvise(const wxString& topic,
                              const wxString& item);

    //@{
    /**
        Called by the client application to poke data into the server.
        Can be used to transfer arbitrary data to the server. Causes the server
        connection's OnPoke() member to be called. Returns @true if successful.
    */
    bool Poke(const wxString& item, const void* data, size_t size,
              wxIPCFormat format = wxIPC_PRIVATE);
    bool Poke(const wxString& item, const char* data);
    bool Poke(const wxString& item, const wchar_t* data);
    bool Poke(const wxString& item, const wxString data);
    //@}

    /**
        Called by the client application to request data from the server.
        Causes the server connection's OnRequest() member to be called.

        Returns a character string (actually a pointer to the connection's buffer) if
        successful, @NULL otherwise.
    */
    virtual const void* Request(const wxString& item, size_t* size = 0,
                        wxIPCFormat format = wxIPC_TEXT);

    /**
        Called by the client application to ask if an advise loop can be started
        with the server.

        Causes the server connection's OnStartAdvise() member to be called.
        Returns @true if the server okays it, @false otherwise.
    */
    virtual bool StartAdvise(const wxString& item);

    /**
        Called by the client application to ask if an advise loop can be stopped.
        Causes the server connection's OnStopAdvise() member to be called.
        Returns @true if the server okays it, @false otherwise.
    */
    virtual bool StopAdvise(const wxString& item);
};

