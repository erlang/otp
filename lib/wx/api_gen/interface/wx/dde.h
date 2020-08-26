/////////////////////////////////////////////////////////////////////////////
// Name:        dde.h
// Purpose:     interface of wxDDEConnection
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxDDEConnection

    A wxDDEConnection object represents the connection between a client and a
    server. It can be created by making a connection using a wxDDEClient
    object, or by the acceptance of a connection by a wxDDEServer object.
    The bulk of a DDE (Dynamic Data Exchange) conversation is controlled by calling
    members in a wxDDEConnection object or by overriding its members.

    An application should normally derive a new connection class from
    wxDDEConnection, in order to override the communication event handlers to
    do something interesting.

    This DDE-based implementation is available on Windows only, but a
    platform-independent, socket-based version of this API is available using
    wxTCPConnection.

    @library{wxbase}
    @category{ipc}
    @onlyfor{wxmsw}

    @see wxConnectionBase, wxDDEClient, wxDDEServer, @ref overview_ipc
*/
class wxDDEConnection : public wxConnectionBase
{
public:
    /**
        Constructs a connection object. If no user-defined connection object is
        to be derived from wxDDEConnection, then the constructor should not be
        called directly, since the default connection object will be provided
        on requesting (or accepting) a connection. However, if the user defines
        his or her own derived connection object, the
        wxDDEServer::OnAcceptConnection() and/or
        wxDDEClient::OnMakeConnection() members should be replaced by functions
        which construct the new connection object.

        A default buffer will be associated with this connection.
    */
    wxDDEConnection();
    /**
        Constructs a connection object. If no user-defined connection object is
        to be derived from wxDDEConnection, then the constructor should not be
        called directly, since the default connection object will be provided
        on requesting (or accepting) a connection. However, if the user defines
        his or her own derived connection object, the
        wxDDEServer::OnAcceptConnection() and/or
        wxDDEClient::OnMakeConnection() members should be replaced by functions
        which construct the new connection object.

        @param buffer
            Buffer for this connection object to use in transactions.
        @param size
            Size of the buffer given.
    */
    wxDDEConnection(void* buffer, size_t size);

    //@{
    /**
        Called by the server application to advise the client of a change in
        the data associated with the given item. Causes the client connection's
        OnAdvise() member to be called.

        @return @true if successful.
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
        corresponding connection object in the other program. The default
        behaviour of OnDisconnect() is to delete the connection, but the
        calling application must explicitly delete its side of the connection
        having called Disconnect().

        @return @true if successful.
    */
    bool Disconnect();

    //@{
    /**
        Called by the client application to execute a command on the server.
        Can also be used to transfer arbitrary data to the server (similar to
        Poke() in that respect). Causes the server connection's OnExecute()
        member to be called.

        @return @true if successful.
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
    virtual bool OnAdvise(const wxString& topic, const wxString& item,
                          const void* data, size_t size, wxIPCFormat format);

    /**
        Message sent to the client or server application when the other
        application notifies it to delete the connection. Default behaviour is
        to delete the connection object.
    */
    virtual bool OnDisconnect();

    /**
        Message sent to the server application when the client notifies it to
        execute the given data. Note that there is no item associated with
        this message.
    */
    virtual bool OnExecute(const wxString& topic, const void* data,
                           size_t size, wxIPCFormat format);

    /**
        Message sent to the server application when the client notifies it to
        accept the given data.
    */
    virtual bool OnPoke(const wxString& topic, const wxString& item,
                        const void* data, size_t size, wxIPCFormat format);

    /**
        Message sent to the server application when the client calls Request().
        The server should respond by returning a character string from
        OnRequest(), or @NULL to indicate no data.
    */
    virtual const void* OnRequest(const wxString& topic,
                                  const wxString& item, size_t* size,
                                  wxIPCFormat format);

    /**
        Message sent to the server application by the client, when the client
        wishes to start an "advise loop" for the given topic and item. The
        server can refuse to participate by returning @false.
    */
    virtual bool OnStartAdvise(const wxString& topic, const wxString& item);

    /**
        Message sent to the server application by the client, when the client
        wishes to stop an "advise loop" for the given topic and item. The
        server can refuse to stop the advise loop by returning @false, although
        this doesn't have much meaning in practice.
    */
    virtual bool OnStopAdvise(const wxString& topic, const wxString& item);

    //@{
    /**
        Called by the client application to poke data into the server. Can be
        used to transfer arbitrary data to the server. Causes the server
        connection's OnPoke() member to be called.

        @return @true if successful.
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

        @return A character string (actually a pointer to the connection's
                 buffer) if successful, @NULL otherwise.
    */
    const void* Request(const wxString& item, size_t* size,
                        wxIPCFormat format = wxIPC_TEXT);

    /**
        Called by the client application to ask if an advise loop can be
        started with the server. Causes the server connection's OnStartAdvise()
        member to be called.

        @return @true if the server okays it, @false otherwise.
    */
    bool StartAdvise(const wxString& item);

    /**
        Called by the client application to ask if an advise loop can be
        stopped. Causes the server connection's OnStopAdvise() member to be
        called.

        @return @true if the server okays it, @false otherwise.
    */
    bool StopAdvise(const wxString& item);
};



/**
    @class wxDDEClient

    A wxDDEClient object represents the client part of a client-server DDE
    (Dynamic Data Exchange) conversation.

    To create a client which can communicate with a suitable server, you need
    to derive a class from wxDDEConnection and another from wxDDEClient. The
    custom wxDDEConnection class will intercept communications in a
    "conversation" with a server, and the custom wxDDEServer is required so
    that a user-overridden OnMakeConnection() member can return a
    wxDDEConnection of the required class, when a connection is made.

    This DDE-based implementation is available on Windows only, but a
    platform-independent, socket-based version of this API is available using
    wxTCPClient.

    @library{wxbase}
    @category{ipc}
    @onlyfor{wxmsw}

    @see wxDDEServer, wxDDEConnection, @ref overview_ipc
*/
class wxDDEClient : public wxObject
{
public:
    /**
        Constructs a client object.
    */
    wxDDEClient();

    /**
        Tries to make a connection with a server specified by the host (machine
        name under UNIX, ignored under Windows), service name (must contain an
        integer port number under UNIX), and topic string. If the server allows
        a connection, a wxDDEConnection object will be returned.

        The type of wxDDEConnection returned can be altered by overriding the
        OnMakeConnection() member to return your own derived connection object.
    */
    wxConnectionBase* MakeConnection(const wxString& host,
                                     const wxString& service,
                                     const wxString& topic);

    /**
        The type of wxDDEConnection returned from a MakeConnection() call can
        be altered by deriving the OnMakeConnection() member to return your own
        derived connection object. By default, a wxDDEConnection object is
        returned.

        The advantage of deriving your own connection class is that it will
        enable you to intercept messages initiated by the server, such as
        wxDDEConnection::OnAdvise(). You may also want to store
        application-specific data in instances of the new class.
    */
    wxConnectionBase* OnMakeConnection();

    /**
        Returns @true if this is a valid host name, @false otherwise. This
        always returns @true under MS Windows.
    */
    bool ValidHost(const wxString& host);
};



/**
    @class wxDDEServer

    A wxDDEServer object represents the server part of a client-server DDE
    (Dynamic Data Exchange) conversation.

    This DDE-based implementation is available on Windows only, but a
    platform-independent, socket-based version of this API is available using
    wxTCPServer.

    @library{wxbase}
    @category{ipc}
    @onlyfor{wxmsw}

    @see wxDDEClient, wxDDEConnection, @ref overview_ipc
*/
class wxDDEServer
{
public:
    /**
        Constructs a server object.
    */
    wxDDEServer();

    /**
        Registers the server using the given service name. Under UNIX, the
        string must contain an integer id which is used as an Internet port
        number. @false is returned if the call failed (for example, if the port
        number is already in use).
    */
    bool Create(const wxString& service);

    /**
        When a client calls wxDDEClient::MakeConnection(), the server receives
        the message and this member is called. The application should derive a
        member to intercept this message and return a connection object of
        either the standard wxDDEConnection type, or of a user-derived type.

        If the @a topic is "STDIO", the application may wish to refuse the
        connection. Under UNIX, when a server is created the
        OnAcceptConnection() message is always sent for standard input and
        output, but in the context of DDE messages it doesn't make a lot of
        sense.
    */
    virtual wxConnectionBase* OnAcceptConnection(const wxString& topic);
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_misc */
//@{

/**
    Called when wxWidgets exits, to clean up the DDE system. This no longer
    needs to be called by the application.

    @see wxDDEInitialize()

    @header{wx/dde.h}
*/
void wxDDECleanUp();

/**
    Initializes the DDE system. May be called multiple times without harm.

    This no longer needs to be called by the application: it will be called by
    wxWidgets if necessary.

    @see wxDDEServer, wxDDEClient, wxDDEConnection, wxDDECleanUp()

    @header{wx/dde.h}
*/
void wxDDEInitialize();

//@}

