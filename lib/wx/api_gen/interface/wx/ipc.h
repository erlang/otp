/////////////////////////////////////////////////////////////////////////////
// Name:        ipc.h
// Purpose:     interface of wxConnection
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxConnection

    A wxConnection object represents the connection between a client and a server.
    It is created by making a connection using a wxClient object, or by the acceptance
    of a connection by a wxServer object.

    The bulk of a DDE-like (Dynamic Data Exchange) conversation is controlled by
    calling members in a @b wxConnection object or by overriding its members.
    The actual DDE-based implementation using wxDDEConnection is available on
    Windows only, but a platform-independent, socket-based version of this API is
    available using wxTCPConnection, which has the same API.

    An application should normally derive a new connection class from wxConnection,
    in order to override the communication event handlers to do something interesting.

    @library{wxbase}
    @category{ipc}

    @see wxClient, wxServer, @ref overview_ipc
*/
class wxConnection : public wxObject
{
public:
    //@{
    /**
        Constructs a connection object.

        If no user-defined connection object is to be derived from wxConnection,
        then the constructor should not be called directly, since the default connection
        object will be provided on requesting (or accepting) a connection.

        However, if the user defines his or her own derived connection object,
        the wxServer::OnAcceptConnection and/or wxClient::OnMakeConnection
        members should be replaced by functions which construct the new
        connection object.

        If the arguments of the wxConnection constructor are void then
        the wxConnection object manages its own connection buffer,
        allocating memory as needed. A programmer-supplied buffer cannot
        be increased if necessary, and the program will assert if it is
        not large enough.

        The programmer-supplied buffer is included mainly for backwards compatibility.
    */
    wxConnection();
    wxConnection(void* buffer, size_t size);
    //@}

    //@{
    /**
        Called by the server application to advise the client of a change
        in the data associated with the given item. Causes the client
        connection's OnAdvise() member to be called.

        @return @true if successful.
    */
    bool Advise(const wxString& item, const void* data, size_t size,
                wxIPCFormat format = wxIPC_PRIVATE);
    bool Advise(const wxString& item, const char* data);
    bool Advise(const wxString& item, const wchar_t* data);
    bool Advise(const wxString& item, const wxString data);
    //@}

    /**
        Called by the client or server application to disconnect from the
        other program; it causes the OnDisconnect() message to be sent to the
        corresponding connection object in the other program.

        Returns @true if successful or already disconnected.
        The application that calls Disconnect() must explicitly delete
        its side of the connection.
    */
    bool Disconnect();

    //@{
    /**
        Called by the client application to execute a command on the server.
        Can also be used to transfer arbitrary data to the server (similar to
        Poke() in that respect). Causes the server connection's OnExec()
        member to be called. Returns @true if successful.
    */
    bool Execute(const void* data, size_t size,
                 wxIPCFormat format = wxIPC_PRIVATE);
    bool Execute(const char* data);
    bool Execute(const wchar_t* data);
    bool Execute(const wxString data);
    //@}

    /**
        Message sent to the client application when the server notifies it of a
        change in the data associated with the given item, using Advise().
    */
    virtual bool OnAdvise(const wxString& topic,
                          const wxString& item,
                          const void* data,
                          size_t size,
                          wxIPCFormat format);

    /**
        Message sent to the client or server application when the other
        application notifies it to end the connection.

        The default behaviour is to delete the connection object and return @true,
        so applications should generally override OnDisconnect() (finally calling
        the inherited method as well) so that they know the connection object is
        no longer available.
    */
    virtual bool OnDisconnect();

    /**
        Message sent to the server application when the client notifies
        it to execute the given data, using Execute().

        Note that there is no item associated with this message.
    */
    virtual bool OnExec(const wxString& topic, const wxString& data);

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
        The server's OnRequest() method should respond by returning a character
        string, or @NULL to indicate no data, and setting *size.

        The character string must of course persist after the call returns.
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
        Can be used to transfer arbitrary data to the server.
        Causes the server connection's OnPoke() member to be called.
        If size is -1 the size is computed from the string length of data.

        Returns @true if successful.
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
        Size may be @NULL or a pointer to a variable to receive the size of the
        requested item.

        Returns a character string (actually a pointer to the connection's buffer)
        if successful, @NULL otherwise. This buffer does not need to be deleted.
    */
    const void* Request(const wxString& item, size_t* size,
                        wxIPCFormat format = wxIPC_TEXT);

    /**
        Called by the client application to ask if an advise loop can be started
        with the server. Causes the server connection's OnStartAdvise()
        member to be called.
        Returns @true if the server okays it, @false otherwise.
    */
    bool StartAdvise(const wxString& item);

    /**
        Called by the client application to ask if an advise loop can be stopped.
        Causes the server connection's OnStopAdvise() member to be called.
        Returns @true if the server okays it, @false otherwise.
    */
    bool StopAdvise(const wxString& item);

    /**
        Returns true if the format is one of the text formats.

        The text formats are wxIPC_TEXT, wxIPC_UNICODETEXT and wxIPC_UTF8TEXT.
     */
    static bool IsTextFormat(wxIPCFormat format);

    /**
        Returns the data in any of the text formats as string.

        @param data
            The raw data pointer as used with any of the other methods of this
            class.
        @param size
            The size of the data buffer pointed to by @a data.
        @param format
            The format of the data. It must be a text one, i.e. such that
            IsTextFormat() returns @true for it.
        @return
            The string representation of the data. If the format is not text,
            an assertion failure is triggered and empty string is returned.
     */
    static wxString
    GetTextFromData(const void *data, size_t size, wxIPCFormat format);
};



/**
    @class wxClient

    A wxClient object represents the client part of a client-server
    DDE-like (Dynamic Data Exchange) conversation.
    The actual DDE-based implementation using wxDDEClient is available on Windows
    only, but a platform-independent, socket-based version of this API is available
    using wxTCPClient, which has the same API.

    To create a client which can communicate with a suitable server, you need to
    derive a class from wxConnection and another from wxClient.
    The custom wxConnection class will intercept communications in a 'conversation'
    with a server, and the custom wxClient is required so that a user-overridden
    wxClient::OnMakeConnection member can return a wxConnection of the required
    class, when a connection is made.

    Look at the IPC sample and the @ref overview_ipc for an example of how to do this.

    @library{wxbase}
    @category{ipc}

    @see wxServer, wxConnection, @ref overview_ipc
*/
class wxClient : public wxObject
{
public:
    /**
        Constructs a client object.
    */
    wxClient();

    /**
        Tries to make a connection with a server by host (machine name
        under UNIX - use 'localhost' for same machine; ignored when using
        native DDE in Windows), service name and topic string.

        If the server allows a connection, a wxConnection object will be returned.
        The type of wxConnection returned can be altered by overriding the
        OnMakeConnection() member to return your own derived connection object.

        Under Unix, the service name may be either an integer port
        identifier in which case an Internet domain socket will be used
        for the communications, or a valid file name (which shouldn't
        exist and will be deleted afterwards) in which case a Unix domain
        socket is created.

        @note Using Internet domain sockets is extremely insecure for IPC as
              there is absolutely no access control for them, use Unix domain
              sockets whenever possible!
    */
    wxConnectionBase* MakeConnection(const wxString& host,
                                     const wxString& service,
                                     const wxString& topic);

    /**
        Called by MakeConnection(), by default this simply returns a new wxConnection
        object. Override this method to return a wxConnection descendant customised
        for the application.

        The advantage of deriving your own connection class is that it will enable
        you to intercept messages initiated by the server, such as wxConnection::OnAdvise.
        You  may also want to store application-specific data in instances of
        the new class.
    */
    wxConnectionBase* OnMakeConnection();

    /**
        Returns @true if this is a valid host name, @false otherwise.
        This always returns @true under MS Windows.
    */
    bool ValidHost(const wxString& host);
};



/**
    @class wxServer

    A wxServer object represents the server part of a client-server DDE-like
    (Dynamic Data Exchange) conversation. The actual DDE-based implementation
    using wxDDEServer is available on Windows only, but a platform-independent,
    socket-based version of this API is available using wxTCPServer, which has
    the same API.

    To create a server which can communicate with a suitable client, you need to
    derive a class from wxConnection and another from wxServer.
    The custom wxConnection class will intercept communications in a 'conversation'
    with a client, and the custom wxServer is required so that a user-overridden
    wxServer::OnAcceptConnection member can return a wxConnection of the required
    class, when a connection is made.
    Look at the IPC sample and the @ref overview_ipc for an example of how to do this.

    @library{wxbase}
    @category{ipc}

    @see wxClient, wxConnection, IPC, @ref overview_ipc
*/
class wxServer
{
public:
    /**
        Constructs a server object.
    */
    wxServer();

    /**
        Registers the server using the given service name.
        Under Unix, the service name may be either an integer port identifier in
        which case an Internet domain socket will be used for the communications,
        or a valid file name (which shouldn't exist and will be deleted afterwards)
        in which case a Unix domain socket is created.

        @false is returned if the call failed (for example, the port number is
        already in use).
    */
    bool Create(const wxString& service);

    /**
        When a client calls @b MakeConnection, the server receives the
        message and this member is called. The application should derive a
        member to intercept this message and return a connection object of
        either the standard wxConnection type, or (more likely) of a
        user-derived type.

        If the topic is @b STDIO, the application may wish to refuse the
        connection. Under UNIX, when a server is created the OnAcceptConnection()
        message is always sent for standard input and output, but in the context
        of DDE messages it doesn't make a lot of sense.
    */
    virtual wxConnectionBase* OnAcceptConnection(const wxString& topic);
};

