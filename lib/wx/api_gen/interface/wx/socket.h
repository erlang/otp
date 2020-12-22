/////////////////////////////////////////////////////////////////////////////
// Name:        socket.h
// Purpose:     interface of wxIP*address, wxSocket* classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    The type of the native socket.

    Notice that the definition below is simplified and this type is not always
    int, e.g. it is a 64 bit integer type under Win64.

    @since 2.9.5
  */
typedef int wxSOCKET_T;

/**
    @class wxIPaddress

    wxIPaddress is an abstract base class for all internet protocol address
    objects. Currently, only wxIPV4address is implemented. An experimental
    implementation for IPV6, wxIPV6address, is being developed.

    @library{wxnet}
    @category{net}
*/
class wxIPaddress : public wxSockAddress
{
public:
    /**
        Internally, this is the same as setting the IP address to @b INADDR_ANY.

        On IPV4 implementations, 0.0.0.0

        On IPV6 implementations, ::

        @return @true on success, @false if something went wrong.
    */
    bool AnyAddress();

    /**
        Internally, this is the same as setting the IP address to @b INADDR_BROADCAST.

        On IPV4 implementations, 255.255.255.255

        @return @true on success, @false if something went wrong.
    */
    virtual bool BroadcastAddress() = 0;

    /**
        Set the address to hostname, which can be a host name or an IP-style address
        in a format dependent on implementation.

        @return @true on success, @false if something goes wrong (invalid
                hostname or invalid IP address).
    */
    bool Hostname(const wxString& hostname);

    /**
        Returns the hostname which matches the IP address.
    */
    wxString Hostname() const;

    /**
        Returns a wxString containing the IP address.
    */
    virtual wxString IPAddress() const = 0;

    /**
        Determines if current address is set to localhost.

        @return @true if address is localhost, @false if internet address.
    */
    virtual bool IsLocalHost() const = 0;

    /**
        Set address to localhost.

        On IPV4 implementations, 127.0.0.1

        On IPV6 implementations, ::1

        @return @true on success, @false if something went wrong.
    */
    bool LocalHost();

    /**
        Set the port to that corresponding to the specified service.

        @return @true on success, @false if something goes wrong (invalid @a service).
    */
    bool Service(const wxString& service);

    /**
        Set the port to that corresponding to the specified service.

        @return @true on success, @false if something goes wrong (invalid @a service).
    */
    bool Service(unsigned short service);

    /**
        Returns the current service.
    */
    unsigned short Service() const;
};


/**
    @class wxIPV4address

    A class for working with IPv4 network addresses.

    @library{wxnet}
    @category{net}
*/
class wxIPV4address : public wxIPaddress
{
public:
    /**
        Set address to any of the addresses of the current machine.

        Whenever possible, use this function instead of LocalHost(),
        as this correctly handles multi-homed hosts and avoids other small
        problems. Internally, this is the same as setting the IP address
        to @b INADDR_ANY.

        @return @true on success, @false if something went wrong.
    */
    bool AnyAddress();

    /**
        Set the address to hostname, which can be a host name or an IP-style address
        in dot notation(<tt>a.b.c.d</tt>).

        @return @true on success, @false if something goes wrong (invalid
                hostname or invalid IP address).
    */
    bool Hostname(const wxString& hostname);

    /**
        Returns the hostname which matches the IP address.
    */
    virtual wxString Hostname() const;

    /**
        Returns a wxString containing the IP address in dot quad (127.0.0.1) format.
    */
    virtual wxString IPAddress() const;

    /**
        Set address to localhost (127.0.0.1).

        Whenever possible, use AnyAddress() instead of this one, as that one will
        correctly handle multi-homed hosts and avoid other small problems.

        @return @true on success, @false if something went wrong.
    */
    bool LocalHost();

    /**
        Set the port to that corresponding to the specified @a service.

        @return @true on success, @false if something goes wrong (invalid @a service).
    */
    bool Service(const wxString& service);

    /**
        Set the port to that corresponding to the specified @a service.

        @return @true on success, @false if something goes wrong (invalid @a service).
    */
    bool Service(unsigned short service);

    /**
        Returns the current service.
    */
    unsigned short Service() const;
};



/**
    @class wxSocketServer

    @todo describe me.

    @library{wxnet}
    @category{net}
*/
class wxSocketServer : public wxSocketBase
{
public:
    /**
        Constructs a new server and tries to bind to the specified @e address.

        Before trying to accept new connections, remember to test whether it succeeded
        with wxSocketBase:IsOk().

        @param address
            Specifies the local address for the server (e.g. port number).
        @param flags
            Socket flags (See wxSocketBase::SetFlags()).
    */
    wxSocketServer(const wxSockAddress& address,
                   wxSocketFlags flags = wxSOCKET_NONE);

    /**
        Destructor (it doesn't close the accepted connections).
    */
    virtual ~wxSocketServer();

    /**
        Accepts an incoming connection request, and creates a new wxSocketBase
        object which represents the server-side of the connection.

        If @a wait is @true and there are no pending connections to be
        accepted, it will wait for the next incoming connection to
        arrive.

        @warning This method will block the GUI.

        If @a wait is @false, it will try to accept a pending connection
        if there is one, but it will always return immediately without blocking
        the GUI. If you want to use Accept() in this way, you can either check for
        incoming connections with WaitForAccept() or catch @b wxSOCKET_CONNECTION events,
        then call Accept() once you know that there is an incoming connection waiting
        to be accepted.

        @return Returns an opened socket connection, or @NULL if an error
                occurred or if the wait parameter was @false and there
                were no pending connections.

        @see WaitForAccept(), wxSocketBase::SetNotify(),
             wxSocketBase::Notify(), AcceptWith()
    */
    wxSocketBase* Accept(bool wait = true);

    /**
        Accept an incoming connection using the specified socket object.

        @param socket
            Socket to be initialized
        @param wait
            See Accept() for more info.

        @return Returns @true on success, or @false if an error occurred or
                if the wait parameter was @false and there were no pending
                connections.

        @see WaitForAccept(), wxSocketBase::SetNotify(),
             wxSocketBase::Notify(), Accept()
    */
    bool AcceptWith(wxSocketBase& socket, bool wait = true);

    /**
        Wait for an incoming connection.

        Use it if you want to call Accept() or AcceptWith() with @e wait set
        to @false, to detect when an incoming connection is waiting to be accepted.

        @param seconds
            Number of seconds to wait. If -1, it will wait for the default
            timeout, as set with wxSocketBase::SetTimeout().
        @param millisecond
            Number of milliseconds to wait.

        @return @true if an incoming connection arrived, @false if the timeout
                elapsed.

        @see Accept(), AcceptWith(), wxSocketBase::InterruptWait()
    */
    bool WaitForAccept(long seconds = -1, long millisecond = 0);
};


/**
    @class wxSocketClient

    @todo describe me.

    @library{wxnet}
    @category{net}
*/
class wxSocketClient : public wxSocketBase
{
public:
    /**
        Constructor.

        Notice that if the object is created from a worker thread or if it is
        created from the main thread but the event loop is not running, @a
        flags parameter @em must include ::wxSOCKET_BLOCK as non-blocking
        sockets require dispatching events, which can only be done in the main
        thread.

        @param flags
            Socket flags (See wxSocketBase::SetFlags())
    */
    wxSocketClient(wxSocketFlags flags = wxSOCKET_NONE);

    /**
        Destructor. Please see wxSocketBase::Destroy().
    */
    virtual ~wxSocketClient();

    /**
        Connects to a server using the specified address.

        If @a wait is @true, Connect() will wait until the connection
        completes.

        @warning This method will block the GUI.

        If @a wait is @false, Connect() will try to establish the connection
        and return immediately, without blocking the GUI. When used this way,
        even if Connect() returns @false, the connection request can be
        completed later. To detect this, use WaitOnConnect(), or catch
        @b wxSOCKET_CONNECTION events (for successful establishment) and
        @b wxSOCKET_LOST events (for connection failure).

        @param address
            Address of the server.
        @param wait
            If @true, waits for the connection to complete.

        @return @true if the connection is established and no error occurs.
                If @a wait was true, and Connect() returns @false, an error
                occurred and the connection failed.
                If @a wait was @false, and Connect() returns @false, you should
                still be prepared to handle the completion of this connection request,
                either with WaitOnConnect() or by watching wxSOCKET_CONNECTION
                and wxSOCKET_LOST events.

        @see WaitOnConnect(), wxSocketBase::SetNotify(), wxSocketBase::Notify()
    */
    virtual bool Connect(const wxSockAddress& address, bool wait = true);

    /**
        Connects to a server using the specified address.

        If @a wait is @true, Connect() will wait until the connection
        completes. @b Warning: This will block the GUI.

        If @a wait is @false, Connect() will try to establish the connection
        and return immediately, without blocking the GUI. When used this way,
        even if Connect() returns @false, the connection request can be
        completed later. To detect this, use WaitOnConnect(), or catch
        @b wxSOCKET_CONNECTION events (for successful establishment) and
        @b wxSOCKET_LOST events (for connection failure).

        @param address
            Address of the server.
        @param local
            Bind to the specified local address and port before connecting.
            The local address and port can also be set using SetLocal(),
            and then using the 2-parameter Connect() method.
        @param wait
            If @true, waits for the connection to complete.

        @return @true if the connection is established and no error occurs.
                If @a wait was true, and Connect() returns @false, an error
                occurred and the connection failed.
                If @a wait was @false, and Connect() returns @false, you should
                still be prepared to handle the completion of this connection request,
                either with WaitOnConnect() or by watching wxSOCKET_CONNECTION
                and wxSOCKET_LOST events.

        @see WaitOnConnect(), wxSocketBase::SetNotify(), wxSocketBase::Notify()
    */
    bool Connect(const wxSockAddress& address, const wxSockAddress& local,
                 bool wait = true);

    /**
        Wait until a connection request completes, or until the specified timeout
        elapses. Use this function after issuing a call to Connect() with
        @e wait set to @false.

        @param seconds
            Number of seconds to wait.
            If -1, it will wait for the default timeout, as set with wxSocketBase::SetTimeout().
        @param milliseconds
            Number of milliseconds to wait.

        @return
            WaitOnConnect() returns @true if the connection request completes.
            This does not necessarily mean that the connection was
            successfully established; it might also happen that the
            connection was refused by the peer. Use wxSocketBase::IsConnected()
            to distinguish between these two situations.
            @n @n If the timeout elapses, WaitOnConnect() returns @false.
            @n @n These semantics allow code like this:
            @code
            // Issue the connection request
            client->Connect(addr, false);

            // Wait until the request completes or until we decide to give up
            bool waitmore = true;
            while ( !client->WaitOnConnect(seconds, millis) && waitmore )
            {
                // possibly give some feedback to the user,
                // and update waitmore as needed.
            }
            bool success = client->IsConnected();
            @endcode
    */
    bool WaitOnConnect(long seconds = -1, long milliseconds = 0);
};



/**
    @class wxSockAddress

    You are unlikely to need to use this class: only wxSocketBase uses it.

    @library{wxnet}
    @category{net}

    @see wxSocketBase, wxIPaddress, wxIPV4address
*/
class wxSockAddress : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxSockAddress();

    /**
        Default destructor.
    */
    virtual ~wxSockAddress();

    /**
        Delete all information about the address.
    */
    virtual void Clear();

    /**
        Returns the length of the socket address.
    */
    int SockAddrLen();

    /**
        Returns the pointer to the low-level representation of the address.

        This can be used to pass socket address information to a 3rd party
        library.

        @return
            Pointer to a sockaddr-derived struct.
    */
    const sockaddr *GetAddressData() const;

    /**
        Returns the length of the buffer retrieved by GetAddressData().

        @return
            The size of the sockaddr-derived struct corresponding to this
            address.
    */
    int GetAddressDataLen() const;
};



/**
    @class wxSocketEvent

    This event class contains information about socket events.
    This kind of events are sent to the event handler specified with
    wxSocketBase::SetEventHandler.

    @beginEventTable{wxSocketEvent}
    @event{EVT_SOCKET(id, func)}
        Process a socket event, supplying the member function.
    @endEventTable

    @library{wxnet}
    @category{net}

    @see wxSocketBase, wxSocketClient, wxSocketServer
*/
class wxSocketEvent : public wxEvent
{
public:
    /**
        Constructor.
    */
    wxSocketEvent(int id = 0);

    /**
        Gets the client data of the socket which generated this event, as
        set with wxSocketBase::SetClientData().
    */
    void* GetClientData() const;

    /**
        Returns the socket object to which this event refers to.
        This makes it possible to use the same event handler for different sockets.
    */
    wxSocketBase* GetSocket() const;

    /**
        Returns the socket event type.
    */
    wxSocketNotify GetSocketEvent() const;
};


/**
    wxSocket error return values.
*/
enum wxSocketError
{
    wxSOCKET_NOERROR,       ///< No error happened.
    wxSOCKET_INVOP,         ///< Invalid operation.
    wxSOCKET_IOERR,         ///< Input/Output error.
    wxSOCKET_INVADDR,       ///< Invalid address passed to wxSocket.
    wxSOCKET_INVSOCK,       ///< Invalid socket (uninitialized).
    wxSOCKET_NOHOST,        ///< No corresponding host.
    wxSOCKET_INVPORT,       ///< Invalid port.
    wxSOCKET_WOULDBLOCK,    ///< The socket is non-blocking and the operation would block.
    wxSOCKET_TIMEDOUT,      ///< The timeout for this operation expired.
    wxSOCKET_MEMERR         ///< Memory exhausted.
};


/**
    @anchor wxSocketEventFlags

    wxSocket Event Flags.

    A brief note on how to use these events:

    The @b wxSOCKET_INPUT event will be issued whenever there is data available
    for reading. This will be the case if the input queue was empty and new data
    arrives, or if the application has read some data yet there is still more data
    available. This means that the application does not need to read all available
    data in response to a @b wxSOCKET_INPUT event, as more events will be produced
    as necessary.

    The @b wxSOCKET_OUTPUT event is issued when a socket is first connected with
    Connect() or accepted with Accept(). After that, new events will be generated
    only after an output operation fails with @b wxSOCKET_WOULDBLOCK and buffer space
    becomes available again. This means that the application should assume that it can
    write data to the socket until an @b wxSOCKET_WOULDBLOCK error occurs; after this,
    whenever the socket becomes writable again the application will be notified with
    another @b wxSOCKET_OUTPUT event.

    The @b wxSOCKET_CONNECTION event is issued when a delayed connection request completes
    successfully (client) or when a new connection arrives at the incoming queue (server).

    The @b wxSOCKET_LOST event is issued when a close indication is received for the socket.
    This means that the connection broke down or that it was closed by the peer. Also, this
    event will be issued if a connection request fails.
*/
enum wxSocketEventFlags
{
    wxSOCKET_INPUT,         ///< There is data available for reading.
    wxSOCKET_OUTPUT,        ///< The socket is ready to be written to.
    wxSOCKET_CONNECTION,    ///< Incoming connection request (server), or
                            ///< successful connection establishment (client).
    wxSOCKET_LOST           ///< The connection has been closed.
};


/**
    @anchor wxSocketFlags

    wxSocket Flags.

    A brief overview on how to use these flags follows.

    If no flag is specified (this is the same as @b wxSOCKET_NONE),
    IO calls will return after some data has been read or written, even
    when the transfer might not be complete. This is the same as issuing
    exactly one blocking low-level call to @b recv() or @b send(). Note
    that @e blocking here refers to when the function returns, not
    to whether the GUI blocks during this time.

    If @b wxSOCKET_NOWAIT is specified, IO calls will return immediately.
    Read operations will retrieve only available data. Write operations will
    write as much data as possible, depending on how much space is available
    in the output buffer. This is the same as issuing exactly one nonblocking
    low-level call to @b recv() or @b send(). Note that @e nonblocking here
    refers to when the function returns, not to whether the GUI blocks during
    this time.  Also note that this flag impacts both Read and Write
    operations.  If it is desired to control Read independently of Write, for
    example you want no wait on Read(), but you do want to wait on Write(), then
    use wxSOCKET_NOWAIT_READ and wxSOCKET_NOWAIT_WRITE.

    If @b wxSOCKET_NOWAIT_READ (this flag is new since wxWidgets 2.9.5) is
    specified, Read operations will return immediately. Read operations will
    retrieve only available data. This is the same as issuing exactly one
    nonblocking low-level call to @b recv(). Note that @e nonblocking here
    refers to when the function returns, not to whether the GUI blocks during
    this time.  This flag should not be enabled if ReadMsg() is going to be
    used (it will be ignored), if you do then thread-safety may be at risk.
    Note that wxSOCKET_NOWAIT_READ impacts only Read operations and does not
    impact Write operations, allowing Read and Write operations to be set
    differently.

    If @b wxSOCKET_NOWAIT_WRITE (this flag is new since wxWidgets 2.9.5) is
    specified, Write operations will return immediately. Write operations will
    write as much data as possible, depending on how much space is available in
    the output buffer. This is the same as issuing exactly one nonblocking
    low-level call to @b send(). Note that @e nonblocking here refers to when
    the function returns, not to whether the GUI blocks during this time.  This
    flag should not be enabled if WriteMsg() is going to be used (it will be
    ignored), if you use it then thread safety may be at risk. Note that
    wxSOCKET_NOWAIT_WRITE impacts only Write operations and does not impact
    Read operations, allowing Read and Write operations to be set differently.

    If @b wxSOCKET_WAITALL is specified, IO calls won't return until ALL
    the data has been read or written (or until an error occurs), blocking if
    necessary, and issuing several low level calls if necessary. This is the
    same as having a loop which makes as many blocking low-level calls to
    @b recv() or @b send() as needed so as to transfer all the data. Note
    that @e blocking here refers to when the function returns, not
    to whether the GUI blocks during this time.  Note that wxSOCKET_WAITALL
    impacts both Read and Write operations.  If you desire to wait
    for all on just Read operations, but not on Write operations, (or vice versa),
    use wxSOCKET_WAITALL_READ or wxSOCKET_WAITALL_WRITE.

    If @b wxSOCKET_WAITALL_READ (this flag is new since wxWidgets 2.9.5) is
    specified, Read operations won't return until ALL the data has been read
    (or until an error occurs), blocking if necessary, and issuing several low
    level calls if necessary. This is the same as having a loop which makes as
    many blocking low-level calls to @b recv() as needed so as to transfer all
    the data. Note that @e blocking here refers to when the function returns,
    not to whether the GUI blocks during this time.  Note that
    wxSOCKET_WAITALL_READ only has an impact on Read operations, and has no
    impact on Write operations, allowing Read and Write operations to have
    different settings.

    If @b wxSOCKET_WAITALL_WRITE (this flag is new since wxWidgets 2.9.5) is
    specified, Write() and WriteMsg() calls won't return until ALL the data has
    been written (or until an error occurs), blocking if necessary, and issuing
    several low level calls if necessary. This is the same as having a loop
    which makes as many blocking low-level calls to @b send() as needed so as
    to transfer all the data. Note that @e blocking here refers to when the
    function returns, not to whether the GUI blocks during this time.  Note
    that wxSOCKET_WAITALL_WRITE only has an impact on Write operations, and has
    no impact on Read operations, allowing Read and Write operations to have
    different settings.

    The @b wxSOCKET_BLOCK flag controls whether the GUI blocks during
    IO operations. If this flag is specified, the socket will not yield
    during IO calls, so the GUI will remain blocked until the operation
    completes. If it is not used, then the application must take extra
    care to avoid unwanted reentrance.

    The @b wxSOCKET_REUSEADDR flag controls the use of the @b SO_REUSEADDR standard
    @b setsockopt() flag. This flag allows the socket to bind to a port that is
    already in use. This is mostly used on UNIX-based systems to allow rapid starting
    and stopping of a server, otherwise you may have to wait several minutes for the
    port to become available.

    @b wxSOCKET_REUSEADDR can also be used with socket clients to (re)bind to a
    particular local port for an outgoing connection.
    This option can have surprising platform dependent behaviour, so check the
    documentation for your platform's implementation of setsockopt().

    Note that on BSD-based systems(e.g. macOS), use of
    @b wxSOCKET_REUSEADDR implies @b SO_REUSEPORT in addition to
    @b SO_REUSEADDR to be consistent with Windows.

    The @b wxSOCKET_BROADCAST flag controls the use of the @b SO_BROADCAST standard
    @b setsockopt() flag. This flag allows the socket to use the broadcast address,
    and is generally used in conjunction with @b wxSOCKET_NOBIND and
    wxIPaddress::BroadcastAddress().

    So:
    - @b wxSOCKET_NONE will try to read at least SOME data, no matter how much.
    - @b wxSOCKET_NOWAIT will always return immediately, even if it cannot
      read or write ANY data.
    - @b wxSOCKET_WAITALL will only return when it has read or written ALL
      the data.
    - @b wxSOCKET_BLOCK has nothing to do with the previous flags and
      it controls whether the GUI blocks.
    - @b wxSOCKET_REUSEADDR controls special platform-specific behaviour for
      reusing local addresses/ports.
*/
enum
{
    wxSOCKET_NONE = 0,      ///< Normal functionality.
    wxSOCKET_NOWAIT = 1,    ///< Read/write as much data as possible and return immediately.
    wxSOCKET_WAITALL = 2,   ///< Wait for all required data to be read/written unless an error occurs.
    wxSOCKET_BLOCK = 4,     ///< Block the GUI (do not yield) while reading/writing data.
    wxSOCKET_REUSEADDR = 8, ///< Allows the use of an in-use port.
    wxSOCKET_BROADCAST = 16, ///< Switches the socket to broadcast mode
    wxSOCKET_NOBIND = 32,   ///< Stops the socket from being bound to a specific
                            ///< adapter (normally used in conjunction with
                            ///< @b wxSOCKET_BROADCAST)
    wxSOCKET_NOWAIT_READ = 64,    ///< Read as much data as possible and return immediately
    wxSOCKET_WAITALL_READ = 128,  ///< Wait for all required data to be read unless an error occurs.
    wxSOCKET_NOWAIT_WRITE = 256,   ///< Write as much data as possible and return immediately
    wxSOCKET_WAITALL_WRITE = 512   ///< Wait for all required data to be written unless an error occurs.
};


/**
    @class wxSocketBase

    wxSocketBase is the base class for all socket-related objects, and it
    defines all basic IO functionality.

    @note
    When using wxSocket from multiple threads, even implicitly (e.g. by using
    wxFTP or wxHTTP in another thread) you must initialize the sockets from the
    main thread by calling Initialize() before creating the other ones.

    @beginEventEmissionTable{wxSocketEvent}
    @event{EVT_SOCKET(id, func)}
        Process a @c wxEVT_SOCKET event.
        See @ref wxSocketEventFlags and @ref wxSocketFlags for more info.
    @endEventTable

    @library{wxnet}
    @category{net}

    @see wxSocketEvent, wxSocketClient, wxSocketServer, @sample{sockets},
         @ref wxSocketFlags, ::wxSocketEventFlags, ::wxSocketError
*/
class wxSocketBase : public wxObject
{
public:

    /**
        @name Construction and Destruction
    */
    //@{

    /**
        Default constructor.

        Don't use it directly; instead, use wxSocketClient to construct a socket client,
        or wxSocketServer to construct a socket server.
    */
    wxSocketBase();

    /**
        Destructor.

        Do not destroy a socket using the delete operator directly;
        use Destroy() instead. Also, do not create socket objects in the stack.
    */
    virtual ~wxSocketBase();

    /**
        Destroys the socket safely.

        Use this function instead of the delete operator, since otherwise socket events
        could reach the application even after the socket has been destroyed. To prevent
        this problem, this function appends the wxSocket to a list of object to be deleted
        on idle time, after all events have been processed. For the same reason, you should
        avoid creating socket objects in the stack.

        Destroy() calls Close() automatically.

        @return Always @true.
    */
    bool Destroy();

    /**
        Perform the initialization needed in order to use the sockets.

        This function is called from wxSocket constructor implicitly and so
        normally doesn't need to be called explicitly. There is however one
        important exception: as this function must be called from the main
        (UI) thread, if you use wxSocket from multiple threads you must call
        Initialize() from the main thread before creating wxSocket objects in
        the other ones.

        It is safe to call this function multiple times (only the first call
        does anything) but you must call Shutdown() exactly once for every call
        to Initialize().

        This function should only be called from the main thread.

        @return
            @true if the sockets can be used, @false if the initialization
            failed and sockets are not available at all.
     */
    static bool Initialize();

    /**
        Shut down the sockets.

        This function undoes the call to Initialize() and must be called after
        every successful call to Initialize().

        This function should only be called from the main thread, just as
        Initialize().
     */
    static void Shutdown();

    //@}


    /**
        @name Socket State
    */
    //@{

    /**
        Returns @true if an error occurred in the last IO operation.

        Use this function to check for an error condition after one of the
        following calls: Discard(), Peek(), Read(), ReadMsg(), Unread(), Write(), WriteMsg().
    */
    bool Error() const;

    /**
        Return the local address of the socket.

        @return @true if no error happened, @false otherwise.
    */
    virtual bool GetLocal(wxSockAddress& addr) const;

    /**
        Return the peer address field of the socket.

        @return @true if no error happened, @false otherwise.
    */
    virtual bool GetPeer(wxSockAddress& addr) const;

    /**
        Return the socket timeout in seconds.

        The timeout can be set using SetTimeout() and is 10 minutes by default.
     */
    long GetTimeout() const;

    /**
        Returns @true if the socket is connected.
    */
    bool IsConnected() const;

    /**
        Check if the socket can be currently read or written.

        This might mean that queued data is available for reading or, for streamed
        sockets, that the connection has been closed, so that a read operation will
        complete immediately without blocking (unless the @b wxSOCKET_WAITALL flag
        is set, in which case the operation might still block).
    */
    bool IsData();

    /**
        Returns @true if the socket is not connected.
    */
    bool IsDisconnected() const;

    /**
        Returns @true if the socket is initialized and ready and @false in other
        cases.

        @remarks
        For wxSocketClient, IsOk() won't return @true unless the client is connected to a server.
        For wxSocketServer, IsOk() will return @true if the server could bind to the specified address
        and is already listening for new connections.
        IsOk() does not check for IO errors; use Error() instead for that purpose.
    */
    bool IsOk() const;

    /**
        Returns the number of bytes read or written by the last IO call.

        Use this function to get the number of bytes actually transferred
        after using one of the following IO calls: Discard(), Peek(), Read(),
        ReadMsg(), Unread(), Write(), WriteMsg().

        @deprecated
        This function is kept mostly for backwards compatibility.  Use
        LastReadCount() or LastWriteCount() instead.  LastCount() is still
        needed for use with less commonly used functions: Discard(),
        Peek(), and Unread().
    */
    wxUint32 LastCount() const;

    /**
        Returns the number of bytes read by the last Read() or ReadMsg()
        call (receive direction only).

        This function is thread-safe, in case Read() is executed in a
        different thread than Write().  Use LastReadCount() instead of
        LastCount() for this reason.

        Unlike LastCount(), the functions Discard(), Peek(), and Unread()
        are currently not supported by LastReadCount().

        @since 2.9.5
    */
    wxUint32 LastReadCount() const;

    /**
        Returns the number of bytes written by the last Write() or WriteMsg()
        call (transmit direction only).

        This function is thread-safe, in case Write() is executed in a
        different thread than Read().  Use LastWriteCount() instead of
        LastCount() for this reason.

        @since 2.9.5
    */
    wxUint32 LastWriteCount() const;

    /**
        Returns the last wxSocket error. See @ref wxSocketError .

        @note
        This function merely returns the last error code,
        but it should not be used to determine if an error has occurred (this
        is because successful operations do not change the LastError value).
        Use Error() first, in order to determine if the last IO call failed.
        If this returns @true, use LastError() to discover the cause of the error.
    */
    wxSocketError LastError() const;

    /**
        Restore the previous state of the socket, as saved with SaveState().

        Calls to SaveState() and RestoreState() can be nested.

        @see SaveState()
    */
    void RestoreState();

    /**
        Save the current state of the socket in a stack.

        Socket state includes flags, as set with SetFlags(), event mask, as set
        with SetNotify() and Notify(), user data, as set with SetClientData().
        Calls to SaveState and RestoreState can be nested.

        @see RestoreState()
    */
    void SaveState();

    //@}


    /**
        @name Basic I/O

        See also: wxSocketServer::WaitForAccept(), wxSocketClient::WaitOnConnect()
    */
    //@{

    /**
        Shut down the socket, disabling further transmission and reception of
        data and disable events for the socket and frees the associated system
        resources.

        Upon socket destruction, Close() is automatically called, so in most cases
        you won't need to do it yourself, unless you explicitly want to shut down
        the socket, typically to notify the peer that you are closing the connection.

        @remarks
        Although Close() immediately disables events for the socket, it is possible
        that event messages may be waiting in the application's event queue.
        The application must therefore be prepared to handle socket event messages even
        after calling Close().
    */
    virtual bool Close();

    /**
        Shuts down the writing end of the socket.

        This function simply calls the standard shutdown() function on the
        underlying socket, indicating that nothing will be written to this
        socket any more.
     */
    void ShutdownOutput();

    /**
        Delete all bytes in the incoming queue.

        This function always returns immediately and its operation is not
        affected by IO flags.

        Use LastCount() to verify the number of bytes actually discarded.

        If you use Error(), it will always return @false.
    */
    wxSocketBase& Discard();

    /**
        Returns current IO flags, as set with SetFlags()
    */
    wxSocketFlags GetFlags() const;

    /**
        Use this function to interrupt any wait operation currently in progress.

        Note that this is not intended as a regular way to interrupt a Wait call,
        but only as an escape mechanism for exceptional situations where it is
        absolutely necessary to use it, for example to abort an operation due to
        some exception or abnormal problem. InterruptWait is automatically called
        when you Close() a socket (and thus also upon
        socket destruction), so you don't need to use it in these cases.

        @see  Wait(), WaitForLost(), WaitForRead(), WaitForWrite(),
              wxSocketServer::WaitForAccept(), wxSocketClient::WaitOnConnect()
    */
    void InterruptWait();

    /**
        Peek into the socket by copying the next bytes which would be read by
        Read() into the provided buffer.

        Peeking a buffer doesn't delete it from the socket input queue, i.e.
        calling Read() will return the same data.

        Use LastCount() to verify the number of bytes actually peeked.

        Use Error() to determine if the operation succeeded.

        @param buffer
            Buffer where to put peeked data.
        @param nbytes
            Number of bytes.

        @return Returns a reference to the current object.

        @remarks
            The exact behaviour of Peek() depends on the combination of flags being used.
            For a detailed explanation, see SetFlags()

        @see Error(), LastError(), LastCount(), SetFlags()
    */
    wxSocketBase& Peek(void* buffer, wxUint32 nbytes);

    /**
        Read up to the given number of bytes from the socket.

        Use LastReadCount() to verify the number of bytes actually read.
        Use Error() to determine if the operation succeeded.

        @param buffer
            Buffer where to put read data.
        @param nbytes
            Number of bytes.

        @return Returns a reference to the current object.

        @remarks
            The exact behaviour of Read() depends on the combination of flags being used.
            For a detailed explanation, see SetFlags()

        @see Error(), LastError(), LastReadCount(),
             SetFlags()
    */
    wxSocketBase& Read(void* buffer, wxUint32 nbytes);

    /**
        Receive a message sent by WriteMsg().

        If the buffer passed to the function isn't big enough, the remaining
        bytes will be discarded. This function always waits for the buffer to
        be entirely filled, unless an error occurs.

        Use LastReadCount() to verify the number of bytes actually read.

        Use Error() to determine if the operation succeeded.

        @param buffer
            Buffer where to put read data.
        @param nbytes
            Size of the buffer.

        @return Returns a reference to the current object.

        @remarks
            ReadMsg() will behave as if the @b wxSOCKET_WAITALL flag was always set
            and it will always ignore the @b wxSOCKET_NOWAIT flag.
            The exact behaviour of ReadMsg() depends on the @b wxSOCKET_BLOCK flag.
            For a detailed explanation, see SetFlags().
            For thread safety, in case ReadMsg() and WriteMsg() are called in
            different threads, it is a good idea to call
            SetFlags(wxSOCKET_WAITALL|wx_SOCKET_BLOCK) before the first calls
            to ReadMsg() and WriteMsg() in different threads, as each of these
            functions will call SetFlags() which performs read/modify/write.  By
            setting these flags before the multi-threading, it will ensure that
            they don't get reset by thread race conditions.

        @see Error(), LastError(), LastReadCount(), SetFlags(), WriteMsg()
    */
    wxSocketBase& ReadMsg(void* buffer, wxUint32 nbytes);

    /**
        Use SetFlags to customize IO operation for this socket.

        The @a flags parameter may be a combination of flags ORed together.
        Notice that not all combinations of flags affecting the IO calls
        (Read() and Write()) make sense, e.g. @b wxSOCKET_NOWAIT can't be
        combined with @b wxSOCKET_WAITALL nor with @b wxSOCKET_BLOCK.

        The following flags can be used:
        @beginFlagTable
        @flag{wxSOCKET_NONE}
            Default mode: the socket will read some data in the IO calls and
            will process events to avoid blocking UI while waiting for the data
            to become available.
        @flag{wxSOCKET_NOWAIT}
            Don't wait for the socket to become ready in IO calls, read as much
            data as is available -- potentially 0 bytes -- and return
            immediately.
        @flag{wxSOCKET_WAITALL}
            Don't return before the entire amount of data specified in IO calls
            is read or written unless an error occurs. If this flag is not
            specified, the IO calls return as soon as any amount of data, even
            less than the total number of bytes, is processed.
        @flag{wxSOCKET_BLOCK}
            Don't process the UI events while waiting for the socket to become
            ready. This means that UI will be unresponsive during socket IO.
        @flag{wxSOCKET_REUSEADDR}
            Allows the use of an in-use port (wxServerSocket only).
        @flag{wxSOCKET_BROADCAST}
            Switches the socket to broadcast mode.
        @flag{wxSOCKET_NOBIND}
            Stops the socket from being bound to a specific adapter (normally
            used in conjunction with @b wxSOCKET_BROADCAST).
        @endFlagTable

        For more information on socket events see @ref wxSocketFlags .
    */
    void SetFlags(wxSocketFlags flags);

    /**
        Set the local address and port to use.

        This function must always be called for the server sockets but may also
        be called for client sockets, if it is, @b bind() is called before @b
        connect().
    */
    virtual bool SetLocal(const wxIPV4address& local);

    /**
        Set the default socket timeout in seconds.

        This timeout applies to all IO calls, and also to the Wait() family of
        functions if you don't specify a wait interval. Initially, the default
        timeout is 10 minutes.
    */
    void SetTimeout(long seconds);

    /**
        Put the specified data into the input queue.

        The data in the buffer will be returned by the next call to Read().

        This function is not affected by wxSocket flags.

        If you use LastCount(), it will always return @a nbytes.

        If you use Error(), it will always return @false.

        @param buffer
            Buffer to be unread.
        @param nbytes
            Number of bytes.

        @return Returns a reference to the current object.

        @see Error(), LastCount(), LastError()
    */
    wxSocketBase& Unread(const void* buffer, wxUint32 nbytes);

    /**
        Wait for any socket event.

        Possible socket events are:
        @li The socket becomes readable.
        @li The socket becomes writable.
        @li An ongoing connection request has completed (wxSocketClient only)
        @li An incoming connection request has arrived (wxSocketServer only)
        @li The connection has been closed.

        Note that it is recommended to use the individual @b WaitForXXX()
        functions to wait for the required condition, instead of this one.

        @param seconds
            Number of seconds to wait.
            If -1, it will wait for the default timeout,
            as set with SetTimeout().
        @param millisecond
            Number of milliseconds to wait.

        @return
           @true when any of the above conditions is satisfied or @false if the
           timeout was reached.

        @see InterruptWait(), wxSocketServer::WaitForAccept(),
             WaitForLost(), WaitForRead(),
             WaitForWrite(), wxSocketClient::WaitOnConnect()
    */
    bool Wait(long seconds = -1, long millisecond = 0);

    /**
        Wait until the connection is lost.

        This may happen if the peer gracefully closes the connection or if the
        connection breaks.

        @param seconds
            Number of seconds to wait.
            If -1, it will wait for the default timeout,
            as set with SetTimeout().
        @param millisecond
            Number of milliseconds to wait.

        @return Returns @true if the connection was lost, @false if the timeout
                was reached.

        @see InterruptWait(), Wait()
    */
    bool WaitForLost(long seconds = -1, long millisecond = 0);

    /**
        Wait until the socket is readable.

        This might mean that queued data is available for reading or, for streamed
        sockets, that the connection has been closed, so that a read operation will
        complete immediately without blocking (unless the @b wxSOCKET_WAITALL flag
        is set, in which case the operation might still block).

        Notice that this function should not be called if there is already data
        available for reading on the socket.

        @param seconds
            Number of seconds to wait.
            If -1, it will wait for the default timeout,
            as set with SetTimeout().
        @param millisecond
            Number of milliseconds to wait.

        @return Returns @true if the socket becomes readable, @false on timeout.

        @see InterruptWait(), Wait()
    */
    bool WaitForRead(long seconds = -1, long millisecond = 0);

    /**
        Wait until the socket becomes writable.

        This might mean that the socket is ready to send new data, or for streamed
        sockets, that the connection has been closed, so that a write operation is
        guaranteed to complete immediately (unless the @b wxSOCKET_WAITALL flag is set,
        in which case the operation might still block).

        Notice that this function should not be called if the socket is already
        writable.

        @param seconds
            Number of seconds to wait.
            If -1, it will wait for the default timeout,
            as set with SetTimeout().
        @param millisecond
            Number of milliseconds to wait.

        @return Returns @true if the socket becomes writable, @false on timeout.

        @see InterruptWait(), Wait()
    */
    bool WaitForWrite(long seconds = -1, long millisecond = 0);

    /**
        Write up to the given number of bytes to the socket.

        Use LastWriteCount() to verify the number of bytes actually written.

        Use Error() to determine if the operation succeeded.

        @param buffer
            Buffer with the data to be sent.
        @param nbytes
            Number of bytes.

        @return Returns a reference to the current object.

        @remarks

        The exact behaviour of Write() depends on the combination of flags being used.
        For a detailed explanation, see SetFlags().

        @see Error(), LastError(), LastWriteCount(), SetFlags()
    */
    wxSocketBase& Write(const void* buffer, wxUint32 nbytes);

    /**
        Sends a buffer which can be read using ReadMsg().

        WriteMsg() sends a short header before the data so that ReadMsg()
        knows how much data should be actually read.

        This function always waits for the entire buffer to be sent, unless an
        error occurs.

        Use LastWriteCount() to verify the number of bytes actually written.

        Use Error() to determine if the operation succeeded.

        @param buffer
            Buffer with the data to be sent.
        @param nbytes
            Number of bytes to send.

        @return Returns a reference to the current object.

        @remarks

        WriteMsg() will behave as if the @b wxSOCKET_WAITALL flag was always set and
        it will always ignore the @b wxSOCKET_NOWAIT flag. The exact behaviour of
        WriteMsg() depends on the @b wxSOCKET_BLOCK flag. For a detailed explanation,
        see SetFlags().
        For thread safety, in case ReadMsg() and WriteMsg() are called in
        different threads, it is a good idea to call
        @code SetFlags(wxSOCKET_WAITALL|wx_SOCKET_BLOCK) @endcode before the
        first calls to ReadMsg() and WriteMsg() in different threads, as each
        of these functions calls SetFlags() which performs read/modify/write.
        By setting these flags before the multi-threading, it will ensure that
        they don't get reset by thread race conditions.

        @see  Error(), LastError(), LastWriteCount(), SetFlags(), ReadMsg()

    */
    wxSocketBase& WriteMsg(const void* buffer, wxUint32 nbytes);

    //@}


    /**
        @name Handling Socket Events
    */
    //@{

    /**
        Returns a pointer of the client data for this socket, as set with
        SetClientData()
    */
    void* GetClientData() const;

    /**
        According to the @a notify value, this function enables
        or disables socket events. If @a notify is @true, the events
        configured with SetNotify() will
        be sent to the application. If @a notify is @false; no events
        will be sent.
    */
    void Notify(bool notify);

    /**
        Sets user-supplied client data for this socket. All socket events will
        contain a pointer to this data, which can be retrieved with
        the wxSocketEvent::GetClientData() function.
    */
    void SetClientData(void* data);

    /**
        Sets an event handler to be called when a socket event occurs. The
        handler will be called for those events for which notification is
        enabled with SetNotify() and
        Notify().

        @param handler
            Specifies the event handler you want to use.
        @param id
            The id of socket event.

        @see SetNotify(), Notify(), wxSocketEvent, wxEvtHandler
    */
    void SetEventHandler(wxEvtHandler& handler, int id = -1);

    /**
        Specifies which socket events are to be sent to the event handler.
        The @a flags parameter may be combination of flags ORed together. The
        following flags can be used:

        @beginFlagTable
        @flag{wxSOCKET_INPUT_FLAG} to receive @b wxSOCKET_INPUT.
        @flag{wxSOCKET_OUTPUT_FLAG} to receive @b wxSOCKET_OUTPUT.
        @flag{wxSOCKET_CONNECTION_FLAG} to receive @b wxSOCKET_CONNECTION.
        @flag{wxSOCKET_LOST_FLAG} to receive @b wxSOCKET_LOST.
        @endFlagTable

        For example:

        @code
        sock.SetNotify(wxSOCKET_INPUT_FLAG | wxSOCKET_LOST_FLAG);
        sock.Notify(true);
        @endcode

        In this example, the user will be notified about incoming socket data and
        whenever the connection is closed.

        For more information on socket events see @ref wxSocketEventFlags .
    */
    void SetNotify(wxSocketEventFlags flags);

    /**
        Returns the native socket descriptor.

        This is intended to use with rarely used specific platform features
        that can only be accessed via the actual socket descriptor.

        Do not use this for reading or writing data from or to the socket as
        this would almost surely interfere with wxSocket code logic and result
        in unexpected behaviour.

        The socket must be successfully initialized, e.g. connected for client
        sockets, before this method can be called.

        @return Returns the native socket descriptor.

        @since 2.9.5
    */
    wxSOCKET_T GetSocket() const;

    //@}
};



/**
    @class wxDatagramSocket

    @todo docme

    @library{wxnet}
    @category{net}
*/
class wxDatagramSocket : public wxSocketBase
{
public:
    /**
        Constructor.

        @param addr
            The socket address.
        @param flags
            Socket flags (See wxSocketBase::SetFlags()).
    */
    wxDatagramSocket(const wxSockAddress& addr,
                     wxSocketFlags flags = wxSOCKET_NONE);

    /**
        Destructor. Please see wxSocketBase::Destroy().
    */
    virtual ~wxDatagramSocket();

    /**
        Write a buffer of @a nbytes bytes to the socket.

        Use wxSocketBase::LastWriteCount() to verify the number of bytes actually wrote.
        Use wxSocketBase::Error() to determine if the operation succeeded.

        @param address
            The address of the destination peer for this data.
        @param buffer
            Buffer where read data is.
        @param nbytes
            Number of bytes.

        @return Returns a reference to the current object.

        @see wxSocketBase::LastError(), wxSocketBase::SetFlags()
    */
    wxDatagramSocket& SendTo(const wxSockAddress& address,
                             const void* buffer, wxUint32 nbytes);
};

