/////////////////////////////////////////////////////////////////////////////
// Name:        process.h
// Purpose:     interface of wxProcess
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxProcess

    The objects of this class are used in conjunction with the ::wxExecute() function.
    When a wxProcess object is passed to ::wxExecute(), its OnTerminate() virtual method
    is called when the process terminates. This allows the program to be (asynchronously)
    notified about the process termination and also retrieve its exit status which is
    unavailable from ::wxExecute() in the case of asynchronous execution.

    @note
        If the @c wxEVT_END_PROCESS event sent after termination is processed by the
        parent, then it is responsible for deleting the wxProcess object which sent it.
        However, if it is not processed, the object will <b>delete itself</b> and so the
        library users should only delete those objects whose notifications have been
        processed (and call wxProcess::Detach for others).
        This also means that unless you're going to process the @c wxEVT_END_PROCESS event,
        you <b>must</b> allocate the wxProcess class on the heap.

    wxProcess also supports IO redirection of the child process. For this, you have
    to call its Redirect() method before passing it to ::wxExecute().
    If the child process was launched successfully, GetInputStream(), GetOutputStream()
    and GetErrorStream() can then be used to retrieve the streams corresponding to the
    child process standard output, input and error output respectively.

    @beginEventEmissionTable{wxProcessEvent}
    @event{EVT_END_PROCESS(id, func)}
        Process a @c wxEVT_END_PROCESS event, sent by wxProcess::OnTerminate upon
        the external process termination.
    @endEventTable

    @library{wxbase}
    @category{appmanagement}

    @beginWxPerlOnly
    In wxPerl this class has an additional @c Destroy method,
    for explicit destruction.
    @endWxPerlOnly

    @see wxExecute(), @ref page_samples_exec
*/
class wxProcess : public wxEvtHandler
{
public:
    /**
        Constructs a process object. @a id is only used in the case you want to
        use wxWidgets events. It identifies this object, or another window that will
        receive the event.

        If the @a parent parameter is different from @NULL, it will receive
        a @c wxEVT_END_PROCESS notification event (you should insert @c EVT_END_PROCESS
        macro in the event table of the parent to handle it) with the given @a id.

        @param parent
            The event handler parent.
        @param id
            id of an event.
    */
    wxProcess(wxEvtHandler* parent = NULL, int id = -1);

    /**
        Creates an object without any associated parent (and hence no id neither)
        but allows specifying the @a flags which can have the value of
        @c wxPROCESS_DEFAULT or @c wxPROCESS_REDIRECT.

        Specifying the former value has no particular effect while using the latter
        one is equivalent to calling Redirect().
    */
    wxProcess(int flags);

    /**
        Destroys the wxProcess object.
    */
    virtual ~wxProcess();

    /**
        Activates a GUI process by bringing up its main window to the front.

        This is a convenient method which tries to bring this process to the
        users attention.

        Currently this is implemented in wxMSW only and simply returns @false
        under the other platforms. Notice that this function can also return
        @false under MSW if, for example, the process doesn't have any windows.

        @since 3.1.0
     */
    bool Activate() const;

    /**
        Closes the output stream (the one connected to the stdin of the child
        process).

        This function can be used to indicate to the child process that
        there is no more data to be read - usually, a filter program will only
        terminate when the input stream is closed.

        Notice that GetOutputStream() will return @NULL after the output stream
        is closed.
    */
    void CloseOutput();

    /**
        Detaches this event handler from the parent specified in the constructor
        (see wxEvtHandler::Unlink() for a similar but not identical function).

        Normally, a wxProcess object is deleted by its parent when it receives the
        notification about the process termination.

        However, it might happen that the parent object is destroyed before the external
        process is terminated (e.g. a window from which this external process was launched
        is closed by the user) and in this case it @b should not delete the wxProcess
        object, but @b should call Detach() instead.

        After the wxProcess object is detached from its parent, no notification events
        will be sent to the parent and the object will delete itself upon reception of
        the process termination notification.
    */
    void Detach();

    /**
        Returns @true if the given process exists in the system.

        @see Kill(), @ref page_samples_exec "Exec sample"
    */
    static bool Exists(int pid);

    /**
        Returns an input stream which corresponds to the standard error output (stderr)
        of the child process.
    */
    wxInputStream* GetErrorStream() const;

    /**
        It returns an input stream corresponding to the standard output stream of the
        subprocess. If it is @NULL, you have not turned on the redirection.

        @see Redirect().
    */
    wxInputStream* GetInputStream() const;

    /**
        It returns an output stream corresponding to the input stream of the subprocess.

        If it is @NULL, you have not turned on the redirection or already
        called CloseOutput().

        @see Redirect().
    */
    wxOutputStream* GetOutputStream() const;

    /**
        Returns the process ID of the process launched by Open() or set by
        wxExecute() (if you passed this wxProcess as argument).
    */
    long GetPid() const;

    /**
        Returns @true if there is data to be read on the child process standard
        error stream.

        @see IsInputAvailable()
    */
    bool IsErrorAvailable() const;

    /**
        Returns @true if there is data to be read on the child process standard
        output stream.

        This allows writing simple (and extremely inefficient) polling-based code
        waiting for a better mechanism in future wxWidgets versions.
        See the @ref page_samples_exec "exec sample" for an example of using this
        function.

        @see IsInputOpened()
    */
    bool IsInputAvailable() const;

    /**
        Returns @true if the child process standard output stream is opened.
    */
    bool IsInputOpened() const;

    /**
        Send the specified signal to the given process. Possible signal values
        can be one of the ::wxSignal enumeration values.

        @c wxSIGNONE, @c wxSIGKILL and @c wxSIGTERM have the same meaning
        under both Unix and Windows but all the other signals are equivalent to
        @c wxSIGTERM under Windows.

        The @a flags parameter can be @c wxKILL_NOCHILDREN (the default),
        or @c wxKILL_CHILDREN, in which case the child processes of this
        process will be killed too. Note that under Unix, for @c wxKILL_CHILDREN
        to work you should have created the process passing @c wxEXEC_MAKE_GROUP_LEADER.

        Returns the element of ::wxKillError enum.

        @see Exists(), wxKill(), @ref page_samples_exec "Exec sample"
    */
    static wxKillError Kill(int pid, wxSignal sig = wxSIGTERM,
                            int flags = wxKILL_NOCHILDREN);

    /**
        It is called when the process with the pid @a pid finishes.
        It raises a wxWidgets event when it isn't overridden.

        Note that this function won't be called if you Kill() the process.

        @param pid
            The pid of the process which has just terminated.
        @param status
            The exit code of the process.
    */
    virtual void OnTerminate(int pid, int status);

    /**
        This static method replaces the standard @c popen() function: it launches
        the process specified by the @a cmd parameter and returns the wxProcess
        object which can be used to retrieve the streams connected to the standard
        input, output and error output of the child process.

        If the process couldn't be launched, @NULL is returned.

        @remarks
        In any case the returned pointer should @b not be deleted, rather the process
        object will be destroyed automatically when the child process terminates. This
        does mean that the child process should be told to quit before the main program
        exits to avoid memory leaks.

        @param cmd
            The command to execute, including optional arguments.
        @param flags
            The flags to pass to ::wxExecute().
            Note: @c wxEXEC_SYNC should not be used.

        @return A pointer to new wxProcess object or @NULL on error.

        @see ::wxExecute()
    */
    static wxProcess* Open(const wxString& cmd,
                           int flags = wxEXEC_ASYNC);

    /**
        Turns on redirection.

        ::wxExecute() will try to open a couple of pipes to catch the subprocess stdio.
        The caught input stream is returned by GetOutputStream() as a non-seekable stream.
        The caught output stream is returned by GetInputStream() as a non-seekable stream.
    */
    void Redirect();

    /**
        Sets the priority of the process, between 0 (lowest) and 100 (highest).
        It can only be set before the process is created.

        The following symbolic constants can be used in addition to raw
        values in 0..100 range:
          - @b wxPRIORITY_MIN: 0
          - @b wxPRIORITY_DEFAULT: 50
          - @b wxPRIORITY_MAX: 100

        @since 2.9.5
    */
    void SetPriority(unsigned priority);
};



/**
    @class wxProcessEvent

    A process event is sent to the wxEvtHandler specified to wxProcess
    when a process is terminated.

    @beginEventTable{wxProcessEvent}
    @event{EVT_END_PROCESS(id, func)}
        Process a @c wxEVT_END_PROCESS event. @a id is the identifier of the process
        object (the id passed to the wxProcess constructor) or a window to receive
        the event.
    @endEventTable

    @library{wxbase}
    @category{events}

    @see wxProcess, @ref overview_events
*/
class wxProcessEvent : public wxEvent
{
public:
    /**
        Constructor.

        Takes a wxProcessObject or window id, a process id and an exit status.
    */
    wxProcessEvent(int id = 0, int pid = 0, int exitcode = 0);

    /**
        Returns the exist status.
    */
    int GetExitCode();

    /**
        Returns the process id.
    */
    int GetPid();
};


wxEventType wxEVT_END_PROCESS;

