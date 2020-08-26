/////////////////////////////////////////////////////////////////////////////
// Name:        wx/evtloop.h
// Purpose:     wxEventLoop and related classes
// Author:      Vadim Zeitlin
// Copyright:   (C) 2008 Vadim Zeitlin
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxEventLoopBase

    Base class for all event loop implementations.

    An event loop is a class which queries the queue of native events sent
    to the wxWidgets application and dispatches them to the appropriate
    wxEvtHandlers.

    An object of this class is created by wxAppTraits::CreateEventLoop() and
    used by wxApp to run the main application event loop.
    Temporary event loops are usually created by wxDialog::ShowModal().

    You can create your own event loop if you need, provided that you restore
    the main event loop once yours is destroyed (see wxEventLoopActivator).

    Notice that there can be more than one event loop at any given moment, e.g.
    an event handler called from the main loop can show a modal dialog, which
    starts its own loop resulting in two nested loops, with the modal dialog
    being the active one (its IsRunning() returns @true). And a handler for a
    button inside the modal dialog can, of course, create another modal dialog
    with its own event loop and so on. So in general event loops form a stack
    and only the event loop at the top of the stack is considered to be active.
    It is also the only loop that can be directly asked to terminate by calling
    Exit() (which is done by wxDialog::EndModal()), an outer event loop can't
    be stopped while an inner one is still running. It is however possible to
    ask an outer event loop to terminate as soon as all its nested loops exit
    and the control returns back to it by using ScheduleExit().

    @library{wxbase}
    @category{appmanagement}

    @see wxApp, wxEventLoopActivator
*/
class wxEventLoopBase
{
public:
    /**
        Return the currently active (running) event loop.

        May return @NULL if there is no active event loop (e.g. during
        application startup or shutdown).
     */
    static wxEventLoopBase *GetActive();

    /**
        Set currently active (running) event loop.

        Called by wxEventLoopActivator, use an instance of this class instead
        of calling this method directly to ensure that the previously active
        event loop is restored.

        Results in a call to wxAppConsole::OnEventLoopEnter.
     */
    static void SetActive(wxEventLoopBase* loop);

    /**
        Returns @true if this is the main loop executed by wxApp::OnRun().
    */
    bool IsMain() const;


    /**
        @name Dispatch and processing
    */
    //@{

    /**
        Start the event loop, return the exit code when it is finished.

        Logically, this method calls Dispatch() in a loop until it returns
        @false and also takes care of generating idle events during each loop
        iteration. However not all implementations of this class really
        implement it like this (e.g. wxGTK does not) so you shouldn't rely on
        Dispatch() being called from inside this function.

        @return The argument passed to Exit() which terminated this event loop.
     */
    virtual int Run() = 0;

    /**
        Return true if this event loop is currently running.

        Notice that even if this event loop hasn't terminated yet but has just
        spawned a nested (e.g. modal) event loop, this method would return
        @false.
     */
    bool IsRunning() const;

    /**
        Use this to check whether the event loop was successfully created
        before using it
     */
    virtual bool IsOk() const;

    /**
        Exit the currently running loop with the given exit code.

        The loop will exit, i.e. its Run() method will return, during the next
        event loop iteration.

        Notice that this method can only be used if this event loop is the
        currently running one, i.e. its IsRunning() returns @true. If this is
        not the case, an assert failure is triggered and nothing is done as
        outer event loops can't be exited from immediately. Use ScheduleExit()
        if you'd like to exit this loop even if it doesn't run currently.
     */
    virtual void Exit(int rc = 0);

    /**
        Schedule an exit from the loop with the given exit code.

        This method is similar to Exit() but can be called even if this event
        loop is not the currently running one -- and if it is the active loop,
        then it works in exactly the same way as Exit().

        The loop will exit as soon as the control flow returns to it, i.e.
        after any nested loops terminate.

        @since 2.9.5
     */
    virtual void ScheduleExit(int rc = 0) = 0;

    /**
        Return true if any events are available.

        If this method returns @true, calling Dispatch() will not block.
     */
    virtual bool Pending() const = 0;

    /**
        Dispatches the next event in the windowing system event queue.
        Blocks until an event appears if there are none currently
        (use Pending() if this is not wanted).

        This can be used for programming event loops, e.g.

        @code
        while (evtloop->Pending())
            evtloop->Dispatch();
        @endcode

        @return @false if the event loop should stop and @true otherwise.

        @see Pending(), wxEventLoopBase
    */
    virtual bool Dispatch() = 0;

    /**
        Dispatch an event but not wait longer than the specified timeout for
        it.

        If an event is received before the specified @a timeout expires, it is
        processed and the function returns 1 normally or 0 if the event loop
        should quite. Otherwise, i.e. if the timeout expires, the functions
        returns -1 without processing any events.

        @param timeout
            The maximal time to wait for the events in milliseconds.

        @return
            1 if an event was processed, 0 if the event loop should quit or -1
            if the timeout expired.
     */
    virtual int DispatchTimeout(unsigned long timeout) = 0;

    /**
        Called by wxWidgets to wake up the event loop even if it is currently
        blocked inside Dispatch().
     */
    virtual void WakeUp() = 0;

    //@}


    /**
        @name Idle handling
    */
    //@{

    /**
        Makes sure that idle events are sent again.
    */
    void WakeUpIdle();

    /**
        This virtual function is called  when the application becomes idle and
        normally just sends wxIdleEvent to all interested parties.

        It should return @true if more idle events are needed, @false if not.
    */
    virtual bool ProcessIdle();

    //@}


    /**
        @name Yield-related hooks
    */
    //@{

    /**
        Returns @true if called from inside Yield() or from inside YieldFor().
    */
    virtual bool IsYielding() const;

    /**
        Yields control to pending messages in the windowing system.

        This can be useful, for example, when a time-consuming process writes to a
        text window. Without an occasional yield, the text window will not be updated
        properly, and on systems with cooperative multitasking, other processes
        will not respond.

        Caution should be exercised, however, since yielding may allow the
        user to perform actions which are not compatible with the current task.
        Disabling menu items or whole menus during processing can avoid unwanted
        reentrance of code: see ::wxSafeYield for a better function.

        Note that Yield() will not flush the message logs. This is intentional as
        calling Yield() is usually done to quickly update the screen and popping up
        a message box dialog may be undesirable. If you do wish to flush the log
        messages immediately (otherwise it will be done during the next idle loop
        iteration), call wxLog::FlushActive.

        If @a onlyIfNeeded parameter is @true and the flow control is already
        inside Yield(), i.e. IsYielding() returns @true, the method just
        silently returns @false and doesn't do anything.
    */
    bool Yield(bool onlyIfNeeded = false);

    /**
        Works like Yield() with @e onlyIfNeeded == @true, except that it allows
        the caller to specify a mask of the ::wxEventCategory values which
        indicates which events should be processed and which should instead
        be "delayed" (i.e. processed by the main loop later).

        Note that this is a safer alternative to Yield() since it ensures that
        only the events you're interested to will be processed; i.e. this method
        helps to avoid unwanted reentrancies.

        Note that currently only wxMSW and wxGTK do support selective yield of
        native events coming from the underlying GUI toolkit.
        wxWidgets events posted using wxEvtHandler::AddPendingEvent or
        wxEvtHandler::QueueEvent are instead selectively processed by all ports.

        @see wxEvent::GetEventCategory
    */
    bool YieldFor(long eventsToProcess);

    /**
        Returns @true if the given event category is allowed inside
        a YieldFor() call (i.e. compares the given category against the
        last mask passed to YieldFor()).

        @see wxEvent::GetEventCategory
    */
    virtual bool IsEventAllowedInsideYield(wxEventCategory cat) const;

    //@}


protected:
    /**
        This function is called before the event loop terminates, whether this
        happens normally (because of Exit() call) or abnormally (because of an
        exception thrown from inside the loop).

        The default implementation calls wxAppConsole::OnEventLoopExit.
     */
    virtual void OnExit();
};

/**
    @class wxEventLoopActivator

    Makes an event loop temporarily active.

    This class is used to make the event loop active during its life-time,
    e.g.:
    @code
        class MyEventLoop : public wxEventLoopBase { ... };

        void RunMyLoop()
        {
            MyEventLoop loop;
            wxEventLoopActivator activate(&loop);

            ...
        } // the previously active event loop restored here
    @endcode

    @library{wxbase}
    @category{appmanagement}

    @see wxEventLoopBase
*/
class wxEventLoopActivator
{
public:
    /**
        Makes the loop passed as the parameter currently active.

        This saves the current return value of wxEventLoopBase::GetActive() and
        then calls wxEventLoopBase::SetActive() with the given @a loop.
     */
    wxEventLoopActivator(wxEventLoopBase *loop);

    /**
        Restores the previously active event loop stored by the constructor.
     */
    ~wxEventLoopActivator();
};

/**
    @class wxGUIEventLoop

    A generic implementation of the GUI event loop.

    @library{wxbase}
    @category{appmanagement}
*/
class wxGUIEventLoop : public wxEventLoopBase
{
public:
    wxGUIEventLoop();
    virtual ~wxGUIEventLoop();
};


