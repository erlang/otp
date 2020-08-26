/////////////////////////////////////////////////////////////////////////////
// Name:        thread.h
// Purpose:     interface of all thread-related wxWidgets classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/** See wxCondition. */
enum wxCondError
{
    wxCOND_NO_ERROR = 0,
    wxCOND_INVALID,
    wxCOND_TIMEOUT,         //!< WaitTimeout() has timed out
    wxCOND_MISC_ERROR
};


/**
    @class wxCondition

    wxCondition variables correspond to pthread conditions or to Win32 event objects.
    They may be used in a multithreaded application to wait until the given condition
    becomes @true which happens when the condition becomes signaled.

    @note In C++11 programs, prefer using @c std::condition to this class.

    For example, if a worker thread is doing some long task and another thread has
    to wait until it is finished, the latter thread will wait on the condition
    object and the worker thread will signal it on exit (this example is not
    perfect because in this particular case it would be much better to just
    wxThread::Wait for the worker thread, but if there are several worker threads
    it already makes much more sense).

    Note that a call to wxCondition::Signal may happen before the other thread calls
    wxCondition::Wait and, just as with the pthread conditions, the signal is then
    lost and so if you want to be sure that you don't miss it you must keep the
    mutex associated with the condition initially locked and lock it again before calling
    wxCondition::Signal. Of course, this means that this call is going to block
    until wxCondition::Wait is called by another thread.

    @section condition_example Example

    This example shows how a main thread may launch a worker thread which starts
    running and then waits until the main thread signals it to continue:

    @code
    class MySignallingThread : public wxThread
    {
    public:
        MySignallingThread(wxMutex *mutex, wxCondition *condition)
        {
            m_mutex = mutex;
            m_condition = condition;
        }

        virtual ExitCode Entry()
        {
            ... do our job ...

            // tell the other(s) thread(s) that we're about to terminate: we must
            // lock the mutex first or we might signal the condition before the
            // waiting threads start waiting on it!
            wxMutexLocker lock(*m_mutex);
            m_condition->Broadcast(); // same as Signal() here -- one waiter only

            return 0;
        }

    private:
        wxCondition *m_condition;
        wxMutex *m_mutex;
    };

    int main()
    {
        wxMutex mutex;
        wxCondition condition(mutex);

        // the mutex should be initially locked
        mutex.Lock();

        // create and run the thread but notice that it won't be able to
        // exit (and signal its exit) before we unlock the mutex below
        MySignallingThread *thread = new MySignallingThread(&mutex, &condition);

        thread->Run();

        // wait for the thread termination: Wait() atomically unlocks the mutex
        // which allows the thread to continue and starts waiting
        condition.Wait();

        // now we can exit
        return 0;
    }
    @endcode

    Of course, here it would be much better to simply use a joinable thread and
    call wxThread::Wait on it, but this example does illustrate the importance of
    properly locking the mutex when using wxCondition.

    @library{wxbase}
    @category{threading}

    @see wxThread, wxMutex
*/
class wxCondition
{
public:
    /**
        Default and only constructor.
        The @a mutex must be locked by the caller before calling Wait() function.
        Use IsOk() to check if the object was successfully initialized.
    */
    wxCondition(wxMutex& mutex);

    /**
        Destroys the wxCondition object.

        The destructor is not virtual so this class should not be used polymorphically.
    */
    ~wxCondition();

    /**
        Broadcasts to all waiting threads, waking all of them up.

        Note that this method may be called whether the mutex associated with
        this condition is locked or not.

        @see Signal()
    */
    wxCondError Broadcast();

    /**
        Returns @true if the object had been initialized successfully, @false
        if an error occurred.
    */
    bool IsOk() const;

    /**
        Signals the object waking up at most one thread.

        If several threads are waiting on the same condition, the exact thread
        which is woken up is undefined. If no threads are waiting, the signal is
        lost and the condition would have to be signalled again to wake up any
        thread which may start waiting on it later.

        Note that this method may be called whether the mutex associated with this
        condition is locked or not.

        @see Broadcast()
    */
    wxCondError Signal();

    /**
        Waits until the condition is signalled.

        This method atomically releases the lock on the mutex associated with this
        condition (this is why it must be locked prior to calling Wait()) and puts the
        thread to sleep until Signal() or  Broadcast() is called.
        It then locks the mutex again and returns.

        Note that even if Signal() had been called before Wait() without waking
        up any thread, the thread would still wait for another one and so it is
        important to ensure that the condition will be signalled after
        Wait() or the thread may sleep forever.

        @return Returns wxCOND_NO_ERROR on success, another value if an error occurred.

        @see WaitTimeout()
    */
    wxCondError Wait();

    /**
        Waits until the condition is signalled and the associated condition true.

        This is a convenience overload that may be used to ignore spurious
        awakenings while waiting for a specific condition to become true.

        Equivalent to
        @code
        while ( !predicate() )
        {
            wxCondError e = Wait();
            if ( e != wxCOND_NO_ERROR )
                return e;
        }
        return wxCOND_NO_ERROR;
        @endcode

        The predicate would typically be a C++11 lambda:
        @code
        condvar.Wait([]{return value == 1;});
        @endcode

        @since 3.0
    */
    template<typename Functor>
    wxCondError Wait(const Functor& predicate);

    /**
        Waits until the condition is signalled or the timeout has elapsed.

        This method is identical to Wait() except that it returns, with the
        return code of @c wxCOND_TIMEOUT as soon as the given timeout expires.

        @param milliseconds
            Timeout in milliseconds

        @return Returns wxCOND_NO_ERROR if the condition was signalled,
                wxCOND_TIMEOUT if the timeout elapsed before this happened or
                another error code from wxCondError enum.
    */
    wxCondError WaitTimeout(unsigned long milliseconds);
};


/**
    @class wxCriticalSectionLocker

    This is a small helper class to be used with wxCriticalSection objects.

    A wxCriticalSectionLocker enters the critical section in the constructor and
    leaves it in the destructor making it much more difficult to forget to leave
    a critical section (which, in general, will lead to serious and difficult
    to debug problems).

    Example of using it:

    @code
    void Set Foo()
    {
        // gs_critSect is some (global) critical section guarding access to the
        // object "foo"
        wxCriticalSectionLocker locker(gs_critSect);

        if ( ... )
        {
            // do something
            ...

            return;
        }

        // do something else
        ...

        return;
    }
    @endcode

    Without wxCriticalSectionLocker, you would need to remember to manually leave
    the critical section before each @c return.

    @library{wxbase}
    @category{threading}

    @see wxCriticalSection, wxMutexLocker
*/
class wxCriticalSectionLocker
{
public:
    /**
        Constructs a wxCriticalSectionLocker object associated with given
        @a criticalsection and enters it.
    */
    wxCriticalSectionLocker(wxCriticalSection& criticalsection);

    /**
        Destructor leaves the critical section.
    */
    ~wxCriticalSectionLocker();
};



/**
    @class wxThreadHelper

    The wxThreadHelper class is a mix-in class that manages a single background
    thread, either detached or joinable (see wxThread for the differences).
    By deriving from wxThreadHelper, a class can implement the thread
    code in its own wxThreadHelper::Entry() method and easily share data and
    synchronization objects between the main thread and the worker thread.

    Doing this prevents the awkward passing of pointers that is needed when the
    original object in the main thread needs to synchronize with its worker thread
    in its own wxThread derived object.

    For example, wxFrame may need to make some calculations in a background thread
    and then display the results of those calculations in the main window.

    Ordinarily, a wxThread derived object would be created with the calculation
    code implemented in wxThread::Entry. To access the inputs to the calculation,
    the frame object would often need to pass a pointer to itself to the thread object.
    Similarly, the frame object would hold a pointer to the thread object.

    Shared data and synchronization objects could be stored in either object
    though the object without the data would have to access the data through
    a pointer.
    However with wxThreadHelper the frame object and the thread object are
    treated as the same object. Shared data and synchronization variables are
    stored in the single object, eliminating a layer of indirection and the
    associated pointers.

    Example:
    @code
        class MyFrame : public wxFrame, public wxThreadHelper
        {
        public:
            MyFrame(...)
            {
                // It is also possible to use event tables, but dynamic binding is simpler.
                Bind(wxEVT_THREAD, &MyFrame::OnThreadUpdate, this);
            }

            ~MyFrame()
            {
                // it's better to do any thread cleanup in the OnClose()
                // event handler, rather than in the destructor.
                // This is because the event loop for a top-level window is not
                // active anymore when its destructor is called and if the thread
                // sends events when ending, they won't be processed unless
                // you ended the thread from OnClose.
                // See @ref overview_windowdeletion for more info.
            }

            ...
            void DoStartALongTask();
            void OnThreadUpdate(wxThreadEvent& evt);
            void OnClose(wxCloseEvent& evt);
            ...

        protected:
            virtual wxThread::ExitCode Entry();

            // the output data of the Entry() routine:
            char m_data[1024];
            wxCriticalSection m_dataCS; // protects field above

            wxDECLARE_EVENT_TABLE();
        };

        wxBEGIN_EVENT_TABLE(MyFrame, wxFrame)
            EVT_CLOSE(MyFrame::OnClose)
        wxEND_EVENT_TABLE()

        void MyFrame::DoStartALongTask()
        {
            // we want to start a long task, but we don't want our GUI to block
            // while it's executed, so we use a thread to do it.
            if (CreateThread(wxTHREAD_JOINABLE) != wxTHREAD_NO_ERROR)
            {
                wxLogError("Could not create the worker thread!");
                return;
            }

            // go!
            if (GetThread()->Run() != wxTHREAD_NO_ERROR)
            {
                wxLogError("Could not run the worker thread!");
                return;
            }
        }

        wxThread::ExitCode MyFrame::Entry()
        {
            // VERY IMPORTANT: this function gets executed in the secondary thread context!
            // Do not call any GUI function inside this function; rather use wxQueueEvent():

            int offset = 0;

            // here we do our long task, periodically calling TestDestroy():
            while (!GetThread()->TestDestroy())
            {
                // since this Entry() is implemented in MyFrame context we don't
                // need any pointer to access the m_data, m_processedData, m_dataCS
                // variables... very nice!

                // this is an example of the generic structure of a download thread:
                char buffer[1024];
                download_chunk(buffer, 1024);     // this takes time...

                {
                    // ensure no one reads m_data while we write it
                    wxCriticalSectionLocker lock(m_dataCS);
                    memcpy(m_data+offset, buffer, 1024);
                    offset += 1024;
                }

                // signal to main thread that download is complete
                wxQueueEvent(GetEventHandler(), new wxThreadEvent());
            }

            // TestDestroy() returned true (which means the main thread asked us
            // to terminate as soon as possible) or we ended the long task...
            return (wxThread::ExitCode)0;
        }

        void MyFrame::OnClose(wxCloseEvent&)
        {
            // important: before terminating, we _must_ wait for our joinable
            // thread to end, if it's running; in fact it uses variables of this
            // instance and posts events to *this event handler

            if (GetThread() &&      // DoStartALongTask() may have not been called
                GetThread()->IsRunning())
                GetThread()->Wait();

            Destroy();
        }

        void MyFrame::OnThreadUpdate(wxThreadEvent& evt)
        {
            // ...do something... e.g. m_pGauge->Pulse();

            // read some parts of m_data just for fun:
            wxCriticalSectionLocker lock(m_dataCS);
            wxPrintf("%c", m_data[100]);
        }
    @endcode

    @library{wxbase}
    @category{threading}

    @see wxThread, wxThreadEvent
*/
class wxThreadHelper
{
public:
    /**
        This constructor simply initializes internal member variables and tells
        wxThreadHelper which type the thread internally managed should be.
    */
    wxThreadHelper(wxThreadKind kind = wxTHREAD_JOINABLE);

    /**
        The destructor frees the resources associated with the thread, forcing
        it to terminate (it uses wxThread::Kill function).

        Because of the wxThread::Kill unsafety, you should always wait
        (with wxThread::Wait) for joinable threads to end or call wxThread::Delete
        on detached threads, instead of relying on this destructor for stopping
        the thread.
    */
    virtual ~wxThreadHelper();

    /**
        This is the entry point of the thread.

        This function is pure virtual and must be implemented by any derived class.
        The thread execution will start here.

        You'll typically want your Entry() to look like:
        @code
            wxThread::ExitCode Entry()
            {
                while (!GetThread()->TestDestroy())
                {
                    // ... do some work ...

                    if (IsWorkCompleted)
                        break;

                    if (HappenedStoppingError)
                        return (wxThread::ExitCode)1;   // failure
                }

                return (wxThread::ExitCode)0;           // success
            }
        @endcode

        The returned value is the thread exit code which is only useful for
        joinable threads and is the value returned by @c "GetThread()->Wait()".

        This function is called by wxWidgets itself and should never be called
        directly.
    */
    virtual ExitCode Entry() = 0;

    /**
        Callback called by Delete() before actually deleting the thread.

        This function can be overridden by the derived class to perform some
        specific task when the thread is gracefully destroyed. Notice that it
        will be executed in the context of the thread that called Delete() and
        <b>not</b> in this thread's context.

        TestDestroy() will be true for the thread before OnDelete() gets
        executed.

        @since 2.9.2

        @see OnKill(), OnExit()
    */
    virtual void OnDelete();

    /**
        Callback called by wxThread::Kill() before actually killing the thread.

        This function can be overridden by the derived class to perform some
        specific task when the thread is terminated. Notice that it will be
        executed in the context of the thread that called wxThread::Kill() and <b>not</b>
        in this thread's context.

        @since 2.9.2

        @see OnDelete(), OnExit()
    */
    virtual void OnKill();

    /**
        Callback called by wxThread::Exit() before actually exiting the thread.
        This function will not be called if the thread was killed with wxThread::Kill.

        This function can be overridden by the derived class to perform some
        specific task when the thread is exited. The base class version does
        nothing and doesn't need to be called if this method is overridden.

        Note that this function is protected since wxWidgets 3.1.1,
        but previously existed as a private method since 2.9.2.

        @see OnDelete(), OnKill()
    */
    virtual void OnExit();

    /**
        @deprecated
        Use CreateThread() instead.
    */
    wxThreadError Create(unsigned int stackSize = 0);

    /**
        Creates a new thread of the given @a kind.

        The thread object is created in the suspended state, and you
        should call @ref wxThread::Run "GetThread()->Run()" to start running it.

        You may optionally specify the stack size to be allocated to it (ignored
        on platforms that don't support setting it explicitly, e.g. Unix).

        @return One of the ::wxThreadError enum values.
    */
    wxThreadError CreateThread(wxThreadKind kind = wxTHREAD_JOINABLE,
                               unsigned int stackSize = 0);

    /**
        This is a public function that returns the wxThread object associated with
        the thread.
    */
    wxThread* GetThread() const;

    /**
        Returns the last type of thread given to the CreateThread() function
        or to the constructor.
    */
    wxThreadKind GetThreadKind() const;
};

/**
   Possible critical section types
*/

enum wxCriticalSectionType
{
    wxCRITSEC_DEFAULT,
      /** Recursive critical section under both Windows and Unix */

    wxCRITSEC_NON_RECURSIVE
      /** Non-recursive critical section under Unix, recursive under Windows */
};

/**
    @class wxCriticalSection

    A critical section object is used for exactly the same purpose as a wxMutex.
    The only difference is that under Windows platform critical sections are only
    visible inside one process, while mutexes may be shared among processes,
    so using critical sections is slightly more efficient.

    The terminology is also slightly different: mutex may be locked (or acquired)
    and unlocked (or released) while critical section is entered and left by the program.

    Finally, you should try to use wxCriticalSectionLocker class whenever
    possible instead of directly using wxCriticalSection for the same reasons
    wxMutexLocker is preferable to wxMutex - please see wxMutex for an example.

    @library{wxbase}
    @category{threading}

    @note Critical sections can be used before the wxWidgets library is fully
          initialized. In particular, it's safe to create global
          wxCriticalSection instances.

    @see wxThread, wxCondition, wxCriticalSectionLocker
*/
class wxCriticalSection
{
public:
    /**
        Default constructor initializes critical section object.
        By default critical sections are recursive under Unix and Windows.
    */
    wxCriticalSection( wxCriticalSectionType critSecType = wxCRITSEC_DEFAULT );

    /**
        Destructor frees the resources.
    */
    ~wxCriticalSection();

    /**
        Enter the critical section (same as locking a mutex): if another thread
        has already entered it, this call will block until the other thread
        calls Leave().
        There is no error return for this function.

        After entering the critical section protecting a data variable,
        the thread running inside the critical section may safely use/modify it.

        Note that entering the same critical section twice or more from the same
        thread doesn't result in a deadlock; in this case in fact this function will
        immediately return.
    */
    void Enter();

    /**
        Try to enter the critical section (same as trying to lock a mutex).
        If it can't, immediately returns false.

        @since 2.9.3
    */
    bool TryEnter();

    /**
        Leave the critical section allowing other threads use the global data
        protected by it. There is no error return for this function.
    */
    void Leave();
};

/**
    The possible thread wait types.

    @since 2.9.2
*/
enum wxThreadWait
{
    /**
        No events are processed while waiting.

        This is the default under all platforms except for wxMSW.
     */
    wxTHREAD_WAIT_BLOCK,

    /**
        Yield for event dispatching while waiting.

        This flag is dangerous as it exposes the program using it to unexpected
        reentrancies in the same way as calling wxYield() function does so you
        are strongly advised to avoid its use and not wait for the thread
        termination from the main (GUI) thread at all to avoid making your
        application unresponsive.

        Also notice that this flag is not portable as it is only implemented in
        wxMSW and simply ignored under the other platforms.
     */
    wxTHREAD_WAIT_YIELD,

    /**
        Default wait mode for wxThread::Wait() and wxThread::Delete().

        For compatibility reasons, the default wait mode is currently
        wxTHREAD_WAIT_YIELD if WXWIN_COMPATIBILITY_2_8 is defined (and it is
        by default). However, as mentioned above, you're strongly encouraged to
        not use wxTHREAD_WAIT_YIELD and pass wxTHREAD_WAIT_BLOCK to wxThread
        method explicitly.
     */
    wxTHREAD_WAIT_DEFAULT = wxTHREAD_WAIT_YIELD
};

/**
  The possible thread kinds.
*/
enum wxThreadKind
{
    /** Detached thread */
    wxTHREAD_DETACHED,

    /** Joinable thread */
    wxTHREAD_JOINABLE
};

/**
  The possible thread errors.
*/
enum wxThreadError
{
    /** No error */
    wxTHREAD_NO_ERROR = 0,

    /** No resource left to create a new thread. */
    wxTHREAD_NO_RESOURCE,

    /** The thread is already running. */
    wxTHREAD_RUNNING,

    /** The thread isn't running. */
    wxTHREAD_NOT_RUNNING,

    /** Thread we waited for had to be killed. */
    wxTHREAD_KILLED,

    /** Some other error */
    wxTHREAD_MISC_ERROR
};

/**
    @class wxThread

    A thread is basically a path of execution through a program.
    Threads are sometimes called @e light-weight processes, but the fundamental difference
    between threads and processes is that memory spaces of different processes are
    separated while all threads share the same address space.

    @note In C++11 programs, consider using @c std::thread instead of this class.

    While it makes it much easier to share common data between several threads, it
    also makes it much easier to shoot oneself in the foot, so careful use of
    synchronization objects such as mutexes (see wxMutex) or critical sections
    (see wxCriticalSection) is recommended.
    In addition, don't create global thread objects because they allocate memory
    in their constructor, which will cause problems for the memory checking system.


    @section thread_types Types of wxThreads

    There are two types of threads in wxWidgets: @e detached and @e joinable,
    modeled after the POSIX thread API. This is different from the Win32 API
    where all threads are joinable.

    By default wxThreads in wxWidgets use the @b detached behaviour.
    Detached threads delete themselves once they have completed, either by themselves
    when they complete processing or through a call to Delete(), and thus
    @b must be created on the heap (through the new operator, for example).

    Typically you'll want to store the instances of the detached wxThreads you
    allocate, so that you can call functions on them.
    Because of their nature however you'll need to always use a critical section
    when accessing them:

    @code
    // declare a new type of event, to be used by our MyThread class:
    wxDECLARE_EVENT(wxEVT_COMMAND_MYTHREAD_COMPLETED, wxThreadEvent);
    wxDECLARE_EVENT(wxEVT_COMMAND_MYTHREAD_UPDATE, wxThreadEvent);
    class MyFrame;

    class MyThread : public wxThread
    {
    public:
        MyThread(MyFrame *handler)
            : wxThread(wxTHREAD_DETACHED)
            { m_pHandler = handler }
        ~MyThread();

    protected:
        virtual ExitCode Entry();
        MyFrame *m_pHandler;
    };

    class MyFrame : public wxFrame
    {
    public:
        ...
        ~MyFrame()
        {
            // it's better to do any thread cleanup in the OnClose()
            // event handler, rather than in the destructor.
            // This is because the event loop for a top-level window is not
            // active anymore when its destructor is called and if the thread
            // sends events when ending, they won't be processed unless
            // you ended the thread from OnClose.
            // See @ref overview_windowdeletion for more info.
        }
        ...
        void DoStartThread();
        void DoPauseThread();

        // a resume routine would be nearly identic to DoPauseThread()
        void DoResumeThread() { ... }

        void OnThreadUpdate(wxThreadEvent&);
        void OnThreadCompletion(wxThreadEvent&);
        void OnClose(wxCloseEvent&);

    protected:
        MyThread *m_pThread;
        wxCriticalSection m_pThreadCS;    // protects the m_pThread pointer

        friend class MyThread;            // allow it to access our m_pThread

        wxDECLARE_EVENT_TABLE();
    };

    wxBEGIN_EVENT_TABLE(MyFrame, wxFrame)
        EVT_CLOSE(MyFrame::OnClose)
        EVT_MENU(Minimal_Start,  MyFrame::DoStartThread)
        EVT_COMMAND(wxID_ANY, wxEVT_COMMAND_MYTHREAD_UPDATE, MyFrame::OnThreadUpdate)
        EVT_COMMAND(wxID_ANY, wxEVT_COMMAND_MYTHREAD_COMPLETED, MyFrame::OnThreadCompletion)
    wxEND_EVENT_TABLE()

    wxDEFINE_EVENT(wxEVT_COMMAND_MYTHREAD_COMPLETED, wxThreadEvent);
    wxDEFINE_EVENT(wxEVT_COMMAND_MYTHREAD_UPDATE, wxThreadEvent);

    void MyFrame::DoStartThread()
    {
        m_pThread = new MyThread(this);

        if ( m_pThread->Run() != wxTHREAD_NO_ERROR )
        {
            wxLogError("Can't create the thread!");
            delete m_pThread;
            m_pThread = NULL;
        }

        // after the call to wxThread::Run(), the m_pThread pointer is "unsafe":
        // at any moment the thread may cease to exist (because it completes its work).
        // To avoid dangling pointers OnThreadExit() will set m_pThread
        // to NULL when the thread dies.
    }

    wxThread::ExitCode MyThread::Entry()
    {
        while (!TestDestroy())
        {
            // ... do a bit of work...

            wxQueueEvent(m_pHandler, new wxThreadEvent(wxEVT_COMMAND_MYTHREAD_UPDATE));
        }

        // signal the event handler that this thread is going to be destroyed
        // NOTE: here we assume that using the m_pHandler pointer is safe,
        //       (in this case this is assured by the MyFrame destructor)
        wxQueueEvent(m_pHandler, new wxThreadEvent(wxEVT_COMMAND_MYTHREAD_COMPLETED));

        return (wxThread::ExitCode)0;     // success
    }

    MyThread::~MyThread()
    {
        wxCriticalSectionLocker enter(m_pHandler->m_pThreadCS);

        // the thread is being destroyed; make sure not to leave dangling pointers around
        m_pHandler->m_pThread = NULL;
    }

    void MyFrame::OnThreadCompletion(wxThreadEvent&)
    {
        wxMessageOutputDebug().Printf("MYFRAME: MyThread exited!\n");
    }

    void MyFrame::OnThreadUpdate(wxThreadEvent&)
    {
        wxMessageOutputDebug().Printf("MYFRAME: MyThread update...\n");
    }

    void MyFrame::DoPauseThread()
    {
        // anytime we access the m_pThread pointer we must ensure that it won't
        // be modified in the meanwhile; since only a single thread may be
        // inside a given critical section at a given time, the following code
        // is safe:
        wxCriticalSectionLocker enter(m_pThreadCS);

        if (m_pThread)         // does the thread still exist?
        {
            // without a critical section, once reached this point it may happen
            // that the OS scheduler gives control to the MyThread::Entry() function,
            // which in turn may return (because it completes its work) making
            // invalid the m_pThread pointer

            if (m_pThread->Pause() != wxTHREAD_NO_ERROR )
                wxLogError("Can't pause the thread!");
        }
    }

    void MyFrame::OnClose(wxCloseEvent&)
    {
        {
            wxCriticalSectionLocker enter(m_pThreadCS);

            if (m_pThread)         // does the thread still exist?
            {
                wxMessageOutputDebug().Printf("MYFRAME: deleting thread");

                if (m_pThread->Delete() != wxTHREAD_NO_ERROR )
                    wxLogError("Can't delete the thread!");
            }
        }       // exit from the critical section to give the thread
                // the possibility to enter its destructor
                // (which is guarded with m_pThreadCS critical section!)

        while (1)
        {
            { // was the ~MyThread() function executed?
                wxCriticalSectionLocker enter(m_pThreadCS);
                if (!m_pThread) break;
            }

            // wait for thread completion
            wxThread::This()->Sleep(1);
        }

        Destroy();
    }
    @endcode

    For a more detailed and comprehensive example, see @sample{thread}.
    For a simpler way to share data and synchronization objects between
    the main and the secondary thread see wxThreadHelper.

    Conversely, @b joinable threads do not delete themselves when they are done
    processing and as such are safe to create on the stack. Joinable threads
    also provide the ability for one to get value it returned from Entry()
    through Wait().
    You shouldn't hurry to create all the threads joinable, however, because this
    has a disadvantage as well: you @b must Wait() for a joinable thread or the
    system resources used by it will never be freed, and you also must delete the
    corresponding wxThread object yourself if you did not create it on the stack.
    In contrast, detached threads are of the "fire-and-forget" kind: you only have
    to start a detached thread and it will terminate and destroy itself.


    @section thread_deletion wxThread Deletion

    Regardless of whether it has terminated or not, you should call Wait() on a
    @b joinable thread to release its memory, as outlined in @ref thread_types.
    If you created a joinable thread on the heap, remember to delete it manually
    with the @c delete operator or similar means as only detached threads handle
    this type of memory management.

    Since @b detached threads delete themselves when they are finished processing,
    you should take care when calling a routine on one. If you are certain the
    thread is still running and would like to end it, you may call Delete()
    to gracefully end it (which implies that the thread will be deleted after
    that call to Delete()). It should be implied that you should @b never attempt
    to delete a detached thread with the @c delete operator or similar means.

    As mentioned, Wait() or Delete() functions attempt to gracefully terminate a
    joinable and a detached thread, respectively. They do this by waiting until
    the thread in question calls TestDestroy() or ends processing (i.e. returns
    from wxThread::Entry).

    Obviously, if the thread does call TestDestroy() and does not end, the
    thread which called Wait() or Delete() will come to halt.
    This is why it's important to call TestDestroy() in the Entry() routine of
    your threads as often as possible and immediately exit when it returns @true.

    As a last resort you can end the thread immediately through Kill(). It is
    strongly recommended that you do not do this, however, as it does not free
    the resources associated with the object (although the wxThread object of
    detached threads will still be deleted) and could leave the C runtime
    library in an undefined state.


    @section thread_secondary wxWidgets Calls in Secondary Threads

    All threads other than the "main application thread" (the one running
    wxApp::OnInit() or the one your main function runs in, for example) are
    considered "secondary threads".

    GUI calls, such as those to a wxWindow or wxBitmap are explicitly not safe
    at all in secondary threads and could end your application prematurely.
    This is due to several reasons, including the underlying native API and
    the fact that wxThread does not run a GUI event loop similar to other APIs
    as MFC.

    A workaround for some wxWidgets ports is calling wxMutexGUIEnter()
    before any GUI calls and then calling wxMutexGUILeave() afterwords.
    However, the recommended way is to simply process the GUI calls in the main
    thread through an event that is posted by wxQueueEvent().
    This does not imply that calls to these classes are thread-safe, however,
    as most wxWidgets classes are not thread-safe, including wxString.


    @section thread_poll Don't Poll a wxThread

    A common problem users experience with wxThread is that in their main thread
    they will check the thread every now and then to see if it has ended through
    IsRunning(), only to find that their application has run into problems
    because the thread is using the default behaviour (i.e. it's @b detached) and
    has already deleted itself.
    Naturally, they instead attempt to use joinable threads in place of the previous
    behaviour. However, polling a wxThread for when it has ended is in general a
    bad idea - in fact calling a routine on any running wxThread should be avoided
    if possible. Instead, find a way to notify yourself when the thread has ended.

    Usually you only need to notify the main thread, in which case you can
    post an event to it via wxQueueEvent().
    In the case of secondary threads you can call a routine of another class
    when the thread is about to complete processing and/or set the value of
    a variable, possibly using mutexes (see wxMutex) and/or other synchronization
    means if necessary.

    @library{wxbase}
    @category{threading}

    @see wxThreadHelper, wxMutex, wxCondition, wxCriticalSection,
         @ref overview_thread
*/
class wxThread
{
public:
    /**
        The return type for the thread functions.
    */
    typedef void* ExitCode;

    /**
        This constructor creates a new detached (default) or joinable C++
        thread object. It does not create or start execution of the real thread -
        for this you should use the Run() method.

        The possible values for @a kind parameters are:
          - @b wxTHREAD_DETACHED - Creates a detached thread.
          - @b wxTHREAD_JOINABLE - Creates a joinable thread.
    */
    wxThread(wxThreadKind kind = wxTHREAD_DETACHED);

    /**
        The destructor frees the resources associated with the thread.
        Notice that you should never delete a detached thread -- you may only call
        Delete() on it or wait until it terminates (and auto destructs) itself.

        Because the detached threads delete themselves, they can only be allocated on the heap.
        Joinable threads should be deleted explicitly. The Delete() and Kill() functions
        will not delete the C++ thread object. It is also safe to allocate them on stack.
    */
    virtual ~wxThread();

    /**
        Creates a new thread.

        The thread object is created in the suspended state, and you should call Run()
        to start running it. You may optionally specify the stack size to be allocated
        to it (Ignored on platforms that don't support setting it explicitly,
        eg. Unix system without @c pthread_attr_setstacksize).

        If you do not specify the stack size, the system's default value is used.

        @note
            It is not necessary to call this method since 2.9.5, Run() will create
            the thread internally. You only need to call Create() if you need to do
            something with the thread (e.g. pass its ID to an external library)
            before it starts.

        @return One of:
          - @b wxTHREAD_NO_ERROR - No error.
          - @b wxTHREAD_NO_RESOURCE - There were insufficient resources to create the thread.
          - @b wxTHREAD_NO_RUNNING - The thread is already running
    */
    wxThreadError Create(unsigned int stackSize = 0);

    /**
        Calling Delete() requests termination of any thread.

        Note that Delete() doesn't actually stop the thread, but simply asks it
        to terminate and so will work only if the thread calls TestDestroy()
        periodically. For detached threads, Delete() returns immediately,
        without waiting for the thread to actually terminate, while for
        joinable threads it does wait for the thread to terminate and may also
        return its exit code in @a rc argument.

        @param rc
            For joinable threads, filled with the thread exit code on
            successful return, if non-@NULL. For detached threads this
            parameter is not used.

        @param waitMode
            As described in wxThreadWait documentation, wxTHREAD_WAIT_BLOCK
            should be used as the wait mode even although currently
            wxTHREAD_WAIT_YIELD is for compatibility reasons. This parameter is
            new in wxWidgets 2.9.2.

        @note
            This function works on a joinable thread but in that case makes
            the TestDestroy() function of the thread return @true and then
            waits for its completion (i.e. it differs from Wait() because
            it asks the thread to terminate before waiting).

        See @ref thread_deletion for a broader explanation of this routine.
    */
    wxThreadError Delete(ExitCode *rc = NULL,
                         wxThreadWait waitMode = wxTHREAD_WAIT_DEFAULT);

    /**
        Returns the number of system CPUs or -1 if the value is unknown.

        For multi-core systems the returned value is typically the total number
        of @e cores, since the OS usually abstract a single N-core CPU
        as N different cores.

        @see SetConcurrency()
    */
    static int GetCPUCount();

    /**
        Returns the platform specific thread ID of the current thread as a long.

        This can be used to uniquely identify threads, even if they are not wxThreads.

        @see GetMainId()
    */
    static wxThreadIdType GetCurrentId();

    /**
        Gets the thread identifier: this is a platform dependent number that uniquely
        identifies the thread throughout the system during its existence
        (i.e.\ the thread identifiers may be reused).
    */
    wxThreadIdType GetId() const;

    /**
        Gets the native thread handle.

        This method only exists in wxMSW, use GetId() in portable code.

        @since 3.1.0
    */
    WXHANDLE MSWGetHandle() const;

    /**
        Returns the thread kind as it was given in the ctor.

        @since 2.9.0
    */
    wxThreadKind GetKind() const;

    /**
        Returns the thread ID of the main thread.

        @see IsMain()

        @since 2.9.1
     */
    static wxThreadIdType GetMainId();

    /**
        Gets the priority of the thread, between 0 (lowest) and 100 (highest).

        @see SetPriority()
    */
    unsigned int GetPriority() const;

    /**
        Returns @true if the thread is alive (i.e.\ started and not terminating).

        Note that this function can only safely be used with joinable threads, not
        detached ones as the latter delete themselves and so when the real thread is
        no longer alive, it is not possible to call this function because
        the wxThread object no longer exists.
    */
    bool IsAlive() const;

    /**
        Returns @true if the thread is of the detached kind, @false if it is a
        joinable one.
    */
    bool IsDetached() const;

    /**
        Returns @true if the calling thread is the main application thread.

        Main thread in the context of wxWidgets is the one which initialized
        the library.

        @see GetMainId(), GetCurrentId()
    */
    static bool IsMain();

    /**
        Returns @true if the thread is paused.
    */
    bool IsPaused() const;

    /**
        Returns @true if the thread is running.

        This method may only be safely used for joinable threads, see the remark in
        IsAlive().
    */
    bool IsRunning() const;

    /**
        Immediately terminates the target thread.

        @b "This function is dangerous and should be used with extreme care"
        (and not used at all whenever possible)! The resources allocated to the
        thread will not be freed and the state of the C runtime library may become
        inconsistent. Use Delete() for detached threads or Wait() for joinable
        threads instead.

        For detached threads Kill() will also delete the associated C++ object.
        However this will not happen for joinable threads and this means that you will
        still have to delete the wxThread object yourself to avoid memory leaks.

        In neither case OnExit() of the dying thread will be called, so no
        thread-specific cleanup will be performed.
        This function can only be called from another thread context, i.e. a thread
        cannot kill itself.

        It is also an error to call this function for a thread which is not running or
        paused (in the latter case, the thread will be resumed first) -- if you do it,
        a @b wxTHREAD_NOT_RUNNING error will be returned.
    */
    wxThreadError Kill();

    /**
        Suspends the thread.

        Under some implementations (Win32), the thread is suspended immediately,
        under others it will only be suspended when it calls TestDestroy() for
        the next time (hence, if the thread doesn't call it at all, it won't be
        suspended).

        This function can only be called from another thread context.
    */
    wxThreadError Pause();

    /**
        Resumes a thread suspended by the call to Pause().

        This function can only be called from another thread context.
    */
    wxThreadError Resume();

    /**
        Starts the thread execution.

        Note that once you Run() a @b detached thread, @e any function call you do
        on the thread pointer (you must allocate it on the heap) is @e "unsafe";
        i.e. the thread may have terminated at any moment after Run() and your pointer
        may be dangling. See @ref thread_types for an example of safe manipulation
        of detached threads.

        This function can only be called from another thread context.

        Finally, note that once a thread has completed and its Entry() function
        returns, you cannot call Run() on it again (an assert will fail in debug
        builds or @c wxTHREAD_RUNNING will be returned in release builds).
    */
    wxThreadError Run();

    /**
        Sets the thread concurrency level for this process.

        This is, roughly, the number of threads that the system tries to schedule
        to run in parallel.
        The value of 0 for @a level may be used to set the default one.

        @return @true on success or @false otherwise (for example, if this function is
                not implemented for this platform -- currently everything except Solaris).
    */
    static bool SetConcurrency(size_t level);

    /**
        Sets the priority of the thread, between 0 (lowest) and 100 (highest).

        The following symbolic constants can be used in addition to raw
        values in 0..100 range:
          - @c wxPRIORITY_MIN: 0
          - @c wxPRIORITY_DEFAULT: 50
          - @c wxPRIORITY_MAX: 100

        Please note that currently this function is not implemented when using
        the default (@c SCHED_OTHER) scheduling policy under POSIX systems.
    */
    void SetPriority(unsigned int priority);

    /**
        Pauses the thread execution for the given amount of time.

        This is the same as wxMilliSleep().
    */
    static void Sleep(unsigned long milliseconds);

    /**
        This function should be called periodically by the thread to ensure that
        calls to Pause() and Delete() will work.

        If it returns @true, the thread should exit as soon as possible.
        Notice that under some platforms (POSIX), implementation of Pause() also
        relies on this function being called, so not calling it would prevent
        both stopping and suspending thread from working.
    */
    virtual bool TestDestroy();

    /**
        Return the thread object for the calling thread.

        @NULL is returned if the calling thread is the main (GUI) thread, but
        IsMain() should be used to test whether the thread is really the main one
        because @NULL may also be returned for the thread not created with wxThread
        class. Generally speaking, the return value for such a thread is undefined.
    */
    static wxThread* This();

    /**
        Waits for a @b joinable thread to terminate and returns the value the thread
        returned from Entry() or @c "(ExitCode)-1" on error. Notice that, unlike
        Delete(), this function doesn't cancel the thread in any way so the caller
        waits for as long as it takes to the thread to exit.

        You can only Wait() for @b joinable (not detached) threads.

        This function can only be called from another thread context.

        @param flags
            As described in wxThreadWait documentation, wxTHREAD_WAIT_BLOCK
            should be used as the wait mode even although currently
            wxTHREAD_WAIT_YIELD is for compatibility reasons. This parameter is
            new in wxWidgets 2.9.2.

        See @ref thread_deletion for a broader explanation of this routine.
    */
    ExitCode Wait(wxThreadWait flags = wxTHREAD_WAIT_DEFAULT);

    /**
        Give the rest of the thread's time-slice to the system allowing the other
        threads to run.

        Note that using this function is @b strongly discouraged, since in
        many cases it indicates a design weakness of your threading model
        (as does using Sleep() functions).

        Threads should use the CPU in an efficient manner, i.e. they should
        do their current work efficiently, then as soon as the work is done block
        on a wakeup event (wxCondition, wxMutex, select(), poll(), ...) which will
        get signalled e.g. by other threads or a user device once further thread
        work is available.
        Using Yield() or Sleep() indicates polling-type behaviour, since we're
        fuzzily giving up our timeslice and wait until sometime later we'll get
        reactivated, at which time we realize that there isn't really much to do
        and Yield() again...

        The most critical characteristic of Yield() is that it's operating system
        specific: there may be scheduler changes which cause your thread to not
        wake up relatively soon again, but instead many seconds later,
        causing huge performance issues for your application.

        <strong>
        With a well-behaving, CPU-efficient thread the operating system is likely
        to properly care for its reactivation the moment it needs it, whereas with
        non-deterministic, Yield-using threads all bets are off and the system
        scheduler is free to penalize them drastically</strong>, and this effect
        gets worse with increasing system load due to less free CPU resources available.
        You may refer to various Linux kernel @c sched_yield discussions for more
        information.

        See also Sleep().
    */
    static void Yield();

protected:

    /**
        This is the entry point of the thread.

        This function is pure virtual and must be implemented by any derived class.
        The thread execution will start here.

        The returned value is the thread exit code which is only useful for
        joinable threads and is the value returned by Wait().
        This function is called by wxWidgets itself and should never be called
        directly.
    */
    virtual ExitCode Entry() = 0;

    /**
        This is a protected function of the wxThread class and thus can only be called
        from a derived class. It also can only be called in the context of this
        thread, i.e. a thread can only exit from itself, not from another thread.

        This function will terminate the OS thread (i.e. stop the associated path of
        execution) and also delete the associated C++ object for detached threads.
        OnExit() will be called just before exiting.
    */
    void Exit(ExitCode exitcode = 0);
};


/** See wxSemaphore. */
enum wxSemaError
{
    wxSEMA_NO_ERROR = 0,
    wxSEMA_INVALID,         //!< semaphore hasn't been initialized successfully
    wxSEMA_BUSY,            //!< returned by TryWait() if Wait() would block
    wxSEMA_TIMEOUT,         //!< returned by WaitTimeout()
    wxSEMA_OVERFLOW,        //!< Post() would increase counter past the max
    wxSEMA_MISC_ERROR
};

/**
    @class wxSemaphore

    wxSemaphore is a counter limiting the number of threads concurrently accessing
    a shared resource. This counter is always between 0 and the maximum value
    specified during the semaphore creation. When the counter is strictly greater
    than 0, a call to wxSemaphore::Wait() returns immediately and decrements the
    counter. As soon as it reaches 0, any subsequent calls to wxSemaphore::Wait
    block and only return when the semaphore counter becomes strictly positive
    again as the result of calling wxSemaphore::Post which increments the counter.

    In general, semaphores are useful to restrict access to a shared resource
    which can only be accessed by some fixed number of clients at the same time.
    For example, when modeling a hotel reservation system a semaphore with the counter
    equal to the total number of available rooms could be created. Each time a room
    is reserved, the semaphore should be acquired by calling wxSemaphore::Wait
    and each time a room is freed it should be released by calling wxSemaphore::Post.

    @library{wxbase}
    @category{threading}
*/
class wxSemaphore
{
public:
    /**
        Specifying a @a maxcount of 0 actually makes wxSemaphore behave as if
        there is no upper limit. If @a maxcount is 1, the semaphore behaves almost as a
        mutex (but unlike a mutex it can be released by a thread different from the one
        which acquired it).

        @a initialcount is the initial value of the semaphore which must be between
        0 and @a maxcount (if it is not set to 0).
    */
    wxSemaphore(int initialcount = 0, int maxcount = 0);

    /**
        Destructor is not virtual, don't use this class polymorphically.
    */
    ~wxSemaphore();

    /**
        Increments the semaphore count and signals one of the waiting
        threads in an atomic way. Returns @e wxSEMA_OVERFLOW if the count
        would increase the counter past the maximum.

        @return One of:
            - wxSEMA_NO_ERROR: There was no error.
            - wxSEMA_INVALID : Semaphore hasn't been initialized successfully.
            - wxSEMA_OVERFLOW: Post() would increase counter past the max.
            - wxSEMA_MISC_ERROR: Miscellaneous error.
    */
    wxSemaError Post();

    /**
        Same as Wait(), but returns immediately.

        @return One of:
            - wxSEMA_NO_ERROR: There was no error.
            - wxSEMA_INVALID: Semaphore hasn't been initialized successfully.
            - wxSEMA_BUSY: Returned by TryWait() if Wait() would block, i.e. the count is zero.
            - wxSEMA_MISC_ERROR: Miscellaneous error.
    */
    wxSemaError TryWait();

    /**
        Wait indefinitely until the semaphore count becomes strictly positive
        and then decrement it and return.

        @return One of:
            - wxSEMA_NO_ERROR: There was no error.
            - wxSEMA_INVALID: Semaphore hasn't been initialized successfully.
            - wxSEMA_MISC_ERROR: Miscellaneous error.
    */
    wxSemaError Wait();

    /**
        Same as Wait(), but with a timeout limit.

        @return One of:
            - wxSEMA_NO_ERROR: There was no error.
            - wxSEMA_INVALID: Semaphore hasn't been initialized successfully.
            - wxSEMA_TIMEOUT: Timeout occurred without receiving semaphore.
            - wxSEMA_MISC_ERROR: Miscellaneous error.
    */
    wxSemaError WaitTimeout(unsigned long timeout_millis);
};



/**
    @class wxMutexLocker

    This is a small helper class to be used with wxMutex objects.

    A wxMutexLocker acquires a mutex lock in the constructor and releases
    (or unlocks) the mutex in the destructor making it much more difficult to
    forget to release a mutex (which, in general, will promptly lead to serious
    problems). See wxMutex for an example of wxMutexLocker usage.

    @library{wxbase}
    @category{threading}

    @see wxMutex, wxCriticalSectionLocker
*/
class wxMutexLocker
{
public:
    /**
        Constructs a wxMutexLocker object associated with mutex and locks it.
        Call IsOk() to check if the mutex was successfully locked.
    */
    wxMutexLocker(wxMutex& mutex);

    /**
        Destructor releases the mutex if it was successfully acquired in the ctor.
    */
    ~wxMutexLocker();

    /**
        Returns @true if mutex was acquired in the constructor, @false otherwise.
    */
    bool IsOk() const;
};


/**
    The possible wxMutex kinds.
*/
enum wxMutexType
{
    /** Normal non-recursive mutex: try to always use this one. */
    wxMUTEX_DEFAULT,

    /** Recursive mutex: don't use these ones with wxCondition. */
    wxMUTEX_RECURSIVE
};


/**
    The possible wxMutex errors.
*/
enum wxMutexError
{
    /** The operation completed successfully. */
    wxMUTEX_NO_ERROR = 0,

    /** The mutex hasn't been initialized. */
    wxMUTEX_INVALID,

     /** The mutex is already locked by the calling thread. */
    wxMUTEX_DEAD_LOCK,

    /** The mutex is already locked by another thread. */
    wxMUTEX_BUSY,

    /** An attempt to unlock a mutex which is not locked. */
    wxMUTEX_UNLOCKED,

    /** wxMutex::LockTimeout() has timed out. */
    wxMUTEX_TIMEOUT,

    /** Any other error */
    wxMUTEX_MISC_ERROR
};


/**
    @class wxMutex

    A mutex object is a synchronization object whose state is set to signaled when
    it is not owned by any thread, and nonsignaled when it is owned. Its name comes
    from its usefulness in coordinating mutually-exclusive access to a shared
    resource as only one thread at a time can own a mutex object.

    @note In C++11 programs, prefer using @c std::mutex to this class.

    Mutexes may be recursive in the sense that a thread can lock a mutex which it
    had already locked before (instead of dead locking the entire process in this
    situation by starting to wait on a mutex which will never be released while the
    thread is waiting) but using them is not recommended under Unix and they are
    @b not recursive by default. The reason for this is that recursive
    mutexes are not supported by all Unix flavours and, worse, they cannot be used
    with wxCondition.

    For example, when several threads use the data stored in the linked list,
    modifications to the list should only be allowed to one thread at a time
    because during a new node addition the list integrity is temporarily broken
    (this is also called @e program @e invariant).

    @code
    // this variable has an "s_" prefix because it is static: seeing an "s_" in
    // a multithreaded program is in general a good sign that you should use a
    // mutex (or a critical section)
    static wxMutex s_mutexProtectingTheGlobalData;

    // we store some numbers in this global array which is presumably used by
    // several threads simultaneously
    wxArrayInt s_data;

    void MyThread::AddNewNode(int num)
    {
        // ensure that no other thread accesses the list

        // Note that using Lock() and Unlock() explicitly is not recommended
        // and only done here for illustrative purposes, prefer to use
        // wxMutexLocker, as shown below, instead!
        s_mutexProtectingTheGlobalData.Lock();

        s_data.Add(num);

        s_mutexProtectingTheGlobaData.Unlock();
    }

    // return true if the given number is greater than all array elements
    bool MyThread::IsGreater(int num)
    {
        // before using the list we must acquire the mutex
        wxMutexLocker lock(s_mutexProtectingTheGlobalData);

        size_t count = s_data.Count();
        for ( size_t n = 0; n < count; n++ )
        {
            if ( s_data[n] > num )
                return false;
        }

        return true;
    }
    @endcode

    Notice how wxMutexLocker was used in the second function to ensure that the
    mutex is unlocked in any case: whether the function returns true or false
    (because the destructor of the local object @e lock is always called).
    Using this class instead of directly using wxMutex is, in general, safer
    and is even more so if your program uses C++ exceptions.

    @library{wxbase}
    @category{threading}

    @see wxThread, wxCondition, wxMutexLocker, wxCriticalSection
*/
class wxMutex
{
public:
    /**
        Default constructor.
    */
    wxMutex(wxMutexType type = wxMUTEX_DEFAULT);

    /**
        Destroys the wxMutex object.
    */
    ~wxMutex();

    /**
        Locks the mutex object.
        This is equivalent to LockTimeout() with infinite timeout.

        Note that if this mutex is already locked by the caller thread,
        this function doesn't block but rather immediately returns.

        @return One of: @c wxMUTEX_NO_ERROR, @c wxMUTEX_DEAD_LOCK.
    */
    wxMutexError Lock();

    /**
        Try to lock the mutex object during the specified time interval.

        @return One of: @c wxMUTEX_NO_ERROR, @c wxMUTEX_DEAD_LOCK, @c wxMUTEX_TIMEOUT.
    */
    wxMutexError LockTimeout(unsigned long msec);

    /**
        Tries to lock the mutex object. If it can't, returns immediately with an error.

        @return One of: @c wxMUTEX_NO_ERROR, @c wxMUTEX_BUSY.
    */
    wxMutexError TryLock();

    /**
        Unlocks the mutex object.

        @return One of: @c wxMUTEX_NO_ERROR, @c wxMUTEX_UNLOCKED.
    */
    wxMutexError Unlock();
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_thread */
//@{

/**
    This macro declares a (static) critical section object named @a cs if
    @c wxUSE_THREADS is 1 and does nothing if it is 0.

    @header{wx/thread.h}
*/
#define wxCRIT_SECT_DECLARE(cs)

/**
    This macro declares a critical section object named @a cs if
    @c wxUSE_THREADS is 1 and does nothing if it is 0. As it doesn't include
    the @c static keyword (unlike wxCRIT_SECT_DECLARE()), it can be used to
    declare a class or struct member which explains its name.

    @header{wx/thread.h}
*/
#define wxCRIT_SECT_DECLARE_MEMBER(cs)

/**
    This macro creates a wxCriticalSectionLocker named @a name and associated
    with the critical section @a cs if @c wxUSE_THREADS is 1 and does nothing
    if it is 0.

    @header{wx/thread.h}
*/
#define wxCRIT_SECT_LOCKER(name, cs)

/**
    This macro combines wxCRIT_SECT_DECLARE() and wxCRIT_SECT_LOCKER(): it
    creates a static critical section object and also the lock object
    associated with it. Because of this, it can be only used inside a function,
    not at global scope. For example:

    @code
    int IncCount()
    {
        static int s_counter = 0;

        wxCRITICAL_SECTION(counter);

        return ++s_counter;
    }
    @endcode

    Note that this example assumes that the function is called the first time
    from the main thread so that the critical section object is initialized
    correctly by the time other threads start calling it, if this is not the
    case this approach can @b not be used and the critical section must be made
    a global instead.

    @header{wx/thread.h}
*/
#define wxCRITICAL_SECTION(name)

/**
    This macro is equivalent to
    @ref wxCriticalSection::Leave "critical_section.Leave()" if
    @c wxUSE_THREADS is 1 and does nothing if it is 0.

    @header{wx/thread.h}
*/
#define wxLEAVE_CRIT_SECT(critical_section)

/**
    This macro is equivalent to
    @ref wxCriticalSection::Enter "critical_section.Enter()" if
    @c wxUSE_THREADS is 1 and does nothing if it is 0.

    @header{wx/thread.h}
*/
#define wxENTER_CRIT_SECT(critical_section)

/**
    Returns @true if this thread is the main one. Always returns @true if
    @c wxUSE_THREADS is 0.

    @header{wx/thread.h}
*/
bool wxIsMainThread();



/**
    This function must be called when any thread other than the main GUI thread
    wants to get access to the GUI library. This function will block the
    execution of the calling thread until the main thread (or any other thread
    holding the main GUI lock) leaves the GUI library and no other thread will
    enter the GUI library until the calling thread calls wxMutexGuiLeave().

    Typically, these functions are used like this:

    @code
    void MyThread::Foo()
    {
        // before doing any GUI calls we must ensure that
        // this thread is the only one doing it!

        wxMutexGuiEnter();

        // Call GUI here:
        my_window->DrawSomething();

        wxMutexGuiLeave();
    }
    @endcode

    This function is only defined on platforms which support preemptive
    threads and only works under some ports (wxMSW currently).

    @note Under GTK, no creation of top-level windows is allowed in any thread
          but the main one.

    @header{wx/thread.h}
*/
void wxMutexGuiEnter();

/**
    This function is only defined on platforms which support preemptive
    threads.

    @see wxMutexGuiEnter()

    @header{wx/thread.h}
*/
void wxMutexGuiLeave();

//@}

