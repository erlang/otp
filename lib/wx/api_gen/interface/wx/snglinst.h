/////////////////////////////////////////////////////////////////////////////
// Name:        snglinst.h
// Purpose:     interface of wxSingleInstanceChecker
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxSingleInstanceChecker

    wxSingleInstanceChecker class allows checking that only a single instance of a
    program is running.

    To do it, you should create an object of this class. As long as this object
    is alive, calls to wxSingleInstanceChecker::IsAnotherRunning from other
    processes will return @true.

    As the object should have the life span as big as possible, it makes sense to
    create it either as a global or in wxApp::OnInit.
    For example:

    @code
    bool MyApp::OnInit()
    {
        m_checker = new wxSingleInstanceChecker;
        if ( m_checker->IsAnotherRunning() )
        {
            wxLogError(_("Another program instance is already running, aborting."));

            delete m_checker; // OnExit() won't be called if we return false
            m_checker = NULL;

            return false;
        }

        ... more initializations ...

        return true;
    }

    int MyApp::OnExit()
    {
        delete m_checker;

        return 0;
    }
    @endcode

    Note that by default wxSingleInstanceChecker::CreateDefault() is used to
    create the checker meaning that it will be initialized differently for
    different users and thus will allow different users to run the application
    concurrently which is usually the required behaviour. However if only a
    single instance of a program should be running system-wide, you need to
    call Create() with a custom name which does @em not include wxGetUserId().

    This class is implemented for Win32 and Unix platforms (supporting @c fcntl()
    system call, but almost all of modern Unix systems do) only.

    @library{wxbase}
    @category{appmanagement}
*/
class wxSingleInstanceChecker
{
public:
    /**
        Default constructor.

        You may call Create() after using it or directly call
        IsAnotherRunning() in which case CreateDefault() will be implicitly
        used.
    */
    wxSingleInstanceChecker();

    /**
        Constructor calling Create().

        This constructor does exactly the same thing as Create() but doesn't
        allow to check for errors.
    */
    wxSingleInstanceChecker(const wxString& name,
                            const wxString& path = wxEmptyString);

    /**
        Destructor frees the associated resources.

        Note that it is not virtual, this class is not meant to be used polymorphically.
    */
    ~wxSingleInstanceChecker();

    /**
        Initialize the object if it had been created using the default constructor.

        Note that you can't call Create() more than once, so calling it if the
        non default ctor had been used is an error.

        @param name
            Must be given and be as unique as possible. It is used as the
            mutex name under Win32 and the lock file name under Unix.
            wxApp::GetAppName() and wxGetUserId() are commonly used to construct
            this parameter.
        @param path
            The path is optional and is ignored under Win32 and used as the
            directory to create the lock file in under Unix
            (default is wxGetHomeDir()).

        @return
            Returns @false if initialization failed, it doesn't mean that
            another instance is running -- use IsAnotherRunning() to check for
            it.

        @note
            One of possible reasons while Create() may fail on Unix is that the lock
            file used for checking already exists but was not created by the user.
            Therefore applications shouldn't treat failure of this function as fatal
            condition, because doing so would open them to the possibility of a
            Denial of Service attack. Instead, they should alert the user about
            the problem and offer to continue execution without checking if
            another instance is running.
    */
    bool Create(const wxString& name,
                const wxString& path = wxEmptyString);

    /**
        Calls Create() with default name.

        This method simply calls Create() with a string composed of
        wxApp::GetAppName() and wxGetUserId().

        Because this method uses wxApp::GetAppName(), it may only be called
        after the global application was constructed.

        @since 2.9.1
     */
    bool CreateDefault();

    /**
        Returns @true if another copy of this program is already running and
        @false otherwise.

        Notice that if the object was created using the default constructor
        Create() hadn't been called before this method, it will call
        CreateDefault() automatically.
    */
    bool IsAnotherRunning() const;
};

