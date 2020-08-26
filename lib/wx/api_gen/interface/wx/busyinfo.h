/////////////////////////////////////////////////////////////////////////////
// Name:        busyinfo.h
// Purpose:     interface of wxBusyInfo
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxBusyInfo

    This class makes it easy to tell your user that the program is temporarily busy.

    Normally the main thread should always return to the main loop to continue
    dispatching events as quickly as possible, hence this class shouldn't be
    needed. However if the main thread does need to block, this class provides
    a simple way to at least show this to the user: just create a wxBusyInfo
    object on the stack, and within the current scope, a message window will be
    shown.

    For example:

    @code
        wxBusyInfo wait("Please wait, working...");

        for (int i = 0; i < 100000; i++)
        {
            DoACalculation();
        }
    @endcode

    It works by creating a window in the constructor, and deleting it
    in the destructor.

    This window is rather plain by default but can be customized by passing
    wxBusyInfo constructor an object of wxBusyInfoFlags class instead of a
    simple message. Here is an example from the dialogs sample:

    @code
        wxBusyInfo info
            (
                wxBusyInfoFlags()
                    .Parent(this)
                    .Icon(wxArtProvider::GetIcon(wxART_PRINT,
                                                 wxART_OTHER, wxSize(128, 128)))
                    .Title("<b>Printing your document</b>")
                    .Text("Please wait...")
                    .Foreground(*wxWHITE)
                    .Background(*wxBLACK)
                    .Transparency(4*wxALPHA_OPAQUE/5)
            );
    @endcode

    This shows that separate title and text can be set, and that simple markup
    (@ref wxControl::SetLabelMarkup()) can be used in them, and that it's also
    possible to add an icon and customize the colours and transparency of the
    window.

    You may also want to call wxTheApp->Yield() to refresh the window
    periodically (in case it had been obscured by other windows, for
    example) like this:

    @code
        wxWindowDisabler disableAll;
        wxBusyInfo wait("Please wait, working...");

        for (int i = 0; i < 100000; i++)
        {
            DoACalculation();

            if ( !(i % 1000) )
                wxTheApp->Yield();
        }
    @endcode

    but take care to not cause undesirable reentrancies when doing it (see
    wxApp::Yield for more details). The simplest way to do it is to use
    wxWindowDisabler class as illustrated in the above example.

    Note that a wxBusyInfo is always built with the @c wxSTAY_ON_TOP window style
    (see wxFrame window styles for more info).

    @library{wxcore}
    @category{cmndlg}
*/
class wxBusyInfo
{
public:
    /**
        General constructor.

        This constructor allows specifying all supported attributes by calling
        the appropriate methods on wxBusyInfoFlags object passed to it as
        parameter. All of them are optional but usually at least the message
        should be specified.

        @since 3.1.0
     */
    wxBusyInfo(const wxBusyInfoFlags& flags);

    /**
        Simple constructor specifying only the message and the parent.

        This constructs a busy info window as child of @a parent and displays
        @a msg in it. It is exactly equivalent to using
        @code
            wxBusyInfo(wxBusyInfoFlags().Parent(parent).Label(message))
        @endcode

        @note If @a parent is not @NULL you must ensure that it is not
              closed while the busy info is shown.
    */
    wxBusyInfo(const wxString& msg, wxWindow* parent = NULL);

    /**
        Update the information text.

        The @a text string may contain markup as described in
        wxControl::SetLabelMarkup().

        @since 3.1.3
    */
    void UpdateText(const wxString& str);

    /**
        Same as UpdateText() but doesn't interpret the string as containing markup.

        @since 3.1.3
    */
    void UpdateLabel(const wxString& str);

    /**
        Hides and closes the window containing the information text.
    */
    virtual ~wxBusyInfo();
};

/**
    Parameters for wxBusyInfo.

    This class exists only in order to make passing attributes to wxBusyInfo
    constructor easier and the code doing it more readable.

    All methods of this class return the reference to the object on which they
    are called, making it possible to chain them together, e.g. typically you
    would just create a temporary wxBusyInfoFlags object and then call the
    methods corresponding to the attributes you want to set, before finally
    passing the result to wxBusyInfo constructor, e.g.:
    @code
        wxBusyInfo info
            (
                wxBusyInfoFlags()
                    .Parent(window)
                    .Icon(icon)
                    .Title("Some text")
                    .Text("Some more text")
                    .Foreground(wxColour(...))
                    .Background(wxColour(...))
            );
    @endcode

    @since 3.1.0
 */
class wxBusyInfoFlags
{
public:
    /**
        Default constructor initializes all attributes to default values.

        Call the other methods to really fill in the object.
     */
    wxBusyInfoFlags();

    /// Sets the parent for wxBusyInfo.
    wxBusyInfoFlags& Parent(wxWindow* parent);

    /// Sets the icon to show in wxBusyInfo.
    wxBusyInfoFlags& Icon(const wxIcon& icon);

    /**
        Sets the title, shown prominently in wxBusyInfo window.

        The @a title string may contain markup as described in
        wxControl::SetLabelMarkup().
     */
    wxBusyInfoFlags& Title(const wxString& title);

    /**
        Sets the more detailed text, shown under the title, if any.

        The @a text string may contain markup as described in
        wxControl::SetLabelMarkup().
     */
    wxBusyInfoFlags& Text(const wxString& text);

    /**
        Same as Text() but doesn't interpret the string as containing markup.

        This method should be used if the text shown in wxBusyInfo comes from
        external source and so may contain characters having special meaning in
        simple markup, e.g. '<'.
     */
    wxBusyInfoFlags& Label(const wxString& label);

    /// Sets the foreground colour of the title and text strings.
    wxBusyInfoFlags& Foreground(const wxColour& foreground);

    /// Sets the background colour of wxBusyInfo window.
    wxBusyInfoFlags& Background(const wxColour& background);

    /**
        Sets the transparency of wxBusyInfo window.

        @param alpha Value in wxALPHA_TRANSPARENT (0) to wxALPHA_OPAQUE (255)
            range.

        @see wxTopLevelWindow::SetTransparent()
     */
    wxBusyInfoFlags& Transparency(wxByte alpha);
};
