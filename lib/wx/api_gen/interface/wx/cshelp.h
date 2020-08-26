/////////////////////////////////////////////////////////////////////////////
// Name:        cshelp.h
// Purpose:     interface of wxHelpProvider
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxHelpProvider

    wxHelpProvider is an abstract class used by a program implementing
    context-sensitive help to show the help text for the given window.

    The current help provider must be explicitly set by the application using
    Set().

    @library{wxcore}
    @category{help}

    @see wxContextHelp, wxContextHelpButton, wxSimpleHelpProvider,
         wxHelpControllerHelpProvider, wxWindow::SetHelpText(),
         wxWindow::GetHelpTextAtPoint()
*/
class wxHelpProvider
{
public:
    /**
        Virtual destructor for any base class.
    */
    virtual ~wxHelpProvider();

    /**
        Associates the text with the given window.

        @remarks
        Although all help providers have these functions to allow making
        wxWindow::SetHelpText() work, not all of them implement the functions.
    */
    virtual void AddHelp(wxWindow* window, const wxString& text);

    /**
        Associates the text with the given ID.

        This help text will be shown for all windows with ID @a id, unless they
        have more specific help text associated using the other AddHelp()
        prototype.  May be used to set the same help string for all Cancel
        buttons in the application, for example.

        @remarks
        Although all help providers have these functions to allow making
        wxWindow::SetHelpText() work, not all of them implement the functions.
    */
    virtual void AddHelp(wxWindowID id, const wxString& text);

    /**
        Returns pointer to help provider instance.

        Unlike some other classes, the help provider is not created on demand.
        This must be explicitly done by the application using Set().
    */
    static wxHelpProvider* Get();

    /**
        This version associates the given text with all windows with this id.
        May be used to set the same help string for all Cancel buttons in
        the application, for example.
    */
    virtual wxString GetHelp(const wxWindow* window) = 0;

    /**
        Removes the association between the window pointer and the help text.
        This is called by the wxWindow destructor. Without this, the table of
        help strings will fill up and when window pointers are reused, the
        wrong help string will be found.
    */
    virtual void RemoveHelp(wxWindow* window);

    /**
        Set the current, application-wide help provider.

        @return Pointer to previous help provider or @NULL if there wasn't any.
    */
    static wxHelpProvider* Set(wxHelpProvider* helpProvider);

    /**
        Shows help for the given window.

        Override this function if the help doesn't depend on the exact position
        inside the window, otherwise you need to override ShowHelpAtPoint().
        Returns @true if help was shown, or @false if no help was available for
        this window.
    */
    virtual bool ShowHelp(wxWindow* window);

    /**
        This function may be overridden to show help for the window when it
        should depend on the position inside the window, By default this method
        forwards to ShowHelp(), so it is enough to only implement the latter if
        the help doesn't depend on the position.

        @param window
            Window to show help text for.
        @param point
            Coordinates of the mouse at the moment of help event emission.
        @param origin
            Help event origin, see wxHelpEvent::GetOrigin.

        @return @true if help was shown, or @false if no help was available
                for this window.

        @since 2.7.0
    */
    virtual bool ShowHelpAtPoint(wxWindow* window, const wxPoint& point,
                                 wxHelpEvent::Origin origin);
};



/**
    @class wxHelpControllerHelpProvider

    wxHelpControllerHelpProvider is an implementation of wxHelpProvider which
    supports both context identifiers and plain text help strings. If the help
    text is an integer, it is passed to wxHelpController::DisplayContextPopup().
    Otherwise, it shows the string in a tooltip as per wxSimpleHelpProvider. If
    you use this with a wxCHMHelpController instance on windows, it will use
    the native style of tip window instead of wxTipWindow.

    You can use the convenience function wxContextId() to convert an integer
    context id to a string for passing to wxWindow::SetHelpText().

    @library{wxcore}
    @category{help}

    @see wxHelpProvider, wxSimpleHelpProvider, wxContextHelp,
         wxWindow::SetHelpText(), wxWindow::GetHelpTextAtPoint()
*/
class wxHelpControllerHelpProvider : public wxSimpleHelpProvider
{
public:
    /**
        Note that the instance doesn't own the help controller. The help
        controller should be deleted separately.
    */
    wxHelpControllerHelpProvider(wxHelpControllerBase* hc = NULL);

    /**
        Returns the help controller associated with this help provider.
    */
    wxHelpControllerBase* GetHelpController() const;

    /**
        Sets the help controller associated with this help provider.
    */
    void SetHelpController(wxHelpControllerBase* hc);
};



/**
    @class wxContextHelp

    This class changes the cursor to a query and puts the application into a
    'context-sensitive help mode'.  When the user left-clicks on a window
    within the specified window, a @c wxEVT_HELP event is sent to that control,
    and the application may respond to it by popping up some help.

    For example:
    @code
    wxContextHelp contextHelp(myWindow);
    @endcode

    There are a couple of ways to invoke this behaviour implicitly:

    - Use the wxDIALOG_EX_CONTEXTHELP style for a dialog (Windows only). This
      will put a question mark in the titlebar, and Windows will put the
      application into context-sensitive help mode automatically, with further
      programming.

    - Create a wxContextHelpButton, whose predefined behaviour is
      to create a context help object.  Normally you will write your application
      so that this button is only added to a dialog for non-Windows platforms
      (use wxDIALOG_EX_CONTEXTHELP on Windows).

    Note that on macOS, the cursor does not change when in context-sensitive
    help mode.

    @library{wxcore}
    @category{help}

    @see wxHelpEvent, wxHelpController, wxContextHelpButton
*/
class wxContextHelp : public wxObject
{
public:
    /**
        Constructs a context help object, calling BeginContextHelp() if
        @a doNow is @true (the default).

        If @a window is @NULL, the top window is used.
    */
    wxContextHelp(wxWindow* window = NULL, bool doNow = true);

    /**
        Destroys the context help object.
    */
    virtual ~wxContextHelp();

    /**
        Puts the application into context-sensitive help mode. @a window is the
        window which will be used to catch events; if @NULL, the top window
        will be used.

        Returns @true if the application was successfully put into
        context-sensitive help mode.
        This function only returns when the event loop has finished.
    */
    bool BeginContextHelp(wxWindow* window);

    /**
        Ends context-sensitive help mode. Not normally called by the
        application.
    */
    bool EndContextHelp();
};



/**
    @class wxContextHelpButton

    Instances of this class may be used to add a question mark button that when
    pressed, puts the application into context-help mode. It does this by
    creating a wxContextHelp object which itself generates a @c wxEVT_HELP event
    when the user clicks on a window.

    On Windows, you may add a question-mark icon to a dialog by use of the
    wxDIALOG_EX_CONTEXTHELP extra style, but on other platforms you will have
    to add a button explicitly, usually next to OK, Cancel or similar buttons.

    @library{wxcore}
    @category{help}

    @see wxBitmapButton, wxContextHelp
*/
class wxContextHelpButton : public wxBitmapButton
{
public:
    /**
        Constructor, creating and showing a context help button.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Button identifier. Defaults to wxID_CONTEXT_HELP.
        @param pos
            Button position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Button size.
            If ::wxDefaultSize is specified then the button is sized appropriately
            for the question mark bitmap.
        @param style
            Window style.

        @remarks
        Normally you only need pass the parent window to the constructor, and
        use the defaults for the remaining parameters.
    */
    wxContextHelpButton(wxWindow* parent,
                        wxWindowID id = wxID_CONTEXT_HELP,
                        const wxPoint& pos = wxDefaultPosition,
                        const wxSize& size = wxDefaultSize,
                        long style = 0);
};


/**
    @class wxSimpleHelpProvider

    wxSimpleHelpProvider is an implementation of wxHelpProvider which supports
    only plain text help strings, and shows the string associated with the
    control (if any) in a tooltip.

    @library{wxcore}
    @category{help}

    @see wxHelpProvider, wxHelpControllerHelpProvider, wxContextHelp,
         wxWindow::SetHelpText()(, wxWindow::GetHelpTextAtPoint()
*/
class wxSimpleHelpProvider : public wxHelpProvider
{
public:

};

