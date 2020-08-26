/////////////////////////////////////////////////////////////////////////////
// Name:        panel.h
// Purpose:     interface of wxPanel
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxPanel

    A panel is a window on which controls are placed. It is usually placed within
    a frame. Its main feature over its parent class wxWindow is code for handling
    child windows and TAB traversal, which is implemented natively if possible
    (e.g. in wxGTK) or by wxWidgets itself otherwise.

    @note Tab traversal is implemented through an otherwise undocumented
          intermediate wxControlContainer class from which any class can derive
          in addition to the normal wxWindow base class. Please see @c wx/containr.h
          and @c wx/panel.h to find out how this is achieved.

    @note if not all characters are being intercepted by your OnKeyDown or
          OnChar handler, it may be because you are using the @c wxTAB_TRAVERSAL style,
          which grabs some keypresses for use by child controls.

    @remarks By default, a panel has the same colouring as a dialog.

    @beginEventEmissionTable{wxNavigationKeyEvent}
    @event{EVT_NAVIGATION_KEY(func)}
        Process a navigation key event.
    @endEventTable

    @library{wxcore}
    @category{miscwnd}

    @see wxDialog
*/
class wxPanel : public wxWindow
{
public:

    /**
        Default constructor.
    */
    wxPanel();

    /**
        Constructor.

        @param parent
            The parent window.
        @param id
            An identifier for the panel. @c wxID_ANY is taken to mean a default.
        @param pos
            The panel position. The value ::wxDefaultPosition indicates a default position,
            chosen by either the windowing system or wxWidgets, depending on platform.
        @param size
            The panel size. The value ::wxDefaultSize indicates a default size, chosen by
            either the windowing system or wxWidgets, depending on platform.
        @param style
            The window style. See wxPanel.
        @param name
            Window name.

        @see Create()
    */
    wxPanel(wxWindow* parent, wxWindowID id = wxID_ANY,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize,
            long style = wxTAB_TRAVERSAL,
            const wxString& name = wxPanelNameStr);

    /**
        Destructor. Deletes any child windows before deleting the physical window.
    */
    virtual ~wxPanel();

    /**
        This method is overridden from wxWindow::AcceptsFocus()
        and returns @true only if there is no child window in the panel which
        can accept the focus. This is reevaluated each time a child
        window is added or removed from the panel.
    */
    bool AcceptsFocus() const;

    /**
        Used for two-step panel construction. See wxPanel() for details.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxTAB_TRAVERSAL,
                const wxString& name = wxPanelNameStr);

    /**
        Sends a wxInitDialogEvent, which in turn transfers data to the dialog via
        validators.

        @see wxInitDialogEvent
    */
    virtual void InitDialog();

    /**
        See wxWindow::SetAutoLayout(): when auto layout is on, this function gets
        called automatically when the window is resized.
    */
    virtual bool Layout();

    /**
        The default handler for @c wxEVT_SYS_COLOUR_CHANGED.

        @param event
            The colour change event.

        @remarks Changes the panel's colour to conform to the current settings
                 (Windows only). Add an event table entry for your panel
                 class if you wish the behaviour to be different (such
                 as keeping a user-defined background colour). If you do
                 override this function, call wxEvent::Skip() to propagate
                 the notification to child windows and controls.

        @see wxSysColourChangedEvent
    */
    void OnSysColourChanged(wxSysColourChangedEvent& event);

    /**
        Overrides wxWindow::SetFocus().

        This method uses the (undocumented) mix-in class wxControlContainer which manages
        the focus and TAB logic for controls which usually have child controls.

        In practice, if you call this method and the control has at least
        one child window, the focus will be given to the child window.

        @see wxFocusEvent, wxWindow::SetFocus()
    */
    virtual void SetFocus();

    /**
        In contrast to SetFocus() (see above) this will set the focus to the panel
        even if there are child windows in the panel. This is only rarely needed.
    */
    void SetFocusIgnoringChildren();
};

