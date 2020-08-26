/////////////////////////////////////////////////////////////////////////////
// Name:        srchctrl.h
// Purpose:     interface of wxSearchCtrl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxSearchCtrl

    A search control is a composite control with a search button, a text
    control, and a cancel button.

    This control is implemented natively under macOS and GTK 3.6 or later and
    generically for all the other platforms.

    @beginStyleTable
    @style{wxTE_PROCESS_TAB}
           The control will receive @c wxEVT_CHAR events for TAB pressed -
           normally, TAB is used for passing to the next control in a dialog
           instead. For the control created with this style, you can still use
           Ctrl-Enter to pass to the next control from the keyboard.
    @style{wxTE_NOHIDESEL}
           By default, the Windows text control doesn't show the selection
           when it doesn't have focus - use this style to force it to always
           show it. It doesn't do anything under other platforms.
    @style{wxTE_LEFT}
           The text in the control will be left-justified (default).
    @style{wxTE_CENTRE}
           The text in the control will be centered (currently wxMSW and
           wxGTK2 only).
    @style{wxTE_RIGHT}
           The text in the control will be right-justified (currently wxMSW
           and wxGTK2 only).
    @style{wxTE_CAPITALIZE}
           On PocketPC and Smartphone, causes the first letter to be
           capitalized.
    @endStyleTable

    @beginEventEmissionTable{wxCommandEvent}
    To react to the changes in the control contents, use wxEVT_TEXT event, just
    as you would do with wxTextCtrl. However it is recommended to use
    wxEVT_SEARCH to actually start searching to avoid doing it too soon, while
    the user is still typing (note that wxEVT_SEARCH is also triggered by
    pressing Enter in the control).
    @event{EVT_SEARCH(id, func)}
        Respond to a @c wxEVT_SEARCH event, generated when the
        search button is clicked. Note that this does not initiate a search on
        its own, you need to perform the appropriate action in your event
        handler. You may use @code event.GetString() @endcode to retrieve the
        string to search for in the event handler code.
    @event{EVT_SEARCH_CANCEL(id, func)}
        Respond to a @c wxEVT_SEARCH_CANCEL event, generated when the
        cancel button is clicked.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{searchctrl}

    @see wxTextCtrl
*/
class wxSearchCtrl : public wxTextCtrl
{
public:
    /**
      Default constructor
    */
    wxSearchCtrl();

    /**
        Constructor, creating and showing a text control.

        @param parent
            Parent window. Should not be @NULL.
        @param id
            Control identifier. A value of -1 denotes a default value.
        @param value
            Default text value.
        @param pos
            Text control position.
        @param size
            Text control size.
        @param style
            Window style. See wxSearchCtrl.
        @param validator
            Window validator.
        @param name
            Window name.

        @see wxTextCtrl::Create, wxValidator
    */
    wxSearchCtrl(wxWindow* parent, wxWindowID id,
                 const wxString& value = wxEmptyString,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = 0,
                 const wxValidator& validator = wxDefaultValidator,
                 const wxString& name = wxSearchCtrlNameStr);

    /**
        Destructor, destroying the search control.
    */
    virtual ~wxSearchCtrl();


    bool Create(wxWindow* parent, wxWindowID id,
                 const wxString& value = wxEmptyString,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = 0,
                 const wxValidator& validator = wxDefaultValidator,
                 const wxString& name = wxSearchCtrlNameStr);

    /**
        Returns a pointer to the search control's menu object or @NULL if there is no
        menu attached.
    */
    virtual wxMenu* GetMenu();

    /**
        Returns the search button visibility value.
        If there is a menu attached, the search button will be visible regardless of
        the search button visibility value.
    */
    virtual bool IsSearchButtonVisible() const;

    /**
       Returns the cancel button's visibility state.
    */
    virtual bool IsCancelButtonVisible() const;

    /**
        Sets the search control's menu object.
        If there is already a menu associated with the search control it is deleted.

        @param menu
            Menu to attach to the search control.
    */
    virtual void SetMenu(wxMenu* menu);

    /**
        Shows or hides the cancel button.

        Note that this function does nothing in the native GTK version of the
        control: "Cancel" button is always shown automatically if the control
        is not empty and hidden if it is empty.
    */
    virtual void ShowCancelButton(bool show);

    /**
        Sets the search button visibility value on the search control.

        If there is a menu attached, the search button will be visible regardless of
        the search button visibility value.

        Note that this function does nothing in the native GTK version of the
        control: "Search" button is always shown there.
    */
    virtual void ShowSearchButton(bool show);

    /**
       Set the text to be displayed in the search control when the user has
       not yet typed anything in it.
    */
    void        SetDescriptiveText(const wxString& text);

    /**
       Return the text displayed when there is not yet any user input.
    */
    wxString    GetDescriptiveText() const;
};


wxEventType  wxEVT_SEARCH_CANCEL;
wxEventType  wxEVT_SEARCH;
