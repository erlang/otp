/////////////////////////////////////////////////////////////////////////////
// Name:        tglbtn.h
// Purpose:     interface of wxBitmapToggleButton, wxToggleButton
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


wxEventType wxEVT_TOGGLEBUTTON;

/**
    @class wxToggleButton

    wxToggleButton is a button that stays pressed when clicked by the user.
    In other words, it is similar to wxCheckBox in functionality but looks like a wxButton.

    Since wxWidgets version 2.9.0 this control emits an update UI event.

    You can see wxToggleButton in action in @ref page_samples_widgets.

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_TOGGLEBUTTON(id, func)}
        Handles a wxEVT_TOGGLEBUTTON event.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{togglebutton}

    @see wxCheckBox, wxButton, wxBitmapToggleButton
*/
class wxToggleButton : public wxAnyButton
{
public:
    /**
      Default constructor.
    */
    wxToggleButton();

    /**
        Constructor, creating and showing a toggle button.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Toggle button identifier. The value wxID_ANY indicates a default value.
        @param label
            Text to be displayed next to the toggle button.
        @param pos
            Toggle button position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Toggle button size.
            If ::wxDefaultSize is specified then a default size is chosen.
        @param style
            Window style. See wxToggleButton.
        @param val
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxToggleButton(wxWindow* parent, wxWindowID id,
                   const wxString& label,
                   const wxPoint& pos = wxDefaultPosition,
                   const wxSize& size = wxDefaultSize,
                   long style = 0,
                   const wxValidator& val = wxDefaultValidator,
                   const wxString& name = wxCheckBoxNameStr);

    /**
        Destructor, destroying the toggle button.
    */
    virtual ~wxToggleButton();

    /**
        Creates the toggle button for two-step construction.
        See wxToggleButton() for details.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& label,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxValidator& val = wxDefaultValidator,
                const wxString& name = wxCheckBoxNameStr);

    /**
        Gets the state of the toggle button.

        @return Returns @true if it is pressed, @false otherwise.
    */
    virtual bool GetValue() const;

    /**
        Sets the toggle button to the given state.
        This does not cause a @c EVT_TOGGLEBUTTON event to be emitted.

        @param state
            If @true, the button is pressed.
    */
    virtual void SetValue(bool state);
};


/**
    @class wxBitmapToggleButton

    wxBitmapToggleButton is a wxToggleButton that contains a bitmap instead of
    text.

    This class is not available in all ports currently (although it is
    available in the major ones), test for @c wxHAS_BITMAPTOGGLEBUTTON to
    determine whether it can be used (in addition for possibly testing for
    @c wxUSE_TOGGLEBTN which can be set to 0 to explicitly disable support for
    this class and wxToggleButton).

    This control emits an update UI event.

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_TOGGLEBUTTON(id, func)}
        Handles a wxEVT_TOGGLEBUTTON event.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
*/
class wxBitmapToggleButton : public wxToggleButton
{
public:
    /**
        Default constructor.
    */
    wxBitmapToggleButton();

    /**
        Constructor, creating and showing a toggle button with the bitmap @e label.
        Internally calls Create().
    */
    wxBitmapToggleButton(wxWindow* parent, wxWindowID id,
                         const wxBitmap& label,
                         const wxPoint& pos = wxDefaultPosition,
                         const wxSize& size = wxDefaultSize,
                         long style = 0,
                         const wxValidator& val = wxDefaultValidator,
                         const wxString& name = wxCheckBoxNameStr);

    /**
        Create method for two-step construction.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxBitmap& label,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxValidator& val = wxDefaultValidator,
                const wxString& name = wxCheckBoxNameStr);

    /**
        Gets the state of the toggle button.

        @return Returns @true if it is pressed, @false otherwise.
    */
    virtual bool GetValue() const;

    /**
        Sets the toggle button to the given state.
        This does not cause a @c EVT_TOGGLEBUTTON event to be emitted.

        @param state
            If @true, the button is pressed.
    */
    virtual void SetValue(bool state);
};

