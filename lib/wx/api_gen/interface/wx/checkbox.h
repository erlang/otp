/////////////////////////////////////////////////////////////////////////////
// Name:        checkbox.h
// Purpose:     interface of wxCheckBox
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/*
 * wxCheckBox style flags
 * (Using wxCHK_* because wxCB_* is used by wxComboBox).
 * Determine whether to use a 3-state or 2-state
 * checkbox. 3-state enables to differentiate
 * between 'unchecked', 'checked' and 'undetermined'.
 *
 * In addition to the styles here it is also possible to specify just 0 which
 * is treated the same as wxCHK_2STATE for compatibility (but using explicit
 * flag is preferred).
 */
#define wxCHK_2STATE           0x4000
#define wxCHK_3STATE           0x1000

/*
 * If this style is set the user can set the checkbox to the
 * undetermined state. If not set the undetermined set can only
 * be set programmatically.
 * This style can only be used with 3 state checkboxes.
 */
#define wxCHK_ALLOW_3RD_STATE_FOR_USER 0x2000

/**
    The possible states of a 3-state wxCheckBox (Compatible with the 2-state
    wxCheckBox).
*/
enum wxCheckBoxState
{
    wxCHK_UNCHECKED,
    wxCHK_CHECKED,
    wxCHK_UNDETERMINED  ///< 3-state checkbox only
};

/**
    @class wxCheckBox

    A checkbox is a labelled box which by default is either on (checkmark is
    visible) or off (no checkmark). Optionally (when the wxCHK_3STATE style
    flag is set) it can have a third state, called the mixed or undetermined
    state. Often this is used as a "Does Not Apply" state.

    @beginStyleTable
    @style{wxCHK_2STATE}
           Create a 2-state checkbox. This is the default.
    @style{wxCHK_3STATE}
           Create a 3-state checkbox. Not implemented in wxGTK1.
    @style{wxCHK_ALLOW_3RD_STATE_FOR_USER}
           By default a user can't set a 3-state checkbox to the third state.
           It can only be done from code. Using this flags allows the user to
           set the checkbox to the third state by clicking.
    @style{wxALIGN_RIGHT}
           Makes the text appear on the left of the checkbox.
    @endStyleTable

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_CHECKBOX(id, func)}
           Process a @c wxEVT_CHECKBOX event, when the checkbox
           is clicked.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{checkbox}

    @see wxRadioButton, wxCommandEvent
*/
class wxCheckBox : public wxControl
{
public:
    /**
        Default constructor.

        @see Create(), wxValidator
    */
    wxCheckBox();

    /**
        Constructor, creating and showing a checkbox.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Checkbox identifier. The value wxID_ANY indicates a default value.
        @param label
            Text to be displayed next to the checkbox.
        @param pos
            Checkbox position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Checkbox size.
            If ::wxDefaultSize is specified then a default size is chosen.
        @param style
            Window style. See wxCheckBox.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxCheckBox(wxWindow* parent, wxWindowID id,
               const wxString& label,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = 0,
               const wxValidator& validator = wxDefaultValidator,
               const wxString& name = wxCheckBoxNameStr);

    /**
        Destructor, destroying the checkbox.
    */
    virtual ~wxCheckBox();

    /**
        Creates the checkbox for two-step construction. See wxCheckBox()
        for details.
    */
    bool Create(wxWindow* parent, wxWindowID id, const wxString& label,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxCheckBoxNameStr);

    /**
        Gets the state of a 2-state checkbox.

        @return Returns @true if it is checked, @false otherwise.
    */
    virtual bool GetValue() const;

    /**
        Gets the state of a 3-state checkbox. Asserts when the function is used
        with a 2-state checkbox.
    */
    wxCheckBoxState Get3StateValue() const;

    /**
        Returns whether or not the checkbox is a 3-state checkbox.

        @return @true if this checkbox is a 3-state checkbox, @false if it's
                a 2-state checkbox.
    */
    bool Is3State() const;

    /**
        Returns whether or not the user can set the checkbox to the third
        state.

        @return @true if the user can set the third state of this checkbox,
                @false if it can only be set programmatically or if it's a
                2-state checkbox.
    */
    bool Is3rdStateAllowedForUser() const;

    /**
        This is just a maybe more readable synonym for GetValue(): just as the
        latter, it returns @true if the checkbox is checked and @false
        otherwise.
    */
    bool IsChecked() const;

    /**
        Sets the checkbox to the given state. This does not cause a
        @c wxEVT_CHECKBOX event to get emitted.

        @param state
            If @true, the check is on, otherwise it is off.
    */
    virtual void SetValue(bool state);

    /**
        Sets the checkbox to the given state. This does not cause a
        @c wxEVT_CHECKBOX event to get emitted.

        Asserts when the checkbox is a 2-state checkbox and setting the state
        to wxCHK_UNDETERMINED.
    */
    void Set3StateValue(wxCheckBoxState state);
};

