/////////////////////////////////////////////////////////////////////////////
// Name:        radiobut.h
// Purpose:     interface of wxRadioButton
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxRadioButton

    A radio button item is a button which usually denotes one of several
    mutually exclusive options. It has a text label next to a (usually) round
    button.

    You can create a group of mutually-exclusive radio buttons by specifying
    @c wxRB_GROUP for the first in the group. The group ends when another
    radio button group is created, or there are no more radio buttons.

    @beginStyleTable
    @style{wxRB_GROUP}
           Marks the beginning of a new group of radio buttons.
    @style{wxRB_SINGLE}
           In some circumstances, radio buttons that are not consecutive
           siblings trigger a hang bug in Windows (only). If this happens, add
           this style to mark the button as not belonging to a group, and
           implement the mutually-exclusive group behaviour yourself.
    @endStyleTable

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_RADIOBUTTON(id, func)}
           Process a @c wxEVT_RADIOBUTTON event, when the
           radiobutton is clicked.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{radiobutton}

    @see @ref overview_events, wxRadioBox, wxCheckBox
*/
class wxRadioButton : public wxControl
{
public:
    /**
        Default constructor.

        @see Create(), wxValidator
    */
    wxRadioButton();

    /**
        Constructor, creating and showing a radio button.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value @c wxID_ANY indicates a default value.
        @param label
            Label for the radio button.
        @param pos
            Window position. If ::wxDefaultPosition is specified then a default
            position is chosen.
        @param size
            Window size. If ::wxDefaultSize is specified then a default size
            is chosen.
        @param style
            Window style. See wxRadioButton.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxRadioButton(wxWindow* parent, wxWindowID id,
                  const wxString& label,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = 0,
                  const wxValidator& validator = wxDefaultValidator,
                  const wxString& name = wxRadioButtonNameStr);

    /**
        Destructor, destroying the radio button item.
    */
    virtual ~wxRadioButton();

    /**
        Creates the choice for two-step construction. See wxRadioButton() for
        further details.
    */
    bool Create(wxWindow* parent, wxWindowID id, const wxString& label,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxRadioButtonNameStr);

    /**
        Returns @true if the radio button is checked, @false otherwise.
    */
    virtual bool GetValue() const;

    /**
        Sets the radio button to checked or unchecked status. This does not cause a
        @c wxEVT_RADIOBUTTON event to get emitted.

        If the radio button belongs to a radio group exactly one button in the
        group may be checked and so this method can be only called with @a
        value set to @true. To uncheck a radio button in a group you must check
        another button in the same group.

        @note Under MSW, the focused radio button is always selected, i.e. its
            value is @true. And, conversely, calling @c SetValue(true) will
            also set focus to the radio button if the focus had previously been
            on another radio button in the same group -- as otherwise setting
            it on wouldn't work.

        @param value
            @true to check, @false to uncheck.
    */
    virtual void SetValue(bool value);
};

