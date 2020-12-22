/////////////////////////////////////////////////////////////////////////////
// Name:        choice.h
// Purpose:     interface of wxChoice
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxChoice

    A choice item is used to select one of a list of strings. Unlike a
    wxListBox, only the selection is visible until the user pulls down the
    menu of choices.

    @beginStyleTable
    @style{wxCB_SORT}
           Sorts the entries alphabetically.
    @endStyleTable

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_CHOICE(id, func)}
           Process a @c wxEVT_CHOICE event, when an item on the
           list is selected.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{choice}

    @see wxListBox, wxComboBox, wxCommandEvent
*/
class wxChoice : public wxControl,
                 public wxItemContainer
{
public:
    /**
        Default constructor.

        @see Create(), wxValidator
    */
    wxChoice();

    /**
        Constructor, creating and showing a choice.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param pos
            Window position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Window size.
            If ::wxDefaultSize is specified then the choice is sized appropriately.
        @param n
            Number of strings with which to initialise the choice control.
        @param choices
            An array of strings with which to initialise the choice control.
        @param style
            Window style. See wxChoice.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    wxChoice( wxWindow *parent, wxWindowID id,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize,
            int n = 0, const wxString choices[] = NULL,
            long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxChoiceNameStr );

    /**
        Constructor, creating and showing a choice.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param pos
            Window position.
        @param size
            Window size. If wxDefaultSize is specified then the choice is sized
            appropriately.
        @param choices
            An array of strings with which to initialise the choice control.
        @param style
            Window style. See wxChoice.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator

        @beginWxPerlOnly
        Use an array reference for the @a choices parameter.
        @endWxPerlOnly
    */
    wxChoice( wxWindow *parent, wxWindowID id,
            const wxPoint& pos,
            const wxSize& size,
            const wxArrayString& choices,
            long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxChoiceNameStr );

    /**
        Destructor, destroying the choice item.
    */
    virtual ~wxChoice();

    //@{
    /**
        Creates the choice for two-step construction. See wxChoice().
    */
    bool Create( wxWindow *parent, wxWindowID id,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize,
            int n = 0, const wxString choices[] = NULL,
            long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxChoiceNameStr );
    bool Create( wxWindow *parent, wxWindowID id,
            const wxPoint& pos,
            const wxSize& size,
            const wxArrayString& choices,
            long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxChoiceNameStr );
    //@}

    /**
        Gets the number of columns in this choice item.

        @remarks This is implemented for GTK and Motif only and always
                 returns 1 for the other platforms.
    */
    virtual int GetColumns() const;

    /**
        Unlike wxControlWithItems::GetSelection() which only returns the
        accepted selection value (the selection in the control once the
        user closes the dropdown list), this function returns the current
        selection. That is, while the dropdown list is shown, it returns the
        currently selected item in it. When it is not shown, its result is the
        same as for the other function.

        @since 2.6.2.
               In older versions, wxControlWithItems::GetSelection() itself
               behaved like this.
    */
    virtual int GetCurrentSelection() const;

    /**
        Sets the number of columns in this choice item.

        @param n
            Number of columns.

        @remarks This is implemented for GTK and Motif only and doesn’t do
                 anything under other platforms.
    */
    virtual void SetColumns(int n = 1);

    virtual bool IsSorted() const;

    virtual unsigned int GetCount() const ;
    virtual int GetSelection() const ;
    virtual void SetSelection(int n);

    virtual int FindString(const wxString& s, bool bCase = false) const;
    virtual wxString GetString(unsigned int n) const ;
    virtual void SetString(unsigned int pos, const wxString& s);
};

