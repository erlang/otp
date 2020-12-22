/////////////////////////////////////////////////////////////////////////////
// Name:        checklst.h
// Purpose:     interface of wxCheckListBox
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxCheckListBox

    A wxCheckListBox is like a wxListBox, but allows items to be checked or
    unchecked.

    When using this class under Windows wxWidgets must be compiled with
    wxUSE_OWNER_DRAWN set to 1.

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_CHECKLISTBOX(id, func)}
        Process a @c wxEVT_CHECKLISTBOX event, when an item in
        the check list box is checked or unchecked. wxCommandEvent::GetInt()
        will contain the index of the item that was checked or unchecked.
        wxCommandEvent::IsChecked() is not valid! Use wxCheckListBox::IsChecked()
        instead.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{checklistbox}

    @see wxListBox, wxChoice, wxComboBox, wxListCtrl, wxCommandEvent
*/
class wxCheckListBox : public wxListBox
{
public:
    /**
        Default constructor.
    */
    wxCheckListBox();

    //@{
    /**
        Constructor, creating and showing a list box.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param pos
            Window position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Window size.
            If ::wxDefaultSize is specified then the window is sized appropriately.
        @param n
            Number of strings with which to initialise the control.
        @param choices
            An array of strings with which to initialise the control.
        @param style
            Window style. See wxCheckListBox.
        @param validator
            Window validator.
        @param name
            Window name.

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    wxCheckListBox(wxWindow* parent, wxWindowID id,
                   const wxPoint& pos = wxDefaultPosition,
                   const wxSize& size = wxDefaultSize,
                   int n = 0,
                   const wxString choices[] = NULL,
                   long style = 0,
                   const wxValidator& validator = wxDefaultValidator,
                   const wxString& name = "listBox");
    /**
        Constructor, creating and showing a list box.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param pos
            Window position.
        @param size
            Window size. If wxDefaultSize is specified then the window is sized
            appropriately.
        @param choices
            An array of strings with which to initialise the control.
        @param style
            Window style. See wxCheckListBox.
        @param validator
            Window validator.
        @param name
            Window name.

        @beginWxPerlOnly
        Use an array reference for the @a choices parameter.
        @endWxPerlOnly
    */
    wxCheckListBox(wxWindow* parent, wxWindowID id,
                   const wxPoint& pos,
                   const wxSize& size,
                   const wxArrayString& choices,
                   long style = 0,
                   const wxValidator& validator = wxDefaultValidator,
                   const wxString& name = "listBox");
    //@}

    bool Create(wxWindow *parent,
                wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                int nStrings = 0,
                const wxString choices[] = NULL,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxListBoxNameStr);

    bool Create(wxWindow *parent,
                wxWindowID id,
                const wxPoint& pos,
                const wxSize& size,
                const wxArrayString& choices,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxListBoxNameStr);

    /**
        Destructor, destroying the list box.
    */
    virtual ~wxCheckListBox();

    /**
        Checks the given item. Note that calling this method does not result in
        a @c wxEVT_CHECKLISTBOX event being emitted.

        @param item
            Index of item to check.
        @param check
            @true if the item is to be checked, @false otherwise.
    */
    void Check(unsigned int item, bool check = true);

    /**
        Returns @true if the given item is checked, @false otherwise.

        @param item
            Index of item whose check status is to be returned.
    */
    bool IsChecked(unsigned int item) const;

    /**
        Return the indices of the checked items.

        @param checkedItems
            A reference to the array that is filled with the indices of the
            checked items.
        @return The number of checked items.

        @see Check(), IsChecked()

        @since 2.9.5
    */
    unsigned int GetCheckedItems(wxArrayInt& checkedItems) const;
};

