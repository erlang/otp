/////////////////////////////////////////////////////////////////////////////
// Name:        bmpcbox.h
// Purpose:     interface of wxBitmapComboBox
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxBitmapComboBox

    A combobox that displays bitmap in front of the list items.
    It currently only allows using bitmaps of one size, and resizes itself
    so that a bitmap can be shown next to the text field.

    @remarks
    While wxBitmapComboBox contains the wxComboBox API, but it might not actually
    be derived from that class. In fact, if the platform does not have a native
    implementation, wxBitmapComboBox will inherit from wxOwnerDrawnComboBox.
    You can determine if the implementation is generic by checking whether
    @c wxGENERIC_BITMAPCOMBOBOX is defined. Currently wxBitmapComboBox is
    implemented natively for MSW and GTK+.

    @beginStyleTable
    @style{wxCB_READONLY}
           Creates a combobox without a text editor. On some platforms the
           control may appear very different when this style is used.
    @style{wxCB_SORT}
           Sorts the entries in the list alphabetically.
    @style{wxTE_PROCESS_ENTER}
           The control will generate the event wxEVT_TEXT_ENTER
           (otherwise pressing Enter key is either processed internally by the
           control or used for navigation between dialog controls).
           Windows only.
    @endStyleTable

    @todo create wxCB_PROCESS_ENTER rather than reusing wxTE_PROCESS_ENTER!

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_COMBOBOX(id, func)}
           Process a @c wxEVT_COMBOBOX event, when an item on
           the list is selected.
    @event{EVT_TEXT(id, func)}
           Process a @c wxEVT_TEXT event, when the combobox text changes.
    @event{EVT_TEXT_ENTER(id, func)}
           Process a @c wxEVT_TEXT_ENTER event, when RETURN is pressed in
           the combobox.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{bitmapcombobox}

    @see wxComboBox, wxChoice, wxOwnerDrawnComboBox, wxCommandEvent
*/
class wxBitmapComboBox : public wxComboBox
{
public:
    /**
        Default ctor.
    */
    wxBitmapComboBox();

    /**
        Constructor, creating and showing a combobox.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param value
            Initial selection string. An empty string indicates no selection.
        @param pos
            Initial position.
        @param size
            Initial size.
        @param n
            Number of strings with which to initialise the control.
        @param choices
            An array of strings with which to initialise the control.
        @param style
            The window style, see wxCB_* flags.
        @param validator
            Validator which can be used for additional data checks.
        @param name
            Control name.

        @see Create(), wxValidator
    */
    wxBitmapComboBox(wxWindow* parent, wxWindowID id = wxID_ANY,
                     const wxString& value = wxEmptyString,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     int n = 0,
                     const wxString choices[] = NULL,
                     long style = 0,
                     const wxValidator& validator = wxDefaultValidator,
                     const wxString& name = wxBitmapComboBoxNameStr);

    /**
        Constructor, creating and showing a combobox.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param value
            Initial selection string. An empty string indicates no selection.
        @param pos
            Initial position.
        @param size
            Initial size.
        @param choices
            An wxArrayString with which to initialise the control.
        @param style
            The window style, see wxCB_* flags.
        @param validator
            Validator which can be used for additional data checks.
        @param name
            Control name.

        @see Create(), wxValidator
    */
    wxBitmapComboBox(wxWindow* parent, wxWindowID id,
                     const wxString& value,
                     const wxPoint& pos,
                     const wxSize& size,
                     const wxArrayString& choices,
                     long style,
                     const wxValidator& validator = wxDefaultValidator,
                     const wxString& name = wxBitmapComboBoxNameStr);

    /**
        Destructor, destroying the combobox.
    */
    virtual ~wxBitmapComboBox();

    /**
        Adds the item to the end of the combo box.
    */
    int Append(const wxString& item,
               const wxBitmap& bitmap = wxNullBitmap);

    /**
        Adds the item to the end of the combo box, associating the given
        untyped, client data pointer @a clientData with the item.
    */
    int Append(const wxString& item, const wxBitmap& bitmap,
               void* clientData);

    /**
        Adds the item to the end of the combo box, associating the given typed
        client data pointer @a clientData with the item.
    */
    int Append(const wxString& item, const wxBitmap& bitmap,
               wxClientData* clientData);

    /**
        Creates the combobox for two-step construction.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& value,
                const wxPoint& pos,
                const wxSize& size,
                int n, const wxString choices[],
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxBitmapComboBoxNameStr);

    /**
        Creates the combobox for two-step construction.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& value,
                const wxPoint& pos,
                const wxSize& size,
                const wxArrayString& choices,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxBitmapComboBoxNameStr);

    /**
        Returns the size of the bitmaps used in the combo box.
        If the combo box is empty, then ::wxDefaultSize is returned.
    */
    virtual wxSize GetBitmapSize() const;

    /**
        Returns the bitmap of the item with the given index.
    */
    virtual wxBitmap GetItemBitmap(unsigned int n) const;

    /**
        Inserts the item into the list before @a pos.
        Not valid for @c wxCB_SORT style, use Append() instead.
    */
    int Insert(const wxString& item, const wxBitmap& bitmap,
               unsigned int pos);

    /**
        Inserts the item into the list before pos, associating the given
        untyped, client data pointer with the item.
        Not valid for @c wxCB_SORT style, use Append() instead.
    */
    int Insert(const wxString& item, const wxBitmap& bitmap,
               unsigned int pos,
               void* clientData);

    /**
        Inserts the item into the list before pos, associating the given typed
        client data pointer with the item.
        Not valid for @c wxCB_SORT style, use Append() instead.
    */
    int Insert(const wxString& item, const wxBitmap& bitmap,
               unsigned int pos,
               wxClientData* clientData);

    /**
        Sets the bitmap for the given item.
    */
    virtual void SetItemBitmap(unsigned int n, const wxBitmap& bitmap);
};

