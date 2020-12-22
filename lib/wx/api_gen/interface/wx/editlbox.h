/////////////////////////////////////////////////////////////////////////////
// Name:        editlbox.h
// Purpose:     interface of wxEditableListBox
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#define wxEL_ALLOW_NEW          0x0100
#define wxEL_ALLOW_EDIT         0x0200
#define wxEL_ALLOW_DELETE       0x0400
#define wxEL_NO_REORDER         0x0800
#define wxEL_DEFAULT_STYLE      (wxEL_ALLOW_NEW | wxEL_ALLOW_EDIT | wxEL_ALLOW_DELETE)

/**
    @class wxEditableListBox

    An editable listbox is composite control that lets the user easily enter,
    delete and reorder a list of strings.

    @beginStyleTable
    @style{wxEL_ALLOW_NEW}
           Allows the user to enter new strings.
    @style{wxEL_ALLOW_EDIT}
           Allows the user to edit existing strings.
    @style{wxEL_ALLOW_DELETE}
           Allows the user to delete existing strings.
    @style{wxEL_NO_REORDER}
           Does not allow the user to reorder the strings.
    @style{wxEL_DEFAULT_STYLE}
           Default style: wxEL_ALLOW_NEW|wxEL_ALLOW_EDIT|wxEL_ALLOW_DELETE.
    @endStyleTable

    The control uses a wxListCtrl internally and emit its events.

    @library{wxcore}
    @category{ctrl}

    @see wxListBox, wxListCtrl
*/
class wxEditableListBox : public wxPanel
{
public:
    /**
        Default ctor.
    */
    wxEditableListBox();

    /**
        Constructor, creating and showing a list box.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param label
            The text shown just before the list control.
        @param pos
            Window position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Window size.
            If ::wxDefaultSize is specified then the window is sized appropriately.
        @param style
            Window style. See wxEditableListBox.
        @param name
            Window name.

        @see Create()
    */
    wxEditableListBox(wxWindow* parent, wxWindowID id,
                      const wxString& label,
                      const wxPoint& pos = wxDefaultPosition,
                      const wxSize& size = wxDefaultSize,
                      long style = wxEL_DEFAULT_STYLE,
                      const wxString& name = wxEditableListBoxNameStr);

    /**
        Destructor, destroying the list box.
    */
    virtual ~wxEditableListBox();

    /**
        Creates the editable listbox for two-step construction.
        See wxEditableListBox() for further details.
    */
    bool Create(wxWindow* parent, wxWindowID id, const wxString& label,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxEL_DEFAULT_STYLE,
                const wxString& name = wxEditableListBoxNameStr);

    /**
        Replaces current contents with given strings.
    */
    void SetStrings(const wxArrayString& strings);


    /**
        Returns in the given array the current contents of the control
        (the array will be erased before control's contents are appended).
    */
    void GetStrings(wxArrayString& strings) const;
};

