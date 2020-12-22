/////////////////////////////////////////////////////////////////////////////
// Name:        combobox.h
// Purpose:     interface of wxComboBox
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxComboBox

    A combobox is like a combination of an edit control and a listbox.

    It can be displayed as static list with editable or read-only text field;
    or a drop-down list with text field; or a drop-down list without a text
    field depending on the platform and presence of wxCB_READONLY style.

    A combobox permits a single selection only. Combobox items are numbered
    from zero.

    If you need a customized combobox, have a look at wxComboCtrl,
    wxOwnerDrawnComboBox, wxComboPopup and the ready-to-use wxBitmapComboBox.

    Please refer to wxTextEntry documentation for the description of methods
    operating with the text entry part of the combobox and to wxItemContainer
    for the methods operating with the list of strings. Notice that at least
    under MSW wxComboBox doesn't behave correctly if it contains strings
    differing in case only so portable programs should avoid adding such
    strings to this control.

    @beginStyleTable
    @style{wxCB_SIMPLE}
           Creates a combobox with a permanently displayed list. Windows only.
    @style{wxCB_DROPDOWN}
           Creates a combobox with a drop-down list. MSW and Motif only.
    @style{wxCB_READONLY}
           A combobox with this style behaves like a wxChoice (and may look in
           the same way as well, although this is platform-dependent), i.e. it
           allows the user to choose from the list of options but doesn't allow
           to enter a value not present in the list.
    @style{wxCB_SORT}
           Sorts the entries in the list alphabetically.
    @style{wxTE_PROCESS_ENTER}
           The control will generate the event @c wxEVT_TEXT_ENTER that can be
           handled by the program. Otherwise, i.e. either if this style not
           specified at all, or it is used, but there is no event handler for
           this event or the event handler called wxEvent::Skip() to avoid
           overriding the default handling, pressing Enter key is either
           processed internally by the control or used to activate the default
           button of the dialog, if any.
    @endStyleTable

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_COMBOBOX(id, func)}
           Process a @c wxEVT_COMBOBOX event, when an item on
           the list is selected. Note that calling GetValue() returns the new
           value of selection.
    @event{EVT_TEXT(id, func)}
           Process a @c wxEVT_TEXT event, when the combobox text
           changes.
    @event{EVT_TEXT_ENTER(id, func)}
           Process a @c wxEVT_TEXT_ENTER event, when RETURN is pressed in
           the combobox (notice that the combobox must have been created with
           wxTE_PROCESS_ENTER style to receive this event).
    @event{EVT_COMBOBOX_DROPDOWN(id, func)}
           Process a @c wxEVT_COMBOBOX_DROPDOWN event, which is generated
           when the list box part of the combo box is shown (drops down).
           Notice that this event is only supported by wxMSW, wxGTK with GTK+
           2.10 or later, and wxOSX/Cocoa.
    @event{EVT_COMBOBOX_CLOSEUP(id, func)}
           Process a @c wxEVT_COMBOBOX_CLOSEUP event, which is generated
           when the list box of the combo box disappears (closes up). This
           event is only generated for the same platforms as
           @c wxEVT_COMBOBOX_DROPDOWN above.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{combobox}

    @see wxListBox, wxTextCtrl, wxChoice, wxCommandEvent
*/
class wxComboBox : public wxControl,
                   public wxItemContainer,
                   public wxTextEntry
{
public:
    /**
        Default constructor.
    */
    wxComboBox();

    /**
        Constructor, creating and showing a combobox.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param value
            Initial selection string. An empty string indicates no selection.
            Notice that for the controls with @c wxCB_READONLY style this
            string must be one of the valid choices if it is not empty.
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
            Window style. See wxComboBox.
        @param validator
            Window validator.
        @param name
            Window name.

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly

        @see Create(), wxValidator
    */
    wxComboBox(wxWindow* parent, wxWindowID id,
               const wxString& value = wxEmptyString,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               int n = 0,
               const wxString choices[] = NULL,
               long style = 0,
               const wxValidator& validator = wxDefaultValidator,
               const wxString& name = wxComboBoxNameStr);
    /**
        Constructor, creating and showing a combobox.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param value
            Initial selection string. An empty string indicates no selection.
        @param pos
            Window position.
        @param size
            Window size. If wxDefaultSize is specified then the window is sized
            appropriately.
        @param choices
            An array of strings with which to initialise the control.
        @param style
            Window style. See wxComboBox.
        @param validator
            Window validator.
        @param name
            Window name.

        @beginWxPerlOnly
        Use an array reference for the @a choices parameter.
        @endWxPerlOnly

        @see Create(), wxValidator
    */
    wxComboBox(wxWindow* parent, wxWindowID id,
               const wxString& value,
               const wxPoint& pos,
               const wxSize& size,
               const wxArrayString& choices,
               long style = 0,
               const wxValidator& validator = wxDefaultValidator,
               const wxString& name = wxComboBoxNameStr);

    /**
        Destructor, destroying the combobox.
    */
    virtual ~wxComboBox();

    //@{
    /**
        Creates the combobox for two-step construction. Derived classes should
        call or replace this function. See wxComboBox() for further details.
    */
    bool Create(wxWindow *parent, wxWindowID id,
                const wxString& value = wxEmptyString,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                int n = 0, const wxString choices[] = (const wxString *) NULL,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxComboBoxNameStr);
    bool Create(wxWindow *parent, wxWindowID id,
                const wxString& value,
                const wxPoint& pos,
                const wxSize& size,
                const wxArrayString& choices,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxComboBoxNameStr);
    //@}

    /**
        Returns the item being selected right now.

        This function does the same things as wxChoice::GetCurrentSelection()
        and returns the item currently selected in the dropdown list if it's
        open or the same thing as wxControlWithItems::GetSelection() otherwise.
    */
    virtual int GetCurrentSelection() const;

    /**
        Same as wxTextEntry::GetInsertionPoint().

        @note Under wxMSW, this function always returns 0 if the combobox
              doesn't have the focus.
    */
    virtual long GetInsertionPoint() const;

    /**
        IsEmpty() is not available in this class.

        This method is documented here only to notice that it can't be used
        with this class because of the ambiguity between the methods with the
        same name inherited from wxItemContainer and wxTextEntry base classes.

        Because of this, any attempt to call it results in a compilation error
        and you should use either IsListEmpty() or IsTextEmpty() depending on
        what exactly do you want to test.
     */
    bool IsEmpty() const;

    /**
        Returns true if the list of combobox choices is empty.

        Use this method instead of (not available in this class) IsEmpty() to
        test if the list of items is empty.

        @since 2.9.3
     */
    bool IsListEmpty() const;

    /**
        Returns true if the text of the combobox is empty.

        Use this method instead of (not available in this class) IsEmpty() to
        test if the text currently entered into the combobox is empty.

        @since 2.9.3
     */
    bool IsTextEmpty() const;

    /**
        Same as wxTextEntry::SetSelection().
    */
    virtual void SetSelection(long from, long to);

    /**
        Sets the text for the combobox text field.

        For normal, editable comboboxes with a text entry field calling this
        method will generate a @c wxEVT_TEXT event, consistently with
        wxTextEntry::SetValue() behaviour, use wxTextEntry::ChangeValue() if
        this is undesirable.

        For controls with @c wxCB_READONLY style the method behaves somewhat
        differently: the string must be in the combobox choices list (the check
        for this is case-insensitive) and @c wxEVT_TEXT is @e not generated in
        this case.

        @param text
            The text to set.
    */
    virtual void SetValue(const wxString& text);

    /**
        Shows the list box portion of the combo box.

        Currently this method is implemented in wxMSW, wxGTK and wxOSX/Cocoa.

        Notice that calling this function will generate a
        @c wxEVT_COMBOBOX_DROPDOWN event except under wxOSX where
        generation of this event is not supported at all.

        @since 2.9.1
    */
    virtual void Popup();

    /**
        Hides the list box portion of the combo box.

        Currently this method is implemented in wxMSW, wxGTK and wxOSX/Cocoa.

        Notice that calling this function will generate a
        @c wxEVT_COMBOBOX_CLOSEUP event except under wxOSX where
        generation of this event is not supported at all.

        @since 2.9.1
    */
    virtual void Dismiss();

    virtual int GetSelection() const;
    virtual void GetSelection(long *from, long *to) const;
    virtual void SetSelection(int n);
    virtual int FindString(const wxString& s, bool bCase = false) const;
    virtual wxString GetString(unsigned int n) const;
    virtual wxString GetStringSelection() const;

    /**
        Changes the text of the specified combobox item.

        Notice that if the item is the currently selected one, i.e. if its text
        is displayed in the text part of the combobox, then the text is also
        replaced with the new @a text.
     */
    virtual void SetString(unsigned int n, const wxString& text);

    virtual unsigned int GetCount() const;
};

