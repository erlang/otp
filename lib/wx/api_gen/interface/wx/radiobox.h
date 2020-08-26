/////////////////////////////////////////////////////////////////////////////
// Name:        radiobox.h
// Purpose:     interface of wxRadioBox
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxRadioBox

    A radio box item is used to select one of number of mutually exclusive
    choices.  It is displayed as a vertical column or horizontal row of
    labelled buttons.

    @beginStyleTable
    @style{wxRA_SPECIFY_ROWS}
           The major dimension parameter refers to the maximum number of rows.
    @style{wxRA_SPECIFY_COLS}
           The major dimension parameter refers to the maximum number of
           columns.
    @endStyleTable

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_RADIOBOX(id, func)}
           Process a @c wxEVT_RADIOBOX event, when a radiobutton
           is clicked.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{radiobox}

    @see @ref overview_events, wxRadioButton, wxCheckBox
*/
class wxRadioBox : public wxControl, wxItemContainerImmutable
{
public:

    /**
        Default constructor.

        @see Create(), wxValidator
    */
    wxRadioBox();

    /**
        Constructor, creating and showing a radiobox.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value @c wxID_ANY indicates a default value.
        @param label
            Label for the static box surrounding the radio buttons.
        @param pos
            Window position. If ::wxDefaultPosition is specified then a
            default position is chosen.
        @param size
            Window size. If ::wxDefaultSize is specified then a default size
            is chosen.
        @param n
            Number of choices with which to initialize the radiobox.
        @param choices
            An array of choices with which to initialize the radiobox.
        @param majorDimension
            Specifies the maximum number of rows (if style contains
            @c wxRA_SPECIFY_ROWS) or columns (if style contains
            @c wxRA_SPECIFY_COLS) for a two-dimensional radiobox. The default
            value of 0 means to use the number of items, i.e. @a n.
        @param style
            Window style. See wxRadioBox.
        @param validator
            Window validator.
        @param name
            Window name.

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly

        @see Create(), wxValidator
    */
    wxRadioBox(wxWindow* parent, wxWindowID id,
               const wxString& label,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               int n = 0,
               const wxString choices[] = NULL,
               int majorDimension = 0,
               long style = wxRA_SPECIFY_COLS,
               const wxValidator& validator = wxDefaultValidator,
               const wxString& name = wxRadioBoxNameStr);

    /**
        Constructor, creating and showing a radiobox.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value @c wxID_ANY indicates a default value.
        @param label
            Label for the static box surrounding the radio buttons.
        @param pos
            Window position. If ::wxDefaultPosition is specified then a
            default position is chosen.
        @param size
            Window size. If ::wxDefaultSize is specified then a default size
            is chosen.
        @param choices
            An array of choices with which to initialize the radiobox.
        @param majorDimension
            Specifies the maximum number of rows (if style contains
            @c wxRA_SPECIFY_ROWS) or columns (if style contains
            @c wxRA_SPECIFY_COLS) for a two-dimensional radiobox. The default
            value of 0 means to use the number of items, i.e. number of
            elements in @a choices.
        @param style
            Window style. See wxRadioBox.
        @param validator
            Window validator.
        @param name
            Window name.

        @beginWxPerlOnly
        Use an array reference for the @a choices parameter.
        @endWxPerlOnly

        @see Create(), wxValidator
    */
    wxRadioBox(wxWindow* parent, wxWindowID id,
               const wxString& label,
               const wxPoint& pos,
               const wxSize& size,
               const wxArrayString& choices,
               int majorDimension = 0,
               long style = wxRA_SPECIFY_COLS,
               const wxValidator& validator = wxDefaultValidator,
               const wxString& name = wxRadioBoxNameStr);

    /**
        Destructor, destroying the radiobox item.
    */
    virtual ~wxRadioBox();

    /**
        Creates the radiobox for two-step construction. See wxRadioBox()
        for further details.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& label,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                int n = 0,
                const wxString choices[] = NULL,
                int majorDimension = 0,
                long style = wxRA_SPECIFY_COLS,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxRadioBoxNameStr);

    /**
        Creates the radiobox for two-step construction. See wxRadioBox()
        for further details.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& label,
                const wxPoint& pos,
                const wxSize& size,
                const wxArrayString& choices,
                int majorDimension = 0,
                long style = wxRA_SPECIFY_COLS,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxRadioBoxNameStr);

    /**
        Enables or disables an individual button in the radiobox.

        @param enable
            @true to enable, @false to disable.
        @param n
            The zero-based button to enable or disable.

        @see wxWindow::Enable()

    */
    virtual bool Enable(unsigned int n, bool enable = true);

    /**
        Finds a button matching the given string, returning the position if found,
        or @c wxNOT_FOUND if not found.

        @param string
            The string to find.
        @param bCase
            Should the search be case-sensitive?
    */
    virtual int FindString(const wxString& string, bool bCase = false) const;

    /**
        Returns the number of columns in the radiobox.
    */
    unsigned int GetColumnCount() const;

    /**
        Returns a radio box item under the point, a zero-based item index, or @c
        wxNOT_FOUND if no item is under the point.

        @param pt
            Point in client coordinates.
    */
    virtual int GetItemFromPoint(const wxPoint& pt) const;

    /**
        Returns the helptext associated with the specified @a item if any or @c
        wxEmptyString.

        @param item
            The zero-based item index.

        @see SetItemHelpText()
    */
    wxString GetItemHelpText(unsigned int item) const;

    /**
        Returns the tooltip associated with the specified @a item if any or @NULL.

        @see SetItemToolTip(), wxWindow::GetToolTip()
    */
    wxToolTip* GetItemToolTip(unsigned int item) const;

    /**
        Returns the number of rows in the radiobox.
    */
    unsigned int GetRowCount() const;

    /**
        Returns @true if the item is enabled or @false if it was disabled using
        @ref Enable(unsigned int,bool) "Enable(n, false)".

        This function is currently only implemented in wxMSW, wxGTK, wxQT and
        wxUniversal and always returns @true in the other ports.

        @param n
            The zero-based button position.
    */
    virtual bool IsItemEnabled(unsigned int n) const;

    /**
        Returns @true if the item is currently shown or @false if it was hidden
        using @ref Show(unsigned int,bool) "Show(n, false)".

        Note that this function returns @true for an item which hadn't been hidden
        even if the entire radiobox is not currently shown.

        This function is currently only implemented in wxMSW, wxGTK, wxQT and
        wxUniversal and always returns @true in the other ports.

        @param n
            The zero-based button position.
    */
    virtual bool IsItemShown(unsigned int n) const;

    /**
        Sets the helptext for an item. Empty string erases any existing helptext.

        @param item
            The zero-based item index.
        @param helptext
            The help text to set for the item.

        @see GetItemHelpText()
    */
    void SetItemHelpText(unsigned int item, const wxString& helptext);

    /**
        Sets the tooltip text for the specified item in the radio group.

        This function is currently only implemented in wxMSW and wxGTK2 and
        does nothing in the other ports.

        @param item
            Index of the item the tooltip will be shown for.
        @param text
            Tooltip text for the item, the tooltip is removed if empty.

        @see GetItemToolTip(), wxWindow::SetToolTip()
    */
    void SetItemToolTip(unsigned int item, const wxString& text);

    /**
        Sets the selection to the given item.

        Notice that a radio box always has selection, so @a n must be valid
        here and passing @c wxNOT_FOUND is not allowed.
     */
    virtual void SetSelection(int n);

    /**
        Shows or hides individual buttons.

        @param show
            @true to show, @false to hide.
        @param item
            The zero-based position of the button to show or hide.

        @return
            @true if the item has been shown or hidden or @false if nothing
            was done because it already was in the requested state.

        @see
            wxWindow::Show()

    */
    virtual bool Show(unsigned int item, bool show = true);


    // pure virtuals that have implementations here
    virtual unsigned int GetCount() const;
    virtual wxString GetString(unsigned int n) const;
    virtual void SetString(unsigned int n, const wxString& string);
    virtual int GetSelection() const;

};
