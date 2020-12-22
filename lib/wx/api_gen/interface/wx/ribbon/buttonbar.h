///////////////////////////////////////////////////////////////////////////////
// Name:        ribbon/buttonbar.h
// Purpose:     interface of wxRibbonButtonBar
// Author:      Peter Cawley
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    Flags for button bar button size and state.

    Buttons on a ribbon button bar can each come in three sizes: small, medium,
    and large. In some places this is called the size class, and the term size
    used for the pixel width and height associated with a particular size
    class.

    A button can also be in zero or more hovered or active states, or in the
    disabled state.
*/
enum wxRibbonButtonBarButtonState
{
    /**
        Button is small (the interpretation of small is dependent upon the art
        provider, but it will be smaller than medium).
    */
    wxRIBBON_BUTTONBAR_BUTTON_SMALL     = 0 << 0,

    /**
        Button is medium sized (the interpretation of medium is dependent upon
        the art provider, but it will be between small and large).
    */
    wxRIBBON_BUTTONBAR_BUTTON_MEDIUM    = 1 << 0,

    /**
        Button is large (the interpretation of large is dependent upon the art
        provider, but it will be larger than medium).
    */
    wxRIBBON_BUTTONBAR_BUTTON_LARGE     = 2 << 0,

    /**
        A mask to extract button size from a combination of flags.
    */
    wxRIBBON_BUTTONBAR_BUTTON_SIZE_MASK = 3 << 0,

    /**
        The normal (non-dropdown) region of the button is being hovered over by
        the mouse cursor. Only applicable to normal and hybrid buttons.
    */
    wxRIBBON_BUTTONBAR_BUTTON_NORMAL_HOVERED    = 1 << 3,

    /**
        The dropdown region of the button is being hovered over by the mouse
        cursor. Only applicable to dropdown and hybrid buttons.
    */
    wxRIBBON_BUTTONBAR_BUTTON_DROPDOWN_HOVERED  = 1 << 4,

    /**
        A mask to extract button hover state from a combination of flags.
    */
    wxRIBBON_BUTTONBAR_BUTTON_HOVER_MASK        = wxRIBBON_BUTTONBAR_BUTTON_NORMAL_HOVERED | wxRIBBON_BUTTONBAR_BUTTON_DROPDOWN_HOVERED,

    /**
        The normal (non-dropdown) region of the button is being pressed.
        Only applicable to normal and hybrid buttons.
    */
    wxRIBBON_BUTTONBAR_BUTTON_NORMAL_ACTIVE     = 1 << 5,

    /**
        The dropdown region of the button is being pressed.
        Only applicable to dropdown and hybrid buttons.
    */
    wxRIBBON_BUTTONBAR_BUTTON_DROPDOWN_ACTIVE   = 1 << 6,

    /**
       A mask to extract active flags
     */
    wxRIBBON_BUTTONBAR_BUTTON_ACTIVE_MASK       = wxRIBBON_BUTTONBAR_BUTTON_NORMAL_ACTIVE | wxRIBBON_BUTTONBAR_BUTTON_DROPDOWN_ACTIVE,

    /**
        The button is disabled. Hover flags may still be set when a button
        is disabled, but should be ignored during drawing if the button is
        disabled.
    */
    wxRIBBON_BUTTONBAR_BUTTON_DISABLED          = 1 << 7,

    /**
        The button is a toggle button which is currently in the toggled state.
    */
    wxRIBBON_BUTTONBAR_BUTTON_TOGGLED           = 1 << 8,

    /**
        A mask to extract button state from a combination of flags.
    */
    wxRIBBON_BUTTONBAR_BUTTON_STATE_MASK        = 0x1F8,
};

/**
    @class wxRibbonButtonBar

    A ribbon button bar is similar to a traditional toolbar. It contains one or
    more buttons (button bar buttons, not wxButtons), each of which has a label
    and an icon. It differs from a wxRibbonToolBar in several ways:
      @li Individual buttons can grow and contract.
      @li Buttons have labels as well as bitmaps.
      @li Bitmaps are typically larger (at least 32x32 pixels) on a button bar
        compared to a tool bar (which typically has 16x15).
      @li There is no grouping of buttons on a button bar
      @li A button bar typically has a border around each individual button,
        whereas a tool bar typically has a border around each group of buttons.

    @beginEventEmissionTable{wxRibbonButtonBarEvent}
    @event{EVT_RIBBONBUTTONBAR_CLICKED(id, func)}
        Triggered when the normal (non-dropdown) region of a button on the
        button bar is clicked.
    @event{EVT_RIBBONBUTTONBAR_DROPDOWN_CLICKED(id, func)}
        Triggered when the dropdown region of a button on the button bar is
        clicked. wxRibbonButtonBarEvent::PopupMenu() should be called by the
        event handler if it wants to display a popup menu (which is what most
        dropdown buttons should be doing).
    @endEventTable

    @library{wxribbon}
    @category{ribbon}
*/
class wxRibbonButtonBar : public wxRibbonControl
{
public:
    /**
        Default constructor.
        With this constructor, Create() should be called in order to create
        the button bar.
    */
    wxRibbonButtonBar();

    /**
        Construct a ribbon button bar with the given parameters.

        @param parent
            Parent window for the button bar (typically a wxRibbonPanel).
        @param id
            An identifier for the button bar. @c wxID_ANY is taken to mean a default.
        @param pos
            Initial position of the button bar.
        @param size
            Initial size of the button bar.
        @param style
            Button bar style, currently unused.
    */
    wxRibbonButtonBar(wxWindow* parent,
                  wxWindowID id = wxID_ANY,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = 0);

    /**
        Destructor.
    */
    virtual ~wxRibbonButtonBar();

    /**
        Create a button bar in two-step button bar construction.
        Should only be called when the default constructor is used, and
        arguments have the same meaning as in the full constructor.
    */
    bool Create(wxWindow* parent,
                wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0);

    /**
        Add a button to the button bar (simple version).
    */
    virtual wxRibbonButtonBarButtonBase* AddButton(
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxString& help_string,
                wxRibbonButtonKind kind = wxRIBBON_BUTTON_NORMAL);

    /**
        Add a dropdown button to the button bar (simple version).

        @see AddButton()
    */
    virtual wxRibbonButtonBarButtonBase* AddDropdownButton(
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Add a hybrid button to the button bar (simple version).

        @see AddButton()
    */
    virtual wxRibbonButtonBarButtonBase* AddHybridButton(
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Add a toggle button to the button bar (simple version).

        @see AddButton()
    */
    virtual wxRibbonButtonBarButtonBase* AddToggleButton(
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Add a button to the button bar.

        @param button_id
            ID of the new button (used for event callbacks).
        @param label
            Label of the new button.
        @param bitmap
            Large bitmap of the new button. Must be the same size as all other
            large bitmaps used on the button bar.
        @param bitmap_small
            Small bitmap of the new button. If left as null, then a small
            bitmap will be automatically generated. Must be the same size as
            all other small bitmaps used on the button bar.
        @param bitmap_disabled
            Large bitmap of the new button when it is disabled. If left as
            null, then a bitmap will be automatically generated from @a bitmap.
        @param bitmap_small_disabled
            Small bitmap of the new button when it is disabled. If left as
            null, then a bitmap will be automatically generated from @a
            bitmap_small.
        @param kind
            The kind of button to add.
        @param help_string
            The UI help string to associate with the new button.

        @return An opaque pointer which can be used only with other button bar
            methods.

        @see AddDropdownButton()
        @see AddHybridButton()
        @see AddToggleButton()
    */
    virtual wxRibbonButtonBarButtonBase* AddButton(
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxBitmap& bitmap_small = wxNullBitmap,
                const wxBitmap& bitmap_disabled = wxNullBitmap,
                const wxBitmap& bitmap_small_disabled = wxNullBitmap,
                wxRibbonButtonKind kind = wxRIBBON_BUTTON_NORMAL,
                const wxString& help_string = wxEmptyString);

    /**
        Inserts a button to the button bar (simple version) at the given position.

        @see AddButton()

        @since 2.9.4
    */
    virtual wxRibbonButtonBarButtonBase* InsertButton(
                size_t pos,
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxString& help_string,
                wxRibbonButtonKind kind = wxRIBBON_BUTTON_NORMAL);

    /**
        Inserts a dropdown button to the button bar (simple version) at the
            given position.

        @see InsertButton()
        @see AddDropdownButton()
        @see AddButton()

        @since 2.9.4
    */
    virtual wxRibbonButtonBarButtonBase* InsertDropdownButton(
                size_t pos,
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Inserts a hybrid button to the button bar (simple version) at the given
            position.

        @see InsertButton()
        @see AddHybridButton()
        @see AddButton()

        @since 2.9.4
    */
    virtual wxRibbonButtonBarButtonBase* InsertHybridButton(
                size_t pos,
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Inserts a toggle button to the button bar (simple version) at the given
            position.

        @see InsertButton()
        @see AddToggleButton()
        @see AddButton()

        @since 2.9.4
    */
    virtual wxRibbonButtonBarButtonBase* InsertToggleButton(
                size_t pos,
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Insert a button to the button bar at the given position.

        @param pos
            Position of the new button in the button bar.
        @param button_id
            ID of the new button (used for event callbacks).
        @param label
            Label of the new button.
        @param bitmap
            Large bitmap of the new button. Must be the same size as all other
            large bitmaps used on the button bar.
        @param bitmap_small
            Small bitmap of the new button. If left as null, then a small
            bitmap will be automatically generated. Must be the same size as
            all other small bitmaps used on the button bar.
        @param bitmap_disabled
            Large bitmap of the new button when it is disabled. If left as
            null, then a bitmap will be automatically generated from @a bitmap.
        @param bitmap_small_disabled
            Small bitmap of the new button when it is disabled. If left as
            null, then a bitmap will be automatically generated from @a
            bitmap_small.
        @param kind
            The kind of button to add.
        @param help_string
            The UI help string to associate with the new button.

        @return An opaque pointer which can be used only with other button bar
            methods.

        @see InsertDropdownButton()
        @see InsertHybridButton()
        @see InsertToggleButton()
        @see AddButton()

        @since 2.9.4
    */
    virtual wxRibbonButtonBarButtonBase* InsertButton(
                size_t pos,
                int button_id,
                const wxString& label,
                const wxBitmap& bitmap,
                const wxBitmap& bitmap_small = wxNullBitmap,
                const wxBitmap& bitmap_disabled = wxNullBitmap,
                const wxBitmap& bitmap_small_disabled = wxNullBitmap,
                wxRibbonButtonKind kind = wxRIBBON_BUTTON_NORMAL,
                const wxString& help_string = wxEmptyString);

    /**
        Returns the number of buttons in this button bar.

        @since 2.9.4
    */
    virtual size_t GetButtonCount() const;

    /**
        Set the client object associated with a button. The button bar
        owns the given object and takes care of its deletion.
        Please, note that you cannot use both client object and client data.

        @since 2.9.5
    */
    void SetItemClientObject(wxRibbonButtonBarButtonBase* item, wxClientData* data);

    /**
        Get the client object associated with a button.

        @since 2.9.5
    */
    wxClientData* GetItemClientObject(const wxRibbonButtonBarButtonBase* item) const;

    /**
        Set the client data associated with a button.
        Please, note that you cannot use both client object and client data.

        @since 2.9.5
    */
    void SetItemClientData(wxRibbonButtonBarButtonBase* item, void* data);

    /**
        Get the client data associated with a button.

        @since 2.9.5
    */
    void* GetItemClientData(const wxRibbonButtonBarButtonBase* item) const;

    /**
        Returns the N-th button of the bar.

        @see GetButtonCount()

        @since 2.9.5
    */
    virtual wxRibbonButtonBarButtonBase *GetItem(size_t n) const;

    /**
        Returns the first button having a given id or NULL if none matches.

        @since 2.9.5
    */
    virtual wxRibbonButtonBarButtonBase *GetItemById(int id) const;

    /**
        Returns the id of a button.

        @since 2.9.5
    */
    virtual int GetItemId(wxRibbonButtonBarButtonBase *item) const;

    /**
        Calculate button layouts and positions.

        Must be called after buttons are added to the button bar, as otherwise
        the newly added buttons will not be displayed. In normal situations, it
        will be called automatically when wxRibbonBar::Realize() is called.
    */
    virtual bool Realize();

    /**
        Delete all buttons from the button bar.

        @see DeleteButton()
    */
    virtual void ClearButtons();

    /**
        Delete a single button from the button bar.

        The corresponding button is deleted by this function, so any pointers to
        it previously obtained by GetItem() or GetItemById() become invalid.

        @see ClearButtons()
    */
    virtual bool DeleteButton(int button_id);

    /**
        Enable or disable a single button on the bar.

        @param button_id
            ID of the button to enable or disable.
        @param enable
            @true to enable the button, @false to disable it.
    */
    virtual void EnableButton(int button_id, bool enable = true);

    /**
        Set a toggle button to the checked or unchecked state.

        @param button_id
            ID of the toggle button to manipulate.
        @param checked
            @true to set the button to the toggled/pressed/checked state,
            @false to set it to the untoggled/unpressed/unchecked state.
    */
    virtual void ToggleButton(int button_id, bool checked);

    /**
        Changes the bitmap of an existing button.

        @param button_id
            ID of the button to manipulate.
        @param bitmap
            Large bitmap of the new button. Must be the same size as all other
            large bitmaps used on the button bar.
        @param bitmap_small
            Small bitmap of the new button. If left as null, then a small
            bitmap will be automatically generated. Must be the same size as
            all other small bitmaps used on the button bar.
        @param bitmap_disabled
            Large bitmap of the new button when it is disabled. If left as
            null, then a bitmap will be automatically generated from @a bitmap.
        @param bitmap_small_disabled
            Small bitmap of the new button when it is disabled. If left as
            null, then a bitmap will be automatically generated from @a
            bitmap_small.

        @since 3.1.2
    */
    virtual void SetButtonIcon(
                int button_id,
                const wxBitmap& bitmap,
                const wxBitmap& bitmap_small = wxNullBitmap,
                const wxBitmap& bitmap_disabled = wxNullBitmap,
                const wxBitmap& bitmap_small_disabled = wxNullBitmap);

    /**
        Changes the label text of an existing button.

        @param button_id
            ID of the button to manipulate.
        @param label
            New label of the button.

        @remarks
            If text size has changed, Realize() must be called
            on the top level wxRibbonBar object to recalculate panel sizes.
            Use SetButtonTextMinWidth() to avoid calling Realize()
            after every change.

        @see SetButtonTextMinWidth

        @since 3.1.2
    */
    virtual void SetButtonText(int button_id, const wxString& label);

    /**
        Sets the minimum width of the button label, to indicate to
        the wxRibbonArtProvider layout mechanism that this is the
        minimum required size.

        You have to call Realize() after calling this function to
        apply the given minimum width.

        @param button_id
            ID of the button to manipulate.
        @param min_width_medium
            Requested minimum width of the button text in pixel
            if the button is medium size.
        @param min_width_large
            Requested minimum width of the button text in pixel
            if the button is large size.

        @remarks
            This function is used together with SetButtonText() to change
            button labels on the fly without modifying the button bar layout.

        @see SetButtonText()

        @since 3.1.2
    */
    virtual void SetButtonTextMinWidth(int button_id,
                int min_width_medium, int min_width_large);

    /**
        Sets the minimum width of the button label, to indicate to
        the wxRibbonArtProvider layout mechanism that this is the
        minimum required size.

        You have to call Realize() after calling this function to
        apply the given minimum width.

        @param button_id
            ID of the button to manipulate.
        @param label
            The minimum width is set to the width of this label.

        @remarks
            This function is used together with SetButtonText() to change
            button labels on the fly without modifying the button bar layout.

        @see SetButtonText()

        @since 3.1.2
    */
    virtual void SetButtonTextMinWidth(int button_id, const wxString& label);

    /**
        Sets the minimum size class of a ribbon button.

        You have to call Realize() after calling this function to
        apply the given minimum size.

        @param button_id
            ID of the button to manipulate.
        @param min_size_class
            The minimum size-class of the button. Buttons on a button bar
            can have three distinct sizes: wxRIBBON_BUTTONBAR_BUTTON_SMALL,
            wxRIBBON_BUTTONBAR_BUTTON_MEDIUM, and wxRIBBON_BUTTONBAR_BUTTON_LARGE.

        @since 3.1.2
    */
    virtual void SetButtonMinSizeClass(int button_id,
                wxRibbonButtonBarButtonState min_size_class);

    /**
        Sets the maximum size class of a ribbon button.

        You have to call Realize() after calling this function to
        apply the given maximum size.

        @param button_id
            ID of the button to manipulate.
        @param max_size_class
            The maximum size-class of the button. Buttons on a button bar
            can have three distinct sizes: wxRIBBON_BUTTONBAR_BUTTON_SMALL,
            wxRIBBON_BUTTONBAR_BUTTON_MEDIUM, and wxRIBBON_BUTTONBAR_BUTTON_LARGE.

        @since 3.1.2
    */
    virtual void SetButtonMaxSizeClass(int button_id,
                wxRibbonButtonBarButtonState max_size_class);

    /**
        Returns the active item of the button bar or NULL if there is none.
        The active button is the one being clicked.

        @since 2.9.5
    */
    virtual wxRibbonButtonBarButtonBase *GetActiveItem() const;

    /**
        Returns the hovered item of the button bar or NULL if there is none.
        The hovered button is the one the mouse is over.

        @since 2.9.5
    */
    virtual wxRibbonButtonBarButtonBase *GetHoveredItem() const;

    /**
        Indicates whether tooltips are shown for disabled buttons.

        By default they are not shown.

        @since 2.9.5
    */
    void SetShowToolTipsForDisabled(bool show);

    /**
        Sets whether tooltips should be shown for disabled buttons or not.

        You may wish to show it to explain why a button is disabled or
        what it normally does when enabled.

        @since 2.9.5
    */
    bool GetShowToolTipsForDisabled() const;

};

/**
    @class wxRibbonButtonBarEvent

    Event used to indicate various actions relating to a button on a
    wxRibbonButtonBar. For toggle buttons, IsChecked() can be used to test
    the state of the button.

    See wxRibbonButtonBar for available event types.

    @library{wxribbon}
    @category{events,ribbon}

    @see wxRibbonBar
*/
class wxRibbonButtonBarEvent : public wxCommandEvent
{
public:
    /**
        Constructor.
    */
    wxRibbonButtonBarEvent(wxEventType command_type = wxEVT_NULL,
                       int win_id = 0,
                       wxRibbonButtonBar* bar = NULL,
                       wxRibbonButtonBarButtonBase* button = NULL);

    /**
        Returns the bar which contains the button which the event relates to.
    */
    wxRibbonButtonBar* GetBar();

    /**
        Sets the button bar relating to this event.
    */
    void SetBar(wxRibbonButtonBar* bar);

    /**
        Returns the button which the event relates to.

        @since 2.9.5
    */
    wxRibbonButtonBarButtonBase* GetButton();

    /**
        Sets the button relating to this event.

        @since 2.9.5
    */
    void SetButton(wxRibbonButtonBarButtonBase* bar);

    /**
        Display a popup menu as a result of this (dropdown clicked) event.
    */
    bool PopupMenu(wxMenu* menu);
};


wxEventType wxEVT_RIBBONBUTTONBAR_CLICKED;
wxEventType wxEVT_RIBBONBUTTONBAR_DROPDOWN_CLICKED;
