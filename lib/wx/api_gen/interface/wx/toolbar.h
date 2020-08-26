/////////////////////////////////////////////////////////////////////////////
// Name:        toolbar.h
// Purpose:     interface of wxToolBar
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

enum wxToolBarToolStyle
{
    wxTOOL_STYLE_BUTTON    = 1,
    wxTOOL_STYLE_SEPARATOR = 2,
    wxTOOL_STYLE_CONTROL
};


/** wxToolBar style flags */
enum
{
    /** lay out the toolbar horizontally */
    wxTB_HORIZONTAL  = wxHORIZONTAL,
    wxTB_TOP         = wxTB_HORIZONTAL,

    /** lay out the toolbar vertically */
    wxTB_VERTICAL    = wxVERTICAL,
    wxTB_LEFT        = wxTB_VERTICAL,

    /** "flat" buttons (Win32/GTK only) */
    wxTB_FLAT,

    /** dockable toolbar (GTK only) */
    wxTB_DOCKABLE,

    /** don't show the icons (they're shown by default) */
    wxTB_NOICONS,

    /** show the text (not shown by default) */
    wxTB_TEXT,

    /** don't show the divider between toolbar and the window (Win32 only) */
    wxTB_NODIVIDER,

    /** no automatic alignment (Win32 only, useless) */
    wxTB_NOALIGN,

    /** show the text and the icons alongside, not vertically stacked (Win32/GTK) */
    wxTB_HORZ_LAYOUT,
    wxTB_HORZ_TEXT   = wxTB_HORZ_LAYOUT | wxTB_TEXT,

    /** don't show the toolbar short help tooltips */
    wxTB_NO_TOOLTIPS,

    /** lay out toolbar at the bottom of the window */
    wxTB_BOTTOM,

    /** lay out toolbar at the right edge of the window */
    wxTB_RIGHT,

    /** flags that are closest to the native look*/
    wxTB_DEFAULT_STYLE = wxTB_HORIZONTAL
};



/**
    @class wxToolBarToolBase

    A toolbar tool represents one item on the toolbar.

    It has a unique id (except for the separators), the style (telling whether
    it is a normal button, separator or a control), the state (toggled or not,
    enabled or not) and short and long help strings. The default
    implementations use the short help string for the tooltip text which is
    popped up when the mouse pointer enters the tool and the long help string
    for the applications status bar.

    Notice that the toolbar can @e not be modified by changing its tools via
    the (intentionally undocumented here) setter methods of this class, all the
    modifications must be done using the methods of wxToolBar itself.
*/
class wxToolBarToolBase : public wxObject
{
public:
    wxToolBarToolBase(wxToolBarBase *tbar = NULL,
                      int toolid = wxID_SEPARATOR,
                      const wxString& label = wxEmptyString,
                      const wxBitmap& bmpNormal = wxNullBitmap,
                      const wxBitmap& bmpDisabled = wxNullBitmap,
                      wxItemKind kind = wxITEM_NORMAL,
                      wxObject *clientData = NULL,
                      const wxString& shortHelpString = wxEmptyString,
                      const wxString& longHelpString = wxEmptyString);

    wxToolBarToolBase(wxToolBarBase *tbar,
                      wxControl *control,
                      const wxString& label);

    int GetId() const;

    wxControl *GetControl() const;
    wxToolBarBase *GetToolBar() const;

    bool IsStretchable() const;
    bool IsButton() const;
    bool IsControl() const;
    bool IsSeparator() const;
    bool IsStretchableSpace() const;
    int GetStyle() const;
    wxItemKind GetKind() const;
    void MakeStretchable();

    bool IsEnabled() const;
    bool IsToggled() const;
    bool CanBeToggled() const;

    const wxBitmap& GetNormalBitmap() const;
    const wxBitmap& GetDisabledBitmap() const;

    const wxBitmap& GetBitmap() const;
    const wxString& GetLabel() const;

    const wxString& GetShortHelp() const;
    const wxString& GetLongHelp() const;

    wxObject *GetClientData() const;

    bool Enable(bool enable);
    bool Toggle(bool toggle);
    bool SetToggle(bool toggle);
    bool SetShortHelp(const wxString& help);
    bool SetLongHelp(const wxString& help);
    void Toggle();
    void SetNormalBitmap(const wxBitmap& bmp);
    void SetDisabledBitmap(const wxBitmap& bmp);
    void SetLabel(const wxString& label);
    void SetClientData(wxObject *clientData);

    void Detach();
    void Attach(wxToolBarBase *tbar);

    void SetDropdownMenu(wxMenu *menu);
    wxMenu *GetDropdownMenu() const;
};




/**
    @class wxToolBar

    A toolbar is a bar of buttons and/or other controls usually placed below
    the menu bar in a wxFrame.

    You may create a toolbar that is managed by a frame calling
    wxFrame::CreateToolBar(). Under Pocket PC, you should always use this
    function for creating the toolbar to be managed by the frame, so that
    wxWidgets can use a combined menubar and toolbar. Where you manage your
    own toolbars, create wxToolBar as usual.

    There are several different types of tools you can add to a toolbar.
    These types are controlled by the ::wxItemKind enumeration.

    Note that many methods in wxToolBar such as wxToolBar::AddTool return a
    @c wxToolBarToolBase* object.
    This should be regarded as an opaque handle representing the newly added
    toolbar item, providing access to its id and position within the toolbar.
    Changes to the item's state should be made through calls to wxToolBar methods,
    for example wxToolBar::EnableTool.
    Calls to @c wxToolBarToolBase methods (undocumented by purpose) will not change
    the visible state of the item within the tool bar.

    After you have added all the tools you need, you must call Realize() to
    effectively construct and display the toolbar.

    <b>wxMSW note</b>: Note that under wxMSW toolbar paints tools to reflect
    system-wide colours. If you use more than 16 colours in your tool bitmaps,
    you may wish to suppress this behaviour, otherwise system colours in your
    bitmaps will inadvertently be mapped to system colours.
    To do this, set the msw.remap system option before creating the toolbar:
    @code
    wxSystemOptions::SetOption("msw.remap", 0);
    @endcode
    If you wish to use 32-bit images (which include an alpha channel for
    transparency) use:
    @code
    wxSystemOptions::SetOption("msw.remap", 2);
    @endcode
    Then colour remapping is switched off, and a transparent background
    used. But only use this option under Windows XP with true colour:
    @code
    if (wxTheApp->GetComCtl32Version() >= 600 && ::wxDisplayDepth() >= 32)
    @endcode

    @beginStyleTable
    @style{wxTB_FLAT}
        Gives the toolbar a flat look (Windows and GTK only).
    @style{wxTB_DOCKABLE}
        Makes the toolbar floatable and dockable (GTK only).
    @style{wxTB_HORIZONTAL}
        Specifies horizontal layout (default).
    @style{wxTB_VERTICAL}
        Specifies vertical layout.
    @style{wxTB_TEXT}
        Shows the text in the toolbar buttons; by default only icons are shown.
    @style{wxTB_NOICONS}
        Specifies no icons in the toolbar buttons; by default they are shown.
    @style{wxTB_NODIVIDER}
        Specifies no divider (border) above the toolbar (Windows only)
    @style{wxTB_NOALIGN}
        Specifies no alignment with the parent window (Windows only, not very
        useful).
    @style{wxTB_HORZ_LAYOUT}
        Shows the text and the icons alongside, not vertically stacked (Windows
        and GTK 2 only). This style must be used with @c wxTB_TEXT.
    @style{wxTB_HORZ_TEXT}
        Combination of @c wxTB_HORZ_LAYOUT and @c wxTB_TEXT.
    @style{wxTB_NO_TOOLTIPS}
        Don't show the short help tooltips for the tools when the mouse hovers
        over them.
    @style{wxTB_BOTTOM}
        Align the toolbar at the bottom of parent window.
    @style{wxTB_RIGHT}
        Align the toolbar at the right side of parent window.
    @style{wxTB_DEFAULT_STYLE}
        Combination of @c wxTB_HORIZONTAL and @c wxTB_FLAT. This style is new
        since wxWidgets 2.9.5.
    @endStyleTable

    See also @ref overview_windowstyles. Note that the wxMSW native toolbar
    ignores @c wxTB_NOICONS style. Also, toggling the @c wxTB_TEXT works only
    if the style was initially on.

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_TOOL(id, func)}
        Process a @c wxEVT_TOOL event (a synonym for @c
        wxEVT_MENU). Pass the id of the tool.
    @event{EVT_MENU(id, func)}
        The same as EVT_TOOL().
    @event{EVT_TOOL_RANGE(id1, id2, func)}
        Process a @c wxEVT_TOOL event for a range of
        identifiers. Pass the ids of the tools.
    @event{EVT_MENU_RANGE(id1, id2, func)}
        The same as EVT_TOOL_RANGE().
    @event{EVT_TOOL_RCLICKED(id, func)}
        Process a @c wxEVT_TOOL_RCLICKED event. Pass the id of the
        tool.  (Not available on wxOSX.)
    @event{EVT_TOOL_RCLICKED_RANGE(id1, id2, func)}
        Process a @c wxEVT_TOOL_RCLICKED event for a range of ids. Pass
        the ids of the tools.  (Not available on wxOSX.)
    @event{EVT_TOOL_ENTER(id, func)}
        Process a @c wxEVT_TOOL_ENTER event. Pass the id of the toolbar
        itself. The value of wxCommandEvent::GetSelection() is the tool id, or
        -1 if the mouse cursor has moved off a tool.  (Not available on wxOSX.)
    @event{EVT_TOOL_DROPDOWN(id, func)}
        Process a @c wxEVT_TOOL_DROPDOWN event. If unhandled,
        displays the default dropdown menu set using
        wxToolBar::SetDropdownMenu().
    @endEventTable

    The toolbar class emits menu commands in the same way that a frame menubar
    does, so you can use one EVT_MENU() macro for both a menu item and a toolbar
    button. The event handler functions take a wxCommandEvent argument. For most
    event macros, the identifier of the tool is passed, but for EVT_TOOL_ENTER()
    the toolbar window identifier is passed and the tool identifier is retrieved
    from the wxCommandEvent. This is because the identifier may be @c wxID_ANY when the
    mouse moves off a tool, and @c wxID_ANY is not allowed as an identifier in the event
    system.

    @library{wxcore}
    @category{miscwnd}

    @see @ref overview_toolbar
*/
class wxToolBar : public wxControl
{
public:
    /**
        Default constructor.
    */
    wxToolBar();

    /**
        Constructs a toolbar.

        @param parent
            Pointer to a parent window.
        @param id
            Window identifier. If -1, will automatically create an identifier.
        @param pos
            Window position. ::wxDefaultPosition indicates that wxWidgets should
            generate a default position for the window.
            If using the wxWindow class directly, supply an actual position.
        @param size
            Window size. ::wxDefaultSize indicates that wxWidgets should generate
            a default size for the window.
        @param style
            Window style. See wxToolBar initial description for details.
        @param name
            Window name.

        @remarks After a toolbar is created, you use AddTool() and perhaps
                 AddSeparator(), and then you must call Realize() to construct
                 and display the toolbar tools.
    */
    wxToolBar(wxWindow* parent, wxWindowID id,
              const wxPoint& pos = wxDefaultPosition,
              const wxSize& size = wxDefaultSize,
              long style = wxTB_HORIZONTAL,
              const wxString& name = wxToolBarNameStr);

    /**
        Toolbar destructor.
    */
    virtual ~wxToolBar();

    /**
        Adds a new check (or toggle) tool to the toolbar. The parameters are the
        same as in AddTool().

        @see AddTool()
    */
    wxToolBarToolBase* AddCheckTool(int toolId, const wxString& label,
                                    const wxBitmap& bitmap1,
                                    const wxBitmap& bmpDisabled = wxNullBitmap,
                                    const wxString& shortHelp = wxEmptyString,
                                    const wxString& longHelp = wxEmptyString,
                                    wxObject* clientData = NULL);

    /**
        Adds any control to the toolbar, typically e.g.\ a wxComboBox.

        @param control
            The control to be added.
        @param label
            Text to be displayed near the control.

        @remarks
            wxMac: labels are only displayed if wxWidgets is built with @c
            wxMAC_USE_NATIVE_TOOLBAR set to 1
    */
    virtual wxToolBarToolBase* AddControl(wxControl* control,
                                          const wxString& label = wxEmptyString);

    /**
        Adds a new radio tool to the toolbar. Consecutive radio tools form a
        radio group such that exactly one button in the group is pressed at any
        moment, in other words whenever a button in the group is pressed the
        previously pressed button is automatically released. You should avoid
        having the radio groups of only one element as it would be impossible
        for the user to use such button.

        By default, the first button in the radio group is initially pressed,
        the others are not.


        @see AddTool()
    */
    wxToolBarToolBase* AddRadioTool(int toolId, const wxString& label,
                                    const wxBitmap& bitmap1,
                                    const wxBitmap& bmpDisabled = wxNullBitmap,
                                    const wxString& shortHelp = wxEmptyString,
                                    const wxString& longHelp = wxEmptyString,
                                    wxObject* clientData = NULL);

    /**
        Adds a separator for spacing groups of tools.

        Notice that the separator uses the look appropriate for the current
        platform so it can be a vertical line (MSW, some versions of GTK) or
        just an empty space or something else.

        @see AddTool(), SetToolSeparation(), AddStretchableSpace()
    */
    virtual wxToolBarToolBase* AddSeparator();

    /**
        Adds a stretchable space to the toolbar.

        Any space not taken up by the fixed items (all items except for
        stretchable spaces) is distributed in equal measure between the
        stretchable spaces in the toolbar. The most common use for this method
        is to add a single stretchable space before the items which should be
        right-aligned in the toolbar, but more exotic possibilities are
        possible, e.g. a stretchable space may be added in the beginning and
        the end of the toolbar to centre all toolbar items.

        @see AddTool(), AddSeparator(), InsertStretchableSpace()

        @since 2.9.1
     */
    wxToolBarToolBase *AddStretchableSpace();

    //@{
    /**
        Adds a tool to the toolbar.

        @param tool
            The tool to be added.

        @remarks After you have added tools to a toolbar, you must call
                 Realize() in order to have the tools appear.

        @see AddSeparator(), AddCheckTool(), AddRadioTool(),
             InsertTool(), DeleteTool(), Realize(), SetDropdownMenu()
    */
    virtual wxToolBarToolBase* AddTool(wxToolBarToolBase* tool);

    /**
        Adds a tool to the toolbar. This most commonly used version has fewer
        parameters than the full version below which specifies the more rarely
        used button features.

        @param toolId
            An integer by which the tool may be identified in subsequent
            operations.
        @param label
            The string to be displayed with the tool. This string may include
            mnemonics, i.e. characters prefixed by an ampersand ("&"), but they
            are stripped from it and not actually shown in the toolbar as tools
            can't be activated from keyboard.
        @param bitmap
            The primary tool bitmap.
        @param shortHelp
            This string is used for the tools tooltip.
        @param kind
            May be ::wxITEM_NORMAL for a normal button (default), ::wxITEM_CHECK
            for a checkable tool (such tool stays pressed after it had been
            toggled) or ::wxITEM_RADIO for a checkable tool which makes part of
            a radio group of tools each of which is automatically unchecked
            whenever another button in the group is checked. ::wxITEM_DROPDOWN
            specifies that a drop-down menu button will appear next to the
            tool button (only GTK+ and MSW). Call SetDropdownMenu() afterwards.

        @remarks After you have added tools to a toolbar, you must call
            Realize() in order to have the tools appear.

        @see AddSeparator(), AddCheckTool(), AddRadioTool(),
             InsertTool(), DeleteTool(), Realize(), SetDropdownMenu()
    */
    wxToolBarToolBase* AddTool(int toolId, const wxString& label,
                               const wxBitmap& bitmap,
                               const wxString& shortHelp = wxEmptyString,
                               wxItemKind kind = wxITEM_NORMAL);

    /**
        Adds a tool to the toolbar.

        @param toolId
            An integer by which the tool may be identified in subsequent
            operations.
        @param label
            The string to be displayed with the tool.
        @param bitmap
            The primary tool bitmap.
        @param bmpDisabled
            The bitmap used when the tool is disabled. If it is equal to
            ::wxNullBitmap (default), the disabled bitmap is automatically
            generated by greying the normal one.
        @param kind
            May be ::wxITEM_NORMAL for a normal button (default), ::wxITEM_CHECK
            for a checkable tool (such tool stays pressed after it had been
            toggled) or ::wxITEM_RADIO for a checkable tool which makes part of
            a radio group of tools each of which is automatically unchecked
            whenever another button in the group is checked. ::wxITEM_DROPDOWN
            specifies that a drop-down menu button will appear next to the
            tool button (only GTK+ and MSW). Call SetDropdownMenu() afterwards.
        @param shortHelpString
            This string is used for the tools tooltip.
        @param longHelpString
            This string is shown in the statusbar (if any) of the parent frame
            when the mouse pointer is inside the tool.
        @param clientData
            An optional pointer to client data which can be retrieved later
            using GetToolClientData().

        @remarks After you have added tools to a toolbar, you must call
            Realize() in order to have the tools appear.

        @see AddSeparator(), AddCheckTool(), AddRadioTool(),
             InsertTool(), DeleteTool(), Realize(), SetDropdownMenu()
    */
    wxToolBarToolBase* AddTool(int toolId, const wxString& label,
                               const wxBitmap& bitmap,
                               const wxBitmap& bmpDisabled,
                               wxItemKind kind = wxITEM_NORMAL,
                               const wxString& shortHelpString = wxEmptyString,
                               const wxString& longHelpString = wxEmptyString,
                               wxObject* clientData = NULL);
    //@}

    /**
        Deletes all the tools in the toolbar.
    */
    virtual void ClearTools();

    /**
        Removes the specified tool from the toolbar and deletes it. If you don't
        want to delete the tool, but just to remove it from the toolbar (to
        possibly add it back later), you may use RemoveTool() instead.

        @note It is unnecessary to call Realize() for the change to take
            place, it will happen immediately.

        @returns @true if the tool was deleted, @false otherwise.

        @see DeleteToolByPos()
    */
    virtual bool DeleteTool(int toolId);

    /**
        This function behaves like DeleteTool() but it deletes the tool at the
        specified position and not the one with the given id.
    */
    virtual bool DeleteToolByPos(size_t pos);

    /**
        Enables or disables the tool.

        @param toolId
            ID of the tool to enable or disable, as passed to AddTool().
        @param enable
            If @true, enables the tool, otherwise disables it.

        @remarks Some implementations will change the visible state of the tool
            to indicate that it is disabled.


        @see GetToolEnabled(), ToggleTool()
    */
    virtual void EnableTool(int toolId, bool enable);

    /**
        Returns a pointer to the tool identified by @a id or @NULL if no
        corresponding tool is found.
    */
    wxToolBarToolBase* FindById(int id) const;

    /**
        Returns a pointer to the control identified by @a id or @NULL if no
        corresponding control is found.
    */
    virtual wxControl* FindControl(int id);

    /**
        Finds a tool for the given mouse position.

        @param x
            X position.
        @param y
            Y position.

        @return A pointer to a tool if a tool is found, or @NULL otherwise.

        @remarks Currently not implemented in wxGTK (always returns @NULL
        there).
    */
    virtual wxToolBarToolBase* FindToolForPosition(wxCoord x, wxCoord y) const;

    /**
        Returns the left/right and top/bottom margins, which are also used for
        inter-toolspacing.

        @see SetMargins()
    */
    wxSize GetMargins() const;

    /**
        Returns the size of bitmap that the toolbar expects to have.

        The default bitmap size is platform-dependent: for example, it is 16*15
        for MSW and 24*24 for GTK. This size does @em not necessarily indicate
        the best size to use for the toolbars on the given platform, for this
        you should use @c wxArtProvider::GetNativeSizeHint(wxART_TOOLBAR) but
        in any case, as the bitmap size is deduced automatically from the size
        of the bitmaps associated with the tools added to the toolbar, it is
        usually unnecessary to call SetToolBitmapSize() explicitly.

        @remarks Note that this is the size of the bitmap you pass to AddTool(),
            and not the eventual size of the tool button.

        @see SetToolBitmapSize(), GetToolSize()
    */
    virtual wxSize GetToolBitmapSize() const;

    /**
        Returns a pointer to the tool at ordinal position @a pos.

        Don't confuse this with FindToolForPosition().

        @since 2.9.1

        @see GetToolsCount()
    */
    wxToolBarToolBase *GetToolByPos(int pos);

    const wxToolBarToolBase *GetToolByPos(int pos) const;

    /**
        Get any client data associated with the tool.

        @param toolId
            ID of the tool in question, as passed to AddTool().

        @return Client data, or @NULL if there is none.
    */
    virtual wxObject* GetToolClientData(int toolId) const;

    /**
        Called to determine whether a tool is enabled (responds to user input).

        @param toolId
            ID of the tool in question, as passed to AddTool().

        @return @true if the tool is enabled, @false otherwise.

        @see EnableTool()
    */
    virtual bool GetToolEnabled(int toolId) const;

    /**
        Returns the long help for the given tool.

        @param toolId
            ID of the tool in question, as passed to AddTool().

        @see SetToolLongHelp(), SetToolShortHelp()
    */
    virtual wxString GetToolLongHelp(int toolId) const;

    /**
        Returns the value used for packing tools.

        @see SetToolPacking()
    */
    virtual int GetToolPacking() const;

    /**
        Returns the tool position in the toolbar, or @c wxNOT_FOUND if the tool
        is not found.

        @param toolId
            ID of the tool in question, as passed to AddTool().
    */
    virtual int GetToolPos(int toolId) const;

    /**
        Returns the default separator size.

        @see SetToolSeparation()
    */
    virtual int GetToolSeparation() const;

    /**
        Returns the short help for the given tool.

        @param toolId
            ID of the tool in question, as passed to AddTool().

        @see GetToolLongHelp(), SetToolShortHelp()
    */
    virtual wxString GetToolShortHelp(int toolId) const;

    /**
        Returns the size of a whole button, which is usually larger than a tool
        bitmap because of added 3D effects.

        @see SetToolBitmapSize(), GetToolBitmapSize()
    */
    virtual wxSize GetToolSize() const;

    /**
        Gets the on/off state of a toggle tool.

        @param toolId
            ID of the tool in question, as passed to AddTool().

        @return @true if the tool is toggled on, @false otherwise.

        @see ToggleTool()
    */
    virtual bool GetToolState(int toolId) const;

    /**
        Returns the number of tools in the toolbar.
    */
    size_t GetToolsCount() const;

    /**
        Inserts the control into the toolbar at the given position. You must
        call Realize() for the change to take place.

        @see AddControl(), InsertTool()
    */
    virtual wxToolBarToolBase* InsertControl(size_t pos, wxControl* control,
                                             const wxString& label = wxEmptyString);

    /**
        Inserts the separator into the toolbar at the given position. You must
        call Realize() for the change to take place.

        @see AddSeparator(), InsertTool()
    */
    virtual wxToolBarToolBase* InsertSeparator(size_t pos);

    /**
        Inserts a stretchable space at the given position.

        See AddStretchableSpace() for details about stretchable spaces.

        @see InsertTool(), InsertSeparator()

        @since 2.9.1
     */
    wxToolBarToolBase *InsertStretchableSpace(size_t pos);

    //@{
    /**
        Inserts the tool with the specified attributes into the toolbar at the
        given position.

        You must call Realize() for the change to take place.

        @see AddTool(), InsertControl(), InsertSeparator()

        @return The newly inserted tool or @NULL on failure. Notice that with
            the overload taking @a tool parameter the caller is responsible for
            deleting the tool in the latter case.
    */
    wxToolBarToolBase* InsertTool(  size_t pos,
                                    int toolId,
                                    const wxString& label,
                                    const wxBitmap& bitmap,
                                    const wxBitmap& bmpDisabled = wxNullBitmap,
                                    wxItemKind kind = wxITEM_NORMAL,
                                    const wxString& shortHelp = wxEmptyString,
                                    const wxString& longHelp = wxEmptyString,
                                    wxObject *clientData = NULL);

    wxToolBarToolBase* InsertTool(size_t pos,
                                  wxToolBarToolBase* tool);
    //@}

    /**
        Called when the user clicks on a tool with the left mouse button. This
        is the old way of detecting tool clicks; although it will still work,
        you should use the EVT_MENU() or EVT_TOOL() macro instead.

        @param toolId
            The identifier passed to AddTool().
        @param toggleDown
            @true if the tool is a toggle and the toggle is down, otherwise is
            @false.

        @return If the tool is a toggle and this function returns @false, the
                toggle state (internal and visual) will not be changed. This
                provides a way of specifying that toggle operations are not
                permitted in some circumstances.

        @see OnMouseEnter(), OnRightClick()
    */
    virtual bool OnLeftClick(int toolId, bool toggleDown);

    /**
        This is called when the mouse cursor moves into a tool or out of the
        toolbar. This is the old way of detecting mouse enter events;
        although it will still work, you should use the EVT_TOOL_ENTER()
        macro instead.

        @param toolId
            Greater than -1 if the mouse cursor has moved into the tool, or -1
            if the mouse cursor has moved. The programmer can override this to
            provide extra information about the tool, such as a short
            description on the status line.

        @remarks With some derived toolbar classes, if the mouse moves quickly
                 out of the toolbar, wxWidgets may not be able to detect it.
                 Therefore this function may not always be called when expected.
    */
    virtual void OnMouseEnter(int toolId);

    /**
        @deprecated This is the old way of detecting tool right clicks;
                    although it will still work, you should use the
                    EVT_TOOL_RCLICKED() macro instead.

        Called when the user clicks on a tool with the right mouse button. The
        programmer should override this function to detect right tool clicks.

        @param toolId
            The identifier passed to AddTool().
        @param x
            The x position of the mouse cursor.
        @param y
            The y position of the mouse cursor.

        @remarks A typical use of this member might be to pop up a menu.

        @see OnMouseEnter(), OnLeftClick()
    */
    virtual void OnRightClick(int toolId, long x, long y);

    /**
        This function should be called after you have added tools.
    */
    virtual bool Realize();

    /**
        Removes the given tool from the toolbar but doesn't delete it. This
        allows inserting/adding this tool back to this (or another) toolbar later.

        @note It is unnecessary to call Realize() for the change to take place,
            it will happen immediately.


        @see DeleteTool()
    */
    virtual wxToolBarToolBase* RemoveTool(int id);

    /**
        Sets the dropdown menu for the tool given by its @e id. The tool itself
        will delete the menu when it's no longer needed. Only supported under
        GTK+ und MSW.

        If you define a EVT_TOOL_DROPDOWN() handler in your program, you must
        call wxEvent::Skip() from it or the menu won't be displayed.
    */
    bool SetDropdownMenu(int id, wxMenu* menu);

    //@{
    /**
        Set the values to be used as margins for the toolbar.

        @param x
            Left margin, right margin and inter-tool separation value.
        @param y
            Top margin, bottom margin and inter-tool separation value.

        @remarks This must be called before the tools are added if absolute
            positioning is to be used, and the default (zero-size) margins are
            to be overridden.

        @see GetMargins()
    */
    virtual void SetMargins(int x, int y);

    /**
        Set the margins for the toolbar.

        @param size
            Margin size.

        @remarks This must be called before the tools are added if absolute
            positioning is to be used, and the default (zero-size) margins are
            to be overridden.

        @see GetMargins(), wxSize
    */
    void SetMargins(const wxSize& size);
    //@}

    /**
        Sets the default size of each tool bitmap. The default bitmap size is 16
        by 15 pixels.

        @param size
            The size of the bitmaps in the toolbar.

        @remarks This should be called to tell the toolbar what the tool bitmap
            size is. Call it before you add tools.

        @see GetToolBitmapSize(), GetToolSize()
    */
    virtual void SetToolBitmapSize(const wxSize& size);

    /**
        Sets the client data associated with the tool.

        @param id
            ID of the tool in question, as passed to AddTool().
        @param clientData
            The client data to use.
    */
    virtual void SetToolClientData(int id, wxObject* clientData);

    /**
        Sets the bitmap to be used by the tool with the given ID when the tool
        is in a disabled state. This can only be used on Button tools, not
        controls.

        @param id
            ID of the tool in question, as passed to AddTool().
        @param bitmap
            Bitmap to use for disabled tools.

        @note The native toolbar classes on the main platforms all synthesize
            the disabled bitmap from the normal bitmap, so this function will
            have no effect on those platforms.

    */
    virtual void SetToolDisabledBitmap(int id, const wxBitmap& bitmap);

    /**
        Sets the long help for the given tool.

        @param toolId
            ID of the tool in question, as passed to AddTool().
        @param helpString
            A string for the long help.

        @remarks You might use the long help for displaying the tool purpose on
            the status line.

        @see GetToolLongHelp(), SetToolShortHelp(),
    */
    virtual void SetToolLongHelp(int toolId, const wxString& helpString);

    /**
        Sets the bitmap to be used by the tool with the given ID. This can only
        be used on Button tools, not controls.

        @param id
            ID of the tool in question, as passed to AddTool().
        @param bitmap
            Bitmap to use for normals tools.
    */
    virtual void SetToolNormalBitmap(int id, const wxBitmap& bitmap);

    /**
        Sets the value used for spacing tools. The default value is 1.

        @param packing
            The value for packing.

        @remarks The packing is used for spacing in the vertical direction if
            the toolbar is horizontal, and for spacing in the horizontal
            direction if the toolbar is vertical.

        @see GetToolPacking()
    */
    virtual void SetToolPacking(int packing);

    /**
        Sets the default separator size. The default value is 5.

        @param separation
            The separator size.

        @see AddSeparator()
    */
    virtual void SetToolSeparation(int separation);

    /**
        Sets the short help for the given tool.

        @param toolId
            ID of the tool in question, as passed to AddTool().
        @param helpString
            The string for the short help.

        @remarks An application might use short help for identifying the tool
            purpose in a tooltip.


        @see GetToolShortHelp(), SetToolLongHelp()
    */
    virtual void SetToolShortHelp(int toolId, const wxString& helpString);

    /**
        Toggles a tool on or off. This does not cause any event to get emitted.

        @param toolId
            ID of the tool in question, as passed to AddTool().
        @param toggle
            If @true, toggles the tool on, otherwise toggles it off.

        @remarks Only applies to a tool that has been specified as a toggle
            tool.
    */
    virtual void ToggleTool(int toolId, bool toggle);


    /**
       Factory function to create a new toolbar tool.
    */
    virtual wxToolBarToolBase *CreateTool(int toolId,
                                          const wxString& label,
                                          const wxBitmap& bmpNormal,
                                          const wxBitmap& bmpDisabled = wxNullBitmap,
                                          wxItemKind kind = wxITEM_NORMAL,
                                          wxObject *clientData = NULL,
                                          const wxString& shortHelp = wxEmptyString,
                                          const wxString& longHelp = wxEmptyString);
    /**
       Factory function to create a new control toolbar tool.
    */
    virtual wxToolBarToolBase *CreateTool(wxControl *control,
                                          const wxString& label);

    /**
       Factory function to create a new separator toolbar tool.
    */
    wxToolBarToolBase *CreateSeparator();
};

