/////////////////////////////////////////////////////////////////////////////
// Name:        menu.h
// Purpose:     interface of wxMenuBar
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxMenuBar

    A menu bar is a series of menus accessible from the top of a frame.

    @remarks
    To respond to a menu selection, provide a handler for EVT_MENU, in the frame
    that contains the menu bar.

    If you have a toolbar which uses the same identifiers as your EVT_MENU entries,
    events from the toolbar will also be processed by your EVT_MENU event handlers.

    Tip: under Windows, if you discover that menu shortcuts (for example, Alt-F
    to show the file menu) are not working, check any EVT_CHAR events you are
    handling in child windows.
    If you are not calling event.Skip() for events that you don't process in
    these event handlers, menu shortcuts may cease to work.

    @library{wxcore}
    @category{menus}

    @see wxMenu, @ref overview_events
*/
class wxMenuBar : public wxWindow
{
public:
    /**
        Construct an empty menu bar.

        @param style
            If wxMB_DOCKABLE the menu bar can be detached (wxGTK only).
    */
    wxMenuBar(long style = 0);

    /**
        Construct a menu bar from arrays of menus and titles.

        @param n
            The number of menus.
        @param menus
            An array of menus. Do not use this array again - it now belongs to
            the menu bar.
        @param titles
            An array of title strings. Deallocate this array after creating
            the menu bar.
        @param style
            If wxMB_DOCKABLE the menu bar can be detached (wxGTK only).

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    wxMenuBar(size_t n, wxMenu* menus[], const wxString titles[],
              long style = 0);

    /**
        Destructor, destroying the menu bar and removing it from the parent
        frame (if any).
    */
    virtual ~wxMenuBar();

    /**
        Adds the item to the end of the menu bar.

        @param menu
            The menu to add. Do not deallocate this menu after calling Append().
        @param title
            The title of the menu, must be non-empty.

        @return @true on success, @false if an error occurred.

        @see Insert()
    */
    virtual bool Append(wxMenu* menu, const wxString& title);

    /**
        Checks or unchecks a menu item.

        @param id
            The menu item identifier.
        @param check
            If @true, checks the menu item, otherwise the item is unchecked.

        @remarks Only use this when the menu bar has been associated with a
                 frame; otherwise, use the wxMenu equivalent call.
    */
    void Check(int id, bool check);

    /**
        Enables or disables (greys out) a menu item.

        @param id
            The menu item identifier.
        @param enable
            @true to enable the item, @false to disable it.

        @remarks Only use this when the menu bar has been associated with a
                 frame; otherwise, use the wxMenu equivalent call.
    */
    void Enable(int id, bool enable);

    /**
        Returns true if the menu with the given index is enabled.

        @param pos
            The menu position (0-based)

        @since 2.9.4
    */
    bool IsEnabledTop(size_t pos) const;

    /**
        Enables or disables a whole menu.

        @param pos
            The position of the menu, starting from zero.
        @param enable
            @true to enable the menu, @false to disable it.

        @remarks Only use this when the menu bar has been associated with a frame.
    */
    virtual void EnableTop(size_t pos, bool enable);

    /**
        Finds the menu item object associated with the given menu item identifier.

        @param id
            Menu item identifier.
        @param menu
            If not @NULL, menu will get set to the associated menu.

        @return The found menu item object, or @NULL if one was not found.

        @beginWxPerlOnly
        In wxPerl this method takes just the @a id parameter;
        in scalar context it returns the associated @c Wx::MenuItem, in list
        context it returns a 2-element list (item, submenu).
        @endWxPerlOnly
    */
    virtual wxMenuItem* FindItem(int id, wxMenu** menu = NULL) const;

    /**
        Returns the index of the menu with the given @a title or @c wxNOT_FOUND if no
        such menu exists in this menubar.

        The @a title parameter may specify either the menu title
        (with accelerator characters, i.e. @c "&File") or just the
        menu label (@c "File") indifferently.
    */
    int FindMenu(const wxString& title) const;

    /**
        Finds the menu item id for a menu name/menu item string pair.

        @param menuString
            Menu title to find.
        @param itemString
            Item to find.

        @return The menu item identifier, or wxNOT_FOUND if none was found.

        @remarks Any special menu codes are stripped out of source and target
                 strings before matching.
    */
    virtual int FindMenuItem(const wxString& menuString,
                             const wxString& itemString) const;

    /**
        Gets the help string associated with the menu item identifier.

        @param id
            The menu item identifier.

        @return The help string, or the empty string if there was no help string
                or the menu item was not found.

        @see SetHelpString()
    */
    wxString GetHelpString(int id) const;

    /**
        Gets the label associated with a menu item.

        @param id
            The menu item identifier.

        @return The menu item label, or the empty string if the item was not
                found.

        @remarks Use only after the menubar has been associated with a frame.
    */
    wxString GetLabel(int id) const;

    /**
        Returns the label of a top-level menu. Note that the returned string does not
        include the accelerator characters which could have been specified in the menu
        title string during its construction.

        @param pos
            Position of the menu on the menu bar, starting from zero.

        @return The menu label, or the empty string if the menu was not found.

        @remarks Use only after the menubar has been associated with a frame.

        @deprecated
        This function is deprecated in favour of GetMenuLabel() and GetMenuLabelText().

        @see SetLabelTop()
    */
    wxString GetLabelTop(size_t pos) const;

    /**
        Returns the menu at @a menuIndex (zero-based).
    */
    wxMenu* GetMenu(size_t menuIndex) const;

    /**
        Returns the number of menus in this menubar.
    */
    size_t GetMenuCount() const;

    /**
        Returns the label of a top-level menu. Note that the returned string
        includes the accelerator characters that have been specified in the menu
        title string during its construction.

        @param pos
            Position of the menu on the menu bar, starting from zero.

        @return The menu label, or the empty string if the menu was not found.

        @remarks Use only after the menubar has been associated with a frame.

        @see GetMenuLabelText(), SetMenuLabel()
    */
    virtual wxString GetMenuLabel(size_t pos) const;

    /**
        Returns the label of a top-level menu. Note that the returned string does not
        include any accelerator characters that may have been specified in the menu
        title string during its construction.

        @param pos
            Position of the menu on the menu bar, starting from zero.

        @return The menu label, or the empty string if the menu was not found.

        @remarks Use only after the menubar has been associated with a frame.

        @see GetMenuLabel(), SetMenuLabel()
    */
    virtual wxString GetMenuLabelText(size_t pos) const;

    /**
        Inserts the menu at the given position into the menu bar. Inserting menu at
        position 0 will insert it in the very beginning of it, inserting at position
        GetMenuCount() is the same as calling Append().

        @param pos
            The position of the new menu in the menu bar
        @param menu
            The menu to add. wxMenuBar owns the menu and will free it.
        @param title
            The title of the menu.

        @return @true on success, @false if an error occurred.

        @see Append()
    */
    virtual bool Insert(size_t pos, wxMenu* menu, const wxString& title);

    /**
        Determines whether an item is checked.

        @param id
            The menu item identifier.

        @return @true if the item was found and is checked, @false otherwise.
    */
    bool IsChecked(int id) const;

    /**
        Determines whether an item is enabled.

        @param id
            The menu item identifier.

        @return @true if the item was found and is enabled, @false otherwise.
    */
    bool IsEnabled(int id) const;

    /**
        Redraw the menu bar
    */
    virtual void Refresh(bool eraseBackground = true, const wxRect* rect = NULL);

    /**
        Removes the menu from the menu bar and returns the menu object - the caller
        is responsible for deleting it. This function may be used together with
        Insert() to change the menubar dynamically.

        @see Replace()
    */
    virtual wxMenu* Remove(size_t pos);

    /**
        Replaces the menu at the given position with another one.

        @param pos
            The position of the new menu in the menu bar
        @param menu
            The menu to add.
        @param title
            The title of the menu.

        @return The menu which was previously at position pos.
                The caller is responsible for deleting it.

        @see Insert(), Remove()
    */
    virtual wxMenu* Replace(size_t pos, wxMenu* menu, const wxString& title);

    /**
        Sets the help string associated with a menu item.

        @param id
            Menu item identifier.
        @param helpString
            Help string to associate with the menu item.

        @see GetHelpString()
    */
    void SetHelpString(int id, const wxString& helpString);

    /**
        Sets the label of a menu item.

        @param id
            Menu item identifier.
        @param label
            Menu item label.

        @remarks Use only after the menubar has been associated with a frame.

        @see GetLabel()
    */
    void SetLabel(int id, const wxString& label);

    /**
        Sets the label of a top-level menu.

        @param pos
            The position of a menu on the menu bar, starting from zero.
        @param label
            The menu label.

        @remarks Use only after the menubar has been associated with a frame.

        @deprecated
        This function has been deprecated in favour of SetMenuLabel().

        @see GetLabelTop()
    */
    void SetLabelTop(size_t pos, const wxString& label);

    /**
        Sets the label of a top-level menu.

        @param pos
            The position of a menu on the menu bar, starting from zero.
        @param label
            The menu label.

        @remarks Use only after the menubar has been associated with a frame.
    */
    virtual void SetMenuLabel(size_t pos, const wxString& label);

    /**
        Enables you to set the global menubar on Mac, that is, the menubar displayed
        when the app is running without any frames open.

        @param menubar
            The menubar to set.

        @remarks Only exists on Mac, other platforms do not have this method.

        @onlyfor{wxosx}
    */
    static void MacSetCommonMenuBar(wxMenuBar* menubar);

    /**
        Enables you to get the global menubar on Mac, that is, the menubar displayed
        when the app is running without any frames open.

        @return The global menubar.

        @remarks Only exists on Mac, other platforms do not have this method.

        @onlyfor{wxosx}
    */
    static wxMenuBar* MacGetCommonMenuBar();

    /**
        Returns the Apple menu.

        This is the leftmost menu with application's name as its title. You
        shouldn't remove any items from it, but it is safe to insert extra menu
        items or submenus into it.

        @onlyfor{wxosx}
        @since 3.0.1
     */
    wxMenu *OSXGetAppleMenu() const;

    wxFrame *GetFrame() const;
    bool IsAttached() const;
    virtual void Attach(wxFrame *frame);
    virtual void Detach();

};



/**
    @class wxMenu

    A menu is a popup (or pull down) list of items, one of which may be
    selected before the menu goes away (clicking elsewhere dismisses the
    menu). Menus may be used to construct either menu bars or popup menus.

    A menu item has an integer ID associated with it which can be used to
    identify the selection, or to change the menu item in some way. A menu item
    with a special identifier @e wxID_SEPARATOR is a separator item and doesn't
    have an associated command but just makes a separator line appear in the
    menu.

    @note
    Please note that @e wxID_ABOUT and @e wxID_EXIT are predefined by wxWidgets
    and have a special meaning since entries using these IDs will be taken out
    of the normal menus under macOS and will be inserted into the system menu
    (following the appropriate macOS interface guideline).

    Menu items may be either @e normal items, @e check items or @e radio items.
    Normal items don't have any special properties while the check items have a
    boolean flag associated to them and they show a checkmark in the menu when
    the flag is set.
    wxWidgets automatically toggles the flag value when the item is clicked and its
    value may be retrieved using either wxMenu::IsChecked method of wxMenu or
    wxMenuBar itself or by using wxEvent::IsChecked when you get the menu
    notification for the item in question.

    The radio items are similar to the check items except that all the other items
    in the same radio group are unchecked when a radio item is checked. The radio
    group is formed by a contiguous range of radio items, i.e. it starts at the
    first item of this kind and ends with the first item of a different kind (or
    the end of the menu). Notice that because the radio groups are defined in terms
    of the item positions inserting or removing the items in the menu containing
    the radio items risks to not work correctly.


    @section menu_allocation Allocation strategy

    All menus must be created on the @b heap because all menus attached to a
    menubar or to another menu will be deleted by their parent when it is
    deleted. The only exception to this rule are the popup menus (i.e. menus
    used with wxWindow::PopupMenu()) as wxWidgets does not destroy them to
    allow reusing the same menu more than once. But the exception applies only
    to the menus themselves and not to any submenus of popup menus which are
    still destroyed by wxWidgets as usual and so must be heap-allocated.

    As the frame menubar is deleted by the frame itself, it means that normally
    all menus used are deleted automatically.


    @section menu_eventhandling Event handling

    Event handlers for the commands generated by the menu items can be
    connected directly to the menu object itself using wxEvtHandler::Bind(). If
    this menu is a submenu of another one, the events from its items can also
    be processed in the parent menu and so on, recursively.

    If the menu is part of a menu bar, then events can also be handled in
    wxMenuBar object.

    Finally, menu events can also be handled in the associated window, which is
    either the wxFrame associated with the menu bar this menu belongs to or the
    window for which wxWindow::PopupMenu() was called for the popup menus.

    See @ref overview_events_bind for how to bind event handlers to the various
    objects.

    @library{wxcore}
    @category{menus}

    @see wxMenuBar, wxWindow::PopupMenu, @ref overview_events,
         @ref wxFileHistory "wxFileHistory (most recently used files menu)"
*/
class wxMenu : public wxEvtHandler
{
public:

    /**
        Constructs a wxMenu object.
    */
    wxMenu();

    /**
        Constructs a wxMenu object.

        @param style
            If set to wxMENU_TEAROFF, the menu will be detachable (wxGTK and wxQT only).
    */
    wxMenu(long style);

    /**
        Constructs a wxMenu object with a title.

        @param title
            Title at the top of the menu (not always supported).
        @param style
            If set to wxMENU_TEAROFF, the menu will be detachable (wxGTK and wxQT only).
    */
    wxMenu(const wxString& title, long style = 0);

    /**
        Destructor, destroying the menu.

        @note
            Under Motif, a popup menu must have a valid parent (the window
            it was last popped up on) when being destroyed. Therefore, make sure
            you delete or re-use the popup menu @e before destroying the parent
            window. Re-use in this context means popping up the menu on a different
            window from last time, which causes an implicit destruction and
            recreation of internal data structures.
    */
    virtual ~wxMenu();

    /**
        Adds a menu item.

        @param id
            The menu command identifier.
            See @ref overview_windowids for more information about IDs (same
            considerations apply to both window and menu item IDs).
        @param item
            The string to appear on the menu item.
            See wxMenuItem::SetItemLabel() for more details.
        @param helpString
            An optional help string associated with the item.
            By default, the handler for the @c wxEVT_MENU_HIGHLIGHT event displays
            this string in the status line.
        @param kind
            May be @c wxITEM_SEPARATOR, @c wxITEM_NORMAL, @c wxITEM_CHECK or @c wxITEM_RADIO.

        Example:
        @code
        m_pFileMenu->Append(ID_NEW_FILE, "&New file\tCTRL+N", "Creates a new XYZ document");
        @endcode
        or even better for stock menu items (see wxMenuItem::wxMenuItem):
        @code
        m_pFileMenu->Append(wxID_NEW, "", "Creates a new XYZ document");
        @endcode

        @remarks
        This command can be used after the menu has been shown, as well as on
        initial creation of a menu or menubar.

        @see AppendSeparator(), AppendCheckItem(), AppendRadioItem(),
             AppendSubMenu(), Insert(), SetLabel(), GetHelpString(),
             SetHelpString(), wxMenuItem
    */
    wxMenuItem* Append(int id, const wxString& item = wxEmptyString,
                       const wxString& helpString = wxEmptyString,
                       wxItemKind kind = wxITEM_NORMAL);

    /**
        Adds a submenu.

        @deprecated This function is deprecated, use AppendSubMenu() instead.

        @param id
            The menu command identifier.
        @param item
            The string to appear on the menu item.
        @param subMenu
            Pull-right submenu.
        @param helpString
            An optional help string associated with the item.
            By default, the handler for the @c wxEVT_MENU_HIGHLIGHT event displays
            this string in the status line.

        @see AppendSeparator(), AppendCheckItem(), AppendRadioItem(),
             AppendSubMenu(), Insert(), SetLabel(), GetHelpString(),
             SetHelpString(), wxMenuItem
    */
    wxMenuItem* Append(int id, const wxString& item, wxMenu* subMenu,
                       const wxString& helpString = wxEmptyString);

    /**
        Adds a menu item object.

        This is the most generic variant of Append() method because it may be
        used for both items (including separators) and submenus and because
        you can also specify various extra properties of a menu item this way,
        such as bitmaps and fonts.

        @param menuItem
            A menuitem object. It will be owned by the wxMenu object after this
            function is called, so do not delete it yourself.

        @remarks
            See the remarks for the other Append() overloads.

        @see AppendSeparator(), AppendCheckItem(), AppendRadioItem(),
             AppendSubMenu(), Insert(), SetLabel(), GetHelpString(),
             SetHelpString(), wxMenuItem
    */
    wxMenuItem* Append(wxMenuItem* menuItem);

    /**
        Adds a checkable item to the end of the menu.

        @see Append(), InsertCheckItem()
    */
    wxMenuItem* AppendCheckItem(int id, const wxString& item,
                                const wxString& help = wxEmptyString);

    /**
        Adds a radio item to the end of the menu.
        All consequent radio items form a group and when an item in the group is
        checked, all the others are automatically unchecked.

        @note Radio items are not supported under wxMotif.

        @see Append(), InsertRadioItem()
    */
    wxMenuItem* AppendRadioItem(int id, const wxString& item,
                                const wxString& help = wxEmptyString);

    /**
        Adds a separator to the end of the menu.

        @see Append(), InsertSeparator()
    */
    wxMenuItem* AppendSeparator();

    /**
        Adds the given @a submenu to this menu. @a text is the text shown in the
        menu for it and @a help is the help string shown in the status bar when the
        submenu item is selected.

        @see Insert(), Prepend()
    */
    wxMenuItem* AppendSubMenu(wxMenu* submenu, const wxString& text,
                              const wxString& help = wxEmptyString);

    /**
        Inserts a break in a menu, causing the next appended item to appear in
        a new column.

        This function only actually inserts a break in wxMSW and does nothing
        under the other platforms.
    */
    virtual void Break();

    /**
        Checks or unchecks the menu item.

        @param id
            The menu item identifier.
        @param check
            If @true, the item will be checked, otherwise it will be unchecked.

        @see IsChecked()
    */
    void Check(int id, bool check);

    /**
        Deletes the menu item from the menu. If the item is a submenu, it will
        @b not be deleted. Use Destroy() if you want to delete a submenu.

        @param id
            Id of the menu item to be deleted.

        @see FindItem(), Destroy(), Remove()
    */
    bool Delete(int id);

    /**
        Deletes the menu item from the menu. If the item is a submenu, it will
        @b not be deleted. Use Destroy() if you want to delete a submenu.

        @param item
            Menu item to be deleted.

        @see FindItem(), Destroy(), Remove()
    */
    bool Delete(wxMenuItem* item);

    /**
        Deletes the menu item from the menu. If the item is a submenu, it will
        be deleted. Use Remove() if you want to keep the submenu (for example,
        to reuse it later).

        @param id
            Id of the menu item to be deleted.

        @see FindItem(), Delete(), Remove()
    */
    bool Destroy(int id);

    /**
        Deletes the menu item from the menu. If the item is a submenu, it will
        be deleted. Use Remove() if you want to keep the submenu (for example,
        to reuse it later).

        @param item
            Menu item to be deleted.

        @see FindItem(), Delete(), Remove()
    */
    bool Destroy(wxMenuItem* item);

    /**
        Enables or disables (greys out) a menu item.

        @param id
            The menu item identifier.
        @param enable
            @true to enable the menu item, @false to disable it.

        @see IsEnabled()
    */
    void Enable(int id, bool enable);

    /**
      Finds the menu item object associated with the given menu item identifier
      and, optionally, the position of the item in the menu.

      Unlike FindItem(), this function doesn't recurse but only looks at the
      direct children of this menu.

      @param id
          The identifier of the menu item to find.
      @param pos
          If the pointer is not @NULL, it is filled with the item's position if
          it was found or @c (size_t)wxNOT_FOUND otherwise.
      @return
        Menu item object or @NULL if not found.
     */
    wxMenuItem *FindChildItem(int id, size_t *pos = NULL) const;

    /**
        Finds the menu id for a menu item string.

        @param itemString
            Menu item string to find.

        @return Menu item identifier, or wxNOT_FOUND if none is found.

        @remarks Any special menu codes are stripped out of source and target
                 strings before matching.
    */
    virtual int FindItem(const wxString& itemString) const;

    /**
        Finds the menu item object associated with the given menu item identifier and,
        optionally, the (sub)menu it belongs to.

        @param id
            Menu item identifier.
        @param menu
            If the pointer is not @NULL, it will be filled with the item's
            parent menu (if the item was found)

        @return Menu item object or NULL if none is found.
    */
    wxMenuItem* FindItem(int id, wxMenu** menu = NULL) const;

    /**
        Returns the wxMenuItem given a position in the menu.
    */
    wxMenuItem* FindItemByPosition(size_t position) const;

    /**
        Returns the help string associated with a menu item.

        @param id
            The menu item identifier.

        @return The help string, or the empty string if there is no help string
                or the item was not found.

        @see SetHelpString(), Append()
    */
    virtual wxString GetHelpString(int id) const;

    /**
        Returns a menu item label.

        @param id
            The menu item identifier.

        @return The item label, or the empty string if the item was not found.

        @see GetLabelText(), SetLabel()
    */
    wxString GetLabel(int id) const;

    /**
        Returns a menu item label, without any of the original mnemonics and
        accelerators.

        @param id
            The menu item identifier.

        @return The item label, or the empty string if the item was not found.

        @see GetLabel(), SetLabel()
    */
    wxString GetLabelText(int id) const;

    /**
        Returns the number of items in the menu.
    */
    size_t GetMenuItemCount() const;

    //@{
    /**
        Returns the list of items in the menu.

        wxMenuItemList is a pseudo-template list class containing wxMenuItem
        pointers, see wxList.
    */
    wxMenuItemList& GetMenuItems();
    const wxMenuItemList& GetMenuItems() const;
    //@}

    /**
        Returns the title of the menu.

        @see SetTitle()
    */
    const wxString& GetTitle() const;

    /**
        Inserts the given @a item before the position @a pos.

        Inserting the item at position GetMenuItemCount() is the same
        as appending it.

        @see Append(), Prepend()
    */
    wxMenuItem* Insert(size_t pos, wxMenuItem* menuItem);

    /**
        Inserts the given @a item before the position @a pos.

        Inserting the item at position GetMenuItemCount() is the same
        as appending it.

        @see Append(), Prepend()
    */
    wxMenuItem* Insert(size_t pos, int id,
                       const wxString& item = wxEmptyString,
                       const wxString& helpString = wxEmptyString,
                       wxItemKind kind = wxITEM_NORMAL);

    /**
        Inserts the given @a submenu before the position @a pos.
        @a text is the text shown in the menu for it and @a help is the
        help string shown in the status bar when the submenu item is selected.

        @see AppendSubMenu(), Prepend()
    */
    wxMenuItem* Insert(size_t pos, int id, const wxString& text,
                       wxMenu* submenu, const wxString& help = wxEmptyString);

    /**
        Inserts a checkable item at the given position.

        @see Insert(), AppendCheckItem()
    */
    wxMenuItem* InsertCheckItem(size_t pos, int id, const wxString& item,
                                const wxString& helpString = wxEmptyString);

    /**
        Inserts a radio item at the given position.

        @see Insert(), AppendRadioItem()
    */
    wxMenuItem* InsertRadioItem(size_t pos, int id, const wxString& item,
                                const wxString& helpString = wxEmptyString);

    /**
        Inserts a separator at the given position.

        @see Insert(), AppendSeparator()
    */
    wxMenuItem* InsertSeparator(size_t pos);

    /**
        Determines whether a menu item is checked.

        @param id
            The menu item identifier.

        @return @true if the menu item is checked, @false otherwise.

        @see Check()
    */
    bool IsChecked(int id) const;

    /**
        Determines whether a menu item is enabled.

        @param id
            The menu item identifier.

        @return @true if the menu item is enabled, @false otherwise.

        @see Enable()
    */
    bool IsEnabled(int id) const;

    /**
        Inserts the given @a item at position 0, i.e.\ before all the other
        existing items.

        @see Append(), Insert()
    */
    wxMenuItem* Prepend(wxMenuItem* item);

    /**
        Inserts the given @a item at position 0, i.e.\ before all the other
        existing items.

        @see Append(), Insert()
    */
    wxMenuItem* Prepend(int id, const wxString& item = wxEmptyString,
                        const wxString& helpString = wxEmptyString,
                        wxItemKind kind = wxITEM_NORMAL);

    /**
        Inserts the given @a submenu at position 0.

        @see AppendSubMenu(), Insert()
    */
    wxMenuItem* Prepend(int id, const wxString& text, wxMenu* submenu,
                        const wxString& help = wxEmptyString);

    /**
        Inserts a checkable item at position 0.

        @see Prepend(), AppendCheckItem()
    */
    wxMenuItem* PrependCheckItem(int id, const wxString& item,
                                 const wxString& helpString = wxEmptyString);

    /**
        Inserts a radio item at position 0.

        @see Prepend(), AppendRadioItem()
    */
    wxMenuItem* PrependRadioItem(int id, const wxString& item,
                                 const wxString& helpString = wxEmptyString);

    /**
        Inserts a separator at position 0.

        @see Prepend(), AppendSeparator()
    */
    wxMenuItem* PrependSeparator();

    /**
        Removes the menu item from the menu but doesn't delete the associated C++
        object. This allows you to reuse the same item later by adding it back to
        the menu (especially useful with submenus).

        @param id
            The identifier of the menu item to remove.

        @return A pointer to the item which was detached from the menu.
    */
    wxMenuItem* Remove(int id);

    /**
        Removes the menu item from the menu but doesn't delete the associated C++
        object. This allows you to reuse the same item later by adding it back to
        the menu (especially useful with submenus).

        @param item
            The menu item to remove.

        @return A pointer to the item which was detached from the menu.
    */
    wxMenuItem* Remove(wxMenuItem* item);

    /**
        Sets an item's help string.

        @param id
            The menu item identifier.
        @param helpString
            The help string to set.

        @see GetHelpString()
    */
    virtual void SetHelpString(int id, const wxString& helpString);

    /**
        Sets the label of a menu item.

        @param id
            The menu item identifier.
        @param label
            The menu item label to set.

        @see Append(), GetLabel()
    */
    void SetLabel(int id, const wxString& label);

    /**
        Sets the title of the menu.

        @param title
            The title to set.

        @remarks Notice that you can only call this method directly for the
            popup menus, to change the title of a menu that is part of a menu
            bar you need to use wxMenuBar::SetLabelTop().

        @see GetTitle()
    */
    virtual void SetTitle(const wxString& title);

    /**
        Update the state of all menu items, recursively, by generating @c
        wxEVT_UPDATE_UI events for them.

        This is an internal wxWidgets function and shouldn't normally be called
        from outside of the library. If it is called, @a source argument should
        not be used, it is deprecated and exists only for backwards
        compatibility.
    */
    void UpdateUI(wxEvtHandler* source = NULL);


    void SetInvokingWindow(wxWindow *win);
    wxWindow *GetInvokingWindow() const;
    wxWindow *GetWindow() const;
    long GetStyle() const;
    void SetParent(wxMenu *parent);
    wxMenu *GetParent() const;

    virtual void Attach(wxMenuBar *menubar);
    virtual void Detach();
    bool IsAttached() const;

};

