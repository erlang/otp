/////////////////////////////////////////////////////////////////////////////
// Name:        treectrl.h
// Purpose:     interface of wxTreeItemData
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxTreeCtrl

    A tree control presents information as a hierarchy, with items that may be
    expanded to show further items. Items in a tree control are referenced by
    wxTreeItemId handles, which may be tested for validity by calling
    wxTreeItemId::IsOk().

    A similar control with a fully native implementation for GTK+ and macOS
    as well is wxDataViewTreeCtrl.

    To intercept events from a tree control, use the event table macros
    described in wxTreeEvent.

    @beginStyleTable
    @style{wxTR_EDIT_LABELS}
        Use this style if you wish the user to be able to edit labels in the
        tree control.
    @style{wxTR_NO_BUTTONS}
        For convenience to document that no buttons are to be drawn.
    @style{wxTR_HAS_BUTTONS}
        Use this style to show + and - buttons to the left of parent items.
    @style{wxTR_TWIST_BUTTONS}
        Selects alternative style of @c +/@c - buttons and shows rotating
        ("twisting") arrows instead. Currently this style is only implemented
        under Microsoft Windows Vista and later Windows versions and is ignored
        under the other platforms as enabling it is equivalent to using
        wxSystemThemedControl::EnableSystemTheme().
    @style{wxTR_NO_LINES}
        Use this style to hide vertical level connectors.
    @style{wxTR_FULL_ROW_HIGHLIGHT}
        Use this style to have the background colour and the selection highlight
        extend over the entire horizontal row of the tree control window. (This
        flag is ignored under Windows unless you specify @c wxTR_NO_LINES as
        well.)
    @style{wxTR_LINES_AT_ROOT}
        Use this style to show lines leading to the root nodes (unless no
        @c wxTR_NO_LINES is also used, in which case no lines are shown). Note
        that in the MSW version, if this style is omitted, not only the lines,
        but also the button used for expanding the root item is not shown,
        which can be unexpected, so it is recommended to always use it.
    @style{wxTR_HIDE_ROOT}
        Use this style to suppress the display of the root node, effectively
        causing the first-level nodes to appear as a series of root nodes.
    @style{wxTR_ROW_LINES}
        Use this style to draw a contrasting border between displayed rows.
    @style{wxTR_HAS_VARIABLE_ROW_HEIGHT}
        Use this style to cause row heights to be just big enough to fit the
        content. If not set, all rows use the largest row height. The default is
        that this flag is unset. Generic only.
    @style{wxTR_SINGLE}
        For convenience to document that only one item may be selected at a
        time. Selecting another item causes the current selection, if any, to be
        deselected. This is the default.
    @style{wxTR_MULTIPLE}
        Use this style to allow a range of items to be selected. If a second
        range is selected, the current range, if any, is deselected.
    @style{wxTR_DEFAULT_STYLE}
        The set of flags that are closest to the defaults for the native control
        for a particular toolkit.
    @endStyleTable

    @beginEventEmissionTable{wxTreeEvent}
    @event{EVT_TREE_BEGIN_DRAG(id, func)}
          Begin dragging with the left mouse button.
          If you want to enable left-dragging you need to intercept this event
          and explicitly call wxTreeEvent::Allow(), as it's vetoed by default.
          Processes a @c wxEVT_TREE_BEGIN_DRAG event type.
    @event{EVT_TREE_BEGIN_RDRAG(id, func)}
          Begin dragging with the right mouse button.
          If you want to enable right-dragging you need to intercept this event
          and explicitly call wxTreeEvent::Allow(), as it's vetoed by default.
          Processes a @c wxEVT_TREE_BEGIN_RDRAG event type.
    @event{EVT_TREE_END_DRAG(id, func)}
          End dragging with the left or right mouse button.
          Processes a @c wxEVT_TREE_END_DRAG event type.
    @event{EVT_TREE_BEGIN_LABEL_EDIT(id, func)}
          Begin editing a label. This can be prevented by calling Veto().
          Processes a @c wxEVT_TREE_BEGIN_LABEL_EDIT event type.
    @event{EVT_TREE_END_LABEL_EDIT(id, func)}
          Finish editing a label. This can be prevented by calling Veto().
          Processes a @c wxEVT_TREE_END_LABEL_EDIT event type.
    @event{EVT_TREE_DELETE_ITEM(id, func)}
          An item was deleted.
          Processes a @c wxEVT_TREE_DELETE_ITEM event type.
    @event{EVT_TREE_GET_INFO(id, func)}
          Request information from the application.
          Processes a @c wxEVT_TREE_GET_INFO event type.
    @event{EVT_TREE_SET_INFO(id, func)}
          Information is being supplied.
          Processes a @c wxEVT_TREE_SET_INFO event type.
    @event{EVT_TREE_ITEM_ACTIVATED(id, func)}
          The item has been activated, i.e. chosen by double clicking it with
          mouse or from keyboard.
          Processes a @c wxEVT_TREE_ITEM_ACTIVATED event type.
    @event{EVT_TREE_ITEM_COLLAPSED(id, func)}
          The item has been collapsed.
          Processes a @c wxEVT_TREE_ITEM_COLLAPSED event type.
    @event{EVT_TREE_ITEM_COLLAPSING(id, func)}
          The item is being collapsed. This can be prevented by calling Veto().
          Processes a @c wxEVT_TREE_ITEM_COLLAPSING event type.
    @event{EVT_TREE_ITEM_EXPANDED(id, func)}
          The item has been expanded.
          Processes a @c wxEVT_TREE_ITEM_EXPANDED event type.
    @event{EVT_TREE_ITEM_EXPANDING(id, func)}
          The item is being expanded. This can be prevented by calling Veto().
          Processes a @c wxEVT_TREE_ITEM_EXPANDING event type.
    @event{EVT_TREE_ITEM_RIGHT_CLICK(id, func)}
          The user has clicked the item with the right mouse button.
          Processes a @c wxEVT_TREE_ITEM_RIGHT_CLICK event type.
    @event{EVT_TREE_ITEM_MIDDLE_CLICK(id, func)}
          The user has clicked the item with the middle mouse button. This is
          only supported by the generic control.
          Processes a @c wxEVT_TREE_ITEM_MIDDLE_CLICK event type.
    @event{EVT_TREE_SEL_CHANGED(id, func)}
          Selection has changed.
          Processes a @c wxEVT_TREE_SEL_CHANGED event type.
    @event{EVT_TREE_SEL_CHANGING(id, func)}
          Selection is changing. This can be prevented by calling Veto().
          Processes a @c wxEVT_TREE_SEL_CHANGING event type.
    @event{EVT_TREE_KEY_DOWN(id, func)}
          A key has been pressed.
          Processes a @c wxEVT_TREE_KEY_DOWN event type.
    @event{EVT_TREE_ITEM_GETTOOLTIP(id, func)}
          The opportunity to set the item tooltip is being given to the application
          (call wxTreeEvent::SetToolTip). Windows only.
          Processes a @c wxEVT_TREE_ITEM_GETTOOLTIP event type.
    @event{EVT_TREE_ITEM_MENU(id, func)}
          The context menu for the selected item has been requested, either by a
          right click or by using the menu key. Notice that these events always
          carry a valid tree item and so are not generated when (right)
          clicking outside of the items area. If you need to handle such
          events, consider using @c wxEVT_CONTEXT_MENU instead.
          Processes a @c wxEVT_TREE_ITEM_MENU event type.
    @event{EVT_TREE_STATE_IMAGE_CLICK(id, func)}
          The state image has been clicked.
          Processes a @c wxEVT_TREE_STATE_IMAGE_CLICK event type.
    @endEventTable


    See also @ref overview_windowstyles.

    @b Win32 @b notes:

    wxTreeCtrl class uses the standard common treeview control under Win32
    implemented in the system library comctl32.dll. Some versions of this
    library are known to have bugs with handling the tree control colours: the
    usual symptom is that the expanded items leave black (or otherwise
    incorrectly coloured) background behind them, especially for the controls
    using non-default background colour. The recommended solution is to upgrade
    the comctl32.dll to a newer version: see
    http://www.microsoft.com/downloads/details.aspx?familyid=cb2cf3a2-8025-4e8f-8511-9b476a8d35d2

    @library{wxcore}
    @category{ctrl}
    @appearance{treectrl}

    @see wxDataViewTreeCtrl, wxTreeEvent, wxTreeItemData, @ref overview_treectrl,
         wxListBox, wxListCtrl, wxImageList
*/
class wxTreeCtrl : public wxControl
{
public:
    /**
        Default Constructor.
    */
    wxTreeCtrl();

    /**
        Constructor, creating and showing a tree control.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value @c wxID_ANY indicates a default value.
        @param pos
            Window position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Window size.
            If ::wxDefaultSize is specified then the window is sized appropriately.
        @param style
            Window style. See wxTreeCtrl.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxTreeCtrl(wxWindow* parent, wxWindowID id = wxID_ANY,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = wxTR_DEFAULT_STYLE,
               const wxValidator& validator = wxDefaultValidator,
               const wxString& name = wxTreeCtrlNameStr);


    /**
        Destructor, destroying the tree control.
    */
    virtual ~wxTreeCtrl();

    /**
        Adds the root node to the tree, returning the new item.

        The @a image and @a selImage parameters are an index within the normal
        image list specifying the image to use for unselected and selected
        items, respectively. If @a image > -1 and @a selImage is -1, the same
        image is used for both selected and unselected items.
    */
    virtual wxTreeItemId AddRoot(const wxString& text, int image = -1,
                                 int selImage = -1,
                                 wxTreeItemData* data = NULL);

    /**
        Appends an item to the end of the branch identified by @a parent, return
        a new item id.

        The @a image and @a selImage parameters are an index within the normal
        image list specifying the image to use for unselected and selected
        items, respectively. If @a image > -1 and @a selImage is -1, the same
        image is used for both selected and unselected items.
    */
    wxTreeItemId AppendItem(const wxTreeItemId& parent,
                            const wxString& text,
                            int image = -1,
                            int selImage = -1,
                            wxTreeItemData* data = NULL);

    /**
        Sets the buttons image list. The button images assigned with this method
        will be automatically deleted by wxTreeCtrl as appropriate (i.e. it
        takes ownership of the list).

        Setting or assigning the button image list enables the display of image
        buttons. Once enabled, the only way to disable the display of button
        images is to set the button image list to @NULL.

        This function is only available in the generic version.

        @see SetButtonsImageList().
    */
    void AssignButtonsImageList(wxImageList* imageList);

    /**
        Sets the normal image list. The image list assigned with this method
        will be automatically deleted by wxTreeCtrl as appropriate (i.e. it
        takes ownership of the list).

        @see SetImageList().
    */
    void AssignImageList(wxImageList* imageList);

    /**
        Sets the state image list. Image list assigned with this method will be
        automatically deleted by wxTreeCtrl as appropriate (i.e. it takes
        ownership of the list).

        @see SetStateImageList().
    */
    void AssignStateImageList(wxImageList* imageList);

    /**
        Collapses the given item.
    */
    virtual void Collapse(const wxTreeItemId& item);

    /**
        Collapses the root item.

        @see ExpandAll()
    */
    void CollapseAll();

    /**
        Collapses this item and all of its children, recursively.

        @see ExpandAllChildren()
    */
    void CollapseAllChildren(const wxTreeItemId& item);

    /**
        Collapses the given item and removes all children.
    */
    virtual void CollapseAndReset(const wxTreeItemId& item);

    /**
        Creates the tree control.
        See wxTreeCtrl::wxTreeCtrl() for further details.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxTR_DEFAULT_STYLE,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxTreeCtrlNameStr);

    /**
        Deletes the specified item. A @c EVT_TREE_DELETE_ITEM event will be
        generated.

        This function may cause a subsequent call to GetNextChild() to fail.
    */
    virtual void Delete(const wxTreeItemId& item);

    /**
        Deletes all items in the control.

        This function generates @c wxEVT_TREE_DELETE_ITEM events for each item
        being deleted, including the root one if it is shown, i.e. unless
        wxTR_HIDE_ROOT style is used.
    */
    virtual void DeleteAllItems();

    /**
        Deletes all children of the given item (but not the item itself).

        A @c wxEVT_TREE_DELETE_ITEM event will be generated for every item
        being deleted.

        If you have called SetItemHasChildren(), you may need to call it again
        since DeleteChildren() does not automatically clear the setting.
    */
    virtual void DeleteChildren(const wxTreeItemId& item);

    /**
        Starts editing the label of the given @a item. This function generates a
        @c EVT_TREE_BEGIN_LABEL_EDIT event which can be vetoed so that no text
        control will appear for in-place editing.

        If the user changed the label (i.e. s/he does not press ESC or leave the
        text control without changes, a @c EVT_TREE_END_LABEL_EDIT event will be
        sent which can be vetoed as well.

        @see EndEditLabel(), wxTreeEvent
    */
    virtual wxTextCtrl *EditLabel(const wxTreeItemId& item,
                                  wxClassInfo* textCtrlClass = wxCLASSINFO(wxTextCtrl));

    /**
        Enable or disable a beep if there is no match for the currently
        entered text when searching for the item from keyboard.

        The default is to not beep in this case except in wxMSW where the
        beep is always generated by the native control and cannot be disabled,
        i.e. calls to this function do nothing there.

        @since 2.9.5
    */
    void EnableBellOnNoMatch(bool on = true);

    /**
        Ends label editing. If @a cancelEdit is @true, the edit will be
        cancelled.

        @note
            This function is currently supported under Windows only.

        @see EditLabel()
    */
    virtual void EndEditLabel(const wxTreeItemId& item, bool discardChanges = false);

    /**
        Scrolls and/or expands items to ensure that the given item is visible.

        This method can be used, and will work, even while the window is frozen
        (see wxWindow::Freeze()).
    */
    virtual void EnsureVisible(const wxTreeItemId& item);

    /**
        Expands the given item.
    */
    virtual void Expand(const wxTreeItemId& item);

    /**
        Expands all items in the tree.
    */
    void ExpandAll();

    /**
        Expands the given item and all its children recursively.
    */
    void ExpandAllChildren(const wxTreeItemId& item);

    /**
        Retrieves the rectangle bounding the @a item. If @a textOnly is @true,
        only the rectangle around the item's label will be returned, otherwise
        the item's image is also taken into account.

        The return value is @true if the rectangle was successfully retrieved or
        @c @false if it was not (in this case @a rect is not changed) -- for
        example, if the item is currently invisible.

        Notice that the rectangle coordinates are logical, not physical ones.
        So, for example, the x coordinate may be negative if the tree has a
        horizontal scrollbar and its position is not 0.

        @beginWxPerlOnly
        In wxPerl this method only takes the @a item and
        @a textOnly parameters and returns a @c Wx::Rect (or @c undef).
        @endWxPerlOnly
    */
    virtual bool GetBoundingRect(const wxTreeItemId& item, wxRect& rect,
                                 bool textOnly = false) const;

    /**
        Returns the buttons image list (from which application-defined button
        images are taken).

        This function is only available in the generic version.
    */
    wxImageList* GetButtonsImageList() const;

    /**
        Returns the number of items in the branch. If @a recursively is @true,
        returns the total number of descendants, otherwise only one level of
        children is counted.
    */
    virtual size_t GetChildrenCount(const wxTreeItemId& item,
                                    bool recursively = true) const;

    /**
        Returns the number of items in the control.
    */
    virtual unsigned int GetCount() const;

    /**
        Returns the edit control being currently used to edit a label. Returns
        @NULL if no label is being edited.

        @note This is currently only implemented for wxMSW.
    */
    virtual wxTextCtrl* GetEditControl() const;

    /**
        Returns the first child; call GetNextChild() for the next child.

        For this enumeration function you must pass in a 'cookie' parameter
        which is opaque for the application but is necessary for the library to
        make these functions reentrant (i.e. allow more than one enumeration on
        one and the same object simultaneously). The cookie passed to
        GetFirstChild() and GetNextChild() should be the same variable.

        Returns an invalid tree item (i.e. wxTreeItemId::IsOk() returns @false)
        if there are no further children.

        @beginWxPerlOnly
        In wxPerl this method only takes the @a item parameter, and
        returns a 2-element list (item, cookie).
        @endWxPerlOnly

        @see GetNextChild(), GetNextSibling()
    */
    virtual wxTreeItemId GetFirstChild(const wxTreeItemId& item,
                                       wxTreeItemIdValue& cookie) const;

    /**
        Returns the first visible item.
    */
    virtual wxTreeItemId GetFirstVisibleItem() const;

    /**
        Returns the item last clicked or otherwise selected.
        Unlike GetSelection(), it can be used whether or not
        the control has the @c wxTR_MULTIPLE style.

        @since 2.9.1
    */
    virtual wxTreeItemId GetFocusedItem() const;


    /**
        Clears the currently focused item

        @since 2.9.1
    */
    virtual void ClearFocusedItem();

    /**
        Sets the currently focused item.

        @param item
            The item to make the current one. It must be valid.
        @since 2.9.1
    */
    virtual void SetFocusedItem(const wxTreeItemId& item);


    /**
        Returns the normal image list.
    */
    wxImageList* GetImageList() const;

    /**
        Returns the current tree control indentation.
    */
    virtual unsigned int GetIndent() const;

    /**
        Returns the current tree control spacing.  This is the number of
        horizontal pixels between the buttons and the state images.
    */
    unsigned int GetSpacing() const;

    /**
        Returns the background colour of the item.
    */
    virtual wxColour GetItemBackgroundColour(const wxTreeItemId& item) const;

    /**
        Returns the tree item data associated with the item.

        @see wxTreeItemData

        @beginWxPerlOnly
        wxPerl provides the following shortcut method:
        - GetPlData(item): returns the Perl data
          associated with the Wx::TreeItemData. It is just the same as
          tree->GetItemData(item)->GetData().
        @endWxPerlOnly
    */
    virtual wxTreeItemData* GetItemData(const wxTreeItemId& item) const;

    /**
        Returns the font of the item label.

        If the font hadn't been explicitly set for the specified @a item with
        SetItemFont(), returns an invalid ::wxNullFont font. GetFont() can be
        used to retrieve the global tree control font used for the items
        without any specific font.
    */
    virtual wxFont GetItemFont(const wxTreeItemId& item) const;

    /**
        Gets the specified item image. The value of @a which may be:
        - ::wxTreeItemIcon_Normal: to get the normal item image.
        - ::wxTreeItemIcon_Selected: to get the selected item image (i.e. the
            image which is shown when the item is currently selected).
        - ::wxTreeItemIcon_Expanded: to get the expanded image (this only makes
            sense for items which have children - then this image is shown when
            the item is expanded and the normal image is shown when it is
            collapsed).
        - ::wxTreeItemIcon_SelectedExpanded: to get the selected expanded image
            (which is shown when an expanded item is currently selected).
    */
    virtual int GetItemImage(const wxTreeItemId& item,
                             wxTreeItemIcon which = wxTreeItemIcon_Normal) const;

    /**
        Returns the item's parent.
    */
    virtual wxTreeItemId GetItemParent(const wxTreeItemId& item) const;

    /**
        Gets the specified item state.
    */
    int GetItemState(const wxTreeItemId& item) const;

    /**
        Returns the item label.
    */
    virtual wxString GetItemText(const wxTreeItemId& item) const;

    /**
        Returns the colour of the item label.
    */
    virtual wxColour GetItemTextColour(const wxTreeItemId& item) const;

    /**
        Returns the last child of the item (or an invalid tree item if this item
        has no children).

        @see GetFirstChild(), GetNextSibling(), GetLastChild()
    */
    virtual wxTreeItemId GetLastChild(const wxTreeItemId& item) const;

    /**
        Returns the next child; call GetFirstChild() for the first child. For
        this enumeration function you must pass in a 'cookie' parameter which is
        opaque for the application but is necessary for the library to make
        these functions reentrant (i.e. allow more than one enumeration on one
        and the same object simultaneously). The cookie passed to
        GetFirstChild() and GetNextChild() should be the same.

        Returns an invalid tree item if there are no further children.

        @beginWxPerlOnly
        In wxPerl this method returns a 2-element list
        (item, cookie) instead of modifying its parameters.
        @endWxPerlOnly

        @see GetFirstChild()
    */
    virtual wxTreeItemId GetNextChild(const wxTreeItemId& item,
                                      wxTreeItemIdValue& cookie) const;

    /**
        Returns the next sibling of the specified item; call GetPrevSibling()
        for the previous sibling.

        Returns an invalid tree item if there are no further siblings.

        @see GetPrevSibling()
    */
    virtual wxTreeItemId GetNextSibling(const wxTreeItemId& item) const;

    /**
        Returns the next visible item or an invalid item if this item is the
        last visible one.

        @note The @a item itself must be visible.
    */
    virtual wxTreeItemId GetNextVisible(const wxTreeItemId& item) const;

    /**
        Returns the previous sibling of the specified item; call
        GetNextSibling() for the next sibling.

        Returns an invalid tree item if there are no further children.

        @see GetNextSibling()
    */
    virtual wxTreeItemId GetPrevSibling(const wxTreeItemId& item) const;

    /**
        Returns the previous visible item or an invalid item if this item is the
        first visible one.

        @note The @a item itself must be visible.
    */
    virtual wxTreeItemId GetPrevVisible(const wxTreeItemId& item) const;

    /**
        Returns @true if the control will use a quick calculation for the best
        size, looking only at the first and last items. The default is @false.

        @see SetQuickBestSize()
    */
    bool GetQuickBestSize() const;

    /**
        Returns the root item for the tree control.
    */
    virtual wxTreeItemId GetRootItem() const;

    /**
        Returns the selection, or an invalid item if there is no selection. This
        function only works with the controls without @c wxTR_MULTIPLE style,
        use GetSelections() for the controls which do have this style
        or, if a single item is wanted, use GetFocusedItem().
    */
    virtual wxTreeItemId GetSelection() const;

    /**
        Fills the array of tree items passed in with the currently selected
        items. This function can be called only if the control has the @c
        wxTR_MULTIPLE style.

        Returns the number of selected items.

        @beginWxPerlOnly
        In wxPerl this method takes no parameters and returns a list of
        @c Wx::TreeItemId.
        @endWxPerlOnly
    */
    virtual size_t GetSelections(wxArrayTreeItemIds& selection) const;

    /**
        Returns the state image list (from which application-defined state
        images are taken).
    */
    wxImageList* GetStateImageList() const;

    /**
        Calculates which (if any) item is under the given @a point, returning
        the tree item id at this point plus extra information @a flags. @a flags
        is a bitlist of the following:

        - @c wxTREE_HITTEST_ABOVE: Above the client area.
        - @c wxTREE_HITTEST_BELOW: Below the client area.
        - @c wxTREE_HITTEST_NOWHERE: In the client area but below the last item.
        - @c wxTREE_HITTEST_ONITEMBUTTON: On the button associated with an item.
        - @c wxTREE_HITTEST_ONITEMICON: On the bitmap associated with an item.
        - @c wxTREE_HITTEST_ONITEMINDENT: In the indentation associated with an item.
        - @c wxTREE_HITTEST_ONITEMLABEL: On the label (string) associated with an item.
        - @c wxTREE_HITTEST_ONITEMRIGHT: In the area to the right of an item.
        - @c wxTREE_HITTEST_ONITEMSTATEICON: On the state icon for a tree view
          item that is in a user-defined state.
        - @c wxTREE_HITTEST_TOLEFT: To the right of the client area.
        - @c wxTREE_HITTEST_TORIGHT: To the left of the client area.

        @beginWxPerlOnly
        In wxPerl this method only takes the @a point parameter
        and returns a 2-element list (item, flags).
        @endWxPerlOnly
    */
    wxTreeItemId HitTest(const wxPoint& point, int& flags) const;


    /**
        Inserts an item after a given one (@a previous).

        The @a image and @a selImage parameters are an index within the normal
        image list specifying the image to use for unselected and selected
        items, respectively. If @a image > -1 and @a selImage is -1, the same
        image is used for both selected and unselected items.
    */
    wxTreeItemId InsertItem(const wxTreeItemId& parent,
                            const wxTreeItemId& previous,
                            const wxString& text,
                            int image = -1,
                            int selImage = -1,
                            wxTreeItemData* data = NULL);

    /**
        Inserts an item before one identified
        by its position (@a pos). @a pos must be less than or equal to
        the number of children.

        The @a image and @a selImage parameters are an index within the normal
        image list specifying the image to use for unselected and selected
        items, respectively. If @a image > -1 and @a selImage is -1, the same
        image is used for both selected and unselected items.
    */
    wxTreeItemId InsertItem(const wxTreeItemId& parent,
                            size_t pos,
                            const wxString& text,
                            int image = -1,
                            int selImage = -1,
                            wxTreeItemData* data = NULL);

    /**
        Returns @true if the given item is in bold state.

        @see SetItemBold()
    */
    virtual bool IsBold(const wxTreeItemId& item) const;

    /**
        Returns @true if the control is empty (i.e.\ has no items, even no root
        one).
    */
    bool IsEmpty() const;

    /**
        Returns @true if the item is expanded (only makes sense if it has
        children).
    */
    virtual bool IsExpanded(const wxTreeItemId& item) const;

    /**
        Returns @true if the item is selected.
    */
    virtual bool IsSelected(const wxTreeItemId& item) const;

    /**
        Returns @true if the item is visible on the screen.
    */
    virtual bool IsVisible(const wxTreeItemId& item) const;

    /**
        Returns @true if the item has children.
    */
    virtual bool ItemHasChildren(const wxTreeItemId& item) const;

    /**
        Override this function in the derived class to change the sort order of
        the items in the tree control. The function should return a negative,
        zero or positive value if the first item is less than, equal to or
        greater than the second one.

        Please note that you @b must use wxRTTI macros wxDECLARE_DYNAMIC_CLASS()
        and wxIMPLEMENT_DYNAMIC_CLASS() if you override this function because
        otherwise the base class considers that it is not overridden and uses
        the default comparison, i.e. sorts the items alphabetically, which
        allows it optimize away the calls to the virtual function completely.

        @see SortChildren()
    */
    virtual int OnCompareItems(const wxTreeItemId& item1,
                               const wxTreeItemId& item2);

    /**
        Appends an item as the first child of @a parent, return a new item id.

        The @a image and @a selImage parameters are an index within the normal
        image list specifying the image to use for unselected and selected
        items, respectively. If @a image > -1 and @a selImage is -1, the same
        image is used for both selected and unselected items.
    */
    wxTreeItemId PrependItem(const wxTreeItemId& parent,
                             const wxString& text,
                             int image = -1,
                             int selImage = -1,
                             wxTreeItemData* data = NULL);

    /**
        Scrolls the specified item into view.

        Note that this method doesn't work while the window is frozen (See
        wxWindow::Freeze()), at least under MSW.

        @see EnsureVisible()
    */
    virtual void ScrollTo(const wxTreeItemId& item);

    /**
        Selects the given item.

        In multiple selection controls, can be also used to deselect a
        currently selected item if the value of @a select is @false.

        Notice that calling this method will generate
        @c wxEVT_TREE_SEL_CHANGING and @c wxEVT_TREE_SEL_CHANGED
        events and that the change could be vetoed by the former event handler.
    */
    virtual void SelectItem(const wxTreeItemId& item, bool select = true);

    /**
        Sets the buttons image list (from which application-defined button
        images are taken).

        The button images assigned with this method will @b not be deleted by
        @ref wxTreeCtrl "wxTreeCtrl"'s destructor, you must delete it yourself.
        Setting or assigning the button image list enables the display of image
        buttons. Once enabled, the only way to disable the display of button
        images is to set the button image list to @NULL.

        @note This function is only available in the generic version.

        @see AssignButtonsImageList().
    */
    void SetButtonsImageList(wxImageList* imageList);

    /**
        Sets the normal image list. The image list assigned with this method
        will @b not be deleted by @ref wxTreeCtrl "wxTreeCtrl"'s destructor, you
        must delete it yourself.

        @see AssignImageList().
    */
    virtual void SetImageList(wxImageList* imageList);

    /**
        Sets the indentation for the tree control.
    */
    virtual void SetIndent(unsigned int indent);

    /**
        Sets the spacing for the tree control. Spacing is the number of
        horizontal pixels between the buttons and the state images.
        This has no effect under wxMSW.
    */
    void SetSpacing(unsigned int spacing);

    /**
        Sets the colour of the item's background.
    */
    virtual void SetItemBackgroundColour(const wxTreeItemId& item,
                                         const wxColour& col);

    /**
        Makes item appear in bold font if @a bold parameter is @true or resets
        it to the normal state.

        @see IsBold()
    */
    virtual void SetItemBold(const wxTreeItemId& item, bool bold = true);

    /**
        Sets the item client data.

        Notice that the client data previously associated with the @a item (if
        any) is @em not freed by this function and so calling this function
        multiple times for the same item will result in memory leaks unless you
        delete the old item data pointer yourself.

        @beginWxPerlOnly
        wxPerl provides the following shortcut method:
        - SetPlData(item, data): sets the Perl data
          associated with the @c Wx::TreeItemData. It is just the same as
          tree->GetItemData(item)->SetData(data).
        @endWxPerlOnly
    */
    virtual void SetItemData(const wxTreeItemId& item, wxTreeItemData* data);


    /**
        Gives the item the visual feedback for Drag'n'Drop actions, which is
        useful if something is dragged from the outside onto the tree control
        (as opposed to a DnD operation within the tree control, which already
        is implemented internally).
    */
    virtual void SetItemDropHighlight(const wxTreeItemId& item,
                                      bool highlight = true);

    /**
        Sets the item's font. All items in the tree should have the same height
        to avoid text clipping, so the fonts height should be the same for all
        of them, although font attributes may vary.

        @see SetItemBold()
    */
    virtual void SetItemFont(const wxTreeItemId& item, const wxFont& font);

    /**
        Force appearance of the button next to the item. This is useful to
        allow the user to expand the items which don't have any children now,
        but instead adding them only when needed, thus minimizing memory
        usage and loading time.
    */
    virtual void SetItemHasChildren(const wxTreeItemId& item,
                                    bool hasChildren = true);

    /**
        Sets the specified item's image. See GetItemImage() for the description
        of the @a which parameter.
    */
    virtual void SetItemImage(const wxTreeItemId& item, int image,
                              wxTreeItemIcon which = wxTreeItemIcon_Normal);

    /**
        Sets the specified item state. The value of @a state may be an index
        into the state image list, or one of the special values:
        - @c wxTREE_ITEMSTATE_NONE: to disable the item state (the state image will
            be not displayed).
        - @c wxTREE_ITEMSTATE_NEXT: to set the next item state.
        - @c wxTREE_ITEMSTATE_PREV: to set the previous item state.
    */
    void SetItemState(const wxTreeItemId& item, int state);

    /**
        Sets the item label.
    */
    virtual void SetItemText(const wxTreeItemId& item, const wxString& text);

    /**
        Sets the colour of the item's text.
    */
    virtual void SetItemTextColour(const wxTreeItemId& item,
                                   const wxColour& col);

    /**
        If @true is passed, specifies that the control will use a quick
        calculation for the best size, looking only at the first and last items.
        Otherwise, it will look at all items. The default is @false.

        @see GetQuickBestSize()
    */
    void SetQuickBestSize(bool quickBestSize);

    /**
        Sets the state image list (from which application-defined state images
        are taken). Image list assigned with this method will @b not be deleted
        by @ref wxTreeCtrl "wxTreeCtrl"'s destructor, you must delete it
        yourself.

        @see AssignStateImageList().
    */
    virtual void SetStateImageList(wxImageList* imageList);

    /**
        Sets the mode flags associated with the display of the tree control. The
        new mode takes effect immediately.

        @note Generic only; MSW ignores changes.
    */
    void SetWindowStyle(long styles);

    /**
        Sorts the children of the given item using OnCompareItems().
        You should override that method to change the sort order (the default is
        ascending case-sensitive alphabetical order).

        @see wxTreeItemData, OnCompareItems()
    */
    virtual void SortChildren(const wxTreeItemId& item);

    /**
        Toggles the given item between collapsed and expanded states.
    */
    virtual void Toggle(const wxTreeItemId& item);

    /**
        Toggles the given item between selected and unselected states. For
        multiselection controls only.
    */
    void ToggleItemSelection(const wxTreeItemId& item);

    /**
        Removes the selection from the currently selected item (if any).
    */
    virtual void Unselect();

    /**
        This function either behaves the same as Unselect() if the control
        doesn't have @c wxTR_MULTIPLE style, or removes the selection from all
        items if it does have this style.
    */
    virtual void UnselectAll();

    /**
        Unselects the given item. This works in multiselection controls only.
    */
    void UnselectItem(const wxTreeItemId& item);

    /**
        Select all the immediate children of the given parent.

        This function can be used with multiselection controls only.

        @since 2.9.1
    */
    virtual void SelectChildren(const wxTreeItemId& parent);
};



/**
    @class wxTreeEvent

    A tree event holds information about events associated with wxTreeCtrl
    objects.

    To process input from a tree control, use these event handler macros to
    direct input to member functions that take a wxTreeEvent argument.

    @beginEventTable{wxTreeEvent}
    @event{EVT_TREE_BEGIN_DRAG(id, func)}
        Begin dragging with the left mouse button. If you want to enable
        left-dragging you need to intercept this event and explicitly call
        wxTreeEvent::Allow(), as it's vetoed by default. Also notice that the
        control must have an associated image list (see SetImageList()) to
        drag its items under MSW.
    @event{EVT_TREE_BEGIN_RDRAG(id, func)}
        Begin dragging with the right mouse button. If you want to enable
        right-dragging you need to intercept this event and explicitly call
        wxTreeEvent::Allow(), as it's vetoed by default.
    @event{EVT_TREE_END_DRAG(id, func)}
        End dragging with the left or right mouse button.
    @event{EVT_TREE_BEGIN_LABEL_EDIT(id, func)}
        Begin editing a label. This can be prevented by calling Veto().
    @event{EVT_TREE_END_LABEL_EDIT(id, func)}
        Finish editing a label. This can be prevented by calling Veto().
    @event{EVT_TREE_DELETE_ITEM(id, func)}
        Delete an item.
    @event{EVT_TREE_GET_INFO(id, func)}
        Request information from the application.
    @event{EVT_TREE_SET_INFO(id, func)}
        Information is being supplied.
    @event{EVT_TREE_ITEM_ACTIVATED(id, func)}
        The item has been activated, i.e. chosen by double clicking it with
        mouse or from keyboard.
    @event{EVT_TREE_ITEM_COLLAPSED(id, func)}
        The item has been collapsed.
    @event{EVT_TREE_ITEM_COLLAPSING(id, func)}
        The item is being collapsed. This can be prevented by calling Veto().
    @event{EVT_TREE_ITEM_EXPANDED(id, func)}
        The item has been expanded.
    @event{EVT_TREE_ITEM_EXPANDING(id, func)}
        The item is being expanded. This can be prevented by calling Veto().
    @event{EVT_TREE_ITEM_RIGHT_CLICK(id, func)}
        The user has clicked the item with the right mouse button.
    @event{EVT_TREE_ITEM_MIDDLE_CLICK(id, func)}
        The user has clicked the item with the middle mouse button.
    @event{EVT_TREE_SEL_CHANGED(id, func)}
        Selection has changed.
    @event{EVT_TREE_SEL_CHANGING(id, func)}
        Selection is changing. This can be prevented by calling Veto().
    @event{EVT_TREE_KEY_DOWN(id, func)}
        A key has been pressed.
    @event{EVT_TREE_ITEM_GETTOOLTIP(id, func)}
        The opportunity to set the item tooltip is being given to the
        application (call SetToolTip()). Windows only.
    @event{EVT_TREE_ITEM_MENU(id, func)}
        The context menu for the selected item has been requested, either by a
        right click or by using the menu key.
    @event{EVT_TREE_STATE_IMAGE_CLICK(id, func)}
        The state image has been clicked.
    @endEventTable

    @library{wxcore}
    @category{events}

    @see wxTreeCtrl
*/
class wxTreeEvent : public wxNotifyEvent
{
public:
    /**
        Constructor, used by wxWidgets itself only.
    */
    wxTreeEvent(wxEventType commandType, wxTreeCtrl* tree,
                const wxTreeItemId& item = wxTreeItemId());

    /**
        Returns the item (valid for all events).
    */
    wxTreeItemId GetItem() const;

    /**
        Returns the key code if the event is a key event. Use GetKeyEvent() to
        get the values of the modifier keys for this event (i.e. Shift or Ctrl).
    */
    int GetKeyCode() const;

    /**
        Returns the key event for @c EVT_TREE_KEY_DOWN events.
    */
    const wxKeyEvent& GetKeyEvent() const;

    /**
        Returns the label if the event is a begin or end edit label event.
    */
    const wxString& GetLabel() const;

    /**
        Returns the old item index (valid for @c EVT_TREE_SEL_CHANGING and
        @c EVT_TREE_SEL_CHANGED events).
    */
    wxTreeItemId GetOldItem() const;

    /**
        Returns the position of the mouse pointer if the event is a drag or
        menu-context event.

        In both cases the position is in client coordinates - i.e. relative to
        the wxTreeCtrl window (so that you can pass it directly to e.g.
        wxWindow::PopupMenu()).
    */
    wxPoint GetPoint() const;

    /**
        Returns @true if the label edit was cancelled. This should be called
        from within an @c EVT_TREE_END_LABEL_EDIT handler.
    */
    bool IsEditCancelled() const;

    /**
        Set the tooltip for the item (valid for @c EVT_TREE_ITEM_GETTOOLTIP
        events). Windows only.
    */
    void SetToolTip(const wxString& tooltip);
};


wxEventType wxEVT_TREE_BEGIN_DRAG;
wxEventType wxEVT_TREE_BEGIN_RDRAG;
wxEventType wxEVT_TREE_BEGIN_LABEL_EDIT;
wxEventType wxEVT_TREE_END_LABEL_EDIT;
wxEventType wxEVT_TREE_DELETE_ITEM;
wxEventType wxEVT_TREE_GET_INFO;
wxEventType wxEVT_TREE_SET_INFO;
wxEventType wxEVT_TREE_ITEM_EXPANDED;
wxEventType wxEVT_TREE_ITEM_EXPANDING;
wxEventType wxEVT_TREE_ITEM_COLLAPSED;
wxEventType wxEVT_TREE_ITEM_COLLAPSING;
wxEventType wxEVT_TREE_SEL_CHANGED;
wxEventType wxEVT_TREE_SEL_CHANGING;
wxEventType wxEVT_TREE_KEY_DOWN;
wxEventType wxEVT_TREE_ITEM_ACTIVATED;
wxEventType wxEVT_TREE_ITEM_RIGHT_CLICK;
wxEventType wxEVT_TREE_ITEM_MIDDLE_CLICK;
wxEventType wxEVT_TREE_END_DRAG;
wxEventType wxEVT_TREE_STATE_IMAGE_CLICK;
wxEventType wxEVT_TREE_ITEM_GETTOOLTIP;
wxEventType wxEVT_TREE_ITEM_MENU;

