///////////////////////////////////////////////////////////////////////////////
// Name:        interface/wx/treelist.h
// Purpose:     wxTreeListCtrl class documentation
// Author:      Vadim Zeitlin
// Created:     2011-08-17
// Copyright:   (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
   wxTreeListCtrl styles.

   Notice that using wxTL_USER_3STATE implies wxTL_3STATE and wxTL_3STATE in
   turn implies wxTL_CHECKBOX.
*/
enum
{
    wxTL_SINGLE         = 0x0000,       /// This is the default anyhow.
    wxTL_MULTIPLE       = 0x0001,       /// Allow multiple selection.
    wxTL_CHECKBOX       = 0x0002,       /// Show checkboxes in the first column.
    wxTL_3STATE         = 0x0004,       /// Allow 3rd state in checkboxes.
    wxTL_USER_3STATE    = 0x0008,       /// Allow user to set 3rd state.
    /**
        Don't show the column headers.

        By default this control shows the column headers, using this class
        allows avoiding this and showing only the data.

        @since 2.9.5
     */
    wxTL_NO_HEADER      = 0x0010,

    wxTL_DEFAULT_STYLE  = wxTL_SINGLE,
    wxTL_STYLE_MASK     = wxTL_SINGLE |
                          wxTL_MULTIPLE |
                          wxTL_CHECKBOX |
                          wxTL_3STATE |
                          wxTL_USER_3STATE
};


/**
    @class wxTreeListItem

    Unique identifier of an item in wxTreeListCtrl.

    This is an opaque class which can't be used by the application in any other
    way than receiving or passing it to wxTreeListCtrl and checking for
    validity.

    @see wxTreeListCtrl

    @library{wxcore}
    @category{ctrl}

    @since 2.9.3
 */
class wxTreeListItem
{
public:
    /**
        Only the default constructor is publicly accessible.

        Default constructing a wxTreeListItem creates an invalid item.
     */
    wxTreeListItem();

    /**
        Return true if the item is valid.
     */
    bool IsOk() const;
};


/**
    @class wxTreeListItemComparator

    Class defining sort order for the items in wxTreeListCtrl.

    @see wxTreeListCtrl

    @library{wxcore}
    @category{ctrl}

    @since 2.9.3
 */
class wxTreeListItemComparator
{
public:
    /**
        Default constructor.

        Notice that this class is not copiable, comparators are not passed by
        value.
     */
    wxTreeListItemComparator();

    /**
        Pure virtual function which must be overridden to define sort order.

        The comparison function should return negative, null or positive value
        depending on whether the first item is less than, equal to or greater
        than the second one. The items should be compared using their values
        for the given column.

        @param treelist
            The control whose contents is being sorted.
        @param column
            The column of this control used for sorting.
        @param first
            First item to compare.
        @param second
            Second item to compare.
        @return
            A negative value if the first item is less than (i.e. should appear
            above) the second one, zero if the two items are equal or a
            positive value if the first item is greater than (i.e. should
            appear below) the second one.
     */
    virtual int
    Compare(wxTreeListCtrl* treelist,
            unsigned column,
            wxTreeListItem first,
            wxTreeListItem second) = 0;

    /**
        Trivial but virtual destructor.

        Although this class is not used polymorphically by wxWidgets itself,
        provide virtual dtor in case it's used like this in the user code.
     */
    virtual ~wxTreeListItemComparator();
};


/**
    Container of multiple items.
 */
typedef wxVector<wxTreeListItem> wxTreeListItems;


/**
    Special wxTreeListItem value meaning "insert before the first item".

    This value can be passed to wxTreeListCtrl::InsertItem() to achieve the
    same effect as calling wxTreeListCtrl::PrependItem().
 */
extern const wxTreeListItem wxTLI_FIRST;


/**
    Special wxTreeListItem value meaning "insert after the last item".

    This value can be passed to wxTreeListCtrl::InsertItem() to achieve the
    same effect as calling wxTreeListCtrl::AppendItem().
 */
extern const wxTreeListItem wxTLI_LAST;


/**
    @class wxTreeListCtrl

    A control combining wxTreeCtrl and wxListCtrl features.

    This is a multi-column tree control optionally supporting images and
    checkboxes for the items in the first column.

    It is currently implemented using wxDataViewCtrl internally but provides a
    much simpler interface for the common use case it addresses. Thus, one of
    the design principles for this control is simplicity and intentionally
    doesn't provide all the features of wxDataViewCtrl. Most importantly, this
    class stores all its data internally and doesn't require you to define a
    custom model for it.

    Instead, this controls works like wxTreeCtrl or non-virtual wxListCtrl and
    allows you to simply add items to it using wxTreeListCtrl::AppendItem() and
    related methods. Typically, you start by setting up the columns (you must
    have at least one) by calling wxTreeListCtrl::AppendColumn() and then add
    the items. While only the text of the first column can be specified when
    adding them, you can use wxTreeListCtrl::SetItemText() to set the text of
    the other columns.


    Unlike wxTreeCtrl or wxListCtrl this control can sort its items on its own.
    To allow user to sort the control contents by clicking on some column you
    should use wxCOL_SORTABLE flag when adding that column to the control. When
    a column with this flag is clicked, the control resorts itself using the
    values in this column. By default the sort is done using alphabetical order
    comparison of the items text, which is not always correct (e.g. this
    doesn't work for the numeric columns). To change this you may use
    SetItemComparator() method to provide a custom comparator, i.e. simply an
    object that implements comparison between the two items. The treelist
    sample shows an example of doing this. And if you need to sort the control
    programmatically, you can call SetSortColumn() method.


    Here are the styles supported by this control. Notice that using
    wxTL_USER_3STATE implies wxTL_3STATE and wxTL_3STATE in turn implies
    wxTL_CHECKBOX.

    @beginStyleTable
    @style{wxTL_SINGLE}
        Single selection, this is the default.
    @style{wxTL_MULTIPLE}
        Allow multiple selection, see GetSelections().
    @style{wxTL_CHECKBOX}
        Show the usual, 2 state, checkboxes for the items in the first column.
    @style{wxTL_3STATE}
        Show the checkboxes that can possibly be set by the program, but not
        the user, to a third, undetermined, state, for the items in the first
        column. Implies wxTL_CHECKBOX.
    @style{wxTL_USER_3STATE}
        Same as wxTL_3STATE but the user can also set the checkboxes to the
        undetermined state. Implies wxTL_3STATE.
    @style{wxTL_NO_HEADER}
        Don't show the column headers, that are shown by default. Notice that
        this style is only available since wxWidgets 2.9.5.
    @style{wxTL_DEFAULT_STYLE}
        Style used by the control by default, just wxTL_SINGLE currently.
    @endStyleTable

    @beginEventTable{wxTreeListEvent}
    @event{EVT_TREELIST_SELECTION_CHANGED(id, func)}
        Process @c wxEVT_TREELIST_SELECTION_CHANGED event and notifies
        about the selection change in the control. In the single selection case
        the item indicated by the event has been selected and previously
        selected item, if any, was deselected. In multiple selection case, the
        selection of this item has just changed (it may have been either
        selected or deselected) but notice that the selection of other items
        could have changed as well, use wxTreeListCtrl::GetSelections() to
        retrieve the new selection if necessary.
    @event{EVT_TREELIST_ITEM_EXPANDING(id, func)}
        Process @c wxEVT_TREELIST_ITEM_EXPANDING event notifying about
        the given branch being expanded. This event is sent before the
        expansion occurs and can be vetoed to prevent it from happening.
    @event{EVT_TREELIST_ITEM_EXPANDED(id, func)}
        Process @c wxEVT_TREELIST_ITEM_EXPANDED event notifying about
        the expansion of the given branch. This event is sent after the
        expansion occurs and can't be vetoed.
    @event{EVT_TREELIST_ITEM_CHECKED(id, func)}
        Process @c wxEVT_TREELIST_ITEM_CHECKED event notifying about
        the user checking or unchecking the item. You can use
        wxTreeListCtrl::GetCheckedState() to retrieve the new item state and
        wxTreeListEvent::GetOldCheckedState() to get the previous one.
    @event{EVT_TREELIST_ITEM_ACTIVATED(id, func)}
        Process @c wxEVT_TREELIST_ITEM_ACTIVATED event notifying about
        the user double clicking the item or activating it from keyboard.
    @event{EVT_TREELIST_ITEM_CONTEXT_MENU(id, func)}
        Process @c wxEVT_TREELIST_ITEM_CONTEXT_MENU event indicating
        that the popup menu for the given item should be displayed.
    @event{EVT_TREELIST_COLUMN_SORTED(id, func)}
        Process @c wxEVT_TREELIST_COLUMN_SORTED event indicating that
        the control contents has just been resorted using the specified column.
        The event doesn't carry the sort direction, use GetSortColumn() method
        if you need to know it.
    @endEventTable

    @library{wxcore}
    @category{ctrl}

    @since 2.9.3

    @see wxTreeCtrl, wxDataViewCtrl
 */
class wxTreeListCtrl : public wxWindow
{
public:
    /**
        Default constructor, call Create() later.

        This constructor is used during two-part construction process when it
        is impossible or undesirable to create the window when constructing the
        object.
     */
    wxTreeListCtrl();

    /**
        Full constructing, creating the object and its window.

        See Create() for the parameters description.
     */
    wxTreeListCtrl(wxWindow* parent,
                   wxWindowID id,
                   const wxPoint& pos = wxDefaultPosition,
                   const wxSize& size = wxDefaultSize,
                   long style = wxTL_DEFAULT_STYLE,
                   const wxString& name = wxTreeListCtrlNameStr);

    /**
        Create the control window.

        Can be only called for the objects created using the default
        constructor and exactly once.

        @param parent
            The parent window, must be non-NULL.
        @param id
            The window identifier, may be ::wxID_ANY.
        @param pos
            The initial window position, usually unused.
        @param size
            The initial window size, usually unused.
        @param style
            The window style, see their description in the class documentation.
        @param name
            The name of the window.
     */
    bool Create(wxWindow* parent,
                wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxTL_DEFAULT_STYLE,
                const wxString& name = wxTreeListCtrlNameStr);


    /**
        @name Image list methods.

        Like wxTreeCtrl and wxListCtrl this class uses wxImageList so if you
        intend to use item icons with it, you must construct wxImageList
        containing them first and then specify the indices of the icons in this
        image list when adding the items later.
     */
    //@{

    /// A constant indicating that no image should be used for an item.
    static const int NO_IMAGE = -1;

    /**
        Sets the image list and gives its ownership to the control.

        The image list assigned with this method will be automatically deleted
        by wxTreeCtrl as appropriate (i.e. it takes ownership of the list).

        @see SetImageList().
    */
    void AssignImageList(wxImageList* imageList);

    /**
        Sets the image list.

        The image list assigned with this method will @b not be deleted by the
        control itself and you will need to delete it yourself, use
        AssignImageList() to give the image list ownership to the control.

        @param imageList
            Image list to use, may be @NULL to not show any images any more.
    */
    void SetImageList(wxImageList* imageList);

    //@}


    /**
        @name Column methods.
     */
    //@{

    /**
        Add a column with the given title and attributes.

        @param title
            The column label.
        @param width
            The width of the column in pixels or the special
            wxCOL_WIDTH_AUTOSIZE value indicating that the column should adjust
            to its contents. Notice that the last column is special and will
            be always resized to fill all the space not taken by the other
            columns, i.e. the width specified here is ignored for it.
        @param align
            Alignment of both the column header and its items.
        @param flags
            Column flags, currently can include wxCOL_RESIZABLE to allow the
            user to resize the column and wxCOL_SORTABLE to allow the user to
            resort the control contents by clicking on this column.
        @return
            Index of the new column or -1 on failure.
     */
    int AppendColumn(const wxString& title,
                     int width = wxCOL_WIDTH_AUTOSIZE,
                     wxAlignment align = wxALIGN_LEFT,
                     int flags = wxCOL_RESIZABLE);

    /// Return the total number of columns.
    unsigned GetColumnCount() const;

    /**
        Delete the column with the given index.

        @param col
            Column index in 0 to GetColumnCount() (exclusive) range.
        @return
            True if the column was deleted, false if index is invalid or
            deleting the column failed for some other reason.
     */
    bool DeleteColumn(unsigned col);

    /**
        Delete all columns.

        @see DeleteAllItems()
     */
    void ClearColumns();

    /**
        Change the width of the given column.

        Set column width to either the given value in pixels or to the value
        large enough to fit all of the items if width is wxCOL_WIDTH_AUTOSIZE.

        Notice that setting the width of the last column is ignored as this
        column is always resized to fill the space left by the other columns.
     */
    void SetColumnWidth(unsigned col, int width);

    /// Get the current width of the given column in pixels.
    int GetColumnWidth(unsigned col) const;

    /**
        Get the width appropriate for showing the given text.

        This is typically used as second argument for AppendColumn() or with
        SetColumnWidth().
     */
    int WidthFor(const wxString& text) const;

    //@}


    /**
        @name Adding and removing items.

        When adding items, the parent and text of the first column of the new item
        must always be specified, the rest is optional.

        Each item can have two images: one used for closed state and another
        for opened one. Only the first one is ever used for the items that
        don't have children. And both are not set by default.

        It is also possible to associate arbitrary client data pointer with the
        new item. It will be deleted by the control when the item is deleted
        (either by an explicit DeleteItem() call or because the entire control
        is destroyed).
     */
    //@{

    /// Same as InsertItem() with wxTLI_LAST.
    wxTreeListItem AppendItem(wxTreeListItem parent,
                              const wxString& text,
                              int imageClosed = NO_IMAGE,
                              int imageOpened = NO_IMAGE,
                              wxClientData* data = NULL);

    /**
        Insert a new item into the tree.

        @param parent
            The item parent. Must be valid, may be GetRootItem().
        @param previous
            The previous item that this one should be inserted immediately
            after. It must be valid but may be one of the special values
            wxTLI_FIRST or wxTLI_LAST indicating that the item should be either
            inserted before the first child of its parent (if any) or after the
            last one.
        @param text
            The item text.
        @param imageClosed
            The normal item image, may be NO_IMAGE to not show any image.
        @param imageOpened
            The item image shown when it's in the expanded state.
        @param data
            Optional client data pointer that can be later retrieved using
            GetItemData() and will be deleted by the tree when the item itself
            is deleted.
     */
    wxTreeListItem InsertItem(wxTreeListItem parent,
                              wxTreeListItem previous,
                              const wxString& text,
                              int imageClosed = NO_IMAGE,
                              int imageOpened = NO_IMAGE,
                              wxClientData* data = NULL);

    /// Same as InsertItem() with wxTLI_FIRST.
    wxTreeListItem PrependItem(wxTreeListItem parent,
                               const wxString& text,
                               int imageClosed = NO_IMAGE,
                               int imageOpened = NO_IMAGE,
                               wxClientData* data = NULL);

    /// Delete the specified item.
    void DeleteItem(wxTreeListItem item);

    /// Delete all tree items.
    void DeleteAllItems();

    //@}


    /**
        @name Methods for the tree navigation.

        The tree has an invisible root item which is the hidden parent of all
        top-level items in the tree. Starting from it it is possible to iterate
        over all tree items using GetNextItem().

        It is also possible to iterate over just the children of the given item
        by using GetFirstChild() to get the first of them and then calling
        GetNextSibling() to retrieve all the others.
     */
    //@{

    /// Return the (never shown) root item.
    wxTreeListItem GetRootItem() const;

    /**
        Return the parent of the given item.

        All the tree items visible in the tree have valid parent items, only
        the never shown root item has no parent.
     */
    wxTreeListItem GetItemParent(wxTreeListItem item) const;

    /**
        Return the first child of the given item.

        Item may be the root item.

        Return value may be invalid if the item doesn't have any children.
     */
    wxTreeListItem GetFirstChild(wxTreeListItem item) const;

    /**
        Return the next sibling of the given item.

        Return value may be invalid if there are no more siblings.
     */
    wxTreeListItem GetNextSibling(wxTreeListItem item) const;

    /**
        Return the first item in the tree.

        This is the first child of the root item.

        @see GetNextItem()
     */
    wxTreeListItem GetFirstItem() const;

    /**
        Get item after the given one in the depth-first tree-traversal order.

        Calling this function starting with the result of GetFirstItem() allows
        iterating over all items in the tree.

        The iteration stops when this function returns an invalid item, i.e.
        @code
            for ( wxTreeListItem item = tree->GetFirstItem();
                  item.IsOk();
                  item = tree->GetNextItem(item) )
            {
                ... Do something with every tree item ...
            }
        @endcode
     */
    wxTreeListItem GetNextItem(wxTreeListItem item) const;

    //@}


    /**
        @name Items attributes
     */
    //@{

    /**
        Return the text of the given item.

        By default, returns the text of the first column but any other one can
        be specified using @a col argument.
     */
    const wxString& GetItemText(wxTreeListItem item, unsigned col = 0) const;

    /**
        Set the text of the specified column of the given item.
     */
    void SetItemText(wxTreeListItem item, unsigned col, const wxString& text);

    /**
        Set the text of the first column of the given item.
     */
    void SetItemText(wxTreeListItem item, const wxString& text);

    /**
        Set the images for the given item.

        See InsertItem() for the images parameters descriptions.
     */
    void SetItemImage(wxTreeListItem item, int closed, int opened = NO_IMAGE);

    /**
        Get the data associated with the given item.

        The returned pointer may be @NULL.

        It must not be deleted by the caller as this will be done by the
        control itself.
     */
    wxClientData* GetItemData(wxTreeListItem item) const;

    /**
        Set the data associated with the given item.

        Previous client data, if any, is deleted when this function is called
        so it may be used to delete the current item data object and reset it
        by passing @NULL as @a data argument.
     */
    void SetItemData(wxTreeListItem item, wxClientData* data);

    //@}


    /**
        @name Expanding and collapsing tree branches.

        Notice that calling neither Expand() nor Collapse() method generates
        any events.
     */
    //@{

    /**
        Expand the given tree branch.
     */
    void Expand(wxTreeListItem item);

    /**
        Collapse the given tree branch.
     */
    void Collapse(wxTreeListItem item);

    /**
        Return whether the given item is expanded.
     */
    bool IsExpanded(wxTreeListItem item) const;

    //@}


    /**
        @name Selection methods.

        The behaviour of the control is different in single selection mode (the
        default) and multi-selection mode (if @c wxTL_MULTIPLE was specified
        when creating it). Not all methods can be used in both modes and some
        of those that can don't behave in the same way in two cases.
     */
    //@{

    /**
        Return the currently selected item.

        This method can't be used with multi-selection controls, use
        GetSelections() instead.

        The return value may be invalid if no item has been selected yet. Once
        an item in a single selection control was selected, it will keep a
        valid selection.
     */
    wxTreeListItem GetSelection() const;

    /**
        Fill in the provided array with all the selected items.

        This method can be used in both single and multi-selection case.

        The previous array contents is destroyed.

        Returns the number of selected items.
     */
    unsigned GetSelections(wxTreeListItems& selections) const;

    /**
        Select the given item.

        In single selection mode, deselects any other selected items, in
        multi-selection case it adds to the selection.
     */
    void Select(wxTreeListItem item);

    /**
        Deselect the given item.

        This method can be used in multiple selection mode only.
     */
    void Unselect(wxTreeListItem item);

    /**
        Return true if the item is selected.

        This method can be used in both single and multiple selection modes.
     */
    bool IsSelected(wxTreeListItem item) const;

    /**
        Select all the control items.

        Can be only used in multi-selection mode.
     */
    void SelectAll();

    /**
        Deselect all the control items.

        Can be only used in multi-selection mode.
     */
    void UnselectAll();

    /**
        Call this to ensure that the given item is visible.

        @since 3.1.0
     */
    void EnsureVisible(wxTreeListItem item);

    //@}


    /**
        @name Checkbox handling

        Methods in this section can only be used with the controls created with
        wxTL_CHECKBOX style.
     */
    //@{

    /**
        Change the item checked state.

        @param item
            Valid non-root tree item.
        @param state
            One of wxCHK_CHECKED, wxCHK_UNCHECKED or, for the controls with
            wxTL_3STATE or wxTL_USER_3STATE styles, wxCHK_UNDETERMINED.
     */
    void CheckItem(wxTreeListItem item, wxCheckBoxState state = wxCHK_CHECKED);

    /**
        Change the checked state of the given item and all its children.

        This is the same as CheckItem() but checks or unchecks not only this
        item itself but all its children recursively as well.
     */
    void CheckItemRecursively(wxTreeListItem item,
                              wxCheckBoxState state = wxCHK_CHECKED);

    /**
        Uncheck the given item.

        This is synonymous with CheckItem(wxCHK_UNCHECKED).
     */
    void UncheckItem(wxTreeListItem item);

    /**
        Update the state of the parent item to reflect the checked state of its
        children.

        This method updates the parent of this item recursively: if this item
        and all its siblings are checked, the parent will become checked as
        well. If this item and all its siblings are unchecked, the parent will
        be unchecked. And if the siblings of this item are not all in the same
        state, the parent will be switched to indeterminate state. And then the
        same logic will be applied to the parents parent and so on recursively.

        This is typically called when the state of the given item has changed
        from EVT_TREELIST_ITEM_CHECKED() handler in the controls which have
        wxTL_3STATE flag. Notice that without this flag this function can't
        work as it would be unable to set the state of a parent with both
        checked and unchecked items so it's only allowed to call it when this
        flag is set.
     */
    void UpdateItemParentStateRecursively(wxTreeListItem item);

    /**
        Return the checked state of the item.

        The return value can be wxCHK_CHECKED, wxCHK_UNCHECKED or
        wxCHK_UNDETERMINED.
     */
    wxCheckBoxState GetCheckedState(wxTreeListItem item) const;

    /**
        Return true if all children of the given item are in the specified
        state.

        This is especially useful for the controls with @c wxTL_3STATE style to
        allow to decide whether the parent effective state should be the same
        @a state, if all its children are in it, or ::wxCHK_UNDETERMINED.

        @see UpdateItemParentStateRecursively()
     */
    bool AreAllChildrenInState(wxTreeListItem item,
                               wxCheckBoxState state) const;

    //@}

    /**
        @name Sorting.

        If some control columns were added with wxCOL_SORTABLE flag, clicking
        on them will automatically resort the control using the custom
        comparator set by SetItemComparator() or by doing alphabetical
        comparison by default.

        In any case, i.e. even if the user can't sort the control by clicking
        on its header, you may call SetSortColumn() to sort it programmatically
        and call GetSortColumn() to determine whether it's sorted now and, if
        so, by which column and in which order.
     */
    //@{

    /**
        Set the column to use for sorting and the order in which to sort.

        Calling this method resorts the control contents using the values of
        the items in the specified column. Sorting uses custom comparator set
        with SetItemComparator() or alphabetical comparison of items texts if
        none was specified.

        Notice that currently there is no way to reset sort order.

        @param col
            A valid column index.
        @param ascendingOrder
            Indicates whether the items should be sorted in ascending (A to Z)
            or descending (Z to A) order.
     */
    void SetSortColumn(unsigned col, bool ascendingOrder = true);

    /**
        Return the column currently used for sorting, if any.

        If the control is currently unsorted, the function simply returns
        @false and doesn't modify any of its output parameters.

        @param col
            Receives the index of the column used for sorting if non-@NULL.
        @param ascendingOrder
            Receives @true or @false depending on whether the items are sorted
            in ascending or descending order.
        @return
            @true if the control is sorted or @false if it isn't sorted at all.
     */
    bool GetSortColumn(unsigned* col, bool* ascendingOrder = NULL);

    /**
        Set the object to use for comparing the items.

        This object will be used when the control is being sorted because the
        user clicked on a sortable column or SetSortColumn() was called.

        The provided pointer is stored by the control so the object it points
        to must have a life-time equal or greater to that of the control
        itself. In addition, the pointer can be @NULL to stop using custom
        comparator and revert to the default alphabetical comparison.
     */
    void SetItemComparator(wxTreeListItemComparator* comparator);

    //@}


    /**
        @name View window.

        This control itself is entirely covered by the "view window" which is
        currently a wxDataViewCtrl but if you want to avoid relying on this to
        allow your code to work with later versions which might not be
        wxDataViewCtrl-based, use GetView() function only and only use
        GetDataView() if you really need to call wxDataViewCtrl methods on it.
     */
    //@{

    /**
        Return the view part of this control as a wxWindow.

        This method always returns non-@NULL pointer once the window was
        created.
     */
    wxWindow* GetView() const;

    /**
        Return the view part of this control as wxDataViewCtrl.

        This method may return @NULL in the future, non wxDataViewCtrl-based,
        versions of this class, use GetView() unless you really need to use
        wxDataViewCtrl methods on the returned object.
     */
    wxDataViewCtrl* GetDataView() const;

    //@}
};



/**
    Event generated by wxTreeListCtrl.

    @since 2.9.3
 */
class wxTreeListEvent : public wxNotifyEvent
{
public:
    wxTreeListEvent();

    /**
        Return the item affected by the event.

        This is the item being selected, expanded, checked or activated
        (depending on the event type).
     */
    wxTreeListItem GetItem() const;

    /**
        Return the previous state of the item checkbox.

        This method can be used with @c wxEVT_TREELIST_ITEM_CHECKED
        events only.

        Notice that the new state of the item can be retrieved using
        wxTreeListCtrl::GetCheckedState().
     */
    wxCheckBoxState GetOldCheckedState() const;

    /**
        Return the column affected by the event.

        This is currently only used with @c wxEVT_TREELIST_COLUMN_SORTED
        event.
     */
    unsigned GetColumn() const;
};


/**
    Type of wxTreeListEvent event handlers.

    This macro should be used with wxEvtHandler::Connect() when connecting to
    wxTreeListCtrl events.
 */
#define wxTreeListEventHandler(func) \
    wxEVENT_HANDLER_CAST(wxTreeListEventFunction, func)


wxEventType wxEVT_TREELIST_SELECTION_CHANGED;
wxEventType wxEVT_TREELIST_ITEM_EXPANDING;
wxEventType wxEVT_TREELIST_ITEM_EXPANDED;
wxEventType wxEVT_TREELIST_ITEM_CHECKED;
wxEventType wxEVT_TREELIST_ITEM_ACTIVATED;
wxEventType wxEVT_TREELIST_ITEM_CONTEXT_MENU;
wxEventType wxEVT_TREELIST_COLUMN_SORTED;
