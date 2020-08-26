/////////////////////////////////////////////////////////////////////////////
// Name:        wx/listctrl.h
// Purpose:     interface of wxListCtrl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/// style flags
#define wxLC_VRULES          0x0001
#define wxLC_HRULES          0x0002

#define wxLC_ICON            0x0004
#define wxLC_SMALL_ICON      0x0008
#define wxLC_LIST            0x0010
#define wxLC_REPORT          0x0020

#define wxLC_ALIGN_TOP       0x0040
#define wxLC_ALIGN_LEFT      0x0080
#define wxLC_AUTOARRANGE     0x0100
#define wxLC_VIRTUAL         0x0200
#define wxLC_EDIT_LABELS     0x0400
#define wxLC_NO_HEADER       0x0800
#define wxLC_NO_SORT_HEADER  0x1000
#define wxLC_SINGLE_SEL      0x2000
#define wxLC_SORT_ASCENDING  0x4000
#define wxLC_SORT_DESCENDING 0x8000

#define wxLC_MASK_TYPE       (wxLC_ICON | wxLC_SMALL_ICON | wxLC_LIST | wxLC_REPORT)
#define wxLC_MASK_ALIGN      (wxLC_ALIGN_TOP | wxLC_ALIGN_LEFT)
#define wxLC_MASK_SORT       (wxLC_SORT_ASCENDING | wxLC_SORT_DESCENDING)


/// Mask flags to tell app/GUI what fields of wxListItem are valid
#define wxLIST_MASK_STATE           0x0001
#define wxLIST_MASK_TEXT            0x0002
#define wxLIST_MASK_IMAGE           0x0004
#define wxLIST_MASK_DATA            0x0008
#define wxLIST_SET_ITEM             0x0010
#define wxLIST_MASK_WIDTH           0x0020
#define wxLIST_MASK_FORMAT          0x0040

/// State flags for indicating the state of an item
#define wxLIST_STATE_DONTCARE       0x0000
#define wxLIST_STATE_DROPHILITED    0x0001      // MSW only
#define wxLIST_STATE_FOCUSED        0x0002
#define wxLIST_STATE_SELECTED       0x0004
#define wxLIST_STATE_CUT            0x0008      // MSW only

/// Hit test flags, used in HitTest
#define wxLIST_HITTEST_ABOVE            0x0001  // Above the control's client area.
#define wxLIST_HITTEST_BELOW            0x0002  // Below the control's client area.
#define wxLIST_HITTEST_NOWHERE          0x0004  // Inside the control's client area but not over an item.
#define wxLIST_HITTEST_ONITEMICON       0x0020  // Over an item's icon.
#define wxLIST_HITTEST_ONITEMLABEL      0x0080  // Over an item's text.
#define wxLIST_HITTEST_ONITEMSTATEICON  0x0200  // Over the checkbox of an item.
#define wxLIST_HITTEST_TOLEFT           0x0400  // To the left of the control's client area.
#define wxLIST_HITTEST_TORIGHT          0x0800  // To the right of the control's client area.

#define wxLIST_HITTEST_ONITEM (wxLIST_HITTEST_ONITEMICON | wxLIST_HITTEST_ONITEMLABEL | wxLIST_HITTEST_ONITEMSTATEICON)

/// GetSubItemRect constants
#define wxLIST_GETSUBITEMRECT_WHOLEITEM -1l

/// Flags for GetNextItem (MSW only except wxLIST_NEXT_ALL)
enum
{
    wxLIST_NEXT_ABOVE,          // Searches for an item above the specified item
    wxLIST_NEXT_ALL,            // Searches for subsequent item by index
    wxLIST_NEXT_BELOW,          // Searches for an item below the specified item
    wxLIST_NEXT_LEFT,           // Searches for an item to the left of the specified item
    wxLIST_NEXT_RIGHT           // Searches for an item to the right of the specified item
};

/// Alignment flags for Arrange (MSW only except wxLIST_ALIGN_LEFT)
enum
{
    wxLIST_ALIGN_DEFAULT,
    wxLIST_ALIGN_LEFT,
    wxLIST_ALIGN_TOP,
    wxLIST_ALIGN_SNAP_TO_GRID
};

/// Column format (MSW only except wxLIST_FORMAT_LEFT)
enum wxListColumnFormat
{
    wxLIST_FORMAT_LEFT,
    wxLIST_FORMAT_RIGHT,
    wxLIST_FORMAT_CENTRE,
    wxLIST_FORMAT_CENTER = wxLIST_FORMAT_CENTRE
};

/// Autosize values for SetColumnWidth
enum
{
    wxLIST_AUTOSIZE = -1,
    wxLIST_AUTOSIZE_USEHEADER = -2      // partly supported by generic version
};

/// Flag values for GetItemRect
enum
{
    wxLIST_RECT_BOUNDS,
    wxLIST_RECT_ICON,
    wxLIST_RECT_LABEL
};

/// Flag values for FindItem (MSW only)
enum
{
    wxLIST_FIND_UP,
    wxLIST_FIND_DOWN,
    wxLIST_FIND_LEFT,
    wxLIST_FIND_RIGHT
};




/**
    @class wxListCtrl

    A list control presents lists in a number of formats: list view, report view,
    icon view and small icon view. In any case, elements are numbered from zero.
    For all these modes, the items are stored in the control and must be added to
    it using wxListCtrl::InsertItem method.

    A special case of report view quite different from the other modes of the list
    control is a virtual control in which the items data (including text, images
    and attributes) is managed by the main program and is requested by the control
    itself only when needed which allows having controls with millions of items
    without consuming much memory. To use virtual list control you must use
    wxListCtrl::SetItemCount first and override at least wxListCtrl::OnGetItemText
    (and optionally wxListCtrl::OnGetItemImage or wxListCtrl::OnGetItemColumnImage and
    wxListCtrl::OnGetItemAttr) to return the information about the items when the
    control requests it.

    Virtual list control can be used as a normal one except that no operations
    which can take time proportional to the number of items in the control happen
    -- this is required to allow having a practically infinite number of items.
    For example, in a multiple selection virtual list control, the selections won't
    be sent when many items are selected at once because this could mean iterating
    over all the items.

    Using many of wxListCtrl features is shown in the
    @ref page_samples_listctrl "corresponding sample".

    To intercept events from a list control, use the event table macros described
    in wxListEvent.

    <b>wxMac Note</b>: Starting with wxWidgets 2.8, wxListCtrl uses a native
    implementation for report mode, and uses a generic implementation for other
    modes. You can use the generic implementation for report mode as well by setting
    the @c mac.listctrl.always_use_generic system option (see wxSystemOptions) to 1.


    @beginStyleTable
    @style{wxLC_LIST}
           Multicolumn list view, with optional small icons. Columns are
           computed automatically, i.e. you don't set columns as in
           @c wxLC_REPORT. In other words, the list wraps, unlike a wxListBox.
    @style{wxLC_REPORT}
           Single or multicolumn report view, with optional header.
    @style{wxLC_VIRTUAL}
           The application provides items text on demand. May only be used
           with @c wxLC_REPORT.
    @style{wxLC_ICON}
           Large icon view, with optional labels.
    @style{wxLC_SMALL_ICON}
           Small icon view, with optional labels.
    @style{wxLC_ALIGN_TOP}
           Icons align to the top. Win32 default, Win32 only.
    @style{wxLC_ALIGN_LEFT}
           Icons align to the left.
    @style{wxLC_AUTOARRANGE}
           Icons arrange themselves. Win32 only.
    @style{wxLC_EDIT_LABELS}
           Labels are editable: the application will be notified when editing
           starts.
    @style{wxLC_NO_HEADER}
           No header in report mode.
    @style{wxLC_SINGLE_SEL}
           Single selection (default is multiple).
    @style{wxLC_SORT_ASCENDING}
           Sort in ascending order. (You must still supply a comparison callback
           in wxListCtrl::SortItems.)
    @style{wxLC_SORT_DESCENDING}
           Sort in descending order. (You must still supply a comparison callback
           in wxListCtrl::SortItems.)
    @style{wxLC_HRULES}
           Draws light horizontal rules between rows in report mode.
    @style{wxLC_VRULES}
           Draws light vertical rules between columns in report mode.
    @endStyleTable


    @beginEventEmissionTable{wxListEvent}
    @event{EVT_LIST_BEGIN_DRAG(id, func)}
           Begin dragging with the left mouse button.
          Processes a @c wxEVT_LIST_BEGIN_DRAG event type.
    @event{EVT_LIST_BEGIN_RDRAG(id, func)}
           Begin dragging with the right mouse button.
           Processes a @c wxEVT_LIST_BEGIN_RDRAG event type.
    @event{EVT_LIST_BEGIN_LABEL_EDIT(id, func)}
           Begin editing a label. This can be prevented by calling Veto().
           Processes a @c wxEVT_LIST_BEGIN_LABEL_EDIT event type.
    @event{EVT_LIST_END_LABEL_EDIT(id, func)}
           Finish editing a label. This can be prevented by calling Veto().
           Processes a @c wxEVT_LIST_END_LABEL_EDIT event type.
    @event{EVT_LIST_DELETE_ITEM(id, func)}
           An item was deleted.
           Processes a @c wxEVT_LIST_DELETE_ITEM event type.
    @event{EVT_LIST_DELETE_ALL_ITEMS(id, func)}
           All items were deleted.
           Processes a @c wxEVT_LIST_DELETE_ALL_ITEMS event type.
    @event{EVT_LIST_ITEM_SELECTED(id, func)}
           The item has been selected. Notice that the mouse is captured by the
           control itself when this event is generated, see @ref
           overview_events_with_mouse_capture "event handling overview".
           Processes a @c wxEVT_LIST_ITEM_SELECTED event type.
    @event{EVT_LIST_ITEM_DESELECTED(id, func)}
           The item has been deselected.
           Processes a @c wxEVT_LIST_ITEM_DESELECTED event type.
    @event{EVT_LIST_ITEM_ACTIVATED(id, func)}
           The item has been activated (ENTER or double click).
           Processes a @c wxEVT_LIST_ITEM_ACTIVATED event type.
    @event{EVT_LIST_ITEM_FOCUSED(id, func)}
           The currently focused item has changed.
           Processes a @c wxEVT_LIST_ITEM_FOCUSED event type.
    @event{EVT_LIST_ITEM_MIDDLE_CLICK(id, func)}
           The middle mouse button has been clicked on an item. This is
           only supported by the generic control.
           Processes a @c wxEVT_LIST_ITEM_MIDDLE_CLICK event type.
    @event{EVT_LIST_ITEM_RIGHT_CLICK(id, func)}
          The right mouse button has been clicked on an item.
          Processes a @c wxEVT_LIST_ITEM_RIGHT_CLICK event type.
    @event{EVT_LIST_KEY_DOWN(id, func)}
           A key has been pressed.
           Processes a @c wxEVT_LIST_KEY_DOWN event type.
    @event{EVT_LIST_INSERT_ITEM(id, func)}
           An item has been inserted.
           Processes a @c wxEVT_LIST_INSERT_ITEM event type.
    @event{EVT_LIST_COL_CLICK(id, func)}
           A column (m_col) has been left-clicked.
           Processes a @c wxEVT_LIST_COL_CLICK event type.
    @event{EVT_LIST_COL_RIGHT_CLICK(id, func)}
           A column (m_col) has been right-clicked.
           Processes a @c wxEVT_LIST_COL_RIGHT_CLICK event type.
    @event{EVT_LIST_COL_BEGIN_DRAG(id, func)}
           The user started resizing a column - can be vetoed.
           Processes a @c wxEVT_LIST_COL_BEGIN_DRAG event type.
    @event{EVT_LIST_COL_DRAGGING(id, func)}
           The divider between columns is being dragged.
           Processes a @c wxEVT_LIST_COL_DRAGGING event type.
    @event{EVT_LIST_COL_END_DRAG(id, func)}
           A column has been resized by the user.
           Processes a @c wxEVT_LIST_COL_END_DRAG event type.
    @event{EVT_LIST_CACHE_HINT(id, func)}
           Prepare cache for a virtual list control.
           Processes a @c wxEVT_LIST_CACHE_HINT event type.
    @event{EVT_LIST_ITEM_CHECKED(id, func)}
           The item has been checked.
           Processes a @c wxEVT_LIST_ITEM_CHECKED event type (new since wxWidgets 3.1.0).
    @event{EVT_LIST_ITEM_UNCHECKED(id, func)}
           The item has been unchecked.
           Processes a @c wxEVT_LIST_ITEM_UNCHECKED event type (new since wxWidgets 3.1.0).
    @endEventTable

    @note Under wxMSW this control uses wxSystemThemedControl for an explorer
    style appearance by default since wxWidgets 3.1.0. If this is not desired,
    you can call wxSystemThemedControl::EnableSystemTheme with @c false
    argument to disable this.

    @library{wxcore}
    @category{ctrl}
    @appearance{listctrl}

    @see @ref overview_listctrl, wxListView, wxListBox, wxTreeCtrl, wxImageList,
         wxListEvent, wxListItem, wxEditableListBox
*/
class wxListCtrl : public wxControl
{
public:
    /**
       Default constructor.
    */
    wxListCtrl();

    /**
        Constructor, creating and showing a list control.

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
        @param style
            Window style. See wxListCtrl.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxListCtrl(wxWindow* parent, wxWindowID id,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = wxLC_ICON,
               const wxValidator& validator = wxDefaultValidator,
               const wxString& name = wxListCtrlNameStr);

    /**
        Destructor, destroying the list control.
    */
    virtual ~wxListCtrl();

    /**
        Adds a new column to the list control in report view mode.

        This is just a convenient wrapper for InsertColumn() which adds the new
        column after all the existing ones without having to specify its
        position explicitly.

        @since 2.9.4
     */
    long AppendColumn(const wxString& heading,
                      wxListColumnFormat format = wxLIST_FORMAT_LEFT,
                      int width = -1);

    /**
        Arranges the items in icon or small icon view.
        This only has effect on Win32. @a flag is one of:
        - wxLIST_ALIGN_DEFAULT: Default alignment.
        - wxLIST_ALIGN_LEFT: Align to the left side of the control.
        - wxLIST_ALIGN_TOP: Align to the top side of the control.
        - wxLIST_ALIGN_SNAP_TO_GRID: Snap to grid.
    */
    bool Arrange(int flag = wxLIST_ALIGN_DEFAULT);

    /**
        Sets the image list associated with the control and takes ownership of it
        (i.e. the control will, unlike when using SetImageList(), delete the list
        when destroyed). @a which is one of @c wxIMAGE_LIST_NORMAL, @c wxIMAGE_LIST_SMALL,
        @c wxIMAGE_LIST_STATE (the last is unimplemented).

        @see SetImageList()
    */
    void AssignImageList(wxImageList* imageList, int which);

    /**
        Deletes all items and all columns.

        @note  This sends an event of type @c wxEVT_LIST_DELETE_ALL_ITEMS
               under all platforms.
    */
    void ClearAll();

    /**
        Creates the list control. See wxListCtrl() for further details.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxLC_ICON,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxListCtrlNameStr);

    /**
       Delete all columns in the list control.

       @return @true if all columns were successfully deleted, @false otherwise.
    */
    bool DeleteAllColumns();

    /**
        Deletes all items in the list control.

        This function does @e not send the @c wxEVT_LIST_DELETE_ITEM
        event because deleting many items from the control would be too slow
        then (unlike wxListCtrl::DeleteItem) but it does send the special @c
        wxEVT_LIST_DELETE_ALL_ITEMS event if the control was not empty.
        If it was already empty, nothing is done and no event is sent.

        @return @true if the items were successfully deleted or if the control
            was already empty, @false if an error occurred while deleting the
            items.
    */
    bool DeleteAllItems();

    /**
        Deletes a column.
    */
    bool DeleteColumn(int col);

    /**
        Deletes the specified item.
        This function sends the @c wxEVT_LIST_DELETE_ITEM event for the
        item being deleted.

        @see DeleteAllItems()
    */
    bool DeleteItem(long item);

    /**
        Starts editing the label of the given item.

        This function generates a @c EVT_LIST_BEGIN_LABEL_EDIT event which can be
        vetoed so that no text control will appear for in-place editing.

        If the user changed the label (i.e. s/he does not press ESC or leave
        the text control without changes, a @c EVT_LIST_END_LABEL_EDIT event
        will be sent which can be vetoed as well.
    */
    wxTextCtrl* EditLabel(long item,
                          wxClassInfo* textControlClass = wxCLASSINFO(wxTextCtrl));

    /**
        Enable alternating row background colours (also called zebra striping).

        This method can only be called for the control in virtual report mode,
        i.e. having ::wxLC_REPORT and ::wxLC_VIRTUAL styles.

        When enabling alternating colours, the appropriate colour for the even
        rows is chosen automatically depending on the default foreground and
        background colours which are used for the odd rows.

        @param enable
            If @true, enable alternating row background colours, i.e. different
            colours for the odd and even rows. If @false, disable this feature
            and use the same background colour for all rows.

        @since 2.9.5

        @see SetAlternateRowColour()
     */
    void EnableAlternateRowColours(bool enable = true);

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
        Finish editing the label.

        This method allows one to programmatically end editing a list control item
        in place. Usually it will only be called when editing is in progress,
        i.e. if GetEditControl() returns non-NULL. In particular, do not call
        it from EVT_LIST_BEGIN_LABEL_EDIT handler as the edit control is not
        yet fully created by then, just veto the event in this handler instead
        to prevent the editing from even starting.

        Notice that calling this method will result in EVT_LIST_END_LABEL_EDIT
        event being generated.

        Currently only implemented in wxMSW.

        @param cancel If @true, discard the changes made by user, as if @c
            Escape key was pressed. Otherwise, accept the changes as if @c
            Return was pressed.
        @return @true if item editing was finished or @false if no item as
            being edited.
     */
    bool EndEditLabel(bool cancel);

    /**
        Ensures this item is visible.
    */
    bool EnsureVisible(long item);

    /**
        Find an item whose label matches this string, starting from start or the
        beginning if start is @c -1. The string comparison is case insensitive.

        If @a partial is @true then this method will look for items which begin with @a str.

        @return The next matching item if any or @c -1 (wxNOT_FOUND) otherwise.
    */
    long FindItem(long start, const wxString& str,
                  bool partial = false);

    /**
        Find an item whose data matches this data, starting from start or the
        beginning if 'start' is @c -1.

        @beginWxPerlOnly
        In wxPerl this method is implemented as FindItemData(start, data).
        @endWxPerlOnly

        @return The next matching item if any or @c -1 (wxNOT_FOUND) otherwise.
    */
    long FindItem(long start, wxUIntPtr data);

    /**
        Find an item nearest this position in the specified direction,
        starting from @a start or the beginning if @a start is -1.

        @beginWxPerlOnly
        In wxPerl this method is implemented as FindItemAtPos(start, pt, direction).
        @endWxPerlOnly

        @return The next matching item if any or @c -1 (wxNOT_FOUND) otherwise.
    */
    long FindItem(long start, const wxPoint& pt, int direction);

    /**
        Gets information about this column.
        See SetItem() for more information.

        @beginWxPerlOnly
        In wxPerl this method takes only the @a col parameter and
        returns a @c Wx::ListItem (or @c undef).
        @endWxPerlOnly
    */
    bool GetColumn(int col, wxListItem& item) const;

    /**
        Returns the number of columns.
    */
    int GetColumnCount() const;

    /**
        Gets the column index from its position in visual order.

        After calling SetColumnsOrder(), the index returned by this function
        corresponds to the value of the element number @a pos in the array
        returned by GetColumnsOrder().

        Please see SetColumnsOrder() documentation for an example and
        additional remarks about the columns ordering.

        @see GetColumnOrder()
    */
    int GetColumnIndexFromOrder(int pos) const;

    /**
        Gets the column visual order position.

        This function returns the index of the column which appears at the
        given visual position, e.g. calling it with @a col equal to 0 returns
        the index of the first shown column.

        Please see SetColumnsOrder() documentation for an example and
        additional remarks about the columns ordering.

        @see GetColumnsOrder(), GetColumnIndexFromOrder()
    */
    int GetColumnOrder(int col) const;

    /**
        Gets the column width (report view only).
    */
    int GetColumnWidth(int col) const;

    /**
        Returns the array containing the orders of all columns.

        On error, an empty array is returned.

        Please see SetColumnsOrder() documentation for an example and
        additional remarks about the columns ordering.

        @see GetColumnOrder(), GetColumnIndexFromOrder()
    */
    wxArrayInt GetColumnsOrder() const;

    /**
        Gets the number of items that can fit vertically in the visible area of
        the list control (list or report view) or the total number of items in
        the list control (icon or small icon view).
    */
    int GetCountPerPage() const;

    /**
        Returns the edit control being currently used to edit a label.
        Returns @NULL if no label is being edited.

        @note It is currently only implemented for wxMSW and the generic version,
              not for the native macOS version.
    */
    wxTextCtrl* GetEditControl() const;

    /**
        Returns the specified image list. @a which may be one of:
        - wxIMAGE_LIST_NORMAL: The normal (large icon) image list.
        - wxIMAGE_LIST_SMALL: The small icon image list.
        - wxIMAGE_LIST_STATE: The user-defined state image list (unimplemented).
    */
    wxImageList* GetImageList(int which) const;

    /**
        Gets information about the item. See SetItem() for more information.

        You must call @e info.SetId() to set the ID of item you're interested in
        before calling this method, and @e info.SetMask() with the flags indicating
        what fields you need to retrieve from @a info.

        @beginWxPerlOnly
        In wxPerl this method takes as parameter the ID of the item
        and (optionally) the column, and returns a Wx::ListItem object.
        @endWxPerlOnly
    */
    bool GetItem(wxListItem& info) const;

    /**
        Returns the colour for this item.
        If the item has no specific colour, returns an invalid colour
        (and not the default background control of the control itself).

        @see GetItemTextColour()
    */
    wxColour GetItemBackgroundColour(long item) const;

    /**
        Returns the number of items in the list control.
    */
    int GetItemCount() const;

    /**
        Gets the application-defined data associated with this item.
    */
    wxUIntPtr GetItemData(long item) const;

    /**
        Returns the item's font.
    */
    wxFont GetItemFont(long item) const;

    /**
        Returns the position of the item, in icon or small icon view.

        @beginWxPerlOnly
        In wxPerl this method takes only the @a item parameter and
        returns a @c Wx::Point (or @c undef).
        @endWxPerlOnly
    */
    bool GetItemPosition(long item, wxPoint& pos) const;

    /**
        Returns the rectangle representing the item's size and position, in physical
        coordinates.

        @a code is one of wxLIST_RECT_BOUNDS, wxLIST_RECT_ICON, wxLIST_RECT_LABEL.

        @beginWxPerlOnly
        In wxPerl this method takes only the @a item and @a code parameters and
        returns a @c Wx::Rect (or @c undef).
        @endWxPerlOnly
    */
    bool GetItemRect(long item, wxRect& rect,
                     int code = wxLIST_RECT_BOUNDS) const;

    /**
        Retrieves the spacing between icons in pixels: horizontal spacing is
        returned as @c x component of the wxSize object and the vertical spacing
        as its @c y component.
    */
    wxSize GetItemSpacing() const;

    /**
        Gets the item state. For a list of state flags, see SetItem().
        The @a stateMask indicates which state flags are of interest.
    */
    int GetItemState(long item, long stateMask) const;

    /**
        Gets the item text for this item.

        @param item
            Item (zero-based) index.
        @param col
            Item column (zero-based) index. Column 0 is the default. This
            parameter is new in wxWidgets 2.9.1.
    */
    wxString GetItemText(long item, int col = 0) const;

    /**
        Returns the colour for this item.

        If the item has no specific colour, returns an invalid colour (and not the
        default foreground control of the control itself as this wouldn't allow
        distinguishing between items having the same colour as the current control
        foreground and items with default colour which, hence, have always the
        same colour as the control).
    */
    wxColour GetItemTextColour(long item) const;

    /**
        Searches for an item with the given geometry or state, starting from
        @a item but excluding the @a item itself.

        If @a item is -1, the first item that matches the specified flags will be returned.
        Returns the first item with given state following @a item or -1 if no such item found.
        This function may be used to find all selected items in the control like this:

        @code
        long item = -1;
        for ( ;; )
        {
            item = listctrl->GetNextItem(item,
                                        wxLIST_NEXT_ALL,
                                        wxLIST_STATE_SELECTED);
            if ( item == -1 )
                break;

            // this item is selected - do whatever is needed with it
            wxLogMessage("Item %ld is selected.", item);
        }
        @endcode

        @a geometry can be one of:
        - wxLIST_NEXT_ABOVE: Searches for an item above the specified item.
        - wxLIST_NEXT_ALL: Searches for subsequent item by index.
        - wxLIST_NEXT_BELOW: Searches for an item below the specified item.
        - wxLIST_NEXT_LEFT: Searches for an item to the left of the specified item.
        - wxLIST_NEXT_RIGHT: Searches for an item to the right of the specified item.

        @note this parameter is only supported by wxMSW currently and ignored on
              other platforms.

        @a state can be a bitlist of the following:
        - wxLIST_STATE_DONTCARE: Don't care what the state is.
        - wxLIST_STATE_DROPHILITED: The item indicates it is a drop target.
        - wxLIST_STATE_FOCUSED: The item has the focus.
        - wxLIST_STATE_SELECTED: The item is selected.
        - wxLIST_STATE_CUT: The item is selected as part of a cut and paste operation.
    */
    long GetNextItem(long item, int geometry = wxLIST_NEXT_ALL,
                     int state = wxLIST_STATE_DONTCARE) const;

    /**
        Returns the number of selected items in the list control.
    */
    int GetSelectedItemCount() const;

    /**
        Returns the rectangle representing the size and position, in physical
        coordinates, of the given subitem, i.e. the part of the row @a item in the
        column @a subItem.

        This method is only meaningful when the wxListCtrl is in the report mode.
        If @a subItem parameter is equal to the special value
        @c wxLIST_GETSUBITEMRECT_WHOLEITEM the return value is the same as
        for GetItemRect().

        @a code can be one of @c wxLIST_RECT_BOUNDS, @c wxLIST_RECT_ICON or
        @c wxLIST_RECT_LABEL.

        Note that using @c wxLIST_RECT_ICON with any sub-item but the first one
        isn't very useful as only the first sub-item can have an icon in
        wxListCtrl. In this case, i.e. for @c subItem > 0, this function simply
        returns an empty rectangle in @a rect.

        @since 2.7.0
    */
    bool GetSubItemRect(long item, long subItem, wxRect& rect,
                        int code = wxLIST_RECT_BOUNDS) const;

    /**
        Gets the text colour of the list control.
    */
    wxColour GetTextColour() const;

    /**
        Gets the index of the topmost visible item when in list or report view.
    */
    long GetTopItem() const;

    /**
        Returns the rectangle taken by all items in the control. In other words, if the
        controls client size were equal to the size of this rectangle, no scrollbars
        would be needed and no free space would be left.

        Note that this function only works in the icon and small icon views, not in
        list or report views (this is a limitation of the native Win32 control).
    */
    wxRect GetViewRect() const;

    /**
        Set the alternative row background colour to a specific colour.

        It is recommended to call EnableAlternateRowColours() instead of using
        these methods as native implementations of this control might support
        alternating row colours but not setting the exact colour to be used for
        them.

        As EnableAlternateRowColours(), this method can only be used with
        controls having ::wxLC_REPORT and ::wxLC_VIRTUAL styles.

        @param colour
            A valid alternative row background colour to enable alternating
            rows or invalid colour to disable them and use the same colour for
            all rows.

        @since 2.9.5
     */
    void SetAlternateRowColour(const wxColour& colour);

    /**
        Get the alternative row background colour.

        @since 3.1.0
        @see SetAlternateRowColour()
     */
    wxColour GetAlternateRowColour() const;

    /**
        Determines which item (if any) is at the specified point, giving details
        in @a flags. Returns index of the item or @c wxNOT_FOUND if no item is at
        the specified point.

        @a flags will be a combination of the following flags:
        - wxLIST_HITTEST_ABOVE: Above the control's client area.
        - wxLIST_HITTEST_BELOW: Below the control's client area.
        - wxLIST_HITTEST_TOLEFT: To the left of the control's client area.
        - wxLIST_HITTEST_TORIGHT: To the right of the control's client area.
        - wxLIST_HITTEST_NOWHERE: Inside the control's client area but not over an item.
        - wxLIST_HITTEST_ONITEMICON: Over an item's icon.
        - wxLIST_HITTEST_ONITEMLABEL: Over an item's text.
        - wxLIST_HITTEST_ONITEMSTATEICON: Over the checkbox of an item.
        - wxLIST_HITTEST_ONITEM: Combination of @c wxLIST_HITTEST_ONITEMICON,
          @c wxLIST_HITTEST_ONITEMLABEL, @c wxLIST_HITTEST_ONITEMSTATEICON.

        If @a ptrSubItem is not @NULL and the wxListCtrl is in the report
        mode the subitem (or column) number will also be provided.
        This feature is only available in version 2.7.0 or higher and is currently only
        implemented under wxMSW and requires at least comctl32.dll of version 4.70 on
        the host system or the value stored in @a ptrSubItem will be always -1.
        To compile this feature into wxWidgets library you need to have access to
        commctrl.h of version 4.70 that is provided by Microsoft.

        @beginWxPerlOnly
        In wxPerl this method only takes the @a point parameter
        and returns a 2-element list (item, flags).
        @endWxPerlOnly
    */
    long HitTest(const wxPoint& point, int& flags, long* ptrSubItem = NULL) const;

    /**
        Returns true if the control is currently using ::wxLC_REPORT style.
     */
    bool InReportView() const;

    /**
        For report view mode (only), inserts a column.

        For more details, see SetItem(). Also see InsertColumn(long, const wxString&, int, int)
        overload for a usually more convenient
        alternative to this method and the description of how the item width
        is interpreted by this method.
    */
    long InsertColumn(long col, const wxListItem& info);

    /**
        For report view mode (only), inserts a column.

        Insert a new column in the list control in report view mode at the
        given position specifying its most common attributes.

        Notice that to set the image for the column you need to use
        InsertColumn(long, const wxListItem&) overload and specify ::wxLIST_MASK_IMAGE
        in the item mask.

        @param col
            The index where the column should be inserted. Valid indices are
            from 0 up to GetColumnCount() inclusive and the latter can be used
            to append the new column after the last existing one.
        @param heading
            The string specifying the column heading.
        @param format
            The flags specifying the control heading text alignment.
        @param width
            If positive, the width of the column in pixels. Otherwise it can be
            @c wxLIST_AUTOSIZE to choose the default size for the column or @c
            wxLIST_AUTOSIZE_USEHEADER to fit the column width to @a heading or
            to extend to fill all the remaining space for the last column.
            Notice that in case of @c wxLIST_AUTOSIZE fixed width is used as
            there are no items in this column to use for determining its best
            size yet. If you want to fit the column to its contents, use
            SetColumnWidth() after adding the items with values in this column.
        @return
            The index of the inserted column or -1 if adding it failed.
    */
    long InsertColumn(long col, const wxString& heading,
                      int format = wxLIST_FORMAT_LEFT,
                      int width = wxLIST_AUTOSIZE);

    /**
        Inserts an item, returning the index of the new item if successful, -1 otherwise.

        @param info
            wxListItem object
    */
    long InsertItem(wxListItem& info);

    /**
        Insert a string item.

        @param index
            Index of the new item, supplied by the application
        @param label
            String label

        @beginWxPerlOnly
        In wxPerl this method is implemented as InsertStringItem(index, label).
        @endWxPerlOnly
    */
    long InsertItem(long index, const wxString& label);

    /**
        Insert an image item.

        @param index
            Index of the new item, supplied by the application
        @param imageIndex
            Index into the image list associated with this control and view style

        @beginWxPerlOnly
        In wxPerl this method is implemented as InsertImageItem(index, imageIndex).
        @endWxPerlOnly
    */
    long InsertItem(long index, int imageIndex);

    /**
        Insert an image/string item.

        @param index
            Index of the new item, supplied by the application
        @param label
            String label
        @param imageIndex
            Index into the image list associated with this control and view style

        @beginWxPerlOnly
        In wxPerl this method is implemented as InsertImageStringItem(index, label, imageIndex).
        @endWxPerlOnly
    */
    long InsertItem(long index, const wxString& label,
                    int imageIndex);

    /**
        Returns true if the control doesn't currently contain any items.

        Note that the control with some columns is still considered to be empty
        if it has no rows.

        @since 3.1.3
     */
    bool IsEmpty() const;

    /**
        Returns true if the control is currently in virtual report view.
     */
    bool IsVirtual() const;

    /**
        Redraws the given @e item.

        This is only useful for the virtual list controls as without calling this
        function the displayed value of the item doesn't change even when the
        underlying data does change.

        @see RefreshItems()
    */
    void RefreshItem(long item);

    /**
        Redraws the items between @a itemFrom and @e itemTo.
        The starting item must be less than or equal to the ending one.

        Just as RefreshItem() this is only useful for virtual list controls.
    */
    void RefreshItems(long itemFrom, long itemTo);

    /**
        Scrolls the list control. If in icon, small icon or report view mode,
        @a dx specifies the number of pixels to scroll. If in list view mode,
        @a dx specifies the number of columns to scroll. @a dy always specifies
        the number of pixels to scroll vertically.

        @note This method is currently only implemented in the Windows version.
    */
    bool ScrollList(int dx, int dy);

    /**
        Sets the background colour.

        Note that the wxWindow::GetBackgroundColour() function of wxWindow base
        class can be used to retrieve the current background colour.
    */
    virtual bool SetBackgroundColour(const wxColour& col);

    /**
        Sets information about this column.
        See SetItem() for more information.
    */
    bool SetColumn(int col, wxListItem& item);

    /**
        Sets the column width.

        @a width can be a width in pixels or @c wxLIST_AUTOSIZE (-1) or
        @c wxLIST_AUTOSIZE_USEHEADER (-2).

        @c wxLIST_AUTOSIZE will resize the column to the length of its longest item.

        @c wxLIST_AUTOSIZE_USEHEADER will resize the column to the length of the
        header (Win32) or 80 pixels (other platforms).

        In small or normal icon view, @a col must be -1, and the column width is set
        for all columns.
    */
    bool SetColumnWidth(int col, int width);

    /**
        Changes the order in which the columns are shown.

        By default, the columns of a list control appear on the screen in order
        of their indices, i.e. the column 0 appears first, the column 1 next
        and so on. However by using this function it is possible to arbitrarily
        reorder the columns visual order and the user can also rearrange the
        columns interactively by dragging them. In this case, the index of the
        column is not the same as its order and the functions GetColumnOrder()
        and GetColumnIndexFromOrder() should be used to translate between them.
        Notice that all the other functions still work with the column indices,
        i.e. the visual positioning of the columns on screen doesn't affect the
        code setting or getting their values for example.

        The @a orders array must have the same number elements as the number of
        columns and contain each position exactly once. Its n-th element
        contains the index of the column to be shown in n-th position, so for a
        control with three columns passing an array with elements 2, 0 and 1
        results in the third column being displayed first, the first one next
        and the second one last.

        Example of using it:
        @code
            wxListCtrl *list = new wxListCtrl(...);
            for ( int i = 0; i < 3; i++ )
                list->InsertColumn(i, wxString::Format("Column %d", i));

            wxArrayInt order(3);
            order[0] = 2;
            order[1] = 0;
            order[2] = 1;
            list->SetColumnsOrder(order);

            // now list->GetColumnsOrder() will return order and
            // list->GetColumnIndexFromOrder(n) will return order[n] and
            // list->GetColumnOrder() will return 1, 2 and 0 for the column 0,
            // 1 and 2 respectively
        @endcode

        Please notice that this function makes sense for report view only and
        currently is only implemented in wxMSW port. To avoid explicit tests
        for @c __WXMSW__ in your code, please use @c wxHAS_LISTCTRL_COLUMN_ORDER
        as this will allow it to start working under the other platforms when
        support for the column reordering is added there.

        @see GetColumnsOrder()
    */
    bool SetColumnsOrder(const wxArrayInt& orders);

    /**
        Change the font and the colours used for the list control header.

        This method can be used to change the appearance of the header shown by
        the control in report mode (unless @c wxLC_NO_HEADER style is used).

        Currently it is implemented only for wxMSW and does nothing in the
        other ports.

        @param attr The object containing the font and text and background
            colours to use. It may be default, i.e. not specify any custom font
            nor colours, to reset any previously set custom attribute.
        @return @true if the attributes have been updated or @false if this is
            not supported by the current platform.

        @since 3.1.1
    */
    bool SetHeaderAttr(const wxItemAttr& attr);

    /**
        Sets the image list associated with the control.

        @a which is one of @c wxIMAGE_LIST_NORMAL, @c wxIMAGE_LIST_SMALL,
        @c wxIMAGE_LIST_STATE (the last is unimplemented).

        This method does not take ownership of the image list, you have to
        delete it yourself.

        @see AssignImageList()
    */
    void SetImageList(wxImageList* imageList, int which);

    /**
        Check if the item is visible.

        An item is considered visible if at least one pixel of it is present
        on the screen.

        @since 3.1.3
    */
    bool IsVisible(long item) const;

    /**
        Sets the data of an item.

        Using the wxListItem's mask and state mask, you can change only selected
        attributes of a wxListCtrl item.

        @return @true if the item was successfully updated or @false if the
            update failed for some reason (e.g. an invalid item index).
    */
    bool SetItem(wxListItem& info);

    /**
        Sets an item string field at a particular column.

        @return @true if the item was successfully updated or @false if the
        update failed for some reason (e.g. an invalid item index).
    */
    bool SetItem(long index, int column, const wxString& label, int imageId = -1);

    /**
        Sets the background colour for this item.
        This function only works in report view mode.
        The colour can be retrieved using GetItemBackgroundColour().
    */
    void SetItemBackgroundColour(long item, const wxColour& col);

    /**
        Sets the image associated with the item.
        In report view, you can specify the column.
        The image is an index into the image list associated with the list control.
    */
    bool SetItemColumnImage(long item, long column, int image);

    /**
        This method can only be used with virtual list controls.

        It is used to indicate to the control the number of items it contains.
        After calling it, the main program should be ready to handle calls to
        various item callbacks (such as wxListCtrl::OnGetItemText) for all
        items in the range from 0 to @a count.

        Notice that the control is not necessarily redrawn after this call as
        it may be undesirable if an item which is not visible on the screen
        anyhow was added to or removed from a control displaying many items, if
        you do need to refresh the display you can just call Refresh() manually.
    */
    void SetItemCount(long count);

    /**
        Associates application-defined data with this item.

        Notice that this function cannot be used to associate pointers with the control
        items, use SetItemPtrData() instead.
    */
    bool SetItemData(long item, long data);

    /**
         Sets the item's font.
    */
    void SetItemFont(long item, const wxFont& font);

    /**
        Sets the unselected and selected images associated with the item.
        The images are indices into the image list associated with the list control.
    */
    bool SetItemImage(long item, int image, int selImage = -1);


    /**
        Sets the position of the item, in icon or small icon view. Windows only.
    */
    bool SetItemPosition(long item, const wxPoint& pos);

    /**
        Associates application-defined data with this item.

        The @a data parameter may be either an integer or a pointer cast to the
        @c wxUIntPtr type which is guaranteed to be large enough to be able to
        contain all integer types and pointers.

        @since 2.8.4
    */
    bool SetItemPtrData(long item, wxUIntPtr data);

    /**
        Sets the item state.

        The @a stateMask is a combination of @c wxLIST_STATE_XXX constants
        described in wxListItem documentation. For each of the bits specified
        in @a stateMask, the corresponding state is set or cleared depending on
        whether @a state argument contains the same bit or not.

        So to select an item you can use
        @code
            list->SetItemState(item, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
        @endcode
        while to deselect it you should use
        @code
            list->SetItemState(item, 0, wxLIST_STATE_SELECTED);
        @endcode

        Consider using wxListView if possible to avoid dealing with this
        error-prone and confusing method.

        Also notice that contrary to the usual rule that only user actions
        generate events, this method does generate wxEVT_LIST_ITEM_SELECTED
        event when it is used to select an item.
    */
    bool SetItemState(long item, long state, long stateMask);

    /**
        Sets the item text for this item.
    */
    void SetItemText(long item, const wxString& text);

    /**
        Sets the colour for this item.
        This function only works in report view.
        The colour can be retrieved using GetItemTextColour().
    */
    void SetItemTextColour(long item, const wxColour& col);

    /**
        Adds or removes a single window style.
    */
    void SetSingleStyle(long style, bool add = true);

    /**
        Sets the text colour of the list control.
    */
    void SetTextColour(const wxColour& col);

    /**
        Sets the whole window style, deleting all items.
    */
    void SetWindowStyleFlag(long style);

    /**
        Call this function to sort the items in the list control. Sorting is done
        using the specified @a fnSortCallBack function. This function must have the
        following prototype:
        @code
        int wxCALLBACK wxListCompareFunction(wxIntPtr item1, wxIntPtr item2, wxIntPtr sortData)
        @endcode

        It is called each time when the two items must be compared and should return 0
        if the items are equal, negative value if the first item is less than the
        second one and positive value if the first one is greater than the second one
        (the same convention as used by @c qsort(3)).

        The parameter @e item1 is the client data associated with the first item (@b NOT the index).
        The parameter @e item2 is the client data associated with the second item (@b NOT the index).
        The parameter @e data is the value passed to SortItems() itself.

        Notice that the control may only be sorted on client data associated with
        the items, so you must use SetItemData if you want to be able to sort the
        items in the control.

        Please see the @ref page_samples_listctrl for an example of using this function.

        @beginWxPerlOnly
        In wxPerl the comparison function must take just two parameters;
        however, you may use a closure to achieve an effect similar to the
        SortItems third parameter.
        @endWxPerlOnly
    */
    bool SortItems(wxListCtrlCompare fnSortCallBack, wxIntPtr data);

    /**
        Returns true if checkboxes are enabled for list items.

        @see EnableCheckBoxes()

        @since 3.1.0
    */
    bool HasCheckBoxes() const;

    /**
        Enable or disable checkboxes for list items.

        @param enable If @true, enable checkboxes, otherwise disable checkboxes.
        @return @true if checkboxes are supported, @false otherwise.

        In a list control with wxLC_VIRTUAL style you have to keep track of the
        checkbox state. When a checkbox is clicked (EVT_LIST_ITEM_CHECKED
        or EVT_LIST_ITEM_UNCHECKED) you have to update the state and refresh
        the item yourself.

        @see OnGetItemIsChecked() RefreshItem()

        @since 3.1.0
    */
    bool EnableCheckBoxes(bool enable = true);

    /**
        Return true if the checkbox for the given wxListItem is checked.

        Always returns false if checkboxes support hadn't been enabled.

        @param item Item (zero-based) index.

        @since 3.1.0
    */
    bool IsItemChecked(long item) const;

    /**
        Check or uncheck a wxListItem in a control using checkboxes.

        This method only works if checkboxes support had been successfully
        enabled using EnableCheckBoxes().

        @param item Item (zero-based) index.
        @param check If @true, check the item, otherwise uncheck.

        @since 3.1.0
    */
    void CheckItem(long item, bool check);

protected:

    /**
        This function may be overridden in the derived class for a control with
        @c wxLC_VIRTUAL style. It should return the attribute for the specified
        @c item or @NULL to use the default appearance parameters.

        wxListCtrl will not delete the pointer or keep a reference of it.
        You can return the same wxItemAttr pointer for every OnGetItemAttr() call.

        The base class version always returns @NULL.

        @see OnGetItemImage(), OnGetItemColumnImage(), OnGetItemText(),
             OnGetItemColumnAttr(), OnGetItemIsChecked()
    */
    virtual wxItemAttr* OnGetItemAttr(long item) const;

     /**
        This function may be overridden in the derived class for a control with
        @c wxLC_VIRTUAL style.

        It should return the attribute for the for the specified @a item and @a
        column or @NULL to use the default appearance parameters.

        The base class version returns @c OnGetItemAttr(item).

        @note Currently this function is only called under wxMSW, the other
            ports only support OnGetItemAttr()

        @see OnGetItemAttr(), OnGetItemText(),
             OnGetItemImage(), OnGetItemColumnImage(),
    */
    virtual wxItemAttr* OnGetItemColumnAttr(long item, long column) const;

    /**
        Override this function in the derived class for a control with
        @c wxLC_VIRTUAL and @c wxLC_REPORT styles in order to specify the image
        index for the given line and column.

        The base class version always calls OnGetItemImage() for the first column, else
        it returns -1.

        @see OnGetItemText(), OnGetItemImage(), OnGetItemAttr(),
             OnGetItemColumnAttr()
    */
    virtual int OnGetItemColumnImage(long item, long column) const;

    /**
        This function must be overridden in the derived class for a control with
        @c wxLC_VIRTUAL style having an "image list" (see SetImageList(); if the
        control doesn't have an image list, it is not necessary to override it).
        It should return the index of the items image in the controls image list
        or -1 for no image.

        In a control with @c wxLC_REPORT style, OnGetItemImage() only gets called for
        the first column of each line.

        The base class version always returns -1.

        @see OnGetItemText(), OnGetItemColumnImage(), OnGetItemAttr()
    */
    virtual int OnGetItemImage(long item) const;

    /**
        This function @b must be overridden in the derived class for a control with
        @c wxLC_VIRTUAL style. It should return the string containing the text of
        the given @a column for the specified @c item.

        @see SetItemCount(), OnGetItemImage(), OnGetItemColumnImage(), OnGetItemAttr()
    */
    virtual wxString OnGetItemText(long item, long column) const;

    /**
        This function @b must be overridden in the derived class for a control with
        @c wxLC_VIRTUAL style that uses checkboxes. It should return whether the
        checkbox of the specified @c item is checked.

        @see EnableCheckBoxes(), OnGetItemText()

        @since 3.1.2
    */
    virtual bool OnGetItemIsChecked(long item) const;
};



/**
    @class wxListEvent

    A list event holds information about events associated with wxListCtrl objects.

    @beginEventTable{wxListEvent}
    @event{EVT_LIST_BEGIN_DRAG(id, func)}
        Begin dragging with the left mouse button.
    @event{EVT_LIST_BEGIN_RDRAG(id, func)}
        Begin dragging with the right mouse button.
    @event{EVT_LIST_BEGIN_LABEL_EDIT(id, func)}
        Begin editing a label. This can be prevented by calling Veto().
    @event{EVT_LIST_END_LABEL_EDIT(id, func)}
        Finish editing a label. This can be prevented by calling Veto().
    @event{EVT_LIST_DELETE_ITEM(id, func)}
        Delete an item.
    @event{EVT_LIST_DELETE_ALL_ITEMS(id, func)}
        Delete all items.
    @event{EVT_LIST_ITEM_SELECTED(id, func)}
        The item has been selected. Notice that the mouse is captured by the
        control itself when this event is generated, see @ref
        overview_events_with_mouse_capture "event handling overview".
    @event{EVT_LIST_ITEM_DESELECTED(id, func)}
        The item has been deselected. GetIndex() may be -1 with virtual lists.
    @event{EVT_LIST_ITEM_ACTIVATED(id, func)}
        The item has been activated (ENTER or double click).
    @event{EVT_LIST_ITEM_FOCUSED(id, func)}
        The currently focused item has changed.
    @event{EVT_LIST_ITEM_MIDDLE_CLICK(id, func)}
        The middle mouse button has been clicked on an item.
    @event{EVT_LIST_ITEM_RIGHT_CLICK(id, func)}
        The right mouse button has been clicked on an item.
    @event{EVT_LIST_KEY_DOWN(id, func)}
        A key has been pressed. GetIndex() may be -1 if no item is selected.
    @event{EVT_LIST_INSERT_ITEM(id, func)}
        An item has been inserted.
    @event{EVT_LIST_COL_CLICK(id, func)}
        A column (m_col) has been left-clicked.
    @event{EVT_LIST_COL_RIGHT_CLICK(id, func)}
        A column (m_col) (which can be -1 if the click occurred outside any column)
        has been right-clicked.
    @event{EVT_LIST_COL_BEGIN_DRAG(id, func)}
        The user started resizing a column - can be vetoed.
    @event{EVT_LIST_COL_DRAGGING(id, func)}
        The divider between columns is being dragged.
    @event{EVT_LIST_COL_END_DRAG(id, func)}
        A column has been resized by the user.
    @event{EVT_LIST_CACHE_HINT(id, func)}
        Prepare cache for a virtual list control
    @event{EVT_LIST_ITEM_CHECKED(id, func)}
        The item has been checked (new since wxWidgets 3.1.0).
    @event{EVT_LIST_ITEM_UNCHECKED(id, func)}
        The item has been unchecked (new since wxWidgets 3.1.0).
    @endEventTable


    @library{wxcore}
    @category{events}

    @see wxListCtrl
*/
class wxListEvent : public wxNotifyEvent
{
public:
    /**
        Constructor.
    */
    wxListEvent(wxEventType commandType = wxEVT_NULL, int id = 0);

    /**
        For @c EVT_LIST_CACHE_HINT event only: return the first item which the
        list control advises us to cache.
    */
    long GetCacheFrom() const;

    /**
        For @c EVT_LIST_CACHE_HINT event only: return the last item (inclusive)
        which the list control advises us to cache.
    */
    long GetCacheTo() const;

    /**
        The column position: it is only used with @c COL events.

        For the column dragging events, it is the column to the left of the divider
        being dragged, for the column click events it may be -1 if the user clicked
        in the list control header outside any column.
    */
    int GetColumn() const;

    /**
        The data.
    */
    wxUIntPtr GetData() const;

    /**
        The image.
    */
    int GetImage() const;

    /**
        The item index.
    */
    long GetIndex() const;

    /**
        An item object, used by some events. See also wxListCtrl::SetItem.
    */
    const wxListItem& GetItem() const;

    /**
        Key code if the event is a keypress event.
    */
    int GetKeyCode() const;

    /**
        The (new) item label for @c EVT_LIST_END_LABEL_EDIT event.
    */
    const wxString& GetLabel() const;

    /**
        The mask.
    */
    long GetMask() const;

    /**
        The position of the mouse pointer if the event is a drag event.
    */
    wxPoint GetPoint() const;

    /**
        The text.
    */
    const wxString& GetText() const;

    /**
        This method only makes sense for @c EVT_LIST_END_LABEL_EDIT message and
        returns @true if it the label editing has been cancelled by the user
        (GetLabel() returns an empty string in this case but it doesn't allow the
        application to distinguish between really cancelling the edit and the
        admittedly rare case when the user wants to rename it to an empty string).
    */
    bool IsEditCancelled() const;


    /**
       @see GetKeyCode()
    */
    void SetKeyCode(int code);

    /**
       @see GetIndex()
    */
    void SetIndex(long index);

    /**
       @see GetColumn()
    */
    void SetColumn(int col);

    /**
       @see GetPoint()
    */
    void SetPoint(const wxPoint& point);

    /**
       @see GetItem()
    */
    void SetItem(const wxListItem& item);


    /**
       @see GetCacheFrom()
    */
    void SetCacheFrom(long cacheFrom);

    /**
       @see GetCacheTo()
    */
    void SetCacheTo(long cacheTo);

};


wxEventType wxEVT_LIST_BEGIN_DRAG;
wxEventType wxEVT_LIST_BEGIN_RDRAG;
wxEventType wxEVT_LIST_BEGIN_LABEL_EDIT;
wxEventType wxEVT_LIST_END_LABEL_EDIT;
wxEventType wxEVT_LIST_DELETE_ITEM;
wxEventType wxEVT_LIST_DELETE_ALL_ITEMS;
wxEventType wxEVT_LIST_ITEM_SELECTED;
wxEventType wxEVT_LIST_ITEM_DESELECTED;
wxEventType wxEVT_LIST_KEY_DOWN;
wxEventType wxEVT_LIST_INSERT_ITEM;
wxEventType wxEVT_LIST_COL_CLICK;
wxEventType wxEVT_LIST_ITEM_RIGHT_CLICK;
wxEventType wxEVT_LIST_ITEM_MIDDLE_CLICK;
wxEventType wxEVT_LIST_ITEM_ACTIVATED;
wxEventType wxEVT_LIST_CACHE_HINT;
wxEventType wxEVT_LIST_COL_RIGHT_CLICK;
wxEventType wxEVT_LIST_COL_BEGIN_DRAG;
wxEventType wxEVT_LIST_COL_DRAGGING;
wxEventType wxEVT_LIST_COL_END_DRAG;
wxEventType wxEVT_LIST_ITEM_FOCUSED;
wxEventType wxEVT_LIST_ITEM_CHECKED;
wxEventType wxEVT_LIST_ITEM_UNCHECKED;


/**
    @class wxListView

    This class currently simply presents a simpler to use interface for the
    wxListCtrl -- it can be thought of as a @e façade for that complicated class.

    Using it is preferable to using wxListCtrl directly whenever possible because
    in the future some ports might implement wxListView but not the full set of
    wxListCtrl features.

    Other than different interface, this class is identical to wxListCtrl.
    In particular, it uses the same events, same window styles and so on.

    @library{wxcore}
    @category{ctrl}
    @appearance{listctrl}

    @see wxListView::SetColumnImage
*/
class wxListView : public wxListCtrl
{
public:
    /**
       Default constructor.
    */
    wxListView();

    /**
        Constructor, creating and showing a listview control.

        @param parent
            Parent window. Must not be @NULL.
        @param winid
            Window identifier. The value wxID_ANY indicates a default value.
        @param pos
            Window position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Window size.
            If ::wxDefaultSize is specified then the window is sized appropriately.
        @param style
            Window style. See wxListCtrl.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxListView(wxWindow* parent, wxWindowID winid = wxID_ANY,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = wxLC_REPORT,
               const wxValidator& validator = wxDefaultValidator,
               const wxString& name = wxListCtrlNameStr);

    /**
        Destructor, destroying the listview control.
    */
    virtual ~wxListView();

    /**
        Resets the column image -- after calling this function, no image will be shown.

        @param col
            the column to clear image for

        @see SetColumnImage()
    */
    void ClearColumnImage(int col);

    /**
        Sets focus to the item with the given @a index.
    */
    void Focus(long index);

    /**
        Returns the first selected item in a (presumably) multiple selection control.
        Together with GetNextSelected() it can be used to iterate over all selected
        items in the control.

        @return The first selected item, if any, -1 otherwise.
    */
    long GetFirstSelected() const;

    /**
        Returns the currently focused item or -1 if none.

        @see IsSelected(), Focus()
    */
    long GetFocusedItem() const;

    /**
        Used together with GetFirstSelected() to iterate over all selected items
        in the control.

        @return Returns the next selected item or -1 if there are no more of them.
    */
    long GetNextSelected(long item) const;

    /**
        Returns @true if the item with the given @a index is selected,
        @false otherwise.

        @see GetFirstSelected(), GetNextSelected()
    */
    bool IsSelected(long index) const;

    /**
        Selects or unselects the given item.

        Notice that this method inherits the unusual behaviour of
        wxListCtrl::SetItemState() which sends a wxEVT_LIST_ITEM_SELECTED event
        when it is used to select an item, contrary to the usual rule that only
        the user actions result in selection.

        @param n
            the item to select or unselect
        @param on
            if @true (default), selects the item, otherwise unselects it
    */
    void Select(long n, bool on = true);

    /**
        Sets the column image for the specified column.
        To use the column images, the control must have a valid image list with
        at least one image.

        @param col
            the column to set image for
        @param image
            the index of the column image in the controls image list
    */
    void SetColumnImage(int col, int image);
};



/**
    @class wxListItem

    This class stores information about a wxListCtrl item or column.

    wxListItem is a class which contains information about:
    - Zero based item position; see SetId() and GetId().
    - Zero based column index; see SetColumn() and GetColumn().
    - The label (or header for columns); see SetText() and GetText().
    - The zero based index into an image list; see GetImage() and SetImage().
    - Application defined data; see SetData() and GetData().
    - For columns only: the width of the column; see SetWidth() and GetWidth().
    - For columns only: the format of the column; one of @c wxLIST_FORMAT_LEFT,
      @c wxLIST_FORMAT_RIGHT, @c wxLIST_FORMAT_CENTRE. See SetAlign() and GetAlign().
    - The state of the item; see SetState() and GetState().
      This is a bitlist of the following flags:
        - @c wxLIST_STATE_FOCUSED: The item has the focus.
        - @c wxLIST_STATE_SELECTED: The item is selected.
        - @c wxLIST_STATE_DONTCARE: No special flags (the value of this constant is 0).
        - @c wxLIST_STATE_DROPHILITED: The item is highlighted to receive a drop event. Win32 only.
        - @c wxLIST_STATE_CUT: The item is in the cut state. Win32 only.
    - A mask indicating which state flags are valid; this is a bitlist of the
      flags reported above for the item state. See SetStateMask() and GetStateMask().
    - A mask indicating which fields of this class are valid; see SetMask() and GetMask().
      This is a bitlist of the following flags:
        - @c wxLIST_MASK_STATE: The state field is valid.
        - @c wxLIST_MASK_TEXT: The label field is valid.
        - @c wxLIST_MASK_IMAGE: The image field is valid.
        - @c wxLIST_MASK_DATA: The application-defined data field is valid.
        - @c wxLIST_MASK_WIDTH: The column width field is valid.
        - @c wxLIST_MASK_FORMAT: The column format field is valid.

    The wxListItem object can also contain item-specific colour and font
    information: for this you need to call one of SetTextColour(), SetBackgroundColour()
    or SetFont() functions on it passing it the colour/font to use.
    If the colour/font is not specified, the default list control colour/font is used.

    @library{wxcore}
    @category{data}

    @see wxListCtrl
*/
class wxListItem : public wxObject
{
public:
    /**
        Constructor.
    */
    wxListItem();

    /**
        Resets the item state to the default.
    */
    void Clear();

    /**
        Returns the alignment for this item.
        Can be one of @c wxLIST_FORMAT_LEFT, @c wxLIST_FORMAT_RIGHT or @c wxLIST_FORMAT_CENTRE.
    */
    wxListColumnFormat GetAlign() const;

    /**
        Returns the background colour for this item.
    */
    wxColour GetBackgroundColour() const;

    /**
        Returns the zero-based column; meaningful only in report mode.
    */
    int GetColumn() const;

    /**
        Returns client data associated with the control.
        Please note that client data is associated with the item and not with subitems.
    */
    wxUIntPtr GetData() const;

    /**
        Returns the font used to display the item.
    */
    wxFont GetFont() const;

    /**
        Returns the zero-based item position.
    */
    long GetId() const;

    /**
        Returns the zero-based index of the image associated with the item into
        the image list.
    */
    int GetImage() const;

    /**
        Returns a bit mask indicating which fields of the structure are valid.

        Can be any combination of the following values:
        - wxLIST_MASK_STATE: @b GetState is valid.
        - wxLIST_MASK_TEXT: @b GetText is valid.
        - wxLIST_MASK_IMAGE: @b GetImage is valid.
        - wxLIST_MASK_DATA: @b GetData is valid.
        - wxLIST_MASK_WIDTH: @b GetWidth is valid.
        - wxLIST_MASK_FORMAT: @b GetFormat is valid.
    */
    long GetMask() const;

    /**
        Returns a bit field representing the state of the item.

        Can be any combination of:
        - wxLIST_STATE_DONTCARE: No special flags (the values of this constant is 0).
        - wxLIST_STATE_DROPHILITED: The item is highlighted to receive a drop event. Win32 only.
        - wxLIST_STATE_FOCUSED: The item has the focus.
        - wxLIST_STATE_SELECTED: The item is selected.
        - wxLIST_STATE_CUT: The item is in the cut state. Win32 only.
    */
    long GetState() const;

    /**
        Returns the label/header text.
    */
    const wxString& GetText() const;

    /**
        Returns the text colour.
    */
    wxColour GetTextColour() const;

    /**
        Meaningful only for column headers in report mode. Returns the column width.
    */
    int GetWidth() const;

    /**
        Sets the alignment for the item. See also GetAlign()
    */
    void SetAlign(wxListColumnFormat align);

    /**
        Sets the background colour for the item.
    */
    void SetBackgroundColour(const wxColour& colBack);

    /**
        Sets the zero-based column. Meaningful only in report mode.
    */
    void SetColumn(int col);

    //@{
    /**
        Sets client data for the item.
        Please note that client data is associated with the item and not with subitems.
    */
    void SetData(long data);
    void SetData(void* data);
    //@}

    /**
        Sets the font for the item.
    */
    void SetFont(const wxFont& font);

    /**
        Sets the zero-based item position.
    */
    void SetId(long id);

    /**
        Sets the zero-based index of the image associated with the item
        into the image list.
    */
    void SetImage(int image);

    /**
        Sets the mask of valid fields. See GetMask().
    */
    void SetMask(long mask);

    /**
        Sets the item state flags (note that the valid state flags are influenced
        by the value of the state mask, see wxListItem::SetStateMask).

        See GetState() for valid flag values.
    */
    void SetState(long state);

    /**
        Sets the bitmask that is used to determine which of the state flags
        are to be set. See also SetState().
    */
    void SetStateMask(long stateMask);

    /**
        Sets the text label for the item.
    */
    void SetText(const wxString& text);

    /**
        Sets the text colour for the item.
    */
    void SetTextColour(const wxColour& colText);

    /**
        Meaningful only for column headers in report mode. Sets the column width.
    */
    void SetWidth(int width);
};

