/////////////////////////////////////////////////////////////////////////////
// Name:        wx/headerctrl.h
// Purpose:     interface of wxHeaderCtrl
// Author:      Vadim Zeitlin
// Created:     2008-12-01
// Copyright:   (c) 2008 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


enum
{
    // allow column drag and drop
    wxHD_ALLOW_REORDER = 0x0001,

    // allow hiding (and showing back) the columns using the menu shown by
    // right clicking the header
    wxHD_ALLOW_HIDE = 0x0002,

    // force putting column images on right
    wxHD_BITMAP_ON_RIGHT = 0x0004,

    // style used by default when creating the control
    wxHD_DEFAULT_STYLE = wxHD_ALLOW_REORDER
};



/**
    @class wxHeaderCtrl

    wxHeaderCtrl is the control containing the column headings which is usually
    used for display of tabular data.

    It is used as part of wxGrid, in generic version wxDataViewCtrl and report
    view of wxListCtrl but can be also used independently. In general this
    class is meant to be used as part of another control which already stores
    the column information somewhere as it can't be used directly: instead you
    need to inherit from it and implement the GetColumn() method to provide
    column information. See wxHeaderCtrlSimple for a concrete control class
    which can be used directly.

    In addition to labeling the columns, the control has the following
    features:
        - Column reordering support, either by explicitly configuring the
          columns order and calling SetColumnsOrder() or by dragging the
          columns interactively (if enabled).
        - Display of the icons in the header: this is often used to display a
          sort or reverse sort indicator when the column header is clicked.

    Notice that this control itself doesn't do anything other than displaying
    the column headers. In particular column reordering and sorting must still
    be supported by the associated control displaying the real data under the
    header. Also remember to call ScrollWindow() method of the control if the
    associated data display window has a horizontal scrollbar, otherwise the
    headers wouldn't align with the data when the window is scrolled.

    This control is implemented using the native header control under MSW
    systems and a generic implementation elsewhere.


    @section headerctrl_improvements Future Improvements

    Some features are supported by the native MSW control and so could be
    easily implemented in this version of wxHeaderCtrl but need to be
    implemented in the generic version as well to be really useful. Please let
    us know if you need or, better, plan to work on implementing, any of them:
        - Displaying bitmaps instead of or together with the text
        - Custom drawn headers
        - Filters associated with a column.


    @beginStyleTable
    @style{wxHD_ALLOW_REORDER}
        If this style is specified (it is by default), the user can reorder
        the control columns by dragging them.
    @style{wxHD_ALLOW_HIDE}
        If this style is specified, the control shows a popup menu allowing the
        user to change the columns visibility on right mouse click. Notice that
        the program can always hide or show the columns, this style only
        affects the users capability to do it.
    @style{wxHD_BITMAP_ON_RIGHT}
        The column image, if any, will be shown on the right side if this style
        is used. Note that this style is only implemented in wxMSW currently
        and doesn't do anything under the other platforms. It is available
        since wxWidgets 3.1.1.
    @style{wxHD_DEFAULT_STYLE}
        Symbolic name for the default control style, currently equal to
        @c wxHD_ALLOW_REORDER.
    @endStyleTable

    @beginEventEmissionTable{wxHeaderCtrlEvent}
    @event{EVT_HEADER_CLICK(id, func)}
        A column heading was clicked.
    @event{EVT_HEADER_RIGHT_CLICK(id, func)}
        A column heading was right clicked.
    @event{EVT_HEADER_MIDDLE_CLICK(id, func)}
        A column heading was clicked with the middle mouse button.
    @event{EVT_HEADER_DCLICK(id, func)}
        A column heading was double clicked.
    @event{EVT_HEADER_RIGHT_DCLICK(id, func)}
        A column heading was right double clicked.
    @event{EVT_HEADER_MIDDLE_DCLICK(id, func)}
        A column heading was double clicked with the middle mouse button.
    @event{EVT_HEADER_SEPARATOR_DCLICK(id, func)}
        Separator to the right of the specified column was double clicked
        (this action is commonly used to resize the column to fit its
        contents width and the control provides UpdateColumnWidthToFit() method
        to make implementing this easier).
    @event{EVT_HEADER_BEGIN_RESIZE(id, func)}
        The user started to drag the separator to the right of the column
        with the specified index (this can only happen for the columns for
        which wxHeaderColumn::IsResizeable() returns true). The event can
        be vetoed to prevent the column from being resized. If it isn't,
        the resizing and end resize (or dragging cancelled) events will be
        generated later.
    @event{EVT_HEADER_RESIZING(id, func)}
        The user is dragging the column with the specified index resizing
        it and its current width is wxHeaderCtrlEvent::GetWidth().
        The event can be vetoed to stop the dragging operation completely at
        any time.
    @event{EVT_HEADER_END_RESIZE(id, func)}
        The user stopped dragging the column by releasing the mouse.
        The column should normally be resized to the value of
        wxHeaderCtrlEvent::GetWidth().
    @event{EVT_HEADER_BEGIN_REORDER(id, func)}
        The user started to drag the column with the specified index (this
        can only happen for the controls with wxHD_ALLOW_REORDER style).
        This event can be vetoed to prevent the column from being reordered,
        otherwise the end reorder message will be generated later.
    @event{EVT_HEADER_END_REORDER(id, func)}
        The user dropped the column in its new location. The event can be
        vetoed to prevent the column from being placed at the new position
        or handled to update the display of the data in the associated
        control to match the new column location (available from
        wxHeaderCtrlEvent::GetNewOrder()).
    @event{EVT_HEADER_DRAGGING_CANCELLED(id, func)}
        The resizing or reordering operation currently in progress was
        cancelled. This can happen if the user pressed Esc key while
        dragging the mouse or the mouse capture was lost for some other
        reason. You only need to handle this event if your application
        entered into some modal mode when resizing or reordering began, in
        which case it should handle this event in addition to the matching
        end resizing or reordering ones.
    @endEventTable

    @library{wxcore}
    @category{ctrl}

    @see wxGrid, wxListCtrl, wxDataViewCtrl
*/
class wxHeaderCtrl : public wxControl
{
public:
    /**
        Default constructor not creating the underlying window.

        You must use Create() after creating the object using this constructor.
     */
    wxHeaderCtrl();

    /**
        Constructor creating the window.

        Please see Create() for the parameters documentation.
     */
    wxHeaderCtrl(wxWindow *parent,
                 wxWindowID winid = wxID_ANY,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = wxHD_DEFAULT_STYLE,
                 const wxString& name = wxHeaderCtrlNameStr);

    /**
        Create the header control window.

        @param parent
            The parent window. The header control should be typically
            positioned along the top edge of this window.
        @param winid
            Id of the control or @c wxID_ANY if you don't care.
        @param pos
            The initial position of the control.
        @param size
            The initial size of the control (usually not very useful as this
            control will typically be resized to have the same width as the
            associated data display control).
        @param style
            The control style, @c wxHD_DEFAULT_STYLE by default. Notice that
            the default style allows the user to reorder the columns by
            dragging them and you need to explicitly turn this feature off by
            using @code wxHD_DEFAULT_STYLE & ~wxHD_ALLOW_REORDER @endcode if
            this is undesirable.
        @param name
            The name of the control.
     */
    bool Create(wxWindow *parent,
                wxWindowID winid = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxHD_DEFAULT_STYLE,
                const wxString& name = wxHeaderCtrlNameStr);

    /**
        Set the number of columns in the control.

        The control will use GetColumn() to get information about all the
        new columns and refresh itself, i.e. this method also has the same
        effect as calling UpdateColumn() for all columns but it should only be
        used if the number of columns really changed.
     */
    void SetColumnCount(unsigned int count);

    /**
        Return the number of columns in the control.

        @return
            Number of columns as previously set by SetColumnCount().

        @see IsEmpty()
     */
    unsigned int GetColumnCount() const;

    /**
        Return whether the control has any columns.

        @see GetColumnCount()
     */
    bool IsEmpty() const;

    /**
        Update the column with the given index.

        When the value returned by GetColumn() changes, this method must be
        called to notify the control about the change and update the visual
        display to match the new column data.

        @param idx
            The column index, must be less than GetColumnCount().
     */
    void UpdateColumn(unsigned int idx);

    /**
        Change the columns display order.

        The display order defines the order in which the columns appear on the
        screen and does @em not affect the interpretation of indices by all the
        other class methods.

        The @a order array specifies the column indices corresponding to the
        display positions.

        @param order
            A permutation of all column indices, i.e. an array of size
            GetColumnsOrder() containing all column indices exactly once. The
            n-th element of this array defines the index of the column shown at
            the n-th position from left (for the default left-to-right writing
            direction).

        @see wxListCtrl::SetColumnsOrder()
     */
    void SetColumnsOrder(const wxArrayInt& order);

    /**
        Return the array describing the columns display order.

        For the controls without wxHD_ALLOW_REORDER style the returned array
        will be the same as was passed to SetColumnsOrder() previously or
        define the default order (with n-th element being n) if it hadn't been
        called. But for the controls with wxHD_ALLOW_REORDER style, the columns
        can be also reordered by user.
     */
    wxArrayInt GetColumnsOrder() const;

    /**
        Return the index of the column displayed at the given position.

        @param pos
            The display position, e.g. 0 for the left-most column, 1 for the
            next one and so on until GetColumnCount() - 1.

        @see GetColumnPos()
     */
    unsigned int GetColumnAt(unsigned int pos) const;

    /**
        Get the position at which this column is currently displayed.

        Notice that a valid position is returned even for the hidden columns
        currently.

        @param idx
            The column index, must be less than GetColumnCount().

        @see GetColumnAt()
     */
    unsigned int GetColumnPos(unsigned int idx) const;

    /**
        Reset the columns order to the natural one.

        After calling this function, the column with index @c idx appears at
        position @c idx in the control.
     */
    void ResetColumnsOrder();

    /**
        Helper function to manipulate the array of column indices.

        This function reshuffles the array of column indices indexed by
        positions (i.e. using the same convention as for SetColumnsOrder()) so
        that the column with the given index is found at the specified
        position.

        @param order
            Array containing the indices of columns in order of their
            positions.
        @param idx
            The index of the column to move.
        @param pos
            The new position for the column @a idx.
     */
    static void MoveColumnInOrderArray(wxArrayInt& order,
                                       unsigned int idx,
                                       unsigned int pos);

    /**
        Show the popup menu allowing the user to show or hide the columns.

        This functions shows the popup menu containing all columns with check
        marks for the ones which are currently shown and allows the user to
        check or uncheck them to toggle their visibility. It is called from the
        default EVT_HEADER_RIGHT_CLICK handler for the controls which have
        wxHD_ALLOW_HIDE style. And if the column has wxHD_ALLOW_REORDER style
        as well, the menu also contains an item to customize the columns shown
        using which results in ShowCustomizeDialog() being called, please see
        its description for more details.

        If a column was toggled, UpdateColumnVisibility() virtual function is
        called so it must be implemented for the controls with wxHD_ALLOW_HIDE
        style or if you call this function explicitly.

        @param pt
            The position of the menu, in the header window coordinates.
        @param title
            The title for the menu if not empty.
        @return
            @true if a column was shown or hidden or @false if nothing was
            done, e.g. because the menu was cancelled.
     */
    bool ShowColumnsMenu(const wxPoint& pt, const wxString& title = wxString());

    /**
        Helper function appending the checkable items corresponding to all the
        columns to the given menu.

        This function is used by ShowColumnsMenu() but can also be used if you
        show your own custom columns menu and still want all the columns shown
        in it. It appends menu items with column labels as their text and
        consecutive ids starting from @a idColumnsBase to the menu and checks
        the items corresponding to the currently visible columns.

        Example of use:
        @code
            wxMenu menu;
            menu.Append(100, "Some custom command");
            menu.AppendSeparator();
            AddColumnsItems(menu, 200);
            const int rc = GetPopupMenuSelectionFromUser(menu, pt);
            if ( rc >= 200 )
                ... toggle visibility of the column rc-200 ...
        @endcode

        @param menu
            The menu to append the items to. It may be currently empty or not.
        @param idColumnsBase
            The id for the menu item corresponding to the first column, the
            other ones are consecutive starting from it. It should be positive.
     */
    void AddColumnsItems(wxMenu& menu, int idColumnsBase = 0);

    /**
        Show the column customization dialog.

        This function displays a modal dialog containing the list of all
        columns which the user can use to reorder them as well as show or hide
        individual columns.

        If the user accepts the changes done in the dialog, the virtual
        methods UpdateColumnVisibility() and UpdateColumnsOrder() will be
        called so they must be overridden in the derived class if this method
        is ever called. Please notice that the user will be able to invoke it
        interactively from the header popup menu if the control has both
        wxHD_ALLOW_HIDE and wxHD_ALLOW_REORDER styles.

        @see wxRearrangeDialog
     */
    bool ShowCustomizeDialog();

    /**
        Returns width needed for given column's title.

        @since 2.9.4
     */
    int GetColumnTitleWidth(const wxHeaderColumn& col);

    /**
        Returns width needed for the column with the given index.

        This is just a convenient wrapper for the overload taking
        wxHeaderColumn.

        @since 3.1.3
     */
    int GetColumnTitleWidth(unsigned int idx);

protected:
    /**
        Method to be implemented by the derived classes to return the
        information for the given column.

        @param idx
            The column index, between 0 and the value last passed to
            SetColumnCount().
     */
    virtual const wxHeaderColumn& GetColumn(unsigned int idx) const = 0;

    /**
        Method called when the column visibility is changed by the user.

        This method is called from ShowColumnsMenu() or ShowCustomizeDialog()
        when the user interactively hides or shows a column. A typical
        implementation will simply update the internally stored column state.
        Notice that there is no need to call UpdateColumn() from this method as
        it is already done by wxHeaderCtrl itself.

        The base class version doesn't do anything and must be overridden if
        this method is called.

        @param idx
            The index of the column whose visibility was toggled.
        @param show
            The new visibility value, @true if the column is now shown or
            @false if it is not hidden.
     */
    virtual void UpdateColumnVisibility(unsigned int idx, bool show);

    /**
        Method called when the columns order is changed in the customization
        dialog.

        This method is only called from ShowCustomizeDialog() when the user
        changes the order of columns. In particular it is @em not called if a
        single column changes place because the user dragged it to the new
        location, the EVT_HEADER_END_REORDER event handler should be used to
        react to this.

        A typical implementation in a derived class will update the display
        order of the columns in the associated control, if any. Notice that
        there is no need to call SetColumnsOrder() from it as wxHeaderCtrl does
        it itself.

        The base class version doesn't do anything and must be overridden if
        this method is called.

        @param order
            The new column order. This array uses the same convention as
            SetColumnsOrder().
     */
    virtual void UpdateColumnsOrder(const wxArrayInt& order);

    /**
        Method which may be implemented by the derived classes to allow double
        clicking the column separator to resize the column to fit its contents.

        When a separator is double clicked, the default handler of
        EVT_HEADER_SEPARATOR_DCLICK event calls this function and refreshes the
        column if it returns @true so to implement the resizing of the column
        to fit its width on header double click you need to implement this
        method using logic similar to this example:
        @code
            class MyHeaderColumn : public wxHeaderColumn
            {
            public:
                ...

                void SetWidth(int width) { m_width = width; }
                virtual int GetWidth() const { return m_width; }

            private:
                int m_width;
            };

            class MyHeaderCtrl : public wxHeaderCtrl
            {
            public:
            protected:
                virtual wxHeaderColumn& GetColumn(unsigned int idx) const
                {
                    return m_cols[idx];
                }

                virtual bool UpdateColumnWidthToFit(unsigned int idx, int widthTitle)
                {
                    int widthContents = ... compute minimal width for column idx ...
                    m_cols[idx].SetWidth(wxMax(widthContents, widthTitle));
                    return true;
                }

                wxVector<MyHeaderColumn> m_cols;
            };
        @endcode

        Base class version simply returns @false.

        @param idx
            The zero-based index of the column to update.
        @param widthTitle
            Contains minimal width needed to display the column header itself
            and will usually be used as a starting point for the fitting width
            calculation.

        @return
            @true to indicate that the column was resized, i.e. GetColumn() now
            returns the new width value, and so must be refreshed or @false
            meaning that the control didn't reach to the separator double click.
     */
    virtual bool UpdateColumnWidthToFit(unsigned int idx, int widthTitle);

    /**
        Can be overridden in the derived class to update internal data
        structures when the number of the columns in the control changes.

        This method is called by SetColumnCount() before effectively changing
        the number of columns.

        The base class version does nothing but it is good practice to still
        call it from the overridden version in the derived class.
     */
    virtual void OnColumnCountChanging(unsigned int count);
};


/**
    @class wxHeaderCtrlSimple

    wxHeaderCtrlSimple is a concrete header control which can be used directly,
    without inheriting from it as you need to do when using wxHeaderCtrl
    itself.

    When using it, you need to use simple AppendColumn(), InsertColumn() and
    DeleteColumn() methods instead of setting the number of columns with
    SetColumnCount() and returning the information about them from the
    overridden GetColumn().

    @library{wxcore}
    @category{ctrl}

    @see wxHeaderCtrl
*/
class wxHeaderCtrlSimple : public wxHeaderCtrl
{
public:
    /**
        Default constructor not creating the underlying window.

        You must use Create() after creating the object using this constructor.
     */
    wxHeaderCtrlSimple();

    /**
        Constructor creating the window.

        Please see the base class wxHeaderCtrl::Create() method for the
        parameters description.
     */
    wxHeaderCtrlSimple(wxWindow *parent,
                       wxWindowID winid = wxID_ANY,
                       const wxPoint& pos = wxDefaultPosition,
                       const wxSize& size = wxDefaultSize,
                       long style = wxHD_DEFAULT_STYLE,
                       const wxString& name = wxHeaderCtrlNameStr);

    /**
        Insert the column at the given position.

        @param col
            The column to insert. Notice that because of the existence of
            implicit conversion from wxString to wxHeaderColumn a string
            can be passed directly here.
        @param idx
            The position of the new column, from 0 to GetColumnCount(). Using
            GetColumnCount() means to append the column to the end.

        @see AppendColumn()
     */
    void InsertColumn(const wxHeaderColumnSimple& col, unsigned int idx);

    /**
        Append the column to the end of the control.

        @see InsertColumn()
     */
    void AppendColumn(const wxHeaderColumnSimple& col);

    /**
        Delete the column at the given position.

        @see InsertColumn(), AppendColumn()
     */
    void DeleteColumn(unsigned int idx);

    /**
        Show or hide the column.

        Initially the column is shown by default or hidden if it was added with
        wxCOL_HIDDEN flag set.

        When a column is hidden, it doesn't appear at all on the screen but its
        index is still taken into account when working with other columns. E.g.
        if there are three columns 0, 1 and 2 and the column 1 is hidden you
        still need to use index 2 to refer to the last visible column.

        @param idx
            The index of the column to show or hide, from 0 to GetColumnCount().
        @param show
            Indicates whether the column should be shown (default) or hidden.
     */
    void ShowColumn(unsigned int idx, bool show = true);

    /**
        Hide the column with the given index.

        This is the same as calling @code ShowColumn(idx, false) @endcode.

        @param idx
            The index of the column to show or hide, from 0 to GetColumnCount().
     */
    void HideColumn(unsigned int idx);

    /**
        Update the column sort indicator.

        The sort indicator, if shown, is typically an arrow pointing upwards or
        downwards depending on whether the control contents is sorted in
        ascending or descending order.

        @param idx
            The column to set the sort indicator for.
            If @c -1 is given, then the currently shown sort indicator
            will be removed.
        @param sortOrder
            If @true or @false show the sort indicator corresponding to
            ascending or descending sort order respectively.
     */
    void ShowSortIndicator(unsigned int idx, bool sortOrder = true);

    /**
        Remove the sort indicator from the column being used as sort key.

        @see ShowSortIndicator
     */
    void RemoveSortIndicator();

protected:
    /**
        This function can be overridden in the classes deriving from this
        control instead of overriding UpdateColumnWidthToFit().

        To implement automatic column resizing to fit its contents width when
        the column divider is double clicked, you need to simply return the
        fitting width for the given column @a idx from this method, the control
        will automatically use the biggest value between the one returned from
        here and the one needed for the display of the column title itself.

        The base class version returns -1 indicating that this function is not
        implemented.
     */
    virtual int GetBestFittingWidth(unsigned int idx) const;
};

/**
    @class wxHeaderCtrlEvent

    Event class representing the events generated by wxHeaderCtrl.

    @library{wxcore}
    @category{events}

    @see wxHeaderCtrl
*/
class wxHeaderCtrlEvent : public wxNotifyEvent
{
public:
    wxHeaderCtrlEvent(wxEventType commandType = wxEVT_NULL, int winid = 0);
    wxHeaderCtrlEvent(const wxHeaderCtrlEvent& event);

    /**
        Return the index of the column affected by this event.

        This method can be called for all header control events.
     */
    int GetColumn() const;
    void SetColumn(int col);

    /**
        Return the current width of the column.

        This method can only be called for the dragging events.
     */
    int GetWidth() const;
    void SetWidth(int width);

    /**
        Return the new order of the column.

        This method can only be called for a reorder event for which it
        indicates the tentative new position for the column GetColumn()
        selected by the user. If the event is not vetoed, this will become the
        new column position in wxHeaderCtrl::GetColumnsOrder().
     */
    unsigned int GetNewOrder() const;
    void SetNewOrder(unsigned int order);
};



wxEventType wxEVT_HEADER_CLICK;
wxEventType wxEVT_HEADER_RIGHT_CLICK;
wxEventType wxEVT_HEADER_MIDDLE_CLICK;
wxEventType wxEVT_HEADER_DCLICK;
wxEventType wxEVT_HEADER_RIGHT_DCLICK;
wxEventType wxEVT_HEADER_MIDDLE_DCLICK;
wxEventType wxEVT_HEADER_SEPARATOR_DCLICK;
wxEventType wxEVT_HEADER_BEGIN_RESIZE;
wxEventType wxEVT_HEADER_RESIZING;
wxEventType wxEVT_HEADER_END_RESIZE;
wxEventType wxEVT_HEADER_BEGIN_REORDER;
wxEventType wxEVT_HEADER_END_REORDER;
wxEventType wxEVT_HEADER_DRAGGING_CANCELLED;
