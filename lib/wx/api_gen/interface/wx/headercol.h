/////////////////////////////////////////////////////////////////////////////
// Name:        wx/headercol.h
// Purpose:     interface of wxHeaderColumn
// Author:      Vadim Zeitlin
// Created:     2008-12-01
// Copyright:   (c) 2008 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Column width special values.
 */
enum
{
    /// Special value used for column width meaning unspecified or default.
    wxCOL_WIDTH_DEFAULT = -1,

    /**
        Size the column automatically to fit all values.

        @note On macOS, this style is only implemented in the Cocoa build on
              macOS >= 10.5; it behaves identically to wxCOL_WIDTH_DEFAULT otherwise.
     */
    wxCOL_WIDTH_AUTOSIZE = -2
};

/**
    Bit flags used as wxHeaderColumn flags.
 */
enum
{
    /// Column can be resized (included in default flags).
    wxCOL_RESIZABLE   = 1,

    /// Column can be clicked to toggle the sort order by its contents.
    wxCOL_SORTABLE    = 2,

    /// Column can be dragged to change its order (included in default).
    wxCOL_REORDERABLE = 4,

    /// Column is not shown at all.
    wxCOL_HIDDEN      = 8,

    /// Default flags for wxHeaderColumn ctor.
    wxCOL_DEFAULT_FLAGS = wxCOL_RESIZABLE | wxCOL_REORDERABLE
};

/**
    @class wxHeaderColumn

    Represents a column header in controls displaying tabular data such as
    wxDataViewCtrl or wxGrid.

    Notice that this is an abstract base class which is implemented (usually
    using the information stored in the associated control) by the different
    controls using wxHeaderCtrl. As the control only needs to retrieve the
    information about the column, this class defines only the methods for
    accessing the various column properties but not for changing them as the
    setters might not be needed at all, e.g. if the column attributes can only
    be changed via the methods of the main associated control (this is the case
    for wxGrid for example). If you do want to allow changing them directly
    using the column itself, you should inherit from wxSettableHeaderColumn
    instead of this class.

    Finally, if you don't already store the column information at all anywhere,
    you should use the concrete wxHeaderColumnSimple class and
    wxHeaderCtrlSimple.

    @library{wxcore}
    @category{ctrl}
 */
class wxHeaderColumn
{
public:
    /**
        Get the text shown in the column header.
     */
    virtual wxString GetTitle() const = 0;

    /**
        Returns the bitmap in the header of the column, if any.

        If the column has no associated bitmap, wxNullBitmap should be returned.
    */
    virtual wxBitmap GetBitmap() const = 0;

    /**
        Returns the current width of the column.

        @return
            Width of the column in pixels, never wxCOL_WIDTH_DEFAULT or
            wxCOL_WIDTH_AUTOSIZE.
    */
    virtual int GetWidth() const = 0;

    /**
        Return the minimal column width.

        @return
            The minimal width such that the user can't resize the column to
            lesser size (notice that it is still possible to set the column
            width to smaller value from the program code). Return 0 from here
            to allow resizing the column to arbitrarily small size.
     */
    virtual int GetMinWidth() const = 0;

    /**
        Returns the current column alignment.

        @return
            One of wxALIGN_CENTRE, wxALIGN_LEFT or wxALIGN_RIGHT.
     */
    virtual wxAlignment GetAlignment() const = 0;


    /**
        Get the column flags.

        This method retrieves all the flags at once, you can also use HasFlag()
        to test for any individual flag or IsResizeable(), IsSortable(),
        IsReorderable() and IsHidden() to test for particular flags.
     */
    virtual int GetFlags() const = 0;

    /**
        Return @true if the specified flag is currently set for this column.
     */
    bool HasFlag(int flag) const;


    /**
        Return true if the column can be resized by the user.

        Equivalent to HasFlag(wxCOL_RESIZABLE).
     */
    virtual bool IsResizeable() const;

    /**
        Returns @true if the column can be clicked by user to sort the control
        contents by the field in this column.

        This corresponds to wxCOL_SORTABLE flag which is off by default.
    */
    virtual bool IsSortable() const;

    /**
        Returns @true if the column can be dragged by user to change its order.

        This corresponds to wxCOL_REORDERABLE flag which is on by default.
    */
    virtual bool IsReorderable() const;

    /**
        Returns @true if the column is currently hidden.

        This corresponds to wxCOL_HIDDEN flag which is off by default.
     */
    virtual bool IsHidden() const;

    /**
        Returns @true if the column is currently shown.

        This corresponds to the absence of wxCOL_HIDDEN flag.
     */
    bool IsShown() const;


    /**
        Returns @true if the column is currently used for sorting.
     */
    virtual bool IsSortKey() const = 0;

    /**
        Returns @true, if the sort order is ascending.

        Notice that it only makes sense to call this function if the column is
        used for sorting at all, i.e. if IsSortKey() returns @true.
    */
    virtual bool IsSortOrderAscending() const = 0;
};

/**
    @class wxSettableHeaderColumn

    Adds methods to set the column attributes to wxHeaderColumn.

    This class adds setters for the column attributes defined by
    wxHeaderColumn. It is still an abstract base class and needs to be
    implemented before using it with wxHeaderCtrl.

    @library{wxcore}
    @category{ctrl}
 */
class wxSettableHeaderColumn : public wxHeaderColumn
{
public:
    /**
        Set the text to display in the column header.
     */
    virtual void SetTitle(const wxString& title) = 0;

    /**
        Set the bitmap to be displayed in the column header.

        Notice that the bitmaps displayed in different columns of the same
        control must all be of the same size.
     */
    virtual void SetBitmap(const wxBitmap& bitmap) = 0;

    /**
        Set the column width.

        @param width
            The column width in pixels or the special wxCOL_WIDTH_DEFAULT
            (meaning to use default width) or wxCOL_WIDTH_AUTOSIZE (size to
            fit the content) value.
     */
    virtual void SetWidth(int width) = 0;

    /**
        Set the minimal column width.

        This method can be used with resizable columns (i.e. those for which
        wxCOL_RESIZABLE flag is set in GetFlags() or, alternatively,
        IsResizeable() returns @true) to prevent the user from making them
        narrower than the given width.

        @param minWidth
            The minimal column width in pixels, may be 0 to remove any
            previously set restrictions.
     */
    virtual void SetMinWidth(int minWidth) = 0;

    /**
        Set the alignment of the column header.

        @param align
            The text alignment in horizontal direction only or wxALIGN_NOT to
            use the default alignment, The possible values here are
            wxALIGN_CENTRE, wxALIGN_LEFT or wxALIGN_RIGHT with
            wxALIGN_CENTRE_HORIZONTAL being also supported as synonym for
            wxALIGN_CENTRE for consistency (but notice that GetAlignment()
            never returns it).
    */
    virtual void SetAlignment(wxAlignment align) = 0;


    /**
        Set the column flags.

        This method allows setting all flags at once, see also generic
        ChangeFlag(), SetFlag(), ClearFlag() and ToggleFlag() methods below as
        well as specific SetResizeable(), SetSortable(), SetReorderable() and
        SetHidden() ones.

        @param flags
            Combination of wxCOL_RESIZABLE, wxCOL_SORTABLE, wxCOL_REORDERABLE
            and wxCOL_HIDDEN bit flags.
     */
    virtual void SetFlags(int flags) = 0;

    /**
        Set or clear the given flag.

        @param flag
            The flag to set or clear.
        @param set
            If @true, set the flag, i.e. equivalent to calling SetFlag(),
            otherwise clear it, as ClearFlag().

        @see SetFlags()
     */
    void ChangeFlag(int flag, bool set);

    /**
        Set the specified flag for the column.

        @see SetFlags()
     */
    void SetFlag(int flag);

    /**
        Clear the specified flag for the column.

        @see SetFlags()
     */
    void ClearFlag(int flag);

    /**
        Toggle the specified flag for the column.

        If the flag is currently set, equivalent to ClearFlag(), otherwise --
        to SetFlag().

        @see SetFlags()
     */
    void ToggleFlag(int flag);


    /**
        Call this to enable or disable interactive resizing of the column by
        the user.

        By default, the columns are resizable.

        Equivalent to ChangeFlag(wxCOL_RESIZABLE, resizable).
     */
    virtual void SetResizeable(bool resizable);

    /**
        Allow clicking the column to sort the control contents by the field in
        this column.

        By default, the columns are not sortable so you need to explicitly call
        this function to allow sorting by the field corresponding to this
        column.

        Equivalent to ChangeFlag(wxCOL_SORTABLE, sortable).
     */
    virtual void SetSortable(bool sortable);

    /**
        Allow changing the column order by dragging it.

        Equivalent to ChangeFlag(wxCOL_REORDERABLE, reorderable).
     */
    virtual void SetReorderable(bool reorderable);

    /**
        Hide or show the column.

        By default all columns are shown but some of them can be completely
        hidden from view by calling this function.

        Equivalent to ChangeFlag(wxCOL_HIDDEN, hidden).
     */
    virtual void SetHidden(bool hidden);


    /**
        Don't use this column for sorting.

        This is the reverse of SetSortOrder() and is called to indicate that
        this column is not used for sorting any longer.
     */
    void UnsetAsSortKey();

    /**
        Sets this column as the sort key for the associated control.

        This function indicates that this column is currently used for sorting
        the control and also sets the sorting direction. Notice that actual
        sorting is only done in the control associated with the header, this
        function doesn't do any sorting on its own.

        Don't confuse this function with SetSortable() which should be used to
        indicate that the column @em may be used for sorting while this one is
        used to indicate that it currently @em is used for sorting. Of course,
        SetSortOrder() can be only called for sortable columns.

        @param ascending
            If @true, sort in ascending order, otherwise in descending order.
     */
    virtual void SetSortOrder(bool ascending) = 0;

    /**
        Inverses the sort order.

        This function is typically called when the user clicks on a column used
        for sorting to change sort order from ascending to descending or vice
        versa.

        @see SetSortOrder(), IsSortOrderAscending()
     */
    void ToggleSortOrder();
};

/**
    @class wxHeaderColumnSimple

    Simple container for the information about the column.

    This is a concrete class implementing all wxSettableHeaderColumn class
    methods in a trivial way, i.e. by just storing the information in the
    object itself. It is used by and with wxHeaderCtrlSimple, e.g.
    @code
        wxHeaderCtrlSimple * header = new wxHeaderCtrlSimple(...);
        wxHeaderColumnSimple col("Title");
        col.SetWidth(100);
        col.SetSortable(100);
        header->AppendColumn(col);
    @endcode

    @library{wxcore}
    @category{ctrl}
 */
class wxHeaderColumnSimple : public wxSettableHeaderColumn
{
public:
    //@{
    /**
        Constructor for a column header.

        The first constructor creates a header showing the given text @a title
        while the second one creates one showing the specified @a bitmap image.
     */
    wxHeaderColumnSimple(const wxString& title,
                         int width = wxCOL_WIDTH_DEFAULT,
                         wxAlignment align = wxALIGN_NOT,
                         int flags = wxCOL_DEFAULT_FLAGS);

    wxHeaderColumnSimple(const wxBitmap &bitmap,
                         int width = wxCOL_WIDTH_DEFAULT,
                         wxAlignment align = wxALIGN_CENTER,
                         int flags = wxCOL_DEFAULT_FLAGS);
    //@}

    //@{

    /// Trivial implementations of the base class pure virtual functions.

    virtual void SetTitle(const wxString& title);
    virtual wxString GetTitle() const;
    virtual void SetBitmap(const wxBitmap& bitmap);
    virtual wxBitmap GetBitmap() const;
    virtual void SetWidth(int width);
    virtual int GetWidth() const;
    virtual void SetMinWidth(int minWidth);
    virtual int GetMinWidth() const;
    virtual void SetAlignment(wxAlignment align);
    virtual wxAlignment GetAlignment() const;
    virtual void SetFlags(int flags);
    virtual int GetFlags() const;
    virtual bool IsSortKey() const;
    virtual void SetSortOrder(bool ascending);
    virtual bool IsSortOrderAscending() const;

    //@}
};
