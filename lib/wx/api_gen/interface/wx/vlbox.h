/////////////////////////////////////////////////////////////////////////////
// Name:        vlbox.h
// Purpose:     interface of wxVListBox
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxVListBox

    wxVListBox is a wxListBox-like control with the following two main
    differences from a regular wxListBox: it can have an arbitrarily huge
    number of items because it doesn't store them itself but uses the
    OnDrawItem() callback to draw them (so it is a virtual listbox) and its
    items can have variable height as determined by OnMeasureItem() (so it is
    also a listbox with the lines of variable height).

    Also, as a consequence of its virtual nature, it doesn't have any methods
    to append or insert items in it as it isn't necessary to do it: you just
    have to call SetItemCount() to tell the control how many items it should
    display. Of course, this also means that you will never use this class
    directly because it has pure virtual functions, but will need to derive
    your own class from it (for example, wxHtmlListBox).

    However it emits the same events as wxListBox and the same event macros may
    be used with it. Since wxVListBox does not store its items itself, the
    events will only contain the index, not any contents such as the string of
    an item.

    @library{wxcore}
    @category{ctrl}

    @see wxSimpleHtmlListBox, wxHtmlListBox
*/
class wxVListBox : public wxVScrolledWindow
{
public:
    /**
        Default constructor, you must call Create() later.
    */
    wxVListBox();
    /**
        Normal constructor which calls Create() internally.
    */
    wxVListBox(wxWindow* parent, wxWindowID id = wxID_ANY,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = 0, const wxString& name = wxVListBoxNameStr);

    /**
        Destructor.
    */
    virtual ~wxVListBox();

    /**
        Deletes all items from the control.
    */
    void Clear();

    /**
        Creates the control. To finish creating it you also should call
        SetItemCount() to let it know about the number of items it contains.

        The only special style which may be used with wxVListBox is
        @c wxLB_MULTIPLE which indicates that the listbox should support
        multiple selection.

        @return @true on success or @false if the control couldn't be created.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = 0,
                const wxString& name = wxVListBoxNameStr);

    /**
        Deselects all the items in the listbox. This method is only valid for
        multi selection listboxes.

        @return @true if any items were changed, i.e. if there had been any
                 selected items before, or @false if all the items were already
                 deselected.

        @see SelectAll(), Select()
    */
    bool DeselectAll();

    /**
        Returns the index of the first selected item in the listbox or
        @c wxNOT_FOUND if no items are currently selected.

        @a cookie is an opaque parameter which should be passed to the
        subsequent calls to GetNextSelected(). It is needed in order to allow
        parallel iterations over the selected items.

        Here is a typical example of using these functions:

        @code
        unsigned long cookie;
        int item = hlbox->GetFirstSelected(cookie);
        while ( item != wxNOT_FOUND )
        {
            // ... process item ...
            item = hlbox->GetNextSelected(cookie);
        }
        @endcode

        This method is only valid for multi selection listboxes.
    */
    int GetFirstSelected(unsigned long& cookie) const;

    /**
        Get the number of items in the control.

        @see SetItemCount()
    */
    size_t GetItemCount() const;

    /**
        Returns the margins used by the control. The @c x field of the returned
        point is the horizontal margin and the @c y field is the vertical one.

        @see SetMargins()
    */
    wxPoint GetMargins() const;

    /**
        Returns the rectangle occupied by this item in physical coordinates.

        If the item is not currently visible, returns an empty rectangle.

        @since 2.9.0
     */
    wxRect GetItemRect(size_t item) const;

    /**
        Returns the index of the next selected item or @c wxNOT_FOUND if there
        are no more.

        This method is only valid for multi selection listboxes.

        @see GetFirstSelected()
    */
    int GetNextSelected(unsigned long& cookie) const;

    /**
        Returns the number of the items currently selected.

        It is valid for both single and multi selection controls. In the former
        case it may only return 0 or 1 however.

        @see IsSelected(), GetFirstSelected(), GetNextSelected()
    */
    size_t GetSelectedCount() const;

    /**
        Get the currently selected item or @c wxNOT_FOUND if there is no
        selection.
    */
    int GetSelection() const;

    /**
        Returns the background colour used for the selected cells. By default
        the standard system colour is used.

        @see wxSystemSettings::GetColour(), SetSelectionBackground()
    */
    const wxColour& GetSelectionBackground() const;

    /**
        Returns @true if the listbox was created with @c wxLB_MULTIPLE style
        and so supports multiple selection or @false if it is a single
        selection listbox.
    */
    bool HasMultipleSelection() const;

    /**
        Returns @true if this item is the current one, @false otherwise.

        The current item is always the same as selected one for the single
        selection listbox and in this case this method is equivalent to
        IsSelected() but they are different for multi selection listboxes where
        many items may be selected but only one (at most) is current.
    */
    bool IsCurrent(size_t item) const;

    /**
        Returns @true if this item is selected, @false otherwise.
    */
    bool IsSelected(size_t item) const;

    /**
        Selects or deselects the specified item which must be valid (i.e.\ not
        equal to @c wxNOT_FOUND).

        @return @true if the items selection status has changed or @false
                 otherwise.

        This function is only valid for the multiple selection listboxes, use
        SetSelection() for the single selection ones.
    */
    bool Select(size_t item, bool select = true);

    /**
        Selects all the items in the listbox.

        @return @true if any items were changed, i.e. if there had been any
                 unselected items before, or @false if all the items were
                 already selected.

        This method is only valid for multi selection listboxes.

        @see DeselectAll(), Select()
    */
    bool SelectAll();

    /**
        Selects all items in the specified range which may be given in any
        order.

        @return @true if the items selection status has changed or @false
                 otherwise.

        This method is only valid for multi selection listboxes.

        @see SelectAll(), Select()
    */
    bool SelectRange(size_t from, size_t to);

    /**
        Set the number of items to be shown in the control.

        This is just a synonym for wxVScrolledWindow::SetRowCount().
    */
    virtual void SetItemCount(size_t count);

    //@{
    /**
        Set the margins: horizontal margin is the distance between the window
        border and the item contents while vertical margin is half of the
        distance between items.

        By default both margins are 0.
    */
    void SetMargins(const wxPoint& pt);
    void SetMargins(wxCoord x, wxCoord y);
    //@}

    /**
        Set the selection to the specified item, if it is -1 the selection is
        unset. The selected item will be automatically scrolled into view if it
        isn't currently visible.

        This method may be used both with single and multiple selection
        listboxes.
    */
    void SetSelection(int selection);

    /**
        Sets the colour to be used for the selected cells background. The
        background of the standard cells may be changed by simply calling
        wxWindow::SetBackgroundColour().

        @note Using a non-default background colour may result in control
              having an appearance different from the similar native controls
              and should be avoided in general.

        @see GetSelectionBackground()
    */
    void SetSelectionBackground(const wxColour& col);

    /**
        Toggles the state of the specified @a item, i.e.\ selects it if it was
        unselected and deselects it if it was selected.

        This method is only valid for multi selection listboxes.

        @see Select()
    */
    void Toggle(size_t item);

protected:

    /**
        The derived class must implement this function to actually draw the
        item with the given index on the provided DC.

        @param dc
            The device context to use for drawing.
        @param rect
            The bounding rectangle for the item being drawn (DC clipping
            region is set to this rectangle before calling this function).
        @param n
            The index of the item to be drawn.

        @todo Change this function signature to non-const.
    */
    virtual void OnDrawItem(wxDC& dc, const wxRect& rect, size_t n) const = 0;

    /**
        This method is used to draw the item's background and, maybe, a border
        around it.

        The base class version implements a reasonable default behaviour which
        consists in drawing the selected item with the standard background
        colour and drawing a border around the item if it is either selected or
        current.

        @todo Change this function signature to non-const.
    */
    virtual void OnDrawBackground(wxDC& dc, const wxRect& rect, size_t n) const;

    /**
        This method may be used to draw separators between the lines. The
        rectangle passed to it may be modified, typically to deflate it a bit
        before passing to OnDrawItem().

        The base class version of this method doesn't do anything.

        @param dc
            The device context to use for drawing.
        @param rect
            The bounding rectangle for the item.
        @param n
            The index of the item.

        @todo Change this function signature to non-const.
    */
    virtual void OnDrawSeparator(wxDC& dc, wxRect& rect, size_t n) const;

    /**
        The derived class must implement this method to return the height of
        the specified item (in pixels).
    */
    virtual wxCoord OnMeasureItem(size_t n) const = 0;
};

