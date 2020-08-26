/////////////////////////////////////////////////////////////////////////////
// Name:        gbsizer.h
// Purpose:     interface of wxGBPosition
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxGBPosition

    This class represents the position of an item in a virtual grid of rows and
    columns managed by a wxGridBagSizer.

    @library{wxcore}
    @category{winlayout}
*/
class wxGBPosition
{
public:
    /**
        Default constructor, setting the row and column to (0,0).
    */
    wxGBPosition();
    /**
        Construct a new wxGBPosition, setting the row and column.
    */
    wxGBPosition(int row, int col);

    /**
        Get the current column value.
    */
    int GetCol() const;

    /**
        Get the current row value.
    */
    int GetRow() const;

    /**
        Set a new column value.
    */
    void SetCol(int col);

    /**
        Set a new row value.
    */
    void SetRow(int row);

    /**
        Compare inequality of two wxGBPositions.
    */
    bool operator!=(const wxGBPosition& p) const;

    /**
        Compare equality of two wxGBPositions.
    */
    bool operator==(const wxGBPosition& p) const;
};



/**
    @class wxGridBagSizer

    A wxSizer that can lay out items in a virtual grid like a wxFlexGridSizer
    but in this case explicit positioning of the items is allowed using
    wxGBPosition, and items can optionally span more than one row and/or column
    using wxGBSpan.

    @library{wxcore}
    @category{winlayout}
*/
class wxGridBagSizer : public wxFlexGridSizer
{
public:
    /**
        Constructor, with optional parameters to specify the gap between the
        rows and columns.
    */
    wxGridBagSizer(int vgap = 0, int hgap = 0);

    //@{
    /**
        Adds the given item to the given position.

        @return A valid pointer if the item was successfully placed at the
                 given position, or @NULL if something was already there.
    */
    wxSizerItem* Add(wxWindow* window, const wxGBPosition& pos,
                     const wxGBSpan& span = wxDefaultSpan,
                     int flag = 0, int border = 0, wxObject* userData = NULL);
    wxSizerItem* Add(wxSizer* sizer, const wxGBPosition& pos,
                     const wxGBSpan& span = wxDefaultSpan,
                     int flag = 0, int border = 0, wxObject* userData = NULL);
    wxSizerItem* Add(wxGBSizerItem* item);

    /**
        Adds a spacer to the given position.
        @a width and @a height specify the dimension of the spacer to be added.

        @return A valid pointer if the spacer was successfully placed at the
                 given position, or @NULL if something was already there.
    */
    wxSizerItem* Add(int width, int height, const wxGBPosition& pos,
                     const wxGBSpan& span = wxDefaultSpan,
                     int flag = 0, int border = 0, wxObject* userData = NULL);

    //@}
    /**
        Called when the managed size of the sizer is needed or when layout
        needs done.
    */
    wxSize CalcMin();

    //@{
    /**
        Look at all items and see if any intersect (or would overlap) the given
        item. Returns @true if so, @false if there would be no overlap. If an
        @a excludeItem is given then it will not be checked for intersection,
        for example it may be the item we are checking the position of.
    */
    bool CheckForIntersection(wxGBSizerItem* item,
                              wxGBSizerItem* excludeItem = NULL);
    bool CheckForIntersection(const wxGBPosition& pos, const wxGBSpan& span,
                              wxGBSizerItem* excludeItem = NULL);
    //@}

    //@{
    /**
        Find the sizer item for the given window or subsizer, returns @NULL if
        not found. (non-recursive)
    */
    wxGBSizerItem* FindItem(wxWindow* window);
    wxGBSizerItem* FindItem(wxSizer* sizer);
    //@}

    /**
        Return the sizer item located at the point given in pt, or @NULL if
        there is no item at that point. The (x,y) coordinates in @a pt
        correspond to the client coordinates of the window using the sizer for
        layout. (non-recursive)
    */
    wxGBSizerItem* FindItemAtPoint(const wxPoint& pt);

    /**
        Return the sizer item for the given grid cell, or @NULL if there is no
        item at that position. (non-recursive)
    */
    wxGBSizerItem* FindItemAtPosition(const wxGBPosition& pos);

    /**
        Return the sizer item that has a matching user data (it only compares
        pointer values) or @NULL if not found. (non-recursive)
    */
    wxGBSizerItem* FindItemWithData(const wxObject* userData);

    /**
        Get the size of the specified cell, including hgap and vgap. Only valid
        after window layout has been performed.
    */
    wxSize GetCellSize(int row, int col) const;

    /**
        Get the size used for cells in the grid with no item.
    */
    wxSize GetEmptyCellSize() const;

    //@{
    /**
        Get the grid position of the specified item.
    */
    wxGBPosition GetItemPosition(wxWindow* window);
    wxGBPosition GetItemPosition(wxSizer* sizer);
    wxGBPosition GetItemPosition(size_t index);
    //@}

    //@{
    /**
        Get the row/col spanning of the specified item.
    */
    wxGBSpan GetItemSpan(wxWindow* window);
    wxGBSpan GetItemSpan(wxSizer* sizer);
    wxGBSpan GetItemSpan(size_t index);
    //@}

    /**
        Called when the managed size of the sizer is needed or when layout
        needs done.
    */
    virtual void RepositionChildren(const wxSize& minSize);

    /**
        Set the size used for cells in the grid with no item.
    */
    void SetEmptyCellSize(const wxSize& sz);

    //@{
    /**
        Set the grid position of the specified item. Returns @true on success.
        If the move is not allowed (because an item is already there) then
        @false is returned.
    */
    bool SetItemPosition(wxWindow* window, const wxGBPosition& pos);
    bool SetItemPosition(wxSizer* sizer, const wxGBPosition& pos);
    bool SetItemPosition(size_t index, const wxGBPosition& pos);
    //@}

    //@{
    /**
        Set the row/col spanning of the specified item. Returns @true on
        success. If the move is not allowed (because an item is already there)
        then @false is returned.
    */
    bool SetItemSpan(wxWindow* window, const wxGBSpan& span);
    bool SetItemSpan(wxSizer* sizer, const wxGBSpan& span);
    bool SetItemSpan(size_t index, const wxGBSpan& span);
    //@}
};



/**
    @class wxGBSizerItem

    The wxGBSizerItem class is used by the wxGridBagSizer for tracking the
    items in the sizer. It adds grid position and spanning information to the
    normal wxSizerItem by adding wxGBPosition and wxGBSpan attributes. Most of
    the time you will not need to use a wxGBSizerItem directly in your code,
    but there are a couple of cases where it is handy.

    @library{wxcore}
    @category{winlayout}
*/
class wxGBSizerItem : public wxSizerItem
{
public:
    /**
        Construct a sizer item for tracking a spacer.
    */
    wxGBSizerItem(int width, int height, const wxGBPosition& pos,
                  const wxGBSpan& span=wxDefaultSpan, int flag=0, int border=0,
                  wxObject* userData=NULL);
    /**
        Construct a sizer item for tracking a window.
    */
    wxGBSizerItem(wxWindow* window, const wxGBPosition& pos,
                  const wxGBSpan& span=wxDefaultSpan, int flag=0, int border=0,
                  wxObject* userData=NULL);
    /**
        Construct a sizer item for tracking a subsizer.
    */
    wxGBSizerItem(wxSizer* sizer, const wxGBPosition& pos,
                  const wxGBSpan& span=wxDefaultSpan, int flag=0, int border=0,
                  wxObject* userData=NULL);

    /**
        Get the row and column of the endpoint of this item.
    */
    void GetEndPos(int& row, int& col);

    //@{
    /**
        Get the grid position of the item.
    */
    wxGBPosition GetPos() const;
    void GetPos(int& row, int& col) const;
    //@}

    //@{
    /**
        Get the row and column spanning of the item.
    */
    wxGBSpan GetSpan() const;
    void GetSpan(int& rowspan, int& colspan) const;
    //@}

    /**
        Returns @true if this item and the @a other item intersect.
    */
    bool Intersects(const wxGBSizerItem& other);
    /**
        Returns @true if the given pos/span would intersect with this item.
    */
    bool Intersects(const wxGBPosition& pos, const wxGBSpan& span);

    /**
        If the item is already a member of a sizer then first ensure that there
        is no other item that would intersect with this one at the new
        position, then set the new position. Returns @true if the change is
        successful and after the next Layout the item will be moved.
    */
    bool SetPos(const wxGBPosition& pos);

    /**
        If the item is already a member of a sizer then first ensure that there
        is no other item that would intersect with this one with its new
        spanning size, then set the new spanning. Returns @true if the change
        is successful and after the next Layout the item will be resized.
    */
    bool SetSpan(const wxGBSpan& span);


    wxGridBagSizer* GetGBSizer() const;
    void SetGBSizer(wxGridBagSizer* sizer);
};



/**
    @class wxGBSpan

    This class is used to hold the row and column spanning attributes of items
    in a wxGridBagSizer.

    @library{wxcore}
    @category{winlayout}
*/
class wxGBSpan
{
public:
    /**
        Default constructor, setting the rowspan and colspan to (1,1) meaning
        that the item occupies one cell in each direction.
    */
    wxGBSpan();
    /**
        Construct a new wxGBSpan, setting the @a rowspan and @a colspan.
    */
    wxGBSpan(int rowspan, int colspan);

    /**
        Get the current colspan value.
    */
    int GetColspan() const;

    /**
        Get the current rowspan value.
    */
    int GetRowspan() const;

    /**
        Set a new colspan value.
    */
    void SetColspan(int colspan);

    /**
        Set a new rowspan value.
    */
    void SetRowspan(int rowspan);

    /**
        Compare inequality of two wxGBSpans.
    */
    bool operator!=(const wxGBSpan& o) const;

    /**
        Compare equality of two wxGBSpans.
    */
    bool operator==(const wxGBSpan& o) const;
};


const wxGBSpan wxDefaultSpan;
