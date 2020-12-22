/////////////////////////////////////////////////////////////////////////////
// Name:        html/htmlcell.h
// Purpose:     interface of wxHtml*Cell
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
   @class wxHtmlRenderingStyle

   wxHtmlSelection is data holder with information about text selection.
   Selection is defined by two positions (beginning and end of the selection)
   and two leaf(!) cells at these positions.

   @library{wxhtml}
   @category{html}
*/
class wxHtmlSelection
{
public:
    wxHtmlSelection();

    // this version is used for the user selection defined with the mouse
    void Set(const wxPoint& fromPos, const wxHtmlCell *fromCell,
             const wxPoint& toPos, const wxHtmlCell *toCell);
    void Set(const wxHtmlCell *fromCell, const wxHtmlCell *toCell);

    const wxHtmlCell *GetFromCell() const;
    const wxHtmlCell *GetToCell() const;

    // these values are in absolute coordinates:
    const wxPoint& GetFromPos() const;
    const wxPoint& GetToPos() const;

    // these are From/ToCell's private data
    void ClearFromToCharacterPos();
    bool AreFromToCharacterPosSet() const;

    void SetFromCharacterPos (wxCoord pos);
    void SetToCharacterPos (wxCoord pos);
    wxCoord GetFromCharacterPos () const;
    wxCoord GetToCharacterPos () const;

    bool IsEmpty() const;
};



enum wxHtmlSelectionState
{
    wxHTML_SEL_OUT,     // currently rendered cell is outside the selection
    wxHTML_SEL_IN,      // ... is inside selection
    wxHTML_SEL_CHANGING // ... is the cell on which selection state changes
};


/**
   @class wxHtmlRenderingState

   Selection state is passed to wxHtmlCell::Draw so that it can render itself
   differently e.g. when inside text selection or outside it.

   @library{wxhtml}
   @category{html}
*/
class wxHtmlRenderingState
{
public:
    wxHtmlRenderingState();

    void SetSelectionState(wxHtmlSelectionState s);
    wxHtmlSelectionState GetSelectionState() const;

    void SetFgColour(const wxColour& c);
    const wxColour& GetFgColour() const;
    void SetBgColour(const wxColour& c);
    const wxColour& GetBgColour() const;
    void SetBgMode(int m);
    int GetBgMode() const;
};



/**
    @class wxHtmlRenderingStyle

    Allows HTML rendering customizations.
    This class is used when rendering wxHtmlCells as a callback.

    @library{wxhtml}
    @category{html}

    @see wxHtmlRenderingInfo
*/
class wxHtmlRenderingStyle
{
public:
    /**
        Returns the colour to use for the selected text.
    */
    virtual wxColour GetSelectedTextColour(const wxColour& clr) = 0;

    /**
        Returns the colour to use for the selected text's background.
    */
    virtual wxColour GetSelectedTextBgColour(const wxColour& clr) = 0;
};


/**
    @class wxHtmlRenderingInfo

    This class contains information given to cells when drawing them.
    Contains rendering state, selection information and rendering style object
    that can be used to customize the output.

    @library{wxhtml}
    @category{html}

    @see @ref overview_html_cells, wxHtmlCell
*/
class wxHtmlRenderingInfo
{
public:
    /**
        Default ctor.
    */
    wxHtmlRenderingInfo();

    //@{
    /**
        Accessors.
    */
    void SetSelection(wxHtmlSelection *s);
    wxHtmlSelection *GetSelection() const;

    void SetStyle(wxHtmlRenderingStyle *style);
    wxHtmlRenderingStyle& GetStyle();

    wxHtmlRenderingState& GetState();
    //@}
};



// Flags for wxHtmlCell::FindCellByPos
enum
{
    wxHTML_FIND_EXACT             = 1,
    wxHTML_FIND_NEAREST_BEFORE    = 2,
    wxHTML_FIND_NEAREST_AFTER     = 4
};


// Superscript/subscript/normal script mode of a cell
enum wxHtmlScriptMode
{
    wxHTML_SCRIPT_NORMAL,
    wxHTML_SCRIPT_SUB,
    wxHTML_SCRIPT_SUP
};


/**
    @class wxHtmlCell

    Internal data structure. It represents fragments of parsed HTML page, the
    so-called @b cell - a word, picture, table, horizontal line and so on.
    It is used by wxHtmlWindow and wxHtmlWinParser to represent HTML page in memory.

    You can divide cells into two groups : @e visible cells with non-zero width and
    height and @e helper cells (usually with zero width and height) that perform
    special actions such as color or font change.

    @library{wxhtml}
    @category{html}

    @see @ref overview_html_cells, wxHtmlContainerCell
*/
class wxHtmlCell : public wxObject
{
public:
    /**
        Constructor.
    */
    wxHtmlCell();

    /**
        This method is called when paginating HTML, e.g.\ when printing.

        User code should never call this function, but may need to override it
        in custom HTML cell classes with any specific page breaking
        requirements.

        On input, @a pagebreak contains y-coordinate of page break (i.e. the
        horizontal line that should not be crossed by words, images etc.)
        relative to the parent cell on entry and may be modified to request a
        page break at a position before it if this cell cannot be divided into
        two pieces (each one on its own page).

        Note that page break must still happen on the current page, i.e. the
        returned value must be strictly greater than @code *pagebreak -
        pageHeight @endcode and less or equal to @c *pagebreak for the value of
        @a pagebreak on input.

        @param pagebreak
            position in pixels of the pagebreak.
        @param pageHeight
            the height in pixels of the page drawable area

        @return @true if pagebreak was modified, @false otherwise.
    */
    virtual bool AdjustPagebreak(int* pagebreak, int pageHeight) const;

    /**
        Renders the cell.

        @param dc
            Device context to which the cell is to be drawn.
        @param x,y
            Coordinates of parent's upper left corner (origin). You must
            add this to m_PosX,m_PosY when passing coordinates to dc's methods
            Example:
            @code
                dc->DrawText("hello", x + m_PosX, y + m_PosY)
            @endcode
        @param view_y1
            y-coord of the first line visible in window.
            This is used to optimize rendering speed.
        @param view_y2
            y-coord of the last line visible in window.
            This is used to optimize rendering speed.
        @param info
            Additional information for the rendering of the cell.
    */
    virtual void Draw(wxDC& dc, int x, int y, int view_y1, int view_y2, wxHtmlRenderingInfo& info);

    /**
        This method is called instead of Draw() when the cell is certainly out of
        the screen (and thus invisible). This is not nonsense - some tags (like
        wxHtmlColourCell or font setter) must be drawn even if they are invisible!

        @param dc
            Device context to which the cell is to be drawn.
        @param x,y
            Coordinates of parent's upper left corner. You must
            add this to m_PosX,m_PosY when passing coordinates to dc's methods
            Example:
            @code
                dc->DrawText("hello", x + m_PosX, y + m_PosY)
            @endcode
        @param info
            Additional information for the rendering of the cell.
    */
    virtual void DrawInvisible(wxDC& dc, int x , int y, wxHtmlRenderingInfo& info);

    /**
        Returns pointer to itself if this cell matches condition (or if any of the
        cells following in the list matches), @NULL otherwise.
        (In other words if you call top-level container's Find() it will
        return pointer to the first cell that matches the condition)

        It is recommended way how to obtain pointer to particular cell or
        to cell of some type (e.g. wxHtmlAnchorCell reacts on wxHTML_COND_ISANCHOR
        condition).

        @param condition
            Unique integer identifier of condition
        @param param
            Optional parameters
    */
    virtual const wxHtmlCell* Find(int condition, const void* param) const;

    /**
       Find a cell inside this cell positioned at the given coordinates
       (relative to this's positions). Returns NULL if no such cell exists.
       The flag can be used to specify whether to look for terminal or
       nonterminal cells or both. In either case, returned cell is deepest
       cell in cells tree that contains [x,y].
    */
    virtual wxHtmlCell *FindCellByPos(wxCoord x, wxCoord y,
                                  unsigned flags = wxHTML_FIND_EXACT) const;


    /**
        Returns descent value of the cell (m_Descent member).
        See explanation:
        @image html htmlcell_descent.png
    */
    int GetDescent() const;

    /**
        Returns pointer to the first cell in the list.
        You can then use child's GetNext() method to obtain pointer to the next
        cell in list.

        @note This shouldn't be used by the end user. If you need some way of
              finding particular cell in the list, try Find() method instead.
    */
    virtual wxHtmlCell* GetFirstChild() const;

    /**
        Returns height of the cell (m_Height member).
    */
    int GetHeight() const;

    /**
        Returns unique cell identifier if there is any, the empty string otherwise.
    */
    const wxString& GetId() const;

    /**
        Returns hypertext link if associated with this cell or @NULL otherwise.
        See wxHtmlLinkInfo. (Note: this makes sense only for visible tags).

        @param x,y
            Coordinates of position where the user pressed mouse button.
            These coordinates are used e.g. by COLORMAP. Values are relative to the
            upper left corner of THIS cell (i.e. from 0 to m_Width or m_Height)
    */
    virtual wxHtmlLinkInfo* GetLink(int x = 0, int y = 0) const;

    /**
        Returns cursor to show when mouse pointer is over the cell.

        @param window
            interface to the parent HTML window

        @see GetMouseCursorAt()
    */
    virtual wxCursor GetMouseCursor(wxHtmlWindowInterface* window) const;

    /**
        Returns cursor to show when mouse pointer is over the specified point.

        This function should be overridden instead of GetMouseCursorAt() if
        the cursor should depend on the exact position of the mouse in the
        window.

        @param window
            interface to the parent HTML window
        @param rePos
            Position to show cursor.

        @since 3.0
     */
    virtual wxCursor GetMouseCursorAt(wxHtmlWindowInterface* window,
                                      const wxPoint& rePos) const;

    /**
        Returns pointer to the next cell in list (see htmlcell.h if you're
        interested in details).
    */
    wxHtmlCell* GetNext() const;

    /**
        Returns pointer to parent container.
    */
    wxHtmlContainerCell* GetParent() const;

    /**
        Returns X position within parent (the value is relative to parent's
        upper left corner). The returned value is meaningful only if
        parent's Layout() was called before!
    */
    int GetPosX() const;

    /**
        Returns Y position within parent (the value is relative to parent's
        upper left corner). The returned value is meaningful only if
        parent's Layout() was called before!
    */
    int GetPosY() const;

    /**
        Returns width of the cell (m_Width member).
    */
    int GetWidth() const;

    /**
        Layouts the cell.

        This method performs two actions:
        -# adjusts the cell's width according to the fact that maximal possible
           width is @e w (this has sense when working with horizontal lines, tables etc.)
        -# prepares layout (=fill-in m_PosX, m_PosY (and sometimes m_Height) members)
           based on actual width @e w

        It must be called before displaying cells structure because m_PosX and
        m_PosY are undefined (or invalid) before calling Layout().
    */
    virtual void Layout(int w);

    /**
        This function is simple event handler.
        Each time the user clicks mouse button over a cell within wxHtmlWindow
        this method of that cell is called.
        Default behaviour is to call wxHtmlWindow::LoadPage.

        @param window
            interface to the parent HTML window
        @param pos
            coordinates of mouse click (this is relative to cell's origin
        @param event
            mouse event that triggered the call

        @return @true if a link was clicked, @false otherwise.

        @since 2.7.0 (before OnMouseClick() method served a similar purpose).

        @note
        If you need more "advanced" event handling you should use wxHtmlBinderCell instead.
    */
    virtual bool ProcessMouseClick(wxHtmlWindowInterface* window,
                                   const wxPoint& pos,
                                   const wxMouseEvent& event);

    /**
        Sets unique cell identifier. Default value is no identifier, i.e. empty string.
    */
    void SetId(const wxString& id);

    /**
        Sets the hypertext link associated with this cell.
        (Default value is wxHtmlLinkInfo("", "") (no link))
    */
    void SetLink(const wxHtmlLinkInfo& link);

    /**
        Sets the next cell in the list. This shouldn't be called by user - it is
        to be used only by wxHtmlContainerCell::InsertCell.
    */
    void SetNext(wxHtmlCell* cell);

    /**
        Sets parent container of this cell.
        This is called from wxHtmlContainerCell::InsertCell.
    */
    void SetParent(wxHtmlContainerCell* p);

    /**
        Sets the cell's position within parent container.
    */
    virtual void SetPos(int x, int y);

    /**
       Converts the cell into text representation. If sel != NULL then
       only part of the cell inside the selection is converted.
    */
    virtual wxString ConvertToText(wxHtmlSelection* sel) const;

};



/**
    @class wxHtmlContainerCell

    The wxHtmlContainerCell class is an implementation of a cell that may
    contain more cells in it. It is heavily used in the wxHTML layout algorithm.

    @library{wxhtml}
    @category{html}

    @see @ref overview_html_cells
*/
class wxHtmlContainerCell : public wxHtmlCell
{
public:
    /**
        Constructor. @a parent is pointer to parent container or @NULL.
    */
    explicit wxHtmlContainerCell(wxHtmlContainerCell* parent);

    /**
        Detach a child cell.

        Detaching a cell removes it from this container and allows reattaching
        it to another one by using InsertCell(). Alternatively, this method can
        be used to selectively remove some elements of the HTML document tree
        by deleting the cell after calling it.

        @param cell Must be non-null and an immediate child of this cell.

        @since 3.1.2
     */
    void Detach(wxHtmlCell* cell);

    /**
        Returns container's horizontal alignment.
    */
    int GetAlignHor() const;

    /**
        Returns container's vertical alignment.
    */
    int GetAlignVer() const;

    /**
        Returns the background colour of the container or @c wxNullColour if no
        background colour is set.
    */
    wxColour GetBackgroundColour();

    /**
        Returns the indentation. @a ind is one of the @b wxHTML_INDENT_* constants.

        @note You must call GetIndentUnits() with same @a ind parameter in order
              to correctly interpret the returned integer value.
              It is NOT always in pixels!
    */
    int GetIndent(int ind) const;

    /**
        Returns the units of indentation for @a ind where @a ind is one
        of the @b wxHTML_INDENT_* constants.
    */
    int GetIndentUnits(int ind) const;

    /**
        Inserts a new cell into the container.

        Note that the container takes ownership of the cell and will delete it
        when it itself is destroyed.
    */
    void InsertCell(wxHtmlCell* cell);

    /**
        Sets the container's alignment (both horizontal and vertical) according to
        the values stored in @e tag. (Tags @c ALIGN parameter is extracted.)
        In fact it is only a front-end to SetAlignHor() and SetAlignVer().
    */
    void SetAlign(const wxHtmlTag& tag);

    /**
        Sets the container's @e horizontal alignment.
        During wxHtmlCell::Layout each line is aligned according to @a al value.

        @param al
            new horizontal alignment. May be one of these values:
            - wxHTML_ALIGN_LEFT: lines are left-aligned (default)
            - wxHTML_ALIGN_JUSTIFY: lines are justified
            - wxHTML_ALIGN_CENTER: lines are centered
            - wxHTML_ALIGN_RIGHT: lines are right-aligned
    */
    void SetAlignHor(int al);

    /**
        Sets the container's @e vertical alignment. This is per-line alignment!

        @param al
            new vertical alignment. May be one of these values:
            - wxHTML_ALIGN_BOTTOM: cells are over the line (default)
            - wxHTML_ALIGN_CENTER: cells are centered on line
            - wxHTML_ALIGN_TOP: cells are under the line

        @image html htmlcontcell_alignv.png
    */
    void SetAlignVer(int al);

    /**
        Sets the background colour for this container.
    */
    void SetBackgroundColour(const wxColour& clr);

    /**
        Sets the border (frame) colours. A border is a rectangle around the container.

        @param clr1
            Colour of top and left lines
        @param clr2
            Colour of bottom and right lines
        @param border
            Size of the border in pixels
    */
    void SetBorder(const wxColour& clr1, const wxColour& clr2, int border = 1);

    /**
        Sets the indentation (free space between borders of container and subcells).

        @image html htmlcontcell_indent.png

        @param i
            Indentation value.
        @param what
            Determines which of the four borders we're setting. It is OR
            combination of following constants:
            - wxHTML_INDENT_TOP: top border
            - wxHTML_INDENT_BOTTOM: bottom
            - wxHTML_INDENT_LEFT: left
            - wxHTML_INDENT_RIGHT: right
            - wxHTML_INDENT_HORIZONTAL: left and right
            - wxHTML_INDENT_VERTICAL: top and bottom
            - wxHTML_INDENT_ALL: all 4 borders
        @param units
            Units of i. This parameter affects interpretation of value.
            - wxHTML_UNITS_PIXELS: @a i is number of pixels
            - wxHTML_UNITS_PERCENT: @a i is interpreted as percents of width
                                    of parent container
    */
    void SetIndent(int i, int what, int units = wxHTML_UNITS_PIXELS);

    /**
        Sets minimal height of the container.
        When container's wxHtmlCell::Layout is called, m_Height is set depending
        on layout of subcells to the height of area covered by layed-out subcells.
        Calling this method guarantees you that the height of container is never
        smaller than @a h - even if the subcells cover much smaller area.

        @param h
            The minimal height.
        @param align
            If height of the container is lower than the minimum height, empty space
            must be inserted somewhere in order to ensure minimal height.
            This parameter is one of @c wxHTML_ALIGN_TOP, @c wxHTML_ALIGN_BOTTOM,
            @c wxHTML_ALIGN_CENTER. It refers to the contents, not to the
            empty place.
    */
    void SetMinHeight(int h, int align = wxHTML_ALIGN_TOP);

    /**
        Sets floating width adjustment.

        The normal behaviour of container is that its width is the same as the width of
        parent container (and thus you can have only one sub-container per line).
        You can change this by setting the floating width adjustment.

        @param w
            Width of the container. If the value is negative it means
            complement to full width of parent container.
            E.g. @code SetWidthFloat(-50, wxHTML_UNITS_PIXELS) @endcode sets the
            width of container to parent's width minus 50 pixels. This is useful when
            creating tables - you can call SetWidthFloat(50) and SetWidthFloat(-50).
        @param units
            Units of w This parameter affects the interpretation of  value.
            - wxHTML_UNITS_PIXELS: @a w is number of pixels
            - wxHTML_UNITS_PERCENT: @a w is interpreted as percents of width
                                    of parent container
    */
    void SetWidthFloat(int w, int units);

    /**
        Sets floating width adjustment.

        The normal behaviour of container is that its width is the same as the width of
        parent container (and thus you can have only one sub-container per line).
        You can change this by setting the floating width adjustment.

        @param tag
            In the second version of method, @a w and @a units info is extracted
            from tag's WIDTH parameter.
        @param pixel_scale
            This is number of real pixels that equals to 1 HTML pixel.
    */
    void SetWidthFloat(const wxHtmlTag& tag,
                       double pixel_scale = 1.0);
};



/**
    @class wxHtmlLinkInfo

    This class stores all necessary information about hypertext links
    (as represented by \<A\> tag in HTML documents).
    In current implementation it stores URL and target frame name.

    @note Frames are not currently supported by wxHTML!

    @library{wxhtml}
    @category{html}
*/
class wxHtmlLinkInfo : public wxObject
{
public:
    /**
        Default ctor.
    */
    wxHtmlLinkInfo();

    /**
        Construct hypertext link from HREF (aka URL) and TARGET (name of target frame).
    */
    wxHtmlLinkInfo(const wxString& href,
                   const wxString& target = wxEmptyString);

    /**
        Return pointer to event that generated OnLinkClicked() event.
        Valid only within wxHtmlWindow::OnLinkClicked, @NULL otherwise.
    */
    const wxMouseEvent* GetEvent() const;

    /**
        Return @e HREF value of the \<A\> tag.
    */
    wxString GetHref() const;

    /**
        Return pointer to the cell that was clicked.
        Valid only within wxHtmlWindow::OnLinkClicked, @NULL otherwise.
    */
    const wxHtmlCell* GetHtmlCell() const;

    /**
        Return @e TARGET value of the \<A\> tag (this value is used to specify
        in which frame should be the page pointed by @ref GetHref() Href opened).
    */
    wxString GetTarget() const;
};

/**
    @class wxHtmlColourCell

    This cell changes the colour of either the background or the foreground.

    @library{wxhtml}
    @category{html}
*/
class wxHtmlColourCell : public wxHtmlCell
{
public:
    /**
        Constructor.

        @param clr
            The color
        @param flags
            Can be one of following:
            - wxHTML_CLR_FOREGROUND: change color of text
            - wxHTML_CLR_BACKGROUND: change background color
    */
    wxHtmlColourCell(const wxColour& clr, int flags = wxHTML_CLR_FOREGROUND);
};



/**
    @class wxHtmlWidgetCell

    wxHtmlWidgetCell is a class that provides a connection between HTML cells and
    widgets (an object derived from wxWindow).
    You can use it to display things like forms, input boxes etc. in an HTML window.

    wxHtmlWidgetCell takes care of resizing and moving window.

    @library{wxhtml}
    @category{html}
*/
class wxHtmlWidgetCell : public wxHtmlCell
{
public:
    /**
        Constructor.

        @param wnd
            Connected window. It is parent window @b must be the wxHtmlWindow object
            within which it is displayed!
        @param w
            Floating width. If non-zero width of wnd window is adjusted so that it is
            always w percents of parent container's width. (For example w = 100 means
            that the window will always have same width as parent container).
    */
    wxHtmlWidgetCell(wxWindow* wnd, int w = 0);
};



/**
    @class wxHtmlWordCell

    This html cell represents a single word or text fragment in the document stream.

    @library{wxhtml}
    @category{html}
*/
class wxHtmlWordCell : public wxHtmlCell
{
public:
    wxHtmlWordCell(const wxString& word, const wxDC& dc);
};


/**
    @class wxHtmlWordWithTabsCell

    wxHtmlWordCell is a specialization for storing text fragments with
    embedded tab characters.

    @library{wxhtml}
    @category{html}
*/
class wxHtmlWordWithTabsCell : public wxHtmlWordCell
{
public:
    wxHtmlWordWithTabsCell(const wxString& word,
                           const wxString& wordOrig,
                           size_t linepos,
                           const wxDC& dc);
};


/**
    @class wxHtmlFontCell

    This cell represents a font change in the document stream.

    @library{wxhtml}
    @category{html}
*/
class wxHtmlFontCell : public wxHtmlCell
{
public:
    wxHtmlFontCell(wxFont *font);
};
