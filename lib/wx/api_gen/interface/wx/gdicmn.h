/////////////////////////////////////////////////////////////////////////////
// Name:        gdicmn.h
// Purpose:     interface of wxRealPoint
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    Bitmap type flags. See wxBitmap and wxImage classes.
*/
enum wxBitmapType
{
    wxBITMAP_TYPE_INVALID,
    wxBITMAP_TYPE_BMP,
    wxBITMAP_TYPE_BMP_RESOURCE,
    wxBITMAP_TYPE_RESOURCE = wxBITMAP_TYPE_BMP_RESOURCE,
    wxBITMAP_TYPE_ICO,
    wxBITMAP_TYPE_ICO_RESOURCE,
    wxBITMAP_TYPE_CUR,
    wxBITMAP_TYPE_CUR_RESOURCE,
    wxBITMAP_TYPE_XBM,
    wxBITMAP_TYPE_XBM_DATA,
    wxBITMAP_TYPE_XPM,
    wxBITMAP_TYPE_XPM_DATA,
    wxBITMAP_TYPE_TIFF,
    wxBITMAP_TYPE_TIF = wxBITMAP_TYPE_TIFF,
    wxBITMAP_TYPE_TIFF_RESOURCE,
    wxBITMAP_TYPE_TIF_RESOURCE = wxBITMAP_TYPE_TIFF_RESOURCE,
    wxBITMAP_TYPE_GIF,
    wxBITMAP_TYPE_GIF_RESOURCE,
    wxBITMAP_TYPE_PNG,
    wxBITMAP_TYPE_PNG_RESOURCE,
    wxBITMAP_TYPE_JPEG,
    wxBITMAP_TYPE_JPEG_RESOURCE,
    wxBITMAP_TYPE_PNM,
    wxBITMAP_TYPE_PNM_RESOURCE,
    wxBITMAP_TYPE_PCX,
    wxBITMAP_TYPE_PCX_RESOURCE,
    wxBITMAP_TYPE_PICT,
    wxBITMAP_TYPE_PICT_RESOURCE,
    wxBITMAP_TYPE_ICON,
    wxBITMAP_TYPE_ICON_RESOURCE,
    wxBITMAP_TYPE_ANI,
    wxBITMAP_TYPE_IFF,
    wxBITMAP_TYPE_TGA,
    wxBITMAP_TYPE_MACCURSOR,
    wxBITMAP_TYPE_MACCURSOR_RESOURCE,
    wxBITMAP_TYPE_ANY = 50
};

/**
    Polygon filling mode. See wxDC::DrawPolygon.
*/
enum wxPolygonFillMode
{
    wxODDEVEN_RULE = 1,
    wxWINDING_RULE
};

/**
    Standard cursors.

    Notice that under wxMSW some of these cursors are defined in @c wx.rc file
    and not by the system itself so you should include this file from your own
    resource file (possibly creating a trivial resource file just containing a
    single include line if you don't need it otherwise) to be able to use them.

    See wxCursor.
*/
enum wxStockCursor
{
    wxCURSOR_NONE,
    wxCURSOR_ARROW,             ///< A standard arrow cursor.
    wxCURSOR_RIGHT_ARROW,       ///< A standard arrow cursor pointing to the right.
    wxCURSOR_BULLSEYE,          ///< Bullseye cursor.
    wxCURSOR_CHAR,              ///< Rectangular character cursor.
    wxCURSOR_CROSS,             ///< A cross cursor.
    wxCURSOR_HAND,              ///< A hand cursor.
    wxCURSOR_IBEAM,             ///< An I-beam cursor (vertical line).
    wxCURSOR_LEFT_BUTTON,       ///< Represents a mouse with the left button depressed.
    wxCURSOR_MAGNIFIER,         ///< A magnifier icon.
    wxCURSOR_MIDDLE_BUTTON,     ///< Represents a mouse with the middle button depressed.
    wxCURSOR_NO_ENTRY,          ///< A no-entry sign cursor.
    wxCURSOR_PAINT_BRUSH,       ///< A paintbrush cursor.
    wxCURSOR_PENCIL,            ///< A pencil cursor.
    wxCURSOR_POINT_LEFT,        ///< A cursor that points left.
    wxCURSOR_POINT_RIGHT,       ///< A cursor that points right.
    wxCURSOR_QUESTION_ARROW,    ///< An arrow and question mark.
    wxCURSOR_RIGHT_BUTTON,      ///< Represents a mouse with the right button depressed.
    wxCURSOR_SIZENESW,          ///< A sizing cursor pointing NE-SW.
    wxCURSOR_SIZENS,            ///< A sizing cursor pointing N-S.
    wxCURSOR_SIZENWSE,          ///< A sizing cursor pointing NW-SE.
    wxCURSOR_SIZEWE,            ///< A sizing cursor pointing W-E.
    wxCURSOR_SIZING,            ///< A general sizing cursor.
    wxCURSOR_SPRAYCAN,          ///< A spraycan cursor.
    wxCURSOR_WAIT,              ///< A wait cursor.
    wxCURSOR_WATCH,             ///< A watch cursor.
    wxCURSOR_BLANK,             ///< Transparent cursor.
    wxCURSOR_DEFAULT,           ///< Standard X11 cursor (only in wxGTK).
    wxCURSOR_COPY_ARROW ,       ///< MacOS Theme Plus arrow (only in wxMac).
    wxCURSOR_CROSS_REVERSE,     ///< Only available on wxX11.
    wxCURSOR_DOUBLE_ARROW,      ///< Only available on wxX11.
    wxCURSOR_BASED_ARROW_UP,    ///< Only available on wxX11.
    wxCURSOR_BASED_ARROW_DOWN,  ///< Only available on wxX11.
    wxCURSOR_ARROWWAIT,         ///< A wait cursor with a standard arrow.
    wxCURSOR_MAX
};

/**
    Flags used by wxControl::Ellipsize function.
*/
enum wxEllipsizeFlags
{
    /// No special flags.
    wxELLIPSIZE_FLAGS_NONE = 0,

    /**
        Take mnemonics into account when calculating the text width.

        With this flag when calculating the size of the passed string,
        mnemonics characters (see wxControl::SetLabel) will be automatically
        reduced to a single character. This leads to correct calculations only
        if the string passed to Ellipsize() will be used with
        wxControl::SetLabel. If you don't want ampersand to be interpreted as
        mnemonics (e.g. because you use wxControl::SetLabelText) then don't use
        this flag.
     */
    wxELLIPSIZE_FLAGS_PROCESS_MNEMONICS = 1,

    /**
        Expand tabs in spaces when calculating the text width.

        This flag tells wxControl::Ellipsize() to calculate the width of tab
        characters @c '\\t' as 6 spaces.
     */
    wxELLIPSIZE_FLAGS_EXPAND_TABS = 2,

    /// The default flags for wxControl::Ellipsize.
    wxELLIPSIZE_FLAGS_DEFAULT = wxELLIPSIZE_FLAGS_PROCESS_MNEMONICS|
                                wxELLIPSIZE_FLAGS_EXPAND_TABS
};


/**
    The different ellipsization modes supported by the
    wxControl::Ellipsize and wxRendererNative::DrawItemText() functions.
*/
enum wxEllipsizeMode
{
    /// Don't ellipsize the text at all. @since 2.9.1
    wxELLIPSIZE_NONE,

    /// Put the ellipsis at the start of the string, if the string needs ellipsization.
    wxELLIPSIZE_START,

    /// Put the ellipsis in the middle of the string, if the string needs ellipsization.
    wxELLIPSIZE_MIDDLE,

    /// Put the ellipsis at the end of the string, if the string needs ellipsization.
    wxELLIPSIZE_END
};



/**
    @class wxRealPoint

    A wxRealPoint is a useful data structure for graphics operations.

    It contains floating point @e x and @e y members.
    See wxPoint for an integer version.

    Note that the coordinates stored inside a wxRealPoint object may be negative
    and that wxRealPoint functions do not perform any check against negative values.

    @library{wxcore}
    @category{data}

    @see wxPoint
*/
class wxRealPoint
{
public:
    /**
        Initializes to zero the x and y members.
    */
    wxRealPoint();

    /**
        Initializes the point with the given coordinates.
    */
    wxRealPoint(double x, double y);

    /**
        Converts the given wxPoint (with integer coordinates) to a wxRealPoint.
    */
    wxRealPoint(const wxPoint& pt);

    /**
        @name Miscellaneous operators

        Note that these operators are documented as class members
        (to make them easier to find) but, as their prototype shows,
        they are implemented as global operators; note that this is
        transparent to the user but it helps to understand why the
        following functions are documented to take the wxPoint they
        operate on as an explicit argument.
    */
    //@{
    wxRealPoint& operator=(const wxRealPoint& pt);

    bool operator ==(const wxRealPoint& p1, const wxRealPoint& p2);
    bool operator !=(const wxRealPoint& p1, const wxRealPoint& p2);

    wxRealPoint operator +(const wxRealPoint& p1, const wxRealPoint& p2);
    wxRealPoint operator -(const wxRealPoint& p1, const wxRealPoint& p2);

    wxRealPoint& operator +=(const wxRealPoint& pt);
    wxRealPoint& operator -=(const wxRealPoint& pt);

    wxRealPoint operator +(const wxRealPoint& pt, const wxSize& sz);
    wxRealPoint operator -(const wxRealPoint& pt, const wxSize& sz);
    wxRealPoint operator +(const wxSize& sz, const wxRealPoint& pt);
    wxRealPoint operator -(const wxSize& sz, const wxRealPoint& pt);

    wxRealPoint& operator +=(const wxSize& sz);
    wxRealPoint& operator -=(const wxSize& sz);

    wxSize operator /(const wxRealPoint& sz, int factor);
    wxSize operator *(const wxRealPoint& sz, int factor);
    wxSize operator *(int factor, const wxSize& sz);
    wxSize& operator /=(int factor);
    wxSize& operator *=(int factor);
    //@}

    /**
        X coordinate of this point.
    */
    double x;

    /**
        Y coordinate of this point.
    */
    double y;
};



/**
    Represents a rectangle with integer coordinates.

    @c x and @c y members specify the coordinates of the rectangle top-left
    corner and @c width and @c height specify its width and height
    respectively. The usual C++ semi-open interval convention is used: point
    @c p lies inside the rectangle if and only if both conditions below are
    satisfied:
    @code
        x <= p.x < x + width
        y <= p.y < y + height
    @endcode

    In other words, the rectangle left and right boundaries are at @c x and @c
    x+width-1 and its top and bottom boundaries are at @c y and @c y+height-1
    respectively.

    Note that the x and y coordinates may be negative, but width and height are
    always strictly positive for non-empty rectangles.

    @library{wxcore}
    @category{data}

    @see wxPoint, wxSize
*/
class wxRect
{
public:
    /**
        Default constructor.
        Initializes to zero the internal @a x, @a y, @a width and @a height members.
    */
    wxRect();
    /**
        Creates a wxRect object from @a x, @a y, @a width and @a height values.
    */
    wxRect(int x, int y, int width, int height);
    /**
        Creates a wxRect object from top-left and bottom-right points.

        The width of the rectangle will be @c bottomRight.x-topLeft.x+1 and the
        height will be @c bottomRight.y-topLeft.y+1.
    */
    wxRect(const wxPoint& topLeft, const wxPoint& bottomRight);
    /**
        Creates a wxRect object from position @a pos and @a size values.
    */
    wxRect(const wxPoint& pos, const wxSize& size);
    /**
        Creates a wxRect object from @a size values at the origin.
    */
    wxRect(const wxSize& size);

    //@{
    /**
        Returns the rectangle having the same size as this one but centered
        relatively to the given rectangle @a r. By default, rectangle is
        centred in both directions but if @a dir includes only @c wxVERTICAL or
        only @c wxHORIZONTAL, then it is only centered in this direction while
        the other component of its position remains unchanged.
    */
    wxRect CentreIn(const wxRect& r, int dir = wxBOTH) const;
    wxRect CenterIn(const wxRect& r, int dir = wxBOTH) const;
    //@}

    /**
        Returns @true if the given point is inside the rectangle (or on its
        boundary) and @false otherwise.
    */
    bool Contains(int x, int y) const;
    /**
        Returns @true if the given point is inside the rectangle (or on its
        boundary) and @false otherwise.
    */
    bool Contains(const wxPoint& pt) const;
    /**
        Returns @true if the given rectangle is completely inside this
        rectangle (or touches its boundary) and @false otherwise.
    */
    bool Contains(const wxRect& rect) const;

    //@{
    /**
        Decrease the rectangle size.

        This method is the opposite from Inflate(): Deflate(a, b) is equivalent
        to Inflate(-a, -b). Please refer to Inflate() for full description.
    */
    wxRect& Deflate(wxCoord dx, wxCoord dy);
    wxRect& Deflate(const wxSize& diff);
    wxRect& Deflate(wxCoord diff);
    wxRect  Deflate(wxCoord dx, wxCoord dy) const;
    //@}

    /**
        Gets the bottom point of the rectangle.
    */
    int GetBottom() const;

    /**
        Gets the position of the bottom left corner.
    */
    wxPoint GetBottomLeft() const;

    /**
        Gets the position of the bottom right corner.
    */
    wxPoint GetBottomRight() const;

    /**
        Gets the height member.
    */
    int GetHeight() const;

    /**
        Gets the left point of the rectangle (the same as GetX()).
    */
    int GetLeft() const;

    /**
        Gets the position.
    */
    wxPoint GetPosition() const;

    /**
        Gets the right point of the rectangle.
    */
    int GetRight() const;

    /**
        Gets the size.

        @see SetSize()
    */
    wxSize GetSize() const;

    /**
        Gets the top point of the rectangle (the same as GetY()).
    */
    int GetTop() const;

    /**
        Gets the position of the top left corner of the rectangle, same as
        GetPosition().
    */
    wxPoint GetTopLeft() const;

    /**
        Gets the position of the top right corner.
    */
    wxPoint GetTopRight() const;

    /**
        Gets the width member.
    */
    int GetWidth() const;

    /**
        Gets the x member.
    */
    int GetX() const;

    /**
        Gets the y member.
    */
    int GetY() const;

    //@{
    /**
        Increases the size of the rectangle.

        The left border is moved farther left and the right border is moved
        farther right by @a dx. The upper border is moved farther up and the
        bottom border is moved farther down by @a dy. (Note that the width and
        height of the rectangle thus change by 2*dx and 2*dy, respectively.) If
        one or both of @a dx and @a dy are negative, the opposite happens: the
        rectangle size decreases in the respective direction.

        Inflating and deflating behaves "naturally". Defined more precisely,
        that means:
        -# "Real" inflates (that is, @a dx and/or @a dy = 0) are not
           constrained. Thus inflating a rectangle can cause its upper left
           corner to move into the negative numbers. (2.5.4 and older forced
           the top left coordinate to not fall below (0, 0), which implied a
           forced move of the rectangle.)
        -# Deflates are clamped to not reduce the width or height of the
           rectangle below zero. In such cases, the top-left corner is
           nonetheless handled properly. For example, a rectangle at (10, 10)
           with size (20, 40) that is inflated by (-15, -15) will become
           located at (20, 25) at size (0, 10). Finally, observe that the width
           and height are treated independently. In the above example, the
           width is reduced by 20, whereas the height is reduced by the full 30
           (rather than also stopping at 20, when the width reached zero).

        @see Deflate()
    */
    wxRect& Inflate(wxCoord dx, wxCoord dy);
    wxRect& Inflate(const wxSize& diff);
    wxRect& Inflate(wxCoord diff);
    wxRect Inflate(wxCoord dx, wxCoord dy) const;
    //@}

    /**
        Modifies this rectangle to contain the overlapping portion of this rectangle
        and the one passed in as parameter.

        @return This rectangle, modified.
    */
    wxRect& Intersect(const wxRect& rect);

    /**
        Returns the overlapping portion of this rectangle and the one passed in as
        parameter.
    */
    wxRect Intersect(const wxRect& rect) const;

    /**
        Returns @true if this rectangle has a non-empty intersection with the
        rectangle @a rect and @false otherwise.
    */
    bool Intersects(const wxRect& rect) const;

    /**
        Returns @true if this rectangle has a width or height less than or
        equal to 0 and @false otherwise.
    */
    bool IsEmpty() const;

    //@{
    /**
        Moves the rectangle by the specified offset. If @a dx is positive, the
        rectangle is moved to the right, if @a dy is positive, it is moved to the
        bottom, otherwise it is moved to the left or top respectively.
    */
    void Offset(wxCoord dx, wxCoord dy);
    void Offset(const wxPoint& pt);
    //@}

    /**
        Sets the height.
    */
    void SetHeight(int height);

    /**
        Sets the position.
    */
    void SetPosition(const wxPoint& pos);

    /**
        Sets the size.

        @see GetSize()
    */
    void SetSize(const wxSize& s);

    /**
        Sets the width.
    */
    void SetWidth(int width);

    /**
        Sets the x position.
    */
    void SetX(int x);

    /**
        Sets the y position.
    */
    void SetY(int y);

    /**
       Set the left side of the rectangle.

       Notice that because the rectangle stores its left side and width,
       calling SetLeft() changes the right side position too -- but does
       preserve the width.
    */
    void SetLeft(int left);

    /**
       Set the right side of the rectangle.

       Notice that this doesn't affect GetLeft() return value but changes the
       rectangle width to set its right side to the given position.
     */
    void SetRight(int right);

    /**
       Set the top edge of the rectangle.

       Notice that because the rectangle stores its top side and height,
       calling SetTop() changes the bottom side position too -- but does
       preserve the height.
     */
    void SetTop(int top);

    /**
       Set the bottom edge of the rectangle.

       Notice that this doesn't affect GetTop() return value but changes the
       rectangle height to set its bottom side to the given position.
     */
    void SetBottom(int bottom);

    /**
       Set the top-left point of the rectangle.
     */
    void SetTopLeft(const wxPoint &p);

    /**
       Set the bottom-right point of the rectangle.
     */
    void SetBottomRight(const wxPoint &p);

    /**
       Set the top-right point of the rectangle.
     */
    void SetTopRight(const wxPoint &p);

    /**
       Set the bottom-left point of the rectangle.
     */
    void SetBottomLeft(const wxPoint &p);


    //@{
    /**
        Modifies the rectangle to contain the bounding box of this rectangle
        and the one passed in as parameter.
    */
    wxRect Union(const wxRect& rect) const;
    wxRect& Union(const wxRect& rect);
    //@}

    /**
        Inequality operator.
    */
    bool operator !=(const wxRect& r1, const wxRect& r2);

    //@{
    /**
        Like Union(), but doesn't treat empty rectangles specially.
    */
    wxRect operator +(const wxRect& r1, const wxRect& r2);
    wxRect& operator +=(const wxRect& r);
    //@}

    //@{
    /**
        Returns the intersection of two rectangles (which may be empty).
    */
    wxRect operator *(const wxRect& r1, const wxRect& r2);
    wxRect& operator *=(const wxRect& r);
    //@}

    /**
        Assignment operator.
    */
    wxRect& operator=(const wxRect& rect);

    /**
        Equality operator.
    */
    bool operator ==(const wxRect& r1, const wxRect& r2);

    /**
        Height member.
    */
    int height;

    /**
        Width member.
    */
    int width;

    /**
        x coordinate of the top-left corner of the rectangle.
    */
    int x;

    /**
        y coordinate of the top-left corner of the rectangle.
    */
    int y;
};



/**
    @class wxPoint

    A wxPoint is a useful data structure for graphics operations.

    It contains integer @e x and @e y members.
    See wxRealPoint for a floating point version.

    Note that the width and height stored inside a wxPoint object may be negative
    and that wxPoint functions do not perform any check against negative values
    (this is used to e.g. store the special -1 value in ::wxDefaultPosition instance).

    @library{wxcore}
    @category{data}

    @stdobjects
    ::wxDefaultPosition

    @see wxRealPoint
*/
class wxPoint
{
public:
    /**
        Constructs a point.
        Initializes the internal x and y coordinates to zero.
    */
    wxPoint();

    /**
        Initializes the point object with the given @a x and @a y coordinates.
    */
    wxPoint(int x, int y);

    /**
        Converts the given wxRealPoint (with floating point coordinates) to a
        wxPoint instance.

        Notice that this truncates the floating point values of @a pt
        components, if you want to round them instead you need to do it
        manually, e.g.
        @code
            #include <wx/math.h>    // for wxRound()

            wxRealPoint rp = ...;
            wxPoint p(wxRound(rp.x), wxRound(rp.y));
        @endcode
    */
    wxPoint(const wxRealPoint& pt);

    /**
        @name Miscellaneous operators

        Note that these operators are documented as class members
        (to make them easier to find) but, as their prototype shows,
        they are implemented as global operators; note that this is
        transparent to the user but it helps to understand why the
        following functions are documented to take the wxPoint they
        operate on as an explicit argument.
    */
    //@{
    wxPoint& operator=(const wxPoint& pt);

    bool operator ==(const wxPoint& p1, const wxPoint& p2);
    bool operator !=(const wxPoint& p1, const wxPoint& p2);

    wxPoint operator +(const wxPoint& p1, const wxPoint& p2);
    wxPoint operator -(const wxPoint& p1, const wxPoint& p2);

    wxPoint& operator +=(const wxPoint& pt);
    wxPoint& operator -=(const wxPoint& pt);

    wxPoint operator +(const wxPoint& pt, const wxSize& sz);
    wxPoint operator -(const wxPoint& pt, const wxSize& sz);
    wxPoint operator +(const wxSize& sz, const wxPoint& pt);
    wxPoint operator -(const wxSize& sz, const wxPoint& pt);

    wxPoint& operator +=(const wxSize& sz);
    wxPoint& operator -=(const wxSize& sz);

    wxSize operator /(const wxPoint& sz, int factor);
    wxSize operator *(const wxPoint& sz, int factor);
    wxSize operator *(int factor, const wxSize& sz);
    wxSize& operator /=(int factor);
    wxSize& operator *=(int factor);
    //@}


    /**
        @name Defaults handling.

        Test for and set non-specified wxPoint components.

        Although a wxPoint is always initialized to (0, 0), wxWidgets commonly
        uses wxDefaultCoord (defined as @c -1) to indicate that a point hasn't
        been initialized or specified. In particular, ::wxDefaultPosition is
        used in many places with this meaning.
     */
    //@{

    /**
        Returns @true if neither of the point components is equal to
        wxDefaultCoord.

        This method is typically used before calling SetDefaults().

        @since 2.9.2
    */
    bool IsFullySpecified() const;

    /**
        Combine this object with another one replacing the uninitialized
        values.

        It is typically used like this:

        @code
        if ( !pos.IsFullySpecified() )
        {
            pos.SetDefaults(GetDefaultPosition());
        }
        @endcode

        @see IsFullySpecified()

        @since 2.9.2
    */
    void SetDefaults(const wxPoint& pt);
    //@}

    /**
        x member.
    */
    int x;

    /**
        y member.
    */
    int  y;
};

/**
    Global instance of a wxPoint initialized with values (-1,-1).
*/
const wxPoint wxDefaultPosition;


/**
    @class wxColourDatabase

    wxWidgets maintains a database of standard RGB colours for a predefined
    set of named colours. The application may add to this set if desired by
    using AddColour() and may use it to look up colours by names using Find()
    or find the names for the standard colour using FindName().

    There is one predefined, global instance of this class called
    ::wxTheColourDatabase.

    The standard database contains at least the following colours:

    @beginTable
    <tr><td>
       AQUAMARINE
    @n BLACK
    @n BLUE
    @n BLUE VIOLET
    @n BROWN
    @n CADET BLUE
    @n CORAL
    @n CORNFLOWER BLUE
    @n CYAN
    @n DARK GREY
    @n DARK GREEN
    @n DARK OLIVE GREEN
    @n DARK ORCHID
    @n DARK SLATE BLUE
    @n DARK SLATE GREY
    @n DARK TURQUOISE
    @n DIM GREY
    </td><td>
       FIREBRICK
    @n FOREST GREEN
    @n GOLD
    @n GOLDENROD
    @n GREY
    @n GREEN
    @n GREEN YELLOW
    @n INDIAN RED
    @n KHAKI
    @n LIGHT BLUE
    @n LIGHT GREY
    @n LIGHT STEEL BLUE
    @n LIME GREEN
    @n MAGENTA
    @n MAROON
    @n MEDIUM AQUAMARINE
    @n MEDIUM BLUE
    </td><td>
       MEDIUM FOREST GREEN
    @n MEDIUM GOLDENROD
    @n MEDIUM ORCHID
    @n MEDIUM SEA GREEN
    @n MEDIUM SLATE BLUE
    @n MEDIUM SPRING GREEN
    @n MEDIUM TURQUOISE
    @n MEDIUM VIOLET RED
    @n MIDNIGHT BLUE
    @n NAVY
    @n ORANGE
    @n ORANGE RED
    @n ORCHID
    @n PALE GREEN
    @n PINK
    @n PLUM
    @n PURPLE
    </td><td>
       RED
    @n SALMON
    @n SEA GREEN
    @n SIENNA
    @n SKY BLUE
    @n SLATE BLUE
    @n SPRING GREEN
    @n STEEL BLUE
    @n TAN
    @n THISTLE
    @n TURQUOISE
    @n VIOLET
    @n VIOLET RED
    @n WHEAT
    @n WHITE
    @n YELLOW
    @n YELLOW GREEN
    </td></tr>
    @endTable

    @library{wxcore}
    @category{gdi}

    @see wxColour
*/
class wxColourDatabase
{
public:
    /**
        Constructs the colour database. It will be initialized at the first
        use.
    */
    wxColourDatabase();

    /**
        Adds a colour to the database. If a colour with the same name already
        exists, it is replaced.
    */
    void AddColour(const wxString& colourName, const wxColour& colour);

    /**
        Finds a colour given the name. Returns an invalid colour object (that
        is, wxColour::IsOk() will return @false) if the colour wasn't found in
        the database.
    */
    wxColour Find(const wxString& colourName) const;

    /**
        Finds a colour name given the colour. Returns an empty string if the
        colour is not found in the database.
    */
    wxString FindName(const wxColour& colour) const;
};


/**
    Global instance of a wxColourDatabase.
*/
wxColourDatabase* wxTheColourDatabase;


/**
    @class wxSize

    A wxSize is a useful data structure for graphics operations.
    It simply contains integer @e x and @e y members.

    Note that the width and height stored inside a wxSize object may be negative
    and that wxSize functions do not perform any check against negative values
    (this is used to e.g. store the special -1 value in ::wxDefaultSize instance).
    See also IsFullySpecified() and SetDefaults() for utility functions regarding
    the special -1 value.

    wxSize is used throughout wxWidgets as well as wxPoint which, although
    almost equivalent to wxSize, has a different meaning: wxPoint represents a
    position while wxSize represents the size.

    @library{wxcore}
    @category{data}

    @stdobjects
    ::wxDefaultSize

    @see wxPoint, wxRealPoint
*/
class wxSize
{
public:
    /**
        Initializes this size object with zero width and height.
    */
    wxSize();

    /**
        Initializes this size object with the given @a width and @a height.
    */
    wxSize(int width, int height);

    //@{
    /**
        Decreases the size in both x and y directions.

        @see IncBy()
    */
    void DecBy(const wxPoint& pt);
    void DecBy(const wxSize& size);
    void DecBy(int dx, int dy);
    void DecBy(int d);
    //@}

    /**
        Decrements this object so that both of its dimensions are not greater
        than the corresponding dimensions of the @a size.

        @see IncTo()
    */
    void DecTo(const wxSize& size);

    /**
        Decrements this object to be not bigger than the given size ignoring
        non-specified components.

        This is similar to DecTo() but doesn't do anything for x or y
        component if the same component of @a size is not specified, i.e. set
        to ::wxDefaultCoord.

        @since 2.9.5
     */
    void DecToIfSpecified(const wxSize& size);

    /**
        Gets the height member.
    */
    int GetHeight() const;

    /**
        Gets the width member.
    */
    int GetWidth() const;

    //@{
    /**
        Increases the size in both x and y directions.

        @see DecBy()
    */
    void IncBy(const wxPoint& pt);
    void IncBy(const wxSize& size);
    void IncBy(int dx, int dy);
    void IncBy(int d);
    //@}

    /**
        Increments this object so that both of its dimensions are not less than
        the corresponding dimensions of the @a size.

        @see DecTo()
    */
    void IncTo(const wxSize& size);

    /**
        Returns @true if neither of the size object components is equal to -1,
        which is used as default for the size values in wxWidgets (hence the
        predefined ::wxDefaultSize has both of its components equal to -1).

        This method is typically used before calling SetDefaults().
    */
    bool IsFullySpecified() const;

    /**
        Scales the dimensions of this object by the given factors. If you want
        to scale both dimensions by the same factor you can also use
        operator*=().

        @return A reference to this object (so that you can concatenate other
                 operations in the same line).
    */
    wxSize& Scale(double xscale, double yscale);

    /**
        Sets the width and height members.
    */
    void Set(int width, int height);

    /**
        Combine this size object with another one replacing the default (i.e.\ equal to -1)
        components of this object with those of the other. It is typically used like this:

        @code
        if ( !size.IsFullySpecified() )
        {
            size.SetDefaults(GetDefaultSize());
        }
        @endcode

        @see IsFullySpecified()
    */
    void SetDefaults(const wxSize& sizeDefault);

    /**
        Sets the height.
    */
    void SetHeight(int height);

    /**
        Sets the width.
    */
    void SetWidth(int width);


    /**
        @name Miscellaneous operators

        Note that these operators are documented as class members
        (to make them easier to find) but, as their prototype shows,
        they are implemented as global operators; note that this is
        transparent to the user but it helps to understand why the
        following functions are documented to take the wxSize they
        operate on as an explicit argument.
    */
    //@{
    wxSize& operator=(const wxSize& sz);

    bool operator ==(const wxSize& s1, const wxSize& s2);
    bool operator !=(const wxSize& s1, const wxSize& s2);

    wxSize operator +(const wxSize& s1, const wxSize& s2);
    wxSize operator -(const wxSize& s1, const wxSize& s2);
    wxSize& operator +=(const wxSize& sz);
    wxSize& operator -=(const wxSize& sz);

    wxSize operator /(const wxSize& sz, int factor);
    wxSize operator *(const wxSize& sz, int factor);
    wxSize operator *(int factor, const wxSize& sz);
    wxSize& operator /=(int factor);
    wxSize& operator *=(int factor);
    //@}
};

/**
    Global instance of a wxSize object initialized to (-1,-1).
*/
const wxSize wxDefaultSize;




// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_gdi */
//@{

/**
    This macro loads a bitmap from either application resources (on the
    platforms for which they exist, i.e.\ Windows) or from an XPM file.
    This can help to avoid using @ifdef_ when creating bitmaps.

    @see @ref overview_bitmap, wxICON()

    @header{wx/gdicmn.h}
*/
#define wxBITMAP(bitmapName)

/**
    Creates a bitmap from either application resources or embedded image data
    in PNG format.

    This macro is similar to wxBITMAP() but works with bitmap data in PNG
    format and not BMP or XPM.

    Under Windows the given @a bitmapName must be present in the application
    resource file with the type @c RCDATA and refer to a PNG image. I.e. you
    should have a definition similar to the following in your @c .rc file:
    @code
        mybitmap    RCDATA  "mybitmap.png"
    @endcode
    to be able to use @c wxBITMAP_PNG(mybitmap) in the code.

    Under macOS the file with the specified name and "png" extension must be
    present in the "Resources" subdirectory of the application bundle.

    Under the other platforms, this is equivalent to wxBITMAP_PNG_FROM_DATA()
    and so loads the image data from the array called @c bitmapName_png that
    must exist. Notice that it @e must be an array and not a pointer as the
    macro needs to be able to determine its size. Such an array can be produced
    by a number of conversion programs. A very simple one is included in
    wxWidgets distribution as @c misc/scripts/png2c.py.

    Finally notice that you must register PNG image handler to be able to
    load bitmaps from PNG data. This can be done either by calling
    wxInitAllImageHandlers() which also registers all the other image formats
    or including the necessary header:
    @code
        #include <wx/imagpng.h>
    @endcode
    and calling
    @code
        wxImage::AddHandler(new wxPNGHandler);
    @endcode
    in your application startup code.

    @see wxBITMAP_PNG_FROM_DATA()

    @header{wx/gdicmn.h}

    @since 2.9.5
 */
#define wxBITMAP_PNG(bitmapName)

/**
    Creates a bitmap from embedded image data in PNG format.

    This macro is a thin wrapper around wxBitmap::NewFromPNGData() and takes
    just the base name of the array containing the image data and computes its
    size internally. In other words, the array called @c bitmapName_png must
    exist. Notice that it @e must be an array and not a pointer as the macro
    needs to be able to determine its size. Such an array can be produced by a
    number of conversion programs. A very simple one is included in wxWidgets
    distribution as @c misc/scripts/png2c.py.

    You can use wxBITMAP_PNG() to load the PNG bitmaps from resources on the
    platforms that support this and only fall back to loading them from data
    under the other ones (i.e. not Windows and not macOS).

    @header{wx/gdicmn.h}

    @since 2.9.5
 */
#define wxBITMAP_PNG_FROM_DATA(bitmapName)

/**
    This macro loads an icon from either application resources (on the
    platforms for which they exist, i.e.\ Windows) or from an XPM file.
    This can help to avoid using @ifdef_ when creating icons.

    @see @ref overview_bitmap, wxBITMAP()

    @header{wx/gdicmn.h}
*/
#define wxICON(iconName)

/**
    Returns @true if the display is colour, @false otherwise.

    @note Use of this function is not recommended in the new code as it only
        works for the primary display. Use wxDisplay::GetDepth() to retrieve
        the depth of the appropriate display and compare it with 1 instead.

    @header{wx/gdicmn.h}
*/
bool wxColourDisplay();

/**
    Returns the depth of the display (a value of 1 denotes a monochrome
    display).

    @note Use of this function is not recommended in the new code as it only
        works for the primary display. Use wxDisplay::GetDepth() to retrieve
        the depth of the appropriate display instead.

    @header{wx/gdicmn.h}
*/
int wxDisplayDepth();

/**
    Globally sets the cursor; only has an effect on Windows, Mac and GTK+. You
    should call this function with wxNullCursor to restore the system cursor.

    @see wxCursor, wxWindow::SetCursor()

    @header{wx/gdicmn.h}
*/
void wxSetCursor(const wxCursor& cursor);

//@}

/** @addtogroup group_funcmacro_gdi */
//@{
/**
    Returns the dimensions of the work area on the display.

    This is the same as wxGetClientDisplayRect() but allows retrieving the
    individual components instead of the entire rectangle.

    Any of the output pointers can be @NULL if the corresponding value is not
    needed by the caller.

    @see wxDisplay

    @header{wx/gdicmn.h}
*/
void wxClientDisplayRect(int* x, int* y, int* width, int* height);
//@}

/** @addtogroup group_funcmacro_gdi */
//@{
/**
    Returns the dimensions of the work area on the display. On Windows this
    means the area not covered by the taskbar, etc. Other platforms are
    currently defaulting to the whole display until a way is found to provide
    this info for all window managers, etc.

    @see wxDisplay

    @header{wx/gdicmn.h}
*/
wxRect wxGetClientDisplayRect();
//@}

/** @addtogroup group_funcmacro_gdi */
//@{
/**
    Returns the display resolution in pixels per inch.

    The @c x component of the returned wxSize object contains the horizontal
    resolution and the @c y one -- the vertical resolution.

    @note Use of this function is not recommended in the new code as it only
        works for the primary display. Use wxDisplay::GetPPI() to retrieve
        the resolution of the appropriate display instead.

    @header{wx/gdicmn.h}

    @see wxDisplay

    @since 2.9.0
*/
wxSize wxGetDisplayPPI();
//@}

/** @addtogroup group_funcmacro_gdi */
//@{
/**
    Returns the display size in pixels.

    @note Use of this function is not recommended in the new code as it only
        works for the primary display. Use wxDisplay::GetGeometry() to retrieve
        the size of the appropriate display instead.

    Either of output pointers can be @NULL if the caller is not interested in
    the corresponding value.

    @see wxGetDisplaySize(), wxDisplay

    @header{wx/gdicmn.h}
*/
void wxDisplaySize(int* width, int* height);
//@}

/** @addtogroup group_funcmacro_gdi */
//@{
/**
    Returns the display size in pixels.

    @note Use of this function is not recommended in the new code as it only
        works for the primary display. Use wxDisplay::GetGeometry() to retrieve
        the size of the appropriate display instead.

    @see wxDisplay

    @header{wx/gdicmn.h}
*/
wxSize wxGetDisplaySize();
//@}

/** @addtogroup group_funcmacro_gdi */
//@{
/**
    Returns the display size in millimeters.

    Either of output pointers can be @NULL if the caller is not interested in
    the corresponding value.

    @see wxGetDisplaySizeMM(), wxDisplay

    @header{wx/gdicmn.h}
*/
void wxDisplaySizeMM(int* width, int* height);
//@}

/** @addtogroup group_funcmacro_gdi */
//@{
/**
    Returns the display size in millimeters.

    @see wxDisplay

    @header{wx/gdicmn.h}
*/
wxSize wxGetDisplaySizeMM();
//@}

