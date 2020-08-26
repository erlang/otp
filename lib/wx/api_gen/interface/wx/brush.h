/////////////////////////////////////////////////////////////////////////////
// Name:        brush.h
// Purpose:     interface of wxBrush
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    The possible brush styles.
*/
enum wxBrushStyle
{
    wxBRUSHSTYLE_INVALID = -1,

    wxBRUSHSTYLE_SOLID = wxSOLID,
        /**< Solid. */

    wxBRUSHSTYLE_TRANSPARENT = wxTRANSPARENT,
        /**< Transparent (no fill). */

    wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE = wxSTIPPLE_MASK_OPAQUE,
        /**< Uses a bitmap as a stipple; the mask is used for blitting monochrome
             using text foreground and background colors. */

    wxBRUSHSTYLE_STIPPLE_MASK = wxSTIPPLE_MASK,
        /**< Uses a bitmap as a stipple; mask is used for masking areas in the
             stipple bitmap. */

    wxBRUSHSTYLE_STIPPLE = wxSTIPPLE,
        /**< Uses a bitmap as a stipple. */

    wxBRUSHSTYLE_BDIAGONAL_HATCH,
        /**< Backward diagonal hatch. */

    wxBRUSHSTYLE_CROSSDIAG_HATCH,
        /**< Cross-diagonal hatch. */

    wxBRUSHSTYLE_FDIAGONAL_HATCH,
        /**< Forward diagonal hatch. */

    wxBRUSHSTYLE_CROSS_HATCH,
        /**< Cross hatch. */

    wxBRUSHSTYLE_HORIZONTAL_HATCH,
        /**< Horizontal hatch. */

    wxBRUSHSTYLE_VERTICAL_HATCH,
        /**< Vertical hatch. */

    wxBRUSHSTYLE_FIRST_HATCH,
        /**< First of the hatch styles (inclusive). */

    wxBRUSHSTYLE_LAST_HATCH
        /**< Last of the hatch styles (inclusive). */
};



/**
    @class wxBrush

    A brush is a drawing tool for filling in areas. It is used for painting
    the background of rectangles, ellipses, etc. It has a colour and a style.

    On a monochrome display, wxWidgets shows all brushes as white unless the
    colour is really black.

    Do not initialize objects on the stack before the program commences, since
    other required structures may not have been set up yet. Instead, define
    global pointers to objects and create them in wxApp::OnInit or when required.

    An application may wish to create brushes with different characteristics
    dynamically, and there is the consequent danger that a large number of
    duplicate brushes will be created. Therefore an application may wish to
    get a pointer to a brush by using the global list of brushes ::wxTheBrushList,
    and calling the member function wxBrushList::FindOrCreateBrush().

    This class uses reference counting and copy-on-write internally so that
    assignments between two instances of this class are very cheap.
    You can therefore use actual objects instead of pointers without efficiency problems.
    If an instance of this class is changed it will create its own data internally
    so that other instances, which previously shared the data using the reference
    counting, are not affected.

    @library{wxcore}
    @category{gdi}

    @stdobjects
    @li ::wxNullBrush
    @li ::wxBLACK_BRUSH
    @li ::wxBLUE_BRUSH
    @li ::wxCYAN_BRUSH
    @li ::wxGREEN_BRUSH
    @li ::wxYELLOW_BRUSH
    @li ::wxGREY_BRUSH
    @li ::wxLIGHT_GREY_BRUSH
    @li ::wxMEDIUM_GREY_BRUSH
    @li ::wxRED_BRUSH
    @li ::wxTRANSPARENT_BRUSH
    @li ::wxWHITE_BRUSH

    @see wxBrushList, wxDC, wxDC::SetBrush
*/
class wxBrush : public wxGDIObject
{
public:
    /**
        Default constructor.
        The brush will be uninitialised, and wxBrush:IsOk() will return @false.
    */
    wxBrush();

    /**
        Constructs a brush from a colour object and @a style.

        @param colour
            Colour object.
        @param style
            One of the ::wxBrushStyle enumeration values.
    */
    wxBrush(const wxColour& colour, wxBrushStyle style = wxBRUSHSTYLE_SOLID);

    /**
        Constructs a stippled brush using a bitmap.
        The brush style will be set to @c wxBRUSHSTYLE_STIPPLE.
    */
    wxBrush(const wxBitmap& stippleBitmap);

    /**
        Copy constructor, uses @ref overview_refcount "reference counting".
    */
    wxBrush(const wxBrush& brush);

    /**
        Destructor.

        See @ref overview_refcount_destruct for more info.

        @remarks Although all remaining brushes are deleted when the application
                 exits, the application should try to clean up all brushes itself.
                 This is because wxWidgets cannot know if a pointer to the brush
                 object is stored in an application data structure, and there is
                 a risk of double deletion.
    */
    virtual ~wxBrush();

    /**
        Returns a reference to the brush colour.

        @see SetColour()
    */
    virtual wxColour GetColour() const;

    /**
        Gets a pointer to the stipple bitmap. If the brush does not have a @c wxBRUSHSTYLE_STIPPLE
        style, this bitmap may be non-@NULL but uninitialised (i.e. wxBitmap:IsOk() returns @false).

        @see SetStipple()
    */
    virtual wxBitmap* GetStipple() const;

    /**
        Returns the brush style, one of the ::wxBrushStyle values.

        @see SetStyle(), SetColour(), SetStipple()
    */
    virtual wxBrushStyle GetStyle() const;

    /**
        Returns @true if the style of the brush is any of hatched fills.

        @see GetStyle()
    */
    virtual bool IsHatch() const;

    /**
        Returns @true if the brush is initialised.

        Notice that an uninitialized brush object can't be queried for any
        brush properties and all calls to the accessor methods on it will
        result in an assert failure.
    */
    virtual bool IsOk() const;

    /**
        Returns @true if the brush is a valid non-transparent brush.

        This method returns @true if the brush object is initialized and has a
        non-transparent style. Notice that this should be used instead of
        simply testing whether GetStyle() returns a style different from
        wxBRUSHSTYLE_TRANSPARENT if the brush may be invalid as GetStyle()
        would assert in this case.

        @see IsTransparent()

        @since 2.9.2.
     */
    bool IsNonTransparent() const;

    /**
        Returns @true if the brush is transparent.

        A transparent brush is simply a brush with wxBRUSHSTYLE_TRANSPARENT
        style.

        Notice that this function works even for non-initialized brushes (for
        which it returns @false) unlike tests of the form <code>GetStyle() ==
        wxBRUSHSTYLE_TRANSPARENT</code> which would assert if the brush is
        invalid.

        @see IsNonTransparent()

        @since 2.9.2.
     */
    bool IsTransparent() const;


    //@{
    /**
        Sets the brush colour using red, green and blue values.

        @see GetColour()
    */
    virtual void SetColour(const wxColour& colour);
    virtual void SetColour(unsigned char red, unsigned char green, unsigned char blue);
    //@}

    /**
        Sets the stipple bitmap.

        @param bitmap
            The bitmap to use for stippling.

        @remarks The style will be set to @c wxBRUSHSTYLE_STIPPLE, unless the bitmap
                 has a mask associated to it, in which case the style will be set
                 to @c wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE.

        @see wxBitmap
    */
    virtual void SetStipple(const wxBitmap& bitmap);

    /**
        Sets the brush style.

        @param style
            One of the ::wxBrushStyle values.

        @see GetStyle()
    */
    virtual void SetStyle(wxBrushStyle style);

    /**
        Inequality operator.
        See @ref overview_refcount_equality for more info.
    */
    bool operator !=(const wxBrush& brush) const;

    /**
        Equality operator.
        See @ref overview_refcount_equality for more info.
    */
    bool operator ==(const wxBrush& brush) const;
};

/**
    An empty brush.
    wxBrush::IsOk() always returns @false for this object.
*/
wxBrush wxNullBrush;

/**
    Blue brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxBLUE_BRUSH;

/**
    Green brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxGREEN_BRUSH;

/**
    Yellow brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxYELLOW_BRUSH;

/**
    White brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxWHITE_BRUSH;

/**
    Black brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxBLACK_BRUSH;

/**
    Grey brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxGREY_BRUSH;

/**
    Medium grey brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxMEDIUM_GREY_BRUSH;

/**
    Light grey brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxLIGHT_GREY_BRUSH;

/**
    Transparent brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxTRANSPARENT_BRUSH;

/**
    Cyan brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxCYAN_BRUSH;

/**
    Red brush.
    Except for the color it has all standard attributes
    (@c wxBRUSHSTYLE_SOLID, no stipple bitmap, etc...).
*/
wxBrush* wxRED_BRUSH;



/**
    @class wxBrushList

    A brush list is a list containing all brushes which have been created.

    The application should not construct its own brush list: it should use the
    object pointer ::wxTheBrushList.

    @library{wxcore}
    @category{gdi}

    @see wxBrush
*/
class wxBrushList
{
public:
    /**
        Finds a brush with the specified attributes and returns it, else creates a new
        brush, adds it to the brush list, and returns it.

        @param colour
            Colour object.
        @param style
            Brush style. See ::wxBrushStyle for a list of styles.
    */
    wxBrush* FindOrCreateBrush(const wxColour& colour,
                               wxBrushStyle style = wxBRUSHSTYLE_SOLID);
};

/**
    The global wxBrushList instance.
*/
wxBrushList* wxTheBrushList;
