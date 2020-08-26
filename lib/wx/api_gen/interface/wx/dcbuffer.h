/////////////////////////////////////////////////////////////////////////////
// Name:        dcbuffer.h
// Purpose:     interface of wxBufferedDC
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// Assumes the buffer bitmap covers the entire scrolled window,
// and prepares the window DC accordingly
#define wxBUFFER_VIRTUAL_AREA       0x01

// Assumes the buffer bitmap only covers the client area;
// does not prepare the window DC
#define wxBUFFER_CLIENT_AREA        0x02

// Set when not using specific buffer bitmap. Note that this
// is private style and not returned by GetStyle.
#define wxBUFFER_USES_SHARED_BUFFER 0x04


/**
    @class wxBufferedDC

    This class provides a simple way to avoid flicker: when drawing on it,
    everything is in fact first drawn on an in-memory buffer (a wxBitmap) and
    then copied to the screen, using the associated wxDC, only once, when this
    object is destroyed. wxBufferedDC itself is typically associated with
    wxClientDC, if you want to use it in your @c EVT_PAINT handler, you should
    look at wxBufferedPaintDC instead.

    When used like this, a valid @e DC must be specified in the constructor
    while the @e buffer bitmap doesn't have to be explicitly provided, by
    default this class will allocate the bitmap of required size itself.
    However using a dedicated bitmap can speed up the redrawing process by
    eliminating the repeated creation and destruction of a possibly big bitmap.
    Otherwise, wxBufferedDC can be used in the same way as any other device
    context.

    Another possible use for wxBufferedDC is to use it to maintain a
    backing store for the window contents. In this case, the associated @e DC
    may be @NULL but a valid backing store bitmap should be specified.

    Finally, please note that GTK+ 2.0 as well as macOS provide double buffering
    themselves natively. You can either use wxWindow::IsDoubleBuffered() to
    determine whether you need to use buffering or not, or use
    wxAutoBufferedPaintDC to avoid needless double buffering on the systems
    which already do it automatically.

    @library{wxcore}
    @category{dc}

    @see wxDC, wxMemoryDC, wxBufferedPaintDC, wxAutoBufferedPaintDC
*/
class wxBufferedDC : public wxMemoryDC
{
public:
    /**
        Default constructor. You must call one of the Init() methods later in
        order to use the device context.
    */
    wxBufferedDC();

    /**
        Creates a buffer for the provided @a dc. Init() must not be called when
        using this constructor.

        @param dc
            The underlying DC: everything drawn to this object will be flushed
            to this DC when this object is destroyed. You may pass @NULL in
            order to just initialize the buffer, and not flush it.
        @param area
            The size of the bitmap to be used for buffering (this bitmap is
            created internally when it is not given explicitly).
        @param style
            wxBUFFER_CLIENT_AREA to indicate that just the client area of the
            window is buffered, or wxBUFFER_VIRTUAL_AREA to indicate that the
            buffer bitmap covers the virtual area.
    */
    wxBufferedDC(wxDC* dc, const wxSize& area,
                 int style = wxBUFFER_CLIENT_AREA);

    /**
        Creates a buffer for the provided dc. Init() must not be called when
        using this constructor.

        @param dc
            The underlying DC: everything drawn to this object will be flushed
            to this DC when this object is destroyed. You may pass @NULL in
            order to just initialize the buffer, and not flush it.
        @param buffer
            Explicitly provided bitmap to be used for buffering: this is the
            most efficient solution as the bitmap doesn't have to be recreated
            each time but it also requires more memory as the bitmap is never
            freed. The bitmap should have appropriate size, anything drawn
            outside of its bounds is clipped.
        @param style
            wxBUFFER_CLIENT_AREA to indicate that just the client area of the
            window is buffered, or wxBUFFER_VIRTUAL_AREA to indicate that the
            buffer bitmap covers the virtual area.
    */
    wxBufferedDC(wxDC* dc, wxBitmap& buffer = wxNullBitmap,
                 int style = wxBUFFER_CLIENT_AREA);

    /**
        Copies everything drawn on the DC so far to the underlying DC
        associated with this object, if any.
    */
    virtual ~wxBufferedDC();

    //@{
    /**
        Initializes the object created using the default constructor. Please
        see the constructors for parameter details.
    */
    void Init(wxDC* dc, const wxSize& area,
              int style = wxBUFFER_CLIENT_AREA);
    void Init(wxDC* dc, wxBitmap& buffer = wxNullBitmap,
              int style = wxBUFFER_CLIENT_AREA);
    //@}


    /**
       Blits the buffer to the dc, and detaches the dc from the buffer (so it
       can be effectively used once only).

       Usually only called in the destructor or by the destructor of derived
       classes if the BufferedDC must blit before the derived class (which may
       own the dc it's blitting to) is destroyed.
    */
    void UnMask();

    /**
       Set the style.
    */
    void SetStyle(int style);

    /**
       Get the style.
    */
    int GetStyle() const;
};



/**
    @class wxAutoBufferedPaintDC

    This wxDC derivative can be used inside of an @c EVT_PAINT() event handler
    to achieve double-buffered drawing. Just use this class instead of
    wxPaintDC and make sure wxWindow::SetBackgroundStyle() is called with
    wxBG_STYLE_PAINT somewhere in the class initialization code, and that's
    all you have to do to (mostly) avoid flicker.

    The difference between wxBufferedPaintDC and this class is that this class
    won't double-buffer on platforms which have native double-buffering
    already, avoiding any unnecessary buffering to avoid flicker.

    wxAutoBufferedPaintDC is simply a typedef of wxPaintDC on platforms that
    have native double-buffering, otherwise, it is a typedef of
    wxBufferedPaintDC.

    @library{wxcore}
    @category{dc}

    @see wxDC, wxBufferedPaintDC, wxPaintDC
*/
class wxAutoBufferedPaintDC : public wxBufferedPaintDC
{
public:
    /**
        Constructor. Pass a pointer to the window on which you wish to paint.
    */
    wxAutoBufferedPaintDC(wxWindow* window);
};


/**
 * Check if the window is natively double buffered and will return a wxPaintDC
 * if it is, a wxBufferedPaintDC otherwise.  It is the caller's responsibility
 * to delete the wxDC pointer when finished with it.
 */
wxDC* wxAutoBufferedPaintDCFactory(wxWindow* window);


/**
    @class wxBufferedPaintDC

    This is a subclass of wxBufferedDC which can be used inside of an
    @c EVT_PAINT() event handler to achieve double-buffered drawing. Just use
    this class instead of wxPaintDC and make sure
    wxWindow::SetBackgroundStyle() is called with wxBG_STYLE_PAINT somewhere
    in the class initialization code, and that's all you have to do to (mostly)
    avoid flicker. The only thing to watch out for is that if you are using
    this class together with wxScrolled, you probably do @b not want to call
    wxScrolled::PrepareDC() on it as it already does this internally for the
    real underlying wxPaintDC.

    @library{wxcore}
    @category{dc}

    @see wxDC, wxBufferedDC, wxAutoBufferedPaintDC, wxPaintDC
*/
class wxBufferedPaintDC : public wxBufferedDC
{
public:
    //@{
    /**
        As with wxBufferedDC, you may either provide the bitmap to be used for
        buffering or let this object create one internally (in the latter case,
        the size of the client part of the window is used).

        Pass wxBUFFER_CLIENT_AREA for the @a style parameter to indicate that
        just the client area of the window is buffered, or
        wxBUFFER_VIRTUAL_AREA to indicate that the buffer bitmap covers the
        virtual area.
    */
    wxBufferedPaintDC(wxWindow* window, wxBitmap& buffer,
                      int style = wxBUFFER_CLIENT_AREA);
    wxBufferedPaintDC(wxWindow* window,
                      int style = wxBUFFER_CLIENT_AREA);
    //@}

    /**
        Copies everything drawn on the DC so far to the window associated with
        this object, using a wxPaintDC.
    */
    virtual ~wxBufferedPaintDC();
};

