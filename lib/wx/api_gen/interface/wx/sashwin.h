/////////////////////////////////////////////////////////////////////////////
// Name:        sashwin.h
// Purpose:     interface of wxSashWindow
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
   wxSashWindow flags
*/
#define wxSW_NOBORDER         0x0000
#define wxSW_BORDER           0x0020
#define wxSW_3DSASH           0x0040
#define wxSW_3DBORDER         0x0080
#define wxSW_3D (wxSW_3DSASH | wxSW_3DBORDER)


/**
    See wxSashWindow.
*/
enum wxSashEdgePosition
{
    wxSASH_TOP = 0,
    wxSASH_RIGHT,
    wxSASH_BOTTOM,
    wxSASH_LEFT,
    wxSASH_NONE = 100
};

/**
    See wxSashEvent.
*/
enum wxSashDragStatus
{
    wxSASH_STATUS_OK,
    wxSASH_STATUS_OUT_OF_RANGE
};


/**
    @class wxSashWindow

    wxSashWindow allows any of its edges to have a sash which can be dragged
    to resize the window. The actual content window will be created by the
    application as a child of wxSashWindow.

    The window (or an ancestor) will be notified of a drag via a
    wxSashEvent notification.

    @beginStyleTable
    @style{wxSW_3D}
           Draws a 3D effect sash and border.
    @style{wxSW_3DSASH}
           Draws a 3D effect sash.
    @style{wxSW_3DBORDER}
           Draws a 3D effect border.
    @style{wxSW_BORDER}
           Draws a thin black border.
    @endStyleTable

    @beginEventEmissionTable{wxSashEvent}
    @event{EVT_SASH_DRAGGED(id, func)}
           Process a @c wxEVT_SASH_DRAGGED event, when the user has finished
           dragging a sash.
    @event{EVT_SASH_DRAGGED_RANGE(id1, id2, func)}
           Process a @c wxEVT_SASH_DRAGGED_RANGE event, when the user has
           finished dragging a sash. The event handler is called when windows
           with ids in the given range have their sashes dragged.
    @endEventTable

    @library{wxcore}
    @category{miscwnd}

    @see wxSashEvent, wxSashLayoutWindow, @ref overview_events
*/
class wxSashWindow : public wxWindow
{
public:
    /**
        Default ctor.
    */
    wxSashWindow();

    /**
        Constructs a sash window, which can be a child of a frame, dialog or any other
        non-control window.

        @param parent
            Pointer to a parent window.
        @param id
            Window identifier. If -1, will automatically create an identifier.
        @param pos
            Window position. wxDefaultPosition is (-1, -1) which indicates that
            wxSashWindows should generate a default position for the window.
            If using the wxSashWindow class directly, supply an actual position.
        @param size
            Window size. wxDefaultSize is (-1, -1) which indicates that wxSashWindows
            should generate a default size for the window.
        @param style
            Window style. For window styles, please see wxSashWindow.
        @param name
            Window name.
    */
    wxSashWindow(wxWindow* parent, wxWindowID id,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = wxCLIP_CHILDREN | wxSW_3D,
                 const wxString& name = "sashWindow");

    /**
        Destructor.
    */
    virtual ~wxSashWindow();

    /**
        Gets the maximum window size in the x direction.
    */
    virtual int GetMaximumSizeX() const;

    /**
        Gets the maximum window size in the y direction.
    */
    virtual int GetMaximumSizeY() const;

    /**
        Gets the minimum window size in the x direction.
    */
    virtual int GetMinimumSizeX() const;

    /**
        Gets the minimum window size in the y direction.
    */
    virtual int GetMinimumSizeY() const;

    /**
        Returns @true if a sash is visible on the given edge, @false otherwise.

        @param edge
            Edge. One of wxSASH_TOP, wxSASH_RIGHT, wxSASH_BOTTOM, wxSASH_LEFT.

        @see SetSashVisible()
    */
    bool GetSashVisible(wxSashEdgePosition edge) const;

    /**
        Sets the maximum window size in the x direction.
    */
    virtual void SetMaximumSizeX(int min);

    /**
        Sets the maximum window size in the y direction.
    */
    virtual void SetMaximumSizeY(int min);

    /**
        Sets the minimum window size in the x direction.
    */
    virtual void SetMinimumSizeX(int min);

    /**
        Sets the minimum window size in the y direction.
    */
    virtual void SetMinimumSizeY(int min);

    /**
        Call this function to make a sash visible or invisible on a particular edge.

        @param edge
            Edge to change. One of wxSASH_TOP, wxSASH_RIGHT, wxSASH_BOTTOM, wxSASH_LEFT.
        @param visible
            @true to make the sash visible, @false to make it invisible.

        @see GetSashVisible()
    */
    void SetSashVisible(wxSashEdgePosition edge, bool visible);


    /**
       Get border size
    */
    int GetEdgeMargin(wxSashEdgePosition edge) const;

    /**
       Sets the default sash border size
    */
    void SetDefaultBorderSize(int width);

    /**
       Gets the default sash border size
    */
    int GetDefaultBorderSize() const;

    /**
       Sets the additional border size between child and sash window
    */
    void SetExtraBorderSize(int width);

    /**
       Gets the addition border size between child and sash window
    */
    int GetExtraBorderSize() const;

    /**
       Tests for x, y over sash
    */
    wxSashEdgePosition SashHitTest(int x, int y, int tolerance = 2);

    /**
       Resizes subwindows
    */
    void SizeWindows();
};



/**
    @class wxSashEvent

    A sash event is sent when the sash of a wxSashWindow has been
    dragged by the user.

    @remarks
    When a sash belonging to a sash window is dragged by the user, and then released,
    this event is sent to the window, where it may be processed by an event table
    entry in a derived class, a plug-in event handler or an ancestor class.
    Note that the wxSashWindow doesn't change the window's size itself.
    It relies on the application's event handler to do that.
    This is because the application may have to handle other consequences of the resize,
    or it may wish to veto it altogether. The event handler should look at the drag
    rectangle: see wxSashEvent::GetDragRect to see what the new size of the window
    would be if the resize were to be applied.
    It should also call wxSashEvent::GetDragStatus to see whether the drag was
    OK or out of the current allowed range.

    @beginEventTable{wxSashEvent}
    @event{EVT_SASH_DRAGGED(id, func)}
        Process a @c wxEVT_SASH_DRAGGED event, when the user has finished dragging a sash.
    @event{EVT_SASH_DRAGGED_RANGE(id1, id2, func)}
        Process a @c wxEVT_SASH_DRAGGED_RANGE event, when the user has finished
        dragging a sash. The event handler is called when windows with ids in
        the given range have their sashes dragged.
    @endEventTable

    @library{wxcore}
    @category{events}

    @see wxSashWindow, @ref overview_events
*/
class wxSashEvent : public wxCommandEvent
{
public:
    /**
        Constructor.
    */
    wxSashEvent(int id = 0, wxSashEdgePosition edge = wxSASH_NONE);

    /**
        Returns the rectangle representing the new size the window would be if the
        resize was applied. It is up to the application to set the window size if required.
    */
    wxRect GetDragRect() const;

    /**
        Returns the status of the sash: one of wxSASH_STATUS_OK, wxSASH_STATUS_OUT_OF_RANGE.

        If the drag caused the notional bounding box of the window to flip over, for
        example, the drag will be out of rage.
    */
    wxSashDragStatus GetDragStatus() const;

    /**
        Returns the dragged edge.

        The return value is one of wxSASH_TOP, wxSASH_RIGHT, wxSASH_BOTTOM, wxSASH_LEFT.
    */
    wxSashEdgePosition GetEdge() const;


    void SetEdge(wxSashEdgePosition edge);
    void SetDragRect(const wxRect& rect);
    void SetDragStatus(wxSashDragStatus status);
};

wxEventType wxEVT_SASH_DRAGGED;
