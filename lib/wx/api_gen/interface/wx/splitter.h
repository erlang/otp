/////////////////////////////////////////////////////////////////////////////
// Name:        splitter.h
// Purpose:     interface of wxSplitterWindow
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


#define wxSP_NOBORDER         0x0000
#define wxSP_THIN_SASH        0x0000    // NB: the default is 3D sash
#define wxSP_NOSASH           0x0010
#define wxSP_PERMIT_UNSPLIT   0x0040
#define wxSP_LIVE_UPDATE      0x0080
#define wxSP_3DSASH           0x0100
#define wxSP_3DBORDER         0x0200
#define wxSP_NO_XP_THEME      0x0400
#define wxSP_BORDER           wxSP_3DBORDER
#define wxSP_3D               (wxSP_3DBORDER | wxSP_3DSASH)


enum wxSplitMode
{
    wxSPLIT_HORIZONTAL = 1,
    wxSPLIT_VERTICAL
};

enum
{
    wxSPLIT_DRAG_NONE,
    wxSPLIT_DRAG_DRAGGING,
    wxSPLIT_DRAG_LEFT_DOWN
};

/**
    @class wxSplitterWindow

    This class manages up to two subwindows. The current view can be split
    into two programmatically (perhaps from a menu command), and unsplit
    either programmatically or via the wxSplitterWindow user interface.

    @beginStyleTable
    @style{wxSP_3D}
           Draws a 3D effect border and sash.
    @style{wxSP_THIN_SASH}
           Draws a thin sash.
    @style{wxSP_3DSASH}
           Draws a 3D effect sash (part of default style).
    @style{wxSP_3DBORDER}
           Synonym for wxSP_BORDER.
    @style{wxSP_BORDER}
           Draws a standard border.
    @style{wxSP_NOBORDER}
           No border (default).
    @style{wxSP_NO_XP_THEME}
           Under Windows, switches off the attempt to draw the splitter
           using Windows theming, so the borders and sash will take on the
           pre-XP look.
    @style{wxSP_PERMIT_UNSPLIT}
           Always allow to unsplit, even with the minimum pane size other than zero.
    @style{wxSP_LIVE_UPDATE}
           Don't draw XOR line but resize the child windows immediately.
    @endStyleTable


    @beginEventEmissionTable{wxSplitterEvent}
    @event{EVT_SPLITTER_SASH_POS_CHANGING(id, func)}
        The sash position is in the process of being changed.
        May be used to modify the position of the tracking bar to properly
        reflect the position that would be set if the drag were to be completed
        at this point. Processes a @c wxEVT_SPLITTER_SASH_POS_CHANGING event.
    @event{EVT_SPLITTER_SASH_POS_CHANGED(id, func)}
        The sash position was changed. May be used to modify the sash position
        before it is set, or to prevent the change from taking place.
        Processes a @c wxEVT_SPLITTER_SASH_POS_CHANGED event.
    @event{EVT_SPLITTER_UNSPLIT(id, func)}
        The splitter has been just unsplit. Processes a @c wxEVT_SPLITTER_UNSPLIT event.
    @event{EVT_SPLITTER_DCLICK(id, func)}
        The sash was double clicked. The default behaviour is to unsplit the
        window when this happens (unless the minimum pane size has been set
        to a value greater than zero). Processes a @c wxEVT_SPLITTER_DOUBLECLICKED event.
    @endEventTable


    @library{wxcore}
    @category{miscwnd}

    @see wxSplitterEvent, @ref overview_splitterwindow
*/
class wxSplitterWindow : public wxWindow
{
public:
    /**
      Default constructor
    */
    wxSplitterWindow();

    /**
        Constructor for creating the window.

        @param parent
            The parent of the splitter window.
        @param id
            The window identifier.
        @param pos
            The window position.
        @param size
            The window size.
        @param style
            The window style. See wxSplitterWindow.
        @param name
            The window name.

        @remarks
            After using this constructor, you must create either one or two
            subwindows with the splitter window as parent, and then call one
            of Initialize(), SplitVertically() and SplitHorizontally() in order
            to set the pane(s).
            You can create two windows, with one hidden when not being shown;
            or you can create and delete the second pane on demand.

        @see Initialize(), SplitVertically(), SplitHorizontally(), Create()
    */
    wxSplitterWindow(wxWindow* parent, wxWindowID id = wxID_ANY,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxSP_3D,
                     const wxString& name = "splitterWindow");

    /**
        Destroys the wxSplitterWindow and its children.
    */
    virtual ~wxSplitterWindow();

    /**
        Creation function, for two-step construction.
        See wxSplitterWindow() for details.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& point = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = wxSP_3D,
                const wxString& name = "splitter");

    /**
        Returns the current minimum pane size (defaults to zero).

        @see SetMinimumPaneSize()
    */
    int GetMinimumPaneSize() const;

    /**
        Returns the current sash gravity.

        @see SetSashGravity()
    */
    double GetSashGravity() const;

    /**
        Returns the current sash position.

        @see SetSashPosition()
    */
    int GetSashPosition() const;

    /**
        Returns the default sash size in pixels or 0 if it is invisible.

        @see GetDefaultSashSize(), IsSashInvisible()
     */
    int GetSashSize() const;

    /**
        Returns the default sash size in pixels.

        The size of the sash is its width for a vertically split window and its
        height for a horizontally split one. Its other direction is the same as
        the client size of the window in the corresponding direction.

        The default sash size is platform-dependent because it conforms to the
        current platform look-and-feel and cannot be changed.

        @since 2.9.4
     */
    int GetDefaultSashSize() const;

    /**
        Gets the split mode.

        @see SetSplitMode(), SplitVertically(), SplitHorizontally().
    */
    wxSplitMode GetSplitMode() const;

    /**
        Returns the left/top or only pane.
    */
    wxWindow* GetWindow1() const;

    /**
        Returns the right/bottom pane.
    */
    wxWindow* GetWindow2() const;

    /**
        Initializes the splitter window to have one pane.
        The child window is shown if it is currently hidden.

        @param window
            The pane for the unsplit window.

        @remarks This should be called if you wish to initially view only a
                 single pane in the splitter window.

        @see SplitVertically(), SplitHorizontally()
    */
    void Initialize(wxWindow* window);

    /**
        Returns @true if the sash is invisible even when the window is split, @false otherwise.

        @remark This is a shortcut for HasFlag(wxSP_NOSASH)

        @see SetSashInvisible()

        @since 2.9.4
    */
    bool IsSashInvisible() const;

    /**
        Returns @true if the window is split, @false otherwise.
    */
    bool IsSplit() const;

    /**
        Application-overridable function called when the sash is double-clicked with
        the left mouse button.

        @param x
            The x position of the mouse cursor.
        @param y
            The y position of the mouse cursor.

        @remarks The default implementation of this function calls Unsplit if the
                 minimum pane size is zero.

        @see Unsplit()
    */
    virtual void OnDoubleClickSash(int x, int y);

    /**
        Application-overridable function called when the sash position is changed by
        user. It may return @false to prevent the change or @true to allow it.

        @param newSashPosition
            The new sash position (always positive or zero)

        @remarks The default implementation of this function verifies that the
                 sizes of both  panes of the splitter are greater than
                 minimum pane size.
    */
    virtual bool OnSashPositionChange(int newSashPosition);

    /**
        Application-overridable function called when the window is unsplit, either
        programmatically or using the wxSplitterWindow user interface.

        @param removed
            The window being removed.

        @remarks The default implementation of this function simply hides
                 removed. You may wish to delete the window.
    */
    virtual void OnUnsplit(wxWindow* removed);

    /**
        This function replaces one of the windows managed by the wxSplitterWindow with
        another one. It is in general better to use it instead of calling Unsplit()
        and then resplitting the window back because it will provoke much less flicker
        (if any). It is valid to call this function whether the splitter has two
        windows or only one.

        Both parameters should be non-@NULL and @a winOld must specify one of the
        windows managed by the splitter. If the parameters are incorrect or the window
        couldn't be replaced, @false is returned. Otherwise the function will return
        @true, but please notice that it will not delete the replaced window and you
        may wish to do it yourself.

        @see GetMinimumPaneSize()
    */
    bool ReplaceWindow(wxWindow* winOld, wxWindow* winNew);

    /**
        Sets the minimum pane size.

        @param paneSize
            Minimum pane size in pixels.

        @remarks The default minimum pane size is zero, which means that either
                 pane can be reduced to zero by dragging the sash, thus
                 removing one of the panes. To prevent this behaviour
                 (and veto out-of-range sash dragging), set a minimum
                 size, for example 20 pixels. If the wxSP_PERMIT_UNSPLIT
                 style is used when a splitter window is created, the
                 window may be unsplit even if minimum size is non-zero.

        @see GetMinimumPaneSize()
    */
    void SetMinimumPaneSize(int paneSize);

    /**
        Sets the sash gravity.

        @param gravity
            The sash gravity. Value between 0.0 and 1.0.

        @remarks
        Gravity is real factor which controls position of sash while resizing
        wxSplitterWindow. Gravity tells wxSplitterWindow how much will left/top
        window grow while resizing.
        Example values:
        - 0.0: only the bottom/right window is automatically resized
        - 0.5: both windows grow by equal size
        - 1.0: only left/top window grows
        Gravity should be a real value between 0.0 and 1.0.
        Default value of sash gravity is 0.0.
        That value is compatible with previous (before gravity was introduced)
        behaviour of wxSplitterWindow.

        Notice that when sash gravity for a newly created splitter window, it
        is often necessary to explicitly set the splitter size using SetSize()
        to ensure that is big enough for its initial sash position. Otherwise,
        i.e. if the window is created with the default tiny size and only
        resized to its correct size later, the initial sash position will be
        affected by the gravity and typically result in sash being at the
        rightmost position for the gravity of 1. See the example code creating
        wxSplitterWindow in the splitter sample for more details.

        @see GetSashGravity()
    */
    void SetSashGravity(double gravity);

    /**
        Sets the sash position.

        @param position
            The sash position in pixels.
        @param redraw
            If @true, resizes the panes and redraws the sash and border.

        @remarks Does not currently check for an out-of-range value.

        @see GetSashPosition()
    */
    void SetSashPosition(int position, bool redraw = true);

    /**
        Sets the split mode.

        @param mode
            Can be wxSPLIT_VERTICAL or wxSPLIT_HORIZONTAL.

        @remarks Only sets the internal variable; does not update the display.

        @see GetSplitMode(), SplitVertically(), SplitHorizontally().
    */
    void SetSplitMode(int mode);

    /**
        Sets whether the sash should be invisible, even when the window is
        split.

        When the sash is invisible, it doesn't appear on the screen at all and,
        in particular, doesn't allow the user to resize the windows.

        @remarks Only sets the internal variable; does not update the display.

        @param invisible
            If @true, the sash is always invisible, else it is shown when the
            window is split.

        @see IsSashInvisible()

        @since 2.9.4
    */
    void SetSashInvisible(bool invisible=true);

    /**
        Initializes the top and bottom panes of the splitter window.
        The child windows are shown if they are currently hidden.

        @param window1
            The top pane.
        @param window2
            The bottom pane.
        @param sashPosition
            The initial position of the sash. If this value is positive,
            it specifies the size of the upper pane. If it is negative, its
            absolute value gives the size of the lower pane.
            Finally, specify 0 (default) to choose the default position
            (half of the total window height).

        @return @true if successful, @false otherwise (the window was already split).

        @remarks This should be called if you wish to initially view two panes.
                 It can also be called at any subsequent time, but the application
                 should check that the window is not currently split using IsSplit().

        @see SplitVertically(), IsSplit(), Unsplit()
    */
    virtual bool SplitHorizontally(wxWindow* window1, wxWindow* window2,
                                   int sashPosition = 0);

    /**
        Initializes the left and right panes of the splitter window.
        The child windows are shown if they are currently hidden.

        @param window1
            The left pane.
        @param window2
            The right pane.
        @param sashPosition
            The initial position of the sash. If this value is positive, it
            specifies the size of the left pane. If it is negative, it is
            absolute value gives the size of the right pane.
            Finally, specify 0 (default) to choose the default position
            (half of the total window width).

        @return @true if successful, @false otherwise (the window was already split).

        @remarks This should be called if you wish to initially view two panes.
                 It can also be called at any subsequent time, but the
                 application should check that the window is not currently
                 split using IsSplit().

        @see SplitHorizontally(), IsSplit(), Unsplit().
    */
    virtual bool SplitVertically(wxWindow* window1, wxWindow* window2,
                                 int sashPosition = 0);

    /**
        Unsplits the window.

        @param toRemove
            The pane to remove, or @NULL to remove the right or bottom pane.

        @return @true if successful, @false otherwise (the window was not split).

        @remarks This call will not actually delete the pane being removed; it
                 calls OnUnsplit() which can be overridden for the desired
                 behaviour. By default, the pane being removed is hidden.

        @see SplitHorizontally(), SplitVertically(), IsSplit(), OnUnsplit()
    */
    bool Unsplit(wxWindow* toRemove = NULL);

    /**
        Causes any pending sizing of the sash and child panes to take place
        immediately.

        Such resizing normally takes place in idle time, in order to wait for
        layout to be completed. However, this can cause unacceptable flicker as
        the panes are resized after the window has been shown.
        To work around this, you can perform window layout (for example by
        sending a size event to the parent window), and then call this function,
        before showing the top-level window.
    */
    void UpdateSize();
};



/**
    @class wxSplitterEvent

    This class represents the events generated by a splitter control.

    Also there is only one event class, the data associated to the different events
    is not the same and so not all accessor functions may be called for each event.
    The documentation mentions the kind of event(s) for which the given accessor
    function makes sense: calling it for other types of events will result
    in assert failure (in debug mode) and will return meaningless results.

    @beginEventTable{wxSplitterEvent}
    @event{EVT_SPLITTER_SASH_POS_CHANGING(id, func)}
        The sash position is in the process of being changed.
        May be used to modify the position of the tracking bar to properly
        reflect the position that would be set if the drag were to be completed
        at this point. Processes a @c wxEVT_SPLITTER_SASH_POS_CHANGING event.
    @event{EVT_SPLITTER_SASH_POS_CHANGED(id, func)}
        The sash position was changed. May be used to modify the sash position
        before it is set, or to prevent the change from taking place.
        Processes a @c wxEVT_SPLITTER_SASH_POS_CHANGED event.
    @event{EVT_SPLITTER_UNSPLIT(id, func)}
        The splitter has been just unsplit. Processes a @c wxEVT_SPLITTER_UNSPLIT event.
    @event{EVT_SPLITTER_DCLICK(id, func)}
        The sash was double clicked. The default behaviour is to unsplit the
        window when this happens (unless the minimum pane size has been set
        to a value greater than zero). Processes a @c wxEVT_SPLITTER_DOUBLECLICKED event.
    @endEventTable

    @library{wxcore}
    @category{events}

    @see wxSplitterWindow, @ref overview_events
*/
class wxSplitterEvent : public wxNotifyEvent
{
public:
    /**
        Constructor. Used internally by wxWidgets only.
    */
    wxSplitterEvent(wxEventType eventType = wxEVT_NULL,
                    wxSplitterWindow* splitter = NULL);

    /**
        Returns the new sash position.

        May only be called while processing
        @c wxEVT_SPLITTER_SASH_POS_CHANGING and
        @c wxEVT_SPLITTER_SASH_POS_CHANGED events.
    */
    int GetSashPosition() const;

    /**
        Returns a pointer to the window being removed when a splitter window
        is unsplit.

        May only be called while processing
        @c wxEVT_SPLITTER_UNSPLIT events.
    */
    wxWindow* GetWindowBeingRemoved() const;

    /**
        Returns the x coordinate of the double-click point.

        May only be called while processing
        @c wxEVT_SPLITTER_DOUBLECLICKED events.
    */
    int GetX() const;

    /**
        Returns the y coordinate of the double-click point.

        May only be called while processing
        @c wxEVT_SPLITTER_DOUBLECLICKED events.
    */
    int GetY() const;

    /**
        In the case of @c wxEVT_SPLITTER_SASH_POS_CHANGED events,
        sets the new sash position.
        In the case of @c wxEVT_SPLITTER_SASH_POS_CHANGING events,
        sets the new tracking bar position so visual feedback during dragging will
        represent that change that will actually take place. Set to -1 from
        the event handler code to prevent repositioning.

        May only be called while processing
        @c wxEVT_SPLITTER_SASH_POS_CHANGING and
        @c wxEVT_SPLITTER_SASH_POS_CHANGED events.

        @param pos
            New sash position.
    */
    void SetSashPosition(int pos);
};


wxEventType wxEVT_SPLITTER_SASH_POS_CHANGED;
wxEventType wxEVT_SPLITTER_SASH_POS_CHANGING;
wxEventType wxEVT_SPLITTER_DOUBLECLICKED;
wxEventType wxEVT_SPLITTER_UNSPLIT;
