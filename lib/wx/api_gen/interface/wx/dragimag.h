/////////////////////////////////////////////////////////////////////////////
// Name:        dragimag.h
// Purpose:     interface of wxDragImage
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxDragImage

    This class is used when you wish to drag an object on the screen, and a
    simple cursor is not enough.

    On Windows, the Win32 API is used to achieve smooth dragging. On other
    platforms, wxGenericDragImage is used. Applications may also prefer to use
    wxGenericDragImage on Windows, too.

    To use this class, when you wish to start dragging an image, create a
    wxDragImage object and store it somewhere you can access it as the drag
    progresses. Call BeginDrag() to start, and EndDrag() to stop the drag. To
    move the image, initially call Show() and then Move(). If you wish to
    update the screen contents during the drag (for example, highlight an item
    as in the dragimag sample), first call Hide(), update the screen, call
    Move(), and then call Show().

    You can drag within one window, or you can use full-screen dragging either
    across the whole screen, or just restricted to one area of the screen to
    save resources. If you want the user to drag between two windows, then you
    will need to use full-screen dragging.

    If you wish to draw the image yourself, use wxGenericDragImage and override
    DoDrawImage() and GetImageRect().

    @library{wxcore}
    @category{dnd}

    @see @ref page_samples_dragimag
*/
class wxDragImage : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxDragImage();
    /**
        Constructs a drag image from a bitmap and optional cursor.

        @param image
            Bitmap to be used as the drag image. The bitmap can have a mask.
        @param cursor
            Optional cursor to combine with the image.
    */
    wxDragImage(const wxBitmap& image, const wxCursor& cursor = wxNullCursor);
    /**
        Constructs a drag image from an icon and optional cursor.

        @param image
            Icon to be used as the drag image.
        @param cursor
            Optional cursor to combine with the image.
    */
    wxDragImage(const wxIcon& image, const wxCursor& cursor = wxNullCursor);
    /**
        Constructs a drag image from a text string and optional cursor.

        @param text
            Text used to construct a drag image.
        @param cursor
            Optional cursor to combine with the image.
    */
    wxDragImage(const wxString& text, const wxCursor& cursor = wxNullCursor);
    /**
        Constructs a drag image from the text in the given tree control item,
        and optional cursor.

        @param treeCtrl
            Tree control for constructing a tree drag image.
        @param id
            Tree control item id.
    */
    wxDragImage(const wxTreeCtrl& treeCtrl, wxTreeItemId& id);
    /**
        Constructs a drag image from the text in the given list control item,
        and optional cursor.

        @param listCtrl
            List control for constructing a list drag image.
        @param id
            List control item id.
    */
    wxDragImage(const wxListCtrl& listCtrl, long id);
    /**
        Start dragging the image, in a window or full screen.

        You need to then call Show() and Move() to show the image on the
        screen. Call EndDrag() when the drag has finished.

        Note that this call automatically calls CaptureMouse().

        @param hotspot
            The location of the drag position relative to the upper-left corner
            of the image.
        @param window
            The window that captures the mouse, and within which the dragging
            is limited unless fullScreen is @true.
        @param fullScreen
            If @true, specifies that the drag will be visible over the full
            screen, or over as much of the screen as is specified by rect. Note
            that the mouse will still be captured in window.
        @param rect
            If non-@NULL, specifies the rectangle (in screen coordinates) that
            bounds the dragging operation. Specifying this can make the
            operation more efficient by cutting down on the area under
            consideration, and it can also make a visual difference since the
            drag is clipped to this area.
    */
    bool BeginDrag(const wxPoint& hotspot, wxWindow* window,
                   bool fullScreen = false, wxRect* rect = NULL);
    /**
        Start dragging the image, using the first window to capture the mouse
        and the second to specify the bounding area. This form is equivalent to
        using the first form, but more convenient than working out the bounding
        rectangle explicitly.

        You need to then call Show() and Move() to show the image on the
        screen. Call EndDrag() when the drag has finished.

        Note that this call automatically calls CaptureMouse().

        @param hotspot
            The location of the drag position relative to the upper-left corner
            of the image.
        @param window
            The window that captures the mouse, and within which the dragging
            is limited.
        @param boundingWindow
            Specifies the area within which the drag occurs.
    */
    bool BeginDrag(const wxPoint& hotspot, wxWindow* window,
                   wxWindow* boundingWindow);

    /**
        Draws the image on the device context with top-left corner at the given
        position.

        This function is only available with wxGenericDragImage, to allow
        applications to draw their own image instead of using an actual bitmap.
        If you override this function, you must also override GetImageRect().
    */
    virtual bool DoDrawImage(wxDC& dc, const wxPoint& pos) const;

    /**
        Call this when the drag has finished.

        @note This function automatically releases mouse capture.
    */
    bool EndDrag();

    /**
        Returns the rectangle enclosing the image, assuming that the image is
        drawn with its top-left corner at the given point.

        This function is available in wxGenericDragImage only, and may be
        overridden (together with DoDrawImage()) to provide a virtual drawing
        capability.
    */
    virtual wxRect GetImageRect(const wxPoint& pos) const;

    /**
        Hides the image. You may wish to call this before updating the window
        contents (perhaps highlighting an item). Then call Move() and Show().
    */
    bool Hide();

    /**
        Call this to move the image to a new position. The image will only be
        shown if Show() has been called previously (for example at the start of
        the drag).

        @param pt
            The position in client coordinates (relative to the window
            specified in BeginDrag()).

        You can move the image either when the image is hidden or shown, but in
        general dragging will be smoother if you move the image when it is
        shown.
    */
    bool Move(const wxPoint& pt);

    /**
        Shows the image. Call this at least once when dragging.
    */
    bool Show();

    /**
        Override this if you wish to draw the window contents to the backing
        bitmap yourself. This can be desirable if you wish to avoid flicker by
        not having to redraw the updated window itself just before dragging,
        which can cause a flicker just as the drag starts. Instead, paint the
        drag image's backing bitmap to show the appropriate graphic @e minus
        the objects to be dragged, and leave the window itself to be updated by
        the drag image. This can provide eerily smooth, flicker-free drag
        behaviour.

        The default implementation copies the window contents to the backing
        bitmap. A new implementation will normally copy information from
        another source, such as from its own backing bitmap if it has one, or
        directly from internal data structures.

        This function is available in wxGenericDragImage only.
    */
    virtual bool UpdateBackingFromWindow(wxDC& windowDC, wxMemoryDC& destDC,
                                         const wxRect& sourceRect,
                                         const wxRect& destRect) const;
};

