///////////////////////////////////////////////////////////////////////////////
// Name:        wx/mousemanager.h
// Purpose:     documentation of wxMouseEventsManager class
// Author:      Vadim Zeitlin
// Created:     2009-04-20
// Copyright:   (c) 2009 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    @class wxMouseEventsManager

    Helper for handling mouse input events in windows containing multiple
    items.

    This class handles mouse events and synthesizes high-level notifications
    such as clicks and drag events from low level mouse button presses and
    mouse movement events. It is useful because handling the mouse events is
    less obvious than might seem at a first glance: for example, clicks on an
    object should only be generated if the mouse was both pressed and released
    over it and not just released (so it requires storing the previous state)
    and dragging shouldn't start before the mouse moves away far enough.

    This class encapsulates all these dull details for controls containing
    multiple items which can be identified by a positive integer index and you
    just need to implement its pure virtual functions to use it.

    Notice that this class supposes that all items can be identified by an
    integer "index" but it doesn't need to be an ordinal index of the item
    (although this is the most common case) -- it can be any value which can
    be used to uniquely identify an item.

    @library{wxcore}
    @category{events}
 */
class wxMouseEventsManager : public wxEvtHandler
{
public:
    /**
        Default constructor.

        You must call Create() to finish initializing the mouse events manager.
        If possible, avoid the use of this constructor in favour of the other
        one which fully initializes the mouse events manager immediately.
     */
    wxMouseEventsManager();

    /**
        Constructor creates the manager for the window.

        A mouse event manager is always associated with a window and must be
        destroyed by the window when it is destroyed (it doesn't need to be
        allocated on the heap however).
     */
    wxMouseEventsManager(wxWindow *win);

    /**
        Finishes initialization of the object created using default
        constructor.

        Currently always returns @true.
     */
    bool Create(wxWindow *win);

protected:
    /**
        Must be overridden to return the item at the given position.

        @param pos
            The position to test, in physical coordinates.
        @return
            The index of the item at the given position or wxNOT_FOUND if there
            is no item there.
     */
    virtual int MouseHitTest(const wxPoint& pos) = 0;

    /**
        Must be overridden to react to mouse clicks.

        This method is called when the user clicked (i.e. pressed and released
        mouse over the @e same item) and should normally generate a
        notification about this click and return true if it was handled or
        false otherwise, determining whether the original mouse event is
        skipped or not.

        @param item
            The item which was clicked.
        @return
            @true if the mouse event was processed and @false otherwise.
     */
    virtual bool MouseClicked(int item) = 0;

    /**
        Must be overridden to allow or deny dragging of the item.

        This method is called when the user attempts to start dragging the
        given item.

        @param item
            The item which is going to be dragged.
        @param pos
            The position from where it is being dragged.
        @return
            @true to allow the item to be dragged (in which case
            MouseDragging() and MouseDragEnd() will be called later, unless
            MouseDragCancelled() is called instead) or @false to forbid it.
     */
    virtual bool MouseDragBegin(int item, const wxPoint& pos) = 0;

    /**
        Must be overridden to provide feed back while an item is being dragged.

        This method is called while the item is being dragged and should
        normally update the feedback shown on screen (usually this is done
        using wxOverlay).

        Notice that this method will never be called for the items for which
        MouseDragBegin() returns @false. Consequently, if MouseDragBegin()
        always returns @false you can do nothing in this method.

        @param item
            The item being dragged.
        @param pos
            The current position of the item.

        @see MouseDragEnd()
     */
    virtual void MouseDragging(int item, const wxPoint& pos) = 0;

    /**
        Must be overridden to handle item drop.

        This method is called when the mouse is released after dragging the
        item. Normally the item should be positioned at the new location.

        @param item
            The item which was dragged and now dropped.
        @param pos
            The position at which the item was dropped.

        @see MouseDragBegin(), MouseDragging()
     */
    virtual void MouseDragEnd(int item, const wxPoint& pos) = 0;

    /**
        Must be overridden to handle cancellation of mouse dragging.

        This method is called when mouse capture is lost while dragging the
        item and normally should remove the visual feedback drawn by
        MouseDragging() as well as reset any internal variables set in
        MouseDragBegin().

        @see wxMouseCaptureLostEvent
     */
    virtual void MouseDragCancelled(int item) = 0;


    /**
        May be overridden to update the state of an item when it is pressed.

        This method is called when the item is becomes pressed and can be used
        to change its appearance when this happens. It is mostly useful for
        button-like items and doesn't need to be overridden if the items
        shouldn't change their appearance when pressed.

        @param item
            The item being pressed.
     */
    virtual void MouseClickBegin(int item);

    /**
        Must be overridden to reset the item appearance changed by
        MouseClickBegin().

        This method is called if the mouse capture was lost while the item was
        pressed and must be overridden to restore the default item appearance
        if it was changed in MouseClickBegin().

        @see MouseDragCancelled(), wxMouseCaptureLostEvent
     */
    virtual void MouseClickCancelled(int item);
};
