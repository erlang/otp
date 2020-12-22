/////////////////////////////////////////////////////////////////////////////
// Name:        tipwin.h
// Purpose:     interface of wxTipWindow
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxTipWindow

    Shows simple text in a popup tip window on creation.
    This is used by wxSimpleHelpProvider to show popup help.
    The window automatically destroys itself when the user clicks on it or it
    loses the focus.

    You may also use this class to emulate the tooltips when you need finer
    control over them than what the standard tooltips provide.

    @library{wxcore}
    @category{managedwnd}

    @see @ref wxToolTip
*/
class wxTipWindow : public wxWindow
{
public:
    /**
        Constructor. The tip is shown immediately after the window is constructed.

        @param parent
            The parent window, must be non-@NULL
        @param text
            The text to show, may contain the new line characters
        @param maxLength
            The length of each line, in pixels. Set to a very large
            value to avoid wrapping lines
        @param windowPtr
            Simply passed to SetTipWindowPtr() below, please see its
            documentation for the description of this parameter
        @param rectBounds
            If non-@NULL, passed to SetBoundingRect() below, please see its
            documentation for the description of this parameter
    */
    wxTipWindow(wxWindow* parent, const wxString& text,
                wxCoord maxLength = 100,
                wxTipWindow** windowPtr = NULL,
                wxRect* rectBounds = NULL);

    /**
        By default, the tip window disappears when the user clicks the mouse or presses
        a keyboard key or if it loses focus in any other way - for example because the
        user switched to another application window.

        Additionally, if a non-empty @a rectBound is provided, the tip window will
        also automatically close if the mouse leaves this area. This is useful to
        dismiss the tip mouse when the mouse leaves the object it is associated with.

        @param rectBound
            The bounding rectangle for the mouse in the screen coordinates
    */
    void SetBoundingRect(const wxRect& rectBound);

    /**
        When the tip window closes itself (which may happen at any moment and
        unexpectedly to the caller) it may @NULL out the pointer pointed to by
        @a windowPtr. This is helpful to avoid dereferencing the tip window which
        had been already closed and deleted.
    */
    void SetTipWindowPtr(wxTipWindow** windowPtr);
};

