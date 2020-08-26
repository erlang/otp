/////////////////////////////////////////////////////////////////////////////
// Name:        dcscreen.h
// Purpose:     interface of wxScreenDC
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxScreenDC

    A wxScreenDC can be used to paint on the screen. This should normally be
    constructed as a temporary stack object; don't store a wxScreenDC object.

    When using multiple monitors, wxScreenDC corresponds to the entire virtual
    screen composed of all of them. Notice that coordinates on wxScreenDC can
    be negative in this case, see wxDisplay::GetGeometry() for more.

    @library{wxcore}
    @category{dc}

    @see wxDC, wxMemoryDC, wxPaintDC, wxClientDC, wxWindowDC
*/
class wxScreenDC : public wxDC
{
public:
    /**
        Constructor.
    */
    wxScreenDC();

    /**
        Use this in conjunction with StartDrawingOnTop().

        This function destroys the temporary window created to implement on-top
        drawing (X only).
    */
    static bool EndDrawingOnTop();

    /**
        Use this in conjunction with EndDrawingOnTop() to ensure that drawing
        to the screen occurs on top of existing windows. Without this, some
        window systems (such as X) only allow drawing to take place underneath
        other windows.

        This version of StartDrawingOnTop() is used to specify that the area
        that will be drawn on coincides with the given window. It is
        recommended that an area of the screen is specified with
        StartDrawingOnTop(wxRect*) because with large regions, flickering
        effects are noticeable when destroying the temporary transparent window
        used to implement this feature.

        You might use this function when implementing a drag feature, for
        example as in the wxSplitterWindow implementation.

        @remarks This function is probably obsolete since the X implementations
                 allow drawing directly on the screen now. However, the fact
                 that this function allows the screen to be refreshed
                 afterwards, may be useful to some applications.
    */
    static bool StartDrawingOnTop(wxWindow* window);
    /**
        Use this in conjunction with EndDrawingOnTop() to ensure that drawing
        to the screen occurs on top of existing windows. Without this, some
        window systems (such as X) only allow drawing to take place underneath
        other windows.

        This version of StartDrawingOnTop() is used to specify an area of the
        screen which is to be drawn on. If @NULL is passed, the whole screen is
        available. It is recommended that an area of the screen is specified
        with this function rather than with StartDrawingOnTop(wxWindow*),
        because with large regions, flickering effects are noticeable when
        destroying the temporary transparent window used to implement this
        feature.

        You might use this function when implementing a drag feature, for
        example as in the wxSplitterWindow implementation.

        @remarks This function is probably obsolete since the X implementations
                 allow drawing directly on the screen now. However, the fact
                 that this function allows the screen to be refreshed
                 afterwards, may be useful to some applications.
    */
    static bool StartDrawingOnTop(wxRect* rect = NULL);
};

