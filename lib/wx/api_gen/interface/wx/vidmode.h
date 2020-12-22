/////////////////////////////////////////////////////////////////////////////
// Name:        vidmode.h
// Purpose:     interface of wxVideoMode
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @struct wxVideoMode

    Determines the sizes and locations of displays connected to the system.

    @library{wxcore}
    @category{cfg}

    @stdobjects
    ::wxDefaultVideoMode
*/
struct wxVideoMode
{
public:
    /**
        Constructs this class using the given parameters.
    */
    wxVideoMode(int width = 0, int height = 0, int depth = 0, int freq = 0);

    /**
        Returns true if this mode matches the other one in the sense that all
        non zero fields of the other mode have the same value in this one
        (except for refresh which is allowed to have a greater value).
    */
    bool Matches(const wxVideoMode& other) const;

    /**
        Returns the screen width in pixels (e.g.\ 640), 0 means unspecified.
    */
    int GetWidth() const;

    /**
        Returns the screen height in pixels (e.g.\ 480), 0 means unspecified.
    */
    int GetHeight() const;

    /**
        Returns bits per pixel (e.g.\ 32), 1 is monochrome and 0 means
        unspecified/known.
    */
    int GetDepth() const;

    /**
        Returns true if the object has been initialized
    */
    bool IsOk() const;


    bool operator==(const wxVideoMode& m) const;
    bool operator!=(const wxVideoMode& mode) const;



    /**
        The screen width in pixels (e.g.\ 640), 0 means unspecified.
    */
    int w;

    /**
        The screen height in pixels (e.g.\ 480), 0 means unspecified.
    */
    int h;

    /**
        Bits per pixel (e.g.\ 32), 1 is monochrome and 0 means
        unspecified/known.
    */
    int bpp;

    /**
        Refresh frequency in Hz, 0 means unspecified/unknown.
    */
    int refresh;
};

/**
    A global wxVideoMode instance used by wxDisplay.
*/
const wxVideoMode wxDefaultVideoMode;

