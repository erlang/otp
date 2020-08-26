///////////////////////////////////////////////////////////////////////////////
// Name:        ribbon/page.h
// Purpose:     interface of wxRibbonPage
// Author:      Peter Cawley
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    @class wxRibbonPage

    Container for related ribbon panels, and a tab within a ribbon bar.

    @see wxRibbonBar
    @see wxRibbonPanel

    @library{wxribbon}
    @category{ribbon}
*/
class wxRibbonPage : public wxRibbonControl
{
public:
    /**
        Default constructor.
        With this constructor, Create() should be called in order to create
        the ribbon page.
    */
    wxRibbonPage();

    /**
        Constructs a ribbon page, which must be a child of a ribbon bar.

        @param parent
            Pointer to a parent wxRibbonBar (unlike most controls, a wxRibbonPage
            can only have wxRibbonBar as a parent).
        @param id
            Window identifier.
        @param label
            Label to be used in the wxRibbonBar's tab list for this page (if the
            ribbon bar is set to display labels).
        @param icon
            Icon to be used in the wxRibbonBar's tab list for this page (if the
            ribbon bar is set to display icons).
        @param style
            Currently unused, should be zero.
    */
    wxRibbonPage(wxRibbonBar* parent,
                wxWindowID id = wxID_ANY,
                const wxString& label = wxEmptyString,
                const wxBitmap& icon = wxNullBitmap,
                long style = 0);

    /**
        Destructor.
    */
    virtual ~wxRibbonPage();

    /**
        Create a ribbon page in two-step ribbon page construction.
        Should only be called when the default constructor is used, and
        arguments have the same meaning as in the full constructor.
    */
    bool Create(wxRibbonBar* parent,
                wxWindowID id = wxID_ANY,
                const wxString& label = wxEmptyString,
                const wxBitmap& icon = wxNullBitmap,
                long style = 0);

    /**
        Set the art provider to be used. Normally called automatically by
        wxRibbonBar when the page is created, or the art provider changed on the
        bar.

        The new art provider will be propagated to the children of the page.
    */
    void SetArtProvider(wxRibbonArtProvider* art);

    /**
        Get the icon used for the page in the ribbon bar tab area (only
        displayed if the ribbon bar is actually showing icons).
    */
    wxBitmap& GetIcon();

    /**
        Set the size of the page and the external scroll buttons (if any).

        When a page is too small to display all of its children, scroll buttons
        will appear (and if the page is sized up enough, they will disappear again).
        Slightly counter-intuitively, these buttons are created as siblings of the
        page rather than children of the page (to achieve correct cropping and
        paint ordering of the children and the buttons). When there are no scroll
        buttons, this function behaves the same as SetSize(), however when there
        are scroll buttons, it positions them at the edges of the given area, and
        then calls SetSize() with the remaining area.

        This is provided as a separate function to SetSize() rather than within
        the implementation of SetSize(), as interacting algorithms may not expect
        SetSize() to also set the size of siblings.
    */
    void SetSizeWithScrollButtonAdjustment(int x, int y, int width, int height);

    /**
        Expand a rectangle of the page to include external scroll buttons (if
        any). When no scroll buttons are shown, has no effect.

        @param[in,out] rect
            The rectangle to adjust. The width and height will not be reduced,
            and the x and y will not be increased.
    */
    void AdjustRectToIncludeScrollButtons(wxRect* rect) const;

    /**
        Dismiss the current externally expanded panel, if there is one.

        When a ribbon panel automatically minimises, it can be externally
        expanded into a floating window. When the user clicks a button in such
        a panel, the panel should generally re-minimise. Event handlers for
        buttons on ribbon panels should call this method to achieve this
        behaviour.

        @return @true if a panel was minimised, @false otherwise.
    */
    bool DismissExpandedPanel();

    /**
        Perform a full re-layout of all panels on the page.

        Should be called after panels are added to the page, or the sizing
        behaviour of a panel on the page changes (i.e. due to children being
        added to it). Usually called automatically when wxRibbonBar::Realize()
        is called.

        Will invoke wxRibbonPanel::Realize() for all child panels.
    */
    virtual bool Realize();

    /**
        Scroll the page by some amount up / down / left / right.

        When the page's children are too big to fit in the onscreen area given to
        the page, scroll buttons will appear, and the page can be programmatically
        scrolled. Positive values of @a lines will scroll right or down, while
        negative values will scroll up or left (depending on the direction in which
        panels are stacked). A line is equivalent to a constant number of pixels.

        @return @true if the page scrolled at least one pixel in the given
            direction, @false if it did not scroll.

        @see GetMajorAxis()
        @see ScrollPixels()
        @see ScrollSections()
    */
    virtual bool ScrollLines(int lines);

    /**
        Scroll the page by a set number of pixels up / down / left / right.

        When the page's children are too big to fit in the onscreen area given to
        the page, scroll buttons will appear, and the page can be programmatically
        scrolled. Positive values of @a lines will scroll right or down, while
        negative values will scroll up or left (depending on the direction in which
        panels are stacked).

        @return @true if the page scrolled at least one pixel in the given
            direction, @false if it did not scroll.

        @see GetMajorAxis()
        @see ScrollLines()
        @see ScrollSections()
    */
    bool ScrollPixels(int pixels);

    /**
        Scroll the page by an entire child section.

        The @a sections parameter value should be 1 or -1. This will scroll
        enough to uncover a partially visible child section or totally uncover
        the next child section that may not be visible at all.

        @return @true if the page scrolled at least one pixel in the given
            direction, @false if it did not scroll.

        @see ScrollPixels()
        @see ScrollSections()

        @since 2.9.5
    */
    bool ScrollSections(int sections);

    /**
        Get the direction in which ribbon panels are stacked within the page.

        This is controlled by the style of the containing wxRibbonBar, meaning
        that all pages within a bar will have the same major axis. As well as
        being the direction in which panels are stacked, it is also the axis in
        which scrolling will occur (when required).

        @return wxHORIZONTAL or wxVERTICAL (never wxBOTH).
    */
    wxOrientation GetMajorAxis() const;
};
