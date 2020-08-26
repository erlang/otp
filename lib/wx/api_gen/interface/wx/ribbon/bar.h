///////////////////////////////////////////////////////////////////////////////
// Name:        ribbon/bar.h
// Purpose:     interface of wxRibbonBar
// Author:      Peter Cawley
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////


enum wxRibbonBarOption
{
    wxRIBBON_BAR_SHOW_PAGE_LABELS,
    wxRIBBON_BAR_SHOW_PAGE_ICONS,
    wxRIBBON_BAR_FLOW_HORIZONTAL,
    wxRIBBON_BAR_FLOW_VERTICAL,
    wxRIBBON_BAR_SHOW_PANEL_EXT_BUTTONS,
    wxRIBBON_BAR_SHOW_PANEL_MINIMISE_BUTTONS,
    wxRIBBON_BAR_ALWAYS_SHOW_TABS,
    wxRIBBON_BAR_SHOW_TOGGLE_BUTTON,
    wxRIBBON_BAR_SHOW_HELP_BUTTON,
    wxRIBBON_BAR_DEFAULT_STYLE,
    wxRIBBON_BAR_FOLDBAR_STYLE
};


/**
    The possible display modes of the panel area of a wxRibbonBar widget.

    @see wxRibbonBar::ShowPanels()
    @see wxRibbonBar::GetDisplayMode()

    @since 2.9.5
*/
enum wxRibbonDisplayMode
{
    /**
        The panel area is visible and pinned: it remains visible when the
        ribbon bar loses the focus.
     */
    wxRIBBON_BAR_PINNED,

    /**
        The panel area is hidden: only the pages tabs remain visible.
     */
    wxRIBBON_BAR_MINIMIZED,

    /**
        The panel area is visible, but not pinned: it minimizes as soon as the
        focus is lost.
     */
    wxRIBBON_BAR_EXPANDED
};


/**
    @class wxRibbonBarEvent

    Event used to indicate various actions relating to a wxRibbonBar.

    See wxRibbonBar for available event types.

    @library{wxribbon}
    @category{events,ribbon}

    @see wxRibbonBar
*/
class wxRibbonBarEvent : public wxNotifyEvent
{
public:
    /**
        Constructor.
    */
    wxRibbonBarEvent(wxEventType command_type = wxEVT_NULL,
                     int win_id = 0,
                     wxRibbonPage* page = NULL);

    /**
        Returns the page being changed to, or being clicked on.
    */
    wxRibbonPage* GetPage();

    /**
        Sets the page relating to this event.
    */
    void SetPage(wxRibbonPage* page);
};


wxEventType wxEVT_RIBBONBAR_PAGE_CHANGED;
wxEventType wxEVT_RIBBONBAR_PAGE_CHANGING;
wxEventType wxEVT_RIBBONBAR_TAB_MIDDLE_DOWN;
wxEventType wxEVT_RIBBONBAR_TAB_MIDDLE_UP;
wxEventType wxEVT_RIBBONBAR_TAB_RIGHT_DOWN;
wxEventType wxEVT_RIBBONBAR_TAB_RIGHT_UP;
wxEventType wxEVT_RIBBONBAR_TAB_LEFT_DCLICK;
wxEventType wxEVT_RIBBONBAR_TOGGLED;
wxEventType wxEVT_RIBBONBAR_HELP_CLICK;


class wxRibbonPageTabInfo
{
public:
    wxRect rect;
    wxRibbonPage *page;
    int ideal_width;
    int small_begin_need_separator_width;
    int small_must_have_separator_width;
    int minimum_width;
    bool active;
    bool hovered;
    bool highlight;
    bool shown;
};



/**
    @class wxRibbonBar

    Top-level control in a ribbon user interface. Serves as a tabbed container
    for wxRibbonPage - a ribbon user interface typically has a ribbon bar,
    which contains one or more wxRibbonPages, which in turn each contain one
    or more wxRibbonPanels, which in turn contain controls.

    While a wxRibbonBar has tabs similar to a wxNotebook, it does not follow
    the same API for adding pages. Containers like wxNotebook can contain any
    type of window as a page, hence the normal procedure is to create the
    sub-window and then call wxBookCtrlBase::AddPage(). As wxRibbonBar can only
    have wxRibbonPage as children (and a wxRibbonPage can only have a
    wxRibbonBar as parent), when a page is created, it is automatically added
    to the bar - there is no AddPage equivalent to call.

    After all pages have been created, and all controls and panels placed on
    those pages, Realize() must be called.

    @sa wxRibbonPage
    @sa wxRibbonPanel

    @beginStyleTable
    @style{wxRIBBON_BAR_DEFAULT_STYLE}
        Defined as wxRIBBON_BAR_FLOW_HORIZONTAL |
        wxRIBBON_BAR_SHOW_PAGE_LABELS | wxRIBBON_BAR_SHOW_PANEL_EXT_BUTTONS |
        wxRIBBON_BAR_SHOW_TOGGLE_BUTTON | wxRIBBON_BAR_SHOW_HELP_BUTTON.
    @style{wxRIBBON_BAR_FOLDBAR_STYLE}
        Defined as wxRIBBON_BAR_FLOW_VERTICAL | wxRIBBON_BAR_SHOW_PAGE_ICONS
        | wxRIBBON_BAR_SHOW_PANEL_EXT_BUTTONS |
        wxRIBBON_BAR_SHOW_PANEL_MINIMISE_BUTTONS
    @style{wxRIBBON_BAR_SHOW_PAGE_LABELS}
        Causes labels to be shown on the tabs in the ribbon bar.
    @style{wxRIBBON_BAR_SHOW_PAGE_ICONS}
        Causes icons to be shown on the tabs in the ribbon bar.
    @style{wxRIBBON_BAR_FLOW_HORIZONTAL}
        Causes panels within pages to stack horizontally.
    @style{wxRIBBON_BAR_FLOW_VERTICAL}
        Causes panels within pages to stack vertically.
    @style{wxRIBBON_BAR_SHOW_PANEL_EXT_BUTTONS}
        Causes extension buttons to be shown on panels (where the panel has
        such a button).
    @style{wxRIBBON_BAR_SHOW_PANEL_MINIMISE_BUTTONS}
        Causes minimise buttons to be shown on panels (where the panel has
        such a button).
    @style{wxRIBBON_BAR_SHOW_TOGGLE_BUTTON}
        Causes a toggle button to appear on the ribbon bar at top-right corner.
        This style is new since wxWidgets 2.9.5.
    @style{wxRIBBON_BAR_SHOW_HELP_BUTTON}
        Causes a help button to appear on the ribbon bar at the top-right corner.
        This style is new since wxWidgets 2.9.5.
    @endStyleTable


    @beginEventEmissionTable{wxRibbonBarEvent}
    @event{EVT_RIBBONBAR_PAGE_CHANGED(id, func)}
        Triggered after the transition from one page being active to a different
        page being active.
    @event{EVT_RIBBONBAR_PAGE_CHANGING(id, func)}
        Triggered prior to the transition from one page being active to a
        different page being active, and can veto the change.
    @event{EVT_RIBBONBAR_TAB_MIDDLE_DOWN(id, func)}
        Triggered when the middle mouse button is pressed on a tab.
    @event{EVT_RIBBONBAR_TAB_MIDDLE_UP(id, func)}
        Triggered when the middle mouse button is released on a tab.
    @event{EVT_RIBBONBAR_TAB_RIGHT_DOWN(id, func)}
        Triggered when the right mouse button is pressed on a tab.
    @event{EVT_RIBBONBAR_TAB_RIGHT_UP(id, func)}
        Triggered when the right mouse button is released on a tab.
    @event{EVT_RIBBONBAR_TAB_LEFT_DCLICK(id, func)}
        Triggered when the left mouse button is double clicked on a tab.
    @event{EVT_RIBBONBAR_TOGGLED(id, func)}
        Triggered when the button triggering the ribbon bar is clicked. This
        event is new since wxWidgets 2.9.5.
    @event{EVT_RIBBONBAR_HELP_CLICK(id, func)}
        Triggered when the help button is clicked. This even is new since
        wxWidgets 2.9.5.
    @endEventTable

    @library{wxribbon}
    @category{ribbon}
*/
class wxRibbonBar : public wxRibbonControl
{
public:
    /**
        Default constructor.
        With this constructor, Create() should be called in order to create
        the ribbon bar.
    */
    wxRibbonBar();

    /**
        Construct a ribbon bar with the given parameters.
    */
    wxRibbonBar(wxWindow* parent,
                wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxRIBBON_BAR_DEFAULT_STYLE);

    /**
        Create a ribbon bar in two-step ribbon bar construction.
        Should only be called when the default constructor is used, and
        arguments have the same meaning as in the full constructor.
    */
    bool Create(wxWindow* parent,
                wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxRIBBON_BAR_DEFAULT_STYLE);

    /**
        Destructor.
    */
    virtual ~wxRibbonBar();

    /**
        Set the margin widths (in pixels) on the left and right sides of the
        tab bar region of the ribbon bar. These margins will be painted with
        the tab background, but tabs and scroll buttons will never be painted
        in the margins.

        The left margin could be used for rendering something equivalent to the
        "Office Button", though this is not currently implemented. The right
        margin could be used for rendering a help button, and/or MDI buttons,
        but again, this is not currently implemented.
    */
    void SetTabCtrlMargins(int left, int right);

    /**
        Set the art provider to be used be the ribbon bar. Also sets the art
        provider on all current wxRibbonPage children, and any wxRibbonPage
        children added in the future.

        Note that unlike most other ribbon controls, the ribbon bar creates a
        default art provider when initialised, so an explicit call to
        SetArtProvider() is not required if the default art provider is
        sufficient. Also, unlike other ribbon controls, the ribbon bar takes
        ownership of the given pointer, and will delete it when the art
        provider is changed or the bar is destroyed. If this behaviour is not
        desired, then clone the art provider before setting it.
    */
    void SetArtProvider(wxRibbonArtProvider* art);

    /**
        Set the active page by index, without triggering any events.

        @param page
            The zero-based index of the page to activate.
        @return @true if the specified page is now active, @false if it could
            not be activated (for example because the page index is invalid).
    */
    bool SetActivePage(size_t page);

    /**
        Set the active page, without triggering any events.

        @param page
            The page to activate.
        @return @true if the specified page is now active, @false if it could
            not be activated (for example because the given page is not a child
            of the ribbon bar).
    */
    bool SetActivePage(wxRibbonPage* page);

    /**
        Get the index of the active page.

        In the rare case of no page being active, -1 is returned.
    */
    int GetActivePage() const;

    /**
        Get a page by index.

        NULL will be returned if the given index is out of range.
    */
    wxRibbonPage* GetPage(int n);

    /**
        Get the number of pages in this bar.

        @since 2.9.4
    */
    size_t GetPageCount() const;

    /**
        Dismiss the expanded panel of the currently active page.

        Calls and returns the value from wxRibbonPage::DismissExpandedPanel()
        for the currently active page, or @false if there is no active page.
    */
    bool DismissExpandedPanel();

    /**
        Returns the number for a given ribbon bar page.

        The number can be used in other ribbon bar calls.

        @since 2.9.5
    */
    int GetPageNumber(wxRibbonPage* page) const;

    /**
        Delete a single page from this ribbon bar.

        The user must call wxRibbonBar::Realize() after one (or more) calls to
        this function.

        @since 2.9.4
    */
    void DeletePage(size_t n);

    /**
        Delete all pages from the ribbon bar.

        @since 2.9.4
    */
    void ClearPages();

    /**
        Indicates whether the tab for the given page is shown to the user or
        not.

        By default all page tabs are shown.

        @since 2.9.5
    */
    bool IsPageShown(size_t page) const;

    /**
        Show or hide the tab for a given page.

        After showing or hiding a tab, you need to call wxRibbonBar::Realize().
        If you hide the tab for the currently active page (GetActivePage) then
        you should call SetActivePage to activate a different page.

        @since 2.9.5
    */
    void ShowPage(size_t page, bool show_tab=true);

    /**
        Hides the tab for a given page.

        Equivalent to @c ShowPage(page, false).

        @since 2.9.5
    */
    void HidePage(size_t page);

    /**
        Indicates whether a tab is currently highlighted.

        @see AddPageHighlight()

        @since 2.9.5
    */
    bool IsPageHighlighted(size_t page) const;

    /**
        Highlight the specified tab.

        Highlighted tabs have a colour between that of the active tab
        and a tab over which the mouse is hovering. This can be used
        to make a tab (usually temporarily) more noticeable to the user.

        @since 2.9.5
    */
    void AddPageHighlight(size_t page, bool highlight = true);

    /**
        Changes a tab to not be highlighted.

        @see AddPageHighlight()

        @since 2.9.5
    */
    void RemovePageHighlight(size_t page);

    /**
        Shows or hide the panel area of the ribbon bar according to the
        given display mode.

        @since 3.1.0
    */
    void ShowPanels(wxRibbonDisplayMode mode);

    /**
        Shows or hides the panel area of the ribbon bar.

        If the panel area is hidden, then only the tab of the ribbon bar will
        be shown. This is useful for giving the user more screen space to work
        with when he/she doesn't need to see the ribbon's options.

        If the panel is currently shown, this method pins it, use the other
        overload of this method to specify the exact panel display mode to
        avoid it.

        @since 2.9.2
    */
    void ShowPanels(bool show = true);

    /**
        Hides the panel area of the ribbon bar.

        This method behaves like ShowPanels() with @false argument.

        @since 2.9.2
    */
    void HidePanels();

    /**
        Indicates whether the panel area of the ribbon bar is shown.

        @see ShowPanels()

        @since 2.9.2
    */
    bool ArePanelsShown() const;

    /**
        Returns the current display mode of the panel area.

        @see ShowPanels()

        @since 3.1.0
    */
    wxRibbonDisplayMode GetDisplayMode() const;


    /**
        Perform initial layout and size calculations of the bar and its
        children. This must be called after all of the bar's children have been
        created (and their children created, etc.) - if it is not, then windows
        may not be laid out or sized correctly.

        Also calls wxRibbonPage::Realize() on each child page.
    */
    virtual bool Realize();
};
