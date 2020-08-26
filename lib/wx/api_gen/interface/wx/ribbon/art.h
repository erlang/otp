///////////////////////////////////////////////////////////////////////////////
// Name:        ribbon/art.h
// Purpose:     interface of wxRibbonArtProvider
// Author:      Peter Cawley
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    Identifiers for common settings on ribbon art providers which can be used
    to tweak the appearance of the art provider.

    @see wxRibbonArtProvider::GetColour()
    @see wxRibbonArtProvider::GetFont()
    @see wxRibbonArtProvider::GetMetric()
    @see wxRibbonArtProvider::SetColour()
    @see wxRibbonArtProvider::SetFont()
    @see wxRibbonArtProvider::SetMetric()
*/
enum wxRibbonArtSetting
{
    wxRIBBON_ART_TAB_SEPARATION_SIZE,
    wxRIBBON_ART_PAGE_BORDER_LEFT_SIZE,
    wxRIBBON_ART_PAGE_BORDER_TOP_SIZE,
    wxRIBBON_ART_PAGE_BORDER_RIGHT_SIZE,
    wxRIBBON_ART_PAGE_BORDER_BOTTOM_SIZE,
    wxRIBBON_ART_PANEL_X_SEPARATION_SIZE,
    wxRIBBON_ART_PANEL_Y_SEPARATION_SIZE,
    wxRIBBON_ART_TOOL_GROUP_SEPARATION_SIZE,
    wxRIBBON_ART_GALLERY_BITMAP_PADDING_LEFT_SIZE,
    wxRIBBON_ART_GALLERY_BITMAP_PADDING_RIGHT_SIZE,
    wxRIBBON_ART_GALLERY_BITMAP_PADDING_TOP_SIZE,
    wxRIBBON_ART_GALLERY_BITMAP_PADDING_BOTTOM_SIZE,
    wxRIBBON_ART_PANEL_LABEL_FONT,
    wxRIBBON_ART_BUTTON_BAR_LABEL_FONT,
    wxRIBBON_ART_TAB_LABEL_FONT,
    wxRIBBON_ART_BUTTON_BAR_LABEL_COLOUR,
    /// @since 2.9.5
    wxRIBBON_ART_BUTTON_BAR_LABEL_DISABLED_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_HOVER_BORDER_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_HOVER_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_HOVER_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_HOVER_BACKGROUND_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_HOVER_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_ACTIVE_BORDER_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_ACTIVE_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_ACTIVE_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_ACTIVE_BACKGROUND_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_ACTIVE_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_GALLERY_BORDER_COLOUR,
    wxRIBBON_ART_GALLERY_HOVER_BACKGROUND_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_BACKGROUND_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_FACE_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_HOVER_BACKGROUND_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_HOVER_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_HOVER_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_HOVER_FACE_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_ACTIVE_BACKGROUND_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_ACTIVE_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_ACTIVE_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_ACTIVE_FACE_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_DISABLED_BACKGROUND_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_DISABLED_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_DISABLED_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_GALLERY_BUTTON_DISABLED_FACE_COLOUR,
    wxRIBBON_ART_GALLERY_ITEM_BORDER_COLOUR,
    wxRIBBON_ART_TAB_LABEL_COLOUR,
    /// @since 3.1.3
    wxRIBBON_ART_TAB_ACTIVE_LABEL_COLOUR,
    /// @since 3.1.3
    wxRIBBON_ART_TAB_HOVER_LABEL_COLOUR,
    wxRIBBON_ART_TAB_SEPARATOR_COLOUR,
    wxRIBBON_ART_TAB_SEPARATOR_GRADIENT_COLOUR,
    wxRIBBON_ART_TAB_CTRL_BACKGROUND_COLOUR,
    wxRIBBON_ART_TAB_CTRL_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_TAB_HOVER_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_TAB_HOVER_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_TAB_HOVER_BACKGROUND_COLOUR,
    wxRIBBON_ART_TAB_HOVER_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_TAB_ACTIVE_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_TAB_ACTIVE_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_TAB_ACTIVE_BACKGROUND_COLOUR,
    wxRIBBON_ART_TAB_ACTIVE_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_TAB_BORDER_COLOUR,
    wxRIBBON_ART_PANEL_BORDER_COLOUR,
    wxRIBBON_ART_PANEL_BORDER_GRADIENT_COLOUR,
    wxRIBBON_ART_PANEL_MINIMISED_BORDER_COLOUR,
    wxRIBBON_ART_PANEL_MINIMISED_BORDER_GRADIENT_COLOUR,
    wxRIBBON_ART_PANEL_LABEL_BACKGROUND_COLOUR,
    wxRIBBON_ART_PANEL_LABEL_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_PANEL_LABEL_COLOUR,
    wxRIBBON_ART_PANEL_HOVER_LABEL_BACKGROUND_COLOUR,
    wxRIBBON_ART_PANEL_HOVER_LABEL_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_PANEL_HOVER_LABEL_COLOUR,
    wxRIBBON_ART_PANEL_MINIMISED_LABEL_COLOUR,
    wxRIBBON_ART_PANEL_ACTIVE_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_PANEL_ACTIVE_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_PANEL_ACTIVE_BACKGROUND_COLOUR,
    wxRIBBON_ART_PANEL_ACTIVE_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_PAGE_BORDER_COLOUR,
    wxRIBBON_ART_PAGE_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_PAGE_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_PAGE_BACKGROUND_COLOUR,
    wxRIBBON_ART_PAGE_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_PAGE_HOVER_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_PAGE_HOVER_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_PAGE_HOVER_BACKGROUND_COLOUR,
    wxRIBBON_ART_PAGE_HOVER_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_TOOLBAR_BORDER_COLOUR,
    wxRIBBON_ART_TOOLBAR_HOVER_BORDER_COLOUR,
    wxRIBBON_ART_TOOLBAR_FACE_COLOUR,
    wxRIBBON_ART_TOOL_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_TOOL_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_TOOL_BACKGROUND_COLOUR,
    wxRIBBON_ART_TOOL_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_TOOL_HOVER_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_TOOL_HOVER_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_TOOL_HOVER_BACKGROUND_COLOUR,
    wxRIBBON_ART_TOOL_HOVER_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_TOOL_ACTIVE_BACKGROUND_TOP_COLOUR,
    wxRIBBON_ART_TOOL_ACTIVE_BACKGROUND_TOP_GRADIENT_COLOUR,
    wxRIBBON_ART_TOOL_ACTIVE_BACKGROUND_COLOUR,
    wxRIBBON_ART_TOOL_ACTIVE_BACKGROUND_GRADIENT_COLOUR,
    wxRIBBON_ART_BUTTON_BAR_LABEL_HIGHLIGHT_COLOUR, //< @since 3.1.0
    wxRIBBON_ART_BUTTON_BAR_LABEL_HIGHLIGHT_GRADIENT_COLOUR, //< @since 3.1.0
    wxRIBBON_ART_BUTTON_BAR_LABEL_HIGHLIGHT_TOP_COLOUR, //< @since 3.1.0
    wxRIBBON_ART_BUTTON_BAR_LABEL_HIGHLIGHT_GRADIENT_TOP_COLOUR, //< @since 3.1.0
};

/**
    Flags used to describe the direction, state, and/or purpose of a
    ribbon-style scroll button.

    @see wxRibbonArtProvider::DrawScrollButton()
    @see wxRibbonArtProvider::GetScrollButtonMinimumSize()
*/
enum wxRibbonScrollButtonStyle
{
    wxRIBBON_SCROLL_BTN_LEFT = 0, /**< Button will scroll to the left. */
    wxRIBBON_SCROLL_BTN_RIGHT = 1, /**< Button will scroll to the right. */
    wxRIBBON_SCROLL_BTN_UP = 2, /**< Button will scroll upward. */
    wxRIBBON_SCROLL_BTN_DOWN = 3, /**< Button will scroll downward. */

  /** A mask to extract direction from a combination of flags. */
    wxRIBBON_SCROLL_BTN_DIRECTION_MASK = 3,

    wxRIBBON_SCROLL_BTN_NORMAL = 0, /**< Button is not active or hovered. */
    wxRIBBON_SCROLL_BTN_HOVERED = 4, /**< Button has a cursor hovering over it. */
    wxRIBBON_SCROLL_BTN_ACTIVE = 8, /**< Button is being pressed. */

  /** A mask to extract state from a combination of flags. */
    wxRIBBON_SCROLL_BTN_STATE_MASK = 12,

    wxRIBBON_SCROLL_BTN_FOR_OTHER = 0, /**< Button is not for scrolling tabs nor pages. */
    wxRIBBON_SCROLL_BTN_FOR_TABS = 16, /**< Button is for scrolling tabs. */
    wxRIBBON_SCROLL_BTN_FOR_PAGE = 32, /**< Button is for scrolling pages. */

  /** A mask to extract purpose from a combination of flags. */
    wxRIBBON_SCROLL_BTN_FOR_MASK = 48,
};

/**
    Buttons on a ribbon button bar and tools on a ribbon tool bar can each be
    one of three different kinds.
*/
enum wxRibbonButtonKind
{
    /**
        Normal button or tool with a clickable area which causes some generic
        action.
    */
    wxRIBBON_BUTTON_NORMAL    = 1 << 0,

    /**
        Dropdown button or tool with a clickable area which typically causes a
        dropdown menu.
    */
    wxRIBBON_BUTTON_DROPDOWN  = 1 << 1,

    /**
        Button or tool with two clickable areas - one which causes a dropdown
        menu, and one which causes a generic action.
    */
    wxRIBBON_BUTTON_HYBRID    = wxRIBBON_BUTTON_NORMAL | wxRIBBON_BUTTON_DROPDOWN,

    /**
        Normal button or tool with a clickable area which toggles the button
        between a pressed and unpressed state.
    */
    wxRIBBON_BUTTON_TOGGLE    = 1 << 2
};

/**
    @class wxRibbonArtProvider

    wxRibbonArtProvider is responsible for drawing all the components of the ribbon
    interface. This allows a ribbon bar to have a pluggable look-and-feel, while
    retaining the same underlying behaviour. As a single art provider is used for
    all ribbon components, a ribbon bar usually has a consistent (though unique)
    appearance.

    By default, a wxRibbonBar uses an instance of this class called
    @c wxRibbonDefaultArtProvider, which resolves to @c wxRibbonAUIArtProvider,
    @c wxRibbonMSWArtProvider, or @c wxRibbonOSXArtProvider - whichever is most appropriate
    to the current platform. These art providers are all slightly configurable with
    regard to colours and fonts, but for larger modifications, you can derive from
    one of these classes, or write a completely new art provider class.
    Call wxRibbonBar::SetArtProvider to change the art provider being used.

    @library{wxribbon}
    @category{ribbon}

    @see wxRibbonBar
*/
class wxRibbonArtProvider
{
public:
    /**
        Constructor.
    */
    wxRibbonArtProvider();

    /**
        Destructor.
    */
    virtual ~wxRibbonArtProvider();

    /**
        Create a new art provider which is a clone of this one.
    */
    virtual wxRibbonArtProvider* Clone() const = 0;

    /**
        Set the style flags.

        Normally called automatically by wxRibbonBar::SetArtProvider with the ribbon
        bar's style flags, so that the art provider has the same flags as the bar which
        it is serving.
    */
    virtual void SetFlags(long flags) = 0;

    /**
        Get the previously set style flags.
    */
    virtual long GetFlags() const = 0;

    /**
        Get the value of a certain integer setting.
        @a id can be one of the size values of @ref wxRibbonArtSetting.
    */
    virtual int GetMetric(int id) const = 0;

    /**
        Set the value of a certain integer setting to the value @e new_val.
        @a id can be one of the size values of @ref wxRibbonArtSetting.
    */
    virtual void SetMetric(int id, int new_val) = 0;

    /**
        Set the value of a certain font setting to the value @e font.
        @a id can be one of the font values of @ref wxRibbonArtSetting.
    */
    virtual void SetFont(int id, const wxFont& font) = 0;

    /**
        Get the value of a certain font setting.
        @a id can be one of the font values of @ref wxRibbonArtSetting.
    */
    virtual wxFont GetFont(int id) const = 0;

    /**
        Get the value of a certain colour setting.
        @a id can be one of the colour values of @ref wxRibbonArtSetting.
    */
    virtual wxColour GetColour(int id) const = 0;

    /**
        Set the value of a certain colour setting to the value @e colour.
        @a id can be one of the colour values of @ref wxRibbonArtSetting, though
        not all colour settings will have an effect on every art provider.

        @see SetColourScheme()
    */
    virtual void SetColour(int id, const wxColour& colour) = 0;

    /**
        @see wxRibbonArtProvider::GetColour()
    */
    wxColour GetColor(int id) const;

    /**
        @see wxRibbonArtProvider::SetColour()
    */
    void SetColor(int id, const wxColour& color);

    /**
        Get the current colour scheme.

        Returns three colours such that if SetColourScheme() were called with
        them, the colour scheme would be restored to what it was when
        SetColourScheme() was last called. In practice, this usually means that
        the returned values are the three colours given in the last call to
        SetColourScheme(), however if SetColourScheme() performs an idempotent
        operation upon the colours it is given (like clamping a component of
        the colour), then the returned values may not be the three colours
        given in the last call to SetColourScheme().
        If SetColourScheme() has not been called, then the returned values
        should result in a colour scheme similar to, if not identical to, the
        default colours of the art provider.
        Note that if SetColour() is called, then GetColourScheme() does not try
        and return a colour scheme similar to colours being used - it's return
        values are dependent upon the last values given to SetColourScheme(),
        as described above.

        @param[out] primary
            Pointer to a location to store the primary colour, or NULL.
        @param[out] secondary
            Pointer to a location to store the secondary colour, or NULL.
        @param[out] tertiary
            Pointer to a location to store the tertiary colour, or NULL.
    */
    virtual void GetColourScheme(wxColour* primary,
                        wxColour* secondary,
                        wxColour* tertiary) const = 0;

    /**
        Set all applicable colour settings from a few base colours.

        Uses any or all of the three given colours to create a colour scheme,
        and then sets all colour settings which are relevant to the art
        provider using that scheme.
        Note that some art providers may not use the tertiary colour for
        anything, and some may not use the secondary colour either.

        @see SetColour()
        @see GetColourScheme()
    */
    virtual void SetColourScheme(const wxColour& primary,
                        const wxColour& secondary,
                        const wxColour& tertiary) = 0;

    /**
        Draw the background of the tab region of a ribbon bar.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto.
        @param rect
            The rectangle within which to draw.
    */
    virtual void DrawTabCtrlBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect) = 0;

    /**
        Draw a single tab in the tab region of a ribbon bar.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto (not the wxRibbonPage
            associated with the tab being drawn).
        @param tab
            The rectangle within which to draw, and also the tab label, icon,
            and state (active and/or hovered). The drawing rectangle will be
            entirely within a rectangle on the same device context previously
            painted with DrawTabCtrlBackground(). The rectangle's width will
            be at least the minimum value returned by GetBarTabWidth(), and
            height will be the value returned by GetTabCtrlHeight().
    */
    virtual void DrawTab(wxDC& dc,
                        wxWindow* wnd,
                        const wxRibbonPageTabInfo& tab) = 0;

    /**
        Draw a separator between two tabs in a ribbon bar.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto.
        @param rect
            The rectangle within which to draw, which will be entirely within a
            rectangle on the same device context previously painted with
            DrawTabCtrlBackground().
        @param visibility
            The opacity with which to draw the separator. Values are in the range
            [0, 1], with 0 being totally transparent, and 1 being totally opaque.
    */
    virtual void DrawTabSeparator(wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        double visibility) = 0;

    /**
        Draw the background of a ribbon page.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto (which is commonly the wxRibbonPage
            whose background is being drawn, but doesn't have to be).
        @param rect
            The rectangle within which to draw.

        @sa GetPageBackgroundRedrawArea
    */
    virtual void DrawPageBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect) = 0;

    /**
        Draw a ribbon-style scroll button.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto.
        @param rect
            The rectangle within which to draw. The size of this rectangle
            will be at least the size returned by GetScrollButtonMinimumSize()
            for a scroll button with the same style. For tab scroll buttons,
            this rectangle will be entirely within a rectangle on the same
            device context previously painted with DrawTabCtrlBackground(), but
            this is not guaranteed for other types of button (for example,
            page scroll buttons will not be painted on an area previously
            painted with DrawPageBackground()).
        @param style
            A combination of flags from @ref wxRibbonScrollButtonStyle, including
            a direction, a for flag, and one or more states.
    */
    virtual void DrawScrollButton(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        long style) = 0;

    /**
        Draw the background and chrome for a ribbon panel. This should draw
        the border, background, label, and any other items of a panel which
        are outside the client area of a panel.

        Note that when a panel is minimised, this function is not called - only
        DrawMinimisedPanel() is called, so a background should be explicitly
        painted by that if required.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto, which is always the panel
            whose background and chrome is being drawn. The panel label and
            other panel attributes can be obtained by querying this.
        @param rect
            The rectangle within which to draw.
    */
    virtual void DrawPanelBackground(
                        wxDC& dc,
                        wxRibbonPanel* wnd,
                        const wxRect& rect) = 0;

    /**
        Draw the background and chrome for a wxRibbonGallery control. This
        should draw the border, background, scroll buttons, extension button,
        and any other UI elements which are not attached to a specific gallery
        item.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto, which is always the gallery
            whose background and chrome is being drawn. Attributes used during
            drawing like the gallery hover state and individual button states
            can be queried from this parameter by wxRibbonGallery::IsHovered(),
            wxRibbonGallery::GetExtensionButtonState(),
            wxRibbonGallery::GetUpButtonState(), and
            wxRibbonGallery::GetDownButtonState().
        @param rect
            The rectangle within which to draw. This rectangle is the entire
            area of the gallery control, not just the client rectangle.
    */
    virtual void DrawGalleryBackground(
                        wxDC& dc,
                        wxRibbonGallery* wnd,
                        const wxRect& rect) = 0;

    /**
        Draw the background of a single item in a wxRibbonGallery control. This
        is painted on top of a gallery background, and behind the items bitmap.
        Unlike DrawButtonBarButton() and DrawTool(), it is not expected to draw
        the item bitmap - that is done by the gallery control itself.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto, which is always the gallery
            which contains the item being drawn.
        @param rect
            The rectangle within which to draw. The size of this rectangle will
            be the size of the item's bitmap, expanded by gallery item padding
            values (wxRIBBON_ART_GALLERY_BITMAP_PADDING_LEFT_SIZE,
            wxRIBBON_ART_GALLERY_BITMAP_PADDING_RIGHT_SIZE,
            wxRIBBON_ART_GALLERY_BITMAP_PADDING_TOP_SIZE, and
            wxRIBBON_ART_GALLERY_BITMAP_PADDING_BOTTOM_SIZE). The drawing
            rectangle will be entirely within a rectangle on the same device
            context previously painted with DrawGalleryBackground().
        @param item
            The item whose background is being painted. Typically the
            background will vary if the item is hovered, active, or selected;
            wxRibbonGallery::GetSelection(), wxRibbonGallery::GetActiveItem(),
            and wxRibbonGallery::GetHoveredItem() can be called to test if the
            given item is in one of these states.
    */
    virtual void DrawGalleryItemBackground(
                        wxDC& dc,
                        wxRibbonGallery* wnd,
                        const wxRect& rect,
                        wxRibbonGalleryItem* item) = 0;

    /**
        Draw a minimised ribbon panel.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto, which is always the panel
            which is minimised. The panel label can be obtained from this
            window. The minimised icon obtained from querying the window may
            not be the size requested by GetMinimisedPanelMinimumSize() - the
            @a bitmap argument contains the icon in the requested size.
        @param rect
            The rectangle within which to draw. The size of the rectangle will
            be at least the size returned by GetMinimisedPanelMinimumSize().
        @param bitmap
            A copy of the panel's minimised bitmap rescaled to the size
            returned by GetMinimisedPanelMinimumSize().
    */
    virtual void DrawMinimisedPanel(
                        wxDC& dc,
                        wxRibbonPanel* wnd,
                        const wxRect& rect,
                        wxBitmap& bitmap) = 0;

    /**
        Draw the background for a wxRibbonButtonBar control.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto (which will typically be the
            button bar itself, though this is not guaranteed).
        @param rect
            The rectangle within which to draw.
    */
    virtual void DrawButtonBarBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect) = 0;

    /**
        Draw a single button for a wxRibbonButtonBar control.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto.
        @param rect
            The rectangle within which to draw. The size of this rectangle will
            be a size previously returned by GetButtonBarButtonSize(), and the
            rectangle will be entirely within a rectangle on the same device
            context previously painted with DrawButtonBarBackground().
        @param kind
            The kind of button to draw (normal, dropdown or hybrid).
        @param state
            Combination of a size flag and state flags from the
            wxRibbonButtonBarButtonState enumeration.
        @param label
            The label of the button.
        @param bitmap_large
            The large bitmap of the button (or the large disabled bitmap when
            wxRIBBON_BUTTONBAR_BUTTON_DISABLED is set in @a state).
        @param bitmap_small
            The small bitmap of the button (or the small disabled bitmap when
            wxRIBBON_BUTTONBAR_BUTTON_DISABLED is set in @a state).
    */
    virtual void DrawButtonBarButton(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        wxRibbonButtonKind kind,
                        long state,
                        const wxString& label,
                        const wxBitmap& bitmap_large,
                        const wxBitmap& bitmap_small) = 0;

    /**
        Draw the background for a wxRibbonToolBar control.

        @param dc
            The device context to draw onto.
        @param wnd
            The which is being drawn onto. In most cases this will be a
            wxRibbonToolBar, but it doesn't have to be.
        @param rect
            The rectangle within which to draw. Some of this rectangle will
            later be drawn over using DrawToolGroupBackground() and DrawTool(),
            but not all of it will (unless there is only a single group of
            tools).
    */
    virtual void DrawToolBarBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect) = 0;

    /**
        Draw the background for a group of tools on a wxRibbonToolBar control.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto. In most cases this will be a
            wxRibbonToolBar, but it doesn't have to be.
        @param rect
            The rectangle within which to draw. This rectangle is a union of
            the individual tools' rectangles. As there are no gaps between
            tools, this rectangle will be painted over exactly once by calls to
            DrawTool(). The group background could therefore be painted by
            DrawTool(), though it can be conceptually easier and more efficient
            to draw it all at once here. The rectangle will be entirely within
            a rectangle on the same device context previously painted with
            DrawToolBarBackground().
    */
    virtual void DrawToolGroupBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect) = 0;

    /**
        Draw a single tool (for a wxRibbonToolBar control).

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto. In most cases this will be a
            wxRibbonToolBar, but it doesn't have to be.
        @param rect
            The rectangle within which to draw. The size of this rectangle will
            at least the size returned by GetToolSize(), and the height of it
            will be equal for all tools within the same group. The rectangle
            will be entirely within a rectangle on the same device context
            previously painted with DrawToolGroupBackground().
        @param bitmap
            The bitmap to use as the tool's foreground. If the tool is a hybrid
            or dropdown tool, then the foreground should also contain a
            standard dropdown button.
        @param kind
            The kind of tool to draw (normal, dropdown, or hybrid).
        @param state
            A combination of wxRibbonToolBarToolState flags giving the state of
            the tool and it's relative position within a tool group.
    */
    virtual void DrawTool(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        const wxBitmap& bitmap,
                        wxRibbonButtonKind kind,
                        long state) = 0;

    /**
        Draw toggle button on wxRibbonBar. This should draw a small toggle button
        at top right corner of ribbon bar.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto, which is always the panel
            whose background and chrome is being drawn. The panel label and
            other panel attributes can be obtained by querying this.
        @param rect
            The rectangle within which to draw.
        @param mode
            The wxRibbonDisplayMode which should be applied to display button

        @since 2.9.5
    */
    virtual void DrawToggleButton(wxDC& dc,
                                  wxRibbonBar* wnd,
                                  const wxRect& rect,
                                  wxRibbonDisplayMode mode) = 0;

    /**
        Draw help button on wxRibbonBar. This should draw a help button
        at top right corner of ribbon bar.

        @param dc
            The device context to draw onto.
        @param wnd
            The window which is being drawn onto, which is always the panel
            whose background and chrome is being drawn. The panel label and
            other panel attributes can be obtained by querying this.
        @param rect
            The rectangle within which to draw.

        @since 2.9.5
    */
    virtual void DrawHelpButton(wxDC& dc,
                                wxRibbonBar* wnd,
                                const wxRect& rect) = 0;

    /**
        Calculate the ideal and minimum width (in pixels) of a tab in a ribbon
        bar.

        @param dc
            A device context to use when one is required for size calculations.
        @param wnd
            The window onto which the tab will eventually be drawn.
        @param label
            The tab's label (or wxEmptyString if it has none).
        @param bitmap
            The tab's icon (or wxNullBitmap if it has none).
        @param[out] ideal
            The ideal width (in pixels) of the tab.
        @param[out] small_begin_need_separator
            A size less than the @a ideal size, at which a tab separator should
            begin to be drawn (i.e. drawn, but still fairly transparent).
        @param[out] small_must_have_separator
            A size less than the @a small_begin_need_separator size, at which a
            tab separator must be drawn (i.e. drawn at full opacity).
        @param[out] minimum
            A size less than the @a small_must_have_separator size, and greater
            than or equal to zero, which is the minimum pixel width for the tab.
    */
    virtual void GetBarTabWidth(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxString& label,
                        const wxBitmap& bitmap,
                        int* ideal,
                        int* small_begin_need_separator,
                        int* small_must_have_separator,
                        int* minimum) = 0;

    /**
        Calculate the height (in pixels) of the tab region of a ribbon bar.
        Note that as the tab region can contain scroll buttons, the height
        should be greater than or equal to the minimum height for a tab scroll
        button.

        @param dc
            A device context to use when one is required for size calculations.
        @param wnd
            The window onto which the tabs will eventually be drawn.
        @param pages
            The tabs which will acquire the returned height.
    */
    virtual int GetTabCtrlHeight(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRibbonPageTabInfoArray& pages) = 0;

    /**
        Calculate the minimum size (in pixels) of a scroll button.

        @param dc
            A device context to use when one is required for size calculations.
        @param wnd
            The window onto which the scroll button will eventually be drawn.
        @param style
            A combination of flags from @ref wxRibbonScrollButtonStyle, including
            a direction, and a for flag (state flags may be given too, but
            should be ignored, as a button should retain a constant size,
            regardless of its state).
    */
    virtual wxSize GetScrollButtonMinimumSize(
                        wxDC& dc,
                        wxWindow* wnd,
                        long style) = 0;

    /**
        Calculate the size of a panel for a given client size. This should
        increment the given size by enough to fit the panel label and other
        chrome.

        @param dc
            A device context to use if one is required for size calculations.
        @param wnd
            The ribbon panel in question.
        @param client_size
            The client size.
        @param[out] client_offset
            The offset where the client rectangle begins within the panel (may
            be NULL).

        @sa GetPanelClientSize()
    */
    virtual wxSize GetPanelSize(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxSize client_size,
                        wxPoint* client_offset) = 0;

    /**
        Calculate the client size of a panel for a given overall size. This
        should act as the inverse to GetPanelSize(), and decrement the given
        size by enough to fit the panel label and other chrome.

        @param dc
            A device context to use if one is required for size calculations.
        @param wnd
            The ribbon panel in question.
        @param size
            The overall size to calculate client size for.
        @param[out] client_offset
            The offset where the returned client size begins within the given
            @a size (may be NULL).

        @sa GetPanelSize()
    */
    virtual wxSize GetPanelClientSize(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxSize size,
                        wxPoint* client_offset) = 0;

    /**
        Calculate the position and size of the panel extension button.

        @param dc
            A device context to use if one is required for size calculations.
        @param wnd
            The ribbon panel in question.
        @param rect
            The panel rectangle from which calculate extension button rectangle.

        @since 2.9.4
    */
    virtual wxRect GetPanelExtButtonArea(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxRect rect) = 0;

    /**
        Calculate the size of a wxRibbonGallery control for a given client
        size. This should increment the given size by enough to fit the gallery
        border, buttons, and any other chrome.

        @param dc
            A device context to use if one is required for size calculations.
        @param wnd
            The gallery in question.
        @param client_size
            The client size.

        @sa GetGalleryClientSize()
    */
    virtual wxSize GetGallerySize(
                        wxDC& dc,
                        const wxRibbonGallery* wnd,
                        wxSize client_size) = 0;

    /**
        Calculate the client size of a wxRibbonGallery control for a given
        size. This should act as the inverse to GetGallerySize(), and decrement
        the given size by enough to fit the gallery border, buttons, and other
        chrome.

        @param dc
            A device context to use if one is required for size calculations.
        @param wnd
            The gallery in question.
        @param size
            The overall size to calculate the client size for.
        @param[out] client_offset
            The position within the given size at which the returned client
            size begins.
        @param[out] scroll_up_button
            The rectangle within the given size which the scroll up button
            occupies.
        @param[out] scroll_down_button
            The rectangle within the given size which the scroll down button
            occupies.
        @param[out] extension_button
            The rectangle within the given size which the extension button
            occupies.
    */
    virtual wxSize GetGalleryClientSize(
                        wxDC& dc,
                        const wxRibbonGallery* wnd,
                        wxSize size,
                        wxPoint* client_offset,
                        wxRect* scroll_up_button,
                        wxRect* scroll_down_button,
                        wxRect* extension_button) = 0;

    /**
        Calculate the portion of a page background which needs to be redrawn
        when a page is resized. To optimise the drawing of page backgrounds, as
        small an area as possible should be returned. Of course, if the way in
        which a background is drawn means that the entire background needs to
        be repainted on resize, then the entire new size should be returned.

        @param dc
            A device context to use when one is required for size calculations.
        @param wnd
            The page which is being resized.
        @param page_old_size
            The size of the page prior to the resize (which has already been
            painted).
        @param page_new_size
            The size of the page after the resize.
    */
    virtual wxRect GetPageBackgroundRedrawArea(
                        wxDC& dc,
                        const wxRibbonPage* wnd,
                        wxSize page_old_size,
                        wxSize page_new_size) = 0;

    /**
        Calculate the size of a button within a wxRibbonButtonBar.

        @param dc
            A device context to use when one is required for size calculations.
        @param wnd
            The window onto which the button will eventually be drawn (which is
            normally a wxRibbonButtonBar, though this is not guaranteed).
        @param kind
            The kind of button.
        @param size
            The size-class to calculate the size for. Buttons on a button bar
            can have three distinct sizes: wxRIBBON_BUTTONBAR_BUTTON_SMALL,
            wxRIBBON_BUTTONBAR_BUTTON_MEDIUM, and wxRIBBON_BUTTONBAR_BUTTON_LARGE.
            If the requested size-class is not applicable, then @false should
            be returned.
        @param label
            The label of the button.
        @param text_min_width
            The minimum width of the button label.
            Set this to 0 if it is not used.
        @param bitmap_size_large
            The size of all "large" bitmaps on the button bar.
        @param bitmap_size_small
            The size of all "small" bitmaps on the button bar.
        @param[out] button_size
            The size, in pixels, of the button.
        @param[out] normal_region
            The region of the button which constitutes the normal button.
        @param[out] dropdown_region
            The region of the button which constitutes the dropdown button.

        @return @true if a size exists for the button, @false otherwise.
    */
    virtual bool GetButtonBarButtonSize(
                        wxDC& dc,
                        wxWindow* wnd,
                        wxRibbonButtonKind kind,
                        wxRibbonButtonBarButtonState size,
                        const wxString& label,
                        wxCoord text_min_width,
                        wxSize bitmap_size_large,
                        wxSize bitmap_size_small,
                        wxSize* button_size,
                        wxRect* normal_region,
                        wxRect* dropdown_region) = 0;

    /**
        Gets the width of the string if it is used as
        a wxRibbonButtonBar button label.

        @param dc
            A device context to use when one is required for size calculations.
        @param label
            The string whose width shall be calculated.
        @param kind
            The kind of button.
        @param size
            The size-class to calculate the size for. Buttons on a button bar
            can have three distinct sizes: wxRIBBON_BUTTONBAR_BUTTON_SMALL,
            wxRIBBON_BUTTONBAR_BUTTON_MEDIUM, and wxRIBBON_BUTTONBAR_BUTTON_LARGE.
            If the requested size-class is not applicable, then NULL should
            be returned.

        @return Width of the given label text in pixel.

        @note This function only works with single-line strings.

        @since 3.1.2
    */
    virtual wxCoord GetButtonBarButtonTextWidth(
                        wxDC& dc, const wxString& label,
                        wxRibbonButtonKind kind,
                        wxRibbonButtonBarButtonState size) = 0;

    /**
        Calculate the size of a minimised ribbon panel.

        @param dc
            A device context to use when one is required for size calculations.
        @param wnd
            The ribbon panel in question. Attributes like the panel label can
            be queried from this.
        @param[out] desired_bitmap_size
            Optional parameter which is filled with the size of the bitmap
            suitable for a minimised ribbon panel.
        @param[out] expanded_panel_direction
            Optional parameter which is filled with the direction of the
            minimised panel (@c wxEAST or @c wxSOUTH depending on the style).
    */
    virtual wxSize GetMinimisedPanelMinimumSize(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxSize* desired_bitmap_size,
                        wxDirection* expanded_panel_direction) = 0;

    /**
        Calculate the size of a tool within a wxRibbonToolBar.

        @param dc
            A device context to use when one is required for size calculations.
        @param wnd
            The window onto which the tool will eventually be drawn.
        @param bitmap_size
            The size of the tool's foreground bitmap.
        @param kind
            The kind of tool (normal, dropdown, or hybrid).
        @param is_first
            @true if the tool is the first within its group. @false otherwise.
        @param is_last
            @true if the tool is the last within its group. @false otherwise.
        @param[out] dropdown_region
            For dropdown and hybrid tools, the region within the returned
            size which counts as the dropdown part.
    */
    virtual wxSize GetToolSize(
                        wxDC& dc,
                        wxWindow* wnd,
                        wxSize bitmap_size,
                        wxRibbonButtonKind kind,
                        bool is_first,
                        bool is_last,
                        wxRect* dropdown_region) = 0;

    /**
        Calculate the position and size of the ribbon's toggle button.

        @param rect
            The ribbon bar rectangle from which calculate toggle button rectangle.

        @since 2.9.5
    */
    virtual wxRect GetBarToggleButtonArea(const wxRect& rect) = 0;

    /**
        Calculate the position and size of the ribbon's help button.

        @param rect
            The ribbon bar rectangle from which calculate help button rectangle.

        @since 2.9.5
    */
    virtual wxRect GetRibbonHelpButtonArea(const wxRect& rect) = 0;
};



class wxRibbonMSWArtProvider : public wxRibbonArtProvider
{
public:
    wxRibbonMSWArtProvider(bool set_colour_scheme = true);
    virtual ~wxRibbonMSWArtProvider();

    wxRibbonArtProvider* Clone() const;
    void SetFlags(long flags);
    long GetFlags() const;

    int GetMetric(int id) const;
    void SetMetric(int id, int new_val);
    void SetFont(int id, const wxFont& font);
    wxFont GetFont(int id) const;
    wxColour GetColour(int id) const;
    void SetColour(int id, const wxColour& colour);
    void GetColourScheme(wxColour* primary,
                         wxColour* secondary,
                         wxColour* tertiary) const;
    void SetColourScheme(const wxColour& primary,
                         const wxColour& secondary,
                         const wxColour& tertiary);

    int GetTabCtrlHeight(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRibbonPageTabInfoArray& pages);

    void DrawTabCtrlBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    void DrawTab(wxDC& dc,
                 wxWindow* wnd,
                 const wxRibbonPageTabInfo& tab);

    void DrawTabSeparator(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        double visibility);

    void DrawPageBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    void DrawScrollButton(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        long style);

    void DrawPanelBackground(
                        wxDC& dc,
                        wxRibbonPanel* wnd,
                        const wxRect& rect);

    void DrawGalleryBackground(
                        wxDC& dc,
                        wxRibbonGallery* wnd,
                        const wxRect& rect);

    void DrawGalleryItemBackground(
                        wxDC& dc,
                        wxRibbonGallery* wnd,
                        const wxRect& rect,
                        wxRibbonGalleryItem* item);

    void DrawMinimisedPanel(
                        wxDC& dc,
                        wxRibbonPanel* wnd,
                        const wxRect& rect,
                        wxBitmap& bitmap);

    void DrawButtonBarBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    void DrawButtonBarButton(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        wxRibbonButtonKind kind,
                        long state,
                        const wxString& label,
                        const wxBitmap& bitmap_large,
                        const wxBitmap& bitmap_small);

    void DrawToolBarBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    void DrawToolGroupBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    void DrawTool(
                wxDC& dc,
                wxWindow* wnd,
                const wxRect& rect,
                const wxBitmap& bitmap,
                wxRibbonButtonKind kind,
                long state);

    void DrawToggleButton(
                        wxDC& dc,
                        wxRibbonBar* wnd,
                        const wxRect& rect,
                        wxRibbonDisplayMode mode);

    void DrawHelpButton(wxDC& dc,
                        wxRibbonBar* wnd,
                        const wxRect& rect);

    void GetBarTabWidth(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxString& label,
                        const wxBitmap& bitmap,
                        int* ideal,
                        int* small_begin_need_separator,
                        int* small_must_have_separator,
                        int* minimum);

    wxSize GetScrollButtonMinimumSize(
                        wxDC& dc,
                        wxWindow* wnd,
                        long style);

    wxSize GetPanelSize(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxSize client_size,
                        wxPoint* client_offset);

    wxSize GetPanelClientSize(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxSize size,
                        wxPoint* client_offset);

    wxRect GetPanelExtButtonArea(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxRect rect);

    wxSize GetGallerySize(
                        wxDC& dc,
                        const wxRibbonGallery* wnd,
                        wxSize client_size);

    wxSize GetGalleryClientSize(
                        wxDC& dc,
                        const wxRibbonGallery* wnd,
                        wxSize size,
                        wxPoint* client_offset,
                        wxRect* scroll_up_button,
                        wxRect* scroll_down_button,
                        wxRect* extension_button);

    wxRect GetPageBackgroundRedrawArea(
                        wxDC& dc,
                        const wxRibbonPage* wnd,
                        wxSize page_old_size,
                        wxSize page_new_size);

    bool GetButtonBarButtonSize(
                        wxDC& dc,
                        wxWindow* wnd,
                        wxRibbonButtonKind kind,
                        wxRibbonButtonBarButtonState size,
                        const wxString& label,
                        wxCoord text_min_width,
                        wxSize bitmap_size_large,
                        wxSize bitmap_size_small,
                        wxSize* button_size,
                        wxRect* normal_region,
                        wxRect* dropdown_region);

    wxSize GetMinimisedPanelMinimumSize(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxSize* desired_bitmap_size,
                        wxDirection* expanded_panel_direction);

    wxSize GetToolSize(
                        wxDC& dc,
                        wxWindow* wnd,
                        wxSize bitmap_size,
                        wxRibbonButtonKind kind,
                        bool is_first,
                        bool is_last,
                        wxRect* dropdown_region);

    wxRect GetBarToggleButtonArea(const wxRect& rect);

    wxRect GetRibbonHelpButtonArea(const wxRect& rect);
};


class wxRibbonAUIArtProvider : public wxRibbonMSWArtProvider
{
public:
    wxRibbonAUIArtProvider();
    virtual ~wxRibbonAUIArtProvider();

    wxRibbonArtProvider* Clone() const;

    wxColour GetColour(int id) const;
    void SetColour(int id, const wxColour& colour);
    void SetColourScheme(const wxColour& primary,
                         const wxColour& secondary,
                         const wxColour& tertiary);
    void SetFont(int id, const wxFont& font);

    wxSize GetScrollButtonMinimumSize(
                        wxDC& dc,
                        wxWindow* wnd,
                        long style);

    void DrawScrollButton(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        long style);

    wxSize GetPanelSize(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxSize client_size,
                        wxPoint* client_offset);

    wxSize GetPanelClientSize(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxSize size,
                        wxPoint* client_offset);

    wxRect GetPanelExtButtonArea(
                        wxDC& dc,
                        const wxRibbonPanel* wnd,
                        wxRect rect);

    void DrawTabCtrlBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    int GetTabCtrlHeight(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRibbonPageTabInfoArray& pages);

    void GetBarTabWidth(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxString& label,
                        const wxBitmap& bitmap,
                        int* ideal,
                        int* small_begin_need_separator,
                        int* small_must_have_separator,
                        int* minimum);

    void DrawTab(wxDC& dc,
                 wxWindow* wnd,
                 const wxRibbonPageTabInfo& tab);

    void DrawTabSeparator(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        double visibility);

    void DrawPageBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    void DrawPanelBackground(
                        wxDC& dc,
                        wxRibbonPanel* wnd,
                        const wxRect& rect);

    void DrawMinimisedPanel(
                        wxDC& dc,
                        wxRibbonPanel* wnd,
                        const wxRect& rect,
                        wxBitmap& bitmap);

    void DrawGalleryBackground(
                        wxDC& dc,
                        wxRibbonGallery* wnd,
                        const wxRect& rect);

    void DrawGalleryItemBackground(
                        wxDC& dc,
                        wxRibbonGallery* wnd,
                        const wxRect& rect,
                        wxRibbonGalleryItem* item);

    void DrawButtonBarBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    void DrawButtonBarButton(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect,
                        wxRibbonButtonKind kind,
                        long state,
                        const wxString& label,
                        const wxBitmap& bitmap_large,
                        const wxBitmap& bitmap_small);

    void DrawToolBarBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    void DrawToolGroupBackground(
                        wxDC& dc,
                        wxWindow* wnd,
                        const wxRect& rect);

    void DrawTool(
                wxDC& dc,
                wxWindow* wnd,
                const wxRect& rect,
                const wxBitmap& bitmap,
                wxRibbonButtonKind kind,
                long state);

};
