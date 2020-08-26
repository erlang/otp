/////////////////////////////////////////////////////////////////////////////
// Name:        aui/auibook.h
// Purpose:     interface of wxAuiNotebook
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

enum wxAuiNotebookOption
{
    wxAUI_NB_TOP                 = 1 << 0,
    wxAUI_NB_LEFT                = 1 << 1,  // not implemented yet
    wxAUI_NB_RIGHT               = 1 << 2,  // not implemented yet
    wxAUI_NB_BOTTOM              = 1 << 3,
    wxAUI_NB_TAB_SPLIT           = 1 << 4,
    wxAUI_NB_TAB_MOVE            = 1 << 5,
    wxAUI_NB_TAB_EXTERNAL_MOVE   = 1 << 6,
    wxAUI_NB_TAB_FIXED_WIDTH     = 1 << 7,
    wxAUI_NB_SCROLL_BUTTONS      = 1 << 8,
    wxAUI_NB_WINDOWLIST_BUTTON   = 1 << 9,
    wxAUI_NB_CLOSE_BUTTON        = 1 << 10,
    wxAUI_NB_CLOSE_ON_ACTIVE_TAB = 1 << 11,
    wxAUI_NB_CLOSE_ON_ALL_TABS   = 1 << 12,
    wxAUI_NB_MIDDLE_CLICK_CLOSE  = 1 << 13,

    wxAUI_NB_DEFAULT_STYLE = wxAUI_NB_TOP |
                             wxAUI_NB_TAB_SPLIT |
                             wxAUI_NB_TAB_MOVE |
                             wxAUI_NB_SCROLL_BUTTONS |
                             wxAUI_NB_CLOSE_ON_ACTIVE_TAB |
                             wxAUI_NB_MIDDLE_CLICK_CLOSE
};


/**
    @class wxAuiNotebook

    wxAuiNotebook is part of the wxAUI class framework, which represents a
    notebook control, managing multiple windows with associated tabs.

    See also @ref overview_aui.

    wxAuiNotebook is a notebook control which implements many features common in
    applications with dockable panes.
    Specifically, wxAuiNotebook implements functionality which allows the user to
    rearrange tab order via drag-and-drop, split the tab window into many different
    splitter configurations, and toggle through different themes to customize
    the control's look and feel.

    The default theme that is used is wxAuiDefaultTabArt, which provides a modern,
    glossy look and feel.
    The theme can be changed by calling wxAuiNotebook::SetArtProvider.

    @beginStyleTable
    @style{wxAUI_NB_DEFAULT_STYLE}
           Defined as wxAUI_NB_TOP | wxAUI_NB_TAB_SPLIT | wxAUI_NB_TAB_MOVE |
           wxAUI_NB_SCROLL_BUTTONS | wxAUI_NB_CLOSE_ON_ACTIVE_TAB |
           wxAUI_NB_MIDDLE_CLICK_CLOSE.
    @style{wxAUI_NB_TAB_SPLIT}
           Allows the tab control to be split by dragging a tab.
    @style{wxAUI_NB_TAB_MOVE}
           Allows a tab to be moved horizontally by dragging.
    @style{wxAUI_NB_TAB_EXTERNAL_MOVE}
           Allows a tab to be moved to another tab control.
    @style{wxAUI_NB_TAB_FIXED_WIDTH}
           With this style, all tabs have the same width.
    @style{wxAUI_NB_SCROLL_BUTTONS}
           With this style, left and right scroll buttons are displayed.
    @style{wxAUI_NB_WINDOWLIST_BUTTON}
           With this style, a drop-down list of windows is available.
    @style{wxAUI_NB_CLOSE_BUTTON}
           With this style, a close button is available on the tab bar.
    @style{wxAUI_NB_CLOSE_ON_ACTIVE_TAB}
           With this style, the close button is visible on the active tab.
    @style{wxAUI_NB_CLOSE_ON_ALL_TABS}
           With this style, the close button is visible on all tabs.
    @style{wxAUI_NB_MIDDLE_CLICK_CLOSE}
           With this style, middle click on a tab closes the tab.
    @style{wxAUI_NB_TOP}
           With this style, tabs are drawn along the top of the notebook.
    @style{wxAUI_NB_BOTTOM}
           With this style, tabs are drawn along the bottom of the notebook.
    @endStyleTable

    @beginEventEmissionTable{wxAuiNotebookEvent}
    @event{EVT_AUINOTEBOOK_PAGE_CLOSE(id, func)}
        A page is about to be closed. Processes a @c wxEVT_AUINOTEBOOK_PAGE_CLOSE event.
    @event{EVT_AUINOTEBOOK_PAGE_CLOSED(winid, fn)}
        A page has been closed. Processes a @c wxEVT_AUINOTEBOOK_PAGE_CLOSED event.
    @event{EVT_AUINOTEBOOK_PAGE_CHANGED(id, func)}
        The page selection was changed. Processes a @c wxEVT_AUINOTEBOOK_PAGE_CHANGED event.
    @event{EVT_AUINOTEBOOK_PAGE_CHANGING(id, func)}
        The page selection is about to be changed. Processes a  @c wxEVT_AUINOTEBOOK_PAGE_CHANGING event. This event can be vetoed.
    @event{EVT_AUINOTEBOOK_BUTTON(id, func)}
        The window list button has been pressed. Processes a @c wxEVT_AUINOTEBOOK_BUTTON event.
    @event{EVT_AUINOTEBOOK_BEGIN_DRAG(id, func)}
        Dragging is about to begin. Processes a @c wxEVT_AUINOTEBOOK_BEGIN_DRAG event.
    @event{EVT_AUINOTEBOOK_END_DRAG(id, func)}
        Dragging has ended. Processes a @c wxEVT_AUINOTEBOOK_END_DRAG event.
    @event{EVT_AUINOTEBOOK_DRAG_MOTION(id, func)}
        Emitted during a drag and drop operation. Processes a @c wxEVT_AUINOTEBOOK_DRAG_MOTION event.
    @event{EVT_AUINOTEBOOK_ALLOW_DND(id, func)}
        Whether to allow a tab to be dropped. Processes a @c wxEVT_AUINOTEBOOK_ALLOW_DND event. This event must be specially allowed.
    @event{EVT_AUINOTEBOOK_DRAG_DONE(winid, fn)}
        Notify that the tab has been dragged. Processes a @c wxEVT_AUINOTEBOOK_DRAG_DONE event.
    @event{EVT_AUINOTEBOOK_TAB_MIDDLE_DOWN(winid, fn)}
        The middle mouse button is pressed on a tab. Processes a @c wxEVT_AUINOTEBOOK_TAB_MIDDLE_DOWN event.
    @event{EVT_AUINOTEBOOK_TAB_MIDDLE_UP(winid, fn)}
        The middle mouse button is released on a tab. Processes a @c wxEVT_AUINOTEBOOK_TAB_MIDDLE_UP event.
    @event{EVT_AUINOTEBOOK_TAB_RIGHT_DOWN(winid, fn)}
        The right mouse button is pressed on a tab. Processes a @c wxEVT_AUINOTEBOOK_TAB_RIGHT_DOWN event.
    @event{EVT_AUINOTEBOOK_TAB_RIGHT_UP(winid, fn)}
        The right mouse button is released on a tab. Processes a @c wxEVT_AUINOTEBOOK_TAB_RIGHT_UP event.
    @event{EVT_AUINOTEBOOK_BG_DCLICK(winid, fn)}
        Double clicked on the tabs background area. Processes a @c wxEVT_AUINOTEBOOK_BG_DCLICK event.
    @endEventTable

    @library{wxaui}
    @category{aui}
*/
class wxAuiNotebook : public wxBookCtrlBase
{
public:
    /**
        Default ctor.
    */
    wxAuiNotebook();

    /**
        Constructor. Creates a wxAuiNotebok control.
    */
    wxAuiNotebook(wxWindow* parent, wxWindowID id = wxID_ANY,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = wxAUI_NB_DEFAULT_STYLE);

    /**
        Adds a page.
        If the @a select parameter is @true, calling this will generate a page change event.
    */
    bool AddPage(wxWindow* page, const wxString& caption,
                 bool select = false,
                 const wxBitmap& bitmap = wxNullBitmap);

    /**
        Adds a new page.

        The page must have the book control itself as the parent and must not
        have been added to this control previously.

        The call to this function may generate the page changing events.

        @param page
            Specifies the new page.
        @param text
            Specifies the text for the new page.
        @param select
            Specifies whether the page should be selected.
        @param imageId
            Specifies the optional image index for the new page.

        @return @true if successful, @false otherwise.

        @remarks Do not delete the page, it will be deleted by the book control.

        @see InsertPage()
        @since 2.9.3
    */
    virtual bool AddPage(wxWindow *page, const wxString &text, bool select, int imageId);

    /**
        Sets the selection to the next or previous page.
    */
    void AdvanceSelection(bool forward = true);

    /**
        Changes the selection for the given page, returning the previous selection.

        This function behaves as SetSelection() but does @em not generate the
        page changing events.

        See @ref overview_events_prog for more information.
        @since 2.9.3
    */
    virtual int ChangeSelection(size_t n);

    /**
        Creates the notebook window.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0);

    /**
        Deletes all pages.
        @since 2.9.3
    */
    virtual bool DeleteAllPages();

    /**
        Deletes a page at the given index.
        Calling this method will generate a page change event.
    */
    bool DeletePage(size_t page);

    /**
        Returns the associated art provider.
    */
    wxAuiTabArt* GetArtProvider() const;

    /**
        Returns the currently selected page or @NULL.
        @since 2.9.3
    */
    wxWindow* GetCurrentPage () const;

    /**
        Returns the desired height of the notebook for the given page height.
        Use this to fit the notebook to a given page size.
    */
    int GetHeightForPageHeight(int pageHeight);

    /**
        Returns the page specified by the given index.
    */
    wxWindow* GetPage(size_t page_idx) const;

    /**
        Returns the tab bitmap for the page.
    */
    wxBitmap GetPageBitmap(size_t page) const;

    /**
        Returns the number of pages in the notebook.
    */
    size_t GetPageCount() const;

    /**
        Returns the page index for the specified window.
        If the window is not found in the notebook, wxNOT_FOUND is returned.
    */
    int GetPageIndex(wxWindow* page_wnd) const;

    /**
        Returns the tab label for the page.
    */
    wxString GetPageText(size_t page) const;

    /**
        Returns the tooltip for the tab label of the page.

        @since 2.9.4
    */
    wxString GetPageToolTip(size_t pageIdx) const;

    /**
        Returns the currently selected page.
    */
    int GetSelection() const;

    /**
        Returns the height of the tab control.
    */
    int GetTabCtrlHeight() const;

    /**
        InsertPage() is similar to AddPage, but allows the ability to specify the
        insert location.
        If the @a select parameter is @true, calling this will generate a page change
        event.
    */
    bool InsertPage(size_t page_idx, wxWindow* page,
                    const wxString& caption,
                    bool select = false,
                    const wxBitmap& bitmap = wxNullBitmap);

    /**
        Inserts a new page at the specified position.

        @param index
            Specifies the position for the new page.
        @param page
            Specifies the new page.
        @param text
            Specifies the text for the new page.
        @param select
            Specifies whether the page should be selected.
        @param imageId
            Specifies the optional image index for the new page.

        @return @true if successful, @false otherwise.

        @remarks Do not delete the page, it will be deleted by the book control.

        @see AddPage()
        @since 2.9.3
    */
    virtual bool InsertPage(size_t index, wxWindow *page, const wxString &text,
                            bool select, int imageId);

    /**
        Removes a page, without deleting the window pointer.
    */
    bool RemovePage(size_t page);

    /**
        Sets the art provider to be used by the notebook.
    */
    void SetArtProvider(wxAuiTabArt* art);

    /**
        Sets the font for drawing the tab labels, using a bold version of the font for
        selected tab labels.
    */
    virtual bool SetFont(const wxFont& font);

    /**
        Sets the font for measuring tab labels.
    */
    void SetMeasuringFont(const wxFont& font);

    /**
        Sets the font for drawing unselected tab labels.
    */
    void SetNormalFont(const wxFont& font);

    /**
        Sets the bitmap for the page.  To remove a bitmap from the tab caption, pass
        wxNullBitmap.
    */
    bool SetPageBitmap(size_t page, const wxBitmap& bitmap);

    /**
        Sets the image index for the given page. @a image is an index into
        the image list which was set with SetImageList().
        @since 2.9.3
    */
    virtual bool SetPageImage(size_t n, int imageId);

    /**
        Sets the tab label for the page.
    */
    bool SetPageText(size_t page, const wxString& text);

    /**
        Sets the tooltip displayed when hovering over the tab label of the page.

        @return
            @true if tooltip was updated, @false if it failed, e.g. because the
            page index is invalid.

        @since 2.9.4
    */
    bool SetPageToolTip(size_t page, const wxString& text);

    /**
        Sets the font for drawing selected tab labels.
    */
    void SetSelectedFont(const wxFont& font);

    /**
        Sets the page selection.  Calling this method will generate a page change event.
    */
    int SetSelection(size_t new_page);

    /**
        Sets the tab height. By default, the tab control height is calculated
        by measuring the text height and bitmap sizes on the tab captions. Calling this
        method will override that calculation and set the tab control to the specified
        height parameter. A call to this method will override any call to
        SetUniformBitmapSize().

        Specifying -1 as the height will return the control to its default auto-sizing
        behaviour.
    */
    virtual void SetTabCtrlHeight(int height);

    /**
        Ensure that all tabs have the same height, even if some of them don't
        have bitmaps.

        Passing ::wxDefaultSize as @a size undoes the effect of a previous call
        to this function and instructs the control to use dynamic tab height.
     */
    virtual void SetUniformBitmapSize(const wxSize& size);

    /**
        Split performs a split operation programmatically. The argument @a page
        indicates the page that will be split off.  This page will also become the
        active page after the split.

        The @a direction argument specifies where the pane should go, it should be one
        of the following: wxTOP, wxBOTTOM, wxLEFT, or wxRIGHT.
    */
    virtual void Split(size_t page, int direction);

    /**
        Shows the window menu for the active tab control associated with this notebook,
        and returns @true if a selection was made.
    */
    bool ShowWindowMenu();


    /**
        Returns the image index for the given page.
    */
    virtual int GetPageImage(size_t nPage) const;

    /**
        Returns tab control based on point coordinates inside the tab frame.

        @since 3.1.4
    */
    wxAuiTabCtrl* GetTabCtrlFromPoint(const wxPoint& pt);

    /**
        Returns active tab control for this notebook.

        @since 3.1.4
    */
    wxAuiTabCtrl* GetActiveTabCtrl();

    /**
        Finds tab control associated with a given window and its tab index.

        @return @true when the tab control is found, @false otherwise.

        @since 3.1.4
    */
    bool FindTab(wxWindow* page, wxAuiTabCtrl** ctrl, int* idx);
};


/**
    @class wxAuiNotebookPage

    A simple class which holds information about the notebook's pages and their state.

    @library{wxaui}
    @category{aui}
*/
class wxAuiNotebookPage
{
public:
    wxWindow* window;     // page's associated window
    wxString caption;     // caption displayed on the tab
    wxString tooltip;     // tooltip displayed when hovering over tab title
    wxBitmap bitmap;      // tab's bitmap
    wxRect rect;          // tab's hit rectangle
    bool active;          // true if the page is currently active
};


/**
    @class wxAuiTabContainerButton

    A simple class which holds information about wxAuiNotebook tab buttons and their state.

    @library{wxaui}
    @category{aui}
*/
class wxAuiTabContainerButton
{
public:
    /// button's id
    int id;
    /// current state (normal, hover, pressed, etc.)
    int curState;
    /// buttons location (wxLEFT, wxRIGHT, or wxCENTER)
    int location;
    /// button's hover bitmap
    wxBitmap bitmap;
    /// button's disabled bitmap
    wxBitmap disBitmap;
    /// button's hit rectangle
    wxRect rect;
};


/**
    @class wxAuiTabContainer

    wxAuiTabContainer is a class which contains information about each tab.
    It also can render an entire tab control to a specified DC.
    It's not a window class itself, because this code will be used by
    the wxAuiNotebook, where it is disadvantageous to have separate
    windows for each tab control in the case of "docked tabs".

    A derived class, wxAuiTabCtrl, is an actual wxWindow - derived window
    which can be used as a tab control in the normal sense.

    @library{wxaui}
    @category{aui}
*/
class wxAuiTabContainer
{
public:

    /**
        Default ctor.
    */
    wxAuiTabContainer();

    /**
        Default dtor.
    */
    virtual ~wxAuiTabContainer();

    void SetArtProvider(wxAuiTabArt* art);
    wxAuiTabArt* GetArtProvider() const;

    void SetFlags(unsigned int flags);
    unsigned int GetFlags() const;

    bool AddPage(wxWindow* page, const wxAuiNotebookPage& info);
    bool InsertPage(wxWindow* page, const wxAuiNotebookPage& info, size_t idx);
    bool MovePage(wxWindow* page, size_t newIdx);
    bool RemovePage(wxWindow* page);
    bool SetActivePage(wxWindow* page);
    bool SetActivePage(size_t page);
    void SetNoneActive();
    int GetActivePage() const;
    bool TabHitTest(int x, int y, wxWindow** hit) const;
    bool ButtonHitTest(int x, int y, wxAuiTabContainerButton** hit) const;
    wxWindow* GetWindowFromIdx(size_t idx) const;
    int GetIdxFromWindow(wxWindow* page) const;
    size_t GetPageCount() const;
    wxAuiNotebookPage& GetPage(size_t idx);
    const wxAuiNotebookPage& GetPage(size_t idx) const;
    wxAuiNotebookPageArray& GetPages();
    void SetNormalFont(const wxFont& normalFont);
    void SetSelectedFont(const wxFont& selectedFont);
    void SetMeasuringFont(const wxFont& measuringFont);
    void SetColour(const wxColour& colour);
    void SetActiveColour(const wxColour& colour);
    void DoShowHide();
    void SetRect(const wxRect& rect);

    void RemoveButton(int id);
    void AddButton(int id,
                   int location,
                   const wxBitmap& normalBitmap = wxNullBitmap,
                   const wxBitmap& disabledBitmap = wxNullBitmap);

    size_t GetTabOffset() const;
    void SetTabOffset(size_t offset);

    // Is the tab visible?
    bool IsTabVisible(int tabPage, int tabOffset, wxDC* dc, wxWindow* wnd);

    // Make the tab visible if it wasn't already
    void MakeTabVisible(int tabPage, wxWindow* win);

protected:

    virtual void Render(wxDC* dc, wxWindow* wnd);

protected:

    wxAuiTabArt* m_art;
    wxAuiNotebookPageArray m_pages;
    wxAuiTabContainerButtonArray m_buttons;
    wxAuiTabContainerButtonArray m_tabCloseButtons;
    wxRect m_rect;
    size_t m_tabOffset;
    unsigned int m_flags;
};



/**
    @class wxAuiTabArt

    Tab art provider defines all the drawing functions used by wxAuiNotebook.

    This allows the wxAuiNotebook to have a pluggable look-and-feel.

    By default, a wxAuiNotebook uses an instance of this class called
    wxAuiDefaultTabArt which provides bitmap art and a colour scheme that is
    adapted to the major platforms' look. You can either derive from that class
    to alter its behaviour or write a completely new tab art class.

    Another example of creating a new wxAuiNotebook tab bar is wxAuiSimpleTabArt.

    Call wxAuiNotebook::SetArtProvider() to make use of this new tab art.

    @library{wxaui}
    @category{aui}
*/
class wxAuiTabArt
{
public:
    /**
        Constructor.
    */
    wxAuiTabArt();

    /**
        Clones the art object.
    */
    virtual wxAuiTabArt* Clone() = 0;

    /**
        Draws a background on the given area.
    */
    virtual void DrawBackground(wxDC& dc, wxWindow* wnd, const wxRect& rect) = 0;

    /**
        Draws a button.
    */
    virtual void DrawButton(wxDC& dc, wxWindow* wnd, const wxRect& in_rect,
                            int bitmap_id, int button_state, int orientation,
                            wxRect* out_rect) = 0;

    /**
        Draws a tab.
    */
    virtual void DrawTab(wxDC& dc, wxWindow* wnd, const wxAuiNotebookPage& page,
                         const wxRect& rect, int close_button_state,
                         wxRect* out_tab_rect, wxRect* out_button_rect, int* x_extent) = 0;

    /**
        Returns the tab control size.
    */
    virtual int GetBestTabCtrlSize(wxWindow*, const wxAuiNotebookPageArray&, const wxSize&) = 0;

    /**
        Returns the indent size.
    */
    virtual int GetIndentSize() = 0;

    /**
        Returns the tab size for the given caption, bitmap and state.
    */
    virtual wxSize GetTabSize(wxDC& dc, wxWindow* wnd, const wxString& caption,
                              const wxBitmap& bitmap, bool active,
                              int close_button_state, int* x_extent) = 0;

    /**
        Sets flags.
    */
    virtual void SetFlags(unsigned int flags) = 0;

    /**
        Sets the font used for calculating measurements.
    */
    virtual void SetMeasuringFont(const wxFont& font) = 0;

    /**
        Sets the normal font for drawing labels.
    */
    virtual void SetNormalFont(const wxFont& font) = 0;

    /**
        Sets the font for drawing text for selected UI elements.
    */
    virtual void SetSelectedFont(const wxFont& font) = 0;

    /**
        Sets the colour of the inactive tabs.

        @since 2.9.2
    */
    virtual void SetColour(const wxColour& colour) = 0;

    /**
        Sets the colour of the selected tab.

        @since 2.9.2
    */
    virtual void SetActiveColour(const wxColour& colour) = 0;

    /**
        Sets sizing information.
    */
    virtual void SetSizingInfo(const wxSize& tab_ctrl_size, size_t tab_count) = 0;
};

/**
    @class wxAuiNotebookEvent

    This class is used by the events generated by wxAuiNotebook.

    @beginEventEmissionTable{wxAuiNotebookEvent}
    @event{EVT_AUINOTEBOOK_PAGE_CLOSE(id, func)}
        A page is about to be closed. Processes a @c wxEVT_AUINOTEBOOK_PAGE_CLOSE event.
    @event{EVT_AUINOTEBOOK_PAGE_CLOSED(winid, fn)}
        A page has been closed. Processes a @c wxEVT_AUINOTEBOOK_PAGE_CLOSED event.
    @event{EVT_AUINOTEBOOK_PAGE_CHANGED(id, func)}
        The page selection was changed. Processes a @c wxEVT_AUINOTEBOOK_PAGE_CHANGED event.
    @event{EVT_AUINOTEBOOK_PAGE_CHANGING(id, func)}
        The page selection is about to be changed. Processes a  @c wxEVT_AUINOTEBOOK_PAGE_CHANGING event. This event can be vetoed.
    @event{EVT_AUINOTEBOOK_BUTTON(id, func)}
        The window list button has been pressed. Processes a @c wxEVT_AUINOTEBOOK_BUTTON event.
    @event{EVT_AUINOTEBOOK_BEGIN_DRAG(id, func)}
        Dragging is about to begin. Processes a @c wxEVT_AUINOTEBOOK_BEGIN_DRAG event.
    @event{EVT_AUINOTEBOOK_END_DRAG(id, func)}
        Dragging has ended. Processes a @c wxEVT_AUINOTEBOOK_END_DRAG event.
    @event{EVT_AUINOTEBOOK_DRAG_MOTION(id, func)}
        Emitted during a drag and drop operation. Processes a @c wxEVT_AUINOTEBOOK_DRAG_MOTION event.
    @event{EVT_AUINOTEBOOK_ALLOW_DND(id, func)}
        Whether to allow a tab to be dropped. Processes a @c wxEVT_AUINOTEBOOK_ALLOW_DND event. This event must be specially allowed.
    @event{EVT_AUINOTEBOOK_DRAG_DONE(winid, fn)}
        Notify that the tab has been dragged. Processes a @c wxEVT_AUINOTEBOOK_DRAG_DONE event.
    @event{EVT_AUINOTEBOOK_TAB_MIDDLE_DOWN(winid, fn)}
        The middle mouse button is pressed on a tab. Processes a @c wxEVT_AUINOTEBOOK_TAB_MIDDLE_DOWN event.
    @event{EVT_AUINOTEBOOK_TAB_MIDDLE_UP(winid, fn)}
        The middle mouse button is released on a tab. Processes a @c wxEVT_AUINOTEBOOK_TAB_MIDDLE_UP event.
    @event{EVT_AUINOTEBOOK_TAB_RIGHT_DOWN(winid, fn)}
        The right mouse button is pressed on a tab. Processes a @c wxEVT_AUINOTEBOOK_TAB_RIGHT_DOWN event.
    @event{EVT_AUINOTEBOOK_TAB_RIGHT_UP(winid, fn)}
        The right mouse button is released on a tab. Processes a @c wxEVT_AUINOTEBOOK_TAB_RIGHT_UP event.
    @event{EVT_AUINOTEBOOK_BG_DCLICK(winid, fn)}
        Double clicked on the tabs background area. Processes a @c wxEVT_AUINOTEBOOK_BG_DCLICK event.
    @endEventTable

    @library{wxaui}
    @category{events,bookctrl}

    @see wxAuiNotebook, wxBookCtrlEvent
*/
class wxAuiNotebookEvent : public wxBookCtrlEvent
{
public:
    /**
        Constructor.
    */
    wxAuiNotebookEvent(wxEventType command_type = wxEVT_NULL, int win_id = 0);

    wxEvent *Clone();
};

wxEventType wxEVT_AUINOTEBOOK_PAGE_CLOSE;
wxEventType wxEVT_AUINOTEBOOK_PAGE_CHANGED;
wxEventType wxEVT_AUINOTEBOOK_PAGE_CHANGING;
wxEventType wxEVT_AUINOTEBOOK_PAGE_CLOSED;
wxEventType wxEVT_AUINOTEBOOK_BUTTON;
wxEventType wxEVT_AUINOTEBOOK_BEGIN_DRAG;
wxEventType wxEVT_AUINOTEBOOK_END_DRAG;
wxEventType wxEVT_AUINOTEBOOK_DRAG_MOTION;
wxEventType wxEVT_AUINOTEBOOK_ALLOW_DND;
wxEventType wxEVT_AUINOTEBOOK_TAB_MIDDLE_DOWN;
wxEventType wxEVT_AUINOTEBOOK_TAB_MIDDLE_UP;
wxEventType wxEVT_AUINOTEBOOK_TAB_RIGHT_DOWN;
wxEventType wxEVT_AUINOTEBOOK_TAB_RIGHT_UP;
wxEventType wxEVT_AUINOTEBOOK_DRAG_DONE;
wxEventType wxEVT_AUINOTEBOOK_BG_DCLICK;

/**
    Default art provider for wxAuiNotebook.

    @see wxAuiTabArt

    @genericAppearance{auidefaulttabart}

    @library{wxaui}
    @category{aui}
*/

class wxAuiDefaultTabArt : public wxAuiTabArt
{
public:

    wxAuiDefaultTabArt();
    virtual ~wxAuiDefaultTabArt();

    wxAuiTabArt* Clone();
    void SetFlags(unsigned int flags);
    void SetSizingInfo(const wxSize& tabCtrlSize,
                       size_t tabCount);

    void SetNormalFont(const wxFont& font);
    void SetSelectedFont(const wxFont& font);
    void SetMeasuringFont(const wxFont& font);
    void SetColour(const wxColour& colour);
    void SetActiveColour(const wxColour& colour);

    void DrawBackground(
                 wxDC& dc,
                 wxWindow* wnd,
                 const wxRect& rect);

    void DrawTab(wxDC& dc,
                 wxWindow* wnd,
                 const wxAuiNotebookPage& pane,
                 const wxRect& inRect,
                 int closeButtonState,
                 wxRect* outTabRect,
                 wxRect* outButtonRect,
                 int* xExtent);

    void DrawButton(
                 wxDC& dc,
                 wxWindow* wnd,
                 const wxRect& inRect,
                 int bitmapId,
                 int buttonState,
                 int orientation,
                 wxRect* outRect);

    int GetIndentSize();

    wxSize GetTabSize(
                 wxDC& dc,
                 wxWindow* wnd,
                 const wxString& caption,
                 const wxBitmap& bitmap,
                 bool active,
                 int closeButtonState,
                 int* xExtent);

    int ShowDropDown(
                 wxWindow* wnd,
                 const wxAuiNotebookPageArray& items,
                 int activeIdx);

    int GetBestTabCtrlSize(wxWindow* wnd,
                 const wxAuiNotebookPageArray& pages,
                 const wxSize& requiredBmpSize);

protected:
    /**
        The font used for all tabs
     */
    wxFont m_normalFont;
    wxFont m_selectedFont; /// The font used on the selected tab
    wxFont m_measuringFont;
    wxColour m_baseColour;
    wxPen m_baseColourPen;
    wxPen m_borderPen;
    wxBrush m_baseColourBrush;
    wxColour m_activeColour;
    wxBitmap m_activeCloseBmp;
    wxBitmap m_disabledCloseBmp;
    wxBitmap m_activeLeftBmp;
    wxBitmap m_disabledLeftBmp;
    wxBitmap m_activeRightBmp;
    wxBitmap m_disabledRightBmp;
    wxBitmap m_activeWindowListBmp;
    wxBitmap m_disabledWindowListBmp;

    int m_fixedTabWidth;
    int m_tabCtrlHeight;
    unsigned int m_flags;
};


/**
    @class wxAuiSimpleTabArt

    Another standard tab art provider for wxAuiNotebook.

    wxAuiSimpleTabArt is derived from wxAuiTabArt demonstrating how to write a
    completely new tab art class. It can also be used as alternative to
    wxAuiDefaultTabArt.

    @genericAppearance{auisimpletabart}

    @library{wxaui}
    @category{aui}
*/

class wxAuiSimpleTabArt : public wxAuiTabArt
{

public:

    wxAuiSimpleTabArt();
    virtual ~wxAuiSimpleTabArt();

    wxAuiTabArt* Clone();
    void SetFlags(unsigned int flags);

    void SetSizingInfo(const wxSize& tabCtrlSize,
                       size_t tabCount);

    void SetNormalFont(const wxFont& font);
    void SetSelectedFont(const wxFont& font);
    void SetMeasuringFont(const wxFont& font);
    void SetColour(const wxColour& colour);
    void SetActiveColour(const wxColour& colour);

    void DrawBackground(
                 wxDC& dc,
                 wxWindow* wnd,
                 const wxRect& rect);

    void DrawTab(wxDC& dc,
                 wxWindow* wnd,
                 const wxAuiNotebookPage& pane,
                 const wxRect& inRect,
                 int closeButtonState,
                 wxRect* outTabRect,
                 wxRect* outButtonRect,
                 int* xExtent);

    void DrawButton(
                 wxDC& dc,
                 wxWindow* wnd,
                 const wxRect& inRect,
                 int bitmapId,
                 int buttonState,
                 int orientation,
                 wxRect* outRect);

    int GetIndentSize();

    wxSize GetTabSize(
                 wxDC& dc,
                 wxWindow* wnd,
                 const wxString& caption,
                 const wxBitmap& bitmap,
                 bool active,
                 int closeButtonState,
                 int* xExtent);

    int ShowDropDown(
                 wxWindow* wnd,
                 const wxAuiNotebookPageArray& items,
                 int activeIdx);

    int GetBestTabCtrlSize(wxWindow* wnd,
                 const wxAuiNotebookPageArray& pages,
                 const wxSize& requiredBmpSize);

protected:

    wxFont m_normalFont;
    wxFont m_selectedFont;
    wxFont m_measuringFont;
    wxPen m_normalBkPen;
    wxPen m_selectedBkPen;
    wxBrush m_normalBkBrush;
    wxBrush m_selectedBkBrush;
    wxBrush m_bkBrush;
    wxBitmap m_activeCloseBmp;
    wxBitmap m_disabledCloseBmp;
    wxBitmap m_activeLeftBmp;
    wxBitmap m_disabledLeftBmp;
    wxBitmap m_activeRightBmp;
    wxBitmap m_disabledRightBmp;
    wxBitmap m_activeWindowListBmp;
    wxBitmap m_disabledWindowListBmp;

    int m_fixedTabWidth;
    unsigned int m_flags;
};
