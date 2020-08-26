/////////////////////////////////////////////////////////////////////////////
// Name:        bookctrl.h
// Purpose:     interface of wxBookCtrlBase
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Bit flags returned by wxBookCtrl::HitTest().

    Only one of wxBK_HITTEST_ONICON, wxBK_HITTEST_ONLABEL, wxBK_HITTEST_ONITEM
    bits is set if point is over a tab.
    Notice that wxOSX currently only returns wxBK_HITTEST_ONLABEL or
    wxBK_HITTEST_NOWHERE and never the other values, so you should only test
    for these two in the code that should be portable under macOS.
 */
enum
{
    /// No tab at the specified point.
    wxBK_HITTEST_NOWHERE = 1,

    /// The point is over an icon.
    wxBK_HITTEST_ONICON  = 2,

    /// The point is over a tab label.
    wxBK_HITTEST_ONLABEL = 4,

    /// The point if over a tab item but not over its icon or label.
    wxBK_HITTEST_ONITEM  = 16,

    /// The point is over the page area.
    wxBK_HITTEST_ONPAGE  = 8
};

/**
   wxBookCtrl flags (common for wxNotebook, wxListbook, wxChoicebook, wxTreebook)
*/
#define wxBK_DEFAULT          0x0000
#define wxBK_TOP              0x0010
#define wxBK_BOTTOM           0x0020
#define wxBK_LEFT             0x0040
#define wxBK_RIGHT            0x0080
#define wxBK_ALIGN_MASK       (wxBK_TOP | wxBK_BOTTOM | wxBK_LEFT | wxBK_RIGHT)


/**
    @class wxBookCtrlBase

    A book control is a convenient way of displaying multiple pages of information,
    displayed one page at a time. wxWidgets has five variants of this control:

    @li wxChoicebook: controlled by a wxChoice
    @li wxListbook: controlled by a wxListCtrl
    @li wxNotebook: uses a row of tabs
    @li wxTreebook: controlled by a wxTreeCtrl
    @li wxToolbook: controlled by a wxToolBar

    This abstract class is the parent of all these book controls, and provides
    their basic interface.
    This is a pure virtual class so you cannot allocate it directly.

    @library{wxcore}
    @category{bookctrl}

    @see @ref overview_bookctrl
*/
class wxBookCtrlBase : public wxControl, public wxWithImages
{
public:
    enum
    {
        /// Symbolic constant indicating that no image should be used.
        NO_IMAGE = -1
    };

    /**
        Default ctor.
    */
    wxBookCtrlBase();

    /**
        Constructs the book control with the given parameters.
        See Create() for two-step construction.
    */
    wxBookCtrlBase(wxWindow *parent,
                   wxWindowID winid,
                   const wxPoint& pos = wxDefaultPosition,
                   const wxSize& size = wxDefaultSize,
                   long style = 0,
                   const wxString& name = wxEmptyString);

    /**
        Constructs the book control with the given parameters.
    */
    bool Create(wxWindow *parent,
                wxWindowID winid,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxString& name = wxEmptyString);


    /**
        @name Image list functions

        Each page may have an attached image.
        The functions of this group manipulate that image.
    */
    //@{


    /**
        Returns the image index for the given page.
    */
    virtual int GetPageImage(size_t nPage) const = 0;

    /**
        Sets the image index for the given page. @a image is an index into
        the image list which was set with SetImageList().
    */
    virtual bool SetPageImage(size_t page, int image) = 0;

    //@}



    /**
        @name Page text functions

        Each page has a text string attached.
        The functions of this group manipulate that text.
    */
    //@{

    /**
        Returns the string for the given page.
    */
    virtual wxString GetPageText(size_t nPage) const = 0;

    /**
        Sets the text for the given page.
    */
    virtual bool SetPageText(size_t page, const wxString& text) = 0;
    //@}



    /**
        @name Selection functions

        The functions of this group manipulate the selection.
    */
    //@{

    /**
        Returns the currently selected page, or @c wxNOT_FOUND if none was selected.

        Note that this method may return either the previously or newly
        selected page when called from the @c EVT_BOOKCTRL_PAGE_CHANGED handler
        depending on the platform and so wxBookCtrlEvent::GetSelection should be
        used instead in this case.
    */
    virtual int GetSelection() const = 0;

    /**
        Returns the currently selected page or @NULL.
    */
    wxWindow* GetCurrentPage() const;

    /**
        Sets the selection to the given page, returning the previous selection.

        Notice that the call to this function generates the page changing
        events, use the ChangeSelection() function if you don't want these
        events to be generated.

        @see GetSelection()
    */
    virtual int SetSelection(size_t page) = 0;

    /**
        Cycles through the tabs.
        The call to this function generates the page changing events.
    */
    void AdvanceSelection(bool forward = true);

    /**
        Changes the selection to the given page, returning the previous selection.

        This function behaves as SetSelection() but does @em not generate the
        page changing events.

        See @ref overview_events_prog for more information.
    */
    virtual int ChangeSelection(size_t page) = 0;

    /**
        Returns the index of the specified tab window or @c wxNOT_FOUND
        if not found.

        @param page One of the control pages.
        @return The zero-based tab index or @c wxNOT_FOUND if not found.

        @since 2.9.5
    */
    int FindPage(const wxWindow* page) const;

    //@}



    /**
        Sets the width and height of the pages.

        @note This method is currently not implemented for wxGTK.
    */
    virtual void SetPageSize(const wxSize& size);

    /**
        Returns the index of the tab at the specified position or @c wxNOT_FOUND
        if none. If @a flags parameter is non-@NULL, the position of the point
        inside the tab is returned as well.

        @param pt
            Specifies the point for the hit test.
        @param flags
            Return more details about the point, see returned value is a
            combination of ::wxBK_HITTEST_NOWHERE, ::wxBK_HITTEST_ONICON,
            ::wxBK_HITTEST_ONLABEL, ::wxBK_HITTEST_ONITEM,
            ::wxBK_HITTEST_ONPAGE.

        @return Returns the zero-based tab index or @c wxNOT_FOUND if there is no
                tab at the specified position.
    */
    virtual int HitTest(const wxPoint& pt, long* flags = NULL) const;



    /**
        @name Page management functions

        Functions for adding/removing pages from this control.
    */
    //@{

    /**
        Adds a new page.

        The page must have the book control itself as the parent and must not
        have been added to this control previously.

        The call to this function will generate the page changing and page
        changed events if @a select is true, but not when inserting the very
        first page (as there is no previous page selection to switch from in
        this case and so it wouldn't make sense to e.g. veto such event).

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
    */
    virtual bool AddPage(wxWindow* page, const wxString& text,
                         bool select = false, int imageId = NO_IMAGE);

    /**
        Deletes all pages.
    */
    virtual bool DeleteAllPages();

    /**
        Deletes the specified page, and the associated window.

        The call to this function generates the page changing events when
        deleting the currently selected page or a page preceding it in the
        index order, but it does @e not send any events when deleting the last
        page: while in this case the selection also changes, it becomes invalid
        and for compatibility reasons the control never generates events with
        the invalid selection index.
    */
    virtual bool DeletePage(size_t page);

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
    */
    virtual bool InsertPage(size_t index,
                            wxWindow* page,
                            const wxString& text,
                            bool select = false,
                            int imageId = NO_IMAGE) = 0;

    /**
        Deletes the specified page, without deleting the associated window.

        See DeletePage() for a note about the events generated by this
        function.
    */
    virtual bool RemovePage(size_t page);

    /**
        Returns the number of pages in the control.
    */
    virtual size_t GetPageCount() const;

    /**
        Returns the window at the given page position.
    */
    wxWindow* GetPage(size_t page) const;

    //@}


/*
    other functions which may be worth documenting:

    // geometry
    // --------

    // calculate the size of the control from the size of its page
    virtual wxSize CalcSizeFromPage(const wxSize& sizePage) const = 0;

    // get/set size of area between book control area and page area
    unsigned int GetInternalBorder() const { return m_internalBorder; }
    void SetInternalBorder(unsigned int border) { m_internalBorder = border; }

    // Sets/gets the margin around the controller
    void SetControlMargin(int margin) { m_controlMargin = margin; }
    int GetControlMargin() const { return m_controlMargin; }

    // returns true if we have wxBK_TOP or wxBK_BOTTOM style
    bool IsVertical() const { return HasFlag(wxBK_BOTTOM | wxBK_TOP); }

    // set/get option to shrink to fit current page
    void SetFitToCurrentPage(bool fit) { m_fitToCurrentPage = fit; }
    bool GetFitToCurrentPage() const { return m_fitToCurrentPage; }

    // returns the sizer containing the control, if any
    wxSizer* GetControlSizer() const { return m_controlSizer; }

    // we do have multiple pages
    virtual bool HasMultiplePages() const { return true; }

    // we don't want focus for ourselves
    virtual bool AcceptsFocus() const { return false; }

    // returns true if the platform should explicitly apply a theme border
    virtual bool CanApplyThemeBorder() const { return false; }
*/
};

/**
    wxBookCtrl is defined to one of the 'real' book controls.

    See @ref overview_bookctrl for more info.
*/
#define wxBookCtrl      TheBestBookCtrlForTheCurrentPlatform


/**
    @class wxBookCtrlEvent

    This class represents the events generated by book controls (wxNotebook,
    wxListbook, wxChoicebook, wxTreebook, wxAuiNotebook).

    The PAGE_CHANGING events are sent before the current page is changed.
    It allows the program to examine the current page (which can be retrieved
    with wxBookCtrlEvent::GetOldSelection) and to veto the page change by calling
    wxNotifyEvent::Veto if, for example, the current values in the controls
    of the old page are invalid.

    The PAGE_CHANGED events are sent after the page has been changed and the
    program cannot veto it any more, it just informs it about the page change.

    To summarize, if the program is interested in validating the page values
    before allowing the user to change it, it should process the PAGE_CHANGING
    event, otherwise PAGE_CHANGED is probably enough. In any case, it is
    probably unnecessary to process both events at once.

    @library{wxcore}
    @category{events,bookctrl}

    @see wxNotebook, wxListbook, wxChoicebook, wxTreebook, wxToolbook, wxAuiNotebook
*/
class wxBookCtrlEvent : public wxNotifyEvent
{
public:
    /**
        Constructor (used internally by wxWidgets only).
    */
    wxBookCtrlEvent(wxEventType eventType = wxEVT_NULL, int id = 0,
                    int sel = wxNOT_FOUND, int oldSel = wxNOT_FOUND);

    /**
        Returns the page that was selected before the change, @c wxNOT_FOUND if
        none was selected.
    */
    int GetOldSelection() const;

    /**
        Returns the currently selected page, or @c wxNOT_FOUND if none was
        selected.

        @note under Windows, GetSelection() will return the same value as
              GetOldSelection() when called from the @c EVT_BOOKCTRL_PAGE_CHANGING
              handler and not the page which is going to be selected.
    */
    int GetSelection() const;

    /**
        Sets the id of the page selected before the change.
    */
    void SetOldSelection(int page);

    /**
        Sets the selection member variable.
    */
    void SetSelection(int page);
};

