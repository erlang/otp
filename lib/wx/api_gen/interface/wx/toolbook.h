/////////////////////////////////////////////////////////////////////////////
// Name:        toolbook.h
// Purpose:     interface of wxToolbook
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#define wxTBK_BUTTONBAR            0x0100
#define wxTBK_HORZ_LAYOUT          0x8000

wxEventType wxEVT_TOOLBOOK_PAGE_CHANGED;
wxEventType wxEVT_TOOLBOOK_PAGE_CHANGING;


/**
    @class wxToolbook

    wxToolbook is a class similar to wxNotebook but which uses a wxToolBar to
    show the labels instead of the tabs.

    There is no documentation for this class yet but its usage is identical to
    wxNotebook (except for the features clearly related to tabs only), so please
    refer to that class documentation for now. You can also use the
    @ref page_samples_notebook to see wxToolbook in action.

    One feature of this class not supported by wxBookCtrlBase is the support
    for disabling some of the pages, see EnablePage().

    @beginStyleTable
    @style{wxTBK_BUTTONBAR}
        Use wxButtonToolBar-based implementation under macOS (ignored under
        other platforms).
    @style{wxTBK_HORZ_LAYOUT}
        Shows the text and the icons alongside, not vertically stacked (only
        implement under Windows and GTK 2 platforms as it relies on
        @c wxTB_HORZ_LAYOUT flag support).
    @endStyleTable

    The common wxBookCtrl styles described in the @ref overview_bookctrl are
    also supported.

    @beginEventEmissionTable{wxBookCtrlEvent}
    @event{EVT_TOOLBOOK_PAGE_CHANGED(id, func)}
        The page selection was changed.
        Processes a @c wxEVT_TOOLBOOK_PAGE_CHANGED event.
    @event{EVT_TOOLBOOK_PAGE_CHANGING(id, func)}
        The page selection is about to be changed.
        Processes a @c wxEVT_TOOLBOOK_PAGE_CHANGING event.
        This event can be vetoed (using wxNotifyEvent::Veto()).
    @endEventTable

    @library{wxcore}
    @category{bookctrl}

    @see @ref overview_bookctrl, wxBookCtrlBase, wxNotebook,
         @ref page_samples_notebook
*/
class wxToolbook : public wxBookCtrlBase
{
public:
    //@{
    /**
        Constructs a choicebook control.
    */
    wxToolbook();
    wxToolbook(wxWindow* parent, wxWindowID id,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = 0,
                 const wxString& name = wxEmptyString);
    //@}

    /**
       Create the tool book control that has already been constructed with
       the default constructor.
    */
    bool Create(wxWindow *parent,
                wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxString& name = wxEmptyString);

    /**
        Returns the wxToolBarBase associated with the control.
    */
    wxToolBarBase* GetToolBar() const;

    /**
       Enables or disables the specified page.

       Using this function, a page can be disabled when it can't be used, while
       still remaining present to let the users know that more functionality is
       available, even if currently inaccessible.

       Icons for disabled pages are created by wxBitmap::ConvertToDisabled().

       @param page
            The index of the page.
       @param enable
            @true to enable the page and @false to disable it.

       @return @true if successful, @false otherwise (currently only if the
            index is invalid).

       @since 3.1.2
    */
    bool EnablePage(size_t page, bool enable);

   /**
       Enables or disables the specified page.

       This is similar to the overload above, but finds the index of the
       specified page.

       @param page
            Pointer of a page windows inside the book control.
       @param enable
            @true to enable the page and @false to disable it.

       @return @true if successful, @false otherwise, e.g. if @a page is not
           one of the pages of this control.

       @since 3.1.2
    */
    bool EnablePage(wxWindow *page, bool enable);
};

