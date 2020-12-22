/////////////////////////////////////////////////////////////////////////////
// Name:        htmllbox.h
// Purpose:     interface of wxHtmlListBox
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxHtmlListBox

    wxHtmlListBox is an implementation of wxVListBox which shows HTML content in
    the listbox rows. This is still an abstract base class and you will need to
    derive your own class from it (see htlbox sample for the example), but you will
    only need to override a single wxHtmlListBox::OnGetItem function.

    @beginEventEmissionTable{wxHtmlCellEvent,wxHtmlLinkEvent}
    @event{EVT_HTML_CELL_CLICKED(id, func)}
        A wxHtmlCell was clicked.
    @event{EVT_HTML_CELL_HOVER(id, func)}
        The mouse passed over a wxHtmlCell.
    @event{EVT_HTML_LINK_CLICKED(id, func)}
        A wxHtmlCell which contains a hyperlink was clicked.
    @endEventTable

    @library{wxhtml}
    @category{ctrl}

    @see wxSimpleHtmlListBox
*/
class wxHtmlListBox : public wxVListBox
{
public:
    /**
        Normal constructor which calls Create() internally.
    */
    wxHtmlListBox(wxWindow* parent, wxWindowID id = wxID_ANY,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = 0,
                  const wxString& name = wxHtmlListBoxNameStr);

    /**
        Default constructor, you must call Create() later.
    */
    wxHtmlListBox();

    /**
        Destructor cleans up whatever resources we use.
    */
    virtual ~wxHtmlListBox();

    /**
        Creates the control and optionally sets the initial number of items in it
        (it may also be set or changed later with wxVListBox::SetItemCount).

        There are no special styles defined for wxHtmlListBox, in particular the
        wxListBox styles (with the exception of @c wxLB_MULTIPLE) cannot be used here.

        Returns @true on success or @false if the control couldn't be created
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxString& name = wxHtmlListBoxNameStr);

    //@{
    /**
        Returns the wxFileSystem used by the HTML parser of this object.

        The file system object is used to resolve the paths in HTML fragments
        displayed in the control and you should use wxFileSystem::ChangePathTo
        if you use relative paths for the images or other resources embedded in
        your HTML.
    */
    wxFileSystem& GetFileSystem() const;
    const wxFileSystem& GetFileSystem() const;
    //@}

protected:

    /**
        Called when the user clicks on hypertext link. Does nothing by default.
        Overloading this method is deprecated; intercept the event instead.

        @param n
            Index of the item containing the link.
        @param link
            Description of the link.

        @see wxHtmlLinkInfo.
    */
    virtual void OnLinkClicked(size_t n, const wxHtmlLinkInfo& link);

    /**
        This virtual function may be overridden to change the appearance of the
        background of the selected cells in the same way as GetSelectedTextColour().

        It should be rarely, if ever, used because wxVListBox::SetSelectionBackground
        allows changing the selection background for all cells at once and doing
        anything more fancy is probably going to look strangely.

        @see GetSelectedTextColour()
    */
    virtual wxColour GetSelectedTextBgColour(const wxColour& colBg) const;

    /**
        This virtual function may be overridden to customize the appearance of the
        selected cells. It is used to determine how the colour @a colFg is going to
        look inside selection. By default all original colours are completely ignored
        and the standard, system-dependent, selection colour is used but the program
        may wish to override this to achieve some custom appearance.

        @see GetSelectedTextBgColour(),
             wxVListBox::SetSelectionBackground, wxSystemSettings::GetColour
    */
    virtual wxColour GetSelectedTextColour(const wxColour& colFg) const;

    /**
        This function may be overridden to decorate HTML returned by OnGetItem().
    */
    virtual wxString OnGetItemMarkup(size_t n) const;

    /**
        This method must be implemented in the derived class and should return
        the body (i.e.\ without @c html nor @c body tags) of the HTML fragment
        for the given item.

        Note that this function should always return a text fragment for the @a n item
        which renders with the same height both when it is selected and when it's not:
        i.e. if you call, inside your OnGetItem() implementation, @c IsSelected(n) to
        make the items appear differently when they are selected, then you should make
        sure that the returned HTML fragment will render with the same height or else
        you'll see some artifacts when the user selects an item.
    */
    virtual wxString OnGetItem(size_t n) const = 0;
};



/**
    @class wxSimpleHtmlListBox

    wxSimpleHtmlListBox is an implementation of wxHtmlListBox which
    shows HTML content in the listbox rows.

    Unlike wxHtmlListBox, this is not an abstract class and thus it has the
    advantage that you can use it without deriving your own class from it.
    However, it also has the disadvantage that this is not a virtual control and
    thus it's not well-suited for those cases where you need to show a huge number
    of items: every time you add/insert a string, it will be stored internally
    and thus will take memory.

    The interface exposed by wxSimpleHtmlListBox fully implements the
    wxControlWithItems interface, thus you should refer to wxControlWithItems's
    documentation for the API reference for adding/removing/retrieving items in
    the listbox. Also note that the wxVListBox::SetItemCount function is
    @c protected in wxSimpleHtmlListBox's context so that you cannot call it
    directly, wxSimpleHtmlListBox will do it for you.

    Note: in case you need to append a lot of items to the control at once, make
    sure to use the
    @ref wxControlWithItems::Append "Append(const wxArrayString&)" function.

    Thus the only difference between a wxListBox and a wxSimpleHtmlListBox
    is that the latter stores strings which can contain HTML fragments (see the
    list of @ref overview_html_supptags "tags supported by wxHTML").

    Note that the HTML strings you fetch to wxSimpleHtmlListBox should not contain
    the @c \<html\> or @c \<body\> tags.

    @beginStyleTable
    @style{wxHLB_DEFAULT_STYLE}
           The default style: wxBORDER_SUNKEN
    @style{wxHLB_MULTIPLE}
           Multiple-selection list: the user can toggle multiple items on and off.
    @endStyleTable


    A wxSimpleHtmlListBox emits the same events used by wxListBox and by wxHtmlListBox.

    @beginEventEmissionTable
    @event{EVT_LISTBOX(id, func)}
        Process a @c wxEVT_LISTBOX event, when an item on the list
        is selected. See wxCommandEvent.
    @event{EVT_LISTBOX_DCLICK(id, func)}
        Process a @c wxEVT_LISTBOX_DCLICK event, when the listbox is
        double-clicked. See wxCommandEvent.
    @event{EVT_HTML_CELL_CLICKED(id, func)}
        A wxHtmlCell was clicked. See wxHtmlCellEvent.
    @event{EVT_HTML_CELL_HOVER(id, func)}
        The mouse passed over a wxHtmlCell. See wxHtmlCellEvent.
    @event{EVT_HTML_LINK_CLICKED(id, func)}
        A wxHtmlCell which contains a hyperlink was clicked. See wxHtmlLinkEvent
    @endEventTable

    @library{wxhtml}
    @category{ctrl}
    @genericAppearance{simplehtmllistbox}

    @see wxSimpleHtmlListBox::Create
*/
#define wxHLB_DEFAULT_STYLE     wxBORDER_SUNKEN
#define wxHLB_MULTIPLE          wxLB_MULTIPLE

class wxSimpleHtmlListBox : public wxHtmlListBox,
                            public wxItemContainer
{
public:
    /**
        Constructor, creating and showing the HTML list box.

        @param parent
            Parent window. Must not be NULL.
        @param id
            Window identifier. A value of -1 indicates a default value.
        @param pos
            Window position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Window size.
            If ::wxDefaultSize is specified then the window is sized appropriately.
        @param n
            Number of strings with which to initialise the control.
        @param choices
            An array of strings with which to initialise the control.
        @param style
            Window style. See wxHLB_* flags.
        @param validator
            Window validator.
        @param name
            Window name.
    */
    wxSimpleHtmlListBox(wxWindow* parent, wxWindowID id,
                        const wxPoint& pos = wxDefaultPosition,
                        const wxSize& size = wxDefaultSize,
                        int n = 0,
                        const wxString choices[] = NULL,
                        long style = wxHLB_DEFAULT_STYLE,
                        const wxValidator& validator = wxDefaultValidator,
                        const wxString& name = wxSimpleHtmlListBoxNameStr);

    /**
        Constructor, creating and showing the HTML list box.

        @param parent
            Parent window. Must not be NULL.
        @param id
            Window identifier. A value of -1 indicates a default value.
        @param pos
            Window position.
        @param size
            Window size. If wxDefaultSize is specified then the window is sized appropriately.
        @param choices
            An array of strings with which to initialise the control.
        @param style
            Window style. See wxHLB_* flags.
        @param validator
            Window validator.
        @param name
            Window name.
    */
    wxSimpleHtmlListBox(wxWindow* parent, wxWindowID id,
                        const wxPoint& pos,
                        const wxSize& size,
                        const wxArrayString& choices,
                        long style = wxHLB_DEFAULT_STYLE,
                        const wxValidator& validator = wxDefaultValidator,
                        const wxString& name = wxSimpleHtmlListBoxNameStr);

    /**
        Default constructor, you must call Create() later.
    */
    wxSimpleHtmlListBox();

    /**
        Frees the array of stored items and relative client data.
    */
    virtual ~wxSimpleHtmlListBox();

    //@{
    /**
        Creates the HTML listbox for two-step construction.
        See wxSimpleHtmlListBox() for further details.
    */
    bool Create(wxWindow *parent, wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                int n = 0, const wxString choices[] = NULL,
                long style = wxHLB_DEFAULT_STYLE,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxSimpleHtmlListBoxNameStr);
    bool Create(wxWindow *parent, wxWindowID id,
                const wxPoint& pos,
                const wxSize& size,
                const wxArrayString& choices,
                long style = wxHLB_DEFAULT_STYLE,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxSimpleHtmlListBoxNameStr);
    //@}
};

