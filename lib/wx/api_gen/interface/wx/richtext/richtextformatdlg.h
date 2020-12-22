/////////////////////////////////////////////////////////////////////////////
// Name:        richtext/richtextformatdlg.h
// Purpose:     interface of wxRichTextFormattingDialog*
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxRichTextFormattingDialogFactory

    This class provides pages for wxRichTextFormattingDialog, and allows other
    customization of the dialog.

    A default instance of this class is provided automatically.
    If you wish to change the behaviour of the formatting dialog (for example add
    or replace a page), you may derive from this class, override one or more
    functions, and call the static function
    wxRichTextFormattingDialog::SetFormattingDialogFactory.

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextFormattingDialogFactory : public wxObject
{
public:
    /**
        Constructor.
    */
    wxRichTextFormattingDialogFactory();

    /**
        Destructor.
    */
    virtual ~wxRichTextFormattingDialogFactory();

    /**
        Creates the main dialog buttons.
    */
    virtual bool CreateButtons(wxRichTextFormattingDialog* dialog);

    /**
        Creates a page, given a page identifier.
    */
    virtual wxPanel* CreatePage(int page, wxString& title,
                                wxRichTextFormattingDialog* dialog);

    /**
        Creates all pages under the dialog's book control, also calling AddPage().
    */
    virtual bool CreatePages(long pages,
                             wxRichTextFormattingDialog* dialog);

    /**
        Enumerate all available page identifiers.
    */
    virtual int GetPageId(int i) const;

    /**
        Gets the number of available page identifiers.
    */
    virtual int GetPageIdCount() const;

    /**
        Gets the image index for the given page identifier.
    */
    virtual int GetPageImage(int id) const;

    /**
        Set the property sheet style, called at the start of
        wxRichTextFormattingDialog::Create.
    */
    virtual bool SetSheetStyle(wxRichTextFormattingDialog* dialog);

    /**
        Invokes help for the dialog.
    */
    virtual bool ShowHelp(int page,
                          wxRichTextFormattingDialog* dialog);
};



#define wxRICHTEXT_FORMAT_STYLE_EDITOR      0x0001
#define wxRICHTEXT_FORMAT_FONT              0x0002
#define wxRICHTEXT_FORMAT_TABS              0x0004
#define wxRICHTEXT_FORMAT_BULLETS           0x0008
#define wxRICHTEXT_FORMAT_INDENTS_SPACING   0x0010

/**
    @class wxRichTextFormattingDialog

    This dialog allows the user to edit a character and/or paragraph style.

    In the constructor, specify the pages that will be created.
    Use wxRichTextFormattingDialog::GetStyle() to retrieve the common style
    for a given range, and then use wxRichTextFormattingDialog::ApplyStyle()
    to apply the user-selected formatting to a control.

    For example:
    @code
        wxRichTextRange range;
        if (m_richTextCtrl->HasSelection())
            range = m_richTextCtrl->GetSelectionRange();
        else
            range = wxRichTextRange(0, m_richTextCtrl->GetLastPosition()+1);

        int pages = wxRICHTEXT_FORMAT_FONT|wxRICHTEXT_FORMAT_INDENTS_SPACING| \
                    wxRICHTEXT_FORMAT_TABS|wxRICHTEXT_FORMAT_BULLETS;

        wxRichTextFormattingDialog formatDlg(pages, this);
        formatDlg.GetStyle(m_richTextCtrl, range);

        if (formatDlg.ShowModal() == wxID_OK)
        {
            formatDlg.ApplyStyle(m_richTextCtrl, range);
        }
    @endcode

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextFormattingDialog : public wxPropertySheetDialog
{
public:
    enum { Option_AllowPixelFontSize = 0x0001 };

    /**
        Default ctor.
    */
    wxRichTextFormattingDialog();

    /**
        Constructors.

        @param flags
            The pages to show.
        @param parent
            The dialog's parent.
        @param title
            The dialog's title.
        @param id
            The dialog's ID.
        @param pos
            The dialog's position.
        @param sz
            The dialog's size.
        @param style
            The dialog's window style.
    */
    wxRichTextFormattingDialog(long flags, wxWindow* parent, const wxString& title = "Formatting",
                               wxWindowID id = wxID_ANY, const wxPoint& pos = wxDefaultPosition,
                               const wxSize& sz = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE);

    /**
        Destructor.
    */
    virtual ~wxRichTextFormattingDialog();

    /**
        Apply attributes to the given range, only changing attributes that
        need to be changed.
    */
    virtual bool ApplyStyle(wxRichTextCtrl* ctrl, const wxRichTextRange& range,
                            int flags = wxRICHTEXT_SETSTYLE_WITH_UNDO|wxRICHTEXT_SETSTYLE_OPTIMIZE);

    /**
        Creation: see wxRichTextFormattingDialog() "the constructor" for
        details about the parameters.
    */
    bool Create(long flags, wxWindow* parent,
                const wxString& title = wxGetTranslation("Formatting"), wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition, const wxSize& sz = wxDefaultSize,
                long style = wxDEFAULT_DIALOG_STYLE);

    //@{
    /**
        Gets the attributes being edited.
    */
    const wxTextAttr& GetAttributes() const;
    wxTextAttr& GetAttributes();
    //@}

    /**
        Helper for pages to get the top-level dialog.
    */
    static wxRichTextFormattingDialog* GetDialog(wxWindow* win);

    /**
        Helper for pages to get the attributes.
    */
    static wxTextAttr* GetDialogAttributes(wxWindow* win);

    /**
        Helper for pages to get the style.
    */
    static wxRichTextStyleDefinition* GetDialogStyleDefinition(wxWindow* win);

    /**
        Returns the object to be used to customize the dialog and provide pages.
    */
    static wxRichTextFormattingDialogFactory* GetFormattingDialogFactory();

    /**
        Returns the image list associated with the dialog, used for example if showing
        the dialog as a toolbook.
    */
    wxImageList* GetImageList() const;

    /**
        Gets common attributes from the given range and calls SetAttributes().
        Attributes that do not have common values in the given range
        will be omitted from the style's flags.
    */
    virtual bool GetStyle(wxRichTextCtrl* ctrl, const wxRichTextRange& range);

    /**
        Gets the associated style definition, if any.
    */
    virtual wxRichTextStyleDefinition* GetStyleDefinition() const;

    /**
        Gets the associated style sheet, if any.
    */
    virtual wxRichTextStyleSheet* GetStyleSheet() const;

    /**
        Sets the attributes to be edited.
    */
    void SetAttributes(const wxTextAttr& attr);

    /**
        Sets the dialog options, determining what the interface presents to the user.
        Currently the only option is Option_AllowPixelFontSize.
    */
    void SetOptions(int options) { m_options = options; }

    /**
        Gets the dialog options, determining what the interface presents to the user.
        Currently the only option is Option_AllowPixelFontSize.
    */
    int GetOptions() const { return m_options; }

    /**
        Returns @true if the given option is present.
    */
    bool HasOption(int option) const { return (m_options & option) != 0; }

    /**
        Sets the formatting factory object to be used for customization and page
        creation.

        It deletes the existing factory object.
    */
    static void SetFormattingDialogFactory(wxRichTextFormattingDialogFactory* factory);

    /**
        Sets the image list associated with the dialog's property sheet.
    */
    void SetImageList(wxImageList* imageList);

    /**
        Sets the attributes and optionally updates the display, if @a update is @true.
    */
    virtual bool SetStyle(const wxTextAttr& style, bool update = true);

    /**
        Sets the style definition and optionally update the display,
        if @a update is @true.
    */
    virtual bool SetStyleDefinition(const wxRichTextStyleDefinition& styleDef,
                                    wxRichTextStyleSheet* sheet,
                                    bool update = true);

    /**
        Updates the display.
    */
    virtual bool UpdateDisplay();

    /**
        Returns @true if the dialog will restore the last-selected page.
    */
    static bool GetRestoreLastPage();

    /**
        Pass @true if the dialog should restore the last-selected page.
    */
    static void SetRestoreLastPage(bool b);

    /**
        Returns the page identifier of the last page selected (not the control id).
    */
    static int GetLastPage();

    /**
        Sets the page identifier of the last page selected (not the control id).
    */
    static void SetLastPage(int lastPage);

    /**
        Sets the custom colour data for use by the colour dialog.
    */
    static void SetColourData(const wxColourData& colourData);

    /**
        Returns the custom colour data for use by the colour dialog.
    */
    static wxColourData GetColourData();
};

