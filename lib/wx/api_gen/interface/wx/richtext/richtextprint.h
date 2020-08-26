/////////////////////////////////////////////////////////////////////////////
// Name:        richtext/richtextprint.h
// Purpose:     interface of wxRichTextHeaderFooterData
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    These are the header and footer page identifiers, passed to functions such
    as wxRichTextHeaderFooterData::SetFooterText to specify the odd or even page
    for the text.
*/
enum wxRichTextOddEvenPage {
    wxRICHTEXT_PAGE_ODD,
    wxRICHTEXT_PAGE_EVEN,
    wxRICHTEXT_PAGE_ALL,
};


/**
    These are the location identifiers for passing to functions such as
    wxRichTextHeaderFooterData::SetFooterText(), to specify whether the text
    is on the left, centre or right of the page.
*/
enum wxRichTextPageLocation {
    wxRICHTEXT_PAGE_LEFT,
    wxRICHTEXT_PAGE_CENTRE,
    wxRICHTEXT_PAGE_RIGHT
};


/**
    @class wxRichTextHeaderFooterData


    This class represents header and footer data to be passed to the
    wxRichTextPrinting and wxRichTextPrintout classes.

    Headers and footers can be specified independently for odd, even or both page
    sides. Different text can be specified for left, centre and right locations
    on the page, and the font and text colour can also be specified.

    You can specify the following keywords in header and footer text, which will
    be substituted for the actual values during printing and preview.

    - @@DATE@: the current date.
    - @@PAGESCNT@: the total number of pages.
    - @@PAGENUM@: the current page number.
    - @@TIME@: the current time.
    - @@TITLE@: the title of the document, as passed to the wxRichTextPrinting or
      wxRichTextLayout constructor.

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextHeaderFooterData : public wxObject
{
public:
    //@{
    /**
        Constructors.
    */
    wxRichTextHeaderFooterData();
    wxRichTextHeaderFooterData(const wxRichTextHeaderFooterData& data);
    //@}

    /**
        Clears all text.
    */
    void Clear();

    /**
        Copies the data.
    */
    void Copy(const wxRichTextHeaderFooterData& data);

    /**
        Returns the font specified for printing the header and footer.
    */
    const wxFont& GetFont() const;

    /**
        Returns the margin between the text and the footer.
    */
    int GetFooterMargin() const;

    /**
        Returns the footer text on odd or even pages, and at a given position on the
        page (left, centre or right).
    */
    wxString GetFooterText(wxRichTextOddEvenPage page = wxRICHTEXT_PAGE_EVEN,
                           wxRichTextPageLocation location = wxRICHTEXT_PAGE_CENTRE) const;

    /**
        Returns the margin between the text and the header.
    */
    int GetHeaderMargin() const;

    /**
        Returns the header text on odd or even pages, and at a given position on the
        page (left, centre or right).
    */
    wxString GetHeaderText(wxRichTextOddEvenPage page = wxRICHTEXT_PAGE_EVEN,
                           wxRichTextPageLocation location = wxRICHTEXT_PAGE_CENTRE) const;

    /**
        Returns @true if the header and footer will be shown on the first page.
    */
    bool GetShowOnFirstPage() const;

    /**
        Helper function for getting the header or footer text, odd or even pages, and
        at a given position on the page (left, centre or right).
    */
    wxString GetText(int headerFooter, wxRichTextOddEvenPage page,
                     wxRichTextPageLocation location) const;

    /**
        Returns the text colour for drawing the header and footer.
    */
    const wxColour& GetTextColour() const;

    /**
        Initialises the object.
    */
    void Init();

    /**
        Sets the font for drawing the header and footer.
    */
    void SetFont(const wxFont& font);

    /**
        Sets the footer text on odd or even pages, and at a given position on the page
        (left, centre or right).
    */
    void SetFooterText(const wxString& text,
                       wxRichTextOddEvenPage page = wxRICHTEXT_PAGE_ALL,
                       wxRichTextPageLocation location = wxRICHTEXT_PAGE_CENTRE);

    /**
        Sets the header text on odd or even pages, and at a given position on the page
        (left, centre or right).
    */
    void SetHeaderText(const wxString& text,
                       wxRichTextOddEvenPage page = wxRICHTEXT_PAGE_ALL,
                       wxRichTextPageLocation location = wxRICHTEXT_PAGE_CENTRE);

    /**
        Sets the margins between text and header or footer, in tenths of a millimeter.
    */
    void SetMargins(int headerMargin, int footerMargin);

    /**
        Pass @true to show the header or footer on first page (the default).
    */
    void SetShowOnFirstPage(bool showOnFirstPage);

    /**
        Helper function for setting the header or footer text, odd or even pages, and
        at a given position on the page (left, centre or right).
    */
    void SetText(const wxString& text, int headerFooter,
                 wxRichTextOddEvenPage page,
                 wxRichTextPageLocation location);

    /**
        Sets the text colour for drawing the header and footer.
    */
    void SetTextColour(const wxColour& col);

    /**
        Assignment operator.
    */
    void operator operator=(const wxRichTextHeaderFooterData& data);
};



/**
    @class wxRichTextPrintout

    This class implements print layout for wxRichTextBuffer.
    Instead of using it directly, you should normally use the wxRichTextPrinting class.

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextPrintout : public wxPrintout
{
public:
    /**
        Constructor.
    */
    wxRichTextPrintout(const wxString& title = "Printout");

    /**
        Calculates scaling and text, header and footer rectangles.
    */
    void CalculateScaling(wxDC* dc, wxRect& textRect,
                          wxRect& headerRect,
                          wxRect& footerRect);

    /**
        Returns the header and footer data associated with the printout.
    */
    const wxRichTextHeaderFooterData& GetHeaderFooterData() const;

    /**
        Gets the page information.
    */
    virtual void GetPageInfo(int* minPage, int* maxPage, int* selPageFrom,
                             int* selPageTo);

    /**
        Returns a pointer to the buffer being rendered.
    */
    wxRichTextBuffer* GetRichTextBuffer() const;

    /**
        Returns @true if the given page exists in the printout.
    */
    virtual bool HasPage(int page);

    /**
        Prepares for printing, laying out the buffer and calculating pagination.
    */
    virtual void OnPreparePrinting();

    /**
        Does the actual printing for this page.
    */
    virtual bool OnPrintPage(int page);

    /**
        Sets the header and footer data associated with the printout.
    */
    void SetHeaderFooterData(const wxRichTextHeaderFooterData& data);

    /**
        Sets margins in 10ths of millimetre. Defaults to 1 inch for margins.
    */
    void SetMargins(int top = 254, int bottom = 254, int left = 254,
                    int right = 254);

    /**
        Sets the buffer to print. wxRichTextPrintout does not manage this pointer;
        it should be managed by the calling code, such as wxRichTextPrinting.
    */
    void SetRichTextBuffer(wxRichTextBuffer* buffer);
};



/**
    @class wxRichTextPrinting

    This class provides a simple interface for performing wxRichTextBuffer printing
    and previewing. It uses wxRichTextPrintout for layout and rendering.

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextPrinting : public wxObject
{
public:
    /**
        Constructor.

        Optionally pass a title to be used in the preview frame and printing wait
        dialog, and also a parent window for these windows.
    */
    wxRichTextPrinting(const wxString& name = "Printing",
                       wxWindow* parentWindow = NULL);

    /**
        A convenience function to get the footer text.
        See wxRichTextHeaderFooterData for details.
    */
    wxString GetFooterText(wxRichTextOddEvenPage page = wxRICHTEXT_PAGE_EVEN,
                           wxRichTextPageLocation location = wxRICHTEXT_PAGE_CENTRE) const;

    /**
        Returns the internal wxRichTextHeaderFooterData object.
    */
    const wxRichTextHeaderFooterData& GetHeaderFooterData() const;

    /**
        A convenience function to get the header text.
        See wxRichTextHeaderFooterData for details.
    */
    wxString GetHeaderText(wxRichTextOddEvenPage page = wxRICHTEXT_PAGE_EVEN,
                           wxRichTextPageLocation location = wxRICHTEXT_PAGE_CENTRE) const;

    /**
        Returns a pointer to the internal page setup data.
    */
    wxPageSetupDialogData* GetPageSetupData();

    /**
        Returns the parent window to be used for the preview window and printing
        wait dialog.
    */
    wxWindow* GetParentWindow() const;

    /**
        Returns the dimensions to be used for the preview window.
    */
    const wxRect& GetPreviewRect() const;

    /**
        Returns a pointer to the internal print data.
    */
    wxPrintData* GetPrintData();

    /**
        Returns the title of the preview window or printing wait caption.
    */
    const wxString& GetTitle() const;

    /**
        Shows the page setup dialog.
    */
    void PageSetup();

    /**
        Shows a preview window for the given buffer.
        The function takes its own copy of @a buffer.
    */
    bool PreviewBuffer(const wxRichTextBuffer& buffer);

    /**
        Shows a preview window for the given file.

        @a richTextFile can be a text file or XML file, or other file
        depending on the available file handlers.
    */
    bool PreviewFile(const wxString& richTextFile);

    /**
        Prints the given buffer. The function takes its own copy of @a buffer.
        @a showPrintDialog can be @true to show the print dialog, or @false to print quietly.
    */
    bool PrintBuffer(const wxRichTextBuffer& buffer, bool showPrintDialog = true);

    /**
        Prints the given file. @a richTextFile can be a text file or XML file,
        or other file depending on the available file handlers. @a showPrintDialog
        can be @true to show the print dialog, or @false to print quietly.
    */
    bool PrintFile(const wxString& richTextFile, bool showPrintDialog = true);

    /**
        A convenience function to set the footer text.
        See wxRichTextHeaderFooterData for details.
    */
    void SetFooterText(const wxString& text,
                       wxRichTextOddEvenPage page = wxRICHTEXT_PAGE_ALL,
                       wxRichTextPageLocation location = wxRICHTEXT_PAGE_CENTRE);

    /**
        Sets the internal wxRichTextHeaderFooterData object.
    */
    void SetHeaderFooterData(const wxRichTextHeaderFooterData& data);

    /**
        Sets the wxRichTextHeaderFooterData font.
    */
    void SetHeaderFooterFont(const wxFont& font);

    /**
        Sets the wxRichTextHeaderFooterData text colour.
    */
    void SetHeaderFooterTextColour(const wxColour& colour);

    /**
        A convenience function to set the header text.
        See wxRichTextHeaderFooterData for details.
    */
    void SetHeaderText(const wxString& text,
                       wxRichTextOddEvenPage page = wxRICHTEXT_PAGE_ALL,
                       wxRichTextPageLocation location = wxRICHTEXT_PAGE_CENTRE);

    /**
        Sets the page setup data.
    */
    void SetPageSetupData(const wxPageSetupDialogData& pageSetupData);

    /**
        Sets the parent window to be used for the preview window and printing
        wait dialog.
    */
    void SetParentWindow(wxWindow* parent);

    /**
        Sets the dimensions to be used for the preview window.
    */
    void SetPreviewRect(const wxRect& rect);

    /**
        Sets the print data.
    */
    void SetPrintData(const wxPrintData& printData);

    /**
        Pass @true to show the header and footer on the first page.
    */
    void SetShowOnFirstPage(bool show);

    /**
        Pass the title of the preview window or printing wait caption.
    */
    void SetTitle(const wxString& title);
};

