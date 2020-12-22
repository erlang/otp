/////////////////////////////////////////////////////////////////////////////
// Name:        html/winpars.h
// Purpose:     interface of wxHtmlTagsModule
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxHtmlTagsModule

    This class provides easy way of filling wxHtmlWinParser's table of
    tag handlers. It is used almost exclusively together with the set of
    @ref overview_html_handlers "TAGS_MODULE_* macros"

    @library{wxhtml}
    @category{html}

    @see @ref overview_html_handlers, wxHtmlTagHandler, wxHtmlWinTagHandler
*/
class wxHtmlTagsModule : public wxModule
{
public:
    /**
        You must override this method. In most common case its body consists
        only of lines of the following type:
        @code
        parser -> AddTagHandler(new MyHandler);
        @endcode

        It's recommended to use the @b TAGS_MODULE_* macros.

        @param parser
            Pointer to the parser that requested tables filling.
    */
    virtual void FillHandlersTable(wxHtmlWinParser* parser);
};



/**
    @class wxHtmlWinTagHandler

    This is basically wxHtmlTagHandler except that it is extended with protected
    member m_WParser pointing to the wxHtmlWinParser object (value of this member
    is identical to wxHtmlParser's m_Parser).

    @library{wxhtml}
    @category{html}
*/
class wxHtmlWinTagHandler : public wxHtmlTagHandler
{
public:
    /**
        Constructor.
    */
    wxHtmlWinTagHandler();

    /**
        Assigns @a parser to this handler. Each @b instance of handler
        is guaranteed to be called only from the one parser.
    */
    virtual void SetParser(wxHtmlParser* parser);

protected:
    /**
        Value of this attribute is identical to value of m_Parser.
        The only difference is that m_WParser points to wxHtmlWinParser object
        while m_Parser points to wxHtmlParser object. (The same object, but overcast.)
    */
    wxHtmlWinParser* m_WParser;
};



/**
    @class wxHtmlWinParser

    This class is derived from wxHtmlParser and its main goal is to parse HTML
    input so that it can be displayed in wxHtmlWindow.
    It uses a special wxHtmlWinTagHandler.

    @note The product of parsing is a wxHtmlCell (resp. wxHtmlContainer) object.

    @library{wxhtml}
    @category{html}

    @see @ref overview_html_handlers
*/
class wxHtmlWinParser : public wxHtmlParser
{
public:
    /**
        Constructor.

        Don't use the default one, use the constructor with @a wndIface parameter
        (@a wndIface is a pointer to interface object for the associated wxHtmlWindow
        or other HTML rendering window such as wxHtmlListBox).
    */
    wxHtmlWinParser(wxHtmlWindowInterface* wndIface = NULL);

    /**
        Adds module() to the list of wxHtmlWinParser tag handler.
    */
    static void AddModule(wxHtmlTagsModule* module);

    /**
        Closes the container, sets actual container to the parent one
        and returns pointer to it (see @ref overview_html_cells).
    */
    wxHtmlContainerCell* CloseContainer();

    /**
        Creates font based on current setting (see SetFontSize(), SetFontBold(),
        SetFontItalic(), SetFontFixed(), wxHtmlWinParser::SetFontUnderlined)
        and returns pointer to it.

        If the font was already created only a pointer is returned.
    */
    virtual wxFont* CreateCurrentFont();

    /**
        Returns actual text colour.
    */
    const wxColour& GetActualColor() const;

    /**
        Returns default horizontal alignment.
    */
    int GetAlign() const;

    /**
        Returns (average) char height in standard font.
        It is used as DC-independent metrics.

        @note This function doesn't return the @e actual height. If you want to
              know the height of the current font, call @c GetDC->GetCharHeight().
    */
    int GetCharHeight() const;

    /**
        Returns average char width in standard font.
        It is used as DC-independent metrics.

        @note This function doesn't return the @e actual width. If you want to
              know the height of the current font, call @c GetDC->GetCharWidth().
    */
    int GetCharWidth() const;

    /**
        Returns pointer to the currently opened container (see @ref overview_html_cells).
        Common use:
        @code
        m_WParser -> GetContainer() -> InsertCell(new ...);
        @endcode
    */
    wxHtmlContainerCell* GetContainer() const;

    /**
        Returns pointer to the DC used during parsing.
    */
    wxDC* GetDC();

    /**
        Returns wxEncodingConverter class used to do conversion between the
        @ref GetInputEncoding() "input encoding" and the
        @ref GetOutputEncoding() "output encoding".
    */
    wxEncodingConverter* GetEncodingConverter() const;

    /**
        Returns @true if actual font is bold, @false otherwise.
    */
    int GetFontBold() const;

    /**
        Returns actual font face name.
    */
    wxString GetFontFace() const;

    /**
        Returns @true if actual font is fixed face, @false otherwise.
    */
    int GetFontFixed() const;

    /**
        Returns @true if actual font is italic, @false otherwise.
    */
    int GetFontItalic() const;

    /**
        Returns actual font size (HTML size varies from -2 to +4)
    */
    int GetFontSize() const;

    /**
        Returns @true if actual font is underlined, @false otherwise.
    */
    int GetFontUnderlined() const;

    /**
        Returns input encoding.
    */
    wxFontEncoding GetInputEncoding() const;

    /**
        Returns actual hypertext link.
        (This value has a non-empty @ref wxHtmlLinkInfo::GetHref Href string
        if the parser is between \<A\> and \</A\> tags, wxEmptyString otherwise.)
    */
    const wxHtmlLinkInfo& GetLink() const;

    /**
        Returns the colour of hypertext link text.
    */
    const wxColour& GetLinkColor() const;

    /**
        Returns output encoding, i.e. closest match to document's input encoding
        that is supported by operating system.
    */
    wxFontEncoding GetOutputEncoding() const;

    /**
        Returns associated window (wxHtmlWindow). This may be @NULL!
        (You should always test if it is non-@NULL.
        For example @c TITLE handler sets window title only if some window is
        associated, otherwise it does nothing.
   */
    wxHtmlWindowInterface* GetWindowInterface();

    /**
        Opens new container and returns pointer to it (see @ref overview_html_cells).
    */
    wxHtmlContainerCell* OpenContainer();

    /**
        Sets actual text colour. Note: this DOESN'T change the colour!
        You must create wxHtmlColourCell yourself.
    */
    void SetActualColor(const wxColour& clr);

    /**
        Sets default horizontal alignment (see wxHtmlContainerCell::SetAlignHor).
        Alignment of newly opened container is set to this value.
    */
    void SetAlign(int a);

    /**
        Allows you to directly set opened container.
        This is not recommended - you should use OpenContainer() wherever possible.
    */
    wxHtmlContainerCell* SetContainer(wxHtmlContainerCell* c);

    /**
        Sets the DC. This must be called before wxHtmlParser::Parse!

        @a pixel_scale  can be used when rendering to high-resolution
        DCs (e.g. printer) to adjust size of pixel metrics. (Many dimensions in
        HTML are given in pixels -- e.g. image sizes. 300x300 image would be only one
        inch wide on typical printer. With pixel_scale = 3.0 it would be 3 inches.)
    */
    virtual void SetDC(wxDC* dc, double pixel_scale = 1.0e+0);

    /**
        Sets bold flag of actualfont. @a x is either @true of @false.
    */
    void SetFontBold(int x);

    /**
        Sets current font face to @a face. This affects either fixed size
        font or proportional, depending on context (whether the parser is
        inside @c \<TT\> tag or not).
    */
    void SetFontFace(const wxString& face);

    /**
        Sets fixed face flag of actualfont. @a x is either @true of @false.
    */
    void SetFontFixed(int x);

    /**
        Sets italic flag of actualfont. @a x is either @true of @false.
    */
    void SetFontItalic(int x);

    /**
        Sets actual font size (HTML size varies from 1 to 7).
    */
    void SetFontSize(int s);

    /**
        Sets underlined flag of actualfont. @a x is either @true of @false.
    */
    void SetFontUnderlined(int x);

    /**
        Sets fonts. See wxHtmlWindow::SetFonts for detailed description.
    */
    void SetFonts(const wxString& normal_face, const wxString& fixed_face,
                  const int* sizes = 0);

    /**
        Sets input encoding. The parser uses this information to build conversion
        tables from document's encoding to some encoding supported by operating system.
    */
    void SetInputEncoding(wxFontEncoding enc);

    /**
        Sets actual hypertext link.
        Empty link is represented by wxHtmlLinkInfo with @e Href equal
        to wxEmptyString.
    */
    void SetLink(const wxHtmlLinkInfo& link);

    /**
        Sets colour of hypertext link.
    */
    void SetLinkColor(const wxColour& clr);
};

