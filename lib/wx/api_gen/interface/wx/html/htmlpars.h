/////////////////////////////////////////////////////////////////////////////
// Name:        html/htmlpars.h
// Purpose:     interface of wxHtmlTagHandler
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

enum wxHtmlURLType
{
    wxHTML_URL_PAGE,
    wxHTML_URL_IMAGE,
    wxHTML_URL_OTHER
};



/**
    @class wxHtmlTagHandler

    @todo describe me

    @library{wxhtml}
    @category{html}

    @see @ref overview_html_handlers, wxHtmlTag
*/
class wxHtmlTagHandler : public wxObject
{
public:
    /**
        Constructor.
    */
    wxHtmlTagHandler();

    /**
        Returns list of supported tags.
        The list is in uppercase and tags are delimited by ','.
        Example: @c "I,B,FONT,P"
    */
    virtual wxString GetSupportedTags() = 0;

    /**
        This is the core method of each handler. It is called each time
        one of supported tags is detected. @a tag contains all necessary
        info (see wxHtmlTag for details).

        Example:

        @code
        bool MyHandler::HandleTag(const wxHtmlTag& tag)
        {
            ...
            // change state of parser (e.g. set bold face)
            ParseInner(tag);
            ...
            // restore original state of parser
        }
        @endcode

        You shouldn't call ParseInner() if the tag is not paired with an ending one.

        @return @true if ParseInner() was called, @false otherwise.
    */
    virtual bool HandleTag(const wxHtmlTag& tag) = 0;

    /**
        Assigns @a parser to this handler. Each @b instance of handler
        is guaranteed to be called only from the one parser.
    */
    virtual void SetParser(wxHtmlParser* parser);

    /**
       Returns the parser associated with this tag handler.

       @since 2.9.5
    */
    wxHtmlParser* GetParser() const;


protected:

    /**
        This method calls parser's wxHtmlParser::DoParsing method
        for the string between this tag and the paired ending tag:
        @code
        ...<A HREF="x.htm">Hello, world!</A>...
        @endcode

        In this example, a call to ParseInner() (with @a tag pointing to A tag)
        will parse 'Hello, world!'.
    */
    void ParseInner(const wxHtmlTag& tag);

    /**
       Parses given source as if it was tag's inner code (see
       wxHtmlParser::GetInnerSource).  Unlike ParseInner(), this method lets
       you specify the source code to parse. This is useful when you need to
       modify the inner text before parsing.
    */
    void ParseInnerSource(const wxString& source);

    /**
        This attribute is used to access parent parser. It is protected so that
        it can't be accessed by user but can be accessed from derived classes.
    */
    wxHtmlParser* m_Parser;
};



/**
    @class wxHtmlParser

    Classes derived from this handle the @b generic parsing of HTML documents: it
    scans the document and divide it into blocks of tags (where one block consists
    of beginning and ending tag and of text between these two tags).

    It is independent from wxHtmlWindow and can be used as stand-alone parser.

    It uses system of tag handlers to parse the HTML document. Tag handlers
    are not statically shared by all instances but are created for each
    wxHtmlParser instance. The reason is that the handler may contain
    document-specific temporary data used during parsing (e.g. complicated
    structures like tables).

    Typically the user calls only the wxHtmlParser::Parse method.

    @library{wxhtml}
    @category{html}

    @see @ref overview_html_cells, @ref overview_html_handlers, wxHtmlTag
*/
class wxHtmlParser
{
public:
    /**
        Constructor.
    */
    wxHtmlParser();

    /**
        Adds handler to the internal list ( hash table) of handlers.
        This method should not be called directly by user but rather by derived class'
        constructor.

        This adds the handler to this @b instance of wxHtmlParser, not to
        all objects of this class!
        (Static front-end to AddTagHandler is provided by wxHtmlWinParser).

        All handlers are deleted on object deletion.
    */
    virtual void AddTagHandler(wxHtmlTagHandler* handler);

    /**
        Must be overwritten in derived class.

        This method is called by DoParsing() each time a part of text is parsed.
        @a txt is NOT only one word, it is substring of input.
        It is not formatted or preprocessed (so white spaces are unmodified).
    */
    virtual void AddWord(const wxString& txt);

    /**
        Parses the m_Source from @a begin_pos to @a end_pos - 1.
    */
    void DoParsing(const const_iterator& begin_pos, const const_iterator& end_pos);

    /**
        Parses the whole m_Source.
    */
    void DoParsing();

    /**
        This must be called after DoParsing().
    */
    virtual void DoneParser();

    /**
        Returns pointer to the file system. Because each tag handler has
        reference to it is parent parser it can easily request the file by
        calling:
        @code
        wxFSFile *f = m_Parser -> GetFS() -> OpenFile("image.jpg");
        @endcode
    */
    wxFileSystem* GetFS() const;

    /**
        Returns product of parsing.
        Returned value is result of parsing of the document.

        The type of this result depends on internal representation in derived
        parser (but it must be derived from wxObject!).
        See wxHtmlWinParser for details.
    */
    virtual wxObject* GetProduct() = 0;

    /**
        Returns pointer to the source being parsed.
    */
    const wxString* GetSource();

    /**
        Setups the parser for parsing the @a source string.
        (Should be overridden in derived class)
    */
    virtual void InitParser(const wxString& source);

    /**
        Opens given URL and returns @c wxFSFile object that can be used to read data
        from it. This method may return @NULL in one of two cases: either the URL doesn't
        point to any valid resource or the URL is blocked by overridden implementation
        of @e OpenURL in derived class.

        @param type
            Indicates type of the resource. Is one of:
            - wxHTML_URL_PAGE: Opening a HTML page.
            - wxHTML_URL_IMAGE: Opening an image.
            - wxHTML_URL_OTHER: Opening a resource that doesn't fall into
                                any other category.
        @param url
            URL being opened.

        @note
        Always use this method in tag handlers instead of GetFS()->OpenFile()
        because it can block the URL and is thus more secure.
        Default behaviour is to call wxHtmlWindow::OnOpeningURL of the associated
        wxHtmlWindow object (which may decide to block the URL or redirect it to
        another one),if there's any, and always open the URL if the parser is not
        used with wxHtmlWindow.
        Returned wxFSFile object is not guaranteed to point to url, it might have
        been redirected!
    */
    virtual wxFSFile* OpenURL(wxHtmlURLType type, const wxString& url) const;

    /**
        Proceeds parsing of the document. This is end-user method. You can simply
        call it when you need to obtain parsed output (which is parser-specific).

        The method does these things:
        -# calls InitParser(source)
        -# calls DoParsing()
        -# calls GetProduct()
        -# calls DoneParser()
        -# returns value returned by GetProduct()

        You shouldn't use InitParser(), DoParsing(), GetProduct() or DoneParser() directly.
    */
    wxObject* Parse(const wxString& source);

    /**
        Restores parser's state before last call to PushTagHandler().
    */
    void PopTagHandler();

    /**
        Forces the handler to handle additional tags
        (not returned by wxHtmlTagHandler::GetSupportedTags).
        The handler should already be added to this parser.

        @param handler
            the handler
        @param tags
            List of tags (in same format as GetSupportedTags()'s return value).
            The parser will redirect these tags to handler (until call to PopTagHandler()).

        Example:

        Imagine you want to parse following pseudo-html structure:
        @code
        <myitems>
            <param name="one" value="1">
            <param name="two" value="2">
        </myitems>

        <execute>
            <param program="text.exe">
        </execute>
        @endcode

        It is obvious that you cannot use only one tag handler for \<param\> tag.
        Instead you must use context-sensitive handlers for \<param\> inside \<myitems\>
        and \<param\> inside \<execute\>.
        This is the preferred solution:

        @code
        TAG_HANDLER_BEGIN(MYITEM, "MYITEMS")
            TAG_HANDLER_PROC(tag)
            {
                // ...something...

                m_Parser -> PushTagHandler(this, "PARAM");
                ParseInner(tag);
                m_Parser -> PopTagHandler();

                // ...something...
            }
        TAG_HANDLER_END(MYITEM)
        @endcode
    */
    void PushTagHandler(wxHtmlTagHandler* handler,
                        const wxString& tags);

    /**
        Sets the virtual file system that will be used to request additional files.
        (For example @c IMG tag handler requests wxFSFile with the image data.)
    */
    void SetFS(wxFileSystem* fs);

    /**
        Call this function to interrupt parsing from a tag handler.
        No more tags will be parsed afterward. This function may only be called
        from Parse() or any function called by it (i.e. from tag handlers).
    */
    virtual void StopParsing();

protected:

    /**
        This may (and may not) be overwritten in derived class.

        This method is called each time new tag is about to be added.
        @a tag contains information about the tag. (See wxHtmlTag for details.)

        Default (wxHtmlParser) behaviour is this: first it finds a handler capable
        of handling this tag and then it calls handler's HandleTag() method.
    */
    virtual void AddTag(const wxHtmlTag& tag);
};

