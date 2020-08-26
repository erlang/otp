/////////////////////////////////////////////////////////////////////////////
// Name:        html/helpdata.h
// Purpose:     interface of wxHtmlHelpData
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxHtmlBookRecord

    Helper class for wxHtmlHelpData
*/
class wxHtmlBookRecord
{
public:
    wxHtmlBookRecord(const wxString& bookfile, const wxString& basepath,
                     const wxString& title, const wxString& start);

    wxString GetBookFile() const;
    wxString GetTitle() const;
    wxString GetStart() const;
    wxString GetBasePath() const;

    /* SetContentsRange: store in the bookrecord where in the index/contents lists the
     * book's records are stored. This to facilitate searching in a specific book.
     * This code will have to be revised when loading/removing books becomes dynamic.
     * (as opposed to appending only)
     * Note that storing index range is pointless, because the index is alphab. sorted. */
    void SetContentsRange(int start, int end);
    int GetContentsStart() const;
    int GetContentsEnd() const;

    void SetTitle(const wxString& title);
    void SetBasePath(const wxString& path);
    void SetStart(const wxString& start);

    // returns full filename of page (which is part of the book),
    // i.e. with book's basePath prepended. If page is already absolute
    // path, basePath is _not_ prepended.
    wxString GetFullPath(const wxString &page) const;
};



/**
    @class wxHtmlHelpDataItem

    Helper class for wxHtmlHelpData
*/
struct wxHtmlHelpDataItem
{
    wxHtmlHelpDataItem();

    int level;
    wxHtmlHelpDataItem *parent;
    int id;
    wxString name;
    wxString page;
    wxHtmlBookRecord *book;

    // returns full filename of m_Page, i.e. with book's basePath prepended
    wxString GetFullPath() const;

    // returns item indented with spaces if it has level>1:
    wxString GetIndentedName() const;
};




/**
    @class wxHtmlHelpData

    This class is used by wxHtmlHelpController and wxHtmlHelpFrame to access HTML
    help items.

    It is internal class and should not be used directly - except for the case
    you're writing your own HTML help controller.

    @library{wxhtml}
    @category{help,html}
*/
class wxHtmlHelpData : public wxObject
{
public:
    /**
        Constructor.
    */
    wxHtmlHelpData();

    /**
        Adds new book.

        @a book_url is URL (not filename!) of HTML help project (hhp) or ZIP file
        that contains arbitrary number of .hhp projects (this zip file can have
        either .zip or .htb extension, htb stands for "html book").

        Returns success.
    */
    bool AddBook(const wxString& book_url);

    /**
        Returns page's URL based on integer ID stored in project.
    */
    wxString FindPageById(int id);

    /**
        Returns page's URL based on its (file)name.
    */
    wxString FindPageByName(const wxString& page);

    /**
        Returns array with help books info.
    */
    const wxHtmlBookRecArray& GetBookRecArray() const;

    /**
        Returns reference to array with contents entries.
    */
    const wxHtmlHelpDataItems& GetContentsArray() const;

    /**
        Returns reference to array with index entries.
    */
    const wxHtmlHelpDataItems& GetIndexArray() const;

    /**
        Sets the temporary directory where binary cached versions of MS HTML Workshop
        files will be stored. (This is turned off by default and you can enable
        this feature by setting non-empty temp dir.)
    */
    void SetTempDir(const wxString& path);
};

