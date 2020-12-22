/////////////////////////////////////////////////////////////////////////////
// Name:        helpext.h
// Purpose:     interface of wxExtHelpController
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxExtHelpController

    This class implements help via an external browser.
    It requires the name of a directory containing the documentation
    and a file mapping numerical Section numbers to relative URLS.

    The map file contains two or three fields per line:
    numeric_id  relative_URL  [; comment/documentation]

    The numeric_id is the id used to look up the entry in
    DisplaySection()/DisplayBlock(). The relative_URL is a filename of
    an html file, relative to the help directory. The optional
    comment/documentation field (after a ';') is used for keyword
    searches, so some meaningful text here does not hurt.
    If the documentation itself contains a ';', only the part before
    that will be displayed in the listbox, but all of it used for search.

    Lines starting with ';' will be ignored.

    @library{wxcore}
    @category{help}

    @see wxHelpController
*/
class wxExtHelpController : public wxHelpControllerBase
{
public:
    wxExtHelpController(wxWindow* parentWindow = NULL);
    virtual ~wxExtHelpController();

    /**
        Tell it which browser to use.
        The Netscape support will check whether Netscape is already
        running (by looking at the .netscape/lock file in the user's
        home directory) and tell it to load the page into the existing window.

        @param viewer
            The command to call a browser/html viewer.
        @param flags
            Set this to wxHELP_NETSCAPE if the browser is some variant of Netscape.
    */
    virtual void SetViewer(const wxString& viewer = wxEmptyString,
                           long flags = wxHELP_NETSCAPE);

    /**
        This must be called to tell the controller where to find the
        documentation.
        If a locale is set, look in file/localename, i.e.
        If passed "/usr/local/myapp/help" and the current wxLocale is
        set to be "de", then look in "/usr/local/myapp/help/de/"
        first and fall back to "/usr/local/myapp/help" if that
        doesn't exist.

        @param dir
            directory name where to fine the help files

        @return @true on success
    */
    virtual bool Initialize(const wxString& dir);

    /**
        If file is "", reloads file given in Initialize.

        @param file
            Name of help directory.

        @return @true on success
    */
    virtual bool LoadFile(const wxString& file = wxEmptyString);

    /**
        Display list of all help entries.

        @return @true on success
    */
    virtual bool DisplayContents();

    /**
        Display help for id sectionNo.

        @return @true on success
    */
    virtual bool DisplaySection(int sectionNo);

    /**
        Display help for id sectionNo -- identical with DisplaySection().

        @return @true on success
    */
    virtual bool DisplaySection(const wxString& section);

    /**
        Display help for URL (using DisplayHelp) or keyword (using KeywordSearch)

        @return @true on success
    */
    virtual bool DisplayBlock(long blockNo);

    /**
        Search comment/documentation fields in map file and present a
        list to chose from.

        @param k
            string to search for, empty string will list all entries

        @param mode
            optional parameter allows the search the index (wxHELP_SEARCH_INDEX)
            but this currently only supported by the wxHtmlHelpController.

        @return @true on success
    */
    virtual bool KeywordSearch(const wxString& k,
                                wxHelpSearchMode mode = wxHELP_SEARCH_ALL);

    /**
        Does nothing.
    */
    virtual bool Quit();

    /**
        Does nothing.
    */
    virtual void OnQuit();

    /**
        Call the browser using a relative URL.
    */
    virtual bool DisplayHelp(const wxString& relativeURL) ;

    /**
        Allows one to override the default settings for the help frame.
    */
    virtual void SetFrameParameters(const wxString& titleFormat,
                                    const wxSize& size,
                                    const wxPoint& pos = wxDefaultPosition,
                                    bool newFrameEachTime = false);

    /**
        Obtains the latest settings used by the help frame and the help frame.
    */
    virtual wxFrame *GetFrameParameters(wxSize *size = NULL,
                                        wxPoint *pos = NULL,
                                        bool *newFrameEachTime = NULL);
};

