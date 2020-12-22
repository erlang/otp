/////////////////////////////////////////////////////////////////////////////
// Name:        html/helpctrl.h
// Purpose:     interface of wxHtmlHelpController
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#define wxID_HTML_HELPFRAME   (wxID_HIGHEST + 1)

/// This style indicates that the window is
/// embedded in the application and must not be
/// destroyed by the help controller.
#define wxHF_EMBEDDED                0x00008000

/// Create a dialog for the help window.
#define wxHF_DIALOG                  0x00010000

/// Create a frame for the help window.
#define wxHF_FRAME                   0x00020000

/// Make the dialog modal when displaying help.
#define wxHF_MODAL                   0x00040000



/**
    @class wxHtmlHelpController

    This help controller provides an easy way of displaying HTML help in your
    application (see @sample{html}, test example).

    The help system is based on @b books (see wxHtmlHelpController::AddBook).
    A book is a logical section of documentation (for example "User's Guide" or
    "Programmer's Guide" or "C++ Reference" or "wxWidgets Reference").
    The help controller can handle as many books as you want.

    Although this class has an API compatible with other wxWidgets help controllers
    as documented by wxHelpController, it is recommended that you use the enhanced
    capabilities of wxHtmlHelpController's API.

    wxHTML uses Microsoft's HTML Help Workshop project files (.hhp, .hhk, .hhc) as
    its native format. The file format is described in @ref overview_html_helpformats.
    The directory @c helpfiles in the @sample{html} contains sample project files.

    Note that the Microsoft's HTML Help Workshop
    (http://www.microsoft.com/downloads/details.aspx?FamilyID=00535334-c8a6-452f-9aa0-d597d16580cc)
    also runs on other platforms using WINE (http://www.winehq.org/) and it can
    be used to create the .hpp, .hhk and .hhc files through a friendly GUI.
    The commercial tool HelpBlocks (http://www.helpblocks.com) can also create these files.

    @library{wxhtml}
    @category{help,html}

    @see wxBestHelpController, wxHtmlHelpFrame, wxHtmlHelpDialog,
         wxHtmlHelpWindow, wxHtmlModalHelp
*/
class wxHtmlHelpController : public wxHelpControllerBase
{
public:
    /**
        Constructor.

        @param style
            This is a combination of these flags:
            - wxHF_TOOLBAR: The help window has a toolbar.
            - wxHF_FLAT_TOOLBAR: The help window has a toolbar with flat buttons (aka coolbar).
            - wxHF_CONTENTS: The help window has a contents panel.
            - wxHF_INDEX: The help window has an index panel.
            - wxHF_SEARCH: The help window has a search panel.
            - wxHF_BOOKMARKS: The help window has bookmarks controls.
            - wxHF_OPEN_FILES: Allows user to open arbitrary HTML document.
            - wxHF_PRINT: The toolbar contains "print" button.
            - wxHF_MERGE_BOOKS: The contents pane does not show book nodes.
              All books are merged together and appear as single book to the user.
            - wxHF_ICONS_BOOK: All nodes in contents pane have a book icon.
              This is how Microsoft's HTML help viewer behaves.
            - wxHF_ICONS_FOLDER: Book nodes in contents pane have a book icon, book's
              sections have a folder icon. This is the default.
            - wxHF_ICONS_BOOK_CHAPTER: Both book nodes and nodes of top-level
              sections of a book (i.e. chapters) have a book icon, all other sections
              (sections, subsections, ...) have a folder icon.
            - wxHF_EMBEDDED: Specifies that the help controller controls an embedded
              window of class wxHtmlHelpWindow that should not be destroyed when
              the controller is destroyed.
            - wxHF_DIALOG: Specifies that the help controller should create a
              dialog containing the help window.
            - wxHF_FRAME: Specifies that the help controller should create a frame
              containing the help window.
              This is the default if neither wxHF_DIALOG nor wxHF_EMBEDDED is specified.
            - wxHF_MODAL: Specifies that the help controller should create a modal
              dialog containing the help window (used with the wxHF_DIALOG style).
            - wxHF_DEFAULT_STYLE: wxHF_TOOLBAR | wxHF_CONTENTS | wxHF_INDEX |
              wxHF_SEARCH | wxHF_BOOKMARKS | wxHF_PRINT
        @param parentWindow
            This is an optional window to be used as the parent for the help window.
    */
    wxHtmlHelpController(int style = wxHF_DEFAULT_STYLE,
                         wxWindow* parentWindow = NULL);
    wxHtmlHelpController(wxWindow* parentWindow, int style = wxHF_DEFAULT_STYLE);


    /**
        Adds a book (i.e. a @ref overview_html_helpformats ".hhp file"; an HTML Help
        Workshop project file) into the list of loaded books.

        This must be called at least once before displaying any help.
        @a bookFile or @a bookUrl may be either @c ".hhp" file or a ZIP archive
        that contains an arbitrary number of @c ".hhp" files in its top-level
        directory.
        This ZIP archive must have @c ".zip" or @c ".htb" extension (the latter
        stands for "HTML book"). In other words,
        @code
        AddBook(wxFileName("help.zip"))
        @endcode
        is possible and is the recommended way.

        @param bookFile
            Help book filename. It is recommended to use this prototype
            instead of the one taking URL, because it is less error-prone.
        @param showWaitMsg
            If @true then a decoration-less window with progress message is displayed.
    */
    bool AddBook(const wxFileName& bookFile, bool showWaitMsg = false);

    /**
        Adds a book (i.e. a @ref overview_html_helpformats ".hhp file"; an HTML Help
        Workshop project file) into the list of loaded books.

        See the other overload for additional info.

        @param bookUrl
            Help book URL (note that syntax of filename and URL is
            different on most platforms).
        @param showWaitMsg
            If @true then a decoration-less window with progress message is displayed.
    */
    bool AddBook(const wxString& bookUrl, bool showWaitMsg = false);

    /**
        Displays page @a x.
        This is THE important function - it is used to display the help in application.
        You can specify the page in many ways:
        - as direct filename of HTML document
        - as chapter name (from contents) or as a book name
        - as some word from index
        - even as any word (will be searched)

        Looking for the page runs in these steps:
        -# try to locate file named x (if x is for example "doc/howto.htm")
        -# try to open starting page of book named x
        -# try to find x in contents (if x is for example "How To ...")
        -# try to find x in index (if x is for example "How To ...")
        -# switch to Search panel and start searching
    */
    bool Display(const wxString& x);

    /**
        @overload

        This alternative form is used to search help contents by numeric IDs.
    */
    bool Display(int id);

    /**
        Displays help window and focuses contents panel.
    */
    virtual bool DisplayContents();

    /**
        Displays help window and focuses index panel.
    */
    bool DisplayIndex();

    /**
        Displays the help window, focuses search panel and starts searching.
        Returns @true if the keyword was found. Optionally it searches through the
        index (mode = @c wxHELP_SEARCH_INDEX), default the content
        (mode = @c wxHELP_SEARCH_ALL).

        @note
            KeywordSearch() searches only pages listed in @c ".hhc" file(s).
            You should list all pages in the contents file.
    */
    virtual bool KeywordSearch(const wxString& keyword,
                               wxHelpSearchMode mode = wxHELP_SEARCH_ALL);

    /**
        Reads the controller's setting (position of window, etc.)
    */
    virtual void ReadCustomization(wxConfigBase* cfg,
                                   const wxString& path = wxEmptyString);

    /**
        Sets whether the help frame should prevent application from exiting
        if it's the only remaining top level window.

        @param enable
            If @true, the application will not quit unless the help frame is
            closed. Default is @false, i.e. the application does exit if only
            the help window remains opened.

        @see wxApp::SetExitOnFrameDelete()

        @since 2.9.2
    */
    void SetShouldPreventAppExit(bool enable);

    /**
        Sets the path for storing temporary files - cached binary versions of index and
        contents files.

        These binary forms are much faster to read. Default value is empty string
        (empty string means that no cached data are stored). Note that these files
        are @e not deleted when program exits.

        Once created these cached files will be used in all subsequent executions
        of your application. If cached files become older than corresponding @c ".hhp"
        file (e.g. if you regenerate documentation) it will be refreshed.
    */
    void SetTempDir(const wxString& path);

    /**
        Sets format of title of the frame.
        Must contain exactly one "%s" (for title of displayed HTML page).
    */
    void SetTitleFormat(const wxString& format);

    /**
        Associates the @a config object with the controller.

        If there is associated config object, wxHtmlHelpController automatically
        reads and writes settings (including wxHtmlWindow's settings) when needed.
        The only thing you must do is create wxConfig object and call UseConfig().

        If you do not use UseConfig(), wxHtmlHelpController will use the default
        wxConfig object if available (for details see wxConfigBase::Get and
        wxConfigBase::Set).
    */
    void UseConfig(wxConfigBase* config,
                   const wxString& rootpath = wxEmptyString);

    /**
        Stores controllers setting (position of window etc.)
    */
    virtual void WriteCustomization(wxConfigBase* cfg,
                                    const wxString& path = wxEmptyString);

    /**
       Get the current help window
    */
    wxHtmlHelpWindow* GetHelpWindow();

    /**
       Set the help window to be managed by this controller.  This makes it
    possible to have a help window that might not be in a wxHtmlHelpFrame or
    dialog but is embedded in some other window in the application.  Be sure
    to use the wxHF_EMBEDDED style in this case.
    */
    void SetHelpWindow(wxHtmlHelpWindow* helpWindow);

    /**
       Returns the current help frame.  (May be NULL.)
    */
    wxHtmlHelpFrame* GetFrame();

    /**
       Returns the current help dialog. (May be NULL.)
    */
    wxHtmlHelpDialog* GetDialog();



protected:

    /**
        This protected virtual method may be overridden so that when specifying the
        @c wxHF_DIALOG style, the controller uses a different dialog.
    */
    virtual wxHtmlHelpDialog* CreateHelpDialog(wxHtmlHelpData* data);

    /**
        This protected virtual method may be overridden so that the controller
        uses a different frame.
    */
    virtual wxHtmlHelpFrame* CreateHelpFrame(wxHtmlHelpData* data);
};



/**
    @class wxHtmlModalHelp

    This class uses wxHtmlHelpController to display help in a modal dialog.
    This is useful on platforms such as wxMac where if you display help from a
    modal dialog, the help window must itself be a modal dialog.

    Create objects of this class on the stack, for example:

    @code
        // The help can be browsed during the lifetime of this object; when the
        // user quits the help, program execution will continue.
        wxHtmlModalHelp help(parent, "help", "My topic");
    @endcode

    @library{wxhtml}
    @category{help,html}
*/
class wxHtmlModalHelp
{
public:
    /**
        The ctor.

        @param parent
            is the parent of the dialog.
        @param helpFile
            is the HTML help file to show.
        @param topic
            is an optional topic. If this is empty, the help contents will be shown.
        @param style
            is a combination of the flags described in the wxHtmlHelpController
            documentation.
    */
    wxHtmlModalHelp(wxWindow* parent, const wxString& helpFile,
                    const wxString& topic = wxEmptyString,
                    int style = wxHF_DEFAULT_STYLE | wxHF_DIALOG | wxHF_MODAL);
};

