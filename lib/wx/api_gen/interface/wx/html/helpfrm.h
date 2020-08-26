/////////////////////////////////////////////////////////////////////////////
// Name:        html/helpfrm.h
// Purpose:     interface of wxHtmlHelpFrame
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/// style flags for the Help Frame
#define wxHF_TOOLBAR                0x0001
#define wxHF_CONTENTS               0x0002
#define wxHF_INDEX                  0x0004
#define wxHF_SEARCH                 0x0008
#define wxHF_BOOKMARKS              0x0010
#define wxHF_OPEN_FILES             0x0020
#define wxHF_PRINT                  0x0040
#define wxHF_FLAT_TOOLBAR           0x0080
#define wxHF_MERGE_BOOKS            0x0100
#define wxHF_ICONS_BOOK             0x0200
#define wxHF_ICONS_BOOK_CHAPTER     0x0400
#define wxHF_ICONS_FOLDER           0x0000 // this is 0 since it is default
#define wxHF_DEFAULT_STYLE          (wxHF_TOOLBAR | wxHF_CONTENTS | \
                                     wxHF_INDEX | wxHF_SEARCH | \
                                     wxHF_BOOKMARKS | wxHF_PRINT)


/**
    @class wxHtmlHelpFrame

    This class is used by wxHtmlHelpController to display help.
    It is an internal class and should not be used directly - except for the case
    when you're writing your own HTML help controller.

    @library{wxhtml}
    @category{help,html}
*/
class wxHtmlHelpFrame : public wxFrame
{
public:
    wxHtmlHelpFrame(wxHtmlHelpData* data = NULL);

    /**
        Constructor.

        For the possible values of @a style, please see wxHtmlHelpController.
    */
    wxHtmlHelpFrame(wxWindow* parent, wxWindowID id,
                    const wxString& title = wxEmptyString,
                    int style = wxHF_DEFAULT_STYLE,
                    wxHtmlHelpData* data = NULL,
                    wxConfigBase* config = NULL,
                    const wxString& rootpath = wxEmptyString);

    /**
        You may override this virtual method to add more buttons to the help window's
        toolbar. @a toolBar is a pointer to the toolbar and @a style is the style
        flag as passed to the Create() method.

        wxToolBar::Realize is called immediately after returning from this function.
    */
    virtual void AddToolbarButtons(wxToolBar* toolBar, int style);

    /**
        Creates the frame. See @ref wxHtmlHelpFrame() "the constructor"
        for a description of the parameters.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& title = wxEmptyString, int style = wxHF_DEFAULT_STYLE,
                wxConfigBase* config = NULL,
                const wxString& rootpath = wxEmptyString);

    /**
        Returns the help controller associated with the frame.
    */
    wxHtmlHelpController* GetController() const;

    /**
        Sets the help controller associated with the frame.
    */
    void SetController(wxHtmlHelpController* controller);

    /**
        Sets the frame's title format.

        @a format must contain exactly one "%s" (it will be replaced by the page title).
    */
    void SetTitleFormat(const wxString& format);
};

