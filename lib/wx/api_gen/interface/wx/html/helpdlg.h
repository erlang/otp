/////////////////////////////////////////////////////////////////////////////
// Name:        html/helpdlg.h
// Purpose:     interface of wxHtmlHelpDialog
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxHtmlHelpDialog

    This class is used by wxHtmlHelpController to display help.
    It is an internal class and should not be used directly - except for the case
    when you're writing your own HTML help controller.

    @library{wxhtml}
    @category{help,html}
*/
class wxHtmlHelpDialog : public wxDialog
{
public:
    wxHtmlHelpDialog(wxHtmlHelpData* data = NULL);

    /**
        Constructor.

        For the possible values of @a style, please see wxHtmlHelpController.
    */
    wxHtmlHelpDialog(wxWindow* parent, wxWindowID id,
                     const wxString& title = wxEmptyString,
                     int style = wxHF_DEFAULT_STYLE,
                     wxHtmlHelpData* data = NULL);

    /**
        You may override this virtual method to add more buttons to the help window's
        toolbar. @a toolBar is a pointer to the toolbar and @a style is the style
        flag as passed to the Create() method.

        wxToolBar::Realize is called immediately after returning from this function.
    */
    virtual void AddToolbarButtons(wxToolBar* toolBar, int style);

    /**
        Creates the dialog. See @ref wxHtmlHelpDialog() "the constructor"
        for a description of the parameters.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& title = wxEmptyString,
                int style = wxHF_DEFAULT_STYLE);

    /**
        Returns the help controller associated with the dialog.
    */
    wxHtmlHelpController* GetController() const;

    /**
        Sets the help controller associated with the dialog.
    */
    void SetController(wxHtmlHelpController* controller);

    /**
        Sets the dialog's title format.

        @a format must contain exactly one "%s" (it will be replaced by the page title).
    */
    void SetTitleFormat(const wxString& format);
};

