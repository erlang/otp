///////////////////////////////////////////////////////////////////////////////
// Name:        wx/generic/aboutdlgg.h
// Purpose:     generic wxAboutBox() implementation
// Author:      eranon
// Created:     2012-09-25
// Copyright:   (c) 2012 wxWidgets development team
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    @class wxGenericAboutDialog

    This class defines a customizable @e About dialog.

    Note that if you don't need customization, you should use the global
    wxAboutBox() function that is both easier to use and shows the native
    dialog if available.

    To use this class, you need to derive your own class from it and override
    the virtual method DoAddCustomControls().

    To instantiate an object from your wxGenericAboutDialog-based class, you
    can use either the default constructor followed by a call to Create(), or
    directly using the alternate constructor. In either case, you have to
    prepare a wxAboutDialogInfo containing standard information to display in
    an about-box.

    Example of usage, MyAboutDlg being a class derived from wxGenericAboutDialog:
    @code
    void MyFrame::OnAbout(wxCommandEvent& WXUNUSED(event))
    {
        wxAboutDialogInfo aboutInfo;

        aboutInfo.SetName("MyApp");
        aboutInfo.SetVersion(MY_APP_VERSION_STRING);
        aboutInfo.SetDescription(_("My wxWidgets-based application!"));
        aboutInfo.SetCopyright("(C) 1992-2020");
        aboutInfo.SetWebSite("http://myapp.org");
        aboutInfo.AddDeveloper("My Self");

        MyAboutDlg dlgAbout(aboutInfo, this);
        dlgAbout.ShowModal();
    }
    @endcode

    @library{wxcore}
    @category{cmndlg}

    @see wxAboutDialogInfo
*/
class wxGenericAboutDialog
{
public:
    /**
        Default constructor, Create() must be called later.
    */
    wxGenericAboutDialog();

    /**
        Creates the dialog and initializes it with the given information.
    */
    wxGenericAboutDialog(const wxAboutDialogInfo& info, wxWindow* parent = NULL);

    /**
        Initializes the dialog created using the default constructor.
    */
    bool Create(const wxAboutDialogInfo& info, wxWindow* parent = NULL);

protected:
    /**
        This virtual method may be overridden to add more controls to the
        dialog.

        Use the protected AddControl(), AddText() and AddCollapsiblePane()
        methods to add custom controls.

        This method is called during the dialog creation and you don't need to
        call it, only to override it.
    */
    virtual void DoAddCustomControls() { }

    /**
        Add arbitrary control to the sizer content with the specified flags.

        For example, here is how to add an expandable line with a border of 3
        pixels, then a line of text:
        @code
        AddControl(new wxStaticLine(this), wxSizerFlags().Expand().Border(wxALL, 3));

        AddText(_("This line is just an example of custom text."));
        @endcode
    */
    void AddControl(wxWindow *win, const wxSizerFlags& flags);

    /**
        Add arbitrary control to the sizer content and centre it.
    */
    void AddControl(wxWindow *win);

    /**
        Add the given (not empty) text to the sizer content.
    */
    void AddText(const wxString& text);

    /**
        Add a wxCollapsiblePane containing the given text.
    */
    void AddCollapsiblePane(const wxString& title, const wxString& text);
};

/**
    Show generic about dialog.

    This function does the same thing as wxAboutBox() except that it always
    uses the generic wxWidgets version of the dialog instead of the native one.
*/
void wxGenericAboutBox(const wxAboutDialogInfo& info, wxWindow* parent = NULL);
