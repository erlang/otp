/////////////////////////////////////////////////////////////////////////////
// Name:        wx/richmsgdlg.h
// Purpose:     interface of wxRichMessageDialog
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxRichMessageDialog

    Extension of wxMessageDialog with additional functionality.

    This class adds the possibility of using a checkbox (that is especially
    useful for implementing the "Don't ask me again" kind of dialogs) and an
    extra explanatory text which is initially collapsed and not shown to the
    user but can be expanded to show more information.

    Notice that currently the native dialog is used only under MSW when using
    Vista or later Windows version. Elsewhere, or for older versions of
    Windows, a generic implementation which is less familiar to the users is
    used. Because of this it's recommended to use this class only if you do
    need its extra functionality and use wxMessageDialog which does have native
    implementation under all platforms otherwise. However if you do need to put
    e.g. a checkbox in a dialog, you should definitely consider using this
    class instead of using your own custom dialog because it will have much
    better appearance at least under recent Windows versions.

    To use this class, you need to create the dialog object and call
    ShowCheckBox() and/or ShowDetailedText() to configure its contents.
    Other than that, it is used in exactly the same way as wxMessageDialog and
    supports all the styles supported by it. In particular, ShowModal() return
    value is the same as for wxMessageDialog. The only difference is that you
    need to use IsCheckBoxChecked() to examine the checkbox value if you had
    called ShowCheckBox().

    Here is a simple example:
    @code
    void MyFrame::ShowDialog()
    {
        if ( ... shouldn't show this dialog again ... )
            return;

        wxRichMessageDialog dlg(this, "Welcome to my wonderful program!");
        dlg.ShowCheckBox("Don't show welcome dialog again");
        dlg.ShowModal(); // return value ignored as we have "Ok" only anyhow

        if ( dlg.IsCheckBoxChecked() )
            ... make sure we won't show it again the next time ...
    }
    @endcode

    @since 2.9.2

    @library{wxcore}
    @category{cmndlg}

    @see @ref overview_cmndlg_msg
*/
class wxRichMessageDialog : public wxRichMessageDialogBase
{
public:
    /**
        Constructor specifying the rich message dialog properties.
        Works just like the constructor for wxMessageDialog.
    */
    wxRichMessageDialog(wxWindow* parent,
                        const wxString& message,
                        const wxString& caption = wxMessageBoxCaptionStr,
                        long style = wxOK | wxCENTRE);

    /**
        Shows a checkbox with a given label or hides it.

        @param checkBoxText
            If the parameter is non-empty a checkbox will be shown with that
            label, otherwise it will be hidden.
        @param checked
            The initial state of the checkbox.
     */
    void ShowCheckBox(const wxString& checkBoxText, bool checked = false);


    /**
        Retrieves the label for the checkbox.

        @return
            The label for the checkbox, will be the empty string if no
            checkbox is used.
    */
    wxString GetCheckBoxText() const;

    /**
        Shows or hides a detailed text and an expander that is used to
        show or hide the detailed text.

        @param detailedText
            The detailed text that can be expanded when the dialog is shown,
            if empty no detailed text will be used.
    */
    void ShowDetailedText(const wxString& detailedText);

    /**
        Retrieves the detailed text.

        @return
            The detailed text or empty if detailed text is not used.
     */
    wxString GetDetailedText() const;

    /**
        Shows or hides a footer text that is used at the bottom of
        the dialog together with an optional icon.

        @param footerText
            The footer text if empty no footer text will be used.

        @see SetFooterIcon(), GetFooterText()

        @since 3.1.1
    */
    void SetFooterText(const wxString& footerText);

    /**
        Retrieves the footer text.

        @return
            The footer text or empty if footer text is not used.

        @since 3.1.1
    */
    wxString GetFooterText() const;

    /**
        Specify the footer icon set together with the footer text.

        Valid values are @c wxICON_INFORMATION, @c wxICON_WARNING,
        @c wxICON_AUTH_NEEDED and @c wxICON_ERROR (notice that
        @c wxICON_QUESTION is not allowed here).

        @see GetFooterIcon(), SetFooterText()

        @since 3.1.1
    */
    void SetFooterIcon(int icon);

    /**
        Retrieves the footer icon.

        @return
            The footer icon or 0 if footer icon is not used.

        @see SetFooterIcon()

        @since 3.1.1
    */
    int GetFooterIcon() const;

    /**
        Retrieves the state of the checkbox.

        If this method is called before showing the dialog, the initial value
        of the checkbox, as set by ShowCheckBox() is used. If it is called
        after calling wxDialog::ShowModal(), the value set by the user is
        returned.

        @return @true if the checkbox is checked or @false if not.
    */
    bool IsCheckBoxChecked() const;

    /**
        Shows the dialog, returning one of wxID_OK, wxID_CANCEL, wxID_YES, wxID_NO.

        IsCheckBoxChecked() can be called afterwards to retrieve the value of the
        check box if one was used.
    */
    virtual int ShowModal();
};
