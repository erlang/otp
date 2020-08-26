/////////////////////////////////////////////////////////////////////////////
// Name:        dirdlg.h
// Purpose:     interface of wxDirDialog
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


#define wxDD_CHANGE_DIR         0x0100
#define wxDD_DIR_MUST_EXIST     0x0200
#define wxDD_MULTIPLE           0x0400
#define wxDD_SHOW_HIDDEN        0x0001

#define wxDD_NEW_DIR_BUTTON     0 // deprecated, on by default now,

#define wxDD_DEFAULT_STYLE      (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)

/**
    Initial folder for generic directory dialog.
*/
const char wxDirDialogDefaultFolderStr[] = "/";

/**
    Default message for directory selector dialog.
*/
const char wxDirSelectorPromptStr[] = "Select a directory";

/**
    Default name for directory selector dialog.
*/
const char wxDirDialogNameStr[] = "wxDirCtrl";

/**
    @class wxDirDialog

    This class represents the directory chooser dialog.

    @beginStyleTable
    @style{wxDD_DEFAULT_STYLE}
           Equivalent to a combination of wxDEFAULT_DIALOG_STYLE and
           wxRESIZE_BORDER.
    @style{wxDD_DIR_MUST_EXIST}
           The dialog will allow the user to choose only an existing folder.
           When this style is not given, a "Create new directory" button is
           added to the dialog (on Windows) or some other way is provided to
           the user to type the name of a new folder.
    @style{wxDD_CHANGE_DIR}
           Change the current working directory to the directory chosen by the
           user.
           @note This flag cannot be used with the @c wxDD_MULTIPLE style.
    @style{wxDD_MULTIPLE}
           Allow the user to select multiple directories.
           This flag is only available since wxWidgets 3.1.4
    @style{wxDD_SHOW_HIDDEN}
           Show hidden and system folders.
           This flag is only available since wxWidgets 3.1.4
    @endStyleTable

    Notice that @c wxRESIZE_BORDER has special side effect under Windows
    where two different directory selection dialogs are available and this
    style also implicitly selects the new version as the old one always has
    fixed size. As the new version is almost always preferable, it is
    recommended that @c wxRESIZE_BORDER style be always used.
    This is the case if the dialog is created with the default style value but
    if you need to use any additional styles you should still specify @c
    wxDD_DEFAULT_STYLE unless you explicitly need to use the old dialog version
    under Windows. E.g. do
    @code
        wxDirDialog dlg(NULL, "Choose input directory", "",
                        wxDD_DEFAULT_STYLE | wxDD_DIR_MUST_EXIST);
    @endcode
    instead of just using @c wxDD_DIR_MUST_EXIST style alone.

    @remarks MacOS 10.11+ does not display a title bar on the dialog. Use SetMessage()
             to change the string displayed to the user at the top of the dialog after creation.
             The SetTitle() method is provided for compatibility with pre-10.11 MacOS versions
             that do still support displaying the title bar.

    @library{wxcore}
    @category{cmndlg}

    @see @ref overview_cmndlg_dir, wxFileDialog
*/
class wxDirDialog : public wxDialog
{
public:
    /**
        Constructor. Use ShowModal() to show the dialog.

        @param parent
            Parent window.
        @param message
            Message to show on the dialog.
        @param defaultPath
            The default path, or the empty string.
        @param style
            The dialog style. See wxDirDialog
        @param pos
            Dialog position. Ignored under Windows.
        @param size
            Dialog size. Ignored under Windows.
        @param name
            The dialog name, not used.
    */
    wxDirDialog(wxWindow* parent,
                const wxString& message = wxDirSelectorPromptStr,
                const wxString& defaultPath = wxEmptyString,
                long style = wxDD_DEFAULT_STYLE,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                const wxString& name = wxDirDialogNameStr);

    /**
        Destructor.
    */
    virtual ~wxDirDialog();

    /**
        Returns the message that will be displayed on the dialog.
    */
    virtual wxString GetMessage() const;

    /**
        Returns the default or user-selected path.

        @note This function can't be used with dialogs which have the @c wxDD_MULTIPLE style,
              use GetPaths() instead.
    */
    virtual wxString GetPath() const;

    /**
        Fills the array @a paths with the full paths of the chosen directories.

        @note This function should only be used with the dialogs which have @c wxDD_MULTIPLE style,
              use GetPath() for the others.

        @since 3.1.4
    */
    virtual void GetPaths(wxArrayString& paths) const;

    /**
        Sets the message that will be displayed on the dialog.
    */
    virtual void SetMessage(const wxString& message);

    /**
        Sets the default path.
    */
    virtual void SetPath(const wxString& path);

    /**
        Shows the dialog, returning wxID_OK if the user pressed OK, and
        wxID_CANCEL otherwise.
    */
    int ShowModal();
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_dialog */
//@{

/**
    Pops up a directory selector dialog. The arguments have the same meaning
    as those of wxDirDialog::wxDirDialog(). The message is displayed at the
    top, and the default_path, if specified, is set as the initial selection.

    The application must check for an empty return value (if the user pressed
    Cancel). For example:

    @code
    const wxString& dir = wxDirSelector("Choose a folder");
    if ( !dir.empty() )
    {
        ...
    }
    @endcode

    @header{wx/dirdlg.h}
*/
wxString wxDirSelector(const wxString& message = wxDirSelectorPromptStr,
                       const wxString& default_path = wxEmptyString,
                       long style = 0,
                       const wxPoint& pos = wxDefaultPosition,
                       wxWindow* parent = NULL);

//@}

