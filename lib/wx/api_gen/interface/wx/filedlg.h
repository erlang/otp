/////////////////////////////////////////////////////////////////////////////
// Name:        filedlg.h
// Purpose:     interface of wxFileDialog
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

enum
{
    wxFD_OPEN              = 0x0001,
    wxFD_SAVE              = 0x0002,
    wxFD_OVERWRITE_PROMPT  = 0x0004,
    wxFD_NO_FOLLOW         = 0x0008,
    wxFD_FILE_MUST_EXIST   = 0x0010,
    wxFD_CHANGE_DIR        = 0x0080,
    wxFD_PREVIEW           = 0x0100,
    wxFD_MULTIPLE          = 0x0200,
    wxFD_SHOW_HIDDEN       = 0x0400
};

#define wxFD_DEFAULT_STYLE      wxFD_OPEN

/**
    Default wildcard string used in wxFileDialog corresponding to all files.

    It is defined as "*.*" under MSW and "*" everywhere else.
*/
const char wxFileSelectorDefaultWildcardStr[];

/**
    @class wxFileDialog

    This class represents the file chooser dialog.

    The path and filename are distinct elements of a full file pathname.
    If path is ::wxEmptyString, the current directory will be used.
    If filename is ::wxEmptyString, no default filename will be supplied.
    The wildcard determines what files are displayed in the file selector,
    and file extension supplies a type extension for the required filename.

    The typical usage for the open file dialog is:
    @code
    void MyFrame::OnOpen(wxCommandEvent& WXUNUSED(event))
    {
        if (...current content has not been saved...)
        {
            if (wxMessageBox(_("Current content has not been saved! Proceed?"), _("Please confirm"),
                             wxICON_QUESTION | wxYES_NO, this) == wxNO )
                return;
            //else: proceed asking to the user the new file to open
        }

        wxFileDialog
            openFileDialog(this, _("Open XYZ file"), "", "",
                           "XYZ files (*.xyz)|*.xyz", wxFD_OPEN|wxFD_FILE_MUST_EXIST);

        if (openFileDialog.ShowModal() == wxID_CANCEL)
            return;     // the user changed idea...

        // proceed loading the file chosen by the user;
        // this can be done with e.g. wxWidgets input streams:
        wxFileInputStream input_stream(openFileDialog.GetPath());
        if (!input_stream.IsOk())
        {
            wxLogError("Cannot open file '%s'.", openFileDialog.GetPath());
            return;
        }

        ...
    }
    @endcode

    The typical usage for the save file dialog is instead somewhat simpler:
    @code
    void MyFrame::OnSaveAs(wxCommandEvent& WXUNUSED(event))
    {
        wxFileDialog
            saveFileDialog(this, _("Save XYZ file"), "", "",
                           "XYZ files (*.xyz)|*.xyz", wxFD_SAVE|wxFD_OVERWRITE_PROMPT);

        if (saveFileDialog.ShowModal() == wxID_CANCEL)
            return;     // the user changed idea...

        // save the current contents in the file;
        // this can be done with e.g. wxWidgets output streams:
        wxFileOutputStream output_stream(saveFileDialog.GetPath());
        if (!output_stream.IsOk())
        {
            wxLogError("Cannot save current contents in file '%s'.", saveFileDialog.GetPath());
            return;
        }

        ...
    }
    @endcode

    @remarks
    All implementations of the wxFileDialog provide a wildcard filter. Typing a filename
    containing wildcards (*, ?) in the filename text item, and clicking on Ok, will
    result in only those files matching the pattern being displayed.
    The wildcard may be a specification for multiple types of file with a description
    for each, such as:
    @code
         "BMP and GIF files (*.bmp;*.gif)|*.bmp;*.gif|PNG files (*.png)|*.png"
    @endcode
    It must be noted that wildcard support in the native Motif file dialog is quite
    limited: only one file type is supported, and it is displayed without the
    descriptive test; "BMP files (*.bmp)|*.bmp" is displayed as "*.bmp", and both
    "BMP files (*.bmp)|*.bmp|GIF files (*.gif)|*.gif" and "Image files|*.bmp;*.gif"
    are errors.
    On Mac macOS in the open file dialog the filter choice box is not shown by default.
    Instead all given wildcards are appplied at the same time: So in the above
    example all bmp, gif and png files are displayed. To enforce the
    display of the filter choice set the corresponding wxSystemOptions before calling
    the file open dialog:
    @code
         wxSystemOptions::SetOption(wxOSX_FILEDIALOG_ALWAYS_SHOW_TYPES, 1)
    @endcode
    But in contrast to Windows and Unix, where the file type choice filters only
    the selected files, on Mac macOS even in this case the dialog shows all files
    matching all file types. The files which does not match the currently selected
    file type are greyed out and are not selectable.

    @beginStyleTable
    @style{wxFD_DEFAULT_STYLE}
           Equivalent to @c wxFD_OPEN.
    @style{wxFD_OPEN}
           This is an open dialog; usually this means that the default
           button's label of the dialog is "Open". Cannot be combined with @c wxFD_SAVE.
    @style{wxFD_SAVE}
           This is a save dialog; usually this means that the default button's
           label of the dialog is "Save". Cannot be combined with @c wxFD_OPEN.
    @style{wxFD_OVERWRITE_PROMPT}
           For save dialog only: prompt for a confirmation if a file will be
           overwritten.
    @style{wxFD_NO_FOLLOW}
           Directs the dialog to return the path and file name of the selected
           shortcut file, not its target as it does by default. Currently this
           flag is only implemented in wxMSW and wxOSX (where it prevents
           aliases from being resolved). The non-dereferenced link path
           is always returned, even without this flag, under Unix and so using
           it there doesn't do anything. This flag was added in wxWidgets
           3.1.0.
    @style{wxFD_FILE_MUST_EXIST}
           For open dialog only: the user may only select files that actually
           exist. Notice that under macOS the file dialog with @c wxFD_OPEN
           style always behaves as if this style was specified, because it is
           impossible to choose a file that doesn't exist from a standard macOS
           file dialog.
    @style{wxFD_MULTIPLE}
           For open dialog only: allows selecting multiple files.
    @style{wxFD_CHANGE_DIR}
           Change the current working directory (when the dialog is dismissed)
           to the directory where the file(s) chosen by the user are.
    @style{wxFD_PREVIEW}
           Show the preview of the selected files (currently only supported by
           wxGTK).
    @style{wxFD_SHOW_HIDDEN}
          Show hidden files. This flag was added in wxWidgets 3.1.3
    @endStyleTable

    @library{wxcore}
    @category{cmndlg}

    @see @ref overview_cmndlg_file, ::wxFileSelector()
*/
class wxFileDialog : public wxDialog
{
public:
    /**
        Constructor. Use ShowModal() to show the dialog.

        @param parent
            Parent window.
        @param message
            Message to show on the dialog.
        @param defaultDir
            The default directory, or the empty string.
        @param defaultFile
            The default filename, or the empty string.
        @param wildcard
            A wildcard, such as "*.*" or "BMP files (*.bmp)|*.bmp|GIF files (*.gif)|*.gif".
            Note that the native Motif dialog has some limitations with respect to
            wildcards; see the Remarks section above.
        @param style
            A dialog style. See @c wxFD_* styles for more info.
        @param pos
            Dialog position. Not implemented.
        @param size
            Dialog size. Not implemented.
        @param name
            Dialog name. Not implemented.
    */
    wxFileDialog(wxWindow* parent,
                 const wxString& message = wxFileSelectorPromptStr,
                 const wxString& defaultDir = wxEmptyString,
                 const wxString& defaultFile = wxEmptyString,
                 const wxString& wildcard = wxFileSelectorDefaultWildcardStr,
                 long style = wxFD_DEFAULT_STYLE,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 const wxString& name = wxFileDialogNameStr);

    /**
        Destructor.
    */
    virtual ~wxFileDialog();

    /**
        Returns the path of the file currently selected in dialog.

        Notice that this file is not necessarily going to be accepted by the
        user, so calling this function mostly makes sense from an update UI
        event handler of a custom file dialog extra control to update its state
        depending on the currently selected file.

        Currently this function is fully implemented under GTK and MSW and
        always returns an empty string elsewhere.

        @since 2.9.5

        @return The path of the currently selected file or an empty string if
            nothing is selected.

        @see SetExtraControlCreator()
    */
    virtual wxString GetCurrentlySelectedFilename() const;

    /**
        Returns the file type filter index currently selected in dialog.

        Notice that this file type filter is not necessarily going to be the
        one finally accepted by the user, so calling this function mostly makes
        sense from an update UI event handler of a custom file dialog extra
        control to update its state depending on the currently selected file
        type filter.

        Currently this function is fully implemented only under MSW and
        always returns @c wxNOT_FOUND elsewhere.

        @since 3.1.3

        @return The 0-based index of the currently selected file type filter or
            wxNOT_FOUND if nothing is selected.

        @see SetExtraControlCreator()
        @see GetFilterIndex()
        @see SetFilterIndex()
    */
    virtual int GetCurrentlySelectedFilterIndex () const;

    /**
        Returns the default directory.
    */
    virtual wxString GetDirectory() const;

    /**
        If functions SetExtraControlCreator() and ShowModal() were called,
        returns the extra window. Otherwise returns @NULL.

        @since 2.9.0
    */
    wxWindow* GetExtraControl() const;

    /**
        Returns the default filename.

        @note This function can't be used with dialogs which have the @c wxFD_MULTIPLE style,
              use GetFilenames() instead.
    */
    virtual wxString GetFilename() const;

    /**
        Fills the array @a filenames with the names of the files chosen.

        This function should only be used with the dialogs which have @c wxFD_MULTIPLE style,
        use GetFilename() for the others.

        Note that under Windows, if the user selects shortcuts, the filenames
        include paths, since the application cannot determine the full path
        of each referenced file by appending the directory containing the shortcuts
        to the filename.
    */
    virtual void GetFilenames(wxArrayString& filenames) const;

    /**
        Returns the index into the list of filters supplied, optionally, in the
        wildcard parameter.

        Before the dialog is shown, this is the index which will be used when the
        dialog is first displayed.

        After the dialog is shown, this is the index selected by the user.
    */
    virtual int GetFilterIndex() const;

    /**
        Returns the message that will be displayed on the dialog.
    */
    virtual wxString GetMessage() const;

    /**
        Returns the full path (directory and filename) of the selected file.

        @note This function can't be used with dialogs which have the @c wxFD_MULTIPLE style,
              use GetPaths() instead.
    */
    virtual wxString GetPath() const;

    /**
        Fills the array @a paths with the full paths of the files chosen.

        This function should only be used with the dialogs which have @c wxFD_MULTIPLE style,
        use GetPath() for the others.
    */
    virtual void GetPaths(wxArrayString& paths) const;

    /**
        Returns the file dialog wildcard.
    */
    virtual wxString GetWildcard() const;

    /**
        Sets the default directory.
    */
    virtual void SetDirectory(const wxString& directory);

    /**
        The type of function used as an argument for SetExtraControlCreator().

        @since 2.9.0
    */
    typedef wxWindow *(*ExtraControlCreatorFunction)(wxWindow*);

    /**
        Customize file dialog by adding extra window, which is typically placed
        below the list of files and above the buttons.

        SetExtraControlCreator() can be called only once, before calling ShowModal().

        The @c creator function should take pointer to parent window (file dialog)
        and should return a window allocated with operator new.

        @since 2.9.0
    */
    bool SetExtraControlCreator(ExtraControlCreatorFunction creator);

    /**
        Sets the default filename.

        In wxGTK this will have little effect unless a default directory has previously been set.
    */
    virtual void SetFilename(const wxString& setfilename);

    /**
        Sets the default filter index, starting from zero.
    */
    virtual void SetFilterIndex(int filterIndex);

    /**
        Sets the message that will be displayed on the dialog.
    */
    virtual void SetMessage(const wxString& message);

    /**
        Sets the path (the combined directory and filename that will be returned when
        the dialog is dismissed).
    */
    virtual void SetPath(const wxString& path);

    /**
        Sets the wildcard, which can contain multiple file types, for example:
        "BMP files (*.bmp)|*.bmp|GIF files (*.gif)|*.gif".

        Note that the native Motif dialog has some limitations with respect to
        wildcards; see the Remarks section above.
    */
    virtual void SetWildcard(const wxString& wildCard);

    /**
        Shows the dialog, returning @c wxID_OK if the user pressed OK, and @c wxID_CANCEL
        otherwise.
    */
    virtual int ShowModal();
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_dialog */
//@{

/**
    Pops up a file selector box. In Windows, this is the common file selector
    dialog. In X, this is a file selector box with the same functionality. The
    path and filename are distinct elements of a full file pathname. If path
    is empty, the current directory will be used. If filename is empty, no
    default filename will be supplied. The wildcard determines what files are
    displayed in the file selector, and file extension supplies a type
    extension for the required filename. Flags may be a combination of
    wxFD_OPEN, wxFD_SAVE, wxFD_OVERWRITE_PROMPT or wxFD_FILE_MUST_EXIST.

    @note wxFD_MULTIPLE can only be used with wxFileDialog and not here since
          this function only returns a single file name.

    Both the Unix and Windows versions implement a wildcard filter. Typing a
    filename containing wildcards (*, ?) in the filename text item, and
    clicking on Ok, will result in only those files matching the pattern being
    displayed.

    The wildcard may be a specification for multiple types of file with a
    description for each, such as:

    @code
    "BMP files (*.bmp)|*.bmp|GIF files (*.gif)|*.gif"
    @endcode

    The application must check for an empty return value (the user pressed
    Cancel). For example:

    @code
    wxString filename = wxFileSelector("Choose a file to open");
    if ( !filename.empty() )
    {
        // work with the file
        ...
    }
    //else: cancelled by user
    @endcode

    @header{wx/filedlg.h}
*/
wxString wxFileSelector(const wxString& message,
                        const wxString& default_path = wxEmptyString,
                        const wxString& default_filename = wxEmptyString,
                        const wxString& default_extension = wxEmptyString,
                        const wxString& wildcard = wxFileSelectorDefaultWildcardStr,
                        int flags = 0,
                        wxWindow* parent = NULL,
                        int x = wxDefaultCoord,
                        int y = wxDefaultCoord);

/**
    An extended version of wxFileSelector()

    @header{wx/filedlg.h}
*/
wxString wxFileSelectorEx(const wxString& message = wxFileSelectorPromptStr,
                          const wxString& default_path = wxEmptyString,
                          const wxString& default_filename = wxEmptyString,
                          int *indexDefaultExtension = NULL,
                          const wxString& wildcard = wxFileSelectorDefaultWildcardStr,
                          int flags = 0,
                          wxWindow *parent = NULL,
                          int x = wxDefaultCoord,
                          int y = wxDefaultCoord);

/**
    Shows a file dialog asking the user for a file name for opening a file.

    @see wxFileSelector(), wxFileDialog

    @header{wx/filedlg.h}
*/
wxString wxLoadFileSelector(const wxString& what,
                            const wxString& extension,
                            const wxString& default_name = wxEmptyString,
                            wxWindow *parent = NULL);

/**
    Shows a file dialog asking the user for a file name for saving a file.

    @see wxFileSelector(), wxFileDialog

    @header{wx/filedlg.h}
*/
wxString wxSaveFileSelector(const wxString& what,
                            const wxString& extension,
                            const wxString& default_name = wxEmptyString,
                            wxWindow *parent = NULL);

//@}

