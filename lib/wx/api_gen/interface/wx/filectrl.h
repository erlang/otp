/////////////////////////////////////////////////////////////////////////////
// Name:        filectrl.h
// Purpose:     interface of wxFileCtrl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

enum
{
    wxFC_OPEN              = 0x0001,
    wxFC_SAVE              = 0x0002,
    wxFC_MULTIPLE          = 0x0004,
    wxFC_NOSHOWHIDDEN      = 0x0008
};

#define wxFC_DEFAULT_STYLE wxFC_OPEN

/**
    @class wxFileCtrl

    This control allows the user to select a file.

    Two implementations of this class exist, one for Gtk and another generic
    one for all the other ports.

    This class is only available if @c wxUSE_FILECTRL is set to 1.

    @beginStyleTable
    @style{wxFC_DEFAULT_STYLE}
           The default style: wxFC_OPEN
    @style{wxFC_OPEN}
           Creates an file control suitable for opening files. Cannot be
           combined with wxFC_SAVE.
    @style{wxFC_SAVE}
           Creates an file control suitable for saving files. Cannot be
           combined with wxFC_OPEN.
    @style{wxFC_MULTIPLE}
           For open control only, Allows selecting multiple files. Cannot be
           combined with wxFC_SAVE
    @style{wxFC_NOSHOWHIDDEN}
           Hides the "Show Hidden Files" checkbox (Generic only)
    @endStyleTable

    @beginEventEmissionTable{wxFileCtrlEvent}
    @event{EVT_FILECTRL_FILEACTIVATED(id, func)}
        The user activated a file(by double-clicking or pressing Enter)
    @event{EVT_FILECTRL_SELECTIONCHANGED(id, func)}
        The user changed the current selection(by selecting or deselecting a file)
    @event{EVT_FILECTRL_FOLDERCHANGED(id, func)}
        The current folder of the file control has been changed
    @event{EVT_FILECTRL_FILTERCHANGED(id, func)}
        The current file filter of the file control has been changed.
        @since 2.9.1.

    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{filectrl}

    @nativeimpl{wxgtk}

    @see wxGenericDirCtrl
*/
class wxFileCtrl : public wxControl
{
public:
    wxFileCtrl();

    /**
        Constructs the window.

        @param parent
            Parent window, must not be non-@NULL.
        @param id
            The identifier for the control.
        @param defaultDirectory
            The initial directory shown in the control.
            Must be a valid path to a directory or the empty string.
            In case it is the empty string, the current working directory is used.
        @param defaultFilename
            The default filename, or the empty string.
        @param wildCard
            A wildcard specifying which files can be selected,
            such as "*.*" or "BMP files (*.bmp)|*.bmp|GIF files (*.gif)|*.gif".
        @param style
            The window style, see wxFC_* flags.
        @param pos
            Initial position.
        @param size
            Initial size.
        @param name
            Control name.

        @return @true if the control was successfully created or @false if
                 creation failed.
    */

    wxFileCtrl(wxWindow* parent, wxWindowID id,
               const wxString& defaultDirectory = wxEmptyString,
               const wxString& defaultFilename = wxEmptyString,
               const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
               long style = wxFC_DEFAULT_STYLE,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               const wxString& name = wxFileCtrlNameStr);

    /**
        Create function for two-step construction. See wxFileCtrl() for details.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& defaultDirectory = wxEmptyString,
                const wxString& defaultFilename = wxEmptyString,
                const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
                long style = wxFC_DEFAULT_STYLE, const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                const wxString& name = wxFileCtrlNameStr);

    /**
        Returns the current directory of the file control (i.e.\ the directory shown by it).
    */
    virtual wxString GetDirectory() const;

    /**
        Returns the currently selected filename.

        For the controls having the @c wxFC_MULTIPLE style, use GetFilenames() instead.
    */
    virtual wxString GetFilename() const;

    /**
        Fills the array @a filenames with the filenames only of selected items.

        This function should only be used with the controls having the @c wxFC_MULTIPLE
        style, use GetFilename() for the others.

        @remarks filenames is emptied first.
    */
    virtual void GetFilenames(wxArrayString& filenames) const;

    /**
        Returns the zero-based index of the currently selected filter.
    */
    virtual int GetFilterIndex() const;

    /**
        Returns the full path (directory and filename) of the currently selected file.
        For the controls having the @c wxFC_MULTIPLE style, use GetPaths() instead.
    */
    virtual wxString GetPath() const;

    /**
        Fills the array @a paths with the full paths of the files chosen.

        This function should be used with the controls having the @c wxFC_MULTIPLE style,
        use GetPath() otherwise.

        @remarks paths is emptied first.
    */
    virtual void GetPaths(wxArrayString& paths) const;

    /**
        Returns the current wildcard.
    */
    virtual wxString GetWildcard() const;

    /**
        Sets(changes) the current directory displayed in the control.

        @return Returns @true on success, @false otherwise.
    */
    virtual bool SetDirectory(const wxString& directory);

    /**
        Selects a certain file.

        @return Returns @true on success, @false otherwise
    */
    virtual bool SetFilename(const wxString& filename);

    /**
        Changes to a certain directory and selects a certain file.

        If @a path includes the directory part, it must exist, otherwise @false
        is returned and nothing else is done.

        @return Returns @true on success, @false otherwise
    */
    virtual bool SetPath(const wxString& path);

    /**
        Sets the current filter index, starting from zero.
    */
    virtual void SetFilterIndex(int filterIndex);

    /**
        Sets the wildcard, which can contain multiple file types, for example:
        "BMP files (*.bmp)|*.bmp|GIF files (*.gif)|*.gif"
    */
    virtual void SetWildcard(const wxString& wildCard);

    /**
        Sets whether hidden files and folders are shown or not.
    */
    virtual void ShowHidden(bool show);
};


wxEventType wxEVT_FILECTRL_SELECTIONCHANGED;
wxEventType wxEVT_FILECTRL_FILEACTIVATED;
wxEventType wxEVT_FILECTRL_FOLDERCHANGED;
wxEventType wxEVT_FILECTRL_FILTERCHANGED;


/**
    @class wxFileCtrlEvent

    A file control event holds information about events associated with
    wxFileCtrl objects.

    @beginEventTable{wxFileCtrlEvent}
    @event{EVT_FILECTRL_FILEACTIVATED(id, func)}
        The user activated a file(by double-clicking or pressing Enter)
    @event{EVT_FILECTRL_SELECTIONCHANGED(id, func)}
        The user changed the current selection(by selecting or deselecting a file)
    @event{EVT_FILECTRL_FOLDERCHANGED(id, func)}
        The current folder of the file control has been changed
    @event{EVT_FILECTRL_FILTERCHANGED(id, func)}
        The current file filter of the file control has been changed
    @endEventTable

    @library{wxcore}
    @category{events}
*/
class wxFileCtrlEvent : public wxCommandEvent
{
public:
    /**
        Constructor.
    */
    wxFileCtrlEvent(wxEventType type, wxObject *evtObject, int id);

    /**
        Returns the current directory.

        In case of a @b EVT_FILECTRL_FOLDERCHANGED, this method returns the new
        directory.
    */
    wxString GetDirectory() const;

    /**
        Returns the file selected (assuming it is only one file).
    */
    wxString GetFile() const;

    /**
        Returns the files selected.

        In case of a @b EVT_FILECTRL_SELECTIONCHANGED, this method returns the
        files selected after the event.
    */
    wxArrayString GetFiles() const;

    /**
        Returns the current file filter index.

        For a @b EVT_FILECTRL_FILTERCHANGED event, this method returns the new
        file filter index.

        @since 2.9.1
    */
    int GetFilterIndex() const;

    /**
        Sets the files changed by this event.
    */
    void SetFiles(const wxArrayString& files);


    /**
        Sets the directory of this event.
    */
    void SetDirectory( const wxString &directory );

    /**
        Sets the filter index changed by this event.

        @since 2.9.1
    */
    void SetFilterIndex(int index);
};

