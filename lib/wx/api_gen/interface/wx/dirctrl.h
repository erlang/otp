/////////////////////////////////////////////////////////////////////////////
// Name:        dirctrl.h
// Purpose:     interface of wxGenericDirCtrl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

enum
{
    // Only allow directory viewing/selection, no files
    wxDIRCTRL_DIR_ONLY       = 0x0010,
    // When setting the default path, select the first file in the directory
    wxDIRCTRL_SELECT_FIRST   = 0x0020,
    // Show the filter list
    wxDIRCTRL_SHOW_FILTERS   = 0x0040,
    // Use 3D borders on internal controls
    wxDIRCTRL_3D_INTERNAL    = 0x0080,
    // Editable labels
    wxDIRCTRL_EDIT_LABELS    = 0x0100,
    // Allow multiple selection
    wxDIRCTRL_MULTIPLE       = 0x0200,

    wxDIRCTRL_DEFAULT_STYLE  = wxDIRCTRL_3D_INTERNAL
};


/**
    @class wxGenericDirCtrl

    This control can  be used to place a directory listing (with optional
    files) on an arbitrary window.

    The control contains a wxTreeCtrl window representing the directory
    hierarchy, and optionally, a wxChoice window containing a list of filters.

    @beginStyleTable
    @style{wxDIRCTRL_DIR_ONLY}
           Only show directories, and not files.
    @style{wxDIRCTRL_3D_INTERNAL}
           Use 3D borders for internal controls. This is the default.
    @style{wxDIRCTRL_SELECT_FIRST}
           When setting the default path, select the first file in the
           directory.
    @style{wxDIRCTRL_SHOW_FILTERS}
           Show the drop-down filter list.
    @style{wxDIRCTRL_EDIT_LABELS}
           Allow the folder and file labels to be editable.
    @style{wxDIRCTRL_MULTIPLE}
           Allows multiple files and folders to be selected.
    @endStyleTable

    @library{wxcore}
    @category{ctrl}
    @appearance{genericdirctrl}

    @beginEventEmissionTable
    @event{EVT_DIRCTRL_SELECTIONCHANGED(id, func)}
          Selected directory has changed.
          Processes a @c wxEVT_DIRCTRL_SELECTIONCHANGED event type.
          Notice that this event is generated even for the changes done by the
          program itself and not only those done by the user.
          Available since wxWidgets 2.9.5.
    @event{EVT_DIRCTRL_FILEACTIVATED(id, func)}
          The user activated a file by double-clicking or pressing Enter.
          Available since wxWidgets 2.9.5.
    @endEventTable
*/
class wxGenericDirCtrl : public wxControl
{
public:
    /**
        Default constructor.
    */
    wxGenericDirCtrl();

    /**
        Main constructor.

        @param parent
            Parent window.
        @param id
            Window identifier.
        @param dir
            Initial folder.
        @param pos
            Position.
        @param size
            Size.
        @param style
            Window style. Please see wxGenericDirCtrl for a list of possible
            styles.
        @param filter
            A filter string, using the same syntax as that for wxFileDialog.
            This may be empty if filters are not being used. Example:
            @c "All files (*.*)|*.*|JPEG files (*.jpg)|*.jpg"
        @param defaultFilter
            The zero-indexed default filter setting.
        @param name
            The window name.
    */
    wxGenericDirCtrl(wxWindow* parent, wxWindowID id = wxID_ANY,
                     const wxString& dir = wxDirDialogDefaultFolderStr,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxDIRCTRL_DEFAULT_STYLE,
                     const wxString& filter = wxEmptyString,
                     int defaultFilter = 0,
                     const wxString& name = wxTreeCtrlNameStr);

    /**
        Destructor.
    */
    virtual ~wxGenericDirCtrl();

    /**
        Collapse the given @a path.
    */
    virtual bool CollapsePath(const wxString& path);

    /**
        Collapses the entire tree.
    */
    virtual void CollapseTree();

    /**
        Create function for two-step construction. See wxGenericDirCtrl() for
        details.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxString& dir = wxDirDialogDefaultFolderStr,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDIRCTRL_DEFAULT_STYLE,
                const wxString& filter = wxEmptyString, int defaultFilter = 0,
                const wxString& name = wxTreeCtrlNameStr);

    /**
        Tries to expand as much of the given @a path as possible, so that the
        filename or directory is visible in the tree control.
    */
    virtual bool ExpandPath(const wxString& path);

    /**
        Gets the default path.
    */
    virtual wxString GetDefaultPath() const;

    /**
        Gets selected filename path only (else empty string).

        This function doesn't count a directory as a selection.
    */
    virtual wxString GetFilePath() const;

    /**
        Fills the array @a paths with the currently selected filepaths.

        This function doesn't count a directory as a selection.
    */
    virtual void GetFilePaths(wxArrayString& paths) const;

    /**
        Returns the filter string.
    */
    virtual wxString GetFilter() const;

    /**
        Returns the current filter index (zero-based).
    */
    virtual int GetFilterIndex() const;

    /**
        Returns a pointer to the filter list control (if present).
    */
    virtual wxDirFilterListCtrl* GetFilterListCtrl() const;

    /**
        Gets the currently-selected directory or filename.
    */
    virtual wxString GetPath() const;

    /**
        Gets the path corresponding to the given tree control item.

        @since 2.9.5
    */
    wxString GetPath(wxTreeItemId itemId) const;

    /**
        Fills the array @a paths with the selected directories and filenames.
    */
    virtual void GetPaths(wxArrayString& paths) const;

    /**
        Returns the root id for the tree control.
    */
    virtual wxTreeItemId GetRootId();

    /**
        Returns a pointer to the tree control.
    */
    virtual wxTreeCtrl* GetTreeCtrl() const;

    /**
        Initializes variables.
    */
    virtual void Init();

    /**
        Collapse and expand the tree, thus re-creating it from scratch. May be
        used to update the displayed directory content.
    */
    virtual void ReCreateTree();

    /**
        Sets the default path.
    */
    virtual void SetDefaultPath(const wxString& path);

    /**
        Sets the filter string.
    */
    virtual void SetFilter(const wxString& filter);

    /**
        Sets the current filter index (zero-based).
    */
    virtual void SetFilterIndex(int n);

    /**
        Sets the current path.
    */
    virtual void SetPath(const wxString& path);

    /**
        @param show
            If @true, hidden folders and files will be displayed by the
            control. If @false, they will not be displayed.
    */
    virtual void ShowHidden(bool show);

    /**
        Selects the given item.

        In multiple selection controls, can be also used to deselect a
        currently selected item if the value of @a select is false.
        Existing selections are not changed. Only visible items can be
        (de)selected, otherwise use ExpandPath().
    */
    virtual void SelectPath(const wxString& path, bool select = true);

    /**
        Selects only the specified paths, clearing any previous selection.

        Only supported when wxDIRCTRL_MULTIPLE is set.
    */
    virtual void SelectPaths(const wxArrayString& paths);

    /**
        Removes the selection from all currently selected items.
    */
    virtual void UnselectAll();
};



class wxDirFilterListCtrl: public wxChoice
{
public:
    wxDirFilterListCtrl();
    wxDirFilterListCtrl(wxGenericDirCtrl* parent, wxWindowID id = wxID_ANY,
                        const wxPoint& pos = wxDefaultPosition,
                        const wxSize& size = wxDefaultSize,
                        long style = 0);
    bool Create(wxGenericDirCtrl* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0);

    virtual ~wxDirFilterListCtrl();

    void Init();

    //// Operations
    void FillFilterList(const wxString& filter, int defaultFilter);
};

wxEventType wxEVT_DIRCTRL_SELECTIONCHANGED;
wxEventType wxEVT_DIRCTRL_FILEACTIVATED;
