/////////////////////////////////////////////////////////////////////////////
// Name:        filepicker.h
// Purpose:     interface of wxFilePickerCtrl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#define wxFLP_OPEN                    0x0400
#define wxFLP_SAVE                    0x0800
#define wxFLP_OVERWRITE_PROMPT        0x1000
#define wxFLP_FILE_MUST_EXIST         0x2000
#define wxFLP_CHANGE_DIR              0x4000
#define wxFLP_SMALL                   wxPB_SMALL
#define wxFLP_USE_TEXTCTRL            (wxPB_USE_TEXTCTRL)
#define wxFLP_DEFAULT_STYLE           (wxFLP_OPEN|wxFLP_FILE_MUST_EXIST)

#define wxDIRP_DIR_MUST_EXIST         0x0008
#define wxDIRP_CHANGE_DIR             0x0010
#define wxDIRP_SMALL                  wxPB_SMALL
#define wxDIRP_USE_TEXTCTRL           (wxPB_USE_TEXTCTRL)
#define wxDIRP_DEFAULT_STYLE          (wxDIRP_DIR_MUST_EXIST)

wxEventType wxEVT_FILEPICKER_CHANGED;
wxEventType wxEVT_DIRPICKER_CHANGED;


/**
    @class wxFilePickerCtrl

    This control allows the user to select a file. The generic implementation is
    a button which brings up a wxFileDialog when clicked. Native implementation
    may differ but this is usually a (small) widget which give access to the
    file-chooser dialog.
    It is only available if @c wxUSE_FILEPICKERCTRL is set to 1 (the default).

    @beginStyleTable
    @style{wxFLP_DEFAULT_STYLE}
           The default style: includes wxFLP_OPEN | wxFLP_FILE_MUST_EXIST and,
           under wxMSW and wxOSX, wxFLP_USE_TEXTCTRL.
    @style{wxFLP_USE_TEXTCTRL}
           Creates a text control to the left of the picker button which is
           completely managed by the wxFilePickerCtrl and which can be used by
           the user to specify a path (see SetPath). The text control is
           automatically synchronized with button's value. Use functions
           defined in wxPickerBase to modify the text control.
    @style{wxFLP_OPEN}
           Creates a picker which allows the user to select a file to open.
    @style{wxFLP_SAVE}
           Creates a picker which allows the user to select a file to save.
    @style{wxFLP_OVERWRITE_PROMPT}
           Can be combined with wxFLP_SAVE only: ask confirmation to the user
           before selecting a file.
    @style{wxFLP_FILE_MUST_EXIST}
           Can be combined with wxFLP_OPEN only: the file selected in the popup
           wxFileDialog must be an existing file. Notice that it still remains
           possible for the user to enter a non-existent file name in the text
           control if @c wxFLP_USE_TEXTCTRL is also used, this flag is a hint
           for the user rather than a guarantee that the selected file does
           exist for the program.
    @style{wxFLP_CHANGE_DIR}
           Change current working directory on each user file selection change.
    @style{wxFLP_SMALL}
           Use smaller version of the control with a small "..." button instead
           of the normal "Browse" one. This flag is new since wxWidgets 2.9.3.
    @endStyleTable


    @beginEventEmissionTable{wxFileDirPickerEvent}
    @event{EVT_FILEPICKER_CHANGED(id, func)}
        The user changed the file selected in the control either using the
        button or using text control (see wxFLP_USE_TEXTCTRL; note that in
        this case the event is fired only if the user's input is valid,
        e.g. an existing file path if wxFLP_FILE_MUST_EXIST was given).
    @endEventTable

    @library{wxcore}
    @category{pickers}
    @appearance{filepickerctrl}

    @see wxFileDialog, wxFileDirPickerEvent
*/
class wxFilePickerCtrl : public wxPickerBase
{
public:
    wxFilePickerCtrl();

    /**
        Initializes the object and calls Create() with
        all the parameters.
    */
    wxFilePickerCtrl(wxWindow* parent, wxWindowID id,
                     const wxString& path = wxEmptyString,
                     const wxString& message = wxFileSelectorPromptStr,
                     const wxString& wildcard = wxFileSelectorDefaultWildcardStr,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxFLP_DEFAULT_STYLE,
                     const wxValidator& validator = wxDefaultValidator,
                     const wxString& name = wxFilePickerCtrlNameStr);

    /**
        Creates this widget with the given parameters.

        @param parent
            Parent window, must not be non-@NULL.
        @param id
            The identifier for the control.
        @param path
            The initial file shown in the control. Must be a valid path to a file or
            the empty string.
        @param message
            The message shown to the user in the wxFileDialog shown by the control.
        @param wildcard
            A wildcard which defines user-selectable files (use the same syntax as for
            wxFileDialog's wildcards).
        @param pos
            Initial position.
        @param size
            Initial size.
        @param style
            The window style, see wxFLP_* flags.
        @param validator
            Validator which can be used for additional data checks.
        @param name
            Control name.

        @return @true if the control was successfully created or @false if
                creation failed.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& path = wxEmptyString,
                const wxString& message = wxFileSelectorPromptStr,
                const wxString& wildcard = wxFileSelectorDefaultWildcardStr,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxFLP_DEFAULT_STYLE,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxFilePickerCtrlNameStr);

    /**
        Similar to GetPath() but returns the path of the currently selected
        file as a wxFileName object.
    */
    wxFileName GetFileName() const;

    /**
        Returns the absolute path of the currently selected file.
    */
    wxString GetPath() const;

    /**
        This method does the same thing as SetPath() but takes a wxFileName
        object instead of a string.
    */
    void SetFileName(const wxFileName& filename);

    /**
        Set the directory to show when starting to browse for files.

        This function is mostly useful for the file picker controls which have
        no selection initially to configure the directory that should be shown
        if the user starts browsing for files as otherwise the directory of
        initially selected file is used, which is usually the desired
        behaviour and so the directory specified by this function is ignored in
        this case.

        @since 2.9.4
     */
    void SetInitialDirectory(const wxString& dir);

    /**
        Sets the absolute path of the currently selected file.

        If the control uses @c wxFLP_FILE_MUST_EXIST and does not use
        @c wxFLP_USE_TEXTCTRL style, the @a filename must be a name of an
        existing file and will be simply ignored by the native wxGTK
        implementation if this is not the case (the generic implementation used
        under the other platforms accepts even invalid file names currently,
        but this is subject to change in the future, don't rely on being able
        to use non-existent paths with it).
    */
    void SetPath(const wxString& filename);
};



/**
    @class wxDirPickerCtrl

    This control allows the user to select a directory. The generic implementation
    is a button which brings up a wxDirDialog when clicked. Native implementation
    may differ but this is usually a (small) widget which give access to the
    dir-chooser dialog.
    It is only available if @c wxUSE_DIRPICKERCTRL is set to 1 (the default).

    @beginStyleTable
    @style{wxDIRP_DEFAULT_STYLE}
           The default style: includes wxDIRP_DIR_MUST_EXIST and, under wxMSW
           only, wxDIRP_USE_TEXTCTRL.
    @style{wxDIRP_USE_TEXTCTRL}
           Creates a text control to the left of the picker button which is
           completely managed by the wxDirPickerCtrl and which can be used by
           the user to specify a path (see SetPath). The text control is
           automatically synchronized with button's value. Use functions
           defined in wxPickerBase to modify the text control.
    @style{wxDIRP_DIR_MUST_EXIST}
           Creates a picker which allows selecting only existing directories in
           the popup wxDirDialog. Notice that, as with @c wxFLP_FILE_MUST_EXIST,
           it is still possible to enter a non-existent directory even when
           this file is specified if @c wxDIRP_USE_TEXTCTRL style is also used.
           Also note that if @c wxDIRP_USE_TEXTCTRL is not used, the native
           wxGTK implementation always uses this style as it doesn't support
           selecting non-existent directories.
    @style{wxDIRP_CHANGE_DIR}
           Change current working directory on each user directory selection change.
    @style{wxDIRP_SMALL}
           Use smaller version of the control with a small "..." button instead
           of the normal "Browse" one. This flag is new since wxWidgets 2.9.3.
    @endStyleTable

    @beginEventEmissionTable{wxFileDirPickerEvent}
    @event{EVT_DIRPICKER_CHANGED(id, func)}
        The user changed the directory selected in the control either using the
        button or using text control (see wxDIRP_USE_TEXTCTRL; note that in this
        case the event is fired only if the user's input is valid, e.g. an
        existing directory path).
    @endEventTable


    @library{wxcore}
    @category{pickers}
    @appearance{dirpickerctrl}

    @see wxDirDialog, wxFileDirPickerEvent
*/
class wxDirPickerCtrl : public wxPickerBase
{
public:
    wxDirPickerCtrl();

    /**
        Initializes the object and calls Create() with
        all the parameters.
    */
    wxDirPickerCtrl(wxWindow* parent, wxWindowID id,
                    const wxString& path = wxEmptyString,
                    const wxString& message = wxDirSelectorPromptStr,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize,
                    long style = wxDIRP_DEFAULT_STYLE,
                    const wxValidator& validator = wxDefaultValidator,
                    const wxString& name = wxDirPickerCtrlNameStr);

    /**
        Creates the widgets with the given parameters.

        @param parent
            Parent window, must not be non-@NULL.
        @param id
            The identifier for the control.
        @param path
            The initial directory shown in the control. Must be a valid path to a
            directory or the empty string.
        @param message
            The message shown to the user in the wxDirDialog shown by the control.
        @param pos
            Initial position.
        @param size
            Initial size.
        @param style
            The window style, see wxDIRP_* flags.
        @param validator
            Validator which can be used for additional date checks.
        @param name
            Control name.

        @return @true if the control was successfully created or @false if
                creation failed.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& path = wxEmptyString,
                const wxString& message = wxDirSelectorPromptStr,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDIRP_DEFAULT_STYLE,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxDirPickerCtrlNameStr);

    /**
        Returns the absolute path of the currently selected directory as a
        wxFileName object.
        This function is equivalent to GetPath().
    */
    wxFileName GetDirName() const;

    /**
        Returns the absolute path of the currently selected directory.
    */
    wxString GetPath() const;

    /**
        Just like SetPath() but this function takes a wxFileName object.
    */
    void SetDirName(const wxFileName& dirname);

    /**
        Set the directory to show when starting to browse for directories.

        This function is mostly useful for the directory picker controls which
        have no selection initially to configure the directory that should be
        shown if the user starts browsing for directories as otherwise the
        initially selected directory is used, which is usually the desired
        behaviour and so the directory specified by this function is ignored in
        this case.

        @since 2.9.4
     */
    void SetInitialDirectory(const wxString& dir);

    /**
        Sets the absolute path of the currently selected directory.

        If the control uses @c wxDIRP_DIR_MUST_EXIST and does not use
        @c wxDIRP_USE_TEXTCTRL style, the @a dirname must be a name of an
        existing directory and will be simply ignored by the native wxGTK
        implementation if this is not the case.
    */
    void SetPath(const wxString& dirname);
};



/**
    @class wxFileDirPickerEvent

    This event class is used for the events generated by
    wxFilePickerCtrl and by wxDirPickerCtrl.

    @beginEventTable{wxFileDirPickerEvent}
    @event{EVT_FILEPICKER_CHANGED(id, func)}
        Generated whenever the selected file changes.
    @event{EVT_DIRPICKER_CHANGED(id, func)}
        Generated whenever the selected directory changes.
    @endEventTable

    @library{wxcore}
    @category{events}

    @see wxFilePickerCtrl, wxDirPickerCtrl
*/
class wxFileDirPickerEvent : public wxCommandEvent
{
public:
    wxFileDirPickerEvent();

    /**
        The constructor is not normally used by the user code.
    */
    wxFileDirPickerEvent(wxEventType type, wxObject* generator,
                         int id,
                         const wxString& path);

    /**
        Retrieve the absolute path of the file/directory the user has just selected.
    */
    wxString GetPath() const;

    /**
        Set the absolute path of the file/directory associated with the event.
    */
    void SetPath(const wxString& path);
};

