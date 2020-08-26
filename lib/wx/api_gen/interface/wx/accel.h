/////////////////////////////////////////////////////////////////////////////
// Name:        accel.h
// Purpose:     interface of wxAccelerator* classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/** wxAcceleratorEntry flags */
enum wxAcceleratorEntryFlags
{
    /** no modifiers */
    wxACCEL_NORMAL,

    /** hold Alt key down */
    wxACCEL_ALT,

    /** hold Ctrl key down, corresponds to Command key on macOS */
    wxACCEL_CTRL,

    /** hold Shift key down */
    wxACCEL_SHIFT,

    /** corresponds to real Ctrl key on macOS, identic to @c wxACCEL_CTRL on other platforms */
    wxACCEL_RAW_CTRL,

    /** deprecated, identic to @c wxACCEL_CTRL on all platforms. */
    wxACCEL_CMD
};


/**
    @class wxAcceleratorEntry

    An object used by an application wishing to create an accelerator table
    (see wxAcceleratorTable).

    @library{wxcore}
    @category{data}

    @see wxAcceleratorTable, wxWindow::SetAcceleratorTable
*/
class wxAcceleratorEntry
{
public:
    /**
        Constructor.

        @param flags
            A combination of the ::wxAcceleratorEntryFlags values, which
            indicates which modifier keys are held down.
        @param keyCode
            The keycode to be detected. See ::wxKeyCode for a full list of keycodes.
        @param cmd
            The menu or control command identifier (ID).
        @param item
            The menu item associated with this accelerator.
    */
    wxAcceleratorEntry(int flags = 0, int keyCode = 0, int cmd = 0,
                       wxMenuItem *item = NULL);

    /**
        Copy ctor.
    */
    wxAcceleratorEntry(const wxAcceleratorEntry& entry);

    /**
        Returns the command identifier for the accelerator table entry.
    */
    int GetCommand() const;

    /**
        Returns the flags for the accelerator table entry.
    */
    int GetFlags() const;

    /**
        Returns the keycode for the accelerator table entry.
    */
    int GetKeyCode() const;

    /**
        Returns the menu item associated with this accelerator entry.
    */
    wxMenuItem *GetMenuItem() const;

    /**
        Sets the accelerator entry parameters.

        @param flags
            A combination of the ::wxAcceleratorEntryFlags values, which
            indicates which modifier keys are held down.
        @param keyCode
            The keycode to be detected. See ::wxKeyCode for a full list of keycodes.
        @param cmd
            The menu or control command identifier (ID).
        @param item
            The menu item associated with this accelerator.
    */
    void Set(int flags, int keyCode, int cmd, wxMenuItem *item = NULL);

    /**
        Returns @true if this object is correctly initialized.
    */
    bool IsOk() const;

    /**
        Returns a textual representation of this accelerator.

        The returned string is of the form <code>[Alt+][Ctrl+][RawCtrl+][Shift+]Key</code>
        where the modifier keys are present only if the corresponding flag is
        set.
    */
    wxString ToString() const;

    /**
        Returns a textual representation of this accelerator which is
        appropriate for saving in configuration files.

        Unlike the string returned by ToString(), this one is never translated
        so, while it's not suitable for showing to the user, it can be used to
        uniquely identify the accelerator independently of the user language.

        The returned string can still be parsed by FromString().

        @since 2.9.4
    */
    wxString ToRawString() const;

    /**
        Parses the given string and sets the accelerator accordingly.

        @param str
            This string may be either in the same format as returned by
            ToString(), i.e. contain the accelerator itself only, or have the
            format of a full menu item text with i.e. <code>Label TAB
            Accelerator</code>. In the latter case, the part of the string
            before the TAB is ignored. Notice that the latter format is only
            supported for the compatibility with the previous wxWidgets
            versions and the new code should pass only the accelerator string
            itself to this function.

        @return @true if the given string correctly initialized this object
                (i.e. if IsOk() returns true after this call)
    */
    bool FromString(const wxString& str);


    wxAcceleratorEntry& operator=(const wxAcceleratorEntry& entry);
    bool operator==(const wxAcceleratorEntry& entry) const;
    bool operator!=(const wxAcceleratorEntry& entry) const;
};


/**
    @class wxAcceleratorTable

    An accelerator table allows the application to specify a table of keyboard
    shortcuts for menu or button commands.

    The object ::wxNullAcceleratorTable is defined to be a table with no data, and
    is the initial accelerator table for a window.

    Example:

    @code
    wxAcceleratorEntry entries[4];
    entries[0].Set(wxACCEL_CTRL, (int) 'N', ID_NEW_WINDOW);
    entries[1].Set(wxACCEL_CTRL, (int) 'X', wxID_EXIT);
    entries[2].Set(wxACCEL_SHIFT, (int) 'A', ID_ABOUT);
    entries[3].Set(wxACCEL_NORMAL, WXK_DELETE, wxID_CUT);

    wxAcceleratorTable accel(4, entries);
    frame->SetAcceleratorTable(accel);
    @endcode

    @remarks
    An accelerator takes precedence over normal processing and can be a convenient
    way to program some event handling. For example, you can use an accelerator table
    to enable a dialog with a multi-line text control to accept CTRL-Enter as meaning
    'OK'.

    @library{wxcore}
    @category{data}

    @stdobjects
    ::wxNullAcceleratorTable

    @see wxAcceleratorEntry, wxWindow::SetAcceleratorTable
*/
class wxAcceleratorTable : public wxObject
{
public:
    /**
        Default ctor.
    */
    wxAcceleratorTable();

    /**
        Initializes the accelerator table from an array of wxAcceleratorEntry.

        @param n
            Number of accelerator entries.
        @param entries
            The array of entries.

        @beginWxPerlOnly
        The wxPerl constructor accepts a list of either
        Wx::AcceleratorEntry objects or references to 3-element arrays
        [flags, keyCode, cmd] , like the parameters of
        Wx::AcceleratorEntry::new.
        @endWxPerlOnly
    */
    wxAcceleratorTable(int n, const wxAcceleratorEntry entries[]);

    /**
        Loads the accelerator table from a Windows resource (Windows only).

        @onlyfor{wxmsw}

        @param resource
            Name of a Windows accelerator.
    */
    wxAcceleratorTable(const wxString& resource);

    /**
        Destroys the wxAcceleratorTable object.
        See @ref overview_refcount_destruct for more info.
    */
    virtual ~wxAcceleratorTable();

    /**
        Returns @true if the accelerator table is valid.
    */
    bool IsOk() const;
};


// ============================================================================
// Global functions/macros
// ============================================================================

/**
    An empty accelerator table.
*/
wxAcceleratorTable wxNullAcceleratorTable;
