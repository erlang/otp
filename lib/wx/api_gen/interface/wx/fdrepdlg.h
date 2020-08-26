/////////////////////////////////////////////////////////////////////////////
// Name:        fdrepdlg.h
// Purpose:     interface of wxFindDialogEvent, wxFindReplaceDialog
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    See wxFindDialogEvent::GetFlags().
*/
enum wxFindReplaceFlags
{
    /** downward search/replace selected (otherwise - upwards) */
    wxFR_DOWN       = 1,

    /** whole word search/replace selected */
    wxFR_WHOLEWORD  = 2,

    /** case sensitive search/replace selected (otherwise - case insensitive) */
    wxFR_MATCHCASE  = 4
};


/**
    These flags can be specified in wxFindReplaceDialog ctor or Create():
*/
enum wxFindReplaceDialogStyles
{
    /** replace dialog (otherwise find dialog) */
    wxFR_REPLACEDIALOG = 1,

    /** don't allow changing the search direction */
    wxFR_NOUPDOWN      = 2,

    /** don't allow case sensitive searching */
    wxFR_NOMATCHCASE   = 4,

    /** don't allow whole word searching */
    wxFR_NOWHOLEWORD   = 8
};


/**
    @class wxFindDialogEvent

    wxFindReplaceDialog events.

    @beginEventTable{wxFindDialogEvent}
    @event{EVT_FIND(id, func)}
        Find button was pressed in the dialog.
    @event{EVT_FIND_NEXT(id, func)}
        Find next button was pressed in the dialog.
    @event{EVT_FIND_REPLACE(id, func)}
        Replace button was pressed in the dialog.
    @event{EVT_FIND_REPLACE_ALL(id, func)}
        Replace all button was pressed in the dialog.
    @event{EVT_FIND_CLOSE(id, func)}
        The dialog is being destroyed, any pointers to it cannot be used any longer.
    @endEventTable

    @library{wxcore}
    @category{events}
*/
class wxFindDialogEvent : public wxCommandEvent
{
public:
    /**
        Constructor used by wxWidgets only.
    */
    wxFindDialogEvent(wxEventType commandType = wxEVT_NULL,
                      int id = 0);

    /**
        Return the pointer to the dialog which generated this event.
    */
    wxFindReplaceDialog* GetDialog() const;

    /**
        Return the string to find (never empty).
    */
    wxString GetFindString() const;

    /**
        Get the currently selected flags: this is the combination of
        the ::wxFindReplaceFlags enumeration values.
    */
    int GetFlags() const;

    /**
        Return the string to replace the search string with (only for replace and
        replace all events).
    */
    const wxString& GetReplaceString() const;
};

wxEventType wxEVT_FIND;
wxEventType wxEVT_FIND_NEXT;
wxEventType wxEVT_FIND_REPLACE;
wxEventType wxEVT_FIND_REPLACE_ALL;
wxEventType wxEVT_FIND_CLOSE;



/**
    @class wxFindReplaceData

    wxFindReplaceData holds the data for wxFindReplaceDialog.

    It is used to initialize the dialog with the default values and will keep the
    last values from the dialog when it is closed. It is also updated each time a
    wxFindDialogEvent is generated so instead of using the wxFindDialogEvent
    methods you can also directly query this object.

    Note that all @c SetXXX() methods may only be called before showing the
    dialog and calling them has no effect later.

    @library{wxcore}
    @category{cmndlg,data}
*/
class wxFindReplaceData : public wxObject
{
public:
    /**
        Constructor initializes the flags to default value (0).
    */
    wxFindReplaceData(wxUint32 flags = 0);

    /**
        Get the string to find.
    */
    const wxString& GetFindString() const;

    /**
        Get the combination of @c wxFindReplaceFlags values.
    */
    int GetFlags() const;

    /**
        Get the replacement string.
    */
    const wxString& GetReplaceString() const;

    /**
        Set the string to find (used as initial value by the dialog).
    */
    void SetFindString(const wxString& str);

    /**
        Set the flags to use to initialize the controls of the dialog.
    */
    void SetFlags(wxUint32 flags);

    /**
        Set the replacement string (used as initial value by the dialog).
    */
    void SetReplaceString(const wxString& str);
};



/**
    @class wxFindReplaceDialog

    wxFindReplaceDialog is a standard modeless dialog which is used to allow the
    user to search for some text (and possibly replace it with something else).

    The actual searching is supposed to be done in the owner window which is the
    parent of this dialog. Note that it means that unlike for the other standard
    dialogs this one @b must have a parent window. Also note that there is no
    way to use this dialog in a modal way; it is always, by design and
    implementation, modeless.

    Please see the @ref page_samples_dialogs sample for an example of using it.

    @library{wxcore}
    @category{cmndlg}
*/
class wxFindReplaceDialog : public wxDialog
{
public:
    wxFindReplaceDialog();

    /**
        After using default constructor Create() must be called.

        The @a parent and @a data parameters must be non-@NULL.
    */
    wxFindReplaceDialog(wxWindow* parent,
                        wxFindReplaceData* data,
                        const wxString& title,
                        int style = 0);

    /**
        Destructor.
    */
    virtual ~wxFindReplaceDialog();

    /**
        Creates the dialog; use wxWindow::Show to show it on screen.

        The @a parent and @a data parameters must be non-@NULL.
    */
    bool Create(wxWindow* parent, wxFindReplaceData* data,
                const wxString& title, int style = 0);

    /**
        Get the wxFindReplaceData object used by this dialog.
    */
    const wxFindReplaceData* GetData() const;
};

