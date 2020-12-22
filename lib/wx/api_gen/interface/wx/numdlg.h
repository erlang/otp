/////////////////////////////////////////////////////////////////////////////
// Name:        numdlg.h
// Purpose:     interface of wxNumberEntryDialog
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxNumberEntryDialog

    This class represents a dialog that requests a numeric input from the user.

    Currently it is implemented as a generic wxWidgets dialog under all
    platforms.

    You can use a convenience wxGetNumberFromUser() function instead of using
    this dialog.

    @library{wxcore}
    @category{cmndlg}

    @see @ref overview_cmndlg_numeric
*/
class wxNumberEntryDialog : public wxDialog
{
public:
    /**
        Default constructor.

        Call Create() to really create the dialog later.
     */
    wxNumberEntryDialog();

    /**
        Constructor.

        Use ShowModal() to show the dialog.

        See Create() method for parameter description.
    */
    wxNumberEntryDialog(wxWindow *parent,
                        const wxString& message,
                        const wxString& prompt,
                        const wxString& caption,
                        long value, long min, long max,
                        const wxPoint& pos = wxDefaultPosition);

    /**
        @param parent
            Parent window.
        @param message
            Message to show on the dialog.
        @param prompt
            The prompt of the dialog.
        @param caption
            The caption of the dialog.
        @param value
            The default value.
        @param min
            The minimal value.
        @param max
            The maximal value.
        @param pos
            Dialog position.
    */
    bool Create(wxWindow *parent,
                const wxString& message,
                const wxString& prompt,
                const wxString& caption,
                long value, long min, long max,
                const wxPoint& pos = wxDefaultPosition);

    /**
        Returns the value that the user has entered if the user has pressed OK,
        or the original value if the user has pressed Cancel.
    */
    long GetValue() const;
};


// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_dialog */
//@{

/**
    Shows a dialog asking the user for numeric input. The dialogs title is set
    to @c caption, it contains a (possibly) multiline @c message above the
    single line @c prompt and the zone for entering the number.

    The number entered must be in the range @c min to @c max (both of which
    should be positive) and @c value is the initial value of it. If the user
    enters an invalid value, it is forced to fall into the specified range. If
    the user cancels the dialog, the function returns -1. If it is important to
    distinguish between cancelling the dialog and actually entering -1 in it,
    i.e. if -1 is a valid input value, this convenience function can't be used
    and wxNumberEntryDialog should be used directly instead.

    Dialog is centered on its @c parent unless an explicit position is given
    in @c pos.

    @header{wx/numdlg.h}

    @see wxNumberEntryDialog
*/
long wxGetNumberFromUser(const wxString& message,
                         const wxString& prompt,
                         const wxString& caption,
                         long value,
                         long min = 0,
                         long max = 100,
                         wxWindow* parent = NULL,
                         const wxPoint& pos = wxDefaultPosition);

//@}
