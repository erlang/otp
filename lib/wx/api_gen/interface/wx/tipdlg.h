/////////////////////////////////////////////////////////////////////////////
// Name:        tipdlg.h
// Purpose:     interface of wxTipProvider
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxTipProvider

    This is the class used together with wxShowTip() function.
    It must implement wxTipProvider::GetTip function and return the
    current tip from it (different tip each time it is called).

    You will never use this class yourself, but you need it to show startup tips
    with wxShowTip. Also, if you want to get the tips text from elsewhere than a
    simple text file, you will want to derive a new class from wxTipProvider and
    use it instead of the one returned by wxCreateFileTipProvider().

    @library{wxcore}
    @category{misc}

    @see @ref overview_tips, ::wxShowTip
*/
class wxTipProvider
{
public:
    /**
        Constructor.

        @param currentTip
            The starting tip index.
    */
    wxTipProvider(size_t currentTip);

    virtual ~wxTipProvider();

    /**
        Returns the index of the current tip (i.e.\ the one which would be returned by GetTip()).

        The program usually remembers the value returned by this function after calling
        wxShowTip(). Note that it is not the same as the value which was passed to
        wxShowTip + 1 because the user might have pressed the "Next" button in
        the tip dialog.
    */
    size_t GetCurrentTip() const;

    /**
        Return the text of the current tip and pass to the next one.
        This function is pure virtual, it should be implemented in the derived classes.
    */
    virtual wxString GetTip() = 0;
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_dialog */
//@{

/**
    This function creates a wxTipProvider which may be used with wxShowTip().

    @param filename
        The name of the file containing the tips, one per line.
    @param currentTip
        The index of the first tip to show. Normally this index is remembered
        between the 2 program runs.

    @see @ref overview_tips

    @header{wx/tipdlg.h}
*/
wxTipProvider* wxCreateFileTipProvider(const wxString& filename,
                                       size_t currentTip);

/**
    This function shows a "startup tip" to the user. The return value is the
    state of the "Show tips at startup" checkbox.

    @param parent
        The parent window for the modal dialog.
    @param tipProvider
        An object which is used to get the text of the tips. It may be created
        with the wxCreateFileTipProvider() function.
    @param showAtStartup
        Should be true if startup tips are shown, false otherwise. This is
        used as the initial value for "Show tips at startup" checkbox which is
        shown in the tips dialog.

    @see @ref overview_tips

    @header{wx/tipdlg.h}
*/
bool wxShowTip(wxWindow *parent,
               wxTipProvider *tipProvider,
               bool showAtStartup = true);

//@}

