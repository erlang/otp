/////////////////////////////////////////////////////////////////////////////
// Name:        fontdlg.h
// Purpose:     interface of wxFontDialog
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxFontDialog

    This class represents the font chooser dialog.

    @library{wxcore}
    @category{cmndlg}

    @see @ref overview_cmndlg_font, wxFontData, wxGetFontFromUser()
*/
class wxFontDialog : public wxDialog
{
public:
    /**
        Default ctor.
        Create() must be called before the dialog can be shown.
    */
    wxFontDialog();

    /**
        Constructor with parent window.
    */
    wxFontDialog(wxWindow* parent);

    /**
        Constructor.
        Pass a parent window, and the @ref wxFontData "font data" object
        to be used to initialize the dialog controls.
    */
    wxFontDialog(wxWindow* parent, const wxFontData& data);

    /**
        Creates the dialog if the wxFontDialog object had been initialized using
        the default constructor.

        @return @true on success and @false if an error occurred.
    */
    bool Create(wxWindow* parent);

    /**
        Creates the dialog if the wxFontDialog object had been initialized using
        the default constructor.

        @return @true on success and @false if an error occurred.
    */
    bool Create(wxWindow* parent, const wxFontData& data);

    //@{
    /**
        Returns the @ref wxFontData "font data" associated with the
        font dialog.
    */
    const wxFontData& GetFontData() const;
    wxFontData& GetFontData();
    //@}

    /**
        Shows the dialog, returning @c wxID_OK if the user pressed Ok, and
        @c wxID_CANCEL otherwise.

        If the user cancels the dialog (ShowModal returns @c wxID_CANCEL), no font
        will be created. If the user presses OK, a new wxFont will be created and
        stored in the font dialog's wxFontData structure.

        @see GetFontData()
    */
    int ShowModal();
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_dialog */
//@{

/**
    Shows the font selection dialog and returns the font selected by user or
    invalid font (use wxFont::IsOk() to test whether a font is valid) if the
    dialog was cancelled.

    @param parent
        The parent window for the font selection dialog.
    @param fontInit
        If given, this will be the font initially selected in the dialog.
    @param caption
        If given, this will be used for the dialog caption.

    @header{wx/fontdlg.h}
*/
wxFont wxGetFontFromUser(wxWindow* parent,
                         const wxFont& fontInit,
                         const wxString& caption = wxEmptyString);

//@}

