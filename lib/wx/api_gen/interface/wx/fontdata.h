/////////////////////////////////////////////////////////////////////////////
// Name:        fontdata.h
// Purpose:     interface of wxFontData
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxFontData

    This class holds a variety of information related to font dialogs.

    @library{wxcore}
    @category{cmndlg,data}

    @see @ref overview_cmndlg_font, wxFont, wxFontDialog
*/
class wxFontData : public wxObject
{
public:
    /**
        Constructor. Initializes @e fontColour to black, @e showHelp to @false,
        @e allowSymbols to @true, @e enableEffects to @true, @e minSize to 0
        and @e maxSize to 0.
    */
    wxFontData();

    /**
        Enables or disables "effects" under Windows or generic only. This
        refers to the controls for manipulating colour, strikeout and underline
        properties.

        The default value is @true.
    */
    void EnableEffects(bool enable);

    /**
        Under Windows, returns a flag determining whether symbol fonts can be
        selected. Has no effect on other platforms.

        The default value is @true.
    */
    bool GetAllowSymbols() const;

    /**
        Gets the font chosen by the user if the user pressed OK
        (wxFontDialog::ShowModal() returned wxID_OK).
    */
    wxFont GetChosenFont() const;

    /**
        Gets the colour associated with the font dialog.

        The default value is black.
    */
    const wxColour& GetColour() const;

    /**
        Determines whether "effects" are enabled under Windows. This refers to
        the controls for manipulating colour, strikeout and underline
        properties.

        The default value is @true.
    */
    bool GetEnableEffects() const;

    /**
        Returns the state of the flags restricting the selection.

        Note that currently these flags are only effectively used in wxMSW.

        @returns
        - @c wxFONTRESTRICT_NONE If no restriction applies, or a combination of
        the following flags:
        - @c wxFONTRESTRICT_SCALABLE To show only scalable fonts - no raster fonts.
        - @c wxFONTRESTRICT_FIXEDPITCH To show only monospaced fonts.

        The default value is @c wxFONTRESTRICT_NONE.

        @since 3.1.4
    */
    int GetRestrictSelection() const;

    /**
        Gets the font that will be initially used by the font dialog. This
        should have previously been set by the application.
    */
    wxFont GetInitialFont() const;

    /**
        Returns @true if the Help button will be shown (Windows only).

        The default value is @false.
    */
    bool GetShowHelp() const;

    /**
        Restricts the selection to a subset of the available fonts.

        Note that currently these flags are only effectively used in wxMSW and
        are ignored in the other ports.

        Possible values are:

        - @c wxFONTRESTRICT_NONE No restriction, show all fonts in the dialog.
        - @c wxFONTRESTRICT_SCALABLE To show only scalable fonts - no raster fonts.
        - @c wxFONTRESTRICT_FIXEDPITCH To show only monospaced fonts.

        The default value is @c wxFONTRESTRICT_NONE.

        @since 3.1.4
    */
    void RestrictSelection(int flags);

    /**
        Under Windows, determines whether symbol fonts can be selected. Has no
        effect on other platforms.

        The default value is @true.
    */
    void SetAllowSymbols(bool allowSymbols);

    /**
        Sets the font that will be returned to the user (for internal use
        only).
    */
    void SetChosenFont(const wxFont& font);

    /**
        Sets the colour that will be used for the font foreground colour.

        The default colour is black.
    */
    void SetColour(const wxColour& colour);

    /**
        Sets the font that will be initially used by the font dialog.
    */
    void SetInitialFont(const wxFont& font);

    /**
        Sets the valid range for the font point size (Windows only).

        The default is 0, 0 (unrestricted range).
    */
    void SetRange(int min, int max);

    /**
        Determines whether the Help button will be displayed in the font dialog
        (Windows only).

        The default value is @false.
    */
    void SetShowHelp(bool showHelp);

    /**
        Assignment operator for the font data.
    */
    wxFontData& operator =(const wxFontData& data);
};
