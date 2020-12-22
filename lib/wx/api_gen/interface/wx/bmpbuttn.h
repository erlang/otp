/////////////////////////////////////////////////////////////////////////////
// Name:        bmpbuttn.h
// Purpose:     interface of wxBitmapButton
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxBitmapButton

    A bitmap button is a control that contains a bitmap.

    Notice that since wxWidgets 2.9.1 bitmap display is supported by the base
    wxButton class itself and the only tiny advantage of using this class is
    that it allows specifying the bitmap in its constructor, unlike wxButton.
    Please see the base class documentation for more information about images
    support in wxButton.

    @beginStyleTable
    @style{wxBU_LEFT}
           Left-justifies the bitmap label.
    @style{wxBU_TOP}
           Aligns the bitmap label to the top of the button.
    @style{wxBU_RIGHT}
           Right-justifies the bitmap label.
    @style{wxBU_BOTTOM}
           Aligns the bitmap label to the bottom of the button.
    @endStyleTable

    Note that the wxBU_EXACTFIT style supported by wxButton is not used by this
    class as bitmap buttons don't have any minimal standard size by default.

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_BUTTON(id, func)}
           Process a @c wxEVT_BUTTON event, when the button is clicked.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{bitmapbutton}

    @see wxButton
*/
class wxBitmapButton : public wxButton
{
public:
    /**
        Default ctor.
    */
    wxBitmapButton();

    /**
        Constructor, creating and showing a button.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Button identifier. The value wxID_ANY indicates a default value.
        @param bitmap
            Bitmap to be displayed.
        @param pos
            Button position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Button size.
            If ::wxDefaultSize is specified then the button is sized appropriately
            for the bitmap.
        @param style
            Window style. See wxBitmapButton.
        @param validator
            Window validator.
        @param name
            Window name.

        @remarks The bitmap parameter is normally the only bitmap you need to provide,
                 and wxWidgets will draw the button correctly in its different states.
                 If you want more control, call any of the functions SetBitmapPressed(),
                 SetBitmapFocus(), SetBitmapDisabled().

        @see Create(), wxValidator
    */
    wxBitmapButton(wxWindow* parent, wxWindowID id,
                   const wxBitmap& bitmap,
                   const wxPoint& pos = wxDefaultPosition,
                   const wxSize& size = wxDefaultSize,
                   long style = 0,
                   const wxValidator& validator = wxDefaultValidator,
                   const wxString& name = wxButtonNameStr);

    /**
        Button creation function for two-step creation.
        For more details, see wxBitmapButton().
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxBitmap& bitmap,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxButtonNameStr);

    /**
        Helper function creating a standard-looking "Close" button.

        To get the best results, platform-specific code may need to be used to
        create a small, title bar-like "Close" button. This function is
        provided to avoid the need to test for the current platform and creates
        the button with as native look as possible.

        @param parent The button parent window, must be non-@NULL.
        @param winid The identifier for the new button.
        @return The new button.

        @since 2.9.5
     */
    static wxBitmapButton* NewCloseButton(wxWindow* parent, wxWindowID winid);
};

