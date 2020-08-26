/////////////////////////////////////////////////////////////////////////////
// Name:        button.h
// Purpose:     interface of wxButton
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxButton

    A button is a control that contains a text string, and is one of the most
    common elements of a GUI.

    It may be placed on a @ref wxDialog "dialog box" or on a @ref wxPanel panel,
    or indeed on almost any other window.

    By default, i.e. if none of the alignment styles are specified, the label
    is centered both horizontally and vertically. If the button has both a
    label and a bitmap, the alignment styles above specify the location of the
    rectangle combining both the label and the bitmap and the bitmap position
    set with wxButton::SetBitmapPosition() defines the relative position of the
    bitmap with respect to the label (however currently non-default alignment
    combinations are not implemented on all platforms).

    Since version 2.9.1 wxButton supports showing both text and an image
    (currently only when using wxMSW, wxGTK or wxOSX/Cocoa ports), see
    SetBitmap() and SetBitmapLabel(), SetBitmapDisabled() &c methods. In the
    previous wxWidgets versions this functionality was only available in (the
    now trivial) wxBitmapButton class which was only capable of showing an
    image without text.

    A button may have either a single image for all states or different images
    for the following states (different images are not currently supported
    under macOS where the normal image is used for all states):
    @li @b normal: the default state
    @li @b disabled: bitmap shown when the button is disabled.
    @li @b pressed: bitmap shown when the button is pushed (e.g. while the user
        keeps the mouse button pressed on it)
    @li @b focus: bitmap shown when the button has keyboard focus (but is not
        pressed as in this case the button is in the pressed state)
    @li @b current: bitmap shown when the mouse is over the button (but it is
        not pressed although it may have focus). Notice that if current bitmap
        is not specified but the current platform UI uses hover images for the
        buttons (such as Windows or GTK+), then the focus bitmap is used for
        hover state as well. This makes it possible to set focus bitmap only to
        get reasonably good behaviour on all platforms.

    All of the bitmaps must be of the same size and the normal bitmap must be
    set first (to a valid bitmap), before setting any other ones. Also, if the
    size of the bitmaps is changed later, you need to change the size of the
    normal bitmap before setting any other bitmaps with the new size (and you
    do need to reset all of them as their original values can be lost when the
    normal bitmap size changes).

    The position of the image inside the button be configured using
    SetBitmapPosition(). By default the image is on the left of the text.

    Please also notice that GTK+ uses a global setting called @c gtk-button-images
    to determine if the images should be shown in the buttons
    at all. If it is off (which is the case in e.g. Gnome 2.28 by default), no
    images will be shown, consistently with the native behaviour.

    @beginStyleTable
    @style{wxBU_LEFT}
        Left-justifies the label. Windows and GTK+ only.
    @style{wxBU_TOP}
        Aligns the label to the top of the button. Windows and GTK+ only.
    @style{wxBU_RIGHT}
        Right-justifies the bitmap label. Windows and GTK+ only.
    @style{wxBU_BOTTOM}
        Aligns the label to the bottom of the button. Windows and GTK+ only.
    @style{wxBU_EXACTFIT}
        By default, all buttons are made of at least the standard button size,
        even if their contents is small enough to fit into a smaller size. This
        is done for consistency as most platforms use buttons of the same size
        in the native dialogs, but can be overridden by specifying this flag.
        If it is given, the button will be made just big enough for its
        contents. Notice that under MSW the button will still have at least the
        standard height, even with this style, if it has a non-empty label.
    @style{wxBU_NOTEXT}
        Disables the display of the text label in the button even if it has one
        or its id is one of the standard stock ids with an associated label:
        without using this style a button which is only supposed to show a
        bitmap but uses a standard id would display a label too.
    @style{wxBORDER_NONE}
        Creates a button without border. This is currently implemented in MSW,
        GTK2 and OSX/Cocoa.
    @endStyleTable

    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_BUTTON(id, func)}
           Process a @c wxEVT_BUTTON event, when the button is clicked.
    @endEventTable

    @library{wxcore}
    @category{ctrl}
    @appearance{button}

    @see wxBitmapButton
*/
class wxButton : public wxAnyButton
{
public:
    /**
        Default ctor.
    */
    wxButton();

    /**
        Constructor, creating and showing a button.

        The preferred way to create standard buttons is to use default value of
        @a label. If no label is supplied and @a id is one of standard IDs from
        @ref page_stockitems "this list", a standard label will be used. In
        other words, if you use a predefined @c wxID_XXX constant, just omit
        the label completely rather than specifying it. In particular, help
        buttons (the ones with @a id of @c wxID_HELP) under macOS can't
        display any label at all and while wxButton will detect if the standard
        "Help" label is used and ignore it, using any other label will prevent
        the button from correctly appearing as a help button and so should be
        avoided.


        In addition to that, the button will be decorated with stock icons under GTK+ 2.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Button identifier. A value of @c wxID_ANY indicates a default value.
        @param label
            Text to be displayed on the button.
        @param pos
            Button position.
        @param size
            Button size. If the default size is specified then the button is sized
            appropriately for the text.
        @param style
            Window style. See wxButton class description.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxButton(wxWindow* parent, wxWindowID id,
             const wxString& label = wxEmptyString,
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize,
             long style = 0,
             const wxValidator& validator = wxDefaultValidator,
             const wxString& name = wxButtonNameStr);

    /**
        Button creation function for two-step creation.
        For more details, see wxButton().
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxString& label = wxEmptyString,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxButtonNameStr);

    /**
        Returns @true if an authentication needed symbol is displayed on the
        button.

        @remarks This method always returns @false if the platform is not
                 Windows Vista or newer.

        @see SetAuthNeeded()

        @since 2.9.1
     */
    bool GetAuthNeeded() const;


    /**
        Returns the default size for the buttons. It is advised to make all the
        dialog buttons of the same size and this function allows retrieving the
        (platform, and current font dependent) size which should be the best
        suited for this.

        The optional @a win argument is new since wxWidgets 3.1.3 and allows to
        get a per-monitor DPI specific size.
    */
    static wxSize GetDefaultSize(wxWindow* win = NULL);

    /**
        Returns the string label for the button.

        @see SetLabel()
    */
    wxString GetLabel() const;

    /**
        Sets whether an authentication needed symbol should be displayed on the
        button.

        @remarks This method doesn't do anything if the platform is not Windows
                 Vista or newer.

        @see GetAuthNeeded()

        @since 2.9.1
     */
    void SetAuthNeeded(bool needed = true);


    /**
        This sets the button to be the default item in its top-level window
        (e.g. the panel or the dialog box containing it).

        As normal, pressing return causes the default button to be depressed when
        the return key is pressed.

        See also wxWindow::SetFocus() which sets the keyboard focus for windows
        and text panel items, and wxTopLevelWindow::SetDefaultItem().

        @remarks Under Windows, only dialog box buttons respond to this function.

        @return the old default item (possibly @NULL)
    */
    virtual wxWindow* SetDefault();

    /**
        Sets the string label for the button.

        @param label
            The label to set.
    */
    void SetLabel(const wxString& label);
};

