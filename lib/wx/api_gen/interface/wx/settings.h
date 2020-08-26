/////////////////////////////////////////////////////////////////////////////
// Name:        settings.h
// Purpose:     interface of wxSystemSettings
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    Possible values for wxSystemSettings::GetFont() parameter.

    These values map 1:1 the native values supported by the Windows' @c GetStockObject
    function. Note that other ports (other than wxMSW) will try to provide meaningful
    fonts but they usually map the same font to various @c wxSYS_*_FONT values.
*/
enum wxSystemFont
{
    /// Original equipment manufacturer dependent fixed-pitch font.
    wxSYS_OEM_FIXED_FONT = 10,

    /// Windows fixed-pitch (monospaced) font.
    wxSYS_ANSI_FIXED_FONT,

    /// Windows variable-pitch (proportional) font.
    wxSYS_ANSI_VAR_FONT,

    /// System font. By default, the system uses the system font to draw menus,
    /// dialog box controls, and text.
    wxSYS_SYSTEM_FONT,

    /// Device-dependent font.
    wxSYS_DEVICE_DEFAULT_FONT,

    /**
        Default font for user interface objects such as menus and dialog boxes.
        Note that with modern GUIs nothing guarantees that the same font is used
        for all GUI elements, so some controls might use a different font by default.
    */
    wxSYS_DEFAULT_GUI_FONT
};


/**
    Possible values for wxSystemSettings::GetColour() parameter.

    These values map 1:1 the native values supported by the Windows' @c GetSysColor
    function. Note that other ports (other than wxMSW) will try to provide meaningful
    colours but they usually map the same colour to various @c wxSYS_COLOUR_* values.
*/
enum wxSystemColour
{
    wxSYS_COLOUR_SCROLLBAR,           //!< The scrollbar grey area.
    wxSYS_COLOUR_DESKTOP,             //!< The desktop colour.
    wxSYS_COLOUR_ACTIVECAPTION,       //!< Active window caption colour.
    wxSYS_COLOUR_INACTIVECAPTION,     //!< Inactive window caption colour.
    wxSYS_COLOUR_MENU,                //!< Menu background colour.
    wxSYS_COLOUR_WINDOW,              //!< Window background colour.
    wxSYS_COLOUR_WINDOWFRAME,         //!< Window frame colour.
    wxSYS_COLOUR_MENUTEXT,            //!< Colour of the text used in the menus.
    wxSYS_COLOUR_WINDOWTEXT,          //!< Colour of the text used in generic windows.
    wxSYS_COLOUR_CAPTIONTEXT,         //!< Colour of the text used in captions, size boxes and scrollbar arrow boxes.
    wxSYS_COLOUR_ACTIVEBORDER,        //!< Active window border colour.
    wxSYS_COLOUR_INACTIVEBORDER,      //!< Inactive window border colour.
    wxSYS_COLOUR_APPWORKSPACE,        //!< Background colour for MDI applications.
    wxSYS_COLOUR_HIGHLIGHT,           //!< Colour of item(s) selected in a control.
    wxSYS_COLOUR_HIGHLIGHTTEXT,       //!< Colour of the text of item(s) selected in a control.
    wxSYS_COLOUR_BTNFACE,             //!< Face shading colour on push buttons.
    wxSYS_COLOUR_BTNSHADOW,           //!< Edge shading colour on push buttons.
    wxSYS_COLOUR_GRAYTEXT,            //!< Colour of greyed (disabled) text.
    wxSYS_COLOUR_BTNTEXT,             //!< Colour of the text on push buttons.
    wxSYS_COLOUR_INACTIVECAPTIONTEXT, //!< Colour of the text in active captions.
    wxSYS_COLOUR_BTNHIGHLIGHT,        //!< Highlight colour for buttons.
    wxSYS_COLOUR_3DDKSHADOW,          //!< Dark shadow colour for three-dimensional display elements.
    wxSYS_COLOUR_3DLIGHT,             //!< Light colour for three-dimensional display elements.
    wxSYS_COLOUR_INFOTEXT,            //!< Text colour for tooltip controls.
    wxSYS_COLOUR_INFOBK,              //!< Background colour for tooltip controls.
    wxSYS_COLOUR_LISTBOX,             //!< Background colour for list-like controls.
    wxSYS_COLOUR_HOTLIGHT,            //!< Colour for a hyperlink or hot-tracked item.

    /**
        Right side colour in the colour gradient of an active window's title
        bar. @c wxSYS_COLOUR_ACTIVECAPTION specifies the left side colour.
    */
    wxSYS_COLOUR_GRADIENTACTIVECAPTION,

    /**
        Right side colour in the colour gradient of an inactive window's title
        bar. @c wxSYS_COLOUR_INACTIVECAPTION specifies the left side colour.
    */
    wxSYS_COLOUR_GRADIENTINACTIVECAPTION,

    /**
        The colour used to highlight menu items when the menu appears as a flat menu.
        The highlighted menu item is outlined with @c wxSYS_COLOUR_HIGHLIGHT.
    */
    wxSYS_COLOUR_MENUHILIGHT,

    /**
        The background colour for the menu bar when menus appear as flat menus.
        However, @c wxSYS_COLOUR_MENU continues to specify the background colour of the menu popup.
    */
    wxSYS_COLOUR_MENUBAR,

    /**
        Text colour for list-like controls.

        @since 2.9.0
     */
    wxSYS_COLOUR_LISTBOXTEXT,

    /**
        Text colour for the unfocused selection of list-like controls.

        @since 2.9.1
     */
    wxSYS_COLOUR_LISTBOXHIGHLIGHTTEXT,

    wxSYS_COLOUR_MAX



    // synonyms:

    wxSYS_COLOUR_BACKGROUND = wxSYS_COLOUR_DESKTOP,
        //!< Synonym for @c wxSYS_COLOUR_DESKTOP.
    wxSYS_COLOUR_3DFACE = wxSYS_COLOUR_BTNFACE,
        //!< Synonym for @c wxSYS_COLOUR_BTNFACE.
    wxSYS_COLOUR_3DSHADOW = wxSYS_COLOUR_BTNSHADOW,
        //!< Synonym for @c wxSYS_COLOUR_BTNSHADOW.
    wxSYS_COLOUR_BTNHILIGHT = wxSYS_COLOUR_BTNHIGHLIGHT,
        //!< Synonym for @c wxSYS_COLOUR_BTNHIGHLIGHT.
    wxSYS_COLOUR_3DHIGHLIGHT = wxSYS_COLOUR_BTNHIGHLIGHT,
        //!< Synonym for @c wxSYS_COLOUR_BTNHIGHLIGHT.
    wxSYS_COLOUR_3DHILIGHT = wxSYS_COLOUR_BTNHIGHLIGHT,
        //!< Synonym for @c wxSYS_COLOUR_BTNHIGHLIGHT.

    /**
        Synonym for @c wxSYS_COLOUR_BTNFACE.

        On wxMSW this colour should be used as the background colour of
        wxFrames which are used as containers of controls; this is in fact the
        same colour used for the borders of controls like e.g. wxNotebook or
        for the background of e.g. wxPanel.

        @since 2.9.0
    */
    wxSYS_COLOUR_FRAMEBK = wxSYS_COLOUR_BTNFACE
};

/**
    Possible values for wxSystemSettings::GetMetric() index parameter.
*/
enum wxSystemMetric
{
    wxSYS_MOUSE_BUTTONS,      //!< Number of buttons on mouse, or zero if no mouse was installed.
    wxSYS_BORDER_X,           //!< Width of single border.
    wxSYS_BORDER_Y,           //!< Height of single border.
    wxSYS_CURSOR_X,           //!< Width of cursor.
    wxSYS_CURSOR_Y,           //!< Height of cursor.
    wxSYS_DCLICK_X,           //!< Width in pixels of rectangle within which two successive mouse clicks must fall to generate a double-click.
    wxSYS_DCLICK_Y,           //!< Height in pixels of rectangle within which two successive mouse clicks must fall to generate a double-click.
    wxSYS_DRAG_X,             //!< Width in pixels of a rectangle centered on a drag point to allow for limited movement of the mouse pointer before a drag operation begins.
    wxSYS_DRAG_Y,             //!< Height in pixels of a rectangle centered on a drag point to allow for limited movement of the mouse pointer before a drag operation begins.
    wxSYS_EDGE_X,             //!< Width of a 3D border, in pixels.
    wxSYS_EDGE_Y,             //!< Height of a 3D border, in pixels.
    wxSYS_HSCROLL_ARROW_X,    //!< Width of arrow bitmap on horizontal scrollbar.
    wxSYS_HSCROLL_ARROW_Y,    //!< Height of arrow bitmap on horizontal scrollbar.
    wxSYS_HTHUMB_X,           //!< Width of horizontal scrollbar thumb.
    wxSYS_ICON_X,             //!< The default width of an icon.
    wxSYS_ICON_Y,             //!< The default height of an icon.
    wxSYS_ICONSPACING_X,      //!< Width of a grid cell for items in large icon view, in pixels. Each item fits into a rectangle of this size when arranged.
    wxSYS_ICONSPACING_Y,      //!< Height of a grid cell for items in large icon view, in pixels. Each item fits into a rectangle of this size when arranged.
    wxSYS_WINDOWMIN_X,        //!< Minimum width of a window.
    wxSYS_WINDOWMIN_Y,        //!< Minimum height of a window.
    wxSYS_SCREEN_X,           //!< Width of the screen in pixels.
    wxSYS_SCREEN_Y,           //!< Height of the screen in pixels.
    wxSYS_FRAMESIZE_X,        //!< Width of the window frame for a wxTHICK_FRAME window.
    wxSYS_FRAMESIZE_Y,        //!< Height of the window frame for a wxTHICK_FRAME window.
    wxSYS_SMALLICON_X,        //!< Recommended width of a small icon (in window captions, and small icon view).
    wxSYS_SMALLICON_Y,        //!< Recommended height of a small icon (in window captions, and small icon view).
    wxSYS_HSCROLL_Y,          //!< Height of horizontal scrollbar in pixels.
    wxSYS_VSCROLL_X,          //!< Width of vertical scrollbar in pixels.
    wxSYS_VSCROLL_ARROW_X,    //!< Width of arrow bitmap on a vertical scrollbar.
    wxSYS_VSCROLL_ARROW_Y,    //!< Height of arrow bitmap on a vertical scrollbar.
    wxSYS_VTHUMB_Y,           //!< Height of vertical scrollbar thumb.
    wxSYS_CAPTION_Y,          //!< Height of normal caption area.
    wxSYS_MENU_Y,             //!< Height of single-line menu bar.
    wxSYS_NETWORK_PRESENT,    //!< 1 if there is a network present, 0 otherwise.
    wxSYS_PENWINDOWS_PRESENT, //!< 1 if PenWindows is installed, 0 otherwise.
    wxSYS_SHOW_SOUNDS,        //!< Non-zero if the user requires an application to present information
                              //!< visually in situations where it would otherwise present the information
                              //!< only in audible form; zero otherwise.
    wxSYS_SWAP_BUTTONS,       //!< Non-zero if the meanings of the left and right mouse buttons are swapped; zero otherwise.
    wxSYS_DCLICK_MSEC,        //!< Maximal time, in milliseconds, which may pass between subsequent clicks for a double click to be generated.

    /**
        Time, in milliseconds, for how long a blinking caret should
        stay visible during a single blink cycle before it disappears.

        If this value is zero, caret should be visible all the time
        instead of blinking.  If the value is negative, the platform
        does not support the user setting.

        @since 3.1.1
    */
    wxSYS_CARET_ON_MSEC,

    /**
        Time, in milliseconds, for how long a blinking caret should
        stay invisible during a single blink cycle before it reappears.

        If this value is zero, caret should be visible all the time
        instead of blinking.  If the value is negative, the platform
        does not support the user setting.

        @since 3.1.1
    */
    wxSYS_CARET_OFF_MSEC,

    /**
        Time, in milliseconds, for how long a caret should blink after
        a user interaction.  After this timeout has expired, the caret
        should stay continuously visible until the user interacts with
        the caret again (for example by entering, deleting or cutting
        text).  If this value is negative, carets should blink forever;
        if it is zero, carets should not blink at all.

        @since 3.1.1
    */
    wxSYS_CARET_TIMEOUT_MSEC
};

/**
    Possible values for wxSystemSettings::HasFeature() parameter.
*/
enum wxSystemFeature
{
    wxSYS_CAN_DRAW_FRAME_DECORATIONS = 1,
    wxSYS_CAN_ICONIZE_FRAME,
    wxSYS_TABLET_PRESENT
};

/**
    Values for different screen designs. See wxSystemSettings::GetScreenType().
*/
enum wxSystemScreenType
{
    wxSYS_SCREEN_NONE = 0,  //!< Undefined screen type.

    wxSYS_SCREEN_TINY,      //!< Tiny screen, less than 320x240
    wxSYS_SCREEN_PDA,       //!< PDA screen, 320x240 or more but less than 640x480
    wxSYS_SCREEN_SMALL,     //!< Small screen, 640x480 or more but less than 800x600
    wxSYS_SCREEN_DESKTOP    //!< Desktop screen, 800x600 or more
};


/**
    Provides information about the current system appearance.

    An object of this class can be retrieved using
    wxSystemSettings::GetAppearance() and can then be queried for some aspects
    of the current system appearance, notably whether the system is using a
    dark theme, i.e. a theme with predominantly dark background.

    This is useful for custom controls that don't use the standard system
    colours, as they need to adjust the colours used for drawing them to fit in
    the system look.

    @since 3.1.3
 */
class wxSystemAppearance
{
public:
    /**
        Return the name if available or empty string otherwise.

        This is currently only implemented for macOS and returns
        a not necessarily user-readable string such as "NSAppearanceNameAqua"
        there and an empty string under all the other platforms.
     */
    wxString GetName() const;

    /**
        Return true if the current system there is explicitly recognized as
        being a dark theme or if the default window background is dark.

        This method should be used to check whether custom colours more
        appropriate for the default (light) or dark appearance should be used.
     */
    bool IsDark() const;

    /**
        Return true if the default window background is significantly darker
        than foreground.

        This is used by IsDark() if there is no platform-specific way to
        determine whether a dark mode is being used and is generally not very
        useful to call directly.

        @see wxColour::GetLuminance()
     */
    bool IsUsingDarkBackground() const;
};


/**
    @class wxSystemSettings

    wxSystemSettings allows the application to ask for details about the system.

    This can include settings such as standard colours, fonts, and user interface
    element sizes.

    @library{wxcore}
    @category{cfg}

    @see wxFont, wxColour, wxSystemOptions
*/
class wxSystemSettings
{
public:
    /**
        Default constructor.

        You don't need to create an instance of wxSystemSettings
        since all of its functions are static.
    */
    wxSystemSettings();

    /**
        Returns a system colour.

        @param index
            Can be one of the ::wxSystemColour enum values.

        @return
            The returned colour is always valid.
    */
    static wxColour GetColour(wxSystemColour index);

    /**
        Returns a system font.

        @param index
            Can be one of the ::wxSystemFont enum values.

        @return
            The returned font is always valid.
    */
    static wxFont GetFont(wxSystemFont index);

    /**
        Returns the value of a system metric, or -1 if the metric is not supported on
        the current system.

        The value of @a win determines if the metric returned is a global value or
        a wxWindow based value, in which case it might determine the widget, the
        display the window is on, or something similar. The window given should be as
        close to the metric as possible (e.g. a wxTopLevelWindow in case of the
        wxSYS_CAPTION_Y metric).

        @a index can be one of the ::wxSystemMetric enum values.

        @a win is a pointer to the window for which the metric is requested.
        Specifying the @a win parameter is encouraged, because some metrics on some
        ports are not supported without one,or they might be capable of reporting
        better values if given one. If a window does not make sense for a metric,
        one should still be given, as for example it might determine which displays
        cursor width is requested with wxSYS_CURSOR_X.
    */
    static int GetMetric(wxSystemMetric index, wxWindow* win = NULL);

    /**
        Returns the screen type.
        The return value is one of the ::wxSystemScreenType enum values.
    */
    static wxSystemScreenType GetScreenType();

    /**
        Returns the object describing the current system appearance.

        @since 3.1.3
     */
    static wxSystemAppearance GetAppearance();

    /**
        Returns @true if the port has certain feature.
        See the ::wxSystemFeature enum values.
    */
    static bool HasFeature(wxSystemFeature index);
};

