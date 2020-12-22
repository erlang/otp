/////////////////////////////////////////////////////////////////////////////
// Name:        sysopt.h
// Purpose:     interface of wxSystemOptions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxSystemOptions

    wxSystemOptions stores option/value pairs that wxWidgets itself or
    applications can use to alter behaviour at run-time. It can be
    used to optimize behaviour that doesn't deserve a distinct API,
    but is still important to be able to configure.

    System options can be set by the program itself using SetOption() method
    and they also can be set from the program environment by defining an
    environment variable @c wx_option to set the given option for all wxWidgets
    applications or @c wx_appname_option to set it just for the application
    with the given name (as returned by wxApp::GetAppName()). Notice that any
    characters not allowed in the environment variables names, such as periods
    and dashes, should be replaced with underscores. E.g. to define a system
    option "foo-bar" you need to define the environment variable "wx_foo_bar".

    The program may use system options for its own needs but they are mostly
    used to control the behaviour of wxWidgets library itself.

    These options are currently recognised by wxWidgets:

    @section sysopt_all All platforms

    @beginFlagTable
    @flag{exit-on-assert}
        If set to non-zero value, abort the program if an assertion fails. The
        default behaviour in case of assertion failure depends on the build mode
        and can be changed by overriding wxApp::OnAssertFailure() but setting
        this option allows changing it without modifying the program code and
        also applies to asserts which may happen before the wxApp object
        creation or after its destruction.
    @endFlagTable

    @section sysopt_win Windows

    @beginFlagTable
    @flag{no-maskblt}
        1 to never use WIN32's MaskBlt function, 0 to allow it to be used where possible.
        Default: 0. In some circumstances the MaskBlt function can be slower than using
        the fallback code, especially if using DC caching. By default, MaskBlt will be
        used where it is implemented by the operating system and driver.
    @flag{msw.remap}
        If 1 (the default), wxToolBar bitmap colours will be remapped to the current
        theme's values. Set this to 0 to disable this functionality, for example if
        you're using more than 16 colours in your tool bitmaps.
    @flag{msw.window.no-clip-children}
        If 1, windows will not automatically get the WS_CLIPCHILDREN style.
        This restores the way windows are refreshed back to the method used in
        versions of wxWidgets earlier than 2.5.4, and for some complex window
        hierarchies it can reduce apparent refresh delays.
        You may still specify wxCLIP_CHILDREN for individual windows.
    @flag{msw.notebook.themed-background}
        If set to 0, globally disables themed backgrounds on notebook pages.
        Note that this won't disable the theme on the actual notebook background
        (noticeable only if there are no pages).
    @flag{msw.staticbox.optimized-paint}
        If set to 0, switches off optimized wxStaticBox painting.
        Setting this to 0 causes more flicker, but allows applications to paint
        graphics on the parent of a static box (the optimized refresh causes any
        such drawing to disappear).
    @flag{msw.font.no-proof-quality}
        If set to 1, use default fonts quality instead of proof quality when
        creating fonts. With proof quality the fonts have slightly better
        appearance but not all fonts are available in this quality,
        e.g. the Terminal font in small sizes is not and this option may be
        used if wider fonts selection is more important than higher quality.
    @endFlagTable


    @section sysopt_gtk GTK+

    @beginFlagTable
    @flag{gtk.tlw.can-set-transparent}
        wxTopLevelWindow::CanSetTransparent() method normally tries to detect
        automatically whether transparency for top level windows is currently
        supported, however this may sometimes fail and this option allows
        overriding the automatic detection. Setting it to 1 makes the transparency
        be always available (setting it can still fail, of course) and setting it
        to 0 makes it always unavailable.
    @flag{gtk.desktop}
        This option can be set to override the default desktop environment
        determination. Supported values are GNOME and KDE.
    @flag{gtk.window.force-background-colour}
        If 1, the backgrounds of windows with the wxBG_STYLE_COLOUR background
        style are cleared forcibly instead of relying on the underlying GTK+
        window colour. This works around a display problem when running
        applications under KDE with the gtk-qt theme installed (0.6 and below).
    @endFlagTable


    @section sysopt_mac Mac

    @beginFlagTable
    @flag{mac.window-plain-transition}
        If 1, uses a plainer transition when showing a window.
        You can also use the symbol wxMAC_WINDOW_PLAIN_TRANSITION.
    @flag{window-default-variant}
        The default variant used by windows (cast to integer from the wxWindowVariant enum).
        Also known as wxWINDOW_DEFAULT_VARIANT.
    @flag{mac.listctrl.always_use_generic}
        Tells wxListCtrl to use the generic control even when it is capable of
        using the native control instead. Also known as wxMAC_ALWAYS_USE_GENERIC_LISTCTRL.
    @flag{mac.textcontrol-use-spell-checker}
        If 1 activates the spell checking in wxTextCtrl.
    @flag{osx.openfiledialog.always-show-types}
        Per default a wxFileDialog with wxFD_OPEN does not show a types-popup on macOS but allows
        the selection of files from any of the supported types. Setting this to 1 shows a wxChoice
        for selection (if there is more than one supported filetype).
    @endFlagTable


    @section sysopt_motif Motif

    @beginFlagTable
    @flag{motif.largebuttons}
        If 1, uses a bigger default size for wxButtons.
    @endFlagTable


    The compile-time option to include or exclude this functionality is wxUSE_SYSTEM_OPTIONS.

    @library{wxbase}
    @category{cfg}

    @see wxSystemSettings
*/
class wxSystemOptions : public wxObject
{
public:
    /**
        Default constructor.

        You don't need to create an instance of wxSystemOptions since all
        of its functions are static.
    */
    wxSystemOptions();

    /**
        Gets an option. The function is case-insensitive to @a name.
        Returns empty string if the option hasn't been set.

        @see SetOption(), GetOptionInt(), HasOption()
    */
    static wxString GetOption(const wxString& name);

    /**
        Gets an option as an integer. The function is case-insensitive to @a name.
        If the option hasn't been set, this function returns 0.

        @see SetOption(), GetOption(), HasOption()
    */
    static int GetOptionInt(const wxString& name);

    /**
        Returns @true if the given option is present.
        The function is case-insensitive to @a name.

        @see SetOption(), GetOption(), GetOptionInt()
    */
    static bool HasOption(const wxString& name);

    /**
        Returns @true if the option with the given @a name had been set to 0 value.

        This is mostly useful for boolean options for which you can't use
        @c GetOptionInt(name) == 0 as this would also be @true if the option
        hadn't been set at all.
    */
    static bool IsFalse(const wxString& name);

    //@{
    /**
        Sets an option. The function is case-insensitive to @a name.
    */
    static void SetOption(const wxString& name, const wxString& value);
    static void SetOption(const wxString& name, int value);
    //@}
};

