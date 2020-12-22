///////////////////////////////////////////////////////////////////////////////
// Name:        interface/wx/preferences.h
// Purpose:     wxPreferencesEditor class documentation.
// Author:      Vaclav Slavik
// Created:     2013-02-26
// Copyright:   (c) 2013 Vaclav Slavik <vslavik@fastmail.fm>
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    Manage preferences dialog.

    This class encapsulates the differences -- both in appearance and
    behaviour -- between preferences dialogs on different platforms.  In
    particular, macOS preferences look very different from the typical notebook
    control used on other platforms, and both macOS and GTK+ preferences windows
    are modeless unlike Windows options dialogs that are typically modal.

    wxPreferencesEditor is able to hide the differences by hiding the creation
    of preferences window from the API. Instead, you create an instance of
    wxPreferencesEditor and add page descriptions in the form of
    wxPreferencesPage using its AddPage() method. After setting up the editor
    object, you must call Show() to present preferences to the user.

    @note Notice that this class is not derived from wxWindow and hence
          doesn't represent a window, even if its Show() method does create one
          internally.

    @library{wxcore}

    @since 2.9.5
 */
class wxPreferencesEditor
{
public:
    /**
        Constructor.

        Creates an empty editor, use AddPage() to add controls to it.

        @param title The title overriding the default title of the top level
            window used by the editor. It is recommended to not specify this
            parameter to use the native convention for the preferences dialogs
            instead.
     */
    wxPreferencesEditor(const wxString& title = wxString());

    /**
        Destructor.

        Destroying this object hides the associated preferences window if it is
        open at the moment.

        The destructor is non-virtual as this class is not supposed to be
        derived from.
     */
    ~wxPreferencesEditor();

    /**
        Add a new page to the editor.

        The editor takes ownership of the page and will delete it from its
        destructor (but not sooner).

        @see wxPreferencesPage, wxStockPreferencesPage
     */
    void AddPage(wxPreferencesPage *page);

    /**
        Show the preferences dialog or bring it to the top if it's already
        shown.

        Notice that this method may or may not block depending on the platform,
        i.e. depending on whether the dialog is modal or not.

        @param parent The window that invokes the preferences.
                      Call Dismiss() before it's destroyed.
     */
    virtual void Show(wxWindow* parent);

    /**
        Hide the currently shown dialog, if any.

        This is typically called to dismiss the dialog if the object whose
        preferences it is editing was closed.
     */
    void Dismiss();

    /**
        Returns whether changes to values in preferences pages should be
        applied immediately or only when the user clicks the OK button.

        Currently, changes are applied immediately on macOS and GTK+.

        The preprocessor macro `wxHAS_PREF_EDITOR_APPLY_IMMEDIATELY` is defined
        in this case as well.
     */
    static bool ShouldApplyChangesImmediately();

    /**
        Returns whether the preferences dialog is shown modally.

        If this method returns false, as it currently does in wxGTK and wxOSX,
        Show() simply makes the dialog visible and returns immediately. If it
        returns true, as it does in wxMSW and under the other platforms, then
        the dialog is shown modally, i.e. Show() blocks until the user
        dismisses it.

        Notice that it isn't necessary to test the return value of this method
        to use this class normally, its interface is designed to work in both
        cases. However it can sometimes be necessary to call it if the program
        needs to handle modal dialogs specially, e.g. perhaps to block some
        periodic background update operation while a modal dialog is shown.
     */
    static bool ShownModally();
};


/**
    One page of preferences dialog.

    This is the base class for implementation of application's preferences. Its
    methods return various properties of the page, such as title or icon. The
    actual page is created by CreateWindow().

    @see wxStockPreferencesPage

    @library{wxcore}

    @since 2.9.5
 */
class wxPreferencesPage
{
public:
    /// Constructor.
    wxPreferencesPage();

    /// Destructor.
    virtual ~wxPreferencesPage();

    /**
        Return name of the page.

        The name is used for notebook tab's label, icon label etc., depending
        on the platform.
     */
    virtual wxString GetName() const = 0;

    /**
        Return 32x32 icon used for the page on some platforms.

        Currently only used on macOS.

        @note This method is only pure virtual on platforms that require it
              (macOS). On other platforms, it has default implementation that
              returns an invalid bitmap. The preprocessor symbol
              `wxHAS_PREF_EDITOR_ICONS` is defined if this method must be
              implemented.
     */
    virtual wxBitmap GetLargeIcon() const = 0;

    /**
        Create a window for this page.

        The window will be placed into the preferences dialog in
        platform-specific manner. Depending on the platform, this method may
        be called before showing the preferences window, when switching to its
        tab or even more than once. Don't make assumptions about the number of
        times or the specific time when it is called.

        The caller takes ownership of the window.

        wxPanel is usually used, but doesn't have to be.

        @param parent Parent window to use.
     */
    virtual wxWindow *CreateWindow(wxWindow *parent) = 0;
};


/**
    Specialization of wxPreferencesPage useful for certain commonly used
    preferences page.

    On macOS, preferences pages named "General" and "Advanced" are commonly used
    in apps and the OS provides stock icons for them that should be used.
    Instead of reimplementing this behaviour yourself, you can inherit from
    wxStockPreferencesPage and get correct title and icon.

    Notice that this class only implements GetName() and GetLargeIcon(), you
    still have to provide the rest of wxPreferencesPage implementation.

    @library{wxcore}

    @since 2.9.5
 */
class wxStockPreferencesPage : public wxPreferencesPage
{
public:
    /// Kinds of stock pages.
    enum Kind
    {
        /// The "General" page
        Kind_General,
        /// The "Advanced" page
        Kind_Advanced
    };

    /// Constructor.
    wxStockPreferencesPage(Kind kind);

    /// Returns the page's kind.
    Kind GetKind() const;

    /// Reimplemented to return suitable name for the page's kind.
    virtual wxString GetName() const;
    /// Reimplemented to return stock icon on macOS.
    virtual wxBitmap GetLargeIcon() const;
};
