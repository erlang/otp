/////////////////////////////////////////////////////////////////////////////
// Name:        notifmsg.h
// Purpose:     interface of wxNotificationMessage
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxNotificationMessage

    This class allows showing the user a message non intrusively.

    Currently it is implemented natively for Windows, macOS, GTK and uses
    generic toast notifications under the other platforms. It's not recommended
    but @c wxGenericNotificationMessage can be used instead of the native ones.
    This might make sense if your application requires features not available in
    the native implementation.

    Notice that this class is not a window and so doesn't derive from wxWindow.

    @section platform_notes Platform Notes

    @par Windows
    Up to Windows 8 balloon notifications are displayed from an icon in the
    notification area of the taskbar. If your application uses a wxTaskBarIcon
    you should call UseTaskBarIcon() to ensure that only one icon is shown in
    the notification area. Windows 10 displays all notifications as popup
    toasts. To suppress the additional icon in the notification area on
    Windows 10 and for toast notification support on Windows 8 it is
    recommended to call MSWUseToasts() before showing the first notification
    message.

    @par macOS
    The macOS implementation uses Notification Center to display native notifications.
    In order to use actions your notifications must use the alert style. This can
    be enabled by the user in system settings or by setting the
    @c NSUserNotificationAlertStyle value in Info.plist to @c alert. Please note
    that the user always has the option to change the notification style.


    @beginEventEmissionTable{wxCommandEvent}
    @event{wxEVT_NOTIFICATION_MESSAGE_CLICK(id, func)}
           Process a @c wxEVT_NOTIFICATION_MESSAGE_CLICK event, when a notification
           is clicked.
    @event{wxEVT_NOTIFICATION_MESSAGE_DISMISSED(id, func)}
           Process a @c wxEVT_NOTIFICATION_MESSAGE_DISMISSED event, when a notification
           is dismissed by the user or times out.
    @event{wxEVT_NOTIFICATION_MESSAGE_ACTION(id, func)}
           Process a @c wxEVT_NOTIFICATION_MESSAGE_ACTION event, when the user
           selects on of the actions added by AddAction()
    @endEventTable

    @since 2.9.0
    @library{wxcore}
    @category{misc}
*/
class wxNotificationMessage : public wxEvtHandler
{
public:
    /// Possible values for Show() timeout.
    enum
    {
        Timeout_Auto = -1,  ///< Notification will be hidden automatically.
        Timeout_Never = 0   ///< Notification will never time out.
    };

    /**
        Default constructor, use SetParent(), SetTitle() and SetMessage() to
        initialize the object before showing it.
    */
    wxNotificationMessage();

    /**
        Create a notification object with the given attributes.

        See SetTitle(), SetMessage(), SetParent() and SetFlags() for the
        description of the corresponding parameters.
    */
    wxNotificationMessage(const wxString& title, const wxString& message = wxEmptyString,
                          wxWindow* parent = NULL, int flags = wxICON_INFORMATION);

    /**
        Destructor does not hide the notification.

        The notification can continue to be shown even after the C++ object was
        destroyed, call Close() explicitly if it needs to be hidden.
     */
    virtual ~wxNotificationMessage();

    /**
       Add an action to the notification. If supported by the implementation
       this are usually buttons in the notification selectable by the user.

       @return @false if the current implementation or OS version
       does not support actions in notifications.

       @since 3.1.0
    */
    bool AddAction(wxWindowID actionid, const wxString &label = wxString());

    /**
        Hides the notification.

        Returns @true if it was hidden or @false if it couldn't be done
        (e.g. on some systems automatically hidden notifications can't be
        hidden manually).
    */
    bool Close();

    /**
        This parameter can be currently used to specify the icon to show in the
        notification.

        Valid values are @c wxICON_INFORMATION, @c wxICON_WARNING and
        @c wxICON_ERROR (notice that @c wxICON_QUESTION is not allowed here).
        Some implementations of this class may not support the icons.

        @see SetIcon()
    */
    void SetFlags(int flags);

    /**
        Specify a custom icon to be displayed in the notification.

        Some implementations of this class may not support custom icons.

        @see SetFlags()

        @since 3.1.0
    */
    void SetIcon(const wxIcon& icon);

    /**
        Set the main text of the notification.

        This should be a more detailed description than the title but still limited
        to reasonable length (not more than 256 characters).
    */
    void SetMessage(const wxString& message);

    /**
        Set the parent for this notification: the notification will be associated with
        the top level parent of this window or, if this method is not called, with the
        main application window by default.
    */
    void SetParent(wxWindow* parent);

    /**
        Set the title, it must be a concise string (not more than 64 characters), use
        SetMessage() to give the user more details.
    */
    void SetTitle(const wxString& title);

    /**
        Show the notification to the user and hides it after @a timeout seconds
        are elapsed.

        Special values @c Timeout_Auto and @c Timeout_Never can be used here,
        notice that you shouldn't rely on @a timeout being exactly respected
        because the current platform may only support default timeout value
        and also because the user may be able to close the notification.

        @note When using native notifications in wxGTK, the timeout is ignored
            for the notifications with @c wxICON_WARNING or @c wxICON_ERROR
            flags, they always remain shown unless they're explicitly hidden by
            the user, i.e. behave as if Timeout_Auto were given.

        @return @false if an error occurred.
    */
    bool Show(int timeout = Timeout_Auto);

    /**
        If the application already uses a wxTaskBarIcon, it should be connected
        to notifications by using this method. This has no effect if toast
        notifications are used.

        @return the task bar icon which was used previously (may be @c NULL)

        @onlyfor{wxmsw}
    */
    static wxTaskBarIcon *UseTaskBarIcon(wxTaskBarIcon *icon);


    /**
        Enables toast notifications available since Windows 8 and suppresses
        the additional icon in the notification area on Windows 10.

        Toast notifications @b require a shortcut to the application in the
        start menu. The start menu shortcut needs to contain an Application
        User Model ID. It is recommended that the applications setup creates the
        shortcut and the application specifies the setup created shortcut in
        @c shortcutPath. A call to this method will verify (and if necessary
        modify) the shortcut before enabling toast notifications.

        @param shortcutPath
            Path to a shortcut file referencing the applications executable. If
            the string is empty the applications display name will be used. If
            not fully qualified, it will be used as a path relative to the
            users start menu directory. The file extension .lnk is optional.
        @param appId
            The applications <a href="https://msdn.microsoft.com/en-us/library/windows/desktop/dd378459(vs.85).aspx">
            Application User Model ID</a>. If empty it will be extracted from
            the shortcut. If the shortcut does not contain an id an id will be
            automatically created from the applications vendor and app name.

        @return @false if toast notifications could not be enabled.

        @onlyfor{wxmsw}

        @see wxAppConsole::SetAppName(), wxAppConsole::SetVendorName()

        @since 3.1.0
    */
    static bool MSWUseToasts(
        const wxString& shortcutPath = wxString(),
        const wxString& appId = wxString());
};

