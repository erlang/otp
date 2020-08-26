/////////////////////////////////////////////////////////////////////////////
// Name:        taskbarbutton.h
// Purpose:     Interface of taskbar buttons features.
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    State of the taskbar button.
    @since 3.1.0
*/
enum wxTaskBarButtonState
{
    wxTASKBAR_BUTTON_NO_PROGRESS   = 0,
    wxTASKBAR_BUTTON_INDETERMINATE = 1,
    wxTASKBAR_BUTTON_NORMAL        = 2,
    wxTASKBAR_BUTTON_ERROR         = 4,
    wxTASKBAR_BUTTON_PAUSED        = 8
};

/**
    @class wxThumbBarButton

    A thumbnail toolbar button is a control that displayed in the thumbnail
    image of a window in a taskbar button flyout.

    @library{wxcore}
    @category{misc}

    @onlyfor{wxmsw}

    @see wxTaskBarButton
    @since 3.1.0
*/
class WXDLLIMPEXP_CORE wxThumbBarButton {
public:
    /**
        Default constructor to allow 2-phase creation.
    */
    wxThumbBarButton();

    /**
        Constructs the thumbnail toolbar button.

        @param id
            The identifier for the control.
        @param icon
            The icon used as the button image.
        @param tooltip
            The text of the button's tooltip, displayed when the mouse pointer
            hovers over the button.
        @param enable
            If @true (default), the button is active and available to the user.
            If @false, the button is disabled. It is present, but has a visual
            state that indicates that it will not respond to user action.
        @param dismissOnClick
            If @true, when the button is clicked, the taskbar button's flyout
            closes immediately. @false by default.
        @param hasBackground
            If @false, the button border is not drawn. @true by default.
        @param shown
            If @false, the button is not shown to the user. @true by default.
        @param interactive
            If @false, the button is enabled but not interactive; no pressed
            button state is drawn. This flag is intended for instances where
            the button is used in a notification. @true by default.
    */
    wxThumbBarButton(int id,
                     const wxIcon& icon,
                     const wxString& tooltip = wxString(),
                     bool enable = true,
                     bool dismissOnClick = false,
                     bool hasBackground = true,
                     bool shown = true,
                     bool interactive = true);

    bool Create(int id,
                const wxIcon& icon,
                const wxString& tooltip = wxString(),
                bool enable = true,
                bool dismissOnClick = false,
                bool hasBackground = true,
                bool shown = true,
                bool interactive = true);

    virtual ~wxThumbBarButton();

    /**
        Returns the identifier associated with this control.
    */
    int GetID() const;

    /**
        Returns the icon associated with this control.
    */
    const wxIcon& GetIcon() const;

    /**
        Returns the tooltip.
    */
    const wxString& GetTooltip() const;

    /**
        Returns @true if the button is enabled, @false if it has been disabled.
    */
    bool IsEnable() const;

    /**
        Enables or disables the thumbnail toolbar button.
    */
    void Enable(bool enable = true);

    /**
        Equivalent to calling wxThumbBarButton::Enable(false).
    */
    void Disable();

    /**
        Returns @true if the button will dismiss on click.
    */
    bool IsDismissOnClick() const;

    /**
        Whether the window thumbnail is dismissed after a button click.
    */
    void EnableDismissOnClick(bool enable = true);

    /**
        Equivalent to calling wxThumbBarButton::DisableDimissOnClick(false).
    */
    void DisableDimissOnClick();

    /**
        Returns @true if the button has button border.
    */
    bool HasBackground() const;

    /**
        Set the property that whether the button has background.
    */
    void SetHasBackground(bool has = true);

    /**
        Returns @true if the button is shown, @false if it has been hidden.
    */
    bool IsShown() const;

    /**
        Show or hide the thumbnail toolbar button.
    */
    void Show(bool shown = true);

    /**
        Hide the thumbnail toolbar button. Equivalent to calling wxThumbBarButton::Show(false).
    */
    void Hide();

    /**
        Returns @true if the button is interactive.
    */
    bool IsInteractive() const;

    /**
        Set the property which holds whether the button is interactive.

        A non-interactive thumbnail toolbar button does not react to user
        interaction, but is still visually enabled.
    */
    void SetInteractive(bool interactive = true);
};

/**
    @class wxTaskBarButton

    A taskbar button that associated with the window under Windows 7 or later.

    It is used to access the functionality including thumbnail representations,
    thumbnail toolbars, notification and status overlays, and progress
    indicators.

    @note This class is only created and initialized in the internal implementation
    of wxFrame by design. You can only get the pointer of the instance which
    associated with the frame by calling wxFrame::MSWGetTaskBarButton().

    @library{wxcore}
    @category{misc}

    @nativeimpl{wxmsw}
    @onlyfor{wxmsw}

    @see wxFrame::MSWGetTaskBarButton()
    @since 3.1.0
*/
class WXDLLIMPEXP_CORE wxTaskBarButton
{
public:
    /**
        Starts showing a determinate progress indicator.

        Call SetProgressValue() after this call to update the progress
        indicator.

        If @a range is 0, the progress indicator is dismissed.
    */
    virtual void SetProgressRange(int range);

    /**
        Update the progress indicator, setting the progress to the new value .

        @param value Must be in the range from 0 to the argument to the last
        SetProgressRange() call. When it is equal to the range, the progress
        bar is dismissed.
    */
    virtual void SetProgressValue(int value);

    /**
        Makes the progress indicator run in indeterminate mode.

        The first call to this method starts showing the indeterminate progress
        indicator if it hadn't been shown yet.

        Call SetProgressRange(0) to stop showing the progress indicator.
    */
    virtual void PulseProgress();

    /**
        Show in the taskbar.
    */
    virtual void Show(bool show = true);

    /**
        Hide in the taskbar.
    */
    virtual void Hide();

    /**
        Specifies or updates the text of the tooltip that is displayed
        when the mouse pointer rests on an individual preview thumbnail
        in a taskbar button flyout.
    */
    virtual void SetThumbnailTooltip(const wxString& tooltip);

    /**
        Set the state of the progress indicator displayed on a taskbar button.

        @see wxTaskBarButtonState
    */
    virtual void SetProgressState(wxTaskBarButtonState state);

    /**
        Set an overlay icon to indicate application status or a notification top
        the user.

        @param icon
            This should be a small icon, measuring 16x16 pixels at 96 dpi. If an
            overlay icon is already applied to the taskbar button, that existing
            overlay is replaced. Setting with wxNullIcon to remove.
        @param description
            The property holds the description of the overlay for accessibility
            purposes.
    */
    virtual void SetOverlayIcon(const wxIcon& icon,
                                const wxString& description = wxString());

    /**
        Selects a portion of a window's client area to display as that window's
        thumbnail in the taskbar.

        @param  rect
            The portion inside of the window. Setting with an empty wxRect will
            restore the default display of the thumbnail.
    */
    virtual void SetThumbnailClip(const wxRect& rect);

    /**
        Selects the child window area to display as that window's thumbnail in
        the taskbar.
    */
    virtual void SetThumbnailContents(const wxWindow *child);

    /**
        Inserts the given button before the position pos to the taskbar
        thumbnail toolbar.

        @note The number of buttons and separators is limited to 7.

        @see AppendThumbBarButton(), AppendSeparatorInThumbBar()
    */
    virtual bool InsertThumbBarButton(size_t pos, wxThumbBarButton *button);

    /**
        Appends a button to the taskbar thumbnail toolbar.

        @note The number of buttons and separators is limited to 7.

        @see InsertThumbBarButton(), AppendSeparatorInThumbBar()
    */
    virtual bool AppendThumbBarButton(wxThumbBarButton *button);

    /**
        Appends a separator to the taskbar thumbnail toolbar.

        @note The number of buttons and separators is limited to 7.

        @see AppendThumbBarButton(), InsertThumbBarButton()
    */
    virtual bool AppendSeparatorInThumbBar();

    /**
        Removes the thumbnail toolbar button from the taskbar button but doesn't
        delete the associated c++ object.

        @param button
            The thumbnail toolbar button to remove.

        @return A pointer to the button which was detached from the taskbar
            button.
    */
    virtual wxThumbBarButton* RemoveThumbBarButton(wxThumbBarButton *button);

    /**
        Removes the thumbnail toolbar button from the taskbar button but doesn't
        delete the associated c++ object.

        @param id
            The identifier of the thumbnail toolbar button to remove.

        @return A pointer to the button which was detached from the taskbar
            button.
    */
    virtual wxThumbBarButton* RemoveThumbBarButton(int id);
};

/**
    Type of jump list item.

    @since 3.1.0
*/
enum wxTaskBarJumpListItemType
{
    /** A separator, Only tasks category supports separators. */
    wxTASKBAR_JUMP_LIST_SEPARATOR,

    /** A task, represents a link to application. */
    wxTASKBAR_JUMP_LIST_TASK,

    /** Item acts as a link to a file that the application can open. */
    wxTASKBAR_JUMP_LIST_DESTINATION
};

/**
    @class wxTaskBarJumpListItem

    A wxTaskBarJumpListItem represents an item in a jump list category.

    @library{wxcore}
    @category{misc}

    @onlyfor{wxmsw}
    @since 3.1.0
*/
class WXDLLIMPEXP_CORE wxTaskBarJumpListItem
{
public:
    /**
        Constructs a jump list item.

        @param parentCategory
            Category that the jump list item belongs to. Can be NULL if the item
            is going to be added to the category later.
        @param type
            The type for this item.
        @param title
            The title of this item.
        @param filePath
            The filePath of this item, the meaning of which depends on the type
            of this item: If the item type is wxTASKBAR_JUMP_LIST_DESTINATION,
            filePath is the path to a file that can be opened by an application.
            If the item type is wxTASKBAR_JUMP_LIST_TASK, filePath is the path
            to an executable that is executed when this item is clicked by the
            user.
        @param arguments
            The command-line arguments of this item.
        @param tooltip
            The description tooltip of this item.
        @param iconPath
            The path to the file containing the icon.
        @param iconIndex
            The index of the icon, which is specified by iconPath.
    */
    wxTaskBarJumpListItem(wxTaskBarJumpListCategory *parentCategory = NULL,
        wxTaskBarJumpListItemType type = wxTASKBAR_JUMP_LIST_SEPARATOR,
        const wxString& title = wxEmptyString,
        const wxString& filePath = wxEmptyString,
        const wxString& arguments = wxEmptyString,
        const wxString& tooltip = wxEmptyString,
        const wxString& iconPath = wxEmptyString,
        int iconIndex = 0);

    /**
        Returns the type of this item.
    */
    wxTaskBarJumpListItemType GetType() const;

    /**
        Sets the type of this item.
    */
    void SetType(wxTaskBarJumpListItemType type);

    /**
        Returns the title of this item.
    */
    const wxString& GetTitle() const;

    /**
        Sets the title of this item.
    */
    void SetTitle(const wxString& title);

    /**
        Returns the file path of this item.
    */
    const wxString& GetFilePath() const;

    /**
        Sets the file path of this item.
    */
    void SetFilePath(const wxString& filePath);

    /**
        Returns the command-line arguments of this item.
    */
    const wxString& GetArguments() const;

    /**
        Sets the command-line arguments of this item.
    */
    void SetArguments(const wxString& arguments);

    /**
        Returns the description tooltip of this item.
    */
    const wxString& GetTooltip() const;

    /**
        Sets the description tooltip of this item.
    */
    void SetTooltip(const wxString& tooltip);

    /**
        Returns the icon path of this item.
    */
    const wxString& GetIconPath() const;

    /**
        Sets the icon path of this item.
    */
    void SetIconPath(const wxString& iconPath);

    /**
        Returns the icon index of icon in this item.
    */
    int GetIconIndex() const;

    /**
        Sets the icon index of icon in this item.
    */
    void SetIconIndex(int iconIndex);

    /**
        Returns the category this jump list item is in, or NULL if this jump
        list item is not attached.
    */
    wxTaskBarJumpListCategory* GetCategory() const;

    /**
        Sets the parent category which will contain this jump list item.
    */
    void SetCategory(wxTaskBarJumpListCategory *category);
};

/**
    A vector of wxTaskBarJumpListItem pointers.

    @since 3.1.0
*/
typedef wxVector<wxTaskBarJumpListItem*> wxTaskBarJumpListItems;

/**
    @class wxTaskBarJumpListCategory

    This class represents a category of jump list in the taskbar button. There
    are four kinds of categories in Windows: Recent, Frequent, Tasks and
    custom.

    @library{wxcore}
    @category{misc}

    @onlyfor{wxmsw}

    @see wxTaskBarJumpList, wxTaskBarJumpListItem

    @since 3.1.0
*/
class WXDLLIMPEXP_CORE wxTaskBarJumpListCategory
{
public:
    /**
        Constructs the jump list category.

        @param parent
            Jump list that the jump list category belongs to. Can be NULL if
            the category is going to be added to the jump list later.
        @param title
            The title of the category.
    */
    wxTaskBarJumpListCategory(wxTaskBarJumpList *parent = NULL,
                              const wxString& title = wxEmptyString);
    virtual ~wxTaskBarJumpListCategory();

    /**
        Appends a jump list item.

        @param item
            The jump list item to be appended. It will be owned by the
            wxTaskBarJumpListCategory object after this function is called, so
            do not delete it yourself.

        @see Insert(), Prepend()
    */
    wxTaskBarJumpListItem* Append(wxTaskBarJumpListItem *item);

    /**
        Deletes the jump list item from the category.

        @param item
            The jump list item to be deleted.

        @see Remove()
    */
    void Delete(wxTaskBarJumpListItem *item);

    /**
        Removes the jump list item from the category but doesn't delete the
        associated C++ object.

        @param item
            The jump list item to be removed.
    */
    wxTaskBarJumpListItem* Remove(wxTaskBarJumpListItem *item);

    /**
        Returns the wxTaskBarJumpListItem given a position in the category.
    */
    wxTaskBarJumpListItem* FindItemByPosition(size_t pos) const;

    /**
        Inserts the given item before the position pos.

        @see Append(), Prepend()
    */
    wxTaskBarJumpListItem* Insert(size_t pos, wxTaskBarJumpListItem *item);

    /**
        Inserts the given item at position 0, i.e. before all the other existing
        items.

        @see Append(), Insert();
    */
    wxTaskBarJumpListItem* Prepend(wxTaskBarJumpListItem *item);

    /**
        Sets the title of the category.
    */
    void SetTitle(const wxString& title);

    /**
        Gets the title of the category.
    */
    const wxString& GetTitle() const;

    /**
        Gets the jump list items of the category.
    */
    const wxTaskBarJumpListItems& GetItems() const;
};

/**
    A vector of wxTaskBarJumpListCategory pointers.
*/
typedef wxVector<wxTaskBarJumpListCategory*> wxTaskBarJumpListCategories;

/**
    @class wxTaskBarJumpList

    This class is an transparent wrapper around Windows Jump Lists. Jump
    Lists, as an new feature since Windows 7, are lists of recently opened
    items, such as files, folders, or websites, which are organized by the
    program that the user use to open them. Jump Lists don't just show
    shortcuts to files. Sometimes they can also provide quick access to tasks.
    With this class, you can access the recent and frequent category in the
    jump lists. You can also set the tasks category of the Jump Lists of your
    application. What's more, you can add custom category to them.

    @library{wxcore}
    @category{misc}

    @onlyfor{wxmsw}

    @see wxTaskBarJumpListCategory, wxTaskBarJumpListItem
    @since 3.1.0
*/
class WXDLLIMPEXP_CORE wxTaskBarJumpList
{
public:
    /**
        Constructs the jump list.

        @param appID
            Specifies a unique identifier for the application jump list, can be
            empty by default.

            See <a href="http://msdn.microsoft.com/en-us/library/windows/desktop/dd378459(v=vs.85).aspx">
            Application User Model IDs</a> on MSDN for further details.
    */
    wxTaskBarJumpList(const wxString& appID = wxEmptyString);
    virtual ~wxTaskBarJumpList();

    /**
        Shows or hides the recent category.
    */
    void ShowRecentCategory(bool shown = true);

    /**
        Hides the recent category. Equivalent to calling wxTaskBarJumpList::ShowFrequentCategory(false).
    */
    void HideRecentCategory();

    /**
        Shows or hides the frequent category.
    */
    void ShowFrequentCategory(bool shown = true);

    /**
        Hides the frequent category. Equivalent to calling wxTaskBarJumpList::ShowFrequentCategory(false).
    */
    void HideFrequentCategory();

    /**
        Accesses the built in tasks category.

        With the returned tasks category, you can append an new task, remove
        an existing task, modify the task item etc.
    */
    wxTaskBarJumpListCategory& GetTasks() const;

    /**
        Gets the built in frequent category.

        Note that the returned category is read-only.
    */
    const wxTaskBarJumpListCategory& GetFrequentCategory() const;

    /**
        Gets the built in recent category.

        Note that the returned category is read-only.

    */
    const wxTaskBarJumpListCategory& GetRecentCategory() const;

    /**
        Gets the custom categories.
    */
    const wxTaskBarJumpListCategories& GetCustomCategories() const;

    /**
        Add an new custom category.

        @param category
            A wxTaskBarJumpListCategory object. It will be owned by the
            wxTaskBarJumpList object after this function is called, so
            do not delete it yourself.
    */
    void AddCustomCategory(wxTaskBarJumpListCategory* category);

    /**
        Removes the custom category from the jump lists but doesn't delete the
        associated C++ object.

        @param title
            The title of the custom category.
    */
    wxTaskBarJumpListCategory* RemoveCustomCategory(const wxString& title);

    /**
        Deletes the custom category from the jump lists.

        @param title
            The title of the custom category.
    */
    void DeleteCustomCategory(const wxString& title);
};
