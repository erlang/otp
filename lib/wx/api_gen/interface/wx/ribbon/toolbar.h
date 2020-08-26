///////////////////////////////////////////////////////////////////////////////
// Name:        ribbon/toolbar.h
// Purpose:     interface of wxRibbonToolBar
// Author:      Peter Cawley
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////


/**
    @class wxRibbonToolBar

    A ribbon tool bar is similar to a traditional toolbar which has no labels.
    It contains one or more tool groups, each of which contains one or more
    tools. Each tool is represented by a (generally small, i.e. 16x15) bitmap.

    @beginEventEmissionTable{wxRibbonToolBarEvent}
    @event{EVT_RIBBONTOOLBAR_CLICKED(id, func)}
        Triggered when the normal (non-dropdown) region of a tool on the tool
        bar is clicked.
    @event{EVT_RIBBONTOOLBAR_DROPDOWN_CLICKED(id, func)}
        Triggered when the dropdown region of a tool on the tool bar is
        clicked. wxRibbonToolBarEvent::PopupMenu() should be called by the
        event handler if it wants to display a popup menu (which is what most
        dropdown tools should be doing).
    @endEventTable

    @library{wxribbon}
    @category{ribbon}
*/
class wxRibbonToolBar : public wxRibbonControl
{
public:
    /**
        Default constructor.
        With this constructor, Create() should be called in order to create
        the tool bar.
    */
    wxRibbonToolBar();

    /**
        Construct a ribbon tool bar with the given parameters.

        @param parent
            Parent window for the tool bar (typically a wxRibbonPanel).
        @param id
            An identifier for the toolbar. @c wxID_ANY is taken to mean a default.
        @param pos
            Initial position of the tool bar.
        @param size
            Initial size of the tool bar.
        @param style
            Tool bar style, currently unused.
    */
    wxRibbonToolBar(wxWindow* parent,
                  wxWindowID id = wxID_ANY,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = 0);

    /**
        Destructor.
    */
    virtual ~wxRibbonToolBar();

    /**
        Create a tool bar in two-step tool bar construction.
        Should only be called when the default constructor is used, and
        arguments have the same meaning as in the full constructor.
    */
    bool Create(wxWindow* parent,
                wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0);

    /**
        Add a tool to the tool bar (simple version).
    */
    virtual wxRibbonToolBarToolBase* AddTool(
                int tool_id,
                const wxBitmap& bitmap,
                const wxString& help_string,
                wxRibbonButtonKind kind = wxRIBBON_BUTTON_NORMAL);

    /**
        Add a dropdown tool to the tool bar (simple version).

        @see AddTool()
    */
    virtual wxRibbonToolBarToolBase* AddDropdownTool(
                int tool_id,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Add a hybrid tool to the tool bar (simple version).

        @see AddTool()
    */
    virtual wxRibbonToolBarToolBase* AddHybridTool(
                int tool_id,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Add a toggle tool to the tool bar (simple version).

        @since 2.9.4

        @see AddTool()
    */
    virtual wxRibbonToolBarToolBase* AddToggleTool(
        int tool_id,
        const wxBitmap& bitmap,
        const wxString& help_string);

    /**
        Add a tool to the tool bar.

        @param tool_id
            ID of the new tool (used for event callbacks).
        @param bitmap
            Bitmap to use as the foreground for the new tool. Does not have
            to be the same size as other tool bitmaps, but should be similar
            as otherwise it will look visually odd.
        @param bitmap_disabled
            Bitmap to use when the tool is disabled. If left as wxNullBitmap,
            then a bitmap will be automatically generated from @a bitmap.
        @param help_string
            The UI help string to associate with the new tool.
        @param kind
            The kind of tool to add.
        @param client_data
            Client data to associate with the new tool.

        @return An opaque pointer which can be used only with other tool bar
            methods.

        @see AddDropdownTool(), AddHybridTool(), AddSeparator(), InsertTool()
    */
    virtual wxRibbonToolBarToolBase* AddTool(
                int tool_id,
                const wxBitmap& bitmap,
                const wxBitmap& bitmap_disabled = wxNullBitmap,
                const wxString& help_string = wxEmptyString,
                wxRibbonButtonKind kind = wxRIBBON_BUTTON_NORMAL,
                wxObject* client_data = NULL);

    /**
        Add a separator to the tool bar.

        Separators are used to separate tools into groups. As such, a separator
        is not explicitly drawn, but is visually seen as the gap between tool
        groups.
    */
    virtual wxRibbonToolBarToolBase* AddSeparator();

    /**
        Insert a tool to the tool bar (simple version) as the specified
        position.

        @since 2.9.4

        @see InsertTool()
     */
    virtual wxRibbonToolBarToolBase* InsertTool(
                size_t pos,
                int tool_id,
                const wxBitmap& bitmap,
                const wxString& help_string,
                wxRibbonButtonKind kind = wxRIBBON_BUTTON_NORMAL);


    /**
        Insert a dropdown tool to the tool bar (simple version) as the specified
        position.

        @since 2.9.4

        @see AddDropdownTool(), InsertTool()
     */
    virtual wxRibbonToolBarToolBase* InsertDropdownTool(
                size_t pos,
                int tool_id,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Insert a hybrid tool to the tool bar (simple version) as the specified
        position.

        @since 2.9.4

        @see AddHybridTool(), InsertTool()
     */
    virtual wxRibbonToolBarToolBase* InsertHybridTool(
                size_t pos,
                int tool_id,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Insert a toggle tool to the tool bar (simple version) as the specified
        position.

        @since 2.9.4

       @see AddToggleTool(), InsertTool()
     */
    virtual wxRibbonToolBarToolBase* InsertToggleTool(
                size_t pos,
                int tool_id,
                const wxBitmap& bitmap,
                const wxString& help_string = wxEmptyString);

    /**
        Insert a tool to the tool bar at the specified position.

        @param pos
            Position of the new tool (number of tools and separators from the
            beginning of the toolbar).
        @param tool_id
            ID of the new tool (used for event callbacks).
        @param bitmap
            Bitmap to use as the foreground for the new tool. Does not have
            to be the same size as other tool bitmaps, but should be similar
            as otherwise it will look visually odd.
        @param bitmap_disabled
            Bitmap to use when the tool is disabled. If left as wxNullBitmap,
            then a bitmap will be automatically generated from @a bitmap.
        @param help_string
            The UI help string to associate with the new tool.
        @param kind
            The kind of tool to add.
        @param client_data
            Client data to associate with the new tool.

        @return An opaque pointer which can be used only with other tool bar
            methods.

        @since 2.9.4

        @see InsertDropdownTool(), InsertHybridTool(), InsertSeparator()
    */
    virtual wxRibbonToolBarToolBase* InsertTool(
                size_t pos,
                int tool_id,
                const wxBitmap& bitmap,
                const wxBitmap& bitmap_disabled = wxNullBitmap,
                const wxString& help_string = wxEmptyString,
                wxRibbonButtonKind kind = wxRIBBON_BUTTON_NORMAL,
                wxObject* client_data = NULL);

    /**
        Insert a separator to the tool bar at the specified position.

        @since 2.9.4

        @see AddSeparator(), InsertTool()
    */
    virtual wxRibbonToolBarToolBase* InsertSeparator(size_t pos);

    /**
        Deletes all the tools in the toolbar.

        @since 2.9.4
    */
    virtual void ClearTools();

    /**
        Removes the specified tool from the toolbar and deletes it.

        @param tool_id
            ID of the tool to delete.

        @returns @true if the tool was deleted, @false otherwise.

        @since 2.9.4

        @see DeleteToolByPos()
    */
    virtual bool DeleteTool(int tool_id);

    /**
        This function behaves like DeleteTool() but it deletes the tool at the
        specified position and not the one with the given id.
        Useful to delete separators.

        @since 2.9.4
    */
    virtual bool DeleteToolByPos(size_t pos);

    /**
        Returns a pointer to the tool opaque structure by @a id or @NULL if no
        corresponding tool is found.

        @since 2.9.4
    */
    virtual wxRibbonToolBarToolBase* FindById(int tool_id)const;

    /**
        Return the opaque pointer corresponding to the given tool.

        @return an opaque pointer, NULL if is a separator or not found.

        @since 2.9.4
    */
    wxRibbonToolBarToolBase* GetToolByPos(size_t pos)const

    /**
        Returns the number of tools in the toolbar.

        @since 2.9.4
    */
    virtual size_t GetToolCount() const;

    /**
        Return the id associated to the tool opaque structure.

        The structure pointer must not be @NULL.

        @since 2.9.4
    */
    virtual int GetToolId(const wxRibbonToolBarToolBase* tool)const;

    /**
        Get any client data associated with the tool.

        @param tool_id
            ID of the tool in question, as passed to AddTool().

        @return Client data, or @NULL if there is none.

        @since 2.9.4
    */
    virtual wxObject* GetToolClientData(int tool_id)const;

    /**
        Called to determine whether a tool is enabled (responds to user input).

        @param tool_id
            ID of the tool in question, as passed to AddTool().

        @return @true if the tool is enabled, @false otherwise.

        @since 2.9.4

        @see EnableTool()
    */
    virtual bool GetToolEnabled(int tool_id)const;

    /**
        Returns the help string for the given tool.

        @param tool_id
            ID of the tool in question, as passed to AddTool().

        @since 2.9.4
    */
    virtual wxString GetToolHelpString(int tool_id)const;

    /**
        Return the kind of the given tool.

        @param tool_id
            ID of the tool in question, as passed to AddTool().

        @since 2.9.4
    */
    virtual wxRibbonButtonKind GetToolKind(int tool_id)const;

    /**
        Returns the tool position in the toolbar, or @c wxNOT_FOUND if the tool
        is not found.

        @param tool_id
            ID of the tool in question, as passed to AddTool().

        @since 2.9.4
    */
    virtual int GetToolPos(int tool_id)const;

    /**
        Gets the on/off state of a toggle tool.

        @param tool_id
            ID of the tool in question, as passed to AddTool().

        @return @true if the tool is toggled on, @false otherwise.

        @see ToggleTool()

        @since 2.9.4
    */
    virtual bool GetToolState(int tool_id)const;

    /**
        Calculate tool layouts and positions.

        Must be called after tools are added to the tool bar, as otherwise
        the newly added tools will not be displayed.
    */
    virtual bool Realize();

    /**
        Set the number of rows to distribute tool groups over.

        Tool groups can be distributed over a variable number of rows. The way
        in which groups are assigned to rows is not specified, and the order
        of groups may change, but they will be distributed in such a way as to
        minimise the overall size of the tool bar.

        @param nMin
            The minimum number of rows to use.
        @param nMax
            The maximum number of rows to use (defaults to nMin).
    */
    virtual void SetRows(int nMin, int nMax = -1);

    /**
        Sets the client data associated with the tool.

        @param tool_id
            ID of the tool in question, as passed to AddTool().
        @param clientData
            The client data to use.

        @since 2.9.4
    */
    virtual void SetToolClientData(int tool_id, wxObject* clientData);

    /**
        Sets the bitmap to be used by the tool with the given ID when the tool
        is in a disabled state.

        @param tool_id
            ID of the tool in question, as passed to AddTool().
        @param bitmap
            Bitmap to use for disabled tools.

        @since 2.9.4
    */
    virtual void SetToolDisabledBitmap(int tool_id, const wxBitmap &bitmap);

    /**
        Sets the help string shown in tooltip for the given tool.

        @param tool_id
            ID of the tool in question, as passed to AddTool().
        @param helpString
            A string for the help.

        @see GetToolHelpString()

        @since 2.9.4
    */
    virtual void SetToolHelpString(int tool_id, const wxString& helpString);

    /**
        Sets the bitmap to be used by the tool with the given ID.

        @param tool_id
            ID of the tool in question, as passed to AddTool().
        @param bitmap
            Bitmap to use for normals tools.

        @since 2.9.4
    */
    virtual void SetToolNormalBitmap(int tool_id, const wxBitmap &bitmap);

    /**
        Enable or disable a single tool on the bar.

        @param tool_id
            ID of the tool to enable or disable.
        @param enable
            @true to enable the tool, @false to disable it.

        @since 2.9.4
    */
    virtual void EnableTool(int tool_id, bool enable = true);

    /**
        Set a toggle tool to the checked or unchecked state.

        @param tool_id
            ID of the toggle tool to manipulate.
        @param checked
            @true to set the tool to the toggled/pressed/checked state,
            @false to set it to the untoggled/unpressed/unchecked state.

        @since 2.9.4
    */
    virtual void ToggleTool(int tool_id, bool checked);
};


class wxRibbonToolBarEvent : public wxCommandEvent
{
public:
    wxRibbonToolBarEvent(wxEventType command_type = wxEVT_NULL,
                       int win_id = 0,
                         wxRibbonToolBar* bar = NULL);

    wxRibbonToolBar* GetBar();
    void SetBar(wxRibbonToolBar* bar);
    bool PopupMenu(wxMenu* menu);
};


wxEventType wxEVT_RIBBONTOOLBAR_CLICKED;
wxEventType wxEVT_RIBBONTOOLBAR_DROPDOWN_CLICKED;
