/////////////////////////////////////////////////////////////////////////////
// Name:        grid.h
// Purpose:     interface of wxGrid and related classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/// Magic constant which tells (to some functions) to automatically calculate
/// the appropriate size
#define wxGRID_AUTOSIZE (-1)


/**
    @class wxGridCellRenderer

    This class is responsible for actually drawing the cell in the grid. You
    may pass it to the wxGridCellAttr (below) to change the format of one given
    cell or to wxGrid::SetDefaultRenderer() to change the view of all cells.
    This is an abstract class, and you will normally use one of the predefined
    derived classes or derive your own class from it.

    @library{wxcore}
    @category{grid}

    @see wxGridCellAutoWrapStringRenderer, wxGridCellBoolRenderer,
         wxGridCellDateTimeRenderer, wxGridCellEnumRenderer,
         wxGridCellFloatRenderer, wxGridCellNumberRenderer,
         wxGridCellStringRenderer
*/
class wxGridCellRenderer : public wxClientDataContainer, public wxRefCounter
{
public:
    wxGridCellRenderer();

    /**
        This function must be implemented in derived classes to return a copy
        of itself.
    */
    virtual wxGridCellRenderer* Clone() const = 0;

    /**
        Draw the given cell on the provided DC inside the given rectangle using
        the style specified by the attribute and the default or selected state
        corresponding to the isSelected value.

        This pure virtual function has a default implementation which will
        prepare the DC using the given attribute: it will draw the rectangle
        with the background colour from attr and set the text colour and font.
    */
    virtual void Draw(wxGrid& grid, wxGridCellAttr& attr, wxDC& dc,
                      const wxRect& rect, int row, int col,
                      bool isSelected) = 0;

    /**
        Get the preferred size of the cell for its contents.

        This method must be overridden in the derived classes to return the
        minimal fitting size for displaying the content of the given grid cell.

        @see GetBestHeight(), GetBestWidth()
    */
    virtual wxSize GetBestSize(wxGrid& grid, wxGridCellAttr& attr, wxDC& dc,
                               int row, int col) = 0;

    /**
        Get the preferred height of the cell at the given width.

        Some renderers may not have a well-defined best size, but only be able
        to provide the best height at the given width, e.g. this is the case of
        the standard wxGridCellAutoWrapStringRenderer. In this case, they
        should override this method, in addition to GetBestSize().

        @see GetBestWidth()

        @since 3.1.0
    */
    virtual int GetBestHeight(wxGrid& grid, wxGridCellAttr& attr, wxDC& dc,
                              int row, int col, int width);

    /**
        Get the preferred width of the cell at the given height.

        See GetBestHeight(), this method is symmetric to it.

        @since 3.1.0
    */
    virtual int GetBestWidth(wxGrid& grid, wxGridCellAttr& attr, wxDC& dc,
                             int row, int col, int height);

    /**
        Get the maximum possible size for a cell using this renderer, if
        possible.

        This function may be overridden in the derived class if it can return
        the maximum size needed for displaying the cells rendered it without
        iterating over all cells. The base class version simply returns
        ::wxDefaultSize, indicating that this is infeasible and that
        GetBestSize() should be called for each cell individually.

        Note that this method will only be used if
        wxGridTableBase::CanMeasureColUsingSameAttr() is overridden to return
        @true.

        @since 3.1.4
     */
    virtual wxSize GetMaxBestSize(wxGrid& grid,
                                  wxGridCellAttr& attr,
                                  wxDC& dc);

protected:
    /**
        The destructor is private because only DecRef() can delete us.
    */
    virtual ~wxGridCellRenderer();
};

/**
    Smart pointer wrapping wxGridCellRenderer.

    wxGridCellRendererPtr takes ownership of wxGridCellRenderer passed to it on
    construction and calls DecRef() on it automatically when it is destroyed.
    It also provides transparent access to wxGridCellRenderer methods by allowing
    to use objects of this class as if they were wxGridCellRenderer pointers.

    @since 3.1.4

    @category{grid}
*/
typedef wxObjectDataPtr<wxGridCellRenderer> wxGridCellRendererPtr;

/**
    @class wxGridCellAutoWrapStringRenderer

    This class may be used to format string data in a cell. The too
    long lines are wrapped to be shown entirely at word boundaries.

    @library{wxcore}
    @category{grid}

    @see wxGridCellRenderer, wxGridCellBoolRenderer,
         wxGridCellDateTimeRenderer, wxGridCellEnumRenderer,
         wxGridCellFloatRenderer, wxGridCellNumberRenderer,
         wxGridCellStringRenderer
*/

class wxGridCellAutoWrapStringRenderer : public wxGridCellStringRenderer
{
public:
    /**
        Default constructor.
    */
    wxGridCellAutoWrapStringRenderer();
};


/**
    @class wxGridCellBoolRenderer

    This class may be used to format boolean data in a cell.

    @library{wxcore}
    @category{grid}

    @see wxGridCellRenderer, wxGridCellAutoWrapStringRenderer,
         wxGridCellDateTimeRenderer, wxGridCellEnumRenderer,
         wxGridCellFloatRenderer, wxGridCellNumberRenderer,
         wxGridCellStringRenderer
*/
class wxGridCellBoolRenderer : public wxGridCellRenderer
{
public:
    /**
        Default constructor.
    */
    wxGridCellBoolRenderer();
};

/**
    @class wxGridCellDateRenderer

    This class may be used to show a date, without time, in a cell.

    See @ref wxGridCellDateTimeRenderer for a date/time version.
    wxDateTime::Format() is used internally to render the date
    representation. wxDateTime::ParseDate() is used to parse the string
    data entered in the cell.

    @library{wxcore}
    @category{grid}

    @see wxGridCellRenderer, wxGridCellAutoWrapStringRenderer,
         wxGridCellBoolRenderer, wxGridCellEnumRenderer,
         wxGridCellFloatRenderer, wxGridCellNumberRenderer,
         wxGridCellStringRenderer, wxGridCellDateTimeRenderer

    @since 3.1.3
*/
class wxGridCellDateRenderer : public wxGridCellStringRenderer
{
public:
    /**
        Date renderer constructor.

        @param outformat
            strftime()-like format string used to render the output date.
            By default (or if provided format string is empty) localized
            date representation ("%x") is used.
    */
    wxGridCellDateRenderer(const wxString& outformat = wxString());


    /**
        Sets the strftime()-like format string which will be used to render
        the date.

        @param params
            strftime()-like format string used to render the date.
    */
    virtual void SetParameters(const wxString& params);
};

/**
    @class wxGridCellDateTimeRenderer

    This class may be used to format a date/time data in a cell.
    The class wxDateTime is used internally to display the local date/time
    or to parse the string date entered in the cell thanks to the defined format.

    @library{wxcore}
    @category{grid}

    @see wxGridCellRenderer, wxGridCellAutoWrapStringRenderer,
         wxGridCellBoolRenderer, wxGridCellEnumRenderer,
         wxGridCellFloatRenderer, wxGridCellNumberRenderer,
         wxGridCellStringRenderer, wxGridCellDateRenderer
*/
class wxGridCellDateTimeRenderer : public wxGridCellDateRenderer
{
public:
    /**
        Date/time renderer constructor.

        @param outformat
            strptime()-like format string used the parse the output date/time.
        @param informat
            strptime()-like format string used to parse the string entered in the cell.
    */
    wxGridCellDateTimeRenderer(const wxString& outformat = wxDefaultDateTimeFormat,
                               const wxString& informat = wxDefaultDateTimeFormat);


    /**
        Sets the strptime()-like format string which will be used to parse
        the date/time.

        @param params
            strptime()-like format string used to parse the date/time.
    */
    virtual void SetParameters(const wxString& params);
};

/**
    @class wxGridCellEnumRenderer

    This class may be used to render in a cell a number as a textual
    equivalent.

    The corresponding text strings are specified as comma-separated items in
    the string passed to this renderer ctor or SetParameters() method. For
    example, if this string is @c "John,Fred,Bob" the cell will be rendered as
    "John", "Fred" or "Bob" if its contents is 0, 1 or 2 respectively.

    @library{wxcore}
    @category{grid}

    @see wxGridCellRenderer, wxGridCellAutoWrapStringRenderer,
         wxGridCellBoolRenderer, wxGridCellDateTimeRenderer,
         wxGridCellFloatRenderer, wxGridCellNumberRenderer,
         wxGridCellStringRenderer
*/
class wxGridCellEnumRenderer : public wxGridCellStringRenderer
{
public:
    /**
        Enum renderer ctor.

        @param choices
            Comma separated string parameters "item1[,item2[...,itemN]]".
    */
    wxGridCellEnumRenderer( const wxString& choices = wxEmptyString );

    /**
        Sets the comma separated string content of the enum.

        @param params
            Comma separated string parameters "item1[,item2[...,itemN]]".
    */
    virtual void SetParameters(const wxString& params);
};

/**
    Specifier used to format the data to string for the numbers handled by
    wxGridCellFloatRenderer and wxGridCellFloatEditor.

    @since 2.9.3
*/
enum wxGridCellFloatFormat
{
    /// Decimal floating point (%f).
    wxGRID_FLOAT_FORMAT_FIXED       = 0x0010,

    /// Scientific notation (mantissa/exponent) using e character (%e).
    wxGRID_FLOAT_FORMAT_SCIENTIFIC  = 0x0020,

    /// Use the shorter of %e or %f (%g).
    wxGRID_FLOAT_FORMAT_COMPACT     = 0x0040,

    /// To use in combination with one of the above formats for the upper
    /// case version (%F/%E/%G)
    wxGRID_FLOAT_FORMAT_UPPER       = 0x0080,

    /// The format used by default (wxGRID_FLOAT_FORMAT_FIXED).
    wxGRID_FLOAT_FORMAT_DEFAULT     = wxGRID_FLOAT_FORMAT_FIXED
};

/**
    @class wxGridCellFloatRenderer

    This class may be used to format floating point data in a cell.

    @library{wxcore}
    @category{grid}

    @see wxGridCellRenderer, wxGridCellAutoWrapStringRenderer,
         wxGridCellBoolRenderer, wxGridCellDateTimeRenderer,
         wxGridCellEnumRenderer, wxGridCellNumberRenderer,
         wxGridCellStringRenderer
*/
class wxGridCellFloatRenderer : public wxGridCellStringRenderer
{
public:
    /**
        Float cell renderer ctor.

        @param width
            Minimum number of characters to be shown.
        @param precision
            Number of digits after the decimal dot.
        @param format
            The format used to display the string, must be a combination of
            ::wxGridCellFloatFormat enum elements. This parameter is only
            available since wxWidgets 2.9.3.
    */
    wxGridCellFloatRenderer(int width = -1, int precision = -1,
                            int format = wxGRID_FLOAT_FORMAT_DEFAULT);

    /**
        Returns the specifier used to format the data to string.

        The returned value is a combination of ::wxGridCellFloatFormat elements.

        @since 2.9.3
    */
    int GetFormat() const;

    /**
        Returns the precision.
    */
    int GetPrecision() const;

    /**
        Returns the width.
    */
    int GetWidth() const;

    /**
        Set the format to use for display the number.

        @param format
            Must be a combination of ::wxGridCellFloatFormat enum elements.

        @since 2.9.3
    */
    void SetFormat(int format);

    /**
        The parameters string format is "width[,precision[,format]]" where
        @c format should be chosen between f|e|g|E|G (f is used by default)
    */
    virtual void SetParameters(const wxString& params);

    /**
        Sets the precision.
    */
    void SetPrecision(int precision);

    /**
        Sets the width.
    */
    void SetWidth(int width);
};

/**
    @class wxGridCellNumberRenderer

    This class may be used to format integer data in a cell.

    @library{wxcore}
    @category{grid}

    @see wxGridCellRenderer, wxGridCellAutoWrapStringRenderer,
         wxGridCellBoolRenderer, wxGridCellDateTimeRenderer,
         wxGridCellEnumRenderer, wxGridCellFloatRenderer,
         wxGridCellStringRenderer
*/
class wxGridCellNumberRenderer : public wxGridCellStringRenderer
{
public:
    /**
        Default constructor.
    */
    wxGridCellNumberRenderer();
};

/**
    @class wxGridCellStringRenderer

    This class may be used to format string data in a cell; it is the default
    for string cells.

    @library{wxcore}
    @category{grid}

    @see wxGridCellRenderer, wxGridCellAutoWrapStringRenderer,
         wxGridCellBoolRenderer, wxGridCellDateTimeRenderer,
         wxGridCellEnumRenderer, wxGridCellFloatRenderer,
         wxGridCellNumberRenderer
*/
class wxGridCellStringRenderer : public wxGridCellRenderer
{
public:
    /**
        Default constructor.
    */
    wxGridCellStringRenderer();
};


/**
    Represents a source of cell activation, which may be either a user event
    (mouse or keyboard) or the program itself.

    An object of this class is passed to wxGridCellEditor::TryActivate() by the
    library and the code overriding this method may use its GetOrigin() method
    to determine how exactly the cell is being activated.

    @since 3.1.4
 */
class wxGridActivationSource
{
public:
    /// Result of GetOrigin().
    enum Origin
    {
        /// Activated due to an explicit wxGrid::EnableCellEditControl() call.
        Program,

        /// Activated due to the user pressing a key, see GetKeyEvent().
        Key,

        /// Activated due to the user clicking on a cell, see GetMouseEvent().
        Mouse
    };

    /// Get the origin of the activation.
    Origin GetOrigin() const;

    /**
        Get the key event corresponding to the key press activating the cell.

        This method can be called for objects with Key origin only, use
        GetOrigin() to check for this first.
     */
    const wxKeyEvent& GetKeyEvent() const;

    /**
        Get the mouse event corresponding to the click activating the cell.

        This method can be called for objects with Mouse origin only, use
        GetOrigin() to check for this first.
     */
    const wxMouseEvent& GetMouseEvent() const;
};

/**
    Represents the result of wxGridCellEditor::TryActivate().

    Editors overriding wxGridCellEditor::TryActivate() must use one of
    DoNothing(), DoChange() or DoEdit() methods to return an object of this
    type corresponding to the desired action.

    @since 3.1.4
 */
class wxGridActivationResult
{
public:
    /**
        Indicate that nothing should be done and the cell shouldn't be edited
        at all.

        Note that this is different from DoEdit() and may be useful when the
        value of the cell wouldn't change if it were activated anyhow, e.g.
        because the key or mouse event carried by wxGridActivationSource would
        leave the cell value unchanged.
     */
    static wxGridActivationResult DoNothing();

    /**
        Indicate that activating the cell is possible and would change its
        value to the given one.

        This is the method to call for activatable editors, using it will
        result in changing the value of the cell to @a newval without showing
        the editor control at all.

        Note that the change may still be vetoed by wxEVT_GRID_CELL_CHANGING
        handler.
     */
    static wxGridActivationResult DoChange(const wxString& newval);

    /**
        Indicate that the editor control should be shown and the cell should be
        edited normally.

        This is the default return value of wxGridCellEditor::TryActivate().
     */
    static wxGridActivationResult DoEdit();
};

/**
    @class wxGridCellEditor

    This class is responsible for providing and manipulating the in-place edit
    controls for the grid.  Instances of wxGridCellEditor (actually, instances
    of derived classes since it is an abstract class) can be associated with
    the cell attributes for individual cells, rows, columns, or even for the
    entire grid.

    Normally wxGridCellEditor shows some UI control allowing the user to edit
    the cell, but starting with wxWidgets 3.1.4 it's also possible to define
    "activatable" cell editors, that change the value of the cell directly when
    it's activated (typically by pressing Space key or clicking on it), see
    TryActivate() method. Note that when implementing an editor which is always
    activatable, i.e. never shows any in-place editor, it is more convenient to
    derive its class from wxGridCellActivatableEditor than from wxGridCellEditor
    itself.

    @library{wxcore}
    @category{grid}

    @see wxGridCellAutoWrapStringEditor, wxGridCellBoolEditor,
         wxGridCellChoiceEditor, wxGridCellEnumEditor,
         wxGridCellFloatEditor, wxGridCellNumberEditor,
         wxGridCellTextEditor, wxGridCellDateEditor
*/
class wxGridCellEditor : public wxClientDataContainer, public wxRefCounter
{
public:
    /**
        Default constructor.
    */
    wxGridCellEditor();

    /**
        Fetch the value from the table and prepare the edit control to begin
        editing.

        This function should save the original value of the grid cell at the
        given @a row and @a col and show the control allowing the user to
        change it.

        @see EndEdit()
    */
    virtual void BeginEdit(int row, int col, wxGrid* grid) = 0;

    /**
        Create a new object which is the copy of this one.
    */
    virtual wxGridCellEditor* Clone() const = 0;

    /**
        Creates the actual edit control.
    */
    virtual void Create(wxWindow* parent, wxWindowID id,
                        wxEvtHandler* evtHandler) = 0;

    /**
        Final cleanup.
    */
    virtual void Destroy();

    /**
        End editing the cell.

        This function must check if the current value of the editing control is
        valid and different from the original value (available as @a oldval in
        its string form and possibly saved internally using its real type by
        BeginEdit()). If it isn't, it just returns @false, otherwise it must do
        the following:
            - Save the new value internally so that ApplyEdit() could apply it.
            - Fill @a newval (which is never @NULL) with the string
            representation of the new value.
            - Return @true

        Notice that it must @em not modify the grid as the change could still
        be vetoed.

        If the user-defined wxEVT_GRID_CELL_CHANGING event handler doesn't veto
        this change, ApplyEdit() will be called next.
    */
    virtual bool EndEdit(int row, int col, const wxGrid* grid,
                         const wxString& oldval, wxString* newval) = 0;

    /**
        Effectively save the changes in the grid.

        This function should save the value of the control in the grid. It is
        called only after EndEdit() returns @true.
     */
    virtual void ApplyEdit(int row, int col, wxGrid* grid) = 0;

    /**
        Some types of controls on some platforms may need some help with the
        Return key.
    */
    virtual void HandleReturn(wxKeyEvent& event);

    /**
        Returns @true if the edit control has been created.
    */
    bool IsCreated();

    /**
        Draws the part of the cell not occupied by the control: the base class
        version just fills it with background colour from the attribute.
    */
    virtual void PaintBackground(wxDC& dc, const wxRect& rectCell, const wxGridCellAttr& attr);

    /**
        Reset the value in the control back to its starting value.
    */
    virtual void Reset() = 0;

    /**
        Size and position the edit control.
    */
    virtual void SetSize(const wxRect& rect);

    /**
        Show or hide the edit control, use the specified attributes to set
        colours/fonts for it.
    */
    virtual void Show(bool show, wxGridCellAttr* attr = NULL);

    /**
        If the editor is enabled by clicking on the cell, this method will be
        called.
    */
    virtual void StartingClick();

    /**
        If the editor is enabled by pressing keys on the grid, this will be
        called to let the editor do something about that first key if desired.
    */
    virtual void StartingKey(wxKeyEvent& event);

    /**
       Return @true to allow the given key to start editing: the base class
       version only checks that the event has no modifiers. 

       If the key is F2 (special), editing will always start and this
       method will not be called at all (but StartingKey() will)
    */
    virtual bool IsAcceptedKey(wxKeyEvent& event);
    

    /**
       Returns the value currently in the editor control.
     */
    virtual wxString GetValue() const = 0;

    /**
       Get the edit window used by this editor.

       @since 3.1.3
    */
    wxWindow* GetWindow() const;

    /**
       Set the wxWindow that will be used by this cell editor for editing the
       value.

       @since 3.1.3
    */
    void SetWindow(wxWindow* window);

    /**
       Get the wxControl used by this editor.

       This function is preserved for compatibility, but GetWindow() should be
       preferred in the new code as the associated window doesn't need to be of
       a wxControl-derived class.

       Note that if SetWindow() had been called with an object not deriving
       from wxControl, this method will return @NULL.
    */
    wxControl* GetControl();

    /**
       Set the wxControl that will be used by this cell editor for editing the
       value.

       This function is preserved for compatibility, but SetWindow() should be
       preferred in the new code, see GetControl().
    */
    void SetControl(wxControl* control);


    /**
        Function allowing to create an "activatable" editor.

        As explained in this class description, activatable editors don't show
        any edit control but change the cell value directly, when it is
        activated (by any way described by wxGridActivationSource).

        To create such editor, this method must be overridden to return
        wxGridActivationResult::DoChange() passing it the new value of the
        cell. If the change is not vetoed by wxEVT_GRID_CELL_CHANGING handler,
        DoActivate() will be called to actually change the value, so it must be
        overridden as well if TryActivate() is overridden.

        By default, wxGridActivationResult::DoEdit() is returned, meaning that
        this is a normal editor, using an edit control for changing the cell
        value.

        @since 3.1.4
     */
    virtual wxGridActivationResult
    TryActivate(int row, int col, wxGrid* grid,
                const wxGridActivationSource& actSource);

    /**
        Function which must be overridden for "activatable" editors.

        If TryActivate() is overridden to return "change" action, this function
        will be called to actually apply this change. Note that it is not
        passed the value to apply, as it is assumed that the editor class
        stores this value as a member variable anyhow.

        @since 3.1.4
     */
    virtual void DoActivate(int row, int col, wxGrid* grid);

protected:

    /**
        The destructor is private because only DecRef() can delete us.
    */
    virtual ~wxGridCellEditor();
};

/**
    Base class for activatable editors.

    Inheriting from this class makes it simpler to implement editors that
    support only activation, but not in-place editing, as they only need to
    implement TryActivate(), DoActivate() and Clone() methods, but not all the
    other pure virtual methods of wxGridCellEditor.

    @since 3.1.4
 */
class wxGridCellActivatableEditor : public wxGridCellEditor
{
public:
    /**
        Same method as in wxGridCellEditor, but pure virtual.

        Note that the implementation of this method must never return
        wxGridActivationResult::DoEdit() for the editors inheriting from this
        class, as it doesn't support normal editing.
     */
    virtual wxGridActivationResult
    TryActivate(int row, int col, wxGrid* grid,
                const wxGridActivationSource& actSource) = 0;

    /// Same method as in wxGridCellEditor, but pure virtual.
    virtual void DoActivate(int row, int col, wxGrid* grid) = 0;
};

/**
    Smart pointer wrapping wxGridCellEditor.

    wxGridCellEditorPtr takes ownership of wxGridCellEditor passed to it on
    construction and calls DecRef() on it automatically when it is destroyed.
    It also provides transparent access to wxGridCellEditor methods by allowing
    to use objects of this class as if they were wxGridCellEditor pointers.

    @since 3.1.4

    @category{grid}
*/
typedef wxObjectDataPtr<wxGridCellEditor> wxGridCellEditorPtr;

/**
    @class wxGridCellAutoWrapStringEditor

    Grid cell editor for wrappable string/text data.

    @library{wxcore}
    @category{grid}

    @see wxGridCellEditor, wxGridCellBoolEditor, wxGridCellChoiceEditor,
         wxGridCellEnumEditor, wxGridCellFloatEditor,
         wxGridCellNumberEditor, wxGridCellTextEditor,
         wxGridCellDateEditor
*/
class wxGridCellAutoWrapStringEditor : public wxGridCellTextEditor
{
public:
    wxGridCellAutoWrapStringEditor();
};

/**
    @class wxGridCellBoolEditor

    Grid cell editor for boolean data.

    @library{wxcore}
    @category{grid}

    @see wxGridCellEditor, wxGridCellAutoWrapStringEditor,
         wxGridCellChoiceEditor, wxGridCellEnumEditor,
         wxGridCellFloatEditor, wxGridCellNumberEditor,
         wxGridCellTextEditor, wxGridCellDateEditor
*/
class wxGridCellBoolEditor : public wxGridCellEditor
{
public:
    /**
        Default constructor.
    */
    wxGridCellBoolEditor();

    /**
        Returns @true if the given @a value is equal to the string
        representation of the truth value we currently use (see
        UseStringValues()).
    */
    static bool IsTrueValue(const wxString& value);

    /**
        This method allows you to customize the values returned by GetValue()
        for the cell using this editor. By default, the default values of the
        arguments are used, i.e. @c "1" is returned if the cell is checked and
        an empty string otherwise.
    */
    static void UseStringValues(const wxString& valueTrue = "1",
                                const wxString& valueFalse = wxEmptyString);
};

/**
    @class wxGridCellChoiceEditor

    Grid cell editor for string data providing the user a choice from a list of
    strings.

    @library{wxcore}
    @category{grid}

    @see wxGridCellEditor, wxGridCellAutoWrapStringEditor,
         wxGridCellBoolEditor, wxGridCellEnumEditor,
         wxGridCellFloatEditor, wxGridCellNumberEditor,
         wxGridCellTextEditor, wxGridCellDateEditor
*/
class wxGridCellChoiceEditor : public wxGridCellEditor
{
public:
    /**
        Choice cell renderer ctor.

        @param count
            Number of strings from which the user can choose.
        @param choices
            An array of strings from which the user can choose.
        @param allowOthers
            If allowOthers is @true, the user can type a string not in choices
            array.
    */
    wxGridCellChoiceEditor(size_t count = 0,
                           const wxString choices[] = NULL,
                           bool allowOthers = false);

    /**
        Choice cell renderer ctor.

        @param choices
            An array of strings from which the user can choose.
        @param allowOthers
            If allowOthers is @true, the user can type a string not in choices
            array.
    */
    wxGridCellChoiceEditor(const wxArrayString& choices,
                           bool allowOthers = false);

    /**
        Parameters string format is "item1[,item2[...,itemN]]".

        This method can be called before the editor is used for the first time,
        or later, in which case it replaces the previously specified strings
        with the new ones.
    */
    virtual void SetParameters(const wxString& params);
};

/**
    @class wxGridCellEnumEditor

    Grid cell editor which displays an enum number as a textual equivalent
    (e.g. data in cell is 0,1,2 ... n the cell could be displayed as
    "John","Fred"..."Bob" in the combo choice box).

    @library{wxcore}
    @category{grid}

    @see wxGridCellEditor, wxGridCellAutoWrapStringEditor,
         wxGridCellBoolEditor, wxGridCellChoiceEditor,
         wxGridCellTextEditor, wxGridCellFloatEditor,
         wxGridCellNumberEditor, wxGridCellDateEditor
*/
class wxGridCellEnumEditor : public wxGridCellChoiceEditor
{
public:
    /**
        Enum cell editor ctor.

        @param choices
            Comma separated choice parameters "item1[,item2[...,itemN]]".
    */
    wxGridCellEnumEditor( const wxString& choices = wxEmptyString );
};

/**
    @class wxGridCellTextEditor

    Grid cell editor for string/text data.

    @library{wxcore}
    @category{grid}

    @see wxGridCellEditor, wxGridCellAutoWrapStringEditor,
         wxGridCellBoolEditor, wxGridCellChoiceEditor,
         wxGridCellEnumEditor, wxGridCellFloatEditor,
         wxGridCellNumberEditor, wxGridCellDateEditor
*/
class wxGridCellTextEditor : public wxGridCellEditor
{
public:
    /**
        Text cell editor constructor.

        @param maxChars
            Maximum width of text (this parameter is supported starting since
            wxWidgets 2.9.5).
    */
    explicit wxGridCellTextEditor(size_t maxChars = 0);

    /**
        The parameters string format is "n" where n is a number representing
        the maximum width.
    */
    virtual void SetParameters(const wxString& params);

    /**
        Set validator to validate user input.

        @since 2.9.5
    */
    virtual void SetValidator(const wxValidator& validator);
};

/**
    @class wxGridCellFloatEditor

    The editor for floating point numbers data.

    @library{wxcore}
    @category{grid}

    @see wxGridCellEditor, wxGridCellAutoWrapStringEditor,
         wxGridCellBoolEditor, wxGridCellChoiceEditor,
         wxGridCellEnumEditor, wxGridCellNumberEditor,
         wxGridCellTextEditor, wxGridCellDateEditor
*/
class wxGridCellFloatEditor : public wxGridCellTextEditor
{
public:
    /**
        Float cell editor ctor.

        @param width
            Minimum number of characters to be shown.
        @param precision
            Number of digits after the decimal dot.
        @param format
            The format to use for displaying the number, a combination of
            ::wxGridCellFloatFormat enum elements. This parameter is only
            available since wxWidgets 2.9.3.
    */
    wxGridCellFloatEditor(int width = -1, int precision = -1,
                          int format = wxGRID_FLOAT_FORMAT_DEFAULT);

    /**
        The parameters string format is "width[,precision[,format]]" where
        @c format should be chosen between f|e|g|E|G (f is used by default)
    */
    virtual void SetParameters(const wxString& params);
};

/**
    @class wxGridCellNumberEditor

    Grid cell editor for numeric integer data.

    @library{wxcore}
    @category{grid}

    @see wxGridCellEditor, wxGridCellAutoWrapStringEditor,
         wxGridCellBoolEditor, wxGridCellChoiceEditor,
         wxGridCellEnumEditor, wxGridCellFloatEditor,
         wxGridCellTextEditor, wxGridCellDateEditor
*/
class wxGridCellNumberEditor : public wxGridCellTextEditor
{
public:
    /**
        Allows you to specify the range for acceptable data. Values equal to
        -1 for both @a min and @a max indicate that no range checking should be
        done.
    */
    wxGridCellNumberEditor(int min = -1, int max = -1);


    /**
        Parameters string format is "min,max".
    */
    virtual void SetParameters(const wxString& params);

protected:

    /**
        If the return value is @true, the editor uses a wxSpinCtrl to get user
        input, otherwise it uses a wxTextCtrl.
    */
    bool HasRange() const;

    /**
        String representation of the value.
    */
    wxString GetString() const;
};

/**
    @class wxGridCellDateEditor

    Grid cell editor for dates.

    Uses @ref wxDatePickerCtrl as actual edit control.

    @library{wxcore}
    @category{grid}

    @see wxGridCellEditor, wxGridCellAutoWrapStringEditor,
         wxGridCellBoolEditor, wxGridCellChoiceEditor,
         wxGridCellEnumEditor, wxGridCellFloatEditor,
         wxGridCellTextEditor

    @since 3.1.3
*/
class wxGridCellDateEditor : public wxGridCellEditor
{
public:
    /**
        Date editor constructor.
    */
    wxGridCellDateEditor();
};



/**
    @class wxGridFitMode

    Allows to specify the behaviour when the cell contents doesn't fit into its
    allotted space.

    Objects of this class are used with wxGridCellAttr::SetFitMode() and
    wxGrid::SetDefaultCellFitMode() and wxGrid::SetCellFitMode() functions and
    allow to specify what should happen if the cell contents doesn't fit into
    the available space. The possibilities are:

    - Overflow into the cell to the right if it is empty, or possibly several
    cells, if the cell contents still doesn't fit after overflowing into the
    immediately neighbouring cell.
    - Clip the cell contents, discarding the part which doesn't fit.
    - Ellipsize the cell contents, i.e. replace the non-fitting part with
    ellipsis (@c ...), putting the ellipsis at the end by default, but possibly
    at the beginning or in the middle.

    The default behaviour is to overflow, use wxGrid::SetDefaultCellFitMode()
    to change this, for example:
    @code
        grid->SetDefaultCellFitMode(wxGridFitMode::Clip());
    @endcode

    Objects of this class are created using static functions instead of
    constructors for better readability and can't be changed after creating
    them except by using the assignment operator.

    @library{wxcore}
    @category{grid}

    @since 3.1.4
 */
class wxGridFitMode
{
public:
    /**
        Default constructor creates an object not specifying any behaviour.

        This constructor is not very useful, use static methods Clip() and
        Overflow() below to create objects of this class instead.
     */
    wxGridFitMode();

    /**
        Pseudo-constructor for object specifying clipping behaviour.
     */
    static wxGridFitMode Clip();

    /**
        Pseudo-constructor for object specifying overflow behaviour.
     */
    static wxGridFitMode Overflow();

    /**
        Pseudo-constructor for object specifying ellipsize behaviour.
     */
    static wxGridFitMode Ellipsize(wxEllipsizeMode ellipsize = wxELLIPSIZE_END);

    /**
        Return true if the object specifies some particular behaviour.

        This method returns @false for default-constructed objects of this
        type only.
     */
    bool IsSpecified() const;

    /**
        Return true if the object specifies clipping behaviour.

        This method returns @true only for the objects returned by Clip().
     */
    bool IsClip() const;

    /**
        Return true if the object specifies overflow behaviour.

        This method returns @true only for the objects returned by Overflow().
     */
    bool IsOverflow() const;

    /**
        Return ellipsize mode, possibly @c wxELLIPSIZE_NONE.

        For the objects constructed using Ellipsize(), the same ellipsization
        mode as was passed to it is returned. For all the other objects,
        ::wxELLIPSIZE_NONE is.
     */
    wxEllipsizeMode GetEllipsizeMode() const;
};

/**
    @class wxGridCellAttr

    This class can be used to alter the cells' appearance in the grid by
    changing their attributes from the defaults. An object of this class may be
    returned by wxGridTableBase::GetAttr().

    Note that objects of this class are reference-counted and it's recommended
    to use wxGridCellAttrPtr smart pointer class when working with them to
    avoid memory leaks.

    @library{wxcore}
    @category{grid}
*/
class wxGridCellAttr : public wxClientDataContainer, public wxRefCounter
{
public:
    /**
        Kind of the attribute to retrieve.

        @see wxGridCellAttrProvider::GetAttr(), wxGridTableBase::GetAttr()
     */
    enum wxAttrKind
    {
        /// Return the combined effective attribute for the cell.
        Any,

        /// Return the attribute explicitly set for this cell.
        Cell,

        /// Return the attribute set for this cells row.
        Row,

        /// Return the attribute set for this cells column.
        Col,

        Default,
        Merged
    };

    /**
        Default constructor.
    */
    explicit wxGridCellAttr(wxGridCellAttr* attrDefault = NULL);

    /**
        Constructor specifying some of the often used attributes.
    */
    wxGridCellAttr(const wxColour& colText, const wxColour& colBack,
                   const wxFont& font, int hAlign, int vAlign);

    /**
        Creates a new copy of this object.
    */
    wxGridCellAttr* Clone() const;

    /**
        This class is reference counted: it is created with ref count of 1, so
        calling DecRef() once will delete it. Calling IncRef() allows locking
        it until the matching DecRef() is called.
    */
    void DecRef();

    /**
        Get the alignment to use for the cell with the given attribute.

        If this attribute doesn't specify any alignment, the default attribute
        alignment is used (which can be changed using
        wxGrid::SetDefaultCellAlignment() but is left and top by default).

        Notice that @a hAlign and @a vAlign values are always overwritten by
        this function, use GetNonDefaultAlignment() if this is not desirable.

        @param hAlign
            Horizontal alignment is returned here if this argument is non-@NULL.
            It is one of wxALIGN_LEFT, wxALIGN_CENTRE or wxALIGN_RIGHT.
        @param vAlign
            Vertical alignment is returned here if this argument is non-@NULL.
            It is one of wxALIGN_TOP, wxALIGN_CENTRE or wxALIGN_BOTTOM.
    */
    void GetAlignment(int* hAlign, int* vAlign) const;

    /**
        Returns the background colour.
    */
    const wxColour& GetBackgroundColour() const;

    /**
        Returns the cell editor.

        The caller is responsible for calling DecRef() on the returned pointer,
        use GetEditorPtr() to do it automatically.
    */
    wxGridCellEditor* GetEditor(const wxGrid* grid, int row, int col) const;

    /**
        Returns the cell editor.

        This method is identical to GetEditor(), but returns a smart pointer,
        which frees the caller from the need to call DecRef() manually.

        @since 3.1.4
     */
    wxGridCellEditorPtr GetEditorPtr(const wxGrid* grid, int row, int col) const;

    /**
        Returns the font.
    */
    const wxFont& GetFont() const;

    /**
        Get the alignment defined by this attribute.

        Unlike GetAlignment() this function only modifies @a hAlign and @a
        vAlign if this attribute does define a non-default alignment. This
        means that they must be initialized before calling this function and
        that their values will be preserved unchanged if they are different
        from wxALIGN_INVALID.

        For example, the following fragment can be used to use the cell
        alignment if one is defined but right-align its contents by default
        (instead of left-aligning it by default) while still using the default
        vertical alignment:
        @code
            int hAlign = wxALIGN_RIGHT,
                vAlign = wxALIGN_INVALID;
            attr.GetNonDefaultAlignment(&hAlign, &vAlign);
        @endcode

        @since 2.9.1
     */
    void GetNonDefaultAlignment(int *hAlign, int *vAlign) const;

    /**
        Returns the cell renderer.

        The caller is responsible for calling DecRef() on the returned pointer,
        use GetRendererPtr() to do it automatically.
    */
    wxGridCellRenderer* GetRenderer(const wxGrid* grid, int row, int col) const;

    /**
        Returns the cell editor.

        This method is identical to GetRenderer(), but returns a smart pointer,
        which frees the caller from the need to call DecRef() manually.

        @since 3.1.4
     */
    wxGridCellRendererPtr GetRendererPtr(const wxGrid* grid, int row, int col) const;

    /**
        Returns the text colour.
    */
    const wxColour& GetTextColour() const;

    /**
        Returns @true if this attribute has a valid alignment set.
    */
    bool HasAlignment() const;

    /**
        Returns @true if this attribute has a valid background colour set.
    */
    bool HasBackgroundColour() const;

    /**
        Returns @true if this attribute has a valid cell editor set.
    */
    bool HasEditor() const;

    /**
        Returns @true if this attribute has a valid font set.
    */
    bool HasFont() const;

    /**
        Returns @true if this attribute has a valid cell renderer set.
    */
    bool HasRenderer() const;

    /**
        Returns @true if this attribute has a valid text colour set.
    */
    bool HasTextColour() const;

    /**
        This class is reference counted: it is created with ref count of 1, so
        calling DecRef() once will delete it. Calling IncRef() allows locking
        it until the matching DecRef() is called.
    */
    void IncRef();

    /**
        Returns @true if this cell is set as read-only.
    */
    bool IsReadOnly() const;

    /**
        Sets the alignment. @a hAlign can be one of @c wxALIGN_LEFT,
        @c wxALIGN_CENTRE or @c wxALIGN_RIGHT and @a vAlign can be one of
        @c wxALIGN_TOP, @c wxALIGN_CENTRE or @c wxALIGN_BOTTOM.
    */
    void SetAlignment(int hAlign, int vAlign);

    /**
        Sets the background colour.
    */
    void SetBackgroundColour(const wxColour& colBack);

    /**
        @todo Needs documentation.
    */
    void SetDefAttr(wxGridCellAttr* defAttr);

    /**
        Sets the editor to be used with the cells with this attribute.
    */
    void SetEditor(wxGridCellEditor* editor);

    /**
        Sets the font.
    */
    void SetFont(const wxFont& font);

    /**
        Sets the cell as read-only.
    */
    void SetReadOnly(bool isReadOnly = true);

    /**
        Sets the renderer to be used for cells with this attribute. Takes
        ownership of the pointer.
    */
    void SetRenderer(wxGridCellRenderer* renderer);

    /**
        Sets the text colour.
    */
    void SetTextColour(const wxColour& colText);


    void MergeWith(wxGridCellAttr *mergefrom);

    void SetSize(int num_rows, int num_cols);

    /**
        Specifies the behaviour of the cell contents if it doesn't fit into the
        available space.

        @see wxGridFitMode

        @since 3.1.4
     */
    void SetFitMode(wxGridFitMode fitMode);

    /**
        Specifies if cells using this attribute should overflow or clip their
        contents.

        This is the same as calling SetFitMode() with either
        wxGridFitMode::Overflow() or wxGridFitMode::Clip() argument depending
        on whether @a allow is @true or @false.

        Prefer using SetFitMode() directly instead in the new code.
     */
    void SetOverflow(bool allow = true);

    void SetKind(wxAttrKind kind);

    bool HasReadWriteMode() const;
    bool HasOverflowMode() const;
    bool HasSize() const;

    void GetSize(int *num_rows, int *num_cols) const;

    /**
        Returns the fitting mode for the cells using this attribute.

        The returned wxGridFitMode is always specified, i.e.
        wxGridFitMode::IsSpecified() always returns @true. The default value,
        if SetFitMode() hadn't been called before, is "overflow".

        @since 3.1.4
     */
    wxGridFitMode GetFitMode() const;

    /**
        Returns true if the cells using this attribute overflow into the
        neighbouring cells.

        Prefer using GetFitMode() in the new code.
     */
    bool GetOverflow() const;

    /**
        Returns @true if the cell will draw an overflowed text into the
        neighbouring cells.

        Note that only left aligned cells currently can overflow. It means that
        GetFitMode().IsOverflow() should returns true and GetAlignment should
        returns wxALIGN_LEFT for hAlign parameter.

        @since 3.1.4
     */
    bool CanOverflow() const;

    wxAttrKind GetKind();


protected:

    /**
        The destructor is private because only DecRef() can delete us.
    */
    virtual ~wxGridCellAttr();
};

/**
    Smart pointer wrapping wxGridCellAttr.

    wxGridCellAttrPtr takes ownership of wxGridCellAttr passed to it on
    construction and calls DecRef() on it automatically when it is destroyed.
    It also provides transparent access to wxGridCellAttr methods by allowing
    to use objects of this class as if they were wxGridCellAttr pointers.

    @since 3.1.4

    @category{grid}
*/
typedef wxObjectDataPtr<wxGridCellAttr> wxGridCellAttrPtr;

/**
    Base class for header cells renderers.

    A cell renderer can be used to draw the text of a cell's label, and/or
    the border around it.

    @since 2.9.1
 */
class wxGridHeaderLabelsRenderer
{
public:
    /**
        Called by the grid to draw the border around the cell header.

        This method is responsible for drawing the border inside the given @a
        rect and adjusting the rectangle size to correspond to the area inside
        the border, i.e. usually call wxRect::Deflate() to account for the
        border width.

        @param grid
            The grid whose header cell window is being drawn.
        @param dc
            The device context to use for drawing.
        @param rect
            Input/output parameter which contains the border rectangle on input
            and should be updated to contain the area inside the border on
            function return.
     */
    virtual void DrawBorder(const wxGrid& grid,
                            wxDC& dc,
                            wxRect& rect) const = 0;

    /**
        Called by the grid to draw the specified label.

        Notice that the DrawBorder() method is called before this one.

        The default implementation uses wxGrid::GetLabelTextColour() and
        wxGrid::GetLabelFont() to draw the label.
     */
    virtual void DrawLabel(const wxGrid& grid,
                           wxDC& dc,
                           const wxString& value,
                           const wxRect& rect,
                           int horizAlign,
                           int vertAlign,
                           int textOrientation) const;
};

/**
    Base class for row headers renderer.

    This is the same as wxGridHeaderLabelsRenderer currently but we still use a
    separate class for it to distinguish it from wxGridColumnHeaderRenderer
    and wxGridCornerHeaderRenderer.

    @see wxGridRowHeaderRendererDefault

    @see wxGridCellAttrProvider::GetRowHeaderRenderer()

    @since 2.9.1
 */
class wxGridRowHeaderRenderer : public wxGridHeaderLabelsRenderer
{
};

/**
    Base class for column headers renderer.

    This is the same as wxGridHeaderLabelsRenderer currently but we still use a
    separate class for it to distinguish it from wxGridRowHeaderRenderer and
    wxGridCornerHeaderRenderer.

    @see wxGridColumnHeaderRendererDefault

    @see wxGridCellAttrProvider::GetColumnHeaderRenderer()

    @since 2.9.1
 */
class wxGridColumnHeaderRenderer : public wxGridHeaderLabelsRenderer
{
};

/**
    Base class for corner header renderer.

    This is the same as wxGridHeaderLabelsRenderer currently but we still use a
    separate class for it to distinguish it from wxGridRowHeaderRenderer and
    wxGridColumnHeaderRenderer.

    @see wxGridCornerHeaderRendererDefault

    @see wxGridCellAttrProvider::GetCornerRenderer()

    @since 2.9.1
 */
class wxGridCornerHeaderRenderer : public wxGridHeaderLabelsRenderer
{
};

/**
    Default row header renderer.

    You may derive from this class if you need to only override one of its
    methods (i.e. either DrawLabel() or DrawBorder()) but continue to use the
    default implementation for the other one.

    @see wxGridColumnHeaderRendererDefault, wxGridCornerHeaderRendererDefault

    @since 2.9.1
 */
class wxGridRowHeaderRendererDefault : public wxGridRowHeaderRenderer
{
public:
    /// Implement border drawing for the row labels.
    virtual void DrawBorder(const wxGrid& grid,
                            wxDC& dc,
                            wxRect& rect) const;
};

/**
    Default column header renderer.

    @see wxGridRowHeaderRendererDefault, wxGridCornerHeaderRendererDefault

    @since 2.9.1
 */
class wxGridColumnHeaderRendererDefault : public wxGridColumnHeaderRenderer
{
public:
    /// Implement border drawing for the column labels.
    virtual void DrawBorder(const wxGrid& grid,
                            wxDC& dc,
                            wxRect& rect) const;
};

/**
    Default corner window renderer.

    @see wxGridColumnHeaderRendererDefault, wxGridRowHeaderRendererDefault

    @since 2.9.1
 */
class wxGridCornerHeaderRendererDefault : public wxGridCornerHeaderRenderer
{
public:
    /// Implement border drawing for the corner window.
    virtual void DrawBorder(const wxGrid& grid,
                            wxDC& dc,
                            wxRect& rect) const;
};

/**
    Class providing attributes to be used for the grid cells.

    This class both defines an interface which grid cell attributes providers
    should implement -- and which can be implemented differently in derived
    classes -- and a default implementation of this interface which is often
    good enough to be used without modification, especially with not very large
    grids for which the efficiency of attributes storage hardly matters (see
    the discussion below).

    An object of this class can be associated with a wxGrid using
    wxGridTableBase::SetAttrProvider() but it's not necessary to call it if you
    intend to use the default provider as it is used by wxGridTableBase by
    default anyhow.

    Notice that while attributes provided by this class can be set for
    individual cells using SetAttr() or the entire rows or columns using
    SetRowAttr() and SetColAttr() they are always retrieved using GetAttr()
    function.


    The default implementation of this class stores the attributes passed to
    its SetAttr(), SetRowAttr() and SetColAttr() in a straightforward way. A
    derived class may use its knowledge about how the attributes are used in
    your program to implement it much more efficiently: for example, using a
    special background colour for all even-numbered rows can be implemented by
    simply returning the same attribute from GetAttr() if the row number is
    even instead of having to store N/2 row attributes where N is the total
    number of rows in the grid.

    Notice that objects of this class can't be copied.
 */
class wxGridCellAttrProvider : public wxClientDataContainer
{
public:
    /// Trivial default constructor.
    wxGridCellAttrProvider();

    /// Destructor releases any attributes held by this class.
    virtual ~wxGridCellAttrProvider();

    /**
        Get the attribute to use for the specified cell.

        If wxGridCellAttr::Any is used as @a kind value, this function combines
        the attributes set for this cell using SetAttr() and those for its row
        or column (set with SetRowAttr() or SetColAttr() respectively), with
        the cell attribute having the highest precedence.

        Notice that the caller must call DecRef() on the returned pointer if it
        is non-@NULL. GetAttrPtr() method can be used to do this automatically.

        @param row
            The row of the cell.
        @param col
            The column of the cell.
        @param kind
            The kind of the attribute to return.
        @return
            The attribute to use which should be DecRef()'d by caller or @NULL
            if no attributes are defined for this cell.
     */
    virtual wxGridCellAttr *GetAttr(int row, int col,
                                    wxGridCellAttr::wxAttrKind kind) const;

    /**
        Get the attribute to use for the specified cell.

        This method is identical to GetAttr(), but returns a smart pointer,
        which frees the caller from the need to call DecRef() manually.

        @since 3.1.4
     */
    wxGridCellAttrPtr GetAttrPtr(int row, int col,
                                 wxGridCellAttr::wxAttrKind kind) const;

    /*!
        @name Setting attributes.

        All these functions take ownership of the attribute passed to them,
        i.e. will call DecRef() on it themselves later and so it should not be
        destroyed by the caller. The attribute can be @NULL to reset a
        previously set value.
     */
    //@{

    /// Set attribute for the specified cell.
    virtual void SetAttr(wxGridCellAttr *attr, int row, int col);

    /// Set attribute for the specified row.
    virtual void SetRowAttr(wxGridCellAttr *attr, int row);

    /// Set attribute for the specified column.
    virtual void SetColAttr(wxGridCellAttr *attr, int col);

    //@}

    /**
        @name Getting header renderers.

        These functions return the renderers for the given row or column header
        label and the corner window. Unlike cell attributes, these objects are
        not reference counted and are never @NULL so they are returned by
        reference and not pointer and DecRef() shouldn't (and can't) be called
        for them.

        All these functions were added in wxWidgets 2.9.1.
     */
    //@{

    /**
        Return the renderer used for drawing column headers.

        By default wxGridColumnHeaderRendererDefault is returned.

        @see wxGrid::SetUseNativeColLabels(), wxGrid::UseNativeColHeader()

        @since 2.9.1
     */
    virtual const wxGridColumnHeaderRenderer& GetColumnHeaderRenderer(int col);

    /**
        Return the renderer used for drawing row headers.

        By default wxGridRowHeaderRendererDefault is returned.

        @since 2.9.1
     */
    virtual const wxGridRowHeaderRenderer& GetRowHeaderRenderer(int row);

    /**
        Return the renderer used for drawing the corner window.

        By default wxGridCornerHeaderRendererDefault is returned.

        @since 2.9.1
     */
    virtual const wxGridCornerHeaderRenderer& GetCornerRenderer();

    //@}
};

/**
    Message class used by the grid table to send requests and notifications to
    the grid view.

    A message object of this class must be sent to the grid using
    wxGrid::ProcessTableMessage() every time the table changes, e.g. rows are
    added/deleted. The messages are just notifications and don't result in any
    actual changes but just allow the view to react to changes to the model.
*/
class wxGridTableMessage
{
public:
    /**
        Default constructor initializes the object to invalid state.
     */
    wxGridTableMessage();

    /**
        Constructor really initialize the message.

        @param table Pointer to the grid table
        @param id One of wxGridTableRequest enum elements.
        @param comInt1 Position after which the rows are inserted/deleted
        @param comInt2 Number of rows to be inserted/deleted
    */
    wxGridTableMessage(wxGridTableBase *table, int id, int comInt1 = -1, int comInt2 = -1);

    /**
        Sets the table object
    */
    void SetTableObject( wxGridTableBase *table );

    /**
        Gets the table object
    */
    wxGridTableBase * GetTableObject() const;

    /**
        Sets an id
    */
    void SetId( int id );

    /**
        Gets an id
    */
    int  GetId();

    /**
        Set the position after which the insertion/deletion occur
    */
    void SetCommandInt( int comInt1 );

    /**
        Get the position after which the insertion/deletion occur
    */
    int  GetCommandInt();

    /**
        Set the number of rows to be inserted/deleted
    */
    void SetCommandInt2( int comInt2 );

    /**
        Get the number of rows to be inserted/deleted
    */
    int  GetCommandInt2();
};


/**
    Simplest type of data table for a grid for small tables of strings that are
    stored in memory.

    The number of rows and columns in the table can be specified initially but
    may also be changed later dynamically.
 */
class wxGridStringTable
{
public:
    /**
        Default constructor creates an empty table.
     */
    wxGridStringTable();

    /**
        Constructor taking number of rows and columns.
     */
    wxGridStringTable( int numRows, int numCols );

    virtual int GetNumberRows();
    virtual int GetNumberCols();
    virtual wxString GetValue( int row, int col );
    virtual void SetValue( int row, int col, const wxString& s );

    void Clear();
    bool InsertRows( size_t pos = 0, size_t numRows = 1 );
    bool AppendRows( size_t numRows = 1 );
    bool DeleteRows( size_t pos = 0, size_t numRows = 1 );
    bool InsertCols( size_t pos = 0, size_t numCols = 1 );
    bool AppendCols( size_t numCols = 1 );
    bool DeleteCols( size_t pos = 0, size_t numCols = 1 );

    void SetRowLabelValue( int row, const wxString& );
    void SetColLabelValue( int col, const wxString& );
    void SetCornerLabelValue( const wxString& );
    wxString GetRowLabelValue( int row );
    wxString GetColLabelValue( int col );
    wxString GetCornerLabelValue() const;
};

/**
    Represents coordinates of a grid cell.

    An object of this class is simply a (row, column) pair.
 */
class wxGridCellCoords
{
public:
    /**
        Default constructor initializes the object to invalid state.

        Initially the row and column are both invalid (-1) and so operator!()
        for an uninitialized wxGridCellCoords returns false.
     */
    wxGridCellCoords();

    /**
        Constructor taking a row and a column.
     */
    wxGridCellCoords(int row, int col);

    /**
        Return the row of the coordinate.
     */
    int GetRow() const;

    /**
        Set the row of the coordinate.
     */
    void SetRow(int n);

    /**
        Return the column of the coordinate.
     */
    int GetCol() const;

    /**
        Set the column of the coordinate.
     */
    void SetCol(int n);

    /**
        Set the row and column of the coordinate.
     */
    void Set(int row, int col);

    /**
        Assignment operator for coordinate types.
     */
    wxGridCellCoords& operator=(const wxGridCellCoords& other);

    /**
        Equality operator.
     */
    bool operator==(const wxGridCellCoords& other) const;

    /**
        Inequality operator.
     */
     bool operator!=(const wxGridCellCoords& other) const;

    /**
        Checks whether the coordinates are invalid.

        Returns false only if both row and column are -1. Notice that if either
        row or column (but not both) are -1, this method returns true even if
        the object is invalid. This is done because objects in such state
        should actually never exist, i.e. either both coordinates should be -1
        or none of them should be -1.
     */
    bool operator!() const;
};

/**
    Represents coordinates of a block of cells in the grid.

    An object of this class contains coordinates of the left top and the bottom right
    corners of a block.

    @since 3.1.4
 */
class wxGridBlockCoords
{
public:
    /**
        Default constructor initializes the object to invalid state.

        Initially the coordinates are invalid (-1) and so operator!() for an
        uninitialized wxGridBlockCoords returns @true.
     */
    wxGridBlockCoords();

    /**
        Constructor taking a coordinates of the left top and the bottom right
        corners.
     */
    wxGridBlockCoords(int topRow, int leftCol, int bottomRow, int rightCol);

    /**
        Return the row of the left top corner.
     */
    int GetTopRow() const;

    /**
        Set the row of the left top corner.
     */
    void SetTopRow(int row);

    /**
        Return the column of the left top corner.
     */
    int GetLeftCol() const;

    /**
        Set the column of the left top corner.
     */
    void SetLeftCol(int col);

    /**
        Return the row of the bottom right corner.
     */
    int GetBottomRow() const;

    /**
        Set the row of the bottom right corner.
     */
    void SetBottomRow(int row);

    /**
        Return the column of the bottom right corner.
     */
    int GetRightCol() const;

    /**
        Set the column of the bottom right corner.
     */
    void SetRightCol(int col);

    /**
        Return the coordinates of the top left corner.
     */
    wxGridCellCoords GetTopLeft() const
    {
        return wxGridCellCoords(m_topRow, m_leftCol);
    }

    /**
        Return the coordinates of the bottom right corner.
     */
    wxGridCellCoords GetBottomRight() const
    {
        return wxGridCellCoords(m_bottomRow, m_rightCol);
    }

    /**
        Return the canonicalized block where top left coordinates is less
        then bottom right coordinates.
     */
    wxGridBlockCoords Canonicalize() const;

    /**
        Whether the blocks intersect.

        @return
            @true, if the block intersects with the other, @false, otherwise.
     */
    bool Intersects(const wxGridBlockCoords& other) const
    {
        return m_topRow <= other.m_bottomRow && m_bottomRow >= other.m_topRow &&
               m_leftCol <= other.m_rightCol && m_rightCol >= other.m_leftCol;
    }

    /**
        Check whether this block contains the given cell.

        @return
            @true, if the block contains the cell, @false otherwise.
     */
    bool Contains(const wxGridCellCoords& cell) const;

    /**
        Check whether this block contains another one.

        @return
            @true if @a other is entirely contained within this block.
     */
    bool Contains(const wxGridBlockCoords& other) const;

    /**
        Calculates the result blocks by subtracting the other block from this
        block.

        @param other
            The block to subtract from this block.
        @param splitOrientation
            The block splitting orientation (either @c wxHORIZONTAL or
            @c wxVERTICAL).
        @return
            Up to 4 blocks. If block doesn't exist in the result, it has value
            of @c wxGridNoBlockCoords.
     */
    wxGridBlockDiffResult
    Difference(const wxGridBlockCoords& other, int splitOrientation) const;

    /**
        Calculates the symmetric difference of the blocks.

        @param other
            The block to subtract from this block.
        @return
            Up to 4 blocks. If block doesn't exist in the result, it has value
            of @c wxGridNoBlockCoords.
     */
    wxGridBlockDiffResult
    SymDifference(const wxGridBlockCoords& other) const;

    /**
        Equality operator.
     */
    bool operator==(const wxGridBlockCoords& other) const;

    /**
        Inequality operator.
     */
    bool operator!=(const wxGridBlockCoords& other) const;

    /**
        Checks whether the cells block is invalid.

        Returns @true only if all components are -1. Notice that if one
        of the components (but not all) is -1, this method returns @false even
        if the object is invalid. This is done because objects in such state
        should actually never exist, i.e. either all components should be -1
        or none of them should be -1.
     */
    bool operator!() const;

private:
    int m_topRow;
    int m_leftCol;
    int m_bottomRow;
    int m_rightCol;
};

/**
    @class wxGridBlockDiffResult

    The helper struct uses as a result type for difference functions of
    @c wxGridBlockCoords class.

    Parts can be uninitialized (equals to @c wxGridNoBlockCoords), that means
    that the corresponding part doesn't exists in the result.

    @since 3.1.4
 */
struct wxGridBlockDiffResult
{
    wxGridBlockCoords m_parts[4];
};

/**
    Represents a collection of grid blocks that can be iterated over.

    This class provides read-only access to the blocks making up the grid
    selection in the most general case.

    Note that objects of this class can only be returned by wxGrid, but not
    constructed in the application code.

    The preferable way to iterate over it is using C++11 range-for loop:
    @code
        for ( const auto& block: grid->GetSelectedBlocks() ) {
            ... do something with block ...
        }
    @endcode
    When not using C++11, iteration has to be done manually:
    @code
        wxGridBlocks range = grid->GetSelectedBlocks();
        for ( wxGridBlocks::iterator it = range.begin();
              it != range.end();
              ++it ) {
            ... do something with *it ...
        }
    @endcode

    @since 3.1.4
 */
class wxGridBlocks
{
public:
    /**
        Read-only forward iterator type.

        This is an opaque type, which satisfies the forward iterator
        requirements, i.e. provides all the expected operations, such as
        comparison, dereference and pre- and post-increment.
     */
    class iterator
    {
        iterator();

        const wxGridBlockCoords& operator*() const;
        const wxGridBlockCoords* operator->() const;

        iterator& operator++();
        iterator operator++(int);

        bool operator==(const iterator& it) const;
        bool operator!=(const iterator& it) const;
    };

    /// Return iterator corresponding to the beginning of the range.
    iterator begin() const;

    /// Return iterator corresponding to the end of the range.
    iterator end() const;
};

/**
    @class wxGridTableBase

    The almost abstract base class for grid tables.

    A grid table is responsible for storing the grid data and, indirectly, grid
    cell attributes. The data can be stored in the way most convenient for the
    application but has to be provided in string form to wxGrid. It is also
    possible to provide cells values in other formats if appropriate, e.g. as
    numbers.

    This base class is not quite abstract as it implements a trivial strategy
    for storing the attributes by forwarding it to wxGridCellAttrProvider and
    also provides stubs for some other functions. However it does have a number
    of pure virtual methods which must be implemented in the derived classes.

    @see wxGridStringTable

    @library{wxcore}
    @category{grid}
*/
class wxGridTableBase : public wxObject
{
public:
    /**
        Default constructor.
     */
    wxGridTableBase();

    /**
        Destructor frees the attribute provider if it was created.
     */
    virtual ~wxGridTableBase();

    /**
        Must be overridden to return the number of rows in the table.

        For backwards compatibility reasons, this method is not const.
        Use GetRowsCount() instead of it in const methods of derived table
        classes.
     */
    virtual int GetNumberRows() = 0;

    /**
        Must be overridden to return the number of columns in the table.

        For backwards compatibility reasons, this method is not const.
        Use GetColsCount() instead of it in const methods of derived table
        classes,
     */
    virtual int GetNumberCols() = 0;

    /**
        Return the number of rows in the table.

        This method is not virtual and is only provided as a convenience for
        the derived classes which can't call GetNumberRows() without a
        @c const_cast from their const methods.
     */
    int GetRowsCount() const;

    /**
        Return the number of columns in the table.

        This method is not virtual and is only provided as a convenience for
        the derived classes which can't call GetNumberCols() without a
        @c const_cast from their const methods.
     */
    int GetColsCount() const;


    /**
        @name Table Cell Accessors
     */
    //@{

    /**
        May be overridden to implement testing for empty cells.

        This method is used by the grid to test if the given cell is not used
        and so whether a neighbouring cell may overflow into it. By default it
        only returns true if the value of the given cell, as returned by
        GetValue(), is empty.
     */
    virtual bool IsEmptyCell(int row, int col);

    /**
        Same as IsEmptyCell() but taking wxGridCellCoords.

        Notice that this method is not virtual, only IsEmptyCell() should be
        overridden.
     */
    bool IsEmpty(const wxGridCellCoords& coords);

    /**
        Must be overridden to implement accessing the table values as text.
     */
    virtual wxString GetValue(int row, int col) = 0;

    /**
        Must be overridden to implement setting the table values as text.
     */
    virtual void SetValue(int row, int col, const wxString& value) = 0;

    /**
        Returns the type of the value in the given cell.

        By default all cells are strings and this method returns
        @c wxGRID_VALUE_STRING.
     */
    virtual wxString GetTypeName(int row, int col);

    /**
        Returns true if the value of the given cell can be accessed as if it
        were of the specified type.

        By default the cells can only be accessed as strings. Note that a cell
        could be accessible in different ways, e.g. a numeric cell may return
        @true for @c wxGRID_VALUE_NUMBER but also for @c wxGRID_VALUE_STRING
        indicating that the value can be coerced to a string form.
     */
    virtual bool CanGetValueAs(int row, int col, const wxString& typeName);

    /**
        Returns true if the value of the given cell can be set as if it were of
        the specified type.

        @see CanGetValueAs()
     */
    virtual bool CanSetValueAs(int row, int col, const wxString& typeName);

    /**
        Returns the value of the given cell as a long.

        This should only be called if CanGetValueAs() returns @true when called
        with @c wxGRID_VALUE_NUMBER argument. Default implementation always
        return 0.
     */
    virtual long GetValueAsLong(int row, int col);

    /**
        Returns the value of the given cell as a double.

        This should only be called if CanGetValueAs() returns @true when called
        with @c wxGRID_VALUE_FLOAT argument. Default implementation always
        return 0.0.
     */
    virtual double GetValueAsDouble(int row, int col);

    /**
        Returns the value of the given cell as a boolean.

        This should only be called if CanGetValueAs() returns @true when called
        with @c wxGRID_VALUE_BOOL argument. Default implementation always
        return false.
     */
    virtual bool GetValueAsBool(int row, int col);

    /**
        Returns the value of the given cell as a user-defined type.

        This should only be called if CanGetValueAs() returns @true when called
        with @a typeName. Default implementation always return @NULL.

        Note that if the pointer is not null, it will be deleted by the caller,
        so it must be allocated on the heap by any class overriding this
        method. In practice, it means that the value stored internally must be
        cloned on every call.
     */
    virtual void *GetValueAsCustom(int row, int col, const wxString& typeName);

    /**
        Sets the value of the given cell as a long.

        This should only be called if CanSetValueAs() returns @true when called
        with @c wxGRID_VALUE_NUMBER argument. Default implementation doesn't do
        anything.
     */
    virtual void SetValueAsLong(int row, int col, long value);

    /**
        Sets the value of the given cell as a double.

        This should only be called if CanSetValueAs() returns @true when called
        with @c wxGRID_VALUE_FLOAT argument. Default implementation doesn't do
        anything.
     */
    virtual void SetValueAsDouble(int row, int col, double value);

    /**
        Sets the value of the given cell as a boolean.

        This should only be called if CanSetValueAs() returns @true when called
        with @c wxGRID_VALUE_BOOL argument. Default implementation doesn't do
        anything.
     */
    virtual void SetValueAsBool( int row, int col, bool value );

    /**
        Sets the value of the given cell as a user-defined type.

        This should only be called if CanSetValueAs() returns @true when called
        with @a typeName. Default implementation doesn't do anything.
     */
    virtual void SetValueAsCustom(int row, int col, const wxString& typeName,
                                  void *value);

    //@}


    /**
        Called by the grid when the table is associated with it.

        The default implementation stores the pointer and returns it from its
        GetView() and so only makes sense if the table cannot be associated
        with more than one grid at a time.
     */
    virtual void SetView(wxGrid *grid);

    /**
        Returns the last grid passed to SetView().
     */
    virtual wxGrid *GetView() const;


    /*!
        @name Table Structure Modifiers

        Note that none of these functions are pure virtual as they don't have
        to be implemented if the table structure is never modified after
        creation, i.e. neither rows nor columns are ever added or deleted.

        Also note that you do need to implement them if they are called, i.e. if your
        code either calls them directly or uses the matching wxGrid methods, as
        by default they simply do nothing which is definitely inappropriate.
     */
    //@{

    /**
        Clear the table contents.

        This method is used by wxGrid::ClearGrid().
     */
    virtual void Clear();

    /**
        Insert additional rows into the table.

        @param pos
            The position of the first new row.
        @param numRows
            The number of rows to insert.
     */
    virtual bool InsertRows(size_t pos = 0, size_t numRows = 1);

    /**
        Append additional rows at the end of the table.

        This method is provided in addition to InsertRows() as some data models
        may only support appending rows to them but not inserting them at
        arbitrary locations. In such case you may implement this method only
        and leave InsertRows() unimplemented.

        @param numRows
            The number of rows to add.
     */
    virtual bool AppendRows(size_t numRows = 1);

    /**
        Delete rows from the table.

        Notice that currently deleting a row intersecting a multi-cell (see
        SetCellSize()) is not supported and will result in a crash.

        @param pos
            The first row to delete.
        @param numRows
            The number of rows to delete.
     */
    virtual bool DeleteRows(size_t pos = 0, size_t numRows = 1);

    /**
        Exactly the same as InsertRows() but for columns.
     */
    virtual bool InsertCols(size_t pos = 0, size_t numCols = 1);

    /**
        Exactly the same as AppendRows() but for columns.
     */
    virtual bool AppendCols(size_t numCols = 1);

    /**
        Exactly the same as DeleteRows() but for columns.
     */
    virtual bool DeleteCols(size_t pos = 0, size_t numCols = 1);

    //@}

    /*!
        @name Table Row, Column and Corner Labels

        By default the numbers are used for labelling rows and Latin letters for
        labelling columns. If the table has more than 26 columns, the pairs of
        letters are used starting from the 27-th one and so on, i.e. the
        sequence of labels is A, B, ..., Z, AA, AB, ..., AZ, BA, ..., ..., ZZ,
        AAA, ...

        A cell in the top-left corner of a grid can also have a label. It is
        empty by default. Use wxGrid::SetCornerLabelValue() to set it and
        wxGrid::GetCornerLabelValue() to get its' current value.

        @see wxGridTableBase::GetCornerLabelValue, wxGridTableBase::SetCornerLabelValue
     */
    //@{

    /**
        Return the label of the specified row.
     */
    virtual wxString GetRowLabelValue(int row);

    /**
        Return the label of the specified column.
     */
    virtual wxString GetColLabelValue(int col);

    /**
        Return the label of the grid's corner.

        @since 3.1.2
     */
    virtual wxString GetCornerLabelValue() const;

    /**
        Set the given label for the specified row.

        The default version does nothing, i.e. the label is not stored. You
        must override this method in your derived class if you wish
        wxGrid::SetRowLabelValue() to work.
     */
    virtual void SetRowLabelValue(int row, const wxString& label);

    /**
        Exactly the same as SetRowLabelValue() but for columns.
     */
    virtual void SetColLabelValue(int col, const wxString& label);

    /**
        Set the given label for the grid's corner.

        The default version does nothing, i.e. the label is not stored. You
        must override this method in your derived class if you wish
        wxGrid::GetCornerLabelValue() to work.

        @since 3.1.2
     */
    virtual void SetCornerLabelValue( const wxString& );

    //@}


    /**
        @name Attributes Management

        By default the attributes management is delegated to
        wxGridCellAttrProvider class. You may override the methods in this
        section to handle the attributes directly if, for example, they can be
        computed from the cell values.
     */
    //@{

    /**
        Associate this attributes provider with the table.

        The table takes ownership of @a attrProvider pointer and will delete it
        when it doesn't need it any more. The pointer can be @NULL, however
        this won't disable attributes management in the table but will just
        result in a default attributes being recreated the next time any of the
        other functions in this section is called. To completely disable the
        attributes support, should this be needed, you need to override
        CanHaveAttributes() to return @false.
     */
    void SetAttrProvider(wxGridCellAttrProvider *attrProvider);

    /**
        Returns the attribute provider currently being used.

        This function may return @NULL if the attribute provider hasn't been
        neither associated with this table by SetAttrProvider() nor created on
        demand by any other methods.
     */
    wxGridCellAttrProvider *GetAttrProvider() const;

    /**
        Return the attribute for the given cell.

        By default this function is simply forwarded to
        wxGridCellAttrProvider::GetAttr() but it may be overridden to handle
        attributes directly in the table.

        Prefer to use GetAttrPtr() to avoid the need to call DecRef() on the
        returned pointer manually.
     */
    virtual wxGridCellAttr *GetAttr(int row, int col,
                                    wxGridCellAttr::wxAttrKind kind);

    /**
        Return the attribute for the given cell.

        This method is identical to GetAttr(), but returns a smart pointer,
        which frees the caller from the need to call DecRef() manually.

        @since 3.1.4
     */
    wxGridCellAttrPtr GetAttrPtr(int row, int col,
                                 wxGridCellAttr::wxAttrKind kind);

    /**
        Set attribute of the specified cell.

        By default this function is simply forwarded to
        wxGridCellAttrProvider::SetAttr().

        The table takes ownership of @a attr, i.e. will call DecRef() on it.
     */
    virtual void SetAttr(wxGridCellAttr* attr, int row, int col);

    /**
        Set attribute of the specified row.

        By default this function is simply forwarded to
        wxGridCellAttrProvider::SetRowAttr().

        The table takes ownership of @a attr, i.e. will call DecRef() on it.
     */
    virtual void SetRowAttr(wxGridCellAttr *attr, int row);

    /**
        Set attribute of the specified column.

        By default this function is simply forwarded to
        wxGridCellAttrProvider::SetColAttr().

        The table takes ownership of @a attr, i.e. will call DecRef() on it.
     */
    virtual void SetColAttr(wxGridCellAttr *attr, int col);

    //@}

    /**
        Returns true if this table supports attributes or false otherwise.

        By default, the table automatically creates a wxGridCellAttrProvider
        when this function is called if it had no attribute provider before and
        returns @true.
     */
    virtual bool CanHaveAttributes();

    /**
        Override to return true if the same attribute can be used for measuring
        all cells in the given column.

        This function is provided for optimization purposes: it returns @false
        by default, but can be overridden to return @true when all the cells in
        the same grid column use sensibly the same attribute, i.e. they use the
        same renderer (either explicitly, or implicitly, due to their type as
        returned by GetTypeName()) and the font of the same size.

        Returning @true from this function allows AutoSizeColumns() to skip
        looking up the attribute and the renderer for each individual cell,
        which results in very noticeable performance improvements for the grids
        with many rows.

        @since 3.1.4
     */
    virtual bool CanMeasureColUsingSameAttr(int col) const;
};


/**
    Possible types for grid table notifications.
 */
enum wxGridTableRequest
{
    /// New rows have been inserted into the table.
    wxGRIDTABLE_NOTIFY_ROWS_INSERTED,
    /// New rows have been append to the table.
    wxGRIDTABLE_NOTIFY_ROWS_APPENDED,
    /// Rows have been deleted from the table.
    wxGRIDTABLE_NOTIFY_ROWS_DELETED,
    /// New columns have been inserted into the table.
    wxGRIDTABLE_NOTIFY_COLS_INSERTED,
    /// New columns have been append to the table.
    wxGRIDTABLE_NOTIFY_COLS_APPENDED,
    /// Columns have been deleted from the table.
    wxGRIDTABLE_NOTIFY_COLS_DELETED
};



/**
    @class wxGridSizesInfo

    wxGridSizesInfo stores information about sizes of all wxGrid rows or
    columns.

    It assumes that most of the rows or columns (which are both called elements
    here as the difference between them doesn't matter at this class level)
    have the default size and so stores it separately. And it uses a wxHashMap
    to store the sizes of all elements which have the non-default size.

    This structure is particularly useful for serializing the sizes of all
    wxGrid elements at once.

    @library{wxcore}
    @category{grid}
 */
struct wxGridSizesInfo
{
    /**
        Default constructor.

        m_sizeDefault and m_customSizes must be initialized later.
     */
    wxGridSizesInfo();

    /**
        Constructor.

        This constructor is used by wxGrid::GetRowSizes() and GetColSizes()
        methods. User code will usually use the default constructor instead.

        @param defSize
            The default element size.
        @param allSizes
            Array containing the sizes of @em all elements, including those
            which have the default size.
     */
    wxGridSizesInfo(int defSize, const wxArrayInt& allSizes);

    /**
        Get the element size.

        @param pos
            The index of the element.
        @return
            The size for this element, using m_customSizes if @a pos is in it
            or m_sizeDefault otherwise.
     */
    int GetSize(unsigned pos) const;


    /// Default size
    int m_sizeDefault;

    /**
        Map with element indices as keys and their sizes as values.

        This map only contains the elements with non-default size.
     */
    wxUnsignedToIntHashMap m_customSizes;
};



/**
    Rendering styles supported by wxGrid::Render() method.

    @since 2.9.4
 */
enum wxGridRenderStyle
{
    /// Draw grid row header labels.
    wxGRID_DRAW_ROWS_HEADER = 0x001,

    /// Draw grid column header labels.
    wxGRID_DRAW_COLS_HEADER = 0x002,

    /// Draw grid cell border lines.
    wxGRID_DRAW_CELL_LINES = 0x004,

    /**
        Draw a bounding rectangle around the rendered cell area.

        Useful where row or column headers are not drawn or where there is
        multi row or column cell clipping and therefore no cell border at
        the rendered outer boundary.
    */
    wxGRID_DRAW_BOX_RECT = 0x008,

    /**
        Draw the grid cell selection highlight if a selection is present.

        At present the highlight colour drawn depends on whether the grid
        window loses focus before drawing begins.
    */
    wxGRID_DRAW_SELECTION = 0x010,

    /**
        The default render style.

        Includes all except wxGRID_DRAW_SELECTION.
     */
    wxGRID_DRAW_DEFAULT = wxGRID_DRAW_ROWS_HEADER |
                          wxGRID_DRAW_COLS_HEADER |
                          wxGRID_DRAW_CELL_LINES |
                          wxGRID_DRAW_BOX_RECT
};



/**
    @class wxGrid

    wxGrid and its related classes are used for displaying and editing tabular
    data. They provide a rich set of features for display, editing, and
    interacting with a variety of data sources. For simple applications, and to
    help you get started, wxGrid is the only class you need to refer to
    directly. It will set up default instances of the other classes and manage
    them for you. For more complex applications you can derive your own classes
    for custom grid views, grid data tables, cell editors and renderers. The
    @ref overview_grid has examples of simple and more complex applications,
    explains the relationship between the various grid classes and has a
    summary of the keyboard shortcuts and mouse functions provided by wxGrid.

    A wxGridTableBase class holds the actual data to be displayed by a wxGrid
    class. One or more wxGrid classes may act as a view for one table class.
    The default table class is called wxGridStringTable and holds an array of
    strings. An instance of such a class is created by CreateGrid().

    wxGridCellRenderer is the abstract base class for rendering contents in a
    cell. The following renderers are predefined:

    - wxGridCellBoolRenderer
    - wxGridCellFloatRenderer
    - wxGridCellNumberRenderer
    - wxGridCellStringRenderer
    - wxGridCellDateRenderer
    - wxGridCellDateTimeRenderer

    The look of a cell can be further defined using wxGridCellAttr. An object
    of this type may be returned by wxGridTableBase::GetAttr().

    wxGridCellEditor is the abstract base class for editing the value of a
    cell. The following editors are predefined:

    - wxGridCellBoolEditor
    - wxGridCellChoiceEditor
    - wxGridCellFloatEditor
    - wxGridCellNumberEditor
    - wxGridCellTextEditor
    - wxGridCellDateEditor

    Please see wxGridEvent, wxGridSizeEvent, wxGridRangeSelectEvent, and
    wxGridEditorCreatedEvent for the documentation of all event types you can
    use with wxGrid.

    @library{wxcore}
    @category{grid}

    @see @ref overview_grid, wxGridUpdateLocker
*/
class wxGrid : public wxScrolledCanvas
{
public:

    /**
        Different selection modes supported by the grid.
     */
    enum wxGridSelectionModes
    {
        /**
            The default selection mode allowing selection of the individual
            cells as well as of the entire rows and columns.
         */
        wxGridSelectCells,

        /**
            The selection mode allowing the selection of the entire rows only.

            The user won't be able to select any cells or columns in this mode.
         */
        wxGridSelectRows,

        /**
            The selection mode allowing the selection of the entire columns only.

            The user won't be able to select any cells or rows in this mode.
         */
        wxGridSelectColumns,

        /**
            The selection mode allowing the user to select either the entire
            columns or the entire rows but not individual cells nor blocks.

            Notice that while this constant is defined as @code
            wxGridSelectColumns | wxGridSelectRows @endcode this doesn't mean
            that all the other combinations are valid -- at least currently
            they are not.

            @since 2.9.1
         */
        wxGridSelectRowsOrColumns
    };

    /**
        Return values for GetCellSize().

        @since 2.9.1
     */
    enum CellSpan
    {
        /// This cell is inside a span covered by another cell.
        CellSpan_Inside = -1,

        /// This is a normal, non-spanning cell.
        CellSpan_None = 0,

        /// This cell spans several physical wxGrid cells.
        CellSpan_Main
    };

    /**
        Constants defining different support built-in TAB handling behaviours.

        The elements of this enum determine what happens when TAB is pressed
        when the cursor is in the rightmost column (or Shift-TAB is pressed
        when the cursor is in the leftmost one).

        @see SetTabBehaviour(), @c wxEVT_GRID_TABBING

        @since 2.9.5
     */
    enum TabBehaviour
    {
        /// Do nothing, this is default.
        Tab_Stop,

        /// Move to the beginning of the next (or the end of the previous) row.
        Tab_Wrap,

        /// Move to the next (or the previous) control after the grid.
        Tab_Leave
    };

    /**
        @name Constructors and Initialization
     */
    //@{

    /**
        Default constructor.

        You must call Create() to really create the grid window and also call
        CreateGrid() or SetTable() or AssignTable() to initialize its contents.
     */
    wxGrid();
    /**
        Constructor creating the grid window.

        You must call either CreateGrid() or SetTable() or AssignTable() to
        initialize the grid contents before using it.
    */
    wxGrid(wxWindow* parent, wxWindowID id,
           const wxPoint& pos = wxDefaultPosition,
           const wxSize& size = wxDefaultSize,
           long style = wxWANTS_CHARS,
           const wxString& name = wxGridNameStr);

    /**
        Destructor.

        This will also destroy the associated grid table unless you passed a
        table object to the grid and specified that the grid should not take
        ownership of the table (see SetTable()).
    */
    virtual ~wxGrid();

    /**
        Creates the grid window for an object initialized using the default
        constructor.

        You must call either CreateGrid() or SetTable() or AssignTable() to
        initialize the grid contents before using it.
     */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxWANTS_CHARS,
                const wxString& name = wxGridNameStr);

    /**
        Creates a grid with the specified initial number of rows and columns.

        Call this directly after the grid constructor. When you use this
        function wxGrid will create and manage a simple table of string values
        for you. All of the grid data will be stored in memory.

        For applications with more complex data types or relationships, or for
        dealing with very large datasets, you should derive your own grid table
        class and pass a table object to the grid with SetTable() or
        AssignTable().
    */
    bool CreateGrid(int numRows, int numCols,
                    wxGridSelectionModes selmode = wxGridSelectCells);

    /**
        Passes a pointer to a custom grid table to be used by the grid.

        This should be called after the grid constructor and before using the
        grid object. If @a takeOwnership is set to @true then the table will be
        deleted by the wxGrid destructor.

        Use this function instead of CreateGrid() when your application
        involves complex or non-string data or data sets that are too large to
        fit wholly in memory.

        When the custom table should be owned by the grid, consider using the
        simpler AssignTable() function instead of this one with @true value of
        @a takeOwnership parameter.
    */
    bool SetTable(wxGridTableBase* table, bool takeOwnership = false,
                  wxGridSelectionModes selmode = wxGridSelectCells);

    /**
        Assigns a pointer to a custom grid table to be used by the grid.

        This function is identical to SetTable() with @c takeOwnership
        parameter set to @true, i.e. it simply always takes the ownership of
        the passed in pointer. This makes it simpler to use than SetTable() in
        the common case when the table should be owned by the grid object.

        Note that this function should be called at most once and can't be used
        to change the table used by the grid later on or reset it: if such
        extra flexibility is needed, use SetTable() directly.

        @since 3.1.4

        @param table The heap-allocated pointer to the table.
        @param selmode Selection mode to use.
    */
    void AssignTable( wxGridTableBase *table,
                      wxGridSelectionModes selmode = wxGridSelectCells);

    /**
       Receive and handle a message from the table.
    */
    bool ProcessTableMessage(wxGridTableMessage& msg);

    //@}


    /**
        @name Grid Line Formatting
     */
    //@{

    /**
        Turns the drawing of grid lines on or off.
    */
    void EnableGridLines(bool enable = true);

    /**
        Returns the pen used for vertical grid lines.

        This virtual function may be overridden in derived classes in order to
        change the appearance of individual grid lines for the given column
        @a col.

        See GetRowGridLinePen() for an example.
    */
    virtual wxPen GetColGridLinePen(int col);

    /**
        Returns the pen used for grid lines.

        This virtual function may be overridden in derived classes in order to
        change the appearance of grid lines. Note that currently the pen width
        must be 1.

        @see GetColGridLinePen(), GetRowGridLinePen()
    */
    virtual wxPen GetDefaultGridLinePen();

    /**
        Returns the colour used for grid lines.

        @see GetDefaultGridLinePen()
    */
    wxColour GetGridLineColour() const;

    /**
        Returns the pen used for horizontal grid lines.

        This virtual function may be overridden in derived classes in order to
        change the appearance of individual grid line for the given @a row.

        Example:
        @code
        // in a grid displaying music notation, use a solid black pen between
        // octaves (C0=row 127, C1=row 115 etc.)
        wxPen MidiGrid::GetRowGridLinePen(int row)
        {
            if ( row % 12 == 7 )
                return wxPen(*wxBLACK, 1, wxPENSTYLE_SOLID);
            else
                return GetDefaultGridLinePen();
        }
        @endcode
    */
    virtual wxPen GetRowGridLinePen(int row);

    /**
        Returns @true if drawing of grid lines is turned on, @false otherwise.
    */
    bool GridLinesEnabled() const;

    /**
        Sets the colour used to draw grid lines.
    */
    void SetGridLineColour(const wxColour& colour);

    //@}


    /**
        @name Label Values and Formatting
     */
    //@{

    /**
        Sets the arguments to the current column label alignment values.

        Horizontal alignment will be one of @c wxALIGN_LEFT, @c wxALIGN_CENTRE
        or @c wxALIGN_RIGHT.

        Vertical alignment will be one of @c wxALIGN_TOP, @c wxALIGN_CENTRE or
        @c wxALIGN_BOTTOM.
    */
    void GetColLabelAlignment(int* horiz, int* vert) const;

    /**
        Returns the orientation of the column labels (either @c wxHORIZONTAL or
        @c wxVERTICAL).
    */
    int GetColLabelTextOrientation() const;

    /**
        Returns the specified column label.

        The default grid table class provides column labels of the form
        A,B...Z,AA,AB...ZZ,AAA... If you are using a custom grid table you can
        override wxGridTableBase::GetColLabelValue() to provide your own
        labels.
    */
    wxString GetColLabelValue(int col) const;

    /**
        Sets the arguments to the current corner label alignment values.

        Horizontal alignment will be one of @c wxALIGN_LEFT, @c wxALIGN_CENTRE
        or @c wxALIGN_RIGHT.

        Vertical alignment will be one of @c wxALIGN_TOP, @c wxALIGN_CENTRE or
        @c wxALIGN_BOTTOM.

        @since 3.1.2
    */
    void GetCornerLabelAlignment( int *horiz, int *vert ) const;

    /**
        Returns the orientation of the corner label (either @c wxHORIZONTAL or
        @c wxVERTICAL).

        @since 3.1.2
    */
    int GetCornerLabelTextOrientation() const;

    /**
        Returns the (top-left) corner label.

        @since 3.1.2
    */
    wxString GetCornerLabelValue() const;

    /**
        Returns the colour used for the background of row and column labels.
    */
    wxColour GetLabelBackgroundColour() const;

    /**
        Returns the font used for row and column labels.
    */
    wxFont GetLabelFont() const;

    /**
        Returns the colour used for row and column label text.
    */
    wxColour GetLabelTextColour() const;

    /**
        Returns the alignment used for row labels.

        Horizontal alignment will be one of @c wxALIGN_LEFT, @c wxALIGN_CENTRE
        or @c wxALIGN_RIGHT.

        Vertical alignment will be one of @c wxALIGN_TOP, @c wxALIGN_CENTRE or
        @c wxALIGN_BOTTOM.
    */
    void GetRowLabelAlignment(int* horiz, int* vert) const;

    /**
        Returns the specified row label.

        The default grid table class provides numeric row labels. If you are
        using a custom grid table you can override
        wxGridTableBase::GetRowLabelValue() to provide your own labels.
    */
    wxString GetRowLabelValue(int row) const;

    /**
        Hides the column labels by calling SetColLabelSize() with a size of 0.

        The labels can be shown again by calling SetColLabelSize() with a
        height greater than 0.

        Note that when the column labels are hidden, the grid won't have any
        visible border on the top side, which may result in a less than ideal
        appearance. Because of this, you may want to create the grid window
        with a border style, such as @c wxBORDER_SIMPLE, when you don't plan to
        show the column labels for it.

        @see HideRowLabels()
    */
    void HideColLabels();

    /**
        Hides the row labels by calling SetRowLabelSize() with a size of 0.

        The labels can be shown again by calling SetRowLabelSize() with a width
        greater than 0.

        See HideColLabels() for a note explaining why you may want to use a
        border with a grid without the row labels.
    */
    void HideRowLabels();

    /**
        Sets the horizontal and vertical alignment of column label text.

        Horizontal alignment should be one of @c wxALIGN_LEFT,
        @c wxALIGN_CENTRE or @c wxALIGN_RIGHT. Vertical alignment should be one
        of @c wxALIGN_TOP, @c wxALIGN_CENTRE or @c wxALIGN_BOTTOM.
    */
    void SetColLabelAlignment(int horiz, int vert);

    /**
        Sets the orientation of the column labels (either @c wxHORIZONTAL or
        @c wxVERTICAL).
    */
    void SetColLabelTextOrientation(int textOrientation);

    /**
        Set the value for the given column label.

        If you are using a custom grid table you must override
        wxGridTableBase::SetColLabelValue() for this to have any effect.
    */
    void SetColLabelValue(int col, const wxString& value);

    /**
        Sets the horizontal and vertical alignment of the (top-left) corner label text.

        Horizontal alignment should be one of @c wxALIGN_LEFT,
        @c wxALIGN_CENTRE or @c wxALIGN_RIGHT. Vertical alignment should be one
        of @c wxALIGN_TOP, @c wxALIGN_CENTRE or @c wxALIGN_BOTTOM.

        @since 3.1.2
    */
    void SetCornerLabelAlignment( int horiz, int vert );

    /**
        Sets the orientation of the (top-left) corner label (either @c wxHORIZONTAL or
        @c wxVERTICAL).

        @since 3.1.2
    */
    void SetCornerLabelTextOrientation( int textOrientation );

    /**
        Set the value for the (top-left) corner label.

        If you are using a custom grid table you must override
        wxGridTableBase::SetCornerLabelValue() for this to have any effect.

        @since 3.1.2
    */
    void SetCornerLabelValue( const wxString& );

    /**
        Sets the background colour for row and column labels.
    */
    void SetLabelBackgroundColour(const wxColour& colour);

    /**
        Sets the font for row and column labels.
    */
    void SetLabelFont(const wxFont& font);

    /**
        Sets the colour for row and column label text.
    */
    void SetLabelTextColour(const wxColour& colour);

    /**
        Sets the horizontal and vertical alignment of row label text.

        Horizontal alignment should be one of @c wxALIGN_LEFT,
        @c wxALIGN_CENTRE or @c wxALIGN_RIGHT. Vertical alignment should be one
        of @c wxALIGN_TOP, @c wxALIGN_CENTRE or @c wxALIGN_BOTTOM.
    */
    void SetRowLabelAlignment(int horiz, int vert);

    /**
        Sets the value for the given row label.

        If you are using a derived grid table you must override
        wxGridTableBase::SetRowLabelValue() for this to have any effect.
    */
    void SetRowLabelValue(int row, const wxString& value);

    /**
        Call this in order to make the column labels use a native look by using
        wxRendererNative::DrawHeaderButton() internally.

        There is no equivalent method for drawing row columns as there is not
        native look for that. This option is useful when using wxGrid for
        displaying tables and not as a spread-sheet.

        @see UseNativeColHeader()
    */
    void SetUseNativeColLabels(bool native = true);

    /**
        Enable the use of native header window for column labels.

        If this function is called with @true argument, a wxHeaderCtrl is used
        instead to display the column labels instead of drawing them in wxGrid
        code itself. This has the advantage of making the grid look and feel
        perfectly the same as native applications (using SetUseNativeColLabels()
        the grid can be made to look more natively but it still doesn't feel
        natively, notably the column resizing and dragging still works slightly
        differently as it is implemented in wxWidgets itself) but results in
        different behaviour for column and row headers, for which there is no
        equivalent function, and, most importantly, is unsuitable for grids
        with huge numbers of columns as wxHeaderCtrl doesn't support virtual
        mode. Because of this, by default the grid does not use the native
        header control but you should call this function to enable it if you
        are using the grid to display tabular data and don't have thousands of
        columns in it.

        Another difference between the default behaviour and the native header
        behaviour is that the latter provides the user with a context menu
        (which appears on right clicking the header) allowing to rearrange the
        grid columns if CanDragColMove() returns @true. If you want to prevent
        this from happening for some reason, you need to define a handler for
        @c wxEVT_GRID_LABEL_RIGHT_CLICK event which simply does nothing (in
        particular doesn't skip the event) as this will prevent the default
        right click handling from working.

        Also note that currently @c wxEVT_GRID_LABEL_RIGHT_DCLICK event is not
        generated for the column labels if the native columns header is used
        (but this limitation could possibly be lifted in the future).

        Finally, please note that using the native control is currently
        incompatible with freezing columns in the grid (see FreezeTo()) and
        this function will return @false, without doing anything, if it's
        called on a grid in which any columns are frozen.
     */
    bool UseNativeColHeader(bool native = true);

    //@}


    /*!
        @name Cell Formatting

        Note that wxGridCellAttr can be used alternatively to most of these
        methods. See the "Attributes Management" of wxGridTableBase.
     */
    //@{

    /**
        Sets the arguments to the horizontal and vertical text alignment values
        for the grid cell at the specified location.

        Horizontal alignment will be one of @c wxALIGN_LEFT, @c wxALIGN_CENTRE
        or @c wxALIGN_RIGHT.

        Vertical alignment will be one of @c wxALIGN_TOP, @c wxALIGN_CENTRE or
        @c wxALIGN_BOTTOM.
    */
    void GetCellAlignment(int row, int col, int* horiz, int* vert) const;

    /**
        Returns the background colour of the cell at the specified location.
    */
    wxColour GetCellBackgroundColour(int row, int col) const;

    /**
        Returns the font for text in the grid cell at the specified location.
    */
    wxFont GetCellFont(int row, int col) const;

    /**
        Returns the text colour for the grid cell at the specified location.
    */
    wxColour GetCellTextColour(int row, int col) const;

    /**
        Returns the default cell alignment.

        Horizontal alignment will be one of @c wxALIGN_LEFT, @c wxALIGN_CENTRE
        or @c wxALIGN_RIGHT.

        Vertical alignment will be one of @c wxALIGN_TOP, @c wxALIGN_CENTRE or
        @c wxALIGN_BOTTOM.

        @see SetDefaultCellAlignment()
    */
    void GetDefaultCellAlignment(int* horiz, int* vert) const;

    /**
        Returns the current default background colour for grid cells.
    */
    wxColour GetDefaultCellBackgroundColour() const;

    /**
        Returns the current default font for grid cell text.
    */
    wxFont GetDefaultCellFont() const;

    /**
        Returns the current default colour for grid cell text.
    */
    wxColour GetDefaultCellTextColour() const;

    /**
        Sets the horizontal and vertical alignment for grid cell text at the
        specified location.

        Horizontal alignment should be one of @c wxALIGN_LEFT,
        @c wxALIGN_CENTRE or @c wxALIGN_RIGHT.

        Vertical alignment should be one of @c wxALIGN_TOP, @c wxALIGN_CENTRE
        or @c wxALIGN_BOTTOM.
    */
    void SetCellAlignment(int row, int col, int horiz, int vert);
    /**
        Sets the horizontal and vertical alignment for grid cell text at the
        specified location.

        Horizontal alignment should be one of @c wxALIGN_LEFT,
        @c wxALIGN_CENTRE or @c wxALIGN_RIGHT.

        Vertical alignment should be one of @c wxALIGN_TOP, @c wxALIGN_CENTRE
        or @c wxALIGN_BOTTOM.

        @deprecated Please use SetCellAlignment(row, col, horiz, vert) instead.
    */
    void SetCellAlignment(int align, int row, int col);

    /**
        Set the background colour for the given cell or all cells by default.
    */
    void SetCellBackgroundColour(int row, int col, const wxColour& colour);

    /**
        Sets the font for text in the grid cell at the specified location.
    */
    void SetCellFont(int row, int col, const wxFont& font);

    /**
        Sets the text colour for the given cell.
    */
    void SetCellTextColour(int row, int col, const wxColour& colour);
    /**
        Sets the text colour for the given cell.

        @deprecated Please use SetCellTextColour(row, col, colour)
    */
    void SetCellTextColour(const wxColour& val, int row, int col);
    /**
        Sets the text colour for all cells by default.

        @deprecated Please use SetDefaultCellTextColour(colour) instead.
    */
    void SetCellTextColour(const wxColour& colour);

    /**
        Sets the default horizontal and vertical alignment for grid cell text.

        Horizontal alignment should be one of @c wxALIGN_LEFT,
        @c wxALIGN_CENTRE or @c wxALIGN_RIGHT. Vertical alignment should be one
        of @c wxALIGN_TOP, @c wxALIGN_CENTRE or @c wxALIGN_BOTTOM.
    */
    void SetDefaultCellAlignment(int horiz, int vert);

    /**
        Sets the default background colour for grid cells.
    */
    void SetDefaultCellBackgroundColour(const wxColour& colour);

    /**
        Sets the default font to be used for grid cell text.
    */
    void SetDefaultCellFont(const wxFont& font);

    /**
        Sets the current default colour for grid cell text.
    */
    void SetDefaultCellTextColour(const wxColour& colour);

    //@}


    /*!
        @name Cell Values, Editors, and Renderers

        Note that wxGridCellAttr can be used alternatively to most of these
        methods. See the "Attributes Management" of wxGridTableBase.
     */
    //@{

    /**
        Returns @true if the in-place edit control for the current grid cell
        can be used and @false otherwise.

        This function always returns @false for the read-only cells.
    */
    bool CanEnableCellControl() const;

    /**
        Disables in-place editing of grid cells.

        Equivalent to calling EnableCellEditControl(@false).
    */
    void DisableCellEditControl();

    /**
        Enables or disables in-place editing of grid cell data.

        Enabling in-place editing generates @c wxEVT_GRID_EDITOR_SHOWN and, if
        it isn't vetoed by the application, shows the in-place editor which
        allows the user to change the cell value.

        Disabling in-place editing does nothing if the in-place editor isn't
        currently shown, otherwise the @c wxEVT_GRID_EDITOR_HIDDEN event is
        generated but, unlike the "shown" event, it can't be vetoed and the
        in-place editor is dismissed unconditionally.

        Note that it is an error to call this function if the current cell is
        read-only, use CanEnableCellControl() to check for this precondition.
    */
    void EnableCellEditControl(bool enable = true);

    /**
        Makes the grid globally editable or read-only.

        If the edit argument is @false this function sets the whole grid as
        read-only. If the argument is @true the grid is set to the default
        state where cells may be editable. In the default state you can set
        single grid cells and whole rows and columns to be editable or
        read-only via wxGridCellAttr::SetReadOnly(). For single cells you
        can also use the shortcut function SetReadOnly().

        For more information about controlling grid cell attributes see the
        wxGridCellAttr class and the @ref overview_grid.
    */
    void EnableEditing(bool edit);

    /**
        Returns a pointer to the editor for the cell at the specified location.

        See wxGridCellEditor and the @ref overview_grid for more information
        about cell editors and renderers.

        The caller must call DecRef() on the returned pointer.
    */
    wxGridCellEditor* GetCellEditor(int row, int col) const;

    /**
        Returns a pointer to the renderer for the grid cell at the specified
        location.

        See wxGridCellRenderer and the @ref overview_grid for more information
        about cell editors and renderers.

        The caller must call DecRef() on the returned pointer.
    */
    wxGridCellRenderer* GetCellRenderer(int row, int col) const;

    /**
        Returns the string contained in the cell at the specified location.

        For simple applications where a grid object automatically uses a
        default grid table of string values you use this function together with
        SetCellValue() to access cell values. For more complex applications
        where you have derived your own grid table class that contains various
        data types (e.g. numeric, boolean or user-defined custom types) then
        you only use this function for those cells that contain string values.

        See wxGridTableBase::CanGetValueAs() and the @ref overview_grid for
        more information.
    */
    wxString GetCellValue(int row, int col) const;
    /**
        Returns the string contained in the cell at the specified location.

        For simple applications where a grid object automatically uses a
        default grid table of string values you use this function together with
        SetCellValue() to access cell values. For more complex applications
        where you have derived your own grid table class that contains various
        data types (e.g. numeric, boolean or user-defined custom types) then
        you only use this function for those cells that contain string values.

        See wxGridTableBase::CanGetValueAs() and the @ref overview_grid for
        more information.
    */
    wxString GetCellValue(const wxGridCellCoords& coords) const;

    /**
        Returns a pointer to the current default grid cell editor.

        See wxGridCellEditor and the @ref overview_grid for more information
        about cell editors and renderers.
    */
    wxGridCellEditor* GetDefaultEditor() const;

    /**
        Returns the default editor for the specified cell.

        The base class version returns the editor appropriate for the current
        cell type but this method may be overridden in the derived classes to
        use custom editors for some cells by default.

        Notice that the same may be achieved in a usually simpler way by
        associating a custom editor with the given cell or cells.

        The caller must call DecRef() on the returned pointer.
    */
    virtual wxGridCellEditor* GetDefaultEditorForCell(int row, int col) const;
    /**
        Returns the default editor for the specified cell.

        The base class version returns the editor appropriate for the current
        cell type but this method may be overridden in the derived classes to
        use custom editors for some cells by default.

        Notice that the same may be achieved in a usually simpler way by
        associating a custom editor with the given cell or cells.

        The caller must call DecRef() on the returned pointer.
    */
    wxGridCellEditor* GetDefaultEditorForCell(const wxGridCellCoords& c) const;

    /**
        Returns the default editor for the cells containing values of the given
        type.

        The base class version returns the editor which was associated with the
        specified @a typeName when it was registered RegisterDataType() but
        this function may be overridden to return something different. This
        allows overriding an editor used for one of the standard types.

        The caller must call DecRef() on the returned pointer.
    */
    virtual wxGridCellEditor* GetDefaultEditorForType(const wxString& typeName) const;

    /**
        Returns a pointer to the current default grid cell renderer.

        See wxGridCellRenderer and the @ref overview_grid for more information
        about cell editors and renderers.

        The caller must call DecRef() on the returned pointer.
    */
    wxGridCellRenderer* GetDefaultRenderer() const;

    /**
        Returns the default renderer for the given cell.

        The base class version returns the renderer appropriate for the current
        cell type but this method may be overridden in the derived classes to
        use custom renderers for some cells by default.

        The caller must call DecRef() on the returned pointer.
    */
    virtual wxGridCellRenderer* GetDefaultRendererForCell(int row, int col) const;

    /**
        Returns the default renderer for the cell containing values of the
        given type.

        @see GetDefaultEditorForType()
    */
    virtual wxGridCellRenderer* GetDefaultRendererForType(const wxString& typeName) const;

    /**
        Hides the in-place cell edit control.
    */
    void HideCellEditControl();

    /**
        Returns @true if the in-place edit control is currently enabled.
    */
    bool IsCellEditControlEnabled() const;

    /**
        Returns @true if the in-place edit control is currently shown.

        @see HideCellEditControl()
    */
    bool IsCellEditControlShown() const;

    /**
        Returns @true if the current cell is read-only.

        @see SetReadOnly(), IsReadOnly()
    */
    bool IsCurrentCellReadOnly() const;

    /**
        Returns @false if the whole grid has been set as read-only or @true
        otherwise.

        See EnableEditing() for more information about controlling the editing
        status of grid cells.
    */
    bool IsEditable() const;

    /**
        Returns @true if the cell at the specified location can't be edited.

        @see SetReadOnly(), IsCurrentCellReadOnly()
    */
    bool IsReadOnly(int row, int col) const;

    /**
        Register a new data type.

        The data types allow to naturally associate specific renderers and
        editors to the cells containing values of the given type. For example,
        the grid automatically registers a data type with the name
        @c wxGRID_VALUE_STRING which uses wxGridCellStringRenderer and
        wxGridCellTextEditor as its renderer and editor respectively -- this is
        the data type used by all the cells of the default wxGridStringTable,
        so this renderer and editor are used by default for all grid cells.

        However if a custom table returns @c wxGRID_VALUE_BOOL from its
        wxGridTableBase::GetTypeName() method, then wxGridCellBoolRenderer and
        wxGridCellBoolEditor are used for it because the grid also registers a
        boolean data type with this name.

        And as this mechanism is completely generic, you may register your own
        data types using your own custom renderers and editors. Just remember
        that the table must identify a cell as being of the given type for them
        to be used for this cell.

        @param typeName
            Name of the new type. May be any string, but if the type name is
            the same as the name of an already registered type, including one
            of the standard ones (which are @c wxGRID_VALUE_STRING, @c
            wxGRID_VALUE_BOOL, @c wxGRID_VALUE_NUMBER, @c wxGRID_VALUE_FLOAT,
            @c wxGRID_VALUE_CHOICE and @c wxGRID_VALUE_DATE), then the new
            registration information replaces the previously used renderer and
            editor.
        @param renderer
            The renderer to use for the cells of this type. Its ownership is
            taken by the grid, i.e. it will call DecRef() on this pointer when
            it doesn't need it any longer.
        @param editor
            The editor to use for the cells of this type. Its ownership is also
            taken by the grid.
    */
    void RegisterDataType(const wxString& typeName,
                          wxGridCellRenderer* renderer,
                          wxGridCellEditor* editor);

    /**
        Sets the value of the current grid cell to the current in-place edit
        control value.

        This is called automatically when the grid cursor moves from the
        current cell to a new cell. It is also a good idea to call this
        function when closing a grid since any edits to the final cell location
        will not be saved otherwise.
    */
    void SaveEditControlValue();

    /**
        Sets the editor for the grid cell at the specified location.

        The grid will take ownership of the pointer.

        See wxGridCellEditor and the @ref overview_grid for more information
        about cell editors and renderers.
    */
    void SetCellEditor(int row, int col, wxGridCellEditor* editor);

    /**
        Sets the renderer for the grid cell at the specified location.

        The grid will take ownership of the pointer.

        See wxGridCellRenderer and the @ref overview_grid for more information
        about cell editors and renderers.
    */
    void SetCellRenderer(int row, int col, wxGridCellRenderer* renderer);

    /**
        Sets the string value for the cell at the specified location.

        For simple applications where a grid object automatically uses a
        default grid table of string values you use this function together with
        GetCellValue() to access cell values. For more complex applications
        where you have derived your own grid table class that contains various
        data types (e.g. numeric, boolean or user-defined custom types) then
        you only use this function for those cells that contain string values.

        See wxGridTableBase::CanSetValueAs() and the @ref overview_grid for
        more information.
    */
    void SetCellValue(int row, int col, const wxString& s);
    /**
        Sets the string value for the cell at the specified location.

        For simple applications where a grid object automatically uses a
        default grid table of string values you use this function together with
        GetCellValue() to access cell values. For more complex applications
        where you have derived your own grid table class that contains various
        data types (e.g. numeric, boolean or user-defined custom types) then
        you only use this function for those cells that contain string values.

        See wxGridTableBase::CanSetValueAs() and the @ref overview_grid for
        more information.
    */
    void SetCellValue(const wxGridCellCoords& coords, const wxString& s);
    /**
        @deprecated Please use SetCellValue(int,int,const wxString&) or
                    SetCellValue(const wxGridCellCoords&,const wxString&)
                    instead.

        Sets the string value for the cell at the specified location.

        For simple applications where a grid object automatically uses a
        default grid table of string values you use this function together with
        GetCellValue() to access cell values. For more complex applications
        where you have derived your own grid table class that contains various
        data types (e.g. numeric, boolean or user-defined custom types) then
        you only use this function for those cells that contain string values.

        See wxGridTableBase::CanSetValueAs() and the @ref overview_grid for
        more information.
    */
    void SetCellValue(const wxString& val, int row, int col);

    /**
        Sets the specified column to display boolean values.

        @see SetColFormatCustom()
    */
    void SetColFormatBool(int col);

    /**
        Sets the specified column to display data in a custom format.

        This method provides an alternative to defining a custom grid table
        which would return @a typeName from its GetTypeName() method for the
        cells in this column: while it doesn't really change the type of the
        cells in this column, it does associate the renderer and editor used
        for the cells of the specified type with them.

        See the @ref overview_grid for more information on working with custom
        data types.
    */
    void SetColFormatCustom(int col, const wxString& typeName);

    /**
        Sets the specified column to display floating point values with the
        given width and precision.

        @see SetColFormatCustom()
    */
    void SetColFormatFloat(int col, int width = -1, int precision = -1);

    /**
        Sets the specified column to display integer values.

        @see SetColFormatCustom()
    */
    void SetColFormatNumber(int col);

    /**
        Sets the specified column to display date values.

        The @a format argument is used with wxGridCellDateRenderer and allows
        to specify the strftime-like format string to use for displaying the
        dates in this column.

        @see SetColFormatCustom()

        @since 3.1.3
    */
    void SetColFormatDate(int col, const wxString& format = wxString());

    /**
        Sets the default editor for grid cells.

        The grid will take ownership of the pointer.

        See wxGridCellEditor and the @ref overview_grid for more information
        about cell editors and renderers.
    */
    void SetDefaultEditor(wxGridCellEditor* editor);

    /**
        Sets the default renderer for grid cells.

        The grid will take ownership of the pointer.

        See wxGridCellRenderer and the @ref overview_grid for more information
        about cell editors and renderers.
    */
    void SetDefaultRenderer(wxGridCellRenderer* renderer);

    /**
        Makes the cell at the specified location read-only or editable.

        @see IsReadOnly()
    */
    void SetReadOnly(int row, int col, bool isReadOnly = true);

    /**
        Displays the active in-place cell edit control for the current cell
        after it was hidden.

        This method should only be called after calling HideCellEditControl(),
        to start editing the current grid cell use EnableCellEditControl()
        instead.
    */
    void ShowCellEditControl();

    //@}


    /**
        @name Column and Row Sizes

        @see @ref overview_grid_resizing
     */
    //@{

    /**
        Automatically sets the height and width of all rows and columns to fit
        their contents.
    */
    void AutoSize();

    /**
        Automatically adjusts width of the column to fit its label.
    */
    void AutoSizeColLabelSize(int col);

    /**
        Automatically sizes the column to fit its contents. If @a setAsMin is
        @true the calculated width will also be set as the minimal width for
        the column.
    */
    void AutoSizeColumn(int col, bool setAsMin = true);

    /**
        Automatically sizes all columns to fit their contents. If @a setAsMin
        is @true the calculated widths will also be set as the minimal widths
        for the columns.
    */
    void AutoSizeColumns(bool setAsMin = true);

    /**
        Automatically sizes the row to fit its contents. If @a setAsMin is
        @true the calculated height will also be set as the minimal height for
        the row.
    */
    void AutoSizeRow(int row, bool setAsMin = true);

    /**
        Automatically adjusts height of the row to fit its label.
    */
    void AutoSizeRowLabelSize(int col);

    /**
        Automatically sizes all rows to fit their contents. If @a setAsMin is
        @true the calculated heights will also be set as the minimal heights
        for the rows.
    */
    void AutoSizeRows(bool setAsMin = true);

    /**
        Returns the cell fitting mode.

        @see wxGridFitMode

        @since 3.1.4
     */
    wxGridFitMode GetCellFitMode(int row, int col) const;

    /**
        Returns @true if the cell value can overflow.

        This is identical to calling GetCellFitMode() and using
        wxGridFitMode::IsOverflow() on the returned value.

        Prefer using GetCellFitMode() directly in the new code.
    */
    bool GetCellOverflow(int row, int col) const;

    /**
        Returns the current height of the column labels.
    */
    int GetColLabelSize() const;

    /**
        Returns the minimal width to which a column may be resized.

        Use SetColMinimalAcceptableWidth() to change this value globally or
        SetColMinimalWidth() to do it for individual columns.

        @see GetRowMinimalAcceptableHeight()
    */
    int GetColMinimalAcceptableWidth() const;

    /**
        Returns the width of the specified column.
    */
    int GetColSize(int col) const;

    /**
        Returns @true if the specified column is not currently hidden.
     */
    bool IsColShown(int col) const;

    /**
        Returns the default cell fitting mode.

        The default mode is "overflow", but can be modified using
        SetDefaultCellFitMode().

        @see wxGridFitMode

        @since 3.1.4
     */
    wxGridFitMode GetDefaultCellFitMode() const;

    /**
        Returns @true if the cells can overflow by default.

        This is identical to calling GetDefaultCellFitMode() and using
        wxGridFitMode::IsOverflow() on the returned value.

        Prefer using GetDefaultCellFitMode() directly in the new code.
    */
    bool GetDefaultCellOverflow() const;

    /**
        Returns the default height for column labels.
    */
    int GetDefaultColLabelSize() const;

    /**
        Returns the current default width for grid columns.
    */
    int GetDefaultColSize() const;

    /**
        Returns the default width for the row labels.
    */
    int GetDefaultRowLabelSize() const;

    /**
        Returns the current default height for grid rows.
    */
    int GetDefaultRowSize() const;

    /**
        Returns the minimal size to which rows can be resized.

        Use SetRowMinimalAcceptableHeight() to change this value globally or
        SetRowMinimalHeight() to do it for individual cells.

        @see GetColMinimalAcceptableWidth()
    */
    int GetRowMinimalAcceptableHeight() const;

    /**
        Returns the current width of the row labels.
    */
    int GetRowLabelSize() const;

    /**
        Returns the height of the specified row.
    */
    int GetRowSize(int row) const;

    /**
        Returns @true if the specified row is not currently hidden.
     */
    bool IsRowShown(int row) const;

    /**
        Specifies the behaviour of the cell contents if it doesn't fit into the
        available space.

        @see wxGridFitMode

        @since 3.1.4
     */
    void SetCellFitMode(int row, int col, wxGridFitMode fitMode);

    /**
        Sets the overflow permission of the cell.

        Prefer using SetCellFitMode() in the new code.
    */
    void SetCellOverflow(int row, int col, bool allow);

    /**
        Sets the height of the column labels.

        If @a height equals to @c wxGRID_AUTOSIZE then height is calculated
        automatically so that no label is truncated. Note that this could be
        slow for a large table.
    */
    void SetColLabelSize(int height);

    /**
        Sets the minimal @a width to which the user can resize columns.

        @see GetColMinimalAcceptableWidth()
    */
    void SetColMinimalAcceptableWidth(int width);

    /**
        Sets the minimal @a width for the specified column @a col.

        It is usually best to call this method during grid creation as calling
        it later will not resize the column to the given minimal width even if
        it is currently narrower than it.

        @a width must be greater than the minimal acceptable column width as
        returned by GetColMinimalAcceptableWidth().
    */
    void SetColMinimalWidth(int col, int width);

    /**
        Sets the width of the specified column.

        @param col
            The column index.
        @param width
            The new column width in pixels, 0 to hide the column or -1 to fit
            the column width to its label width.
    */
    void SetColSize(int col, int width);

    /**
        Hides the specified column.

        To show the column later you need to call SetColSize() with non-0
        width or ShowCol() to restore the previous column width.

        If the column is already hidden, this method doesn't do anything.

        @param col
            The column index.
     */
    void HideCol(int col);

    /**
        Shows the previously hidden column by resizing it to non-0 size.

        The column is shown again with the same width that it had before
        HideCol() call.

        If the column is currently shown, this method doesn't do anything.

        @see HideCol(), SetColSize()
     */
    void ShowCol(int col);


    /**
        Specifies the default behaviour of the cell contents if it doesn't fit
        into the available space.

        @see wxGridFitMode

        @since 3.1.4
     */
    void SetDefaultCellFitMode(wxGridFitMode fitMode);

    /**
        Sets the default overflow permission of the cells.

        Prefer using SetDefaultCellFitMode() in the new code.
    */
    void SetDefaultCellOverflow( bool allow );

    /**
        Sets the default width for columns in the grid.

        This will only affect columns subsequently added to the grid unless
        @a resizeExistingCols is @true.

        If @a width is less than GetColMinimalAcceptableWidth(), then the
        minimal acceptable width is used instead of it.
    */
    void SetDefaultColSize(int width, bool resizeExistingCols = false);

    /**
        Sets the default height for rows in the grid.

        This will only affect rows subsequently added to the grid unless
        @a resizeExistingRows is @true.

        If @a height is less than GetRowMinimalAcceptableHeight(), then the
        minimal acceptable height is used instead of it.
    */
    void SetDefaultRowSize(int height, bool resizeExistingRows = false);

    /**
        Sets the width of the row labels.

        If @a width equals @c wxGRID_AUTOSIZE then width is calculated
        automatically so that no label is truncated. Note that this could be
        slow for a large table.
    */
    void SetRowLabelSize(int width);

    /**
        Sets the minimal row @a height used by default.

        See SetColMinimalAcceptableWidth() for more information.
    */
    void SetRowMinimalAcceptableHeight(int height);

    /**
        Sets the minimal @a height for the specified @a row.

        See SetColMinimalWidth() for more information.
    */
    void SetRowMinimalHeight(int row, int height);

    /**
        Sets the height of the specified row.

        See SetColSize() for more information.
    */
    void SetRowSize(int row, int height);

    /**
        Hides the specified row.

        To show the row later you need to call SetRowSize() with non-0
        width or ShowRow() to restore its original height.

        If the row is already hidden, this method doesn't do anything.

        @param col
            The row index.
     */
    void HideRow(int col);

    /**
        Shows the previously hidden row.

        The row is shown again with the same height that it had before
        HideRow() call.

        If the row is currently shown, this method doesn't do anything.

        @see HideRow(), SetRowSize()
     */
    void ShowRow(int col);

    /**
        Get size information for all columns at once.

        This method is useful when the information about all column widths
        needs to be saved. The widths can be later restored using
        SetColSizes().

        @sa wxGridSizesInfo, GetRowSizes()
     */
    wxGridSizesInfo GetColSizes() const;

    /**
        Get size information for all row at once.

        @sa wxGridSizesInfo, GetColSizes()
     */
    wxGridSizesInfo GetRowSizes() const;

    /**
        Restore all columns sizes.

        This is usually called with wxGridSizesInfo object previously returned
        by GetColSizes().

        @sa SetRowSizes()
     */
    void SetColSizes(const wxGridSizesInfo& sizeInfo);

    /**
        Restore all rows sizes.

        @sa SetColSizes()
     */
    void SetRowSizes(const wxGridSizesInfo& sizeInfo);

    /**
        Set the size of the cell.

        Specifying a value of more than 1 in @a num_rows or @a num_cols will
        make the cell at (@a row, @a col) span the block of the specified size,
        covering the other cells which would be normally shown in it. Passing 1
        for both arguments resets the cell to normal appearance.

        @see GetCellSize()

        @param row
            The row of the cell.
        @param col
            The column of the cell.
        @param num_rows
            Number of rows to be occupied by this cell, must be >= 1.
        @param num_cols
            Number of columns to be occupied by this cell, must be >= 1.
     */
    void SetCellSize(int row, int col, int num_rows, int num_cols);

    /**
        Get the size of the cell in number of cells covered by it.

        For normal cells, the function fills both @a num_rows and @a num_cols
        with 1 and returns CellSpan_None. For cells which span multiple cells, i.e.
        for which SetCellSize() had been called, the returned values are the
        same ones as were passed to SetCellSize() call and the function return
        value is CellSpan_Main.

        More unexpectedly, perhaps, the returned values may be @em negative for
        the cells which are inside a span covered by a cell occupying multiple
        rows or columns. They correspond to the offset of the main cell of the
        span from the cell passed to this functions and the function returns
        CellSpan_Inside value to indicate this.

        As an example, consider a 3*3 grid with the cell (1, 1) (the one in the
        middle) having a span of 2 rows and 2 columns, i.e. the grid looks like
        @code
            +----+----+----+
            |    |    |    |
            +----+----+----+
            |    |         |
            +----+         |
            |    |         |
            +----+----+----+
        @endcode
        Then the function returns 2 and 2 in @a num_rows and @a num_cols for
        the cell (1, 1) itself and -1 and -1 for the cell (2, 2) as well as -1
        and 0 for the cell (2, 1).

        @param row
            The row of the cell.
        @param col
            The column of the cell.
        @param num_rows
            Pointer to variable receiving the number of rows, must not be @NULL.
        @param num_cols
            Pointer to variable receiving the number of columns, must not be
            @NULL.
        @return
            The kind of this cell span (the return value is new in wxWidgets
            2.9.1, this function was void in previous wxWidgets versions).
     */
    CellSpan GetCellSize( int row, int col, int *num_rows, int *num_cols ) const;

    /**
        Get the number of rows and columns allocated for this cell.

        This overload doesn't return a CellSpan value but the values returned
        may still be negative, see GetCellSize(int, int, int *, int *) for
        details.
     */
    wxSize GetCellSize(const wxGridCellCoords& coords);

    //@}


    /**
        @name User-Resizing and Dragging

        Functions controlling various interactive mouse operations.

        By default, columns and rows can be resized by dragging the edges of
        their labels (this can be disabled using DisableDragColSize() and
        DisableDragRowSize() methods). And if grid line dragging is enabled with
        EnableDragGridSize() they can also be resized by dragging the right or
        bottom edge of the grid cells.

        Columns can also be moved to interactively change their order but this
        needs to be explicitly enabled with EnableDragColMove().
     */
    //@{

    /**
        Return @true if the dragging of cells is enabled or @false otherwise.
    */
    bool CanDragCell() const;

    /**
        Returns @true if columns can be moved by dragging with the mouse.

        Columns can be moved by dragging on their labels.
    */
    bool CanDragColMove() const;

    /**
        Returns @true if the given column can be resized by dragging with the
        mouse.

        This function returns @true if resizing the columns interactively is
        globally enabled, i.e. if DisableDragColSize() hadn't been called, and
        if this column wasn't explicitly marked as non-resizable with
        DisableColResize().
    */
    bool CanDragColSize(int col) const;

    /**
        Return @true if column edges inside the grid can be dragged to resize
        the rows.

        @see CanDragGridSize(), CanDragColSize()

        @since 3.1.4
     */
    bool CanDragGridColEdges() const;

    /**
        Return @true if row edges inside the grid can be dragged to resize the
        rows.

        @see CanDragGridSize(), CanDragRowSize()

        @since 3.1.4
     */
    bool CanDragGridRowEdges() const;

    /**
        Return @true if the dragging of grid lines to resize rows and columns
        is enabled or @false otherwise.
    */
    bool CanDragGridSize() const;

    /**
        Returns @true if the given row can be resized by dragging with the
        mouse.

        This is the same as CanDragColSize() but for rows.
    */
    bool CanDragRowSize(int row) const;

    /**
        Returns @true if columns can be hidden from the popup menu of the native header.

        @since 3.1.3
    */
    bool CanHideColumns() const;

    /**
        Disable interactive resizing of the specified column.

        This method allows one to disable resizing of an individual column in a
        grid where the columns are otherwise resizable (which is the case by
        default).

        Notice that currently there is no way to make some columns resizable in
        a grid where columns can't be resized by default as there doesn't seem
        to be any need for this in practice. There is also no way to make the
        column marked as fixed using this method resizable again because it is
        supposed that fixed columns are used for static parts of the grid and
        so should remain fixed during the entire grid lifetime.

        Also notice that disabling interactive column resizing will not prevent
        the program from changing the column size.

        @see EnableDragColSize()
     */
    void DisableColResize(int col);

    /**
        Disable interactive resizing of the specified row.

        This is the same as DisableColResize() but for rows.

        @see EnableDragRowSize()
     */
    void DisableRowResize(int row);

    /**
        Disables column moving by dragging with the mouse.

        Equivalent to passing @false to EnableDragColMove().
    */
    void DisableDragColMove();

    /**
        Disables column sizing by dragging with the mouse.

        Equivalent to passing @false to EnableDragColSize().
    */
    void DisableDragColSize();

    /**
        Disable mouse dragging of grid lines to resize rows and columns.

        Equivalent to passing @false to EnableDragGridSize()
    */
    void DisableDragGridSize();

    /**
        Disables row sizing by dragging with the mouse.

        Equivalent to passing @false to EnableDragRowSize().
    */
    void DisableDragRowSize();

    /**
        Disables column hiding from the header popup menu.

        Equivalent to passing @false to EnableHidingColumns().

        @since 3.1.3
    */
    void DisableHidingColumns();

    /**
        Enables or disables cell dragging with the mouse.
    */
    void EnableDragCell(bool enable = true);

    /**
        Enables or disables column moving by dragging with the mouse.

        Note that reordering columns by dragging them is currently not
        supported when the grid has any frozen columns (see FreezeTo()) and if
        this method is called with @a enable equal to @true in this situation,
        it returns @false without doing anything. Otherwise it returns @true to
        indicate that it was successful.
    */
    bool EnableDragColMove(bool enable = true);

    /**
        Enables or disables column sizing by dragging with the mouse.

        @see DisableColResize()
    */
    void EnableDragColSize(bool enable = true);

    /**
        Enables or disables row and column resizing by dragging gridlines with
        the mouse.
    */
    void EnableDragGridSize(bool enable = true);

    /**
        Enables or disables row sizing by dragging with the mouse.

        @see DisableRowResize()
    */
    void EnableDragRowSize(bool enable = true);

    /**
        Enables or disables column hiding from the header popup menu.

        Note that currently the popup menu can only be shown when using
        wxHeaderCtrl, i.e. if UseNativeColHeader() had been called.

        If the native header is not used, this method always simply returns
        @false without doing anything, as hiding columns is not supported
        anyhow. If @a enable value is the same as CanHideColumns(), it also
        returns @false to indicate that nothing was done. Otherwise, it returns
        @true to indicate that the value of this option was successfully
        changed.

        The main use case for this method is to disallow hiding the columns
        interactively when using the native header.

        @since 3.1.3

        @see DisableHidingColumns()
    */
    bool EnableHidingColumns(bool enable = true);

    /**
        Returns the column ID of the specified column position.
    */
    int GetColAt(int colPos) const;

    /**
        Returns the position of the specified column.
    */
    int GetColPos(int colID) const;

    /**
        Sets the position of the specified column.
    */
    void SetColPos(int colID, int newPos);

    /**
        Sets the positions of all columns at once.

        This method takes an array containing the indices of the columns in
        their display order, i.e. uses the same convention as
        wxHeaderCtrl::SetColumnsOrder().
    */
    void SetColumnsOrder(const wxArrayInt& order);

    /**
        Resets the position of the columns to the default.
    */
    void ResetColPos();

    //@}


    /**
        @name Cursor Movement
    */
    //@{

    /**
        Returns the current grid cursor position.

        If grid cursor doesn't have any valid position (e.g. if the grid is
        completely empty and doesn't have any rows or columns), returns
        @c wxGridNoCellCoords which has both row and columns set to @c -1.

        @since 3.1.3
     */
    const wxGridCellCoords& GetGridCursorCoords() const;

    /**
        Returns the current grid cell column position.

        @see GetGridCursorCoords()
    */
    int GetGridCursorCol() const;

    /**
        Returns the current grid cell row position.

        @see GetGridCursorCoords()
    */
    int GetGridCursorRow() const;

    /**
        Make the given cell current and ensure it is visible.

        This method is equivalent to calling MakeCellVisible() and
        SetGridCursor() and so, as with the latter, a @c wxEVT_GRID_SELECT_CELL
        event is generated by it and the selected cell doesn't change if the
        event is vetoed.
     */
    void GoToCell(int row, int col);
    /**
        Make the given cell current and ensure it is visible.

        This method is equivalent to calling MakeCellVisible() and
        SetGridCursor() and so, as with the latter, a @c wxEVT_GRID_SELECT_CELL
        event is generated by it and the selected cell doesn't change if the
        event is vetoed.
     */
    void GoToCell(const wxGridCellCoords& coords);

    /**
        Moves the grid cursor down by one row.

        If a block of cells was previously selected it will expand if the
        argument is @true or be cleared if the argument is @false.
    */
    bool MoveCursorDown(bool expandSelection);

    /**
        Moves the grid cursor down in the current column such that it skips to
        the beginning or end of a block of non-empty cells.

        If a block of cells was previously selected it will expand if the
        argument is @true or be cleared if the argument is @false.
    */
    bool MoveCursorDownBlock(bool expandSelection);

    /**
        Moves the grid cursor left by one column.

        If a block of cells was previously selected it will expand if the
        argument is @true or be cleared if the argument is @false.
    */
    bool MoveCursorLeft(bool expandSelection);

    /**
        Moves the grid cursor left in the current row such that it skips to the
        beginning or end of a block of non-empty cells.

        If a block of cells was previously selected it will expand if the
        argument is @true or be cleared if the argument is @false.
    */
    bool MoveCursorLeftBlock(bool expandSelection);

    /**
        Moves the grid cursor right by one column.

        If a block of cells was previously selected it will expand if the
        argument is @true or be cleared if the argument is @false.
    */
    bool MoveCursorRight(bool expandSelection);

    /**
        Moves the grid cursor right in the current row such that it skips to
        the beginning or end of a block of non-empty cells.

        If a block of cells was previously selected it will expand if the
        argument is @true or be cleared if the argument is @false.
    */
    bool MoveCursorRightBlock(bool expandSelection);

    /**
        Moves the grid cursor up by one row.

        If a block of cells was previously selected it will expand if the
        argument is @true or be cleared if the argument is @false.
    */
    bool MoveCursorUp(bool expandSelection);

    /**
        Moves the grid cursor up in the current column such that it skips to
        the beginning or end of a block of non-empty cells.

        If a block of cells was previously selected it will expand if the
        argument is @true or be cleared if the argument is @false.
    */
    bool MoveCursorUpBlock(bool expandSelection);

    /**
        Moves the grid cursor down by some number of rows so that the previous
        bottom visible row becomes the top visible row.
    */
    bool MovePageDown();

    /**
        Moves the grid cursor up by some number of rows so that the previous
        top visible row becomes the bottom visible row.
    */
    bool MovePageUp();

    /**
        Set the grid cursor to the specified cell.

        The grid cursor indicates the current cell and can be moved by the user
        using the arrow keys or the mouse.

        Calling this function generates a @c wxEVT_GRID_SELECT_CELL event and
        if the event handler vetoes this event, the cursor is not moved.

        This function doesn't make the target call visible, use GoToCell() to
        do this.
    */
    void SetGridCursor(int row, int col);
    /**
        Set the grid cursor to the specified cell.

        The grid cursor indicates the current cell and can be moved by the user
        using the arrow keys or the mouse.

        Calling this function generates a @c wxEVT_GRID_SELECT_CELL event and
        if the event handler vetoes this event, the cursor is not moved.

        This function doesn't make the target call visible, use GoToCell() to
        do this.
    */
    void SetGridCursor(const wxGridCellCoords& coords);

    /**
        Set the grid's behaviour when the user presses the TAB key.

        Pressing the TAB key moves the grid cursor right in the current row, if
        there is a cell at the right and, similarly, Shift-TAB moves the cursor
        to the left in the current row if it's not in the first column.

        What happens if the cursor can't be moved because it it's already at
        the beginning or end of the row can be configured using this function,
        see wxGrid::TabBehaviour documentation for the detailed description.

        IF none of the standard behaviours is appropriate, you can always
        handle @c wxEVT_GRID_TABBING event directly to implement a custom
        TAB-handling logic.

        @since 2.9.5
    */
    void SetTabBehaviour(TabBehaviour behaviour);

    //@}


    /**
        @name User Selection
     */
    //@{

    /**
        Deselects all cells that are currently selected.
    */
    void ClearSelection();

    /**
        Deselects a row of cells.
    */
    void DeselectRow( int row );

    /**
        Deselects a column of cells.
    */
    void DeselectCol( int col );

    /**
        Deselects a cell.
    */
    void DeselectCell( int row, int col );

    /**
        Returns a range of grid selection blocks.

        The returned range can be iterated over, e.g. with C++11 range-for loop:
        @code
            for ( const auto block: grid->GetSelectedBlocks() ) {
                if ( block.Intersects(myBlock) )
                    break;
            }
        @endcode

        Notice that the blocks returned by this method are not ordered in any
        particular way and may overlap. For grids using rows or columns-only
        selection modes, GetSelectedRowBlocks() or GetSelectedColBlocks() can
        be more convenient, as they return ordered and non-overlapping blocks.

        @since 3.1.4
    */
    wxGridBlocks GetSelectedBlocks() const;

    /**
        Returns an ordered range of non-overlapping selected rows.

        For the grids using wxGridSelectRows selection mode, returns the
        possibly empty vector containing the coordinates of non-overlapping
        selected row blocks in the natural order, i.e. from smallest to the
        biggest row indices.

        To see the difference between this method and GetSelectedBlocks(),
        consider the case when the user selects rows 2..4 in the grid and then
        also selects (using Ctrl/Shift keys) the rows 1..3. Iterating over the
        result of GetSelectedBlocks() would yield two blocks directly
        corresponding to the users selection, while this method returns a
        vector with a single element corresponding to the rows 1..4.

        This method returns empty vector for the other selection modes.

        @see GetSelectedBlocks(), GetSelectedColBlocks()

        @since 3.1.4
     */
    wxGridBlockCoordsVector GetSelectedRowBlocks() const;

    /**
        Returns an ordered range of non-overlapping selected columns.

        This method is symmetric to GetSelectedRowBlocks(), but is useful only
        in wxGridSelectColumns selection mode.

        @see GetSelectedBlocks()

        @since 3.1.4
     */
    wxGridBlockCoordsVector GetSelectedColBlocks() const;

    /**
        Returns an array of individually selected cells.

        Notice that this array does @em not contain all the selected cells in
        general as it doesn't include the cells selected as part of column, row
        or block selection. You must use this method, GetSelectedCols(),
        GetSelectedRows() and GetSelectionBlockTopLeft() and
        GetSelectionBlockBottomRight() methods to obtain the entire selection
        in general.

        Please notice this behaviour is by design and is needed in order to
        support grids of arbitrary size (when an entire column is selected in
        a grid with a million of columns, we don't want to create an array with
        a million of entries in this function, instead it returns an empty
        array and GetSelectedCols() returns an array containing one element).

        The function can be slow for the big grids, use GetSelectedBlocks()
        in the new code.
    */
    wxGridCellCoordsArray GetSelectedCells() const;

    /**
        Returns an array of selected columns.

        Please notice that this method alone is not sufficient to find all the
        selected columns as it contains only the columns which were
        individually selected but not those being part of the block selection
        or being selected in virtue of all of their cells being selected
        individually, please see GetSelectedCells() for more details.

        The function can be slow for the big grids, use GetSelectedBlocks()
        in the new code.
    */
    wxArrayInt GetSelectedCols() const;

    /**
        Returns an array of selected rows.

        Please notice that this method alone is not sufficient to find all the
        selected rows as it contains only the rows which were individually
        selected but not those being part of the block selection or being
        selected in virtue of all of their cells being selected individually,
        please see GetSelectedCells() for more details.

        The function can be slow for the big grids, use GetSelectedBlocks()
        in the new code.
    */
    wxArrayInt GetSelectedRows() const;

    /**
        Returns the colour used for drawing the selection background.
    */
    wxColour GetSelectionBackground() const;

    /**
        Returns an array of the bottom right corners of blocks of selected
        cells.

        Please see GetSelectedCells() for more information about the selection
        representation in wxGrid.

        The function can be slow for the big grids, use GetSelectedBlocks()
        in the new code.

        @see GetSelectionBlockTopLeft()
    */
    wxGridCellCoordsArray GetSelectionBlockBottomRight() const;

    /**
        Returns an array of the top left corners of blocks of selected cells.

        Please see GetSelectedCells() for more information about the selection
        representation in wxGrid.

        The function can be slow for the big grids, use GetSelectedBlocks()
        in the new code.

        @see GetSelectionBlockBottomRight()
    */
    wxGridCellCoordsArray GetSelectionBlockTopLeft() const;

    /**
        Returns the colour used for drawing the selection foreground.
    */
    wxColour GetSelectionForeground() const;

    /**
        Returns the current selection mode.

        @see SetSelectionMode().
    */
    wxGridSelectionModes GetSelectionMode() const;

    /**
        Returns @true if the given cell is selected.
    */
    bool IsInSelection(int row, int col) const;
    /**
        Returns @true if the given cell is selected.
    */
    bool IsInSelection(const wxGridCellCoords& coords) const;

    /**
        Returns @true if there are currently any selected cells, rows, columns
        or blocks.
    */
    bool IsSelection() const;

    /**
        Selects all cells in the grid.
    */
    void SelectAll();

    /**
        Selects a rectangular block of cells.

        If @a addToSelected is @false then any existing selection will be
        deselected; if @true the column will be added to the existing
        selection.
    */
    void SelectBlock(int topRow, int leftCol, int bottomRow, int rightCol,
                     bool addToSelected = false);
    /**
        Selects a rectangular block of cells.

        If @a addToSelected is @false then any existing selection will be
        deselected; if @true the column will be added to the existing
        selection.
    */
    void SelectBlock(const wxGridCellCoords& topLeft,
                     const wxGridCellCoords& bottomRight,
                     bool addToSelected = false);

    /**
        Selects the specified column.

        If @a addToSelected is @false then any existing selection will be
        deselected; if @true the column will be added to the existing
        selection.

        This method won't select anything if the current selection mode is
        wxGridSelectRows.
    */
    void SelectCol(int col, bool addToSelected = false);

    /**
        Selects the specified row.

        If @a addToSelected is @false then any existing selection will be
        deselected; if @true the row will be added to the existing selection.

        This method won't select anything if the current selection mode is
        wxGridSelectColumns.
    */
    void SelectRow(int row, bool addToSelected = false);

    /**
        Set the colour to be used for drawing the selection background.
    */
    void SetSelectionBackground(const wxColour& c);

    /**
        Set the colour to be used for drawing the selection foreground.
    */
    void SetSelectionForeground(const wxColour& c);

    /**
        Set the selection behaviour of the grid.

        The existing selection is converted to conform to the new mode if
        possible and discarded otherwise (e.g. any individual selected cells
        are deselected if the new mode allows only the selection of the entire
        rows or columns).
    */
    void SetSelectionMode(wxGridSelectionModes selmode);

    //@}


    /**
        @name Scrolling
     */
    //@{

    /**
        Returns the number of pixels per horizontal scroll increment.

        The default is 15.

        @see GetScrollLineY(), SetScrollLineX(), SetScrollLineY()
    */
    int GetScrollLineX() const;

    /**
        Returns the number of pixels per vertical scroll increment.

        The default is 15.

        @see GetScrollLineX(), SetScrollLineX(), SetScrollLineY()
    */
    int GetScrollLineY() const;

    /**
        Returns @true if a cell is either entirely or at least partially
        visible in the grid window.

        By default, the cell must be entirely visible for this function to
        return @true but if @a wholeCellVisible is @false, the function returns
        @true even if the cell is only partially visible.
    */
    bool IsVisible(int row, int col, bool wholeCellVisible = true) const;
    /**
        Returns @true if a cell is either entirely or at least partially
        visible in the grid window.

        By default, the cell must be entirely visible for this function to
        return @true but if @a wholeCellVisible is @false, the function returns
        @true even if the cell is only partially visible.
    */
    bool IsVisible(const wxGridCellCoords& coords,
                   bool wholeCellVisible = true) const;

    /**
        Brings the specified cell into the visible grid cell area with minimal
        scrolling.

        Does nothing if the cell is already visible.
    */
    void MakeCellVisible(int row, int col);
    /**
        Brings the specified cell into the visible grid cell area with minimal
        scrolling.

        Does nothing if the cell is already visible.
    */
    void MakeCellVisible(const wxGridCellCoords& coords);

    /**
        Returns the topmost row of the current visible area.
        Returns -1 if the grid doesn't have any rows.
    */
    int GetFirstFullyVisibleRow() const;
    /**
        Returns the leftmost column of the current visible area.
        Returns -1 if the grid doesn't have any columns.
    */
   int GetFirstFullyVisibleColumn() const;

    /**
        Sets the number of pixels per horizontal scroll increment.

        The default is 15.

        @see GetScrollLineX(), GetScrollLineY(), SetScrollLineY()
    */
    void SetScrollLineX(int x);

    /**
        Sets the number of pixels per vertical scroll increment.

        The default is 15.

        @see GetScrollLineX(), GetScrollLineY(), SetScrollLineX()
    */
    void SetScrollLineY(int y);

    //@}


    /**
        @name Cell and Device Coordinate Translation
     */
    //@{

    /**
        Convert grid cell coordinates to grid window pixel coordinates.

        This function returns the rectangle that encloses the block of cells
        limited by @a topLeft and @a bottomRight cell in device coords and
        clipped to the client size of the grid window.

        @since 3.1.3 Parameter @a gridWindow has been added.

        @see CellToRect()
    */
    wxRect BlockToDeviceRect(const wxGridCellCoords& topLeft,
                             const wxGridCellCoords& bottomRight,
                             const wxGridWindow *gridWindow = NULL) const;

    /**
        Return the rectangle corresponding to the grid cell's size and position
        in logical coordinates.

        @see BlockToDeviceRect()
    */
    wxRect CellToRect(int row, int col) const;
    /**
        Return the rectangle corresponding to the grid cell's size and position
        in logical coordinates.

        @see BlockToDeviceRect()
    */
    wxRect CellToRect(const wxGridCellCoords& coords) const;

    /**
        Returns the grid window that contains the cell.

        In a grid without frozen rows or columns (see FreezeTo()), this will
        always return the same window as GetGridWindow(), however if some parts
        of the grid are frozen, this function returns the window containing the
        given cell.

        @since 3.1.3
     */
    wxGridWindow* CellToGridWindow( int row, int col ) const;

    /// @overload
    wxGridWindow* CellToGridWindow( const wxGridCellCoords& coords ) const;

    /**
        Returns the grid window that includes the input coordinates.

        @since 3.1.3
     */
    wxGridWindow* DevicePosToGridWindow(wxPoint pos) const;

    /// @overload
    wxGridWindow* DevicePosToGridWindow(int x, int y) const;

    /**
        Returns the grid window's offset from the grid starting position taking
        into account the frozen cells.

        If there are no frozen cells, returns (0, 0).

        @since 3.1.3

        @see FreezeTo()
     */
    void GetGridWindowOffset(const wxGridWindow *gridWindow, int &x, int &y) const;

    /// @overload
    wxPoint GetGridWindowOffset(const wxGridWindow *gridWindow) const;

    /**
        Translates the device coordinates to the logical ones, taking into
        account the grid window type.

        @since 3.1.3

        @see wxScrolled::CalcUnscrolledPosition()
     */
    void CalcGridWindowUnscrolledPosition(int x, int y,
                                          int *xx, int *yy,
                                          const wxGridWindow *gridWindow) const;
    /// @overload
    wxPoint CalcGridWindowUnscrolledPosition(const wxPoint& pt,
                                             const wxGridWindow *gridWindow) const;

    /**
        Translates the logical coordinates to the device ones, taking into
        account the grid window type.

        @since 3.1.3

        @see wxScrolled::CalcScrolledPosition()
     */
    void CalcGridWindowScrolledPosition(int x, int y,
                                        int *xx, int *yy,
                                        const wxGridWindow *gridWindow) const;

    /// @overload
    wxPoint CalcGridWindowScrolledPosition(const wxPoint& pt,
                                           const wxGridWindow *gridWindow) const;

    /**
        Returns the column at the given pixel position depending on the window.

        @param x
            The x position to evaluate.
        @param clipToMinMax
            If @true, rather than returning @c wxNOT_FOUND, it returns either
            the first or last column depending on whether @a x is too far to
            the left or right respectively.
        @param gridWindow
            The associated grid window that limits the search (note that this
            parameter is only available since wxWidgets 3.1.3).
            If @a gridWindow is @NULL, it will consider all the cells, no matter
            which grid they belong to.
        @return
            The column index or @c wxNOT_FOUND.
    */
    int XToCol(int x, bool clipToMinMax = false, wxGridWindow *gridWindow = NULL) const;

    /**
        Returns the column whose right hand edge is close to the given logical
        @a x position.

        If no column edge is near to this position @c wxNOT_FOUND is returned.
    */
    int XToEdgeOfCol(int x) const;

    /**
        Translates logical pixel coordinates to the grid cell coordinates.

        Notice that this function expects logical coordinates on input so if
        you use this function in a mouse event handler you need to translate
        the mouse position, which is expressed in device coordinates, to
        logical ones.

        The parameter @a gridWindow is new since wxWidgets 3.1.3. If it is
        specified, i.e. non-@NULL, the coordinates must be in this window
        coordinate system and only the cells of this window are considered,
        i.e. the function returns @c wxNOT_FOUND if the coordinates are out of
        bounds.

        If @a gridWindow is @NULL, coordinates are relative to the main grid
        window and all cells are considered.

        @see XToCol(), YToRow()
     */
    wxGridCellCoords XYToCell(int x, int y, wxGridWindow *gridWindow = NULL) const;

    /// @overload
    wxGridCellCoords XYToCell(const wxPoint& pos, wxGridWindow *gridWindow = NULL) const;

    // XYToCell(int, int, wxGridCellCoords&) overload is intentionally
    // undocumented, using it is ugly and non-const reference parameters are
    // not used in wxWidgets API

    /**
        Returns the row whose bottom edge is close to the given logical @a y
        position.

        If no row edge is near to this position @c wxNOT_FOUND is returned.
    */
    int YToEdgeOfRow(int y) const;

    /**
        Returns the grid row that corresponds to the logical @a y coordinate.


        The parameter @a gridWindow is new since wxWidgets 3.1.3. If it is
        specified, i.e. non-@NULL, only the cells of this window are
        considered, i.e. the function returns @c wxNOT_FOUND if @a y is out of
        bounds.

        If @a gridWindow is @NULL, the function returns @c wxNOT_FOUND only if
        there is no row at all at the @a y position.
    */
    int YToRow(int y, bool clipToMinMax = false, wxGridWindow *gridWindow = NULL) const;

    //@}


    /**
        @name Miscellaneous Functions
     */
    //@{

    /**
        Appends one or more new columns to the right of the grid.

        The @a updateLabels argument is not used at present. If you are using a
        derived grid table class you will need to override
        wxGridTableBase::AppendCols(). See InsertCols() for further
        information.

        @return @true on success or @false if appending columns failed.
    */
    bool AppendCols(int numCols = 1, bool updateLabels = true);

    /**
        Appends one or more new rows to the bottom of the grid.

        The @a updateLabels argument is not used at present. If you are using a
        derived grid table class you will need to override
        wxGridTableBase::AppendRows(). See InsertRows() for further
        information.

        @return @true on success or @false if appending rows failed.
    */
    bool AppendRows(int numRows = 1, bool updateLabels = true);

    /**
        Return @true if the horizontal grid lines stop at the last column
        boundary or @false if they continue to the end of the window.

        The default is to clip grid lines.

        @see ClipHorzGridLines(), AreVertGridLinesClipped()
     */
    bool AreHorzGridLinesClipped() const;

    /**
        Return @true if the vertical grid lines stop at the last row
        boundary or @false if they continue to the end of the window.

        The default is to clip grid lines.

        @see ClipVertGridLines(), AreHorzGridLinesClipped()
     */
    bool AreVertGridLinesClipped() const;

    /**
        Increments the grid's batch count.

        When the count is greater than zero repainting of the grid is
        suppressed. Each call to BeginBatch must be matched by a later call to
        EndBatch(). Code that does a lot of grid modification can be enclosed
        between BeginBatch() and EndBatch() calls to avoid screen flicker. The
        final EndBatch() call will cause the grid to be repainted.

        Notice that you should use wxGridUpdateLocker which ensures that there
        is always a matching EndBatch() call for this BeginBatch() if possible
        instead of calling this method directly.
    */
    void BeginBatch();

    /**
        Clears all data in the underlying grid table and repaints the grid.

        The table is not deleted by this function. If you are using a derived
        table class then you need to override wxGridTableBase::Clear() for this
        function to have any effect.
    */
    void ClearGrid();

    /**
        Change whether the horizontal grid lines are clipped by the end of the
        last column.

        By default the grid lines are not drawn beyond the end of the last
        column but after calling this function with @a clip set to @false they
        will be drawn across the entire grid window.

        @see AreHorzGridLinesClipped(), ClipVertGridLines()
     */
    void ClipHorzGridLines(bool clip);

    /**
        Change whether the vertical grid lines are clipped by the end of the
        last row.

        By default the grid lines are not drawn beyond the end of the last
        row but after calling this function with @a clip set to @false they
        will be drawn across the entire grid window.

        @see AreVertGridLinesClipped(), ClipHorzGridLines()
     */
    void ClipVertGridLines(bool clip);

    /**
        Deletes one or more columns from a grid starting at the specified
        position.

        The @a updateLabels argument is not used at present. If you are using a
        derived grid table class you will need to override
        wxGridTableBase::DeleteCols(). See InsertCols() for further
        information.

        @return @true on success or @false if deleting columns failed.
    */
    bool DeleteCols(int pos = 0, int numCols = 1, bool updateLabels = true);

    /**
        Deletes one or more rows from a grid starting at the specified
        position.

        The @a updateLabels argument is not used at present. If you are using a
        derived grid table class you will need to override
        wxGridTableBase::DeleteRows(). See InsertRows() for further
        information.

        @return @true on success or @false if deleting rows failed.
    */
    bool DeleteRows(int pos = 0, int numRows = 1, bool updateLabels = true);

    /**
        Sets or resets the frozen columns and rows.

        @param row
            The number of rows to freeze, 0 means to unfreeze all rows.
        @param col
            The number of columns to freeze, 0 means to unfreeze all columns.
        @return @true on success or @false if it failed.

        Note that this method doesn't do anything, and returns @false, if any
        of the following conditions are true:
        - Either @a row or @a col are out of range
        - Size of the frozen area would be bigger than the current viewing area
        - There are any merged cells in the area to be frozen
        - Grid uses a native header control (see UseNativeColHeader())

        (some of these limitations could be lifted in the future).

        @since 3.1.3
     */
    bool FreezeTo(unsigned row, unsigned col);

    /// @overload
    bool FreezeTo(const wxGridCellCoords& coords);

    /**
        Decrements the grid's batch count.

        When the count is greater than zero repainting of the grid is
        suppressed. Each previous call to BeginBatch() must be matched by a
        later call to EndBatch(). Code that does a lot of grid modification can
        be enclosed between BeginBatch() and EndBatch() calls to avoid screen
        flicker. The final EndBatch() will cause the grid to be repainted.

        @see wxGridUpdateLocker
    */
    void EndBatch();

    /**
        Overridden wxWindow method.
    */
    virtual void Fit();

    /**
        Causes immediate repainting of the grid.

        Use this instead of the usual wxWindow::Refresh().
    */
    void ForceRefresh();

    /**
        Returns the number of times that BeginBatch() has been called without
        (yet) matching calls to EndBatch(). While the grid's batch count is
        greater than zero the display will not be updated.
    */
    int GetBatchCount() const;

    /**
        Returns the total number of grid columns.

        This is the same as the number of columns in the underlying grid table.
    */
    int GetNumberCols() const;

    /**
        Returns the total number of grid rows.

        This is the same as the number of rows in the underlying grid table.
    */
    int GetNumberRows() const;

    /**
        Returns the number of frozen grid columns.

        If there are no frozen columns, returns 0.

        @since 3.1.3

        @see FreezeTo()
     */
    int GetNumberFrozenCols() const;

    /**
        Returns the number of frozen grid rows.

        If there are no frozen rows, returns 0.

        @since 3.1.3

        @see FreezeTo()
     */
    int GetNumberFrozenRows() const;

    /**
        Returns the attribute for the given cell creating one if necessary.

        If the cell already has an attribute, it is returned. Otherwise a new
        attribute is created, associated with the cell and returned. In any
        case the caller must call DecRef() on the returned pointer.

        Prefer to use GetOrCreateCellAttrPtr() to avoid the need to call
        DecRef() on the returned pointer.

        This function may only be called if CanHaveAttributes() returns @true.
    */
    wxGridCellAttr *GetOrCreateCellAttr(int row, int col) const;

    /**
        Returns the attribute for the given cell creating one if necessary.

        This method is identical to GetOrCreateCellAttr(), but returns a smart
        pointer, which frees the caller from the need to call DecRef()
        manually.

        @since 3.1.4
     */
    wxGridCellAttrPtr GetOrCreateCellAttrPtr(int row, int col) const;

    /**
        Returns a base pointer to the current table object.

        The returned pointer is still owned by the grid.
    */
    wxGridTableBase *GetTable() const;

    /**
        Inserts one or more new columns into a grid with the first new column
        at the specified position.

        Notice that inserting the columns in the grid requires grid table
        cooperation: when this method is called, grid object begins by
        requesting the underlying grid table to insert new columns. If this is
        successful the table notifies the grid and the grid updates the
        display. For a default grid (one where you have called CreateGrid())
        this process is automatic. If you are using a custom grid table
        (specified with SetTable() or AssignTable()) then you must override
        wxGridTableBase::InsertCols() in your derived table class.

        @param pos
            The position which the first newly inserted column will have.
        @param numCols
            The number of columns to insert.
        @param updateLabels
            Currently not used.
        @return
            @true if the columns were successfully inserted, @false if an error
            occurred (most likely the table couldn't be updated).
    */
    bool InsertCols(int pos = 0, int numCols = 1, bool updateLabels = true);

    /**
        Inserts one or more new rows into a grid with the first new row at the
        specified position.

        Notice that you must implement wxGridTableBase::InsertRows() if you use
        a grid with a custom table, please see InsertCols() for more
        information.

        @param pos
            The position which the first newly inserted row will have.
        @param numRows
            The number of rows to insert.
        @param updateLabels
            Currently not used.
        @return
            @true if the rows were successfully inserted, @false if an error
            occurred (most likely the table couldn't be updated).
    */
    bool InsertRows(int pos = 0, int numRows = 1, bool updateLabels = true);

    /**
        Invalidates the cached attribute for the given cell.

        For efficiency reasons, wxGrid may cache the recently used attributes
        (currently it caches only the single most recently used one, in fact)
        which can result in the cell appearance not being refreshed even when
        the attribute returned by your custom wxGridCellAttrProvider-derived
        class has changed. To force the grid to refresh the cell attribute,
        this function may be used. Notice that calling it will not result in
        actually redrawing the cell, you still need to call
        wxWindow::RefreshRect() to invalidate the area occupied by the cell in
        the window to do this. Also note that you don't need to call this
        function if you store the attributes in wxGrid itself, i.e. use its
        SetAttr() and similar methods, it is only useful when using a separate
        custom attributes provider.

        @param row
            The row of the cell whose attribute needs to be queried again.
        @param col
            The column of the cell whose attribute needs to be queried again.

        @since 2.9.2
     */
    void RefreshAttr(int row, int col);

    /**
        Draws part or all of a wxGrid on a wxDC for printing or display.

        Pagination can be accomplished by using sequential Render() calls
        with appropriate values in wxGridCellCoords topLeft and bottomRight.

        @param dc
            The wxDC to be drawn on.
        @param pos
            The position on the wxDC where rendering should begin. If not
            specified drawing will begin at the wxDC MaxX() and MaxY().
        @param size
            The size of the area on the wxDC that the rendered wxGrid should
            occupy. If not specified the drawing will be scaled to fit the
            available dc width or height. The wxGrid's aspect ratio is
            maintained whether or not size is specified.
        @param topLeft
            The top left cell of the block to be drawn. Defaults to ( 0, 0 ).
        @param bottomRight
            The bottom right cell of the block to be drawn. Defaults to row and
            column counts.
        @param style
            A combination of values from wxGridRenderStyle.

        @since 2.9.4
     */
    void Render( wxDC& dc,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 const wxGridCellCoords& topLeft = wxGridCellCoords( -1, -1 ),
                 const wxGridCellCoords& bottomRight = wxGridCellCoords( -1, -1 ),
                 int style = wxGRID_DRAW_DEFAULT );

    /**
        Sets the cell attributes for the specified cell.

        The grid takes ownership of the attribute pointer.

        See the wxGridCellAttr class for more information about controlling
        cell attributes.
    */
    void SetAttr(int row, int col, wxGridCellAttr *attr);

    /**
        Sets the cell attributes for all cells in the specified column.

        For more information about controlling grid cell attributes see the
        wxGridCellAttr cell attribute class and the @ref overview_grid.
    */
    void SetColAttr(int col, wxGridCellAttr* attr);

    /**
        Sets the extra margins used around the grid area.

        A grid may occupy more space than needed for its data display and
        this function allows setting how big this extra space is
    */
    void SetMargins(int extraWidth, int extraHeight);

    /**
        Sets the cell attributes for all cells in the specified row.

        The grid takes ownership of the attribute pointer.

        See the wxGridCellAttr class for more information about controlling
        cell attributes.
    */
    void SetRowAttr(int row, wxGridCellAttr* attr);


    wxArrayInt CalcRowLabelsExposed( const wxRegion& reg,
                                     wxGridWindow *gridWindow = NULL) const;
    wxArrayInt CalcColLabelsExposed( const wxRegion& reg,
                                     wxGridWindow *gridWindow = NULL) const;
    wxGridCellCoordsArray CalcCellsExposed( const wxRegion& reg,
                                            wxGridWindow *gridWindow = NULL) const;

    //@}


    /**
        @name Sorting support.

        wxGrid doesn't provide any support for sorting the data but it does
        generate events allowing the user code to sort it and supports
        displaying the sort indicator in the column used for sorting.

        To use wxGrid sorting support you need to handle wxEVT_GRID_COL_SORT
        event (and not veto it) and resort the data displayed in the grid. The
        grid will automatically update the sorting indicator on the column
        which was clicked.

        You can also call the functions in this section directly to update the
        sorting indicator. Once again, they don't do anything with the grid
        data, it remains your responsibility to actually sort it appropriately.
     */
    //@{

    /**
        Return the column in which the sorting indicator is currently
        displayed.

        Returns @c wxNOT_FOUND if sorting indicator is not currently displayed
        at all.

        @see SetSortingColumn()
     */
    int GetSortingColumn() const;

    /**
        Return @true if this column is currently used for sorting.

        @see GetSortingColumn()
     */
    bool IsSortingBy(int col) const;

    /**
        Return @true if the current sorting order is ascending or @false if it
        is descending.

        It only makes sense to call this function if GetSortingColumn() returns
        a valid column index and not @c wxNOT_FOUND.

        @see SetSortingColumn()
     */
    bool IsSortOrderAscending() const;

    /**
        Set the column to display the sorting indicator in and its direction.

        @param col
            The column to display the sorting indicator in or @c wxNOT_FOUND to
            remove any currently displayed sorting indicator.
        @param ascending
            If @true, display the ascending sort indicator, otherwise display
            the descending sort indicator.

        @see GetSortingColumn(), IsSortOrderAscending()
     */
    void SetSortingColumn(int col, bool ascending = true);

    /**
        Remove any currently shown sorting indicator.

        This is equivalent to calling SetSortingColumn() with @c wxNOT_FOUND
        first argument.
     */
    void UnsetSortingColumn();
    //@}


    /**
        @name Accessors for component windows.

        Return the various child windows of wxGrid.

        wxGrid is an empty parent window for at least 4 children representing
        the column labels window (top), the row labels window (left), the
        corner window (top left) and the main grid window. It may be necessary
        to use these individual windows and not the wxGrid window itself if you
        need to handle events for them (using wxEvtHandler::Bind()) or do
        something else requiring the use of the correct window pointer. Notice
        that you should not, however, change these windows (e.g. reposition
        them or draw over them) because they are managed by wxGrid itself.

        When parts of the grid are frozen using FreezeTo() function, the main
        grid window contains only the unfrozen part and additional windows are
        used for the parts containing frozen rows and/or columns and the corner
        window if both some rows and some columns are frozen.
     */
    //@{

    /**
        Return the main grid window containing the grid cells.

        This window is always shown.
     */
    wxWindow *GetGridWindow() const;

    /**
        Return the corner grid window containing frozen cells.

        This window is shown only when there are frozen rows and columns.

        @since 3.1.3
     */
    wxWindow* GetFrozenCornerGridWindow() const;

    /**
        Return the rows grid window containing row frozen cells.

        This window is shown only when there are frozen rows.

        @since 3.1.3
     */
    wxWindow* GetFrozenRowGridWindow() const;

    /**
        Return the columns grid window containing column frozen cells.

        This window is shown only when there are frozen columns.

        @since 3.1.3
     */
    wxWindow* GetFrozenColGridWindow() const;

    /**
        Return the row labels window.

        This window is not shown if the row labels were hidden using
        HideRowLabels().
     */
    wxWindow *GetGridRowLabelWindow() const;

    /**
        Return the column labels window.

        This window is not shown if the columns labels were hidden using
        HideColLabels().

        Depending on whether UseNativeColHeader() was called or not this can be
        either a wxHeaderCtrl or a plain wxWindow.  This function returns a valid
        window pointer in either case but in the former case you can also use
        GetGridColHeader() to access it if you need wxHeaderCtrl-specific
        functionality.
     */
    wxWindow *GetGridColLabelWindow() const;

    /**
        Return the window in the top left grid corner.

        This window is shown only of both columns and row labels are shown and
        normally doesn't contain anything. Clicking on it is handled by wxGrid
        however and can be used to select the entire grid.
     */
    wxWindow *GetGridCornerLabelWindow() const;

    /**
        Return the header control used for column labels display.

        This function can only be called if UseNativeColHeader() had been
        called.

        @see IsUsingNativeHeader()
     */
    wxHeaderCtrl *GetGridColHeader() const;

    /**
        Return true if native header control is currently being used.

        @since 3.1.4
     */
    bool IsUsingNativeHeader() const;

    //@}


    virtual void DrawCellHighlight( wxDC& dc, const wxGridCellAttr *attr );

    virtual void DrawRowLabels( wxDC& dc, const wxArrayInt& rows );
    virtual void DrawRowLabel( wxDC& dc, int row );

    virtual void DrawColLabels( wxDC& dc, const wxArrayInt& cols );
    virtual void DrawColLabel( wxDC& dc, int col );

    virtual void DrawCornerLabel(wxDC& dc);

    void DrawTextRectangle( wxDC& dc, const wxString& text, const wxRect& rect,
                            int horizontalAlignment = wxALIGN_LEFT,
                            int verticalAlignment = wxALIGN_TOP,
                            int textOrientation = wxHORIZONTAL ) const;

    void DrawTextRectangle( wxDC& dc, const wxArrayString& lines, const wxRect& rect,
                            int horizontalAlignment = wxALIGN_LEFT,
                            int verticalAlignment = wxALIGN_TOP,
                            int textOrientation = wxHORIZONTAL ) const;

    wxColour GetCellHighlightColour() const;
    int      GetCellHighlightPenWidth() const;
    int      GetCellHighlightROPenWidth() const;

    void SetCellHighlightColour( const wxColour& );
    void SetCellHighlightPenWidth(int width);
    void SetCellHighlightROPenWidth(int width);
    void SetGridFrozenBorderColour( const wxColour& );
    void SetGridFrozenBorderPenWidth( int width );


protected:
    /**
        Returns @true if this grid has support for cell attributes.

        The grid supports attributes if it has the associated table which, in
        turn, has attributes support, i.e. wxGridTableBase::CanHaveAttributes()
        returns @true.
    */
    bool CanHaveAttributes() const;

    /**
        Get the minimal width of the given column/row.

        The value returned by this function may be different from that returned
        by GetColMinimalAcceptableWidth() if SetColMinimalWidth() had been
        called for this column.
    */
    int GetColMinimalWidth(int col) const;

    /**
        Returns the coordinate of the right border specified column.
    */
    int GetColRight(int col) const;

    /**
        Returns the coordinate of the left border specified column.
    */
    int GetColLeft(int col) const;

    /**
        Returns the minimal size for the given column.

        The value returned by this function may be different from that returned
        by GetRowMinimalAcceptableHeight() if SetRowMinimalHeight() had been
        called for this row.
    */
    int GetRowMinimalHeight(int col) const;
};



/**
    @class wxGridUpdateLocker

    This small class can be used to prevent wxGrid from redrawing during its
    lifetime by calling wxGrid::BeginBatch() in its constructor and
    wxGrid::EndBatch() in its destructor. It is typically used in a function
    performing several operations with a grid which would otherwise result in
    flicker. For example:

    @code
    void MyFrame::Foo()
    {
        m_grid = new wxGrid(this, ...);

        wxGridUpdateLocker noUpdates(m_grid);
        m_grid-AppendColumn();
        // ... many other operations with m_grid ...
        m_grid-AppendRow();

        // destructor called, grid refreshed
    }
    @endcode

    Using this class is easier and safer than calling wxGrid::BeginBatch() and
    wxGrid::EndBatch() because you don't risk missing the call the latter (due
    to an exception for example).

    @library{wxcore}
    @category{grid}
*/
class wxGridUpdateLocker
{
public:
    /**
        Creates an object preventing the updates of the specified @a grid. The
        parameter could be @NULL in which case nothing is done. If @a grid is
        non-@NULL then the grid must exist for longer than this
        wxGridUpdateLocker object itself.

        The default constructor could be followed by a call to Create() to set
        the grid object later.
    */
    wxGridUpdateLocker(wxGrid* grid = NULL);

    /**
        Destructor reenables updates for the grid this object is associated
        with.
    */
    ~wxGridUpdateLocker();

    /**
        This method can be called if the object had been constructed using the
        default constructor. It must not be called more than once.
    */
    void Create(wxGrid* grid);
};



/**
    @class wxGridEvent

    This event class contains information about various grid events.

    Notice that all grid event table macros are available in two versions:
    @c EVT_GRID_XXX and @c EVT_GRID_CMD_XXX. The only difference between the
    two is that the former doesn't allow to specify the grid window identifier
    and so takes a single parameter, the event handler, but is not suitable if
    there is more than one grid control in the window where the event table is
    used (as it would catch the events from all the grids). The version with @c
    CMD takes the id as first argument and the event handler as the second one
    and so can be used with multiple grids as well. Otherwise there are no
    difference between the two and only the versions without the id are
    documented below for brevity.

    @beginEventTable{wxGridEvent}
    @event{EVT_GRID_CELL_CHANGING(func)}
        The user is about to change the data in a cell. The new cell value as
        string is available from GetString() event object method. This event
        can be vetoed if the change is not allowed.
        Processes a @c wxEVT_GRID_CELL_CHANGING event type.
    @event{EVT_GRID_CELL_CHANGED(func)}
        The user changed the data in a cell. The old cell value as string is
        available from GetString() event object method. Notice that vetoing
        this event still works for backwards compatibility reasons but any new
        code should only veto EVT_GRID_CELL_CHANGING event and not this one.
        Processes a @c wxEVT_GRID_CELL_CHANGED event type.
    @event{EVT_GRID_CELL_LEFT_CLICK(func)}
        The user clicked a cell with the left mouse button. Processes a
        @c wxEVT_GRID_CELL_LEFT_CLICK event type.
    @event{EVT_GRID_CELL_LEFT_DCLICK(func)}
        The user double-clicked a cell with the left mouse button. Processes a
        @c wxEVT_GRID_CELL_LEFT_DCLICK event type.
    @event{EVT_GRID_CELL_RIGHT_CLICK(func)}
        The user clicked a cell with the right mouse button. Processes a
        @c wxEVT_GRID_CELL_RIGHT_CLICK event type.
    @event{EVT_GRID_CELL_RIGHT_DCLICK(func)}
        The user double-clicked a cell with the right mouse button. Processes a
        @c wxEVT_GRID_CELL_RIGHT_DCLICK event type.
    @event{EVT_GRID_EDITOR_HIDDEN(func)}
        The editor for a cell was hidden. Processes a
        @c wxEVT_GRID_EDITOR_HIDDEN event type.
    @event{EVT_GRID_EDITOR_SHOWN(func)}
        The editor for a cell was shown. Processes a
        @c wxEVT_GRID_EDITOR_SHOWN event type.
    @event{EVT_GRID_LABEL_LEFT_CLICK(func)}
        The user clicked a label with the left mouse button. Processes a
        @c wxEVT_GRID_LABEL_LEFT_CLICK event type.
    @event{EVT_GRID_LABEL_LEFT_DCLICK(func)}
        The user double-clicked a label with the left mouse button. Processes a
        @c wxEVT_GRID_LABEL_LEFT_DCLICK event type.
    @event{EVT_GRID_LABEL_RIGHT_CLICK(func)}
        The user clicked a label with the right mouse button. Processes a
        @c wxEVT_GRID_LABEL_RIGHT_CLICK event type.
    @event{EVT_GRID_LABEL_RIGHT_DCLICK(func)}
        The user double-clicked a label with the right mouse button. Processes
        a @c wxEVT_GRID_LABEL_RIGHT_DCLICK event type.
    @event{EVT_GRID_SELECT_CELL(func)}
        The given cell was made current, either by user or by the program via a
        call to SetGridCursor() or GoToCell(). Processes a
        @c wxEVT_GRID_SELECT_CELL event type.
    @event{EVT_GRID_COL_MOVE(func)}
        The user tries to change the order of the columns in the grid by
        dragging the column specified by GetCol(). This event can be vetoed to
        either prevent the user from reordering the column change completely
        (but notice that if you don't want to allow it at all, you simply
        shouldn't call wxGrid::EnableDragColMove() in the first place), vetoed
        but handled in some way in the handler, e.g. by really moving the
        column to the new position at the associated table level, or allowed to
        proceed in which case wxGrid::SetColPos() is used to reorder the
        columns display order without affecting the use of the column indices
        otherwise.
        This event macro corresponds to @c wxEVT_GRID_COL_MOVE event type.
    @event{EVT_GRID_COL_SORT(func)}
        This event is generated when a column is clicked by the user and its
        name is explained by the fact that the custom reaction to a click on a
        column is to sort the grid contents by this column. However the grid
        itself has no special support for sorting and it's up to the handler of
        this event to update the associated table. But if the event is handled
        (and not vetoed) the grid supposes that the table was indeed resorted
        and updates the column to indicate the new sort order and refreshes
        itself.
        This event macro corresponds to @c wxEVT_GRID_COL_SORT event type.
    @event{EVT_GRID_TABBING(func)}
        This event is generated when the user presses TAB or Shift-TAB in the
        grid. It can be used to customize the simple default TAB handling
        logic, e.g. to go to the next non-empty cell instead of just the next
        cell. See also wxGrid::SetTabBehaviour(). This event is new since
        wxWidgets 2.9.5.
    @endEventTable

    @library{wxcore}
    @category{grid,events}
*/
class wxGridEvent : public wxNotifyEvent
{
public:
    /**
        Default constructor.
    */
    wxGridEvent();
    /**
        Constructor for initializing all event attributes.
    */
    wxGridEvent(int id, wxEventType type, wxObject* obj,
                int row = -1, int col = -1, int x = -1, int y = -1,
                bool sel = true, const wxKeyboardState& kbd = wxKeyboardState());

    /**
        Returns @true if the Alt key was down at the time of the event.
    */
    bool AltDown() const;

    /**
        Returns @true if the Control key was down at the time of the event.
    */
    bool ControlDown() const;

    /**
        Column at which the event occurred.

        Notice that for a @c wxEVT_GRID_SELECT_CELL event this column is the
        column of the newly selected cell while the previously selected cell
        can be retrieved using wxGrid::GetGridCursorCol().
    */
    virtual int GetCol();

    /**
        Position in pixels at which the event occurred.
    */
    wxPoint GetPosition();

    /**
        Row at which the event occurred.

        Notice that for a @c wxEVT_GRID_SELECT_CELL event this row is the row
        of the newly selected cell while the previously selected cell can be
        retrieved using wxGrid::GetGridCursorRow().
    */
    virtual int GetRow();

    /**
        Returns @true if the Meta key was down at the time of the event.
    */
    bool MetaDown() const;

    /**
        Returns @true if the user is selecting grid cells, or @false if
        deselecting.
    */
    bool Selecting();

    /**
        Returns @true if the Shift key was down at the time of the event.
    */
    bool ShiftDown() const;
};



/**
    @class wxGridSizeEvent

    This event class contains information about a row/column resize event.

    @beginEventTable{wxGridSizeEvent}
    @event{EVT_GRID_CMD_COL_SIZE(id, func)}
        The user resized a column, corresponds to @c wxEVT_GRID_COL_SIZE event
        type.
    @event{EVT_GRID_CMD_ROW_SIZE(id, func)}
        The user resized a row, corresponds to @c wxEVT_GRID_ROW_SIZE event
        type.
    @event{EVT_GRID_COL_SIZE(func)}
        Same as EVT_GRID_CMD_COL_SIZE() but uses `wxID_ANY` id.
    @event{EVT_GRID_COL_AUTO_SIZE(func)}
        This event is sent when a column must be resized to its best size, e.g.
        when the user double clicks the column divider. The default
        implementation simply resizes the column to fit the column label (but
        not its contents as this could be too slow for big grids). This macro
        corresponds to @c wxEVT_GRID_COL_AUTO_SIZE event type and is new since
        wxWidgets 2.9.5.
    @event{EVT_GRID_ROW_SIZE(func)}
        Same as EVT_GRID_CMD_ROW_SIZE() but uses `wxID_ANY` id.
    @endEventTable

    @library{wxcore}
    @category{grid,events}
*/
class wxGridSizeEvent : public wxNotifyEvent
{
public:
    /**
        Default constructor.
    */
    wxGridSizeEvent();
    /**
        Constructor for initializing all event attributes.
    */
    wxGridSizeEvent(int id, wxEventType type, wxObject* obj,
                    int rowOrCol = -1, int x = -1, int y = -1,
                    const wxKeyboardState& kbd = wxKeyboardState());

    /**
        Returns @true if the Alt key was down at the time of the event.
    */
    bool AltDown() const;

    /**
        Returns @true if the Control key was down at the time of the event.
    */
    bool ControlDown() const;

    /**
        Position in pixels at which the event occurred.
    */
    wxPoint GetPosition();

    /**
        Row or column at that was resized.
    */
    int GetRowOrCol();

    /**
        Returns @true if the Meta key was down at the time of the event.
    */
    bool MetaDown() const;

    /**
        Returns @true if the Shift key was down at the time of the event.
    */
    bool ShiftDown() const;
};



/**
    @class wxGridRangeSelectEvent

    @beginEventTable{wxGridRangeSelectEvent}
    @event{EVT_GRID_RANGE_SELECT(func)}
        The user selected a group of contiguous cells. Processes a
        @c wxEVT_GRID_RANGE_SELECT event type.
    @event{EVT_GRID_CMD_RANGE_SELECT(id, func)}
        The user selected a group of contiguous cells; variant taking a window
        identifier. Processes a @c wxEVT_GRID_RANGE_SELECT event type.
    @endEventTable

    @library{wxcore}
    @category{grid,events}
*/
class wxGridRangeSelectEvent : public wxNotifyEvent
{
public:
    /**
        Default constructor.
    */
    wxGridRangeSelectEvent();
    /**
        Constructor for initializing all event attributes.
    */
    wxGridRangeSelectEvent(int id, wxEventType type,
                           wxObject* obj,
                           const wxGridCellCoords& topLeft,
                           const wxGridCellCoords& bottomRight,
                           bool sel = true, const wxKeyboardState& kbd = wxKeyboardState());

    /**
        Returns @true if the Alt key was down at the time of the event.
    */
    bool AltDown() const;

    /**
        Returns @true if the Control key was down at the time of the event.
    */
    bool ControlDown() const;

    /**
        Top left corner of the rectangular area that was (de)selected.
    */
    wxGridCellCoords GetBottomRightCoords();

    /**
        Bottom row of the rectangular area that was (de)selected.
    */
    int GetBottomRow();

    /**
        Left column of the rectangular area that was (de)selected.
    */
    int GetLeftCol();

    /**
        Right column of the rectangular area that was (de)selected.
    */
    int GetRightCol();

    /**
        Top left corner of the rectangular area that was (de)selected.
    */
    wxGridCellCoords GetTopLeftCoords();

    /**
        Top row of the rectangular area that was (de)selected.
    */
    int GetTopRow();

    /**
        Returns @true if the Meta key was down at the time of the event.
    */
    bool MetaDown() const;

    /**
        Returns @true if the area was selected, @false otherwise.
    */
    bool Selecting();

    /**
        Returns @true if the Shift key was down at the time of the event.
    */
    bool ShiftDown() const;
};



/**
    @class wxGridEditorCreatedEvent

    @beginEventTable{wxGridEditorCreatedEvent}
    @event{EVT_GRID_EDITOR_CREATED(func)}
        The editor for a cell was created. Processes a
        @c wxEVT_GRID_EDITOR_CREATED event type.
    @event{EVT_GRID_CMD_EDITOR_CREATED(id, func)}
        The editor for a cell was created; variant taking a window identifier.
        Processes a @c wxEVT_GRID_EDITOR_CREATED event type.
    @endEventTable

    @library{wxcore}
    @category{grid,events}
*/
class wxGridEditorCreatedEvent : public wxCommandEvent
{
public:
    /**
        Default constructor.
    */
    wxGridEditorCreatedEvent();
    /**
        Constructor for initializing all event attributes.
    */
    wxGridEditorCreatedEvent(int id, wxEventType type, wxObject* obj,
                             int row, int col, wxControl* ctrl);

    /**
        Returns the column at which the event occurred.
    */
    int GetCol();

    /**
        Returns the edit control.

        This function is preserved for compatibility, but GetWindow() should be
        preferred in the new code as the associated window doesn't need to be of
        a wxControl-derived class.

        Note that if SetWindow() had been called with an object not deriving
        from wxControl, this method will return @NULL.
    */
    wxControl* GetControl();

    /**
        Returns the row at which the event occurred.
    */
    int GetRow();

    /**
        Returns the edit window.

        @since 3.1.3
    */
    wxWindow* GetWindow();

    /**
        Sets the column at which the event occurred.
    */
    void SetCol(int col);

    /**
        Sets the edit control.

        This function is preserved for compatibility, but SetWindow() should be
        preferred in the new code, see GetControl().
    */
    void SetControl(wxControl* ctrl);

    /**
        Sets the row at which the event occurred.
    */
    void SetRow(int row);

    /**
        Sets the edit window.

        @since 3.1.3
    */
    void SetWindow(wxWindow* window);
};


wxEventType wxEVT_GRID_CELL_LEFT_CLICK;
wxEventType wxEVT_GRID_CELL_RIGHT_CLICK;
wxEventType wxEVT_GRID_CELL_LEFT_DCLICK;
wxEventType wxEVT_GRID_CELL_RIGHT_DCLICK;
wxEventType wxEVT_GRID_LABEL_LEFT_CLICK;
wxEventType wxEVT_GRID_LABEL_RIGHT_CLICK;
wxEventType wxEVT_GRID_LABEL_LEFT_DCLICK;
wxEventType wxEVT_GRID_LABEL_RIGHT_DCLICK;
wxEventType wxEVT_GRID_ROW_SIZE;
wxEventType wxEVT_GRID_COL_SIZE;
wxEventType wxEVT_GRID_COL_AUTO_SIZE;
wxEventType wxEVT_GRID_RANGE_SELECT;
wxEventType wxEVT_GRID_CELL_CHANGING;
wxEventType wxEVT_GRID_CELL_CHANGED;
wxEventType wxEVT_GRID_SELECT_CELL;
wxEventType wxEVT_GRID_EDITOR_SHOWN;
wxEventType wxEVT_GRID_EDITOR_HIDDEN;
wxEventType wxEVT_GRID_EDITOR_CREATED;
wxEventType wxEVT_GRID_CELL_BEGIN_DRAG;
wxEventType wxEVT_GRID_COL_MOVE;
wxEventType wxEVT_GRID_COL_SORT;
wxEventType wxEVT_GRID_TABBING;

