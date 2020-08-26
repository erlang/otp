/////////////////////////////////////////////////////////////////////////////
// Name:        richtext/richtextstyles.h
// Purpose:     interface of wxRichTextStyleListCtrl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxRichTextStyleListCtrl

    This class incorporates a wxRichTextStyleListBox and a choice control that
    allows the user to select the category of style to view.

    It is demonstrated in the wxRichTextCtrl sample in @c samples/richtext.

    To use wxRichTextStyleListCtrl, add the control to your window hierarchy and
    call wxRichTextStyleListCtrl::SetStyleType with one of
    wxRichTextStyleListBox::wxRICHTEXT_STYLE_ALL,
    wxRichTextStyleListBox::wxRICHTEXT_STYLE_PARAGRAPH,
    wxRichTextStyleListBox::wxRICHTEXT_STYLE_CHARACTER and
    wxRichTextStyleListBox::wxRICHTEXT_STYLE_LIST to set the current view.

    Associate the control with a style sheet and rich text control with
    SetStyleSheet and SetRichTextCtrl, so that when a style is double-clicked,
    it is applied to the selection.

    @beginStyleTable
    @style{wxRICHTEXTSTYLELIST_HIDE_TYPE_SELECTOR}
           This style hides the category selection control.
    @endStyleTable

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextStyleListCtrl : public wxControl
{
public:
    //@{
    /**
        Constructors.
    */
    wxRichTextStyleListCtrl(wxWindow* parent,
                            wxWindowID id = wxID_ANY,
                            const wxPoint& pos = wxDefaultPosition,
                            const wxSize& size = wxDefaultSize,
                            long style = 0);
    wxRichTextStyleListCtrl();
    //@}

    /**
        Creates the windows.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0);

    /**
        Returns the associated rich text control, if any.
    */
    wxRichTextCtrl* GetRichTextCtrl() const;

    /**
        Returns the wxChoice control used for selecting the style category.
    */
    wxChoice* GetStyleChoice() const;

    /**
        Returns the wxListBox control used to view the style list.
    */
    wxRichTextStyleListBox* GetStyleListBox() const;

    /**
        Returns the associated style sheet, if any.
    */
    wxRichTextStyleSheet* GetStyleSheet() const;

    /**
        Returns the type of style to show in the list box.
    */
    wxRichTextStyleListBox::wxRichTextStyleType GetStyleType() const;

    /**
        Associates the control with a wxRichTextCtrl.
    */
    void SetRichTextCtrl(wxRichTextCtrl* ctrl);

    /**
        Associates the control with a style sheet.
    */
    void SetStyleSheet(wxRichTextStyleSheet* styleSheet);

    /**
        Sets the style type to display.

        One of
        - wxRichTextStyleListBox::wxRICHTEXT_STYLE_ALL,
        - wxRichTextStyleListBox::wxRICHTEXT_STYLE_PARAGRAPH,
        - wxRichTextStyleListBox::wxRICHTEXT_STYLE_CHARACTER
        - wxRichTextStyleListBox::wxRICHTEXT_STYLE_LIST.
    */
    void SetStyleType(wxRichTextStyleListBox::wxRichTextStyleType styleType);

    /**
        Updates the style list box.
    */
    void UpdateStyles();
};



/**
    @class wxRichTextStyleDefinition

    This is a base class for paragraph and character styles.

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextStyleDefinition : public wxObject
{
public:
    /**
        Constructor.
    */
    wxRichTextStyleDefinition(const wxString& name = wxEmptyString);

    /**
        Destructor.
    */
    virtual ~wxRichTextStyleDefinition();

    /**
        Returns the style on which this style is based.
    */
    const wxString& GetBaseStyle() const;

    /**
        Returns the style's description.
    */
    const wxString& GetDescription() const;

    /**
        Returns the style name.
    */
    const wxString& GetName() const;

    //@{
    /**
        Returns the attributes associated with this style.
    */
    wxRichTextAttr GetStyle() const;
    const wxRichTextAttr GetStyle() const;
    //@}

    /**
        Returns the style attributes combined with the attributes of the specified base
        style, if any. This function works recursively.
    */
    virtual wxRichTextAttr GetStyleMergedWithBase(const wxRichTextStyleSheet* sheet) const;

    /**
        Sets the name of the style that this style is based on.
    */
    void SetBaseStyle(const wxString& name);

    /**
        Sets the style description.
    */
    void SetDescription(const wxString& descr);

    /**
        Sets the name of the style.
    */
    void SetName(const wxString& name);

    /**
        Sets the attributes for this style.
    */
    void SetStyle(const wxRichTextAttr& style);

    /**
        Returns the definition's properties.
    */
    wxRichTextProperties& GetProperties();

    /**
        Returns the definition's properties.
    */
    const wxRichTextProperties& GetProperties() const;

    /**
        Sets the definition's properties.
    */
    void SetProperties(const wxRichTextProperties& props);
};



/**
    @class wxRichTextParagraphStyleDefinition

    This class represents a paragraph style definition, usually added to a
    wxRichTextStyleSheet.

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextParagraphStyleDefinition : public wxRichTextStyleDefinition
{
public:
    /**
        Constructor.
    */
    wxRichTextParagraphStyleDefinition(const wxString& name = wxEmptyString);

    /**
        Destructor.
    */
    virtual ~wxRichTextParagraphStyleDefinition();

    /**
        Returns the style that should normally follow this style.
    */
    const wxString& GetNextStyle() const;

    /**
        Sets the style that should normally follow this style.
    */
    void SetNextStyle(const wxString& name);
};



/**
    @class wxRichTextStyleListBox

    This is a listbox that can display the styles in a wxRichTextStyleSheet,
    and apply the selection to an associated wxRichTextCtrl.

    See @c samples/richtext for an example of how to use it.

    @library{wxrichtext}
    @category{richtext}

    @see wxRichTextStyleComboCtrl, @ref overview_richtextctrl
*/
class wxRichTextStyleListBox : public wxHtmlListBox
{
public:

    /// Which type of style definition is currently showing?
    enum wxRichTextStyleType
    {
        wxRICHTEXT_STYLE_ALL,
        wxRICHTEXT_STYLE_PARAGRAPH,
        wxRICHTEXT_STYLE_CHARACTER,
        wxRICHTEXT_STYLE_LIST,
        wxRICHTEXT_STYLE_BOX
    };

    /**
        Constructor.
    */
    wxRichTextStyleListBox(wxWindow* parent,
                           wxWindowID id = wxID_ANY,
                           const wxPoint& pos = wxDefaultPosition,
                           const wxSize& size = wxDefaultSize,
                           long style = 0);
    wxRichTextStyleListBox();

    /**
        Destructor.
    */
    virtual ~wxRichTextStyleListBox();

    /**
        Creates the window.
    */
    bool Create(wxWindow* parent,
                wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0);

    /**
        Applies the @e ith style to the associated rich text control.
    */
    void ApplyStyle(int i);

    /**
        Converts units in tenths of a millimetre to device units.
    */
    int ConvertTenthsMMToPixels(wxDC& dc, int units) const;

    /**
        Creates a suitable HTML fragment for a definition.
    */
    wxString CreateHTML(wxRichTextStyleDefinition* def) const;

    /**
        If the return value is @true, clicking on a style name in the list will
        immediately apply the style to the associated rich text control.
    */
    bool GetApplyOnSelection() const;

    /**
        Returns the wxRichTextCtrl associated with this listbox.
    */
    wxRichTextCtrl* GetRichTextCtrl() const;

    /**
        Gets a style for a listbox index.
    */
    wxRichTextStyleDefinition* GetStyle(size_t i) const;

    /**
        Returns the style sheet associated with this listbox.
    */
    wxRichTextStyleSheet* GetStyleSheet() const;

    /**
        Returns the type of style to show in the list box.
    */
    wxRichTextStyleListBox::wxRichTextStyleType GetStyleType() const;

    /**
        Implements left click behaviour, applying the clicked style to the
        wxRichTextCtrl.
    */
    void OnLeftDown(wxMouseEvent& event);

    /**
        If @a applyOnSelection is @true, clicking on a style name in the list will
        immediately apply the style to the associated rich text control.
    */
    void SetApplyOnSelection(bool applyOnSelection);

    /**
        Associates the listbox with a wxRichTextCtrl.
    */
    void SetRichTextCtrl(wxRichTextCtrl* ctrl);

    /**
        Associates the control with a style sheet.
    */
    void SetStyleSheet(wxRichTextStyleSheet* styleSheet);

    /**
        Sets the style type to display. One of
        - wxRichTextStyleListBox::wxRICHTEXT_STYLE_ALL,
        - wxRichTextStyleListBox::wxRICHTEXT_STYLE_PARAGRAPH,
        - wxRichTextStyleListBox::wxRICHTEXT_STYLE_CHARACTER
        - wxRichTextStyleListBox::wxRICHTEXT_STYLE_LIST.
    */
    void SetStyleType(wxRichTextStyleListBox::wxRichTextStyleType styleType);

    /**
        Updates the list from the associated style sheet.
    */
    void UpdateStyles();

protected:

    /**
        Returns the HTML for this item.
    */
    virtual wxString OnGetItem(size_t n) const;
};



/**
    @class wxRichTextStyleComboCtrl

    This is a combo control that can display the styles in a wxRichTextStyleSheet,
    and apply the selection to an associated wxRichTextCtrl.

    See @c samples/richtext for an example of how to use it.

    @library{wxrichtext}
    @category{richtext}

    @see wxRichTextStyleListBox, @ref overview_richtextctrl
*/
class wxRichTextStyleComboCtrl : public wxComboCtrl
{
public:
    /**
        Constructor.
    */
    wxRichTextStyleComboCtrl(wxWindow* parent,
                             wxWindowID id = wxID_ANY,
                             const wxPoint& pos = wxDefaultPosition,
                             const wxSize& size = wxDefaultSize,
                             long style = 0);
    wxRichTextStyleComboCtrl();

    /**
        Destructor.
    */
    virtual ~wxRichTextStyleComboCtrl();

    /**
        Creates the windows.
    */
    bool Create(wxWindow* parent,
                wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0);

    /**
        Returns the wxRichTextCtrl associated with this control.
    */
    wxRichTextCtrl* GetRichTextCtrl() const;

    /**
        Returns the style sheet associated with this control.
    */
    wxRichTextStyleSheet* GetStyleSheet() const;

    /**
        Associates the control with a wxRichTextCtrl.
    */
    void SetRichTextCtrl(wxRichTextCtrl* ctrl);

    /**
        Associates the control with a style sheet.
    */
    void SetStyleSheet(wxRichTextStyleSheet* styleSheet);

    /**
        Updates the combo control from the associated style sheet.
    */
    void UpdateStyles();
};



/**
    @class wxRichTextCharacterStyleDefinition

    This class represents a character style definition, usually added to a
    wxRichTextStyleSheet.

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextCharacterStyleDefinition : public wxRichTextStyleDefinition
{
public:
    /**
        Constructor.
    */
    wxRichTextCharacterStyleDefinition(const wxString& name = wxEmptyString);

    /**
        Destructor.
    */
    virtual ~wxRichTextCharacterStyleDefinition();
};



/**
    @class wxRichTextListStyleDefinition

    This class represents a list style definition, usually added to a
    wxRichTextStyleSheet.

    The class inherits paragraph attributes from wxRichTextStyleParagraphDefinition,
    and adds 10 further attribute objects, one for each level of a list.
    When applying a list style to a paragraph, the list style's base and
    appropriate level attributes are merged with the paragraph's existing attributes.

    You can apply a list style to one or more paragraphs using wxRichTextCtrl::SetListStyle.
    You can also use the functions wxRichTextCtrl::NumberList, wxRichTextCtrl::PromoteList and
    wxRichTextCtrl::ClearListStyle.

    As usual, there are wxRichTextBuffer versions of these functions
    so that you can apply them directly to a buffer without requiring a control.

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextListStyleDefinition : public wxRichTextParagraphStyleDefinition
{
public:
    /**
        Constructor.
    */
    wxRichTextListStyleDefinition(const wxString& name = wxEmptyString);

    /**
        Destructor.
    */
    virtual ~wxRichTextListStyleDefinition();

    /**
        This function combines the given paragraph style with the list style's base
        attributes and level style matching the given indent, returning the combined attributes.

        If @a styleSheet is specified, the base style for this definition will also be
        included in the result.
    */
    wxRichTextAttr CombineWithParagraphStyle(int indent,
                                         const wxRichTextAttr& paraStyle,
                                         wxRichTextStyleSheet* styleSheet = NULL);

    /**
        This function finds the level (from 0 to 9) whose indentation attribute mostly
        closely matches @a indent (expressed in tenths of a millimetre).
    */
    int FindLevelForIndent(int indent) const;

    /**
        This function combines the list style's base attributes and the level style
        matching the given indent, returning the combined attributes.

        If @a styleSheet is specified, the base style for this definition will also be
        included in the result.
    */
    wxRichTextAttr GetCombinedStyle(int indent,
                                wxRichTextStyleSheet* styleSheet = NULL);

    /**
        This function combines the list style's base attributes and the style for the
        specified level, returning the combined attributes.

        If @a styleSheet is specified, the base style for this definition will also be
        included in the result.
    */

    wxRichTextAttr GetCombinedStyleForLevel(int level,
                                     wxRichTextStyleSheet* styleSheet = NULL);

    /**
        Returns the style for the given level. @a level is a number between 0 and 9.
    */
    const wxRichTextAttr* GetLevelAttributes(int level) const;

    /**
        Returns the number of levels. This is hard-wired to 10.
        Returns the style for the given level. @e level is a number between 0 and 9.
    */
    int GetLevelCount() const;

    /**
        Returns @true if the given level has numbered list attributes.
    */
    bool IsNumbered(int level) const;

    /**
        Sets the style for the given level. @a level is a number between 0 and 9.
        The first and most flexible form uses a wxTextAttr object, while the second
        form is for convenient setting of the most commonly-used attributes.
    */
    void SetLevelAttributes(int level, const wxRichTextAttr& attr);
};



/**
    @class wxRichTextStyleSheet

    A style sheet contains named paragraph and character styles that make it
    easy for a user to apply combinations of attributes to a wxRichTextCtrl.

    You can use a wxRichTextStyleListBox in your user interface to show available
    styles to the user, and allow application of styles to the control.

    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextStyleSheet : public wxObject
{
public:
    /**
        Constructor.
    */
    wxRichTextStyleSheet();

    /**
        Destructor.
    */
    virtual ~wxRichTextStyleSheet();

    /**
        Adds a definition to the character style list.
    */
    bool AddCharacterStyle(wxRichTextCharacterStyleDefinition* def);

    /**
        Adds a definition to the list style list.
    */
    bool AddListStyle(wxRichTextListStyleDefinition* def);

    /**
        Adds a definition to the paragraph style list.
    */
    bool AddParagraphStyle(wxRichTextParagraphStyleDefinition* def);

    /**
        Adds a definition to the appropriate style list.
    */
    bool AddStyle(wxRichTextStyleDefinition* def);

    /**
        Deletes all styles.
    */
    void DeleteStyles();

    /**
        Finds a character definition by name.
    */
    wxRichTextCharacterStyleDefinition* FindCharacterStyle(const wxString& name,
                                                           bool recurse = true) const;

    /**
        Finds a list definition by name.
    */
    wxRichTextListStyleDefinition* FindListStyle(const wxString& name,
                                                 bool recurse = true) const;

    /**
        Finds a paragraph definition by name.
    */
    wxRichTextParagraphStyleDefinition* FindParagraphStyle(const wxString& name,
                                                           bool recurse = true) const;

    /**
        Finds a style definition by name.
    */
    wxRichTextStyleDefinition* FindStyle(const wxString& name) const;

    /**
        Returns the @e nth character style.
    */
    wxRichTextCharacterStyleDefinition* GetCharacterStyle(size_t n) const;

    /**
        Returns the number of character styles.
    */
    size_t GetCharacterStyleCount() const;

    /**
        Returns the style sheet's description.
    */
    const wxString& GetDescription() const;

    /**
        Returns the @e nth list style.
    */
    wxRichTextListStyleDefinition* GetListStyle(size_t n) const;

    /**
        Returns the number of list styles.
    */
    size_t GetListStyleCount() const;

    /**
        Returns the style sheet's name.
    */
    const wxString& GetName() const;

    /**
        Returns the @e nth paragraph style.
    */
    wxRichTextParagraphStyleDefinition* GetParagraphStyle(size_t n) const;

    /**
        Returns the number of paragraph styles.
    */
    size_t GetParagraphStyleCount() const;

    /**
        Removes a character style.
    */
    bool RemoveCharacterStyle(wxRichTextStyleDefinition* def,
                              bool deleteStyle = false);

    /**
        Removes a list style.
    */
    bool RemoveListStyle(wxRichTextStyleDefinition* def,
                         bool deleteStyle = false);

    /**
        Removes a paragraph style.
    */
    bool RemoveParagraphStyle(wxRichTextStyleDefinition* def,
                              bool deleteStyle = false);

    /**
        Removes a style.
    */
    bool RemoveStyle(wxRichTextStyleDefinition* def,
                     bool deleteStyle = false);

    /**
        Sets the style sheet's description.
    */
    void SetDescription(const wxString& descr);

    /**
        Sets the style sheet's name.
    */
    void SetName(const wxString& name);

    /**
        Returns the sheet's properties.
    */
    wxRichTextProperties& GetProperties();

    /**
        Returns the sheet's properties.
    */
    const wxRichTextProperties& GetProperties() const;

    /**
        Sets the sheet's properties.
    */
    void SetProperties(const wxRichTextProperties& props);
};

