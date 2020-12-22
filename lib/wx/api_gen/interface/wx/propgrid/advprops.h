/////////////////////////////////////////////////////////////////////////////
// Name:        advprops.h
// Purpose:     interfaces of wxPropertyGrid Advanced Properties (font,
//              colour, etc.)
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////




// Web colour is currently unsupported
#define wxPG_COLOUR_WEB_BASE        0x10000


#define wxPG_COLOUR_CUSTOM      0xFFFFFF
#define wxPG_COLOUR_UNSPECIFIED (wxPG_COLOUR_CUSTOM+1)

/** @class wxColourPropertyValue

    Because text, background and other colours tend to differ between
    platforms, wxSystemColourProperty must be able to select between system
    colour and, when necessary, to pick a custom one. wxSystemColourProperty
    value makes this possible.
*/
class wxColourPropertyValue : public wxObject
{
public:
    /** An integer value relating to the colour, and which exact
        meaning depends on the property with which it is used.

        For wxSystemColourProperty:

        Any of wxSYS_COLOUR_XXX, or any web-colour ( use wxPG_TO_WEB_COLOUR
        macro - (currently unsupported) ), or wxPG_COLOUR_CUSTOM.

        For custom colour properties without values array specified:

        index or wxPG_COLOUR_CUSTOM

        For custom colour properties <b>with</b> values array specified:

        m_arrValues[index] or wxPG_COLOUR_CUSTOM
    */
    wxUint32    m_type;

    /** Resulting colour. Should be correct regardless of type. */
    wxColour    m_colour;

    wxColourPropertyValue();
    wxColourPropertyValue( const wxColourPropertyValue& v );
    wxColourPropertyValue( const wxColour& colour );
    wxColourPropertyValue( wxUint32 type );
    wxColourPropertyValue( wxUint32 type, const wxColour& colour );

    virtual ~wxColourPropertyValue();

    void Init( wxUint32 type, const wxColour& colour );

    void operator=(const wxColourPropertyValue& cpv);
};



/** @class wxFontProperty
    @ingroup classes
    Property representing wxFont.

    <b>Supported special attributes:</b>
    - ::wxPG_DIALOG_TITLE: Sets a specific title for the font dialog (since 3.1.3).
*/
class wxFontProperty : public wxEditorDialogProperty
{
public:
    wxFontProperty(const wxString& label = wxPG_LABEL,
                   const wxString& name = wxPG_LABEL,
                   const wxFont& value = wxFont());
    virtual ~wxFontProperty();
    virtual void OnSetValue();
    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual wxVariant ChildChanged( wxVariant& thisValue,
                                    int childIndex,
                                    wxVariant& childValue ) const;
    virtual void RefreshChildren();

protected:
    virtual bool DisplayEditorDialog(wxPropertyGrid* pg, wxVariant& value);
};




/** If set, then match from list is searched for a custom colour. */
#define wxPG_PROP_TRANSLATE_CUSTOM      wxPG_PROP_CLASS_SPECIFIC_1


/** @class wxSystemColourProperty
    @ingroup classes
    Has dropdown list of wxWidgets system colours. Value used is
    of wxColourPropertyValue type.

    <b>Supported special attributes:</b>
    ::wxPG_COLOUR_ALLOW_CUSTOM, ::wxPG_COLOUR_HAS_ALPHA
*/
class wxSystemColourProperty : public wxEnumProperty
{
public:

    wxSystemColourProperty( const wxString& label = wxPG_LABEL,
                            const wxString& name = wxPG_LABEL,
                            const wxColourPropertyValue&
                                value = wxColourPropertyValue() );
    virtual ~wxSystemColourProperty();

    virtual void OnSetValue();
    virtual bool IntToValue(wxVariant& variant,
                            int number,
                            int argFlags = 0) const;

    /**
        Override in derived class to customize how colours are printed as
        strings.
    */
    virtual wxString ColourToString( const wxColour& col, int index,
                                     int argFlags = 0 ) const;

    /** Returns index of entry that triggers colour picker dialog
        (default is last).
    */
    virtual int GetCustomColourIndex() const;

    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;
    virtual bool OnEvent( wxPropertyGrid* propgrid,
                          wxWindow* primary, wxEvent& event );
    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );
    virtual wxSize OnMeasureImage( int item ) const;
    virtual void OnCustomPaint( wxDC& dc,
                                const wxRect& rect, wxPGPaintData& paintdata );

    // Helper function to show the colour dialog
    bool QueryColourFromUser( wxVariant& variant ) const;

    /** Default is to use wxSystemSettings::GetColour(index). Override to use
        custom colour tables etc.
    */
    virtual wxColour GetColour( int index ) const;

    wxColourPropertyValue GetVal( const wxVariant* pVariant = NULL ) const;

protected:

    // Special constructors to be used by derived classes.
    wxSystemColourProperty( const wxString& label, const wxString& name,
        const wxChar* const* labels, const long* values, wxPGChoices* choicesCache,
        const wxColourPropertyValue& value );

    wxSystemColourProperty( const wxString& label, const wxString& name,
        const wxChar* const* labels, const long* values, wxPGChoices* choicesCache,
        const wxColour& value );

    void Init( int type, const wxColour& colour );

    // Utility functions for internal use
    virtual wxVariant DoTranslateVal( wxColourPropertyValue& v ) const;
    wxVariant TranslateVal( wxColourPropertyValue& v ) const;
    wxVariant TranslateVal( int type, const wxColour& colour ) const;

    // Translates colour to a int value, return wxNOT_FOUND if no match.
    int ColToInd( const wxColour& colour ) const;
};



/** @class wxColourProperty
    @ingroup classes
    Allows to select a colour from the list or with colour dialog. Value used
    is of wxColourPropertyValue type.

    <b>Supported special attributes:</b>
    ::wxPG_COLOUR_ALLOW_CUSTOM, ::wxPG_COLOUR_HAS_ALPHA
*/
class wxColourProperty : public wxSystemColourProperty
{
public:
    wxColourProperty( const wxString& label = wxPG_LABEL,
                      const wxString& name = wxPG_LABEL,
                      const wxColour& value = *wxWHITE );
    virtual ~wxColourProperty();

    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual wxColour GetColour( int index ) const;

protected:
    virtual wxVariant DoTranslateVal( wxColourPropertyValue& v ) const;
};



/** @class wxCursorProperty
    @ingroup classes
    Property representing wxCursor.
*/
class wxCursorProperty : public wxEnumProperty
{
public:
    wxCursorProperty( const wxString& label= wxPG_LABEL,
                      const wxString& name= wxPG_LABEL,
                      int value = 0 );
    virtual ~wxCursorProperty();

    virtual wxSize OnMeasureImage( int item ) const;
    virtual void OnCustomPaint( wxDC& dc,
                                const wxRect& rect, wxPGPaintData& paintdata );
};


const wxString& wxPGGetDefaultImageWildcard();

/** @class wxImageFileProperty
    @ingroup classes
    Property representing image file(name).

    <b>Supported special attributes:</b>
    - ::wxPG_DIALOG_TITLE: Sets a specific title for the file dialog (since 3.1.3).
    - ::wxPG_FILE_DIALOG_STYLE: Sets a specific wxFileDialog style for the file dialog.
    - ::wxPG_FILE_WILDCARD: Sets wildcard (see wxFileDialog for format details), "All
    files..." is default.
    - ::wxPG_FILE_SHOW_FULL_PATH: Default @true. When @false, only the file name is shown
    (i.e. drive and directory are hidden).
    - ::wxPG_FILE_SHOW_RELATIVE_PATH: If set, then the filename is shown relative to the
    given path string.
    - ::wxPG_FILE_INITIAL_PATH: Sets the initial path of where to look for files.
*/
class wxImageFileProperty : public wxFileProperty
{
public:

    wxImageFileProperty( const wxString& label= wxPG_LABEL,
                         const wxString& name = wxPG_LABEL,
                         const wxString& value = wxEmptyString);
    virtual ~wxImageFileProperty();

    virtual void OnSetValue();

    virtual wxSize OnMeasureImage( int item ) const;
    virtual void OnCustomPaint( wxDC& dc,
                                const wxRect& rect, wxPGPaintData& paintdata );

protected:
    wxBitmap*   m_pBitmap; // final thumbnail area
    wxImage*    m_pImage; // intermediate thumbnail area
};



/** @class wxMultiChoiceProperty
    @ingroup classes
    Property that manages a value resulting from wxMultiChoiceDialog. Value is
    array of strings. You can get value as array of choice values/indices by
    calling wxMultiChoiceProperty::GetValueAsArrayInt().

    <b>Supported special attributes:</b>
    - ::wxPG_ATTR_MULTICHOICE_USERSTRINGMODE: If > 0, allow user to manually
      enter strings that are not in the list of choices. If this value is 1,
      user strings are preferably placed in front of valid choices. If value is
      2, then those strings will placed behind valid choices.
    - ::wxPG_DIALOG_TITLE: Sets a specific title for the editor dialog (since 3.1.3).
*/
class wxMultiChoiceProperty : public wxEditorDialogProperty
{
public:

    wxMultiChoiceProperty( const wxString& label,
                           const wxString& name,
                           const wxArrayString& strings,
                           const wxArrayString& value );
    wxMultiChoiceProperty( const wxString& label,
                           const wxString& name,
                           const wxPGChoices& choices,
                           const wxArrayString& value = wxArrayString() );

    wxMultiChoiceProperty( const wxString& label = wxPG_LABEL,
                           const wxString& name = wxPG_LABEL,
                           const wxArrayString& value = wxArrayString() );

    virtual ~wxMultiChoiceProperty();

    virtual void OnSetValue();
    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue(wxVariant& variant,
                               const wxString& text,
                               int argFlags = 0) const;

    wxArrayInt GetValueAsArrayInt() const;

protected:
    virtual bool DisplayEditorDialog(wxPropertyGrid* pg, wxVariant& value);

    void GenerateValueAsString( wxVariant& value, wxString* target ) const;

    // Returns translation of values into string indices.
    wxArrayInt GetValueAsIndices() const;

    wxArrayString       m_valueAsStrings;  // Value as array of strings

    // Cache displayed text since generating it is relatively complicated.
    wxString            m_display;
    // How to handle user strings
    int                 m_userStringMode;
};



/** @class wxDateProperty
    @ingroup classes
    Property representing wxDateTime.

    <b>Supported special attributes:</b>
    - ::wxPG_DATE_FORMAT: Determines displayed date format.
    - ::wxPG_DATE_PICKER_STYLE: Determines window style used with wxDatePickerCtrl.
       Default is ::wxDP_DEFAULT | ::wxDP_SHOWCENTURY. Using ::wxDP_ALLOWNONE
       enables additional support for unspecified property value.
*/
class wxDateProperty : public wxPGProperty
{
public:

    wxDateProperty( const wxString& label = wxPG_LABEL,
                    const wxString& name = wxPG_LABEL,
                    const wxDateTime& value = wxDateTime() );
    virtual ~wxDateProperty();

    virtual void OnSetValue();
    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue(wxVariant& variant,
                               const wxString& text,
                               int argFlags = 0) const;

    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );

    void SetFormat( const wxString& format );
    const wxString& GetFormat() const;

    void SetDateValue( const wxDateTime& dt );
    wxDateTime GetDateValue() const;

    long GetDatePickerStyle() const;

protected:
    wxString        m_format;
    long            m_dpStyle;  // DatePicker style

    static wxString ms_defaultDateFormat;
    static wxString DetermineDefaultDateFormat( bool showCentury );
};



class wxPGSpinCtrlEditor : public wxPGTextCtrlEditor
{
public:
    virtual ~wxPGSpinCtrlEditor();

    wxString GetName() const;
    virtual wxPGWindowList CreateControls(wxPropertyGrid* propgrid,
                                          wxPGProperty* property,
                                          const wxPoint& pos,
                                          const wxSize& size) const;
    virtual bool OnEvent( wxPropertyGrid* propgrid, wxPGProperty* property,
        wxWindow* wnd, wxEvent& event ) const;
};


extern wxPGEditor* wxPGEditor_SpinCtrl;
extern wxPGEditor* wxPGEditor_DatePickerCtrl;
