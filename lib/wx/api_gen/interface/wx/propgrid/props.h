/////////////////////////////////////////////////////////////////////////////
// Name:        props.h
// Purpose:     interface of some wxPGProperty subclasses
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////



/** @class wxPGInDialogValidator
    @ingroup classes
    Creates and manages a temporary wxTextCtrl for validation purposes.
    Uses wxPropertyGrid's current editor, if available.
*/
class wxPGInDialogValidator
{
public:
    wxPGInDialogValidator();
    ~wxPGInDialogValidator();

    bool DoValidate( wxPropertyGrid* propGrid,
                     wxValidator* validator,
                     const wxString& value );
};


// -----------------------------------------------------------------------
// Property classes
// -----------------------------------------------------------------------

#define wxPG_PROP_PASSWORD  wxPG_PROP_CLASS_SPECIFIC_2

/** @class wxStringProperty
    @ingroup classes
    Basic property with string value.

    <b>Supported special attributes:</b>
    - ::wxPG_STRING_PASSWORD: set to @true in order to enable ::wxTE_PASSWORD
    on the editor.

    @remarks
    - If value "<composed>" is set, then actual value is formed (or composed)
      from values of child properties.
*/
class wxStringProperty : public wxPGProperty
{
public:
    wxStringProperty( const wxString& label = wxPG_LABEL,
                      const wxString& name = wxPG_LABEL,
                      const wxString& value = wxEmptyString );
    virtual ~wxStringProperty();

    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;

    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );

    /** This is updated so "<composed>" special value can be handled.
    */
    virtual void OnSetValue();
};


/** Constants used with NumericValidation<>().
*/
enum wxPGNumericValidationConstants
{
    /** Instead of modifying the value, show an error message.
    */
    wxPG_PROPERTY_VALIDATION_ERROR_MESSAGE      = 0,

    /** Modify value, but stick with the limitations.
    */
    wxPG_PROPERTY_VALIDATION_SATURATE           = 1,

    /** Modify value, wrap around on overflow.
    */
    wxPG_PROPERTY_VALIDATION_WRAP               = 2
};



/**
    A more comprehensive numeric validator class.
*/
class wxNumericPropertyValidator : public wxTextValidator
{
public:
    enum NumericType
    {
        Signed = 0,
        Unsigned,
        Float
    };

    wxNumericPropertyValidator( NumericType numericType, int base = 10 );
    virtual ~wxNumericPropertyValidator() { }
    virtual bool Validate(wxWindow* parent);
};

/** @class wxNumericProperty
    @ingroup classes

    This is an abstract class which serves as a base class for numeric properties,
    like wxIntProperty, wxUIntProperty, wxFloatProperty.

    <b>Supported special attributes:</b>
    - ::wxPG_ATTR_MIN, ::wxPG_ATTR_MAX: Specify acceptable value range.
    - ::wxPG_ATTR_SPINCTRL_STEP: How much number changes when SpinCtrl editor
    button is pressed (or up/down on keyboard).
    - ::wxPG_ATTR_SPINCTRL_WRAP: Specify if value modified with SpinCtrl editor
    wraps at Min/Max.
    - ::wxPG_ATTR_SPINCTRL_MOTION: Specify if value can also by changed with
    SpinCtrl editor by moving mouse when left mouse button is being pressed.

    @since 3.1.3
*/
class wxNumericProperty : public wxPGProperty
{
public:
    virtual ~wxNumericProperty();

    virtual bool DoSetAttribute(const wxString& name, wxVariant& value);

    /**
        Returns what would be the new value of the property after adding
        SpinCtrl editor step to the current value. Current value range
        and wrapping (if enabled) are taken into account.
        This member has to be implemented in derived properties.

        @param stepScale
        SpinCtrl editor step is first multiplied by this factor and next
        added to the current value.

        @return
        Value which property would have after adding SpinCtrl editor step.

        @remark
        Current property value is not changed.
    */
    virtual wxVariant AddSpinStepValue(long stepScale) const = 0;

    /**
        Return @true if value can be changed with SpinCtrl editor by moving
        the mouse.
    */
    bool UseSpinMotion() const;

protected:
    /**
        Constructor is protected because wxNumericProperty is only a base
        class for other numeric property classes.
    */
    wxNumericProperty(const wxString& label, const wxString& name);

    wxVariant m_minVal;
    wxVariant m_maxVal;
    bool      m_spinMotion;
    wxVariant m_spinStep;
    bool      m_spinWrap;
};


/** @class wxIntProperty
    @ingroup classes
    Basic property with integer value.

    Seamlessly supports 64-bit integer (wxLongLong) on overflow.

    <b>Example how to use seamless 64-bit integer support</b>

      Getting value:

      @code
          wxLongLong_t value = pg->GetPropertyValueAsLongLong();
      @endcode

         or

      @code
          wxLongLong_t value;
          wxVariant variant = property->GetValue();
          if ( variant.GetType() == "wxLongLong" )
              value = wxLongLongFromVariant(variant);
          else
              value = variant.GetLong();
      @endcode

      Setting value:

       @code
          pg->SetPropertyValue(longLongVal);
      @endcode

         or

      @code
          property->SetValue(WXVARIANT(longLongVal));
      @endcode


    <b>Supported special attributes:</b>
    - ::wxPG_ATTR_MIN, ::wxPG_ATTR_MAX, ::wxPG_ATTR_SPINCTRL_STEP,
    ::wxPG_ATTR_SPINCTRL_WRAP, ::wxPG_ATTR_SPINCTRL_MOTION:
    like in wxNumericProperty.
*/
class wxIntProperty : public wxNumericProperty
{
public:
    wxIntProperty( const wxString& label = wxPG_LABEL,
                   const wxString& name = wxPG_LABEL,
                   long value = 0 );
    virtual ~wxIntProperty();

    wxIntProperty( const wxString& label,
                   const wxString& name,
                   const wxLongLong& value );
    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;
    virtual bool ValidateValue( wxVariant& value,
                                wxPGValidationInfo& validationInfo ) const;
    virtual bool IntToValue( wxVariant& variant,
                             int number,
                             int argFlags = 0 ) const;
    static wxValidator* GetClassValidator();
    virtual wxValidator* DoGetValidator() const;
    virtual wxVariant AddSpinStepValue(long stepScale) const;
};


/** @class wxUIntProperty
    @ingroup classes
    Basic property with unsigned integer value.
    Seamlessly supports 64-bit integer (wxULongLong) on overflow.

    <b>Supported special attributes:</b>
    - ::wxPG_UINT_BASE: Define base. Valid constants are ::wxPG_BASE_OCT,
    ::wxPG_BASE_DEC, ::wxPG_BASE_HEX and ::wxPG_BASE_HEXL (lowercase characters).
    Arbitrary bases are <b>not</b> supported.
    - ::wxPG_UINT_PREFIX: Possible values are ::wxPG_PREFIX_NONE, ::wxPG_PREFIX_0x,
    and ::wxPG_PREFIX_DOLLAR_SIGN. Only ::wxPG_PREFIX_NONE works with Decimal
    and Octal numbers.
    - ::wxPG_ATTR_MIN, ::wxPG_ATTR_MAX, ::wxPG_ATTR_SPINCTRL_STEP,
    ::wxPG_ATTR_SPINCTRL_WRAP, ::wxPG_ATTR_SPINCTRL_MOTION:
    like in wxNumericProperty.

    @remarks
    - For example how to use seamless 64-bit integer support, see wxIntProperty
    documentation (just use wxULongLong instead of wxLongLong).
*/
class wxUIntProperty : public wxNumericProperty
{
public:
    wxUIntProperty( const wxString& label = wxPG_LABEL,
                    const wxString& name = wxPG_LABEL,
                    unsigned long value = 0 );
    virtual ~wxUIntProperty();
    wxUIntProperty( const wxString& label,
                    const wxString& name,
                    const wxULongLong& value );
    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;
    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );
    virtual bool ValidateValue( wxVariant& value,
                                wxPGValidationInfo& validationInfo ) const;
    virtual wxValidator* DoGetValidator () const;
    virtual bool IntToValue( wxVariant& variant,
                             int number,
                             int argFlags = 0 ) const;
    virtual wxVariant AddSpinStepValue(long stepScale) const;

protected:
    wxByte      m_base;
    wxByte      m_realBase; // translated to 8,16,etc.
    wxByte      m_prefix;
};


/** @class wxFloatProperty
    @ingroup classes
    Basic property with double-precision floating point value.

    <b>Supported special attributes:</b>
    - ::wxPG_FLOAT_PRECISION: Sets the (max) precision used when floating point
    value is rendered as text. The default -1 means infinite precision.
    - ::wxPG_ATTR_MIN, ::wxPG_ATTR_MAX, ::wxPG_ATTR_SPINCTRL_STEP,
    ::wxPG_ATTR_SPINCTRL_WRAP, ::wxPG_ATTR_SPINCTRL_MOTION:
    like in wxNumericProperty.
*/
class wxFloatProperty : public wxNumericProperty
{
public:
    wxFloatProperty( const wxString& label = wxPG_LABEL,
                     const wxString& name = wxPG_LABEL,
                     double value = 0.0 );
    virtual ~wxFloatProperty();

    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;
    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );
    virtual bool ValidateValue( wxVariant& value,
                                wxPGValidationInfo& validationInfo ) const;

    static wxValidator* GetClassValidator();
    virtual wxValidator* DoGetValidator () const;
    virtual wxVariant AddSpinStepValue(long stepScale) const;

protected:
    int m_precision;
};



/** @class wxBoolProperty
    @ingroup classes
    Basic property with boolean value.

    <b>Supported special attributes:</b>
    - ::wxPG_BOOL_USE_CHECKBOX: Set to @true to use check box editor instead
    of combo box.
    - ::wxPG_BOOL_USE_DOUBLE_CLICK_CYCLING: Set to @true to cycle combo box
    instead showing the list.
*/
class wxBoolProperty : public wxPGProperty
{
public:
    wxBoolProperty( const wxString& label = wxPG_LABEL,
                    const wxString& name = wxPG_LABEL,
                    bool value = false );
    virtual ~wxBoolProperty();

    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;
    virtual bool IntToValue( wxVariant& variant,
                             int number, int argFlags = 0 ) const;
    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );
};



// If set, then selection of choices is static and should not be
// changed (i.e. returns NULL in GetPropertyChoices).
#define wxPG_PROP_STATIC_CHOICES    wxPG_PROP_CLASS_SPECIFIC_1

/** @class wxEnumProperty
    @ingroup classes
    You can derive custom properties with choices from this class. See
    wxBaseEnumProperty for remarks.

    @remarks
    - Updating private index is important. You can do this either by calling
    SetIndex() in IntToValue, and then letting wxBaseEnumProperty::OnSetValue
    be called (by not implementing it, or by calling super class function in
    it) -OR- you can just call SetIndex in OnSetValue.
*/
class wxEnumProperty : public wxPGProperty
{
public:
    wxEnumProperty( const wxString& label = wxPG_LABEL,
                    const wxString& name = wxPG_LABEL,
                    const wxChar* const* labels = NULL,
                    const long* values = NULL,
                    int value = 0 );

    wxEnumProperty( const wxString& label,
                    const wxString& name,
                    wxPGChoices& choices,
                    int value = 0 );

    // Special constructor for caching choices (used by derived class)
    wxEnumProperty( const wxString& label,
                    const wxString& name,
                    const wxChar* const* labels,
                    const long* values,
                    wxPGChoices* choicesCache,
                    int value = 0 );

    wxEnumProperty( const wxString& label,
                    const wxString& name,
                    const wxArrayString& labels,
                    const wxArrayInt& values = wxArrayInt(),
                    int value = 0 );

    virtual ~wxEnumProperty();

    size_t GetItemCount() const;

    virtual void OnSetValue();
    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;
    virtual bool ValidateValue( wxVariant& value,
                                wxPGValidationInfo& validationInfo ) const;

    // If wxPG_FULL_VALUE is not set in flags, then the value is interpreted
    // as index to choices list. Otherwise, it is actual value.
    virtual bool IntToValue( wxVariant& variant,
                             int number,
                             int argFlags = 0 ) const;

    //
    // Additional virtuals

    // This must be overridden to have non-index based value
    virtual int GetIndexForValue( int value ) const;

    // GetChoiceSelection needs to overridden since m_index is
    // the true index, and various property classes derived from
    // this take advantage of it.
    virtual int GetChoiceSelection() const;

protected:

    int GetIndex() const;
    void SetIndex( int index );

    bool ValueFromString_( wxVariant& value,
                           const wxString& text,
                           int argFlags ) const;
    bool ValueFromInt_( wxVariant& value, int intVal, int argFlags ) const;

    static void ResetNextIndex();

};



/** @class wxEditEnumProperty
    @ingroup classes
    wxEnumProperty with wxString value and writable combo box editor.

    @remarks
    Uses int value, similar to wxEnumProperty, unless text entered by user is
    is not in choices (in which case string value is used).
*/
class wxEditEnumProperty : public wxEnumProperty
{
public:

    wxEditEnumProperty( const wxString& label,
                        const wxString& name,
                        const wxChar* const* labels,
                        const long* values,
                        const wxString& value );

    wxEditEnumProperty( const wxString& label = wxPG_LABEL,
                        const wxString& name = wxPG_LABEL,
                        const wxArrayString& labels = wxArrayString(),
                        const wxArrayInt& values = wxArrayInt(),
                        const wxString& value = wxEmptyString );

    wxEditEnumProperty( const wxString& label,
                        const wxString& name,
                        wxPGChoices& choices,
                        const wxString& value = wxEmptyString );

    // Special constructor for caching choices (used by derived class)
    wxEditEnumProperty( const wxString& label,
                        const wxString& name,
                        const wxChar* const* labels,
                        const long* values,
                        wxPGChoices* choicesCache,
                        const wxString& value );

    virtual ~wxEditEnumProperty();

};



/** @class wxFlagsProperty
    @ingroup classes
    Represents a bit set that fits in a long integer. wxBoolProperty
    sub-properties are created for editing individual bits. Textctrl is created
    to manually edit the flags as a text; a continuous sequence of spaces,
    commas and semicolons is considered as a flag id separator.
    <b>Note:</b> When changing "choices" (ie. flag labels) of wxFlagsProperty,
    you will need to use SetPropertyChoices - otherwise they will not get
    updated properly.
*/
class wxFlagsProperty : public wxPGProperty
{
public:

    wxFlagsProperty( const wxString& label,
                     const wxString& name,
                     const wxChar* const* labels,
                     const long* values = NULL,
                     long value = 0 );

    wxFlagsProperty( const wxString& label,
                     const wxString& name,
                     wxPGChoices& choices,
                     long value = 0 );

    wxFlagsProperty( const wxString& label = wxPG_LABEL,
                     const wxString& name = wxPG_LABEL,
                     const wxArrayString& labels = wxArrayString(),
                     const wxArrayInt& values = wxArrayInt(),
                     int value = 0 );

    virtual ~wxFlagsProperty ();

    virtual void OnSetValue();
    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int flags ) const;
    virtual wxVariant ChildChanged( wxVariant& thisValue,
                                    int childIndex,
                                    wxVariant& childValue ) const;
    virtual void RefreshChildren();
    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );

    // GetChoiceSelection needs to overridden since m_choices is
    // used and value is integer, but it is not index.
    virtual int GetChoiceSelection() const;

    // helpers
    size_t GetItemCount() const;
    const wxString& GetLabel( size_t ind ) const;

protected:
    // Used to detect if choices have been changed
    wxPGChoicesData*        m_oldChoicesData;

    // Needed to properly mark changed sub-properties
    long                    m_oldValue;

    // Converts string id to a relevant bit.
    long IdToBit( const wxString& id ) const;

    // Creates children and sets value.
    void Init();
};


/** @class wxEditorDialogProperty
    @ingroup classes

    This is an abstract class which serves as a base class for the properties
    having a button triggering an editor dialog, like e.g. wxLongStringProperty,
    wxDirProperty, wxFileProperty.

    <b>Supported special attributes:</b>
    - ::wxPG_DIALOG_TITLE: Sets a specific title for the editor dialog.

    @since 3.1.3
*/
class wxEditorDialogProperty : public wxPGProperty
{
public:
    virtual ~wxEditorDialogProperty();

    virtual wxPGEditorDialogAdapter* GetEditorDialog() const;
    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );

protected:
    /**
        Constructor is protected because wxEditorDialogProperty is only
        the base class for other property classes.
    */
    wxEditorDialogProperty(const wxString& label, const wxString& name);

    /**
        Shows editor dialog. Value to be edited should be read from
        @a value, and if dialog is not cancelled, it should be stored back
        and @true should be returned.

        @param value
        Value to be edited.

        @param pg
        Property grid in which property is displayed.

        @return
        Returns @true if editor dialog was not cancelled and @a value
        was updated.
    */
    virtual bool DisplayEditorDialog(wxPropertyGrid* pg, wxVariant& value) = 0;

    wxString  m_dlgTitle;
    long      m_dlgStyle;
};


// Indicates first bit useable by derived properties.
#define wxPG_PROP_SHOW_FULL_FILENAME  wxPG_PROP_CLASS_SPECIFIC_1

/** @class wxFileProperty
    @ingroup classes
    Like wxLongStringProperty, but the button triggers file selector instead.

    <b>Supported special attributes:</b>
    - ::wxPG_DIALOG_TITLE: Sets a specific title for the file dialog (since 3.1.3).
    - ::wxPG_FILE_DIALOG_STYLE: Sets a specific wxFileDialog style for the file
    dialog (since 2.9.4).
    - ::wxPG_FILE_WILDCARD: Sets wildcard (see wxFileDialog for format details),
    "All files..." is default.
    - ::wxPG_FILE_SHOW_FULL_PATH: Default @true. When @false, only the file name is
    shown (i.e. drive and directory are hidden).
    - ::wxPG_FILE_SHOW_RELATIVE_PATH: If set, then the filename is shown relative
    to the given path string.
    - ::wxPG_FILE_INITIAL_PATH: Sets the initial path of where to look for files.
*/
class wxFileProperty : public wxEditorDialogProperty
{
public:

    wxFileProperty( const wxString& label = wxPG_LABEL,
                    const wxString& name = wxPG_LABEL,
                    const wxString& value = wxEmptyString );
    virtual ~wxFileProperty ();

    virtual void OnSetValue();
    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;
    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );

    static wxValidator* GetClassValidator();
    virtual wxValidator* DoGetValidator() const;

    /**
        Returns filename to file represented by current value.
    */
    wxFileName GetFileName() const;

protected:
    virtual bool DisplayEditorDialog(wxPropertyGrid* pg, wxVariant& value);

    wxString    m_wildcard;
    wxString    m_basePath;
    wxString    m_initialPath;
    int         m_indFilter;
};


#define wxPG_PROP_ACTIVE_BTN    wxPG_PROP_CLASS_SPECIFIC_1

/** @class wxLongStringProperty
    @ingroup classes
    Like wxStringProperty, but has a button that triggers a small text
    editor dialog.

    <b>Supported special attributes:</b>
    - ::wxPG_DIALOG_TITLE: Sets a specific title for the text editor dialog (since 3.1.3).
*/
class wxLongStringProperty : public wxEditorDialogProperty
{
public:

    wxLongStringProperty( const wxString& label = wxPG_LABEL,
                          const wxString& name = wxPG_LABEL,
                          const wxString& value = wxEmptyString );
    virtual ~wxLongStringProperty();

    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;

protected:
    virtual bool DisplayEditorDialog(wxPropertyGrid* pg, wxVariant& value);
};


/** @class wxDirProperty
    @ingroup classes
    Like wxLongStringProperty, but the button triggers directory selector
    instead.

    <b>Supported special attributes:</b>
    - ::wxPG_DIALOG_TITLE: Sets a specific title for the file dialog (since 3.1.3).
*/
class wxDirProperty : public wxEditorDialogProperty
{
public:
    wxDirProperty( const wxString& label = wxPG_LABEL,
                   const wxString& name = wxPG_LABEL,
                   const wxString& value = wxEmptyString );
    virtual ~wxDirProperty();

    virtual wxString ValueToString(wxVariant& value, int argFlags = 0) const;
    virtual bool StringToValue(wxVariant& variant, const wxString& text,
                               int argFlags = 0) const;
    virtual wxValidator* DoGetValidator() const;

protected:
    virtual bool DisplayEditorDialog(wxPropertyGrid* pg, wxVariant& value);
};


// wxBoolProperty specific flags
#define wxPG_PROP_USE_CHECKBOX      wxPG_PROP_CLASS_SPECIFIC_1
// DCC = Double Click Cycles
#define wxPG_PROP_USE_DCC           wxPG_PROP_CLASS_SPECIFIC_2



/** @class wxArrayStringProperty
    @ingroup classes
    Property that manages a list of strings.

    <b>Supported special attributes:</b>
    - ::wxPG_ARRAY_DELIMITER: Sets string delimiter character.
    - ::wxPG_DIALOG_TITLE: Sets a specific title for the editor dialog (since 3.1.3).
*/
class wxArrayStringProperty : public wxEditorDialogProperty
{
public:
    wxArrayStringProperty( const wxString& label = wxPG_LABEL,
                           const wxString& name = wxPG_LABEL,
                           const wxArrayString& value = wxArrayString() );
    virtual ~wxArrayStringProperty();

    virtual void OnSetValue();
    virtual wxString ValueToString( wxVariant& value, int argFlags = 0 ) const;
    virtual bool StringToValue( wxVariant& variant,
                                const wxString& text,
                                int argFlags = 0 ) const;
    virtual bool DoSetAttribute( const wxString& name, wxVariant& value );

    /**
        Implement in derived class for custom array-to-string conversion.
    */
    virtual void ConvertArrayToString(const wxArrayString& arr,
                                      wxString* pString,
                                      const wxUniChar& delimiter) const;

    /**
        Shows string editor dialog to edit the individual item. Value to be edited
        should be read from @a value, and if dialog is not cancelled, it
        should be stored back and @true should be returned if that was the case.
    */
    virtual bool OnCustomStringEdit( wxWindow* parent, wxString& value );

    /** Creates wxPGArrayEditorDialog for string editing.
    */
    virtual wxPGArrayEditorDialog* CreateEditorDialog();

    enum ConversionFlags
    {
        Escape          = 0x01,
        QuoteStrings    = 0x02
    };

    /**
        Generates contents for string @a dst based on the contents of
        wxArrayString @a src.
    */
    static void ArrayStringToString( wxString& dst, const wxArrayString& src,
                                     wxUniChar delimiter, int flags );

protected:
    virtual bool DisplayEditorDialog(wxPropertyGrid* pg, wxVariant& value);

    /**
        Previously this was to be implemented in derived class for array-to-
        string conversion. Now you should implement ConvertValueToString()
        instead.
    */
    virtual void GenerateValueAsString();

    wxString        m_display; // Cache for displayed text.
    wxUniChar       m_delimiter;
    wxString        m_customBtnText;
};


// -----------------------------------------------------------------------
// wxPGArrayEditorDialog
// -----------------------------------------------------------------------

#define wxAEDIALOG_STYLE \
    (wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxOK | wxCANCEL | wxCENTRE)

class wxPGArrayEditorDialog : public wxDialog
{
public:
    wxPGArrayEditorDialog();
    virtual ~wxPGArrayEditorDialog();

    void Init();

    bool Create( wxWindow *parent,
                 const wxString& message,
                 const wxString& caption,
                 long style = wxAEDIALOG_STYLE,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& sz = wxDefaultSize );

    void EnableCustomNewAction();

    /** Sets tooltip text for button allowing the user to enter new string.
        @since 3.1.3
    */
    void SetNewButtonText(const wxString& text);

    /** Set value modified by dialog.
    */
    virtual void SetDialogValue( const wxVariant& value );

    /** Return value modified by dialog.
    */
    virtual wxVariant GetDialogValue() const;

    /** Override to return wxValidator to be used with the wxTextCtrl
        in dialog. Note that the validator is used in the standard
        wx way, ie. it immediately prevents user from entering invalid
        input.

        @remarks
        Dialog frees the validator.
    */
    virtual wxValidator* GetTextCtrlValidator() const;

    /** Returns true if array was actually modified
    */
    bool IsModified() const;

    int GetSelection() const;

protected:
    wxEditableListBox*  m_elb;
    wxWindow*           m_elbSubPanel;
    wxWindow*           m_lastFocused;

    /** A new item, edited by user, is pending at this index.
        It will be committed once list ctrl item editing is done.
    */
    int             m_itemPendingAtIndex;

    bool            m_modified;
    bool            m_hasCustomNewAction;

    virtual wxString ArrayGet( size_t index ) = 0;
    virtual size_t ArrayGetCount() = 0;
    virtual bool ArrayInsert( const wxString& str, int index ) = 0;
    virtual bool ArraySet( size_t index, const wxString& str ) = 0;
    virtual void ArrayRemoveAt( int index ) = 0;
    virtual void ArraySwap( size_t first, size_t second ) = 0;

    virtual bool OnCustomNewAction(wxString* resString);
};


// -----------------------------------------------------------------------
// wxPGArrayStringEditorDialog
// -----------------------------------------------------------------------

class wxPGArrayStringEditorDialog : public wxPGArrayEditorDialog
{
public:
    wxPGArrayStringEditorDialog();
    virtual ~wxPGArrayStringEditorDialog() { }

    void Init();

    virtual void SetDialogValue( const wxVariant& value );
    virtual wxVariant GetDialogValue() const;

    void SetCustomButton( const wxString& custBtText,
                          wxArrayStringProperty* pcc );

    virtual bool OnCustomNewAction(wxString* resString);

protected:
    wxArrayString   m_array;

    wxArrayStringProperty*     m_pCallingClass;

    virtual wxString ArrayGet( size_t index );
    virtual size_t ArrayGetCount();
    virtual bool ArrayInsert( const wxString& str, int index );
    virtual bool ArraySet( size_t index, const wxString& str );
    virtual void ArrayRemoveAt( int index );
    virtual void ArraySwap( size_t first, size_t second );
};

