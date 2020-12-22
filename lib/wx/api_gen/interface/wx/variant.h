/////////////////////////////////////////////////////////////////////////////
// Name:        variant.h
// Purpose:     interface of wxVariant
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxVariant

    The wxVariant class represents a container for any type. A variant's value
    can be changed at run time, possibly to a different type of value.

    @note As of wxWidgets 2.9.1, wxAny has become the preferred variant class.
          While most controls still use wxVariant in their interface, you
          can start using wxAny in your code because of an implicit conversion
          layer. See below for more information.

    As standard, wxVariant can store values of type bool, wxChar, double, long,
    string, string list, time, date, void pointer, list of strings, and list of
    variants. However, an application can extend wxVariant's capabilities by
    deriving from the class wxVariantData and using the wxVariantData form of
    the wxVariant constructor or assignment operator to assign this data to a
    variant. Actual values for user-defined types will need to be accessed via
    the wxVariantData object, unlike the case for basic data types where
    convenience functions such as GetLong() can be used.

    Under Microsoft Windows, three additional wxVariantData-derived classes --
    wxVariantDataCurrency, wxVariantDataErrorCode and wxVariantDataSafeArray --
    are available for interoperation with OLE VARIANT when using wxAutomationObject.

    Pointers to any wxObject derived class can also easily be stored in a
    wxVariant. wxVariant will then use wxWidgets' built-in RTTI system to set
    the type name  (returned by GetType()) and to perform type-safety checks at
    runtime.

    This class is useful for reducing the programming for certain tasks, such
    as an editor for different data types, or a remote procedure call protocol.

    An optional name member is associated with a wxVariant. This might be used,
    for example, in CORBA or OLE automation classes, where named parameters are
    required.

    Note that as of wxWidgets 2.7.1, wxVariant is
    @ref overview_refcount "reference counted". Additionally, the convenience
    macros DECLARE_VARIANT_OBJECT() and IMPLEMENT_VARIANT_OBJECT() were added
    so that adding (limited) support for conversion to and from wxVariant can
    be very easily implemented without modifying either wxVariant or the class
    to be stored by wxVariant. Since assignment operators cannot be declared
    outside the class, the shift left operators are used like this:

    @code
    // in the header file
    DECLARE_VARIANT_OBJECT(MyClass)

    // in the implementation file
    IMPLEMENT_VARIANT_OBJECT(MyClass)

    // in the user code
    wxVariant variant;
    MyClass value;
    variant << value;

    // or
    value << variant;
    @endcode

    For this to work, MyClass must derive from wxObject, implement the
    @ref overview_rtti "wxWidgets RTTI system" and support the assignment
    operator and equality operator for itself. Ideally, it should also be
    reference counted to make copying operations cheap and fast. This can be
    most easily implemented using the reference counting support offered by
    wxObject itself. By default, wxWidgets already implements the shift
    operator conversion for a few of its drawing related classes:

    @code
    IMPLEMENT_VARIANT_OBJECT(wxColour)
    IMPLEMENT_VARIANT_OBJECT(wxImage)
    IMPLEMENT_VARIANT_OBJECT(wxIcon)
    IMPLEMENT_VARIANT_OBJECT(wxBitmap)
    @endcode

    Note that as of wxWidgets 2.9.0, wxVariantData no longer inherits from
    wxObject and wxVariant no longer uses the type-unsafe wxList class for list
    operations but the type-safe wxVariantList class. Also, wxVariantData now
    supports the wxVariantData::Clone() function for implementing the Unshare()
    function. wxVariantData::Clone() is implemented automatically by
    IMPLEMENT_VARIANT_OBJECT().

    Since wxVariantData no longer derives from wxObject, any code that tests
    the type of the data using wxDynamicCast() will require adjustment. You can
    use the macro wxDynamicCastVariantData() with the same arguments as
    wxDynamicCast(), to use C++ RTTI type information instead of wxWidgets
    RTTI.

    @section variant_wxanyconversion wxVariant to wxAny Conversion Layer

    wxAny is a more modern, template-based variant class. It is not
    directly compatible with wxVariant, but there is a transparent conversion
    layer.

    Following is an example how to use these conversions with wxPropertyGrid's
    property class wxPGProperty (which currently uses wxVariants both
    internally and in the public API):

    @code
        // Get property value as wxAny instead of wxVariant
        wxAny value = property->GetValue();

        // Do something with it
        DoSomethingWithString(value.As<wxString>());

        // Write back new value to property
        value = "New Value";
        property->SetValue(value);

    @endcode

    Some caveats:
    @li In wxAny, there are no separate types for handling integers of
        different sizes, so converting wxAny with 'long long' value
        will yield wxVariant of "long" type when the value is small
        enough to fit in without overflow. Otherwise, variant type
        "longlong" is used. Also note that wxAny holding unsigned integer
        will always be converted to "ulonglong" wxVariant type.

    @li Unlike wxVariant, wxAny does not store a (rarely needed) name string.

    @li Because of implicit conversion of wxVariant to wxAny, wxAny cannot
        usually contain value of type wxVariant. In other words,
        any.CheckType<wxVariant>() can never return @true.

    Supplied conversion functions will automatically work with all
    built-in wxVariant types, and also with all user-specified types generated
    using IMPLEMENT_VARIANT_OBJECT(). For hand-built wxVariantData classes,
    you will need to use supplied macros in a following manner:

    @code

    // Declare wxVariantData for data type Foo
    class wxVariantDataFoo: public wxVariantData
    {
    public:
        // interface
        // ...

        DECLARE_WXANY_CONVERSION()
    protected:
        // data storage etc
        // ...
    };

    IMPLEMENT_TRIVIAL_WXANY_CONVERSION(Foo, wxVariantDataFoo)

    @endcode

    @library{wxbase}
    @category{data}

    @see wxVariantData, wxAny
*/
class wxVariant : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxVariant();

    /**
        Constructs a variant directly with a wxVariantData object. wxVariant
        will take ownership of the wxVariantData and will not increase its
        reference count.
    */
    wxVariant(wxVariantData* data, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from another variant by increasing the reference
        count.
    */
    wxVariant(const wxVariant& variant);

    /**
        Constructs a variant by converting it from wxAny.
    */
    wxVariant(const wxAny& any);

    /**
        Constructs a variant from a wide string literal.
    */
    wxVariant(const wxChar* value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a string.
    */
    wxVariant(const wxString& value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a wide char.
    */
    wxVariant(wxChar value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a long.
    */
    wxVariant(long value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a bool.
    */
    wxVariant(bool value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a double.
    */
    wxVariant(double value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a wxLongLong.
    */
    wxVariant(wxLongLong value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a wxULongLong.
    */
    wxVariant(wxULongLong value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a list of variants
    */
    wxVariant(const wxVariantList& value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a void pointer.
    */
    wxVariant(void* value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a pointer to an wxObject
        derived class.
    */
    wxVariant(wxObject* value, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a wxDateTime.
    */
    wxVariant(const wxDateTime& val, const wxString& name = wxEmptyString);

    /**
        Constructs a variant from a wxArrayString.
    */
    wxVariant(const wxArrayString& val, const wxString& name = wxEmptyString);

    /**
        Destructor.

        @note wxVariantData's destructor is protected, so wxVariantData cannot
              usually be deleted. Instead, wxVariantData::DecRef() should be
              called. See @ref overview_refcount_destruct
              "reference-counted object destruction" for more info.
    */
    virtual ~wxVariant();


    /**
        @name List Functionality
    */
    //@{

    /**
        Returns the value at @a idx (zero-based).
    */
    wxVariant operator [](size_t idx) const;
    /**
        Returns a reference to the value at @a idx (zero-based). This can be
        used to change the value at this index.
    */
    wxVariant& operator [](size_t idx);

    /**
        Appends a value to the list.
    */
    void Append(const wxVariant& value);

    /**
        Makes the variant null by deleting the internal data and set the name
        to wxEmptyString.
    */
    void Clear();

    /**
        Deletes the contents of the list.
    */
    void ClearList();

    /**
        Deletes the zero-based @a item from the list.
    */
    bool Delete(size_t item);

    /**
        Returns the number of elements in the list.
    */
    size_t GetCount() const;

    /**
        Returns a reference to the wxVariantList class used by wxVariant if
        this wxVariant is currently a list of variants.
    */
    wxVariantList& GetList() const;

    /**
        Inserts a value at the front of the list.
    */
    void Insert(const wxVariant& value);

    /**
        Makes an empty list. This differs from a null variant which has no
        data; a null list is of type list, but the number of elements in the
        list is zero.
    */
    void NullList();

    //@}


    //@{
    /**
        Retrieves and converts the value of this variant to the type that
        @a value is.
    */
    bool Convert(long* value) const;
    bool Convert(bool* value) const;
    bool Convert(double* value) const;
    bool Convert(wxString* value) const;
    bool Convert(wxChar* value) const;
    bool Convert(wxLongLong* value) const;
    bool Convert(wxULongLong* value) const;
    bool Convert(wxDateTime* value) const;
    //@}

    /**
        Converts wxVariant into wxAny.
    */
    wxAny GetAny() const;

    /**
        Returns the string array value.
    */
    wxArrayString GetArrayString() const;

    /**
        Returns the boolean value.
    */
    bool GetBool() const;

    /**
        Returns the character value.
    */
    wxUniChar GetChar() const;

    /**
        Returns a pointer to the internal variant data. To take ownership of
        this data, you must call its wxVariantData::IncRef() method. When you
        stop using it, wxVariantData::DecRef() must be called as well.
    */
    wxVariantData* GetData() const;

    /**
        Returns the date value.
    */
    wxDateTime GetDateTime() const;

    /**
        Returns the floating point value.
    */
    double GetDouble() const;

    /**
        Returns the integer value.
    */
    long GetLong() const;

    /**
        Returns the signed 64-bit integer value.
    */
    wxLongLong GetLongLong() const;

    /**
        Returns a constant reference to the variant name.
    */
    const wxString& GetName() const;

    /**
        Gets the string value.
    */
    wxString GetString() const;

    /**
        Returns the value type as a string.

        The built-in types are:
        - "bool"
        - "char"
        - "datetime"
        - "double"
        - "list"
        - "long"
        - "longlong"
        - "string"
        - "ulonglong"
        - "arrstring"
        - "void*"

        If the variant is null, the value type returned is the string "null"
        (not the empty string).
    */
    wxString GetType() const;

    /**
        Returns the unsigned 64-bit integer value.
    */
    wxULongLong GetULongLong() const;

    /**
        Gets the void pointer value.

        Notice that this method can be used for null objects (i.e. those for
        which IsNull() returns @true) and will return @NULL for them.
    */
    void* GetVoidPtr() const;

    /**
        Gets the wxObject pointer value.
    */
    wxObject* GetWxObjectPtr() const;

    /**
        Returns @true if there is no data associated with this variant, @false
        if there is data.
    */
    bool IsNull() const;

    /**
        Returns @true if @a type matches the type of the variant, @false
        otherwise.
    */
    bool IsType(const wxString& type) const;

    /**
        Returns @true if the data is derived from the class described by
        @a type, @false otherwise.
    */
    bool IsValueKindOf(const wxClassInfo* type) const;

    /**
        Makes the variant null by deleting the internal data.
    */
    void MakeNull();

    /**
        Makes a string representation of the variant value (for any type).
    */
    wxString MakeString() const;

    /**
        Returns @true if @a value matches an element in the list.
    */
    bool Member(const wxVariant& value) const;

    /**
        Sets the internal variant data, deleting the existing data if there is
        any.
    */
    void SetData(wxVariantData* data);

    /**
        Makes sure that any data associated with this variant is not shared
        with other variants. For this to work, wxVariantData::Clone() must be
        implemented for the data types you are working with.
        wxVariantData::Clone() is implemented for all the default data types.
    */
    bool Unshare();

    //@{
    /**
        Inequality test operator.
    */
    bool operator !=(const wxVariant& value) const;
    bool operator !=(const wxString& value) const;
    bool operator !=(const wxChar* value) const;
    bool operator !=(wxChar value) const;
    bool operator !=(long value) const;
    bool operator !=(bool value) const;
    bool operator !=(double value) const;
    bool operator !=(wxLongLong value) const;
    bool operator !=(wxULongLong value) const;
    bool operator !=(void* value) const;
    bool operator !=(wxObject* value) const;
    bool operator !=(const wxVariantList& value) const;
    bool operator !=(const wxArrayString& value) const;
    bool operator !=(const wxDateTime& value) const;
    //@}

    //@{
    /**
        Assignment operator, using @ref overview_refcount "reference counting"
        if possible.
    */
    void operator =(const wxVariant& value);
    void operator =(wxVariantData* value);
    void operator =(const wxString& value);
    void operator =(const wxChar* value);
    void operator =(wxChar value);
    void operator =(long value);
    void operator =(bool value);
    void operator =(double value);
    bool operator =(wxLongLong value) const;
    bool operator =(wxULongLong value) const;
    void operator =(void* value);
    void operator =(wxObject* value);
    void operator =(const wxVariantList& value);
    void operator =(const wxDateTime& value);
    void operator =(const wxArrayString& value);
    //@}

    //@{
    /**
        Equality test operator.
    */
    bool operator ==(const wxVariant& value) const;
    bool operator ==(const wxString& value) const;
    bool operator ==(const wxChar* value) const;
    bool operator ==(wxChar value) const;
    bool operator ==(long value) const;
    bool operator ==(bool value) const;
    bool operator ==(double value) const;
    bool operator ==(wxLongLong value) const;
    bool operator ==(wxULongLong value) const;
    bool operator ==(void* value) const;
    bool operator ==(wxObject* value) const;
    bool operator ==(const wxVariantList& value) const;
    bool operator ==(const wxArrayString& value) const;
    bool operator ==(const wxDateTime& value) const;
    //@}

    //@{
    /**
        Operators for implicit conversion, using appropriate getter member
        function.
    */
    double operator double() const;
    long operator long() const;
    wxLongLong operator wxLongLong() const;
    wxULongLong operator wxULongLong() const;
    //@}

    /**
        Operator for implicit conversion to a pointer to a void, using
        GetVoidPtr().
    */
    void* operator void*() const;

    /**
        Operator for implicit conversion to a wxChar, using GetChar().
    */
    char operator wxChar() const;

    /**
        Operator for implicit conversion to a pointer to a wxDateTime, using
        GetDateTime().
    */
    void* operator wxDateTime() const;

    /**
        Operator for implicit conversion to a string, using MakeString().
    */
    wxString operator wxString() const;
};



/**
    @class wxVariantData

    The wxVariantData class is used to implement a new type for wxVariant.
    Derive from wxVariantData, and override the pure virtual functions.

    wxVariantData is @ref overview_refcount "reference counted", but you don't
    normally have to care about this, as wxVariant manages the count
    automatically. However, in case your application needs to take ownership of
    wxVariantData, be aware that the object is created with a reference count
    of 1, and passing it to wxVariant will not increase this. In other words,
    IncRef() needs to be called only if you both take ownership of
    wxVariantData and pass it to a wxVariant. Also note that the destructor is
    protected, so you can never explicitly delete a wxVariantData instance.
    Instead, DecRef() will delete the object automatically when the reference
    count reaches zero.

    @library{wxbase}
    @category{data}

    @see wxVariant, wxGetVariantCast()
*/
class wxVariantData : public wxObjectRefData
{
public:
    /**
        Default constructor.
    */
    wxVariantData();

    /**
        This function can be overridden to clone the data. You must implement
        this function in order for wxVariant::Unshare() to work for your data.
        This function is implemented for all built-in data types.
    */
    virtual wxVariantData* Clone() const;

    /**
        Decreases reference count. If the count reaches zero, the object is
        automatically deleted.

        @note The destructor of wxVariantData is protected, so delete cannot be
              used as normal. Instead, DecRef() should be called.
    */
    void DecRef();

    /**
        Returns @true if this object is equal to @a data.
    */
    virtual bool Eq(wxVariantData& data) const = 0;

    /**
        Converts value to wxAny, if possible. Return @true if successful.
    */
    virtual bool GetAny(wxAny* any) const;

    /**
        Returns the string type of the data.
    */
    virtual wxString GetType() const = 0;

    /**
        If the data is a wxObject returns a pointer to the objects wxClassInfo
        structure, if the data isn't a wxObject the method returns @NULL.
    */
    virtual wxClassInfo* GetValueClassInfo();

    /**
        Increases reference count. Note that initially wxVariantData has
        reference count of 1.
    */
    void IncRef();

    /**
        Reads the data from @a stream.
    */
    virtual bool Read(istream& stream);

    /**
        Reads the data from @a string.
    */
    virtual bool Read(wxString& string);

    /**
        Writes the data to @a stream.
    */
    virtual bool Write(ostream& stream) const;
    /**
        Writes the data to @a string.
    */
    virtual bool Write(wxString& string) const;
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_rtti */
//@{

/**
    This macro returns a pointer to the data stored in @a var (wxVariant) cast
    to the type @a classname if the data is of this type (the check is done
    during the run-time) or @NULL otherwise.

    @header{wx/variant.h}

    @see @ref overview_rtti, wxDynamicCast()
*/
#define wxGetVariantCast(var, classname)

//@}

