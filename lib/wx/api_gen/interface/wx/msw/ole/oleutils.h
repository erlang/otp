/////////////////////////////////////////////////////////////////////////////
// Name:        msw/ole/oleutils.h
// Purpose:     interface of OLE helpers
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    Flags used for conversions between wxVariant and OLE @c VARIANT.

    These flags are used by wxAutomationObject for its wxConvertOleToVariant()
    calls. They can be obtained by wxAutomationObject::GetConvertVariantFlags()
    and set by wxAutomationObject::SetConvertVariantFlags().

    @since 3.0

    @see wxVariantDataSafeArray
*/
enum wxOleConvertVariantFlags
{
    /**
        Default value.
    */
    wxOleConvertVariant_Default = 0,

    /**
        If this flag is used, SAFEARRAYs contained in OLE @c VARIANTs will be
        returned as wxVariants with wxVariantDataSafeArray type instead of
        wxVariants with the list type containing the (flattened) SAFEARRAY's
        elements.
    */
    wxOleConvertVariant_ReturnSafeArrays = 1
};


/**
    @class wxVariantDataCurrency

    This class represents a thin wrapper for Microsoft Windows @c CURRENCY type.

    It is used for converting between wxVariant and OLE @c VARIANT
    with type set to @c VT_CURRENCY. When wxVariant stores
    wxVariantDataCurrency, it returns "currency" as its type.

    An example of setting and getting @c CURRENCY value to and from wxVariant:
    @code
    CURRENCY cy;
    wxVariant variant;

    // set wxVariant to currency type
    if ( SUCCEEDED(VarCyFromR8(123.45, &cy)) )  // set cy to 123.45
    {
        variant.SetData(new wxVariantDataCurrency(cy));

        // or instead of the line above you could write:
        // wxVariantDataCurrency wxCy;
        // wxCy.SetValue(cy);
        // variant.SetData(wxCy.Clone());
    }

    // get CURRENCY value from wxVariant
    if ( variant.GetType() == "currency" )
    {
        wxVariantDataCurrency*
            wxCy = wxDynamicCastVariantData(variant.GetData(), wxVariantDataCurrency);
        cy = wxCy->GetValue();
    }
    @endcode

    @onlyfor{wxmsw}
    @since 2.9.5

    @library{wxcore}
    @category{data}

    @see wxAutomationObject, wxVariant, wxVariantData, wxVariantDataErrorCode
*/
class wxVariantDataCurrency : public wxVariantData
{
public:
    /**
        Default constructor initializes the object to 0.0.
    */
    wxVariantDataCurrency();

    /**
        Constructor from CURRENCY.
    */
    wxVariantDataCurrency(CURRENCY value);

    /**
        Returns the stored CURRENCY value.
    */
    CURRENCY GetValue() const;

    /**
        Sets the stored value to @a value.
    */
    void SetValue(CURRENCY value);

    /**
        Returns @true if @a data is of wxVariantDataCurency type
        and contains the same CURRENCY value.
    */
    virtual bool Eq(wxVariantData& data) const;

    /**
        Fills the provided string with the textual representation of this
        object.

        The implementation of this method uses @c VarBstrFromCy() Windows API
        function with @c LOCALE_USER_DEFAULT.
    */
    virtual bool Write(wxString& str) const;

    /**
        Returns a copy of itself.
    */
    wxVariantData* Clone() const;

    /**
        Returns "currency".
    */
    virtual wxString GetType() const;

    /**
        Converts the value of this object to wxAny.
    */
    virtual bool GetAsAny(wxAny* any) const;
};


/**
    @class wxVariantDataErrorCode

    This class represents a thin wrapper for Microsoft Windows @c SCODE type
    (which is the same as @c HRESULT).

    It is used for converting between a wxVariant and OLE @c VARIANT with type set
    to @c VT_ERROR. When wxVariant stores wxVariantDataErrorCode, it returns
    "errorcode" as its type. This class can be used for returning error codes
    of automation calls or exchanging values with other applications: e.g.
    Microsoft Excel returns VARIANTs with @c VT_ERROR type for cell values with
    errors (one of XlCVError constants, displayed as e.g. "#DIV/0!" or "#REF!"
    there) etc. See wxVariantDataCurrency for an example of how to  exchange
    values between wxVariant and a native type not directly supported by it.

    @onlyfor{wxmsw}
    @since 2.9.5

    @library{wxcore}
    @category{data}

    @see wxAutomationObject, wxVariant, wxVariantData, wxVariantDataCurrency
*/
class wxVariantDataErrorCode : public wxVariantData
{
public:
    /**
        Constructor initializes the object to @a value or @c S_OK if no value was
        passed.
    */
    wxVariantDataErrorCode(SCODE value = S_OK);

    /**
        Returns the stored @c SCODE value.
    */
    SCODE GetValue() const;

    /**
        Set the stored value to @a value.
    */
    void SetValue(SCODE value);

    /**
        Returns @true if @a data is of wxVariantDataErrorCode type
        and contains the same @c SCODE value.
    */
    virtual bool Eq(wxVariantData& data) const;

    /**
        Fills the provided string with the textual representation of this
        object.

        The error code is just a number, so it's output as such.
    */
    virtual bool Write(wxString& str) const;

    /**
        Returns a copy of itself.
    */
    wxVariantData* Clone() const;

    /**
        Returns "errorcode".
    */
    virtual wxString GetType() const { return wxS("errorcode"); }

    /**
        Converts the value of this object to wxAny.
    */
    virtual bool GetAsAny(wxAny* any) const;
};

/**
    @class wxVariantDataSafeArray

    This class stores @c SAFEARRAY in a wxVariant. It can be used
    to pass arrays having more than one dimension with wxAutomationObject methods.

    When wxVariant stores wxVariantDataSafeArray, it returns "safearray" as its type.

    wxVariantDataSafeArray does NOT manage the @c SAFEARRAY it points to.
    If you want to pass it to a wxAutomationObject as a parameter:
        -# Create and fill a @c SAFEARRAY.
        -# Assign the @c SAFEARRAY pointer to it and store it in a wxVariant.
        -# Call a wxAutomationObject method (such as CallMethod() or PutProperty()) with the wxVariant as a parameter.
        -# wxAutomationObject will destroy the array after the automation call.

    An example of creating a two-dimensional @c SAFEARRAY containing <tt>VARIANT</tt>s
    and storing it in a wxVariant, using a utility class wxSafeArray<varType>.
    @code
    const size_t dimensions = 2;
    const long rowCount = 1000;
    const long columnCount = 20;

    SAFEARRAYBOUND bounds[dimensions];
    wxSafeArray<VT_VARIANT> safeArray;

    bounds[0].lLbound = 0; // elements start at 0
    bounds[0].cElements = rowCount;
    bounds[1].lLbound = 0; // elements start at 0
    bounds[1].cElements = columnCount;

    if ( !safeArray.Create(bounds, dimensions) )
        return false;

    long indices[dimensions];

    for ( long row = 0; row < rowCount; ++row )
    {
        indices[0] = row;

        for ( long column = 0; column < columnCount; ++column )
        {
            indices[1] = column;
            if ( !safeArray.SetElement(indices, wxString::Format("R%u C%u", row, column)) )
               return false;
        }
    }

    range.PutProperty("Value", wxVariant(new wxVariantDataSafeArray(safeArray.Detach())));
    @endcode

    If you want to receive a @c SAFEARRAY in a wxVariant as a result of an wxAutomationObject
    call:
        -# Call wxAutomationObject::SetConvertVariantFlags() with parameter
           ::wxOleConvertVariant_ReturnSafeArrays (otherwise the data would be
           sent as a flattened one-dimensional list).
        -# Call a wxAutomationObject method (such as CallMethod() or GetProperty()).
        -# Retrieve the @c SAFEARRAY from the returned wxVariant.
        -# Process the data in the @c SAFEARRAY.
        -# Destroy the @c SAFEARRAY when you no longer need it.

    The following example shows how to process a two-dimensional @c SAFEARRAY
    with @c VT_VARIANT type received from a wxAutomationObject call,
    using a utility class wxSafeArray<varType>.
    @code
    const size_t dimensions = 2;

    wxVariant result;

    range.SetConvertVariantFlags(wxOleConvertVariant_ReturnSafeArrays);
    result = range.GetProperty("Value");

    if ( !result.IsType("safearray") )
       return false;

    wxSafeArray<VT_VARIANT> safeArray;
    wxVariantDataSafeArray* const
        sa = wxStaticCastVariantData(result.GetData(), wxVariantDataSafeArray);

    if ( !safeArray.Attach(sa->GetValue()) )
    {
        if ( !safeArray.HasArray() )
            ::SafeArrayDestroy(sa->GetValue()); // we have to dispose the SAFEARRAY ourselves
        return false;
    }

    if ( safeArray.GetDim() != dimensions ) // we are expecting 2 dimensions
        return false; // SAFEARRAY will be disposed by safeArray's dtor

    long rowStart, columnStart;
    long rowCount, columnCount;
    long indices[dimensions];
    wxVariant value;

    // get start indices and item counts for rows and columns
    safeArray.GetLBound(1, rowStart);
    safeArray.GetLBound(2, columnStart);
    safeArray.GetUBound(1, rowCount);
    safeArray.GetUBound(2, columnCount);

    for ( long row = rowStart; row <= rowCount; ++row )
    {
        indices[0] = row;

        for ( long column = columnStart; column <= columnCount; ++column )
        {
            indices[1] = column;
            if ( !safeArray.GetElement(indices, value) )
                return false;
            // do something with value
        }
    }
    // SAFEARRAY will be disposed by safeArray's dtor
    @endcode

    @onlyfor{wxmsw}
    @since 2.9.5

    @library{wxcore}
    @category{data}

    @see wxAutomationObject, wxSafeArray<varType>, wxVariant, wxVariantData
*/
class wxVariantDataSafeArray : public wxVariantData
{
public:
    /**
        Constructor initializes the object to @a value.
    */
    explicit wxVariantDataSafeArray(SAFEARRAY* value = NULL);

    /**
        Returns the stored array.
    */
    SAFEARRAY* GetValue() const;

    /**
        Set the stored array.
    */
    void SetValue(SAFEARRAY* value);

    /**
        Returns @true if @a data is of wxVariantDataSafeArray type
        and contains the same SAFEARRAY* value.
    */
    virtual bool Eq(wxVariantData& data) const;

    /**
        Fills the provided string with the textual representation of this
        object.

        Only the address of @c SAFEARRAY pointer is output.
    */
    virtual bool Write(wxString& str) const;

    /**
        Returns a copy of itself.
    */
    wxVariantData* Clone() const;

    /**
        Returns "safearray".
    */
    virtual wxString GetType() const;

    /**
        Converts the value of this object to wxAny.
    */
    virtual bool GetAsAny(wxAny* any) const;
};
