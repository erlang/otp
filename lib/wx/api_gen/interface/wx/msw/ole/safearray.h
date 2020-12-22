/////////////////////////////////////////////////////////////////////////////
// Name:        msw/ole/automtn.h
// Purpose:     interface of wxSafeArray
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxSafeArray<varType>

    wxSafeArray<varType> is wxWidgets wrapper for working with MS Windows @c
    SAFEARRAY used in Component Object Model (COM) and OLE Automation APIs.

    It also has convenience functions for converting between @c SAFEARRAY and
    wxVariant with list type or wxArrayString.

    wxSafeArray is a template class which must be created with an appropriate
    type matching the underlying @c VARIANT type (such as @c VT_VARIANT or @c
    VT_BSTR).

    See wxVariantDataSafeArray documentation for examples of using it.

    @onlyfor{wxmsw}
    @since 3.0

    @library{wxcore}
    @category{data}

    @see wxAutomationObject, wxVariantDataSafeArray, wxVariant    
*/
template <VARTYPE varType>
class wxSafeArray<varType>
{
public:

    /**
        The default constructor.
    */
    wxSafeArray();

    /**
        The destructor unlocks and destroys the owned @c SAFEARRAY.
    */
    ~wxSafeArray();

    /**
        Creates and locks a zero-based one-dimensional @c SAFEARRAY with the
        given number of elements.
    */
    bool Create(size_t count);

    /**
        Creates and locks a @c SAFEARRAY.

        See @c SafeArrayCreate() in MSDN documentation for more information.
    */
    bool Create(SAFEARRAYBOUND* bound, size_t dimensions);

    /**
        Creates a zero-based one-dimensional @c SAFEARRAY from wxVariant
        with the list type.

        Can be called only for wxSafeArray<@c VT_VARIANT>.
    */
    bool CreateFromListVariant(const wxVariant& variant);

    /**
        Creates a zero-based one-dimensional @c SAFEARRAY from wxArrayString.

        Can be called only for wxSafeArray<@c VT_BSTR>.
    */
    bool CreateFromArrayString(const wxArrayString& strings);

    /**
        Attaches and locks an existing @c SAFEARRAY.

        The array must have the same @c VARTYPE as this wxSafeArray was
        instantiated with.
    */
    bool Attach(SAFEARRAY* array);

    /**
        Unlocks the owned @c SAFEARRAY, returns it and gives up its ownership.
    */
    SAFEARRAY* Detach();

    /**
        Unlocks and destroys the owned @c SAFEARRAY.
    */
    void Destroy();

    /**
        Returns @true if it has a valid @c SAFEARRAY.
    */
    bool HasArray() const { return m_array != NULL; }

    /**
        Returns the number of dimensions.
    */
    size_t GetDim() const;

    /**
        Returns lower bound for dimension @a dim in @a bound.

        Dimensions start at @c 1.
    */
    bool GetLBound(size_t dim, long& bound) const;

    /**
        Returns upper bound for dimension @a dim in @a bound.

        Dimensions start at @c 1.
    */
    bool GetUBound(size_t dim, long& bound) const;

    /**
        Returns element count for dimension @a dim. Dimensions start at @c 1.
    */
    size_t GetCount(size_t dim) const;

    /**
        Change the value of the specified element.

        @a indices have the same row-column order as @c rgIndices i
        @c SafeArrayPutElement(), i.e., the right-most dimension is
        <tt>rgIndices[0]</tt> and the left-most dimension is stored at
        <tt>rgIndices[</tt>GetDim()<tt> – 1]</tt>.

        @a element must be of type matching @c varType this wxSafeArray was
        created with. For example, wxString for wxSafeArray<@c VT_BSTR>,
        wxVariant for wxSafeArray<@c VT_VARIANT>, or @c double for
        wxSafeArray<@c VT_R8>.
    */
    bool SetElement(long* indices, const externT& element);

    /**
        Retrieve the value of the specified element.

        @a indices have the same row-column order as @c rgIndices in
        @c SafeArrayGetElement(), i.e., the right-most dimension is
        <tt>rgIndices[0]</tt> and the left-most dimension is stored at
        <tt>rgIndices[</tt>GetDim()<tt> – 1]</tt>.

        @a element must be of type matching @c varType this wxSafeArray was
        created with. For example, wxString for wxSafeArray<@c VT_BSTR>,
        wxVariant for wxSafeArray<@c VT_VARIANT>, or @c double for
        wxSafeArray<@c VT_R8>.
    */
    bool GetElement(long* indices, externT& element) const;

    /**
        Converts the array to a wxVariant with the list type, regardless of the
        underlying @c SAFEARRAY type.

        If the array is multidimensional, it is flattened using the algorithm
        originally employed in wxConvertOleToVariant().
    */
    bool ConvertToVariant(wxVariant& variant) const;

    /**
        Converts an array to wxArrayString.

        Can be called only for wxSafeArray<@c VT_BSTR>. If the array is
        multidimensional, it is flattened using the algorithm originally
        employed in wxConvertOleToVariant().
    */
    bool ConvertToArrayString(wxArrayString& strings) const;

    /**
        Converts @a psa to wxVariant.

        @see wxSafeArray<varType>::ConvertToVariant(wxVariant&) const
    */
    static bool ConvertToVariant(SAFEARRAY* psa, wxVariant& variant);

    /**
        Converts @a psa to wxArrayString.

        @see wxSafeArray<varType>::ConvertToArrayString(wxArrayString&) const

    */
    static bool ConvertToArrayString(SAFEARRAY* psa, wxArrayString& strings);
};
