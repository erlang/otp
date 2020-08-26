/////////////////////////////////////////////////////////////////////////////
// Name:        any.h
// Purpose:     interface of wxAny
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxAny

    The wxAny class represents a container for any type. Its value
    can be changed at run time, possibly to a different type of value.

    wxAny is a backwards-incompatible (but convertible) successor class for
    wxVariant, essentially doing the same thing in a more modern, template-
    based manner and with transparent support for any user data type.

    Some pseudo-code'ish example of use with arbitrary user data:

    @code
    void SomeFunction()
    {
        MyClass myObject;
        wxAny any = myObject;

        // Do something
        // ...

        // Let's do a sanity check to make sure that any still holds
        // data of correct type.
        if ( any.CheckType<MyClass>() )
        {
            // Thank goodness, still a correct type.
            MyClass myObject2 = any.As<MyClass>();
        }
        else
        {
            // Something has gone horribly wrong!
            wxFAIL();
        }
    }
    @endcode

    When compared to wxVariant, there are various internal implementation
    differences as well. For instance, wxAny only allocates separate data
    object in heap for large objects (i.e. ones with size more than
    WX_ANY_VALUE_BUFFER_SIZE, which at the time of writing is 16 bytes).

    @note When performing conversions between strings and floating point
        numbers, the representation of numbers in C locale is always used.
        I.e. @code wxAny("1.23").GetAs<double>() @endcode will always work,
        even if the current locale uses comma as decimal separator.

    @library{wxbase}
    @category{data}

    @see wxAnyValueType, wxVariant, @ref overview_cpp_rtti_disabled
*/
class wxAny
{
public:
    /**
        Default constructor. It seeds the object with a null value.
    */
    wxAny();

    /**
        Constructs wxAny from data.
    */
    template<typename T>
    wxAny(const T& value);

    /**
        Constructs wxAny from another wxAny.
    */
    wxAny(const wxAny& any);

    /**
        Constructs wxAny, converting value from wxVariant.

        @remarks Because of this conversion, it is not usually possible to
                 have wxAny that actually holds a wxVariant. If wxVariant
                 cannot be converted to a specific data type, wxAny will then
                 hold and manage reference to wxVariantData* similar to how
                 wxVariant does.
    */
    wxAny(const wxVariant& variant);

    /**
        Destructor.
    */
    ~wxAny();

    /**
        This template function converts wxAny into given type. In most cases
        no type conversion is performed, so if the type is incorrect an
        assertion failure will occur.

        @remarks For convenience, conversion is done when T is wxString. This
                 is useful when a string literal (which are treated as
                 const char* and const wchar_t*) has been assigned to wxAny.
    */
    template<typename T>
    T As() const;

    /**
        Use this template function for checking if this wxAny holds
        a specific C++ data type.

        @see wxAnyValueType::CheckType()
    */
    template<typename T>
    bool CheckType() const;

    /**
        Template function that retrieves and converts the value of this
        wxAny to the type that T* value is.

        @return Returns @true if conversion was successful.
    */
    template<typename T>
    bool GetAs(T* value) const;

    /**
        Specialization of GetAs() that allows conversion of wxAny into
        wxVariant.

        @return Returns @true if conversion was successful. Conversion usually
                only fails if variant used custom wxVariantData that did not
                implement the wxAny to wxVariant conversion functions.
    */
    bool GetAs(wxVariant* value) const;

    /**
        Returns the value type as wxAnyValueType instance.

        @remarks You cannot reliably test whether two wxAnys are of
                 same value type by simply comparing return values
                 of wxAny::GetType(). Instead, use wxAny::HasSameType().

        @see HasSameType()
    */
    const wxAnyValueType* GetType() const;

    /**
        Returns @true if this and another wxAny have the same
        value type.
    */
    bool HasSameType(const wxAny& other) const;

    /**
        Tests if wxAny is null (that is, whether there is no data).
    */
    bool IsNull() const;

    /**
        Makes wxAny null (that is, clears it).
    */
    void MakeNull();

    //@{
    /**
        @name Assignment operators
    */
    template<typename T>
    wxAny& operator=(const T &value);
    wxAny& operator=(const wxAny &any);
    wxAny& operator=(const wxVariant &variant);
    //@}

    //@{
    /**
        @name Equality operators

        @remarks Generic template-based comparison operators have not been
                provided for various code consistency reasons, so for custom
                data types you have do something like this:

                @code
                if ( any.CheckType<MyClass*>() &&
                     any.As<MyClass*>() == myObjectPtr )
                {
                    // Do something if any stores myObjectPtr
                }
                @endcode
    */
    bool operator==(signed char value) const;
    bool operator==(signed short value) const;
    bool operator==(signed int value) const;
    bool operator==(signed long value) const;
    bool operator==(wxLongLong_t value) const;
    bool operator==(unsigned char value) const;
    bool operator==(unsigned short value) const;
    bool operator==(unsigned int value) const;
    bool operator==(unsigned long value) const;
    bool operator==(wxULongLong_t value) const;
    bool operator==(float value) const;
    bool operator==(double value) const;
    bool operator==(bool value) const;
    bool operator==(const char* value) const;
    bool operator==(const wchar_t* value) const;
    bool operator==(const wxString& value) const;
    //@}

    //@{
    /**
        @name Inequality operators
    */
    bool operator!=(signed char value) const;
    bool operator!=(signed short value) const;
    bool operator!=(signed int value) const;
    bool operator!=(signed long value) const;
    bool operator!=(wxLongLong_t value) const;
    bool operator!=(unsigned char value) const;
    bool operator!=(unsigned short value) const;
    bool operator!=(unsigned int value) const;
    bool operator!=(unsigned long value) const;
    bool operator!=(wxULongLong_t value) const;
    bool operator!=(float value) const;
    bool operator!=(double value) const;
    bool operator!=(bool value) const;
    bool operator!=(const char* value) const;
    bool operator!=(const wchar_t* value) const;
    bool operator!=(const wxString& value) const;
    //@}
};


/**
    Size of the wxAny value buffer.
*/
enum
{
    WX_ANY_VALUE_BUFFER_SIZE = 16
};

/**
    Type for buffer within wxAny for holding data.
*/
union wxAnyValueBuffer
{
    void*   m_ptr;
    wxByte  m_buffer[WX_ANY_VALUE_BUFFER_SIZE];
};


/**
    @class wxAnyValueType

    wxAnyValueType is base class for value type functionality for C++ data
    types used with wxAny. Usually the default template will create a
    satisfactory wxAnyValueType implementation for a data type, but
    sometimes you may need to add some customization. To do this you will need
    to add specialized template of wxAnyValueTypeImpl<>. Often your only
    need may be to add dynamic type conversion which would be done like
    this:

    @code
        template<>
        class wxAnyValueTypeImpl<MyClass> :
            public wxAnyValueTypeImplBase<MyClass>
        {
            WX_DECLARE_ANY_VALUE_TYPE(wxAnyValueTypeImpl<MyClass>)
        public:
            wxAnyValueTypeImpl() :
                wxAnyValueTypeImplBase<MyClass>() { }
            virtual ~wxAnyValueTypeImpl() { }

            virtual bool ConvertValue(const wxAnyValueBuffer& src,
                                      wxAnyValueType* dstType,
                                      wxAnyValueBuffer& dst) const
            {
                // GetValue() is a static member function implemented
                // in wxAnyValueTypeImplBase<>.
                MyClass value = GetValue(src);

                // TODO: Convert value from src buffer to destination
                //       type and buffer. If cannot be done, return
                //       false. This is a simple sample.
                if ( dstType->CheckType<wxString>() )
                {
                    wxString s = value.ToString();
                    wxAnyValueTypeImpl<wxString>::SetValue(s, dst);
                }
                else
                {
                    return false;
                }
            }
        };

        //
        // Following must be placed somewhere in your source code
        WX_IMPLEMENT_ANY_VALUE_TYPE(wxAnyValueTypeImpl<MyClass>)
    @endcode

    wxAnyValueTypeImplBase<> template, from which we inherit in the above
    example, contains the bulk of the default wxAnyValueTypeImpl<> template
    implementation, and as such allows you to easily add some minor
    customization.

    If you need a have complete control over the type interpretation, you
    will need to derive a class directly from wxAnyValueType, like this:

    @code
        template <>
        class wxAnyValueTypeImpl<MyClass> : public wxAnyValueType
        {
            WX_DECLARE_ANY_VALUE_TYPE(wxAnyValueTypeImpl<MyClass>)
        public:
            virtual void DeleteValue(wxAnyValueBuffer& buf) const
            {
                // TODO: Free the data in buffer
                // It is important to clear the buffer like this
                // at the end of DeleteValue().
                buf.m_ptr = NULL;
            }

            virtual void CopyBuffer(const wxAnyValueBuffer& src,
                                    wxAnyValueBuffer& dst) const
            {
                // TODO: Copy value from one buffer to another.
                //       dst is already uninitialized and does not
                //       need to be freed.
            }

            virtual bool ConvertValue(const wxAnyValueBuffer& src,
                                      wxAnyValueType* dstType,
                                      wxAnyValueBuffer& dst) const
            {
                // TODO: Convert value from src buffer to destination
                //       type and buffer.
            }

            //
            // Following static functions must be implemented
            //

            static void SetValue(const T& value,
                                 wxAnyValueBuffer& buf)
            {
                // TODO: Store value into buf.
            }

            static const T& GetValue(const wxAnyValueBuffer& buf)
            {
                // TODO: Return reference to value stored in buffer.
            }
        };

        //
        // Following must be placed somewhere in your source code
        WX_IMPLEMENT_ANY_VALUE_TYPE(wxAnyValueTypeImpl<MyClass>)

    @endcode

    @library{wxbase}
    @category{data}

    @see wxAny
*/
class wxAnyValueType
{
public:
    /**
        Default constructor.
    */
    wxAnyValueType();

    /**
        Destructor.
    */
    virtual ~wxAnyValueType();

    /**
        Use this template function for checking if wxAnyValueType represents
        a specific C++ data type.

        @see wxAny::CheckType()
    */
    template <typename T>
    bool CheckType() const;

    /**
        Convert value into buffer of different type. Return false if
        not possible.
    */
    virtual bool ConvertValue(const wxAnyValueBuffer& src,
                              wxAnyValueType* dstType,
                              wxAnyValueBuffer& dst) const = 0;

    /**
        Implement this for buffer-to-buffer copy.

        @param src
            This is the source data buffer.

        @param dst
            This is the destination data buffer that is in either
            uninitialized or freed state.
    */
    virtual void CopyBuffer(const wxAnyValueBuffer& src,
                            wxAnyValueBuffer& dst) const = 0;

    /**
        This function is called every time the data in wxAny
        buffer needs to be freed.
    */
    virtual void DeleteValue(wxAnyValueBuffer& buf) const = 0;

    /**
        This function is used for internal type matching.
    */
    virtual bool IsSameType(const wxAnyValueType* otherType) const = 0;
};
