/////////////////////////////////////////////////////////////////////////////
// Name:        msw/ole/automtn.h
// Purpose:     interface of wxAutomationObject
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Automation object creation flags.

    These flags can be used with wxAutomationObject::GetInstance().

    @since 2.9.2
*/
enum wxAutomationInstanceFlags
{
    /**
        Only use the existing instance, never create a new one.

        This flag can be used to forbid the creation of a new instance if none
        is currently running.
     */
    wxAutomationInstance_UseExistingOnly = 0,

    /**
        Create a new instance if there are no existing ones.

        This flag corresponds to the default behaviour of
        wxAutomationObject::GetInstance() and means that if getting an existing
        instance failed, we should call wxAutomationObject::CreateInstance() to
        create a new one.
     */
    wxAutomationInstance_CreateIfNeeded = 1,

    /**
        Do not show an error message if no existing instance is currently
        running.

        All other errors will still be reported as usual.
     */
    wxAutomationInstance_SilentIfNone = 2
};

/**
    @class wxAutomationObject

    The @b wxAutomationObject class represents an OLE automation object containing
    a single data member,
    an @c IDispatch pointer. It contains a number of functions that make it easy to
    perform
    automation operations, and set and get properties. The class makes heavy use of
    the wxVariant class.

    The usage of these classes is quite close to OLE automation usage in Visual
    Basic. The API is
    high-level, and the application can specify multiple properties in a single
    string. The following example
    gets the current Microsoft Excel instance, and if it exists, makes the active cell bold.

    @code
    wxAutomationObject excelObject;

    if ( excelObject.GetInstance("Excel.Application") )
        excelObject.PutProperty("ActiveCell.Font.Bold", true);
    @endcode

    Note that this class obviously works under Windows only.

    @onlyfor{wxmsw}

    @library{wxcore}
    @category{data}

    @see wxVariant, wxVariantDataCurrency, wxVariantDataErrorCode, wxVariantDataSafeArray
*/
class wxAutomationObject : public wxObject
{
public:
    /**
        Constructor, taking an optional @c IDispatch pointer which will be released when
        the
        object is deleted.
    */
    wxAutomationObject(WXIDISPATCH* dispatchPtr = NULL);

    /**
        Destructor. If the internal @c IDispatch pointer is non-null, it will be released.
    */
    ~wxAutomationObject();

    //@{
    /**
        Calls an automation method for this object. The first form takes a method name,
        number of
        arguments, and an array of variants. The second form takes a method name and
        zero to six
        constant references to variants. Since the variant class has constructors for
        the basic
        data types, and C++ provides temporary objects automatically, both of the
        following lines
        are syntactically valid:
        @code
        wxVariant res = obj.CallMethod("Sum", wxVariant(1.2), wxVariant(3.4));
        wxVariant res = obj.CallMethod("Sum", 1.2, 3.4);
        @endcode

        Note that @a method can contain dot-separated property names, to save the
        application
        needing to call GetProperty() several times using several temporary objects. For
        example:
        @code
        object.CallMethod("ActiveWorkbook.ActiveSheet.Protect", "MyPassword");
        @endcode
    */
    wxVariant CallMethod(const wxString& method, int noArgs,
                         wxVariant args[]) const;
    wxVariant CallMethod(const wxString& method, ... ) const;
    //@}

    /**
        Creates a new object based on the @a progID, returning @true if the object was
        successfully created,
        or @false if not.

        @see GetInstance()
    */
    bool CreateInstance(const wxString& progId) const;

    /**
        Checks if the object is in a valid state.

        Returns @true if the object was successfully initialized or @false if
        it has no valid @c IDispatch pointer.

        @see GetDispatchPtr()
     */
    bool IsOk() const;

    /**
        Gets the @c IDispatch pointer.

        Notice that the return value of this function is an untyped pointer but
        it can be safely cast to @c IDispatch.
    */
    WXIDISPATCH* GetDispatchPtr() const;

    /**
        Retrieves the current object associated with the specified ProgID, and
        attaches the @c IDispatch pointer to this object.

        If attaching to an existing object failed and @a flags includes
        ::wxAutomationInstance_CreateIfNeeded flag, a new object will be created.
        Otherwise this function will normally log an error message which may be
        undesirable if the object may or may not exist. The
        ::wxAutomationInstance_SilentIfNone flag can be used to prevent the error
        from being logged in this case.

        Returns @true if a pointer was successfully retrieved, @false
        otherwise.

        Note that this cannot cope with two instances of a given OLE object being
        active simultaneously,
        such as two copies of Microsoft Excel running. Which object is referenced cannot
        currently be specified.

        @param progId COM ProgID, e.g. "Excel.Application"
        @param flags The creation flags (this parameters was added in wxWidgets
            2.9.2)

        @see CreateInstance()
    */
    bool GetInstance(const wxString& progId,
                     int flags = wxAutomationInstance_CreateIfNeeded) const;

    /**
        Retrieves a property from this object, assumed to be a dispatch pointer, and
        initialises @a obj with it.
        To avoid having to deal with @c IDispatch pointers directly, use this function in
        preference
        to GetProperty() when retrieving objects
        from other objects.
        Note that an @c IDispatch pointer is stored as a @c void* pointer in wxVariant
        objects.

        @see GetProperty()
    */
    bool GetObject(wxAutomationObject& obj, const wxString& property,
                   int noArgs = 0,
                   wxVariant args[] = NULL) const;

    //@{
    /**
        Gets a property value from this object. The first form takes a property name,
        number of
        arguments, and an array of variants. The second form takes a property name and
        zero to six
        constant references to variants. Since the variant class has constructors for
        the basic
        data types, and C++ provides temporary objects automatically, both of the
        following lines
        are syntactically valid:
        @code
        wxVariant res = obj.GetProperty("Range", wxVariant("A1"));
        wxVariant res = obj.GetProperty("Range", "A1");
        @endcode

        Note that @a property can contain dot-separated property names, to save the
        application
        needing to call GetProperty several times using several temporary objects.

        @see GetObject(), PutProperty()
    */
    wxVariant GetProperty(const wxString& property, int noArgs,
                          wxVariant args[]) const;
    wxVariant GetProperty(const wxString& property, ... ) const;
    //@}

    /**
        This function is a low-level implementation that allows access to the @c IDispatch
        Invoke function.
        It is not meant to be called directly by the application, but is used by other
        convenience functions.

        @param member
            The member function or property name.
        @param action
            Bitlist: may contain @c DISPATCH_PROPERTYPUT, @c DISPATCH_PROPERTYPUTREF,
            @c DISPATCH_PROPERTYGET, @c DISPATCH_METHOD.
        @param retValue
            Return value (ignored if there is no return value)
        @param noArgs
            Number of arguments in args or ptrArgs.
        @param args
            If non-null, contains an array of variants.
        @param ptrArgs
            If non-null, contains an array of constant pointers to variants.

        @return @true if the operation was successful, @false otherwise.

        @remarks Two types of argument array are provided, so that when possible
                 pointers are used for efficiency.
    */
    bool Invoke(const wxString& member, int action,
                wxVariant& retValue, int noArgs,
                wxVariant args[],
                const wxVariant* ptrArgs[] = 0) const;

    //@{
    /**
        Puts a property value into this object. The first form takes a property name,
        number of
        arguments, and an array of variants. The second form takes a property name and
        zero to six
        constant references to variants. Since the variant class has constructors for
        the basic
        data types, and C++ provides temporary objects automatically, both of the
        following lines
        are syntactically valid:
        @code
        obj.PutProperty("Value", wxVariant(23));
        obj.PutProperty("Value", 23);
        @endcode

        Note that @a property can contain dot-separated property names, to save the
        application
        needing to call GetProperty() several times using several temporary objects.
    */
    bool PutProperty(const wxString& property, int noArgs,
                     wxVariant args[]);
    bool PutProperty(const wxString& property, ... );
    //@}

    /**
        Sets the @c IDispatch pointer, does not check if there is already one.
        You may need to cast from @c IDispatch* to @c WXIDISPATCH* when calling this function.
    */
    void SetDispatchPtr(WXIDISPATCH* dispatchPtr);

    /**
        Returns the locale identifier used in automation calls.

        The default is @c LOCALE_SYSTEM_DEFAULT but the objects obtained by
        GetObject() inherit the locale identifier from the one that created
        them.

        @since 2.9.5
    */
    WXLCID GetLCID() const;

    /**
        Sets the locale identifier to be used in automation calls performed by
        this object.

        The default value is @c LOCALE_SYSTEM_DEFAULT.

        Notice that any automation objects created by this one inherit the same
        @c LCID.

        @since 2.9.5
    */
    void SetLCID(WXLCID lcid);

    /**
        Returns the flags used for conversions between wxVariant and OLE
        @c VARIANT, see #wxOleConvertVariantFlags.

        The default value is ::wxOleConvertVariant_Default for compatibility but
        it can be changed using SetConvertVariantFlags().

        Notice that objects obtained by GetObject() inherit the flags from the
        one that created them.

        @since 3.0
    */
    long GetConvertVariantFlags() const;

    /**
        Sets the flags used for conversions between wxVariant and OLE @c VARIANT,
        see #wxOleConvertVariantFlags.

        The default value is ::wxOleConvertVariant_Default.

        @since 3.0
    */
    void SetConvertVariantFlags(long flags);
};
