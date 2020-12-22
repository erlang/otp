/////////////////////////////////////////////////////////////////////////////
// Name:        object.h
// Purpose:     interface of wxRefCounter
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/** @class wxObjectRefData

    This class is just a typedef to wxRefCounter and is used by wxObject.

    Derive classes from this to store your own data in wxObject-derived
    classes. When retrieving information from a wxObject's reference data,
    you will need to cast to your own derived class.

    Below is an example illustrating how to store reference counted
    data in a class derived from wxObject including copy-on-write
    semantics.

    @section objectrefdata_example Example

    @code
    // include file
    // ------------

    class MyCar : public wxObject
    {
    public:
        MyCar() { }
        MyCar( int price );

        bool IsOk() const { return m_refData != NULL; }

        bool operator == ( const MyCar& car ) const;
        bool operator != (const MyCar& car) const { return !(*this == car); }

        void SetPrice( int price );
        int GetPrice() const;

    protected:
        virtual wxObjectRefData *CreateRefData() const;
        virtual wxObjectRefData *CloneRefData(const wxObjectRefData *data) const;

        wxDECLARE_DYNAMIC_CLASS(MyCar);
    };


    // implementation
    // --------------

    // the reference data class is typically a private class only visible in the
    // implementation source file of the refcounted class.
    class MyCarRefData : public wxObjectRefData
    {
    public:
        MyCarRefData()
        {
            m_price = 0;
        }

        MyCarRefData( const MyCarRefData& data )
            : wxObjectRefData()
        {
            // copy refcounted data; this is usually a time- and memory-consuming operation
            // and is only done when two (or more) MyCar instances need to unshare a
            // common instance of MyCarRefData
            m_price = data.m_price;
        }

        bool operator == (const MyCarRefData& data) const
        {
            return m_price == data.m_price;
        }

    private:
        // in real world, reference counting is usually used only when
        // the wxObjectRefData-derived class holds data very memory-consuming;
        // in this example the various MyCar instances may share a MyCarRefData
        // instance which however only takes 4 bytes for this integer!
        int m_price;
    };


    #define M_CARDATA ((MyCarRefData *)m_refData)
    wxIMPLEMENT_DYNAMIC_CLASS(MyCar, wxObject);

    MyCar::MyCar( int price )
    {
        // here we init the MyCar internal data:
        m_refData = new MyCarRefData();
        M_CARDATA->m_price = price;
    }

    wxObjectRefData *MyCar::CreateRefData() const
    {
        return new MyCarRefData;
    }

    wxObjectRefData *MyCar::CloneRefData(const wxObjectRefData *data) const
    {
        return new MyCarRefData(*(MyCarRefData *)data);
    }

    bool MyCar::operator == ( const MyCar& car ) const
    {
        if (m_refData == car.m_refData)
            return true;
        if (!m_refData || !car.m_refData)
            return false;

        // here we use the MyCarRefData::operator==() function.
        // Note however that this comparison may be very slow if the
        // reference data contains a lot of data to be compared.
        return ( *(MyCarRefData*)m_refData == *(MyCarRefData*)car.m_refData );
    }

    void MyCar::SetPrice( int price )
    {
        // since this function modifies one of the MyCar internal property,
        // we need to be sure that the other MyCar instances which share the
        // same MyCarRefData instance are not affected by this call.
        // I.e. it's very important to call UnShare() in all setters of
        // refcounted classes!
        UnShare();

        M_CARDATA->m_price = price;
    }

    int MyCar::GetPrice() const
    {
        wxCHECK_MSG( IsOk(), -1, "invalid car" );

        return M_CARDATA->m_price;
    }
    @endcode


    @library{wxbase}
    @category{rtti}

    @see wxObject, wxObjectDataPtr<T>, @ref overview_refcount
*/
typedef wxRefCounter wxObjectRefData;


/**
    @class wxRefCounter

    This class is used to manage reference-counting providing a simple
    interface and a counter. wxRefCounter can be easily used together
    with wxObjectDataPtr<T> to ensure that no calls to wxRefCounter::DecRef()
    are missed - thus avoiding memory leaks.

    wxObjectRefData is a typedef to wxRefCounter and is used as the
    built-in reference counted storage for wxObject-derived classes.

    @library{wxbase}
    @category{rtti}

    @see wxObject, wxObjectRefData, wxObjectDataPtr<T>, @ref overview_refcount
*/
class wxRefCounter
{
protected:
    /**
        Destructor.

        It's declared @c protected so that wxRefCounter instances
        will never be destroyed directly but only as result of a DecRef() call.
    */
    virtual ~wxRefCounter();

public:
    /**
        Default constructor. Initialises the internal reference count to 1.
    */
    wxRefCounter();

    /**
        Decrements the reference count associated with this shared data and, if
        it reaches zero, destroys this instance of wxRefCounter releasing its
        memory.

        Please note that after calling this function, the caller should
        absolutely avoid to use the pointer to this instance since it may not be
        valid anymore.
    */
    void DecRef();

    /**
        Returns the reference count associated with this shared data.

        When this goes to zero during a DecRef() call, the object will auto-free itself.
    */
    int GetRefCount() const;

    /**
        Increments the reference count associated with this shared data.
    */
    void IncRef();
};



/**
    @class wxObject

    This is the root class of many of the wxWidgets classes.

    It declares a virtual destructor which ensures that destructors get called
    for all derived class objects where necessary.

    wxObject is the hub of a dynamic object creation scheme, enabling a program
    to create instances of a class only knowing its string class name, and to
    query the class hierarchy.

    The class contains optional debugging versions of @b new and @b delete, which
    can help trace memory allocation and deallocation problems.

    wxObject can be used to implement @ref overview_refcount "reference counted"
    objects, such as wxPen, wxBitmap and others
    (see @ref overview_refcount_list "this list").
    See wxRefCounter and @ref overview_refcount for more info about
    reference counting.

    @library{wxbase}
    @category{rtti}

    @see wxClassInfo, @ref overview_debugging, @ref overview_refcount,
         wxObjectDataRef, wxObjectDataPtr<T>
*/
class wxObject
{
public:

    /**
        Default ctor; initializes to @NULL the internal reference data.
    */
    wxObject();

    /**
        Copy ctor.

        Sets the internal wxObject::m_refData pointer to point to the same
        instance of the wxObjectRefData-derived class pointed by @c other and
        increments the refcount of wxObject::m_refData.
    */
    wxObject(const wxObject& other);

    /**
        Destructor.

        Performs dereferencing, for those objects that use reference counting.
    */
    virtual ~wxObject();

    /**
        This virtual function is redefined for every class that requires run-time
        type information, when using the ::wxDECLARE_CLASS macro (or similar).
    */
    virtual wxClassInfo* GetClassInfo() const;

    /**
        Returns the wxObject::m_refData pointer, i.e.\ the data referenced by this object.

        @see Ref(), UnRef(), wxObject::m_refData, SetRefData(), wxObjectRefData
    */
    wxObjectRefData* GetRefData() const;

    /**
        Determines whether this class is a subclass of (or the same class as)
        the given class.

        Example:

        @code
        bool tmp = obj->IsKindOf(wxCLASSINFO(wxFrame));
        @endcode

        @param info
            A pointer to a class information object, which may be obtained
            by using the ::wxCLASSINFO macro.

        @return @true if the class represented by info is the same class as this
                 one or is derived from it.
    */
    bool IsKindOf(const wxClassInfo* info) const;

    /**
        Returns @true if this object has the same data pointer as @a obj.

        Notice that @true is returned if the data pointers are @NULL in both objects.

        This function only does a @e shallow comparison, i.e. it doesn't compare
        the objects pointed to by the data pointers of these objects.

        @see @ref overview_refcount
    */
    bool IsSameAs(const wxObject& obj) const;

    /**
        Makes this object refer to the data in @a clone.

        @param clone
            The object to 'clone'.

        @remarks First this function calls UnRef() on itself to decrement
                 (and perhaps free) the data it is currently referring to.
                 It then sets its own wxObject::m_refData to point to that of @a clone,
                 and increments the reference count inside the data.

        @see UnRef(), SetRefData(), GetRefData(), wxObjectRefData
    */
    void Ref(const wxObject& clone);

    /**
        Sets the wxObject::m_refData pointer.

        @see Ref(), UnRef(), GetRefData(), wxObjectRefData
    */
    void SetRefData(wxObjectRefData* data);

    /**
        Decrements the reference count in the associated data, and if it is zero,
        deletes the data.

        The wxObject::m_refData member is set to @NULL.

        @see Ref(), SetRefData(), GetRefData(), wxObjectRefData
    */
    void UnRef();

    /**
        This is the same of AllocExclusive() but this method is public.
    */
    void UnShare();

    /**
        The @e delete operator is defined for debugging versions of the library only,
        when the identifier @c __WXDEBUG__ is defined.

        It takes over memory deallocation, allowing wxDebugContext operations.
    */
    void operator delete(void *buf);

    /**
        The @e new operator is defined for debugging versions of the library only, when
        the identifier @c __WXDEBUG__ is defined.

        It takes over memory allocation, allowing wxDebugContext operations.
    */
    void* operator new(size_t size, const wxString& filename = NULL, int lineNum = 0);

protected:
    /**
        Ensure that this object's data is not shared with any other object.

        If we have no data, it is created using CreateRefData();
        if we have shared data (i.e. data with a reference count greater than 1),
        it is copied using CloneRefData(); otherwise nothing is done (the data
        is already present and is not shared by other object instances).

        If you use this function you should make sure that you override the
        CreateRefData() and CloneRefData() functions in your class otherwise
        an assertion will fail at runtime.
    */
    void AllocExclusive();

    /**
        Creates a new instance of the wxObjectRefData-derived class specific to
        this object and returns it.

        This is usually implemented as a one-line call:
        @code
        wxObjectRefData *MyObject::CreateRefData() const
        {
            return new MyObjectRefData;
        }
        @endcode
    */
    virtual wxObjectRefData *CreateRefData() const;

    /**
        Creates a new instance of the wxObjectRefData-derived class specific to
        this object and initializes it copying @a data.

        This is usually implemented as a one-line call:
        @code
        wxObjectRefData *MyObject::CloneRefData(const wxObjectRefData *data) const
        {
            // rely on the MyObjectRefData copy ctor:
            return new MyObjectRefData(*(MyObjectRefData *)data);
        }
        @endcode
    */
    virtual wxObjectRefData *CloneRefData(const wxObjectRefData *data) const;

    /**
        Pointer to an object which is the object's reference-counted data.

        @see Ref(), UnRef(), SetRefData(), GetRefData(), wxObjectRefData
    */
    wxObjectRefData* m_refData;
};



/**
    @class wxClassInfo

    This class stores meta-information about classes.

    Instances of this class are not generally defined directly by an application,
    but indirectly through use of macros such as ::wxDECLARE_DYNAMIC_CLASS and
    ::wxIMPLEMENT_DYNAMIC_CLASS.

    @library{wxbase}
    @category{rtti}

    @see @ref overview_rtti_classinfo, wxObject
*/
class wxClassInfo
{
public:
    /**
        Constructs a wxClassInfo object.

        The supplied macros implicitly construct objects of this class, so there is no
        need to create such objects explicitly in an application.
    */
    wxClassInfo(const wxChar* className,
                const wxClassInfo* baseClass1,
                const wxClassInfo* baseClass2,
                int size, wxObjectConstructorFn fn);

    /**
        Creates an object of the appropriate kind.

        @return @NULL if the class has not been declared dynamically creatable
                 (typically, this happens for abstract classes).
    */
    wxObject* CreateObject() const;

    /**
        Finds the wxClassInfo object for a class with the given @a name.
    */
    static wxClassInfo* FindClass(const wxString& className);

    /**
        Returns the name of the first base class (@NULL if none).
    */
    const wxChar* GetBaseClassName1() const;

    /**
        Returns the name of the second base class (@NULL if none).
    */
    const wxChar* GetBaseClassName2() const;

    /**
        Returns the string form of the class name.
    */
    const wxChar* GetClassName() const;

    /**
        Returns the size of the class.
    */
    int GetSize() const;

    /**
        Returns @true if this class info can create objects of the associated class.
    */
    bool IsDynamic() const;

    /**
        Returns @true if this class is a kind of (inherits from) the given class.
    */
    bool IsKindOf(const wxClassInfo* info) const;
};



/**

    This is an helper template class primarily written to avoid memory leaks because
    of missing calls to wxRefCounter::DecRef() and wxObjectRefData::DecRef().

    Despite the name this template can actually be used as a smart pointer for any
    class implementing the reference counting interface which only consists of the two
    methods @b T::IncRef() and @b T::DecRef().

    The difference to wxSharedPtr<T> is that wxObjectDataPtr<T> relies on the reference
    counting to be in the class pointed to, where instead wxSharedPtr<T> implements the
    reference counting itself.

    Below is an example illustrating how to implement reference counted
    data using wxRefCounter and wxObjectDataPtr<T> with copy-on-write
    semantics.

    @section objectdataptr_example Example

    @code
    class MyCarRefData: public wxRefCounter
    {
    public:
        MyCarRefData( int price = 0 ) : m_price(price) { }
        MyCarRefData( const MyCarRefData& data ) : m_price(data.m_price) { }

        void SetPrice( int price )  { m_price = price; }
        int GetPrice() const        { return m_price; }

    protected:
        int m_price;
    };

    class MyCar
    {
    public:
        // initializes this MyCar assigning to the
        // internal data pointer a new instance of MyCarRefData
        MyCar( int price = 0 ) : m_data( new MyCarRefData(price) )
        {
        }

        MyCar& operator =( const MyCar& tocopy )
        {
            // shallow copy: this is just a fast copy of pointers; the real
            // memory-consuming data which typically is stored inside
            // MyCarRefData is not copied here!
            m_data = tocopy.m_data;
            return *this;
        }

        bool operator == ( const MyCar& other ) const
        {
            if (m_data.get() == other.m_data.get())
                return true; // this instance and the 'other' one share the
                             // same MyCarRefData data...

            return (m_data.GetPrice() == other.m_data.GetPrice());
        }

        void SetPrice( int price )
        {
            // make sure changes to this class do not affect other instances
            // currently sharing our same refcounted data:
            UnShare();

            m_data->SetPrice( price );
        }

        int GetPrice() const
        {
           return m_data->GetPrice();
        }

        wxObjectDataPtr<MyCarRefData> m_data;

    protected:
        void UnShare()
        {
            if (m_data->GetRefCount() == 1)
                return;

            m_data.reset( new MyCarRefData( *m_data ) );
        }
    };
    @endcode


    @library{wxbase}
    @category{rtti,smartpointers}

    @see wxObject, wxObjectRefData, @ref overview_refcount, wxSharedPtr<T>,
         wxScopedPtr<T>, wxWeakRef<T>
*/
template <class T>
class wxObjectDataPtr<T>
{
public:
    /**
        Constructor.

        @a ptr is a pointer to the reference counted object to which this class points.
        If @a ptr is not NULL @b T::IncRef() will be called on the object.
    */
    wxObjectDataPtr<T>(T* ptr = NULL);

    /**
        This copy constructor increases the count of the reference counted object to
        which @a tocopy points and then this class will point to, as well.
    */
    wxObjectDataPtr<T>(const wxObjectDataPtr<T>& tocopy);


    /**
        Decreases the reference count of the object to which this class points.
    */
    ~wxObjectDataPtr<T>();

    /**
        Gets a pointer to the reference counted object to which this class points.
    */
    T* get() const;

    /**
        Reset this class to ptr which points to a reference counted object and
        calls T::DecRef() on the previously owned object.
    */
    void reset(T *ptr);

    /**
        Release the owned pointer, making caller responsible for decrementing
        its reference count.

        This method should be used only for interoperating with the existing
        code working with raw pointers, typically when returning a raw pointer
        from a function.

        After calling this function, this object becomes invalid, i.e. it
        doesn't hold any valid pointer value any more.

        @since 3.1.4
    */
    T* release();

    /**
        Conversion to a boolean expression (in a variant which is not
        convertible to anything but a boolean expression).

        If this class contains a valid pointer it will return @true, if it contains
        a @NULL pointer it will return @false.
    */
    operator unspecified_bool_type() const;

    /**
        Returns a reference to the object.

        If the internal pointer is @NULL this method will cause an assert in debug mode.
    */
    T& operator*() const;

    /**
        Returns a pointer to the reference counted object to which this class points.

        If this the internal pointer is @NULL, this method will assert in debug mode.
    */
    T* operator->() const;

    //@{
    /**
        Assignment operator.
    */
    wxObjectDataPtr<T>& operator=(const wxObjectDataPtr<T>& tocopy);
    wxObjectDataPtr<T>& operator=(T* ptr);
    //@}
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_rtti */
//@{

/**
    Returns a pointer to the wxClassInfo object associated with this class.

    @header{wx/object.h}
*/
#define wxCLASSINFO( className )

/**
    Used inside a class declaration to declare that the class should be
    made known to the class hierarchy, but objects of this class cannot be created
    dynamically.

    @header{wx/object.h}

    Example:

    @code
    class wxCommand: public wxObject
    {
        wxDECLARE_ABSTRACT_CLASS(wxCommand);

    private:
        ...
    public:
        ...
    };
    @endcode
*/
#define wxDECLARE_ABSTRACT_CLASS( className )

/**
    Used inside a class declaration to make the class known to wxWidgets RTTI
    system and also declare that the objects of this class should be
    dynamically creatable from run-time type information. Notice that this
    implies that the class should have a default constructor, if this is not
    the case consider using wxDECLARE_ABSTRACT_CLASS().

    @header{wx/object.h}

    Example:

    @code
    class wxFrame: public wxWindow
    {
        wxDECLARE_DYNAMIC_CLASS(wxFrame);

    private:
        const wxString& frameTitle;
    public:
        ...
    };
    @endcode
*/
#define wxDECLARE_DYNAMIC_CLASS( className )

/**
    Used inside a class declaration to declare that the class should be made
    known to the class hierarchy, but objects of this class cannot be created
    dynamically.

    The same as wxDECLARE_ABSTRACT_CLASS().

    @header{wx/object.h}
*/
#define wxDECLARE_CLASS( className )

/**
    Used in a C++ implementation file to complete the declaration of a class
    that has run-time type information.

    @header{wx/object.h}

    Example:

    @code
    wxIMPLEMENT_ABSTRACT_CLASS(wxCommand, wxObject);

    wxCommand::wxCommand()
    {
        ...
    }
    @endcode
*/
#define wxIMPLEMENT_ABSTRACT_CLASS( className, baseClassName )

/**
    Used in a C++ implementation file to complete the declaration of a class
    that has run-time type information and two base classes.

    @header{wx/object.h}
*/
#define wxIMPLEMENT_ABSTRACT_CLASS2( className, baseClassName1, baseClassName2 )

/**
    Used in a C++ implementation file to complete the declaration of a class
    that has run-time type information, and whose instances can be created
    dynamically.

    @header{wx/object.h}

    Example:

    @code
    wxIMPLEMENT_DYNAMIC_CLASS(wxFrame, wxWindow);

    wxFrame::wxFrame()
    {
        ...
    }
    @endcode
*/
#define wxIMPLEMENT_DYNAMIC_CLASS( className, baseClassName )

/**
    Used in a C++ implementation file to complete the declaration of a class
    that has run-time type information, and whose instances can be created
    dynamically. Use this for classes derived from two base classes.

    @header{wx/object.h}
*/
#define wxIMPLEMENT_DYNAMIC_CLASS2( className, baseClassName1, baseClassName2 )

/**
    Synonym for wxIMPLEMENT_ABSTRACT_CLASS().

    Please prefer to use the more clear, if longer,
    ::wxIMPLEMENT_ABSTRACT_CLASS in the new code.

    @header{wx/object.h}
*/
#define wxIMPLEMENT_CLASS( className, baseClassName )

/**
    Synonym for wxIMPLEMENT_ABSTRACT_CLASS2().

    Please prefer to use the more clear, if longer,
    ::wxIMPLEMENT_ABSTRACT_CLASS2 in the new code.

    @header{wx/object.h}
*/
#define wxIMPLEMENT_CLASS2( className, baseClassName1, baseClassName2 )

/**
    This case doesn’t correspond to any standard cast but exists solely to make
    casts which possibly result in a truncation of an integer value more
    readable.

    @header{wx/defs.h}
*/
#define wx_truncate_cast(T, x)

/**
    This macro expands into <tt>const_cast<classname *>(ptr)</tt> if the compiler
    supports const_cast or into an old, C-style cast, otherwise.

    @header{wx/defs.h}

    @see wxDynamicCast(), wxStaticCast()
*/
#define wxConstCast( ptr, classname )

/**
    This macro returns the pointer @e ptr cast to the type @e classname * if
    the pointer is of this type (the check is done during the run-time) or
    @NULL otherwise. Usage of this macro is preferred over obsoleted
    wxObject::IsKindOf() function.

    The @e ptr argument may be @NULL, in which case @NULL will be returned.

    @header{wx/object.h}

    Example:

    @code
    wxWindow *win = wxWindow::FindFocus();
    wxTextCtrl *text = wxDynamicCast(win, wxTextCtrl);
    if ( text )
    {
        // a text control has the focus...
    }
    else
    {
        // no window has the focus or it is not a text control
    }
    @endcode

    @see @ref overview_rtti, wxDynamicCastThis(), wxConstCast(), wxStaticCast()
*/
#define wxDynamicCast( ptr, classname )

/**
    This macro is equivalent to <tt>wxDynamicCast(this, classname)</tt> but the latter provokes
    spurious compilation warnings from some compilers (because it tests whether
    @c this pointer is non-@NULL which is always true), so this macro should be
    used to avoid them.

    @header{wx/object.h}

    @see wxDynamicCast()
*/
#define wxDynamicCastThis( classname )

/**
    This macro checks that the cast is valid in debug mode (an assert failure
    will result if wxDynamicCast(ptr, classname) == @NULL) and then returns the
    result of executing an equivalent of <tt>static_cast<classname *>(ptr)</tt>.

    @header{wx/object.h}

    @see wxDynamicCast(), wxConstCast()
*/
#define wxStaticCast( ptr, classname )

/**
    Creates and returns an object of the given class, if the class has been
    registered with the dynamic class system using DECLARE... and IMPLEMENT...
    macros.

    @header{wx/object.h}
*/
wxObject *wxCreateDynamicObject(const wxString& className);

//@}

/** @addtogroup group_funcmacro_debug */
//@{

/**
    This is defined in debug mode to be call the redefined new operator
    with filename and line number arguments. The definition is:

    @code
    #define WXDEBUG_NEW new(__FILE__,__LINE__)
    @endcode

    In non-debug mode, this is defined as the normal new operator.

    @header{wx/object.h}
*/
#define WXDEBUG_NEW( arg )

//@}

