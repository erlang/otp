/////////////////////////////////////////////////////////////////////////////
// Name:        wx/scopedptr.h
// Purpose:     interface of wxScopedPtr
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxScopedPtr

    This is a simple scoped smart pointer implementation that is similar to
    the Boost smart pointers (see http://www.boost.org) but rewritten
    to use macros instead.

    Since wxWidgets 2.9.0 there is also a templated version of this class
    with the same name. See wxScopedPtr<T>.

    A smart pointer holds a pointer to an object. The memory used by the object is
    deleted when the smart pointer goes out of scope. This class is different from
    the @c std::auto_ptr<> in so far as it doesn't provide copy constructor
    nor assignment operator. This limits what you can do with it but is much less
    surprising than the "destructive copy" behaviour of the standard class.

    @b Example:

    Below is an example of using a wxWidgets scoped smart pointer and pointer array.

    @code
    class MyClass{ ... };

    // declare a smart pointer to a MyClass called wxMyClassPtr
    wxDECLARE_SCOPED_PTR(MyClass, wxMyClassPtr)
    // declare a smart pointer to an array of chars
    wxDECLARE_SCOPED_ARRAY(char, wxCharArray)

    ...

    // define the first pointer class, must be complete
    wxDEFINE_SCOPED_PTR(MyClass, wxMyClassPtr)
    // define the second pointer class
    wxDEFINE_SCOPED_ARRAY(char, wxCharArray)

    // create an object with a new pointer to MyClass
    wxMyClassPtr theObj(new MyClass());
    // reset the pointer (deletes the previous one)
    theObj.reset(new MyClass());

    // access the pointer
    theObj->MyFunc();

    // create an object with a new array of chars
    wxCharArray theCharObj(new char[100]);

    // access the array
    theCharObj[0] = "!";
    @endcode

    @section scopedptr_newpointers Declaring new smart pointer types

    To declare the smart pointer class @c CLASSNAME containing pointer to
    a (possibly incomplete) type @c TYPE you should use
    @code
        wxDECLARE_SCOPED_PTR( TYPE,        // type of the values
                              CLASSNAME ); // name of the class
    @endcode
    And later, when @c TYPE is fully defined, you must also use
    @code
        wxDEFINE_SCOPED_PTR( TYPE, CLASSNAME );
    @endcode
    to implement the scoped pointer class.

    The first argument of these macro is the pointer type, the second is the name
    of the new smart pointer class being created. Below we will use wxScopedPtr
    to represent the scoped pointer class, but the user may create the class with
    any legal name.

    Alternatively, if you don't have to separate the point of declaration and
    definition of this class and if you accept the standard naming convention,
    that is that the scoped pointer for the class @c Foo is called @c FooPtr,
    you can use a single macro which replaces two macros above:
    @code
        wxDEFINE_SCOPED_PTR_TYPE( TYPE );
    @endcode
    Once again, in this cass @c CLASSNAME will be @c TYPEPtr.

    @library{wxbase}
    @category{smartpointers}

    @see wxScopedArray
*/
class wxScopedPtr
{
public:
    /**
        Creates the smart pointer with the given pointer or none if @NULL.

        On compilers that support it, this uses the explicit keyword.
    */
    explicit wxScopedPtr(type* T = NULL);

    /**
        Destructor frees the pointer help by this object if it is not @NULL.
    */
    ~wxScopedPtr();

    /**
        This operator gets the pointer stored in the smart pointer or returns
        @NULL if there is none.
    */
    T* get() const;

    /**
        This operator works like the standard C++ pointer operator to return the object
        being pointed to by the pointer.

        If the internal pointer is @NULL this method will cause an assert in debug mode.
    */
    T& operator *() const;

    /**
        Smart pointer member access. Returns pointer to its object.

        If the internal pointer is @NULL this method will cause an assert in debug mode.
    */
    T* operator ->() const;

    /**
        Returns the currently hold pointer and resets the smart pointer object to
        @NULL.

        @remarks
        After a call to this function the caller is responsible for deleting the
        pointer.
    */
    T* release();

    /**
        Deletes the currently held pointer and sets it to @a p or to @NULL if no
        arguments are specified.

        @note
        This function does check to make sure that the pointer you are assigning
        is not the same pointer that is already stored.
    */
    reset(T* p  = NULL);

    /**
        Swap the pointer inside the smart pointer with @a other. The pointer being
        swapped must be of the same type (hence the same class name).
    */
    swap(wxScopedPtr& other);
};

/**
    @class wxScopedTiedPtr

    This is a variation on the topic of wxScopedPtr. This class is also a smart pointer
    but in addition it "ties" the pointer value to another variable. In other words,
    during the life time of this class the value of that variable is set to be the same
    as the value of the pointer itself and it is reset to its old value when the object
    is destroyed. This class is especially useful when converting the existing code
    (which may already store the pointers value in some variable) to the smart pointers.

    @library{wxbase}
    @category{smartpointers}
*/
class wxScopedTiedPtr : public wxScopedPtr
{
public:
    /**
        Constructor creates a smart pointer initialized with @a ptr and stores
        @a ptr in the location specified by @a ppTie which must not be @NULL.
    */
    wxScopedTiedPtr(T** ppTie, T* ptr);

    /**
        Destructor frees the pointer help by this object and restores the value
        stored at the tied location (as specified in the @ref wxScopedTiedPtr() constructor)
        to the old value.

        @warning
        This location may now contain an uninitialized value if it hadn't been
        initialized previously, in particular don't count on it magically being @NULL!
    */
    ~wxScopedTiedPtr();
};



/**
    A scoped pointer template class.

    It is the template version of the old-style @ref wxScopedPtr "scoped pointer macros".

    Notice that objects of this class intentionally cannot be copied.

    @library{wxbase}
    @category{smartpointers}

    @see wxSharedPtr<T>, wxWeakRef<T>
*/
template<typename T>
class wxScopedPtr<T>
{
public:
    /**
        Constructor takes ownership of the pointer.

        @param ptr
            Pointer allocated with @c new or @NULL.
    */
    wxScopedPtr(T* ptr = NULL);

    /**
        Destructor deletes the pointer.
    */
    ~wxScopedPtr();

    /**
        Returns pointer to object or @NULL.
    */
    T* get() const;

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
        Smart pointer member access. Returns pointer to object.

        If the internal pointer is @NULL this method will cause an assert in debug mode.
    */
    T* operator->() const;

    /**
        Releases the current pointer and returns it.

        @remarks
        Afterwards the caller is responsible for deleting
        the data contained in the scoped pointer before.
    */
    T* release();

    /**
        Reset pointer to the value of @a ptr.
        The previous pointer will be deleted.
    */
    void reset(T* ptr = NULL);

    /**
        Swaps pointers.
    */
    void swap(wxScopedPtr<T>& ot);
};

