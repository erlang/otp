/////////////////////////////////////////////////////////////////////////////
// Name:        wx/scopedarray.h
// Purpose:     interface of wxScopedArray
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxScopedArray

    This is a simple scoped smart pointer array implementation that is similar to
    the Boost smart pointers (see http://www.boost.org/) but rewritten to
    use macros instead.

    @b Example:

    Below is an example of using a wxWidgets scoped smart pointer and pointer array.

    @code
    class MyClass { ... };

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

    <b>Declaring new smart pointer types:</b>
    @code
    wxDECLAR_SCOPED_ARRAY( TYPE,        // type of the values
                           CLASSNAME ); // name of the class
    @endcode

    A smart pointer holds a pointer to an object (which must be complete when
    wxDEFINE_SCOPED_ARRAY() is called).

    The memory used by the object is deleted when the smart pointer goes out of
    scope. The first argument of the macro is the pointer type, the second is the
    name of the new smart pointer class being created. Below we will use wxScopedArray
    to represent the scoped pointer array class, but the user may create the class with
    any legal name.

    @library{wxbase}
    @category{smartpointers}

    @see wxScopedPtr
*/
class wxScopedArray
{
public:
    /**
        Creates the smart pointer with the given pointer or none if @NULL.  On
        compilers that support it, this uses the explicit keyword.
    */
    wxScopedArray(type*  T = NULL);

    /**
        This operator gets the pointer stored in the smart pointer or returns @NULL if
        there is none.
    */
    const T* get();

    /**
        This operator acts like the standard [] indexing operator for C++ arrays.  The
        function does not do bounds checking.
    */
    const T& operator [](long int i);

    /**
        Deletes the currently held pointer and sets it to 'p' or to @NULL if no
        arguments are specified. This function does check to make sure that the
        pointer you are assigning is not the same pointer that is already stored.
    */
    reset(T* p  = NULL);

    /**
        Swap the pointer inside the smart pointer with @a ot. The pointer being swapped
        must be of the same type (hence the same class name).
    */
    swap(wxScopedArray& ot);
};

/**
    A scoped array template class.

    This class is similar to boost scoped_array class:
    http://www.boost.org/doc/libs/1_37_0/libs/smart_ptr/scoped_array.htm

    Notice that objects of this class intentionally cannot be copied.

    @library{wxbase}
    @category{smartpointers}
 */
template <class T>
class wxScopedArray
{
public:
    /// The type of the array elements.
    typedef T element_type;

    /**
        Constructor takes ownership of the given array.

        If @a array is @NULL, reset() must presumably be called later.

        @param array
            An array allocated using @c new[] or @NULL.
     */
    explicit wxScopedArray(T * array = NULL);

    /**
        Constructor allocating a new array of the specified size.

        @param count
            The number of elements to allocate.

        @since 3.1.0
     */
    explicit wxScopedArray(size_t count);

    /// Destructor destroy the array.
    ~wxScopedArray();

    /**
        Conversion to a boolean expression (in a variant which is not
        convertible to anything but a boolean expression).

        If this class contains a valid array it will return @true, if it contains
        a @NULL pointer it will return @false.
    */
    operator unspecified_bool_type() const;

    /**
        Change the array pointer stored.

        The previously stored array is deleted.

        @param array
            An array allocated using @c new[] or @NULL.
     */
    void reset(T *array = NULL);

    /**
        Return the n-th element of the array.

        Must not be called if the array has no valid pointer.
     */
    T& operator[](size_t n) const;

    /**
        Return the array pointer.

        The returned pointer may be @NULL. It must not be deleted by the
        caller, call @c reset(NULL) instead.
     */
    T *get() const;

    /// Swaps the contents of this array with another one.
    void swap(wxScopedArray &other);
};
