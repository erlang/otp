/////////////////////////////////////////////////////////////////////////////
// Name:        weakref.h
// Purpose:     interface of wxWeakRefDynamic<T>, wxWeakRef<T>
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    wxWeakRefDynamic<T> is a template class for weak references that is used in
    the same way as wxWeakRef<T>. The only difference is that wxWeakRefDynamic
    defaults to using @c dynamic_cast for establishing the object reference
    (while wxWeakRef defaults to @c static_cast).

    So, wxWeakRef will detect a type mismatch during compile time and will
    have a little better run-time performance. The role of wxWeakRefDynamic
    is to handle objects which derived type one does not know.

    @note wxWeakRef<T> selects an implementation based on the static type of T.
          If T does not have wxTrackable statically, it defaults to a mixed-
          mode operation, where it uses @c dynamic_cast as the last measure
          (if available from the compiler and enabled when building wxWidgets).

    For general cases, wxWeakRef<T> is the better choice.

    For API documentation, see: wxWeakRef<T>.

    @tparam T
        The type to which the smart pointer points to.

    @nolibrary
    @category{smartpointers}
*/
template<typename T>
class wxWeakRefDynamic<T>
{
public:

};



/**
    wxWeakRef<T> is a template class for weak references to wxWidgets objects,
    such as wxEvtHandler, wxWindow and wxObject.
    A weak reference behaves much like an ordinary pointer, but when the object
    pointed is destroyed, the weak reference is automatically reset to a @NULL pointer.

    wxWeakRef<T> can be used whenever one must keep a pointer to an object
    that one does not directly own, and that may be destroyed before the object
    holding the reference.

    wxWeakRef<T> is a small object and the mechanism behind it is fast
    (@b O(1)). So the overall cost of using it is small.

    Example:

    @code
    wxWindow *wnd = new wxWindow( parent, wxID_ANY, "wxWindow" );
    wxWeakRef<wxWindow> wr = wnd;
    wxWindowRef wr2 = wnd;        // Same as above, but using a typedef
    // Do things with window
    wnd->Show( true );
    // Weak ref is used like an ordinary pointer
    wr->Show( false );
    wnd->Destroy();
    // Now the weak ref has been reset, so we don't risk accessing
    // a dangling pointer:
    wxASSERT( wr==NULL );
    @endcode

    wxWeakRef<T> works for any objects that are derived from wxTrackable.
    By default, wxEvtHandler and wxWindow derive from wxTrackable.
    However, wxObject does not, so types like wxFont and wxColour are not
    trackable. The example below shows how to create a wxObject derived class
    that is trackable:

    @code
    class wxMyTrackableObject : public wxObject, public wxTrackable
    {
        // ... other members here
    };
    @endcode

    The following types of weak references are predefined:

    @code
    typedef wxWeakRef<wxEvtHandler>  wxEvtHandlerRef;
    typedef wxWeakRef<wxWindow>      wxWindowRef;
    @endcode

    @tparam T
        The type to which the smart pointer points to.

    @nolibrary
    @category{smartpointers}

    @see wxSharedPtr<T>, wxScopedPtr<T>
*/
template<typename T>
class wxWeakRef<T> : public wxTrackerNode
{
public:
    /// Type of the element stored by this reference.
    typedef T element_type;

    /**
        Constructor. The weak reference is initialized to @e pobj.
    */
    wxWeakRef(T* pobj = NULL);

    /**
        Copy constructor.
    */
    wxWeakRef(const wxWeakRef<T>& wr);

    /**
        Destructor.
    */
    virtual ~wxWeakRef();

    /**
        Called when the tracked object is destroyed. Be default sets
        internal pointer to @NULL.
        You need to call this method if you override it.
    */
    virtual void OnObjectDestroy();

    /**
        Release currently tracked object and rests object reference.
    */
    void Release();

    /**
        Returns pointer to the tracked object or @NULL.
    */
    T* get() const;

    /**
        Release currently tracked object and start tracking the same object as
        the wxWeakRef @e wr.
    */
    T* operator =(wxWeakRef<T>& wr);

    /**
        Implicit conversion to T*.
        Returns pointer to the tracked object or @NULL.
    */
    T* operator*() const;

    /**
        Returns a reference to the tracked object.
        If the internal pointer is @NULL this method will cause an assert in debug mode.
    */
    T& operator*() const;

    /**
        Smart pointer member access. Returns a pointer to the tracked object.
        If the internal pointer is @NULL this method will cause an assert in debug mode.
    */
    T* operator->();

    /**
        Releases the currently tracked object and starts tracking @e pobj.
        A weak reference may be reset by passing @e @NULL as @e pobj.
    */
    T* operator=(T* pobj);
};

