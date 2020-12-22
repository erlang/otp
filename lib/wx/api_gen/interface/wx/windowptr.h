///////////////////////////////////////////////////////////////////////////////
// Name:        interface/wx/windowptr.h
// Purpose:     wxWindowPtr<T> class documentation.
// Author:      Vaclav Slavik
// Created:     2013-09-02
// Copyright:   (c) 2013 Vaclav Slavik <vslavik@fastmail.fm>
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    A reference-counted smart pointer for holding wxWindow instances.

    This specialization of wxSharedPtr<T> is useful for holding
    wxWindow-derived objects. Unlike wxSharedPtr<T> or @c std::shared_ptr<>, it
    doesn't use the delete operator to destroy the value when reference count
    drops to zero, but calls wxWindow::Destroy() to safely destroy the window.

    The template parameter T must be wxWindow or a class derived from it.

    @library{wxcore}
    @category{smartpointers}

    @since 3.0

    @see wxSharedPtr<T>
*/
template<typename T>
class wxWindowPtr<T> : public wxSharedPtr<T>
{
public:
    /// Default constructor.
    wxWindowPtr();

    /**
        Constructor.

        Creates shared pointer from the raw pointer @a ptr and takes ownership
        of it.
    */
    explicit wxWindowPtr(T* ptr);

    /**
        Constructor.

        Creates shared pointer from the raw pointer @a ptr and deleter @a d
        and takes ownership of it.

        @param ptr  The raw pointer.
        @param d    Deleter - a functor that is called instead of delete to
                    free the @a ptr raw pointer when its reference count drops to
                    zero.

    */
    template<typename Deleter>
    explicit wxWindowPtr(T* ptr, Deleter d);

    /// Copy constructor.
    wxWindowPtr(const wxWindowPtr<T>& tocopy);

    /**
        Assignment operator.

        Releases any previously held pointer and creates a reference to @a ptr.
    */
    wxWindowPtr<T>& operator=(T* ptr);

    /**
        Assignment operator.

        Releases any previously held pointer and creates a reference to the
        same object as @a topcopy.
    */
    wxWindowPtr<T>& operator=(const wxWindowPtr<T>& tocopy);

    /**
        Reset pointer to @a ptr.

        If the reference count of the previously owned pointer was 1 it will be deleted.
    */
    void reset(T* ptr = NULL);
};

