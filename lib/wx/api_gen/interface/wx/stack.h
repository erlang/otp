/////////////////////////////////////////////////////////////////////////////
// Name:        wx/stack.h
// Purpose:     interface of wxStack<T>
// Author:      Vadim Zeitlin
// Copyright:   (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    wxStack<T> is similar to @c std::stack and can be used exactly like it.

    If wxWidgets is compiled in STL mode, wxStack will just be a typedef to
    @c std::stack but the advantage of this class is that it is also available
    on the (rare) platforms where STL is not, so using it makes the code
    marginally more portable. If you only target the standard desktop
    platforms, please always use @c std::stack directly instead.

    The main difference of this class compared to the standard version is that
    it always uses wxVector<T> as the underlying container and doesn't allow
    specifying an alternative container type. Another missing part is that the
    comparison operators between wxStacks are not currently implemented. Other
    than that, this class is exactly the same as @c std::stack, so please refer
    to the STL documentation for further information.

    @nolibrary
    @category{containers}

    @see @ref overview_container, wxVector<T>

    @since 2.9.2
*/
template <typename T>
class wxStack<T>
{
public:
    /// Type of the underlying container used.
    typedef wxVector<T> container_type;

    /// Type returned by size() method.
    typedef typename container_type::size_type size_type;

    /// Type of the elements stored in the stack.
    typedef typename container_type::value_type value_type;


    /**
        Stack can be created either empty or initialized with the contents of
        an existing compatible container.
     */
    //@{
    wxStack();
    explicit wxStack(const container_type& cont);
    //@}

    /// Return whether the stack is currently empty.
    bool empty() const;

    /// Return the number of elements in the stack.
    size_type size() const;

    /**
        Return the element on top of the stack.
     */
    //@{
    value_type& top();
    const value_type& top();
    //@}

    /// Adds an element to the stack.
    void push(const value_type& val);

    /// Removes the element currently on top of the stack.
    void pop();
};
