/////////////////////////////////////////////////////////////////////////////
// Name:        list.h
// Purpose:     interface of wxList<T>
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    The wxList<T> class provides linked list functionality.

    This class has been rewritten to be type safe and to provide the full API of
    the STL std::list container and should be used like it.
    The exception is that wxList<T> actually stores pointers and therefore its
    iterators return pointers and not references to the actual objects in the list
    (see example below) and @e value_type is defined as @e T*.
    wxList<T> destroys an object after removing it only if wxList<T>::DeleteContents
    has been called.

    wxList<T> is not a real template and it requires that you declare and define
    each wxList<T> class in your program. This is done with @e WX_DECLARE_LIST
    and @e WX_DEFINE_LIST macros (see example). We hope that we'll be able to
    provide a proper template class providing both the STL @c std::list and the old
    wxList API in the future.

    Please refer to the STL @c std::list documentation (see http://www.cppreference.com/wiki/stl/list/start)
    for further information on how to use the class.
    Below we documented both the supported STL and the legacy API
    that originated from the old wxList class and which can still be used alternatively
    for the same class.

    Note that if you compile wxWidgets in STL mode (@c wxUSE_STL defined as 1)
    then wxList<T> will actually derive from @c std::list and just add a legacy
    compatibility layer for the old wxList class.

    @code
    // this part might be in a header or source (.cpp) file
    class MyListElement
    {
        ... // whatever
    };

    // this macro declares and partly implements MyList class
    WX_DECLARE_LIST(MyListElement, MyList);

    ...

    // the only requirement for the rest is to be AFTER the full declaration of
    // MyListElement (for WX_DECLARE_LIST forward declaration is enough), but
    // usually it will be found in the source file and not in the header

    #include <wx/listimpl.cpp>
    WX_DEFINE_LIST(MyList);


    MyList list;
    MyListElement element;
    list.Append(&element);     // ok
    list.Append(17);           // error: incorrect type

    // let's iterate over the list in STL syntax
    MyList::iterator iter;
    for (iter = list.begin(); iter != list.end(); ++iter)
    {
        MyListElement *current = *iter;

        ...process the current element...
    }

    // the same with the legacy API from the old wxList class
    MyList::compatibility_iterator node = list.GetFirst();
    while (node)
    {
        MyListElement *current = node->GetData();

        ...process the current element...

        node = node->GetNext();
    }
    @endcode

    For compatibility with previous versions wxList and wxStringList classes are
    still defined, but their usage is deprecated and they will disappear in the
    future versions completely.
    The use of the latter is especially discouraged as it is not only unsafe but
    is also much less efficient than wxArrayString class.

    @tparam T
        The type stored in the wxList nodes.

    @library{wxbase}
    @category{containers}

    @see wxArray<T>, wxVector<T>, wxNode<T>
*/
template<typename T>
class wxList<T>
{
public:
    /**
        Default constructor.
    */
    wxList<T>();

    /**
        Constructor which initialized the list with an array of @a count elements.
    */
    wxList<T>(size_t count, T* elements[]);

    /**
        Destroys the list, but does not delete the objects stored in the list
        unless you called DeleteContents(@true ).
    */
    ~wxList<T>();

    /**
        Appends the pointer to @a object to the list.
    */
    wxList<T>::compatibility_iterator Append(T* object);

    /**
        Clears the list.
        Deletes the actual objects if DeleteContents( @true ) was called previously.
    */
    void Clear();

    /**
        If @a destroy is @true, instructs the list to call @e delete
        on objects stored in the list whenever they are removed.
        The default is @false.
    */
    void DeleteContents(bool destroy);

    /**
        Deletes the given element referred to by @a iter from the list
        if @a iter is a valid iterator. Returns @true if successful.

        Deletes the actual object if DeleteContents( @true ) was called previously.
    */
    bool DeleteNode(const compatibility_iterator& iter);

    /**
        Finds the given @a object and removes it from the list, returning
        @true if successful.

        Deletes @a object if DeleteContents( @true ) was called previously.
    */
    bool DeleteObject(T* object);

    /**
        Removes element referred to be @a iter.

        Deletes the actual object if DeleteContents( @true ) was called previously.
    */
    void Erase(const compatibility_iterator& iter);

    /**
        Returns the iterator referring to @a object or @NULL if none found.
    */
    wxList<T>::compatibility_iterator Find(T* object) const;

    /**
        Returns the number of elements in the list.
    */
    size_t GetCount() const;

    /**
        Returns the first iterator in the list (@NULL if the list is empty).
    */
    wxList<T>::compatibility_iterator GetFirst() const;

    /**
        Returns the last iterator in the list (@NULL if the list is empty).
    */
    wxList<T>::compatibility_iterator GetLast() const;

    /**
        Returns the index of @a obj within the list or @c wxNOT_FOUND if
        @a obj is not found in the list.
    */
    int IndexOf(T* obj) const;

    /**
        Inserts @a object at the beginning of the list.
    */
    wxList<T>::compatibility_iterator Insert(T* object);

    /**
        Inserts @a object at @a position.
    */
    wxList<T>::compatibility_iterator Insert(size_t position,
                                           T* object);

    /**
        Inserts @a object before the object referred to be @a iter.
    */
    wxList<T>::compatibility_iterator Insert(compatibility_iterator iter,
                                           T* object);

    /**
        Returns @true if the list is empty, @false otherwise.
    */
    bool IsEmpty() const;

    /**
        Returns the iterator referring to the object at the given
        @a index in the list.
    */
    wxList<T>::compatibility_iterator Item(size_t index) const;

    /**
        Check if the object is present in the list.

        @see Find()
    */
    bool Member(T* object) const;

    /**
        @deprecated This function is deprecated, use Item() instead.
    */
    wxList<T>::compatibility_iterator Nth(int n) const;

    /**
        @deprecated This function is deprecated, use wxList::GetCount instead.
        Returns the number of elements in the list.
    */
    int Number() const;

    /**
        Allows the sorting of arbitrary lists by giving a function to compare
        two list elements. We use the system @b qsort function for the actual
        sorting process.
    */
    void Sort(wxSortCompareFunction compfunc);

    /**
       Clears the list and item from @a first to @a last from another list to it.
    */
    void assign(const_iterator first, const const_iterator& last);

    /**
       Clears the list and adds @a n items with value @a v to it.
    */
    void assign(size_type n, const_reference v = value_type());

    /**
        Returns the last item of the list.
    */
    reference back();

    /**
        Returns the last item of the list as a const reference.
    */
    const_reference back() const;

    /**
        Returns an iterator pointing to the beginning of the list.
    */
    iterator begin();

    /**
        Returns a const iterator pointing to the beginning of the list.
    */
    const_iterator begin() const;

    /**
        Removes all items from the list.
    */
    void clear();

    /**
        Returns @e @true if the list is empty.
    */
    bool empty() const;

    /**
        Returns a const iterator pointing at the end of the list.
    */
    const_iterator end() const;

    /**
        Returns a iterator pointing at the end of the list.
    */
    iterator end() const;

    /**
        Erases the given item
    */
    iterator erase(const iterator& it);

    /**
        Erases the items from @e first to @e last.
    */
    iterator erase(const iterator& first,
                   const iterator& last);

    /**
        Returns the first item in the list.
    */
    reference front() const;

    /**
        Returns the first item in the list as a const reference.
    */
    const_reference front() const;

    /**
        Inserts an item at the head of the list
    */
    iterator insert(const iterator& it);

    /**
        Inserts an item at the given position
    */
    void insert(const iterator& it, size_type n);

    /**
        Inserts several items at the given position.
    */
    void insert(const iterator& it, const_iterator first,
                const const_iterator& last);

    /**
        Returns the largest possible size of the list.
    */
    size_type max_size() const;

    /**
        Removes the last item from the list.
    */
    void pop_back();

    /**
        Removes the first item from the list.
    */
    void pop_front();

    /**
        Adds an item to end of the list.
    */
    void push_back(const_reference v = value_type());

    /**
        Adds an item to the front of the list.
    */
    void push_front(const_reference v = value_type());

    /**
        Returns a reverse iterator pointing to the beginning of the
        reversed list.
    */
    reverse_iterator rbegin();

    /**
        Returns a const reverse iterator pointing to the beginning of the
        reversed list.
    */
    const_reverse_iterator rbegin() const;

    /**
        Removes an item from the list.
    */
    void remove(const_reference v);

    /**
        Returns a reverse iterator pointing to the end of the reversed list.
    */
    reverse_iterator rend();

    /**
        Returns a const reverse iterator pointing to the end of the reversed list.
    */
    const_reverse_iterator rend() const;

    /**
        Resizes the list.

        If the list is longer than @a n, then items are removed until the list
        becomes long @a n.
        If the list is shorter than @a n items with the value @a v are appended
        to the list until the list becomes long @a n.
    */
    void resize(size_type n, value_type v = value_type());

    /**
        Reverses the list.
    */
    void reverse();

    /**
        Returns the size of the list.
    */
    size_type size() const;

    /**
        Returns a wxVector holding the list elements.

        @since 2.9.5
    */
    wxVector<T> AsVector() const;
};



/**
    wxNode<T> is the node structure used in linked lists (see wxList) and derived
    classes. You should never use wxNode<T> class directly, however, because it
    works with untyped (@c void *) data and this is unsafe.
    Use wxNode<T>-derived classes which are automatically defined by WX_DECLARE_LIST
    and WX_DEFINE_LIST macros instead as described in wxList documentation
    (see example there).

    Also note that although there is a class called wxNode, it is defined for backwards
    compatibility only and usage of this class is strongly deprecated.

    In the documentation below, the type @c T should be thought of as a
    "template" parameter: this is the type of data stored in the linked list or,
    in other words, the first argument of WX_DECLARE_LIST macro. Also, wxNode is
    written as wxNodeT even though it isn't really a template class -- but it
    helps to think of it as if it were.

    @tparam T
        The type stored in the wxNode.

    @library{wxbase}
    @category{data}

    @see wxList<T>, wxHashTable
*/
template<typename T>
class wxNode<T>
{
public:
    /**
        Retrieves the client data pointer associated with the node.
    */
    T* GetData() const;

    /**
        Retrieves the next node or @NULL if this node is the last one.
    */
    wxNode<T>* GetNext() const;

    /**
        Retrieves the previous node or @NULL if this node is the first one in the list.
    */
    wxNode<T>* GetPrevious();

    /**
        Returns the zero-based index of this node within the list. The return value
        will be @c wxNOT_FOUND if the node has not been added to a list yet.
    */
    int IndexOf();

    /**
        Sets the data associated with the node (usually the pointer will have been
        set when the node was created).
    */
    void SetData(T* data);
};

