/////////////////////////////////////////////////////////////////////////////
// Name:        vector.h
// Purpose:     interface of wxVector<T>
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**

    wxVector<T> is a template class which implements most of the @c std::vector
    class and can be used like it.

    If wxWidgets is compiled in STL mode, wxVector will just be a typedef to
    @c std::vector. Just like for @c std::vector, objects stored in wxVector<T>
    need to be @e assignable but don't have to be @e "default constructible".

    Please refer to the STL documentation for further information.

    @nolibrary
    @category{containers}

    @see @ref overview_container, wxList<T>, wxArray<T>, wxVectorSort<T>
*/
template<typename T>
class wxVector<T>
{
public:
    typedef size_t size_type;
    typedef size_t difference_type;
    typedef T value_type;
    typedef value_type* pointer;
    typedef value_type* iterator;
    typedef const value_type* const_iterator;
    typedef value_type& reference;

    /**
        Constructor.
    */
    wxVector();

    /**
        Constructor initializing the vector with the given number of
        default-constructed objects.
     */
    wxVector(size_type size);

    /**
        Constructor initializing the vector with the given number of
        copies of the given object.
     */
    wxVector(size_type size, const value_type& value);

    /**
        Constructor initializing the vector with the elements in the given
        range.

        The @a InputIterator template parameter must be an input iterator type.
        This constructor adds all elements from @a first until, not not
        including, @a last to the vector.

        @since 2.9.5
     */
    template <class InputIterator>
    wxVector(InputIterator first, InputIterator last);

    /**
        Copy constructor.
    */
    wxVector(const wxVector<T>& c);

    /**
        Destructor.
    */
    ~wxVector();

    /**
        Resizes the vector to @a n and assigns @a v to all elements.

        @see resize()

        @since 2.9.5
     */
    void assign(size_type n, const value_type& v);

    /**
        Assigns the elements in the given range to the vector.

        The @a InputIterator template parameter must be an input iterator type.
        This method clears the vector and then adds all elements from @a first
        until, not not including, @a last to it.

        @since 2.9.5
     */
    template <class InputIterator>
    void assign(InputIterator first, InputIterator last);

    /**
        Returns item at position @a idx.
    */
    const value_type& at(size_type idx) const;

    /**
        Returns item at position @a idx.
    */
    value_type& at(size_type idx);

    /**
        Return the last item.
    */
    const value_type& back() const;

    /**
        Return the last item.
    */
    value_type& back();

    /**
        Return iterator to beginning of the vector.
    */
    const_iterator begin() const;

    /**
        Return iterator to beginning of the vector.
    */
    iterator begin();

    /**
        Return reverse iterator to end of the vector.
    */
    reverse_iterator rbegin();

    /**
        Return reverse iterator to beginning of the vector.
    */
    reverse_iterator rend();


    /**
        Returns vector's current capacity, i.e.\ how much memory is allocated.

        @see reserve()
    */
    size_type capacity() const;

    /**
        Clears the vector.
    */
    void clear();

    /**
        Returns @true if the vector is empty.
    */
    bool empty() const;

    /**
        Returns iterator to the end of the vector.
    */
    const_iterator end() const;

    /**
        Returns iterator to the end of the vector.
    */
    iterator end();

    /**
        Erase item pointed to by iterator @a it.

        @return Iterator pointing to the item immediately after the erased one.
    */
    iterator erase(iterator it);

    /**
        Erase items in the range @a first to @a last (@a last is not erased).

        @return Iterator pointing to the item immediately after the erased
                range.
    */
    iterator erase(iterator first, iterator last);

    /**
        Returns the first item.
    */
    const value_type& front() const;

    /**
        Returns the first item.
    */
    value_type& front();

    /**
        Insert item @a v at given position @a it.

        @return Iterator for the inserted item.
    */
    iterator insert(iterator it, const value_type& v = value_type());

    /**
        Insert the given number of copies of @a v at the given position.

        @return Iterator for the first inserted item.

        @since 3.1.1
     */
    iterator insert(iterator it, size_type count, const value_type& v);

    /**
        Assignment operator.
    */
    wxVector& operator=(const wxVector& vb);

    /**
        Equality operator.

        @since 3.1.1
    */
    wxVector& operator==(const wxVector& vb) const;

    /**
        Inequality operator.

        @since 3.1.1
    */
    wxVector& operator!=(const wxVector& vb) const;

    /**
        Returns item at position @a idx.
    */
    const value_type& operator[](size_type idx) const;

    /**
        Returns item at position @a idx.
    */
    value_type& operator[](size_type idx);

    /**
        Removes the last item.
    */
    void pop_back();

    /**
        Adds an item to the end of the vector.
    */
    void push_back(const value_type& v);

    /**
        Reserves memory for at least @a n items.

        @see capacity()
    */
    void reserve(size_type n);

    /**
        Makes the vector of size @a n.

        If @a n is less than the current size(), the elements at the end of the
        vector are erased. If it is greater, then the vector is completed with
        either the copies of the given object @a v or @c value_type() objects
        until it becomes of size @a n.
     */
    //@{
    void resize(size_type n);
    void resize(size_type n, const value_type& v);
    //@}

    /**
        Free unused memory allocated by the vector.

        Reduces the memory used by the vector to the bare minimum required to
        hold its current number of elements, possibly 0.

        After calling this method, capacity() returns the same as size().

        @since 3.1.1
     */
    void shrink_to_fit();

    /**
        Returns the size of the vector.
    */
    size_type size() const;

    /**
        Efficiently exchanges contents of this vector with another one.

        After the execution of this function the contents of this vector is
        equal to the original contents of @a v and the contents of @a v becomes
        the original contents of this vector without copying the data.

        @since 2.9.1
     */
    void swap(wxVector& v);
};


/**
   Sort the contents of a @c wxVector<T>.  In a STL build this function will
   be defined as a thin wrapper around std::sort.  To be sortable the
   contained type must support the less-than operator.

   @code
   wxVector<SomeClass> v;
   ... // items are added to the vector v...
   wxVectorSort(v);
   @endcode

   @see wxVector<T>
*/
template<typename T>
void wxVectorSort(wxVector<T>& v);
