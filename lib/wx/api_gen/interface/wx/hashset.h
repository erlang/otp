/////////////////////////////////////////////////////////////////////////////
// Name:        hashset.h
// Purpose:     interface of wxHashSet
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxHashSet

    This is a simple, type-safe, and reasonably efficient hash set class,
    whose interface is a subset of the interface of STL containers.

    The interface is similar to std::tr1::hash_set or std::set classes but
    notice that, unlike std::set, the contents of a hash set is not sorted.

    Example:
    @code
        class MyClass { ... };

        // same, with MyClass* keys (only uses pointer equality!)
        WX_DECLARE_HASH_SET( MyClass*, wxPointerHash, wxPointerEqual, MySet1 );
        // same, with int keys
        WX_DECLARE_HASH_SET( int, wxIntegerHash, wxIntegerEqual, MySet2 );
        // declare a hash set with string keys
        WX_DECLARE_HASH_SET( wxString, wxStringHash, wxStringEqual, MySet3 );

        MySet1 h1;
        MySet2 h1;
        MySet3 h3;

        // store and retrieve values
        h1.insert( new MyClass( 1 ) );

        h3.insert( "foo" );
        h3.insert( "bar" );
        h3.insert( "baz" );

        int size = h3.size(); // now is three
        bool has_foo = h3.find( "foo" ) != h3.end();

        h3.insert( "bar" ); // still has size three

        // iterate over all the elements in the class
        MySet3::iterator it;
        for( it = h3.begin(); it != h3.end(); ++it )
        {
            wxString key = *it;
            // do something useful with key
        }
    @endcode


    @section hashset_declaringnew Declaring new hash set types

    @code
    WX_DECLARE_HASH_SET( KEY_T,      // type of the keys
                         HASH_T,     // hasher
                         KEY_EQ_T,   // key equality predicate
                         CLASSNAME); // name of the class
    @endcode
    The HASH_T and KEY_EQ_T are the types used for the hashing function and key
    comparison. wxWidgets provides three predefined hashing functions:
    wxIntegerHash for integer types ( int, long, short, and their unsigned counterparts ),
    wxStringHash for strings ( wxString, wxChar*, char* ), and wxPointerHash for
    any kind of pointer.
    Similarly three equality predicates: wxIntegerEqual, wxStringEqual, wxPointerEqual
    are provided. Using this you could declare a hash set using int values like this:

    @code
        WX_DECLARE_HASH_SET( int,
                            wxIntegerHash,
                            wxIntegerEqual,
                            MySet );

        // using a user-defined class for keys
        class MyKey { ... };

        // hashing function
        class MyKeyHash
        {
        public:
            MyKeyHash() { }

            unsigned long operator()( const MyKey& k ) const
                {
                    // compute the hash
                }

            MyKeyHash& operator=(const MyKeyHash&) { return *this; }
        };

        // comparison operator
        class MyKeyEqual
        {
        public:
            MyKeyEqual() { }
            bool operator()( const MyKey& a, const MyKey& b ) const
                {
                    // compare for equality
                }

            MyKeyEqual& operator=(const MyKeyEqual&) { return *this; }
        };

        WX_DECLARE_HASH_SET( MyKey,      // type of the keys
                            MyKeyHash,  // hasher
                            MyKeyEqual, // key equality predicate
                            CLASSNAME); // name of the class
    @endcode


    @section hashset_types Types

    In the documentation below you should replace wxHashSet with the name you
    used in the class declaration.

    - wxHashSet::key_type: Type of the hash keys
    - wxHashSet::mapped_type: Type of hash keys
    - wxHashSet::value_type: Type of hash keys
    - wxHashSet::iterator: Used to enumerate all the elements in a hash set;
                           it is similar to a value_type*
    - wxHashSet::const_iterator: Used to enumerate all the elements in a constant
                                 hash set; it is similar to a const value_type*
    - wxHashSet::size_type: Used for sizes
    - wxHashSet::Insert_Result: The return value for insert()


    @section hashset_iter Iterators

    An iterator is similar to a pointer, and so you can use the usual pointer
    operations: ++it ( and it++ ) to move to the next element, *it to access the
    element pointed to, *it to access the value of the element pointed to.
    Hash sets provide forward only iterators, this means that you can't use \--it,
    it + 3, it1 - it2.

    @library{wxbase}
    @category{containers}
*/
class wxHashSet
{
public:
    /**
        The size parameter is just a hint, the table will resize automatically
        to preserve performance.
    */
    wxHashSet(size_type size = 10);

    /**
        Copy constructor.
    */
    wxHashSet(const wxHashSet& set);

    //@{
    /**
        Returns an iterator pointing at the first element of the hash set.
        Please remember that hash sets do not guarantee ordering.
    */
    const_iterator begin() const;
    iterator begin();
    //@}

    /**
        Removes all elements from the hash set.
    */
    void clear();

    /**
        Counts the number of elements with the given key present in the set.
        This function returns only 0 or 1.
    */
    size_type count(const key_type& key) const;

    /**
        Returns @true if the hash set does not contain any elements, @false otherwise.
    */
    bool empty() const;

    //@{
    /**
        Returns an iterator pointing at the one-after-the-last element of the hash set.
        Please remember that hash sets do not guarantee ordering.
    */
    const_iterator end() const;
    iterator end();
    //@}

    /**
        Erases the element with the given key, and returns the number of elements
        erased (either 0 or 1).
    */
    size_type erase(const key_type& key);

    //@{
    /**
        Erases the element pointed to by the iterator. After the deletion
        the iterator is no longer valid and must not be used.
    */
    void erase(iterator it);
    void erase(const_iterator it);
    //@}

    //@{
    /**
        If an element with the given key is present, the functions returns
        an iterator pointing at that element, otherwise an invalid iterator
        is returned.
        i.e.
        @code
            hashset.find( non_existent_key ) == hashset.end()
        @endcode
    */
    iterator find(const key_type& key) const;
    const_iterator find(const key_type& key) const;
    //@}

    /**
        Inserts the given value in the hash set.
        The return value is equivalent to a
        @code std::pair<wxHashMap::iterator, bool> @endcode
        The iterator points to the inserted element, the boolean value is @true
        if @a v was actually inserted.
    */
    Insert_Result insert(const value_type& v);

    /**
        Returns the number of elements in the set.
    */
    size_type size() const;
};

