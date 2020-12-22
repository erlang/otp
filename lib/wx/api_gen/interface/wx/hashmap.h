/////////////////////////////////////////////////////////////////////////////
// Name:        hashmap.h
// Purpose:     interface of wxHashMap
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxHashMap

    This is a simple, type-safe, and reasonably efficient hash map class,
    whose interface is a subset of the interface of STL containers.
    In particular, the interface is modelled after std::map, and the various,
    non-standard, std::hash_map (http://www.cppreference.com/wiki/stl/map/start).

    Example:
    @code
        class MyClass { ... };

        // declare a hash map with string keys and int values
        WX_DECLARE_STRING_HASH_MAP( int, MyHash5 );
        // same, with int keys and MyClass* values
        WX_DECLARE_HASH_MAP( int, MyClass*, wxIntegerHash, wxIntegerEqual, MyHash1 );
        // same, with wxString keys and int values
        WX_DECLARE_STRING_HASH_MAP( int, MyHash3 );
        // same, with wxString keys and values
        WX_DECLARE_STRING_HASH_MAP( wxString, MyHash2 );

        MyHash1 h1;
        MyHash2 h2;

        // store and retrieve values
        h1[1] = new MyClass( 1 );
        h1[10000000] = NULL;
        h1[50000] = new MyClass( 2 );
        h2["Bill"] = "ABC";
        wxString tmp = h2["Bill"];
        // since element with key "Joe" is not present, this will return
        // the default value, which is an empty string in the case of wxString
        MyClass tmp2 = h2["Joe"];

        // iterate over all the elements in the class
        MyHash2::iterator it;
        for( it = h2.begin(); it != h2.end(); ++it )
        {
            wxString key = it->first, value = it->second;
            // do something useful with key and value
        }
    @endcode


    @section hashmap_declaringnew Declaring new hash table types

    @code
        WX_DECLARE_STRING_HASH_MAP( VALUE_T,     // type of the values
                                    CLASSNAME ); // name of the class
    @endcode
    Declares a hash map class named CLASSNAME, with wxString keys and VALUE_T values.

    @code
        WX_DECLARE_VOIDPTR_HASH_MAP( VALUE_T,     // type of the values
                                    CLASSNAME ); // name of the class
    @endcode
    Declares a hash map class named CLASSNAME, with void* keys and VALUE_T values.

    @code
    WX_DECLARE_HASH_MAP( KEY_T,      // type of the keys
                         VALUE_T,    // type of the values
                         HASH_T,     // hasher
                         KEY_EQ_T,   // key equality predicate
                         CLASSNAME); // name of the class
    @endcode
    The HASH_T and KEY_EQ_T are the types used for the hashing function and
    key comparison. wxWidgets provides three predefined hashing functions:
    @c wxIntegerHash for integer types ( int, long, short, and their unsigned counterparts ),
    @c wxStringHash for strings ( wxString, wxChar*, char* ), and @c wxPointerHash for
    any kind of pointer.
    Similarly three equality predicates: @c wxIntegerEqual, @c wxStringEqual,
    @c wxPointerEqual are provided.
    Using this you could declare a hash map mapping int values to wxString like this:

    @code
    WX_DECLARE_HASH_MAP( int,
                         wxString,
                         wxIntegerHash,
                         wxIntegerEqual,
                         MyHash );

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

    WX_DECLARE_HASH_MAP( MyKey,      // type of the keys
                         SOME_TYPE,  // any type you like
                         MyKeyHash,  // hasher
                         MyKeyEqual, // key equality predicate
                         CLASSNAME); // name of the class
    @endcode


    @section hashmap_types Types

    In the documentation below you should replace wxHashMap with the name you used
    in the class declaration.

    - wxHashMap::key_type: Type of the hash keys.
    - wxHashMap::mapped_type: Type of the values stored in the hash map.
    - wxHashMap::value_type: Equivalent to struct { key_type first; mapped_type second }.
    - wxHashMap::iterator: Used to enumerate all the elements in a hash map;
                           it is similar to a value_type*.
    - wxHashMap::const_iterator: Used to enumerate all the elements in a constant
                                 hash map; it is similar to a const value_type*.
    - wxHashMap::size_type: Used for sizes.
    - wxHashMap::Insert_Result: The return value for insert().


    @section hashmap_iter Iterators

    An iterator is similar to a pointer, and so you can use the usual pointer operations:
    ++it ( and it++ ) to move to the next element, *it to access the element pointed to,
    it->first ( it->second ) to access the key ( value ) of the element pointed to.

    Hash maps provide forward only iterators, this means that you can't use \--it,
    it + 3, it1 - it2.


    @section hashmap_predef Predefined hashmap types

    wxWidgets defines the following hashmap types:
    - wxLongToLongHashMap (uses long both for keys and values)
    - wxStringToStringHashMap (uses wxString both for keys and values)


    @library{wxbase}
    @category{containers}
*/
class wxHashMap
{
public:
    /**
        The size parameter is just a hint, the table will resize automatically
        to preserve performance.
    */
    wxHashMap(size_type size = 10);

    /**
        Copy constructor.
    */
    wxHashMap(const wxHashMap& map);

    //@{
    /**
        Returns an iterator pointing at the first element of the hash map.
        Please remember that hash maps do not guarantee ordering.
    */
    const_iterator begin() const;
    iterator begin();
    //@}

    /**
        Removes all elements from the hash map.
    */
    void clear();

    /**
        Counts the number of elements with the given key present in the map.
        This function returns only 0 or 1.
    */
    size_type count(const key_type& key) const;

    /**
        Returns @true if the hash map does not contain any elements, @false otherwise.
    */
    bool empty() const;

    //@{
    /**
        Returns an iterator pointing at the one-after-the-last element of the hash map.
        Please remember that hash maps do not guarantee ordering.
    */
    const_iterator end() const;
    iterator end();
    //@}

    //@{
    /**
        Erases the element with the given key, and returns the number of elements
        erased (either 0 or 1).
    */
    size_type erase(const key_type& key);

    /**
        Erases the element pointed to by the iterator. After the deletion
        the iterator is no longer valid and must not be used.
    */
    void erase(iterator it);
    void erase(const_iterator it);
    //@}

    //@{
    /**
        If an element with the given key is present, the functions returns an
        iterator pointing at that element, otherwise an invalid iterator is
        returned.

        @code
            hashmap.find( non_existent_key ) == hashmap.end()
        @endcode
    */
    iterator find(const key_type& key) const;
    const_iterator find(const key_type& key) const;
    //@}

    /**
        Inserts the given value in the hash map.
        The return value is equivalent to a
        @code std::pair<wxHashMap::iterator, bool> @endcode
        The iterator points to the inserted element, the boolean value is @true
        if @a v was actually inserted.
    */
    Insert_Result insert(const value_type& v);

    /**
        Use the key as an array subscript.
        The only difference is that if the given key is not present in the hash map,
        an element with the default @c value_type() is inserted in the table.
    */
    mapped_type operator[](const key_type& key);

    /**
        Returns the number of elements in the map.
    */
    size_type size() const;
};

