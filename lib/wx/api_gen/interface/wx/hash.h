/////////////////////////////////////////////////////////////////////////////
// Name:        hash.h
// Purpose:     interface of wxHashTable
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxHashTable

    @deprecated
    Please note that this class is retained for backward compatibility
    reasons; you should use wxHashMap.

    This class provides hash table functionality for wxWidgets, and for an
    application if it wishes.  Data can be hashed on an integer or string key.

    Example:
    @code
        wxHashTable table(wxKEY_STRING);

        wxPoint *point = new wxPoint(100, 200);
        table.Put("point 1", point);

        ....

        wxPoint *found_point = (wxPoint *)table.Get("point 1");
    @endcode

    A hash table is implemented as an array of pointers to lists.
    When no data has been stored, the hash table takes only a little more space
    than this array (default size is 1000). When a data item is added, an integer
    is constructed from the integer or string key that is within the bounds of the array.
    If the array element is @NULL, a new (keyed) list is created for the element.
    Then the data object is appended to the list, storing the key in case other
    data objects need to be stored in the list also (when a 'collision' occurs).

    Retrieval involves recalculating the array index from the key, and searching
    along the keyed list for the data object whose stored key matches the passed key.
    Obviously this is quicker when there are fewer collisions, so hashing will
    become inefficient if the number of items to be stored greatly exceeds the
    size of the hash table.

    @library{wxbase}
    @category{containers}

    @see wxList
*/
class wxHashTable : public wxObject
{
public:
    /**
        Constructor. @a key_type is one of wxKEY_INTEGER, or wxKEY_STRING,
        and indicates what sort of keying is required. @a size is optional.
    */
    wxHashTable(wxKeyType key_type = wxKEY_INTEGER, size_t size = 1000);

    /**
        Destroys the hash table.
    */
    virtual ~wxHashTable();

    /**
        The counterpart of Next().  If the application wishes to iterate
        through all the data in the hash table, it can call BeginFind() and
        then loop on Next().
    */
    void BeginFind();

    /**
        Clears the hash table of all nodes (but as usual, doesn't delete user data).
    */
    void Clear();

    //@{
    /**
        Deletes entry in hash table and returns the user's data (if found).
    */
    wxObject* Delete(long key);
    wxObject* Delete(const wxString& key);
    //@}

    /**
        If set to @true data stored in hash table will be deleted when hash table
        object is destroyed.
    */
    void DeleteContents(bool flag);

    //@{
    /**
        Gets data from the hash table, using an integer or string key
        (depending on which has table constructor was used).
    */
    wxObject* Get(long key);
    wxObject* Get(const char* key);
    //@}

    /**
        Returns the number of elements in the hash table.
    */
    size_t GetCount() const;

    /**
        Makes an integer key out of a string. An application may wish to make a key
        explicitly (for instance when combining two data values to form a key).
    */
    static long MakeKey(const wxString& string);

    /**
        If the application wishes to iterate through all the data in the hash
        table, it can call BeginFind() and then loop on Next(). This function
        returns a @b wxHashTable::Node pointer (or @NULL if there are no more nodes).

        The return value is functionally equivalent to @b wxNode but might not be
        implemented as a @b wxNode. The user will probably only wish to use the
        wxNode::GetData() method to retrieve the data; the node may also be deleted.
    */
    wxHashTable::Node* Next();

    //@{
    /**
        Inserts data into the hash table, using an integer or string key (depending on
        which has table constructor was used).
        The key string is copied and stored by the hash table implementation.
    */
    void Put(long key, wxObject* object);
    void Put(const char* key, wxObject* object);
    //@}
};

