/////////////////////////////////////////////////////////////////////////////
// Name:        msw/registry.h
// Purpose:     interface of wxRegKey
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxRegKey

    wxRegKey is a class representing the Windows registry (it is only available
    under Windows) which can be used to create, query, set and delete registry keys.

    The Windows registry contains data stored by Windows and the applications.
    The data are stored in a tree structure, where a tree node is called a key.
    Each key can contain subkeys as well as values. There are several predefined keys
    that are used as main entry points to the registry, the most commonly used being:

    @li @c HKEY_CLASSES_ROOT (HKCR)
    @li @c HKEY_CURRENT_USER (HKCU)
    @li @c HKEY_LOCAL_MACHINE (HKLM)
    @li @c HKEY_CURRENT_CONFIG (HKCC)
    @li @c HKEY_USERS (HKU)

    The values can be in these formats:

    @li String Value
    @li Binary Value
    @li DWORD Value
    @li Multi String Value
    @li Expandable String Value

    @onlyfor{wxmsw}

    @b Example:

    @code
    // This assumes that the key already exists, use HasSubKey() to check
    // for the key existence if necessary.
    wxRegKey key(wxRegKey::HKCU, "Software\\MyKey");

    // Create a new value "MyValue" and set it to 12.
    key.SetValue("MyValue", 12);

    // Read the value back.
    long value;
    key.QueryValue("MyValue", &value);
    wxLogMessage("Registry value: %ld", value);

    // Enumerate the subkeys.
    wxString keyName;
    long index = 0;

    for ( bool cont = key.GetFirstKey(keyName, index);
          cont;
          cont = key.GetNextKey(keyName, index) )
    {
        wxLogMessage("Subkey name: %s", keyName);
    }
    @endcode


    @library{wxbase}
    @category{cfg}
*/
class wxRegKey
{
public:
    /**
        Default constructor, initializes to @c HKEY_CLASSES_ROOT.

        The @a viewMode parameter is new since wxWidgets 2.9.2.
    */
    wxRegKey(WOW64ViewMode viewMode = WOW64ViewMode_Default);
    /**
        The constructor to set the full name of the key.

        The @a viewMode parameter is new since wxWidgets 2.9.2.
    */
    wxRegKey(const wxString& strKey,
        WOW64ViewMode viewMode = WOW64ViewMode_Default);
    /**
        The constructor to set the full name of the key using one of the
        standard keys, that is, HKCR, HKCU, HKLM, HKUSR, HKPD (obsolete),
        HKCC or HKDD (obsolete).
        The @a viewMode parameter is new since wxWidgets 2.9.2.
    */
    wxRegKey(StdKey keyParent, const wxString& strKey,
        WOW64ViewMode viewMode = WOW64ViewMode_Default);
    /**
        The constructor to set the full name of the key under a previously
        created parent. The registry view is inherited from the parent.
    */
    wxRegKey(const wxRegKey& keyParent, const wxString& strKey);

    /**
        Access modes for wxRegKey.
    */
    enum AccessMode
    {
        Read, ///< Read-only
        Write ///< Read and Write
    };

    /**
        The standard registry key enumerator.
    */
    enum StdKey
    {
    HKCR,  ///< HKEY_CLASSES_ROOT
    HKCU,  ///< HKEY_CURRENT_USER
    HKLM,  ///< HKEY_LOCAL_MACHINE
    HKUSR, ///< HKEY_USERS
    HKPD,  ///< HKEY_PERFORMANCE_DATA (Obsolete under XP and later)
    HKCC,  ///< HKEY_CURRENT_CONFIG
    HKDD,  ///< HKEY_DYN_DATA (Obsolete under XP and later)
    HKMAX
    };

    /**
        The value type enumerator.
    */
    enum ValueType
    {
    Type_None,                ///< No value type
    Type_String,              ///< Unicode null-terminated string
    Type_Expand_String,       ///< Unicode null-terminated string
                              ///< (with environment variable references)
    Type_Binary,              ///< Free form binary
    Type_Dword,               ///< 32-bit number
    Type_Dword_little_endian, ///< 32-bit number (same as Type_Dword)
    Type_Dword_big_endian,    ///< 32-bit number
    Type_Link,                ///< Symbolic Link (Unicode)
    Type_Multi_String,        ///< Multiple Unicode strings
    Type_Resource_list,       ///< Resource list in the resource map
    Type_Full_resource_descriptor,  ///< Resource list in the hardware description
    Type_Resource_requirements_list ///<
    };

    /**
        Used to determine how the registry will be viewed, either as
        32-bit or 64-bit.

        @since 2.9.2
    */
    enum WOW64ViewMode
    {
        /**
            Uses 32-bit registry for 32-bit applications and
            64-bit registry for 64-bit ones.
        */
        WOW64ViewMode_Default,

        /**
            Can be used in 64-bit apps to access the 32-bit registry,
            has no effect (i.e. treated as default) in 32-bit apps.
        */
        WOW64ViewMode_32,

        /**
            Can be used in 32-bit apps to access the 64-bit registry,
            has no effect (i.e. treated as default) in 64-bit apps.
        */
        WOW64ViewMode_64
    };

    /**
        Closes the key.
    */
    void Close();

    /**
        Copy the entire contents of the key recursively to another location
        using the name. Returns @true if successful.
    */
    bool Copy(const wxString& szNewName);
    /**
        Copy the entire contents of the key recursively to another location
        using the key. Returns @true if successful.
    */
    bool Copy(wxRegKey& keyDst);

    /**
        Copy the value to another key, possibly changing its name. By default
        it will remain the same. Returns @true if successful.
    */
    bool CopyValue(const wxString& szValue, wxRegKey& keyDst,
                  const wxString& szNewName = wxEmptyString);
    /**
        Creates the key. Will fail if the key already exists and @a bOkIfExists
        is @false. Returns @true if successful.
    */
    bool Create(bool bOkIfExists = true);

    /**
        Deletes the subkey with all its subkeys and values recursively.
    */
    void DeleteKey(const wxString& szKey);

    /**
        Deletes this key and all its subkeys and values recursively.
    */
    void DeleteSelf();

    /**
        Deletes the named value or use an empty string argument to remove the
        default value of the key.
    */
    void DeleteValue(const wxString& szKey);

    /**
        Returns @true if the key exists.
    */
    bool Exists() const;

    /**
        Write the contents of this key and all its subkeys to the given file.
        (The file will not be overwritten; it's an error if it already exists.)
        Note that we export the key in REGEDIT4 format, not RegSaveKey() binary
        format nor the newer REGEDIT5. Returns @true if successful.
    */
    bool Export(const wxString& filename) const;
    /**
        Write the contents of this key and all its subkeys to the opened stream.
        Returns @true if successful.
    */
    bool Export(wxOutputStream& ostr) const;

    /**
        Gets the first key. Returns @true if successful.
    */
    bool GetFirstKey(wxString& strKeyName, long& lIndex);

    /**
        Gets the first value of this key. Returns @true if successful.
    */
    bool GetFirstValue(wxString& strValueName, long& lIndex);

    /**
        Gets information about the key. Returns @true if successful.

        @param pnSubKeys
            The number of subkeys.
        @param pnMaxKeyLen
            The maximum length of the subkey name.
        @param pnValues
            The number of values.
        @param pnMaxValueLen
            The maximum length of a value.
    */
    bool GetKeyInfo(size_t* pnSubKeys, size_t* pnMaxKeyLen,
                    size_t* pnValues, size_t* pnMaxValueLen) const;

    /**
        Gets the name of the registry key.
    */
    wxString GetName(bool bShortPrefix = true) const;

    /**
        Retrieves the registry view used by this key.

        @since 2.9.2

        @return The registry view given at the object's construction.
    */
    WOW64ViewMode GetView() const { return m_viewMode; }

    /**
        Gets the next key. Returns @true if successful.
    */
    bool GetNextKey(wxString& strKeyName, long& lIndex) const;

    /**
        Gets the next key value for this key. Returns @true if successful.
    */
    bool GetNextValue(wxString& strValueName, long& lIndex) const;

    /**
        Gets the value type.
    */
    ValueType GetValueType(const wxString& szValue) const;

    /**
        Returns @true if given subkey exists.
    */
    bool HasSubKey(const wxString& szKey) const;

    /**
        Returns @true if any subkeys exist.
    */
    bool HasSubkeys() const;

    /**
        Returns @true if the value exists.
    */
    bool HasValue(const wxString& szValue) const;

    /**
        Returns @true if any values exist.
    */
    bool HasValues() const;

    /**
        Returns @true if this key is empty, nothing under this key.
    */
    bool IsEmpty() const;

    /**
        Returns @true if the value contains a number.
    */
    bool IsNumericValue(const wxString& szValue) const;

    /**
        Returns @true if the key is opened.
    */
    bool IsOpened() const;

    /**
        Explicitly opens the key. This method also allows the key to be opened
        in read-only mode by passing wxRegKey::Read instead of default
        wxRegKey::Write parameter. Returns @true if successful.
    */
    bool Open(AccessMode mode = Write);

    /**
        Assignment operator to set the default value of the key.
    */
    wxRegKey& operator=(const wxString& strValue);

    /**
        Return the default value of the key.
    */
    wxString QueryDefaultValue() const;

    /**
        Retrieves the raw string value. Returns @true if successful.
        An empty @a szValue queries the default/unnamed key value.
    */
    bool QueryRawValue(const wxString& szValue, wxString& strValue) const;

    /**
        Retrieves the raw or expanded string value. Returns @true if successful.
        An empty @a szValue queries the default/unnamed key value.
    */
    bool QueryValue(const wxString& szValue, wxString& strValue, bool raw) const;

    /**
        Retrieves the numeric value. Returns @true if successful.
        An empty @a szValue queries the default/unnamed key value.
    */
    bool QueryValue(const wxString& szValue, long* plValue) const;

    /**
        Retrieves the binary structure. Returns @true if successful.
        An empty @a szValue queries the default/unnamed key value.
    */
    bool QueryValue(const wxString& szValue, wxMemoryBuffer& buf) const;

    /**
        Renames the key. Returns @true if successful.
    */
    bool Rename(const wxString& szNewName);

    /**
        Renames a value. Returns @true if successful.
    */
    bool RenameValue(const wxString& szValueOld,
                     const wxString& szValueNew);

    /**
        Preallocate some memory for the name. For wxRegConfig usage only.
    */
    void ReserveMemoryForName(size_t bytes);

    /**
        Set or change the HKEY handle.
    */
    void SetHkey(WXHKEY hKey);

    /**
        Set the full key name. The name is absolute. It should start with
        HKEY_xxx.
    */
    void SetName(const wxString& strKey);
    /**
        Set the name relative to the parent key
    */
    void SetName(StdKey keyParent, const wxString& strKey);
    /**
        Set the name relative to the parent key
    */
    void SetName(const wxRegKey& keyParent, const wxString& strKey);

    /**
        Sets the given @a szValue which must be numeric. If the value doesn't
        exist, it is created. Returns @true if successful.
        An empty @a szValue sets the default/unnamed key value.
    */
    bool SetValue(const wxString& szValue, long lValue);
    /**
        Sets the given @a szValue which must be string. If the value doesn't
        exist, it is created. Returns @true if successful.
        An empty @a szValue sets the default/unnamed key value.
    */
    bool SetValue(const wxString& szValue, const wxString& strValue);
    /**
        Sets the given @a szValue which must be binary. If the value doesn't
        exist, it is created. Returns @true if successful.
        An empty @a szValue sets the default/unnamed key value.
    */
    bool SetValue(const wxString& szValue, const wxMemoryBuffer& buf);
};
