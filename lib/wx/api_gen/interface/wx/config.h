/////////////////////////////////////////////////////////////////////////////
// Name:        config.h
// Purpose:     interface of wxConfigBase
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


// Flags for constructor style parameter
enum
{
    wxCONFIG_USE_LOCAL_FILE = 1,
    wxCONFIG_USE_GLOBAL_FILE = 2,
    wxCONFIG_USE_RELATIVE_PATH = 4,
    wxCONFIG_USE_NO_ESCAPE_CHARACTERS = 8,
    wxCONFIG_USE_SUBDIR = 16
};


/**
    @class wxConfigBase

    wxConfigBase defines the basic interface of all config classes. It cannot
    be used by itself (it is an abstract base class) and you will always use
    one of its derivations: wxFileConfig, wxRegConfig or any other.

    However, usually you don't even need to know the precise nature of the
    class you're working with but you would just use the wxConfigBase methods.
    This allows you to write the same code regardless of whether you're working
    with the registry under Windows or text-based config files under Unix.
    To make writing the portable code even easier, wxWidgets provides a typedef
    wxConfig which is mapped onto the native wxConfigBase implementation on the
    given platform: i.e. wxRegConfig under Windows and wxFileConfig otherwise.

    See @ref overview_config for a description of all features of this class.

    It is highly recommended to use static functions Get() and/or Set(), so
    please have a look at them.

    Related Include Files:

    @li @c <wx/config.h>   - Let wxWidgets choose a wxConfig class for your
                             platform.
    @li @c <wx/confbase.h> - Base config class.
    @li @c <wx/fileconf.h> - wxFileConfig class.
    @li @c <wx/msw/regconf.h> - wxRegConfig class, see also wxRegKey.


    @section configbase_example Example

    Here is how you would typically use this class:

    @code
    // using wxConfig instead of writing wxFileConfig or wxRegConfig enhances
    // portability of the code
    wxConfig *config = new wxConfig("MyAppName");

    wxString str;
    if ( config->Read("LastPrompt", &str) ) {
        // last prompt was found in the config file/registry and its value is
        // now in str
        // ...
    }
    else {
        // no last prompt...
    }

    // another example: using default values and the full path instead of just
    // key name: if the key is not found , the value 17 is returned
    long value = config->ReadLong("/LastRun/CalculatedValues/MaxValue", 17);

    // at the end of the program we would save everything back
    config->Write("LastPrompt", str);
    config->Write("/LastRun/CalculatedValues/MaxValue", value);

    // the changes will be written back automatically
    delete config;
    @endcode

    This basic example, of course, doesn't show all wxConfig features, such as
    enumerating, testing for existence and deleting the entries and groups of
    entries in the config file, its abilities to automatically store the
    default values or expand the environment variables on the fly. However, the
    main idea is that using this class is easy and that it should normally do
    what you expect it to.

    @note In the documentation of this class, the words "config file" also mean
          "registry hive" for wxRegConfig and, generally speaking, might mean
          any physical storage where a wxConfigBase-derived class stores its
          data.


    @section configbase_static Static Functions

    The static functions provided deal with the "default" config object.
    Although its usage is not at all mandatory it may be convenient to use a
    global config object instead of creating and deleting the local config
    objects each time you need one (especially because creating a wxFileConfig
    object might be a time consuming operation). In this case, you may create
    this global config object in the very start of the program and Set() it as
    the default. Then, from anywhere in your program, you may access it using
    the Get() function. This global wxConfig object will be deleted by
    wxWidgets automatically if it exists. Note that this implies that if you do
    delete this object yourself (usually in wxApp::OnExit()) you must use
    Set(@NULL) to prevent wxWidgets from deleting it the second time.

    As it happens, you may even further simplify the procedure described above:
    you may forget about calling Set(). When Get() is called and there is no
    current object, it will create one using Create() function. To disable this
    behaviour DontCreateOnDemand() is provided.

    @note You should use either Set() or Get() because wxWidgets library itself
          would take advantage of it and could save various information in it.
          For example wxFontMapper or Unix version of wxFileDialog have the
          ability to use wxConfig class.


    @section configbase_paths Path Management

    As explained in the @ref overview_config "config overview", the config
    classes support a file system-like hierarchy of keys (files) and groups
    (directories). As in the file system case, to specify a key in the config
    class you must use a path to it. Config classes also support the notion of
    the current group, which makes it possible to use the relative paths. To
    clarify all this, here is an example (it is only for the sake of
    demonstration, it doesn't do anything sensible!):

    @code
    wxConfig *config = new wxConfig("FooBarApp");

    // right now the current path is '/'
    conf->Write("RootEntry", 1);

    // go to some other place: if the group(s) don't exist, they will be created
    conf->SetPath("/Group/Subgroup");

    // create an entry in subgroup
    conf->Write("SubgroupEntry", 3);

    // '..' is understood
    conf->Write("../GroupEntry", 2);
    conf->SetPath("..");

    wxASSERT( conf->ReadLong("Subgroup/SubgroupEntry", 0) == 3 );

    // use absolute path: it is allowed, too
    wxASSERT( conf->ReadLong("/RootEntry", 0) == 1 );
    @endcode

    It is highly recommended that you restore the path to its old value on
    function exit:

    @code
    void foo(wxConfigBase *config)
    {
        wxString strOldPath = config->GetPath();

        config->SetPath("/Foo/Data");
        // ...

        config->SetPath(strOldPath);
    }
    @endcode

    Otherwise the assert in the following example will surely fail (we suppose
    here that the foo() function is the same as above except that it doesn’t
    save and restore the path):

    @code
    void bar(wxConfigBase *config)
    {
        config->Write("Test", 17);

        foo(config);

        // we're reading "/Foo/Data/Test" here! -1 will probably be returned...
        wxASSERT( config->ReadLong("Test", -1) == 17 );
    }
    @endcode

    Finally, the path separator in wxConfigBase and derived classes is always
    "/", regardless of the platform (i.e. it is not "\\" under Windows).


    @section configbase_enumeration Enumeration

    The enumeration functions allow you to enumerate all entries and groups in
    the config file. All functions here return @false when there are no more
    items.

    You must pass the same index to GetNext() and GetFirst() (don't modify it).
    Please note that it is not the index of the current item (you will have
    some great surprises with wxRegConfig if you assume this) and you shouldn't
    even look at it: it is just a "cookie" which stores the state of the
    enumeration. It can't be stored inside the class because it would prevent
    you from running several enumerations simultaneously, that's why you must
    pass it explicitly.

    Having said all this, enumerating the config entries/groups is very simple:

    @code
    wxConfigBase *config = ...;
    wxArrayString aNames;

    // enumeration variables
    wxString str;
    long dummy;

    // first enum all entries
    bool bCont = config->GetFirstEntry(str, dummy);
    while ( bCont ) {
        aNames.Add(str);

        bCont = config->GetNextEntry(str, dummy);
    }

    // ... we have all entry names in aNames...

    // now all groups...
    bCont = config->GetFirstGroup(str, dummy);
    while ( bCont ) {
        aNames.Add(str);

        bCont = config->GetNextGroup(str, dummy);
    }

    // ... we have all group (and entry) names in aNames...
    @endcode

    There are also functions to get the number of entries/subgroups without
    actually enumerating them, but you will probably never need them.


    @section configbase_keyaccess Key Access

    The key access functions are the core of wxConfigBase class: they allow you
    to read and write config file data. All Read() functions take a default
    value which will be returned if the specified key is not found in the
    config file.

    Currently, supported types of data are: wxString, @c long, @c double,
    @c bool, wxColour and any other types for which the functions
    wxToString() and wxFromString() are defined.

    Try not to read long values into string variables and vice versa:
    although it just might work with wxFileConfig, you will get a system
    error with wxRegConfig because in the Windows registry the different
    types of entries are indeed used.

    Final remark: the @a szKey parameter for all these functions can
    contain an arbitrary path (either relative or absolute), not just the
    key name.

    @library{wxbase}
    @category{cfg}

    @see wxConfigPathChanger
*/
class wxConfigBase : public wxObject
{
public:
    /**
        This is the default and only constructor of the wxConfigBase class, and
        derived classes.

        @param appName
            The application name. If this is empty, the class will normally use
            wxApp::GetAppName() to set it. The application name is used in the
            registry key on Windows, and can be used to deduce the local
            filename parameter if that is missing.
        @param vendorName
            The vendor name. If this is empty, it is assumed that no vendor
            name is wanted, if this is optional for the current config class.
            The vendor name is appended to the application name for
            wxRegConfig.
        @param localFilename
            Some config classes require a local filename. If this is not
            present, but required, the application name will be used instead.
        @param globalFilename
            Some config classes require a global filename. If this is not
            present, but required, the application name will be used instead.
        @param style
            Can be one of @c wxCONFIG_USE_LOCAL_FILE and @c wxCONFIG_USE_GLOBAL_FILE.
            @n The style interpretation depends on the config class and is ignored
            by some implementations. For wxFileConfig, these styles determine
            whether a local or global config file is created or used: if
            @c wxCONFIG_USE_GLOBAL_FILE is used, then settings are read from the
            global config file and if @c wxCONFIG_USE_LOCAL_FILE is used, settings
            are read from and written to local config file (if they are both
            set, global file is read first, then local file, overwriting global
            settings). If the flag is present but the parameter is empty, the
            parameter will be set to a default. If the parameter is present but
            the style flag not, the relevant flag will be added to the style.
            For wxRegConfig, the GLOBAL flag refers to the @c HKLM key while LOCAL
            one is for the usual @c HKCU one.
            @n For wxFileConfig you can also add @c wxCONFIG_USE_RELATIVE_PATH by
            logically or'ing it to either of the _FILE options to tell
            wxFileConfig to use relative instead of absolute paths.
            @n On non-VMS Unix systems, the default local configuration file is
            "~/.appname". However, this path may be also used as user data
            directory (see wxStandardPaths::GetUserDataDir()) if the
            application has several data files. In this case
            @c wxCONFIG_USE_SUBDIR flag, which changes the default local
            configuration file to "~/.appname/appname" should be used. Notice
            that this flag is ignored if @a localFilename is provided.
            @c wxCONFIG_USE_SUBDIR is new since wxWidgets version 2.8.2.
            @n For wxFileConfig, you can also add
            @c wxCONFIG_USE_NO_ESCAPE_CHARACTERS which will turn off character
            escaping for the values of entries stored in the config file: for
            example a foo key with some backslash characters will be stored as
            "foo=C:\mydir" instead of the usual storage of "foo=C:\\mydir".
            @n The @c wxCONFIG_USE_NO_ESCAPE_CHARACTERS style can be helpful if your
            config file must be read or written to by a non-wxWidgets program
            (which might not understand the escape characters). Note, however,
            that if @c wxCONFIG_USE_NO_ESCAPE_CHARACTERS style is used, it is
            now your application's responsibility to ensure that there is no
            newline or other illegal characters in a value, before writing that
            value to the file.
        @param conv
            This parameter is only used by wxFileConfig when compiled in
            Unicode mode. It specifies the encoding in which the configuration
            file is written.

        @remarks By default, environment variable expansion is on and recording
                 defaults is off.
    */
    wxConfigBase(const wxString& appName = wxEmptyString,
                 const wxString& vendorName = wxEmptyString,
                 const wxString& localFilename = wxEmptyString,
                 const wxString& globalFilename = wxEmptyString,
                 long style = 0,
                 const wxMBConv& conv = wxConvAuto());

    /**
        Empty but ensures that dtor of all derived classes is virtual.
    */
    virtual ~wxConfigBase();


    /**
        @name Path Management

        See @ref configbase_paths
    */
    //@{

    /**
        Retrieve the current path (always as absolute path).
    */
    virtual const wxString& GetPath() const = 0;

    /**
        Set current path: if the first character is '/', it is the absolute
        path, otherwise it is a relative path. '..' is supported. If @a strPath
        doesn't exist, it is created.

        @see wxConfigPathChanger
    */
    virtual void SetPath(const wxString& strPath) = 0;

    //@}


    /**
        @name Enumeration

        See @ref configbase_enumeration
    */
    //@{

    /**
        Gets the first entry.

        @beginWxPerlOnly
        In wxPerl this method takes no parameters and returns a 3-element
        list (continue_flag, string, index_for_getnextentry).
        @endWxPerlOnly
    */
    virtual bool GetFirstEntry(wxString& str, long& index) const = 0;

    /**
        Gets the first group.

        @beginWxPerlOnly
        In wxPerl this method takes no parameters and returns a 3-element
        list (continue_flag, string, index_for_getnextentry).
        @endWxPerlOnly
    */
    virtual bool GetFirstGroup(wxString& str, long& index) const = 0;

    /**
        Gets the next entry.

        @beginWxPerlOnly
        In wxPerl this method only takes the @a index parameter and
        returns a 3-element list (continue_flag, string,
        index_for_getnextentry).
        @endWxPerlOnly
    */
    virtual bool GetNextEntry(wxString& str, long& index) const = 0;

    /**
        Gets the next group.

        @beginWxPerlOnly
        In wxPerl this method only takes the @a index parameter and
        returns a 3-element list (continue_flag, string,
        index_for_getnextentry).
        @endWxPerlOnly
    */
    virtual bool GetNextGroup(wxString& str, long& index) const = 0;

    /**
        Get number of entries in the current group.
    */
    virtual size_t GetNumberOfEntries(bool bRecursive = false) const = 0;

    /**
        Get number of entries/subgroups in the current group, with or without
        its subgroups.
    */
    virtual size_t GetNumberOfGroups(bool bRecursive = false) const = 0;

    //@}


    enum EntryType
    {
        Type_Unknown,
        Type_String,
        Type_Boolean,
        Type_Integer,
        Type_Float
    };

    /**
        @name Tests of Existence
    */
    //@{

    /**
        @return @true if either a group or an entry with a given name exists.
    */
    bool Exists(const wxString& strName) const;

    /**
        Returns the type of the given entry or @e Unknown if the entry doesn't
        exist. This function should be used to decide which version of Read()
        should be used because some of wxConfig implementations will complain
        about type mismatch otherwise: e.g., an attempt to read a string value
        from an integer key with wxRegConfig will fail.
    */
    virtual wxConfigBase::EntryType GetEntryType(const wxString& name) const;

    /**
        @return @true if the entry by this name exists.
    */
    virtual bool HasEntry(const wxString& strName) const = 0;

    /**
        @return @true if the group by this name exists.
    */
    virtual bool HasGroup(const wxString& strName) const = 0;

    //@}


    /**
        @name Miscellaneous Functions
    */
    //@{

    /**
        Returns the application name.
    */
    wxString GetAppName() const;

    /**
        Returns the vendor name.
    */
    wxString GetVendorName() const;

    //@}


    /**
        @name Key Access

        See @ref configbase_keyaccess
    */
    //@{

    /**
        Permanently writes all changes (otherwise, they're only written from
        object's destructor).
    */
    virtual bool Flush(bool bCurrentOnly = false) = 0;

    /**
        Read a string from the key, returning @true if the value was read. If
        the key was not found, @a str is not changed.

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    bool Read(const wxString& key, wxString* str) const;
    /**
        Read a string from the key. The default value is returned if the key
        was not found.

        @return @true if value was really read, @false if the default was used.

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    bool Read(const wxString& key, wxString* str,
              const wxString& defaultVal) const;
    /**
        Another version of Read(), returning the string value directly.

        @beginWxPerlOnly
        In wxPerl, this can be called as:
        - Read(key): returns the empty string if no key is found
        - Read(key, default): returns the default value if no key is found
        @endWxPerlOnly
    */
    const wxString Read(const wxString& key,
                        const wxString& defaultVal) const;
    /**
        Reads a long value, returning @true if the value was found. If the
        value was not found, @a l is not changed.

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    bool Read(const wxString& key, long* l) const;
    /**
        Reads a long value, returning @true if the value was found. If the
        value was not found, @a defaultVal is used instead.

        @beginWxPerlOnly
        In wxPerl, this can be called as:
        - ReadInt(key): returns the 0 if no key is found
        - ReadInt(key, default): returns the default value if no key is found
        @endWxPerlOnly
    */
    bool Read(const wxString& key, long* l,
              long defaultVal) const;
    /**
        Reads a double value, returning @true if the value was found. If the
        value was not found, @a d is not changed.

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    bool Read(const wxString& key, double* d) const;
    /**
        Reads a double value, returning @true if the value was found. If the
        value was not found, @a defaultVal is used instead.

        @beginWxPerlOnly
        In wxPerl, this can be called as:
        - ReadFloat(key): returns the 0.0 if no key is found
        - ReadFloat(key, default): returns the default value if no key is found
        @endWxPerlOnly
    */
    bool Read(const wxString& key, double* d,
                     double defaultVal) const;

    /**
        Reads a float value, returning @true if the value was found.

        If the value was not found, @a f is not changed.

        Notice that the value is read as a double but must be in a valid range
        for floats for the function to return @true.

        @since 2.9.1

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    bool Read(const wxString& key, float* f) const;
    /**
        Reads a float value, returning @true if the value was found.

        If the value was not found, @a defaultVal is used instead.

        Notice that the value is read as a double but must be in a valid range
        for floats for the function to return @true.

        @since 2.9.1

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    bool Read(const wxString& key, float* f, float defaultVal) const;

    /**
        Reads a boolean value, returning @true if the value was found. If the
        value was not found, @a b is not changed.

        @since 2.9.1

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    bool Read(const wxString& key, bool* b) const;
    /**
        Reads a boolean value, returning @true if the value was found. If the
        value was not found, @a defaultVal is used instead.

        @beginWxPerlOnly
        In wxPerl, this can be called as:
        - ReadBool(key): returns false if no key is found
        - ReadBool(key, default): returns the default value if no key is found
        @endWxPerlOnly
    */
    bool Read(const wxString& key, bool* d,
              bool defaultVal) const;
    /**
        Reads a binary block, returning @true if the value was found. If the
        value was not found, @a buf is not changed.
    */
    bool Read(const wxString& key, wxMemoryBuffer* buf) const;
    /**
        Reads a value of type T, for which function wxFromString() is defined,
        returning @true if the value was found. If the value was not found,
        @a value is not changed.
    */
    bool Read(const wxString& key, T* value) const;
    /**
        Reads a value of type T, for which function wxFromString() is defined,
        returning @true if the value was found. If the value was not found,
        @a defaultVal is used instead.
    */
    bool Read(const wxString& key, T* value,
              const T& defaultVal) const;

    /**
        Reads a bool value from the key and returns it. @a defaultVal is
        returned if the key is not found.
    */
    bool ReadBool(const wxString& key, bool defaultVal) const;

    /**
        Reads a double value from the key and returns it. @a defaultVal is
        returned if the key is not found.
    */
    double ReadDouble(const wxString& key, double defaultVal) const;

    /**
        Reads a long value from the key and returns it. @a defaultVal is
        returned if the key is not found.
    */
    long ReadLong(const wxString& key, long defaultVal) const;

    /**
        Reads a value of type T (for which the function wxFromString() must be
        defined) from the key and returns it. @a defaultVal is returned if the
        key is not found.
    */
    T ReadObject(const wxString& key, T const& defaultVal) const;

    /**
        Writes the wxString value to the config file and returns @true on
        success.
    */
    bool Write(const wxString& key, const wxString& value);
    /**
        Writes the long value to the config file and returns @true on success.
    */
    bool Write(const wxString& key, long value);
    /**
        Writes the double value to the config file and returns @true on
        success.

        Notice that if floating point numbers are saved as strings (as is the
        case with the configuration files used by wxFileConfig), this function
        uses the C locale for writing out the number, i.e. it will always use a
        period as the decimal separator, irrespectively of the current locale.
        This behaviour is new since wxWidgets 2.9.1 as the current locale was
        used before, but the change should be transparent because both C and
        current locales are tried when reading the numbers back.
    */
    bool Write(const wxString& key, double value);
    /**
        Writes the bool value to the config file and returns @true on success.
    */
    bool Write(const wxString& key, bool value);
    /**
        Writes the wxMemoryBuffer value to the config file and returns @true on
        success.
    */
    bool Write(const wxString& key, const wxMemoryBuffer& buf);
    /**
        Writes the specified value to the config file and returns @true on
        success. The function wxToString() must be defined for type @e T.
    */
    bool Write(const wxString& key, T const& buf);

    //@}


    /**
        @name Rename Entries/Groups

        These functions allow renaming entries or subgroups of the current
        group. They will return @false on error, typically because either the
        entry/group with the original name doesn't exist, because the
        entry/group with the new name already exists or because the function is
        not supported in this wxConfig implementation.
    */
    //@{

    /**
        Renames an entry in the current group. The entries names (both the old
        and the new one) shouldn't contain backslashes, i.e. only simple names
        and not arbitrary paths are accepted by this function.

        @return @false if @a oldName doesn't exist or if @a newName already
                exists.
    */
    virtual bool RenameEntry(const wxString& oldName,
                             const wxString& newName) = 0;

    /**
        Renames a subgroup of the current group. The subgroup names (both the
        old and the new one) shouldn't contain backslashes, i.e. only simple
        names and not arbitrary paths are accepted by this function.

        @return @false if @a oldName doesn't exist or if @a newName already
                exists.
    */
    virtual bool RenameGroup(const wxString& oldName,
                             const wxString& newName) = 0;

    //@}


    /**
        @name Delete Entries/Groups

        These functions delete entries and/or groups of entries from the config
        file. DeleteAll() is especially useful if you want to erase all traces
        of your program presence: for example, when you uninstall it.
    */
    //@{

    /**
        Delete the whole underlying object (disk file, registry key, ...).
        Primarily for use by uninstallation routine.
    */
    virtual bool DeleteAll() = 0;

    /**
        Deletes the specified entry and the group it belongs to if it was the
        last key in it and the second parameter is @true.
    */
    virtual bool DeleteEntry(const wxString& key,
                             bool bDeleteGroupIfEmpty = true) = 0;

    /**
        Delete the group (with all subgroups). If the current path is under the
        group being deleted it is changed to its deepest still existing
        component. E.g. if the current path is @c "/A/B/C/D" and the group @c C
        is deleted, the path becomes @c "/A/B".
    */
    virtual bool DeleteGroup(const wxString& key) = 0;

    //@}


    /**
        @name Options

        Some aspects of wxConfigBase behaviour can be changed during run-time.
        The first of them is the expansion of environment variables in the
        string values read from the config file: for example, if you have the
        following in your config file:

        @code
        # config file for my program
        UserData = $HOME/data

        # the following syntax is valid only under Windows
        UserData = %windir%\\data.dat
        @endcode

        The call to Read("UserData") will return something like
        @c "/home/zeitlin/data" on linux for example.

        Although this feature is very useful, it may be annoying if you read a
        value which contains '$' or '%' symbols (% is used for environment
        variables expansion under Windows) which are not used for environment
        variable expansion. In this situation you may call
        SetExpandEnvVars(@false) just before reading this value and
        SetExpandEnvVars(@true) just after. Another solution would be to prefix
        the offending symbols with a backslash.
    */
    //@{

    /**
        Returns @true if we are expanding environment variables in key values.
    */
    bool IsExpandingEnvVars() const;

    /**
        Returns @true if we are writing defaults back to the config file.
    */
    bool IsRecordingDefaults() const;

    /**
        Determine whether we wish to expand environment variables in key
        values.
    */
    void SetExpandEnvVars(bool bDoIt = true);

    /**
        Sets whether defaults are recorded to the config file whenever an
        attempt to read the value which is not present in it is done.

        If on (default is off) all default values for the settings used by the
        program are written back to the config file. This allows the user to
        see what config options may be changed and is probably useful only for
        wxFileConfig.
    */
    void SetRecordDefaults(bool bDoIt = true);

    //@}


    /**
        Create a new config object and sets it as the current one.

        This function will create the most appropriate implementation of
        wxConfig available for the current platform. By default this means that
        the system registry will be used for storing the configuration
        information under MSW and a file under the user home directory (see
        wxStandardPaths::GetUserConfigDir()) elsewhere.

        If you prefer to use the configuration files everywhere, you can define
        @c wxUSE_CONFIG_NATIVE to 0 when compiling wxWidgets. Or you can simply
        always create wxFileConfig explicitly.

        Finally, if you want to create a custom wxConfig subclass you may
        change this function behaviour by overriding wxAppTraits::CreateConfig()
        to create it. An example when this could be useful could be an
        application which could be installed either normally (in which case the
        default behaviour of using wxRegConfig is appropriate) or in a
        "portable" way in which case a wxFileConfig with a file in the program
        directory would be used and the choice would be done in CreateConfig()
        at run-time.
    */
    static wxConfigBase* Create();

    /**
        Calling this function will prevent @e Get() from automatically creating
        a new config object if the current one is @NULL. It might be useful to
        call it near the program end to prevent "accidental" creation of a new
        config object.
    */
    static void DontCreateOnDemand();

    /**
        Get the current config object. If there is no current object and
        @a CreateOnDemand is @true, this creates one (using Create()) unless
        DontCreateOnDemand() was called previously.
    */
    static wxConfigBase* Get(bool CreateOnDemand = true);

    /**
        Sets the config object as the current one, returns the pointer to the
        previous current object (both the parameter and returned value may be
        @NULL).
    */
    static wxConfigBase* Set(wxConfigBase* pConfig);
};


/**
    @class wxConfigPathChanger

    A handy little class which changes the current path in a wxConfig object and restores it in dtor.
    Declaring a local variable of this type, it's possible to work in a specific directory
    and ensure that the path is automatically restored when the function returns.

    For example:
    @code
    // this function loads some settings from the given wxConfig object;
    // the path selected inside it is left unchanged
    bool LoadMySettings(wxConfigBase* cfg)
    {
        wxConfigPathChanger changer(cfg, "/Foo/Data/SomeString");
        wxString str;
        if ( !config->Read("SomeString", &str) ) {
            wxLogError("Couldn't read SomeString!");
            return false;
                // NOTE: without wxConfigPathChanger it would be easy to forget to
                //       set the old path back into the wxConfig object before this return!
        }

        // do something useful with SomeString...

        return true;    // again: wxConfigPathChanger dtor will restore the original wxConfig path
    }
    @endcode

    @library{wxbase}
    @category{cfg}
*/
class wxConfigPathChanger
{
public:

    /**
        Changes the path of the given wxConfigBase object so that the key @a strEntry is accessible
        (for read or write).

        In other words, the ctor uses wxConfigBase::SetPath() with everything which precedes the
        last slash of @a strEntry, so that:
        @code
        wxConfigPathChanger(wxConfigBase::Get(), "/MyProgram/SomeKeyName");
        @endcode
        has the same effect of:
        @code
        wxConfigPathChanger(wxConfigBase::Get(), "/MyProgram/");
        @endcode
    */
    wxConfigPathChanger(const wxConfigBase *pContainer, const wxString& strEntry);

    /**
        Restores the path selected, inside the wxConfig object passed to the ctor, to the path which was
        selected when the wxConfigPathChanger ctor was called.
    */
    ~wxConfigPathChanger();

    /**
        Returns the name of the key which was passed to the ctor.
        The "name" is just anything which follows the last slash of the string given to the ctor.
    */
    const wxString& Name() const;

    /**
        This method must be called if the original path inside the wxConfig object
        (i.e. the current path at the moment of creation of this wxConfigPathChanger object)
        could have been deleted, thus preventing wxConfigPathChanger from restoring the not
        existing (any more) path.

        If the original path doesn't exist any more, the path will be restored to
        the deepest still existing component of the old path.
    */
    void UpdateIfDeleted();
};

