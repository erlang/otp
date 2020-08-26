/////////////////////////////////////////////////////////////////////////////
// Name:        dynlib.h
// Purpose:     interface of wxDynamicLibrary and wxDynamicLibraryDetails
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxDynamicLibraryDetails

    This class is used for the objects returned by the
    wxDynamicLibrary::ListLoaded() method and contains the information about a
    single module loaded into the address space of the current process. A
    module in this context may be either a dynamic library or the main program
    itself.

    @library{wxbase}
    @category{appmanagement}
*/
class wxDynamicLibraryDetails
{
public:
    /**
        Retrieves the load address and the size of this module.

        @param addr
            The pointer to the location to return load address in, may be
            @NULL.
        @param len
            Pointer to the location to return the size of this module in
            memory in, may be @NULL.

        @return @true if the load address and module size were retrieved,
                 @false if this information is not available.
    */
    bool GetAddress(void* addr, size_t* len) const;

    /**
        Returns the base name of this module, e.g.\ @c "kernel32.dll" or
        @c "libc-2.3.2.so".
    */
    wxString GetName() const;

    /**
        Returns the full path of this module if available, e.g.\ @c "c:\windows\system32\kernel32.dll"
        or @c "/lib/libc-2.3.2.so".
    */
    wxString GetPath() const;

    /**
        Returns the version of this module, e.g.\ @c "5.2.3790.0" or @c "2.3.2".
        The returned string is empty if the version information is not
        available.
    */
    wxString GetVersion() const;
};



/**
    Dynamic library category used with wxDynamicLibrary::CanonicalizeName().
*/
enum wxDynamicLibraryCategory
{
    wxDL_LIBRARY,   ///< Standard library.
    wxDL_MODULE     ///< Loadable module/plugin.
};

/**
    Dynamic library plugin category used with
    wxDynamicLibrary::CanonicalizePluginName().
*/
enum wxPluginCategory
{
    wxDL_PLUGIN_GUI,    ///< Plugin that uses GUI classes.
    wxDL_PLUGIN_BASE    ///< wxBase-only plugin.
};

/**
    @class wxDynamicLibrary

    wxDynamicLibrary is a class representing dynamically loadable library
    (Windows DLL, shared library under Unix etc). Just create an object of
    this class to load a library and don't worry about unloading it -- it will
    be done in the objects destructor automatically.

    The following flags can be used with wxDynamicLibrary() or Load():

    @beginStyleTable
    @style{wxDL_LAZY}
           Equivalent of RTLD_LAZY under Unix, ignored elsewhere.
    @style{wxDL_NOW}
           Equivalent of RTLD_NOW under Unix, ignored elsewhere.
    @style{wxDL_GLOBAL}
           Equivalent of RTLD_GLOBAL under Unix, ignored elsewhere.
    @style{wxDL_VERBATIM}
           Don't try to append the appropriate extension to the library name
           (this is done by default).
    @style{wxDL_DEFAULT}
           Default flags, same as wxDL_NOW currently.
    @style{wxDL_QUIET}
           Don't log an error message if the library couldn't be loaded.
    @endStyleTable

    @library{wxbase}
    @category{appmanagement}
*/
class wxDynamicLibrary
{
public:
    /**
        Default constructor.
    */
    wxDynamicLibrary();
    /**
        Constructor. Calls Load() with the given @a name.
    */
    wxDynamicLibrary(const wxString& name, int flags = wxDL_DEFAULT);

    /**
        Returns the platform-specific dynamic library file extension, or
        depending on @a flags, the plugin file extension. The leading dot
        is included.

        For example, on Windows @c ".dll" is returned, and either @c ".dylib"
        or @c ".bundle" on macOS.
    */
    static wxString GetDllExt(wxDynamicLibraryCategory cat = wxDL_LIBRARY);

    /**
        Returns the platform-specific full name for the library called @a name.
        E.g. it adds a @c ".dll" extension under Windows and @c "lib" prefix
        and @c ".so", @c ".sl" or @c ".dylib" extension under Unix.

        @see CanonicalizePluginName()
    */
    static wxString CanonicalizeName(const wxString& name,
                                     wxDynamicLibraryCategory cat = wxDL_LIBRARY);

    /**
        This function does the same thing as CanonicalizeName() but for
        wxWidgets plugins. The only difference is that compiler and version
        information are added to the name to ensure that the plugin which is
        going to be loaded will be compatible with the main program.
    */
    static wxString CanonicalizePluginName(const wxString& name,
                                           wxPluginCategory cat = wxDL_PLUGIN_GUI);

    /**
        Detaches this object from its library handle, i.e.\ the object will not
        unload the library any longer in its destructor but it is now the
        callers responsibility to do this using Unload().
    */
    wxDllType Detach();

    /**
        Return a valid handle for the main program itself or @NULL if symbols
        from the main program can't be loaded on this platform.
    */
    static wxDllType GetProgramHandle();

    /**
        Returns pointer to symbol @a name in the library or @NULL if the
        library contains no such symbol.

        @see wxDYNLIB_FUNCTION()
    */
    void* GetSymbol(const wxString& name, bool* success = 0) const;

    /**
        This function is available only under Windows as it is only useful when
        dynamically loading symbols from standard Windows DLLs. Such functions
        have either @c 'A' (in ANSI build) or @c 'W' (in Unicode, or wide
        character build) suffix if they take string parameters. Using this
        function, you can use just the base name of the function and the
        correct suffix is appended automatically depending on the current
        build. Otherwise, this method is identical to GetSymbol().

        @onlyfor{wxmsw}
    */
    void* GetSymbolAorW(const wxString& name) const;

    /**
        Returns @true if the symbol with the given @a name is present in the
        dynamic library, @false otherwise. Unlike GetSymbol(), this function
        doesn't log an error message if the symbol is not found.

        @since 2.5.4
    */
    bool HasSymbol(const wxString& name) const;

    /**
        Returns @true if the library was successfully loaded, @false otherwise.
    */
    bool IsLoaded() const;

    /**
        This static method returns a wxArray containing the details of all
        modules loaded into the address space of the current project. The array
        elements are objects of the type: wxDynamicLibraryDetails. The array
        will be empty if an error occurred.

        This method is currently implemented only under Win32 and Linux and is
        useful mostly for diagnostics purposes.
    */
    static wxDynamicLibraryDetailsArray ListLoaded();

    /**
        Returns the load address of the module containing the specified address
        or @NULL if not found.

        If the second argument @a path is not @NULL, it is filled with the full
        path to the file the module was loaded from upon a successful return.

        This method is implemented under MSW and Unix platforms providing
        `dladdr()` call (which include Linux and various BSD systems) and
        always returns @NULL elsewhere.

        @since 3.1.0
    */
    static void* GetModuleFromAddress(const void* addr, wxString* path = NULL);

    /**
        Loads DLL with the given @a name into memory. The @a flags argument can
        be a combination of the styles outlined in the class description.

        Returns @true if the library was successfully loaded, @false otherwise.
    */
    bool Load(const wxString& name, int flags = wxDL_DEFAULT);

    /**
        Unloads the library from memory. wxDynamicLibrary object automatically
        calls this method from its destructor if it had been successfully
        loaded.
    */
    void Unload();
    /**
        Unloads the library from memory. wxDynamicLibrary object automatically
        calls this method from its destructor if it had been successfully
        loaded.

        This version of Unload() is only used if you need to keep the library
        in memory during a longer period of time than the scope of the
        wxDynamicLibrary object. In this case you may call Detach() and store
        the handle somewhere and call this static method later to unload it.
    */
    static void Unload(wxDllType handle);
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_misc */
//@{

/**
    When loading a function from a DLL you always have to cast the returned
    <tt>void *</tt> pointer to the correct type and, even more annoyingly, you
    have to repeat this type twice if you want to declare and define a function
    pointer all in one line.

    This macro makes this slightly less painful by allowing you to specify the
    type only once, as the first parameter, and creating a variable of this
    type named after the function but with @c pfn prefix and initialized with
    the function @a name from the wxDynamicLibrary @a dynlib.

    @param type
        The type of the function.
    @param name
        The name of the function to load, not a string (without quotes, it is
        quoted automatically by the macro).
    @param dynlib
        The library to load the function from.

    @header{wx/dynlib.h}
*/
#define wxDYNLIB_FUNCTION(type, name, dynlib)

//@}

