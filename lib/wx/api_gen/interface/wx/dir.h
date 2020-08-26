/////////////////////////////////////////////////////////////////////////////
// Name:        dir.h
// Purpose:     interface of wxDir and wxDirTraverser
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Possible return values of wxDirTraverser callback functions.
*/
enum wxDirTraverseResult
{
    wxDIR_IGNORE = -1,  ///< Ignore this directory but continue with others.
    wxDIR_STOP,         ///< Stop traversing.
    wxDIR_CONTINUE      ///< Continue into this directory.
};

/**
    The return value of wxDir::GetTotalSize() in case of error.
*/
wxULongLong wxInvalidSize;

/**
    @class wxDirTraverser

    wxDirTraverser is an abstract interface which must be implemented by
    objects passed to wxDir::Traverse() function.

    Example of use (this works almost like wxDir::GetAllFiles()):

    @code
    class wxDirTraverserSimple : public wxDirTraverser
    {
    public:
        wxDirTraverserSimple(wxArrayString& files) : m_files(files) { }

        virtual wxDirTraverseResult OnFile(const wxString& filename)
        {
            m_files.Add(filename);
            return wxDIR_CONTINUE;
        }

        virtual wxDirTraverseResult OnDir(const wxString& WXUNUSED(dirname))
        {
            return wxDIR_CONTINUE;
        }

    private:
        wxArrayString& m_files;
    };

    // get the names of all files in the array
    wxArrayString files;
    wxDirTraverserSimple traverser(files);

    wxDir dir(dirname);
    dir.Traverse(traverser);
    @endcode

    @library{wxbase}
    @category{file}
*/
class wxDirTraverser
{
public:
    /**
        This function is called for each directory. It may return ::wxDIR_STOP
        to abort traversing completely, ::wxDIR_IGNORE to skip this directory
        but continue with others or ::wxDIR_CONTINUE to enumerate all files and
        subdirectories in this directory.

        This is a pure virtual function and must be implemented in the derived
        class.
    */
    virtual wxDirTraverseResult OnDir(const wxString& dirname) = 0;

    /**
        This function is called for each file. It may return ::wxDIR_STOP to
        abort traversing (for example, if the file being searched is found) or
        ::wxDIR_CONTINUE to proceed.

        This is a pure virtual function and must be implemented in the derived
        class.
    */
    virtual wxDirTraverseResult OnFile(const wxString& filename) = 0;

    /**
        This function is called for each directory which we failed to open for
        enumerating. It may return ::wxDIR_STOP to abort traversing completely,
        ::wxDIR_IGNORE to skip this directory but continue with others or
        ::wxDIR_CONTINUE to retry opening this directory once again.

        The base class version always returns ::wxDIR_IGNORE.
    */
    virtual wxDirTraverseResult OnOpenError(const wxString& openerrorname);
};



/**
    These flags affect the behaviour of GetFirst/GetNext() and Traverse(),
    determining what types are included in the list of items they produce.
*/
enum wxDirFlags
{
    wxDIR_FILES     = 0x0001,   ///< Includes files.
    wxDIR_DIRS      = 0x0002,   ///< Includes directories.
    wxDIR_HIDDEN    = 0x0004,   ///< Includes hidden files.
    wxDIR_DOTDOT    = 0x0008,   ///< Includes "." and "..".

    /**
        Don't follow symbolic links during the directory traversal.

        This flag is ignored under systems not supporting symbolic links (i.e.
        non-Unix ones).

        Notice that this flag is @e not included in wxDIR_DEFAULT and so the
        default behaviour of wxDir::Traverse() is to follow symbolic links,
        even if they lead outside of the directory being traversed.

        @since 2.9.5
     */
    wxDIR_NO_FOLLOW = 0x0010,

    /**
        Default directory traversal flags include both files and directories,
        even hidden.

        Notice that by default wxDIR_NO_FOLLOW is @e not included, meaning that
        symbolic links are followed by default. If this is not desired, you
        must pass that flag explicitly.
     */
    wxDIR_DEFAULT   = wxDIR_FILES | wxDIR_DIRS | wxDIR_HIDDEN
};

/**
    @class wxDir

    wxDir is a portable equivalent of Unix open/read/closedir functions which
    allow enumerating of the files in a directory. wxDir allows enumerating
    files as well as directories.

    wxDir also provides a flexible way to enumerate files recursively using
    Traverse() or a simpler GetAllFiles() function.

    Example of use:

    @code
    wxDir dir(wxGetCwd());

    if ( !dir.IsOpened() )
    {
        // deal with the error here - wxDir would already log an error message
        // explaining the exact reason of the failure
        return;
    }

    puts("Enumerating object files in current directory:");

    wxString filename;

    bool cont = dir.GetFirst(&filename, filespec, flags);
    while ( cont )
    {
        printf("%s\n", filename.c_str());

        cont = dir.GetNext(&filename);
    }
    @endcode

    @library{wxbase}
    @category{file}
*/
class wxDir
{
public:
    /**
        Default constructor, use Open() afterwards.
    */
    wxDir();
    /**
        Opens the directory for enumeration, use IsOpened() to test for errors.
    */
    wxDir(const wxString& dir);

    /**
        Destructor cleans up the associated resources by calling Close().

        It is not virtual and so this class is not meant to be used
        polymorphically.
    */
    ~wxDir();

    /**
        Close the directory.

        The object can't be used after closing it, but Open() may be called
        again to reopen it later.

        @since 2.9.5
    */
    void Close();

    /**
        Test for existence of a directory with the given name.
    */
    static bool Exists(const wxString& dir);

    /**
        The function returns the path of the first file matching the given
        @a filespec or an empty string if there are no files matching it.

        The @a flags parameter may or may not include ::wxDIR_FILES, the
        function always behaves as if it were specified. By default, @a flags
        includes ::wxDIR_DIRS and so the function recurses into the
        subdirectories but if this flag is not specified, the function
        restricts the search only to the directory @a dirname itself.
        See ::wxDirFlags for the list of the possible flags.

        @see Traverse()
    */
    static wxString FindFirst(const wxString& dirname,
                              const wxString& filespec,
                              int flags = wxDIR_DEFAULT);

    /**
        The function appends the names of all the files under directory
        @a dirname to the array @a files (note that its old content is
        preserved). Only files matching the @a filespec are taken, with empty
        spec matching all the files.

        The @a flags parameter should always include ::wxDIR_FILES or the array
        would be unchanged and should include ::wxDIR_DIRS flag to recurse into
        subdirectories (both flags are included in the value by default).
        See ::wxDirFlags for the list of the possible flags.

        @return Returns the total number of files found while traversing
                the directory @a dirname (i.e. the number of entries appended
                to the @a files array).

        @see Traverse()
    */
    static size_t GetAllFiles(const wxString& dirname, wxArrayString* files,
                              const wxString& filespec = wxEmptyString,
                              int flags = wxDIR_DEFAULT);

    /**
        Start enumerating all files matching @a filespec (or all files if it is
        empty) and @e flags, return @true on success.
        See ::wxDirFlags for the list of the possible flags.
    */
    bool GetFirst(wxString* filename,
                  const wxString& filespec = wxEmptyString,
                  int flags = wxDIR_DEFAULT) const;

    /**
        Returns the name of the directory itself.

        The returned string does not have the trailing path separator (slash or
        backslash).

        Notice that in spite of this the last character of the returned string
        can still be the path separator if this directory is the root one.
        Because of this, don't append @c wxFILE_SEP_PATH to the returned value
        if you do need a slash-terminated directory name but use
        GetNameWithSep() instead to avoid having duplicate consecutive slashes.
    */
    wxString GetName() const;

    /**
        Returns the name of the directory with the path separator appended.

        The last character of the returned string is always @c wxFILE_SEP_PATH
        unless the string is empty, indicating that this directory is invalid.

        @see GetName()

        @since 2.9.4
     */
    wxString GetNameWithSep() const;

    /**
        Continue enumerating files which satisfy the criteria specified by the
        last call to GetFirst().
    */
    bool GetNext(wxString* filename) const;

    /**
        Returns the size (in bytes) of all files recursively found in @c dir or
        @c wxInvalidSize in case of error.

        In case it happens that while traversing folders a file's size cannot
        be read, that file is added to the @a filesSkipped array, if not @NULL,
        and then skipped. This usually happens with some special folders which
        are locked by the operating system or by another process. Remember that
        when the size of @a filesSkipped is not zero, then the returned value
        is not 100% accurate and, if the skipped files were big, it could be
        far from real size of the directory.

        @see wxFileName::GetHumanReadableSize(), wxGetDiskSpace()
    */
    static wxULongLong GetTotalSize(const wxString& dir,
                                    wxArrayString* filesSkipped = NULL);

    /**
        Returns @true if the directory contains any files matching the given
        @a filespec. If @a filespec is empty, look for any files at all. In any
        case, even hidden files are taken into account.
    */
    bool HasFiles(const wxString& filespec = wxEmptyString) const;

    /**
        Returns @true if the directory contains any subdirectories (if a non
        empty @a dirspec is given, only check for directories matching it).
        The hidden subdirectories are taken into account as well.
    */
    bool HasSubDirs(const wxString& dirspec = wxEmptyString) const;

    /**
        Returns @true if the directory was successfully opened by a previous
        call to Open().
    */
    bool IsOpened() const;

    /**
        Creates a directory.

        This is just an alias for wxFileName::Mkdir(); refer to that function
        for more info.
    */
    static bool Make(const wxString &dir, int perm = wxS_DIR_DEFAULT,
                     int flags = 0);

    /**
        Open the directory for enumerating, returns @true on success or @false
        if an error occurred.
    */
    bool Open(const wxString& dir);

    /**
        Removes a directory.

        This is just an alias for wxFileName::Rmdir(); refer to that function
        for more info.
    */
    static bool Remove(const wxString &dir, int flags = 0);

    /**
        Enumerate all files and directories under the given directory.

        If @a flags contains ::wxDIR_DIRS this enumeration is recursive, i.e.
        all the subdirectories of the given one and the files inside them will
        be traversed. Otherwise only the files in this directory itself are.

        If @a flags doesn't contain ::wxDIR_FILES then only subdirectories are
        examined but not normal files. It doesn't make sense to not specify
        either ::wxDIR_DIRS or ::wxDIR_FILES and usually both of them should be
        specified, as is the case by default.

        For each directory found, @ref wxDirTraverser::OnDir() "sink.OnDir()"
        is called and @ref wxDirTraverser::OnFile() "sink.OnFile()" is called
        for every file. Depending on the return value, the enumeration may
        continue or stop. If entering a subdirectory fails, @ref
        wxDirTraverser::OnOpenError() "sink.OnOpenError()" is called.

        The function returns the total number of files found or @c "(size_t)-1"
        on error.

        See ::wxDirFlags for the full list of the possible flags.

        @see GetAllFiles()
    */
    size_t Traverse(wxDirTraverser& sink,
                    const wxString& filespec = wxEmptyString,
                    int flags = wxDIR_DEFAULT) const;
};

