/////////////////////////////////////////////////////////////////////////////
// Name:        filesys.h
// Purpose:     interface of wxFileSystem, wxFileSystemHandler, wxFSFile
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    Open Bit Flags
*/
enum wxFileSystemOpenFlags
{
    wxFS_READ = 1,      /**< Open for reading */
    wxFS_SEEKABLE = 4   /**< Returned stream will be seekable */
};


/**
    @class wxFileSystem

    This class provides an interface for opening files on different file systems.
    It can handle absolute and/or local filenames.

    It uses a system of handlers (see wxFileSystemHandler) to provide access to
    user-defined virtual file systems.

    @library{wxbase}
    @category{vfs}

    @see wxFileSystemHandler, wxFSFile, @ref overview_fs
*/
class wxFileSystem : public wxObject
{
public:
    /**
        Constructor.

        The initial current path of this object will be empty
        (i.e. GetPath() == wxEmptyString) which means that e.g. OpenFile()
        or FindFirst() functions will use current working directory as
        current path (see also wxGetCwd).
    */
    wxFileSystem();

    /**
        This static function adds new handler into the list of handlers
        (see wxFileSystemHandler) which provide access to virtual FS.

        Note that if two handlers for the same protocol are added, the last
        added one takes precedence.

        @note You can call:
              @code
              wxFileSystem::AddHandler(new My_FS_Handler);
              @endcode
              This is because (a) AddHandler is a static method, and (b) the
              handlers are deleted in wxFileSystem's destructor so that you
              don't have to care about it.
    */
    static void AddHandler(wxFileSystemHandler* handler);

    /**
       Remove a filesystem handler from the list of handlers.
    */
    static wxFileSystemHandler* RemoveHandler(wxFileSystemHandler *handler);

    /**
        Sets the current location. @a location parameter passed to OpenFile() is
        relative to this path.

        @remarks Unless @a is_dir is @true the @a location parameter is not the
                 directory name but the name of the file in this directory.

        All these commands change the path to "dir/subdir/":

        @code
        ChangePathTo("dir/subdir/xh.htm");
        ChangePathTo("dir/subdir", true);
        ChangePathTo("dir/subdir/", true);
        @endcode

        Example:
        @code
        f = fs->OpenFile("hello.htm"); // opens file 'hello.htm'
        fs->ChangePathTo("subdir/folder", true);
        f = fs->OpenFile("hello.htm"); // opens file 'subdir/folder/hello.htm' !!
        @endcode

        @param location
            the new location. Its meaning depends on the value of is_dir
        @param is_dir
            if @true location is new directory.
            If @false (the default) location is file in the new directory.
    */
    void ChangePathTo(const wxString& location, bool is_dir = false);

    /**
        Converts a wxFileName into an URL.

        @see URLToFileName(), wxFileName
    */
    static wxString FileNameToURL(const wxFileName& filename);

    /**
        Looks for the file with the given name @a file in a colon or semi-colon
        (depending on the current platform) separated list of directories in @a path.

        If the file is found in any directory, returns @true and the full path
        of the file in @a str, otherwise returns @false and doesn't modify @a str.

        @param pStr
            Receives the full path of the file, must not be @NULL
        @param path
            wxPATH_SEP-separated list of directories
        @param file
            the name of the file to look for
    */
    bool FindFileInPath(wxString* pStr, const wxString& path,
                        const wxString& file);

    /**
        Works like ::wxFindFirstFile().

        Returns the name of the first filename (within filesystem's current path)
        that matches @a wildcard.

        @param wildcard
            The wildcard that the filename must match
        @param flags
            One of wxFILE (only files), wxDIR (only directories) or 0 (both).
    */
    wxString FindFirst(const wxString& wildcard, int flags = 0);

    /**
        Returns the next filename that matches the parameters passed to FindFirst().
    */
    wxString FindNext();

    /**
        Returns the actual path (set by wxFileSystem::ChangePathTo).
    */
    wxString GetPath() const;

    /**
        This static function returns @true if there is a registered handler which can
        open the given location.
    */
    static bool HasHandlerForPath(const wxString& location);

    /**
        Opens the file and returns a pointer to a wxFSFile object or @NULL if failed.

        It first tries to open the file in relative scope (based on value passed to
        ChangePathTo() method) and then as an absolute path.

        Note that the user is responsible for deleting the returned wxFSFile.
        @a flags can be one or more of the ::wxFileSystemOpenFlags values
        combined together.

        A stream opened with just the default @e wxFS_READ flag may
        or may not be seekable depending on the underlying source.

        Passing @e "wxFS_READ | wxFS_SEEKABLE" for @a flags will back
        a stream that is not natively seekable with memory or a file
        and return a stream that is always seekable.

        @note
        The @a location argument is, despite this method's name @em not
        a filename. It is a "location", aka wxFileSystem URL (see
        @ref overview_fs).
    */
    wxFSFile* OpenFile(const wxString& location,
                       int flags = wxFS_READ);

    /**
        Converts URL into a well-formed filename.
        The URL must use the @c file protocol.
    */
    static wxFileName URLToFileName(const wxString& url);
};



/**
    @class wxFSFile

    This class represents a single file opened by wxFileSystem.
    It provides more information than wxWidgets' input streams
    (stream, filename, mime type, anchor).

    @note Any pointer returned by a method of wxFSFile is valid only as long as
          the wxFSFile object exists. For example a call to GetStream()
          doesn't @e create the stream but only returns the pointer to it.
          In other words after 10 calls to GetStream() you will have obtained
          ten identical pointers.

    @library{wxbase}
    @category{vfs,file}

    @see wxFileSystemHandler, wxFileSystem, @ref overview_fs
*/
class wxFSFile : public wxObject
{
public:
    /**
        Constructor. You probably won't use it. See the Note for details.

        It is seldom used by the application programmer but you will need it if
        you are writing your own virtual FS. For example you may need something
        similar to wxMemoryInputStream, but because wxMemoryInputStream doesn't
        free the memory when destroyed and thus passing a memory stream pointer
        into wxFSFile constructor would lead to memory leaks, you can write your
        own class derived from wxFSFile:

        @code
        class wxMyFSFile : public wxFSFile
        {
            private:
                void *m_Mem;
            public:
                wxMyFSFile(.....)
                ~wxMyFSFile() {free(m_Mem);}
                    // of course dtor is virtual ;-)
        };
        @endcode

        If you are not sure of the meaning of these params, see the description
        of the GetXXXX() functions.

        @param stream
            The input stream that will be used to access data
        @param location
            The full location (aka filename) of the file
        @param mimetype
            MIME type of this file. It may be left empty, in which
            case the type will be determined from file's extension (location must
            not be empty in this case).
        @param anchor
            Anchor. See GetAnchor() for details.
        @param modif
            Modification date and time for this file.
    */
    wxFSFile(wxInputStream* stream, const wxString& location,
             const wxString& mimetype, const wxString& anchor,
             wxDateTime modif);

    /**
        Detaches the stream from the wxFSFile object. That is, the
        stream obtained with GetStream() will continue its existence
        after the wxFSFile object is deleted.

        You will have to delete the stream yourself.
    */
    wxInputStream* DetachStream();

    /**
        Returns anchor (if present). The term of @b anchor can be easily
        explained using few examples:

        @verbatim
        index.htm#anchor                      // 'anchor' is anchor
        index/wx001.htm                       // NO anchor here!
        archive/main.zip#zip:index.htm#global // 'global'
        archive/main.zip#zip:index.htm        // NO anchor here!
        @endverbatim

        Usually an anchor is presented only if the MIME type is 'text/html'.
        But it may have some meaning with other files; for example myanim.avi#200
        may refer to position in animation or reality.wrl#MyView may refer
        to a predefined view in VRML.
    */
    const wxString& GetAnchor() const;

    /**
        Returns full location of the file, including path and protocol.

        Examples:
        @verbatim
        http://www.wxwidgets.org
        http://www.ms.mff.cuni.cz/~vsla8348/wxhtml/archive.zip#zip:info.txt
        file:/home/vasek/index.htm
        relative-file.htm
        @endverbatim
    */
    const wxString& GetLocation() const;

    /**
        Returns the MIME type of the content of this file.

        It is either extension-based (see wxMimeTypesManager) or extracted from
        HTTP protocol Content-Type header.
    */
    const wxString& GetMimeType() const;

    /**
        Returns time when this file was modified.
    */
    wxDateTime GetModificationTime() const;

    /**
        Returns pointer to the stream.

        You can use the returned stream to directly access data.
        You may suppose that the stream provide Seek and GetSize functionality
        (even in the case of the HTTP protocol which doesn't provide
        this by default. wxHtml uses local cache to work around
        this and to speed up the connection).
    */
    wxInputStream* GetStream() const;
};



/**
    @class wxFileSystemHandler

    Classes derived from wxFileSystemHandler are used to access virtual file systems.

    Its public interface consists of two methods: wxFileSystemHandler::CanOpen
    and wxFileSystemHandler::OpenFile.

    It provides additional protected methods to simplify the process
    of opening the file: GetProtocol(), GetLeftLocation(), GetRightLocation(),
    GetAnchor(), GetMimeTypeFromExt().

    Please have a look at overview (see wxFileSystem) if you don't know how locations
    are constructed.

    Also consult the @ref overview_fs_wxhtmlfs "list of available handlers".

    Note that the handlers are shared by all instances of wxFileSystem.

    @remarks
    wxHTML library provides handlers for local files and HTTP or FTP protocol.

    @note
    The location parameter passed to OpenFile() or CanOpen() methods is always an
    absolute path. You don't need to check the FS's current path.

    @beginWxPerlOnly
    In wxPerl, you need to derive your file system handler class
    from @c Wx::PlFileSystemHandler.
    @endWxPerlOnly

    @library{wxbase}
    @category{vfs}

    @see wxFileSystem, wxFSFile, @ref overview_fs
*/
class wxFileSystemHandler : public wxObject
{
public:
    /**
        Constructor.
    */
    wxFileSystemHandler();

    /**
        Returns @true if the handler is able to open this file. This function doesn't
        check whether the file exists or not, it only checks if it knows the protocol.
        Example:

        @code
        bool MyHand::CanOpen(const wxString& location)
        {
            return (GetProtocol(location) == "http");
        }
        @endcode

        Must be overridden in derived handlers.
    */
    virtual bool CanOpen(const wxString& location) = 0;

    /**
        Works like ::wxFindFirstFile().

        Returns the name of the first filename (within filesystem's current path)
        that matches @e wildcard. @a flags may be one of wxFILE (only files),
        wxDIR (only directories) or 0 (both).

        This method is only called if CanOpen() returns @true.
    */
    virtual wxString FindFirst(const wxString& wildcard,
                               int flags = 0);

    /**
        Returns next filename that matches parameters passed to wxFileSystem::FindFirst.

        This method is only called if CanOpen() returns @true and FindFirst()
        returned a non-empty string.
    */
    virtual wxString FindNext();

    /**
        Returns the MIME type based on @b extension of @a location.
        (While wxFSFile::GetMimeType() returns real MIME type - either
         extension-based or queried from HTTP.)

        Example:
        @code
        GetMimeTypeFromExt("index.htm") == "text/html"
        @endcode
    */
    static wxString GetMimeTypeFromExt(const wxString& location);

    /**
        Opens the file and returns wxFSFile pointer or @NULL if failed.
        Must be overridden in derived handlers.

        @param fs
            Parent FS (the FS from that OpenFile was called).
            See the ZIP handler for details of how to use it.
        @param location
            The absolute location of file.
    */
    virtual wxFSFile* OpenFile(wxFileSystem& fs, const wxString& location) = 0;

protected:

    /**
        Returns the anchor if present in the location.
        See wxFSFile::GetAnchor for details.

        Example:
        @code
        GetAnchor("index.htm#chapter2") == "chapter2"
        @endcode

        @note the anchor is NOT part of the left location.
    */
    static wxString GetAnchor(const wxString& location);

    /**
        Returns the left location string extracted from @e location.

        Example:
        @code
        GetLeftLocation("file:myzipfile.zip#zip:index.htm") == "file:myzipfile.zip"
        @endcode
    */
    static wxString GetLeftLocation(const wxString& location);

    /**
        Returns the protocol string extracted from @a location.

        Example:
        @code
        GetProtocol("file:myzipfile.zip#zip:index.htm") == "zip"
        @endcode
    */
    static wxString GetProtocol(const wxString& location);

    /**
        Returns the right location string extracted from @a location.

        Example:
        @code
        GetRightLocation("file:myzipfile.zip#zip:index.htm") == "index.htm"
        @endcode
    */
    static wxString GetRightLocation(const wxString& location);
};


/**
    Input stream for virtual file stream files.

    The stream reads data from wxFSFile obtained from wxFileSystem. It is
    especially useful to allow using virtual files with other wxWidgets
    functions and classes working with streams, e.g. for loading images or
    animations from virtual files and not only physical ones.

    @library{wxbase}
    @category{streams}

    @see wxWrapperInputStream, wxFSFile

    @since 2.9.4
*/
class wxFSInputStream : public wxWrapperInputStream
{
public:
    /**
        Create a stream associated with the data of the given virtual file
        system file.

        @param filename
            The name of the input file passed to wxFileSystem::OpenFile().
        @param flags
            Combination of flags from wxFileSystemOpenFlags. ::wxFS_READ is
            implied, i.e. it is always added to the flags value.

        Use wxStreamBase::IsOk() to verify if the constructor succeeded.
    */
    wxFileInputStream(const wxString& filename, int flags = 0);

    /**
        Returns @true if the stream is initialized and ready.
    */
    bool IsOk() const;
};
