/////////////////////////////////////////////////////////////////////////////
// Name:        mimetype.h
// Purpose:     interface of wxMimeTypesManager
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxMimeTypesManager

    This class allows the application to retrieve information about all known
    MIME types from a system-specific location and the filename extensions to the
    MIME types and vice versa.

    MIME stands for "Multipurpose Internet Mail Extensions" and was originally
    used in mail protocols. It's standardized by several RFCs.

    Under Windows, the MIME type information is queried from registry.
    Under Linux and Unix, it is queried from the XDG data directories.

    Currently, wxMimeTypesManager is limited to @e reading MIME type information.

    The application should not construct its own manager: it should use the
    object pointer ::wxTheMimeTypesManager.
    The functions GetFileTypeFromMimeType() and GetFileTypeFromExtension()
    return a wxFileType object which may be further queried for file description,
    icon and other attributes.

    @section mimetypemanager_helpers Helper functions

    All of these functions are static (i.e. don't need a wxMimeTypesManager object
    to call them) and provide some useful operations for string representations of
    MIME types. Their usage is recommended instead of directly working with MIME
    types using wxString functions.

    - wxMimeTypesManager::IsOfType()

    @section mimetypemanager_query Query database

    These functions are the heart of this class: they allow to find a file type
    object from either file extension or MIME type.
    If the function is successful, it returns a pointer to the wxFileType object
    which must be deleted by the caller, otherwise @NULL will be returned.

    - wxMimeTypesManager::GetFileTypeFromMimeType()
    - wxMimeTypesManager::GetFileTypeFromExtension()

    @library{wxbase}
    @category{cfg}

    @see wxFileType
*/
class wxMimeTypesManager
{
public:
    /**
        Constructor puts the object in the "working" state.
    */
    wxMimeTypesManager();

    /**
        Destructor is not virtual, so this class should not be derived from.
    */
    ~wxMimeTypesManager();

    /**
        This function may be used to provide hard-wired fallbacks for the MIME types
        and extensions that might not be present in the system MIME database.
        Please see the typetest sample for an example of using it.
    */
    void AddFallbacks(const wxFileTypeInfo* fallbacks);

    /**
        Gather information about the files with given extension and return the
        corresponding wxFileType object or @NULL if the extension is unknown.

        The @a extension parameter may have, or not, the leading dot, if it has it,
        it is stripped automatically. It must not however be empty.
    */
    wxFileType* GetFileTypeFromExtension(const wxString& extension);

    /**
        Gather information about the files with given MIME type and return the
        corresponding wxFileType object or @NULL if the MIME type is unknown.
    */
    wxFileType* GetFileTypeFromMimeType(const wxString& mimeType);


    /**
        This function returns @true if either the given @a mimeType is exactly
        the same as @a wildcard or if it has the same category and the subtype of
        @a wildcard is '*'. Note that the '*' wildcard is not allowed in
        @a mimeType itself.

        The comparison done by this function is case insensitive so it is not
        necessary to convert the strings to the same case before calling it.
    */
    static bool IsOfType(const wxString& mimeType, const wxString& wildcard);


    /**
       Create a new association using the fields of wxFileTypeInfo (at least
       the MIME type and the extension should be set).
    */
    wxFileType *Associate(const wxFileTypeInfo& ftInfo);

    /**
       Undo Associate().
    */
    bool Unassociate(wxFileType *ft) ;

    /**
       Enumerate all known file types.  Returns the number of retrieved items.
     */
    size_t EnumAllFileTypes(wxArrayString& mimetypes);

};


/**
    The global wxMimeTypesManager instance.
*/
wxMimeTypesManager* wxTheMimeTypesManager;



/**
    @class wxFileType

    This class holds information about a given @e file type.

    File type is the same as MIME type under Unix, but under Windows it corresponds
    more to an extension than to MIME type (in fact, several extensions may
    correspond to a file type).

    This object may be created in several different ways: the program might know the
    file extension and wish to find out the corresponding MIME type or, conversely, it
    might want to find the right extension for the file to which it writes the
    contents of given MIME type. Depending on how it was created some fields may be
    unknown so the return value of all the accessors @b must be checked: @false
    will be returned if the corresponding information couldn't be found.

    The objects of this class are never created by the application code but are
    returned by wxMimeTypesManager::GetFileTypeFromMimeType and
    wxMimeTypesManager::GetFileTypeFromExtension methods.
    But it is your responsibility to delete the returned pointer when you're done
    with it!

    A brief reminder about what the MIME types are (see the RFC 1341 for more
    information): basically, it is just a pair category/type (for example,
    "text/plain") where the category is a basic indication of what a file is.
    Examples of categories are "application", "image", "text", "binary", and
    type is a precise definition of the document format: "plain" in the example
    above means just ASCII text without any formatting, while "text/html" is the
    HTML document source.

    A MIME type may have one or more associated extensions: "text/plain" will
    typically correspond to the extension ".txt", but may as well be associated with
    ".ini" or ".conf".


    @section filetype_example MessageParameters class

    One of the most common usages of MIME is to encode an e-mail message.
    The MIME type of the encoded message is an example of a message parameter.
    These parameters are found in the message headers ("Content-XXX").

    At the very least, they must specify the MIME type and the version of MIME
    used, but almost always they provide additional information about the message
    such as the original file name or the charset (for the text documents).
    These parameters may be useful to the program used to open, edit, view or
    print the message, so, for example, an e-mail client program will have to
    pass them to this program. Because wxFileType itself cannot know about
    these parameters, it uses MessageParameters class to query them.

    The default implementation only requires the caller to provide the file name
    (always used by the program to be called - it must know which file to open)
    and the MIME type and supposes that there are no other parameters.

    If you wish to supply additional parameters, you must derive your own class
    from MessageParameters and override GetParamValue() function, for example:

    @code
    // provide the message parameters for the MIME type manager
    class MailMessageParameters : public wxFileType::MessageParameters
    {
    public:
        MailMessageParameters(const wxString& filename,
                                const wxString& mimetype)
            : wxFileType::MessageParameters(filename, mimetype)
        {
        }

        virtual wxString GetParamValue(const wxString& name) const
        {
            // parameter names are not case-sensitive
            if ( name.CmpNoCase("charset") == 0 )
                return "US-ASCII";
            else
                return wxFileType::MessageParameters::GetParamValue(name);
        }
    };
    @endcode

    Now you only need to create an object of this class and pass it to, for example,
    GetOpenCommand like this:

    @code
    wxString command;
    if ( filetype->GetOpenCommand(&command,
                                MailMessageParameters("foo.txt", "text/plain")) )
    {
        // the full command for opening the text documents is in 'command'
        // (it might be "notepad foo.txt" under Windows or "cat foo.txt" under Unix)
    }
    else
    {
        // we don't know how to handle such files...
    }
    @endcode

    Windows: As only the file name is used by the program associated with the
    given extension anyhow (but no other message parameters), there is no need
    to ever derive from MessageParameters class for a Windows-only program.


    @library{wxbase}
    @category{data}

    @see wxMimeTypesManager
*/
class wxFileType
{
private:
    /**
        The default constructor is private because you should never create objects of
        this type: they are only returned by wxMimeTypesManager methods.
    */
    wxFileType();

public:
    /**
        Class representing message parameters.

        An object of this class may be passed to wxFileType::GetOpenCommand()
        and GetPrintCommand() if more than the file name needs to be specified.
     */
    class MessageParameters
    {
    public:
        /// Trivial default constructor.
        MessageParameters();

        /// Constructor taking a filename and a mime type.
        MessageParameters(const wxString& filename,
                          const wxString& mimetype = wxEmptyString);

        /// Return the filename.
        const wxString& GetFileName() const;

        /// Return the MIME type.
        const wxString& GetMimeType() const;

        /// Overridable method for derived classes. Returns empty string by default.
        virtual wxString GetParamValue(const wxString& name) const;

        /// Trivial but virtual dtor as this class can be inherited from.
        virtual ~MessageParameters();
    };

    /**
        Copy ctor.
    */
    wxFileType(const wxFileTypeInfo& ftInfo);

    /**
        The destructor of this class is not virtual, so it should not be derived from.
    */
    ~wxFileType();

    /**
        This function is primarily intended for GetOpenCommand and GetPrintCommand
        usage but may be also used by the application directly if, for example, you
        want to use some non-default command to open the file.

        The function replaces all occurrences of:
        - %s with the full file name
        - %t with the MIME type
        - %{param} with the value of the parameter @e param
        using the MessageParameters object you pass to it.

        If there is no '%s' in the command string (and the string is not empty), it is
        assumed that the command reads the data on stdin and so the effect is the same
        as " %s" were appended to the string.

        Unlike all other functions of this class, there is no error return for this
        function.
    */
    static wxString ExpandCommand(const wxString& command,
                                  const MessageParameters& params);

    /**
        If the function returns @true, the string pointed to by @a desc is filled
        with a brief description for this file type: for example, "text document" for
        the "text/plain" MIME type.
    */
    bool GetDescription(wxString* desc) const;

    /**
        If the function returns @true, the array @a extensions is filled
        with all extensions associated with this file type: for example, it may
        contain the following two elements for the MIME type "text/html"
        (notice the absence of the leading dot): "html" and "htm".

        @b Windows: This function is currently not implemented: there is no
        (efficient) way to retrieve associated extensions from the given MIME type
        on this platform, so it will only return @true if the wxFileType object was
        created by wxMimeTypesManager::GetFileTypeFromExtension function in the
        first place.
    */
    bool GetExtensions(wxArrayString& extensions);

    /**
        If the function returns @true, the @c iconLoc is filled with the
        location of the icon for this MIME type.

        A wxIcon may be created from @a iconLoc later.

        \note Under Unix MIME manager gathers information about icons from
        GNOME and KDE settings and thus GetIcon's success depends on
        availability of these desktop environments.
    */
    bool GetIcon(wxIconLocation* iconLoc) const;

    /**
        If the function returns @true, the string pointed to by @a mimeType is filled
        with full MIME type specification for this file type: for example, "text/plain".
    */
    bool GetMimeType(wxString* mimeType) const;

    /**
        Same as GetMimeType() but returns array of MIME types.

        This array will contain only one item in most cases but sometimes,
        notably under Unix with KDE, may contain more MIME types.
        This happens when one file extension is mapped to different MIME types
        by KDE, mailcap and mime.types.
    */
    bool GetMimeTypes(wxArrayString& mimeTypes) const;

    //@{
    /**
        With the first version of this method, if the @true is returned, the
        string pointed to by @a command is filled with the command which must be
        executed (see wxExecute()) in order to open the file of the given type.

        In this case, the name of the file as well as any other parameters
        is retrieved from MessageParameters() class.

        In the second case, only the filename is specified and the command to be used
        to open this kind of file is returned directly. An empty string is returned to
        indicate that an error occurred (typically meaning that there is no standard way
        to open this kind of files).
    */
    bool GetOpenCommand(wxString* command, const MessageParameters& params);
    wxString GetOpenCommand(const wxString& filename) const;
    //@}

    /**
        If the function returns @true, the string pointed to by @a command is filled
        with the command which must be executed (see wxExecute()) in order to
        print the file of the given type.

        The name of the file is retrieved from the MessageParameters class.
    */
    bool GetPrintCommand(wxString* command,
                         const MessageParameters& params) const;


    /**
        The returned string is the command to be executed in order to
        open/print/edit the file of the given type.

        If the string is empty, the lookup for the @a verb failed.

        The name of the file is retrieved from the MessageParameters class.

        @see wxExecute()

        @since 3.1.1
    */
    wxString GetExpandedCommand(const wxString& verb,
                                const wxFileType::MessageParameters& params) const;

    /**
       Returns the number of commands for this mime type, and fills the verbs
       and commands arrays with the command information.
     */
    size_t GetAllCommands(wxArrayString *verbs, wxArrayString *commands,
                          const wxFileType::MessageParameters& params) const;
};



/**
    Container of information about wxFileType.

    This class simply stores information associated with the file type. It
    doesn't do anything on its own and is used only to allow constructing
    wxFileType from it (instead of specifying all the constituent pieces
    separately) and also with wxMimeTypesManager::AddFallbacks().
 */
class wxFileTypeInfo
{
public:
    /**
        Default constructor creates an invalid file type info object.

        Such invalid/empty object should be used to terminate the list of file
        types passed to wxMimeTypesManager::AddFallbacks().
     */
    wxFileTypeInfo();

    /**
        Constructor specifying just the MIME type name.

        Use the various setter methods below to fully initialize the object.

        @since 2.9.2
     */
    wxFileTypeInfo(const wxString& mimeType);

    /**
        Constructor allowing to specify all the fields at once.

        This is a vararg constructor taking an arbitrary number of extensions
        after the first four required parameters. The list must be terminated
        by @c wxNullPtr, notice that @c NULL can't be used here in portable
        code (C++0x @c nullptr can be used as well if your compiler supports
        it).
     */
    wxFileTypeInfo(const wxString& mimeType,
                   const wxString& openCmd,
                   const wxString& printCmd,
                   const wxString& description,
                   const wxString& extension,
                   ...);

    /**
       Constructor using an array of string elements corresponding to the
       parameters of the ctor above in the same order.
    */
    wxFileTypeInfo(const wxArrayString& sArray);

    /**
        Add another extension associated with this file type.

        @since 2.9.2
     */
    void AddExtension(const wxString& ext);

    /**
        Set the file type description.

        @since 2.9.2
     */
    void SetDescription(const wxString& description);

    /**
        Set the command to be used for opening files of this type.

        @since 2.9.2
     */
    void SetOpenCommand(const wxString& command);

    /**
        Set the command to be used for printing files of this type.

        @since 2.9.2
     */
    void SetPrintCommand(const wxString& command);

    /**
        Set the short description for the files of this type.

        This is only used under MSW for some of the registry keys used for the
        file type registration.
     */
    void SetShortDesc(const wxString& shortDesc);

    /**
       Set the icon information.
    */
    void SetIcon(const wxString& iconFile, int iconIndex = 0);

    /**
       Get the MIME type
    */
    const wxString& GetMimeType() const;

    /**
       Get the open command
    */
    const wxString& GetOpenCommand() const;

    /**
       Get the print command
    */
    const wxString& GetPrintCommand() const;

    /**
       Get the short description (only used under Win32 so far)
    */
    const wxString& GetShortDesc() const;

    /**
       Get the long, user visible description
    */
    const wxString& GetDescription() const;

    /**
       Get the array of all extensions
    */
    const wxArrayString& GetExtensions() const;

    /**
       Get the number of extensions.
    */
    size_t GetExtensionsCount() const;

    /**
       Get the icon filename
    */
    const wxString& GetIconFile() const;

    /**
       Get the index of the icon within the icon file.
    */
    int GetIconIndex() const;

};
