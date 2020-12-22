/////////////////////////////////////////////////////////////////////////////
// Name:        debugrpt.h
// Purpose:     interface of wxDebugReport*
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxDebugReportPreview

    This class presents the debug report to the user and allows him to veto
    report entirely or remove some parts of it. Although not mandatory, using
    this class is strongly recommended as data included in the debug report
    might contain sensitive private information and the user should be notified
    about it as well as having a possibility to examine the data which had been
    gathered to check whether this is effectively the case and discard the
    debug report if it is.

    wxDebugReportPreview is an abstract base class, currently the only concrete
    class deriving from it is wxDebugReportPreviewStd.

    @library{wxqa}
    @category{debugging}
*/
class wxDebugReportPreview
{
public:
    /**
        Default constructor.
    */
    wxDebugReportPreview();

    /**
        Destructor is trivial as well but should be virtual for a base class.
    */
    virtual ~wxDebugReportPreview();

    /**
        Present the report to the user and allow him to modify it by removing
        some or all of the files and, potentially, adding some notes.

        @return @true if the report should be processed or @false if the user
                 chose to cancel report generation or removed all files from
                 it.
    */
    virtual bool Show(wxDebugReport& dbgrpt) const = 0;
};



/**
    @class wxDebugReportCompress

    wxDebugReportCompress is a wxDebugReport which compresses all the files in
    this debug report into a single ZIP file in its wxDebugReport::Process()
    function.

    @library{wxqa}
    @category{debugging}
*/
class wxDebugReportCompress : public wxDebugReport
{
public:
    /**
        Default constructor does nothing special.
    */
    wxDebugReportCompress();

    /**
        Set the directory where the debug report should be generated.

        By default, the debug report is generated under user temporary files
        directory. This is usually fine if it is meant to be processed in some
        way (e.g. automatically uploaded to a remote server) but if the user is
        asked to manually upload or send the report, it may be more convenient
        to generate it in e.g. the users home directory and this function
        allows doing this.

        Notice that it should be called before wxDebugReport::Process() or it
        has no effect.

        @param dir
            The full path to an existing directory where the debug report file
            should be generated.

        @since 2.9.1
     */
    void SetCompressedFileDirectory(const wxString& dir);

    /**
        Set the base name of the generated debug report file.

        This function is similar to SetCompressedFileDirectory() but allows
        changing the base name of the file. Notice that the file extension will
        always be @c .zip.

        By default, a unique name constructed from wxApp::GetAppName(), the
        current process id and the current date and time is used.

        @param name
            The base name (i.e. without extension) of the file.

        @since 2.9.1
     */
    void SetCompressedFileBaseName(const wxString& name);

    /**
        Returns the full path of the compressed file (empty if creation
        failed).
    */
    const wxString& GetCompressedFileName() const;
};



/**
    @class wxDebugReport

    wxDebugReport is used to generate a debug report, containing information
    about the program current state. It is usually used from
    wxApp::OnFatalException() as shown in the @ref page_samples_debugrpt.

    A wxDebugReport object contains one or more files. A few of them can be
    created by the class itself but more can be created from the outside and
    then added to the report. Also note that several virtual functions may be
    overridden to further customize the class behaviour.

    Once a report is fully assembled, it can simply be left in the temporary
    directory so that the user can email it to the developers (in which case
    you should still use wxDebugReportCompress to compress it in a single file)
    or uploaded to a Web server using wxDebugReportUpload (setting up the Web
    server to accept uploads is your responsibility, of course). Other
    handlers, for example for automatically emailing the report, can be defined
    as well but are not currently included in wxWidgets.

    A typical usage example:

    @code
    wxDebugReport report;
    wxDebugReportPreviewStd preview;

    report.AddCurrentContext();  // could also use AddAll()
    report.AddCurrentDump();     // to do both at once

    if ( preview.Show(report) )
        report.Process();
    @endcode

    @library{wxqa}
    @category{debugging}
*/
class wxDebugReport
{
public:
    /**
        This enum is used for functions that report either the current state or
        the state during the last (fatal) exception.
    */
    enum Context {
        Context_Current,
        Context_Exception
    };

    /**
        The constructor creates a temporary directory where the files that will
        be included in the report are created. Use IsOk() to check for errors.
    */
    wxDebugReport();

    /**
        The destructor normally destroys the temporary directory created in the
        constructor with all the files it contains. Call Reset() to prevent
        this from happening.
    */
    virtual ~wxDebugReport();

    /**
        Adds all available information to the report. Currently this includes a
        text (XML) file describing the process context and, under Win32, a
        minidump file.
    */
    void AddAll(Context context = Context_Exception);

    /**
        Add an XML file containing the current or exception context and the
        stack trace.
    */
    virtual bool AddContext(Context ctx);

    /**
        The same as calling AddContext(Context_Current).
    */
    bool AddCurrentContext();

    /**
        The same as calling AddDump(Context_Current).
    */
    bool AddCurrentDump();

    /**
        Adds the minidump file to the debug report.

        Minidumps are only available under recent Win32 versions
        (@c dbghlp32.dll can be installed under older systems to make minidumps
        available).
    */
    virtual bool AddDump(Context ctx);

    /**
        The same as calling AddContext(Context_Exception).
    */
    bool AddExceptionContext();

    /**
        The same as calling AddDump(Context_Exception).
    */
    bool AddExceptionDump();

    /**
        Add another file to the report. If @a filename is an absolute path, it
        is copied to a file in the debug report directory with the same name.
        Otherwise the file will be searched in the temporary directory returned
        by GetDirectory().

        The argument @a description only exists to be displayed to the user in
        the report summary shown by wxDebugReportPreview.

        @see GetDirectory(), AddText()
    */
    virtual void AddFile(const wxString& filename, const wxString& description);

    /**
        This is a convenient wrapper around AddFile(). It creates the file with
        the given @a name and writes @a text to it, then adds the file to the
        report. The @a filename shouldn't contain the path.

        @return @true if file could be added successfully, @false if an IO
                 error occurred.
    */
    bool AddText(const wxString& filename, const wxString& text,
                 const wxString& description);

    /**
        This method should be used to construct the full name of the files
        which you wish to add to the report using AddFile().

        @return The name of the temporary directory used for the files in this
                 report.
    */
    const wxString& GetDirectory() const;

    /**
        Retrieves the name (relative to GetDirectory()) and the description of
        the file with the given index. If @a n is greater than or equal to the
        number of files, then @false is returned.
    */
    bool GetFile(size_t n, wxString* name, wxString* desc) const;

    /**
        Gets the current number files in this report.
    */
    size_t GetFilesCount() const;

    /**
        Gets the name used as a base name for various files, by default
        wxApp::GetAppName() is used.
    */
    virtual wxString GetReportName() const;

    /**
        Returns @true if the object was successfully initialized. If this
        method returns @false the report can't be used.
    */
    bool IsOk() const;

    /**
        Processes this report: the base class simply notifies the user that the
        report has been generated. This is usually not enough -- instead you
        should override this method to do something more useful to you.
    */
    bool Process();

    /**
        Removes the file from report: this is used by wxDebugReportPreview to
        allow the user to remove files potentially containing private
        information from the report.
    */
    void RemoveFile(const wxString& name);

    /**
        Resets the directory name we use. The object can't be used any more
        after this as it becomes uninitialized and invalid.
    */
    void Reset();

protected:

    /**
        This function may be overridden to add arbitrary custom context to the
        XML context file created by AddContext(). By default, it does nothing.
    */
    virtual void DoAddCustomContext(wxXmlNode* nodeRoot);

    /**
        This function may be overridden to modify the contents of the exception
        tag in the XML context file.
    */
    virtual bool DoAddExceptionInfo(wxXmlNode* nodeContext);

    /**
        This function may be overridden to modify the contents of the modules
        tag in the XML context file.
    */
    virtual bool DoAddLoadedModules(wxXmlNode* nodeModules);

    /**
        This function may be overridden to modify the contents of the system
        tag in the XML context file.
    */
    virtual bool DoAddSystemInfo(wxXmlNode* nodeSystemInfo);
};



/**
    @class wxDebugReportPreviewStd

    wxDebugReportPreviewStd is a standard debug report preview window. It
    displays a dialog allowing the user to examine the contents of a debug
    report, remove files from and add notes to it.

    @library{wxqa}
    @category{debugging}
*/
class wxDebugReportPreviewStd : public wxDebugReportPreview
{
public:
    /**
        Trivial default constructor.
    */
    wxDebugReportPreviewStd();

    /**
        Shows the dialog.

        @see wxDebugReportPreview::Show()
    */
    bool Show(wxDebugReport& dbgrpt) const;
};



/**
    @class wxDebugReportUpload

    This class is used to upload a compressed file using HTTP POST request. As
    this class derives from wxDebugReportCompress, before upload the report is
    compressed in a single ZIP file.

    @library{wxqa}
    @category{debugging}
*/
class wxDebugReportUpload : public wxDebugReportCompress
{
public:
    /**
        This class will upload the compressed file created by its base class to
        an HTML multipart/form-data form at the specified address. The @a url
        is the upload page address, @a input is the name of the @c "type=file"
        control on the form used for the file name and @a action is the value
        of the form action field. The report is uploaded using the @e curl
        program which should be available, the @e curl parameter may be used to
        specify the full path to it.
    */
    wxDebugReportUpload(const wxString& url, const wxString& input,
                        const wxString& action,
                        const wxString& curl = "curl");

protected:
    /**
        This function may be overridden in a derived class to show the output
        from curl: this may be an HTML page or anything else that the server
        returned. Value returned by this function becomes the return value of
        wxDebugReport::Process().
    */
    virtual bool OnServerReply(const wxArrayString& reply);
};

