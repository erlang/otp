/////////////////////////////////////////////////////////////////////////////
// Name:        docview.h
// Purpose:     interface of various doc/view framework classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    A vector of wxDocument pointers.

    @since 2.9.5
*/
typedef wxVector<wxDocument*> wxDocVector;

/**
    A vector of wxView pointers.

    @since 2.9.5
*/
typedef wxVector<wxView*> wxViewVector;

/**
    A vector of wxDocTemplate pointers.

    @since 2.9.5
*/
typedef wxVector<wxDocTemplate*> wxDocTemplateVector;

/**
    @class wxDocTemplate

    The wxDocTemplate class is used to model the relationship between a
    document class and a view class.

    @library{wxcore}
    @category{docview}

    @see @ref overview_docview_wxdoctemplate, wxDocument, wxView
*/
class wxDocTemplate : public wxObject
{
public:
    /**
        Constructor. Create instances dynamically near the start of your
        application after creating a wxDocManager instance, and before doing
        any document or view operations.

        @param manager
            The document manager object which manages this template.
        @param descr
            A short description of what the template is for. This string will
            be displayed in the file filter list of Windows file selectors.
        @param filter
            An appropriate file filter such as "*.txt".
        @param dir
            The default directory to use for file selectors.
        @param ext
            The default file extension (such as "txt").
        @param docTypeName
            A name that should be unique for a given type of document, used for
            gathering a list of views relevant to a particular document.
        @param viewTypeName
            A name that should be unique for a given view.
        @param docClassInfo
            A pointer to the run-time document class information as returned by
            the wxCLASSINFO() macro, e.g. wxCLASSINFO(MyDocumentClass). If this is
            not supplied, you will need to derive a new wxDocTemplate class and
            override the CreateDocument() member to return a new document
            instance on demand.
        @param viewClassInfo
            A pointer to the run-time view class information as returned by the
            wxCLASSINFO() macro, e.g. wxCLASSINFO(MyViewClass). If this is not
            supplied, you will need to derive a new wxDocTemplate class and
            override the CreateView() member to return a new view instance on
            demand.
        @param flags
            A bit list of the following:
            - wxTEMPLATE_VISIBLE       - The template may be displayed to the
                                         user in dialogs.
            - wxTEMPLATE_INVISIBLE     - The template may not be displayed to
                                         the user in dialogs.
            - wxDEFAULT_TEMPLATE_FLAGS - Defined as wxTEMPLATE_VISIBLE.

        @beginWxPerlOnly

        In wxPerl @a docClassInfo and @a viewClassInfo can be either
        @c Wx::ClassInfo objects or strings containing the name of the
        perl packages which are to be used as @c Wx::Document and
        @c Wx::View classes (they must have a constructor named new);
        as an example:

        - Wx::DocTemplate->new(docmgr, descr, filter, dir, ext,
          docTypeName, viewTypeName, docClassInfo, viewClassInfo,
          flags): will construct document and view objects from the
          class information.
        - Wx::DocTemplate->new(docmgr, descr, filter, dir, ext,
          docTypeName, viewTypeName, docClassName, viewClassName,
          flags): will construct document and view objects from perl
          packages.
        - Wx::DocTemplate->new(docmgr, descr, filter, dir, ext,
          docTypeName, viewTypeName):
          in this case @c Wx::DocTemplate::CreateDocument() and
          @c Wx::DocTemplate::CreateView() must be overridden
        @endWxPerlOnly
    */
    wxDocTemplate(wxDocManager* manager, const wxString& descr,
                  const wxString& filter, const wxString& dir,
                  const wxString& ext, const wxString& docTypeName,
                  const wxString& viewTypeName, wxClassInfo* docClassInfo = 0,
                  wxClassInfo* viewClassInfo = 0,
                  long flags = wxTEMPLATE_VISIBLE);

    /**
        Destructor.
    */
    virtual ~wxDocTemplate();

    /**
        Creates a new instance of the associated document class. If you have
        not supplied a wxClassInfo parameter to the template constructor, you
        will need to override this function to return an appropriate document
        instance.

        This function calls InitDocument() which in turns calls
        wxDocument::OnCreate().
    */
    virtual wxDocument* CreateDocument(const wxString& path, long flags = 0);

    /**
        Creates a new instance of the associated view class.

        If you have not supplied a wxClassInfo parameter to the template
        constructor, you will need to override this function to return an
        appropriate view instance.

        If the new view initialization fails, it must call
        wxDocument::RemoveView() for consistency with the default behaviour of
        this function.
    */
    virtual wxView* CreateView(wxDocument* doc, long flags = 0);

    /**
        This function implements the default (very primitive) format detection
        which checks if the extension is that of the template.

        @param path
            The path to be checked against the template.
    */
    virtual bool FileMatchesTemplate(const wxString& path);

    /**
        Returns the default file extension for the document data, as passed to
        the document template constructor.
    */
    wxString GetDefaultExtension() const;

    /**
        Returns the text description of this template, as passed to the
        document template constructor.
    */
    wxString GetDescription() const;

    /**
        Returns the default directory, as passed to the document template
        constructor.
    */
    wxString GetDirectory() const;

    /**
        Returns the run-time class information that allows document
        instances to be constructed dynamically, as passed to the document
        template constructor.
    */
    wxClassInfo* GetDocClassInfo() const;

    /**
        Returns a pointer to the document manager instance for which this
        template was created.
    */
    wxDocManager* GetDocumentManager() const;

    /**
        Returns the document type name, as passed to the document template
        constructor.
    */
    virtual wxString GetDocumentName() const;

    /**
        Returns the file filter, as passed to the document template
        constructor.
    */
    wxString GetFileFilter() const;

    /**
        Returns the flags, as passed to the document template constructor.
    */
    long GetFlags() const;

    /**
        Returns a reference to the wxPageSetupDialogData associated with the
        printing operations of this document manager.
    */
    //@{
    wxPageSetupDialogData& GetPageSetupDialogData();
    const wxPageSetupDialogData& GetPageSetupDialogData() const;
    //@}

    /**
        Returns the run-time class information that allows view instances
        to be constructed dynamically, as passed to the document template
        constructor.
    */
    wxClassInfo* GetViewClassInfo() const;

    /**
        Returns the view type name, as passed to the document template
        constructor.
    */
    virtual wxString GetViewName() const;

    /**
        Initialises the document, calling wxDocument::OnCreate().

        This is called from CreateDocument().

        If you override this method, notice that you must @em delete the @a doc
        if its initialization fails for consistency with the default behaviour.

        @param doc
            The document to initialize.
        @param path
            The associated file path.
        @param flags
            Flags passed to CreateDocument().
        @return
            @true if the initialization was successful or @false if it failed
            in which case @a doc should be deleted by this function.
    */
    virtual bool InitDocument(wxDocument* doc,
                              const wxString& path,
                              long flags = 0);

    /**
        Returns @true if the document template can be shown in user dialogs,
        @false otherwise.
    */
    bool IsVisible() const;

    /**
        Sets the default file extension.
    */
    void SetDefaultExtension(const wxString& ext);

    /**
        Sets the template description.
    */
    void SetDescription(const wxString& descr);

    /**
        Sets the default directory.
    */
    void SetDirectory(const wxString& dir);

    /**
        Sets the pointer to the document manager instance for which this
        template was created. Should not be called by the application.
    */
    void SetDocumentManager(wxDocManager* manager);

    /**
        Sets the file filter.
    */
    void SetFileFilter(const wxString& filter);

    /**
        Sets the internal document template flags (see the constructor
        description for more details).
    */
    void SetFlags(long flags);

    /**
        The default extension for files of this type.
    */
    wxString m_defaultExt;

    /**
        A short description of this template.
    */
    wxString m_description;

    /**
        The default directory for files of this type.
    */
    wxString m_directory;

    /**
        Run-time class information that allows document instances to be
        constructed dynamically.
    */
    wxClassInfo* m_docClassInfo;

    /**
        The named type of the document associated with this template.
    */
    wxString m_docTypeName;

    /**
        A pointer to the document manager for which this template was created.
    */
    wxDocTemplate* m_documentManager;

    /**
        The file filter (such as "*.txt") to be used in file selector dialogs.
    */
    wxString m_fileFilter;

    /**
        The flags passed to the constructor.
    */
    long m_flags;

    /**
        Run-time class information that allows view instances to be constructed
        dynamically.
    */
    wxClassInfo* m_viewClassInfo;

    /**
        The named type of the view associated with this template.
    */
    wxString m_viewTypeName;
};



/**
    @class wxDocManager

    The wxDocManager class is part of the document/view framework supported by
    wxWidgets, and cooperates with the wxView, wxDocument and wxDocTemplate
    classes.

    @library{wxcore}
    @category{docview}

    @see @ref overview_docview_wxdocmanager, wxDocument, wxView, wxDocTemplate,
         wxFileHistory
*/
class wxDocManager : public wxEvtHandler
{
public:
    /**
        Constructor. Create a document manager instance dynamically near the
        start of your application before doing any document or view operations.

        If @a initialize is @true, the Initialize() function will be called to
        create a default history list object. If you derive from wxDocManager,
        you may wish to call the base constructor with @false, and then call
        Initialize() in your own constructor, to allow your own Initialize() or
        OnCreateFileHistory functions to be called.

        @param flags
            Currently unused.
        @param initialize
            Indicates whether Initialize() should be called by this ctor.
    */
    wxDocManager(long flags = 0, bool initialize = true);

    /**
        Destructor.
    */
    virtual ~wxDocManager();

    /**
        Sets the current view.
    */
    virtual void ActivateView(wxView* doc, bool activate = true);

    /**
        Adds the document to the list of documents.
    */
    void AddDocument(wxDocument* doc);

    /**
        Adds a file to the file history list, if we have a pointer to an
        appropriate file menu.
    */
    virtual void AddFileToHistory(const wxString& filename);

    /**
        Adds the template to the document manager's template list.
    */
    void AssociateTemplate(wxDocTemplate* temp);

    /**
        Search for a particular document template.

        Example:
        @code
           // creating a document instance of the specified document type:
           m_doc = (MyDoc*)docManager->FindTemplate(CLASSINFO(MyDoc))->
                        CreateDocument(wxEmptyString, wxDOC_SILENT);
        @endcode

        @param classinfo
            Class info of a document class for which a wxDocTemplate had been
            previously created.

        @return
            Pointer to a wxDocTemplate, or @NULL if none found.

        @since 2.9.2
     */
    wxDocTemplate* FindTemplate(const wxClassInfo* classinfo);


    /**
        Search for the document corresponding to the given file.

        @param path
            Document file path.
        @return
            Pointer to a wxDocument, or @NULL if none found.

        @since 2.9.5
     */
    wxDocument* FindDocumentByPath(const wxString& path) const;

    /**
        Closes the specified document.

        If @a force is @true, the document is closed even if it has unsaved
        changes.

        @param doc
            The document to close, must be non-@NULL.
        @param force
            If @true, close the document even if wxDocument::Close() returns
            @false.
        @return
            @true if the document was closed or @false if closing it was
            cancelled by user (only in @a force = @false case).
     */
    bool CloseDocument(wxDocument *doc, bool force = false);

    /**
        Closes all currently opened documents.

        @see CloseDocument()
    */
    bool CloseDocuments(bool force = true);

    /**
        Creates a new document.

        This function can either create a document corresponding to a new
        file or to an already existing one depending on whether @c wxDOC_NEW is
        specified in the @a flags.

        By default, this function asks the user for the type of document to
        open and the path to its file if it's not specified, i.e. if @a path is
        empty. Specifying @c wxDOC_SILENT flag suppresses any prompts and means
        that the @a path must be non-empty and there must be a registered
        document template handling the extension of this file, otherwise a
        warning message is logged and the function returns @NULL. Notice that
        @c wxDOC_SILENT can be combined with @c wxDOC_NEW, however in this case
        the @a path must still be specified, even if the file with this path
        typically won't exist.

        Finally notice that if this document manager was configured to allow
        only a limited number of simultaneously opened documents using
        SetMaxDocsOpen(), this function will try to close the oldest existing
        document if this number was reached before creating a new document.
        And if closing the old document fails (e.g. because it was vetoed by
        user), this function fails as well.

        @param path
            Path to a file or an empty string. If the path is empty, the user
            will be asked to select it (thus, this is incompatible with the use
            of @c wxDOC_SILENT). The file should exist unless @a flags includes
            @c wxDOC_NEW.
        @param flags
            By default, none. May include @c wxDOC_NEW to indicate that the new
            document corresponds to a new file and not an existing one and
            @c wxDOC_SILENT to suppress any dialogs asking the user about the
            file path and type.
        @return a new document object or @NULL on failure.
    */
    virtual wxDocument* CreateDocument(const wxString& path, long flags = 0);

    /**
        Creates an empty new document.

        This is equivalent to calling CreateDocument() with @c wxDOC_NEW flags
        and without the file name.
     */
    wxDocument *CreateNewDocument();

    /**
        Creates a new view for the given document. If more than one view is
        allowed for the document (by virtue of multiple templates mentioning
        the same document type), a choice of view is presented to the user.
    */
    virtual wxView* CreateView(wxDocument* doc, long flags = 0);

    /**
        Removes the template from the list of templates.
    */
    void DisassociateTemplate(wxDocTemplate* temp);

    /**
        Appends the files in the history list to all menus managed by the file
        history object.
    */
    virtual void FileHistoryAddFilesToMenu();
    /**
        Appends the files in the history list to the given @a menu only.
    */
    virtual void FileHistoryAddFilesToMenu(wxMenu* menu);

    /**
        Loads the file history from a config object.

        @see wxConfigBase
    */
    virtual void FileHistoryLoad(const wxConfigBase& config);

    /**
        Removes the given menu from the list of menus managed by the file
        history object.
    */
    virtual void FileHistoryRemoveMenu(wxMenu* menu);

    /**
        Saves the file history into a config object. This must be called
        explicitly by the application.

        @see wxConfigBase
    */
    virtual void FileHistorySave(wxConfigBase& resourceFile);

    /**
        Use this menu for appending recently-visited document filenames, for
        convenient access. Calling this function with a valid menu pointer
        enables the history list functionality.

        @note You can add multiple menus using this function, to be managed by
              the file history object.
    */
    virtual void FileHistoryUseMenu(wxMenu* menu);

    /**
        Given a path, try to find template that matches the extension. This is
        only an approximate method of finding a template for creating a
        document.
    */
    virtual wxDocTemplate* FindTemplateForPath(const wxString& path);

    /**
        Returns the view to apply a user command to.

        This method tries to find the view that the user wants to interact
        with. It returns the same view as GetCurrentDocument() if there is any
        currently active view but falls back to the first view of the first
        document if there is no active view.

        @since 2.9.5
     */
    wxView* GetAnyUsableView() const;

    /**
        Returns the document associated with the currently active view (if
        any).
    */
    wxDocument* GetCurrentDocument() const;

    /**
        Returns the currently active view.

        This method can return @NULL if no view is currently active.

        @see GetAnyUsableView()
    */
    virtual wxView* GetCurrentView() const;

    /**
        Returns a vector of wxDocument pointers.

        @since 2.9.5
    */
    wxDocVector GetDocumentsVector() const;

    /**
        Returns a vector of wxDocTemplate pointers.

        @since 2.9.5
    */
    wxDocTemplateVector GetTemplatesVector() const;

    /**
        Returns a reference to the list of documents.
    */
    wxList& GetDocuments();

    /**
        Returns a pointer to file history.
    */
    virtual wxFileHistory* GetFileHistory() const;

    /**
        Returns the number of files currently stored in the file history.
    */
    virtual size_t GetHistoryFilesCount() const;

    /**
        Returns the directory last selected by the user when opening a file.
        Initially empty.
    */
    wxString GetLastDirectory() const;

    /**
        Returns the number of documents that can be open simultaneously.
    */
    int GetMaxDocsOpen() const;

    /**
        Returns a reference to the list of associated templates.
    */
    wxList& GetTemplates();

    /**
        Initializes data; currently just calls OnCreateFileHistory().

        Some data cannot always be initialized in the constructor because the
        programmer must be given the opportunity to override functionality. If
        OnCreateFileHistory() was called from the constructor, an overridden
        virtual OnCreateFileHistory() would not be called due to C++'s
        'interesting' constructor semantics. In fact Initialize() @e is called
        from the wxDocManager constructor, but this can be vetoed by passing
        @false to the second argument, allowing the derived class's constructor
        to call Initialize(), possibly calling a different
        OnCreateFileHistory() from the default.

        The bottom line: if you're not deriving from Initialize(), forget it
        and construct wxDocManager with no arguments.
    */
    virtual bool Initialize();

    /**
        Return a string containing a suitable default name for a new document.
        By default this is implemented by appending an integer counter to the
        string @b unnamed but can be overridden in the derived classes to do
        something more appropriate.
    */
    virtual wxString MakeNewDocumentName();

    /**
        A hook to allow a derived class to create a different type of file
        history. Called from Initialize().
    */
    virtual wxFileHistory* OnCreateFileHistory();

    /**
        Closes and deletes the currently active document.
    */
    void OnFileClose(wxCommandEvent& event);

    /**
        Closes and deletes all the currently opened documents.
    */
    void OnFileCloseAll(wxCommandEvent& event);

    /**
        Creates a document from a list of templates (if more than one
        template).
    */
    void OnFileNew(wxCommandEvent& event);

    /**
        Creates a new document and reads in the selected file.
    */
    void OnFileOpen(wxCommandEvent& event);

    /**
        Reverts the current document by calling wxDocument::Revert() for the
        current document.
    */
    void OnFileRevert(wxCommandEvent& event);

    /**
        Saves the current document by calling wxDocument::Save() for the
        current document.
    */
    void OnFileSave(wxCommandEvent& event);

    /**
        Calls wxDocument::SaveAs() for the current document.
    */
    void OnFileSaveAs(wxCommandEvent& event);

    /**
        Removes the document from the list of documents.
    */
    void RemoveDocument(wxDocument* doc);

    /**
        Under Windows, pops up a file selector with a list of filters
        corresponding to document templates. The wxDocTemplate corresponding to
        the selected file's extension is returned.

        On other platforms, if there is more than one document template a
        choice list is popped up, followed by a file selector.

        This function is used in CreateDocument().

        @beginWxPerlOnly
        In wxPerl @a templates is a reference to a list of templates.
        If you override this method in your document manager it must
        return two values, e.g.:

        @code
        (doctemplate, path) = My::DocManager->SelectDocumentPath(...);
        @endcode
        @endWxPerlOnly
    */
    virtual wxDocTemplate* SelectDocumentPath(wxDocTemplate** templates,
                                              int noTemplates, wxString& path,
                                              long flags, bool save = false);

    /**
        Returns a document template by asking the user (if there is more than
        one template). This function is used in CreateDocument().

        @param templates
            Pointer to an array of templates from which to choose a desired
            template.
        @param noTemplates
            Number of templates being pointed to by the templates pointer.
        @param sort
            If more than one template is passed into templates, then this
            parameter indicates whether the list of templates that the user
            will have to choose from is sorted or not when shown the choice box
            dialog. Default is @false.

        @beginWxPerlOnly
        In wxPerl @a templates is a reference to a list of templates.
        @endWxPerlOnly
    */
    virtual wxDocTemplate* SelectDocumentType(wxDocTemplate** templates,
                                              int noTemplates,
                                              bool sort = false);

    /**
        Returns a document template by asking the user (if there is more than
        one template), displaying a list of valid views. This function is used
        in CreateView(). The dialog normally will not appear because the array
        of templates only contains those relevant to the document in question,
        and often there will only be one such.

        @param templates
            Pointer to an array of templates from which to choose a desired
            template.
        @param noTemplates
            Number of templates being pointed to by the templates pointer.
        @param sort
            If more than one template is passed into templates, then this
            parameter indicates whether the list of templates that the user
            will have to choose from is sorted or not when shown the choice box
            dialog. Default is @false.

        @beginWxPerlOnly
        In wxPerl @a templates is a reference to a list of templates.
        @endWxPerlOnly
    */
    virtual wxDocTemplate* SelectViewType(wxDocTemplate** templates,
                                          int noTemplates, bool sort = false);

    /**
        Sets the directory to be displayed to the user when opening a file.
        Initially this is empty.
    */
    void SetLastDirectory(const wxString& dir);

    /**
        Sets the maximum number of documents that can be open at a time. By
        default, this is @c INT_MAX, i.e. the number of documents is unlimited.
        If you set it to 1, existing documents will be saved and deleted when
        the user tries to open or create a new one (similar to the behaviour of
        Windows Write, for example). Allowing multiple documents gives
        behaviour more akin to MS Word and other Multiple Document Interface
        applications.
    */
    void SetMaxDocsOpen(int n);


protected:
    /**
        Called when a file selected from the MRU list doesn't exist any more.

        The default behaviour is to remove the file from the MRU (most recently
        used) files list and the corresponding menu and notify the user about
        it but this method can be overridden to customize it.

        For example, an application may want to just give an error about the
        missing file @a filename but not remove it from the file history. Or it
        could ask the user whether the file should be kept or removed.

        Notice that this method is called only if the file selected by user
        from the MRU files in the menu doesn't exist, but not if opening it
        failed for any other reason because in the latter case the default
        behaviour of removing the file from the MRU list is inappropriate.
        If you still want to do it, you would need to do it by calling
        RemoveFileFromHistory() explicitly in the part of the file opening code
        that may fail.

        @since 2.9.3

        @param n
            The index of the file in the MRU list, it can be passed to
            RemoveFileFromHistory() to remove this file from the list.
        @param filename
            The full name of the file.
     */
    virtual void OnMRUFileNotExist(unsigned n, const wxString& filename);

    /**
        Create the frame used for print preview.

        This method can be overridden if you need to change the behaviour or
        appearance of the preview window. By default, a standard wxPreviewFrame
        is created.

        @since 2.9.1

        @param preview The associated preview object.
        @param parent The parent window for the frame.
        @param title The suggested title for the print preview frame.
        @return A new print preview frame, must not return @NULL.
    */
    virtual wxPreviewFrame* CreatePreviewFrame(wxPrintPreviewBase* preview,
                                               wxWindow* parent,
                                               const wxString& title);

    /**
        The currently active view.
    */
    wxView* m_currentView;

    /**
        Stores the integer to be used for the next default document name.
    */
    int m_defaultDocumentNameCounter;

    /**
        A list of all documents.
    */
    wxList m_docs;

    /**
        A pointer to an instance of wxFileHistory, which manages the history of
        recently-visited files on the File menu.
    */
    wxFileHistory* m_fileHistory;

    /**
        The directory last selected by the user when opening a file.
    */
    wxString m_lastDirectory;

    /**
        Stores the maximum number of documents that can be opened before
        existing documents are closed.

        By default, this is @c INT_MAX i.e. practically unlimited.
    */
    int m_maxDocsOpen;
};



/**
    @class wxView

    The view class can be used to model the viewing and editing component of
    an application's file-based data. It is part of the document/view framework
    supported by wxWidgets, and cooperates with the wxDocument, wxDocTemplate
    and wxDocManager classes.

    @library{wxcore}
    @category{docview}

    @see @ref overview_docview_wxview, wxDocument, wxDocTemplate, wxDocManager
*/
class wxView : public wxEvtHandler
{
public:
    /**
        Constructor. Define your own default constructor to initialize
        application-specific data.
    */
    wxView();

    /**
        Destructor. Removes itself from the document's list of views.
    */
    virtual ~wxView();

    /**
        Call this from your view frame's wxDocChildFrame::OnActivate() member
        to tell the framework which view is currently active. If your windowing
        system doesn't call wxDocChildFrame::OnActivate(), you may need to call
        this function from any place where you know the view must be active,
        and the framework will need to get the current view.

        The prepackaged view frame wxDocChildFrame calls Activate() from its
        wxDocChildFrame::OnActivate() member.

        This function calls OnActivateView().
    */
    virtual void Activate(bool activate);

    /**
        Closes the view by calling OnClose(). If @a deleteWindow is @true, this
        function should delete the window associated with the view.
    */
    virtual bool Close(bool deleteWindow = true);

    /**
        Gets a pointer to the document associated with the view.
    */
    wxDocument* GetDocument() const;

    /**
        Returns a pointer to the document manager instance associated with this
        view.
    */
    wxDocManager* GetDocumentManager() const;

    /**
        Gets the frame associated with the view (if any). Note that this
        "frame" is not a wxFrame at all in the generic MDI implementation which
        uses notebook pages instead of frames and this is why this method
        returns a wxWindow and not a wxFrame.
    */
    wxWindow* GetFrame() const;

    /**
        Gets the name associated with the view (passed to the wxDocTemplate
        constructor). Not currently used by the framework.
    */
    wxString GetViewName() const;

    /**
        Called when a view is activated by means of Activate(). The default
        implementation does nothing.
    */
    virtual void OnActivateView(bool activate, wxView* activeView,
                                wxView* deactiveView);

    /**
        Called when the filename has changed. The default implementation
        constructs a suitable title and sets the title of the view frame (if any).
    */
    virtual void OnChangeFilename();

    /**
        Implements closing behaviour. The default implementation calls
        wxDocument::Close() to close the associated document. Does not delete
        the view. The application may wish to do some cleaning up operations in
        this function, @e if a call to wxDocument::Close() succeeded. For
        example, if your views all share the same window, you need to
        disassociate the window from the view and perhaps clear the window. If
        @a deleteWindow is @true, delete the frame associated with the view.
    */
    virtual bool OnClose(bool deleteWindow);

    /**
        Override this to clean up the view when the document is being closed.
    */
    virtual void OnClosingDocument();

    /**
        wxDocManager or wxDocument creates a wxView via a wxDocTemplate. Just
        after the wxDocTemplate creates the wxView, it calls OnCreate(). The
        wxView can create a wxDocChildFrame (or derived class) in its
        wxView::OnCreate() member function. This wxDocChildFrame provides user
        interface elements to view and/or edit the contents of the wxDocument.

        By default, simply returns @true. If the function returns @false, the
        view will be deleted.
    */
    virtual bool OnCreate(wxDocument* doc, long flags);

    /**
        If the printing framework is enabled in the library, this function
        returns a wxPrintout object for the purposes of printing. It should
        create a new object every time it is called; the framework will delete
        objects it creates.

        By default, this function returns an instance of wxDocPrintout, which
        prints and previews one page by calling OnDraw().

        Override to return an instance of a class other than wxDocPrintout.
    */
    virtual wxPrintout* OnCreatePrintout();

    /**
        Override this function to render the view on the given device context.
    */
    virtual void OnDraw(wxDC* dc) = 0;

    /**
        Called when the view should be updated.

        @param sender
            A pointer to the wxView that sent the update request, or @NULL if
            no single view requested the update (for instance, when the
            document is opened).
        @param hint
            This is unused currently, but may in future contain
            application-specific information for making updating more
            efficient.
    */
    virtual void OnUpdate(wxView* sender, wxObject* hint = 0);

    /**
        Associates the given document with the view. Normally called by the
        framework.
    */
    virtual void SetDocument(wxDocument* doc);

    /**
        Sets the frame associated with this view. The application should call
        this if possible, to tell the view about the frame.

        See GetFrame() for the explanation about the mismatch between the
        "Frame" in the method name and the type of its parameter.
    */
    void SetFrame(wxWindow* frame);

    /**
        Sets the view type name. Should only be called by the framework.
    */
    void SetViewName(const wxString& name);

    /**
        The document associated with this view. There may be more than one view
        per document, but there can never be more than one document for one
        view.
    */
    wxDocument* m_viewDocument;

    /**
        Frame associated with the view, if any.
    */
    wxFrame* m_viewFrame;

    /**
        The view type name given to the wxDocTemplate constructor, copied to
        this variable when the view is created. Not currently used by the
        framework.
    */
    wxString m_viewTypeName;
};



/**
    @class wxDocChildFrame

    The wxDocChildFrame class provides a default frame for displaying documents
    on separate windows. This class can only be used for SDI (not MDI) child
    frames.

    The class is part of the document/view framework supported by wxWidgets,
    and cooperates with the wxView, wxDocument, wxDocManager and wxDocTemplate
    classes.

    Notice that this class handles ::wxEVT_ACTIVATE event and activates the
    child view on receiving it. Don't intercept this event unless you want to
    prevent from this happening.

    The same remark applies to ::wxEVT_CLOSE_WINDOW, as wxDocParentFrame the
    frame handles this event by trying to close the associated view.

    @library{wxcore}
    @category{docview}

    @see @ref overview_docview, @ref page_samples_docview, wxFrame
*/
class wxDocChildFrame : public wxFrame
{
public:
    /**
        Constructor.
    */
    wxDocChildFrame(wxDocument* doc, wxView* view, wxFrame* parent,
                    wxWindowID id, const wxString& title,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize,
                    long style = wxDEFAULT_FRAME_STYLE,
                    const wxString& name = wxFrameNameStr);

    /**
        Destructor.
    */
    virtual ~wxDocChildFrame();

    /**
        Returns the document associated with this frame.
    */
    wxDocument* GetDocument() const;

    /**
        Returns the view associated with this frame.
    */
    wxView* GetView() const;

    /**
        Sets the document for this frame.
    */
    void SetDocument(wxDocument* doc);

    /**
        Sets the view for this frame.
    */
    void SetView(wxView* view);

    /**
        The document associated with the frame.
    */
    wxDocument* m_childDocument;

    /**
        The view associated with the frame.
    */
    wxView* m_childView;
};



/**
    @class wxDocParentFrame

    The wxDocParentFrame class provides a default top-level frame for
    applications using the document/view framework. This class can only be used
    for SDI (not MDI) parent frames.

    It cooperates with the wxView, wxDocument, wxDocManager and wxDocTemplate
    classes.

    Notice that this class processes ::wxEVT_CLOSE_WINDOW event and tries to
    close all open views from its handler. If all the views can be closed, i.e.
    if none of them contains unsaved changes or the user decides to not save
    them, the window is destroyed. Don't intercept this event in your code
    unless you want to replace this logic.

    @library{wxcore}
    @category{docview}

    @see @ref overview_docview, @ref page_samples_docview, wxFrame
*/
class wxDocParentFrame : public wxFrame
{
public:
    /**
        Default constructor.
    */
    wxDocParentFrame();
    /**
        Constructor.
    */
    wxDocParentFrame(wxDocManager* manager, wxFrame* parent,
                     wxWindowID id, const wxString& title,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxDEFAULT_FRAME_STYLE,
                     const wxString& name = wxFrameNameStr);

    /**
        Destructor.
    */
    virtual ~wxDocParentFrame();

    /**
        Used in two-step construction.
    */
    bool Create(wxDocManager* manager, wxFrame* parent, wxWindowID id,
                const wxString& title, const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = 541072960,
                const wxString& name = wxFrameNameStr);

    /**
        Returns the associated document manager object.
    */
    wxDocManager* GetDocumentManager() const;
};



/**
    @class wxDocument

    The document class can be used to model an application's file-based data.

    It is part of the document/view framework supported by wxWidgets, and
    cooperates with the wxView, wxDocTemplate and wxDocManager classes.

    A normal document is the one created without parent document and is
    associated with a disk file. Since version 2.9.2 wxWidgets also supports a
    special kind of documents called <em>child documents</em> which are virtual
    in the sense that they do not correspond to a file but rather to a part of
    their parent document. Because of this, the child documents can't be
    created directly by user but can only be created by the parent document
    (usually when it's being created itself). They also can't be independently
    saved. A child document has its own view with the corresponding window.
    This view can be closed by user but, importantly, is also automatically
    closed when its parent document is closed. Thus, child documents may be
    convenient for creating additional windows which need to be closed when the
    main document is. The docview sample demonstrates this use of child
    documents by creating a child document containing the information about the
    parameters of the image opened in the main document.

    @library{wxcore}
    @category{docview}

    @see @ref overview_docview, wxView, wxDocTemplate, wxDocManager
*/
class wxDocument : public wxEvtHandler
{
public:
    /**
        Constructor. Define your own default constructor to initialize
        application-specific data.

        @param parent
            Specifying a non-@c NULL parent document here makes this document a
            special <em>child document</em>, see their description in the class
            documentation. Notice that this parameter exists but is ignored in
            wxWidgets versions prior to 2.9.1.
    */
    wxDocument(wxDocument* parent = NULL);

    /**
        Destructor. Removes itself from the document manager.
    */
    virtual ~wxDocument();

    /**
        If the view is not already in the list of views, adds the view and
        calls OnChangedViewList().
    */
    virtual bool AddView(wxView* view);

    /**
        Returns true if the document hasn't been modified since the last time
        it had been saved.

        Notice that this function returns @false if the document had been never
        saved at all, so it may be also used to test whether it makes sense to
        save the document: if it returns @true, there is nothing to save but if
        @false is returned, it can be saved, even if it might be not modified
        (this can be used to create an empty document file by the user).

        @see IsModified(), GetDocumentSaved()

        @since 2.9.0
     */
    bool AlreadySaved() const;

    /**
        Activate the first view of the document if any.

        This function simply calls the Raise() method of the frame of the first
        view. You may need to override the Raise() method to get the desired
        effect if you are not using a standard wxFrame for your view. For
        instance, if your document is inside its own notebook tab you could
        implement Raise() like this:

        @code
        void MyNotebookPage::Raise()
        {
            wxNotebook* notebook = wxStaticCast(GetParent(), wxNotebook);
            notebook->SetSelection(notebook->FindPage(this));
        }
        @endcode

        @see GetFirstView()

        @since 2.9.5
     */
    void Activate() const;

    /**
        Closes the document, by calling OnSaveModified() and then (if this
        returned @true) OnCloseDocument(). This does not normally delete the
        document object, use DeleteAllViews() to do this implicitly.
    */
    virtual bool Close();

    /**
        Calls wxView::Close() and deletes each view. Deleting the final view
        will implicitly delete the document itself, because the wxView
        destructor calls RemoveView(). This in turns calls OnChangedViewList(),
        whose default implementation is to save and delete the document if no
        views exist.
    */
    virtual bool DeleteAllViews();

    /**
        Virtual method called from OnCloseDocument().

        This method may be overridden to perform any additional cleanup which
        might be needed when the document is closed.

        The return value of this method is currently ignored.

        The default version does nothing and simply returns @true.
     */
    virtual bool DeleteContents();

    /**
        Returns a pointer to the command processor associated with this
        document.

        @see wxCommandProcessor
    */
    virtual wxCommandProcessor* GetCommandProcessor() const;

    /**
        Gets a pointer to the associated document manager.
    */
    virtual wxDocManager* GetDocumentManager() const;

    /**
        Gets the document type name for this document. See the comment for
        @ref m_documentTypeName.
    */
    wxString GetDocumentName() const;

    /**
        Return true if this document had been already saved.

        @see IsModified()
     */
    bool GetDocumentSaved() const;

    /**
        Gets a pointer to the template that created the document.
    */
    virtual wxDocTemplate* GetDocumentTemplate() const;

    /**
        Intended to return a suitable window for using as a parent for
        document-related dialog boxes. By default, uses the frame associated
        with the first view.
    */
    virtual wxWindow* GetDocumentWindow() const;

    /**
        Gets the filename associated with this document, or "" if none is
        associated.
    */
    wxString GetFilename() const;

    /**
        A convenience function to get the first view for a document, because in
        many cases a document will only have a single view.

        @see GetViews()
    */
    wxView* GetFirstView() const;

    /**
        Gets the title for this document. The document title is used for an
        associated frame (if any), and is usually constructed by the framework
        from the filename.
    */
    wxString GetTitle() const;

    /**
        Return the document name suitable to be shown to the user. The default
        implementation uses the document title, if any, of the name part of the
        document filename if it was set or, otherwise, the string @b unnamed.
    */
    virtual wxString GetUserReadableName() const;

    /**
        Returns a vector of wxView pointers.

        @since 2.9.5
    */
    wxViewVector GetViewsVector() const;

    //@{
    /**
        Returns the list whose elements are the views on the document.

        @see GetFirstView()
    */
    wxList& GetViews();
    const wxList& GetViews() const;
    //@}

    /**
        Returns true if this document is a child document corresponding to a
        part of the parent document and not a disk file as usual.

        This method can be used to check whether file-related operations make
        sense for this document as they only apply to top-level documents and
        not child ones.

        @since 2.9.2
     */
    bool IsChildDocument() const;

    /**
        Returns @true if the document has been modified since the last save,
        @false otherwise. You may need to override this if your document view
        maintains its own record of being modified.

        @see Modify()
    */
    virtual bool IsModified() const;

    //@{
    /**
        Override this function and call it from your own LoadObject() before
        streaming your own data. LoadObject() is called by the framework
        automatically when the document contents need to be loaded.

        @note This version of LoadObject() may not exist depending on how
              wxWidgets was configured.
    */
    virtual istream& LoadObject(istream& stream);
    virtual wxInputStream& LoadObject(wxInputStream& stream);
    //@}

    /**
        Call with @true to mark the document as modified since the last save,
        @false otherwise. You may need to override this if your document view
        maintains its own record of being modified.

        @see IsModified()
    */
    virtual void Modify(bool modify);

    /**
        Called when a view is added to or deleted from this document. The
        default implementation saves and deletes the document if no views exist
        (the last one has just been removed).
    */
    virtual void OnChangedViewList();

    /**
        This virtual function is called when the document is being closed.

        The default implementation calls DeleteContents() (which may be
        overridden to perform additional cleanup) and sets the modified flag to
        @false. You can override it to supply additional behaviour when the
        document is closed with Close().

        Notice that previous wxWidgets versions used to call this function also
        from OnNewDocument(), rather counter-intuitively. This is no longer the
        case since wxWidgets 2.9.0.
    */
    virtual bool OnCloseDocument();

    /**
        Called just after the document object is created to give it a chance to
        initialize itself.

        The default implementation uses the template associated with the
        document to create an initial view.

        For compatibility reasons, this method may either delete the document
        itself if its initialization fails or not do it in which case it is
        deleted by caller. It is recommended to delete the document explicitly
        in this function if it can't be initialized.

        @param path
            The associated file path.
        @param flags
            Flags passed to CreateDocument().
        @return
            @true if the initialization was successful or @false if it failed.
    */
    virtual bool OnCreate(const wxString& path, long flags);

    /**
        Override this function if you want a different (or no) command
        processor to be created when the document is created. By default, it
        returns an instance of wxCommandProcessor.

        @see wxCommandProcessor
    */
    virtual wxCommandProcessor* OnCreateCommandProcessor();

    /**
        The default implementation calls OnSaveModified() and DeleteContents(),
        makes a default title for the document, and notifies the views that the
        filename (in fact, the title) has changed.
    */
    virtual bool OnNewDocument();

    /**
        Constructs an input file stream for the given filename (which must not
        be empty), and calls LoadObject(). If LoadObject() returns @true, the
        document is set to unmodified; otherwise, an error message box is
        displayed. The document's views are notified that the filename has
        changed, to give windows an opportunity to update their titles. All of
        the document's views are then updated.
    */
    virtual bool OnOpenDocument(const wxString& filename);

    /**
        Constructs an output file stream for the given filename (which must not
        be empty), and calls SaveObject(). If SaveObject() returns @true, the
        document is set to unmodified; otherwise, an error message box is
        displayed.
    */
    virtual bool OnSaveDocument(const wxString& filename);

    /**
        If the document has been modified, prompts the user to ask if the
        changes should be saved. If the user replies Yes, the Save() function
        is called. If No, the document is marked as unmodified and the function
        succeeds. If Cancel, the function fails.
    */
    virtual bool OnSaveModified();

    /**
        Removes the view from the document's list of views.

        If the view was really removed, also calls OnChangedViewList().

        @return @true if the view was removed or @false if the document didn't
            have this view in the first place.
    */
    virtual bool RemoveView(wxView* view);

    /**
        Saves the document by calling OnSaveDocument() if there is an
        associated filename, or SaveAs() if there is no filename.
    */
    virtual bool Save();

    /**
        Prompts the user for a file to save to, and then calls
        OnSaveDocument().
    */
    virtual bool SaveAs();

    /**
        Discard changes and load last saved version.

        Prompts the user first, and then calls DoOpenDocument() to reload the
        current file.
    */
    virtual bool Revert();

    //@{
    /**
        Override this function and call it from your own SaveObject() before
        streaming your own data. SaveObject() is called by the framework
        automatically when the document contents need to be saved.

        @note This version of SaveObject() may not exist depending on how
              wxWidgets was configured.
    */
    virtual ostream& SaveObject(ostream& stream);
    virtual wxOutputStream& SaveObject(wxOutputStream& stream);
    //@}

    /**
        Sets the command processor to be used for this document. The document
        will then be responsible for its deletion. Normally you should not call
        this; override OnCreateCommandProcessor() instead.

        @see wxCommandProcessor
    */
    virtual void SetCommandProcessor(wxCommandProcessor* processor);

    /**
        Sets the document type name for this document. See the comment for
        @ref m_documentTypeName.
    */
    void SetDocumentName(const wxString& name);

    /**
        Sets the pointer to the template that created the document. Should only
        be called by the framework.
    */
    virtual void SetDocumentTemplate(wxDocTemplate* templ);

    /**
        Sets if this document has been already saved or not.

        Normally there is no need to call this function as the document-view
        framework does it itself as the documents are loaded from and saved to
        the files. However it may be useful in some particular cases, for
        example it may be called with @false argument to prevent the user
        from saving the just opened document into the same file if this
        shouldn't be done for some reason (e.g. file format version changes and
        a new extension should be used for saving).

        @see GetDocumentSaved(), AlreadySaved()
     */
    void SetDocumentSaved(bool saved = true);

    /**
        Sets the filename for this document. Usually called by the framework.

        Calls OnChangeFilename() which in turn calls wxView::OnChangeFilename() for
        all views if @a notifyViews is @true.
    */
    void SetFilename(const wxString& filename, bool notifyViews = false);

    /**
        If @a notifyViews is @true, wxView::OnChangeFilename() is called for
        all views.

        @since 2.9.0
    */
    virtual void OnChangeFilename(bool notifyViews);

    /**
        Sets the title for this document. The document title is used for an
        associated frame (if any), and is usually constructed by the framework
        from the filename.
    */
    void SetTitle(const wxString& title);

    /**
        Updates all views. If @a sender is non-@NULL, does not update this
        view. @a hint represents optional information to allow a view to
        optimize its update.
    */
    virtual void UpdateAllViews(wxView* sender = NULL, wxObject* hint = NULL);

protected:
    /**
        This method is called by OnSaveDocument() to really save the document
        contents to the specified file.

        Base class version creates a file-based stream and calls SaveObject().
        Override this if you need to do something else or prefer not to use
        SaveObject() at all.
     */
    virtual bool DoSaveDocument(const wxString& file);

    /**
        This method is called by OnOpenDocument() to really load the document
        contents from the specified file.

        Base class version creates a file-based stream and calls LoadObject().
        Override this if you need to do something else or prefer not to use
        LoadObject() at all.
     */
    virtual bool DoOpenDocument(const wxString& file);

    /**
        A pointer to the command processor associated with this document.
    */
    wxCommandProcessor* m_commandProcessor;

    /**
        Filename associated with this document ("" if none).
    */
    wxString m_documentFile;

    /**
        @true if the document has been modified, @false otherwise.
    */
    bool m_documentModified;

    /**
        A pointer to the template from which this document was created.
    */
    wxDocTemplate* m_documentTemplate;

    /**
        Document title. The document title is used for an associated frame (if
        any), and is usually constructed by the framework from the filename.
    */
    wxString m_documentTitle;

    /**
        The document type name given to the wxDocTemplate constructor, copied
        to this variable when the document is created. If several document
        templates are created that use the same document type, this variable is
        used in wxDocManager::CreateView() to collate a list of alternative
        view types that can be used on this kind of document. Do not change the
        value of this variable.
    */
    wxString m_documentTypeName;

    /**
        List of wxView instances associated with this document.
    */
    wxList m_documentViews;
};


// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_file */
//@{

/**
    Copies the given file to @a stream. Useful when converting an old
    application to use streams (within the document/view framework, for
    example).

    @header{wx/docview.h}
*/
bool wxTransferFileToStream(const wxString& filename,
                            ostream& stream);

/**
    Copies the given stream to the file @a filename. Useful when converting an
    old application to use streams (within the document/view framework, for
    example).

    @header{wx/docview.h}
*/
bool wxTransferStreamToFile(istream& stream,
                             const wxString& filename);

//@}

