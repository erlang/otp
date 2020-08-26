/////////////////////////////////////////////////////////////////////////////
// Name:        xrc/xmlres.h
// Purpose:     interface of wxXmlResource
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Flags which can be used with wxXmlResource::wxXmlResource.
*/
enum wxXmlResourceFlags
{
    /** Translatable strings will be translated via _(). */
    wxXRC_USE_LOCALE     = 1,

    /** Subclass property of object nodes will be ignored (useful for previews in XRC editors). */
    wxXRC_NO_SUBCLASSING = 2,

    /** Prevent the XRC files from being reloaded from disk in case they have been modified there
        since being last loaded (may slightly speed up loading them). */
    wxXRC_NO_RELOADING   = 4,

    /**
        Expand environment variables for paths in XRC (such as bitmaps or icons).

        @since 3.1.3
    */
    wxXRC_USE_ENVVARS    = 8
};


/**
    @class wxXmlResource

    This is the main class for interacting with the XML-based resource system.

    The class holds XML resources from one or more .xml files, binary files or zip
    archive files.

    Note that this is a singleton class and you'll never allocate/deallocate it.
    Just use the static wxXmlResource::Get() getter.

    @see @ref overview_xrc, @ref overview_xrcformat

    @library{wxxrc}
    @category{xrc}
*/
class wxXmlResource : public wxObject
{
public:
    /**
        Constructor.

        @param filemask
            The XRC file, archive file, or wildcard specification that will be
            used to load all resource files inside a zip archive.
        @param flags
            One or more value of the ::wxXmlResourceFlags enumeration.
        @param domain
            The name of the gettext catalog to search for translatable strings.
            By default all loaded catalogs will be searched.
            This provides a way to allow the strings to only come from a specific catalog.
    */
    wxXmlResource(const wxString& filemask,
                  int flags = wxXRC_USE_LOCALE,
                  const wxString& domain = wxEmptyString);

    /**
        Constructor.

        @param flags
            One or more value of the ::wxXmlResourceFlags enumeration.
        @param domain
            The name of the gettext catalog to search for translatable strings.
            By default all loaded catalogs will be searched.
            This provides a way to allow the strings to only come from a specific catalog.
    */
    wxXmlResource(int flags = wxXRC_USE_LOCALE,
                  const wxString& domain = wxEmptyString);

    /**
        Destructor.
    */
    virtual ~wxXmlResource();

    /**
        Initializes only a specific handler (or custom handler).
        Convention says that the handler name is equal to the control's name plus
        'XmlHandler', for example wxTextCtrlXmlHandler, wxHtmlWindowXmlHandler.

        The XML resource compiler (wxxrc) can create include file that contains
        initialization code for all controls used within the resource.
        Note that this handler must be allocated on the heap, since it will be
        deleted by ClearHandlers() later.
    */
    void AddHandler(wxXmlResourceHandler* handler);

    /**
       Add a new handler at the beginning of the handler list.
     */
    void InsertHandler(wxXmlResourceHandler *handler);


    /**
        Attaches an unknown control to the given panel/window/dialog.
        Unknown controls are used in conjunction with \<object class="unknown"\>.
    */
    bool AttachUnknownControl(const wxString& name,
                              wxWindow* control,
                              wxWindow* parent = NULL);

    /**
        Removes all handlers and deletes them (this means that any handlers
        added using AddHandler() must be allocated on the heap).
    */
    void ClearHandlers();

    /**
       Registers subclasses factory for use in XRC.  This is useful only for
       language bindings developers who need a way to implement subclassing in
       wxWidgets ports that don't support wxRTTI (e.g. wxPython).
    */
    static void AddSubclassFactory(wxXmlSubclassFactory *factory);


    /**
        Compares the XRC version to the argument.

        Returns -1 if the XRC version is less than the argument,
        +1 if greater, and 0 if they are equal.
    */
    int CompareVersion(int major, int minor, int release, int revision) const;

    /**
        Returns a string ID corresponding to the given numeric ID.

        The string returned is such that calling GetXRCID() with it as
        parameter yields @a numId. If there is no string identifier
        corresponding to the given numeric one, an empty string is returned.

        Notice that, unlike GetXRCID(), this function is slow as it checks all
        of the identifiers used in XRC.

        @since 2.9.0
     */
    static wxString FindXRCIDById(int numId);

    /**
        Gets the global resources object or creates one if none exists.
    */
    static wxXmlResource* Get();

    /**
        Returns the domain (message catalog) that will be used to load
        translatable strings in the XRC.
    */
    const wxString& GetDomain() const;

    /**
        Returns flags, which may be a bitlist of ::wxXmlResourceFlags
        enumeration values.
    */
    int GetFlags() const;

    /**
        Returns the wxXmlNode containing the definition of the object with the
        given name or @NULL.

        This function recursively searches all the loaded XRC files for an
        object with the specified @a name. If the object is found, the
        wxXmlNode corresponding to it is returned, so this function can be used
        to access additional information defined in the XRC file and not used
        by wxXmlResource itself, e.g. contents of application-specific XML
        tags.

        @param name
            The name of the resource which must be unique for this function to
            work correctly, if there is more than one resource with the given
            name the choice of the one returned by this function is undefined.
        @return
            The node corresponding to the resource with the given name or @NULL.
    */
    const wxXmlNode *GetResourceNode(const wxString& name) const;

    /**
        Returns version information (a.b.c.d = d + 256*c + 2562*b + 2563*a).
    */
    long GetVersion() const;

    /**
        Returns a numeric ID that is equivalent to the string ID used in an XML resource.

        If an unknown @a str_id is requested (i.e. other than wxID_XXX or integer),
        a new record is created which associates the given string with a number.

        If @a value_if_not_found is @c wxID_NONE, the number is obtained via
        wxNewId(). Otherwise @a value_if_not_found is used.

        Macro @c XRCID(name) is provided for convenient use in event tables.

        @note IDs returned by XRCID() cannot be used with the <tt>EVT_*_RANGE</tt>
              macros, because the order in which they are assigned to symbolic @a name
              values is not guaranteed.
    */
    static int GetXRCID(const wxString& str_id, int value_if_not_found = wxID_NONE);

    /**
        Initializes handlers for all supported controls/windows.

        This will make the executable quite big because it forces linking against
        most of the wxWidgets library.
    */
    void InitAllHandlers();

    /**
        Loads resources from XML files that match given filemask.

        Example:
        @code
            if (!wxXmlResource::Get()->Load("rc/*.xrc"))
                wxLogError("Couldn't load resources!");
        @endcode

        @note
        If wxUSE_FILESYS is enabled, this method understands wxFileSystem URLs
        (see wxFileSystem::FindFirst()).

        @note
        If you are sure that the argument is name of single XRC file (rather
        than an URL or a wildcard), use LoadFile() instead.

        @see LoadFile(), LoadAllFiles()
    */
    bool Load(const wxString& filemask);

    /**
        Simpler form of Load() for loading a single XRC file.

        @since 2.9.0

        @see Load(), LoadAllFiles()
    */
    bool LoadFile(const wxFileName& file);

    /**
        Loads all .xrc files from directory @a dirname.

        Tries to load as many files as possible; if there's an error while
        loading one file, it still attempts to load other files.

        @since 2.9.0

        @see LoadFile(), Load()
    */
    bool LoadAllFiles(const wxString& dirname);

    /**
        Loads a bitmap resource from a file.
    */
    wxBitmap LoadBitmap(const wxString& name);

    /**
        Loads a dialog. @a parent points to parent window (if any).
    */
    wxDialog* LoadDialog(wxWindow* parent, const wxString& name);

    /**
        Loads a dialog. @a parent points to parent window (if any).

        This form is used to finish creation of an already existing instance (the main
        reason for this is that you may want to use derived class with a new event table).
        Example:

        @code
          MyDialog dlg;
          wxXmlResource::Get()->LoadDialog(&dlg, mainFrame, "my_dialog");
          dlg.ShowModal();
        @endcode
    */
    bool LoadDialog(wxDialog* dlg, wxWindow* parent, const wxString& name);

    /**
       Loads a frame from the resource. @a parent points to parent window (if any).
    */
    wxFrame *LoadFrame(wxWindow* parent, const wxString& name);

    /**
        Loads the contents of a frame onto an existing wxFrame.

        This form is used to finish creation of an already existing instance
        (the main reason for this is that you may want to use derived class
        with a new event table).
    */
    bool LoadFrame(wxFrame* frame, wxWindow* parent,
                   const wxString& name);

    /**
        Loads an icon resource from a file.
    */
    wxIcon LoadIcon(const wxString& name);

    /**
        Loads menu from resource. Returns @NULL on failure.
    */
    wxMenu* LoadMenu(const wxString& name);

    //@{
    /**
        Loads a menubar from resource. Returns @NULL on failure.
    */
    wxMenuBar* LoadMenuBar(wxWindow* parent, const wxString& name);
    wxMenuBar* LoadMenuBar(const wxString& name);
    //@}

    //@{
    /**
        Load an object from the resource specifying both the resource name and the
        class name.

        The first overload lets you load nonstandard container windows and returns
        @NULL on failure. The second one lets you finish the creation of an existing
        instance and returns @false on failure.

        In either case, only the resources defined at the top level of XRC
        files can be loaded by this function, use LoadObjectRecursively() if
        you need to load an object defined deeper in the hierarchy.
    */
    wxObject* LoadObject(wxWindow* parent, const wxString& name,
                         const wxString& classname);
    bool LoadObject(wxObject* instance, wxWindow* parent,
                    const wxString& name,
                    const wxString& classname);
    //@}

    //@{
    /**
        Load an object from anywhere in the resource tree.

        These methods are similar to LoadObject() but may be used to load an
        object from anywhere in the resource tree and not only the top level.
        Note that you will very rarely need to do this as in normal use the
        entire container window (defined at the top level) is loaded and not
        its individual children but this method can be useful in some
        particular situations.

        @since 2.9.1
    */
    wxObject* LoadObjectRecursively(wxWindow* parent,
                                    const wxString& name,
                                    const wxString& classname);
    bool LoadObjectRecursively(wxObject* instance, wxWindow* parent,
                    const wxString& name,
                    const wxString& classname);
    //@}

    /**
        Loads a panel. @a parent points to the parent window.
    */
    wxPanel* LoadPanel(wxWindow* parent, const wxString& name);

    /**
        Loads a panel. @a parent points to the parent window.
        This form is used to finish creation of an already existing instance.
    */
    bool LoadPanel(wxPanel* panel, wxWindow* parent, const wxString& name);

    /**
        Loads a toolbar.
    */
    wxToolBar* LoadToolBar(wxWindow* parent, const wxString& name);

    /**
        Sets the global resources object and returns a pointer to the previous one
        (may be @NULL).
    */
    static wxXmlResource* Set(wxXmlResource* res);

    /**
        Sets the domain (message catalog) that will be used to load
        translatable strings in the XRC.
    */
    void SetDomain(const wxString& domain);

    /**
        Sets flags (bitlist of ::wxXmlResourceFlags enumeration values).
    */
    void SetFlags(int flags);

    /**
        This function unloads a resource previously loaded by Load().

        Returns @true if the resource was successfully unloaded and @false if it
        hasn't been found in the list of loaded resources.
    */
    bool Unload(const wxString& filename);

protected:
    /**
        Reports error in XRC resources to the user.

        Any errors in XRC input files should be reported using this method
        (or its wxXmlResourceHandler::ReportError() equivalent). Unlike
        wxLogError(), this method presents the error to the user in a more
        usable form. In particular, the output is compiler-like and contains
        information about the exact location of the error.

        @param context XML node the error occurred in or relates to. This can
                       be @NULL, but should be the most specific node possible,
                       as its line number is what is reported to the user.
        @param message Text of the error message. This string should always
                       be in English (i.e. not wrapped in _()). It shouldn't
                       be a sentence -- it should start with lower-case letter
                       and shouldn't have a trailing period or exclamation
                       point.

        @since 2.9.0

        @see wxXmlResourceHandler::ReportError(), DoReportError()
     */
    void ReportError(const wxXmlNode *context, const wxString& message);

    /**
        Implementation of XRC resources errors reporting.

        This method is called by ReportError() and shouldn't be called
        directly; use ReportError() or wxXmlResourceHandler::ReportError()
        to log errors.

        Default implementation uses wxLogError().

        @param xrcFile  File the error occurred in or empty string if it
                        couldn't be determined.
        @param position XML node where the error occurred or @NULL if it
                        couldn't be determined.
        @param message  Text of the error message. See ReportError()
                        documentation for details of the string's format.

        @note
        You may override this method in a derived class to customize errors
        reporting. If you do so, you'll need to either use the derived class
        in all your code or call wxXmlResource::Set() to change the global
        wxXmlResource instance to your class.

        @since 2.9.0

        @see ReportError()
    */
    virtual void DoReportError(const wxString& xrcFile, const wxXmlNode *position,
                               const wxString& message);
};



/**
    @class wxXmlResourceHandler

    wxXmlResourceHandler is an abstract base class for resource handlers
    capable of creating a control from an XML node.

    See @ref overview_xrc for details.

    @library{wxxrc}
    @category{xrc}
*/
class wxXmlResourceHandler : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxXmlResourceHandler();

    /**
        Destructor.
    */
    virtual ~wxXmlResourceHandler();

    /**
        Creates an object (menu, dialog, control, ...) from an XML node.
        Should check for validity. @a parent is a higher-level object
        (usually window, dialog or panel) that is often necessary to
        create the resource.

        If @b instance is non-@NULL it should not create a new instance via
        'new' but should rather use this one, and call its Create method.
    */
    wxObject* CreateResource(wxXmlNode* node, wxObject* parent,
                             wxObject* instance);

    /**
        Called from CreateResource after variables were filled.
    */
    virtual wxObject* DoCreateResource() = 0;

    /**
        Returns @true if it understands this node and can create
        a resource from it, @false otherwise.

        @note
        You must not call any wxXmlResourceHandler methods except IsOfClass()
        from this method! The instance is not yet initialized with node data
        at the time CanHandle() is called and it is only safe to operate on
        node directly or to call IsOfClass().
    */
    virtual bool CanHandle(wxXmlNode* node) = 0;

    /**
        Sets the parent resource.
    */
    void SetParentResource(wxXmlResource* res);


protected:

    /**
        Add a style flag (e.g. @c wxMB_DOCKABLE) to the list of flags
        understood by this handler.
    */
    void AddStyle(const wxString& name, int value);

    /**
        Add styles common to all wxWindow-derived classes.
    */
    void AddWindowStyles();

    /**
        Creates children.
    */
    void CreateChildren(wxObject* parent, bool this_hnd_only = false);

    /**
        Helper function.
    */
    void CreateChildrenPrivately(wxObject* parent,
                                 wxXmlNode* rootnode = NULL);

    /**
        Creates a resource from a node.
    */
    wxObject* CreateResFromNode(wxXmlNode* node, wxObject* parent,
                                wxObject* instance = NULL);

    /**
        Creates an animation (see wxAnimation) from the filename specified in @a param.

        It is recommended to provide @a ctrl argument to this function (which
        is only available in wxWidgets 3.1.4 or later) to make sure that the
        created animation is compatible with the specified control, otherwise a
        wxAnimation object compatible with the default wxAnimationCtrl
        implementation is created.
    */
    wxAnimation* GetAnimation(const wxString& param = "animation",
                              wxAnimationCtrlBase* ctrl = NULL);

    /**
        Gets a bitmap.
    */
    wxBitmap GetBitmap(const wxString& param = "bitmap",
                       const wxArtClient& defaultArtClient = wxART_OTHER,
                       wxSize size = wxDefaultSize);
    /**
        Gets a bitmap from an XmlNode.

        @since 2.9.1
    */
    wxBitmap GetBitmap(const wxXmlNode* node,
                       const wxArtClient& defaultArtClient = wxART_OTHER,
                       wxSize size = wxDefaultSize);

    /**
        Gets a bool flag (1, t, yes, on, true are @true, everything else is @false).
    */
    bool GetBool(const wxString& param, bool defaultv = false);

    /**
        Gets colour in HTML syntax (\#RRGGBB).
    */
    wxColour GetColour(const wxString& param,
                       const wxColour& defaultColour = wxNullColour);

    /**
        Returns the current file system.
    */
    wxFileSystem& GetCurFileSystem();

    /**
        Gets a dimension (may be in dialog units).
    */
    wxCoord GetDimension(const wxString& param, wxCoord defaultv = 0,
                         wxWindow* windowToUse = 0);

    /**
        Gets a direction.

        If the given @a param is not present or has empty value, @a dirDefault is
        returned by default. Otherwise the value of the parameter is parsed and
        a warning is generated if it's not one of @c wxLEFT, @c wxTOP, @c
        wxRIGHT or @c wxBOTTOM.

        @since 2.9.3
     */
    wxDirection GetDirection(const wxString& param, wxDirection dirDefault = wxLEFT);

    /**
        Gets a font.
    */
    wxFont GetFont(const wxString& param = "font");

    /**
        Returns the XRCID.
    */
    int GetID();

    /**
        Returns an icon.
    */
    wxIcon GetIcon(const wxString& param = "icon",
                   const wxArtClient& defaultArtClient = wxART_OTHER,
                   wxSize size = wxDefaultSize);

    /**
        Gets an icon from an XmlNode.

        @since 2.9.1
    */
    wxIcon GetIcon(const wxXmlNode* node,
                   const wxArtClient& defaultArtClient = wxART_OTHER,
                   wxSize size = wxDefaultSize);

    /**
        Returns an icon bundle.

        @note
        Bundles can be loaded either with stock IDs or from files that contain
        more than one image (e.g. Windows icon files). If a file contains only
        single image, a bundle with only one icon will be created.

        @since 2.9.0
     */
    wxIconBundle GetIconBundle(const wxString& param,
                               const wxArtClient& defaultArtClient = wxART_OTHER);

    /**
        Creates an image list from the @a param markup data.

        @return
            The new instance of wxImageList or @NULL if no data is found.

        @since 2.9.1
    */
    wxImageList *GetImageList(const wxString& param = "imagelist");

    /**
        Gets the integer value from the parameter.
    */
    long GetLong(const wxString& param, long defaultv = 0);

    /**
        Gets a float value from the parameter.
    */
    float GetFloat(const wxString& param, float defaultv = 0);

    /**
        Returns the resource name.
    */
    wxString GetName();

    /**
        Checks if the given node is an object node.

        Object nodes are those named "object" or "object_ref".

        @since 3.1.0
    */
    bool IsObjectNode(const wxXmlNode *node) const;
    /**
        Gets node content from wxXML_ENTITY_NODE.
    */
    wxString GetNodeContent(wxXmlNode* node);

    /**
        Gets the parent of the node given.

        This method is safe to call with @NULL argument, it just returns @NULL
        in this case.

        @since 3.1.0
    */
    wxXmlNode *GetNodeParent(const wxXmlNode *node) const;

    /**
        Gets the next sibling node related to the given node, possibly @NULL.

        This method is safe to call with @NULL argument, it just returns @NULL
        in this case.

        @since 3.1.0
    */
    wxXmlNode *GetNodeNext(const wxXmlNode *node) const;

    /**
        Gets the first child of the given node or @NULL.

        This method is safe to call with @NULL argument, it just returns @NULL
        in this case.

        @since 3.1.0
    */
    wxXmlNode *GetNodeChildren(const wxXmlNode *node) const;


    /**
        Finds the node or returns @NULL.
    */
    wxXmlNode* GetParamNode(const wxString& param);

    /**
        Finds the parameter value or returns the empty string.
    */
    wxString GetParamValue(const wxString& param);

    /**
        Returns the node parameter value.

        @since 2.9.1
    */
    wxString GetParamValue(const wxXmlNode* node);

    /**
        Gets the position (may be in dialog units).
    */
    wxPoint GetPosition(const wxString& param = "pos");

    /**
        Gets the size (may be in dialog units).
    */
    wxSize GetSize(const wxString& param = "size", wxWindow* windowToUse = 0);

    /**
        Gets style flags from text in form "flag | flag2| flag3 |..."
        Only understands flags added with AddStyle().
    */
    int GetStyle(const wxString& param = "style", int defaults = 0);

    /**
        Gets text from param and does some conversions:
        - replaces \\n, \\r, \\t by respective characters (according to C syntax)
        - replaces @c $ by @c  and @c $$ by @c $ (needed for @c _File to @c File
          translation because of XML syntax)
        - calls wxGetTranslations (unless disabled in wxXmlResource)
    */
    wxString GetText(const wxString& param, bool translate = true);

    /**
        Gets a file path from the given node.

        This function expands environment variables in the path if
        wxXRC_USE_ENVVARS is used.

        @since 3.1.3
    */
    wxString GetFilePath(const wxXmlNode* node);

    /**
        Check to see if a parameter exists.
    */
    bool HasParam(const wxString& param);

    /**
        Convenience function.
        Returns @true if the node has a property class equal to classname,
        e.g. object class="wxDialog".
    */
    bool IsOfClass(wxXmlNode* node, const wxString& classname);

    /**
        Sets common window options.
    */
    void SetupWindow(wxWindow* wnd);

    /**
        Reports error in XRC resources to the user.

        See wxXmlResource::ReportError() for more information.

        @since 2.9.0
     */
    void ReportError(wxXmlNode *context, const wxString& message);

    /**
        Like ReportError(wxXmlNode*, const wxString&), but uses the node
        of currently processed object (m_node) as the context.

        @since 2.9.0
     */
    void ReportError(const wxString& message);

    /**
        Like ReportError(wxXmlNode*, const wxString&), but uses the node
        of parameter @a param of the currently processed object as the context.
        This is convenience function for reporting errors in particular
        parameters.

        @since 2.9.0
     */
    void ReportParamError(const wxString& param, const wxString& message);


    /**
       After CreateResource has been called this will return the current
       wxXmlResource object.

       @since 2.9.5
    */
    wxXmlResource* GetResource() const;

    /**
       After CreateResource has been called this will return the XML node
       being processed.

       @since 2.9.5
    */
    wxXmlNode* GetNode() const;

    /**
       After CreateResource has been called this will return the class name of
       the XML resource node being processed.

       @since 2.9.5
    */
    wxString GetClass() const;

    /**
       After CreateResource has been called this will return the current
       item's parent, if any.

       @since 2.9.5
    */
    wxObject* GetParent() const;

    /**
       After CreateResource has been called this will return the instance that
       the XML resource content should be created upon, if it has already been
       created.  If @NULL then the handler should create the object itself.

       @since 2.9.5
    */
    wxObject* GetInstance() const;

    /**
       After CreateResource has been called this will return the item's parent
       as a wxWindow.

       @since 2.9.5
    */
    wxWindow* GetParentAsWindow() const;
};

