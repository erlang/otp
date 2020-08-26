/////////////////////////////////////////////////////////////////////////////
// Name:        xml/xml.h
// Purpose:     interface of wxXmlNode, wxXmlAttribute, wxXmlDocument
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/// Represents XML node type.
enum wxXmlNodeType
{
    // note: values are synchronized with xmlElementType from libxml
    wxXML_ELEMENT_NODE       =  1,
    wxXML_ATTRIBUTE_NODE     =  2,
    wxXML_TEXT_NODE          =  3,
    wxXML_CDATA_SECTION_NODE =  4,
    wxXML_ENTITY_REF_NODE    =  5,
    wxXML_ENTITY_NODE        =  6,
    wxXML_PI_NODE            =  7,
    wxXML_COMMENT_NODE       =  8,
    wxXML_DOCUMENT_NODE      =  9,
    wxXML_DOCUMENT_TYPE_NODE = 10,
    wxXML_DOCUMENT_FRAG_NODE = 11,
    wxXML_NOTATION_NODE      = 12,
    wxXML_HTML_DOCUMENT_NODE = 13
};

/**
    @class wxXmlNode

    Represents a node in an XML document. See wxXmlDocument.

    Each node is named and depending on the node type it may also hold content
    or be given attributes.

    The two most common node types are @c wxXML_ELEMENT_NODE and
    @c wxXML_TEXT_NODE. @c wxXML_ELEMENT_NODE represents a pair of XML element
    tags, whilst @c wxXML_TEXT_NODE represents the text value that can belong
    to the element.

    A @c wxXML_ELEMENT_NODE has a title, and optionally attributes, but does not
    have any content. A @c wxXML_TEXT_NODE does not have a title or attributes
    but should normally have content.

    For example: in the XML fragment <tt>\<title\>hi\</title\></tt> there is an
    element node with the name @c title and a single text node child with the
    text @c hi as content.

    A @c wxXML_PI_NODE represents a Processing Instruction (PI) node with
    the name parameter set as the target and the contents parameter set as
    the instructions. Note that whilst the PI instructions are often in the form
    of pseudo-attributes, these do not use the node's attribute member. It is
    the user's responsibility to code and decode the PI instruction text.

    The @c wxXML_DOCUMENT_TYPE_NODE is not implemented at this time. Instead,
    you should get and set the DOCTYPE values using the wxXmlDocument class.

    If @c wxUSE_UNICODE is 0, all strings are encoded in the encoding given to
    wxXmlDocument::Load (default is UTF-8).

    @note
    Once a wxXmlNode has been added to a wxXmlDocument it becomes owned by the
    document and this has two implications. Firstly, the wxXmlDocument takes
    responsibility for deleting the node so the user should not @c delete it;
    and secondly, a wxXmlNode must always be created on the heap and never on
    the stack.

    @library{wxxml}
    @category{xml}

    @see wxXmlDocument, wxXmlDoctype, wxXmlAttribute
*/
class wxXmlNode
{
public:
    /**
        Creates this XML node and inserts it into the XML tree as a child of
        the specified parent. Once added, the XML tree takes ownership of this
        object and there is no need to delete it.

        @param parent
            The parent node to which append this node instance.
            If this argument is @NULL this new node will be floating and it can
            be appended later to another one using the AddChild() or InsertChild()
            functions. Otherwise the child is added to the XML tree by this
            constructor and it shouldn't be done again.
        @param type
            One of the ::wxXmlNodeType enumeration value.
        @param name
            The name of the node. This is the string which appears between angular brackets.
        @param content
            The content of the node.
            Only meaningful when type is @c wxXML_TEXT_NODE or @c wxXML_CDATA_SECTION_NODE.
        @param attrs
            If not @NULL, this wxXmlAttribute object and its eventual siblings are attached to the node.
        @param next
            If not @NULL, this node and its eventual siblings are attached to the node.
        @param lineNo
            Number of line this node was present at in input file or -1.
    */
    wxXmlNode(wxXmlNode* parent, wxXmlNodeType type,
              const wxString& name,
              const wxString& content = wxEmptyString,
              wxXmlAttribute* attrs = NULL,
              wxXmlNode* next = NULL, int lineNo = -1);

    /**
        A simplified version of the first constructor form, assuming a @NULL parent.

        @param type
            One of the ::wxXmlNodeType enumeration value.
        @param name
            The name of the node. This is the string which appears between angular brackets.
        @param content
            The content of the node.
            Only meaningful when type is @c wxXML_TEXT_NODE or @c wxXML_CDATA_SECTION_NODE.
        @param lineNo
            Number of line this node was present at in input file or -1.
    */
    wxXmlNode(wxXmlNodeType type, const wxString& name,
              const wxString& content = wxEmptyString,
              int lineNo = -1);

    /**
        Copy constructor.

        Note that this does NOT copy siblings and parent pointer, i.e. GetParent()
        and GetNext() will return @NULL after using copy ctor and are never unmodified by operator=().
        On the other hand, it DOES copy children and attributes.
    */
    wxXmlNode(const wxXmlNode& node);

    /**
        The virtual destructor. Deletes attached children and attributes.
    */
    virtual ~wxXmlNode();

    /**
        Appends a attribute with given @a name and @a value to the list of
        attributes for this node.
    */
    virtual void AddAttribute(const wxString& name, const wxString& value);

    /**
        Appends given attribute to the list of attributes for this node.
    */
    virtual void AddAttribute(wxXmlAttribute* attr);

    /**
        Adds node @a child as the last child of this node. Once added, the XML
        tree takes ownership of this object and there is no need to delete it.

        @note
        Note that this function works in O(n) time where @e n is the number
        of existing children. Consequently, adding large number of child
        nodes using this method can be expensive, because it has O(n^2) time
        complexity in number of nodes to be added. Use InsertChildAfter() to
        populate XML tree in linear time.

        @see InsertChild(), InsertChildAfter()
    */
    virtual void AddChild(wxXmlNode* child);

    /**
        Removes the first attributes which has the given @a name from the list of
        attributes for this node.
    */
    virtual bool DeleteAttribute(const wxString& name);

    /**
        Returns true if a attribute named attrName could be found.
        The value of that attribute is saved in value (which must not be @NULL).
    */
    bool GetAttribute(const wxString& attrName, wxString* value) const;

    /**
        Returns the value of the attribute named @a attrName if it does exist.
        If it does not exist, the @a defaultVal is returned.
    */
    wxString GetAttribute(const wxString& attrName,
                          const wxString& defaultVal = wxEmptyString) const;

    /**
        Return a pointer to the first attribute of this node.
    */
    wxXmlAttribute* GetAttributes() const;

    /**
        Returns the first child of this node.
        To get a pointer to the second child of this node (if it does exist), use the
        GetNext() function on the returned value.
    */
    wxXmlNode* GetChildren() const;

    /**
        Returns the content of this node. Can be an empty string.
        Be aware that for nodes of type @c wxXML_ELEMENT_NODE (the most used node type)
        the content is an empty string. See GetNodeContent() for more details.
    */
    const wxString& GetContent() const;

    /**
        Returns the number of nodes which separate this node from @c grandparent.

        This function searches only the parents of this node until it finds
        @a grandparent or the @NULL node (which is the parent of non-linked
        nodes or the parent of a wxXmlDocument's root element node).
    */
    int GetDepth(wxXmlNode* grandparent = NULL) const;

    /**
        Returns a flag indicating whether encoding conversion is necessary when saving. The default is @false.

        You can improve saving efficiency considerably by setting this value.
    */
    bool GetNoConversion() const;

    /**
        Returns line number of the node in the input XML file or @c -1 if it is unknown.
    */
    int GetLineNumber() const;

    /**
        Returns the name of this node.
        Can be an empty string (e.g. for nodes of type @c wxXML_TEXT_NODE or
        @c wxXML_CDATA_SECTION_NODE).
    */
    const wxString& GetName() const;

    /**
        Returns a pointer to the sibling of this node or @NULL if there are no
        siblings.
    */
    wxXmlNode* GetNext() const;

    /**
        Returns the content of the first child node of type @c wxXML_TEXT_NODE
        or @c wxXML_CDATA_SECTION_NODE.
        This function is very useful since the XML snippet @c "tagnametagcontent/tagname"
        is represented by expat with the following tag tree:

        @code
        wxXML_ELEMENT_NODE name="tagname", content=""
        |-- wxXML_TEXT_NODE name="", content="tagcontent"
        @endcode

        or eventually:

        @code
        wxXML_ELEMENT_NODE name="tagname", content=""
        |-- wxXML_CDATA_SECTION_NODE name="", content="tagcontent"
        @endcode

        An empty string is returned if the node has no children of type
        @c wxXML_TEXT_NODE or @c wxXML_CDATA_SECTION_NODE, or if the content
        of the first child of such types is empty.
    */
    wxString GetNodeContent() const;

    /**
        Returns a pointer to the parent of this node or @NULL if this node has no
        parent.
    */
    wxXmlNode* GetParent() const;

    /**
        Returns the type of this node.
    */
    wxXmlNodeType GetType() const;

    /**
        Returns @true if this node has a attribute named @a attrName.
    */
    bool HasAttribute(const wxString& attrName) const;

    /**
        Inserts the @a child node immediately before @a followingNode in the
        children list. Once inserted, the XML tree takes ownership of the new
        child and there is no need to delete it.

        @return @true if @a followingNode has been found and the @a child
                node has been inserted.

        @note
        For historical reasons, @a followingNode may be @NULL. In that case,
        then @a child is prepended to the list of children and becomes the
        first child of this node, i.e. it behaves identically to using the
        first children (as returned by GetChildren()) for @a followingNode).

        @see AddChild(), InsertChildAfter()
    */
    virtual bool InsertChild(wxXmlNode* child, wxXmlNode* followingNode);

    /**
        Inserts the @a child node immediately after @a precedingNode in the
        children list. Once inserted, the XML tree takes ownership of the new
        child and there is no need to delete it.

        @return @true if @a precedingNode has been found and the @a child
                node has been inserted.

        @param child
            The child to insert.
        @param precedingNode
            The node to insert @a child after. As a special case, this can be
            @NULL if this node has no children yet -- in that case, @a child
            will become this node's only child node.

        @since 2.8.8

        @see InsertChild(), AddChild()
    */
    virtual bool InsertChildAfter(wxXmlNode* child, wxXmlNode* precedingNode);

    /**
        Returns @true if the content of this node is a string containing only
        whitespaces (spaces, tabs, new lines, etc).

        Note that this function is locale-independent since the parsing of XML
        documents must always produce the exact same tree regardless of the
        locale it runs under.
    */
    bool IsWhitespaceOnly() const;

    /**
        Removes the given node from the children list.

        Returns @true if the node was found and removed or @false if the node
        could not be found.
        Note that the caller is responsible for deleting the removed node in order
        to avoid memory leaks.
    */
    virtual bool RemoveChild(wxXmlNode* child);

    /**
        Sets as first attribute the given wxXmlAttribute object.

        The caller is responsible for deleting any previously present attributes
        attached to this node.
    */
    void SetAttributes(wxXmlAttribute* attr);

    /**
        Sets as first child the given node.

        The caller is responsible for deleting any previously present children node.
    */
    void SetChildren(wxXmlNode* child);

    /**
        Sets the content of this node.
    */
    void SetContent(const wxString& con);

    /**
        Sets the name of this node.
    */
    void SetName(const wxString& name);

    /**
        Sets as sibling the given node.

        The caller is responsible for deleting any previously present sibling node.
    */
    void SetNext(wxXmlNode* next);

    /**
        Sets a flag to indicate whether encoding conversion is necessary when saving. The default is @false.

        You can improve saving efficiency considerably by setting this value.
    */
    void SetNoConversion(bool noconversion);

    /**
        Sets as parent the given node.

        The caller is responsible for deleting any previously present parent node.
    */
    void SetParent(wxXmlNode* parent);

    /**
        Sets the type of this node.
    */
    void SetType(wxXmlNodeType type);

    /**
        See the copy constructor for more info.
    */
    wxXmlNode& operator=(const wxXmlNode& node);
};



/**
    @class wxXmlAttribute

    Represents a node attribute.

    Example: in <tt>\<img src="hello.gif" id="3"/\></tt>, @c src is an attribute
    with value @c hello.gif and @c id is an attribute with value @c 3.

    @library{wxxml}
    @category{xml}

    @see wxXmlDocument, wxXmlNode
*/
class wxXmlAttribute
{
public:
    /**
        Default constructor.
    */
    wxXmlAttribute();

    /**
        Creates the attribute with given @a name and @a value.
        If @a next is not @NULL, then sets it as sibling of this attribute.
    */
    wxXmlAttribute(const wxString& name, const wxString& value,
                   wxXmlAttribute* next = NULL);

    /**
        The virtual destructor.
    */
    virtual ~wxXmlAttribute();

    /**
        Returns the name of this attribute.
    */
    wxString GetName() const;

    /**
        Returns the sibling of this attribute or @NULL if there are no siblings.
    */
    wxXmlAttribute* GetNext() const;

    /**
        Returns the value of this attribute.
    */
    wxString GetValue() const;

    /**
        Sets the name of this attribute.
    */
    void SetName(const wxString& name);

    /**
        Sets the sibling of this attribute.
    */
    void SetNext(wxXmlAttribute* next);

    /**
        Sets the value of this attribute.
    */
    void SetValue(const wxString& value);
};



/**
    @class wxXmlDoctype

    Represents a DOCTYPE Declaration.

    Example DOCTYPE: <tt>\<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"\></tt>.

    In the above example, "plist" is the name of root element,
    "-//Apple//DTD PLIST 1.0//EN" (without the quotes) is the public identifier and
    "http://www.apple.com/DTDs/PropertyList-1.0.dtd" (again, without the quotes) is
    the system identifier.

    A valid DOCTYPE exists in one of following forms:

    1. A root element name.
    2. A root element name and a system identifier.
    3. A root element name, a system identifier and a public identifier.
    4. A root element name and a public identifier. Although this form is not
    valid XML it is valid for SMGL.

    The DOCTYPE may also contain an internal subset of declarations
    added between square brackets at the end.
    These have not been implemented at this time.

    @since 3.1.0

    @library{wxxml}
    @category{xml}

    @see wxXmlDocument
*/
class wxXmlDoctype
{
public:
    /**
        Creates and possible initializes the DOCTYPE.

        @param name
            The root name.
        @param sysid
            The system identifier.
        @param pubid
            The public identifier.
    */
    wxXmlDoctype(const wxString& name = wxString(),
                 const wxString& sysid = wxString(),
                 const wxString& pubid = wxString());

    /**
        Removes all the DOCTYPE values.
    */
    void Clear();

    /**
        Returns the root name of the document.
    */
    const wxString& GetRootName() const;

    /**
        Returns the system id of the document.
    */
    const wxString& GetSystemId() const;

    /**
        Returns the public id of the document.
    */
    const wxString& GetPublicId() const;

    /**
        Returns the formatted DOCTYPE contents.

        This consists of all the text shown between the opening
        "<!DOCTYPE " and closing ">" of a DOCTYPE declaration.

        If this object is empty or invalid, i.e. IsValid() returns false, this
        method returns an empty string.
    */
    wxString GetFullString() const;

    /**
        Returns true if the contents can produce a valid DOCTYPE string.

        For an object to be valid, it must have a non-empty root name and a
        valid system identifier (currently the validity checks of the latter
        are limited to checking that it doesn't contain both single and double
        quotes).
    */
    bool IsValid() const;
};



//* Special indentation value for wxXmlDocument::Save.
#define wxXML_NO_INDENTATION           (-1)

//* Flags for wxXmlDocument::Load.
enum wxXmlDocumentLoadFlag
{
    wxXMLDOC_NONE,
    wxXMLDOC_KEEP_WHITESPACE_NODES
};



/**
    @class wxXmlDocument

    This class holds XML data/document as parsed by XML parser in the root node.

    wxXmlDocument internally uses the expat library which comes with wxWidgets to
    parse the given stream.

    A wxXmlDocument is in fact a list of wxXmlNode organised into a structure
    that reflects the XML tree being represented by the document.

    @note
    Ownership is passed to the XML tree as each wxXmlNode is added to it,
    and this has two implications. Firstly, the wxXmlDocument takes
    responsibility for deleting the node so the user should not @c delete it;
    and secondly, a wxXmlNode must always be created on the heap and never
    on the stack.

    A simple example of using XML classes is:

    @code
    wxXmlDocument doc;
    if (!doc.Load("myfile.xml"))
        return false;

    // Start processing the XML file.
    if (doc.GetRoot()->GetName() != "myroot-node")
        return false;

    // Examine prologue.
    wxXmlNode *prolog = doc.GetDocumentNode()->GetChildren();
    while (prolog)
    {

        if (prolog->GetType() == wxXML_PI_NODE && prolog->GetName() == "target")
        {

            // Process Process Instruction (PI) contents.
            wxString pi = prolog->GetContent();

            ...

        }
    }

    wxXmlNode *child = doc.GetRoot()->GetChildren();
    while (child)
    {
        if (child->GetName() == "tag1")
        {
            // Process text enclosed by tag1/tag1.
            wxString content = child->GetNodeContent();

            ...

            // Process attributes of tag1.
            wxString attrvalue1 =
                child->GetAttribute("attr1", "default-value");
            wxString attrvalue2 =
                child->GetAttribute("attr2", "default-value");

            ...
        }
        else if (child->GetName() == "tag2")
        {

            // Process tag2 ...
        }

        child = child->GetNext();
    }
    @endcode

    Note that if you want to preserve the original formatting of the loaded file
    including whitespaces and indentation, you need to turn off whitespace-only
    textnode removal and automatic indentation. For example:

    @code
    wxXmlDocument doc;
    doc.Load("myfile.xml", "UTF-8", wxXMLDOC_KEEP_WHITESPACE_NODES);

    // myfile2.xml will be identical to myfile.xml saving it this way:
    doc.Save("myfile2.xml", wxXML_NO_INDENTATION);
    @endcode

    Using default parameters, you will get a reformatted document which in general
    is different from the original loaded content:

    @code
    wxXmlDocument doc;
    doc.Load("myfile.xml");
    doc.Save("myfile2.xml");  // myfile2.xml != myfile.xml
    @endcode

    wxXmlDocument can also be used to create documents. The following code gives
    an example of creating a simple document with two nested element nodes, the
    second of which has an attribute, and a text node. It also demonstrates
    how to write the resulting output to a wxString:

    @code
    // Create a document and add the root node.
    wxXmlDocument xmlDoc;

    wxXmlNode* root = new wxXmlNode(NULL, wxXML_ELEMENT_NODE, "Root");
    xmlDoc.SetRoot(root);

    // Add some XML.
    wxXmlNode* library = new wxXmlNode (root, wxXML_ELEMENT_NODE, "Library");
    library->AddAttribute("type", "CrossPlatformList");
    wxXmlNode* name = new wxXmlNode(library, wxXML_ELEMENT_NODE, "Name");
    name->AddChild(new wxXmlNode(wxXML_TEXT_NODE, "", "wxWidgets"));

    // Write the output to a wxString.
    wxStringOutputStream stream;
    xmlDoc.Save(stream);
    @endcode

    This will produce a document that looks something like the following:

    @code
    <?xml version="1.0" encoding="UTF-8"?>
    <Root>
      <Library type="CrossPlatformList">
        <Name>wxWidgets</Name>
      </Library>
    </Root>
    @endcode

    If the root name value of the DOCTYPE is set, either by loading a file with a
    DOCTYPE declaration or by setting it directly with the SetDoctype member,
    then a DOCTYPE declaration will be added immediately after the XML declaration.

    @library{wxxml}
    @category{xml}

    @see wxXmlNode, wxXmlAttribute, wxXmlDoctype
*/
class wxXmlDocument : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxXmlDocument();

    /**
        Copy constructor. Deep copies all the XML tree of the given document.
    */
    wxXmlDocument(const wxXmlDocument& doc);

    /**
        Loads the given filename using the given encoding. See Load().
    */
    wxXmlDocument(const wxString& filename,
                  const wxString& encoding = "UTF-8");

    /**
        Loads the XML document from given stream using the given encoding. See Load().
    */
    wxXmlDocument(wxInputStream& stream,
                  const wxString& encoding = "UTF-8");

    /**
        Virtual destructor. Frees the document root node.
    */
    virtual ~wxXmlDocument();

    /**
        Appends a Process Instruction or Comment node to the document prologue.

        Calling this function will create a prologue or attach the node to the
        end of an existing prologue.

        @since 2.9.2
    */
    void AppendToProlog(wxXmlNode* node);

    /**
        Detaches the document node and returns it.

        The document node will be set to @NULL and thus IsOk() will
        return @false after calling this function.

        Note that the caller is responsible for deleting the returned node in order
        to avoid memory leaks.

        @since 2.9.2
    */
    wxXmlNode* DetachDocumentNode();

    /**
        Detaches the root entity node and returns it.

        After calling this function, the document node will remain together with
        any prologue nodes, but IsOk() will return @false since the root entity
        will be missing.

        Note that the caller is responsible for deleting the returned node in order
        to avoid memory leaks.
    */
    wxXmlNode* DetachRoot();

    /**
        Returns encoding of in-memory representation of the document
        (same as passed to Load() or constructor, defaults to UTF-8).

        @note this is meaningless in Unicode build where data are stored as @c wchar_t*.
    */
    wxString GetEncoding() const;

    /**
        Returns encoding of document (may be empty).

        @note This is the encoding original file was saved in, @b not the
              encoding of in-memory representation!
    */
    const wxString& GetFileEncoding() const;

    /**
        Returns the DOCTYPE declaration data for the document.

        @since 3.1.0
    */
    const wxXmlDoctype& GetDoctype() const;

    /**
        Returns the output line ending format used for documents.

        @since 3.1.1
    */
    wxTextFileType GetFileType() const;

    /**
        Returns the output line ending string used for documents.

        This string is determined by the last call to SetFileType().

        @since 3.1.1
    */
    wxString GetEOL() const;

    /**
        Returns the document node of the document.

        @since 2.9.2
    */
    wxXmlNode* GetDocumentNode() const;

    /**
        Returns the root element node of the document.
    */
    wxXmlNode* GetRoot() const;

    /**
        Returns the version of document.

        This is the value in the @c \<?xml version="1.0"?\> header of the XML document.
        If the version attribute was not explicitly given in the header, this function
        returns an empty string.
    */
    const wxString& GetVersion() const;

    /**
        Returns @true if the document has been loaded successfully.
    */
    bool IsOk() const;


    /**
        Parses @a filename as an xml document and loads its data.

        If @a flags does not contain wxXMLDOC_KEEP_WHITESPACE_NODES, then, while loading,
        all nodes of type @c wxXML_TEXT_NODE (see wxXmlNode) are automatically skipped
        if they contain whitespaces only.

        The removal of these nodes makes the load process slightly faster and requires
        less memory however makes impossible to recreate exactly the loaded text with a
        Save() call later. Read the initial description of this class for more info.

        Returns true on success, false otherwise.
    */
    virtual bool Load(const wxString& filename,
                      const wxString& encoding = "UTF-8", int flags = wxXMLDOC_NONE);

    /**
        Like Load(const wxString&, const wxString&, int) but takes the data from
        given input stream.
    */
    virtual bool Load(wxInputStream& stream,
                      const wxString& encoding = "UTF-8", int flags = wxXMLDOC_NONE);

    /**
        Saves XML tree creating a file named with given string.

        If @a indentstep is greater than or equal to zero, then, while saving,
        an automatic indentation is added with steps composed by indentstep spaces.

        If @a indentstep is @c wxXML_NO_INDENTATION, then, automatic indentation
        is turned off.
    */
    virtual bool Save(const wxString& filename, int indentstep = 2) const;

    /**
        Saves XML tree in the given output stream.
        See Save(const wxString&, int) for a description of @a indentstep.
    */
    virtual bool Save(wxOutputStream& stream, int indentstep = 2) const;

    /**
        Sets the document node of this document.

        Deletes any previous document node.
        Use DetachDocumentNode() and then SetDocumentNode() if you want to
        replace the document node without deleting the old document tree.

        @since 2.9.2
    */
    void SetDocumentNode(wxXmlNode* node);

    /**
        Sets the encoding of the document.
    */
    void SetEncoding(const wxString& enc);

    /**
        Sets the encoding of the file which will be used to save the document.
    */
    void SetFileEncoding(const wxString& encoding);

    /**
        Sets the data which will appear in the DOCTYPE declaration when the
        document is saved.

        @since 3.1.0
    */
    void SetDoctype(const wxXmlDoctype& doctype);

    /**
        Sets the output line ending formats when the document is saved.

        By default Unix file type is used, i.e. a single ASCII LF (10)
        character is used at the end of lines.

        @since 3.1.1
    */
    void SetFileType(wxTextFileType fileType);

    /**
        Sets the root element node of this document.

        Will create the document node if necessary. Any previous
        root element node is deleted.
    */
    void SetRoot(wxXmlNode* node);

    /**
        Sets the version of the XML file which will be used to save the document.
    */
    void SetVersion(const wxString& version);

    /**
        Deep copies the given document.
    */
    wxXmlDocument& operator=(const wxXmlDocument& doc);

    /**
       Get expat library version information.

       @since 2.9.2
       @see wxVersionInfo
    */
    static wxVersionInfo GetLibraryVersionInfo();
};
