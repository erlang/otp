/////////////////////////////////////////////////////////////////////////////
// Name:        richtext/richtextxml.h
// Purpose:     interface of wxRichTextXMLHandler
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxRichTextXMLHandler

    A handler for loading and saving content in an XML format specific
    to wxRichTextBuffer.

    You can either add the handler to the buffer and load and save through
    the buffer or control API, or you can create an instance of the handler
    on the stack and call its functions directly.


    @section richtextxmlhandler_flags Handler flags

    The following flags can be used with this handler, via the handler's SetFlags()
    function or the buffer or control's SetHandlerFlags() function:

    - wxRICHTEXT_HANDLER_INCLUDE_STYLESHEET
      Include the style sheet in loading and saving operations.


    @library{wxrichtext}
    @category{richtext}
*/
class wxRichTextXMLHandler : public wxRichTextFileHandler
{
public:
    /**
        Constructor.
    */
    wxRichTextXMLHandler(const wxString& name = "XML",
                         const wxString& ext = "xml",
                         int type = wxRICHTEXT_TYPE_XML);

    /**
        Returns @true.
    */
    virtual bool CanLoad() const;

    /**
        Returns @true.
    */
    virtual bool CanSave() const;

    /**
        Recursively exports an object to the stream.
    */
    bool ExportXML(wxOutputStream& stream, wxRichTextObject& obj, int level);

    /**
        Recursively imports an object.
    */
    bool ImportXML(wxRichTextBuffer* buffer, wxRichTextObject* obj, wxXmlNode* node);

    /**
        Call with XML node name, C++ class name so that wxRTC can read in the node.
        If you add a custom object, call this.
    */
    static void RegisterNodeName(const wxString& nodeName, const wxString& className) { sm_nodeNameToClassMap[nodeName] = className; }

    /**
        Cleans up the mapping between node name and C++ class.
    */
    static void ClearNodeToClassMap() { sm_nodeNameToClassMap.clear(); }

protected:

    /**
        Loads buffer context from the given stream.
    */
    virtual bool DoLoadFile(wxRichTextBuffer* buffer, wxInputStream& stream);

    /**
        Saves buffer context to the given stream.
    */
    virtual bool DoSaveFile(wxRichTextBuffer* buffer, wxOutputStream& stream);
};

