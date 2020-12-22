/////////////////////////////////////////////////////////////////////////////
// Name:        xrc/xh_sizer.h
// Purpose:     XML resource handler for wxSizer
// Author:      Kinaou Hervé
// Created:     2010-10-24
// Copyright:   (c) 2010 wxWidgets development team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxSizerXmlHandler

    @class wxXmlResourceHandler

    wxSizerXmlHandler is a class for resource handlers capable of creating
    a wxSizer object from an XML node.

    @see wxXmlResourceHandler, wxSizer

    @library{wxxrc}
    @category{xrc}
*/
class wxSizerXmlHandler : public wxXmlResourceHandler
{
public:
    /**
        Constructor.
        Initializes the attributes and adds the supported styles.
    */
    wxSizerXmlHandler();

    /**
        Creates a sizer, sizeritem or spacer object, depending on
        the current handled node.
        @see wxXmlResourceHandler::DoCreateResource().
    */
    virtual wxObject *DoCreateResource();

    /**
        Returns @true if the given node can be handled by this class.
        If the node concerns a sizer object, the method IsSizerNode is called
        to know if the class is managed or not.
        If the node concerns a sizer item or a spacer, @true is returned.
        Otherwise @false is returned.
        @see wxXmlResourceHandler::CanHandle().
    */
    virtual bool CanHandle(wxXmlNode *node);

protected:
    /**
        Creates an object of type wxSizer from the XML node content.

        This virtual method can be overridden to add support for custom sizer
        classes to the derived handler.

        Notice that if you override this method you would typically overload
        IsSizerNode() as well.

        Example of use of this method:
        @code
        class MySizerXmlHandler : public wxSizerXmlHandler
        {
            ...

        protected:
            bool IsSizerNode(wxXmlNode *node) const
            {
                return IsOfClass(node, "MySizer") ||
                        wxSizerXmlHandler::IsSizerNode(node));
            }

            void DoCreateSizer(const wxString& name)
            {
                if ( name == "MySizer" )
                    return Handle_MySizer();
                else
                    return wxSizerXmlHandler::DoCreateSizer(name);
            }

        private:
            wxSizer* Handle_MySizer()
            {
                // Create your own sizer here from XRC content (see
                // wxXmlResource methods) and return the instance.
            }
        };
        @endcode

        @since 2.9.2
    */
    virtual wxSizer* DoCreateSizer(const wxString& name);

    /**
        Used by CanHandle() to know if the given node contains a sizer
        supported by this class.

        This method should be overridden to allow this handler to be used for
        the custom sizer types.

        See the example in DoCreateSizer() description for how it can be used.

        @since 2.9.2
    */
    virtual bool IsSizerNode(wxXmlNode *node) const;

};
