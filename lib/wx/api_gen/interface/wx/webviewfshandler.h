/////////////////////////////////////////////////////////////////////////////
// Name:        webviewfshandler.h
// Purpose:     interface of wxWebViewFSHandler
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxWebViewFSHandler

    A wxWebView file system handler to support standard wxFileSystem protocols
    of the form <code> example:page.htm </code> The handler allows wxWebView to
    use wxFileSystem in a similar fashion to its use with wxHtml.

    The wxMemoryFSHandler documentation gives an example of how it may be used.

    @since 2.9.5
    @library{wxwebview}
    @category{webview}

    @see wxWebView, wxWebViewHandler, wxWebViewArchiveHandler
 */
class wxWebViewFSHandler : public wxWebViewHandler
{
public:
    /**
        Constructor.
    */
    wxWebViewFSHandler(const wxString& scheme);
    virtual wxFSFile* GetFile(const wxString &uri);
};
