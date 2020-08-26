/////////////////////////////////////////////////////////////////////////////
// Name:        fs_mem.h
// Purpose:     interface of wxMemoryFSHandler
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxMemoryFSHandler

    This wxFileSystem handler can store arbitrary data in memory stream and make
    them accessible via an URL.

    It is particularly suitable for storing bitmaps from resources or included XPM
    files so that they can be used with wxHTML or wxWebView.

    Filenames are prefixed with @c "memory:", e.g. @c "memory:myfile.html".

    Example:

    @code
    #ifndef __WXMSW__
    #include "logo.xpm"
    #endif

    void MyFrame::OnAbout(wxCommandEvent&)
    {
        wxFileSystem::AddHandler(new wxMemoryFSHandler);
        wxMemoryFSHandler::AddFile("logo.png", wxBITMAP(logo), wxBITMAP_TYPE_PNG);
        wxMemoryFSHandler::AddFile("about.htm",
                                "<html><body>About: "
                                "<img src=\"memory:logo.png\"></body></html>");

        wxDialog dlg(this, -1, wxString(_("About")));
        wxBoxSizer *topsizer;
        topsizer = new wxBoxSizer(wxVERTICAL);
    #ifdef USE_WEBVIEW
        wxWebView* browser = wxWebView::New(&dlg, wxID_ANY, wxWebViewDefaultURLStr,
                                 wxDefaultPosition, wxSize(380, 160));
        browser->RegisterHandler(wxSharedPtr<wxWebViewHandler>(new wxWebViewFSHandler("memory")));
        browser->LoadURL("memory:about.htm");
    #else // Use wxHtml
        wxHtmlWindow *browser;
        browser = new wxHtmlWindow(&dlg, -1, wxDefaultPosition,
                                   wxSize(380, 160), wxHW_SCROLLBAR_NEVER);
        browser->SetBorders(0);
        browser->LoadPage("memory:about.htm");
        browser->SetSize(browser->GetInternalRepresentation()->GetWidth(),
                    browser->GetInternalRepresentation()->GetHeight());
    #endif
        topsizer->Add(browser, 1, wxALL, 10);
        topsizer->Add(new wxStaticLine(&dlg, -1), 0, wxEXPAND | wxLEFT | wxRIGHT, 10);
        topsizer->Add(new wxButton(&dlg, wxID_OK, "Ok"),
                    0, wxALL | wxALIGN_RIGHT, 15);
        dlg.SetAutoLayout(true);
        dlg.SetSizer(topsizer);
        topsizer->Fit(&dlg);
        dlg.Centre();
        dlg.ShowModal();

        wxMemoryFSHandler::RemoveFile("logo.png");
        wxMemoryFSHandler::RemoveFile("about.htm");
    }
    @endcode

    @library{wxbase}
    @category{vfs}

    @see wxMemoryFSHandler::AddFileWithMimeType
*/
class wxMemoryFSHandler : public wxFileSystemHandler
{
public:
    /**
        Constructor.
    */
    wxMemoryFSHandler();

    //@{
    /**
        Adds a file to the list of the files stored in memory.

        Stored data (bitmap, text or raw data) will be copied into private memory
        stream and available under name @c "memory:" + @e filename.

        @note you must use a @a type value (aka image format) that wxWidgets
              can save (e.g. JPG, PNG, see wxImage documentation)!

        @see AddFileWithMimeType()
    */
    static void AddFile(const wxString& filename, wxImage& image, wxBitmapType type);
    static void AddFile(const wxString& filename, const wxBitmap& bitmap, wxBitmapType type);
    static void AddFile(const wxString& filename, const wxString& textdata);
    static void AddFile(const wxString& filename, const void *binarydata, size_t size);

    //@}

    //@{
    /**
        Like AddFile(), but lets you explicitly specify added file's MIME type.

        This version should be used whenever you know the MIME type, because it
        makes accessing the files faster.

        @since 2.8.5

        @see AddFile()
    */
    static void AddFileWithMimeType(const wxString& filename,
                                    const wxString& textdata,
                                    const wxString& mimetype);
    static void AddFileWithMimeType(const wxString& filename,
                                    const void* binarydata,
                                    size_t size,
                                    const wxString& mimetype);
    //@}

    /**
        Removes a file from memory FS and frees the occupied memory.
    */
    static void RemoveFile(const wxString& filename);
};

