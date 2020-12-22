///////////////////////////////////////////////////////////////////////////////
// Name:        wx/aui/tabmdi.h
// Purpose:     Documentation of wxAui MDI classes.
// Created:     2016-10-27
// Copyright:   (c) 2016 wxWidgets development team
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////


class wxAuiMDIParentFrame : public wxFrame
{
public:
    wxAuiMDIParentFrame();
    wxAuiMDIParentFrame(wxWindow *parent,
                        wxWindowID winid,
                        const wxString& title,
                        const wxPoint& pos = wxDefaultPosition,
                        const wxSize& size = wxDefaultSize,
                        long style = wxDEFAULT_FRAME_STYLE | wxVSCROLL | wxHSCROLL,
                        const wxString& name = wxFrameNameStr);

    ~wxAuiMDIParentFrame();

    bool Create(wxWindow *parent,
                wxWindowID winid,
                const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_FRAME_STYLE | wxVSCROLL | wxHSCROLL,
                const wxString& name = wxFrameNameStr );

    void SetArtProvider(wxAuiTabArt* provider);
    wxAuiTabArt* GetArtProvider();
    wxAuiNotebook* GetNotebook() const;

    wxMenu* GetWindowMenu() const;
    void SetWindowMenu(wxMenu* pMenu);

    virtual void SetMenuBar(wxMenuBar *pMenuBar);

    void SetChildMenuBar(wxAuiMDIChildFrame *pChild);

    wxAuiMDIChildFrame *GetActiveChild() const;
    void SetActiveChild(wxAuiMDIChildFrame* pChildFrame);

    wxAuiMDIClientWindow *GetClientWindow() const;
    virtual wxAuiMDIClientWindow *OnCreateClient();

    virtual void Cascade();
    virtual void Tile(wxOrientation orient = wxHORIZONTAL);
    virtual void ArrangeIcons();
    virtual void ActivateNext();
    virtual void ActivatePrevious();
};



class wxAuiMDIChildFrame : public wxPanel
{
public:
    wxAuiMDIChildFrame();
    wxAuiMDIChildFrame(wxAuiMDIParentFrame *parent,
                       wxWindowID winid,
                       const wxString& title,
                       const wxPoint& pos = wxDefaultPosition,
                       const wxSize& size = wxDefaultSize,
                       long style = wxDEFAULT_FRAME_STYLE,
                       const wxString& name = wxFrameNameStr);

    virtual ~wxAuiMDIChildFrame();
    bool Create(wxAuiMDIParentFrame *parent,
                wxWindowID winid,
                const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_FRAME_STYLE,
                const wxString& name = wxFrameNameStr);

    virtual void SetMenuBar(wxMenuBar *menuBar);
    virtual wxMenuBar *GetMenuBar() const;

    virtual void SetTitle(const wxString& title);
    virtual wxString GetTitle() const;

    virtual void SetIcons(const wxIconBundle& icons);
    virtual const wxIconBundle& GetIcons() const;

    virtual void SetIcon(const wxIcon& icon);
    virtual const wxIcon& GetIcon() const;

    virtual void Activate();
    virtual bool Destroy();

    virtual bool Show(bool show = true);

    // no status bars
    virtual wxStatusBar* CreateStatusBar(int number = 1,
                                         long style = 1,
                                         wxWindowID winid = 1,
                                         const wxString& name = wxEmptyString);

    virtual wxStatusBar *GetStatusBar() const;
    virtual void SetStatusText( const wxString &text, int number=0 );
    virtual void SetStatusWidths( int n, const int widths_field[] );

    // no toolbar bars
    virtual wxToolBar* CreateToolBar(long style,
                                     wxWindowID winid,
                                     const wxString& name);
    virtual wxToolBar *GetToolBar() const;

    // no maximize etc
    virtual void Maximize(bool maximize = true);
    virtual void Restore();
    virtual void Iconize(bool iconize  = true);
    virtual bool IsMaximized() const;
    virtual bool IsIconized() const;
    virtual bool ShowFullScreen(bool show, long style);
    virtual bool IsFullScreen() const;

    virtual bool IsTopLevel() const;

    void SetMDIParentFrame(wxAuiMDIParentFrame* parent);
    wxAuiMDIParentFrame* GetMDIParentFrame() const;
};


class wxAuiMDIClientWindow : public wxAuiNotebook
{
public:
    wxAuiMDIClientWindow();
    wxAuiMDIClientWindow(wxAuiMDIParentFrame *parent, long style = 0);

    virtual bool CreateClient(wxAuiMDIParentFrame *parent,
                              long style = wxVSCROLL | wxHSCROLL);

    virtual int SetSelection(size_t page);
    virtual wxAuiMDIChildFrame* GetActiveChild();
    virtual void SetActiveChild(wxAuiMDIChildFrame* pChildFrame);
};
