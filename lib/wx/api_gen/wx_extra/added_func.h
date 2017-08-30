//  Added 3.0 functionality

class WXDLLIMPEXP_AUI wxAuiTabArt
{
public:
    virtual void SetColour(const wxColour& colour) = 0;
    virtual void SetActiveColour(const wxColour& colour) = 0;
};

// Api to get data out of paneinfo
class WXDLLIMPEXP_AUI wxAuiPaneInfo
{
 public:
    wxString GetName();
    wxString GetCaption();
    wxIcon GetIcon();

    wxWindow* GetWindow();
    wxFrame* GetFrame();

    int GetDirection();
    int GetLayer();
    int GetRow();
    int GetPosition();

    wxPoint GetFloatingPosition();
    wxSize GetFloatingSize();
};

class wxToolBar {
 public:
    wxToolBarToolBase * AddStretchableSpace();
    wxToolBarToolBase * InsertStretchableSpace(size_t pos);
};


class wxWindow {
 public:
    bool IsDoubleBuffered();
    void SetDoubleBuffered(bool on);
};
