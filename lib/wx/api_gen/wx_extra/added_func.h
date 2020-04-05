//  Added 3.0 functionality

enum wxMouseWheelAxis
    {
        wxMOUSE_WHEEL_VERTICAL,
        wxMOUSE_WHEEL_HORIZONTAL
    };

#define wxMOUSE_BTN_AUX1 4
#define wxMOUSE_BTN_AUX2 5

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


/* class wxWindow { */
/*  public: */
/*     bool IsDoubleBuffered(); */
/*     void SetDoubleBuffered(bool on); */
/* }; */

class wxWindowGTK {
 public:
    double GetContentScaleFactor();
    wxSize GetDPI();

    wxSize FromDIP(const wxSize& sz);
    wxSize ToDIP(const wxSize& sz) const { return ToDIP(sz, this); }

};

class wxDisplay {
    public:
    // get the resolution of this monitor in pixels per inch
    wxSize GetPPI() const;
};

class wxMenuBar {
 public:
    // MacSpecific API
    wxMenu *OSXGetAppleMenu() const { return m_appleMenu; }
    static void SetAutoWindowMenu( bool enable ) { s_macAutoWindowMenu = enable ; }
    static bool GetAutoWindowMenu() { return s_macAutoWindowMenu ; }
};


class wxMouseEvent {
 public:
    wxMouseWheelAxis GetWheelAxis() const;
};
