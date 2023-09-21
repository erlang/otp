// Macro needed in erlang
class WXDLLIMPEXP_XRC wxXmlResource : public wxObject
{
 public:
  /**
     Looks up a control.

     Get a control with @a Name in a window created with XML
     resources. You can use it to set/get values from controls.
     The object is type casted to <b>Type</b>.
     Example: <br />
     @code
     Xrc = wxXmlResource:get(),
     Dlg = wxDialog:new(),
     true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "controls_dialog"),
     LCtrl = xrcctrl(Dlg, "controls_listctrl", wxListCtrl),
     wxListCtrl:insertColumn(LCtrl, 0, "Name", [{width, 200}]),
     @endcode

  */
   static wxObject xrcctrl(wxWindow *Window, wxString Name, wxString Type);
};

// Enable test for valid wxTreeItemId's
class WXDLLIMPEXP_ADV wxTreeCtrlBase : public wxControl
{
 public:
    static bool IsTreeItemIdOk(wxTreeItemId id);
};

// The generator needs constructors  (is this still valid?)
class WXDLLIMPEXP_ADV wxGridCellBoolRenderer : public wxGridCellRenderer
{
 public:
   wxGridCellBoolRenderer();
};

class WXDLLIMPEXP_ADV wxGridCellStringRenderer : public wxGridCellRenderer
{
 public:
   wxGridCellStringRenderer();
};


class wxMenuBar {
 public:
    // MacSpecific API
    static void SetAutoWindowMenu( bool enable ) { s_macAutoWindowMenu = enable ; }
    static bool GetAutoWindowMenu() { return s_macAutoWindowMenu ; }
};

// Deprecated functions in 3.1

class wxWindow {
 public:
  virtual void MakeModal(bool modal = true)
};

// wxListItemAttr Is typedef to wxItemAttr in 3.1
class wxListItemAttr {
public:
      wxListItemAttr() { }
    wxListItemAttr(const wxColour& colText,
                   const wxColour& colBack,
                   const wxFont& font)
        : m_colText(colText), m_colBack(colBack), m_font(font)
    {
    }

    // default copy ctor, assignment operator and dtor are ok
    // setters
    void SetTextColour(const wxColour& colText) { m_colText = colText; }
    void SetBackgroundColour(const wxColour& colBack) { m_colBack = colBack; }
    void SetFont(const wxFont& font) { m_font = font; }

    // accessors
    bool HasTextColour() const { return m_colText.Ok(); }
    bool HasBackgroundColour() const { return m_colBack.Ok(); }
    bool HasFont() const { return m_font.Ok(); }

    const wxColour& GetTextColour() const { return m_colText; }
    const wxColour& GetBackgroundColour() const { return m_colBack; }
    const wxFont& GetFont() const { return m_font; }
};

class wxStyledTextEvent : public wxCommandEvent {
public:
  bool GetDragAllowMove();
};


// No in api?
class wxAuiNotebookEvent : public wxBookCtrlEvent {
public:
  void SetDragSource(wxAuiNotebook* s) { m_dragSource = s; }
  wxAuiNotebook* GetDragSource() const { return m_dragSource; }
};

class wxGLCanvas : public wxWindow
{
public:
  bool CreateSurface();
};
