
//  I have added this doxygen seems to get lost in some macros
//  in sashwin.h

enum wxSashDragStatus
   {
      wxSASH_STATUS_OK,
      wxSASH_STATUS_OUT_OF_RANGE
   };

// Macro needed in erlang
class WXDLLIMPEXP_XRC wxXmlResource : public wxObject
{
 public:
   wxObject xrcctrl(wxWindow *Window, wxString Name, wxString Type);
};

// The generater needs constructors
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

class WXDLLIMPEXP_ADV wxGridCellNumberRenderer : public wxGridCellStringRenderer
{
 public:
   wxGridCellNumberRenderer();
};

// Enable test for valid wxTreeItemId's
class WXDLLIMPEXP_ADV wxTreeCtrlBase : public wxControl
{
 public:
    static bool IsTreeItemIdOk(wxTreeItemId id);
};


// Enable lost macro functionality
class WXDLLEXPORT wxPanel : public wxWindow
{
 public:
    void SetFocusIgnoringChildren();
};
