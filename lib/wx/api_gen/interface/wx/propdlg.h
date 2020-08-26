/////////////////////////////////////////////////////////////////////////////
// Name:        propdlg.h
// Purpose:     interface of wxPropertySheetDialog
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    Values used by wxPropertySheetDialog::SetSheetStyle
*/
enum wxPropertySheetDialogFlags
{
    /**
        Uses the default look and feel for the controller window,
        normally a notebook except on Smartphone where a choice control is used.
    */
    wxPROPSHEET_DEFAULT = 0x0001,

    /**
        Uses a notebook for the controller window.
    */
    wxPROPSHEET_NOTEBOOK = 0x0002,

    /**
        Uses a toolbook for the controller window.
    */
    wxPROPSHEET_TOOLBOOK = 0x0004,

    /**
        Uses a choicebook for the controller window.
    */
    wxPROPSHEET_CHOICEBOOK = 0x0008,

    /**
        Uses a listbook for the controller window.
    */
    wxPROPSHEET_LISTBOOK = 0x0010,

    /**
        Uses a button toolbox for the controller window.
    */
    wxPROPSHEET_BUTTONTOOLBOOK = 0x0020,

    /**
        Uses a treebook for the controller window.
    */
    wxPROPSHEET_TREEBOOK = 0x0040,

    /**
        Shrinks the dialog window to fit the currently selected page
        (common behaviour for property sheets on macOS).
    */
    wxPROPSHEET_SHRINKTOFIT = 0x0100,
};


/**
    @class wxPropertySheetDialog

    This class represents a property sheet dialog: a tabbed dialog
    for showing settings. It is optimized to show flat tabs
    on PocketPC devices, and can be customized to use different
    controllers instead of the default notebook style.

    To use this class, call Create() from your own Create function.
    Then call CreateButtons(), and create pages, adding them to the book control.
    Finally call LayoutDialog().

    For example:

    @code
    bool MyPropertySheetDialog::Create(...)
    {
        if (!wxPropertySheetDialog::Create(...))
            return false;

        CreateButtons(wxOK|wxCANCEL|wxHELP);

        // Add page
        wxPanel* panel = new wxPanel(GetBookCtrl(), ...);
        GetBookCtrl()->AddPage(panel, "General");

        LayoutDialog();
        return true;
    }
    @endcode

    If necessary, override CreateBookCtrl() and AddBookCtrl() to create and add a
    different kind of book control. You will then need to use two-step construction
    for the dialog or change the style of the book control by calling SetSheetStyle()
    before calling Create().

    The @ref page_samples_dialogs shows this class being used with notebook and toolbook
    controllers (for Windows-style and Mac-style settings dialogs).

    To make pages of the dialog scroll when the display is too small to fit the
    whole dialog, you can switch layout adaptation on globally with
    wxDialog::EnableLayoutAdaptation() or per dialog with
    wxDialog::SetLayoutAdaptationMode().

    For more about layout adaptation, see @ref overview_dialog_autoscrolling.

    @library{wxcore}
    @category{managedwnd}
*/
class wxPropertySheetDialog : public wxDialog
{
public:
    /**
       Default constructor. Call Create if using this form of constructor.
    */
    wxPropertySheetDialog();

    /**
        Constructor.
    */
    wxPropertySheetDialog(wxWindow* parent, wxWindowID id,
                          const wxString& title,
                          const wxPoint& pos = wxDefaultPosition,
                          const wxSize& size = wxDefaultSize,
                          long style = wxDEFAULT_DIALOG_STYLE,
                          const wxString& name = wxDialogNameStr);

    /**
        Override this if you wish to add the book control in a way different from the
        standard way (for example, using different spacing).
    */
    virtual void AddBookCtrl(wxSizer* sizer);

    /**
        Call this from your own Create function, before adding buttons and pages.
    */
    bool Create(wxWindow* parent, wxWindowID id, const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_DIALOG_STYLE,
                const wxString& name = wxDialogNameStr);

    /**
        Override this if you wish to create a different kind of book control; by
        default, the value passed to SetSheetStyle() is used to determine the control.

        The default behaviour is to create a notebook except on Smartphone, where a
        choicebook is used.
    */
    virtual wxBookCtrlBase* CreateBookCtrl();

    /**
        Call this to create the buttons for the dialog.
        This calls wxDialog::CreateButtonSizer(), and the flags are the same.

        @note On PocketPC, no buttons are created.
    */
    virtual void CreateButtons(int flags = wxOK|wxCANCEL);

    /**
        Returns the book control that will contain your settings pages.
    */
    wxBookCtrlBase* GetBookCtrl() const;

    /**
        Returns the inner sizer that contains the book control and button sizer.
    */
    wxSizer* GetInnerSizer() const;

    /**
       Set the inner sizer that contains the book control and button sizer.
     */
    void SetInnerSizer(wxSizer* sizer);

    /**
        Returns the sheet style.

        See SetSheetStyle() for allowed values.
    */
    long GetSheetStyle() const;

    /**
        Call this to lay out the dialog.

        @note On PocketPC, this does nothing, since the dialog will be shown full-screen,
              and the layout will be done when the dialog receives a size event.
    */
    virtual void LayoutDialog(int centreFlags = wxBOTH);

    /**
        Sets the book control used for the dialog.

        You will normally not need to use this.
    */
    void SetBookCtrl(wxBookCtrlBase* bookCtrl);

    /**
        You can customize the look and feel of the dialog by setting the sheet style.
        It is a bit list of the ::wxPropertySheetDialogFlags values.
    */
    void SetSheetStyle(long style);


    /**
       Set the border around the whole dialog
    */
    void SetSheetOuterBorder(int border);

    /**
       Returns the border around the whole dialog
    */
    int GetSheetOuterBorder() const;


    /**
       Set the border around the book control only.
    */
    void SetSheetInnerBorder(int border);

    /**
       Returns the border around the book control only.
    */
    int GetSheetInnerBorder() const;


    virtual wxWindow* GetContentWindow() const;

};

