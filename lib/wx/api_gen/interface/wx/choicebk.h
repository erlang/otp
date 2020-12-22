/////////////////////////////////////////////////////////////////////////////
// Name:        choicebk.h
// Purpose:     interface of wxChoicebook
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


// wxChoicebook flags
#define wxCHB_DEFAULT          wxBK_DEFAULT
#define wxCHB_TOP              wxBK_TOP
#define wxCHB_BOTTOM           wxBK_BOTTOM
#define wxCHB_LEFT             wxBK_LEFT
#define wxCHB_RIGHT            wxBK_RIGHT
#define wxCHB_ALIGN_MASK       wxBK_ALIGN_MASK

wxEventType wxEVT_CHOICEBOOK_PAGE_CHANGED;
wxEventType wxEVT_CHOICEBOOK_PAGE_CHANGING;

/**
    @class wxChoicebook

    wxChoicebook is a class similar to wxNotebook, but uses a wxChoice control
    to show the labels instead of the tabs.

    For usage documentation of this class, please refer to the base abstract class
    wxBookCtrl. You can also use the @ref page_samples_notebook to see wxChoicebook in
    action.

    wxChoicebook allows the use of wxBookCtrlBase::GetControlSizer(), allowing
    a program to add other controls next to the choice control. This is
    particularly useful when screen space is restricted, as it often is when
    wxChoicebook is being employed.

    @beginStyleTable
    @style{wxCHB_DEFAULT}
           Choose the default location for the labels depending on the current
           platform (but currently it's the same everywhere, namely wxCHB_TOP).
    @style{wxCHB_TOP}
           Place labels above the page area.
    @style{wxCHB_LEFT}
           Place labels on the left side.
    @style{wxCHB_RIGHT}
           Place labels on the right side.
    @style{wxCHB_BOTTOM}
           Place labels below the page area.
    @endStyleTable

    @beginEventEmissionTable{wxBookCtrlEvent}
    @event{EVT_CHOICEBOOK_PAGE_CHANGED(id, func)}
        The page selection was changed.
        Processes a @c wxEVT_CHOICEBOOK_PAGE_CHANGED event.
    @event{EVT_CHOICEBOOK_PAGE_CHANGING(id, func)}
        The page selection is about to be changed.
        Processes a @c wxEVT_CHOICEBOOK_PAGE_CHANGING event.
        This event can be vetoed (using wxNotifyEvent::Veto()).
    @endEventTable

    @library{wxcore}
    @category{bookctrl}
    @appearance{choicebook}

    @see @ref overview_bookctrl, wxNotebook, @ref page_samples_notebook
*/
class wxChoicebook : public wxBookCtrlBase
{
public:
    //@{
    /**
        Constructs a choicebook control.
    */
    wxChoicebook();
    wxChoicebook(wxWindow* parent, wxWindowID id,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = 0,
                 const wxString& name = wxEmptyString);
    //@}

    /**
       Create the choicebook control that has already been constructed with
       the default constructor.
    */
    bool Create(wxWindow *parent,
                wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxString& name = wxEmptyString);


    /**
        Returns the wxChoice associated with the control.
    */
    wxChoice * GetChoiceCtrl() const;
};

