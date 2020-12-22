/////////////////////////////////////////////////////////////////////////////
// Name:        interface/collheaderctrl.h
// Purpose:     wxCollapsibleHeaderCtrl documentation
// Author:      Tobias Taschner
// Created:     2015-09-19
// Copyright:   (c) 2015 wxWidgets development team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxCollapsibleHeaderCtrl

    Header control above a collapsible pane.

    The collapsible header usually consists of a small indicator of the
    collapsed state and the label text beside it.
    This class is used by the generic implementation of wxCollapsiblePane but
    maybe used in more complex layouts for other uses.

    @beginEventTable{wxCommandEvent}
    @event{EVT_COLLAPSIBLEHEADER_CHANGED(id, func)}
        User changed the collapsed state.
    @endEventTable

    @since 3.1.0

    @library{wxcore}
    @category{ctrl}

    @see wxCollapsiblePane
*/
class wxCollapsibleHeaderCtrl : public wxControl
{
public:
    wxCollapsibleHeaderCtrl();

    /**
        Constructor fully creating the control.

        The arguments have the usual meanings and only @a parent is typically
        required.
    */
    wxCollapsibleHeaderCtrl(wxWindow *parent,
        wxWindowID id,
        const wxString& label,
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize,
        long style = wxBORDER_NONE,
        const wxValidator& validator = wxDefaultValidator,
        const wxString& name = wxCollapsibleHeaderCtrlNameStr);

    /**
        Create the control initialized using the default constructor.

        This method can be used to finish the control creation if it hadn't
        been done already by using the non-default constructor.
    */
    bool Create(wxWindow *parent,
        wxWindowID id,
        const wxString& label,
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize,
        long style = wxBORDER_NONE,
        const wxValidator& validator = wxDefaultValidator,
        const wxString& name = wxCollapsibleHeaderCtrlNameStr);

    /**
        Set collapsed state of the header.
    */
    virtual void SetCollapsed(bool collapsed = true);

    /**
        Returns @c true if the control is collapsed.

        @see SetCollapsed()
    */
    virtual bool IsCollapsed() const;
};
