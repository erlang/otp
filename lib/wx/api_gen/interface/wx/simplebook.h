/////////////////////////////////////////////////////////////////////////////
// Name:        wx/simplebook.h
// Purpose:     wxSimplebook public API documentation.
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxSimplebook

    wxSimplebook is a control showing exactly one of its several pages.

    It implements wxBookCtrlBase class interface but doesn't allow the user to
    change the page being displayed, unlike all the other book control classes,
    only the program can do it.

    This class is created in the same manner as any other wxBookCtrl but then
    the program will typically call ChangeSelection() to show different pages.
    See the @ref page_samples_notebook for an example of wxSimplebook in
    action.

    Notice that it is often convenient to use ShowNewPage() instead of the base
    class AddPage().

    There are no special styles defined for this class as it has no visual
    appearance of its own.

    There are also no special events, this class reuses
    @c wxEVT_BOOKCTRL_PAGE_CHANGING and @c
    wxEVT_BOOKCTRL_PAGE_CHANGED events for the events it generates if
    the program calls SetSelection().

    @library{wxcore}
    @category{bookctrl}

    @see wxBookCtrl, wxNotebook, @ref page_samples_notebook

    @since 2.9.5
*/
class wxSimplebook : public wxBookCtrlBase
{
public:
    /**
        Default constructor.

        Use Create() later to really create the control.
    */
    wxSimplebook();

    /**
        Constructs a simple book control.
    */
    wxSimplebook(wxWindow* parent,
                 wxWindowID id = wxID_ANY,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = 0,
                 const wxString& name = wxEmptyString);

    /**
        Really create the window of an object created using default
        constructor.

        @since 3.0.2
     */
    bool Create(wxWindow* parent,
                wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxString& name = wxEmptyString);

    /**
        Set the effects to use for showing and hiding the pages.

        This method allows specifying the effects passed to
        wxWindow::ShowWithEffect() and wxWindow::HideWithEffect() respectively
        when the pages need to be shown or hidden.

        By default, no effects are used, but as the pages are only changed
        by the program and not the user himself, it may be useful to use some
        visual effects to make the changes more noticeable.

        @param showEffect
            The effect to use for showing the newly selected page.
        @param hideEffect
            The effect to use for hiding the previously selected page.

        @see SetEffectsTimeouts()
     */
    void SetEffects(wxShowEffect showEffect, wxShowEffect hideEffect);

    /**
        Set the same effect to use for both showing and hiding the pages.

        This is the same as <code>SetEffects(effect, effect)</code>.

        @see SetEffectTimeout()
     */
    void SetEffect(wxShowEffect effect);

    /**
        Set the effect timeout to use for showing and hiding the pages.

        This method allows configuring the timeout arguments passed to
        wxWindow::ShowWithEffect() and wxWindow::HideWithEffect() if a
        non-default effect is used.

        If this method is not called, default, system-dependent timeout is
        used.

        @param showTimeout
            Timeout of the show effect, in milliseconds.
        @param hideTimeout
            Timeout of the hide effect, in milliseconds.

        @see SetEffects()
     */
    void SetEffectsTimeouts(unsigned showTimeout, unsigned hideTimeout);

    /**
        Set the same effect timeout to use for both showing and hiding the
        pages.

        This is the same as <code>SetEffectsTimeouts(timeout, timeout)</code>.

        @see SetEffect()
     */
    void SetEffectTimeout(unsigned timeout);

    /**
        Add a new page and show it immediately.

        This is simply a thin wrapper around the base class
        wxBookCtrlBase::AddPage() method using empty label (which is unused by
        this class anyhow) and selecting the new page immediately.
     */
    bool ShowNewPage(wxWindow* page);
};
