/////////////////////////////////////////////////////////////////////////////
// Name:        wx/addremovectrl.h
// Purpose:     documentation of wxAddRemoveCtrl
// Author:      Vadim Zeitlin
// Created:     2015-02-04
// Copyright:   (c) 2015 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxAddRemoveAdaptor

    Object used to mediate between wxAddRemoveCtrl and the control showing the
    list of items which can be added or removed.

    This is a base class from which custom classes used with
    wxAddRemoveCtrl::SetAdaptor() must be derived. Object of this class are
    typically associated with the control showing the list of items on
    creation, i.e. the derived class constructor would normally take a pointer
    to the control which will be returned from GetItemsCtrl() later.

    @since 3.1.0

    @library{wxcore}
 */
class wxAddRemoveAdaptor
{
public:
    /// Default and trivial constructor.
    wxAddRemoveAdaptor();

    /// Trivial but virtual destructor.
    virtual ~wxAddRemoveAdaptor();

    /**
        Override to return the associated control.

        The control must be a child of the associated wxAddRemoveCtrl.
     */
    virtual wxWindow* GetItemsCtrl() const = 0;

    /**
        Override to return whether a new item can be added to the control.

        A typical implementation would simply always return @true, but it is
        also possible to return @false if the list is "full" and can't contain
        any more elements.
     */
    virtual bool CanAdd() const = 0;

    /**
        Override to return whether the currently selected item (if any) can be
        removed from the control.

        The implementation should check if there is a currently selected item
        and possibly if the user is allowed to remove this item.
     */
    virtual bool CanRemove() const = 0;

    /**
        Called when an item should be added.

        A typical implementation would either add a new item to the list
        control and start editing it in place or ask the user for the item to
        add first and then add it to the control returned by GetItemsCtrl().

        Notice that this method can only be called if CanAdd() currently
        returns @true.
     */
    virtual void OnAdd() = 0;

    /**
        Called when the current item should be removed.

        The implementation should remove the currently selected item from the
        control and update the selection.

        Notice that this method can only be called if CanRemove() currently
        returns @true.
     */
    virtual void OnRemove() = 0;
};


/**
    @class wxAddRemoveCtrl

    A class adding buttons to add and remove items to a list-like child
    control.

    This class represents a composite control which combines any control
    capable of showing multiple items, such as wxListBox, wxListCtrl,
    wxTreeCtrl, wxDataViewCtrl or a custom control, with two buttons allowing
    to add items and remove items from this list-like control. The advantage of
    using this control instead of just creating and managing the buttons
    directly is that the correct buttons and layout for the current platform
    are used by this class. E.g. the buttons are positioned under the list
    control under macOS and GTK+ but to its right under MSW and the buttons
    themselves use system-specific bitmaps under macOS.

    This class is always used in conjunction with wxAddRemoveAdaptor which is
    used to actually add items to or remove them from the control containing
    the items when the corresponding button is pressed. The
    @ref page_samples_dialogs "dialogs sample" shows how to do it: first you
    need to derive a new class from wxAddRemoveAdaptor and implement its pure
    virtual methods and then you must call SetAdaptor() with a newly allocated
    object of this class. You also must create the control containing the items
    with wxAddRemoveCtrl as parent. Here are the different steps in pseudocode:
    @code
        wxAddRemoveCtrl* ctrl = new wxAddRemoveCtrl(parent);

        // This can be any kind of control for which OnAdd() and OnRemove()
        // below can be made to work.
        wxListBox* lbox = new wxListBox(ctrl, ...);

        class ListBoxAdaptor : public wxAddRemoveAdaptor
        {
        public:
            explicit ListBoxAdaptor(wxListBox* lbox) : m_lbox(lbox) { }

            virtual wxWindow* GetItemsCtrl() const { return m_lbox; }

            virtual bool CanAdd() const { return true; }
            virtual bool CanRemove() const { return m_lbox->GetSelection() != wxNOT_FOUND; }
            virtual void OnAdd() { ... get the new item from user and add it ... }
            virtual void OnRemove() { m_lbox->Delete(m_lbox->GetSelection()); }

        private:
            wxListBox* m_lbox;
        };

        ctrl->SetAdaptor(new ListBoxAdaptor(lbox));
    @endcode

    @since 3.1.0

    @library{wxcore}
    @category{ctrl}
 */
class wxAddRemoveCtrl : public wxPanel
{
public:
    /**
        Default constructor.

        Use Create() later.
     */
    wxAddRemoveCtrl();

    /**
        Constructor really creating the control window.

        SetAdaptor() still must be called later to finish initializing the
        control.

        Parameters have the same meaning as in wxPanel::Create().
     */
    wxAddRemoveCtrl(wxWindow* parent,
                    wxWindowID winid = wxID_ANY,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize,
                    long style = 0,
                    const wxString& name = wxAddRemoveCtrlNameStr);

    /**
        Create the control window after using the default constructor.

        Parameters have the same meaning as in wxPanel::Create().
     */
    bool Create(wxWindow* parent,
                wxWindowID winid = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxString& name = wxAddRemoveCtrlNameStr);

    /**
        Associate the control with the specified adaptor object.

        This method must be called exactly once to finish initializing this
        object.

        The adapter object must correspond to a control created as a child of
        this window, i.e. wxAddRemoveAdaptor::GetItemsCtrl() must return a
        pointer to an existing child of this control.

        The @a adaptor pointer must be non-NULL and heap-allocated as the
        control takes ownership of it and will delete it later.
     */
    void SetAdaptor(wxAddRemoveAdaptor* adaptor);

    /**
        Sets the tooltips used for the add and remove buttons.

        Show the specified tooltips when the mouse hovers over the buttons used
        to add and remove items, respectively.

        This method can only be used after calling SetAdaptor().
     */
    void SetButtonsToolTips(const wxString& addtip, const wxString& removetip);
};
