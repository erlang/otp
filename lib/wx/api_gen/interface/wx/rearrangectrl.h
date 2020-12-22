/////////////////////////////////////////////////////////////////////////////
// Name:        wx/rearrangectrl.h
// Purpose:     interface of wxRearrangeList
// Author:      Vadim Zeitlin
// Created:     2008-12-15
// Copyright:   (c) 2008 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxRearrangeList

    A listbox-like control allowing the user to rearrange the items and to
    enable or disable them.

    This class allows changing the order of the items shown in it as well as
    checking or unchecking them individually. The data structure used to allow
    this is the order array which contains the items indices indexed by their
    position with an added twist that the unchecked items are represented by
    the bitwise complement of the corresponding index (for any architecture
    using two's complement for negative numbers representation (i.e. just about
    any at all) this means that a checked item N is represented by -N-1 in
    unchecked state). In practice this means that you must apply the C bitwise
    complement operator when constructing the order array, e.g.
    @code
        wxArrayInt order;
        order.push_back(0); // checked item #0
        order.push_back(~1); // unchecked item #1
    @endcode

    So, for example, the array order [1 -3 0] used in conjunction with the
    items array ["first", "second", "third"] means that the items order is
    "second", "third", "first" and the "third" item is unchecked while the
    other two are checked.

    This convention is used both for the order argument of the control ctor or
    Create() and for the array returned from GetCurrentOrder().

    Usually this control will be used together with other controls allowing to
    move the items around in it interactively. The simplest possible solution
    is to use wxRearrangeCtrl which combines it with two standard buttons to
    move the current item up or down.

    Note that while most of the methods for items manipulation such as
    Append(), Insert() or Delete(), inherited from wxItemContainer work as
    expected for this class, Set() somewhat unexpectedly resets the order of
    the items as it clears the control first, also clearing the order as a side
    effect, before adding the new items.

    @since 2.9.0

    @library{wxcore}
    @category{ctrl}
    @genericAppearance{rearrangelist}
*/
class wxRearrangeList : public wxCheckListBox
{
public:
    /**
        Default constructor.

        Create() must be called later to effectively create the control.
     */
    wxRearrangeList();

    /**
        Constructor really creating the control.

        Please see Create() for the parameters description.
     */
    wxRearrangeList(wxWindow *parent,
                    wxWindowID id,
                    const wxPoint& pos,
                    const wxSize& size,
                    const wxArrayInt& order,
                    const wxArrayString& items,
                    long style = 0,
                    const wxValidator& validator = wxDefaultValidator,
                    const wxString& name = wxRearrangeListNameStr);

    /**
        Effectively creates the window for an object created using the default
        constructor.

        This function is very similar to wxCheckListBox::Create() except that
        it has an additional parameter specifying the initial order of the
        items. Please see the class documentation for the explanation of the
        conventions used by the @a order argument.

        @param parent
            The parent window, must be non-@NULL.
        @param id
            The window identifier.
        @param pos
            The initial window position.
        @param size
            The initial window size.
        @param order
            Array specifying the initial order of the items in @a items array.
        @param items
            The items to display in the list.
        @param style
            The control style, there are no special styles for this class but
            the base class styles can be used here.
        @param validator
            Optional window validator.
        @param name
            Optional window name.
     */
    bool Create(wxWindow *parent,
                wxWindowID id,
                const wxPoint& pos,
                const wxSize& size,
                const wxArrayInt& order,
                const wxArrayString& items,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxRearrangeListNameStr);


    /**
        Return the current order of the items.

        The order may be different from the one passed to the constructor if
        MoveCurrentUp() or MoveCurrentDown() were called.
     */
    const wxArrayInt& GetCurrentOrder() const;

    /**
        Return @true if the currently selected item can be moved up.

        This function is useful for EVT_UPDATE_UI handler for the standard "Up"
        button often used together with this control and wxRearrangeCtrl uses
        it in this way.

        @return
            @true if the currently selected item can be moved up in the
            listbox, @false if there is no selection or the current item is the
            first one.

        @see CanMoveCurrentDown()
     */
    bool CanMoveCurrentUp() const;

    /**
        Return @true if the currently selected item can be moved down.

        @see CanMoveCurrentUp()
     */
    bool CanMoveCurrentDown() const;

    /**
        Move the currently selected item one position above.

        This method is useful to implement the standard "Up" button behaviour
        and wxRearrangeCtrl uses it for this.

        @return
            @true if the item was moved or @false if this couldn't be done.

        @see MoveCurrentDown()
     */
    bool MoveCurrentUp();

    /**
        Move the currently selected item one position below.

        @see MoveCurrentUp()
     */
    bool MoveCurrentDown();
};


/**
    @class wxRearrangeCtrl

    A composite control containing a wxRearrangeList and the buttons allowing
    to move the items in it.

    This control is in fact a panel containing the wxRearrangeList control and
    the "Up" and "Down" buttons to move the currently selected item up or down.
    It is used as the main part of a wxRearrangeDialog.

    @since 2.9.0

    @library{wxcore}
    @category{ctrl}
    @genericAppearance{rearrangectrl}
 */
class wxRearrangeCtrl : public wxPanel
{
public:
    /**
        Default constructor.

        Create() must be called later to effectively create the control.
     */
    wxRearrangeCtrl();

    /**
        Constructor really creating the control.

        Please see Create() for the parameters description.
     */
    wxRearrangeCtrl(wxWindow *parent,
                    wxWindowID id,
                    const wxPoint& pos,
                    const wxSize& size,
                    const wxArrayInt& order,
                    const wxArrayString& items,
                    long style = 0,
                    const wxValidator& validator = wxDefaultValidator,
                    const wxString& name = wxRearrangeListNameStr);

    /**
        Effectively creates the window for an object created using the default
        constructor.

        The parameters of this method are the same as for
        wxRearrangeList::Create().
     */
    bool Create(wxWindow *parent,
                wxWindowID id,
                const wxPoint& pos,
                const wxSize& size,
                const wxArrayInt& order,
                const wxArrayString& items,
                long style = 0,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxRearrangeListNameStr);

    /**
        Return the listbox which is the main part of this control.
     */
    wxRearrangeList *GetList() const;
};

/**
    @class wxRearrangeDialog

    A dialog allowing the user to rearrange the specified items.

    This dialog can be used to allow the user to modify the order of the items
    and to enable or disable them individually. For example:
    @code
        wxArrayString items;
        items.push_back("meat");
        items.push_back("fish");
        items.push_back("fruits");
        items.push_back("beer");
        wxArrayInt order;
        order.push_back(3);
        order.push_back(0);
        order.push_back(1);
        order.push_back(2);

        wxRearrangeDialog dlg(NULL,
                              "You can also uncheck the items you don't like "
                              "at all.",
                              "Sort the items in order of preference",
                              order, items);
        if ( dlg.ShowModal() == wxID_OK ) {
            order = dlg.GetOrder();
            for ( size_t n = 0; n < order.size(); n++ ) {
                if ( order[n] >= 0 ) {
                    wxLogMessage("Your most preferred item is \"%s\"",
                                 items[order[n]]);
                    break;
                }
            }
        }
    @endcode

    @since 2.9.0

    @library{wxcore}
    @category{cmndlg}
 */
class wxRearrangeDialog : public wxDialog
{
public:
    /**
        Default constructor.

        Create() must be called later to effectively create the control.
     */
    wxRearrangeDialog();

    /**
        Constructor creating the dialog.

        Please see Create() for the parameters description.
     */
    wxRearrangeDialog(wxWindow *parent,
                      const wxString& message,
                      const wxString& title,
                      const wxArrayInt& order,
                      const wxArrayString& items,
                      const wxPoint& pos = wxDefaultPosition,
                      const wxString& name = wxRearrangeDialogNameStr);

    /**
        Effectively creates the dialog for an object created using the default
        constructor.

        @param parent
            The dialog parent, possibly @NULL.
        @param message
            The message shown inside the dialog itself, above the items list.
        @param title
            The title of the dialog.
        @param order
            The initial order of the items in the convention used by
            wxRearrangeList.
        @param items
            The items to show in the dialog.
        @param pos
            Optional dialog position.
        @param name
            Optional dialog name.
        @return
            @true if the dialog was successfully created or @false if creation
            failed.
     */
    bool Create(wxWindow *parent,
                const wxString& message,
                const wxString& title,
                const wxArrayInt& order,
                const wxArrayString& items,
                const wxPoint& pos = wxDefaultPosition,
                const wxString& name = wxRearrangeDialogNameStr);

    /**
        Customize the dialog by adding extra controls to it.

        This function adds the given @a win to the dialog, putting it just
        below the part occupied by wxRearrangeCtrl. It must be called after
        creating the dialog and you will typically need to process the events
        generated by the extra controls for them to do something useful.

        For example:
        @code
            class MyRearrangeDialog : public wxRearrangeDialog
            {
            public:
                MyRearrangeDialog(wxWindow *parent, ...)
                    : wxRearrangeDialog(parent, ...)
                {
                    wxPanel *panel = new wxPanel(this);
                    wxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
                    sizer->Add(new wxStaticText(panel, wxID_ANY,
                                                "Column width in pixels:"));
                    sizer->Add(new wxTextCtrl(panel, wxID_ANY, ""));
                    panel->SetSizer(sizer);
                    AddExtraControls(panel);
                }

                ... code to update the text control with the currently selected
                    item width and to react to its changes omitted ...
            };
        @endcode

        See also the complete example of a custom rearrange dialog in the
        dialogs sample.

        @param win
            The window containing the extra controls. It must have this dialog
            as its parent.
     */
    void AddExtraControls(wxWindow *win);

    /**
        Return the list control used by the dialog.

        @see wxRearrangeCtrl::GetList()
     */
    wxRearrangeList *GetList() const;

    /**
        Return the array describing the order of items after it was modified by
        the user.

        Please notice that the array will contain negative items if any items
        were unchecked. See wxRearrangeList for more information about the
        convention used for this array.
     */
    wxArrayInt GetOrder() const;
};
