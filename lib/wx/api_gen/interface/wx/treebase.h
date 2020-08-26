/////////////////////////////////////////////////////////////////////////////
// Name:        treebase.h
// Purpose:     interface of wxTreeItemId
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxTreeItemId

    An opaque reference to a tree item.

    @library{wxcore}
    @category{data}

    @see wxTreeCtrl, wxTreeItemData, @ref overview_treectrl
*/
class wxTreeItemId
{
public:
    /**
        Default constructor. A wxTreeItemId is not meant to be constructed
        explicitly by the user; only those returned by the wxTreeCtrl functions
        should be used.
    */
    wxTreeItemId();

    /**
        Returns @true if this instance is referencing a valid tree item.
    */
    bool IsOk() const;

    void* GetID() const;
    void Unset();
};

bool operator==(const wxTreeItemId& left, const wxTreeItemId& right);
bool operator!=(const wxTreeItemId& left, const wxTreeItemId& right);



/**
    @class wxTreeItemData

    wxTreeItemData is some (arbitrary) user class associated with some item. The
    main advantage of having this class is that wxTreeItemData objects are
    destroyed automatically by the tree and, as this class has virtual
    destructor, it means that the memory and any other resources associated with
    a tree item will be automatically freed when it is deleted. Note that we
    don't use wxObject as the base class for wxTreeItemData because the size of
    this class is critical: in many applications, each tree leaf will have
    wxTreeItemData associated with it and the number of leaves may be quite big.

    Also please note that because the objects of this class are deleted by the
    tree using the operator @c delete, they must always be allocated on the heap
    using @c new.

    @library{wxcore}
    @category{containers}

    @see wxTreeCtrl
*/
class wxTreeItemData : public wxClientData
{
public:
    /**
        Default constructor.

        @beginWxPerlOnly
        In wxPerl the constructor accepts a scalar as an optional parameter
        and stores it as client data; use
        - GetData() to retrieve the value.
        - SetData(data) to set it.
        @endWxPerlOnly
    */
    wxTreeItemData();

    /**
        Virtual destructor.
    */
    virtual ~wxTreeItemData();

    /**
        Returns the item associated with this node.
    */
    const wxTreeItemId& GetId() const;

    /**
        Sets the item associated with this node.

        Notice that this function is automatically called by wxTreeCtrl methods
        associating an object of this class with a tree control item such as
        wxTreeCtrl::AppendItem(), wxTreeCtrl::InsertItem() and
        wxTreeCtrl::SetItemData() so there is usually no need to call it
        yourself.
    */
    void SetId(const wxTreeItemId& id);
};

/**
    Indicates which type to associate an image with a wxTreeCtrl item.

    @see wxTreeCtrl::GetItemImage(), wxTreeCtrl::SetItemImage()
*/
enum wxTreeItemIcon
{
    /**
        To get/set the item image for when the item is
        @b not selected and @b not expanded.
    */
    wxTreeItemIcon_Normal,
    /**
        To get/set the item image for when the item is
        @b selected and @b not expanded.
    */
    wxTreeItemIcon_Selected,
    /**
        To get/set the item image for when the item is
        @b not selected and @b expanded.
    */
    wxTreeItemIcon_Expanded,
    /**
        To get/set the item image for when the item is
        @b selected and @b expanded.
    */
    wxTreeItemIcon_SelectedExpanded,
    wxTreeItemIcon_Max
};


/// special values for the 'state' parameter of wxTreeCtrl::SetItemState()
static const int wxTREE_ITEMSTATE_NONE  = -1;   // not state (no display state image)
static const int wxTREE_ITEMSTATE_NEXT  = -2;   // cycle to the next state
static const int wxTREE_ITEMSTATE_PREV  = -3;   // cycle to the previous state

#define wxTR_NO_BUTTONS              0x0000     // for convenience
#define wxTR_HAS_BUTTONS             0x0001     // draw collapsed/expanded btns
#define wxTR_NO_LINES                0x0004     // don't draw lines at all
#define wxTR_LINES_AT_ROOT           0x0008     // connect top-level nodes
#define wxTR_TWIST_BUTTONS           0x0010     // still used by wxTreeListCtrl

#define wxTR_SINGLE                  0x0000     // for convenience
#define wxTR_MULTIPLE                0x0020     // can select multiple items

#define wxTR_HAS_VARIABLE_ROW_HEIGHT 0x0080     // what it says

#define wxTR_EDIT_LABELS             0x0200     // can edit item labels
#define wxTR_ROW_LINES               0x0400     // put border around items
#define wxTR_HIDE_ROOT               0x0800     // don't display root node

#define wxTR_FULL_ROW_HIGHLIGHT      0x2000     // highlight full horz space

// make the default control appearance look more native-like depending on the
// platform
#define wxTR_DEFAULT_STYLE       (wxTR_HAS_BUTTONS | wxTR_LINES_AT_ROOT)


// values for the `flags' parameter of wxTreeCtrl::HitTest() which determine
// where exactly the specified point is situated:

static const int wxTREE_HITTEST_ABOVE            = 0x0001;
static const int wxTREE_HITTEST_BELOW            = 0x0002;
static const int wxTREE_HITTEST_NOWHERE          = 0x0004;
    // on the button associated with an item.
static const int wxTREE_HITTEST_ONITEMBUTTON     = 0x0008;
    // on the bitmap associated with an item.
static const int wxTREE_HITTEST_ONITEMICON       = 0x0010;
    // on the indent associated with an item.
static const int wxTREE_HITTEST_ONITEMINDENT     = 0x0020;
    // on the label (string) associated with an item.
static const int wxTREE_HITTEST_ONITEMLABEL      = 0x0040;
    // on the right of the label associated with an item.
static const int wxTREE_HITTEST_ONITEMRIGHT      = 0x0080;
    // on the label (string) associated with an item.
static const int wxTREE_HITTEST_ONITEMSTATEICON  = 0x0100;
    // on the left of the wxTreeCtrl.
static const int wxTREE_HITTEST_TOLEFT           = 0x0200;
    // on the right of the wxTreeCtrl.
static const int wxTREE_HITTEST_TORIGHT          = 0x0400;
    // on the upper part (first half) of the item.
static const int wxTREE_HITTEST_ONITEMUPPERPART  = 0x0800;
    // on the lower part (second half) of the item.
static const int wxTREE_HITTEST_ONITEMLOWERPART  = 0x1000;

    // anywhere on the item
static const int wxTREE_HITTEST_ONITEM  = wxTREE_HITTEST_ONITEMICON |
                                          wxTREE_HITTEST_ONITEMLABEL;


