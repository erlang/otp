///////////////////////////////////////////////////////////////////////////////
// Name:        ribbon/gallery.h
// Purpose:     interface of wxRibbonGallery
// Author:      Peter Cawley
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

enum wxRibbonGalleryButtonState
{
    wxRIBBON_GALLERY_BUTTON_NORMAL,
    wxRIBBON_GALLERY_BUTTON_HOVERED,
    wxRIBBON_GALLERY_BUTTON_ACTIVE,
    wxRIBBON_GALLERY_BUTTON_DISABLED,
};

/**
    @class wxRibbonGallery

    A ribbon gallery is like a wxListBox, but for bitmaps rather than strings.
    It displays a collection of bitmaps arranged in a grid and allows the user
    to choose one. As there are typically more bitmaps in a gallery than can
    be displayed in the space used for a ribbon, a gallery always has scroll
    buttons to allow the user to navigate through the entire gallery. It also
    has an "extension" button, the behaviour of which is outside the scope of
    the gallery control itself, though it typically displays some kind of
    dialog related to the gallery.

    @beginEventEmissionTable{wxRibbonGalleryEvent}
    @event{EVT_RIBBONGALLERY_SELECTED(id, func)}
        Triggered when the user selects an item from the gallery. Note that the
        ID is that of the gallery, not of the item.
    @event{EVT_RIBBONGALLERY_CLICKED(id, func)}
        Similar to EVT_RIBBONGALLERY_SELECTED but triggered every time a
        gallery item is clicked, even if it is already selected. Note that the
        ID of the event is that of the gallery, not of the item, just as above.
        This event is available since wxWidgets 2.9.2.
    @event{EVT_RIBBONGALLERY_HOVER_CHANGED(id, func)}
        Triggered when the item being hovered over by the user changes. The
        item in the event will be the new item being hovered, or NULL if there
        is no longer an item being hovered. Note that the ID is that of the
        gallery, not of the item.
    @endEventTable
    @beginEventEmissionTable{wxCommandEvent}
    @event{EVT_BUTTON(id, func)}
        Triggered when the "extension" button of the gallery is pressed.
    @endEventTable

    @library{wxribbon}
    @category{ribbon}
*/
class wxRibbonGallery : public wxRibbonControl
{
public:
    /**
        Default constructor.
        With this constructor, Create() should be called in order to create
        the gallery.
    */
    wxRibbonGallery();

    /**
        Construct a ribbon gallery with the given parameters.
        @param parent
            Parent window for the gallery (typically a wxRibbonPanel).
        @param id
            An identifier for the gallery. @c wxID_ANY is taken to mean a default.
        @param pos
            Initial position of the gallery.
        @param size
            Initial size of the gallery.
        @param style
            Gallery style, currently unused.
    */
    wxRibbonGallery(wxWindow* parent,
                  wxWindowID id = wxID_ANY,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = 0);

    /**
        Destructor.
    */
    virtual ~wxRibbonGallery();

    /**
        Create a gallery in two-step gallery construction.
        Should only be called when the default constructor is used, and
        arguments have the same meaning as in the full constructor.
    */
    bool Create(wxWindow* parent,
                wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0);

    /**
        Remove all items from the gallery.
    */
    void Clear();

    /**
        Query if the gallery has no items in it.
    */
    bool IsEmpty() const;

    /**
        Get the number of items in the gallery.
    */
    unsigned int GetCount() const;

    /**
        Get an item by index.
    */
    wxRibbonGalleryItem* GetItem(unsigned int n);

    /**
        Add an item to the gallery (with no client data).
        @param bitmap
            The bitmap to display for the item. Note that all items must
            have equally sized bitmaps.
        @param id
            ID number to associate with the item. Not currently used for
            anything important.
    */
    wxRibbonGalleryItem* Append(const wxBitmap& bitmap, int id);

    /**
        Add an item to the gallery (with simple client data).
        @param bitmap
            The bitmap to display for the item. Note that all items must
            have equally sized bitmaps.
        @param id
            ID number to associate with the item. Not currently used for
            anything important.
        @param clientData
            An opaque pointer to associate with the item.
    */
    wxRibbonGalleryItem* Append(const wxBitmap& bitmap, int id, void* clientData);

    /**
        Add an item to the gallery (with complex client data)
        @param bitmap
            The bitmap to display for the item. Note that all items must
            have equally sized bitmaps.
        @param id
            ID number to associate with the item. Not currently used for
            anything important.
        @param clientData
            An object which contains data to associate with the item. The item
            takes ownership of this pointer, and will delete it when the item
            client data is changed to something else, or when the item is
            destroyed.
    */
    wxRibbonGalleryItem* Append(const wxBitmap& bitmap, int id, wxClientData* clientData);

    /**
        Set the client object associated with a gallery item.
    */
    void SetItemClientObject(wxRibbonGalleryItem* item, wxClientData* data);

    /**
        Get the client object associated with a gallery item.
    */
    wxClientData* GetItemClientObject(const wxRibbonGalleryItem* item) const;

    /**
        Set the client data associated with a gallery item.
    */
    void SetItemClientData(wxRibbonGalleryItem* item, void* data);

    /**
        Get the client data associated with a gallery item.
    */
    void* GetItemClientData(const wxRibbonGalleryItem* item) const;

    /**
        Set the selection to the given item, or removes the selection if
        @a item == NULL.

        Note that this not cause any events to be emitted.
    */
    void SetSelection(wxRibbonGalleryItem* item);

    /**
        Get the currently selected item, or NULL if there is none.

        The selected item is set by SetSelection(), or by the user clicking on
        an item.
    */
    wxRibbonGalleryItem* GetSelection() const;

    /**
        Get the currently hovered item, or NULL if there is none.

        The hovered item is the item underneath the mouse cursor.
    */
    wxRibbonGalleryItem* GetHoveredItem() const;

    /**
        Get the currently active item, or NULL if there is none.

        The active item is the item being pressed by the user, and will thus
        become the selected item if the user releases the mouse button.
    */
    wxRibbonGalleryItem* GetActiveItem() const;

    /**
        Get the state of the scroll up button.
    */
    wxRibbonGalleryButtonState GetUpButtonState() const;

    /**
        Get the state of the scroll down button.
    */
    wxRibbonGalleryButtonState GetDownButtonState() const;

    /**
        Get the state of the "extension" button.
    */
    wxRibbonGalleryButtonState GetExtensionButtonState() const;

    /**
        Query is the mouse is currently hovered over the gallery.

        @return @true if the cursor is within the bounds of the gallery (not
            just hovering over an item), @false otherwise.
    */
    bool IsHovered() const;

    /**
        Scroll the gallery contents by some amount.

        @param lines
          Positive values scroll toward the end of the gallery, while negative
          values scroll toward the start.

        @return @true if the gallery scrolled at least one pixel in the given
            direction, @false if it did not scroll.
    */
    virtual bool ScrollLines(int lines);

    /**
        Scroll the gallery contents by some fine-grained amount.

        @param pixels
          Positive values scroll toward the end of the gallery, while negative
          values scroll toward the start.

        @return @true if the gallery scrolled at least one pixel in the given
            direction, @false if it did not scroll.
    */
    bool ScrollPixels(int pixels);

    /**
        Scroll the gallery to ensure that the given item is visible.
    */
    void EnsureVisible(const wxRibbonGalleryItem* item);
};

/**
    @class wxRibbonGalleryEvent

    @library{wxribbon}
    @category{events,ribbon}

    @see wxRibbonBar
*/
class wxRibbonGalleryEvent : public wxCommandEvent
{
public:
    /**
        Constructor.
    */
    wxRibbonGalleryEvent(wxEventType command_type = wxEVT_NULL,
                         int win_id = 0,
                         wxRibbonGallery* gallery = NULL,
                         wxRibbonGalleryItem* item = NULL);

    /**
        Returns the gallery which the event relates to.
    */
    wxRibbonGallery* GetGallery();

    /**
        Returns the gallery item which the event relates to, or NULL if it does
        not relate to an item.
    */
    wxRibbonGalleryItem* GetGalleryItem();

    /**
        Sets the gallery relating to this event.
    */
    void SetGallery(wxRibbonGallery* gallery);

    /**
        Sets the gallery item relating to this event.
    */
    void SetGalleryItem(wxRibbonGalleryItem* item);
};


wxEventType wxEVT_RIBBONGALLERY_HOVER_CHANGED;
wxEventType wxEVT_RIBBONGALLERY_SELECTED;
wxEventType wxEVT_RIBBONGALLERY_CLICKED;

