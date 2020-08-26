/////////////////////////////////////////////////////////////////////////////
// Name:        imaglist.h
// Purpose:     interface of wxImageList
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
   Flag values for Set/GetImageList
*/
enum
{
    wxIMAGE_LIST_NORMAL, // Normal icons
    wxIMAGE_LIST_SMALL,  // Small icons
    wxIMAGE_LIST_STATE   // State icons: unimplemented (see WIN32 documentation)
};

/**
   Flags for Draw
*/
#define wxIMAGELIST_DRAW_NORMAL         0x0001
#define wxIMAGELIST_DRAW_TRANSPARENT    0x0002
#define wxIMAGELIST_DRAW_SELECTED       0x0004
#define wxIMAGELIST_DRAW_FOCUSED        0x0008


/**
    @class wxImageList

    A wxImageList contains a list of images, which are stored in an unspecified
    form. Images can have masks for transparent drawing, and can be made from a
    variety of sources including bitmaps and icons.

    wxImageList is used principally in conjunction with wxTreeCtrl and
    wxListCtrl classes.

    @library{wxcore}
    @category{gdi}

    @see wxTreeCtrl, wxListCtrl
*/
class wxImageList : public wxObject
{
public:
    /**
        Default ctor.
    */
    wxImageList();

    /**
        Constructor specifying the image size, whether image masks should be created,
        and the initial size of the list.

        @param width
            Width of the images in the list.
        @param height
            Height of the images in the list.
        @param mask
            @true if masks should be created for all images.
        @param initialCount
            The initial size of the list.

        @see Create()
    */
    wxImageList(int width, int height, bool mask = true,
                int initialCount = 1);

    /**
        Adds a new image or images using a bitmap and optional mask bitmap.

        @param bitmap
            Bitmap representing the opaque areas of the image.
        @param mask
            Monochrome mask bitmap, representing the transparent areas of the image.

        @return The new zero-based image index.

        @remarks The original bitmap or icon is not affected by the Add()
                 operation, and can be deleted afterwards.
                 If the bitmap is wider than the images in the list, then the
                 bitmap will automatically be split into smaller images, each
                 matching the dimensions of the image list.
                 This does not apply when adding icons.
    */
    int Add(const wxBitmap& bitmap,
            const wxBitmap& mask = wxNullBitmap);

    /**
        Adds a new image or images using a bitmap and mask colour.

        @param bitmap
            Bitmap representing the opaque areas of the image.
        @param maskColour
            Colour indicating which parts of the image are transparent.

        @return The new zero-based image index.

        @remarks The original bitmap or icon is not affected by the Add()
                 operation, and can be deleted afterwards.
                 If the bitmap is wider than the images in the list, then the
                 bitmap will automatically be split into smaller images, each
                 matching the dimensions of the image list.
                 This does not apply when adding icons.
    */
    int Add(const wxBitmap& bitmap, const wxColour& maskColour);

    /**
        Adds a new image using an icon.

        @param icon
            Icon to use as the image.

        @return The new zero-based image index.

        @remarks The original bitmap or icon is not affected by the Add()
                 operation, and can be deleted afterwards.
                 If the bitmap is wider than the images in the list, then the
                 bitmap will automatically be split into smaller images, each
                 matching the dimensions of the image list.
                 This does not apply when adding icons.

        @onlyfor{wxmsw,wxosx}
    */
    int Add(const wxIcon& icon);

    /**
        Initializes the list. See wxImageList() for details.
    */
    bool Create(int width, int height, bool mask = true,
                int initialCount = 1);

    /**
        Draws a specified image onto a device context.

        @param index
            Image index, starting from zero.
        @param dc
            Device context to draw on.
        @param x
            X position on the device context.
        @param y
            Y position on the device context.
        @param flags
            How to draw the image. A bitlist of a selection of the following:
            - wxIMAGELIST_DRAW_NORMAL: Draw the image normally.
            - wxIMAGELIST_DRAW_TRANSPARENT: Draw the image with transparency.
            - wxIMAGELIST_DRAW_SELECTED: Draw the image in selected state.
            - wxIMAGELIST_DRAW_FOCUSED: Draw the image in a focused state.
        @param solidBackground
            For optimisation - drawing can be faster if the function is told
            that the background is solid.
    */
    virtual bool Draw(int index, wxDC& dc, int x, int y,
                      int flags = wxIMAGELIST_DRAW_NORMAL,
                      bool solidBackground = false);

    /**
        Returns the bitmap corresponding to the given index.
    */
    wxBitmap GetBitmap(int index) const;

    /**
        Returns the icon corresponding to the given index.
    */
    wxIcon GetIcon(int index) const;

    /**
        Returns the number of images in the list.
    */
    virtual int GetImageCount() const;

    /**
        Retrieves the size of the images in the list. Currently, the @a index
        parameter is ignored as all images in the list have the same size.

        @param index
            currently unused, should be 0
        @param width
            receives the width of the images in the list
        @param height
            receives the height of the images in the list

        @return @true if the function succeeded, @false if it failed
                (for example, if the image list was not yet initialized).
    */
    virtual bool GetSize(int index, int& width, int& height) const;

    /**
        Retrieves the size of the image list as passed to Create().

        @return the size of the image list, which may be zero if the image list
            was not yet initialised.
    */
    virtual wxSize GetSize() const;

    /**
        Removes the image at the given position.
    */
    bool Remove(int index);

    /**
        Removes all the images in the list.
    */
    bool RemoveAll();

    /**
        Replaces the existing image with the new image.
        Windows only.

        @param index
            The index of the bitmap to be replaced.
        @param bitmap
            Bitmap representing the opaque areas of the image.
        @param mask
            Monochrome mask bitmap, representing the transparent areas of the image.

        @return @true if the replacement was successful, @false otherwise.

        @remarks The original bitmap or icon is not affected by the Replace()
                 operation, and can be deleted afterwards.
    */
    bool Replace(int index, const wxBitmap& bitmap,
                 const wxBitmap& mask = wxNullBitmap);

    /**
        Replaces the existing image with the new image.

        @param index
            The index of the bitmap to be replaced.
        @param icon
            Icon to use as the image.

        @return @true if the replacement was successful, @false otherwise.

        @remarks The original bitmap or icon is not affected by the Replace()
                 operation, and can be deleted afterwards.

        @onlyfor{wxmsw,wxosx}
    */
    bool Replace(int index, const wxIcon& icon);
};

