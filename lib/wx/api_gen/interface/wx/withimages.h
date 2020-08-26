///////////////////////////////////////////////////////////////////////////////
// Name:        withimages.h
// Purpose:     Interface of  wxWithImages class.
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
   A mixin class to be used with other classes that use a wxImageList.
*/
class wxWithImages
{
public:
    enum
    {
        NO_IMAGE = -1
    };

    wxWithImages();
    virtual ~wxWithImages();

    /**
        Sets the image list for the page control and takes ownership of the list.

        @see wxImageList, SetImageList()
    */
    void AssignImageList(wxImageList* imageList);

    /**
       Sets the image list to use. It does not take ownership of the image
       list, you must delete it yourself.

       @see wxImageList, AssignImageList()
    */
    virtual void SetImageList(wxImageList* imageList);

    /**
        Returns the associated image list, may be NULL.

        @see wxImageList, SetImageList()
    */
    wxImageList* GetImageList() const;

protected:
    /**
       Return true if we have a valid image list.
    */
    bool HasImageList() const;

    /**
       Return the image with the given index from the image list.

       If there is no image list or if index == NO_IMAGE, silently returns
       wxNullIcon.
    */
    wxIcon GetImage(int iconIndex) const;
};
