/////////////////////////////////////////////////////////////////////////////
// Name:        statbmp.h
// Purpose:     interface of wxStaticBitmap
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxStaticBitmap

    A static bitmap control displays a bitmap. Native implementations on some
    platforms are only meant for display of the small icons in the dialog
    boxes.

    If you want to display larger images portably, you may use generic
    implementation wxGenericStaticBitmap declared in \<wx/generic/statbmpg.h\>.

    Notice that for the best results, the size of the control should be the
    same as the size of the image displayed in it, as happens by default if
    it's not resized explicitly. Otherwise, behaviour depends on the
    platform: under MSW, the bitmap is drawn centred inside the control, while
    elsewhere it is drawn at the origin of the control. You can use
    SetScaleMode() to control how the image is scaled inside the control.

    @library{wxcore}
    @category{ctrl}
    @appearance{staticbitmap}

    @see wxBitmap
*/
class wxStaticBitmap : public wxControl
{
public:
    /**
        Specify how the bitmap should be scaled in the control.

        @see SetScaleMode(), GetScaleMode()
    */
    enum ScaleMode
    {
        /**
            The bitmap is displayed in original size. Portions larger then the
            control will be cut off.
        */
        Scale_None,

        /**
            Scale the bitmap to fit the size of the control by changing the
            aspect ratio of the bitmap if necessary.
        */
        Scale_Fill,

        /**
            Scale the bitmap to fit the size of the control by maintaining the
            aspect ratio. Any remaining area of the control will use the background.
        */
        Scale_AspectFit,

        /**
            Scale the bitmap to fill the size of the control. Some portion of
            the bitmap may be clipped to fill the control.
        */
        Scale_AspectFill
    };

    /**
      Default constructor
    */
    wxStaticBitmap();

    /**
        Constructor, creating and showing a static bitmap control.

        @param parent
            Parent window. Should not be @NULL.
        @param id
            Control identifier. A value of -1 denotes a default value.
        @param label
            Bitmap label.
        @param pos
            Window position.
        @param size
            Window size.
        @param style
            Window style. See wxStaticBitmap.
        @param name
            Window name.

        @see Create()
    */
    wxStaticBitmap(wxWindow* parent, wxWindowID id,
                   const wxBitmap& label,
                   const wxPoint& pos = wxDefaultPosition,
                   const wxSize& size = wxDefaultSize,
                   long style = 0,
                   const wxString& name = wxStaticBitmapNameStr);

    /**
        Creation function, for two-step construction. For details see wxStaticBitmap().
    */
    bool Create(wxWindow* parent, wxWindowID id, const wxBitmap& label,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = 0,
                const wxString& name = wxStaticBitmapNameStr);

    /**
        Returns the bitmap currently used in the control.
        Notice that this method can be called even if SetIcon() had been used.

        @see SetBitmap()
    */
    virtual wxBitmap GetBitmap() const;

    /**
        Returns the icon currently used in the control.
        Notice that this method can only be called if SetIcon() had been used: an icon
        can't be retrieved from the control if a bitmap had been set
        (using wxStaticBitmap::SetBitmap).

        @see SetIcon()
    */
    virtual wxIcon GetIcon() const;

    /**
        Sets the bitmap label.

        @param label
            The new bitmap.

        @see GetBitmap()
    */
    virtual void SetBitmap(const wxBitmap& label);

    /**
        Sets the label to the given icon.

        @param label
            The new icon.
    */
    virtual void SetIcon(const wxIcon& label);

    /**
        Sets the scale mode.

        @param scaleMode
            Controls how the bitmap is scaled inside the control.

        @note Currently only the generic implementation supports all scaling modes.
            You may use generic implementation wxGenericStaticBitmap declared in
            \<wx/generic/statbmpg.h\> in all ports.

        @see GetScaleMode()

        @since 3.1.0
    */
    virtual void SetScaleMode(ScaleMode scaleMode);

    /**
        Returns the scale mode currently used in the control.

        @see SetScaleMode()

        @since 3.1.0
    */
    virtual ScaleMode GetScaleMode() const;

};

