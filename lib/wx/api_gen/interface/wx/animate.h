/////////////////////////////////////////////////////////////////////////////
// Name:        animate.h
// Purpose:     interface of wxAnimation* classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Supported animation types.
*/
enum wxAnimationType
{
    wxANIMATION_TYPE_INVALID,

    /** represents an animated GIF file. */
    wxANIMATION_TYPE_GIF,

    /** represents an ANI file. */
    wxANIMATION_TYPE_ANI,

    /** autodetect the filetype. */
    wxANIMATION_TYPE_ANY
};


#define wxAC_NO_AUTORESIZE       (0x0010)
#define wxAC_DEFAULT_STYLE       (wxBORDER_NONE)


/**
    @class wxAnimationCtrl

    This is a static control which displays an animation.
    wxAnimationCtrl API is as simple as possible and won't give you full control
    on the animation; if you need it then use wxMediaCtrl.

    This control is useful to display a (small) animation while doing a long task
    (e.g. a "throbber").

    It is only available if @c wxUSE_ANIMATIONCTRL is set to 1 (the default).

    For the platforms where this control has a native implementation, it may
    have only limited support for the animation types, see @c
    wxGenericAnimationCtrl if you need to support all of them.

    @beginStyleTable
    @style{wxAC_DEFAULT_STYLE}
           The default style: wxBORDER_NONE.
    @style{wxAC_NO_AUTORESIZE}
           By default, the control will adjust its size to exactly fit to the
           size of the animation when SetAnimation is called. If this style
           flag is given, the control will not change its size
    @endStyleTable

    @library{wxcore}
    @category{ctrl}

    @nativeimpl{wxgtk,wxmsw}

    @appearance{animationctrl}

    @see wxAnimation, @sample{animate}
*/
class wxAnimationCtrl : public wxControl
{
public:
    /**
        Initializes the object and calls Create() with
        all the parameters.
    */
    wxAnimationCtrl(wxWindow* parent, wxWindowID id,
                    const wxAnimation& anim = wxNullAnimation,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize,
                    long style = wxAC_DEFAULT_STYLE,
                    const wxString& name = wxAnimationCtrlNameStr);

    /**
        Creates the control with the given @a anim animation.

        After control creation you must explicitly call Play() to start to play
        the animation. Until that function won't be called, the first frame
        of the animation is displayed.

        @param parent
            Parent window, must be non-@NULL.
        @param id
            The identifier for the control.
        @param anim
            The initial animation shown in the control.
        @param pos
            Initial position.
        @param size
            Initial size.
        @param style
            The window style, see wxAC_* flags.
        @param name
            Control name.

        @return @true if the control was successfully created or @false if
                creation failed.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxAnimation& anim = wxNullAnimation,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxAC_DEFAULT_STYLE,
                const wxString& name = wxAnimationCtrlNameStr);

    /**
        Create a new animation object compatible with this control.

        A wxAnimation object created using this function is always compatible
        with controls of this type, see wxAnimation::IsCompatibleWith().

        @see CreateCompatibleAnimation()

        @since 3.1.4
     */
    wxAnimation CreateAnimation() const;

    /**
        Create a new animation object compatible with this control.

        This method does the same thing as CreateAnimation() but is static,
        i.e. can be called without creating any wxAnimationCtrl objects.

        @since 3.1.4
     */
    static wxAnimation CreateCompatibleAnimation();

    /**
        Returns the animation associated with this control.
    */
    virtual wxAnimation GetAnimation() const;

    /**
        Returns the inactive bitmap shown in this control when the;
        see SetInactiveBitmap() for more info.
    */
    wxBitmap GetInactiveBitmap() const;

    /**
        Returns @true if the animation is being played.
    */
    virtual bool IsPlaying() const;

    /**
        Loads the animation from the given file and calls SetAnimation().
        See wxAnimation::LoadFile for more info.
    */
    virtual bool LoadFile(const wxString& file,
                          wxAnimationType animType = wxANIMATION_TYPE_ANY);

    /**
        Loads the animation from the given stream and calls SetAnimation().
        See wxAnimation::Load() for more info.
    */
    virtual bool Load(wxInputStream& file,
                      wxAnimationType animType = wxANIMATION_TYPE_ANY);

    /**
        Starts playing the animation.

        The animation is always played in loop mode (unless the last frame of the
        animation has an infinite delay time) and always start from the first frame
        even if you @ref Stop "stopped" it while some other frame was displayed.
    */
    virtual bool Play();

    /**
        Sets the animation to play in this control.

        If the previous animation is being played, it's @ref Stop() stopped.
        Until Play() isn't called, a static image, the first frame of the given
        animation or the background colour will be shown
        (see SetInactiveBitmap() for more info).
    */
    virtual void SetAnimation(const wxAnimation& anim);

    /**
        Sets the bitmap to show on the control when it's not playing an animation.

        If you set as inactive bitmap ::wxNullBitmap (which is the default), then the
        first frame of the animation is instead shown when the control is inactive;
        in this case, if there's no valid animation associated with the control
        (see SetAnimation()), then the background colour of the window is shown.

        If the control is not playing the animation, the given bitmap will be
        immediately shown, otherwise it will be shown as soon as Stop() is called.

        Note that the inactive bitmap, if smaller than the control's size, will be
        centered in the control; if bigger, it will be stretched to fit it.
    */
    virtual void SetInactiveBitmap(const wxBitmap& bmp);

    /**
        Stops playing the animation.
        The control will show the first frame of the animation, a custom static image or
        the window's background colour as specified by the last SetInactiveBitmap() call.
    */
    virtual void Stop();
};


/**
    @class wxGenericAnimationCtrl

    Generic implementation of wxAnimationCtrl interface.

    If the platform supports a native animation control (currently just wxGTK)
    then this class implements the same interface internally in wxWidgets.
    One advantage of using this class instead of the native version is that
    this version of the control is capable of using animations in other formats
    than the ones supported by the @c gdk-pixbuf library used by wxGTK, which
    typically only supports @c wxANIMATION_TYPE_GIF.

    Note that to use this class you need to explicitly include the generic
    header after including the main one before using it, i.e.
    @code
    #include <wx/animate.h>
    #include <wx/generic/animate.h>
    @endcode

    @library{wxcore}
    @category{gdi}
*/

class wxGenericAnimationCtrl : public wxAnimationCtrl
{
public:
    /**
        Initializes the object and calls Create() with
        all the parameters.
    */
    wxGenericAnimationCtrl(wxWindow* parent, wxWindowID id,
                    const wxAnimation& anim = wxNullAnimation,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize,
                    long style = wxAC_DEFAULT_STYLE,
                    const wxString& name = wxAnimationCtrlNameStr);

    /**
        Creates the control with the given @a anim animation.

        After control creation you must explicitly call Play() to start to play
        the animation. Until that function won't be called, the first frame
        of the animation is displayed.

        @param parent
            Parent window, must be non-@NULL.
        @param id
            The identifier for the control.
        @param anim
            The initial animation shown in the control.
        @param pos
            Initial position.
        @param size
            Initial size.
        @param style
            The window style, see wxAC_* flags.
        @param name
            Control name.

        @return @true if the control was successfully created or @false if
                creation failed.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxAnimation& anim = wxNullAnimation,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxAC_DEFAULT_STYLE,
                const wxString& name = wxAnimationCtrlNameStr);

    /**
       Draw the current frame of the animation into given DC.
       This is fast as current frame is always cached.
    */
    void DrawCurrentFrame(wxDC& dc);

    /**
       Returns a wxBitmap with the current frame drawn in it.
    */
    wxBitmap& GetBackingStore();

    /**
       This overload of Play() lets you specify if the animation must loop or not
    */
    bool Play(bool looped);

    /**
       Specify whether the animation's background colour is to be shown (the default),
       or whether the window background should show through
    */
    void SetUseWindowBackgroundColour(bool useWinBackground = true);

    /**
       Returns @c true if the window's background colour is being used.
    */
    bool IsUsingWindowBackgroundColour() const;
};


/**
   @class wxAnimation

   The @c wxAnimation class handles the interface between the animation
   control and the details of the animation image or data.

    @stdobjects
    ::wxNullAnimation

    @see wxAnimationCtrl, @sample{animate}
*/
class wxAnimation : public wxObject
{
public:
    /**
       Constructs a new empty animation object.

       Call Load() to initialize it.

       @see wxAnimationCtrl::CreateAnimation()
     */
    wxAnimation();

    /**
       Constructs a new animation object and load the animation data from the
       given filename.

        @param name
            A filename.
        @param type
            One of the ::wxAnimationType values; wxANIMATION_TYPE_ANY
            means that the function should try to autodetect the filetype.

        @see wxAnimationCtrl::CreateAnimation()
     */
    wxAnimation(const wxString &name, wxAnimationType type = wxANIMATION_TYPE_ANY);

    /**
       Copy constructor.
    */
    wxAnimation(const wxAnimation& other);

    /**
        Returns @true if animation data is present.
    */
    bool IsOk() const;

    /**
        Returns @true if animation can be used with controls of the given type.

        This function checks if this animation object can be used with
        wxAnimationCtrl of particular type. This will be always the case for
        the platforms where only a single wxAnimationCtrl implementation is
        available, but not necessarily under e.g. wxGTK where both native (but
        limited) GTK implementation and generic implementation can be used.

        @since 3.1.4
     */
    bool IsCompatibleWith(wxClassInfo* ci) const;

    /**
        Returns the delay for the i-th frame in milliseconds.
        If @c -1 is returned the frame is to be displayed forever.
    */
    int GetDelay(unsigned int frame) const;

    /**
        Returns the number of frames for this animation.

        This method is not implemented in the native wxGTK implementation of
        this class and always returns 0 there.
    */
    unsigned int GetFrameCount() const;

    /**
        Returns the i-th frame as a wxImage.

        This method is not implemented in the native wxGTK implementation of
        this class and always returns an invalid image there.
    */
    wxImage GetFrame(unsigned int frame) const;

    /**
       Returns the size of the animation.
    */
    wxSize GetSize() const;

    /**
        Loads an animation from a file.

        @param name
            A filename.
        @param type
            One of the ::wxAnimationType values; wxANIMATION_TYPE_ANY
            means that the function should try to autodetect the filetype.

        @return @true if the operation succeeded, @false otherwise.
    */
    bool LoadFile(const wxString& name, wxAnimationType type = wxANIMATION_TYPE_ANY);

    /**
        Loads an animation from the given stream.

        @param stream
            The stream to use to load the animation.
            Under wxGTK may be any kind of stream; under other platforms
            this must be a seekable stream.
        @param type
            One of the ::wxAnimationType enumeration values.

        @return @true if the operation succeeded, @false otherwise.
    */
    bool Load(wxInputStream& stream, wxAnimationType type = wxANIMATION_TYPE_ANY);


    /**
       Returns the list of animation decoders used by the generic animation
       and @c wxGenericAnimationCtrl.
     */
    static inline wxAnimationDecoderList& GetHandlers();

    /**
       Add a new decoder to the list of animation decoders.
     */
    static void AddHandler(wxAnimationDecoder *handler);

    /**
       Insert a new decoder to the front of the list of animation decoders.
     */
    static void InsertHandler(wxAnimationDecoder *handler);

    /**
       Search for an animation decoder by type.
     */
    static const wxAnimationDecoder *FindHandler( wxAnimationType animType );

    /**
       Load the stock animation decoders (currently GIF and ANI) into the list
       of decoders. This is called automatically at program startup.
    */
    static void InitStandardHandlers();

    /**
       Clear out the animation decoder list. This is called automatically at
       program shutdown.
     */
    static void CleanUpHandlers();
};



// ============================================================================
// Global functions/macros
// ============================================================================

/**
    An empty animation object.
*/
wxAnimation wxNullAnimation;
