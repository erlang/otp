/////////////////////////////////////////////////////////////////////////////
// Name:        glcanvas.h
// Purpose:     interface of wxGLContext and wxGLCanvas
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxGLAttribsBase

    This is the base class for wxGLAttributes and wxGLContextAttrs.
    It stores internally the list required by OS and OpenGL driver for setting
    display and rendering context attributes.

    Normally this class is not used directly. But there's a case where it its
    member functions are useful: setting attributes not handled by wxWidgets.
    Suppose the OpenGL Board sets a new functionality of the context, by adding
    a new attribute (let's call it NEW_CTX_F) and also a new type of context by
    allowing a new bit value (let's call it NEW_BITS) for the CONTEXT_PROFILE_MASK_ARB
    value. These new values can be added to the list using code like this:

    @code
    wxGLContextAttrs cxtAttrs;
    // Some values
    cxtAttrs.CoreProfile().OGLVersion(5, 0); // OGL 5.0, whenever available
    cxtAttrs.PlatformDefaults();
    // Values usually are platform-dependant named (even value assigned!)
    #if defined(__WXMSW__)
        cxtAttrs.AddAttribute(WGL_NEW_CTX_F);
        cxtAttrs.AddAttribBits(WGL_CONTEXT_PROFILE_MASK_ARB, WGL_NEW_BITS);
    #elif defined(__WXX11__)
        cxtAttrs.AddAttribute(GLX_NEW_CTX_F);
        cxtAttrs.AddAttribBits(GLX_CONTEXT_PROFILE_MASK_ARB, GLX_NEW_BITS);
    #else
        // Other platforms
    #endif
    cxtAttrs.EndList(); // Don't forget this
    cxtAttrs.SetNeedsARB(true); // Context attributes are set by an ARB-function
    @endcode

    @since 3.1.0

    @library{wxgl}
    @category{gl}

    @see wxGLCanvas, wxGLContext, wxGLContextAttrs, wxGLAttributes
*/
class WXDLLIMPEXP_GL wxGLAttribsBase
{
public:
    /// Constructor.
    wxGLAttribsBase();

    /**
        Adds an integer value to the list of attributes.

        @param attribute
        The value to add.
    */
    void AddAttribute(int attribute);

    /**
        Combine (bitwise OR) a given value with the existing one, if any.
        This function first searches for an identifier and then combines the
        given value with the value right after the identifier.
        If the identifier is not found, two new values (i.e. the identifier and
        the given value) are added to the list.

        @param searchVal
        The identifier to search for.
        @param combineVal
        The value to combine with the existing one.
    */
    void AddAttribBits(int searchVal, int combineVal);

    /**
        Sets the necessity of using special ARB-functions
        (e.g. wglCreateContextAttribsARB in MSW) for some of the attributes of
        the list.
        Multi-sampling and modern context require these ARB-functions.

        @param needsARB
        @true  if an ARB-function is needed for any of the attributes.

        @see NeedsARB
    */
    void SetNeedsARB(bool needsARB = true);

    /// Delete contents and sets ARB-flag to false.
    void Reset();

    /**
        Returns a pointer to the internal list of attributes. It's very unlikely
        you need this function.
        If the list is empty the returned value is @NULL.
    */
    const int* GetGLAttrs() const;

    /** Returns the size of the internal list of attributes.
        Remember that the last value in the list must be a '0' (zero). So, a
        minimal list is of size = 2, the first value is meaningful and the last is '0'.
    */
    int GetSize();

    /**
        Returns the current value of the ARB-flag.

        @see SetNeedsARB
    */
    bool NeedsARB() const;
};

/**
    @class wxGLAttributes

    This class is used for setting display attributes when drawing through
    OpenGL ("Pixel format" in MSW and OSX parlance, "Configs" in X11).
    While framebuffer depth and samplers are still common, attributes
    like layers overlay or not using true colour are rarely used nowadays.

    Attributes can be chained. For example:
    @code
    wxGLAttributes dispAttrs;
    dispAttrs.PlatformDefaults().MinRGBA(8, 8, 8, 8).DoubleBuffer().Depth(32).EndList();
    @endcode

    Notice that EndList() must be used as the last attribute. Not adding it will
    likely result in nothing displayed at all.

    @note Not all of platform-dependant available attributes are implemented in
          wxWidgets. You can set other attributes by using AddAttribute() and
          AddAttribBits() functions inherited from the base wxGLAttribsBase class.
          While WGL_/GLX_/NS attributes can be added, PFD_ (for MSW) can not.

    @since 3.1.0

    @library{wxgl}
    @category{gl}

    @see wxGLCanvas, wxGLContext, wxGLAttribsBase, wxGLContextAttrs
*/
class wxGLAttributes : public wxGLAttribsBase
{
public:
    /**
        Use true colour instead of colour index rendering for each pixel.
        It makes no effect for macOS.
    */
    wxGLAttributes& RGBA();

    /**
        Specifies the number of bits for colour buffer. For RGBA it's
        normally the sum of the bits per each component.

        @param val
        The number of bits.
    */
    wxGLAttributes& BufferSize(int val);

    /**
        Specifies the framebuffer level. It makes no effect for macOS.

        @param val
        0 for main buffer, >0 for overlay, <0 for underlay.
    */
    wxGLAttributes& Level(int val);

    /**
        Requests using double buffering.
    */
    wxGLAttributes& DoubleBuffer();

    /**
        Use stereoscopic display.
    */
    wxGLAttributes& Stereo();

    /**
        Specifies the number of auxiliary buffers.

        @param val
        The number of auxiliary buffers.
    */
    wxGLAttributes& AuxBuffers(int val);

    /**
        Specifies the minimal number of bits for each colour and alpha.
        On MSW and OSX this function also sets the size of the colour buffer.

        @param mRed
        The minimal number of bits for colour red.
        @param mGreen
        The minimal number of bits for colour green.
        @param mBlue
        The minimal number of bits for colour blue.
        @param mAlpha
        The minimal number of bits for alpha channel.
    */
    wxGLAttributes& MinRGBA(int mRed, int mGreen, int mBlue, int mAlpha);

    /**
        Specifies number of bits for Z-buffer.

        @param val
        Number of bits for Z-buffer (typically 0, 16 or 32).
    */
    wxGLAttributes& Depth(int val);

    /**
        Specifies number of bits for stencil buffer.

        @param val
        Number of bits.
    */
    wxGLAttributes& Stencil(int val);

    /**
        Specifies the minimal number of bits for each accumulator channel.
        On MSW and OSX this function also sets the size of the accumulation buffer.

        @param mRed
        The minimal number of bits for red accumulator.
        @param mGreen
        The minimal number of bits for green accumulator.
        @param mBlue
        The minimal number of bits for blue accumulator.
        @param mAlpha
        The minimal number of bits for alpha accumulator.
    */
    wxGLAttributes& MinAcumRGBA(int mRed, int mGreen, int mBlue, int mAlpha);

    /**
        Use multi-sampling support (antialiasing).

        @param val
        Number of sample buffers, usually 1.
    */
    wxGLAttributes& SampleBuffers(int val);

    /**
        Specifies the number of samplers per pixel.

        @param val
        Number of samplers, e.g. 4 for 2x2 antialiasing.
    */
    wxGLAttributes& Samplers(int val);

    /**
        Used to request a frame buffer sRGB capable. It makes no effect for macOS.
    */
    wxGLAttributes& FrameBuffersRGB();

    /**
        Set some typically needed attributes. E.g. full-acceleration on MSW.

        @see Defaults()
    */
    wxGLAttributes& PlatformDefaults();

    /**
        wxWidgets defaults:
        RGBA, Z-depth 16 bits, double buffering, 1 sample buffer, 4 samplers.

        @see PlatformDefaults()
    */
    wxGLAttributes& Defaults();

    /**
        The set of attributes must end with this one; otherwise, the GPU may
        display nothing at all.
    */
    void EndList();
};

/**
    @class wxGLContextAttrs

    This class is used for setting context attributes.
    Since OpenGL version 3.0 the ARB adds attributes time to time to the rendering
    context functionality. wxWidgets implements attributes up to OGL 4.5, but
    you can set new attributes by using AddAttribute() and AddAttribBits()
    functions inherited from the base wxGLAttribsBase class.

    Attributes can be chained. For example:
    @code
    wxGLContextAttrs cxtAttrs;
    cxtAttrs.CoreProfile().OGLVersion(4, 5).Robust().ResetIsolation().EndList();
    @endcode

    Notice that EndList() must be used as the last attribute. Not adding it will
    likely result in nothing displayed at all.

    @since 3.1.0

    @library{wxgl}
    @category{gl}

    @see wxGLCanvas, wxGLContext, wxGLAttribsBase, wxGLAttributes
*/
class wxGLContextAttrs : public wxGLAttribsBase
{
public:
    /**
        Request an OpenGL core profile for the context.

        If the requested OpenGL version is less than 3.2, this attribute is
        ignored and the functionality of the context is determined solely by
        the requested version.
    */
    wxGLContextAttrs& CoreProfile();

    /**
        Request specific OpenGL core major version number (>= 3).

        @param val
        The major version number requested.

        It has no effect under macOS where specifying CoreProfile() will
        result in using OpenGL version at least 3.2.
    */
    wxGLContextAttrs& MajorVersion(int val);

    /**
        Request specific OpenGL core minor version number.

        @param val
        The minor version number requested, e.g. 2 if OpenGL 3.2 is requested.

        It has no effect under macOS where specifying CoreProfile() will
        result in using OpenGL version at least 3.2.
    */
    wxGLContextAttrs& MinorVersion(int val);

    /**
        An easy way of requesting an OpenGL version.

        @param vmayor
        The major version number requested, e.g. 4 if OpenGL 4.5 is requested.

        @param vminor
        The minor version number requested, e.g. 5 if OpenGL 4.5 is requested.

        It has no effect under macOS where specifying CoreProfile() will
        result in using OpenGL version at least 3.2.
    */
    wxGLContextAttrs& OGLVersion(int vmayor, int vminor);

    /**
        Request a type of context with all OpenGL features from version 1.0 to
        the newest available by the GPU driver. In this mode the GPU may be some
        slower than it would be without this compatibility attribute.
    */
    wxGLContextAttrs& CompatibilityProfile();

    /**
        Request a forward-compatible context.
        Forward-compatible contexts are defined only for OpenGL versions 3.0 and
        later. They must not support functionality marked as deprecated or
        removed by the requested version of the OpenGL API.

        It has no effect under macOS.
    */
    wxGLContextAttrs& ForwardCompatible();

    /**
        Request an ES or ES2 ("Embedded Subsystem") context. These are special
        subsets of OpenGL, lacking some features of the full specification.
        Used mainly in embedded devices such as mobile phones.

        It has no effect under macOS.
    */
    wxGLContextAttrs& ES2();

    /**
        Request debugging functionality. This tells OpenGL to prepare a set where
        some logs are enabled and also allows OGL to send debug messages through
        a callback function.

        It has no effect under macOS.
    */
    wxGLContextAttrs& DebugCtx();

    /**
        Request robustness, or how OpenGL handles out-of-bounds buffer object
        accesses and graphics reset notification behaviours.

        It has no effect under macOS.
    */
    wxGLContextAttrs& Robust();

    /**
        With robustness enabled, never deliver notification of reset events.

        It has no effect under macOS.
    */
    wxGLContextAttrs& NoResetNotify();

    /**
        With robustness enabled, if graphics reset happens, all context state is
        lost.

        It has no effect under macOS.
    */
    wxGLContextAttrs& LoseOnReset();

    /**
        Request OpenGL to protect other applications or shared contexts from reset
        side-effects.

        It has no effect under macOS.
    */
    wxGLContextAttrs& ResetIsolation();

    /**
        Request OpenGL to avoid or not flushing pending commands when the context
        is made no longer current (released). If you don't specify this attribute,
        the GPU driver default is 'flushing'.

        @param val
        0 for not flushing, 1 (wxWidgets default) for flushing pending commands.

        It has no effect under macOS.
    */
    wxGLContextAttrs& ReleaseFlush(int val = 1);

    /**
        Set platform specific defaults. Currently only Unix defaults are
        implemented: use X11 direct rendering and use X11 RGBA type.
    */
    wxGLContextAttrs& PlatformDefaults();

    /**
        The set of attributes must end with this one; otherwise, the GPU may
        display nothing at all.
    */
    void EndList();
};

/**
    @class wxGLContext

    An instance of a wxGLContext represents the state of an OpenGL state
    machine and the connection between OpenGL and the system.

    The OpenGL state includes everything that can be set with the OpenGL API:
    colors, rendering variables, buffer data ids, texture objects, etc. It is
    possible to have multiple rendering contexts share buffer data and textures.
    This feature is specially useful when the application use multiple threads
    for updating data into the memory of the graphics card.

    Whether one only rendering context is used with or bound to multiple output
    windows or if each window has its own bound context is a developer decision.
    It is important to take into account that GPU makers may set different pointers
    to the same OGL function for different contexts. The way these pointers are
    retrieved from the OGL driver should be used again for each new context.

    Binding (making current) a rendering context with another instance of a
    wxGLCanvas however works only if the both wxGLCanvas instances were created
    with the same attributes.

    OpenGL version 3 introduced a new type of specification profile, the modern
    core profile. The old compatibility profile maintains all legacy features.
    Since wxWidgets 3.1.0 you can choose the type of context and even ask for a
    specified OGL version number. However, its advised to use only core profile
    as the compatibility profile may run a bit slower.

    OpenGL core profile specification defines several flags at context creation
    that determine not only the type of context but also some features. Some of
    these flags can be set in the list of attributes used at wxGLCanvas ctor.
    But since wxWidgets 3.1.0 it is strongly encouraged to use the new mechanism:
    setting the context attributes with a wxGLContextAttrs object and the canvas
    attributes with a wxGLAttributes object.

    The best way of knowing if your OpenGL environment supports a specific type
    of context is creating a wxGLContext instance and checking wxGLContext::IsOK().
    If it returns false, then simply delete that instance and create a new one
    with other attributes.

    wxHAS_OPENGL_ES is defined on platforms that only have this implementation
    available (e.g. the iPhone) and don't support the full specification.

    @library{wxgl}
    @category{gl}

    @see wxGLCanvas, wxGLContextAttrs, wxGLAttributes
*/
class wxGLContext : public wxObject
{
public:
    /**
        Constructor.

        @param win
            The canvas that is used to initialize this context. This parameter
            is needed only temporarily, and the caller may do anything with it
            (e.g. destroy the window) after the constructor returned. @n
            It will be possible to bind (make current) this context to any
            other wxGLCanvas that has been created with equivalent attributes
            as win.
        @param other
            Context to share some data with or @NULL (the default) for no
            sharing.
        @param ctxAttrs
            A wxGLContextAttrs pointer to the attributes used for defining the
            flags when OGL >= 3.2 is requested. This is the preferred method
            since wxWidgets 3.1.0. The previous method (still available for
            backwards compatibility) is to define the attributes at wxGLCanvas
            ctor. If this parameter is @NULL (the default) then that previous
            method is taken.
            If no attributes are defined at all, then those provided by the GPU
            driver defaults will be used.
    */
    wxGLContext(wxGLCanvas* win,
                const wxGLContext* other = NULL,
                const wxGLContextAttrs* ctxAttrs = NULL);

    /**
        Checks if the underlying OpenGL rendering context was correctly created
        by the system with the requested attributes. If this function returns
        @false then the wxGLContext object is useless and should be deleted and
        recreated with different attributes.

        @since 3.1.0
    */
    bool IsOK();

    /**
        Makes the OpenGL state that is represented by this rendering context
        current with the wxGLCanvas @e win.

        @note @a win can be a different wxGLCanvas window than the one that was
              passed to the constructor of this rendering context. If @e RC is
              an object of type wxGLContext, the statements
              @e "RC.SetCurrent(win);" and @e "win.SetCurrent(RC);" are
              equivalent, see wxGLCanvas::SetCurrent().
    */
    virtual bool SetCurrent(const wxGLCanvas& win) const;
};

/**
    @anchor wxGL_FLAGS

    Constants for use with wxGLCanvas. Most of the constants must be followed
    by a value. This is indicated by "(F)".
    Instead of these flags, the use of wxGLAttributes and wxGLContextAttrs is
    preferred since wxWidgets 3.1.0.

    @note Not all implementations support options such as stereo, auxiliary
          buffers, alpha channel, and accumulator buffer, use
          wxGLCanvas::IsDisplaySupported() to check for individual visual
          attributes support.
*/
enum
{
    /// Use true color (the default if no attributes at all are specified);
    /// do not use a palette.
    WX_GL_RGBA = 1,

    /// (F) Specifies the number of bits for colour buffer.
    WX_GL_BUFFER_SIZE,

    /// (F) 0 for main buffer, >0 for overlay, <0 for underlay.
    WX_GL_LEVEL,

    /// Use double buffering if present ("on" if no attributes specified).
    WX_GL_DOUBLEBUFFER,

    /// Use stereoscopic display.
    WX_GL_STEREO,

    /// (F) The number of auxiliary buffers.
    WX_GL_AUX_BUFFERS,

    /// (F) Use red buffer with most bits (> MIN_RED bits)
    WX_GL_MIN_RED,

    /// (F) Use green buffer with most bits (> MIN_GREEN bits)
    WX_GL_MIN_GREEN,

    /// (F) Use blue buffer with most bits (> MIN_BLUE bits)
    WX_GL_MIN_BLUE,

    /// (F) Use alpha buffer with most bits (> MIN_ALPHA bits)
    WX_GL_MIN_ALPHA,

    /// (F) Specifies number of bits for Z-buffer (typically 0, 16 or 32).
    WX_GL_DEPTH_SIZE,

    /// (F) Specifies number of bits for stencil buffer.
    WX_GL_STENCIL_SIZE,

    /// (F) Specifies minimal number of red accumulator bits.
    WX_GL_MIN_ACCUM_RED,

    /// (F) Specifies minimal number of green accumulator bits.
    WX_GL_MIN_ACCUM_GREEN,

    /// (F) Specifies minimal number of blue accumulator bits.
    WX_GL_MIN_ACCUM_BLUE,

    /// (F) Specifies minimal number of alpha accumulator bits.
    WX_GL_MIN_ACCUM_ALPHA,

    /// (F) 1 for multi-sampling support (antialiasing)
    WX_GL_SAMPLE_BUFFERS,

    /// (F) 4 for 2x2 antialiasing supersampling on most graphics cards
    WX_GL_SAMPLES,

    /**
        Used to request a frame buffer sRGB capable

        @since 3.1.0
    */
    WX_GL_FRAMEBUFFER_SRGB,

    /**
        (F) Request specific OpenGL core major version number (>= 3).

        This attribute should be followed by the major version number
        requested.

        It has no effect under macOS where specifying ::WX_GL_CORE_PROFILE will
        result in using OpenGL version at least 3.2 but can still be used
        there for portability.

        @since 3.0.3
    */
    WX_GL_MAJOR_VERSION,

    /**
        (F) Request specific OpenGL core minor version number.

        This attribute has the same semantics as ::WX_GL_MAJOR_VERSION but is
        for the minor OpenGL version, e.g. 2 if OpenGL 3.2 is requested.

        @since 3.0.3
    */
    WX_GL_MINOR_VERSION,

    /**
        Request an OpenGL core profile for the context.

        If the requested OpenGL version is less than 3.2, this attribute is
        ignored and the functionality of the context is determined solely by
        the requested version.

        @since 3.0.3
    */
    WX_GL_CORE_PROFILE,

    /**
        Request a type of context with all OpenGL features from version 1.0 to
        the newest available by the GPU driver. In this mode the GPU may be some
        slower than it would be without this compatibility attribute.

        @since 3.1.0
    */
    wx_GL_COMPAT_PROFILE,

    /**
        Request a forward-compatible context.
        Forward-compatible contexts are defined only for OpenGL versions 3.0 and
        later. They must not support functionality marked as deprecated by that
        version of the OpenGL API.

        @since 3.1.0
    */
    WX_GL_FORWARD_COMPAT,

    /**
        Request an ES or ES2 ("Embedded Subsystem") context. These are special
        subsets of OpenGL, lacking some features of the full specification.
        Used mainly in embedded devices such as mobile phones.

        @since 3.1.0
    */
    WX_GL_ES2,

    /**
        Request debugging functionality. This tells OpenGL to prepare a set where
        some logs are enabled and also allows OGL to send debug messages through
        a callback function.

        @since 3.1.0
    */
    WX_GL_DEBUG,

    /**
        Request robustness, or how OpenGL handles out-of-bounds buffer object
        accesses and graphics reset notification behaviours.

        @since 3.1.0
    */
    WX_GL_ROBUST_ACCESS,

    /**
        With robustness enabled, never deliver notification of reset events.

        @since 3.1.0
    */
    WX_GL_NO_RESET_NOTIFY,

    /**
        With robustness enabled, if graphics reset happens, all context state is
        lost.

        @since 3.1.0
    */
    WX_GL_LOSE_ON_RESET,

    /**
        Request OpenGL to protect other apps or shared contexts from reset
        side-effects.

        @since 3.1.0
    */
    WX_GL_RESET_ISOLATION,

    /**
        Request OpenGL to flush pending commands when the context is released.

        @since 3.1.0
    */
    WX_GL_RELEASE_FLUSH,

    /**
        Request OpenGL to avoid flushing pending commands when the context is released.

        @since 3.1.0
    */
    WX_GL_RELEASE_NONE
};

/**
    @class wxGLCanvas

    wxGLCanvas is a class for displaying OpenGL graphics. It is always used in
    conjunction with wxGLContext as the context can only be made current (i.e.
    active for the OpenGL commands) when it is associated to a wxGLCanvas.

    More precisely, you first need to create a wxGLCanvas window and then
    create an instance of a wxGLContext that is initialized with this
    wxGLCanvas and then later use either SetCurrent() with the instance of the
    wxGLContext or wxGLContext::SetCurrent() with the instance of the
    wxGLCanvas (which might be not the same as was used for the creation of the
    context) to bind the OpenGL state that is represented by the rendering
    context to the canvas, and then finally call SwapBuffers() to swap the
    buffers of the OpenGL canvas and thus show your current output.

    Please note that wxGLContext always uses physical pixels, even on the
    platforms where wxWindow uses logical pixels, affected by the coordinate
    scaling, on high DPI displays. Thus, if you want to set the OpenGL view
    port to the size of entire window, you must multiply the result returned by
    wxWindow::GetClientSize() by wxGLCanvas::GetContentScaleFactor() before
    passing it to @c glViewport(). Same considerations apply to other OpenGL
    functions and other coordinates, notably those retrieved from wxMouseEvent
    in the event handlers.

    Notice that versions of wxWidgets previous to 2.9 used to implicitly create a
    wxGLContext inside wxGLCanvas itself. This is still supported in the
    current version but is deprecated now and will be removed in the future,
    please update your code to create the rendering contexts explicitly.

    To set up the attributes for the canvas (number of bits for the depth
    buffer, number of bits for the stencil buffer and so on) you pass them
    in the constructor using a wxGLAttributes instance. You can still use
    the way before 3.1.0 (setting up the correct values of the @e attribList
    parameter) but it's discouraged.

    @note
        On those platforms which use a configure script (e.g. Linux and macOS)
        OpenGL support is automatically enabled if the relative headers and
        libraries are found.
        To switch it on under the other platforms (e.g. Windows), you need to edit
        the @c setup.h file and set @c wxUSE_GLCANVAS to @c 1 and then also pass
        @c USE_OPENGL=1 to the make utility. You may also need to add @c opengl32.lib
        (and @c glu32.lib for old OpenGL versions) to the list of the libraries
        your program is linked with.

    @library{wxgl}
    @category{gl}

    @see wxGLContext, wxGLAttributes, wxGLContextAttrs
*/
class wxGLCanvas : public wxWindow
{
public:
    /**
        Creates a window with the given parameters. Notice that you need to
        create and use a wxGLContext to output to this window.

        @param parent
            Pointer to a parent window.
        @param dispAttrs
            The wxGLAttributes used for setting display attributes (not for
            rendering context attributes).
        @param id
            Window identifier. If -1, will automatically create an identifier.
        @param pos
            Window position. wxDefaultPosition is (-1, -1) which indicates that
            wxWidgets should generate a default position for the window.
        @param size
            Window size. wxDefaultSize is (-1, -1) which indicates that
            wxWidgets should generate a default size for the window. If no
            suitable size can be found, the window will be sized to 20x20
            pixels so that the window is visible but obviously not correctly
            sized.
        @param style
            Window style.
        @param name
            Window name.
        @param palette
            Palette for indexed colour (i.e. non WX_GL_RGBA) mode. Ignored
            under most platforms.

        @since 3.1.0
    */
    wxGLCanvas(wxWindow *parent,
               const wxGLAttributes& dispAttrs,
               wxWindowID id = wxID_ANY,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = 0,
               const wxString& name = wxGLCanvasName,
               const wxPalette& palette = wxNullPalette);

    /**
        This constructor is still available only for compatibility reasons.
        Please use the constructor with wxGLAttributes instead.

        If @a attribList is not specified, wxGLAttributes::PlatformDefaults()
        is used, plus some other attributes (see below).

        @param parent
            Pointer to a parent window.
        @param id
            Window identifier. If -1, will automatically create an identifier.
        @param attribList
            Array of integers. With this parameter you can set the device
            context attributes associated to this window. This array is
            zero-terminated: it should be set up using @ref wxGL_FLAGS
            constants. If a constant should be followed by a value, put it in
            the next array position. For example, WX_GL_DEPTH_SIZE should be
            followed by the value that indicates the number of bits for the
            depth buffer, e.g.:
            @code
            attribList[n++] = WX_GL_DEPTH_SIZE;
            attribList[n++] = 32;
            attribList[n] = 0; // terminate the list
            @endcode
            If the attribute list is not specified at all, i.e. if this
            parameter is @NULL, the default attributes including WX_GL_RGBA and
            WX_GL_DOUBLEBUFFER are used. But notice that if you do specify some
            attributes you also need to explicitly include these two default
            attributes in the list if you need them.
        @param pos
            Window position. wxDefaultPosition is (-1, -1) which indicates that
            wxWidgets should generate a default position for the window.
        @param size
            Window size. wxDefaultSize is (-1, -1) which indicates that
            wxWidgets should generate a default size for the window. If no
            suitable size can be found, the window will be sized to 20x20
            pixels so that the window is visible but obviously not correctly
            sized.
        @param style
            Window style.
        @param name
            Window name.
        @param palette
            Palette for indexed colour (i.e. non WX_GL_RGBA) mode. Ignored
            under most platforms.
    */
    wxGLCanvas(wxWindow* parent, wxWindowID id = wxID_ANY,
               const int* attribList = NULL,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = 0,
               const wxString& name = "GLCanvas",
               const wxPalette& palette = wxNullPalette);

    /**
        Determines if a canvas having the specified attributes is available.
        This only applies for visual attributes, not rendering context attributes.

        @param dispAttrs
            The requested attributes.

        @return @true if attributes are supported.

        @since 3.1.0
    */
    static bool IsDisplaySupported(const wxGLAttributes& dispAttrs);

    /**
        Determines if a canvas having the specified attributes is available.
        This only applies for visual attributes, not rendering context attributes.
        Please, use the new form of this method, using wxGLAttributes.

        @param attribList
            See @a attribList for wxGLCanvas().

        @return @true if attributes are supported.
    */
    static bool IsDisplaySupported(const int* attribList);

    /**
        Returns true if the extension with given name is supported.

        Notice that while this function is implemented for all of GLX, WGL and
        NSOpenGL the extensions names are usually not the same for different
        platforms and so the code using it still usually uses conditional
        compilation.
    */
    static bool IsExtensionSupported(const char *extension);

    /**
        Sets the current colour for this window (using @c glcolor3f()), using
        the wxWidgets colour database to find a named colour.
    */
    bool SetColour(const wxString& colour);

    /**
        Makes the OpenGL state that is represented by the OpenGL rendering
        context @a context current, i.e. it will be used by all subsequent
        OpenGL calls.

        This is equivalent to wxGLContext::SetCurrent() called with this window
        as parameter.

        @note This function may only be called when the window is shown on
              screen, in particular it can't usually be called from the
              constructor as the window isn't yet shown at this moment.

        @return @false if an error occurred.
    */
    bool SetCurrent(const wxGLContext& context) const;

    /**
        Swaps the double-buffer of this window, making the back-buffer the
        front-buffer and vice versa, so that the output of the previous OpenGL
        commands is displayed on the window.

        @return @false if an error occurred.
    */
    virtual bool SwapBuffers();
};
