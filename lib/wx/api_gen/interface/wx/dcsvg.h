/////////////////////////////////////////////////////////////////////////////
// Name:        dcsvg.h
// Purpose:     interface of wxSVGFileDC
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    SVG shape rendering mode.

    These options represent the values defined in the SVG specification:
    https://svgwg.org/svg2-draft/painting.html#ShapeRenderingProperty
*/
enum wxSVGShapeRenderingMode
{
    wxSVG_SHAPE_RENDERING_AUTO = 0,
    wxSVG_SHAPE_RENDERING_OPTIMIZE_SPEED,
    wxSVG_SHAPE_RENDERING_CRISP_EDGES,
    wxSVG_SHAPE_RENDERING_GEOMETRIC_PRECISION,

    wxSVG_SHAPE_RENDERING_OPTIMISE_SPEED = wxSVG_SHAPE_RENDERING_OPTIMIZE_SPEED
};


/**
    @class wxSVGFileDC

    A wxSVGFileDC is a device context onto which graphics and text can be
    drawn, and the output produced as a vector file, in SVG format.

    This format can be read by a range of programs, including a Netscape plugin
    (Adobe) and the open source Inkscape program (http://inkscape.org/).  Full
    details are given in the W3C SVG recommendation (http://www.w3.org/TR/SVG/).

    The intention behind wxSVGFileDC is that it can be used to produce a file
    corresponding to the screen display context, wxSVGFileDC, by passing the
    wxSVGFileDC as a parameter instead of a wxDC. Thus the wxSVGFileDC
    is a write-only class.

    As the wxSVGFileDC is a vector format, raster operations like GetPixel()
    are unlikely to be supported. However, the SVG specification allows for
    raster files to be embedded in the SVG, and so bitmaps, icons and blit
    operations in wxSVGFileDC are supported. By default only PNG format bitmaps
    are supported and these are saved as separate files in the same folder
    as the SVG file, however it is possible to change this behaviour by
    replacing the built in bitmap handler using wxSVGFileDC::SetBitmapHandler().

    More substantial SVG libraries (for reading and writing) are available at
    <a href="http://wxart2d.sourceforge.net/" target="_blank">wxArt2D</a> and
    <a href="http://wxsvg.sourceforge.net/" target="_blank">wxSVG</a>.

    @library{wxcore}
    @category{dc}
*/

class wxSVGFileDC : public wxDC
{
public:
    /**
        Initializes a wxSVGFileDC with the given @a filename, @a width and
        @a height at @a dpi resolution, and an optional @a title.
        The title provides a readable name for the SVG document.
    */
    wxSVGFileDC(const wxString& filename, int width = 320, int height = 240,
                double dpi = 72, const wxString& title = wxString());

    /**
        Draws a rectangle the size of the SVG using the wxDC::SetBackground() brush.
    */
    void Clear();

    /**
        Replaces the default bitmap handler with @a handler.

        By default, an object of wxSVGBitmapFileHandler class is used as bitmap
        handler. You may want to replace it with an object of predefined
        wxSVGBitmapEmbedHandler class to embed the bitmaps in the generated SVG
        instead of storing them in separate files like this:
        @code
        mySVGFileDC->SetBitmapHandler(new wxSVGBitmapEmbedHandler());
        @endcode

        or derive your own bitmap handler class and use it if you need to
        customize the bitmap handling further.

        @param handler The new bitmap handler. If non-NULL, this object takes
            ownership of this handler and will delete it when it is not needed
            any more.

        @since 3.1.0
    */
    void SetBitmapHandler(wxSVGBitmapHandler* handler);

    /**
        Set the shape rendering mode of the generated SVG.
        All subsequent drawing calls will have this rendering mode set in the
        SVG file.

        The default mode is wxSVG_SHAPE_RENDERING_AUTO.

        @since 3.1.3
    */
    void SetShapeRenderingMode(wxSVGShapeRenderingMode renderingMode);

    /**
        Sets the clipping region for this device context to the intersection of
        the given region described by the parameters of this method and the previously
        set clipping region.
        Clipping is implemented in the SVG output using SVG group elements (\<g\>), with
        nested group elements being used to represent clipping region intersections when
        two or more calls are made to SetClippingRegion().
    */
    void SetClippingRegion(wxCoord x, wxCoord y, wxCoord width,
                           wxCoord height);

    /**
        Destroys the current clipping region so that none of the DC is clipped.
        Since intersections arising from sequential calls to SetClippingRegion are represented
        with nested SVG group elements (\<g\>), all such groups are closed when
        DestroyClippingRegion is called.
    */
    void DestroyClippingRegion();

    //@{
    /**
        Function not implemented in this DC class.
    */
    void CrossHair(wxCoord x, wxCoord y);
    bool FloodFill(wxCoord x, wxCoord y, const wxColour& colour,
                   wxFloodFillStyle style = wxFLOOD_SURFACE);
    bool GetPixel(wxCoord x, wxCoord y, wxColour* colour) const;
    void SetPalette(const wxPalette& palette);
    int GetDepth() const;
    void SetLogicalFunction(wxRasterOperationMode function);
    wxRasterOperationMode GetLogicalFunction() const;
    bool StartDoc(const wxString& message);
    void EndDoc();
    void StartPage();
    void EndPage();
    //@}
};

/**
    Abstract base class for handling bitmaps inside a wxSVGFileDC.

    To use it you need to derive a new class from it and override
    ProcessBitmap() to generate a properly a formed SVG image element (see
    http://www.w3.org/TR/SVG/struct.html#ImageElement).

    Two example bitmap handlers are provided in wx/dcsvg.h. The first (default)
    handler will create PNG files in the same folder as the SVG file and uses
    links to them in the SVG. The second handler (wxSVGBitmapEmbedHandler) will
    embed the PNG image in the SVG file using base 64 encoding.

    The handler can be changed by calling wxSVGFileDC::SetBitmapHandler().

    @library{wxcore}
    @category{dc}

    @since 3.1.0
*/
class wxSVGBitmapHandler
{
public:
    /**
        Writes the bitmap representation as SVG to the given stream.

        The XML generated by this function will be inserted into the SVG file
        inline with the XML generated by the main wxSVGFileDC class so it is
        important that the XML is properly formed.

        @param bitmap A valid bitmap to add to SVG.
        @param x Horizontal position of the bitmap.
        @param y Vertical position of the bitmap.
        @param stream The stream to write SVG contents to.
    */
    virtual bool ProcessBitmap(const wxBitmap& bitmap,
                               wxCoord x, wxCoord y,
                               wxOutputStream& stream) const = 0;
};

/**
    Handler embedding bitmaps as base64-encoded PNGs into the SVG.

    @see wxSVGFileDC::SetBitmapHandler().

    @library{wxcore}
    @category{dc}

    @since 3.1.0
*/
class wxSVGBitmapEmbedHandler : public wxSVGBitmapHandler
{
public:
    virtual bool ProcessBitmap(const wxBitmap& bitmap,
                               wxCoord x, wxCoord y,
                               wxOutputStream& stream) const;
};

/**
    Handler saving bitmaps to external PNG files and linking to it from the
    SVG.

    This handler is used by default by wxSVGFileDC. PNG files are created in
    the same folder as the SVG file and are named using the SVG filename
    appended with ``_image#.png``.

    When using wxSVGFileDC::SetBitmapHandler() to set this handler with the
    default constructor, the PNG files are created in the runtime location of
    the application. The save location can be customized by using the
    wxSVGBitmapFileHandler(const wxFileName&) constructor.

    @see wxSVGFileDC::SetBitmapHandler().

    @library{wxcore}
    @category{dc}

    @since 3.1.0
*/
class wxSVGBitmapFileHandler : public wxSVGBitmapHandler
{
public:
    /**
        Create a wxSVGBitmapFileHandler and specify the location where the file
        will be saved.

        @param path The path of the save location. If @a path contains a
        filename, the autogenerated filename will be appended to this name.

        @since 3.1.3
    */
    wxSVGBitmapFileHandler(const wxFileName& path);

    virtual bool ProcessBitmap(const wxBitmap& bitmap,
                               wxCoord x, wxCoord y,
                               wxOutputStream& stream) const;
};
