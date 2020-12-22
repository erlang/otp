///////////////////////////////////////////////////////////////////////////////
// Name:        interface/wx/nonownedwnd.h
// Purpose:     wxNonOwnedWindow class documentation
// Author:      Vadim Zeitlin
// Created:     2011-10-09
// Copyright:   (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////


/**
   Styles that can be used with any wxNonOwnedWindow:
*/
#define wxFRAME_SHAPED          0x0010  // Create a window that is able to be shaped


/**
    Common base class for all non-child windows.

    This is the common base class of wxTopLevelWindow and wxPopupWindow and is
    not used directly.

    Currently the only additional functionality it provides, compared to base
    wxWindow class, is the ability to set the window shape.

    @since 2.9.3
 */
class wxNonOwnedWindow : public wxWindow
{
public:
    /**
        If the platform supports it, sets the shape of the window to that
        depicted by @a region. The system will not display or respond to any
        mouse event for the pixels that lie outside of the region. To reset the
        window to the normal rectangular shape simply call SetShape() again with
        an empty wxRegion. Returns @true if the operation is successful.

        This method is available in this class only since wxWidgets 2.9.3,
        previous versions only provided it in wxTopLevelWindow.

        Note that windows with non default shape have a fixed size and can't be
        resized using SetSize().
    */
    bool SetShape(const wxRegion& region);

    /**
        Set the window shape to the given path.

        Set the window shape to the interior of the given path and also draw
        the window border along the specified path.

        For example, to make a clock-like circular window you could use
        @code
            wxSize size = GetSize();
            wxGraphicsPath
                path = wxGraphicsRenderer::GetDefaultRenderer()->CreatePath();
            path.AddCircle(size.x/2, size.y/2, 30);
            SetShape(path);
        @endcode

        Note that windows with non default shape have a fixed size and can't be
        resized using SetSize().

        As the overload above, this method is not guaranteed to work on all
        platforms but currently does work in wxMSW, wxOSX/Cocoa and wxGTK (with
        the appropriate but almost always present X11 extensions) ports.

        @since 2.9.3
     */
    bool SetShape(const wxGraphicsPath& path);
};
