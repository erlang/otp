/////////////////////////////////////////////////////////////////////////////
// Name:        statline.h
// Purpose:     interface of wxStaticLine
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxStaticLine

    A static line is just a line which may be used in a dialog to separate the
    groups of controls.

    The line may be only vertical or horizontal. Moreover, not all ports
    (notably not wxGTK) support specifying the transversal direction of the
    line (e.g. height for a horizontal line) so for maximal portability you
    should specify it as wxDefaultCoord.

    @beginStyleTable
    @style{wxLI_HORIZONTAL}
           Creates a horizontal line.
    @style{wxLI_VERTICAL}
           Creates a vertical line.
    @endStyleTable

    @library{wxcore}
    @category{ctrl}

    @see wxStaticBox
*/
class wxStaticLine : public wxControl
{
public:
    /**
      Default constructor
    */
    wxStaticLine();

    /**
        Constructor, creating and showing a static line.

        @param parent
            Parent window. Must not be @NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param pos
            Window position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Size. Note that either the height or the width (depending on
            whether the line if horizontal or vertical) is ignored.
        @param style
            Window style (either wxLI_HORIZONTAL or wxLI_VERTICAL).
        @param name
            Window name.

        @see Create()
    */
    wxStaticLine(wxWindow* parent, wxWindowID id = wxID_ANY,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = wxLI_HORIZONTAL,
                 const wxString& name = wxStaticLineNameStr);

    /**
        Creates the static line for two-step construction.
        See wxStaticLine() for further details.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = wxLI_HORIZONTAL,
                const wxString& name = wxStaticLineNameStr);

    /**
        This static function returns the size which will be given to the smaller
        dimension of the static line, i.e. its height for a horizontal line or its
        width for a vertical one.
    */
    static int GetDefaultSize();

    /**
        Returns @true if the line is vertical, @false if horizontal.
    */
    bool IsVertical() const;
};

