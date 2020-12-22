/////////////////////////////////////////////////////////////////////////////
// Name:        gauge.h
// Purpose:     interface of wxGauge
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


// ----------------------------------------------------------------------------
// wxGauge style flags
// ----------------------------------------------------------------------------

#define wxGA_HORIZONTAL      wxHORIZONTAL
#define wxGA_VERTICAL        wxVERTICAL

// Available since Windows 7 only. With this style, the value of gauge will
// reflect on the taskbar button.
#define wxGA_PROGRESS        0x0010
// Win32 only, is default (and only) on some other platforms
#define wxGA_SMOOTH          0x0020
// QT only, display current completed percentage (text default format "%p%")
#define wxGA_TEXT            0x0040

/**
    @class wxGauge

    A gauge is a horizontal or vertical bar which shows a quantity (often
    time).

    wxGauge supports two working modes: determinate and indeterminate progress.

    The first is the usual working mode (see SetValue() and SetRange()) while
    the second can be used when the program is doing some processing but you
    don't know how much progress is being done. In this case, you can
    periodically call the Pulse() function to make the progress bar switch to
    indeterminate mode (graphically it's usually a set of blocks which move or
    bounce in the bar control).

    wxGauge supports dynamic switch between these two work modes.

    There are no user commands for the gauge.

    @beginStyleTable
    @style{wxGA_HORIZONTAL}
           Creates a horizontal gauge.
    @style{wxGA_VERTICAL}
           Creates a vertical gauge.
    @style{wxGA_SMOOTH}
           Creates smooth progress bar with one pixel wide update step (not
           supported by all platforms).
    @style{wxGA_TEXT}
           Display the current value in percents in the gauge itself. This
           style is only supported in wxQt and ignored under the other
           platforms.
           This flag is only available in wxWidgets 3.1.0 and later.
    @style{wxGA_PROGRESS}
           Reflect the value of gauge in the application taskbar button under
           Windows 7 and later and the dock icon under macOS, ignored under
           the other platforms.
           This flag is only available in wxWidgets 3.1.0 and later.

    @endStyleTable

    @library{wxcore}
    @category{ctrl}
    @appearance{gauge}

    @see wxSlider, wxScrollBar
*/
class wxGauge : public wxControl
{
public:
    /**
        Default constructor.
    */
    wxGauge();

    /**
        Constructor, creating and showing a gauge.

        @param parent
            Window parent.
        @param id
            Window identifier.
        @param range
            Integer range (maximum value) of the gauge.
            See SetRange() for more details about the meaning of this value
            when using the gauge in indeterminate mode.
        @param pos
            Window position.
        @param size
            Window size.
        @param style
            Gauge style.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create()
    */
    wxGauge(wxWindow* parent, wxWindowID id, int range,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize,
            long style = wxGA_HORIZONTAL,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxGaugeNameStr);

    /**
        Destructor, destroying the gauge.
    */
    virtual ~wxGauge();

    /**
        Creates the gauge for two-step construction. See wxGauge() for further
        details.
    */
    bool Create(wxWindow* parent, wxWindowID id, int range,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = wxGA_HORIZONTAL,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxGaugeNameStr);

    /**
        Returns the maximum position of the gauge.

        @see SetRange()
    */
    int GetRange() const;

    /**
        Returns the current position of the gauge.

        @see SetValue()
    */
    int GetValue() const;

    /**
        Returns @true if the gauge is vertical (has @c wxGA_VERTICAL style) and
        @false otherwise.
    */
    bool IsVertical() const;

    /**
        Switch the gauge to indeterminate mode (if required) and makes the
        gauge move a bit to indicate the user that some progress has been made.

        @note After calling this function the value returned by GetValue() is
              undefined and thus you need to explicitly call SetValue() if you
              want to restore the determinate mode.
    */
    virtual void Pulse();

    /**
        Sets the range (maximum value) of the gauge. This function makes the
        gauge switch to determinate mode, if it's not already.

        When the gauge is in indeterminate mode, under wxMSW the gauge
        repeatedly goes from zero to @a range and back; under other ports
        when in indeterminate mode, the @a range setting is ignored.

        @see GetRange()
    */
    void SetRange(int range);

    /**
        Sets the position of the gauge. The @a pos must be between 0 and the
        gauge range as returned by GetRange(), inclusive.

        This function makes the gauge switch to determinate mode, if it was in
        indeterminate mode before.

        @param pos
            Position for the gauge level.

        @see GetValue()
    */
    void SetValue(int pos);
};

