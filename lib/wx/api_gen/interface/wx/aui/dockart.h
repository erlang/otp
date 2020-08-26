/////////////////////////////////////////////////////////////////////////////
// Name:        aui/dockart.h
// Purpose:     interface of wxAuiDockArt
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    These are the possible pane dock art settings for wxAuiDefaultDockArt.

    @library{wxaui}
    @category{aui}

*/
enum wxAuiPaneDockArtSetting
{

    /// Customizes the sash size
    wxAUI_DOCKART_SASH_SIZE = 0,

    /// Customizes the caption size
    wxAUI_DOCKART_CAPTION_SIZE = 1,

    /// Customizes the gripper size
    wxAUI_DOCKART_GRIPPER_SIZE = 2,

    /// Customizes the pane border size
    wxAUI_DOCKART_PANE_BORDER_SIZE = 3,

    /// Customizes the pane button size
    wxAUI_DOCKART_PANE_BUTTON_SIZE = 4,

    /// Customizes the background colour, which corresponds to the client area.
    wxAUI_DOCKART_BACKGROUND_COLOUR = 5,

    /// Customizes the sash colour
    wxAUI_DOCKART_SASH_COLOUR = 6,

    /// Customizes the active caption colour
    wxAUI_DOCKART_ACTIVE_CAPTION_COLOUR = 7,

    /// Customizes the active caption gradient colour
    wxAUI_DOCKART_ACTIVE_CAPTION_GRADIENT_COLOUR = 8,

    /// Customizes the inactive caption colour
    wxAUI_DOCKART_INACTIVE_CAPTION_COLOUR = 9,

    /// Customizes the inactive gradient caption colour
    wxAUI_DOCKART_INACTIVE_CAPTION_GRADIENT_COLOUR = 10,

    /// Customizes the active caption text colour
    wxAUI_DOCKART_ACTIVE_CAPTION_TEXT_COLOUR = 11,

    /// Customizes the inactive caption text colour
    wxAUI_DOCKART_INACTIVE_CAPTION_TEXT_COLOUR = 12,

    /// Customizes the border colour
    wxAUI_DOCKART_BORDER_COLOUR = 13,

    /// Customizes the gripper colour
    wxAUI_DOCKART_GRIPPER_COLOUR = 14,

    /// Customizes the caption font
    wxAUI_DOCKART_CAPTION_FONT = 15,

    /// Customizes the gradient type (no gradient, vertical or horizontal)
    wxAUI_DOCKART_GRADIENT_TYPE = 16

};

/**
    These are the possible gradient dock art settings for wxAuiDefaultDockArt

*/
enum wxAuiPaneDockArtGradients
{
    /// No gradient on the captions, in other words a solid colour
    wxAUI_GRADIENT_NONE = 0,

    /// Vertical gradient on the captions, in other words a gradual change in colours from top to bottom
    wxAUI_GRADIENT_VERTICAL = 1,

    /// Horizontal gradient on the captions, in other words a gradual change in colours from left to right
    wxAUI_GRADIENT_HORIZONTAL = 2
};

/**
    These are the possible pane button / wxAuiNotebook button / wxAuiToolBar button states.
*/
enum wxAuiPaneButtonState
{
    /// Normal button state
    wxAUI_BUTTON_STATE_NORMAL = 0,

    /// Hovered button state
    wxAUI_BUTTON_STATE_HOVER = 1 << 1,

    /// Pressed button state
    wxAUI_BUTTON_STATE_PRESSED = 1 << 2,

    /// Disabled button state
    wxAUI_BUTTON_STATE_DISABLED = 1 << 3,

    /// Hidden button state
    wxAUI_BUTTON_STATE_HIDDEN   = 1 << 4,

    /// Checked button state
    wxAUI_BUTTON_STATE_CHECKED  = 1 << 5
};

/**
    These are the possible pane button / wxAuiNotebook button / wxAuiToolBar button identifiers.

*/
enum wxAuiButtonId
{
    /// Shows a close button on the pane
    wxAUI_BUTTON_CLOSE = 101,

    /// Shows a maximize/restore button on the pane
    wxAUI_BUTTON_MAXIMIZE_RESTORE = 102,

    /// Shows a minimize button on the pane
    wxAUI_BUTTON_MINIMIZE = 103,

    /**
        Shows a pin button on the pane
     */
    wxAUI_BUTTON_PIN = 104,

    /**
        Shows an option button on the pane (not implemented)
     */
    wxAUI_BUTTON_OPTIONS = 105,

    /**
        Shows a window list button on the pane (for wxAuiNotebook)
     */
    wxAUI_BUTTON_WINDOWLIST = 106,

    /**
        Shows a left button on the pane (for wxAuiNotebook)
     */
     wxAUI_BUTTON_LEFT = 107,

    /**
        Shows a right button on the pane (for wxAuiNotebook)
     */
     wxAUI_BUTTON_RIGHT = 108,

    /**
        Shows an up button on the pane (not implemented)
     */
    wxAUI_BUTTON_UP = 109,

    /**
        Shows a down button on the pane (not implemented)
     */
    wxAUI_BUTTON_DOWN = 110,

    /**
        Shows one of three possible custom buttons on the pane (not implemented)
     */
    wxAUI_BUTTON_CUSTOM1 = 201,

    /**
        Shows one of three possible custom buttons on the pane (not implemented)
     */
    wxAUI_BUTTON_CUSTOM2 = 202,

    /**
        Shows one of three possible custom buttons on the pane (not implemented)
     */
    wxAUI_BUTTON_CUSTOM3 = 203
};

/**
    @class wxAuiDockArt

    wxAuiDockArt is part of the wxAUI class framework.
    See also @ref overview_aui.

    wxAuiDockArt is the art provider: provides all drawing functionality to the
    wxAui dock manager. This allows the dock manager to have a pluggable look-and-feel.

    By default, a wxAuiManager uses an instance of this class called
    wxAuiDefaultDockArt which provides bitmap art and a colour scheme that is
    adapted to the major platforms' look. You can either derive from that class
    to alter its behaviour or write a completely new dock art class.
    Call wxAuiManager::SetArtProvider to force wxAUI to use your new dock art provider.

    @library{wxaui}
    @category{aui}

    @see wxAuiManager, wxAuiPaneInfo
*/
class wxAuiDockArt
{
public:
    /**
        Constructor.
    */
    wxAuiDockArt();

    /**
        Destructor.
    */
    virtual ~wxAuiDockArt();

    /**
        Draws a background.
    */
    virtual void DrawBackground(wxDC& dc, wxWindow* window, int orientation,
                                const wxRect& rect) = 0;

    /**
        Draws a border.
    */
    virtual void DrawBorder(wxDC& dc, wxWindow* window, const wxRect& rect,
                            wxAuiPaneInfo& pane) = 0;

    /**
        Draws a caption.
    */
    virtual void DrawCaption(wxDC& dc, wxWindow* window, const wxString& text,
                             const wxRect& rect, wxAuiPaneInfo& pane) = 0;

    /**
        Draws a gripper.
    */
    virtual void DrawGripper(wxDC& dc, wxWindow* window, const wxRect& rect,
                             wxAuiPaneInfo& pane) = 0;

    /**
        Draws a button in the pane's title bar.
        @a button can be one of the values of @b wxAuiButtonId.
        @a button_state can be one of the values of @b wxAuiPaneButtonState.
    */
    virtual void DrawPaneButton(wxDC& dc, wxWindow* window, int button,
                                int button_state, const wxRect& rect,
                                wxAuiPaneInfo& pane) = 0;

    /**
        Draws a sash between two windows.
    */
    virtual void DrawSash(wxDC& dc, wxWindow* window, int orientation,
                          const wxRect& rect) = 0;
    /**
        Get the colour of a certain setting.
        @a id can be one of the colour values of @b wxAuiPaneDockArtSetting.
    */
    virtual wxColour GetColour(int id) = 0;

    /**
        Get a font setting.
    */
    virtual wxFont GetFont(int id) = 0;

    /**
        Get the value of a certain setting.
        @a id can be one of the size values of @b wxAuiPaneDockArtSetting.
    */
    virtual int GetMetric(int id) = 0;

    /**
        Set a certain setting with the value @e colour.
        @a id can be one of the colour values of @b wxAuiPaneDockArtSetting.
    */
    virtual void SetColour(int id, const wxColour& colour) = 0;

    /**
        Set a font setting.
    */
    virtual void SetFont(int id, const wxFont& font) = 0;

    /**
        Set a certain setting with the value @e new_val.
        @a id can be one of the size values of @b wxAuiPaneDockArtSetting.
    */
    virtual void SetMetric(int id, int new_val) = 0;
};




/**
    @class wxAuiDefaultDockArt

    This is the default art provider for @ref wxAuiManager.  Dock art
    can be customized by creating a class derived from this one,
    or replacing this class entirely.
*/
class  wxAuiDefaultDockArt : public wxAuiDockArt
{
public:

    wxAuiDefaultDockArt();

    int GetMetric(int metricId);
    void SetMetric(int metricId, int newVal);
    wxColour GetColour(int id);
    void SetColour(int id, const wxColour& colour);
    void SetFont(int id, const wxFont& font);
    wxFont GetFont(int id);

    void DrawSash(wxDC& dc,
                  wxWindow *window,
                  int orientation,
                  const wxRect& rect);

    void DrawBackground(wxDC& dc,
                  wxWindow *window,
                  int orientation,
                  const wxRect& rect);

    void DrawCaption(wxDC& dc,
                  wxWindow *window,
                  const wxString& text,
                  const wxRect& rect,
                  wxAuiPaneInfo& pane);

    void DrawGripper(wxDC& dc,
                  wxWindow *window,
                  const wxRect& rect,
                  wxAuiPaneInfo& pane);

    void DrawBorder(wxDC& dc,
                  wxWindow *window,
                  const wxRect& rect,
                  wxAuiPaneInfo& pane);

    void DrawPaneButton(wxDC& dc,
                  wxWindow *window,
                  int button,
                  int buttonState,
                  const wxRect& rect,
                  wxAuiPaneInfo& pane);

    /**
        @deprecated Not intended for the public API.
    */
    void DrawIcon(wxDC& dc,
                  const wxRect& rect,
                  wxAuiPaneInfo& pane);

protected:

    void DrawCaptionBackground(wxDC& dc, const wxRect& rect, bool active);

    void DrawIcon(wxDC& dc, wxWindow *window, const wxRect& rect, wxAuiPaneInfo& pane);

    void InitBitmaps();

protected:

    wxPen m_borderPen;
    wxBrush m_sashBrush;
    wxBrush m_backgroundBrush;
    wxBrush m_gripperBrush;
    wxFont m_captionFont;
    wxBitmap m_inactiveCloseBitmap;
    wxBitmap m_inactivePinBitmap;
    wxBitmap m_inactiveMaximizeBitmap;
    wxBitmap m_inactiveRestoreBitmap;
    wxBitmap m_activeCloseBitmap;
    wxBitmap m_activePinBitmap;
    wxBitmap m_activeMaximizeBitmap;
    wxBitmap m_activeRestoreBitmap;
    wxPen m_gripperPen1;
    wxPen m_gripperPen2;
    wxPen m_gripperPen3;
    wxColour m_baseColour;
    wxColour m_activeCaptionColour;
    wxColour m_activeCaptionGradientColour;
    wxColour m_activeCaptionTextColour;
    wxColour m_inactiveCaptionColour;
    wxColour m_inactiveCaptionGradientColour;
    wxColour m_inactiveCaptionTextColour;
    int m_borderSize;
    int m_captionSize;
    int m_sashSize;
    int m_buttonSize;
    int m_gripperSize;
    int m_gradientType;
};

