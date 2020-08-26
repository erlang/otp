///////////////////////////////////////////////////////////////////////////////
// Name:        wx/activityindicator.h
// Purpose:     wxActivityIndicator documentation.
// Author:      Vadim Zeitlin
// Created:     2015-03-05
// Copyright:   (c) 2015 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    @class wxActivityIndicator

    Small control showing an animation indicating that the program is currently
    busy performing some background task.

    @since 3.1.0

    @library{wxcore}
    @category{ctrl}
    @appearance{activityindicator}
 */
class wxActivityIndicator : public wxControl
{
public:
    /**
        Default constructor.

        Use Create() to really create the control after using this constructor.
     */
    wxActivityIndicator();

    /**
        Constructor fully creating the control.

        The arguments have the usual meanings and only @a parent is typically
        required.
     */
    explicit wxActivityIndicator(wxWindow* parent,
                                 wxWindowID winid = wxID_ANY,
                                 const wxPoint& pos = wxDefaultPosition,
                                 const wxSize& size = wxDefaultSize,
                                 long style = 0,
                                 const wxString& name = "activityindicator");

    /**
        Create the control initialized using the default constructor.

        This method can be used to finish the control creation if it hadn't
        been done already by using the non-default constructor.
     */
    bool Create(wxWindow* parent,
                wxWindowID winid = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxString& name = "activityindicator");

    /**
        Starts animation of the indicator.

        Does nothing if the indicator is already running.
     */
    void Start();

    /**
        Stops the animation of the indicator.

        Notice that the animation is stopped even if Start() had been called
        multiple times before, i.e. the calls are not cumulative.
     */
    void Stop();

    /**
        Returns true if the control is currently showing activity.

        Returns @false initially, @true once Start() is called and @false again
        after calling Stop().
     */
    bool IsRunning() const;
};
