///////////////////////////////////////////////////////////////////////////////
// Name:        interface/wx/eventfilter.h
// Purpose:     wxEventFilter class documentation
// Author:      Vadim Zeitlin
// Created:     2011-11-21
// Copyright:   (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    A global event filter for pre-processing all the events generated in the
    program.

    This is a very simple class which just provides FilterEvent() virtual
    method to be called by wxEvtHandler before starting process of any event.
    Thus, inheriting from this class and overriding FilterEvent() allows
    capturing and possibly handling or ignoring all the events happening in the
    program. Of course, having event filters adds additional overhead to every
    event processing and so should not be used lightly and your FilterEvent()
    code should try to return as quickly as possible, especially for the events
    it is not interested in.

    An example of using this class:
    @code
    // This class allows determining the last time the user has worked with
    // this application:
    class LastActivityTimeDetector : public wxEventFilter
    {
    public:
        LastActivityTimeDetector()
        {
            wxEvtHandler::AddFilter(this);

            m_last = wxDateTime::Now();
        }

        virtual ~LastActivityTimeDetector()
        {
            wxEvtHandler::RemoveFilter(this);
        }

        virtual int FilterEvent(wxEvent& event)
        {
            // Update the last user activity
            const wxEventType t = event.GetEventType();
            if ( t == wxEVT_KEY_DOWN || t == wxEVT_MOTION ||
                    t == wxEVT_LEFT_DOWN ||
                        t == wxEVT_RIGHT_DOWN ||
                            t == wxEVT_MIDDLE_DOWN )
            {
                m_last = wxDateTime::Now();
            }

            // Continue processing the event normally as well.
            return Event_Skip;
        }

        // This function could be called periodically from some timer to
        // do something (e.g. hide sensitive data or log out from remote
        // server) if the user has been inactive for some time period.
        bool IsInactiveFor(const wxTimeSpan& diff) const
        {
            return wxDateTime::Now() - diff > m_last;
        }

    private:
        wxDateTime m_last;
    };
    @endcode

    Notice that wxApp derives from wxEventFilter and is registered as an event
    filter during its creation so you may also override FilterEvent() method in
    your wxApp-derived class and, in fact, this is often the most convenient
    way to do it. However creating a new class deriving directly from
    wxEventFilter allows isolating the event filtering code in its own
    separate class and also having several independent filters, if necessary.

    @category{events}

    @since 2.9.3
 */
class wxEventFilter
{
public:
    /// Possible return values for FilterEvent().
    enum
    {
        /// Process event as usual.
        Event_Skip = -1,

        /// Don't process the event normally at all.
        Event_Ignore = 0,

        /// Event was already handled, don't process it normally.
        Event_Processed = 1
    };

    /**
        Default constructor.

        Constructor does not register this filter using
        wxEvtHandler::AddFilter(), it's your responsibility to do it when
        necessary.

        Notice that the objects of this class can't be copied.
     */
    wxEventFilter();

    /**
        Destructor.

        You must call wxEvtHandler::RemoveFilter() before destroying this
        object (possibly from the derived class destructor), failure to do this
        is indicated by an assert unless assertions are disabled.
     */
    virtual ~wxEventFilter();

    /**
        Override this method to implement event pre-processing.

        This method allows filtering all the events processed by the program,
        so you should try to return quickly from it to avoid slowing down the
        program to a crawl.

        Although the return type of this method is @c int, this is only due to
        backwards compatibility concerns and the actual return value must be
        one of the @c Event_XXX constants defined above:
            - Event_Skip to continue processing the event normally (this should
            be used in most cases).
            - Event_Ignore to not process this event at all (this can be used
            to suppress some events).
            - Event_Processed to not process this event normally but indicate
            that it was already processed by the event filter and so no default
            processing should take place neither (this should only be used if
            the filter really did process the event).
     */
    virtual int FilterEvent(wxEvent& event) = 0;
};
