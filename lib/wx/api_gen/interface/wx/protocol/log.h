///////////////////////////////////////////////////////////////////////////////
// Name:        wx/protocol/log.h
// Purpose:     interface of wxProtocolLog
// Author:      Vadim Zeitlin
// Created:     2009-03-06
// Copyright:   (c) 2009 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/**
    Class allowing to log network operations performed by wxProtocol.

    @library{wxnet}
    @category{net}

    @see wxProtocol
*/
class wxProtocolLog
{
public:
    /**
        Create object doing the logging using wxLogTrace() with the specified
        trace mask.

        If you override DoLogString() in your class the @a traceMask may be
        left empty but it must have a valid value if you rely on the default
        DoLogString() implementation.
     */
    wxProtocolLog(const wxString& traceMask);

    /**
        Called by wxProtocol-derived objects to log strings sent to the server.

        Default implementation prepends a client-to-server marker to @a str and
        calls DoLogString().
     */
    virtual void LogRequest(const wxString& str);

    /**
        Called by wxProtocol-derived objects to log strings received from the
        server.

        Default implementation prepends a server-to-client marker to @a str and
        calls DoLogString().
     */
    virtual void LogResponse(const wxString& str);

protected:
    /**
        Log the given string.

        This function is called from LogRequest() and LogResponse() and by
        default uses wxLogTrace() with the trace mask specified in the
        constructor but can be overridden to do something different by the
        derived classes.
     */
    virtual void DoLogString(const wxString& str);
};


