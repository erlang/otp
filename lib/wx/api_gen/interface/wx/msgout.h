/////////////////////////////////////////////////////////////////////////////
// Name:        wx/msgout.h
// Purpose:     interface of wxMessageOutput and derived classes
// Author:      Vadim Zeitlin
// Copyright:   (c) 2009 Vadim Zeitlin
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Simple class allowing to write strings to various output channels.

    wxMessageOutput is a low-level class and doesn't provide any of the
    conveniences of wxLog. It simply allows writing a message to some output
    channel: usually file or standard error but possibly also a message box.
    While use of wxLog and related functions is preferable in many cases
    sometimes this simple interface may be more convenient.

    This class itself is an abstract base class for various concrete derived
    classes:
        - wxMessageOutputStderr
        - wxMessageOutputBest
        - wxMessageOutputMessageBox
        - wxMessageOutputLog

    It also provides access to the global message output object which is
    created by wxAppTraits::CreateMessageOutput() which creates an object of
    class wxMessageOutputStderr in console applications and wxMessageOutputBest
    in the GUI ones but may be overridden in user-defined traits class.

    Example of using this class:
    @code
        wxMessageOutputDebug().Printf("name=%s, preparing to greet...", name);
        wxMessageOutput::Get()->Printf("Hello, %s!", name);
    @endcode

    @library{wxbase}
    @category{logging}
 */
class wxMessageOutput
{
public:
    /**
        Return the global message output object.

        This object is never @NULL while the program is running but may be
        @NULL during initialization (before wxApp object is instantiated) or
        shutdown.(after wxApp destruction).

        @see wxAppTraits::CreateMessageOutput()
     */
    static wxMessageOutput* Get();

    /**
        Sets the global message output object.

        Using this function may be a simpler alternative to changing the
        message output object used for your program than overriding
        wxAppTraits::CreateMessageOutput().

        Remember to delete the returned pointer or restore it later with
        another call to Set().
     */
    static wxMessageOutput* Set(wxMessageOutput* msgout);

    /**
        Output a message.

        This function uses the same conventions as standard @c printf().
     */
    void Printf(const wxString& format, ...);

    /**
        Method called by Printf() to really output the text.

        This method is overridden in various derived classes and is also the
        one you should override if you implement a custom message output
        object.

        It may also be called directly instead of Printf(). This is especially
        useful when outputting a user-defined string because it can be simply
        called with this string instead of using
        @code
            msgout.Printf("%s", str);
        @endcode
        (notice that passing user-defined string to Printf() directly is, of
        course, a security risk).
     */
    virtual void Output(const wxString& str) = 0;
};

/**
    Output messages to stderr or another STDIO file stream.

    Implements wxMessageOutput by using stderr or specified file.

    @library{wxbase}
    @category{logging}
 */
class wxMessageOutputStderr : public wxMessageOutput
{
public:
    /**
        Create a new message output object associated with standard error
        stream by default.

        @param fp
            Non-null STDIO file stream. Notice that this object does @e not
            take ownership of this pointer, i.e. the caller is responsible for
            both ensuring that its life-time is great er than life-time of this
            object and for deleting it if necessary.
     */
    wxMessageOutputStderr(FILE *fp = stderr);
};

/**
    Flags used with wxMessageOutputBest.

    See wxMessageOutputBest::wxMessageOutputBest().
 */
enum wxMessageOutputFlags
{
    wxMSGOUT_PREFER_STDERR = 0, ///< use stderr if available (this is the default)
    wxMSGOUT_PREFER_MSGBOX = 1  ///< always use message box if available
};

/**
    Output messages in the best possible way.

    Some systems (e.g. MSW) are capable of showing message boxes even from
    console programs. If this is the case, this class will use message box if
    standard error stream is not available (e.g. running console program not
    from console under Windows) or possibly even always, depending on the value
    of flags constructor argument.

    @library{wxbase}
    @category{logging}
 */
class wxMessageOutputBest : public wxMessageOutputStderr
{
public:
    /**
        Create a new message output object.

        @param flags
            May be either @c wxMSGOUT_PREFER_STDERR (default) meaning that
            standard error will be used if it's available (e.g. program is
            being run from console under Windows) or @c wxMSGOUT_PREFER_MSGBOX
            meaning that a message box will always be used if the current
            system supports showing message boxes from console programs
            (currently only Windows does).
     */
    wxMessageOutputBest(wxMessageOutputFlags flags = wxMSGOUT_PREFER_STDERR);
};

/**
    Output messages to the system debug output channel.

    Under MSW this class outputs messages to the so called debug output. Under
    the other systems it simply uses the standard error stream.

    @library{wxbase}
    @category{logging}
 */
class wxMessageOutputDebug : public wxMessageOutputStderr
{
public:
    /// Default constructor.
    wxMessageOutputDebug();
};

/**
    Output messages by showing them in a message box.

    This class is only available to GUI applications, unlike all the other
    wxMessageOutput-derived classes.

    @library{wxcore}
    @category{logging}
 */
class wxMessageOutputMessageBox : public wxMessageOutput
{
public:
    /// Default constructor.
    wxMessageOutputMessageBox();
};
