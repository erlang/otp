/////////////////////////////////////////////////////////////////////////////
// Name:        clipbrd.h
// Purpose:     interface of wxClipboard
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    The backwards compatible access macro that returns the global clipboard
    object pointer.
*/
#define wxTheClipboard

/**
    @class wxClipboard

    A class for manipulating the clipboard.

    To use the clipboard, you call member functions of the global
    ::wxTheClipboard object.

    See the @ref overview_dataobject for further information.

    Call wxClipboard::Open() to get ownership of the clipboard. If this
    operation returns @true, you now own the clipboard. Call
    wxClipboard::SetData() to put data on the clipboard, or
    wxClipboard::GetData() to retrieve data from the clipboard. Call
    wxClipboard::Close() to close the clipboard and relinquish ownership. You
    should keep the clipboard open only momentarily.

    For example:

    @code
    // Write some text to the clipboard
    if (wxTheClipboard->Open())
    {
        // This data objects are held by the clipboard,
        // so do not delete them in the app.
        wxTheClipboard->SetData( new wxTextDataObject("Some text") );
        wxTheClipboard->Close();
    }

    // Read some text
    if (wxTheClipboard->Open())
    {
        if (wxTheClipboard->IsSupported( wxDF_TEXT ))
        {
            wxTextDataObject data;
            wxTheClipboard->GetData( data );
            wxMessageBox( data.GetText() );
        }
        wxTheClipboard->Close();
    }
    @endcode

    @note On GTK, the clipboard behavior can vary depending on the configuration of
          the end-user's machine. In order for the clipboard data to persist after
          the window closes, a clipboard manager must be installed. Some clipboard
          managers will automatically flush the clipboard after each new piece of
          data is added, while others will not. The @Flush() function will force
          the clipboard manager to flush the data.

    @library{wxcore}
    @category{dnd}

    @see @ref overview_dnd, @ref overview_dataobject, wxDataObject
*/
class wxClipboard : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxClipboard();

    /**
        Destructor.
    */
    virtual ~wxClipboard();

    /**
        Call this function to add the data object to the clipboard.

        This is an obsolete synonym for SetData().
    */
    virtual bool AddData(wxDataObject* data);

    /**
        Clears the global clipboard object and the system's clipboard if
        possible.
    */
    virtual void Clear();

    /**
        Call this function to close the clipboard, having opened it with
        Open().
    */
    virtual void Close();

    /**
        Flushes the clipboard: this means that the data which is currently on
        clipboard will stay available even after the application exits
        (possibly eating memory), otherwise the clipboard will be emptied on
        exit.

        Currently this method is implemented in MSW and GTK and always returns @false
        otherwise.

        @note On GTK, only the non-primary selection can be flushed. Calling this function
              when the clipboard is using the primary selection will return @false and not
              make any data available after the program exits.

        @return @false if the operation is unsuccessful for any reason.
    */
    virtual bool Flush();

    /**
        Call this function to fill @a data with data on the clipboard, if
        available in the required format. Returns @true on success.
    */
    virtual bool GetData(wxDataObject& data);

    /**
        Returns @true if the clipboard has been opened.
    */
    virtual bool IsOpened() const;

    /**
        Returns @true if there is data which matches the data format of the
        given data object currently @b available on the clipboard.

        @todo The name of this function is misleading. This should be renamed
              to something that more accurately indicates what it does.
    */
    virtual bool IsSupported(const wxDataFormat& format);

    /**
        Returns @true if we are using the primary selection, @false if
        clipboard one.

        @see UsePrimarySelection()
    */
    bool IsUsingPrimarySelection() const;

    /**
        Call this function to open the clipboard before calling SetData() and
        GetData().

        Call Close() when you have finished with the clipboard. You should keep
        the clipboard open for only a very short time.

        @return @true on success. This should be tested (as in the sample
                shown above).
    */
    virtual bool Open();

    /**
        Call this function to set the data object to the clipboard.

        The new data object replaces any previously set one, so if the
        application wants to provide clipboard data in several different
        formats, it must use a composite data object supporting all of the
        formats instead of calling this function several times with different
        data objects as this would only leave data from the last one in the
        clipboard.

        After this function has been called, the clipboard owns the data, so do
        not delete the data explicitly.
    */
    virtual bool SetData(wxDataObject* data);

    /**
        On platforms supporting it (all X11-based ports), wxClipboard uses the
        CLIPBOARD X11 selection by default. When this function is called with
        @true, all subsequent clipboard operations will use PRIMARY selection
        until this function is called again with @false.

        On the other platforms, there is no PRIMARY selection and so all
        clipboard operations will fail. This allows implementing the standard
        X11 handling of the clipboard which consists in copying data to the
        CLIPBOARD selection only when the user explicitly requests it (i.e. by
        selecting the "Copy" menu command) but putting the currently selected
        text into the PRIMARY selection automatically, without overwriting the
        normal clipboard contents with the currently selected text on the other
        platforms.
    */
    virtual void UsePrimarySelection(bool primary = false);

    /**
       Returns the global instance (wxTheClipboard) of the clipboard object.
    */
    static wxClipboard *Get();

};

