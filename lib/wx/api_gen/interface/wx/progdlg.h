/////////////////////////////////////////////////////////////////////////////
// Name:        progdlg.h
// Purpose:     interface of wxProgressDialog
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#define wxPD_CAN_ABORT          0x0001
#define wxPD_APP_MODAL          0x0002
#define wxPD_AUTO_HIDE          0x0004
#define wxPD_ELAPSED_TIME       0x0008
#define wxPD_ESTIMATED_TIME     0x0010
#define wxPD_SMOOTH             0x0020
#define wxPD_REMAINING_TIME     0x0040
#define wxPD_CAN_SKIP           0x0080

/**
    @class wxGenericProgressDialog

    This class represents a dialog that shows a short message and a
    progress bar. Optionally, it can display ABORT and SKIP buttons, and
    the elapsed, remaining and estimated time for the end of the progress.

    This class provides a generic implementation of the progress dialog.  If
    the platform has a native progress dialog available then it will be
    accessible using the @a wxProgressDialog class, otherwise it will
    essentially be the same as this class.

    Note that you must be aware that wxProgressDialog internally calls
    wxEventLoopBase::YieldFor with @c wxEVT_CATEGORY_UI and @c wxEVT_CATEGORY_USER_INPUT
    and this may cause unwanted re-entrancies or the out-of-order processing
    of pending events (to help preventing the last problem if you're using
    wxProgressDialog in a multi-threaded application you should be sure to use
    wxThreadEvent for your inter-threads communications).

    Although wxProgressDialog is not really modal, it should be created on the
    stack, and not the heap, as other modal dialogs, e.g. use it like this:
    @code
        void MyFrame::SomeFunc()
        {
            wxProgressDialog dialog(...);
            for ( int i = 0; i < 100; ++i ) {
                if ( !dialog.Update(i)) {
                    // Cancelled by user.
                    break;
                }

                ... do something time-consuming (but not too much) ...
            }
        }
    @endcode
    Note that this becomes even more important if the dialog is instantiated
    during the program initialization, e.g. from wxApp::OnInit(): the dialog
    must be destroyed before the main event loop is started in this case.

    @beginStyleTable
    @style{wxPD_APP_MODAL}
           Make the progress dialog application-modal, i.e. disable all
           application windows while it is shown. If this flag is not given, it
           is only "locally" modal -- that is the input to the parent window is
           disabled, but not to the other ones.
    @style{wxPD_AUTO_HIDE}
           Causes the progress dialog to disappear from screen as soon as the
           maximum value of the progress meter has been reached.
           If this style is not present, the dialog will become a modal dialog
           (see wxDialog::ShowModal) once the maximum value has been reached
           and wait for the user to dismiss it.
    @style{wxPD_SMOOTH}
           Causes smooth progress of the gauge control (uses a wxGauge with the
           @c wxGA_SMOOTH style).
    @style{wxPD_CAN_ABORT}
           This flag tells the dialog that it should have a "Cancel" button
           which the user may press. If this happens, the next call to
           Update() will return @false.
    @style{wxPD_CAN_SKIP}
           This flag tells the dialog that it should have a "Skip" button
           which the user may press. If this happens, the next call to
           Update() will return @true in its skip parameter.
    @style{wxPD_ELAPSED_TIME}
           This flag tells the dialog that it should show elapsed time (since
           creating the dialog).
    @style{wxPD_ESTIMATED_TIME}
           This flag tells the dialog that it should show estimated time.
    @style{wxPD_REMAINING_TIME}
           This flag tells the dialog that it should show remaining time.
    @endStyleTable

    @library{wxcore}
    @category{cmndlg}
*/
class wxGenericProgressDialog : public wxDialog
{
public:
    /**
        Constructor. Creates the dialog, displays it and disables user input
        for other windows, or, if @c wxPD_APP_MODAL flag is not given, for its
        parent window only.

        @param title
            Dialog title to show in titlebar.
        @param message
            Message displayed above the progress bar.
        @param maximum
            Maximum value for the progress bar.
            In the generic implementation the progress bar is constructed
            only if this value is greater than zero.
        @param parent
            Parent window. It will be disabled while this dialog is shown if
            non-null (whether @c wxPD_APP_MODAL is specified or not). Note that
            if you specify null parent and don't use @c wxPD_APP_MODAL, you
            need to take care to avoid reentrancies, i.e. avoiding showing the
            progress dialog again while this one is shown.
        @param style
            The dialog style. See wxProgressDialog.
    */
    wxGenericProgressDialog(const wxString& title, const wxString& message,
                            int maximum = 100,
                            wxWindow* parent = NULL,
                            int style = wxPD_AUTO_HIDE | wxPD_APP_MODAL);

    /**
        Destructor. Deletes the dialog and enables all top level windows.
    */
    virtual ~wxGenericProgressDialog();

    /**
        Returns the last value passed to the Update() function or
        @c wxNOT_FOUND if the dialog has no progress bar.

        @since 2.9.0
    */
    int GetValue() const;

    /**
        Returns the maximum value of the progress meter, as passed to
        the constructor or @c wxNOT_FOUND if the dialog has no progress bar.

        @since 2.9.0
    */
    int GetRange() const;

    /**
        Returns the last message passed to the Update() function;
        if you always passed wxEmptyString to Update() then the message
        set through the constructor is returned.

        @since 2.9.0
    */
    wxString GetMessage() const;

    /**
        Like Update() but makes the gauge control run in indeterminate mode.

        In indeterminate mode the remaining and the estimated time labels (if
        present) are set to "Unknown" or to @a newmsg (if it's non-empty).
        Each call to this function moves the progress bar a bit to indicate
        that some progress was done.

        @see wxGauge::Pulse(), Update()
    */
    virtual bool Pulse(const wxString& newmsg = wxEmptyString, bool* skip = NULL);

    /**
        Can be used to continue with the dialog, after the user had clicked the "Abort" button.
    */
    void Resume();

    /**
        Changes the maximum value of the progress meter given in the constructor.
        This function can only be called (with a positive value) if the value passed
        in the constructor was positive.

        @since 2.9.1
    */
    void SetRange(int maximum);


      /**
         Returns true if the "Cancel" button was pressed.

         Normally a Cancel button press is indicated by Update() returning
         @false but sometimes it may be more convenient to check if the dialog
         was cancelled from elsewhere in the code and this function allows
         doing it.

         It always returns @false if the Cancel button is not shown at all.

         @since 2.9.1
     */
    bool WasCancelled() const;

     /**
         Returns true if the "Skip" button was pressed.

         This is similar to WasCancelled() but returns @true if the "Skip"
         button was pressed, not the "Cancel" one.

         @since 2.9.1
     */
    bool WasSkipped() const;


    /**
        Updates the dialog, setting the progress bar to the new value and
        updating the message if new one is specified.

        Returns @true unless the "Cancel" button has been pressed.

        If @false is returned, the application can either immediately destroy the
        dialog or ask the user for the confirmation and if the abort is not confirmed
        the dialog may be resumed with Resume() function.

        If @a value is the maximum value for the dialog, the behaviour of the
        function depends on whether @c wxPD_AUTO_HIDE was used when the dialog
        was created. If it was, the dialog is hidden and the function returns
        immediately. If it was not, the dialog becomes a modal dialog and waits
        for the user to dismiss it, meaning that this function does not return
        until this happens.

        Notice that if @a newmsg is longer than the currently shown message,
        the dialog will be automatically made wider to account for it. However
        if the new message is shorter than the previous one, the dialog doesn't
        shrink back to avoid constant resizes if the message is changed often.
        To do this and fit the dialog to its current contents you may call
        Fit() explicitly. However the native MSW implementation of this class
        does make the dialog shorter if the new text has fewer lines of text
        than the old one, so it is recommended to keep the number of lines of
        text constant in order to avoid jarring dialog size changes. You may
        also want to make the initial message, specified when creating the
        dialog, wide enough to avoid having to resize the dialog later, e.g. by
        appending a long string of unbreakable spaces (@c wxString(L'\\u00a0',
        100)) to it.

        @param value
            The new value of the progress meter. It should be less than or equal to
            the maximum value given to the constructor.
        @param newmsg
            The new messages for the progress dialog text, if it is
            empty (which is the default) the message is not changed.
        @param skip
            If "Skip" button was pressed since last Update() call,
            this is set to @true.
    */
    virtual bool Update(int value, const wxString& newmsg = wxEmptyString,
                        bool* skip = NULL);
};




/**
    @class wxProgressDialog

    If supported by the platform this class will provide the platform's native
    progress dialog, else it will simply be the @a wxGenericProgressDialog.
*/
class wxProgressDialog : public wxGenericProgressDialog
{
public:
    wxProgressDialog( const wxString& title, const wxString& message,
                      int maximum = 100,
                      wxWindow *parent = NULL,
                      int style = wxPD_APP_MODAL | wxPD_AUTO_HIDE );
};
