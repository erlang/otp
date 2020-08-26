/////////////////////////////////////////////////////////////////////////////
// Name:        wx/modalhook.h
// Purpose:     Public interface of wxModalDialogHook class.
// Author:      Vadim Zeitlin
// Copyright:   (c) 2013 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Allows intercepting all modal dialog calls.

    This class can be used to hook into normal modal dialog handling for some
    special needs. One of the most common use cases is for testing: as
    automatic tests can't continue if a modal dialog is shown while they run,
    this class can be used to avoid showing the modal dialogs during unattended
    execution. wxModalDialogHook can also be used for disabling some background
    operation while a modal dialog is shown.

    To install a modal dialog hook, you need to derive your own class from this
    one and implement its pure virtual Enter() method. Then simply create an
    object of your class and call Register() on it to start receiving calls to
    your overridden Enter() (and possibly Exit()) methods:
    @code
        class MyModalDialogHook : public wxModalDialogHook
        {
        protected:
            virtual int Enter(wxDialog* dialog)
            {
                // Just for demonstration purposes, intercept all uses of
                // wxFileDialog. Notice that this doesn't provide any real
                // sandboxing, of course, the program can still read and write
                // files by not using wxFileDialog to ask the user for their
                // names.
                if ( wxDynamicCast(dialog, wxFileDialog) )
                {
                    wxLogError("Access to file system disallowed.");

                    // Skip showing the file dialog entirely.
                    return wxID_CANCEL;
                }

                m_lastEnter = wxDateTime::Now();

                // Allow the dialog to be shown as usual.
                return wxID_NONE;
            }

            virtual void Exit(wxDialog* dialog)
            {
                // Again, just for demonstration purposes, show how long did
                // the user take to dismiss the dialog. Notice that we
                // shouldn't use wxLogMessage() here as this would result in
                // another modal dialog call and hence infinite recursion. In
                // general, the hooks should be as unintrusive as possible.
                wxLogDebug("%s dialog took %s to be dismissed",
                           dialog->GetClassInfo()->GetClassName(),
                           (wxDateTime::Now() - m_lastEnter).Format());
            }
        };

        class MyApp : public wxApp
        {
        public:
            virtual bool OnInit()
            {
                ...
                m_myHook.Register();
                ...
            }

        private:
            MyModalDialogHook m_myHook;
        };
    @endcode

    @since 2.9.5
 */
class wxModalDialogHook
{
public:
    /**
        Default and trivial constructor.

        The constructor doesn't do anything, call Register() to make this hook
        active.
     */
    wxModalDialogHook();

    /**
        Destructor unregisters the hook if it's currently active.
     */
    virtual ~wxModalDialogHook();

    /**
        Register this hook as being active.

        After registering the hook, its Enter() and Exit() methods will be
        called whenever a modal dialog is shown.

        Notice that the order of registration matters: the last hook registered
        is called first, and if its Enter() returns a value different from
        ::wxID_NONE, the subsequent hooks are skipped.

        It is an error to register the same hook twice.
     */
    void Register();

    /**
        Unregister this hook.

        Notice that is done automatically from the destructor, so usually
        calling this method explicitly is unnecessary.

        The hook must be currently registered.
     */
    void Unregister();

protected:
    /**
        Called by wxWidgets before showing any modal dialogs.

        Override this to be notified whenever a modal dialog is about to be
        shown.

        If the return value of this method is ::wxID_NONE, the dialog is shown
        as usual and Exit() will be called when it is dismissed. If the return
        value is anything else, the dialog is not shown at all and its
        wxDialog::ShowModal() simply returns with the given result. In this
        case, Exit() won't be called neither.

        @param dialog The dialog about to be shown, never @NULL.
        @return wxID_NONE to continue with showing the dialog or anything else
            to skip showing the dialog and just return this value from its
            ShowModal().
     */
    virtual int Enter(wxDialog* dialog) = 0;

    /**
        Called by wxWidgets after dismissing the modal dialog.

        Notice that it won't be called if Enter() hadn't been called because
        another modal hook, registered after this one, intercepted the dialog
        or if our Enter() was called but returned a value different from
        ::wxID_NONE.

        @param dialog The dialog that was shown and dismissed, never @NULL.
     */
    virtual void Exit(wxDialog* dialog);
};
