/////////////////////////////////////////////////////////////////////////////
// Name:        validate.h
// Purpose:     interface of wxValidator
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxValidator

    wxValidator is the base class for a family of validator classes that
    mediate between a class of control, and application data.

    A validator has three major roles:

    -#  To transfer data from a C++ variable or own storage to and from a
        control.
    -#  To validate data in a control, and show an appropriate error message.
    -#  To filter events (such as keystrokes), thereby changing the behaviour
        of the associated control.

    Validators can be plugged into controls dynamically.

    To specify a default, "null" validator, use ::wxDefaultValidator.

    For more information, please see @ref overview_validator.

    @library{wxcore}
    @category{validator}

    @stdobjects
    ::wxDefaultValidator

    @see @ref overview_validator, wxTextValidator, wxGenericValidator,
        wxIntegerValidator, wxFloatingPointValidator
*/
class wxValidator : public wxEvtHandler
{
public:
    /**
        Constructor.
    */
    wxValidator();

    /**
        Destructor.
    */
    virtual ~wxValidator();

    /**
        All validator classes must implement the Clone() function, which
        returns an identical copy of itself.

        This is because validators are passed to control constructors as
        references which must be copied. Unlike objects such as pens and
        brushes, it does not make sense to have a reference counting scheme to
        do this cloning because all validators should have separate data.

        @return This base function returns @NULL.
    */
    virtual wxObject* Clone() const;

    /**
        Returns the window associated with the validator.
    */
    wxWindow* GetWindow() const;

    /**
        This functions switches on or turns off the error sound produced by the
        validators if an invalid key is pressed.

        @since 2.9.1

        @param suppress
            If @true, error sound is not played when a validator detects an
            error. If @false, error sound is enabled.
    */
    static void SuppressBellOnError(bool suppress = true);

    /**
       Returns if the error sound is currently disabled.
    */
    static bool IsSilent();

    /**
        Associates a window with the validator.

        This function is automatically called by wxWidgets when creating a
        wxWindow-derived class instance which takes a wxValidator reference.
        Since wxWidgets 3.1.1, it can be overridden in custom validators in
        order to perform any one-time initialization or checks of the window
        when the validator is associated with it.

        E.g.
        @code
        new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0,
                       wxTextValidator(wxFILTER_ALPHA, &g_data.m_string));
        @endcode
        will automatically link the wxTextValidator instance with the wxTextCtrl
        instance and call SetWindow() method on the wxTextValidator object.
    */
    virtual void SetWindow(wxWindow* window);

    /**
        This overridable function is called when the value in the window must
        be transferred to the validator.

        @return @false if there is a problem.
    */
    virtual bool TransferFromWindow();

    /**
        This overridable function is called when the value associated with the
        validator must be transferred to the window.

        @return @false if there is a problem.
    */
    virtual bool TransferToWindow();

    /**
        This overridable function is called when the value in the associated
        window must be validated.

        @param parent
            The parent of the window associated with the validator.

        @return @false if the value in the window is not valid; you may pop up
                an error dialog.
    */
    virtual bool Validate(wxWindow* parent);
};

/**
    An empty, "null" wxValidator instance.
*/
const wxValidator wxDefaultValidator;

