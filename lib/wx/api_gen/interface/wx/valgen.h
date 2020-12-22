/////////////////////////////////////////////////////////////////////////////
// Name:        valgen.h
// Purpose:     interface of wxGenericValidator
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxGenericValidator

    wxGenericValidator performs data transfer (but not validation or filtering)
    for many type of controls.

    wxGenericValidator supports:
    - wxButton, wxRadioButton, wxToggleButton, wxBitmapToggleButton, wxSpinButton
    - wxCheckBox, wxRadioBox, wxComboBox, wxListBox, wxCheckListBox
    - wxGauge, wxSlider, wxScrollBar, wxChoice, wxStaticText
    - wxSpinCtrl, wxTextCtrl

    It checks the type of the window and uses an appropriate type for it.
    For example, wxButton and wxTextCtrl transfer data to and from a
    wxString variable; wxListBox uses a wxArrayInt; wxCheckBox uses a boolean.

    For more information, please see @ref overview_validator.

    @library{wxcore}
    @category{validator}

    @see @ref overview_validator, wxValidator, wxTextValidator,
        wxIntegerValidator, wxFloatingPointValidator
*/
class wxGenericValidator : public wxValidator
{
public:
    /**
        Copy constructor.

        @param validator
            Validator to copy.
    */
    wxGenericValidator(const wxGenericValidator& validator);
    /**
        Constructor taking a bool pointer. This will be used for wxCheckBox,
        wxRadioButton, wxToggleButton and wxBitmapToggleButton.

        @param valPtr
            A pointer to a variable that contains the value. This variable
            should have a lifetime equal to or longer than the validator
            lifetime (which is usually determined by the lifetime of the
            window).
    */
    wxGenericValidator(bool* valPtr);
    /**
        Constructor taking a wxString pointer. This will be used for wxButton,
        wxComboBox, wxStaticText, wxTextCtrl.

        @param valPtr
            A pointer to a variable that contains the value. This variable
            should have a lifetime equal to or longer than the validator
            lifetime (which is usually determined by the lifetime of the
            window).
    */
    wxGenericValidator(wxString* valPtr);
    /**
        Constructor taking an integer pointer. This will be used for wxChoice,
        wxGauge, wxScrollBar, wxRadioBox, wxSlider, wxSpinButton and
        wxSpinCtrl.

        @param valPtr
            A pointer to a variable that contains the value. This variable
            should have a lifetime equal to or longer than the validator
            lifetime (which is usually determined by the lifetime of the
            window).
    */
    wxGenericValidator(int* valPtr);
    /**
        Constructor taking a wxArrayInt pointer. This will be used for
        wxListBox, wxCheckListBox.

        @param valPtr
            A pointer to a variable that contains the value. This variable
            should have a lifetime equal to or longer than the validator
            lifetime (which is usually determined by the lifetime of the
            window).
    */
    wxGenericValidator(wxArrayInt* valPtr);
    /**
        Constructor taking a wxDateTime pointer. This will be used for
        wxDatePickerCtrl.

        @param valPtr
            A pointer to a variable that contains the value. This variable
            should have a lifetime equal to or longer than the validator
            lifetime (which is usually determined by the lifetime of the
            window).
    */
    wxGenericValidator(wxDateTime* valPtr);
    /**
        Constructor taking a wxFileName pointer. This will be used for
        wxTextCtrl.

        @param valPtr
            A pointer to a variable that contains the value. This variable
            should have a lifetime equal to or longer than the validator
            lifetime (which is usually determined by the lifetime of the
            window).
        @since 2.9.3
    */
    wxGenericValidator(wxFileName* valPtr);
    /**
        Constructor taking a float pointer. This will be used for
        wxTextCtrl.

        @param valPtr
            A pointer to a variable that contains the value. This variable
            should have a lifetime equal to or longer than the validator
            lifetime (which is usually determined by the lifetime of the
            window).
        @since 2.9.3
    */
    wxGenericValidator(float* valPtr);
    /**
        Constructor taking a double pointer. This will be used for
        wxTextCtrl.

        @param valPtr
            A pointer to a variable that contains the value. This variable
            should have a lifetime equal to or longer than the validator
            lifetime (which is usually determined by the lifetime of the
            window).
        @since 2.9.3
    */
    wxGenericValidator(double* valPtr);

    /**
        Destructor.
    */
    virtual ~wxGenericValidator();

    /**
        Clones the generic validator using the copy constructor.
    */
    virtual wxObject* Clone() const;

    /**
        Transfers the value from the window to the appropriate data type.
    */
    virtual bool TransferFromWindow();

    /**
        Transfers the value to the window.
    */
    virtual bool TransferToWindow();
};

