/////////////////////////////////////////////////////////////////////////////
// Name:        wx/valnum.h
// Purpose:     Documentation of numeric validator classes.
// Author:      Vadim Zeitlin based on the submission of Fulvio Senore
// Created:     2010-11-06
// Copyright:   (c) 2010 wxWidgets team
//              (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Bit masks used for numeric validator styles.

    A combination of these flags can be used when creating wxIntegerValidator
    and wxFloatingPointValidator objects and with their SetStyle() methods.

    @since 2.9.2
    @category{validator}
 */
enum wxNumValidatorStyle
{
    /**
        Indicates absence of any other flags.

        This value corresponds to the default behaviour.
     */
    wxNUM_VAL_DEFAULT = 0,

    /**
        Use thousands separators in the numbers.

        When this style is used, numbers are formatted using the thousands
        separators after validating the user entry (if the current locale uses
        the thousands separators character).
     */
    wxNUM_VAL_THOUSANDS_SEPARATOR = 1,

    /**
        Show a value of zero as an empty string.

        With this style a value of zero in the associated variable is
        translated to an empty string and an empty value of the control is
        translated to a value of zero.
     */
    wxNUM_VAL_ZERO_AS_BLANK = 2,

    /**
        Remove trailing zeroes from the fractional part of the number.

        This style can only be used with wxFloatingPointValidator and indicates
        that trailing zeroes should be removed from the control text when it is
        validated. By default, as many zeroes as needed to satisfy the
        precision used when creating the validator will be appended.

        For example, without this style a wxFloatingPointValidator with a
        precision 3 will show the value of 1.5 as "1.500" after validation.
        With this style, the value will be shown as just "1.5" (while a value
        of e.g. 1.567 will still be shown with all the three significant
        digits, of course).
     */
    wxNUM_VAL_NO_TRAILING_ZEROES

};

/**
    wxNumValidator is the common base class for numeric validator classes.

    This class is never used directly, but only as a base class for
    wxIntegerValidator and wxFloatingPointValidator.

    @tparam T
        Type of the values used with this validator.

    @category{validator}

    @since 2.9.2
 */
template <typename T>
class wxNumValidator : public wxValidator
{
public:
    /// Type of the values this validator is used with.
    typedef T ValueType;

    /**
        Sets the minimal value accepted by the validator.

        This value is inclusive, i.e. the value equal to @a min is accepted.
     */
    void SetMin(ValueType min);

    /**
        Gets the minimal value accepted by the validator.

        @since 3.1.3
     */
    ValueType GetMin() const;

    /**
        Sets the maximal value accepted by the validator.

        This value is inclusive, i.e. the value equal to @a max is accepted.
     */
    void SetMax(ValueType max);

    /**
        Gets the maximum value accepted by the validator.

        @since 3.1.3
     */
    ValueType GetMax() const;

    /**
        Sets both minimal and maximal values accepted by the validator.

        Calling this is equivalent to calling both SetMin() and SetMax().
     */
    void SetRange(ValueType min, ValueType max);

    /**
        Gets both minimal and maximal values accepted by the validator.

        @since 3.1.3
     */
    void GetRange(ValueType& min, ValueType& max) const;

    /**
        Change the validator style.

        Can be used to change the style of the validator after its creation.
        The @a style parameter must be a combination of the values from
        wxNumValidatorStyle enum.
     */
    void SetStyle(int style);


    /**
        Override base class method to format the control contents.

        This method is called when the associated window is shown and it fills
        it with the contents of the associated variable, if any, formatted
        according to the validator style.

        It does nothing if there is no associated variable.
     */
    virtual bool TransferToWindow();

    /**
        Override base class method to validate the control contents.

        This method is called to check the correctness of user input and fill
        the associated variable with the controls numeric value. It returns
        false if it is not a number in the configured range or if the control
        contents is empty for a validator without wxNUM_VAL_ZERO_AS_BLANK
        style.

        It does nothing if there is no associated variable.
     */
    virtual bool TransferFromWindow();
};

/**
    Validator for text entries used for integer entry.

    This validator can be used with wxTextCtrl or wxComboBox (and potentially
    any other class implementing wxTextEntry interface) to check that only
    valid integer values can be entered into them.

    This is a template class which can be instantiated for all the integer types
    (i.e. @c short, @c int, @c long and <code>long long</code> if available) as
    well as their unsigned versions.

    By default this validator accepts any integer values in the range
    appropriate for its type, e.g. <code>INT_MIN..INT_MAX</code> for @c int or
    <code>0..USHRT_MAX</code> for <code>unsigned short</code>. This range can
    be restricted further by calling SetMin() and SetMax() or SetRange()
    methods inherited from the base class.

    When the validator displays integers with thousands separators, the
    character used for the separators (usually "." or ",") depends on the locale
    set with wxLocale (note that you shouldn't change locale with setlocale()
    as this can result in a mismatch between the thousands separator used by
    wxLocale and the one used by the run-time library).

    A simple example of using this class:
    @code
        class MyDialog : public wxDialog
        {
        public:
            MyDialog()
            {
                ...
                // Allow positive integers and display them with thousands
                // separators.
                wxIntegerValidator<unsigned long>
                    val(&m_value, wxNUM_VAL_THOUSANDS_SEPARATOR);

                // If the variable were of type "long" and not "unsigned long"
                // we would have needed to call val.SetMin(0) but as it is,
                // this is not needed.

                // Associate it with the text control:
                new wxTextCtrl(this, ..., val);
            }

        private:
            unsigned long m_value;
        };
    @endcode
    For more information, please see @ref overview_validator.

    @library{wxcore}
    @category{validator}

    @see @ref overview_validator, wxValidator, wxGenericValidator,
    wxTextValidator, wxMakeIntegerValidator()

    @since 2.9.2
*/
template <typename T>
class wxIntegerValidator : public wxNumValidator<T>
{
public:
    /// Type of the values this validator is used with.
    typedef T ValueType;

    /**
        Validator constructor.

        @param value
            A pointer to the variable associated with the validator. If non
            @NULL, this variable should have a lifetime equal to or longer than
            the validator lifetime (which is usually determined by the lifetime
            of the window).
        @param style
            A combination of wxNumValidatorStyle enum values with the exception
            of wxNUM_VAL_NO_TRAILING_ZEROES which can't be used here.
    */
    wxIntegerValidator(ValueType *value = NULL, int style = wxNUM_VAL_DEFAULT);
};

/**
    Creates a wxIntegerValidator object with automatic type deduction.

    This function can be used to create wxIntegerValidator object without
    explicitly specifying its type, e.g. write just:
    @code
        new wxTextCtrl(..., wxMakeIntegerValidator(&m_var));
    @endcode
    instead of more verbose
    @code
        new wxTextCtrl(..., wxIntegerValidator<unsigned long>(&m_var));
    @endcode

    @since 2.9.2
 */
template <typename T>
wxIntegerValidator<T>
wxMakeIntegerValidator(T *value, int style = wxNUM_VAL_DEFAULT);

/**
    Validator for text entries used for floating point numbers entry.

    This validator can be used with wxTextCtrl or wxComboBox (and potentially
    any other class implementing wxTextEntry interface) to check that only
    valid floating point values can be entered into them. Currently only fixed
    format is supported on input, i.e. scientific format with mantissa and
    exponent is not supported.

    This template class can be instantiated for either @c float or @c double,
    <code>long double</code> values are not currently supported.

    Similarly to wxIntegerValidator<>, the range for the accepted values is by
    default set appropriately for the type. Additionally, this validator allows
    to specify the maximum number of digits that can be entered after the
    decimal separator. By default this is also set appropriately for the type
    used, e.g. 6 for @c float and 15 for @c double on a typical IEEE-754-based
    implementation. As with the range, the precision can be restricted after
    the validator creation if necessary.

    When the validator displays numbers with decimal or thousands separators,
    the characters used for the separators (usually "." or ",") depend on the
    locale set with wxLocale (note that you shouldn't change locale with
    setlocale() as this can result in a mismatch between the separators used by
    wxLocale and the one used by the run-time library).

    A simple example of using this class:
    @code
        class MyDialog : public wxDialog
        {
        public:
            MyDialog()
            {
                ...
                // Allow floating point numbers from 0 to 100 with 2 decimal
                // digits only and handle empty string as 0 by default.
                wxFloatingPointValidator<float>
                    val(2, &m_value, wxNUM_VAL_ZERO_AS_BLANK);
                val.SetRange(0, 100);

                // Associate it with the text control:
                new wxTextCtrl(this, ..., val);
            }

        private:
            float m_value;
        };
    @endcode

    For more information, please see @ref overview_validator.

    @library{wxcore}
    @category{validator}

    @see @ref overview_validator, wxValidator, wxGenericValidator,
    wxTextValidator, wxMakeIntegerValidator()

    @since 2.9.2
*/
template <typename T>
class wxFloatingPointValidator : public wxNumValidator<T>
{
public:
    /// Type of the values this validator is used with.
    typedef T ValueType;

    /**
        Constructor for validator using the default precision.

        @param value
            A pointer to the variable associated with the validator. If non
            @NULL, this variable should have a lifetime equal to or longer than
            the validator lifetime (which is usually determined by the lifetime
            of the window).
        @param style
            A combination of wxNumValidatorStyle enum values.
    */
    wxFloatingPointValidator(ValueType *value = NULL,
                             int style = wxNUM_VAL_DEFAULT);

    /**
        Constructor for validator specifying the precision.

        @param value
            A pointer to the variable associated with the validator. If non
            @NULL, this variable should have a lifetime equal to or longer than
            the validator lifetime (which is usually determined by the lifetime
            of the window).
        @param style
            A combination of wxNumValidatorStyle enum values.
        @param precision
            The number of decimal digits after the decimal separator to show
            and accept.
    */
    wxFloatingPointValidator(int precision,
                             ValueType *value = NULL,
                             int style = wxNUM_VAL_DEFAULT);


    /**
        Set precision.

        Precision is the number of digits shown (and accepted on input)
        after the decimal point. By default this is set to the maximal
        precision supported by the type handled by the validator in its
        constructor.
     */
    void SetPrecision(unsigned precision);

    /**
        Set factor used for displaying the value.

        The value associated with the validator is multiplied by the factor
        before displaying it and divided by it when retrieving its value from
        the control. By default, the @a factor is 1, so the actual value is not
        affected by it, but it can be set to, for example, 100, to display the
        value in percents while still storing it as absolute value.

        @since 3.1.1
     */
    void SetFactor(double factor);
};

/**
    Creates a wxFloatingPointValidator object with automatic type deduction.

    Similarly to wxMakeIntegerValidator(), this function allows avoiding
    explicitly specifying the validator type.

    @since 2.9.2
 */
//@{
template <typename T>
inline wxFloatingPointValidator<T>
wxMakeFloatingPointValidator(T *value, int style = wxNUM_VAL_DEFAULT);

template <typename T>
inline wxFloatingPointValidator<T>
wxMakeFloatingPointValidator(int precision,
                             T *value, int style = wxNUM_VAL_DEFAULT);
//@}
