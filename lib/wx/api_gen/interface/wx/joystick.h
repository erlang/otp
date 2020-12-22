/////////////////////////////////////////////////////////////////////////////
// Name:        joystick.h
// Purpose:     interface of wxJoystick
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxJoystick

    wxJoystick allows an application to control one or more joysticks.

    @library{wxcore}
    @category{misc}

    @see wxJoystickEvent
*/
class wxJoystick : public wxObject
{
public:
    /**
        Constructor.

        @a joystick may be one of wxJOYSTICK1, wxJOYSTICK2, indicating the joystick
        controller of interest.
    */
    wxJoystick(int joystick = wxJOYSTICK1);

    /**
        Destroys the wxJoystick object.
    */
    virtual ~wxJoystick();

    /**
        Returns the state of the joystick buttons.
        Every button is mapped to a single bit in the returned integer, with the
        first button being mapped to the least significant bit, and so on.

        A bitlist of wxJOY_BUTTONn identifiers, where n is 1, 2, 3 or 4 is available
        for historical reasons.
    */
    int GetButtonState() const;

    /**
        Returns the state of the specified joystick button.

        @param id
            The button id to report, from 0 to GetNumberButtons() - 1
    */
    bool GetButtonState(unsigned int id) const;

    /**
        Returns the manufacturer id.
    */
    int GetManufacturerId() const;

    /**
        Returns the movement threshold, the number of steps outside which the joystick
        is deemed to have
        moved.
    */
    int GetMovementThreshold() const;

    /**
        Returns the number of axes for this joystick.
    */
    int GetNumberAxes() const;

    /**
        Returns the number of buttons for this joystick.
    */
    int GetNumberButtons() const;

    /**
        Returns the number of joysticks currently attached to the computer.
    */
    static int GetNumberJoysticks();

    /**
        Returns the point-of-view position, expressed in continuous, one-hundredth of a
        degree units.

        Returns -1 on error.
    */
    int GetPOVCTSPosition() const;

    /**
        Returns the point-of-view position, expressed in continuous, one-hundredth of a
        degree units, but limited to return 0, 9000, 18000 or 27000.

        Returns -1 on error.
    */
    int GetPOVPosition() const;

    /**
        Returns the maximum polling frequency.
    */
    int GetPollingMax() const;

    /**
        Returns the minimum polling frequency.
    */
    int GetPollingMin() const;

    /**
        Returns the x, y position of the joystick.
    */
    wxPoint GetPosition() const;

    /**
        Returns the position of the specified joystick axis.

        @param axis
            The joystick axis to report, from 0 to GetNumberAxes() - 1.
    */
    int GetPosition(unsigned int axis) const;

    /**
        Returns the product id for the joystick.
    */
    int GetProductId() const;

    /**
        Returns the product name for the joystick.
    */
    wxString GetProductName() const;

    /**
        Returns the maximum rudder position.
    */
    int GetRudderMax() const;

    /**
        Returns the minimum rudder position.
    */
    int GetRudderMin() const;

    /**
        Returns the rudder position.
    */
    int GetRudderPosition() const;

    /**
        Returns the maximum U position.
    */
    int GetUMax() const;

    /**
        Returns the minimum U position.
    */
    int GetUMin() const;

    /**
        Gets the position of the fifth axis of the joystick, if it exists.
    */
    int GetUPosition() const;

    /**
        Returns the maximum V position.
    */
    int GetVMax() const;

    /**
        Returns the minimum V position.
    */
    int GetVMin() const;

    /**
        Gets the position of the sixth axis of the joystick, if it exists.
    */
    int GetVPosition() const;

    /**
        Returns the maximum x position.
    */
    int GetXMax() const;

    /**
        Returns the minimum x position.
    */
    int GetXMin() const;

    /**
        Returns the maximum y position.
    */
    int GetYMax() const;

    /**
        Returns the minimum y position.
    */
    int GetYMin() const;

    /**
        Returns the maximum z position.
    */
    int GetZMax() const;

    /**
        Returns the minimum z position.
    */
    int GetZMin() const;

    /**
        Returns the z position of the joystick.
    */
    int GetZPosition() const;

    /**
        Returns @true if the joystick has a point of view control.
    */
    bool HasPOV() const;

    /**
        Returns @true if the joystick point-of-view supports discrete values
        (centered, forward, backward, left, and right).
    */
    bool HasPOV4Dir() const;

    /**
        Returns @true if the joystick point-of-view supports continuous degree bearings.
    */
    bool HasPOVCTS() const;

    /**
        Returns @true if there is a rudder attached to the computer.
    */
    bool HasRudder() const;

    /**
        Returns @true if the joystick has a U axis.
    */
    bool HasU() const;

    /**
        Returns @true if the joystick has a V axis.
    */
    bool HasV() const;

    /**
        Returns @true if the joystick has a Z axis.
    */
    bool HasZ() const;

    /**
        Returns @true if the joystick is functioning.
    */
    bool IsOk() const;

    /**
        Releases the capture set by @b SetCapture.

        @return @true if the capture release succeeded.

        @see SetCapture(), wxJoystickEvent
    */
    bool ReleaseCapture();

    /**
        Sets the capture to direct joystick events to @a win.

        @param win
            The window that will receive joystick events.
        @param pollingFreq
            If zero, movement events are sent when above the threshold.
            If greater than zero, events are received every @a pollingFreq milliseconds.

        @return @true if the capture succeeded.

        @see ReleaseCapture(), wxJoystickEvent
    */
    bool SetCapture(wxWindow* win, int pollingFreq = 0);

    /**
        Sets the movement threshold, the number of steps outside which the joystick is
        deemed to have moved.
    */
    void SetMovementThreshold(int threshold);
};

