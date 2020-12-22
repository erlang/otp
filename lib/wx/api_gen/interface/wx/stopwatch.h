/////////////////////////////////////////////////////////////////////////////
// Name:        stopwatch.h
// Purpose:     interface of wxStopWatch
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxStopWatch

    The wxStopWatch class allow you to measure time intervals.

    For example, you may use it to measure the time elapsed by some function:

    @code
        wxStopWatch sw;
        CallLongRunningFunction();
        wxLogMessage("The long running function took %ldms to execute",
                     sw.Time());
        sw.Pause();
        ... stopwatch is stopped now ...
        sw.Resume();
        CallLongRunningFunction();
        wxLogMessage("And calling it twice took %ldms in all", sw.Time());
    @endcode

    Since wxWidgets 2.9.3 this class uses @c QueryPerformanceCounter()
    function under MSW to measure the elapsed time. It provides higher
    precision than the usual timer functions but can suffer from bugs in its
    implementation in some Windows XP versions. If you encounter such problems,
    installing a Microsoft hot fix from http://support.microsoft.com/?id=896256
    could be necessary.

    @library{wxbase}
    @category{misc}

    @see wxTimer
*/
class wxStopWatch
{
public:
    /**
        Constructor. This starts the stop watch.
    */
    wxStopWatch();

    /**
        Pauses the stop watch. Call Resume() to resume time measuring again.

        If this method is called several times, @c Resume() must be called the same
        number of times to really resume the stop watch. You may, however, call
        Start() to resume it unconditionally.
    */
    void Pause();

    /**
        Resumes the stop watch which had been paused with Pause().
    */
    void Resume();

    /**
        (Re)starts the stop watch with a given initial value.

        The stopwatch will always be running after calling Start(), even if
        Pause() had been called before and even if it had been called multiple
        times.
    */
    void Start(long milliseconds = 0);

    /**
        Returns the time in milliseconds since the start (or restart) or the last
        call of Pause().

        @see TimeInMicro()
    */
    long Time() const;

    /**
        Returns elapsed time in microseconds.

        This method is similar to Time() but returns the elapsed time in
        microseconds and not milliseconds. Notice that not all platforms really
        can measure times with this precision.

        @since 2.9.3
     */
    wxLongLong TimeInMicro() const;
};

