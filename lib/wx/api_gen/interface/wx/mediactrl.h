/////////////////////////////////////////////////////////////////////////////
// Name:        mediactrl.h
// Purpose:     interface of wxMediaEvent, wxMediaCtrl
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Describes the current state of the media.

    @see wxMediaCtrl::GetState()
 */
enum wxMediaState
{
    /** No media is being currently played. */
    wxMEDIASTATE_STOPPED,

    /** Current media is paused. */
    wxMEDIASTATE_PAUSED,

    /** There is media currently playing. */
    wxMEDIASTATE_PLAYING
};

enum wxMediaCtrlPlayerControls
{
    /** No controls. return wxMediaCtrl to its default state. */
    wxMEDIACTRLPLAYERCONTROLS_NONE           =   0,

    /** Step controls like fastforward, step one frame etc. */
    wxMEDIACTRLPLAYERCONTROLS_STEP           =   1 << 0,

    /** Volume controls like the speaker icon, volume slider, etc. */
    wxMEDIACTRLPLAYERCONTROLS_VOLUME         =   1 << 1,

    /** Default controls for the toolkit. Currently a combination for
        @c wxMEDIACTRLPLAYERCONTROLS_STEP and @c wxMEDIACTRLPLAYERCONTROLS_VOLUME. */
    wxMEDIACTRLPLAYERCONTROLS_DEFAULT        =
                    wxMEDIACTRLPLAYERCONTROLS_STEP |
                    wxMEDIACTRLPLAYERCONTROLS_VOLUME
};

/**
    @class wxMediaEvent

    Event wxMediaCtrl uses.

    @beginEventTable{wxMediaEvent}
    @event{EVT_MEDIA_LOADED(id\, func)}
           Sent when a media has loaded enough data that it can start playing.
           Processes a @c wxEVT_MEDIA_LOADED event type.
    @event{EVT_MEDIA_STOP(id\, func)}
           Sent when a media has switched to the @c wxMEDIASTATE_STOPPED state.
           You may be able to Veto this event to prevent it from stopping,
           causing it to continue playing - even if it has reached that end of
           the media (note that this may not have the desired effect - if you
           want to loop the media, for example, catch the @c EVT_MEDIA_FINISHED
           and play there instead).
           Processes a @c wxEVT_MEDIA_STOP event type.
    @event{EVT_MEDIA_FINISHED(id\, func)}
           Sent when a media has finished playing in a wxMediaCtrl.
           Processes a @c wxEVT_MEDIA_FINISHED event type.
    @event{EVT_MEDIA_STATECHANGED(id\, func)}
           Sent when a media has switched its state (from any media state).
           Processes a @c wxEVT_MEDIA_STATECHANGED event type.
    @event{EVT_MEDIA_PLAY(id\, func)}
           Sent when a media has switched to the @c wxMEDIASTATE_PLAYING state.
           Processes a @c wxEVT_MEDIA_PLAY event type.
    @event{EVT_MEDIA_PAUSE(id\, func)}
           Sent when a media has switched to the @c wxMEDIASTATE_PAUSED state.
           Processes a @c wxEVT_MEDIA_PAUSE event type.
    @endEventTable

    @library{wxmedia}
    @category{events}
*/
class wxMediaEvent : public wxNotifyEvent
{
public:
    /** Default ctor. */
    wxMediaEvent(wxEventType commandType = wxEVT_NULL, int winid = 0);
};



/**
    @class wxMediaCtrl

    wxMediaCtrl is a class for displaying various types of media, such as videos,
    audio files, natively through native codecs.

    wxMediaCtrl uses native backends to render media, for example on Windows
    there is a ActiveMovie/DirectShow backend, and on Macintosh there is a
    QuickTime backend.


    @section mediactrl_rendering_media Rendering media

    Depending upon the backend, wxMediaCtrl can render and display pretty much any
    kind of media that the native system can - such as an image, mpeg video, or mp3
    (without license restrictions - since it relies on native system calls that may
    not technically have mp3 decoding available, for example, it falls outside
    the realm of licensing restrictions).

    For general operation, all you need to do is call Load() to load the file you
    want to render, catch the @c EVT_MEDIA_LOADED event, and then call Play()
    to show the video/audio of the media in that event.

    More complex operations are generally more heavily dependent on the capabilities
    of the backend. For example, QuickTime cannot set the playback rate of certain
    streaming media - while DirectShow is slightly more flexible in that regard.

    @section mediactrl_operation Operation

    When wxMediaCtrl plays a file, it plays until the stop position is reached
    (currently the end of the file/stream). Right before it hits the end of the stream,
    it fires off a @c EVT_MEDIA_STOP event to its parent window, at which point the event
    handler can choose to veto the event, preventing the stream from actually stopping.

    Example:

    @code
    // bind the media event
    Bind(wxMY_ID, wxEVT_MEDIA_STOP, &MyFrame::OnMediaStop, this);

    //...
    void MyFrame::OnMediaStop(wxMediaEvent& evt)
    {
        if( userWantsToSeek )
        {
            m_mediactrl->Seek(m_mediactrl->Length() - 1);
            evt.Veto();
        }
    }
    @endcode

    When wxMediaCtrl stops, either by the @c EVT_MEDIA_STOP not being vetoed, or
    by manually calling Stop(), where it actually stops is not at the beginning,
    rather, but at the beginning of the stream. That is, when it stops and play
    is called, playback is guaranteed to start at the beginning of the media.
    This is because some streams are not seekable, and when stop is called on
    them they return to the beginning, thus wxMediaCtrl tries to keep consistent
    for all types of media.

    Note that when changing the state of the media through Play() and other methods,
    the media may not actually be in the @c wxMEDIASTATE_PLAYING, for example.
    If you are relying on the media being in certain state, catch the event relevant
    to the state. See wxMediaEvent for the kinds of events that you can catch.


    @section mediactrl_video_size Video size

    By default, wxMediaCtrl will scale the size of the video to the requested
    amount passed to either its constructor or Create().
    After calling wxMediaCtrl::Load or performing an equivalent operation,
    you can subsequently obtain the "real" size of the video (if there is any)
    by calling wxMediaCtrl::GetBestSize(). Note that the actual result on the
    display will be slightly different when wxMediaCtrl::ShowPlayerControls is
    activated and the actual video size will be less than specified due to the
    extra controls provided by the native toolkit.
    In addition, the backend may modify wxMediaCtrl::GetBestSize() to include
    the size of the extra controls - so if you want the real size of the video
    just disable wxMediaCtrl::ShowPlayerControls().

    The idea with setting wxMediaCtrl::GetBestSize() to the size of the video is
    that GetBestSize() is a wxWindow-derived function that is called when sizers
    on a window recalculate.
    What this means is that if you use sizers by default the video will show in
    its original size without any extra assistance needed from the user.


    @section mediactrl_player_controls Player controls

    Normally, when you use wxMediaCtrl it is just a window for the video to play in.
    However, some toolkits have their own media player interface.
    For example, QuickTime generally has a bar below the video with a slider.
    A special feature available to wxMediaCtrl, you can use the toolkits interface
    instead of making your own by using the ShowPlayerControls() function.
    There are several options for the flags parameter, with the two general flags
    being @c wxMEDIACTRLPLAYERCONTROLS_NONE which turns off the native interface,
    and @c wxMEDIACTRLPLAYERCONTROLS_DEFAULT which lets wxMediaCtrl decide what
    native controls on the interface.
    Be sure to review the caveats outlined in @ref mediactrl_video_size before doing so.


    @section mediactrl_choosing_backend Choosing a backend

    Generally, you should almost certainly leave this part up to wxMediaCtrl -
    but if you need a certain backend for a particular reason, such as QuickTime
    for playing .mov files, all you need to do to choose a specific backend is
    to pass the name of the backend class to wxMediaCtrl::Create().

    The following are valid backend identifiers:

    - @b wxMEDIABACKEND_DIRECTSHOW: Use ActiveMovie/DirectShow.
      Uses the native ActiveMovie (I.E. DirectShow) control.
      Default backend on Windows and supported by nearly all Windows versions.
      May display a windows media player logo while inactive.
    - @b wxMEDIABACKEND_QUICKTIME: Use QuickTime. Mac Only.
      WARNING: May not work correctly when embedded in a wxNotebook.
    - @b wxMEDIABACKEND_GSTREAMER, Use GStreamer. Unix Only.
      Requires GStreamer 0.10 along with at the very least the xvimagesink,
      xoverlay and gst-play modules of gstreamer to function.
      You need the correct modules to play the relevant files, for example the
      mad module to play mp3s, etc.
    - @b wxMEDIABACKEND_WMP10, Use Windows Media Player 10 (Windows only).
      Works on systems with either Windows Media Player 9 or 10 installed.


    @section mediactrl_creating_backend Creating a backend

    Creating a backend for wxMediaCtrl is a rather simple process.
    Simply derive from wxMediaBackendCommonBase and implement the methods you want.
    The methods in wxMediaBackend correspond to those in wxMediaCtrl except for
    wxMediaCtrl::CreateControl which does the actual creation of the control,
    in cases where a custom control is not needed you may simply call wxControl::Create().

    You need to make sure to use the @c wxDECLARE_CLASS and @c wxIMPLEMENT_CLASS macros.

    The only real tricky part is that you need to make sure the file in compiled in,
    which if there are just backends in there will not happen and you may need to
    use a force link hack (see @c wxFORCE_LINK_MODULE usage in the mediactrl sample).

    There is a rather simple example of how to create a backend in the
    wxActiveXContainer documentation.


    @library{wxmedia}
    @category{media}

    @see wxMediaEvent
*/
class wxMediaCtrl : public wxControl
{
public:
    /**
        Default constructor - you MUST call Create() before calling any
        other methods of wxMediaCtrl.
    */
    wxMediaCtrl();

    /**
        Constructor that calls Create().
        You may prefer to call Create() directly to check to see if
        wxMediaCtrl is available on the system.

        @param parent
            parent of this control.  Must not be @NULL.
        @param id
            id to use for events
        @param fileName
            If not empty, the path of a file to open.
        @param pos
            Position to put control at.
        @param size
            Size to put the control at and to stretch movie to.
        @param style
            Optional styles.
        @param szBackend
            Name of backend you want to use, leave blank to make wxMediaCtrl figure it out.
        @param validator
            validator to use.
        @param name
            Window name.
    */
    wxMediaCtrl(wxWindow* parent, wxWindowID id, const wxString& fileName = wxEmptyString,
                const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
                long style = 0, const wxString& szBackend = wxEmptyString,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = "mediaCtrl");

    /**
        Creates this control.
        Returns @false if it can't load the media located at @a fileName
        or it can't create a backend.

        If you specify a file to open via @a fileName and you don't specify a
        backend to use, wxMediaCtrl tries each of its backends until one that
        can render the path referred to by @a fileName can be found.

        @param parent
            parent of this control.  Must not be @NULL.
        @param id
            id to use for events
        @param fileName
            If not empty, the path of a file to open.
        @param pos
            Position to put control at.
        @param size
            Size to put the control at and to stretch movie to.
        @param style
            Optional styles.
        @param szBackend
            Name of backend you want to use, leave blank to make wxMediaCtrl figure it out.
        @param validator
            validator to use.
        @param name
            Window name.
    */
    bool Create(wxWindow* parent, wxWindowID id, const wxString& fileName = wxEmptyString,
                const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
                long style = 0, const wxString& szBackend = wxEmptyString,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = "mediaCtrl");

    /**
        Obtains the best size relative to the original/natural size of the
        video, if there is any.
        See @ref mediactrl_video_size for more information.
    */
    wxSize GetBestSize() const;

    /**
        Obtains the playback rate, or speed of the media. @c 1.0 represents normal
        speed, while @c 2.0 represents twice the normal speed of the media, for
        example. Not supported on the GStreamer (Unix) backend.

        @return zero on failure.
    */
    double GetPlaybackRate();

    /**
        Obtains the state the playback of the media is in.

        @beginTable
        @row2col{wxMEDIASTATE_STOPPED, The media has stopped.}
        @row2col{wxMEDIASTATE_PAUSED, The media is paused.}
        @row2col{wxMEDIASTATE_PLAYING, The media is currently playing.}
        @endTable
    */
    wxMediaState GetState();

    /**
        Gets the volume of the media from a 0.0 to 1.0 range.

        @note Due to rounding and other errors the value returned may not be the
              exact value sent to SetVolume().
    */
    double GetVolume();

    /**
        Obtains the length - the total amount of time the media has in milliseconds.
    */
    wxFileOffset Length();

    /**
        Loads the file that fileName refers to. Returns @false if loading fails.
    */
    bool Load(const wxString& fileName);

    /**
        Loads the location that uri refers to. Note that this is very
        implementation-dependent, although HTTP URI/URLs are generally
        supported, for example. Returns @false if loading fails.
    */
    bool Load(const wxURI& uri);

    /**
        Loads the location that @c uri refers to with the proxy @c proxy.
        Not implemented on most backends so it should be called with caution.
        Returns @false if loading fails.
    */
    bool Load(const wxURI& uri, const wxURI& proxy);

    /**
        Same as Load(const wxURI& uri). Kept for wxPython compatibility.
    */
    bool LoadURI(const wxString& uri);

    /**
        Same as Load(const wxURI& uri, const wxURI& proxy).
        Kept for wxPython compatibility.
    */
    bool LoadURIWithProxy(const wxString& uri, const wxString& proxy);

    /**
        Pauses playback of the media.
    */
    bool Pause();

    /**
        Resumes playback of the media.
    */
    bool Play();

    /**
        Seeks to a position within the media.

        @todo Document the wxSeekMode parameter @a mode, and perhaps also the
              wxFileOffset and wxSeekMode themselves.
    */
    wxFileOffset Seek(wxFileOffset where, wxSeekMode mode = wxFromStart);

    /**
        Sets the playback rate, or speed of the media, to that referred by @a dRate.
        @c 1.0 represents normal speed, while @c 2.0 represents twice the normal
        speed of the media, for example. Not supported on the GStreamer (Unix) backend.
        Returns @true if successful.
    */
    bool SetPlaybackRate(double dRate);

    /**
        Sets the volume of the media from a 0.0 to 1.0 range to that referred
        by @c dVolume.  @c 1.0 represents full volume, while @c 0.5
        represents half (50 percent) volume, for example.

        @note The volume may not be exact due to conversion and rounding errors,
              although setting the volume to full or none is always exact.
              Returns @true if successful.
    */
    bool SetVolume(double dVolume);

    /**
        A special feature to wxMediaCtrl. Applications using native toolkits such as
        QuickTime usually have a scrollbar, play button, and more provided to
        them by the toolkit. By default wxMediaCtrl does not do this. However, on
        the DirectShow and QuickTime backends you can show or hide the native controls
        provided by the underlying toolkit at will using ShowPlayerControls(). Simply
        calling the function with default parameters tells wxMediaCtrl to use the
        default controls provided by the toolkit. The function takes a
        wxMediaCtrlPlayerControls enumeration, please see available show modes there.

        For more info see @ref mediactrl_player_controls.

        Currently only implemented on the QuickTime and DirectShow backends.
        The function returns @true on success.
    */
    bool ShowPlayerControls(wxMediaCtrlPlayerControls flags = wxMEDIACTRLPLAYERCONTROLS_DEFAULT);

    /**
        Stops the media.

        See @ref mediactrl_operation for an overview of how stopping works.
    */
    bool Stop();

    /**
        Obtains the current position in time within the media in milliseconds.
    */
    wxFileOffset Tell();
};

wxEventType wxEVT_MEDIA_LOADED;
wxEventType wxEVT_MEDIA_STOP;
wxEventType wxEVT_MEDIA_FINISHED;
wxEventType wxEVT_MEDIA_STATECHANGED;
wxEventType wxEVT_MEDIA_PLAY;
wxEventType wxEVT_MEDIA_PAUSE;
