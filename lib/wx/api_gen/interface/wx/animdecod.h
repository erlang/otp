/////////////////////////////////////////////////////////////////////////////
// Name:        wx/animdecod.h
// Purpose:     wxAnimationDecoder
// Author:      Francesco Montorsi
// Copyright:   (c) 2006 Francesco Montorsi
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

enum wxAnimationDisposal
{
    /// No disposal specified. The decoder is not required to take any action.
    wxANIM_UNSPECIFIED = -1,

    /// Do not dispose. The graphic is to be left in place.
    wxANIM_DONOTREMOVE = 0,

    /// Restore to background color. The area used by the graphic must be
    /// restored to the background color.
    wxANIM_TOBACKGROUND = 1,

    /// Restore to previous. The decoder is required to restore the area
    /// overwritten by the graphic with what was there prior to rendering the graphic.
    wxANIM_TOPREVIOUS = 2
};



/**
   @class wxAnimationDecoder

   wxAnimationDecoder is used by @c wxAnimation for loading frames and other
   information for the animation from the animation image file.

 */
class wxAnimationDecoder : public wxObjectRefData
{
public:
    wxAnimationDecoder();

    /**
       Load the animation image frames from the given stream.
    */
    virtual bool Load( wxInputStream& stream ) = 0;

    /**
       Returns @true if this decoder supports loading from the given stream.
    */
    bool CanRead( wxInputStream& stream ) const;

    /**
       Create a copy of this decoder.
    */
    virtual wxAnimationDecoder *Clone() const = 0;

    /**
       Return the animation type this decoder implements.
    */
    virtual wxAnimationType GetType() const = 0;

    /**
       Convert given frame to @c wxImage.
    */
    virtual bool ConvertToImage(unsigned int frame, wxImage *image) const = 0;


    /*
      Get the size of the given animation frame.

      It's possible that not all frames are of the same size; e.g. GIF allows
      to specify that between two frames only a smaller portion of the entire
      animation has changed.
    */
    virtual wxSize GetFrameSize(unsigned int frame) const = 0;

    /*
      Returns the position of the frame, in case it's not as big as the animation size,
      or @c wxPoint(0,0) otherwise.
    */
    virtual wxPoint GetFramePosition(unsigned int frame) const = 0;

    /**
      What should be done after displaying this frame.
    */
    virtual wxAnimationDisposal GetDisposalMethod(unsigned int frame) const = 0;

    /**
       Return the number of milliseconds this frame should be displayed.
       If -1 is returned then the frame must be displayed forever.
    */
    virtual long GetDelay(unsigned int frame) const = 0;

    /**
       The transparent colour for this frame, if any, or @c wxNullColour.
    */
    virtual wxColour GetTransparentColour(unsigned int frame) const = 0;

    wxSize GetAnimationSize() const;
    wxColour GetBackgroundColour() const;
    unsigned int GetFrameCount() const;

protected:
    /**
       Checks the signature of the data in the given stream and returns true if it
       appears to be a valid animation format recognized by the animation decoder;
       this function should modify the stream current position without taking care
       of restoring it since @c CanRead() will do it.
    */
    virtual bool DoCanRead(wxInputStream& stream) const = 0;
};


