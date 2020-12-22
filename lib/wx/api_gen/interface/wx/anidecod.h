/////////////////////////////////////////////////////////////////////////////
// Name:        wx/anidecod.h
// Purpose:     wxANIDecoder, ANI reader for wxImage and wxAnimation
// Author:      Francesco Montorsi
// Copyright:   (c) 2006 Francesco Montorsi
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
   @class wxANIDecoder

   An animation decoder supporting animated cursor (.ani) files.
*/
class  wxANIDecoder : public wxAnimationDecoder
{
public:
    wxANIDecoder();
    ~wxANIDecoder();

    virtual bool Load( wxInputStream& stream );
    virtual wxAnimationDecoder *Clone() const;
    virtual wxAnimationType GetType() const;
    virtual bool ConvertToImage(unsigned int frame, wxImage *image) const;
    virtual wxSize GetFrameSize(unsigned int frame) const;
    virtual wxPoint GetFramePosition(unsigned int frame) const;
    virtual wxAnimationDisposal GetDisposalMethod(unsigned int frame) const;
    virtual long GetDelay(unsigned int frame) const;
    virtual wxColour GetTransparentColour(unsigned int frame) const;

protected:
    virtual bool DoCanRead(wxInputStream& stream) const;    
};
