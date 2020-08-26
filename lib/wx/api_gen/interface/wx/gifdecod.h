/////////////////////////////////////////////////////////////////////////////
// Name:        wx/gifdecod.h
// Purpose:     wxGIFDecoder, GIF reader for wxImage and wxAnimation
// Author:      Guillermo Rodriguez Garcia <guille@iies.es>
// Version:     3.02
// Copyright:   (c) 1999 Guillermo Rodriguez Garcia
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
   @class wxGIFDecoder

   An animation decoder supporting animated GIF files.
*/
class  wxGIFDecoder : public wxAnimationDecoder
{
public:
    wxGIFDecoder();
    ~wxGIFDecoder();

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

