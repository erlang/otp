/////////////////////////////////////////////////////////////////////////////
// Name:        fontutil.h
// Purpose:     interface of wxNativeFontInfo
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxNativeFontInfo

    wxNativeFontInfo is platform-specific font representation: this class
    should be considered as an opaque font description only used by the native
    functions, the user code can only get the objects of this type from
    somewhere and pass it somewhere else (possibly save them somewhere using
    ToString() and restore them using FromString())

    @library{wxcore}
    @category{gdi}
*/
class wxNativeFontInfo
{
public:
    wxNativeFontInfo();
    wxNativeFontInfo(const wxNativeFontInfo& info);
    ~wxNativeFontInfo();

    wxNativeFontInfo& operator=(const wxNativeFontInfo& info);

    void Init();
    void InitFromFont(const wxFont& font);

    int GetPointSize() const;
    float GetFractionalPointSize() const;
    wxSize GetPixelSize() const;
    wxFontStyle GetStyle() const;
    int GetNumericWeight() const;
    wxFontWeight GetWeight() const;
    bool GetUnderlined() const;
    wxString GetFaceName() const;
    wxFontFamily GetFamily() const;
    wxFontEncoding GetEncoding() const;

    void SetPointSize(int pointsize);
    void SetFractionalPointSize(float pointsize);
    void SetPixelSize(const wxSize& pixelSize);
    void SetStyle(wxFontStyle style);
    void SetNumericWeight(int weight);
    void SetWeight(wxFontWeight weight);
    void SetUnderlined(bool underlined);
    bool SetFaceName(const wxString& facename);
    void SetFamily(wxFontFamily family);
    void SetEncoding(wxFontEncoding encoding);

    void SetFaceName(const wxArrayString &facenames);

    bool FromString(const wxString& s);
    wxString ToString() const;

    bool FromUserString(const wxString& s);
    wxString ToUserString() const;
};




