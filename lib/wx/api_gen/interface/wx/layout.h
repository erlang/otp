/////////////////////////////////////////////////////////////////////////////
// Name:        layout.h
// Purpose:     interface of layout constraints classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


enum wxEdge
{
    wxLeft, wxTop, wxRight, wxBottom, wxWidth, wxHeight,
    wxCentre, wxCenter = wxCentre, wxCentreX, wxCentreY
};

enum wxRelationship
{
    wxUnconstrained,
    wxAsIs,
    wxPercentOf,
    wxAbove,
    wxBelow,
    wxLeftOf,
    wxRightOf,
    wxSameAs,
    wxAbsolute
};

const int wxLAYOUT_DEFAULT_MARGIN = 0;

// ----------------------------------------------------------------------------
// wxIndividualLayoutConstraint: a constraint on window position
// ----------------------------------------------------------------------------

class wxIndividualLayoutConstraint : public wxObject
{
public:
    wxIndividualLayoutConstraint();

    // note that default copy ctor and assignment operators are ok

    virtual ~wxIndividualLayoutConstraint();

    void Set(wxRelationship rel,
             wxWindow *otherW,
             wxEdge otherE,
             int val = 0,
             int margin = wxLAYOUT_DEFAULT_MARGIN);

    //
    // Sibling relationships
    //
    void LeftOf(wxWindow *sibling, int margin = wxLAYOUT_DEFAULT_MARGIN);
    void RightOf(wxWindow *sibling, int margin = wxLAYOUT_DEFAULT_MARGIN);
    void Above(wxWindow *sibling, int margin = wxLAYOUT_DEFAULT_MARGIN);
    void Below(wxWindow *sibling, int margin = wxLAYOUT_DEFAULT_MARGIN);

    //
    // 'Same edge' alignment
    //
    void SameAs(wxWindow *otherW, wxEdge edge, int margin = wxLAYOUT_DEFAULT_MARGIN);

    // The edge is a percentage of the other window's edge
    void PercentOf(wxWindow *otherW, wxEdge wh, int per);

    //
    // Edge has absolute value
    //
    void Absolute(int val);

    //
    // Dimension is unconstrained
    //
    void Unconstrained();

    //
    // Dimension is 'as is' (use current size settings)
    //
    void AsIs();

    //
    // Accessors
    //
    wxWindow *GetOtherWindow();
    wxEdge GetMyEdge() const;
    void SetEdge(wxEdge which);
    void SetValue(int v);
    int GetMargin();
    void SetMargin(int m);
    int GetValue() const;
    int GetPercent() const;
    int GetOtherEdge() const;
    bool GetDone() const;
    void SetDone(bool d);
    wxRelationship GetRelationship();
    void SetRelationship(wxRelationship r);

    // Reset constraint if it mentions otherWin
    bool ResetIfWin(wxWindow *otherW);

    // Try to satisfy constraint
    bool SatisfyConstraint(wxLayoutConstraints *constraints, wxWindow *win);

    // Get the value of this edge or dimension, or if this
    // is not determinable, -1.
    int GetEdge(wxEdge which, wxWindow *thisWin, wxWindow *other) const;
};

// ----------------------------------------------------------------------------
// wxLayoutConstraints: the complete set of constraints for a window
// ----------------------------------------------------------------------------

class wxLayoutConstraints : public wxObject
{
public:
    // Edge constraints
    wxIndividualLayoutConstraint left;
    wxIndividualLayoutConstraint top;
    wxIndividualLayoutConstraint right;
    wxIndividualLayoutConstraint bottom;
    // Size constraints
    wxIndividualLayoutConstraint width;
    wxIndividualLayoutConstraint height;
    // Centre constraints
    wxIndividualLayoutConstraint centreX;
    wxIndividualLayoutConstraint centreY;

    wxLayoutConstraints();

    // note that default copy ctor and assignment operators are ok

    virtual ~wxLayoutConstraints();

    bool SatisfyConstraints(wxWindow *win, int *noChanges);
    bool AreSatisfied() const;
};

