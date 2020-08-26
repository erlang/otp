/////////////////////////////////////////////////////////////////////////////
// Name:        affinematrix2d.h
// Purpose:     interface of wxAffineMatrix2D
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxAffineMatrix2D

    A 3x2 matrix representing an affine 2D transformation.

    @library{wxcore}
    @category{misc}

    @since 2.9.2
*/
class wxAffineMatrix2D : public wxAffineMatrix2DBase
{
public:
    /**
        Default constructor.

        The matrix elements are initialize to the identity matrix.
    */
    wxAffineMatrix2D();

    /**
        Get the component values of the matrix.

        @param mat2D
            The rotational components of the matrix (upper 2 x 2), must be
            non-@NULL.
        @param tr
            The translational components of the matrix, may be @NULL.
    */
    void Get(wxMatrix2D* mat2D, wxPoint2DDouble* tr) const;

    /**
        Set all elements of this matrix.

        @param mat2D
            The rotational components of the matrix (upper 2 x 2).
        @param tr
            The translational components of the matrix.
    */
    void Set(const wxMatrix2D& mat2D, const wxPoint2DDouble& tr);

    /**
        Concatenate this matrix with another one.

        The parameter matrix is the multiplicand.

        @param t
            The multiplicand.

        @code
        //           | t.m_11  t.m_12  0 |   | m_11  m_12   0 |
        // matrix' = | t.m_21  t.m_22  0 | x | m_21  m_22   0 |
        //           | t.m_tx  t.m_ty  1 |   | m_tx  m_ty   1 |
        @endcode
    */
    void Concat(const wxAffineMatrix2DBase& t);

    /**
        Invert this matrix.

        If the matrix is not invertible, i.e. if its determinant is 0, returns
        false and doesn't modify it.

        @code
        //           | m_11  m_12  0 |
        // Invert    | m_21  m_22  0 |
        //           | m_tx  m_ty  1 |
        @endcode
    */
    bool Invert();

    /**
        Check if this is the identity matrix.
    */
    bool IsIdentity() const;

    //@{
    /**
        Check that this matrix is identical with @a t.

        @param t
            The matrix compared with this.
    */
    void IsEqual(const wxAffineMatrix2DBase& t);
    bool operator==(const wxAffineMatrix2DBase& t) const;
    //@}

    /**
        Check that this matrix differs from @a t.

        @param t
            The matrix compared with this.
    */
    bool operator!=(const wxAffineMatrix2DBase& t) const;

    /**
        Add the translation to this matrix.

        @param dx
            The translation in x direction.
        @param dy
            The translation in y direction.

        @code
        //           |  1   0   0 |   | m_11  m_12   0 |
        // matrix' = |  0   1   0 | x | m_21  m_22   0 |
        //           | dx  dy   1 |   | m_tx  m_ty   1 |
        @endcode
    */
    void Translate(wxDouble dx, wxDouble dy);

    /**
        Add scaling to this matrix.

        @param xScale
            Scaling in x direction.
        @param yScale
            Scaling in y direction.

        @code
        //           | xScale   0      0 |   | m_11  m_12   0 |
        // matrix' = |   0    yScale   0 | x | m_21  m_22   0 |
        //           |   0      0      1 |   | m_tx  m_ty   1 |
        @endcode
    */
    void Scale(wxDouble xScale, wxDouble yScale);

    /**
        Add mirroring to this matrix.

        @param direction
            The direction(s) used for mirroring. One of wxHORIZONTAL,
            wxVERTICAL or their combination wxBOTH.
    */
    void Mirror(int direction = wxHORIZONTAL);

    /**
        Add clockwise rotation to this matrix.

        @param cRadians
            Rotation angle in radians, clockwise.

        @code
        //           | cos    sin   0 |   | m_11  m_12   0 |
        // matrix' = | -sin   cos   0 | x | m_21  m_22   0 |
        //           |  0      0    1 |   | m_tx  m_ty   1 |
        @endcode
    */
    void Rotate(wxDouble cRadians);

    /**
        Applies this matrix to the point.

        @param p
            The point receiving the transformations.

        @return The point with the transformations applied.

        @code
        //                                    | m_11  m_12   0 |
        // point' = | src.m_x  src._my  1 | x | m_21  m_22   0 |
        //                                    | m_tx  m_ty   1 |
        @endcode
    */
    wxPoint2DDouble TransformPoint(const wxPoint2DDouble& p) const;
    void TransformPoint(wxDouble* x, wxDouble* y) const;

    /**
        Applies the linear part of this matrix, i.e.\ without translation.

        @param p
            The source receiving the transformations.

        @return The source with the transformations applied.

        @code
        //                                   | m_11  m_12   0 |
        // dist' = | src.m_x  src._my  0 | x | m_21  m_22   0 |
        //                                   | m_tx  m_ty   1 |
        @endcode
    */
    wxPoint2DDouble TransformDistance(const wxPoint2DDouble& p) const;
    void TransformDistance(wxDouble* dx, wxDouble* dy) const;
};
