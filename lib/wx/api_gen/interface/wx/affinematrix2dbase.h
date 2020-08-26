/////////////////////////////////////////////////////////////////////////////
// Name:        affinematrix2dbase.h
// Purpose:     wxMatrix2D documentation
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxMatrix2D

    A simple container for 2x2 matrix.

    This simple structure is used with wxAffineMatrix2D.

    @library{wxcore}
    @category{misc}

    @since 2.9.2
 */
struct wxMatrix2D
{
    /**
        Default constructor.

        Initializes the matrix elements to the identity.
     */
    wxMatrix2D(wxDouble v11 = 1,
               wxDouble v12 = 0,
               wxDouble v21 = 0,
               wxDouble v22 = 1);

    /// The matrix elements in the usual mathematical notation.
    wxDouble m_11, m_12, m_21, m_22;
};


/**
   @class wxAffineMatrix2DBase

   A 2x3 matrix representing an affine 2D transformation.

   This is an abstract base class implemented by wxAffineMatrix2D only so far,
   but in the future we also plan to derive wxGraphicsMatrix from it.

    @library{wxcore}
    @category{misc}

    @since 2.9.2
*/
class wxAffineMatrix2DBase
{
public:
    /**
        Default constructor.

        The matrix elements are initialize to the identity matrix.
    */
    wxAffineMatrix2DBase();
    virtual ~wxAffineMatrix2DBase();

    /**
        Set all elements of this matrix.

        @param mat2D
            The rotational components of the matrix (upper 2 x 2).
        @param tr
            The translational components of the matrix.
    */
    virtual void Set(const wxMatrix2D& mat2D, const wxPoint2DDouble& tr) = 0;

    /**
        Get the component values of the matrix.

        @param mat2D
            The rotational components of the matrix (upper 2 x 2), must be
            non-@NULL.
        @param tr
            The translational components of the matrix, may be @NULL.
    */
    virtual void Get(wxMatrix2D* mat2D, wxPoint2DDouble* tr) const = 0;

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
    virtual void Concat(const wxAffineMatrix2DBase& t) = 0;

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
    virtual bool Invert() = 0;

    /**
        Check if this is the identity matrix.
    */
    virtual bool IsIdentity() const = 0;

    //@{
    /**
        Check that this matrix is identical with @a t.

        @param t
            The matrix compared with this.
    */
    virtual bool IsEqual(const wxAffineMatrix2DBase& t) const = 0;
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
    */
    virtual void Translate(wxDouble dx, wxDouble dy) = 0;

    /**
        Add scaling to this matrix.

        @param xScale
            Scaling in x direction.
        @param yScale
            Scaling in y direction.
    */
    virtual void Scale(wxDouble xScale, wxDouble yScale) = 0;

    /**
        Add clockwise rotation to this matrix.

        @param cRadians
            Rotation angle in radians, clockwise.
    */
    virtual void Rotate(wxDouble cRadians) = 0;

    /**
        Add mirroring to this matrix.

        @param direction
            The direction(s) used for mirroring. One of wxHORIZONTAL,
            wxVERTICAL or their combination wxBOTH.
    */
    void Mirror(int direction = wxHORIZONTAL);


    /**
        Applies this matrix to the point.

        @param p
            The point receiving the transformations.

        @return The point with the transformations applied.
    */
    wxPoint2DDouble TransformPoint(const wxPoint2DDouble& p) const;
    void TransformPoint(wxDouble* x, wxDouble* y) const;

    /**
        Applies the linear part of this matrix, i.e.\ without translation.

        @param p
            The source receiving the transformations.

        @return The source with the transformations applied.
    */
    wxPoint2DDouble TransformDistance(const wxPoint2DDouble& p) const;
    void TransformDistance(wxDouble* dx, wxDouble* dy) const;

};
