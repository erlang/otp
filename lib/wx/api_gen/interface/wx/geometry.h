/////////////////////////////////////////////////////////////////////////////
// Name:        geometry.h
// Purpose:     interface of geometry classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


enum wxOutCode
{
    wxInside = 0x00 ,
    wxOutLeft = 0x01 ,
    wxOutRight = 0x02 ,
    wxOutTop = 0x08 ,
    wxOutBottom = 0x04
};


class wxPoint2DInt
{
public :
    wxPoint2DInt();
    wxPoint2DInt( wxInt32 x , wxInt32 y );
    wxPoint2DInt( const wxPoint2DInt &pt );
    wxPoint2DInt( const wxPoint &pt );

    // noops for this class, just return the coords
    void GetFloor( wxInt32 *x , wxInt32 *y ) const;
    void GetRounded( wxInt32 *x , wxInt32 *y ) const;

    wxDouble GetVectorLength() const;
           wxDouble GetVectorAngle() const;
    void SetVectorLength( wxDouble length );
           void SetVectorAngle( wxDouble degrees );
    // set the vector length to 1.0, preserving the angle
    void Normalize();

    wxDouble GetDistance( const wxPoint2DInt &pt ) const;
    wxDouble GetDistanceSquare( const wxPoint2DInt &pt ) const;
    wxInt32 GetDotProduct( const wxPoint2DInt &vec ) const;
    wxInt32 GetCrossProduct( const wxPoint2DInt &vec ) const;

    // the reflection of this point
    wxPoint2DInt operator-();

    wxPoint2DInt& operator=(const wxPoint2DInt& pt);
    wxPoint2DInt& operator+=(const wxPoint2DInt& pt);
    wxPoint2DInt& operator-=(const wxPoint2DInt& pt);
    wxPoint2DInt& operator*=(const wxPoint2DInt& pt);
    wxPoint2DInt& operator*=(wxDouble n);
    wxPoint2DInt& operator*=(wxInt32 n);
    wxPoint2DInt& operator/=(const wxPoint2DInt& pt);
    wxPoint2DInt& operator/=(wxDouble n);
    wxPoint2DInt& operator/=(wxInt32 n);
    operator wxPoint() const;
    bool operator==(const wxPoint2DInt& pt) const;
    bool operator!=(const wxPoint2DInt& pt) const;

    wxInt32 m_x;
    wxInt32 m_y;
};

wxPoint2DInt operator+(const wxPoint2DInt& pt1 , const wxPoint2DInt& pt2);
wxPoint2DInt operator-(const wxPoint2DInt& pt1 , const wxPoint2DInt& pt2);
wxPoint2DInt operator*(const wxPoint2DInt& pt1 , const wxPoint2DInt& pt2);
wxPoint2DInt operator*(wxInt32 n , const wxPoint2DInt& pt);
wxPoint2DInt operator*(wxInt32 n , const wxPoint2DInt& pt);
wxPoint2DInt operator*(const wxPoint2DInt& pt , wxInt32 n);
wxPoint2DInt operator*(const wxPoint2DInt& pt , wxInt32 n);
wxPoint2DInt operator/(const wxPoint2DInt& pt1 , const wxPoint2DInt& pt2);
wxPoint2DInt operator/(const wxPoint2DInt& pt , wxInt32 n);
wxPoint2DInt operator/(const wxPoint2DInt& pt , wxInt32 n);


// wxPoint2Ds represent a point or a vector in a 2d coordinate system

class wxPoint2DDouble
{
public :
    wxPoint2DDouble();
    wxPoint2DDouble( wxDouble x , wxDouble y );
    wxPoint2DDouble( const wxPoint2DDouble &pt );
    wxPoint2DDouble( const wxPoint2DInt &pt );
    wxPoint2DDouble( const wxPoint &pt );

    // two different conversions to integers, floor and rounding
    void GetFloor( wxInt32 *x , wxInt32 *y ) const;
    void GetRounded( wxInt32 *x , wxInt32 *y ) const;

    wxDouble GetVectorLength() const;
     wxDouble GetVectorAngle() const ;
    void SetVectorLength( wxDouble length );
    void SetVectorAngle( wxDouble degrees );
    // set the vector length to 1.0, preserving the angle
    void Normalize();

    wxDouble GetDistance( const wxPoint2DDouble &pt ) const;
    wxDouble GetDistanceSquare( const wxPoint2DDouble &pt ) const;
    wxDouble GetDotProduct( const wxPoint2DDouble &vec ) const;
    wxDouble GetCrossProduct( const wxPoint2DDouble &vec ) const;

    // the reflection of this point
    wxPoint2DDouble operator-();

    wxPoint2DDouble& operator=(const wxPoint2DDouble& pt);
    wxPoint2DDouble& operator+=(const wxPoint2DDouble& pt);
    wxPoint2DDouble& operator-=(const wxPoint2DDouble& pt);
    wxPoint2DDouble& operator*=(const wxPoint2DDouble& pt);
    wxPoint2DDouble& operator*=(wxDouble n);
    wxPoint2DDouble& operator*=(wxInt32 n);
    wxPoint2DDouble& operator/=(const wxPoint2DDouble& pt);
    wxPoint2DDouble& operator/=(wxDouble n);
    wxPoint2DDouble& operator/=(wxInt32 n);

    bool operator==(const wxPoint2DDouble& pt) const;
    bool operator!=(const wxPoint2DDouble& pt) const;

    wxDouble m_x;
    wxDouble m_y;
};

wxPoint2DDouble operator+(const wxPoint2DDouble& pt1 , const wxPoint2DDouble& pt2);
wxPoint2DDouble operator-(const wxPoint2DDouble& pt1 , const wxPoint2DDouble& pt2);
wxPoint2DDouble operator*(const wxPoint2DDouble& pt1 , const wxPoint2DDouble& pt2);
wxPoint2DDouble operator*(wxDouble n , const wxPoint2DDouble& pt);
wxPoint2DDouble operator*(wxInt32 n , const wxPoint2DDouble& pt);
wxPoint2DDouble operator*(const wxPoint2DDouble& pt , wxDouble n);
wxPoint2DDouble operator*(const wxPoint2DDouble& pt , wxInt32 n);
wxPoint2DDouble operator/(const wxPoint2DDouble& pt1 , const wxPoint2DDouble& pt2);
wxPoint2DDouble operator/(const wxPoint2DDouble& pt , wxDouble n);
wxPoint2DDouble operator/(const wxPoint2DDouble& pt , wxInt32 n);



// wxRect2Ds are a axis-aligned rectangles, each side of the rect is parallel to the x- or m_y- axis. The rectangle is either defined by the
// top left and bottom right corner, or by the top left corner and size. A point is contained within the rectangle if
// left <= x < right  and top <= m_y < bottom , thus it is a half open interval.

class wxRect2DDouble
{
public:
    wxRect2DDouble();
    wxRect2DDouble(wxDouble x, wxDouble y, wxDouble w, wxDouble h);

    // single attribute accessors

    wxPoint2DDouble GetPosition() const;
    wxSize GetSize() const;

    // for the edge and corner accessors there are two setters counterparts, the Set.. functions keep the other corners at their
        // position whenever sensible, the Move.. functions keep the size of the rect and move the other corners appropriately

    wxDouble GetLeft() const;
    void SetLeft( wxDouble n );
    void MoveLeftTo( wxDouble n );
    wxDouble GetTop() const;
    void SetTop( wxDouble n );
    void MoveTopTo( wxDouble n );
    wxDouble GetBottom() const;
    void SetBottom( wxDouble n );
    void MoveBottomTo( wxDouble n );
    wxDouble GetRight() const;
    void SetRight( wxDouble n );
    void MoveRightTo( wxDouble n );

    wxPoint2DDouble GetLeftTop() const;
    void SetLeftTop( const wxPoint2DDouble &pt );
    void MoveLeftTopTo( const wxPoint2DDouble &pt );
    wxPoint2DDouble GetLeftBottom() const;
    void SetLeftBottom( const wxPoint2DDouble &pt );
    void MoveLeftBottomTo( const wxPoint2DDouble &pt );
    wxPoint2DDouble GetRightTop() const;
    void SetRightTop( const wxPoint2DDouble &pt );
    void MoveRightTopTo( const wxPoint2DDouble &pt );
    wxPoint2DDouble GetRightBottom() const;
    void SetRightBottom( const wxPoint2DDouble &pt );
    void MoveRightBottomTo( const wxPoint2DDouble &pt );
    wxPoint2DDouble GetCentre() const;
    void SetCentre( const wxPoint2DDouble &pt );
    void MoveCentreTo( const wxPoint2DDouble &pt );
    wxOutCode GetOutCode( const wxPoint2DDouble &pt ) const;
    wxOutCode GetOutcode(const wxPoint2DDouble &pt) const;
    bool Contains( const wxPoint2DDouble &pt ) const;
    bool Contains( const wxRect2DDouble &rect ) const;
    bool IsEmpty() const;
    bool HaveEqualSize( const wxRect2DDouble &rect ) const;

    void Inset( wxDouble x , wxDouble y );
    void Inset( wxDouble left , wxDouble top ,wxDouble right , wxDouble bottom  );
    void Offset( const wxPoint2DDouble &pt );

    void ConstrainTo( const wxRect2DDouble &rect );

    wxPoint2DDouble Interpolate( wxInt32 widthfactor , wxInt32 heightfactor );

    static void Intersect( const wxRect2DDouble &src1 , const wxRect2DDouble &src2 , wxRect2DDouble *dest );
    void Intersect( const wxRect2DDouble &otherRect );
    wxRect2DDouble CreateIntersection( const wxRect2DDouble &otherRect ) const;
    bool Intersects( const wxRect2DDouble &rect ) const;

    static void Union( const wxRect2DDouble &src1 , const wxRect2DDouble &src2 , wxRect2DDouble *dest );
    void Union( const wxRect2DDouble &otherRect );
    void Union( const wxPoint2DDouble &pt );
    wxRect2DDouble CreateUnion( const wxRect2DDouble &otherRect ) const;

    void Scale( wxDouble f );
    void Scale( wxInt32 num , wxInt32 denum );

    wxRect2DDouble& operator = (const wxRect2DDouble& rect);
    bool operator == (const wxRect2DDouble& rect) const;
    bool operator != (const wxRect2DDouble& rect) const;

    wxDouble  m_x;
    wxDouble  m_y;
    wxDouble  m_width;
    wxDouble m_height;
};




// wxRect2Ds are a axis-aligned rectangles, each side of the rect is parallel to the x- or m_y- axis. The rectangle is either defined by the
// top left and bottom right corner, or by the top left corner and size. A point is contained within the rectangle if
// left <= x < right  and top <= m_y < bottom , thus it is a half open interval.

class wxRect2DInt
{
public:
    wxRect2DInt();
    wxRect2DInt( const wxRect& r );
    wxRect2DInt(wxInt32 x, wxInt32 y, wxInt32 w, wxInt32 h);
    wxRect2DInt(const wxPoint2DInt& topLeft, const wxPoint2DInt& bottomRight);
    wxRect2DInt(const wxPoint2DInt& pos, const wxSize& size);
    wxRect2DInt(const wxRect2DInt& rect);

        // single attribute accessors

    wxPoint2DInt GetPosition() const;
    wxSize GetSize() const;

        // for the edge and corner accessors there are two setters counterparts, the Set.. functions keep the other corners at their
        // position whenever sensible, the Move.. functions keep the size of the rect and move the other corners appropriately

    wxInt32 GetLeft() const;
    void SetLeft( wxInt32 n );
    void MoveLeftTo( wxInt32 n );
    wxInt32 GetTop() const;
    void SetTop( wxInt32 n );
    void MoveTopTo( wxInt32 n );
    wxInt32 GetBottom() const;
    void SetBottom( wxInt32 n );
    void MoveBottomTo( wxInt32 n );
    wxInt32 GetRight() const;
    void SetRight( wxInt32 n );
    void MoveRightTo( wxInt32 n );

    wxPoint2DInt GetLeftTop() const;
    void SetLeftTop( const wxPoint2DInt &pt ) ;
    void MoveLeftTopTo( const wxPoint2DInt &pt ) ;
    wxPoint2DInt GetLeftBottom() const ;
    void SetLeftBottom( const wxPoint2DInt &pt ) ;
    void MoveLeftBottomTo( const wxPoint2DInt &pt ) ;
    wxPoint2DInt GetRightTop() const ;
    void SetRightTop( const wxPoint2DInt &pt ) ;
    void MoveRightTopTo( const wxPoint2DInt &pt ) ;
    wxPoint2DInt GetRightBottom() const ;
    void SetRightBottom( const wxPoint2DInt &pt ) ;
    void MoveRightBottomTo( const wxPoint2DInt &pt ) ;
    wxPoint2DInt GetCentre() const ;
    void SetCentre( const wxPoint2DInt &pt ) ;
    void MoveCentreTo( const wxPoint2DInt &pt ) ;
    wxOutCode GetOutCode( const wxPoint2DInt &pt ) const;
    wxOutCode GetOutcode( const wxPoint2DInt &pt ) const;
    bool Contains( const wxPoint2DInt &pt ) const;
    bool Contains( const wxRect2DInt &rect ) const;
    bool IsEmpty() const;
    bool HaveEqualSize( const wxRect2DInt &rect ) const;

    void Inset( wxInt32 x , wxInt32 y );
    void Inset( wxInt32 left , wxInt32 top ,wxInt32 right , wxInt32 bottom  );
    void Offset( const wxPoint2DInt &pt );
    void ConstrainTo( const wxRect2DInt &rect );
    wxPoint2DInt Interpolate( wxInt32 widthfactor , wxInt32 heightfactor );

    static void Intersect( const wxRect2DInt &src1 , const wxRect2DInt &src2 , wxRect2DInt *dest );
    void Intersect( const wxRect2DInt &otherRect );
    wxRect2DInt CreateIntersection( const wxRect2DInt &otherRect ) const;
    bool Intersects( const wxRect2DInt &rect ) const;

    static void Union( const wxRect2DInt &src1 , const wxRect2DInt &src2 , wxRect2DInt *dest );
    void Union( const wxRect2DInt &otherRect );
    void Union( const wxPoint2DInt &pt );
    wxRect2DInt CreateUnion( const wxRect2DInt &otherRect ) const;

    void Scale( wxInt32 f );
    void Scale( wxInt32 num , wxInt32 denum );

    wxRect2DInt& operator = (const wxRect2DInt& rect);
    bool operator == (const wxRect2DInt& rect) const;
    bool operator != (const wxRect2DInt& rect) const;


    wxInt32 m_x;
    wxInt32 m_y;
    wxInt32 m_width;
    wxInt32 m_height;
};





class wxTransform2D
{
public :
    virtual ~wxTransform2D();
    virtual void                    Transform( wxPoint2DInt* pt )const  = 0;
    virtual void                    Transform( wxRect2DInt* r ) const;
    virtual wxPoint2DInt    Transform( const wxPoint2DInt &pt ) const;
    virtual wxRect2DInt        Transform( const wxRect2DInt &r ) const ;

    virtual void                    InverseTransform( wxPoint2DInt* pt ) const  = 0;
    virtual void                    InverseTransform( wxRect2DInt* r ) const ;
    virtual wxPoint2DInt    InverseTransform( const wxPoint2DInt &pt ) const ;
    virtual wxRect2DInt        InverseTransform( const wxRect2DInt &r ) const ;
};


