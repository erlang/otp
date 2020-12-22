/////////////////////////////////////////////////////////////////////////////
// Name:        quantize.h
// Purpose:     interface of wxQuantize
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxQuantize

    Performs quantization, or colour reduction, on a wxImage.

    Functions in this class are static and so a wxQuantize object need not be
    created.

    @library{wxcore}
    @category{misc}
*/
class wxQuantize : public wxObject
{
public:
    /**
        Constructor. You do not need to construct a wxQuantize object since its
        functions are static.
    */
    wxQuantize();

    /**
        Converts input bitmap(s) into 8bit representation with custom palette.
        @a in_rows and @a out_rows are arrays [0..h-1] of pointer to rows
        (@a in_rows contains @a w * 3 bytes per row, @a out_rows @a w bytes per row).
        Fills @a out_rows with indexes into palette (which is also stored into @a palette
        variable).
    */
    static void DoQuantize(unsigned int w, unsigned int h,
                           unsigned char** in_rows, unsigned char** out_rows,
                           unsigned char* palette, int desiredNoColours);

    /**
        Reduce the colours in the source image and put the result into the destination image.
        Both images may be the same, to overwrite the source image.

        Specify an optional palette pointer to receive the resulting palette.
        This palette may be passed to ConvertImageToBitmap, for example.
    */
    static bool Quantize(const wxImage& src, wxImage& dest,
                         wxPalette** pPalette, int desiredNoColours = 236,
                         unsigned char** eightBitData = 0,
                         int flags = wxQUANTIZE_INCLUDE_WINDOWS_COLOURS|
                                     wxQUANTIZE_FILL_DESTINATION_IMAGE|
                                     wxQUANTIZE_RETURN_8BIT_DATA);

    /**
        This version sets a palette in the destination image so you don't
        have to manage it yourself.
    */
    static bool Quantize(const wxImage& src, wxImage& dest,
                         int desiredNoColours = 236,
                         unsigned char** eightBitData = 0,
                         int flags = wxQUANTIZE_INCLUDE_WINDOWS_COLOURS|
                                     wxQUANTIZE_FILL_DESTINATION_IMAGE|
                                     wxQUANTIZE_RETURN_8BIT_DATA);
};

