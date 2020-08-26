/////////////////////////////////////////////////////////////////////////////
// Name:        base64.h
// Purpose:     interface of global functions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_misc */
//@{

/**
    Elements of this enum specify the possible behaviours of wxBase64Decode
    when an invalid character is encountered.
*/
enum wxBase64DecodeMode
{
    wxBase64DecodeMode_Strict,  ///< Normal behaviour: stop at any invalid characters.
    wxBase64DecodeMode_SkipWS,  ///< Skip whitespace characters.
    wxBase64DecodeMode_Relaxed  ///< The most lenient behaviour: simply ignore all invalid characters.
};

/**
    This function encodes the given data using base64.

    To allocate the buffer of the correct size, use wxBase64EncodedSize() or
    call this function with @a dst set to @NULL -- it will then return the
    necessary buffer size.

    This raw encoding function overload writes the output string into the
    provided buffer; the other overloads return it as a wxString.

    @param dst
        The output buffer, may be @NULL to retrieve the needed buffer size.
    @param dstLen
        The output buffer size, ignored if dst is @NULL.
    @param src
        The input buffer, must not be @NULL.
    @param srcLen
        The length of the input data.

    @return @c wxCONV_FAILED if the output buffer is too small.

    @header{wx/base64.h}
*/
size_t wxBase64Encode(char* dst, size_t dstLen,
                      const void* src,
                      size_t srcLen);

/**
    This function encodes the given data using base64 and returns the output as
    a wxString.

    There is no error return.

    @param src
        The input buffer, must not be @NULL.
    @param srcLen
        The length of the input data.

    @header{wx/base64.h}
*/
wxString wxBase64Encode(const void* src, size_t srcLen);

/**
    This function encodes the given data using base64 and returns the output as
    a wxString.

    There is no error return.

    @header{wx/base64.h}
*/
wxString wxBase64Encode(const wxMemoryBuffer& buf);


/**
    Returns the size of the buffer necessary to contain the data encoded in a
    base64 string of length @e srcLen. This can be useful for allocating a
    buffer to be passed to wxBase64Decode().

    @header{wx/base64.h}
*/
size_t wxBase64DecodedSize(size_t srcLen);

/**
    Returns the length of the string with base64 representation of a buffer of
    specified size @e len. This can be useful for allocating the buffer passed
    to wxBase64Encode().

    @header{wx/base64.h}
*/
size_t wxBase64EncodedSize(size_t len);

/**
    This function decodes a Base64-encoded string.

    This overload is a raw decoding function and decodes the data into the
    provided buffer @a dst of the given size @e dstLen. An error is returned if
    the buffer is not large enough -- that is not at least
    wxBase64DecodedSize(srcLen) bytes. Notice that the buffer will @e not be
    @NULL-terminated.

    This overload returns the number of bytes written to the buffer or the
    necessary buffer size if @a dst was @NULL or @c wxCONV_FAILED on error,
    e.g. if the output buffer is too small or invalid characters were
    encountered in the input string.

    @param dst
        Pointer to output buffer, may be @NULL to just compute the necessary
        buffer size.
    @param dstLen
        The size of the output buffer, ignored if dst is @NULL.
    @param src
        The input string, must not be @NULL. For the version using wxString,
        the input string should contain only ASCII characters.
    @param srcLen
        The length of the input string or special value wxNO_LEN if the string
        is @NULL-terminated and the length should be computed by this function
        itself.
    @param mode
        This parameter specifies the function behaviour when invalid characters
        are encountered in input. By default, any such character stops the
        decoding with error. If the mode is wxBase64DecodeMode_SkipWS, then the
        white space characters are silently skipped instead. And if it is
        wxBase64DecodeMode_Relaxed, then all invalid characters are skipped.
    @param posErr
        If this pointer is non-@NULL and an error occurs during decoding, it is
        filled with the index of the invalid character.

    @header{wx/base64.h}
*/
size_t wxBase64Decode(void* dst, size_t dstLen,
                      const char* src,
                      size_t srcLen = wxNO_LEN,
                      wxBase64DecodeMode mode = wxBase64DecodeMode_Strict,
                      size_t *posErr = NULL);

/**
    Decode a Base64-encoded wxString.

    See the wxBase64Decode(void*,size_t,const char*,size_t,wxBase64DecodeMode,size_t*)
    overload for more information about the parameters of this function, the
    only difference between it and this one is that a wxString is used instead
    of a @c char* pointer and its length.

    @since 2.9.1

    @header{wx/base64.h}
 */
size_t wxBase64Decode(void* dst, size_t dstLen,
                      const wxString& str,
                      wxBase64DecodeMode mode = wxBase64DecodeMode_Strict,
                      size_t *posErr = NULL);

/**
    Decode a Base64-encoded string and return decoded contents in a buffer.

    See the wxBase64Decode(void*,size_t,const char*,size_t,wxBase64DecodeMode,size_t*)
    overload for more information about the parameters of this function. The
    difference of this overload is that it allocates a buffer of necessary size
    on its own and returns it, freeing you from the need to do it manually.
    Because of this, it is simpler to use and is recommended for normal use.

    @header{wx/base64.h}
*/
wxMemoryBuffer wxBase64Decode(const char* src,
                              size_t srcLen = wxNO_LEN,
                              wxBase64DecodeMode mode = wxBase64DecodeMode_Strict,
                              size_t *posErr = NULL);

/**
    Decode a Base64-encoded wxString and return decoded contents in a buffer.

    See the wxBase64Decode(void*,size_t,const char*,size_t,wxBase64DecodeMode,size_t*)
    overload for more information about the parameters of this function.

    This overload takes as input a wxString and returns the internally-allocated
    memory as a wxMemoryBuffer, containing the Base64-decoded data.

    @header{wx/base64.h}
*/
wxMemoryBuffer wxBase64Decode(const wxString& src,
                              wxBase64DecodeMode mode = wxBase64DecodeMode_Strict,
                              size_t *posErr = NULL);

//@}

