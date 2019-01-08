// %CopyrightBegin%
//
// Copyright Doug Hogan 2019. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// %CopyrightEnd%

// Coccinelle script to help verify the subset of OpenSSL calls used by Erlang.
// http://coccinelle.lip6.fr
// https://github.com/coccinelle/coccinelle
//
// These work with the Erlang code because it has a rigid coding pattern.
// $ spatch.opt --all-includes -sp_file check_openssl.cocci -dir .

// TODO: These APIs may not have a single check that covers all cases
// or may not be necessary to check.
//
// BN_GENCB_get_arg
// BN_bn2bin
// BN_cmp
// BN_is_bit_set
// BN_is_negative
// BN_is_zero
// BN_num_bits
// DH_get0_key
// DH_size
// EC_GROUP_get_degree
// EC_KEY_get0_group
// EC_KEY_get0_private_key
// EC_KEY_get0_public_key
// EC_KEY_get_conv_form
// EVP_CIPHER_block_size
// EVP_CIPHER_iv_length
// EVP_CIPHER_type
// EVP_MD_CTX_md
// EVP_MD_size
// EVP_aes_128_cbc
// EVP_aes_128_ccm
// EVP_aes_128_cfb128
// EVP_aes_128_cfb8
// EVP_aes_128_ctr
// EVP_aes_128_ecb
// EVP_aes_128_gcm
// EVP_aes_192_cbc
// EVP_aes_192_ccm
// EVP_aes_192_ctr
// EVP_aes_192_ecb
// EVP_aes_192_gcm
// EVP_aes_256_cbc
// EVP_aes_256_ccm
// EVP_aes_256_ctr
// EVP_aes_256_ecb
// EVP_aes_256_gcm
// EVP_bf_cbc
// EVP_bf_cfb64
// EVP_bf_ecb
// EVP_bf_ofb
// EVP_chacha20
// EVP_chacha20_poly1305
// EVP_des_cbc
// EVP_des_cfb8
// EVP_des_ecb
// EVP_des_ede3_cbc
// EVP_des_ede3_cfb8
// EVP_md4
// EVP_md5
// EVP_rc2_cbc
// EVP_ripemd160
// EVP_sha1
// EVP_sha224
// EVP_sha256
// EVP_sha384
// EVP_sha3_224
// EVP_sha3_256
// EVP_sha3_384
// EVP_sha3_512
// EVP_sha512
// OpenSSL_version
// OpenSSL_version_num
// PEM_read_PrivateKey
// PEM_read_PUBKEY
// RSA_size

// Unusual API for OpenSSL: 0 or positive on success and negative value(s) on error.
@openssl_check_negative@
identifier FUNCNEG =~ "^(DH_compute_key|RSA_padding_check_SSLv23)$";
expression X;
identifier L;
position p;
@@

  if (
(
  FUNCNEG(...)@p < 0
|
  (X = FUNCNEG(...)@p) < 0
)
  )
      goto L;

// Unusual API for OpenSSL: positive on success or else error
@openssl_check_positive@
identifier FUNCPOS =~ "^(ECDH_compute_key|EVP_CIPHER_asn1_to_param|EVP_CIPHER_param_to_asn1|EVP_PKEY_CTX_ctrl|RSA_pkey_ctx_ctrl)$";
identifier L;
expression X;
position p;
@@

  if (
(
  FUNCPOS(...)@p < 1
|
  (X = FUNCPOS(...)@p) < 1
)
  )
      goto L;

// Unusual API for OpenSSL: 0=success.
@openssl_check_0@
identifier L;
expression X;
identifier FUNC0 =~ "^(AES_set_decrypt_key|AES_set_encrypt_key|CRYPTO_gcm128_aad|CRYPTO_gcm128_decrypt|CRYPTO_gcm128_finish)$";
position p;
@@

  if (
(
  FUNC0(...)@p != 0
|
  (X = FUNC0(...)@p) != 0
)
  )
      goto L;

// These do not necessarily allocate resources but they may return NULL.
@openssl_check_null@
expression X;
identifier L;
identifier FUNCNULL =~ "^(BN_CTX_new|BN_GENCB_new|BN_MONT_CTX_new|BN_bin2bn|BN_dup|BN_generate_prime|BN_new|CMAC_CTX_new|CRYPTO_clear_realloc|CRYPTO_gcm128_new|CRYPTO_malloc|CRYPTO_realloc|CRYPTO_zalloc|DH_generate_parameters|DH_new|DSA_new|EC_GROUP_dup|EC_GROUP_get0_generator|EC_GROUP_method_of|EC_GROUP_new_curve_GFm|EC_GROUP_new_curve_GFp|EC_KEY_copy|EC_KEY_dup|EC_KEY_get0_engine|EC_KEY_new|EC_KEY_new_by_curve_name|EC_POINT_bn2point|EC_POINT_dup|EC_POINT_new|EC_POINT_point2bn|ENGINE_by_id|ENGINE_get_cipher_engine|ENGINE_get_default_DH|ENGINE_get_default_DSA|ENGINE_get_default_RAND|ENGINE_get_default_RSA|ENGINE_get_digest_engine|ENGINE_get_first|ENGINE_get_id|ENGINE_get_last|ENGINE_get_name|ENGINE_get_next|ENGINE_get_prev|ENGINE_load_private_key|ENGINE_load_public_key|ENGINE_new|EVP_CIPHER_CTX_new|EVP_MAC_CTX_new|EVP_MAC_CTX_new_id|EVP_MD_CTX_new|EVP_MD_meth_new|EVP_PKEY_CTX_new|EVP_PKEY_CTX_new_id|EVP_PKEY_get1_DH|EVP_PKEY_get1_DSA|EVP_PKEY_get1_EC_KEY|EVP_PKEY_get1_RSA|EVP_PKEY_new|EVP_PKEY_new_raw_private_key|EVP_PKEY_new_raw_public_key|EVP_get_cipherbyname|EVP_get_cipherbynid|EVP_get_cipherbyobj|EVP_get_macbyname|EVP_get_macbynid|EVP_get_macbyobj|HMAC|HMAC_CTX_new|OPENSSL_buf2hexstr|OPENSSL_clear_realloc|OPENSSL_hexstr2buf|OPENSSL_malloc|OPENSSL_realloc|OPENSSL_strdup|OPENSSL_strndup|OPENSSL_zalloc|RSA_meth_dup|RSA_meth_new|RSA_new)$";
position p;
@@

(
  if ((X = FUNCNULL(...)@p) == NULL)
      goto L;
|
  X = FUNCNULL(...)@p;
  if (X == NULL)
      goto L;
)

// non-zero=success, 0=failure.  These can be safely used with !
@openssl_check_not@
expression X;
identifier L;
identifier FUNCNOT =~ "^(BN_add|BN_div|BN_exp|BN_from_montgomery|BN_gcd|BN_generate_prime_ex|BN_mod|BN_mod_add|BN_mod_exp|BN_mod_mul|BN_mod_mul_montgomery|BN_mod_sqr|BN_mod_sub|BN_mul|BN_nnmod|BN_priv_rand|BN_priv_rand_range|BN_pseudo_rand|BN_pseudo_rand_range|BN_rand|BN_rand_range|BN_set_bit|BN_set_word|BN_sqr|BN_sub|BN_to_montgomery|CMAC_Final|CMAC_Init|CMAC_Update|CRYPTO_set_mem_debug|CRYPTO_set_mem_functions|DH_check|DH_check_ex|DH_check_params|DH_check_pub_key_ex|DH_generate_key|DH_generate_parameters_ex|DH_set0_key|DH_set0_pqg|DH_set_length|DSA_set0_key|DSA_set0_pqg|EC_GROUP_check|EC_GROUP_check_discriminant|EC_GROUP_copy|EC_GROUP_get_curve_name|EC_GROUP_get_pentanomial_basis|EC_GROUP_get_trinomial_basis|EC_GROUP_precompute_mult|EC_GROUP_set_generator|EC_GROUP_set_seed|EC_KEY_check_key|EC_KEY_generate_key|EC_KEY_key2buf|EC_KEY_oct2key|EC_KEY_oct2priv|EC_KEY_precompute_mult|EC_KEY_priv2buf|EC_KEY_priv2oct|EC_KEY_set_group|EC_KEY_set_private_key|EC_KEY_set_public_key|EC_KEY_set_public_key_affine_coordinates|EC_KEY_up_ref|EC_POINT_add|EC_POINT_copy|EC_POINT_dbl|EC_POINT_get_Jprojective_coordinates_GFp|EC_POINT_get_affine_coordinates_GF2m|EC_POINT_get_affine_coordinates_GFp|EC_POINT_invert|EC_POINT_make_affine|EC_POINT_mul|EC_POINT_oct2point|EC_POINT_point2oct|EC_POINT_set_Jprojective_coordinates_GFp|EC_POINT_set_affine_coordinates_GF2m|EC_POINT_set_affine_coordinates_GFp|EC_POINT_set_compressed_coordinates_GF2m|EC_POINT_set_compressed_coordinates_GFp|EC_POINT_set_to_infinity|EC_POINTs_make_affine|EC_POINTs_mul|ENGINE_add|ENGINE_ctrl_cmd|ENGINE_ctrl_cmd_string|ENGINE_finish|ENGINE_free|ENGINE_init|ENGINE_register_DH|ENGINE_register_DSA|ENGINE_register_EC|ENGINE_register_RAND|ENGINE_register_RSA|ENGINE_register_all_complete|ENGINE_register_ciphers|ENGINE_register_complete|ENGINE_register_digests|ENGINE_register_pkey_asn1_meths|ENGINE_register_pkey_meths|ENGINE_remove|ENGINE_set_RSA|ENGINE_set_default|ENGINE_set_default_DH|ENGINE_set_default_DSA|ENGINE_set_default_EC|ENGINE_set_default_RAND|ENGINE_set_default_RSA|ENGINE_set_digests|ENGINE_set_id|ENGINE_set_init_function|ENGINE_set_load_privkey_function|ENGINE_set_load_pubkey_function|ENGINE_set_name|ENGINE_up_ref|HMAC_CTX_copy|HMAC_CTX_reset|HMAC_Final|HMAC_Init_ex|HMAC_Update|MD2_Init|MD2_Update|MD2_Final|MD4_Init|MD4_Update|MD4_Final|MD5_Init|MD5_Update|MD5_Final|OPENSSL_init_crypto|OPENSSL_mem_debug_pop|OPENSSL_mem_debug_push|RSA_generate_key_ex|RSA_generate_multi_prime_key|RSA_meth_set_finish|RSA_meth_set_sign|RSA_meth_set_verify|RSA_padding_add_SSLv23|RSA_set0_crt_params|RSA_set0_factors|RSA_set0_key|RSA_set0_multi_prime_params)$";
position p;
@@

  if (
(
  !FUNCNOT(...)@p
|
  !(X = FUNCNOT)@p
)
  )
      goto L;

// 1=success.  These may have == 0 or <= 0 or non-one failure so we explicitly check for success.
// Since some EVP_* functions use failure == 0 and others use <= 0, we consolidate all
// EVP_* calls into here so it's less error prone.  In such cases, they all use 1 for success.
@openssl_check_1@
expression X;
identifier L;
identifier FUNC1 =~ "^(EVP_CIPHER_CTX_copy|EVP_CIPHER_CTX_ctrl|EVP_CIPHER_CTX_rand_key|EVP_CIPHER_CTX_reset|EVP_CIPHER_CTX_set_key_length|EVP_CIPHER_CTX_set_padding|EVP_CipherFinal_ex|EVP_CipherInit_ex|EVP_CipherUpdate|EVP_DecryptFinal_ex|EVP_DecryptInit_ex|EVP_DecryptUpdate|EVP_Digest|EVP_DigestFinal|EVP_DigestFinal_ex|EVP_DigestInit|EVP_DigestInit_ex|EVP_DigestSign|EVP_DigestSignInit|EVP_DigestSignUpdate|EVP_DigestSignaFinal|EVP_DigestUpdate|EVP_DigestVerify|EVP_DigestVerifyInit|EVP_EncryptFinal_ex|EVP_EncryptInit_ex|EVP_EncryptUpdate|EVP_MAC_CTX_copy|EVP_MAC_ctrl|EVP_MAC_ctrl_str|EVP_MAC_hex2ctrl|EVP_MAC_init|EVP_MAC_reset|EVP_MAC_str2ctrl|EVP_MAC_update|EVP_MD_CTX_copy|EVP_MD_CTX_copy_ex|EVP_MD_CTX_ctrl|EVP_MD_meth_set_app_datasize|EVP_MD_meth_set_cleanup|EVP_MD_meth_set_copy|EVP_MD_meth_set_ctrl|EVP_MD_meth_set_final|EVP_MD_meth_set_flags|EVP_MD_meth_set_init|EVP_MD_meth_set_input_blocksize|EVP_MD_meth_set_result_size|EVP_MD_meth_set_update|EVP_PKEY_CTX_set_rsa_mgf1_md|EVP_PKEY_CTX_set_rsa_padding|EVP_PKEY_CTX_set_rsa_pss_saltlen|EVP_PKEY_CTX_set_signature|EVP_PKEY_assign|EVP_PKEY_assign_DSA|EVP_PKEY_assign_EC_KEY|EVP_PKEY_assign_RSA|EVP_PKEY_decrypt|EVP_PKEY_decrypt_init|EVP_PKEY_derive|EVP_PKEY_derive_init|EVP_PKEY_derive_set_peer|EVP_PKEY_encrypt|EVP_PKEY_encrypt_init|EVP_PKEY_get1_DH|EVP_PKEY_get_raw_private_key|EVP_PKEY_get_raw_public_key|EVP_PKEY_keygen|EVP_PKEY_keygen_init|EVP_PKEY_set1_DH|EVP_PKEY_sign|EVP_PKEY_sign_init|EVP_PKEY_verify|EVP_PKEY_verify_init|EVP_PKEY_verify_recover|EVP_PKEY_verify_recover_init|EVP_add_mac|RAND_bytes|RAND_priv_bytes)$";
position p;
@@

  if (
(
  FUNC1(...)@p != 1
|
  (X = FUNC1(...)@p) != 1
)
  )
      goto L;


// These are void but here for completeness
@openssl_void@
identifier FUNCVOID =~ "^(AES_cfb128_encrypt|AES_cfb8_encrypt|AES_ige_encrypt|BN_GENCB_set|DSA_get0_key|DSA_get0_pqg|EC_GROUP_set_asn1_flag|EC_GROUP_set_point_conversion_form|ENGINE_get_static_state|ENGINE_unregister_DH|ENGINE_unregister_DSA|ENGINE_unregister_EC|ENGINE_unregister_RAND|ENGINE_unregister_RSA|ENGINE_unregister_ciphers|ENGINE_unregister_digests|ENGINE_unregister_pkey_asn1_meths|ENGINE_unregister_pkey_meths|OpenSSL_add_all_ciphers|OpenSSL_add_all_digests|RAND_seed|RC4|RC4_set_key|RSA_get0_crt_params|RSA_get0_factors|RSA_get0_key)$";
position p;
@@

  FUNCVOID(...)@p;


// Traditionally, OpenSSL didn't adhere to the semantics of free() calls
// allowing for NULL.  However, they have been changing it over time.
// Since Erlang allows for unmaintained versions of OpenSSL, be conservative
// and assume the worst.
@openssl_free@
expression X;
identifier FUNCFREE =~ "^(BN_CTX_free|BN_GENCB_free|BN_clear_free|BN_free|CMAC_CTX_free|CRYPTO_free|DH_free|DSA_free|EC_GROUP_free|EC_KEY_free|EC_POINT_free|EVP_CIPHER_CTX_free|EVP_MD_CTX_free|EVP_PKEY_CTX_free|EVP_PKEY_free|HMAC_CTX_free|RSA_free|RSA_meth_free)$";
position p;
@@

  if (
(
  X
|
  X != NULL
)
  )
      FUNCFREE(X)@p;


// NOTE: Keep these in sync with the above definitions!
//
// Find all of the cases that we haven't marked safe positions of.
//
// This will flag a few false positives because the code isn't using the
// standard pattern.
//
// NOTE: You have to copy the regexps because there doesn't appear to be a way in
// coccinelle to reference a regexp identifier from another rule properly.
@openssl_check_NOT_SAFE@

identifier FUNCNEG =~ "^(DH_compute_key|RSA_padding_check_SSLv23)$";
position pneg != openssl_check_negative.p;

identifier FUNCPOS =~ "^(ECDH_compute_key|EVP_CIPHER_asn1_to_param|EVP_CIPHER_param_to_asn1|EVP_PKEY_CTX_ctrl|RSA_pkey_ctx_ctrl)$";
position ppos != openssl_check_positive.p;

identifier FUNC0 =~ "^(AES_set_decrypt_key|AES_set_encrypt_key|CRYPTO_gcm128_aad|CRYPTO_gcm128_decrypt|CRYPTO_gcm128_finish)$";
position p0 != openssl_check_0.p;

identifier FUNCNULL =~ "^(BN_CTX_new|BN_GENCB_new|BN_MONT_CTX_new|BN_bin2bn|BN_dup|BN_generate_prime|BN_new|CMAC_CTX_new|CRYPTO_clear_realloc|CRYPTO_gcm128_new|CRYPTO_malloc|CRYPTO_realloc|CRYPTO_zalloc|DH_generate_parameters|DH_new|DSA_new|EC_GROUP_dup|EC_GROUP_get0_generator|EC_GROUP_method_of|EC_GROUP_new_curve_GFm|EC_GROUP_new_curve_GFp|EC_KEY_copy|EC_KEY_dup|EC_KEY_get0_engine|EC_KEY_new|EC_KEY_new_by_curve_name|EC_POINT_bn2point|EC_POINT_dup|EC_POINT_new|EC_POINT_point2bn|ENGINE_by_id|ENGINE_get_cipher_engine|ENGINE_get_default_DH|ENGINE_get_default_DSA|ENGINE_get_default_RAND|ENGINE_get_default_RSA|ENGINE_get_digest_engine|ENGINE_get_first|ENGINE_get_id|ENGINE_get_last|ENGINE_get_name|ENGINE_get_next|ENGINE_get_prev|ENGINE_load_private_key|ENGINE_load_public_key|ENGINE_new|EVP_CIPHER_CTX_new|EVP_MAC_CTX_new|EVP_MAC_CTX_new_id|EVP_MD_CTX_new|EVP_MD_meth_new|EVP_PKEY_CTX_new|EVP_PKEY_CTX_new_id|EVP_PKEY_get1_DH|EVP_PKEY_get1_DSA|EVP_PKEY_get1_EC_KEY|EVP_PKEY_get1_RSA|EVP_PKEY_new|EVP_PKEY_new_raw_private_key|EVP_PKEY_new_raw_public_key|EVP_get_cipherbyname|EVP_get_cipherbynid|EVP_get_cipherbyobj|EVP_get_macbyname|EVP_get_macbynid|EVP_get_macbyobj|HMAC|HMAC_CTX_new|OPENSSL_buf2hexstr|OPENSSL_clear_realloc|OPENSSL_hexstr2buf|OPENSSL_malloc|OPENSSL_realloc|OPENSSL_strdup|OPENSSL_strndup|OPENSSL_zalloc|RSA_meth_dup|RSA_meth_new|RSA_new)$";
position pnull != openssl_check_null.p;

identifier FUNCNOT =~ "^(BN_add|BN_div|BN_exp|BN_from_montgomery|BN_gcd|BN_generate_prime_ex|BN_mod|BN_mod_add|BN_mod_exp|BN_mod_mul|BN_mod_mul_montgomery|BN_mod_sqr|BN_mod_sub|BN_mul|BN_nnmod|BN_priv_rand|BN_priv_rand_range|BN_pseudo_rand|BN_pseudo_rand_range|BN_rand|BN_rand_range|BN_set_bit|BN_set_word|BN_sqr|BN_sub|BN_to_montgomery|CMAC_Final|CMAC_Init|CMAC_Update|CRYPTO_set_mem_debug|CRYPTO_set_mem_functions|DH_check|DH_check_ex|DH_check_params|DH_check_pub_key_ex|DH_generate_key|DH_generate_parameters_ex|DH_set0_key|DH_set0_pqg|DH_set_length|DSA_set0_key|DSA_set0_pqg|EC_GROUP_check|EC_GROUP_check_discriminant|EC_GROUP_copy|EC_GROUP_get_curve_name|EC_GROUP_get_pentanomial_basis|EC_GROUP_get_trinomial_basis|EC_GROUP_precompute_mult|EC_GROUP_set_generator|EC_GROUP_set_seed|EC_KEY_check_key|EC_KEY_generate_key|EC_KEY_key2buf|EC_KEY_oct2key|EC_KEY_oct2priv|EC_KEY_precompute_mult|EC_KEY_priv2buf|EC_KEY_priv2oct|EC_KEY_set_group|EC_KEY_set_private_key|EC_KEY_set_public_key|EC_KEY_set_public_key_affine_coordinates|EC_KEY_up_ref|EC_POINT_add|EC_POINT_copy|EC_POINT_dbl|EC_POINT_get_Jprojective_coordinates_GFp|EC_POINT_get_affine_coordinates_GF2m|EC_POINT_get_affine_coordinates_GFp|EC_POINT_invert|EC_POINT_make_affine|EC_POINT_mul|EC_POINT_oct2point|EC_POINT_point2oct|EC_POINT_set_Jprojective_coordinates_GFp|EC_POINT_set_affine_coordinates_GF2m|EC_POINT_set_affine_coordinates_GFp|EC_POINT_set_compressed_coordinates_GF2m|EC_POINT_set_compressed_coordinates_GFp|EC_POINT_set_to_infinity|EC_POINTs_make_affine|EC_POINTs_mul|ENGINE_add|ENGINE_ctrl_cmd|ENGINE_ctrl_cmd_string|ENGINE_finish|ENGINE_free|ENGINE_init|ENGINE_register_DH|ENGINE_register_DSA|ENGINE_register_EC|ENGINE_register_RAND|ENGINE_register_RSA|ENGINE_register_all_complete|ENGINE_register_ciphers|ENGINE_register_complete|ENGINE_register_digests|ENGINE_register_pkey_asn1_meths|ENGINE_register_pkey_meths|ENGINE_remove|ENGINE_set_RSA|ENGINE_set_default|ENGINE_set_default_DH|ENGINE_set_default_DSA|ENGINE_set_default_EC|ENGINE_set_default_RAND|ENGINE_set_default_RSA|ENGINE_set_digests|ENGINE_set_id|ENGINE_set_init_function|ENGINE_set_load_privkey_function|ENGINE_set_load_pubkey_function|ENGINE_set_name|ENGINE_up_ref|HMAC_CTX_copy|HMAC_CTX_reset|HMAC_Final|HMAC_Init_ex|HMAC_Update|MD2_Init|MD2_Update|MD2_Final|MD4_Init|MD4_Update|MD4_Final|MD5_Init|MD5_Update|MD5_Final|OPENSSL_init_crypto|OPENSSL_mem_debug_pop|OPENSSL_mem_debug_push|RSA_generate_key_ex|RSA_generate_multi_prime_key|RSA_meth_set_finish|RSA_meth_set_sign|RSA_meth_set_verify|RSA_padding_add_SSLv23|RSA_set0_crt_params|RSA_set0_factors|RSA_set0_key|RSA_set0_multi_prime_params)$";
position pnot != openssl_check_not.p;

identifier FUNC1 =~ "^(EVP_CIPHER_CTX_copy|EVP_CIPHER_CTX_ctrl|EVP_CIPHER_CTX_rand_key|EVP_CIPHER_CTX_reset|EVP_CIPHER_CTX_set_key_length|EVP_CIPHER_CTX_set_padding|EVP_CipherFinal_ex|EVP_CipherInit_ex|EVP_CipherUpdate|EVP_DecryptFinal_ex|EVP_DecryptInit_ex|EVP_DecryptUpdate|EVP_Digest|EVP_DigestFinal|EVP_DigestFinal_ex|EVP_DigestInit|EVP_DigestInit_ex|EVP_DigestSign|EVP_DigestSignInit|EVP_DigestSignUpdate|EVP_DigestSignaFinal|EVP_DigestUpdate|EVP_DigestVerify|EVP_DigestVerifyInit|EVP_EncryptFinal_ex|EVP_EncryptInit_ex|EVP_EncryptUpdate|EVP_MAC_CTX_copy|EVP_MAC_ctrl|EVP_MAC_ctrl_str|EVP_MAC_hex2ctrl|EVP_MAC_init|EVP_MAC_reset|EVP_MAC_str2ctrl|EVP_MAC_update|EVP_MD_CTX_copy|EVP_MD_CTX_copy_ex|EVP_MD_CTX_ctrl|EVP_MD_meth_set_app_datasize|EVP_MD_meth_set_cleanup|EVP_MD_meth_set_copy|EVP_MD_meth_set_ctrl|EVP_MD_meth_set_final|EVP_MD_meth_set_flags|EVP_MD_meth_set_init|EVP_MD_meth_set_input_blocksize|EVP_MD_meth_set_result_size|EVP_MD_meth_set_update|EVP_PKEY_CTX_set_rsa_mgf1_md|EVP_PKEY_CTX_set_rsa_padding|EVP_PKEY_CTX_set_rsa_pss_saltlen|EVP_PKEY_CTX_set_signature|EVP_PKEY_assign|EVP_PKEY_assign_DSA|EVP_PKEY_assign_EC_KEY|EVP_PKEY_assign_RSA|EVP_PKEY_decrypt|EVP_PKEY_decrypt_init|EVP_PKEY_derive|EVP_PKEY_derive_init|EVP_PKEY_derive_set_peer|EVP_PKEY_encrypt|EVP_PKEY_encrypt_init|EVP_PKEY_get1_DH|EVP_PKEY_get_raw_private_key|EVP_PKEY_get_raw_public_key|EVP_PKEY_keygen|EVP_PKEY_keygen_init|EVP_PKEY_set1_DH|EVP_PKEY_sign|EVP_PKEY_sign_init|EVP_PKEY_verify|EVP_PKEY_verify_init|EVP_PKEY_verify_recover|EVP_PKEY_verify_recover_init|EVP_add_mac|RAND_bytes|RAND_priv_bytes)$";
position p1 != openssl_check_1.p;

identifier FUNCVOID =~ "^(AES_cfb128_encrypt|AES_cfb8_encrypt|AES_ige_encrypt|BN_GENCB_set|DSA_get0_key|DSA_get0_pqg|EC_GROUP_set_asn1_flag|EC_GROUP_set_point_conversion_form|ENGINE_get_static_state|ENGINE_unregister_DH|ENGINE_unregister_DSA|ENGINE_unregister_EC|ENGINE_unregister_RAND|ENGINE_unregister_RSA|ENGINE_unregister_ciphers|ENGINE_unregister_digests|ENGINE_unregister_pkey_asn1_meths|ENGINE_unregister_pkey_meths|OpenSSL_add_all_ciphers|OpenSSL_add_all_digests|RAND_seed|RC4|RC4_set_key|RSA_get0_crt_params|RSA_get0_factors|RSA_get0_key)$";
position pvoid != openssl_void.p;

identifier FUNCFREE =~ "^(BN_CTX_free|BN_GENCB_free|BN_clear_free|BN_free|CMAC_CTX_free|CRYPTO_free|DH_free|DSA_free|EC_GROUP_free|EC_KEY_free|EC_POINT_free|EVP_CIPHER_CTX_free|EVP_MD_CTX_free|EVP_PKEY_CTX_free|EVP_PKEY_free|HMAC_CTX_free|RSA_free|RSA_meth_free)$";
position pfree != openssl_free.p;
@@

(
* FUNCNEG(...)@pneg
|
* FUNCPOS(...)@ppos
|
* FUNCNULL(...)@pnull
|
* FUNC0(...)@p0
|
* FUNC1(...)@p1
|
* FUNCNOT(...)@pnot
|
* FUNCVOID(...)@pvoid
|
* FUNCFREE(...)@pfree
)
