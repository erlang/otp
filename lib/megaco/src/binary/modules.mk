#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 2001-2020. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 
# %CopyrightEnd%

MODULES = \
	megaco_binary_encoder \
	megaco_binary_encoder_lib \
	megaco_ber_encoder \
	megaco_ber_media_gateway_control_v1 \
	megaco_ber_media_gateway_control_v2 \
	megaco_ber_media_gateway_control_v3 \
	megaco_per_encoder \
	megaco_per_media_gateway_control_v1 \
	megaco_per_media_gateway_control_v2 \
	megaco_per_media_gateway_control_v3 \
	megaco_binary_name_resolver_v1 \
	megaco_binary_name_resolver_v2 \
	megaco_binary_name_resolver_v3 \
	megaco_binary_term_id \
	megaco_binary_term_id_gen \
	megaco_binary_transformer_v1 \
	megaco_binary_transformer_v2 \
	megaco_binary_transformer_v3

INTERNAL_HRL_FILES = 

ASN1_V1_SPEC      = MEDIA-GATEWAY-CONTROL-v1
ASN1_V2_SPEC      = MEDIA-GATEWAY-CONTROL-v2
ASN1_V3_SPEC      = MEDIA-GATEWAY-CONTROL-v3

BER_ASN1_V1_SPEC     = megaco_ber_media_gateway_control_v1
PER_ASN1_V1_SPEC     = megaco_per_media_gateway_control_v1

BER_ASN1_V2_SPEC     = megaco_ber_media_gateway_control_v2
PER_ASN1_V2_SPEC     = megaco_per_media_gateway_control_v2

BER_ASN1_V3_SPEC     = megaco_ber_media_gateway_control_v3
PER_ASN1_V3_SPEC     = megaco_per_media_gateway_control_v3

