# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

# In order to update the figures you have to have both dia
# and imagemagick installed.

DIAGRAMS=$(wildcard *.dia)
EPS_DIAGRAMS=$(patsubst %.dia,%.eps,$(DIAGRAMS))
PNG_DIAGRAMS=$(patsubst %.dia,%.png,$(DIAGRAMS))

diagrams: $(EPS_DIAGRAMS)

png: $(PNG_DIAGRAMS)

update_png: png
	git add $(PNG_DIAGRAMS)
	git commit -m "Update internal docs figures"

%.eps: %.dia
	dia --export=$@ $<

%.png: %.eps
	convert $< -resize 65% $@
