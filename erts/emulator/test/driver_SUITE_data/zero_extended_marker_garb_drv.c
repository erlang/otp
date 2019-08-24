/* ``Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

/*
 * Author: Rickard Green
 *
 * Description: Implementation of a driver with an invalid extended
 *		marker.
 */

#define VSN_MISMATCH_DRV_EXTENDED_MARKER	0
#define VSN_MISMATCH_DRV_NAME_STR		"zero_extended_marker_garb_drv"
#define VSN_MISMATCH_DRV_NAME			zero_extended_marker_garb_drv
#define VSN_MISMATCH_DRV_MAJOR_VSN_DIFF		0
#define VSN_MISMATCH_DRV_MINOR_VSN_DIFF		0
 
#include "vsn_mismatch_drv_impl.c"
