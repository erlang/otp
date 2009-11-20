/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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
 * Description: Implementation of a driver that fakes driver version. It
 *              is used for checking that version mismatches are handled
 *              correct by the emulator. The following makros have to be
 *		defined before it can be used:
 *		* VSN_MISMATCH_DRV_NAME_STR
 *		* VSN_MISMATCH_DRV_NAME
 *		* VSN_MISMATCH_DRV_MAJOR_VSN_DIFF
 *		* VSN_MISMATCH_DRV_MINOR_VSN_DIFF
 */

#include "erl_driver.h"

static ErlDrvEntry drv_entry = { 
    NULL /* init */,
    NULL /* start */,
    NULL /* stop */,
    NULL /* output */,
    NULL /* ready_input */,
    NULL /* ready_output */,
    VSN_MISMATCH_DRV_NAME_STR,
    NULL /* finish */,
    NULL /* handle */,
    NULL /* control */,
    NULL /* timeout */,
    NULL /* outputv */,
    NULL /* ready_async */,
    NULL /* flush */,
    NULL /* call */,
    NULL /* event */,
#ifdef VSN_MISMATCH_DRV_EXTENDED_MARKER
    VSN_MISMATCH_DRV_EXTENDED_MARKER,
#else
    ERL_DRV_EXTENDED_MARKER,
#endif
    ERL_DRV_EXTENDED_MAJOR_VERSION + VSN_MISMATCH_DRV_MAJOR_VSN_DIFF,
    ERL_DRV_EXTENDED_MINOR_VERSION + VSN_MISMATCH_DRV_MINOR_VSN_DIFF,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL /* handle2 */,
    NULL /* process_exit */
};

DRIVER_INIT(VSN_MISMATCH_DRV_NAME)
{
    return &drv_entry;
}

