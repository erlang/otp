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
 * Description: Implementation of a driver that fakes different driver
 *              versions and tests driver_system_info(). This file should
 *		be included by an implementation that defines:
 *              * SYS_INFO_DRV_MAJOR_VSN
 *		* SYS_INFO_DRV_MINOR_VSN
 *		* SYS_INFO_DRV_NAME_STR
 *		* SYS_INFO_DRV_NAME
 *		* ERL_DRV_SYS_INFO_SIZE, or SYS_INFO_DRV_LAST_FIELD
 *		and implements:
 *		* static size_t sys_info_drv_max_res_len(ErlDrvSysInfo *)
 *		* static size_t sys_info_drv_sprintf_sys_info(ErlDrvSysInfo *,
 *							      char *)
 *
 */

#if !defined(ERL_DRV_SYS_INFO_SIZE) && defined(SYS_INFO_DRV_LAST_FIELD)

#define ERL_DRV_SYS_INFO_SIZE_FROM_LAST_FIELD(LAST_FIELD) \
  (((size_t) &((ErlDrvSysInfo *) 0)->LAST_FIELD) \
   + sizeof(((ErlDrvSysInfo *) 0)->LAST_FIELD))

#define ERL_DRV_SYS_INFO_SIZE \
  ERL_DRV_SYS_INFO_SIZE_FROM_LAST_FIELD(SYS_INFO_DRV_LAST_FIELD)

#endif

static ErlDrvData start(ErlDrvPort, char *);
static ErlDrvSSizeT control(ErlDrvData, unsigned int,
			    char *, ErlDrvSizeT, char **, ErlDrvSizeT);

static ErlDrvEntry drv_entry = { 
    NULL /* init */,
    start,
    NULL /* stop */,
    NULL /* output */,
    NULL /* ready_input */,
    NULL /* ready_output */,
    SYS_INFO_DRV_NAME_STR,
    NULL /* finish */,
    NULL /* handle */,
    control,
    NULL /* timeout */,
    NULL /* outputv */,
    NULL /* ready_async */,
    NULL /* flush */,
    NULL /* call */,
    NULL /* event */,
    ERL_DRV_EXTENDED_MARKER,
    SYS_INFO_DRV_MAJOR_VSN,
    SYS_INFO_DRV_MINOR_VSN,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL /* handle2 */,
    NULL /* process_exit */
};

DRIVER_INIT(SYS_INFO_DRV_NAME)
{
    return &drv_entry;
}

static ErlDrvData
start(ErlDrvPort port, char *command)
{
    return (ErlDrvData) port;
}

static ErlDrvSSizeT
control(ErlDrvData drv_data,
	unsigned int command,
	char *buf, ErlDrvSizeT len,
	char **rbuf, ErlDrvSizeT rlen)
{
    ErlDrvSSizeT res;
    char *str;
    size_t slen, slen2;
    ErlDrvPort port = (ErlDrvPort) drv_data;
    unsigned deadbeef[] = {0xdeadbeef,
			   0xdeadbeef,
			   0xdeadbeef,
			   0xdeadbeef,
			   0xdeadbeef,
			   0xdeadbeef,
			   0xdeadbeef,
			   0xdeadbeef,
			   0xdeadbeef,
			   0xdeadbeef};
    ErlDrvSysInfo *sip = driver_alloc(ERL_DRV_SYS_INFO_SIZE + sizeof(deadbeef));
    char *beyond_end_format = "error: driver_system_info() wrote beyond end "
	"of the ErlDrvSysInfo struct";
    char *buf_overflow_format = "error: Internal buffer overflow";

    if (!sip) {
	driver_failure_atom(port, "enomem");
	return 0;
    }

    memset((char *) sip, 0xed, ERL_DRV_SYS_INFO_SIZE);
    memcpy(((char *) sip) + ERL_DRV_SYS_INFO_SIZE,
	   (char *) &deadbeef[0],
	   sizeof(deadbeef));

    driver_system_info(sip, ERL_DRV_SYS_INFO_SIZE);

    slen = sys_info_drv_max_res_len(sip);
    slen2 = strlen(beyond_end_format) + 1;
    if (slen2 > slen)
	slen = slen2;
    slen2 = strlen(buf_overflow_format) + 1;
    if (slen2 > slen)
	slen = slen2;
    str = driver_alloc(slen);
    if (!str) {
	driver_free(sip);
	driver_failure_atom(port, "enomem");
	return 0;
    }
    *rbuf = str;

    /* Check that the emulator didn't write beyond ERL_DRV_SYS_INFO_SIZE */
    if (memcmp(((char *) sip) + ERL_DRV_SYS_INFO_SIZE,
	       (char *) &deadbeef[0],
	       sizeof(deadbeef)) != 0) {
	res = sprintf(str, beyond_end_format);
    }
    else {
	res = sys_info_drv_sprintf_sys_info(sip, str);
	if (res > slen)
	    res = sprintf(str, buf_overflow_format);
    }
    driver_free(sip);
    return res;
}


