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
 * Description: Driver that fakes driver version 1.0 and tests
 *              driver_system_info().
 *
 */

#include "sys_info_drv_impl.h"

#define SYS_INFO_DRV_MAJOR_VSN		1
#define SYS_INFO_DRV_MINOR_VSN		0
#define SYS_INFO_DRV_NAME_STR		"sys_info_1_0_drv"
#define SYS_INFO_DRV_NAME		sys_info_1_0_drv
#define SYS_INFO_DRV_LAST_FIELD		smp_support

#define SYS_INFO_DRV_RES_FORMAT "ok: "	\
  "drv_drv_vsn=%d.%d "			\
  "emu_drv_vsn=%d.%d "			\
  "erts_vsn=%s "			\
  "otp_vsn=%s "				\
  "thread=%s "				\
  "smp=%s"


static size_t
sys_info_drv_max_res_len(ErlDrvSysInfo *sip)
{
    size_t slen = strlen(SYS_INFO_DRV_RES_FORMAT) + 1;
    slen += 2*20;	/* drv_drv_vsn */
    slen += 2*20;	/* emu_drv_vsn */
    slen += strlen(sip->erts_version) + 1;
    slen += strlen(sip->otp_release) + 1;
    slen += 5;		/* threads */
    slen += 5;		/* smp */
    return slen;
}

static size_t
sys_info_drv_sprintf_sys_info(ErlDrvSysInfo *sip, char *str)
{
    return sprintf(str,
		   SYS_INFO_DRV_RES_FORMAT,
		   SYS_INFO_DRV_MAJOR_VSN,
		   SYS_INFO_DRV_MINOR_VSN,
		   sip->driver_major_version,
		   sip->driver_minor_version,
		   sip->erts_version,
		   sip->otp_release,
		   sip->thread_support ? "true" : "false",
		   sip->smp_support ? "true" : "false");
}

#include "sys_info_drv_impl.c"
