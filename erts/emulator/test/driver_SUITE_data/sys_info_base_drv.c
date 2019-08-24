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
 * Description: Driver that fakes driver version 3.0 and tests
 *              driver_system_info().
 *
 */

#include "sys_info_drv_impl.h"

#define SYS_INFO_DRV_MAJOR_VSN		3
#define SYS_INFO_DRV_MINOR_VSN		0
#define SYS_INFO_DRV_NAME_STR		"sys_info_base_drv"
#define SYS_INFO_DRV_NAME		sys_info_base_drv
#define SYS_INFO_DRV_LAST_FIELD		smp_support
#define ERL_DRV_SYS_INFO_SIZE		sizeof(ErlDrvSysInfo)

#define SYS_INFO_DRV_RES_FORMAT "ok: "	\
  "drv_drv_vsn=%d.%d "			\
  "emu_drv_vsn=%d.%d "			\
  "erts_vsn=%s "			\
  "otp_vsn=%s "				\
  "thread=%s "				\
  "smp=%s "				\
  "async_thrs=%d "			\
  "sched_thrs=%d "                      \
  "emu_nif_vsn=%d.%d"


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
    slen += 20;		/* async_thrs */
    slen += 20;		/* sched_thrs */
    slen += 2*20;       /* emu_nif_vsn */
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
		   sip->smp_support ? "true" : "false",
		   sip->async_threads,
		   sip->scheduler_threads,
		   sip->nif_major_version,
		   sip->nif_minor_version);
}

#include "sys_info_drv_impl.c"
