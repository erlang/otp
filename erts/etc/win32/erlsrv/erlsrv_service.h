/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
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
 * %CopyrightEnd%
 */
#ifndef _ERLSRV_SERVICE_H
#define _ERLSRV_SERVICE_H

#define CYCLIC_RESTART_LIMIT 10 /* Seconds */
#define SUCCESS_WAIT_TIME (10*1000) /* Wait 5 s before reporting a service
				as really started */
#define NO_SUCCESS_WAIT 0
#define INITIAL_SUCCESS_WAIT 1
#define RESTART_SUCCESS_WAIT 2


int service_main(int argc, wchar_t **argv);

#endif /* _ERLSRV_SERVICE_H */
