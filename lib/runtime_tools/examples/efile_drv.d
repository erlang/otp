/* example usage: dtrace -q -s /path/to/efile_drv.d */
/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2016. All Rights Reserved.
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

BEGIN
{
    op_map[1] = "OPEN";
    op_map[2] = "READ";
    op_map[3] = "LSEEK";
    op_map[4] = "WRITE";
    op_map[5] = "FSTAT";
    op_map[6] = "PWD";
    op_map[7] = "READDIR";
    op_map[8] = "CHDIR";
    op_map[9] = "FSYNC";
    op_map[10] = "MKDIR";
    op_map[11] = "DELETE";
    op_map[12] = "RENAME";
    op_map[13] = "RMDIR";
    op_map[14] = "TRUNCATE";
    op_map[15] = "READ_FILE";
    op_map[16] = "WRITE_INFO";
    op_map[19] = "LSTAT";
    op_map[20] = "READLINK";
    op_map[21] = "LINK";
    op_map[22] = "SYMLINK";
    op_map[23] = "CLOSE";
    op_map[24] = "PWRITEV";
    op_map[25] = "PREADV";
    op_map[26] = "SETOPT";
    op_map[27] = "IPREAD";
    op_map[28] = "ALTNAME";
    op_map[29] = "READ_LINE";
    op_map[30] = "FDATASYNC";
    op_map[31] = "FADVISE";
}

erlang*:::aio_pool-add
{
    printf("async I/O pool port %s queue len %d\n", copyinstr(arg0), arg1);
}

erlang*:::aio_pool-get
{
    printf("async I/O pool port %s queue len %d\n", copyinstr(arg0), arg1);
}

erlang*:::efile_drv-entry
{
    printf("efile_drv enter tag={%d,%d} %s%s | %s (%d) | args: %s %s , %d %d (port %s)\n",
	   arg0, arg1,
           arg2 == NULL ? "" : "user tag ",
           arg2 == NULL ? "" : copyinstr(arg2),
           op_map[arg3], arg3,
	   arg4 == NULL ? "" : copyinstr(arg4),
	   arg5 == NULL ? "" : copyinstr(arg5), arg6, arg7,
           /* NOTE: port name in args[10] is experimental */
           (args[10] == NULL) ? 
	       "?" : copyinstr((user_addr_t) args[10]));
}

erlang*:::efile_drv-int*
{
    printf("async I/O worker tag={%d,%d} | %s (%d) | %s\n",
           arg0, arg1, op_map[arg2], arg2, probename);
}

/* efile_drv-return error case */
erlang*:::efile_drv-return
/arg4 == 0/
{
    printf("efile_drv return tag={%d,%d} %s%s | %s (%d) | errno %d\n",
           arg0, arg1,
           arg2 == NULL ? "" : "user tag ",
           arg2 == NULL ? "" : copyinstr(arg2),
           op_map[arg3], arg3,
           arg5);
}

/* efile_drv-return success case */
erlang*:::efile_drv-return
/arg4 != 0/
{
    printf("efile_drv return tag={%d,%d} %s | %s (%d) ok\n",
           arg0, arg1,
           arg2 == NULL ? "" : copyinstr(arg2),
           op_map[arg3], arg3);
}
