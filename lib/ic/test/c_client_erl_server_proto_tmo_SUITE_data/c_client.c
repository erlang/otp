/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
 *
 */
/* C-client for test of IC.  
 * 
 * TODO:
 *
 * 1. XXX #includes for VxWorks, Windows
 */

#include <stdio.h>
#include <stdlib.h>

#ifndef __WIN32__
#  include <unistd.h>
#endif

#include <string.h>

#ifdef __WIN32__
#  include <time.h>
#  include <sys/timeb.h>
#elif defined VXWORKS
#include <time.h>
#include <sys/times.h>
#else
#include <sys/time.h>
#endif

#include <ctype.h>

#ifdef __WIN32__
#  include <winsock2.h>
#  include <windows.h>
#else
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <netdb.h>
#endif

#include "ei.h"
#include "erl_interface.h"
#include "m_i.h"

#define HOSTNAMESZ 256
#define NODENAMESZ 512

#define INBUFSZ 10
#define OUTBUFSZ 0

#define MAXTRIES 5

#define CHECK_EXCEPTION(x) \
    if ((x)->_major != CORBA_NO_EXCEPTION) { \
	fprintf(stderr,"\n\nException: %s\n\n", \
		(char *)CORBA_exception_value((x))); \
        CORBA_exception_free((x)); \
	return -1; \
    } \

/* XXX Should free things here too! */
#define RETURN_IF_OK(x) \
    if ((x)) {\
	fprintf(stdout, "ok\n");\
	return 0;\
    }\

#define cmp_str(x,y) (!strcmp((x),(y)))
#define cmp_wstr(x,y) (!ic_wstrcmp((x),(y)))

typedef CORBA_Environment IC_Env;

typedef int (*TestFunc)(IC_Env *);
typedef struct {
    char *name; 
    TestFunc func; 
} TestCase;

static char longtext[] = 
"Introduction The IC application is an IDL compiler implemented in Erlang."
" The IDL compiler generates client stubs and server skeletons."
" Several back-ends are supported, and they fall into three main groups."
" For more details on IC compiler options consult the ic(3) manual page."
" Argument passing cases 1 Caller allocates all necessary storage,"
" except that which may be encapsulated and managed within the parameter itself."
" 2 The caller allocates a pointer and passes it by reference to the callee."
" The callee sets the pointer to point to a valid instance of the parameter's type."
" The caller is responsible for releasing the returned storage."
" Following completion of a request, the caller is not allowed to modify any values"
" in the returned storage. To do so the caller must first copy the returned instance"
" into a new instance, then modify the new instance. 3 The caller allocates a"
" pointer to an array slice which has all the same dimensions of the original"
" array except the first, and passes it by reference to the callee. The callee sets"
" the pointer to point to a valid instance of the array. The caller is responsible for"
" releasing the returned storage. Following completion of a request, the caller is not"
" allowed to modify any values in the returned storage. To do so the caller must first"
" copy the returned instance into a new instance, then modify the new instance."
" Generated Files Two files will be generated for each scope. One set of files will be"
" generated for each module and each interface scope. An extra set is generated for"
" those definitions at top level scope. One of the files is a header file(.h), and the"
" other file is a C source code file (.c). In addition to these files a number of C"
" source files will be generated for type encodings, they are named according to the "
"following template: oe_code_<type>.c.";
static char this_node[NODENAMESZ + 1];
static char *progname;

/* Test function prototypes */

static int void_test(IC_Env *env);
static int long_test(IC_Env *env);
static int long_long_test(IC_Env *env);
static int unsigned_short_test(IC_Env *env);
static int unsigned_long_test(IC_Env *env);
static int unsigned_long_long_test(IC_Env *env);
static int double_test(IC_Env *env);
static int char_test(IC_Env *env);
static int wchar_test(IC_Env *env);
static int octet_test(IC_Env *env);
static int bool_test(IC_Env *env);
static int struct_test(IC_Env *env);
static int struct2_test(IC_Env *env);
static int seq1_test(IC_Env *env);
static int seq2_test(IC_Env *env);
static int seq3_test(IC_Env *env);
static int seq4_test(IC_Env *env);
static int seq5_test(IC_Env *env);
static int array1_test(IC_Env *env);
static int array2_test(IC_Env *env);
static int enum_test(IC_Env *env);
static int string1_test(IC_Env *env);
static int string2_test(IC_Env *env);
static int string3_test(IC_Env *env);
static int string4_test(IC_Env *env);
static int pid_test(IC_Env *env);
static int port_test(IC_Env *env);
static int ref_test(IC_Env *env);
static int term_test(IC_Env *env);
static int typedef_test(IC_Env *env);
static int inline_sequence_test(IC_Env *env);
static int term_sequence_test(IC_Env *env);
static int term_struct_test(IC_Env *env);
static int wstring1_test(IC_Env *env);

static TestCase test_cases[] = {
    {"void_test", void_test},
    {"long_test", long_test},
    {"long_long_test", long_long_test},
    {"unsigned_short_test", unsigned_short_test},
    {"unsigned_long_test", unsigned_long_test},
    {"unsigned_long_long_test", unsigned_long_long_test},
    {"double_test", double_test},
    {"char_test", char_test},
    {"wchar_test", wchar_test},
    {"octet_test", octet_test},
    {"bool_test", bool_test},
    {"struct_test", struct_test},
    {"struct2_test", struct2_test},
    {"seq1_test", seq1_test},
    {"seq2_test", seq2_test},
    {"seq3_test", seq3_test},
    {"seq4_test", seq4_test},
    {"seq5_test", seq5_test},
    {"array1_test", array1_test},
    {"array2_test", array2_test},
    {"enum_test", enum_test},
    {"string1_test", string1_test},
    {"string2_test", string2_test},
    {"string3_test", string3_test},
    {"string4_test", string4_test},
    {"pid_test", pid_test},
    {"port_test", port_test},
    {"ref_test", ref_test},
    {"term_test", term_test},
    {"typedef_test", typedef_test},
    {"inline_sequence_test", inline_sequence_test},
    {"term_sequence_test", term_sequence_test},
    {"term_struct_test", term_struct_test},
    {"wstring1_test", wstring1_test},
    {"", NULL}
};

/* Other prototypes */
static int cmp_aseq(m_aseq *a1, m_aseq *a2);
static int cmp_a(m_a *a1, m_a *a2);
static int cmp_bseq(m_bseq *b1, m_bseq *b2);
static int cmp_b(m_b *b1, m_b *b2);
static int cmp_lseq(m_lseq *b1, m_lseq *b2);
static int cmp_etseq(m_etseq *b1, m_etseq *b2);
static int cmp_et(m_et* b1, m_et *b2);
static int cmp_es(m_es *b1, m_es *b2);
static int cmp_arr1(m_arr1 b1, m_arr1 b2);
static int cmp_dd(m_dd b1, m_dd b2);
static int cmp_strRec(m_strRec *b1, m_strRec *b2);
static int cmp_sseq(m_sseq *b1, m_sseq *b2);
static int cmp_pid(erlang_pid *p1, erlang_pid *p2);
static int cmp_port(erlang_port *p1, erlang_port *p2);
static int cmp_ref(erlang_ref *p1, erlang_ref *p2);
static int cmp_s(m_s *b1, m_s *b2);
static int cmp_ssstr3(m_ssstr3 *b1, m_ssstr3 *b2);
static int cmp_ssarr3(m_ssarr3 *b1, m_ssarr3 *b2);
static int cmp_sarr3(m_sarr3 *b1, m_sarr3 *b2);
static int cmp_arr3(m_arr3 b1, m_arr3 b2);

static void print_aseq(m_aseq *a);
static void print_a(m_a *a);
static void print_bseq(m_bseq *b);
static void print_lseq(m_lseq *b);
static void print_b(m_b *b);
static void print_etseq(m_etseq *b);
static void print_et(m_et* b);
static void print_es(m_es *b);
static void print_arr1(long a[500]);
static void print_dd(long a[2][3]);
static void print_strRec(m_strRec* sr);
static void print_sseq(m_sseq *b);
static void print_pid(erlang_pid *p);
static void print_port(erlang_port *p);
static void print_ref(erlang_ref *p);
static void print_term(ETERM *t);
static void print_s(m_s *p);
static void print_ssstr3(m_ssstr3 *b1);
static void print_ssarr3(m_ssarr3 *b1);
static void print_sarr3(m_sarr3 *b1);
static void print_arr3(m_arr3 b1);
static void print_wstr(CORBA_wchar *ws);

static void free_etseq_buf(m_etseq *b);
static void free_et(m_et* b);

#ifdef __WIN32__
typedef struct {
    long tv_sec;
    long tv_usec;
} MyTimeval;
#else
typedef struct timeval MyTimeval;
#endif
static void my_gettimeofday(MyTimeval *tv);
static void showtime(MyTimeval *start, MyTimeval *stop);
static void usage(void);
static void done(int r);



/* main */

#ifdef VXWORKS
int client(int argc, char **argv)
#else
int main(int argc, char **argv)
#endif
{
    struct hostent *hp;
    erlang_pid pid;
    MyTimeval start, stop;
    int i, fd, ires, tres;
    IC_Env *env;
    int tries = 0;
    char *this_node_name = NULL;
    char *peer_node = NULL;
    char *peer_process_name = NULL;
    char *cookie = NULL;
    char host[HOSTNAMESZ + 1];
    TestFunc test_func = NULL;
    TestCase *test_case;
    char *test_case_name = NULL;
    
#ifdef __WIN32__
    WORD wVersionRequested;
    WSADATA wsaData;
    
    wVersionRequested = MAKEWORD(2, 0);
    
    if (WSAStartup(wVersionRequested, &wsaData) != 0) {
	fprintf(stderr, "Could not load winsock2 v2.0 compatible DLL");
	exit(1);
    }
#endif
    
    progname = argv[0];
    host[HOSTNAMESZ] = '\0';
    if (gethostname(host, HOSTNAMESZ) < 0) {
	fprintf(stderr, "Can't find own hostname\n");
	done(1);
    } 
    if ((hp = gethostbyname(host)) == 0) {
	fprintf(stderr, "Can't get ip address for host %s\n", host);
	done(1);
    }
    for (i = 1; i < argc; i++) {
	if (cmp_str(argv[i], "-help")) {
	    usage();
	    done(0);
	} else if (cmp_str(argv[i], "-this-node-name")) {
	    i++;
	    this_node_name = argv[i];
	} else if (cmp_str(argv[i], "-peer-node")) {
	    i++;
	    peer_node = argv[i];
	} else if (cmp_str(argv[i], "-peer-process-name")) {
	    i++;
	    peer_process_name = argv[i];
	} else if (cmp_str(argv[i], "-cookie")) {
	    i++;
	    cookie = argv[i];
	} else if (cmp_str(argv[i], "-test-case")) {
	    i++;
	    test_case_name = argv[i];
	} else {
	    fprintf(stderr, "Error : invalid argument \"%s\"\n", argv[i]);
	    usage();
	    done(1);
	}
    }

    if (this_node_name == NULL || peer_node == NULL || test_case_name == NULL 
	|| peer_process_name == NULL || cookie == NULL) {
	fprintf(stderr, "Error: missing option\n");
	usage();
	done(1);
    }

    test_case = test_cases;
    while (test_case->func) {
	if (cmp_str(test_case->name, test_case_name)) {
	    test_func = test_case->func;
	    break;
	}
	test_case++;
    }
    if (test_func == NULL) {
	fprintf(stderr, "Error: illegal test case: \"%s\"\n", test_case_name);
	done(1);
    }
    
    /* Behead hostname at first dot */
    for (i=0; host[i] != '\0'; i++) {
	if (host[i] == '.') { host[i] = '\0'; break; }
    }
    sprintf(this_node, "%s@%s", this_node_name, host);
    fprintf(stderr, "c_client: this node: \"%s\"\n", this_node);
    fprintf(stderr, "c_client: peer node: \"%s\"\n", peer_node);
    fprintf(stderr, "c_client: test case: \"%s\"\n", test_case_name);

    fprintf(stderr, "c_client: starting\n");

    /* initialize erl_interface */
    erl_init(NULL, 0);

    for (tries = 0; tries < MAXTRIES; tries++) {

	/* connect to erlang node */ 

    	ires = erl_connect_xinit(host, this_node_name, this_node, 
				 (struct in_addr *)*hp->h_addr_list, 
				 cookie, 0);

	fprintf(stderr, "c_client: erl_connect_xinit(): %d\n", ires);
    
	fd = erl_connect(peer_node);
	fprintf(stderr, "c_client: erl_connect(): %d\n", fd);
    
	if (fd >= 0) 
	    break;
	fprintf(stderr, "c_client: cannot connect, retrying\n");
    }
    if (fd < 0) {
	fprintf(stderr, "c_client: cannot connect, exiting\n");
	done(1);
    }
    env = CORBA_Environment_alloc(INBUFSZ, OUTBUFSZ);
    env->_fd = fd; 
    strcpy(env->_regname, peer_process_name);
    env->_to_pid = NULL;
    env->_from_pid = &pid;
    
    strcpy(pid.node, this_node);
    pid.num = fd;
    pid.serial = 0;
    pid.creation = 0;

    my_gettimeofday(&start);
    tres = test_func(env);	/* Call test case */
    my_gettimeofday(&stop);
    showtime(&start, &stop);
    erl_close_connection(fd);

    printf("c_client: env->_inbuf before : %d\n", INBUFSZ);
    printf("c_client: env->_outbuf before : %d\n", OUTBUFSZ);
    printf("c_client: env->_inbuf after : %d\n", env->_inbufsz);
    printf("c_client: env->_outbuf after : %d\n", env->_outbufsz);

    CORBA_free(env->_inbuf);
    CORBA_free(env->_outbuf);
    CORBA_free(env);
    done(tres);
}

static void usage() 
{
    fprintf(stderr, "Usage: %s [-help] -this-node-name <name> "
	    "-peer-node <nodename> -peer-process-name <name> "
	    "-cookie <cookie> -test-case <test case name>\n", progname);
    fprintf(stderr, "Example:\n  %s  -this-node-name kalle "
	    "-peer-node olle@home -peer-process-name idltest "
	    "-cookie oa678er -test-case octet_test\n", progname);
}

static void done(int r) 
{
#ifdef __WIN32__
    WSACleanup();
#endif
    exit(r);
}


/* TESTS */

static int void_test(IC_Env *env)
{
    fprintf(stdout, "\n======== m_i_void test ======\n\n");
    m_i_void_test(NULL,env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(1);
}

static int long_test(IC_Env *env)
{
    long l = 4711, lo, lr;

    fprintf(stdout, "\n======== m_i_long test ======\n\n");
    lr = m_i_long_test(NULL, l, &lo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(l == lo && l == lr);
    if (l != lo)
	fprintf(stdout, " out parameter error, sent: %ld, got: %ld\n", l, lo);
    if (l != lr)
	fprintf(stdout, " result error, sent: %ld, got: %ld\n", l, lr);
    return -1;
}

static int long_long_test(IC_Env *env)
{
    CORBA_long_long ll = 4711, llo, llr;

    fprintf(stdout, "\n======== m_i_longlong test ======\n\n");
    llr = m_i_longlong_test(NULL, ll, &llo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(ll == llo && ll == llr);
    if (ll != llo)
	fprintf(stdout, " out parameter error, sent: %ld, got: %ld\n", 
		ll, llo);
    if (ll != llr)
	fprintf(stdout, " result error, sent: %ld, got: %ld\n", ll, llr);
    return -1;
}

static int unsigned_short_test(IC_Env *env)
{
    unsigned short x, y = 2, z;

    fprintf(stdout, "\n======== m_i_ushort test ======\n\n");
    x = m_i_ushort_test(NULL, y, &z, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(y == z && y == x);
    if (y != z) 
	fprintf(stdout, " out parameter error, sent: %d, got: %d\n", y, z);
    if (y != x)
	fprintf(stdout, " result error, sent: %d, got: %d\n", y, x);
    return -1;
}


static int unsigned_long_test(IC_Env *env)
{
    unsigned long ul = 5050, ulo, ulr;

    fprintf(stdout, "\n======== m_i_ulong test ======\n\n");
    ulr = m_i_ulong_test(NULL, ul, &ulo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(ul == ulo && ul == ulr);
    if (ul != ulo) 
	fprintf(stdout, " out parameter error, sent: %lu, got: %lu\n", 
		ul, ulo);
    if (ul != ulr)
	fprintf(stdout, " result error, sent: %lu, got: %lu\n", ul, ulr);
    return -1;
}

/* 
 * Note: CORBA_unsigned_long_long is in fact a plain long.
 */
static int unsigned_long_long_test(IC_Env *env)
{
    CORBA_unsigned_long_long ull = 5050, ullo, ullr;

    fprintf(stdout, "\n======== m_i_ulonglong test ======\n\n");
    ullr = m_i_ulonglong_test(NULL, ull, &ullo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(ull == ullo && ull == ullr);
    if (ull != ullo)
	fprintf(stdout, " out parameter error, sent: %lu, got: %lu\n", 
		ull, ullo); 
    if (ull != ullr)
	fprintf(stdout, " result error, sent: %lu, got: %lu\n", 
		ull, ullr);
    return -1;
}

static int double_test(IC_Env *env)
{
    double d = 12.1212, db, dr;

    fprintf(stdout, "\n======== m_i_double test ======\n\n");
    dr = m_i_double_test(NULL, d, &db, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(d == db && d == dr);
    if (d != db)
	fprintf(stdout, " out parameter error, sent: %f, got: %f\n", d, db);
    if (d != dr)
	fprintf(stdout, " result error, sent: %f, got: %f\n", d, dr);
    return -1;
}    

static int char_test(IC_Env *env)
{
    char c = 'g', co, cr;

    /* char test */
    fprintf(stdout, "\n======== m_i_char test ======\n\n");
    cr = m_i_char_test(NULL, c, &co, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(c == co && c == cr);
    if (c !=co) 
	fprintf(stdout, " out parameter error, sent: %c, got: %c\n", c, co);
    if (c != cr)
	fprintf(stdout, " result error, sent: %c, got: %c\n", c, cr);
    return -1;
}

static int wchar_test(IC_Env *env)
{
    CORBA_wchar wc = 103, wco, wcr;

    fprintf(stdout, "\n======== m_i_wchar test ======\n\n");
    wcr = m_i_wchar_test(NULL, wc, &wco, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(wc == wco && wc == wcr);
    if (wc != wco)
	fprintf(stdout, " out parameter error, sent: %lu, got: %lu\n", 
		wc, wco);
    if (wc != wcr)
	fprintf(stdout, " result error, sent: %lu, got: %lu\n", 
		wc, wcr);
    return -1;
}    

static int octet_test(IC_Env *env)
{
    char o ='r', oo, or;
 
    fprintf(stdout, "\n======== m_i_octet test ======\n\n");
    or = m_i_octet_test(NULL, o, &oo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(o == oo && o == or);
    if (o != oo)
	fprintf(stdout, " out parameter error, sent: %c, got: %c\n", o, oo);
    if (o != or)
	fprintf(stdout, " result error, sent: %c, got: %c\n", o, or);
    return -1;
}    

static int bool_test(IC_Env *env)
{
    unsigned char i = 0, io, ir;	

    fprintf(stdout, "\n======== m_i_bool test ======\n\n");
    ir = m_i_bool_test(NULL, i, &io, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(i == io && i == ir);
    if (i != io)
	fprintf(stdout, " out parameter error, sent: %d, got: %d\n", i, io);
    if (i != ir)
	fprintf(stdout, " result error, sent: %d, got: %d\n", i, ir);
    return -1;
}

static int struct_test(IC_Env *env)
{
    m_b b = {4711, 'a'}, bo, br;

    fprintf(stdout, "\n======== m_i_struct test ======\n\n");
    br = m_i_struct_test(NULL, &b, &bo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_b(&b, &bo) && cmp_b(&b, &br));
    if (!cmp_b(&b, &bo)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_b(&b);
	fprintf(stdout, " got:\n");
	print_b(&bo);
	fprintf(stdout, "\n");
    }
    if (!cmp_b(&b, &br)) {
	fprintf(stdout, " result error, sent:\n");
	print_b(&b);
	fprintf(stdout, " got:\n");
	print_b(&br);
	fprintf(stdout, "\n");
    }
    return -1;
}

static int struct2_test(IC_Env *env)
{
    m_es esi = {m_peach, 5050}, eso, esr;

    fprintf(stdout, "\n======== m_i_struct2 test ======\n\n");
    esr = m_i_struct2_test(NULL, &esi, &eso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_es(&esi, &eso) && cmp_es(&esi, &esr));
    if (!cmp_es(&esi, &eso)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_es(&esi);
	fprintf(stdout, " got:\n");
	print_es(&eso);
	fprintf(stdout, "\n");
    }
    if (!cmp_es(&esi, &esr)) {
	fprintf(stdout, " result error, sent:\n");
	print_es(&esi);
	fprintf(stdout, " got:\n");
	print_es(&esr);
	fprintf(stdout, "\n");
    }
    return -1;
}


static int seq1_test(IC_Env *env)
{
    m_bseq bs, *bso, *bsr;

    m_b ba[3] = {{4711, 'a'}, {4712, 'b'}, {4713, 'c'}};
    bs._length = 3;
    bs._buffer = ba;

    fprintf(stdout, "\n======== m_i_seq1 test ======\n\n");
    bsr = m_i_seq1_test(NULL, &bs, &bso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_bseq(&bs, bso) && cmp_bseq(&bs, bsr));
    if (!cmp_bseq(&bs, bso)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_bseq(&bs);
	fprintf(stdout, " got:\n");
	print_bseq(bso);
	fprintf(stdout, "\n");
    }
    if (!cmp_bseq(&bs, bsr)) {
	fprintf(stdout, " result error, sent:\n");
	print_bseq(&bs);
	fprintf(stdout, " got:\n");
	print_bseq(bsr);
	fprintf(stdout, "\n");
    }
    CORBA_free(bso);
    CORBA_free(bsr);
    return -1;
}

static int seq2_test(IC_Env *env)
{
    m_b ba[3] = {{4711, 'a'}, {4712, 'b'}, {4713, 'c'}};
    m_a a;
    m_a aa[2];
    m_aseq as, *aso, *asr;

    a.l = 9999;
    a.y._length = 3;
    a.y._buffer = ba;
    a.d = 66.89898989;

    aa[0] = a;
    aa[1] = a;
    as._length = 2;
    as._buffer = aa;

    fprintf(stdout, "\n======== m_i_seq2 test ======\n\n");
    asr = m_i_seq2_test(NULL, &as, &aso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_aseq(&as, aso) && cmp_aseq(&as, asr));
    if (!cmp_aseq(&as, aso)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_aseq(&as);
	fprintf(stdout, " got:\n");
	print_aseq(aso);
	fprintf(stdout, "\n");
    }
    if (!cmp_aseq(&as, asr)) {
	fprintf(stdout, " result error, sent:\n");
	print_aseq(&as);
	fprintf(stdout, " got:\n");
	print_aseq(asr);
	fprintf(stdout, "\n");
    }
    CORBA_free(aso);
    CORBA_free(asr);
    return -1;
}

static int seq3_test(IC_Env *env)
{
    m_lseq lsi, *lso, *lsr;
    long al[500];
    int i=0;
    
    for (i = 0; i < 500; i++)
	al[i]=i;
    lsi._length = 500;
    lsi._buffer = al;

    fprintf(stdout, "\n======== m_i_seq3 test ======\n\n");
    lsr = m_i_seq3_test(NULL, &lsi, &lso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_lseq(&lsi, lso) && cmp_lseq(&lsi, lsr));
    if (!cmp_lseq(&lsi, lso)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_lseq(&lsi);
	fprintf(stdout, " got:\n");
	print_lseq(lso);
	fprintf(stdout, "\n");
    }
    if (!cmp_lseq(&lsi, lsr)) {
	fprintf(stdout, " result error, sent:\n");
	print_lseq(&lsi);
	fprintf(stdout, " got:\n");
	print_lseq(lsr);
	fprintf(stdout, "\n");
    }
    CORBA_free(lso);
    CORBA_free(lsr);
    return -1;
}

static int seq4_test(IC_Env *env)
{
    char *stra0[3] = {"a", "long", "time"}; 
    char *stra1[3] = {"ago", "there", "was"}; 
    char *stra2[3] = {"a", "buggy", "compiler"}; 
    m_sstr3 str3s[3] = {{3, 3, stra0}, {3, 3, stra1}, {3, 3, stra2}};
    m_ssstr3 str3ssi = {3, 3, str3s}; 
    m_ssstr3 *str3sso, *str3ssr;

    fprintf(stdout, "\n======== m_i_seq4 test ======\n\n");
    str3ssr = m_i_seq4_test(NULL, &str3ssi, &str3sso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_ssstr3(&str3ssi, str3sso) && 
		 cmp_ssstr3(&str3ssi, str3ssr));
    if (!cmp_ssstr3(&str3ssi, str3sso)){
	fprintf(stdout, " out parameter error, sent:\n");
	print_ssstr3(&str3ssi);
	fprintf(stdout, " got:\n");
	print_ssstr3(str3sso);
	fprintf(stdout, "\n");
    }
    if (!cmp_ssstr3(&str3ssi, str3ssr)) {
	fprintf(stdout, " result error, sent:\n");
	print_ssstr3(&str3ssi);
	fprintf(stdout, " got:\n");
	print_ssstr3(str3ssr);
	fprintf(stdout, "\n");
    }
    CORBA_free(str3sso);
    CORBA_free(str3ssr);
    return -1;
}

static int seq5_test(IC_Env *env)
{
    m_arr3 arr3a[3] = {
	{4711, 18931947, 3}, 
	{4711, 18931947, 3},
	{4711, 18931947, 3}};
    m_sarr3 arr3sa[3] = {{3, 3, arr3a}, {3, 3, arr3a}, {3, 3, arr3a}};
    m_ssarr3 arr3ssi = {3, 3, arr3sa};
    m_ssarr3 *arr3sso;
    m_ssarr3 *arr3ssr;

    fprintf(stdout, "\n======== m_i_seq5 test ======\n\n");
    arr3ssr = m_i_seq5_test(NULL, &arr3ssi, &arr3sso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_ssarr3(&arr3ssi, arr3sso) &&
		 cmp_ssarr3(&arr3ssi, arr3ssr));
    if (!cmp_ssarr3(&arr3ssi, arr3sso)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_ssarr3(&arr3ssi);
	fprintf(stdout, " got:\n");
	print_ssarr3(arr3sso);
	fprintf(stdout, "\n");
    }
    if (!cmp_ssarr3(&arr3ssi, arr3ssr)) {
	fprintf(stdout, " result error, sent:\n");
	print_ssarr3(&arr3ssi);
	fprintf(stdout, " got:\n");
	print_ssarr3(arr3ssr);
	fprintf(stdout, "\n");
    }
    CORBA_free(arr3sso);
    CORBA_free(arr3ssr);
    return -1;
}

static int array1_test(IC_Env *env)
{
    int i;
    long al[500];
    m_arr1 alo;
    m_arr1_slice* alr;
    
    for (i = 0; i < 500; i++)
	al[i]=i;

    fprintf(stdout, "\n======== m_i_array1 test ======\n\n");
    alr = m_i_array1_test(NULL, al, alo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_arr1(al, alo) && cmp_arr1(al, alr));
    if (!cmp_arr1(al, alo)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_arr1(al);
	fprintf(stdout, " got:\n");
	print_arr1(alo);
	fprintf(stdout, "\n");
    }
    if (!cmp_arr1(al,alr)) {
	fprintf(stdout, " result error, sent:\n");
	print_arr1(al);
	fprintf(stdout, " got:\n");
	print_arr1(alr);
	fprintf(stdout, "\n");
    }
    free(alo);
    free(alr);
    return -1;
}   

static int array2_test(IC_Env *env)
{
    long dl[2][3] = {{11, 2, 7}, {22, 8 ,13}};
    m_dd dlo;
    m_dd_slice* dlr;

    fprintf(stdout, "\n======== m_i_array2 test ======\n\n");
    dlr = m_i_array2_test(NULL, dl, dlo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_dd(dl,dlo) && cmp_dd(dl,dlr));
    if (!cmp_dd(dl,dlo)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_dd(dl);
	fprintf(stdout, " got:\n");
	print_dd(dlo);
	fprintf(stdout, "\n");
    }
    if (!cmp_dd(dl,dlr)) {
	fprintf(stdout, " result error, sent:\n");
	print_dd(dl);
	fprintf(stdout, " got:\n");
	print_dd(dlr);
	fprintf(stdout, "\n");
    }
    free(*dlr);
    return -1;
}

static int enum_test(IC_Env *env)
{
    m_fruit ei = m_banana, eo, er;

    fprintf(stdout, "\n======== m_i_enum test ======\n\n");
    er = m_i_enum_test(NULL, ei, &eo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(ei == eo && ei == er);
    if (ei != eo)
	fprintf(stdout, " out parameter error, sent: %d, got: %d\n", ei, eo);
    if (ei != er)
	fprintf(stdout, " result error, sent: %d, got: %d\n", ei, er);
    return -1;
}

static int string1_test(IC_Env *env)
{
    char* si = longtext;
    char* so;
    char* sr;

    fprintf(stdout, "\n======== m_i_string1 test ======\n\n");
    sr = m_i_string1_test(NULL, si, &so, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_str(si, so) && cmp_str(si, sr));
    if (!cmp_str(si, so))
	fprintf(stdout, " out parameter error, sent: %s, got: %s\n", si, so);
    if (!cmp_str(si, sr))
	fprintf(stdout, " result error, sent: %s, got: %s\n", si, sr);
    CORBA_free(so);
    CORBA_free(sr);
    return -1;
}
  
static int string2_test(IC_Env *env)
{
    char* sa[3] = {"hello", "foo", "bar"};
    m_sseq ssi = {3, 3, sa};
    m_sseq *sso, *ssr;

    fprintf(stdout, "\n======== m_i_string2 test ======\n\n");
    ssr = m_i_string2_test(NULL, &ssi, &sso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_sseq(&ssi, sso) && cmp_sseq(&ssi, sso));
    if (!cmp_sseq(&ssi, sso)) {
	fprintf(stdout, " out parameter error, sent:\n"); 
	print_sseq(&ssi); 
	fprintf(stdout, "got:\n");
	print_sseq(sso); 
    } 
    if (!cmp_sseq(&ssi, ssr)) {
	fprintf(stdout, " result error, sent:\n"); 
	print_sseq(&ssi); 
	fprintf(stdout, "got:\n"); 
	print_sseq(ssr); 
    }    
    CORBA_free(sso);
    CORBA_free(ssr);
    return -1;
}
 
static int string3_test(IC_Env *env)
{
    char* si = longtext;
    char* so;
    char* sr;

    fprintf(stdout, "\n======== m_i_string3 test ======\n\n");
    sr = m_i_string3_test(NULL, si, &so, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_str(si, so) && cmp_str(si, so));
    if (!cmp_str(si, so))
	fprintf(stdout, " out parameter error, sent: %s, got: %s\n", si, so);
    if (!cmp_str(si, sr))
	fprintf(stdout, " result error, sent: %s, got: %s\n", si, sr);
    CORBA_free(so);
    CORBA_free(sr);
    return -1;
}

static int string4_test(IC_Env *env)
{
    char as1[100] = "a string", as2[200] = "help", as3[200] = "hello there";
    m_strRec stri = { 1,	/* dd */
		      as1,	/* str4 */
		      {{'a', 'k'}, {'z', 'g'}, {'n', 'q'}}, /* str7 */
		      {3, 3, "buf"}, /* str5 */
		      as2,	/* str6 */
		      {'m', 'f', 'o'}, /* str8 */
		      as3,	/* str9 */
		      {3, 3, "stu"} /* str10 */
    };
    m_strRec *stro, *strr;

    fprintf(stdout, "\n======== m_i_string4 test ======\n\n");
    strr = m_i_string4_test(NULL, &stri, &stro, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_strRec(&stri,stro) && cmp_strRec(&stri,strr));
    if (!cmp_strRec(&stri,stro)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_strRec(&stri);
	fprintf(stdout, " got:\n");
	print_strRec(stro);
	fprintf(stdout, "\n");
    }
    if (!cmp_strRec(&stri,strr)) {
	fprintf(stdout, " result error, sent:\n");
	print_strRec(&stri);
	fprintf(stdout, " got:\n");
	print_strRec(strr);
	fprintf(stdout, "\n");
    }
    CORBA_free(stro);
    CORBA_free(strr);
    return -1;
}


static int pid_test(IC_Env *env)
{
    erlang_pid pid = {"", 7, 0, 0}, pido, pidr;

    strcpy(pid.node, this_node), /* this currently running node */
    fprintf(stdout, "\n======== m_i_pid test ======\n\n");
    pidr = m_i_pid_test(NULL, &pid, &pido, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_pid(&pid, &pido) && cmp_pid(&pid, &pidr));
    if (!cmp_pid(&pid, &pido)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_pid(&pid);
	fprintf(stdout, "got:\n");
	print_pid(&pido);
    }
    if (!cmp_pid(&pid, &pidr)) {
	fprintf(stdout, " result error, sent:\n");
	print_pid(&pid);
	fprintf(stdout, "got:\n");
	print_pid(&pidr);
    }
    return -1;
}

static int port_test(IC_Env *env)
{
    erlang_port porti = {"node", 5, 1}, porto, portr;

    fprintf(stdout, "\n======== m_i_port test ======\n\n");
    portr = m_i_port_test(NULL, &porti, &porto, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_port(&porti, &porto) && cmp_port(&porti, &portr));
    if (!cmp_port(&porti, &porto)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_port(&porti);
	fprintf(stdout, "got:\n");
	print_port(&porto);
    }
    if (!cmp_port(&porti, &portr)) {
	fprintf(stdout, " result error, sent:\n");
	print_port(&porti);
	fprintf(stdout, "got:\n");
	print_port(&portr);
    }
    return -1;
}

static int ref_test(IC_Env *env)
{
    erlang_ref refi = { "node1", 3, {1, 2, 3}, 1},  
	refo, refr;

    fprintf(stdout, "\n======== m_i_ref test ======\n\n");
    refr = m_i_ref_test(NULL, &refi, &refo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_ref(&refi, &refo) && cmp_ref(&refi, &refr));
    if (!cmp_ref(&refi, &refo)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_ref(&refi);
	fprintf(stdout, "got:\n");
	print_ref(&refo);
    }
    if (!cmp_ref(&refi, &refr)) {
	fprintf(stdout, " result error, sent:\n");
	print_ref(&refi);
	fprintf(stdout, "got:\n");
	print_ref(&refr);
    }
    return -1;
}

static int term_test(IC_Env *env)
{
    ETERM *ti, *to, *tr;

    ti = erl_format("[{hej, 1, 23}, \"string\", {1.23, 45}]");

    fprintf(stdout, "\n======== m_i_term test ======\n\n");
    tr = m_i_term_test(NULL, ti, &to, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(erl_match(ti, to) && erl_match(ti, tr));
    if (!erl_match(ti, to)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_term(ti);
	fprintf(stdout, "got:\n");
	print_term(to);
    }
    if (!erl_match(ti, tr)) {
	fprintf(stdout, " result error, sent:\n");
	print_term(ti);
	fprintf(stdout, "got:\n");
	print_term(tr);
    }
    erl_free_term(ti);
    erl_free_term(to);
    erl_free_term(tr);
    return -1;
}

static int typedef_test(IC_Env *env)
{
    m_banan mbi, mbo;		/* erlang_port */
    m_apa mai;			/* ETERM* */
    m_apa mao = NULL;
    long tl;

    strcpy(mbi.node,"node");
    mbi.id = 15;
    mbi.creation = 1;

    fprintf(stdout, "\n======== m_i_typedef test ======\n\n");
    mai = erl_format("[{hej, 1, 23}, \"string\", {1.23, 45}]");
    tl = m_i_typedef_test(NULL, mai, &mbi, &mao, &mbo, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(erl_match(mai, mao) && cmp_port(&mbi, &mbo) && tl == 4711);
    if (!erl_match(mai, mao)) {
	fprintf(stdout, " out parameter error (term), sent:\n");
	print_term(mai);
	fprintf(stdout, "got:\n");
	print_term(mao);
    }
    if (!cmp_port(&mbi, &mbo)) {
	fprintf(stdout, " out parameter error (port), sent:\n");
	print_port(&mbi);
	fprintf(stdout, "got:\n");
	print_port(&mbo);
    }
    if (tl != 4711) {
	fprintf(stdout, " result error, sent: 4711, got %ld\n", tl);
    }
    erl_free_term(mai);
    erl_free_term(mao);
    return -1;
} 
  
static int inline_sequence_test(IC_Env *env)
{
    int i;
    long al[500];
    m_s isi = {4711, {500, 10, al}},  
	*iso, *isr;

    for (i = 0; i < 500; i++)
	al[i]=i;
    fprintf(stdout, "\n======== m_i_inline_sequence test ======\n\n");
    isr = m_i_inline_sequence_test(NULL, &isi, &iso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_s(&isi, iso) && cmp_s(&isi, isr));
    if (!cmp_s(&isi, iso)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_s(&isi);
	fprintf(stdout, "got:\n");
	print_s(iso);
    }
    if (!cmp_s(&isi, isr)) {
	fprintf(stdout, " result error, sent:\n");
	print_s(&isi);
	fprintf(stdout, "got:\n");
	print_s(isr);
    }
    CORBA_free(iso);
    CORBA_free(isr);
    return -1;
}

static int term_sequence_test(IC_Env *env)
{
    ETERM* et_array[4] = {
	erl_format("[{apa, 1, 23}, \"string\", {1.23, 45}]"),
	erl_format("[{banan, 1, 23}, \"string\", {1.23, 45}]"),
	erl_format("[{apelsin, 1, 23}, \"string\", {1.23, 45}]"),
	erl_format("[{mango, 1, 23}, \"string\", {1.23, 45}]")};
    m_etseq etsi = {4, 4, et_array}, *etso, *etsr;

    fprintf(stdout, "\n======== m_i_term_sequence test ======\n\n");
    etsr = m_i_term_sequence_test(NULL, &etsi, &etso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_etseq(&etsi, etso) && cmp_etseq(&etsi, etsr));
    if (!cmp_etseq(&etsi, etso)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_etseq(&etsi);
	fprintf(stdout, "got:\n");
	print_etseq(etso);
    }
    if (!cmp_etseq(&etsi, etsr)) {
	fprintf(stdout, " result error, sent:\n");
	print_etseq(&etsi);
	fprintf(stdout, "got:\n");
	print_etseq(etsr);
    }
    free_etseq_buf(&etsi);
    free_etseq_buf(etso);
    free_etseq_buf(etsr);
    CORBA_free(etso);
    CORBA_free(etsr);
    return -1;
}

static int term_struct_test(IC_Env *env)
{
    m_et eti = { erl_format("[{hej, 1, 23}, \"string\", {1.23, 45}]"), 
		 121212 };
    m_et eto, etr;

    fprintf(stdout, "\n======== m_i_term_struct test ======\n\n");
    etr = m_i_term_struct_test(NULL, &eti, &eto, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_et(&eti, &eto) && cmp_et(&eti, &etr));
    if (!cmp_et(&eti, &eto)) {
	fprintf(stdout, " out parameter error, sent:\n");
	print_et(&eti);
	fprintf(stdout, "got:\n");
	print_et(&eto);
    }
    if (!cmp_et(&eti, &etr)) {
	fprintf(stdout, " result error, sent:\n");
	print_et(&eti);
	fprintf(stdout, "got:\n");
	print_et(&etr);
    }
    free_et(&eti);
    free_et(&eto);
    free_et(&etr);
    return -1;
}

static int wstring1_test(IC_Env *env)
{
    CORBA_wchar wsi[] = {100, 101, 102, 103, 104, 0}, *wso, *wsr;

    fprintf(stdout, "\n======== m_i_wstring1 test ======\n\n");
    wsr = m_i_wstring1_test(NULL, wsi, &wso, env);
    CHECK_EXCEPTION(env);
    RETURN_IF_OK(cmp_wstr(wsi, wso) && cmp_wstr(wsi, wsr));
    if (!cmp_wstr(wsi, wso)) {
	fprintf(stdout, " out parameter error, sent: \n");
	print_wstr(wsi);
	fprintf(stdout, "got:\n");
	print_wstr(wso);
    }
    if (!cmp_wstr(wsi, wsr)) {
	fprintf(stdout, " result error, sent: \n");
	print_wstr(wsi);
	fprintf(stdout, "got:\n");
	print_wstr(wsr);
    }
    CORBA_free(wso);
    CORBA_free(wsr);
    return -1;
}

/* Compare functions */
static int cmp_aseq(m_aseq *a1, m_aseq *a2)
{
    int i;

    if (a1->_length != a2->_length)
	return 0;
    for (i = 0; i < a1->_length; i++)
	if (cmp_a(&(a1->_buffer[i]), &(a2->_buffer[i])) == 0)
	    return 0;
    return 1;
}

static int cmp_a(m_a *a1, m_a *a2)
{
    return a1->l == a2->l && 
	a1->d == a2->d && 
	cmp_bseq(&a1->y, &a2->y);
}

static int cmp_bseq(m_bseq *b1, m_bseq *b2)
{
    int i;

    if (b1->_length != b2->_length)
	return 0;
    for (i = 0; i < b1->_length; i++)
	if (cmp_b(&(b1->_buffer[i]), &(b2->_buffer[i])) == 0)
	    return 0;
    return 1;
}

static int cmp_b(m_b *b1, m_b *b2)
{
    return b1->l == b2->l && b1->c == b2->c;
}

static int cmp_lseq(m_lseq *b1, m_lseq *b2)
{
    int i;

    if (b1->_length != b2->_length)
	return 0;
    for (i = 0; i < b1->_length; i++)
	if (b1->_buffer[i] != b2->_buffer[i])
	    return 0;
    return 1;
}

static int cmp_etseq(m_etseq *b1, m_etseq *b2)
{
    int i;

    if (b1->_length != b2->_length)
	return 0;
    for (i = 0; i < b1->_length; i++)
	if (!erl_match(b1->_buffer[i], b2->_buffer[i]))
	    return 0;
    return 1;
}

static int cmp_et(m_et* b1, m_et *b2)
{
    return erl_match(b1->e, b2->e) && b1->l == b2->l;
}

static int cmp_es(m_es *b1, m_es *b2)
{
    return b1->f == b2->f && b1->l == b2->l;
}

static int cmp_arr1(m_arr1 b1, m_arr1 b2)
{
    int i;

    for (i = 0; i < 500; i++)
	if (b1[i] != b2[i])
	    return 0;
    return 1; 
}

static int cmp_dd(m_dd b1, m_dd b2)
{

    int i, j;

    for (i = 0; i < 2; i++)
	for (j = 0; j < 3; j++)
	    if (b1[i][j] != b2[i][j])
		return 0;
    return 1; 
}



static int cmp_strRec(m_strRec *b1, m_strRec *b2)
{
    int i, j;

    if (b1->bb != b2->bb) 
	return 0;
    if (!cmp_str(b1->str4,b2->str4))
	return 0;
    if (b1->str5._length != b2->str5._length)
	return 0;
    for (j = 0; j < b1->str5._length; j++)
	if (b1->str5._buffer[j] != b2->str5._buffer[j])
	    return 0;
    if (!cmp_str(b1->str6,b2->str6))
	return 0;
    for (i = 0; i < 2; i++)
	for (j = 0; j < 3; j++)
	    if (b1->str7[i][j] != b2->str7[i][j])
		return 0;
    for (j = 0; j < 3; j++)
	if (b1->str8[j] != b2->str8[j])
	    return 0;
    if (!cmp_str(b1->str9,b2->str9))
	return 0;
    if (b1->str10._length != b2->str10._length)
	return 0;
    for (j = 0; j < b1->str10._length; j++)
	if (b1->str10._buffer[j] != b2->str10._buffer[j])
	    return 0;
    return 1; 
}


static int cmp_sseq(m_sseq *b1, m_sseq *b2)
{
    int i;

    if (b1->_length != b2->_length)
	return 0;
    for (i = 0; i < b1->_length; i++)
	if (!cmp_str(b1->_buffer[i], b2->_buffer[i]))
	    return 0;
    return 1;
}


static int cmp_pid(erlang_pid *p1, erlang_pid *p2)
{
    return cmp_str(p1->node,p2-> node) && 
	p1->num == p2->num && 
	p1->serial == p2->serial &&
	p1->creation == p2->creation;
}

static int cmp_port(erlang_port *p1, erlang_port *p2)
{
    return cmp_str(p1->node,p2-> node) && p1->id == p2->id;
}

static int cmp_ref(erlang_ref *p1, erlang_ref *p2)
{
    return cmp_str(p1->node, p2->node) && 
	p1->len == p2->len &&
	(p1->len < 1 || p1->n[0] == p2->n[0]) &&
	(p1->len < 2 || p1->n[1] == p2->n[1]) &&
	(p1->len < 3 || p1->n[2] == p2->n[2]);
}

static int cmp_s(m_s *b1, m_s *b2)
{
    int i;

    if (b1->l != b2->l)
	return 0;
    if (b1->sl._length != b2->sl._length)
	return 0;
    for (i = 0; i < b1->sl._length; i++)
	if (b1->sl._buffer[i] != b2->sl._buffer[i])
	    return 0;
    return 1;
}


static int cmp_ssstr3(m_ssstr3 *b1, m_ssstr3 *b2)
{
    int i,j;

    if (b1->_length != b2->_length)
	return 0;
    for (i = 0; i < b1->_length; i++) {
	if (b1->_buffer[i]._length != b2->_buffer[i]._length)
	    return 0;
	for (j = 0; j < b1->_buffer[i]._length; j++)
	    if (!cmp_str(b1->_buffer[i]._buffer[j], 
		      b2->_buffer[i]._buffer[j]))
		return 0;
    }
    return 1;
}



static int cmp_ssarr3(m_ssarr3 *b1, m_ssarr3 *b2)
{
    int i;

    if (b1->_length != b2->_length)
	return 0;
    for (i = 0; i < b1->_length; i++) {
	if (!cmp_sarr3(&b1->_buffer[i], &b2->_buffer[i]))
	    return 0;
    }
    return 1;
}

static int cmp_sarr3(m_sarr3 *b1, m_sarr3 *b2)
{
    int i;

    if (b1->_length != b2->_length)
	return 0;
    for (i = 0; i < b1->_length; i++) {
	if (!cmp_arr3(b1->_buffer[i], b2->_buffer[i]))
	    return 0;
    }
    return 1;
}

static int cmp_arr3(m_arr3 b1, m_arr3 b2)
{
    int i;

    for (i = 0; i < sizeof(m_arr3)/sizeof(CORBA_long); i++) {
	if (b1[i] != b2[i])
	    return 0;
    }
    return 1;
}

/* Print functions */
static void print_aseq(m_aseq *a)
{
    int i;
    fprintf(stdout, "\nm_aseq size: %ld  --------\n", a->_length);
    for (i = 0; i < a->_length; i++)
	print_a(&(a->_buffer[i]));
}

static void print_a(m_a *a)
{
    fprintf(stdout, "\nm_a --------\n l: %ld\n d:%f\n", a->l, a->d);
    print_bseq(&a->y);
}

static void print_bseq(m_bseq *b)
{
    int i;

    fprintf(stdout, "\nm_bseq size: %ld  --------\n",b->_length);
    for (i = 0; i < b->_length; i++)
	print_b(&(b->_buffer[i]));
}

static void print_lseq(m_lseq *b)
{
    int i;

    fprintf(stdout, "\nm_lseq size: %ld  --------\n",b->_length);
    for (i = 0; i < b->_length; i++)
	fprintf(stdout, "[%d]: %ld\n", i, b->_buffer[i]);
}

static void print_b(m_b *b)
{
    fprintf(stdout, "\nm_b --------\n l: %ld\n c: %c\n", b->l, b->c);
}


static void print_etseq(m_etseq *b)
{
    int i;

    for (i = 0; i < b->_length; i++) {
	fprintf(stdout, "[%d]:\n", i);
	erl_print_term(stdout, b->_buffer[i]);
    }
}


static void print_et(m_et* b)
{
    fprintf(stdout, "\net struct --------\n");
    erl_print_term(stdout, b->e);
    fprintf(stdout, "long: %ld\n", b->l);
    fprintf(stdout, "\n--------\n");
}

static void print_es(m_es *b)
{
    fprintf(stdout, "\nm_es --------\n f: %d\n l: %ld\n", b->f, b->l);
}


static void print_arr1(long a[10])
{
    int i;

    for (i = 0; i < 10; i++)
	fprintf(stdout, "\n[%d]: %ld\n", i, a[i]);
}

static void print_dd(long a[2][3])
{
    int i, j;

    fprintf(stdout, "\nlong dd[2][3] --------\n");
    for (i = 0; i < 2; i++)
	for (j = 0; j < 3; j++)
	    fprintf(stdout, "\n[%d][%d]: %ld\n", i, j, a[i][j]);
}


static void print_strRec(m_strRec* sr)
{
    int i, j;

    fprintf(stdout, "\nboolean bb : %d\n",sr->bb);
    fprintf(stdout, "string str4 : %s\n",sr->str4);
    fprintf(stdout, "str7[2][3] :\n");
    for (i = 0; i < 2; i++)
	for (j = 0; j < 3; j++)
	    fprintf(stdout, "str7[%d][%d]: %ld\n", i, j, sr->str7[i][j]);
    fprintf(stdout, "str5._length : %ld\n",sr->str5._length);
    for (j = 0; j < sr->str5._length; j++)
	fprintf(stdout, "str5._buffer[%d]: %c\n", j, sr->str5._buffer[j]);
    fprintf(stdout, "string str6 : %s\n",sr->str6);
    fprintf(stdout, "str8 :\n");
    for (j = 0; j < 3; j++)
	fprintf(stdout, "str8[%d]: %c\n", j, sr->str8[j]);
    fprintf(stdout, "string str9 : %s\n",sr->str9);
    fprintf(stdout, "str10._length : %ld\n",sr->str10._length);
    for (j = 0; j < sr->str10._length; j++)
	fprintf(stdout, "str10._buffer[%d]: %c\n", j, sr->str10._buffer[j]);
}

static void print_sseq(m_sseq *b)
{
    int i;

    fprintf(stdout, "\nm_sseq size: %ld  --------\n",b->_length);
    for (i = 0; i < b->_length; i++)
	fprintf(stdout, "%s\n", b->_buffer[i]);

}


static void print_pid(erlang_pid *p)
{
    fprintf(stdout, "\nerlang_pid --------\n node: %s\n num: %d\n "
	    "serial: %d\n creation: %d\n", 
	    p->node, p->num, p->serial, p->creation);
}

static void print_port(erlang_port *p)
{
    fprintf(stdout, "\nerlang_port --------\n node: %s\n id: %d\n "
	    "creation: %d\n", p->node, p->id, p->creation);
}

static void print_ref(erlang_ref *p)
{
    fprintf(stdout, "\nerlang_ref --------\n node: %s\n len: %d\n "
	    "n[0]: %d\n n[1]: %d\n n[2]: %d\n creation: %d\n", 
	    p->node, p->len, p->n[0], p->n[1], p->n[2], p->creation);
}

static void print_term(ETERM *t)
{
    fprintf(stdout, "\nETERM --------\n");
    erl_print_term(stdout, t);
    fprintf(stdout, "\n--------\n");
}

static void print_s(m_s *p)
{
    int i;

    fprintf(stdout, "\n%ld\n", p->l);
    for (i = 0; i < p->sl._length; i++)
	fprintf(stdout, "\n[%d]: %ld\n", i, p->sl._buffer[i]);
}


static void print_ssstr3(m_ssstr3 *b1)
{
    int i,j;

    fprintf(stdout, "\nSSSTR3 --------\n");
    fprintf(stdout,"b1->_length = %ld\n",b1->_length);
    for (i = 0; i < b1->_length; i++) {
	fprintf(stdout,"\nb1->_buffer[%d]._length %ld\n",
		i, b1->_buffer[i]._length);
	for (j = 0; j < b1->_buffer[i]._length; j++)
	    fprintf(stdout,"b1->_buffer[%d]._buffer[%d] = %s\n",
		    i, j, b1->_buffer[i]._buffer[j]);
    }
    fprintf(stdout, "\n--------\n");
}

static void print_wstr(CORBA_wchar *ws)
{
    int i = 0;

    fprintf(stdout, "\nwstr --------\n");
    while (ws[i]) {
	fprintf(stdout, "[%d]: %ld\n", i, ws[i]);
	i++;
    }
    fprintf(stdout, "\n--------\n");
}


static void print_ssarr3(m_ssarr3 *b1)
{
    int i;

    fprintf(stdout, "\nssarr3 --------\n");
    fprintf(stdout,"length: %ld\n",b1->_length);
    fprintf(stdout, "buffer:\n");
    for (i = 0; i < b1->_length; i++) 
	print_sarr3(&b1->_buffer[i]); 
    fprintf(stdout, "\n--------\n");
}

static void print_sarr3(m_sarr3 *b1)
{
    int i;

    fprintf(stdout, "\nsarr3 --------\n");
    fprintf(stdout,"length: %ld\n",b1->_length);
    fprintf(stdout, "buffer:\n");
    for (i = 0; i < b1->_length; i++) 
	print_arr3(b1->_buffer[i]); 
    fprintf(stdout, "\n--------\n");
}

static void print_arr3(m_arr3 b1)
{
    int i;

    fprintf(stdout, "\narr3 --------\n");
    for (i = 0; i < sizeof(m_arr3)/sizeof(CORBA_long); i++) 
	fprintf(stdout, "%ld ", b1[i]); 
    fprintf(stdout, "\n--------\n");
}

static void free_etseq_buf(m_etseq *b)
{
    int i;

    for (i = 0; i < b->_length; i++)
	erl_free_term(b->_buffer[i]);
}

static void free_et(m_et* b)
{
    erl_free_term(b->e);
}

static void showtime(MyTimeval *start, MyTimeval *stop)
{
    MyTimeval elapsed;

    elapsed.tv_sec = stop->tv_sec - start->tv_sec;
    elapsed.tv_usec = stop->tv_usec - start->tv_usec;
    while (elapsed.tv_usec < 0) {
	elapsed.tv_sec -= 1;
	elapsed.tv_usec += 1000000;
    }
    fprintf(stderr,"%ld.%06ld seconds\n",elapsed.tv_sec, elapsed.tv_usec);
}

static void my_gettimeofday(MyTimeval *tv)
#ifdef __WIN32__
#define EPOCH_JULIAN_DIFF 11644473600i64
{
    SYSTEMTIME t;
    FILETIME ft;
    LONGLONG lft;

    GetSystemTime(&t);
    SystemTimeToFileTime(&t, &ft);
    memcpy(&lft, &ft, sizeof(lft));
    tv->tv_usec = (long) ((lft / 10i64) % 1000000i64);
    tv->tv_sec = (long) ((lft / 10000000i64) - EPOCH_JULIAN_DIFF);
}
#elif defined VXWORKS
{
    int rate = sysClkRateGet(); /* Ticks per second */
    unsigned long ctick = tickGet();
    tv->tv_sec = ctick / rate; /* secs since reboot */
    tv->tv_usec = ((ctick - (tv->tv_sec * rate))*1000000)/rate;
}
#else
{
    gettimeofday(tv, NULL);
}
#endif
