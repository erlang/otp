typedef struct call_info_t
{
    struct call_info_t* next;
    unsigned lib_ver;
    int static_cntA;
    int static_cntB;
    char* arg;
    int arg_sz;
    char func_name[1]; /* must be last */
}CallInfo;

#define RT_MAX 5

typedef struct
{
    ErlNifMutex* mtx;
    int calls;
    int ref_cnt;
    CallInfo* call_history;
    ErlNifResourceType* rt_arr[RT_MAX];
}NifModPrivData;

