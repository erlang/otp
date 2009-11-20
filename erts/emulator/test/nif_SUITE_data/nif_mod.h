typedef struct call_info_t
{
    struct call_info_t* next;
    unsigned lib_ver;
    int static_cntA;
    int static_cntB;
    char func_name[1]; /* must be last */
}CallInfo;


typedef struct
{
    int calls;
    int ref_cnt;
    CallInfo* call_history;
}NifModPrivData;

