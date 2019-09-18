
#include "erl_nif.h"

#include "sha-2/sha-256.h"
struct buffer_state;
#include "gen_yielding_sha_256.h"




ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
  ERL_NIF_TERM ret;

  if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
      return enif_make_atom(env, atom);
    }

  return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
  return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}



void* allocator(size_t size, void* context){
  (void)context;
  return enif_alloc(size);
}

void freer(void* data, void* context){
  (void)context;
  enif_free(data);
}

typedef struct {
  void* continuation;
  ErlNifEnv* work_env;
  ErlNifBinary out_bin;
} sha256continuation;

/* #define DEBUG */
#ifdef DEBUG
# define DEBUG_PRINT(x) printf x
long nr_of_yields = 0;
#else
# define DEBUG_PRINT(x) do {} while (0)
#endif

void sha256continuation_ErlNifResourceDtor(ErlNifEnv* caller_env, void* obj){
  sha256continuation* continuation = obj;
  if(continuation->continuation != NULL){
    calc_sha_256_ycf_gen_destroy(continuation->continuation);
    enif_release_binary(&continuation->out_bin);
    enif_free_env(continuation->work_env);
    continuation->continuation = NULL;
    DEBUG_PRINT(("DESTOY\n"));
  }
  DEBUG_PRINT(("DEALLOC\n"));
}

#define REDUCTIONS_UNTIL_YCF_YIELD() (200*300*8)

static ERL_NIF_TERM
sha256_nif_cont_after_yield(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
  long nr_of_reductions = REDUCTIONS_UNTIL_YCF_YIELD();
  sha256continuation* continuation;
  enif_get_resource(env,
                    argv[0],
                    res_type,
                    (void**)&continuation);
  calc_sha_256_ycf_gen_continue(&nr_of_reductions,
                            &continuation->continuation,
                            NULL);
  if(YCF_IS_YIELDED(continuation->continuation)){
#ifdef DEBUG
    nr_of_yields++;
#endif
    return enif_schedule_nif(env, "sha256_nif_cont_after_yield", 0, sha256_nif_cont_after_yield, 1, argv);
  }
  DEBUG_PRINT(("NUMBER OF YCF_YIELD()S %ld \n", nr_of_yields));
  enif_free_env(continuation->work_env);
  ErlNifBinary out_bin = continuation->out_bin;
  return enif_make_binary(env, &out_bin);
}

static ERL_NIF_TERM
sha256_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  /* ErlNifEnv* msg_env; */
  ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
  ErlNifEnv *work_env = enif_alloc_env();
  void* wb = NULL;
  long nr_of_reductions;
  ERL_NIF_TERM newargv[1];
  if(argc != 1 || !enif_is_binary(env, argv[0]))
    {
      return enif_make_badarg(env);
    }
  /* Copy the input binary to the work environemnt so it will be kept when we are yielding */
  ERL_NIF_TERM input_bin_term = enif_make_copy(work_env, argv[0]);
  ErlNifBinary input_bin;
  enif_inspect_binary(work_env, input_bin_term, &input_bin);
  ErlNifBinary out_bin;
  enif_alloc_binary(256/8, &out_bin);
  nr_of_reductions = REDUCTIONS_UNTIL_YCF_YIELD();
#ifdef DEBUG
    nr_of_yields = 0;
#endif
  calc_sha_256_ycf_gen_yielding(&nr_of_reductions,
                            &wb,
                            NULL,
                            allocator,
                            freer,
                            NULL,
                            64,
                            NULL,
                            out_bin.data,
                            input_bin.data,
                            input_bin.size);
  if(YCF_IS_YIELDED(wb)){
    DEBUG_PRINT(("TRAPPED FIRST CALL %ld \n", nr_of_reductions));
    sha256continuation* continuation =
      enif_alloc_resource(res_type,
                          sizeof(sha256continuation));
    continuation->work_env = work_env;
    continuation->continuation = wb;
    continuation->out_bin = out_bin;
    newargv[0] = enif_make_resource(env, continuation);
    enif_release_resource(continuation);
    return enif_schedule_nif(env, "sha256_nif_cont_after_yield", 0, sha256_nif_cont_after_yield, 1, newargv);
  }
  enif_free_env(work_env);
  return enif_make_binary(env, &out_bin);
}

static ErlNifFunc nif_funcs[] = {
  {"sha256", 1, sha256_nif}
};

static int
nifload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  *priv_data = enif_open_resource_type(env,
                                       NULL,
                                       "sha256_nif",
                                       sha256continuation_ErlNifResourceDtor,
                                       ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                       NULL);
  return 0;
}

static int
nifupgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  *priv_data = enif_open_resource_type(env,
                                       NULL,
                                       "sha256_nif",
                                       sha256continuation_ErlNifResourceDtor,
                                       ERL_NIF_RT_TAKEOVER,
                                       NULL);
  return 0;
}

ERL_NIF_INIT(sha256_nif, nif_funcs, nifload, NULL, nifupgrade, NULL);
