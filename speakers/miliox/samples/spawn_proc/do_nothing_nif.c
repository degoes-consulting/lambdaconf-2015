#include <mach/mach_time.h>
#include "erl_nif.h"

static ERL_NIF_TERM timestamp(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned long now = (unsigned long) mach_absolute_time();
    return enif_make_uint64(env, now);
}


static ERL_NIF_TERM diff_timestamp(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned long start = 0;
    unsigned long end   = 0;

    enif_get_uint64(env, argv[0], &end);
    enif_get_uint64(env, argv[1], &start);

    uint64_t elapsed = end - start;

    mach_timebase_info_data_t info;

    uint64_t nanosecs = 0;
    if (mach_timebase_info (&info) == KERN_SUCCESS) {
        nanosecs = elapsed * info.numer / info.denom;
    }

    return enif_make_uint64(env, nanosecs);
}

static ErlNifFunc nif_funcs[] =
{
    {"timestamp", 0, timestamp},
    {"diff_timestamp", 2, diff_timestamp}
};

ERL_NIF_INIT(do_nothing_proc,nif_funcs,NULL,NULL,NULL,NULL)
