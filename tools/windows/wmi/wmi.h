#pragma once

extern "C" {

#include "erl_nif.h"

static int enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf, int size);
static ERL_NIF_TERM dispatch(ErlNifEnv* env, PWMI_LOGIN_INFO login, ERL_NIF_TERM parameter);
static ERL_NIF_TERM execute(ErlNifEnv* env, ERL_NIF_TERM arg);
int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
void unload(ErlNifEnv* env, void* priv_data);

}