#include "table.h"

#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#ifdef __cplusplus
extern "C" {
#endif

static const R_CallMethodDef CallEntries[] = {
    {"write_data_table", (DL_FUNC) &write_data_table, 5},
    {"read_data_table", (DL_FUNC) &read_data_table, 3},
    {"create_data_table", (DL_FUNC) &create_data_table, 5},
    {NULL, NULL, 0}};

void attribute_visible R_init_dynalyzer(DllInfo* dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);

    /* used by external packages linking to dynalyzer code from C */
    R_RegisterCCallable("dynalyzer", "write_data_table", (DL_FUNC) &write_data_table);
    R_RegisterCCallable("dynalyzer", "read_data_table", (DL_FUNC) &read_data_table);
    R_RegisterCCallable("dynalyzer", "create_data_table", (DL_FUNC) &create_data_table);
}

#ifdef __cplusplus
}
#endif
