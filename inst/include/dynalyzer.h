#ifndef DYNALYZER_API_H
#define DYNALYZER_API_H

#include "DataTableStream.h"

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


inline DataTableStream* dynalyzer_create_data_table(const std::string& table_filepath,
                                                    const std::vector<std::string>& column_names,
                                                    bool truncate,
                                                    bool binary = true,
                                                    int compression_level = 0) {
  static DataTableStream* (*fun)(const std::string&, const std::vector<std::string>&, bool, bool, int) = NULL;
    if (fun == NULL)
      fun = (DataTableStream* (*)(const std::string&, const std::vector<std::string>&, bool, bool, int)) R_GetCCallable("dynalyzer", "create_data_table");
    return fun(table_filepath, column_names, truncate, binary, compression_level);
}

#ifdef __cplusplus
extern "C" {
#endif

inline SEXP dynalyzer_write_data_table(SEXP data_frame,
                                                 SEXP table_filepath,
                                                 SEXP truncate,
                                                 SEXP binary,
                                                 SEXP compression_level) {
    static SEXP (*fun)(SEXP, SEXP, SEXP, SEXP, SEXP) = NULL;
    if (fun == NULL)
        fun = (SEXP(*)(SEXP, SEXP, SEXP, SEXP, SEXP)) R_GetCCallable(
            "dynalyzer", "write_data_table");
    return fun(data_frame, table_filepath, truncate, binary, compression_level);
}

inline SEXP dynalyzer_read_data_table(SEXP table_filepath,
                                                SEXP binary,
                                                SEXP compression_level) {
    static SEXP (*fun)(SEXP, SEXP, SEXP) = NULL;
    if (fun == NULL)
        fun = (SEXP(*)(SEXP, SEXP, SEXP)) R_GetCCallable("dynalyzer",
                                                         "read_data_table");
    return fun(table_filepath, binary, compression_level);
}

#ifdef __cplusplus
}
#endif

#endif /* DYNALYZER_API_H */
