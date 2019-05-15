#ifndef DYNALYZER_UTILITIES_H
#define DYNALYZER_UTILITIES_H

#include "stdlibs.h"
#include <algorithm>


#define failwith(format, ...) \
    failwith_impl(__FILE__, __LINE__, format, __VA_ARGS__)

#define failwith_impl(file, line, format, ...)                             \
    do {                                                                   \
        fprintf(stderr, "ERROR [%s:%d] " format, file, line, __VA_ARGS__); \
        exit(EXIT_FAILURE);                                                \
    } while (0)


inline int get_file_size(std::ifstream& file) {
  int position = file.tellg();
    file.seekg(0, std::ios_base::end);
    int length = file.tellg();
    file.seekg(position, std::ios_base::beg);
    return length;
}

inline std::string readfile(std::ifstream& file) {
  std::string contents;
    file.seekg(0, std::ios::end);
    contents.reserve(file.tellg());
    file.seekg(0, std::ios::beg);
    contents.assign(std::istreambuf_iterator<char>(file),
                    std::istreambuf_iterator<char>());
    return contents;
}

inline bool file_exists(const std::string& filepath) {
      return std::ifstream(filepath).good();
}

inline bool sexp_to_bool(SEXP value) {
      return LOGICAL(value)[0] == TRUE;
}

inline int sexp_to_int(SEXP value) {
      return (int) *INTEGER(value);
}

inline std::string sexp_to_string(SEXP value) {
      return std::string(CHAR(STRING_ELT(value, 0)));
}

inline std::string to_string(const char* str) {
      return str ? std::string(str) : std::string("");
}

inline std::string check_string(const char* s) {
    return s == NULL ? "<unknown>" : s;
}

inline void* malloc_or_die(std::size_t size) {
    void* data = std::malloc(size);
    if (data == nullptr) {
        failwith("memory allocation error: unable to allocate %zu bytes.\n",
                 size);
    }
    return data;
}

inline void* calloc_or_die(std::size_t num, std::size_t size) {
    void* data = std::calloc(num, size);
    if (data == nullptr) {
        failwith("memory allocation error: unable to allocate %zu bytes.\n",
                 size);
    }
    return data;
}

inline void* realloc_or_die(void* ptr, std::size_t size) {
    void* data = std::realloc(ptr, size);
    if (data == nullptr) {
        failwith("memory allocation error: unable to reallocate %zu bytes.\n",
                 size);
    }
    return data;
}


#endif /* DYNALYZER_UTILITIES_H */
