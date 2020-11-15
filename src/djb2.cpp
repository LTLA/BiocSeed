#include "Rdefines.h"
#include <R_ext/Rdynload.h>
#include <limits>
#include <cstdint>

/* We'll assume int is at least 32 bits here. We convert from unsigned to
 * signed by reversing the overflow logic.
 */

typedef std::int32_t S;
typedef std::uint32_t U;

int convert_to_int (U hash) { 
    constexpr U intmax = std::numeric_limits<S>::max();

    S output;
    if (hash <= intmax) {
        output = static_cast<S>(hash);
    } else {
        hash -= intmax + 1;
        if (hash==0) { // as this is the NA value.
            output = 0;
        } else {
            output = static_cast<S>(hash);
            output -= static_cast<S>(intmax);
            output -= 1;
        }
    }

    return output;
}

SEXP check_conversion(SEXP incoming) {
    return ScalarInteger(convert_to_int(static_cast<U>(*(REAL(incoming)))));
}

/* Modified from http://www.cse.yorku.ca/~oz/hash.html */ 

SEXP djb2_char(SEXP incoming) {
    const size_t len = length(incoming);

    U hash = 5381;
    for (size_t i = 0; i < len; ++i) {
        SEXP current = STRING_ELT(incoming, i);
        const char* ptr = CHAR(current);
        while (*ptr != '\0') {
            hash = ((hash << 5) + hash) + *ptr; 
            ++ptr;
        }
    }

    return ScalarInteger(convert_to_int(hash));
}

SEXP djb2_int(SEXP incoming) {
    const int * ptr = INTEGER(incoming);
    const size_t len = length(incoming);
    constexpr int width = std::numeric_limits<U>::digits;

    U hash = 5381;
    for (size_t i = 0; i < len; ++i) {
        U current = ptr[i];
        for (int j = 0; j < width; j+=8, current >>= 8) {
            hash = ((hash << 5) + hash) + (current & 0xFF);
        }
    }

    return ScalarInteger(convert_to_int(hash));
}

extern "C" {

static const R_CallMethodDef callMethods[]  = {
    {"djb2_int", (DL_FUNC) &djb2_int, 1},
    {"djb2_char", (DL_FUNC) &djb2_char, 1},
    {"check_conversion", (DL_FUNC) &check_conversion, 1},
    {NULL, NULL, 0}
};

void R_init_BiocSeed(DllInfo *info) {
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}

}
