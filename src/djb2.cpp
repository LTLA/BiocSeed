#include "Rdefines.h"
#include <R_ext/Rdynload.h>
#include <limits>

/* Modified from http://www.cse.yorku.ca/~oz/hash.html */ 

typedef unsigned int T;

SEXP djb2(SEXP incoming) {
    const unsigned char* ptr = RAW(incoming);
    const auto len = length(incoming);
    const unsigned char* end = ptr + len;

    T hash = 5381;
    while (ptr != end) {
        hash = ((hash << 5) + hash) + *ptr; 
        ++ptr;
    }

    // Whittling it down.
    int output;
    constexpr T upper = (static_cast<T>(1) << (std::numeric_limits<T>::digits - 1)); // 1 bit for the sign.

    if (hash < upper) {
        output = hash;
    } else {
        output = (hash - upper);
        if (output == 0) { // this is an NA_integer_, for which set.seed() fails; so we just set it to zero.
            output = 0;
        } else {
            // Two rounds, to avoid implicit case of upper to 'int'.
            constexpr int half = upper/2;
            output -= half;
            output -= half;
        }
    }

    return ScalarInteger(output);
}

extern "C" {

static const R_CallMethodDef callMethods[]  = {
    {"djb2", (DL_FUNC) &djb2, 1},
    {NULL, NULL, 0}
};

void R_init_BiocSeed(DllInfo *info) {
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

}
