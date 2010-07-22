// cl_libxml2.c
//
// This file is part of the cl-libxml2 library, released under Lisp-LGPL.
// See file COPYING for details.
//
// Author: Moskvitin Andrey <archimag@gmail.com>

#include <stdio.h>
#include <stdarg.h>
#include <malloc.h>

typedef void (*lisp_level_error_func) (const char *string);

void cl_libxml2_error_func (lisp_level_error_func lispFun, const char *msg, ...) {
    va_list args;

    if (lispFun != NULL) {
        va_start(args, msg);
                    
        char *strp;
        int n = vasprintf(&strp, msg, args);

        if (n != -1) {
            lispFun(strp);
            free(strp);
        }
        else {
            lispFun("vasprintf: unknow error");
        }
        va_end(args);
    }
    else {
        va_start(args, msg);
        vfprintf(stderr, msg, args);
        va_end(args);
    }
}


