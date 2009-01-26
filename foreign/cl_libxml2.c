// cl_libxml2.c

#include <stdio.h>
#include <stdarg.h>

typedef void (*lisp_level_error_func) (const char *string);


/*
lisp_level_error_func lispGenericError = NULL;

void listSetGenericErrorFunc (lisp_level_error_func func) {
    lispGenericError = func;
}
*/

void cl_libxml2_error_func (lisp_level_error_func lispFun, const char *msg, ...) {
    va_list args;

    if (lispFun != NULL) {
            
        //char **strp;
        char buffer[1024];
    
        va_start(args, msg);
        //int n = vasprintf(strp, msg, args);
        int n = sprintf(buffer, msg, args);
        lispFun(&buffer[0]);
        //free(strp);
        
        va_end(args);
    }
    else {
        fprintf(stderr, msg, args);
    }

    
}

