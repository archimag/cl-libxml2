// cl_libxml2.c

#include <stdio.h>
#include <stdarg.h>

#include <libxml/tree.h>

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

int xmlGetVersion () {
    return LIBXML_VERSION;
}

#if LIBXML_VERSION >= 20700
int xmlGetDocProperties (xmlDocPtr doc) {
    return doc->properties;
}

void xmlSetDocProperties (xmlDocPtr doc, int properties) {
    doc->properties = properties;
}
#endif


