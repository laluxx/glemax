#include "fileio.h"
#include <libguile.h>
#include <sys/stat.h>

static SCM scm_file_exists_p(SCM filename) {
    if (!scm_is_string(filename)) {
        scm_wrong_type_arg("file-exists?", 1, filename);
    }

    char *path = scm_to_locale_string(filename);
    struct stat st;
    int exists = stat(path, &st) == 0;
    free(path);

    return scm_from_bool(exists);
}

void init_fileio_bindings(void) {
    scm_c_define_gsubr("file-exists?", 1, 0, 0, scm_file_exists_p);
}
