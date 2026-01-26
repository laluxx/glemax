#include "glemax.h"
#include <libguile.h>

bool should_quit = false;

static SCM scm_kill_glemax(void) {
    should_quit = true;
    return SCM_BOOL_T;
}

void init_glemax_bindings(void) {
    scm_c_define_gsubr("kill-glemax", 0, 0, 0, scm_kill_glemax);
}


