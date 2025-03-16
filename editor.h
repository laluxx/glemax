#ifndef EDITOR_H
#define EDITOR_H

#include "buffer.h"
#include "history.h"
#include "wm.h"
#include "git.h"
#include "edit.h"
#include "edit.h"
#include "completion.h"

extern CompletionEngine ce;
extern BufferManager bm;
extern KillRing kr;
extern WindowManager wm;
extern NamedHistories nh;
extern Diffs diffs;

#endif // EDITOR_H
