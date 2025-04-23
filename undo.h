#ifndef UNDO_H
#define UNDO_H

#include "buffer.h"


void initUndos(Undos *undos);
void freeUndos(Undos *undos);
void screenshot(Buffer *buffer);
bool undo(Buffer *buffer);
bool redo(Buffer *buffer);
void printUndoInfo(Undos *undos);

#endif
