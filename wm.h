#ifndef WM_H
#define WM_H

#include "buffer.h"
#include "font.h"

// NOTE Window and WindowManager types
// are defined in buffer.h to avoid
// circular dependency between the 2 headers

void initWindowManager(        WindowManager *wm, BufferManager *bm, Font *font, int sw, int sh);
void freeWindowManager(        WindowManager *wm);
void split_window_right(       WindowManager *wm, Font *font, int sw, int sh);
void split_window_below(       WindowManager *wm, Font *font, int sw, int sh);
void delete_window(            WindowManager *wm);
void other_window(             WindowManager *wm, int direction);
void updateWindows(            WindowManager *wm, Font *font, int newWidth, int newHeight);
void printActiveWindowDetails( WindowManager *wm);
bool isBottomWindow(           WindowManager *wm, Window *window);
void swap_window(              WindowManager *wm, int direction);
void shell_command_to_window(  WindowManager *wm, BufferManager *bm, const char *command, int sw, int sh);
void switch_or_split_window(WindowManager *wm, Font *font, char *buffer_name, int sw, int sh);

#endif // WM_H
