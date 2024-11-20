#ifndef THEME_H
#define THEME_H

#include "common.h"

typedef struct {
    char* name;
    Color bg;
    Color cursor;
    Color text;
    Color minibuffer;
    Color modeline;
    Color modeline_highlight;
    Color show_paren_match;
    Color isearch_highlight;
    Color minibuffer_prompt;
    Color region;
    Color message;
    Color type;
    Color string;
    Color number;
    Color function;
    Color preprocessor;
    Color operator;
    Color variable;
    Color keyword;
    Color comment;
    Color null;
    Color negation;
} Theme;

extern Theme themes[];
extern int currentThemeIndex;

#define CT (themes[currentThemeIndex])

Color hexToColor(const char* hexStr);
void initThemes();
void nextTheme();
void previousTheme();

#endif  // THEME_H

