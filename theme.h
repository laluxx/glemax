#ifndef THEME_H
#define THEME_H

#include "common.h"
#include <stdbool.h>

typedef struct {
    char *name;
    Color bg;
    Color cursor;
    Color marked_cursor;
    Color text;
    Color minibuffer;
    Color modeline;
    Color modeline_inactive;
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
    Color success;
    Color warning;
    Color error;
    Color fringe;
    Color diff_hl_change;
    Color diff_hl_insert;
    Color diff_hl_bg;
    Color diff_hl_change_bg;
} Theme;

extern Theme themes[];
extern Theme currentTheme;
extern Theme previousTheme;
extern int currentThemeIndex;
extern int previousThemeIndex;
extern float interpolationProgress;
extern bool theme_lerp;
extern float theme_lerp_speed;
extern float theme_lerp_threshold;

#define CT (currentTheme)

Color hexToColor(const char *hexStr);
void initThemes();
void switchToNextTheme();
void switchToPreviousTheme();
void load_theme(const char *themeName);
Color lerpColor(Color a, Color b, float t);
void updateThemeInterpolation();
void switchToTheme(int newIndex);

// Colors utility

Color blendColors(Color color1, Color color2, float alpha);

#endif // THEME_H
