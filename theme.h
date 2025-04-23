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
    Color region_fg;
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
    Color diff_hl_change_cursor;
    Color diff_hl_insert_cursor;
    Color diff_hl_bg;
    Color diff_hl_change_bg;
    Color clock;
    Color header_line;
    Color rainbow_delimiters_base_face;
    Color rainbow_delimiters_depth_1_face;
    Color rainbow_delimiters_depth_2_face;
    Color rainbow_delimiters_depth_3_face;
    Color rainbow_delimiters_depth_4_face;
    Color rainbow_delimiters_depth_5_face;
    Color rainbow_delimiters_depth_6_face;

    Color diredfl_dir_heading;
    Color diredfl_dir_priv;
    Color diredfl_read_priv;
    Color diredfl_write_priv;
    Color diredfl_exec_priv;
    Color diredfl_no_priv;
    Color diredfl_number;
    Color diredfl_date_time;
    Color diredfl_dir_name;
    Color diredfl_file_suffix;
} Theme;

extern Theme themes[];
extern Theme currentTheme;
extern Theme previousTheme;
extern int   currentThemeIndex;
extern int   previousThemeIndex;
extern float interpolationProgress;
extern bool  theme_lerp;
extern float theme_lerp_speed;
extern float theme_lerp_threshold;

#define CT (currentTheme)

Color hexToColor(const char *hexStr);
void  initThemes();
void  switchToNextTheme();
void  switchToPreviousTheme();
void  load_theme(const char *themeName);
Color lerpColor(Color a, Color b, float t);
void  updateThemeInterpolation();
void  switchToTheme(int newIndex);
bool  colorsEqual(Color a, Color b);

// Color utilities
Color blendColors(Color color1, Color color2, float alpha);



#endif // THEME_H
