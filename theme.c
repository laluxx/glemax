#include "theme.h"
#include "globals.h"
#include <stdio.h>
#include <string.h>

int currentThemeIndex = 0;
int previousThemeIndex = 0;
float interpolationProgress = 1.0f;
Theme themes[10];
Theme currentTheme;
Theme previousTheme;


Color hexToColor(const char *hex) {
    int r, g, b;
    sscanf(hex, "#%02x%02x%02x", &r, &g, &b);
    return (Color){r / 255.0f, g / 255.0f, b / 255.0f, 1.0f};
}

Color lerpColor(Color a, Color b, float t) {
    return (Color){a.r + (b.r - a.r) * t, a.g + (b.g - a.g) * t,
                   a.b + (b.b - a.b) * t, a.a + (b.a - a.a) * t};
}

float easeOutQuad(float t) {
    return t * (2 - t);
}

void updateThemeInterpolation() {
    if (theme_lerp_mode && interpolationProgress < theme_lerp_threshold) {
        interpolationProgress += theme_lerp_speed;
        Theme startTheme = previousTheme;
        Theme endTheme = themes[currentThemeIndex];

        // Interpolate each color component
        currentTheme.bg = lerpColor(startTheme.bg, endTheme.bg, interpolationProgress);
        currentTheme.cursor = lerpColor(startTheme.cursor, endTheme.cursor, interpolationProgress);
        currentTheme.marked_cursor = lerpColor(startTheme.marked_cursor, endTheme.marked_cursor, interpolationProgress);
        currentTheme.text = lerpColor(startTheme.text, endTheme.text, interpolationProgress);
        currentTheme.minibuffer = lerpColor(startTheme.minibuffer, endTheme.minibuffer, interpolationProgress);
        currentTheme.modeline = lerpColor(startTheme.modeline, endTheme.modeline, interpolationProgress);
        currentTheme.modeline_inactive = lerpColor(startTheme.modeline_inactive, endTheme.modeline_inactive, interpolationProgress);
        currentTheme.modeline_highlight = lerpColor(startTheme.modeline_highlight, endTheme.modeline_highlight, interpolationProgress);
        currentTheme.show_paren_match = lerpColor(startTheme.show_paren_match, endTheme.show_paren_match, interpolationProgress);
        currentTheme.isearch_highlight = lerpColor(startTheme.isearch_highlight, endTheme.isearch_highlight, interpolationProgress);
        currentTheme.minibuffer_prompt = lerpColor(startTheme.minibuffer_prompt, endTheme.minibuffer_prompt, interpolationProgress);
        currentTheme.region = lerpColor(startTheme.region, endTheme.region, interpolationProgress);
        currentTheme.message = lerpColor(startTheme.message, endTheme.message, interpolationProgress);
        currentTheme.type = lerpColor(startTheme.type, endTheme.type, interpolationProgress);
        currentTheme.string = lerpColor(startTheme.string, endTheme.string, interpolationProgress);
        currentTheme.number = lerpColor(startTheme.number, endTheme.number, interpolationProgress);
        currentTheme.function = lerpColor(startTheme.function, endTheme.function, interpolationProgress);
        currentTheme.preprocessor = lerpColor(startTheme.preprocessor, endTheme.preprocessor, interpolationProgress);
        currentTheme.operator = lerpColor(startTheme.operator, endTheme.operator, interpolationProgress);
        currentTheme.variable = lerpColor(startTheme.variable, endTheme.variable, interpolationProgress);
        currentTheme.keyword = lerpColor(startTheme.keyword, endTheme.keyword, interpolationProgress);
        currentTheme.comment = lerpColor(startTheme.comment, endTheme.comment, interpolationProgress);
        currentTheme.null = lerpColor(startTheme.null, endTheme.null, interpolationProgress);
        currentTheme.negation = lerpColor(startTheme.negation, endTheme.negation, interpolationProgress);

        if (interpolationProgress >= 1.0f) {
            interpolationProgress = 1.0f;
        }
    } else if (!theme_lerp_mode) {
        currentTheme = themes[currentThemeIndex];
        interpolationProgress = 1.0f;
    }
}

void switchToTheme(int newIndex) {
    if (newIndex < 0 || newIndex >= sizeof(themes) / sizeof(Theme)) {
        return;
    }

    previousTheme = currentTheme;
    previousThemeIndex = currentThemeIndex;
    currentThemeIndex = newIndex;
    interpolationProgress = 0.0f;

    if (!theme_lerp_mode) {
        currentTheme = themes[currentThemeIndex];
    }
}

void switchToNextTheme() {
    int nextIndex = (currentThemeIndex + 1) % (sizeof(themes) / sizeof(Theme));
    switchToTheme(nextIndex);
    printf("Transitioning to next theme: %s\n", themes[nextIndex].name);
}

void switchToPreviousTheme() {
    int prevIndex = (currentThemeIndex - 1 + sizeof(themes) / sizeof(Theme)) %
        (sizeof(themes) / sizeof(Theme));
    switchToTheme(prevIndex);
    printf("Transitioning to previous theme: %s\n", themes[prevIndex].name);
}

void load_theme(const char *themeName) {
    for (int i = 0; i < sizeof(themes) / sizeof(Theme); i++) {
        if (strcmp(themes[i].name, themeName) == 0) {
            switchToTheme(i);
            return;
        }
    }
    printf("Theme '%s' not found.\n", themeName);
}

void initThemes() {
    themes[0] = (Theme){
        .name              = "dark",
        .bg                = hexToColor("#18181B"),
        .cursor            = hexToColor("#e4e4e8"),
        .marked_cursor     = hexToColor("#e4e4e8"),
        .text              = hexToColor("#e4e4e8"),
        .minibuffer        = hexToColor("#18181B"),
        .modeline          = hexToColor("#222225"),
        .modeline_inactive = hexToColor("#222225"),
        .show_paren_match  = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#4d9391"),
        .message           = hexToColor("#4d9391"),
        .region            = hexToColor("#2E403B"),
        .type              = hexToColor("#cd9575"),
        .string            = hexToColor("#6FB593"),
        .number            = hexToColor("#e4e4e8"),
        .function          = hexToColor("#80bcb6"),
        .preprocessor      = hexToColor("#9d81ba"),
        .operator          = hexToColor("#e4e4e8"),
        .variable          = hexToColor("#968cc7"),
        .keyword           = hexToColor("#4d9391"),
        .comment           = hexToColor("#545c5e"),
        .null              = hexToColor("#ab98b5"),
        .negation          = hexToColor("#cd5c60"),
    };
    themes[1] = (Theme){
        .name              = "Gum",
        .bg                = hexToColor("#14171E"),
        .cursor            = hexToColor("#D6A0D1"),
        .marked_cursor     = hexToColor("#9587DD"),
        .text              = hexToColor("#D4D4D6"),
        .minibuffer        = hexToColor("#14171E"),
        .modeline          = hexToColor("#191D26"),
        .modeline_inactive = hexToColor("#191D26"),
        .show_paren_match  = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#272C3A"),
        .minibuffer_prompt = hexToColor("#9587DD"),
        .region            = hexToColor("#14171e"),
        .message           = hexToColor("#9587DD"),
        .type              = hexToColor("#11ccb2"),
        .string            = hexToColor("#62D2DB"),
        .number            = hexToColor("#d4d4d6"),
        .function          = hexToColor("#D6A0D1"),
        .preprocessor      = hexToColor("#c79af4"),
        .operator          = hexToColor("#d4d4d6"),
        .variable          = hexToColor("#41b0f3"),
        .keyword           = hexToColor("#9587DD"),
        .comment           = hexToColor("#454459"),
        .null              = hexToColor("#41b0f3"),
        .negation          = hexToColor("#e55c7a"),
    };
    themes[2] = (Theme){
        .name              = "ocean",
        .bg                = hexToColor("#1A1A25"),
        .cursor            = hexToColor("#F2F2F2"),
        .marked_cursor     = hexToColor("#F2F2F2"),
        .text              = hexToColor("#E6E6E8"),
        .minibuffer        = hexToColor("#1A1A25"),
        .modeline          = hexToColor("#252534"),
        .modeline_inactive = hexToColor("#252534"),
        .show_paren_match  = hexToColor("#252534"),
        .isearch_highlight = hexToColor("#32324A"),
        .minibuffer_prompt = hexToColor("#738FD7"),
        .region            = hexToColor("#2E403B"),
        .message           = hexToColor("#738FD7"),
        .type              = hexToColor("#d24b83"),
        .string            = hexToColor("#7CF083"),
        .number            = hexToColor("#e6e6e8"),
        .function          = hexToColor("#6bd9db"),
        .preprocessor      = hexToColor("#9587DD"),
        .operator          = hexToColor("#e6e6e8"),
        .variable          = hexToColor("#c79af4"),
        .keyword           = hexToColor("#738FD7"),
        .comment           = hexToColor("#545c5e"),
        .null              = hexToColor("#cea2ca"),
        .negation          = hexToColor("#e84c58"),
    };
    themes[3] = (Theme){
        .name              = "temple",
        .bg                = hexToColor("#2B2B2F"),
        .cursor            = hexToColor("#EEDCC1"),
        .marked_cursor     = hexToColor("#EEDCC1"),
        .text              = hexToColor("#EEDCC1"),
        .minibuffer        = hexToColor("#2B2B2F"),
        .modeline          = hexToColor("#303035"),
        .modeline_inactive = hexToColor("#303035"),
        .show_paren_match  = hexToColor("#ef6787"),
        .isearch_highlight = hexToColor("#4e333b"),
        .minibuffer_prompt = hexToColor("#4EB8CA"),
        .region            = hexToColor("#402E33"),
        .message           = hexToColor("#4EB8CA"),
        .type              = hexToColor("#b9c791"),
        .string            = hexToColor("#fbaed2"),
        .number            = hexToColor("#EEDCC1"),
        .function          = hexToColor("#91b9c7"),
        .preprocessor      = hexToColor("#ef6787"),
        .operator          = hexToColor("#EEDCC1"),
        .variable          = hexToColor("#47ba99"),
        .keyword           = hexToColor("#4EB8CA"),
        .comment           = hexToColor("#697375"),
        .null              = hexToColor("#4FA8A3"),
        .negation          = hexToColor("#e84c58"),

    };
    themes[4] = (Theme){
        .name              = "dark+",
        .bg                = hexToColor("#1e1e1e"),
        .cursor            = hexToColor("#237AD3"),
        .marked_cursor     = hexToColor("#237AD3"),
        .text              = hexToColor("#d4d4d4"),
        .minibuffer        = hexToColor("#1e1e1e"),
        .modeline          = hexToColor("#68217A"),
        .modeline_inactive = hexToColor("#1d1d1d"),
        .show_paren_match  = hexToColor("#D16969"),
        .isearch_highlight = hexToColor("#4b474c"),
        .minibuffer_prompt = hexToColor("#237AD3"),
        .region            = hexToColor("#113d69"),
        .message           = hexToColor("#237AD3"),
        .type              = hexToColor("#35CDAF"),
        .string            = hexToColor("#DB8E73"),
        .number            = hexToColor("#d4d4d4"),
        .function          = hexToColor("#D9DAA2"),
        .preprocessor      = hexToColor("#85DDFF"),
        .operator          = hexToColor("#d4d4d4"),
        .variable          = hexToColor("#85DDFF"),
        .keyword           = hexToColor("#339CDB"),
        .comment           = hexToColor("#579C4C"),
        .null              = hexToColor("#339CDB"),
        .negation          = hexToColor("#D16969"),
    };
    themes[5] = (Theme){
        .name              = "doom-one",
        .bg                = hexToColor("#282C34"),
        .cursor            = hexToColor("#51AFEF"),
        .marked_cursor     = hexToColor("#51AFEF"),
        .text              = hexToColor("#BBC2CF"),
        .minibuffer        = hexToColor("#21242B"),
        .modeline          = hexToColor("#1D2026"),
        .modeline_inactive = hexToColor("#21242b"),
        .show_paren_match  = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#51afef"),
        .region            = hexToColor("#42444a"),
        .message           = hexToColor("#51afef"),
        .type              = hexToColor("#ECBE7B"),
        .string            = hexToColor("#98be65"),
        .number            = hexToColor("#bbc2cf"),
        .function          = hexToColor("#c678dd"),
        .preprocessor      = hexToColor("#51afef"),
        .operator          = hexToColor("#bbc2cf"),
        .variable          = hexToColor("#dcaeea"),
        .keyword           = hexToColor("#51afef"),
        .comment           = hexToColor("#5B6268"),
        .null              = hexToColor("#a9a1e1"),
        .negation          = hexToColor("#ff6c6b"),
    };
    themes[6] = (Theme){
        .name              = "city-lights",
        .bg                = hexToColor("#1D252C"),
        .cursor            = hexToColor("#51AFEF"),
        .marked_cursor     = hexToColor("#51AFEF"),
        .text              = hexToColor("#A0B3C5"),
        .minibuffer        = hexToColor("#181E24"),
        .modeline          = hexToColor("#181F25"),
        .modeline_inactive = hexToColor("#1D252C"),
        .show_paren_match  = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#5EC4FF"),
        .region            = hexToColor("#28323B"),
        .message           = hexToColor("#5EC4FF"),
        .type              = hexToColor("#EBBF83"),
        .string            = hexToColor("#539AFC"),
        .number            = hexToColor("#A0B3C5"),
        .function          = hexToColor("#33CED8"),
        .preprocessor      = hexToColor("#5EC4FF"),
        .operator          = hexToColor("#A0B3C5"),
        .variable          = hexToColor("#718CA1"),
        .keyword           = hexToColor("#5EC4FF"),
        .comment           = hexToColor("#41505E"),
        .null              = hexToColor("#E27E8D"),
        .negation          = hexToColor("#5EC4FF"),
    };
    themes[7] = (Theme){
        .name              = "Molokai",
        .bg                = hexToColor("#1C1E1F"),
        .cursor            = hexToColor("#FB2874"),
        .marked_cursor     = hexToColor("#FB2874"),
        .text              = hexToColor("#D6D6D4"),
        .minibuffer        = hexToColor("#222323"),
        .modeline          = hexToColor("#2D2E2E"),
        .modeline_inactive = hexToColor("#171819"),
        .show_paren_match  = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#fd971f"),
        .region            = hexToColor("#4e4e4e"),
        .message           = hexToColor("#fd971f"),
        .type              = hexToColor("#66d9ef"),
        .string            = hexToColor("#e2c770"),
        .number            = hexToColor("#d6d6d4"),
        .function          = hexToColor("#b6e63e"),
        .preprocessor      = hexToColor("#9c91e4"),
        .operator          = hexToColor("#d6d6d4"),
        .variable          = hexToColor("#fd971f"),
        .keyword           = hexToColor("#fb2874"),
        .comment           = hexToColor("#555556"),
        .null              = hexToColor("#fd971f"),
        .negation          = hexToColor("#9c91e4"),
    };
    themes[8] = (Theme){
        .name              = "doom-monokai-ristretto",
        .bg                = hexToColor("#2c2525"),
        .cursor            = hexToColor("#fff1f3"),
        .marked_cursor     = hexToColor("#fff1f3"),
        .text              = hexToColor("#fff1f3"),
        .minibuffer        = hexToColor("#2c2525"),
        .modeline          = hexToColor("#403838"),
        .modeline_inactive = hexToColor("#2c2525"),
        .show_paren_match  = hexToColor("#adda78"),
        .isearch_highlight = hexToColor("#403838"),
        .minibuffer_prompt = hexToColor("#f9cc6c"),
        .region            = hexToColor("#403838"),
        .message           = hexToColor("#f9cc6c"),
        .type              = hexToColor("#85dacc"),
        .string            = hexToColor("#f9cc6c"),
        .number            = hexToColor("#fff1f3"),
        .function          = hexToColor("#adda78"),
        .preprocessor      = hexToColor("#fd6883"),
        .operator          = hexToColor("#fff1f3"),
        .variable          = hexToColor("#fff1f3"),
        .keyword           = hexToColor("#85dacc"),
        .comment           = hexToColor("#72696a"),
        .null              = hexToColor("#a8a9eb"),
        .negation          = hexToColor("#fd6883"),
    };
    themes[9] = (Theme){
        .name              = "doom-nord",
        .bg                = hexToColor("#2E3440"),
        .cursor            = hexToColor("#81A1C1"),
        .marked_cursor     = hexToColor("#81A1C1"),
        .text              = hexToColor("#ECEFF4"),
        .minibuffer        = hexToColor("#2E3440"),
        .modeline          = hexToColor("#292e39"),
        .modeline_inactive = hexToColor("#292e39"),
        .show_paren_match  = hexToColor("#8FBCBB"),
        .isearch_highlight = hexToColor("#5a7087"),
        .minibuffer_prompt = hexToColor("#81A1C1"),
        .region            = hexToColor("#434C5E"),
        .message           = hexToColor("#81A1C1"),
        .type              = hexToColor("#8FBCBB"),
        .string            = hexToColor("#A3BE8C"),
        .number            = hexToColor("#ECEFF4"),
        .function          = hexToColor("#88C0D0"),
        .preprocessor      = hexToColor("#81A1C1"),
        .operator          = hexToColor("#ECEFF4"),
        .variable          = hexToColor("#D8DEE9"),
        .keyword           = hexToColor("#81A1C1"),
        .comment           = hexToColor("#6f7787"),
        .null              = hexToColor("#8FBCBB"),
        .negation          = hexToColor("#BF616A"),
    };

    currentThemeIndex = 0;
    previousThemeIndex = 0;
    currentTheme = themes[currentThemeIndex];
    previousTheme = currentTheme;
    interpolationProgress = 1.0f;
}
