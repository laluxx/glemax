#include <stdio.h>
#include <string.h>
#include "theme.h"

// TODO Theme lerping

int currentThemeIndex = 0;
Theme themes[8];

Color hexToColor(const char* hex) {
    int r, g, b;
    sscanf(hex, "#%02x%02x%02x", &r, &g, &b);
    return (Color){r / 255.0f, g / 255.0f, b / 255.0f, 1.0f}; // NOTE alpha is 1.0f
}

void initThemes() {
    themes[0] = (Theme){
        .name = "dark",
        .bg = hexToColor("#18181B"),
        .cursor = hexToColor("#e4e4e8"),
        .text = hexToColor("#e4e4e8"),
        .minibuffer = hexToColor("#18181B"),
        .modeline = hexToColor("#222225"),
        .show_paren_match = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#4d9391"),
        .message = hexToColor("#4d9391"),
        .region = hexToColor("#2E403B"),
        .type = hexToColor("#cd9575"),
        .string = hexToColor("#6FB593"),
        .number = hexToColor("#e4e4e8"),
        .function = hexToColor("#80bcb6"),
        .preprocessor = hexToColor("#9d81ba"),
        .operator = hexToColor("#e4e4e8"),
        .variable = hexToColor("#968cc7"),
        .keyword = hexToColor("#4d9391"),
        .comment = hexToColor("#545c5e"),
        .null = hexToColor("#ab98b5"),
        .negation = hexToColor("#cd5c60"),
    };

    themes[1] = (Theme){
        .name = "Gum",
        .bg = hexToColor("#14171E"),
        .cursor = hexToColor("#D6A0D1"),
        .text = hexToColor("#D4D4D6"),
        .minibuffer = hexToColor("#14171E"),
        .modeline = hexToColor("#191D26"),
        .show_paren_match = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#272C3A"),
        .minibuffer_prompt = hexToColor("#9587DD"),
        .region = hexToColor("#272C3A"),
        .message = hexToColor("#4d9391"),
        .type = hexToColor("#cd9575"),
        .string = hexToColor("#6FB593"),
        .number = hexToColor("#e4e4e8"),
        .function = hexToColor("#80bcb6"),
        .preprocessor = hexToColor("#9d81ba"),
        .operator = hexToColor("#e4e4e8"),
        .variable = hexToColor("#968cc7"),
        .keyword = hexToColor("#4d9391"),
    };

    themes[2] = (Theme){
        .name = "Ocean",
        .bg = hexToColor("#1A1A25"),
        .cursor = hexToColor("#F2F2F2"),
        .text = hexToColor("#E6E6E8"),
        .minibuffer = hexToColor("#1A1A25"),
        .modeline = hexToColor("#252534"),
        .show_paren_match = hexToColor("#252534"),
        .isearch_highlight = hexToColor("#32324A"),
        .minibuffer_prompt = hexToColor("#738FD7"),
        .region = hexToColor("#2E403B"),
        .message = hexToColor("#4d9391"),
        .type = hexToColor("#cd9575"),
        .string = hexToColor("#6FB593"),
        .number = hexToColor("#e4e4e8"),
        .function = hexToColor("#80bcb6"),
        .preprocessor = hexToColor("#9d81ba"),
        .operator = hexToColor("#e4e4e8"),
        .variable = hexToColor("#968cc7"),
        .keyword = hexToColor("#4d9391"),
    };

    themes[3] = (Theme){
        .name = "Catppuccin",
        .bg = hexToColor("#1E1E2E"),
        .cursor = hexToColor("#B4BEFE"),
        .text = hexToColor("#CDD6F4"),
        .minibuffer = hexToColor("#1E1E2E"),
        .modeline = hexToColor("#181825"),
        .show_paren_match = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#4d9391"),
        .region = hexToColor("#2E403B"),
        .message = hexToColor("#4d9391"),
        .type = hexToColor("#cd9575"),
        .string = hexToColor("#6FB593"),
        .number = hexToColor("#e4e4e8"),
        .function = hexToColor("#80bcb6"),
        .preprocessor = hexToColor("#9d81ba"),
        .operator = hexToColor("#e4e4e8"),
        .variable = hexToColor("#968cc7"),
        .keyword = hexToColor("#4d9391"),
    };

    themes[4] = (Theme){
        .name = "Tokyonight",
        .bg = hexToColor("#1A1B26"),
        .cursor = hexToColor("#7AA2F7"),
        .text = hexToColor("#A9B1D6"),
        .minibuffer = hexToColor("#1A1B26"),
        .modeline = hexToColor("#161620"),
        .show_paren_match = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#4d9391"),
        .region = hexToColor("#2E403B"),
        .message = hexToColor("#4d9391"),
        .type = hexToColor("#cd9575"),
        .string = hexToColor("#6FB593"),
        .number = hexToColor("#e4e4e8"),
        .function = hexToColor("#80bcb6"),
        .preprocessor = hexToColor("#9d81ba"),
        .operator = hexToColor("#e4e4e8"),
        .variable = hexToColor("#968cc7"),
        .keyword = hexToColor("#4d9391"),
    };
  
    themes[5] = (Theme){
        .name = "Doom-one",
        .bg = hexToColor("#282C34"),
        .cursor = hexToColor("#51AFEF"),
        .text = hexToColor("#BBC2CF"),
        .minibuffer = hexToColor("#21242B"),
        .modeline = hexToColor("#1D2026"),
        .show_paren_match = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#4d9391"),
        .region = hexToColor("#2E403B"),
        .message = hexToColor("#4d9391"),
        .type = hexToColor("#cd9575"),
        .string = hexToColor("#6FB593"),
        .number = hexToColor("#e4e4e8"),
        .function = hexToColor("#80bcb6"),
        .preprocessor = hexToColor("#9d81ba"),
        .operator = hexToColor("#e4e4e8"),
        .variable = hexToColor("#968cc7"),
        .keyword = hexToColor("#4d9391"),
    };

    themes[6] = (Theme){
        .name = "City-lights",
        .bg = hexToColor("#1D252C"),
        .cursor = hexToColor("#51AFEF"),
        .text = hexToColor("#A0B3C5"),
        .minibuffer = hexToColor("#181E24"),
        .modeline = hexToColor("#181F25"),
        .show_paren_match = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#4d9391"),
        .region = hexToColor("#2E403B"),
        .message = hexToColor("#4d9391"),
        .type = hexToColor("#cd9575"),
        .string = hexToColor("#6FB593"),
        .number = hexToColor("#e4e4e8"),
        .function = hexToColor("#80bcb6"),
        .preprocessor = hexToColor("#9d81ba"),
        .operator = hexToColor("#e4e4e8"),
        .variable = hexToColor("#968cc7"),
        .keyword = hexToColor("#4d9391"),
    };

    themes[7] = (Theme){
        .name = "Molokai",
        .bg = hexToColor("#1C1E1F"),
        .cursor = hexToColor("#FB2874"),
        .text = hexToColor("#D6D6D4"),
        .minibuffer = hexToColor("#222323"),
        .modeline = hexToColor("#2D2E2E"),
        .show_paren_match = hexToColor("#222225"),
        .isearch_highlight = hexToColor("#303035"),
        .minibuffer_prompt = hexToColor("#4d9391"),
        .region = hexToColor("#2E403B"),
        .message = hexToColor("#4d9391"),
        .type = hexToColor("#cd9575"),
        .string = hexToColor("#6FB593"),
        .number = hexToColor("#e4e4e8"),
        .function = hexToColor("#80bcb6"),
        .preprocessor = hexToColor("#9d81ba"),
        .operator = hexToColor("#e4e4e8"),
        .variable = hexToColor("#968cc7"),
        .keyword = hexToColor("#4d9391"),
    };
}


void nextTheme() {
    currentThemeIndex++;
    if (currentThemeIndex >= sizeof(themes) / sizeof(Theme)) {
        currentThemeIndex = 0;
    }
    printf("Switched to next theme: %s\n", CT.name);
}

void previousTheme() {
    currentThemeIndex--;
    if (currentThemeIndex < 0) {
        currentThemeIndex = sizeof(themes) / sizeof(Theme) - 1;
    }
    printf("Switched to previous theme: %s\n", CT.name);
}


void loadTheme(const char* themeName) {
    for (int i = 0; i < sizeof(themes) / sizeof(Theme); i++) {
        if (strcmp(themes[i].name, themeName) == 0) {
            currentThemeIndex = i;
            return;
        }
    }
    // Handle the case where the theme is not found
    printf("Theme '%s' not found.\n", themeName);
}
