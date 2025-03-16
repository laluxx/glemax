// Clock
#include <renderer.h>
#include <stdbool.h>
#include "theme.h"
#include "globals.h"
#include "clock.h"

const bool digit_patterns[10][5][3] = {
    // 0
    {
        {1, 1, 1},
        {1, 0, 1},
        {1, 0, 1},
        {1, 0, 1},
        {1, 1, 1}
    },
    // 1
    {
        {0, 0, 1},
        {0, 0, 1},
        {0, 0, 1},
        {0, 0, 1},
        {0, 0, 1}
    },
    // 2
    {
        {1, 1, 1},
        {0, 0, 1},
        {1, 1, 1},
        {1, 0, 0},
        {1, 1, 1}
    },
    // 3
    {
        {1, 1, 1},
        {0, 0, 1},
        {1, 1, 1},
        {0, 0, 1},
        {1, 1, 1}
    },
    // 4
    {
        {1, 0, 1},
        {1, 0, 1},
        {1, 1, 1},
        {0, 0, 1},
        {0, 0, 1}
    },
    // 5
    {
        {1, 1, 1},
        {1, 0, 0},
        {1, 1, 1},
        {0, 0, 1},
        {1, 1, 1}
    },
    // 6
    {
        {1, 1, 1},
        {1, 0, 0},
        {1, 1, 1},
        {1, 0, 1},
        {1, 1, 1}
    },
    // 7
    {
        {1, 1, 1},
        {0, 0, 1},
        {0, 0, 1},
        {0, 0, 1},
        {0, 0, 1}
    },
    // 8
    {
        {1, 1, 1},
        {1, 0, 1},
        {1, 1, 1},
        {1, 0, 1},
        {1, 1, 1}
    },
    // 9
    {
        {1, 1, 1},
        {1, 0, 1},
        {1, 1, 1},
        {0, 0, 1},
        {1, 1, 1}
    }
};


void drawClockDigit(int digit, Vec2f position, float squareSize, Color color) {
    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < 3; ++x) {
            if (digit_patterns[digit][y][x]) {
                Vec2f segment_position = {
                    position.x + x * squareSize,
                    position.y + (4 - y) * squareSize  // Flipping the Y-coordinate
                };
                drawRectangle(segment_position, (Vec2f){squareSize, squareSize}, color);
            }
        }
    }
}

void drawClockColon(Vec2f position, float squareSize, Color color) {
    Vec2f top_dot_position = {position.x, position.y + 3.5 * squareSize};        // Flipped Y-coordinate
    Vec2f bottom_dot_position = {position.x, position.y + 0.5 * squareSize};     // Flipped Y-coordinate
    drawRectangle(top_dot_position, (Vec2f){squareSize, squareSize}, color);     // Top dot
    drawRectangle(bottom_dot_position, (Vec2f){squareSize, squareSize}, color);  // Bottom dot
}

// TODO draw it once a minute not 144 times a second
void drawClock(int hours, int minutes) {
    if (!clock_mode) return;

    useShader(clock_shader);

    Color currentClockColor = CT.clock;
    float digitWidth = 3 * clockScale; // Width of each digit, assuming 3 units wide
    float colonWidth = clockScale;     // Assuming colon is 1 unit wide
    float spacing = clockScale;        // Spacing between elements
    float blockMove = 2 * clockScale;  // Define the block move distance

    // Adjust starting positions based on the presence of "1"
    float hourAdjust = (hours % 10 == 1) ? blockMove : 0;      // Adjust if second hour digit is "1"
    float minuteAdjust = (minutes / 10 == 1) ? -blockMove : 0; // Adjust if first minute digit is "1"
    float secondMinuteAdjust = (minutes % 10 == 1 && minutes / 10 != 1)
        ? -blockMove : 0; // Adjust if second minute digit is "1" and first is not "1"

    // Calculate total width and starting X position to center the clock
    float totalWidth = (digitWidth * 4) + colonWidth + (spacing * 3) + hourAdjust + minuteAdjust;
    float startX = clockPosition.x - (totalWidth / 2.0);

    Vec2f firstHourDigitPos = (Vec2f){startX + hourAdjust, clockPosition.y}; // Move first hour digit if needed
    Vec2f secondHourDigitPos = (Vec2f){startX + digitWidth + spacing + hourAdjust, clockPosition.y};
    Vec2f colonPosition = (Vec2f){startX + (digitWidth * 2) + (spacing * 2) + hourAdjust, clockPosition.y};
    Vec2f firstMinuteDigitPos = (Vec2f){colonPosition.x + colonWidth + spacing + minuteAdjust, clockPosition.y};
    Vec2f secondMinuteDigitPos = (Vec2f){firstMinuteDigitPos.x + digitWidth + spacing + secondMinuteAdjust, clockPosition.y};

    // Render the digits and colon based on adjusted positions
    drawClockDigit(hours / 10, firstHourDigitPos, clockScale, currentClockColor);
    drawClockDigit(hours % 10, secondHourDigitPos, clockScale, currentClockColor);
    drawClockColon(colonPosition, clockScale, currentClockColor);
    drawClockDigit(minutes / 10, firstMinuteDigitPos, clockScale, currentClockColor);
    drawClockDigit(minutes % 10, secondMinuteDigitPos, clockScale, currentClockColor);

    flush();
}


