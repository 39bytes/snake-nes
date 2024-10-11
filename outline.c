#include <stdbool.h>
#include <stdint.h>

typedef enum {
    DIR_LEFT = 0,
    DIR_RIGHT = 1,
    DIR_UP = 2,
    DIR_DOWN = 3,
} Direction;

const uint8_t GRID_WIDTH = 16;
const uint8_t GRID_HEIGHT = 15;

static uint8_t snake_len;
static uint8_t snake_head_index;
static Direction snake_direction;
static uint8_t snake_x[256];
static uint8_t snake_y[256];

const uint8_t SNAKE_FRAMES_PER_MOVE = 15;
static uint8_t snake_tick_timer;

static uint8_t apple_x;
static uint8_t apple_y;

void handle_input();
void update_snake_position();
void add_segment();
void generate_new_apple();

void initialize()
{
    snake_len = 3;
    snake_head_index = 0;
    snake_direction = DIR_RIGHT;
    snake_tick_timer = SNAKE_FRAMES_PER_MOVE;
    generate_new_apple();

    snake_x[0] = 2;
    snake_y[0] = 8;

    snake_x[1] = 1;
    snake_y[1] = 8;

    snake_x[2] = 0;
    snake_y[2] = 8;
}

int main()
{

    // game loop
    while (true) {
        // handle input
        handle_input();
        update_snake_position();
    }

    // handle game over
    return 0;
}

void update_snake_position()
{
    uint8_t next_x = snake_x[snake_head_index];
    uint8_t next_y = snake_y[snake_head_index];

    switch (snake_direction) {
    case DIR_LEFT:
        next_x--;
        break;
    case DIR_RIGHT:
        next_x++;
        break;
    case DIR_UP:
        next_y--;
        break;
    case DIR_DOWN:
        next_y++;
        break;
    }

    snake_head_index--;
    snake_x[snake_head_index] = next_x;
    snake_y[snake_head_index] = next_y;

    // Update head tile
    //
    // If eating the apple, then increase size
    // We never overwrite the old segments anyway, just exclude them
    // so incrementing the size has the same effect as keeping the old tail
    if (next_x == apple_x && next_y == apple_y) {
        // Keeping the tail, so no need to remove the tail tile from the nametable
        snake_len++;
        generate_new_apple();
    } else {
        // Pop tail tile
    }
}
