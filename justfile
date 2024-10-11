rom_name := "snake.nes"

build:
    cl65 src/main.s --verbose --target nes -C config.cfg -o {{rom_name}}

run: build
    fceux {{rom_name}}
