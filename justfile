rom_name := "snake.nes"

build:
    cl65 --target nes -C config.cfg -o {{rom_name}} src/main.s 

run: build
    fceux {{rom_name}}
