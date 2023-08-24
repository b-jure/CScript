cmake_minimum_required(VERSION 3.27)

project(skooma)

set(CMAKE_C_STANDARD 11)
set(BIN skooma)
set(SRCDIR ${CMAKE_SOURCE_DIR}/src)
set(CFLAGS -Wall -Werror -Wextra -Wpedantic -save-temps)
set(OPTS -Os)
set(SANITIZER_FLAGS -fsanitize=address -fsanitize=undefined)
set(SRCFILES
    ${SRCDIR}/mem.c
    ${SRCDIR}/main.c
    ${SRCDIR}/debug.c
    ${SRCDIR}/chunk.c
    ${SRCDIR}/value.c
    ${SRCDIR}/array.c
    ${SRCDIR}/vmachine.c
    ${SRCDIR}/compiler.c
    ${SRCDIR}/scanner.c
)

add_executable(${BIN} ${SRCFILES})
target_include_directories(${BIN} PRIVATE ${SRCDIR})
target_compile_options(${BIN} PRIVATE ${CLFAGS} ${SANITIZER_FLAGS} -g)
target_link_options(${BIN} PRIVATE ${SANITIZER_FLAGS})

