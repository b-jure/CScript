cmake_minimum_required(VERSION 3.27)

project(skooma)

set(CMAKE_C_STANDARD 11)
set(BIN skooma)
set(SRCDIR ${CMAKE_SOURCE_DIR}/src)
set(CFLAGS -Wall -Werror -Wextra -Wpedantic)
set(OPTS -Os)
set(SRCFILES
    ${SRCDIR}/mem.c
    ${SRCDIR}/main.c
    ${SRCDIR}/debug.c
    ${SRCDIR}/chunk.c
    ${SRCDIR}/value.c
    ${SRCDIR}/array.c
)

add_executable(${BIN} ${SRCFILES})
target_include_directories(${BIN} PRIVATE ${SRCDIR})
target_compile_options(${BIN} PRIVATE ${CFLAGS} ${OPTS})
