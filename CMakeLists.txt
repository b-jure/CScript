cmake_minimum_required(VERSION 3.27)

project(skooma)

set(CMAKE_C_STANDARD 11)
set(BIN skooma)
set(SRCDIR ${CMAKE_SOURCE_DIR}/src)
set(CFLAGS -Wall -Werror -Wextra -Wpedantic -save-temps)
set(OPTS -O0)
set(SANITIZER_FLAGS -fsanitize=address -fsanitize=undefined)
set(DEPDIR ${CMAKE_SOURCE_DIR}/dependencies)
set(SRCFILES
    ${SRCDIR}/mem.c
    ${SRCDIR}/main.c
    ${SRCDIR}/debug.c
    ${SRCDIR}/chunk.c
    ${SRCDIR}/value.c
    ${SRCDIR}/vmachine.c
    ${SRCDIR}/compiler.c
    ${SRCDIR}/scanner.c
    ${SRCDIR}/object.c
    ${SRCDIR}/hash.c
    ${SRCDIR}/hashtable.c
)

add_executable(${BIN} ${SRCFILES})
target_include_directories(${BIN} PRIVATE ${SRCDIR})
target_compile_options(${BIN} PRIVATE ${CLFAGS} ${SANITIZER_FLAGS} -g -fno-omit-frame-pointer)
target_link_options(${BIN} PRIVATE ${SANITIZER_FLAGS})

# xxHash dependency
option(BUILD_SHARED_LIBS "Build shared libs" OFF)
set(XXHASH_BUILD_ENABLE_INLINE_API OFF)
set(XXHASH_BUILD_XXHSUM OFF)
add_subdirectory(${DEPDIR}/xxHash/cmake_unofficial ${DEPDIR}/xxHash/build EXCLUDE_FROM_ALL)
target_link_libraries(${BIN} PRIVATE xxHash::xxhash) # Link to xxhash
