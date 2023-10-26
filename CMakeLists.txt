cmake_minimum_required(VERSION 3.27)

project(skooma)

# C11 standard
set(CMAKE_C_STANDARD 11)

# Source files directory
set(SRCDIR ${CMAKE_SOURCE_DIR}/src)

# Depencencies directory
set(DEPDIR ${CMAKE_SOURCE_DIR}/dependencies)

# Source files
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

# Common flags
set(CFLAGS -Wall -Werror -Wextra)


# DEBUG
# ASAN
set(ASAN_FLAGS -fsanitize=address -fsanitize=undefined)
# compile flags
set(D_CFLAGS ${CFLAGS} ${ASAN_FLAGS} -DDEBUG -O0 -g -fno-omit-frame-pointer)
# link flags
set(D_LFLAGS ${ASAN_FLAGS})


# RELEASE
# compile flags
set(R_CFLAGS ${CFLAGS} -O3 -flto=auto)
# link flags
set(R_LFLAGS)


# PROFILING
# compile flags
set(P_CFLAGS ${CFLAGS} ${R_CFLAGS} -pg -g)
# link flags
set(P_LFLAGS ${R_LFLAGS} -pg)




# BUILD
set(BIN skooma)
add_executable(${BIN} ${SRCFILES})
target_include_directories(${BIN} PRIVATE ${SRCDIR})

target_compile_options(
    ${BIN}
    PRIVATE
    $<$<CONFIG:Release>:${R_CFLAGS}>
    $<$<CONFIG:Debug>:${D_CFLAGS}>
    $<$<CONFIG:Profiling>:${P_CFLAGS}>
)

target_link_options(
    ${BIN}
    PRIVATE
    $<$<CONFIG:Release>:${R_LFLAGS}>
    $<$<CONFIG:Debug>:${D_LFLAGS}>
    $<$<CONFIG:Profiling>:${P_LFLAGS}>
)

# xxHash dependency
option(BUILD_SHARED_LIBS "Build shared libs" OFF)
set(XXHASH_BUILD_ENABLE_INLINE_API OFF)
set(XXHASH_BUILD_XXHSUM OFF)
add_subdirectory(${DEPDIR}/xxHash/cmake_unofficial ${DEPDIR}/xxHash/build EXCLUDE_FROM_ALL)


# Link binary
target_link_libraries(${BIN} PRIVATE xxHash::xxhash m)




# DEBUG configuration
add_custom_target(debug 
    DEPENDS debug_config
    COMMAND make ${BIN}
)
add_custom_target(debug_config
    COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Debug ${CMAKE_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Building Debug configuration"
)

# RELEASE configuration
add_custom_target(release 
    DEPENDS release_config
    COMMAND make ${BIN}
)
add_custom_target(release_config
    COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Release ${CMAKE_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Building Release configuration"
) 

# PROFILING configuration
add_custom_target(profiling_config
    COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Profiling ${CMAKE_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Building Profiling configuration"
)

add_custom_target(profiling
    DEPENDS profiling_config
    COMMAND make ${BIN}
)
