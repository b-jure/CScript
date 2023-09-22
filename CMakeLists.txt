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
# Debug compile flags
set(SAN_FLAGS -fsanitize=address -fsanitize=undefined)
set(PF_FLAGS -pg)
set(DC_FLAGS ${CFLAGS} ${SAN_FLAGS} ${PF_FLAGS} -DDEBUG -O0 -g -fno-omit-frame-pointer)
# Debug link flags
set(DL_FLAGS ${SAN_FLAGS} ${PF_FLAGS})


# RELEASE
# Release compile flags
set(RC_FLAGS ${CFLAGS} -O3)
# Release link flags
set(RL_FLAGS)



# BUILD
set(BIN skooma)
add_executable(${BIN} ${SRCFILES})
target_include_directories(${BIN} PRIVATE ${SRCDIR})

target_compile_options(
    ${BIN}
    PRIVATE
    $<$<CONFIG:Release>:${RC_FLAGS}>
    $<$<CONFIG:Debug>:${DC_FLAGS}>
)

target_link_options(
    ${BIN}
    PRIVATE
    $<$<CONFIG:Release>:${RL_FLAGS}>
    $<$<CONFIG:Debug>:${DL_FLAGS}>
)

# xxHash dependency
option(BUILD_SHARED_LIBS "Build shared libs" OFF)
set(XXHASH_BUILD_ENABLE_INLINE_API OFF)
set(XXHASH_BUILD_XXHSUM OFF)
add_subdirectory(${DEPDIR}/xxHash/cmake_unofficial ${DEPDIR}/xxHash/build EXCLUDE_FROM_ALL)


# Link binary
target_link_libraries(${BIN} PRIVATE xxHash::xxhash m)



# Custom targets
#
add_custom_target(debug_config
    COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Debug ${CMAKE_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Building Debug configuration"
)

add_custom_target(release_config
    COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Release ${CMAKE_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Building Release configuration"
) 

add_custom_target(debug 
    DEPENDS debug_config
    COMMAND make ${BIN}
)

add_custom_target(release 
    DEPENDS release_config
    COMMAND make ${BIN}
)
