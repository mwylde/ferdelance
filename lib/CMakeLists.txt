cmake_minimum_required(VERSION 3.16)
project("wrapper")

enable_testing()

add_library(unity STATIC unity/unity.c)
add_library(gc STATIC gc.c)
target_include_directories(unity PUBLIC unity)
target_include_directories(gc PUBLIC gc)

# test config
add_executable(gctest gctest.c)
target_link_libraries(gctest unity unity)
target_link_libraries(gctest gc gc)
add_test(gctest gctest)

# output
add_executable (wrapper wrapper.c gc.h gc.c)
