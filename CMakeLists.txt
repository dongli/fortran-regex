cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)

project(fortran-regex VERSION 0.1.0 LANGUAGES Fortran)

set(sources
  src/regex_state_mod.F90
  src/regex_parser_mod.F90
)

add_library(fortran_regex ${sources})

add_executable(regex_test.exe src/regex_test.F90)
target_link_libraries(regex_test.exe fortran_regex)