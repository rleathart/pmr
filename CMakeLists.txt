cmake_minimum_required(VERSION 3.21)

set(PMR_VERSION_MAJOR 0)
set(PMR_VERSION_MINOR 1)
set(PMR_VERSION_PATCH 1)
set(PMR_VERSION ${PMR_VERSION_MAJOR}.${PMR_VERSION_MINOR}.${PMR_VERSION_PATCH})

set(PMR_STD_VER "" CACHE STRING "The C++ standard to provide pmr:: functionality from")

project(pmr VERSION ${PMR_VERSION} LANGUAGES CXX)

option(PMR_ENABLE_TESTS "Build tests along with the `all` target")

set(pmr_srcs
  src/memblocks.cpp
  src/memory_resource.cpp
  src/monotonic_buffer_resource.cpp)
add_library(pmr ${pmr_srcs})

target_include_directories(pmr PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
    PRIVATE src)
target_compile_features(pmr PUBLIC cxx_std_11)
set_target_properties(pmr PROPERTIES
    VERSION ${PMR_VERSION}
    SOVERSION ${PMR_VERSION_MAJOR})

if (NOT "${PMR_STD_VER}" STREQUAL "")
  target_compile_definitions(pmr PUBLIC PMR_STD_VER=${PMR_STD_VER})
endif()

if (MSVC)
  target_compile_options(pmr
      PRIVATE -W4
      PRIVATE -WX
  )
else()
  target_compile_options(pmr
      PRIVATE -Wall
      PRIVATE -Wpedantic
      PRIVATE -Wextra
      PRIVATE -Werror
  )
endif()

include(CTest)

if (PMR_ENABLE_TESTS)
  add_subdirectory(test)
else()
  add_subdirectory(test EXCLUDE_FROM_ALL)
endif()

include(FindDoxygen)
if(DOXYGEN_FOUND)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    add_custom_target(
        docs
        ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating doxygen project documentation in ${CMAKE_CURRENT_BINARY_DIR}." VERBATIM
    )
else()
    add_custom_target(docs COMMENT "Doxygen not found. Please install doxygen to use this target." VERBATIM)
endif()
