#pragma once

#if __cplusplus == 199711L
#error "pmr requires at least C++ 11"
#endif

#ifndef PMR_STD_VER
#if __cplusplus == 201103L
#define PMR_STD_VER 11
#elif __cplusplus == 201402L
#define PMR_STD_VER 14
#elif __cplusplus == 201703L
#define PMR_STD_VER 17
#elif __cplusplus == 202002L
#define PMR_STD_VER 20
#elif __cplusplus == 202302L
#define PMR_STD_VER 23
#elif
#define PMR_STD_VER 17
#endif
#endif
