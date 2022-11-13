#pragma once

#ifdef FIBONACCI_EXPORTS
#define FIBONACCI_API __declspec(dllexport)
#else
#define FIBONACCI_API __declspec(dllimport)
#endif

extern "C" FIBONACCI_API void fibonacci_init(
    const unsigned long long a, const unsigned long long b);

extern "C" FIBONACCI_API bool fibonacci_next();

extern "C" FIBONACCI_API unsigned long long fibonacci_current();

extern "C" FIBONACCI_API unsigned fibonacci_index();
