/**
 *
 * A lightweight scope guard.
 *
 * Typical usage:
 *
 *      void Foo()
 *      {
 *          HANDLE hEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);
 *          SCOPE_GUARD(CloseHandle(hEvent)); // expression guard
 *
 *          HANDLE hEvent2 = CreateEvent(nullptr, FALSE, FALSE, nullptr);
 *          bool fMustCloseEvent2 = true;
 *          SCOPE_GUARD // statement guard; can be used to implement dismissible guards
 *          (
 *              if (fMustCloseEvent2) 
 *              {
 *                  CloseHandle(hEvent2)
 *              }
 *              hEvent2 = nullptr;
 *          )
 *          // ... 
 *          fMustCloseEvent2 = false; // dismiss guard
 *      }
 *
 * The name of the guard object is automatically generated, and it is not meant to be accessed directly.
 *
 */

#pragma once
#include <type_traits>


template<class Func>
class CScopeGuard
{
    static_assert(
        std::is_nothrow_copy_constructible<Func>::value,
        "Function object must guarantee no-throw-on-copy to be used with CScopeGuard - use [&] for lambdas, "
        "or throw() or noexcept on copy constructor for handwritten function objects");
public:
    CScopeGuard(Func func): m_func(func) {}
    ~CScopeGuard() { m_func(); }
private:
    Func m_func;
    CScopeGuard(const CScopeGuard& other); // = delete
    void operator=(const CScopeGuard& other); // = delete
};


// There _has_ to be a double layer of indirection for macros arguments here so that
// __LINE__ fully expands before we use it with ##.
#define SCOPE_GUARD_IMPL(line, body) \
    auto _scopeGuardFunc##line = [&]() -> void { body; }; \
    CScopeGuard<decltype(_scopeGuardFunc##line)> _scopeGuard##line(_scopeGuardFunc##line);
#define SCOPE_GUARD_IMPL2(line, body) SCOPE_GUARD_IMPL(line, body)
#define SCOPE_GUARD(body) SCOPE_GUARD_IMPL2(__LINE__, body)

// SCOPE_RESTORE uses SCOPE_GUARD to save the current value of a variable and restore it
// when the scope ends.
#define SCOPE_RESTORE_IMPL(line, var) \
    auto _scopeRestoreValue##line = var; \
    SCOPE_GUARD(var = _scopeRestoreValue##line);
#define SCOPE_RESTORE_IMPL2(line, var) SCOPE_RESTORE_IMPL(line, var)
#define SCOPE_RESTORE(var) SCOPE_RESTORE_IMPL2(__LINE__, var)

// SCOPE_SET_RESTORE uses SCOPE_RESTORE/SCOPE_GUARD to save the current value of a variable, set it to a new value and restore it
// when the scope ends.
#define SCOPE_SET_RESTORE(var, value) \
    SCOPE_RESTORE(var); \
    var = value;
