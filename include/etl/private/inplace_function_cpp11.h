///\file

/******************************************************************************
The MIT License(MIT)

Embedded Template Library.
https://github.com/ETLCPP/etl
https://www.etlcpp.com

Copyright(c) 2025 BMW AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files(the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions :

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR rhs
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR rhsWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR rhs DEALINGS IN THE
SOFTWARE.
******************************************************************************/

#ifndef ETL_INPLACE_FUNCTION_CPP11_INCLUDED
#define ETL_INPLACE_FUNCTION_CPP11_INCLUDED

#include "../error_handler.h"
#include "../exception.h"
#include "../platform.h"
#include "../type_traits.h"
#include "../utility.h"

#ifndef ETL_INPLACE_FUNCTION_DEFAULT_CAPACITY
  #define ETL_INPLACE_FUNCTION_DEFAULT_CAPACITY 32
#endif
namespace etl
{
  namespace private_inplace_function
  {
    template <typename T>
    struct wrapper
    {
      using type = T;
    };
    template <typename R, typename... Args>
    struct vtable_t ETL_FINAL
    {
      using storage_ptr_t = char*;
      using invoke_func_t = R (*)(storage_ptr_t, Args&&...);
      using copy_or_move_func_t = void (*)(storage_ptr_t, storage_ptr_t);
      using dtor_func_t = void (*)(storage_ptr_t);

      const invoke_func_t       invoke_func{ETL_NULLPTR};
      const copy_or_move_func_t copy_func{[](storage_ptr_t, storage_ptr_t) -> void {}};
      const copy_or_move_func_t move_func{[](storage_ptr_t, storage_ptr_t) -> void {}};
      const dtor_func_t         dtor_func{[](storage_ptr_t) -> void {}};

      ~vtable_t() = default;

      ETL_CONSTEXPR vtable_t() ETL_NOEXCEPT = default;

      template <typename T>
      explicit ETL_CONSTEXPR vtable_t(wrapper<T>) ETL_NOEXCEPT
        : invoke_func{[](storage_ptr_t storage_ptr, Args&&... args) -> R
                      { return (*reinterpret_cast<T*>(storage_ptr))(
                          etl::forward<Args&&>(args)...); }},
          copy_func{[](storage_ptr_t dst_ptr, storage_ptr_t src_ptr) -> void
                    { ::new (dst_ptr) T{*reinterpret_cast<T*>(src_ptr)}; }},
          move_func{[](storage_ptr_t dst_ptr, storage_ptr_t src_ptr) -> void
                    {
                      ::new (dst_ptr) T{etl::move(*reinterpret_cast<T*>(src_ptr))};
                      reinterpret_cast<T*>(src_ptr)->~T();
                    }},
          dtor_func{[](storage_ptr_t src_ptr) -> void
                    { reinterpret_cast<T*>(src_ptr)->~T(); }}
      {
      }

    private:
      vtable_t(const vtable_t&) ETL_DELETE;
      vtable_t(vtable_t&&) ETL_DELETE;

      vtable_t& operator=(const vtable_t&) ETL_DELETE;
      vtable_t& operator=(vtable_t&&) ETL_DELETE;
    };

  }  // namespace private_inplace_function

  template <
    typename Signature,
    size_t Capacity = ETL_INPLACE_FUNCTION_DEFAULT_CAPACITY>
  class inplace_function;

  namespace private_inplace_function
  {
    template <typename>
    struct is_inplace_function : etl::false_type
    {
    };
    template <typename Sig, size_t Cap>
    struct is_inplace_function<inplace_function<Sig, Cap>> : etl::true_type
    {
    };
  }  // namespace private_inplace_function

  class inplace_function_exception : public etl::exception
  {
  public:
    inplace_function_exception(string_type reason_, string_type file_name_, numeric_type line_number_)
      : exception(reason_, file_name_, line_number_)
    {
    }
  };

  class bad_inplace_function_call : public inplace_function_exception
  {
  public:
    bad_inplace_function_call(string_type file_name_, numeric_type line_number_)
      : inplace_function_exception(ETL_ERROR_TEXT("inplace_function:call", ETL_INPLACE_FUNCTION_FILE_ID "A"), file_name_, line_number_)
    {
    }
  };

  template <
    typename R,
    typename... Args,
    size_t Capacity>
  class inplace_function<R(Args...), Capacity> ETL_FINAL
  {
    using vtable_t = private_inplace_function::vtable_t<R, Args...>;
    using vtable_ptr_t = const vtable_t*;

    template <typename, size_t>
    friend class inplace_function;

  public:
    using capacity = etl::integral_constant<size_t, Capacity>;

    inplace_function() ETL_NOEXCEPT = default;

    template <
      typename T,
      typename C = etl::decay_t<T>,
      typename = etl::enable_if_t<
        !private_inplace_function::is_inplace_function<C>::value && etl::is_invocable_r<R, C&, Args...>::value>>
    inplace_function(T&& closure)
    {
      ETL_STATIC_ASSERT(etl::is_copy_constructible<C>::value,
                        "cannot be constructed from non-copyable types");

      ETL_STATIC_ASSERT(sizeof(C) <= Capacity,
                        "internal storage too small");

      static const vtable_t vt{private_inplace_function::wrapper<C>{}};

      vtable_ptr = &vt;
      ::new (storage) C{etl::forward<T>(closure)};
    }

    template <size_t Cap>
    inplace_function(const inplace_function<R(Args...), Cap>& rhs)
      : inplace_function(rhs.vtable_ptr, rhs.vtable_ptr->copy_func, rhs.storage)
    {
      ETL_STATIC_ASSERT(Capacity >= Cap,
                        "internal storage too small");
    }

    template <size_t Cap>
    inplace_function(inplace_function<R(Args...), Cap>&& rhs) ETL_NOEXCEPT
      : inplace_function(rhs.vtable_ptr, rhs.vtable_ptr->move_func, rhs.storage)
    {
      ETL_STATIC_ASSERT(Capacity >= Cap,
                        "internal storage too small");
      rhs.vtable_ptr = &default_vtable;
    }

    inplace_function(const inplace_function& rhs)
      : vtable_ptr{rhs.vtable_ptr}
    {
      vtable_ptr->copy_func(
        storage,
        rhs.storage);
    }

    inplace_function(inplace_function&& rhs) ETL_NOEXCEPT
      : vtable_ptr{rhs.vtable_ptr}
    {
      rhs.vtable_ptr = &default_vtable;
      vtable_ptr->move_func(
        storage,
        rhs.storage);
    }

    inplace_function& operator=(etl::nullptr_t) ETL_NOEXCEPT
    {
      vtable_ptr->dtor_func(&storage);
      vtable_ptr = ETL_NULLPTR;
      return *this;
    }

    inplace_function& operator=(inplace_function rhs) ETL_NOEXCEPT
    {
      vtable_ptr->dtor_func(storage);
      vtable_ptr = rhs.vtable_ptr;
      rhs.vtable_ptr = &default_vtable;
      vtable_ptr->move_func(
        storage,
        rhs.storage);
      return *this;
    }

    ~inplace_function()
    {
      vtable_ptr->dtor_func(storage);
    }

    R operator()(Args... args) const
    {
      ETL_ASSERT(vtable_ptr->invoke_func, ETL_ERROR(bad_inplace_function_call));
      return vtable_ptr->invoke_func(
        storage,
        etl::forward<Args>(args)...);
    }

    explicit ETL_CONSTEXPR operator bool() const ETL_NOEXCEPT
    {
      return vtable_ptr != &default_vtable;
    }

  private:
    vtable_ptr_t vtable_ptr{&default_vtable};
    mutable char storage[Capacity];

    explicit inplace_function(
      vtable_ptr_t                           vtable,
      typename vtable_t::copy_or_move_func_t copy_or_move_func,
      typename vtable_t::storage_ptr_t       storage_ptr)
      : vtable_ptr{vtable}
    {
      copy_or_move_func(storage, storage_ptr);
    }

    static const vtable_t default_vtable;
  };

  template <
    typename R,
    typename... Args,
    size_t Capacity>
  const typename inplace_function<R(Args...), Capacity>::vtable_t
    inplace_function<R(Args...), Capacity>::default_vtable{};
}  // namespace etl

#endif
