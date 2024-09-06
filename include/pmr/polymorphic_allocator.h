#pragma once

#include "detail/config.h"
#include "memory_resource.h"
#include <cassert>
#include <cstddef>
#include <type_traits>
#include <tuple>
#include <limits>

namespace pmr
{
    //! Adapts the pmr::memory_resource interface to the Allocator concept
    #if PMR_STD_VER >= 20
    #if defined(__cpp_lib_byte) && __cpp_lib_byte >= 201603L
    template <typename T = std::byte>
    #else
    template <typename T = unsigned char>
    #endif
    #else
    template <typename T>
    #endif
    class polymorphic_allocator
    {
      public:
        using value_type = T;

#       ifndef DOCS
        // these shouldn't be necessary but the stdlib I'm using to test right
        // now doesn't use std::allocator_traits in std::list

        using pointer = T*;
        using const_pointer = const T*;
        using reference = T&;
        using const_reference = const T&;

        template<typename U>
        struct rebind { using other = polymorphic_allocator<U>; };

#       endif

        //! Instantiate a polymorphic_allocator using pmr::get_default_resource()
        polymorphic_allocator() noexcept;

        //! Instantiate a polymorphic_allocator that will use the same underlying
        //! memory_resource as the supplied argument
        //!
        //! \param other a polymorphic_allocator from which to lookup the
        //!              underlying memory_resource
        polymorphic_allocator(const polymorphic_allocator& other) = default;

        //! Instantiate a polymorphic_allocator that will use the supplied
        //! pmr::memory_resource. This constructor is intentionally _not_
        //! expicit as it is intended to provide a clean conversion from a
        //! pmr::memory_resource pointer.
        //!
        //! \param r The wrapped memory_resource
        polymorphic_allocator(memory_resource* r) noexcept;

        //! Instantiate a polymorphic_allocator that will use the same underlying
        //! memory_resource as the supplied argument
        //!
        //! \param other a polymorphic_allocator from which to lookup the
        //!              underlying memory_resource
        template <typename U>
        polymorphic_allocator(const polymorphic_allocator<U>& other) noexcept;

        polymorphic_allocator& operator=(
                const polymorphic_allocator& other) = default;

        //! Allocates enough _uninitialized_ memory for n copies of T
        //! with alignof(T)
        //!
        //! \param n The number of T instances for which the caller needs space
        //! \return a pointer to the beginning of the allocated block
        pointer allocate(std::size_t n);

        //! Deallocate memory block pointed at by pointer of size n * sizeof(T)
        //!
        //! \param ptr A pointer to the block to deallocate
        //! \param n the number of instances of T that comprise the block being
        //!          deallocated
        void deallocate(pointer ptr, std::size_t n);

        #if PMR_STD_VER >= 20
        //! Allocates nbytes bytes of storage at specified alignment alignment
        //! using the underlying memory resource.
        //!
        //! \param nbytes The number of bytes to allocate
        //! \param alignment The required alignment
        //! \return a pointer to the allocated memory
        [[nodiscard]]
        void* allocate_bytes(std::size_t nbytes, std::size_t alignment = alignof(std::max_align_t));

        //! Deallocates the storage pointed to by p.
        //
        //! \param p A pointer to the block to deallocate
        //! \param nbytes The number of bytes to deallocate
        //! \param alignment The alignment of the memory block
        void deallocate_bytes(void* p, std::size_t nbytes, std::size_t alignment = alignof(std::max_align_t));

        //! Allocates storage for n objects of type U using the underlying memory resource.
        //! If std::numeric_limits<std::size_t>::max() / sizeof(U) < n, throws
        //! std::bad_array_new_length.
        //
        //! \param n Number of objects to allocate
        //! \return Pointer to allocated objects
        template <typename U>
        [[nodiscard]]
        U* allocate_object(std::size_t n = 1);

        //! Deallocates the storage pointed to by p.
        //
        //! \param p A pointer to the block to deallocate
        //! \param n The number of objects to deallocate
        template <typename U>
        void deallocate_object(U* p, std::size_t n = 1);

        //! Allocates and constructs an object of type U.
        //!
        //! \param args the arguments to forward to the constructor of U
        //! \returns A pointer to the allocated and constructed object
        template <typename U, typename... Args>
        [[nodiscard]]
        U* new_object(Args&&... args);

        //! Destroys the object of type U and deallocates storage allocated for it.
        //!
        //! \param p pointer to the object to destroy and deallocate
        template <typename U>
        void delete_object(U* p);
        #endif

        //! Constructs an instance of U via allocator-aware construction rules
        //!
        //! \param ptr A pointer to a block of memory big enough to hold an
        //!            instance of U
        //! \param args The arguments to forward to U's constructor
        template <typename U, typename... Args>
        void construct(U* ptr, Args&&... args);

        //! Piecewise constructs a std::pair<T1, T2> from x and y using
        //! allocator-aware construction rules.
        //!
        //! \param ptr A pointer to a block of memory big enough to hold
        //!            the std::pair being constructed
        //! \param pc The piecewise construction tag
        //! \param x The args to pass to the constructor of T1
        //! \param y The args to pass to the constructor of T2
        template <typename T1, typename T2,
                  typename... Args1, typename... Args2>
        void construct(std::pair<T1, T2>* ptr, std::piecewise_construct_t pc,
                std::tuple<Args1...> x, std::tuple<Args2...> y );

        //! Equivalent to
        //! \code{.cpp}
        //! construct(p, std::piecewise_construct, std::tuple<>(), std::tuple<>())
        //! \endcode
        template<typename T1, typename T2>
        void construct(std::pair<T1, T2>* p);

        //! Equivalent to
        //! \code{.cpp}
        //! construct(p, std::piecewise_construct,
        //!     std::forward_as_tuple(std::forward<U>(x)),
        //!     std::forward_as_tuple(std::forward<V>(y)))
        //! \endcode
        template<typename T1, typename T2, typename U, typename V>
        void construct(std::pair<T1, T2>* p, U&& x, V&& y);

        //! Equivalent to
        //! \code{.cpp}
        //! construct(p, std::piecewise_construct,
        //!     std::forward_as_tuple(xy.first),
        //!     std::forward_as_tuple(xy.second))
        //! \endcode
        template<typename T1, typename T2, typename U, typename V>
        void construct(std::pair<T1, T2>* p, const std::pair<U, V>& xy);

        //! Equivalent to
        //! \code{.cpp}
        //! construct(p, std::piecewise_construct,
        //!     std::forward_as_tuple(std::forward<U>(xy.first)),
        //!     std::forward_as_tuple(std::forward<V>(xy.second)))
        //! \endcode
        template<typename T1, typename T2, typename U, typename V>
        void construct(std::pair<T1, T2>* p, std::pair<U, V>&& xy);

        template <typename U>
        void destroy(U* ptr);

        polymorphic_allocator select_on_container_copy_construction() const;

        //! Access the underlying memory_resource
        //! \return the memory_resource backing this allocator
        memory_resource* resource() const;

      private:
        struct unaware{}; // ctor doesn't take allocator args
        struct aware_tagged{}; // first 2 args of ctor are (std::allocator_arg_t, alloc,...)
        struct aware_trailing{}; // last arg of ctor is convertible from alloc

        template <typename U, typename A, typename... Args>
        struct construct_style
        {
            using type = typename std::conditional<
                std::uses_allocator<U, A>::value &&
                std::is_constructible<U, std::allocator_arg_t, A, Args...>::value,
                    aware_tagged,
                    typename std::conditional<
                        std::uses_allocator<U, A>::value &&
                        std::is_constructible<U, Args..., A>::value,
                            aware_trailing,
                            unaware
                    >::type
                >::type;
        };


        template <typename U, typename A, typename... Args>
        using construct_style_t = typename construct_style<U, A, Args...>::type;

        memory_resource* m_memory;

        template <typename U, typename... Args>
        void construct(unaware, U* ptr, Args&&... args);

        template <typename U, typename... Args>
        void construct(aware_trailing, U* ptr, Args&&... args);

        template <typename U, typename... Args>
        void construct(aware_tagged, U* ptr, Args&&... args);

        template <typename Tuple>
        Tuple&& construct_tuple(unaware, Tuple& t);

        template <typename... Args>
        auto construct_tuple(aware_trailing, std::tuple<Args...>& t) ->
            decltype(std::tuple_cat(std::move(t), std::make_tuple(m_memory)));

        template <typename... Args>
        auto construct_tuple(aware_tagged, std::tuple<Args...>& t) ->
            decltype(std::tuple_cat(
                std::make_tuple(std::allocator_arg, m_memory), std::move(t)));

        friend bool operator==(const polymorphic_allocator& lhs,
                const polymorphic_allocator& rhs)
        {
            return *lhs.m_memory == *rhs.m_memory;
        }


        friend bool operator!=(const polymorphic_allocator& lhs,
                const polymorphic_allocator& rhs)
        {
            return !(lhs == rhs);
        }
    };


    template <typename T>
    polymorphic_allocator<T>::polymorphic_allocator() noexcept
        : polymorphic_allocator(get_default_resource())
    {
    }


    template <typename T>
    template <typename U>
    polymorphic_allocator<T>::polymorphic_allocator(
            const polymorphic_allocator<U>& other) noexcept
        : m_memory{other.resource()}
    {
    }


    template <typename T>
    polymorphic_allocator<T>::polymorphic_allocator(
            memory_resource* mem) noexcept
        : m_memory{mem}
    {
        assert(mem);
    }


    template <typename T>
    T*
    polymorphic_allocator<T>::allocate(std::size_t n)
    {
        return static_cast<T*>(
                m_memory->allocate(n * sizeof(T), alignof(T)));
    }


    template <typename T>
    void
    polymorphic_allocator<T>::deallocate(T* ptr, std::size_t n)
    {
        m_memory->deallocate(ptr, n * sizeof(T), alignof(T));
    }

    #if PMR_STD_VER >= 20

    template <typename T>
    void* polymorphic_allocator<T>::allocate_bytes(std::size_t nbytes, std::size_t alignment)
    {
      return resource()->allocate(nbytes, alignment);
    }

    template <typename T>
    void polymorphic_allocator<T>::deallocate_bytes(void* p, std::size_t nbytes, std::size_t alignment)
    {
      resource()->deallocate(p, nbytes, alignment);
    }

    template <typename T>
    template <typename U>
    U* polymorphic_allocator<T>::allocate_object(std::size_t n)
    {
      if (std::numeric_limits<size_t>::max() / sizeof(U) < n)
        throw std::bad_array_new_length();
      return static_cast<U*>(allocate_bytes(n * sizeof(U), alignof(U)));
    }

    template <typename T>
    template <typename U>
    void polymorphic_allocator<T>::deallocate_object(U* p, std::size_t n)
    {
      deallocate_bytes(p, n * sizeof(U), alignof(U));
    }

    template <typename T>
    template <typename U, typename... Args>
    U* polymorphic_allocator<T>::new_object(Args&&... args)
    {
      U* p = allocate_object<U>();
      try
      {
        construct(p, std::forward<Args>(args)...);
      }
      catch (...)
      {
        deallocate_object(p);
        throw;
      }
      return p;
    }

    template <typename T>
    template <typename U>
    void polymorphic_allocator<T>::delete_object(U* p)
    {
      destroy(p);
      deallocate_object(p);
    }

    #endif

    template <typename T>
    template <typename U, typename... Args>
    void
    polymorphic_allocator<T>::construct(U* ptr, Args&&... args)
    {
        auto style = construct_style_t<U, memory_resource*, Args...>{};
        construct(style, ptr, std::forward<Args>(args)...);
    }


    template <typename T>
    template <typename T1, typename T2, typename... Args1, typename... Args2>
    void
    polymorphic_allocator<T>::construct(std::pair<T1, T2>* ptr,
            std::piecewise_construct_t, std::tuple<Args1...> x,
            std::tuple<Args2...> y)
    {
        auto x_style = construct_style_t<T1, memory_resource*, Args1...>{};
        auto y_style = construct_style_t<T2, memory_resource*, Args2...>{};

        ::new (ptr) std::pair<T1, T2>(std::piecewise_construct,
                construct_tuple(x_style, x), construct_tuple(y_style, y));
    }


    template <typename T>
    template<typename T1, typename T2>
    void polymorphic_allocator<T>::construct(std::pair<T1, T2>* p)
    {
        construct(p, std::piecewise_construct,
                std::tuple<>(), std::tuple<>());
    }


    template <typename T>
    template<typename T1, typename T2, typename U, typename V>
    void polymorphic_allocator<T>::construct(std::pair<T1, T2>* p, U&& x, V&& y)
    {
        construct(p, std::piecewise_construct,
                std::forward_as_tuple(std::forward<U>(x)),
                std::forward_as_tuple(std::forward<V>(y)));
    }


    template <typename T>
    template<typename T1, typename T2, typename U, typename V>
    void polymorphic_allocator<T>::construct(
            std::pair<T1, T2>* p, const std::pair<U, V>& xy)
    {
        construct(p, std::piecewise_construct,
                std::forward_as_tuple(xy.first),
                std::forward_as_tuple(xy.second));
    }


    template <typename T>
    template<typename T1, typename T2, typename U, typename V>
    void polymorphic_allocator<T>::construct(std::pair<T1, T2>* p, std::pair<U, V>&& xy )
    {
        construct(p, std::piecewise_construct,
                std::forward_as_tuple(std::forward<U>(xy.first)),
                std::forward_as_tuple(std::forward<V>(xy.second)));
    }


    template <typename T>
    template <typename U>
    void
    polymorphic_allocator<T>::destroy(U* ptr)
    {
        ptr->~U();
    }


    template <typename T>
    polymorphic_allocator<T>
    polymorphic_allocator<T>::select_on_container_copy_construction() const
    {
        return polymorphic_allocator();
    }


    template <typename T>
    memory_resource*
    polymorphic_allocator<T>::resource() const
    {
        return m_memory;
    }


    template <typename T>
    template <typename U, typename... Args>
    void
    polymorphic_allocator<T>::construct(unaware, U* ptr, Args&&... args)
    {
        ::new (ptr) U(std::forward<Args>(args)...);
    }


    template <typename T>
    template <typename U, typename... Args>
    void
    polymorphic_allocator<T>::construct(aware_trailing, U* ptr, Args&&... args)
    {
        ::new (ptr) U(std::forward<Args>(args)..., m_memory);
    }


    template <typename T>
    template <typename U, typename... Args>
    void
    polymorphic_allocator<T>::construct(aware_tagged, U* ptr, Args&&... args)
    {
        ::new (ptr) U(std::allocator_arg, m_memory, std::forward<Args>(args)...);
    }


    template <typename T>
    template <typename Tuple>
    Tuple&&
    polymorphic_allocator<T>::construct_tuple(unaware, Tuple& t)
    {
        return std::move(t);
    }


    template <typename T>
    template <typename... Args>
    auto
    polymorphic_allocator<T>::construct_tuple(
            aware_trailing, std::tuple<Args...>& t) ->
        decltype(std::tuple_cat(std::move(t), std::make_tuple(m_memory)))
    {
        return std::tuple_cat(std::move(t), std::make_tuple(m_memory));
    }


    template <typename T>
    template <typename... Args>
    auto
    polymorphic_allocator<T>::construct_tuple(
            aware_tagged, std::tuple<Args...>& t) ->
        decltype(std::tuple_cat(
                std::make_tuple(std::allocator_arg, m_memory), std::move(t)))
    {
        return std::tuple_cat(std::make_tuple(std::allocator_arg, m_memory),
                std::move(t));
    }

}
