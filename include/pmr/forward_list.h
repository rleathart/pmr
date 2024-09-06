#pragma once

#include <forward_list>
#include "polymorphic_allocator.h"

namespace pmr
{
    template <typename T>
    using forward_list = std::forward_list<T, polymorphic_allocator<T>>;
}
