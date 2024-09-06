#pragma once

#include <unordered_set>
#include "polymorphic_allocator.h"

namespace pmr
{
    template <typename K,
              typename Hash = std::hash<K>,
              typename Pred = std::equal_to<K>>
    using unordered_set =
        std::unordered_set<K, Hash, Pred, polymorphic_allocator<K>>;


    template <typename K,
              typename Hash = std::hash<K>,
              typename Pred = std::equal_to<K>>
    using unordered_multiset =
        std::unordered_multiset<K, Hash, Pred, polymorphic_allocator<K>>;
}
