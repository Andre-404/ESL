#pragma once
#include <cstdint>
#include <cstring>
#include <span>
#include <bit>

#include "gc-config.h"

namespace gc::detail {
    // General bit twiddling helpers
    inline constexpr std::size_t word_bits = 64;
    template<class Fn>
    constexpr void each_free_run(uint64_t x, Fn fn) {
        if (x == 0) return fn(0u, uint32_t(word_bits));
        
        for (uint32_t bit = 0; bit < word_bits;) {
            auto rest = x >> bit; // bit < 64, so in range
            if (rest == 0) return fn(bit, uint32_t(word_bits) - bit);
            auto free_len = uint32_t(std::countr_zero(rest)); // rest != 0, so < 64
            
            if (free_len) fn(bit, free_len);   // len < 64 since rest != 0
            // rest >> free_len has bit 0 set, so countr_one >= 1 and bit strictly increases
            bit += free_len + uint32_t(std::countr_one(rest >> free_len));
        }
    }

    class bitmap {
        uint64_t* _words = nullptr;
        
        static constexpr uint64_t ones = ~uint64_t(0);

        static constexpr std::size_t word_idx(std::size_t i) { return i / word_bits; }
        static constexpr std::size_t bit_idx(std::size_t i) { return i % word_bits; }

        // Mask of bits [lo, hi) inside one word. hi is in [1, word_bits]; shifting by
        // word_bits is UB, so the top end is spelled out separately.
        static constexpr uint64_t range_mask(std::size_t lo, std::size_t hi) {
            return (ones << lo) & (hi == word_bits ? ones : ~(ones << hi));
        }

        // A range [start, start+n) laid onto words: the words [first_word, last_word] it
        // touches, with the in-range bits of the boundary words given by head_mask/tail_mask.
        // When the range lives in a single word (first_word == last_word), head_mask carries
        // the whole range and tail_mask is unused. Callers handle n == 0 before calling.
        struct split {
            std::size_t first_word, last_word;
            uint64_t head_mask, tail_mask;
        };
        static split split_range(std::size_t start, std::size_t n) {
             auto end = start + n;
            auto fw = word_idx(start), lw = word_idx(end - 1);
            auto tail_hi = end - lw * word_bits;
            if (fw == lw) return { fw, lw, range_mask(bit_idx(start), tail_hi), 0 };
            return { fw, lw, range_mask(bit_idx(start), word_bits), range_mask(0, tail_hi) };
        }

        template<bool Set>
        void apply(std::size_t start, std::size_t n) {
            if (n == 0) return;
            auto s = split_range(start, n);
            auto merge = [](uint64_t& w, uint64_t m) { if constexpr (Set) w |= m; else w &= ~m; };
            
            merge(_words[s.first_word], s.head_mask);
            if (s.first_word == s.last_word) return;

            if (s.last_word > s.first_word + 1) {
                memset(
                    _words + s.first_word + 1,
                    Set ? 0xFF : 0x00,
                    (s.last_word - s.first_word - 1) * sizeof(uint64_t)
                );
            }
            merge(_words[s.last_word], s.tail_mask);
        }

        // Is any bit in the range set - or, with Complement, any bit clear?
        template<bool Complement>
        bool scan(std::size_t start, std::size_t n) const {
            if (n == 0) return false;
            auto word = [&](std::size_t w) { return Complement ? ~_words[w] : _words[w]; };
            auto s = split_range(start, n);

            if (word(s.first_word) & s.head_mask) return true;

            for (auto w = s.first_word + 1; w < s.last_word; ++w) if (word(w)) return true;
            return (word(s.last_word) & s.tail_mask) != 0;
        }

        const uint64_t* chunk_ptr(std::size_t chunk_idx) const {
            return _words + chunk_idx * config::chunk_words;
        }

    public:

        bitmap() = default;
        explicit bitmap(uint64_t* words) : _words(words) {}

        bool test(std::size_t i) const { return _words[word_idx(i)] >> bit_idx(i) & 1; }
        void set(std::size_t i) { _words[word_idx(i)] |= uint64_t(1) << bit_idx(i); }
        void clear(std::size_t i) { _words[word_idx(i)] &= ~(uint64_t(1) << bit_idx(i)); }

        void set_range(std::size_t start, std::size_t n) { apply<true>(start, n); }
        void clear_range(std::size_t start, std::size_t n) { apply<false>(start, n); }

        bool any_set(std::size_t start, std::size_t n) const { return scan<false>(start, n); }
        // True when every bit in the range is set. Empty ranges are vacuously true.
        bool all_set(std::size_t start, std::size_t n) const { return !scan<true>(start, n); }

        std::span<const uint64_t, config::chunk_words> chunk(std::size_t chunk_idx) const {
            return std::span<const uint64_t, config::chunk_words>{
                chunk_ptr(chunk_idx), config::chunk_words
            };
        }
    };
}
