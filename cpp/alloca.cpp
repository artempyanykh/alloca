// Copyright (c) 2023 Artem Pianykh

#include <array>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <limits>
#include <memory>
#include <numeric>
#include <random>
#include <vector>

namespace {
std::random_device rd;  // a seed source for the random number engine
std::mt19937 gen(rd()); // mersenne_twister_engine seeded with rd()

const std::vector<std::size_t> size_classes{32, 64, 128, 256};

// macos sys malloc: threshold for mem release (<=256 retains; >=257 releases)
// const std::vector<std::size_t> size_classes{257};

// a random number generator to pick one of the size classes above
std::uniform_int_distribution<std::size_t> distrib(0, size_classes.size() - 1);

using bytes_t = std::unique_ptr<char[]>;

bytes_t alloc_single(std::size_t nbytes) {
  return std::make_unique<char[]>(nbytes);
}

std::vector<bytes_t> alloc_many(std::size_t nbytes) {
  std::vector<bytes_t> result(1024);
  std::vector<std::size_t> class_counts(size_classes.size());

  std::int64_t nbytes_left = nbytes;
  while (nbytes_left > 0) {
    std::size_t n_size_class = distrib(gen);
    std::size_t size_class = size_classes[n_size_class];

    result.push_back(alloc_single(size_class));

    class_counts[n_size_class] += size_class;
    nbytes_left -= size_class;
  }

  int total_allocated =
      std::accumulate(class_counts.begin(), class_counts.end(), 0);

  std::cout << "Allocated " << total_allocated << " bytes, by size class: {";
  auto sep = "";
  for (const auto cc : class_counts) {
    std::cout << sep << cc;
    sep = ", ";
  }
  std::cout << "}" << std::endl;

  return result;
}
} // namespace

int main(int argc, char **argv) {
  constexpr auto max_stream_size = std::numeric_limits<std::streamsize>::max();

  for (;;) {
    std::size_t mbytes;
    std::cout << "Enter the # of megabytes to allocate > ";
    std::cin >> mbytes;
    std::cin.ignore(max_stream_size, '\n');

    auto nbytes = 1024 * 1024 * mbytes;
    auto data = alloc_many(nbytes);

    std::cout << "Press ENTER to release the memory ";
    std::cin.ignore(max_stream_size, '\n');
  }

  return 0;
}
