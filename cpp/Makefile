target/alloca: alloca.cpp target
	c++ -std=c++20 -O2 $< -o target/alloca

target/jalloca: alloca.cpp target
	c++ -std=c++20 -O2 $< -o target/jalloca -L`jemalloc-config --libdir` -Wl,-rpath,`jemalloc-config --libdir` -ljemalloc `jemalloc-config --libs`

target:
	mkdir -p target

.PHONY: fmt
fmt:
	clang-format -i alloca.cpp

run: target/alloca
	target/alloca

jrun: target/jalloca
	target/jalloca

.PHONY: clean
clean:
	rm -rf target
