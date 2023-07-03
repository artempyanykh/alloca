target/alloca: alloca.cpp target
	c++ -std=c++20 -O2 $< -o target/alloca

target:
	mkdir -p target

.PHONY: fmt
fmt:
	clang-format -i alloca.cpp

run: target/alloca
	target/alloca

.PHONY: clean
clean:
	rm -rf target
