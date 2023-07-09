build_all: target/alloca4 target/alloca5

target:
	mkdir -p target

target/4: target
	mkdir -p target/4

target/5: target
	mkdir -p target/5

target/alloca4: alloca.ml target/4
	cp $< target/4
	(cd target/4; opam exec --switch=4.14.1 -- ocamlopt -O2 -o alloca $<)
	cp target/4/alloca target/alloca4

target/alloca5: alloca.ml target/5
	cp $< target/5
	(cd target/5; opam exec --switch=5-compact -- ocamlopt -O2 -o alloca $<)
	cp target/5/alloca target/alloca5

run4: target/alloca4
	$<

run5: target/alloca5
	$<

.PHONY: clean
clean:
	rm -rf target
