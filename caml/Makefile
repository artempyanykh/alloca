build_all: target/alloca4 target/alloca5 target/alloca5pr target/alloca5c

target:
	mkdir -p target

target/4: target
	mkdir -p target/4

target/5: target
	mkdir -p target/5

target/5pr: target
	mkdir -p target/5pr

target/5c: target
	mkdir -p target/5c

target/alloca4: alloca.ml target/4
	cp $< target/4
	(cd target/4; opam exec --switch=4.14.1 -- ocamlopt -O2 -o alloca $<)
	cp -f target/4/alloca target/alloca4

target/alloca5: alloca.ml target/5
	cp $< target/5
	(cd target/5; opam exec --switch=5.0.0 -- ocamlopt -O2 -o alloca $<)
	cp -f target/5/alloca target/alloca5

target/alloca5pr: alloca.ml target/5pr
	cp $< target/5pr
	(cd target/5pr; opam exec --switch=5-sadiq-pool-release -- ocamlopt -O2 -o alloca $<)
	cp -f target/5pr/alloca target/alloca5pr

target/alloca5c: alloca.ml target/5c
	cp $< target/5c
	(cd target/5c; opam exec --switch=/home/arr/dev/ocaml -- ocamlopt -O2 -o alloca $<)
	cp -f target/5c/alloca target/alloca5c

run4: target/alloca4
	$<

run5: target/alloca5
	$<

run5pr: target/alloca5pr
	$<

run5c: target/alloca5c
	$<

.PHONY: clean
clean:
	rm -rf target
