all: test ambc

test: tests
	./tests

tests: src/*
	rustc --test --out-dir . src/tests.rs

ambc: src/*
	rustc --out-dir . $<

clean:
	rm -f ambc tests
