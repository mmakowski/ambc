ambc: src/ambc.rs
	rustc --out-dir . $<

clean:
	rm -fr ambc
