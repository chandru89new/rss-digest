build:
	cabal build --disable-profiling --enable-split-sections && cabal install --overwrite-policy=always --disable-profiling --enable-split-sections

debug-build:
	cabal build --ghc-options="-g -O0" && cabal install --overwrite-policy=always
	
clean-build:
	cabal clean && make build

strip:
	strip dist-newstyle/build/aarch64-osx/ghc-9.2.5/rss-digest-0.1.0.0/x/rss-digest/build/rss-digest/rss-digest