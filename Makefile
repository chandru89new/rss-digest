build:
	cabal build --disable-profiling --enable-split-sections && cabal install --overwrite-policy=always --disable-profiling --enable-split-sections

debug-build:
	cabal build --ghc-options="-prof -fprof-auto -rtsopts" && cabal install --overwrite-policy=always
	
clean-build:
	cabal clean && make build

gen-profile-pdf:
	hp2ps -c -e8in rdigest.hp && ps2pdf rdigest.ps rdigest.pdf && open rdigest.pdf

# refresh-with-profiling:
# 	rdigest refresh +RTS -hc -p -RTS

strip:
	strip dist-newstyle/build/aarch64-osx/ghc-9.2.5/rdigest-0.1.0.0/x/rdigest/build/rdigest/rdigest