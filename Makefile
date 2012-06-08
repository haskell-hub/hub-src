OD = dist/build/hub
ID = /usr/hs/bin
HC = mkdir -p $(OD); ghc -XHaskell2010 --make -O1 -outputdir build -Wall
VR = $(shell runghc Version)
CM = "for building Hub ($(VR))" 
OP = '\nNo hub on path: ensure hub-src.har or hub.cabal packages are installed\n\n' 

all: hub

hub: prep
	$(HC) -o $(OD)/hub hub.hs

prep:
	hub load    build-hub <build-hub.har || printf $(OP)
	hub comment build-hub $(CM)          || true
	hub set     build-hub                || true
	runhaskell prep

install:
	install -D $(OD)/hub    $(DESTDIR)$(ID)/hub
	install -D man/hub.1.gz $(DESTDIR)/usr/share/man/man1/hub.1.gz
	install -D man/hub.5.gz $(DESTDIR)/usr/share/man/man5/hub.5.gz

clean:
	cabal clean
	rm -rf build
