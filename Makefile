SHELL=/bin/bash
BNFC?=/home/students/inf/PUBLIC/MRJP/bin/bnfc
all:
	-mkdir build
	cd src && \
	$(BNFC) --functor MyLatte.cf && \
	happy -gca ParMyLatte.y && \
	alex -g LexMyLatte.x && \
	ghc --make Main.hs -odir ../build -hidir ../build -o ../interpreter

clean:
	-rm -rf build
	-rm -f src/{DocMyLatte,LexMyLatte,ParMyLatte,SkelMyLatte,PrintMyLatte,AbsMyLatte,ErrM,TestMyLatte}.*
	-rm -f interpreter
