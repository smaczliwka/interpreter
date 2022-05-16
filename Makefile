all:
	-mkdir build
	cd src && \
	bnfc --functor MyLatte.cf && \
	#sed -i '/fail/d' ErrM.hs && \
	happy -gca ParMyLatte.y && \
	alex -g LexMyLatte.x && \
	ghc --make Main.hs -odir ../build -hidir ../build -o ../interpreter
	
clean:
	-rm -rf build
	-rm -f src/{DocMyLatte,LexMyLatte,ParMyLatte,SkelMyLatte,PrintMyLatte,AbsMyLatte,ErrM,TestMyLatte}.*
	-rm -f interpreter
