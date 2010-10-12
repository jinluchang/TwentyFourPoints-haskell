all :
	ghc -o main -Wall --make Main.hs

clean :
	rm *.o *.hi
