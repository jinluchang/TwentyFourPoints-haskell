name = points24

hc = ghc -Wall -O2
main = Main.hs
source = Main.hs
clean = $(name) *.hi *.o

all : $(name)

run : $(name)
	time ./$(name) > result.dat
	wc -l result.dat

$(name) : $(source)
	$(hc) --make -o $@ $(main)

clean :
	rm $(clean)

