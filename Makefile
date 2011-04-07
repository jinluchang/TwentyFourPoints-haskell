name = points24

hc = ghc -Wall -O2
main = Main.hs
source = Main.hs
clean = $(name) *.hi *.o

all : $(name)

run : $(name)
	./$(name)

$(name) : $(source)
	$(hc) --make -o $@ $(main)

clean :
	rm $(clean)

