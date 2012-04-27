
all:
	make -C src .depend
	make -C src all

clean:
	make -C src clean
