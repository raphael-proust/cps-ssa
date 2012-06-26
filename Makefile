
all:
	make -C src depend
	make -C src native
	make -C tests all

clean:
	make -C src purge
	make -C tests clean
