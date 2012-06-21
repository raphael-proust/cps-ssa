
all:
	make -C tests all

clean:
	make -C src purge
	make -C tests clean
