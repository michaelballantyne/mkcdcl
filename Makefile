.DEFAULT_GOAL := minisat-c-bindings

.PHONY : minisat-c-bindings minisat submods

submods:
	git submodule init
	git submodule update

minisat-c-bindings: minisat submods
	cd minisat-c-bindings; sh build.sh

minisat: submods
	cd minisat; mkdir build; cd build; cmake -DCMAKE_INSTALL_PREFIX:PATH=install ../; make install

clean:
	rm -rf minisat/build
	rm -rf minisat-c-bindings/install
