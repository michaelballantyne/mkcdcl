.DEFAULT_GOAL := minisat-c-bindings

.PHONY : minisat-c-bindings minisat submods

submods:
	git submodule init
	git submodule update

minisat-c-bindings: minisat
	cd minisat-c-bindings; make config prefix=install; make config MINISAT_LIB="-L../minisat/build/install/lib -lminisat"; make config MINISAT_INCLUDE=-I../minisat/build/install/include; CXXFLAGS="-I../minisat -std=c++11" make install

minisat: 
	cd minisat; mkdir build; cd build; cmake -DCMAKE_INSTALL_PREFIX:PATH=install ../; make install

clean:
	rm -rf minisat/build
	rm -rf minisat-c-bindings/build
	rm -rf minisat-c-bindings/install
