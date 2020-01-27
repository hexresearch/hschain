runtests: test_evpow
	./test_evpow

test_evpow: test_evpow.c evpow.c evpow.h
	gcc -O3 -o test_evpow -Ipicosat-965 test_evpow.c evpow.c picosat-965/picosat.c ${shell pkg-config --libs --cflags openssl}

