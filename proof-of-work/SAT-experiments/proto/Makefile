runperftests: test_evpow_strategies
	./test_evpow_strategies

runtests: test_evpow
	./test_evpow

clean:
	rm -f test_evpow test_evpow_strategies

evpow-grid-search: evpow.c evpow.h evpow-grid-search.c
	gcc -O3 -o evpow-grid-search -Iyalsat-03v evpow-grid-search.c evpow.c yalsat-03v/yals.c ${shell pkg-config --libs --cflags openssl}

test_evpow: test_evpow.c evpow.c evpow.h
	gcc -O3 -o test_evpow -Ipicosat-965 -Iyalsat-03v picosat-965/picosat.c test_evpow.c evpow.c ${shell pkg-config --libs --cflags openssl}

test_evpow_strategies: test_evpow_strategies.c evpow.c evpow.h
	gcc -O3 -o test_evpow_strategies -Iyalsat-03v test_evpow_strategies.c evpow.c yalsat-03v/yals.c ${shell pkg-config --libs --cflags openssl}

