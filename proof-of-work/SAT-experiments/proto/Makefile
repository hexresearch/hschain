runperftests: test_evpow_strategies
	./test_evpow_strategies

runtests: test_evpow
	./test_evpow

clean:
	rm -f test_evpow test_evpow_strategies

evpow-stats: evpow.c evpow.h evpow-stats.c
	gcc -O3 -o evpow-stats -Iyalsat-03v evpow-stats.c evpow.c yalsat-03v/yals.c ${shell pkg-config --libs --cflags openssl}

test_evpow: test_evpow.c evpow.c evpow.h
	gcc -O3 -o test_evpow -Ipicosat-965 -Iyalsat-03v picosat-965/picosat.c test_evpow.c evpow.c ${shell pkg-config --libs --cflags openssl}

test_evpow_strategies: test_evpow_strategies.c evpow.c evpow.h
	gcc -O3 -o test_evpow_strategies -Iyalsat-03v test_evpow_strategies.c evpow.c yalsat-03v/yals.c ${shell pkg-config --libs --cflags openssl}

