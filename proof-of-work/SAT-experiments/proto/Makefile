runperftests: test_evpow_strategies
	./test_evpow_strategies

runtests: test_evpow
	./test_evpow

clean:
	rm -f test_evpow test_evpow_strategies

test_evpow: test_evpow.c evpow.c evpow.h
	gcc -O3 -o test_evpow -Ipicosat-965 -Iyalsat-03v picosat-965/picosat.c test_evpow.c evpow.c ${shell pkg-config --libs --cflags openssl}

test_evpow_strategies: test_evpow_strategies.c evpow.c evpow.h
	gcc -O3 -o test_evpow_strategies -Ipicosat-965 -Iyalsat-03v picosat-965/picosat.c test_evpow_strategies.c evpow.c yalsat-03v/yals.c ${shell pkg-config --libs --cflags openssl}

