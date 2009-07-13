all: compile

# encodings must be first
compile: ebin/encodings.beam \
	$(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

ebin/%.beam: src/%.erl
	erlc -Wall -pa ebin -o ebin/ $<

test: compile
	./test

doc: compile
	erl -noshell -pa ebin -s test_encodings doc -s init stop

clean:
	rm -f ebin/*.beam erl_crash.dump \
		src/doc/*.html src/doc/*.css src/doc/*.png src/doc/edoc-info
