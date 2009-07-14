# Copyright (C) 2009 Dmitry Vasiliev <dima@hlabs.spb.ru>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
