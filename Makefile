.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = factor tank gauge\
       tests db\

ERL = erl -boot start_clean 

compile: ${MODS:%=%.beam} 
	@echo "make clean - clean up"

all: compile 

clean:	
	rm -rf *.beam *~


