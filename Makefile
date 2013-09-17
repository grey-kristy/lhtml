
all: compile

compile:
	erlc *.erl

test:
	erl -s example test -s init stop
	
clean:
	rm *.beam
	
