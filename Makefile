all:
	erlc *.erl
	mkdir -p ebin
	mv *.beam ebin/

clean:
	rm ebin/*.beam
