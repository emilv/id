all:
	erlc *.erl
	mv *.beam ebin/

clean:
	rm ebin/*.beam