all:
	dot -Tps deps.gv -o deps.ps
	convert deps.ps deps.png
	rm -f deps.ps
	
clean:
	rm -f deps.ps deps.png
