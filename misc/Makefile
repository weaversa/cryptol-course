all: deps.svg deps.png

deps.svg: deps.gv
	dot -Tsvg deps.gv -o deps.svg

deps.png: deps.gv
	dot -Tps deps.gv -o deps.ps
	convert deps.ps deps.png
	rm -f deps.ps
clean:
	rm -f deps.svg deps.png
