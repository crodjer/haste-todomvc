todo:
	cd src && hastec Todo.hs -o ../js/app.js

watch:
	# Watchman: https://github.com/crodjer/watchman
	watchman Makefile src/Todo.hs src/**/*.hs -- make todo

server:
	python -mhttp.server 9001
