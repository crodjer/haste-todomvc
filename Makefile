app:
	cd src && hastec Todo.hs -o ../js/app.js

deploy:
	git checkout gh-pages
	git merge master
	make
	git add js
	cp -r bower_components js/app.js tmp
	git commit -am "Automated Build" || true
	git push origin gh-pages
	git checkout -
	cp -r tmp/bower_components bower_components
	cp -r tmp/app.js js/

watch:
	# Watchman: https://github.com/crodjer/watchman
	watchman Makefile src/Todo.hs src/**/*.hs -- make todo

server:
	python -mhttp.server 9001
