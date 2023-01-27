build:
	mkdir -p public
	mkdir -p public/icons
	mkdir -p public/images

	cp src/index.html public
	cp src/site.webmanifest public
	cp src/*.css public
	cp src/icons/* public/icons/
	cp src/images/* public/images/
	
	elm make src/Main.elm --output=elm.js --optimize
	uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output public/elm.js

live:
	# Elm Live: https://github.com/wking-io/elm-live
	elm-live src/Main.elm --open --hot --dir=src --start-page=index.html -- --output=src/elm.js --debug
