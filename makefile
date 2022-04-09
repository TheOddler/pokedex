build:
	mkdir -p public
	mkdir -p public/data
	mkdir -p public/icons

	cp src/index.html public
	cp src/site.webmanifest public
	cp src/data/* public/data/
	cp src/icons/* public/icons/
	
	elm make src/Main.elm --output=public/elm.js --optimize

live:
	# Elm Live: https://github.com/wking-io/elm-live
	elm-live src/Main.elm --open --hot --dir=src --start-page=index.html -- --output=src/elm.js --debug
