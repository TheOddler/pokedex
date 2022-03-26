build:
	mkdir -p public
	mkdir -p public/css
	mkdir -p public/data

	cp src/index.html public
	cp src/css/*.css public/css/
	cp src/data/* public/data/
	elm make src/Main.elm --output=public/elm.js --optimize

live:
	# Elm Live: https://github.com/wking-io/elm-live
	elm-live src/Main.elm --open --hot --dir=src --start-page=index.html -- --output=src/elm.js --debug
