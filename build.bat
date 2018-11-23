mkdir public
mkdir public\css

cp src/index.html public
cp src/css/*.css public/css/
elm make src/Main.elm --output=public/elm.js --optimize
