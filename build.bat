mkdir public
mkdir public\css
mkdir public\data

cp src/index.html public
cp src/css/*.css public/css/
cp src/data/* public/data/
elm make src/Main.elm --output=public/elm.js --optimize
