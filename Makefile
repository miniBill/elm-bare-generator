examples/Generated.elm: build/elm.js src/main.js examples/schema.bare
	node src/main.js examples/schema.bare $@ Generated

build/elm.js: src/Main.elm
	elm make --output=$@ $^
