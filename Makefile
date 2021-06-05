examples/Generated.elm: build/elm.js src/main.js examples/schema.bare
	node src/main.js examples/schema.bare $@ Generated
	elm-format --yes $@

build/elm.js: src/Main.elm src/Generator.elm
	elm make --output=$@ src/Main.elm
