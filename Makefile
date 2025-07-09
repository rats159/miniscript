# recommended build stuff
build:
	odin build src -o:speed --out:miniscript -vet

run:
	odin run src -vet -debug -- --path $(path) --print-ast --print-tokens

clean:
	rm miniscript