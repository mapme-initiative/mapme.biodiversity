.PHONY: draft article
draft:
	docker run --rm -it \
		-e JOURNAL=joss \
		-v .:/data \
		-u $(id -u)\:$(id -g) \
		openjournals/inara \
		-o pdf \
		./paper.md

article:
	docker run --rm -it \
		-e JOURNAL=joss \
		-v .:/data \
		-u $(id -u)\:$(id -g) \
		openjournals/inara \
		-o pdf \
		-p \
		./paper.md
