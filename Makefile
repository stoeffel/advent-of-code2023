.PHONY: build test watch watch-test ask-for-mode

build:
	sbt compile

test: build
	sbt test

watch:
	sbt ~test -deprecation

run:
	sbt ~run
