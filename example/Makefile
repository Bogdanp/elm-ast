SOURCES = $(shell find ../src -type f -name "*.elm" -print)

elm.js: Main.elm $(SOURCES)
	elm make --output=elm.js Main.elm
