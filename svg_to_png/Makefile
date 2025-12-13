SVG_FILES := $(shell find . -type f -name "*.svg")

PNG_FILES := $(patsubst %.svg,%.png,$(SVG_FILES))

all : $(PNG_FILES)

%.png : %.svg
	convert -density 300 $< $@

clean:
	rm -f $(PNG_FILES)
