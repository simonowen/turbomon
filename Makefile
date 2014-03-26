NAME=turbomon
UNAME := $(shell uname -s)
SRC_BANKS=$(wildcard TM_?.asm)

.PHONY: dist clean

$(NAME).dsk: $(NAME).asm $(SRC_BANKS)
	pyz80.py -I samdos2 --exportfile=$(NAME).sym $(NAME).asm

run: $(NAME).dsk
ifeq ($(UNAME),Darwin)
	open $(NAME).dsk
else
	xdg-open $(NAME).dsk
endif

clean:
	rm -f $(NAME).dsk $(NAME).sym
