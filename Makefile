# Author: Samuele Giraudo
# Creation: 2024-06
# Modifications: 2024-06, 2024-07

CC = ocamlbuild
FLAGS = -r -cflags -w,A-4-70 -menhir "menhir --explain"
#LIBS =-package unix -package extlib -use-menhir
LIBS =-package unix -use-menhir

SRC_DIR = Sources

NAME = Llama
EXEC = $(NAME).native

.PHONY: all
all: $(NAME)

.PHONY: $(NAME)
$(NAME):
	$(CC) $(FLAGS) $(LIBS) $(SRC_DIR)/$(EXEC)
	mv -f $(EXEC) $(NAME)

.PHONY: noassert
noassert: FLAGS += -cflag -noassert
noassert: all

.PHONY: profile
profile: FLAGS += -tag debug
profile: noassert

.PHONY: clean
clean:
	rm -rf _build
	rm -f $(NAME)

.PHONY: stats
stats:
	wc $(SRC_DIR)/*.ml $(SRC_DIR)/*.mly $(SRC_DIR)/*.mll

