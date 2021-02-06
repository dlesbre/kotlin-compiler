BLD=Build
SRC=Sources
DBG=Debug
FLAGS= -I $(BLD) -c
OBJS=$(addsuffix .cmo, $(addprefix $(BLD)/,erreurs syntax parser lexer \
                        type_syntax environnement type_functions typeur \
						allocation compiler))
COMP=ocamlc

#raccourcis

all: $(BLD) pkotlinc
.PHONY: clean test lexer typeur parser
clean: $(BLD) 
	rm -rf $(BLD) pkotlinc
lexer: $(BLD) $(BLD)/lexer.cmo
parser: $(BLD) $(BLD)/parser.cmo
typeur: $(BLD) $(BLD)/typeur.cmo
pkotlinc: $(BLD) $(BLD)/main.out
	cp $(BLD)/main.out ./pkotlinc
test: pkotlinc
	cp ./pkotlinc ../tests-3-dec/tests/

$(BLD)/main.out: $(OBJS) $(BLD)/main.cmo
	$(COMP) -I $(BLD) -o $@ $(OBJS) $(BLD)/main.cmo
$(BLD)/interpretor.out: $(OBJS) $(BLD)/tests.cmo $(BLD)/interpretor.cmo
	$(COMP) -I $(BLD) -o $@ $(OBJS) $(BLD)/tests.cmo $(BLD)/interpretor.cmo

# General rules
$(BLD)/%.cmo: $(BLD)/%.ml
	$(COMP) $(FLAGS) -o $@ $<
$(BLD)/%.cmi: $(BLD)/%.mli
	$(COMP) $(FLAGS) -o $@ $<
$(BLD)/%.cmo: $(SRC)/%.ml
	$(COMP) $(FLAGS) -o $@ $<
$(BLD)/%.cmi: $(SRC)/%.mli
	$(COMP) $(FLAGS) -o $@ $<

# Dependencies
$(BLD)/main.cmo: $(BLD)/lexer.cmo $(BLD)/typeur.cmi $(BLD)/compiler.cmo $(SRC)/main.ml
$(BLD)/compiler.cmo: $(BLD)/compiler.cmi $(SRC)/compiler.ml
$(BLD)/compiler.cmi: $(BLD)/allocation.cmi $(SRC)/compiler.mli
$(BLD)/allocation.cmo: $(BLD)/allocation.cmi $(SRC)/allocation.ml
$(BLD)/allocation.cmi: $(BLD)/type_syntax.cmo $(SRC)/allocation.mli
$(BLD)/typeur.cmo: $(BLD)/typeur.cmi $(SRC)/typeur.ml
$(BLD)/typeur.cmi: $(BLD)/type_functions.cmi $(SRC)/typeur.mli
$(BLD)/type_functions.cmo: $(BLD)/type_functions.cmi $(SRC)/type_functions.ml
$(BLD)/type_functions.cmi: $(BLD)/environnement.cmi $(SRC)/type_functions.mli
$(BLD)/environnement.cmo: $(BLD)/environnement.cmi $(SRC)/environnement.ml
$(BLD)/environnement.cmi: $(BLD)/type_syntax.cmo $(SRC)/environnement.mli
$(BLD)/type_syntax.cmo: $(BLD)/syntax.cmo $(SRC)/type_syntax.ml
$(BLD)/lexer.cmo: $(BLD)/parser.cmi $(BLD)/lexer.ml
$(BLD)/parser.cmo: $(BLD)/parser.cmi $(BLD)/parser.ml
$(BLD)/parser.cmi: $(BLD)/syntax.cmo $(BLD)/parser.mli
$(BLD)/syntax.cmo: $(BLD)/erreurs.cmi $(SRC)/syntax.ml
$(BLD)/erreurs.cmo: $(BLD)/erreurs.cmi $(SRC)/erreurs.ml
$(BLD)/erreurs.cmi: $(SRC)/erreurs.mli 

# other
$(BLD)/lexer.ml: $(SRC)/lexer.mll
	ocamllex -o $(BLD)/lexer.ml $<
$(BLD)/parser.ml $(BLD)/parser.mli: $(SRC)/parser.mly
	menhir -v --base $(BLD)/parser $<
$(BLD):
	mkdir $(BLD)
