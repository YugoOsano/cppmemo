# Play with Makefile
# https://itchyny.hatenablog.com/entry/20120213/1329135107
#
all:
	@echo Hello, World!

# make METHOD=1
ifeq ($(METHOD),1)
	@echo METHOD is $(METHOD)
else
	@echo METHOD is not 1, but $(METHOD)
endif

# assignment to values is done with := or =.

# := is close to regular script languages' counterpart;
#    would be evaluated in sequence.
# =  does lazy evaluation.

$(warning $(SOURCE))
$(warning $(CC))

SOURCE := foo.c
CC      = gcc

$(warning $(SOURCE))
$(warning $(CC))

LIST := huge.c hoge hige.c hoge.c hoga.c hogehogeeee.c

$(warning $(filter hoge, $(LIST)))
$(warning $(filter hoge%, $(LIST)))

# make runinpart
runinpart:
	@echo Hello, part of the world!
