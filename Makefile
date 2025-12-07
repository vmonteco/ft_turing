#******************************************************************************#
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: vmonteco <vmonteco@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/11/13 05:03:29 by vmonteco          #+#    #+#              #
#    Updated: 2025/12/07 05:00:06 by vmonteco         ###   ########.fr        #
#                                                                              #
#******************************************************************************#

NAME						= ft_turing

# SRC
## Utils:
### Tests:
UTILS_TESTS_SUBDIR			= tests
UTILS_TESTS_FILENAMES		= package.lisp \
							  tests.lisp
UTILS_TESTS_SRC				= $(addprefix \
								$(UTILS_TESTS_SUBDIR)/, \
								$(UTILS_TESTS_FILENAMES) \
							  )
###
UTILS_SUBDIR				= utils
UTILS_FILENAMES				= utils.asd \
							  package.lisp \
							  read-file.lisp
UTILS_SRC					= $(addprefix \
								$(UTILS_SUBDIR)/, \
								$(UTILS_TEST_SRC) \
								$(UTILS_FILENAMES) \
							  )
##

## Turing emulator:
### Tests:
EMULATOR_TESTS_SUBDIR		= tests
EMULATOR_TESTS_FILENAMES	= package.lisp \
							  tests.lisp
EMULATOR_TESTS_SRC			= $(addprefix \
								$(EMULATOR_TESTS_SUBDIR)/, \
								$(EMULATOR_TESTS_FILENAMES) \
							  )
###
EMULATOR_SUBDIR				= emulator
-EMULATOR_FILENAMES			= package.lisp \
							  emulate-turing-machine.lisp \
EMULATOR_SRC				= $(addprefix \
								$(EMULATOR_SUBDIR)/, \
								$(EMULATOR_FILENAMES) \
							  )
##
SRC_DIR						= ./src
SRC							= $(addprefix $(SRC_DIR)/, \
								$(EMULATOR_SRC) \
								ft_turing.asd \
								main.lisp \
								package.lisp \
								tests/package.lisp \
								tests/tests.lisp \
							  )
#

all: $(NAME)

$(NAME): scripts/build.lisp $(SRC)
	cd $(SRC_DIR); ../$<

test: scripts/test.lisp FORCE
	cd $(SRC_DIR); ../$<

clean:

fclean: clean
	$(RM) $(NAME)

re: fclean all

FORCE:

.PHONY: all test clean fclean re FORCE
