#******************************************************************************#
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: vmonteco <vmonteco@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/11/13 05:03:29 by vmonteco          #+#    #+#              #
#    Updated: 2025/12/21 04:52:59 by vmonteco         ###   ########.fr        #
#                                                                              #
#******************************************************************************#

NAME								= ft_turing

# SRC
## Utils:
### Tests:
UTILS_TESTS_SUBDIR					= tests
UTILS_TESTS_FILENAMES				= package.lisp \
									  tests.lisp
UTILS_TESTS_SRC						= $(addprefix \
										$(UTILS_TESTS_SUBDIR)/, \
										$(UTILS_TESTS_FILENAMES) \
							 		)
###
UTILS_SUBDIR						= utils
UTILS_FILENAMES						= utils.asd \
									  package.lisp
UTILS_SRC							= $(addprefix \
										$(UTILS_SUBDIR)/, \
										$(UTILS_TEST_SRC) \
										$(UTILS_FILENAMES) \
									)
##

## Turing machine maker:
### Tests:
MACHINE_MAKER_TESTS_SUBDIR			= tests
MACHINE_MAKER_TESTS_FILENAMES		= package.lisp \
									  tests.lisp
MACHINE_MAKER_TESTS_SRC				= $(addprefix \
										$(MACHINE_MAKER_TESTS_SUBDIR)/, \
										$(MACHINE_MAKER_TESTS_FILENAMES) \
									)
###
MACHINE_MAKER_SUBDIR				= machine-maker
MACHINE_MAKER_FILENAMES				= package.lisp \
									  hardware.lisp \
									  emulate-turing-machine.lisp \
MACHINE_MAKER_SRC					= $(addprefix \
										$(MACHINE_MAKER_SUBDIR)/, \
										$(MACHINE_MAKER_FILENAMES) \
									)
##
SRC_DIR								= ./src
SRC									= $(addprefix $(SRC_DIR)/, \
										$(MACHINE_MAKER_SRC) \
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
