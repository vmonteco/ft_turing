#******************************************************************************#
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: vmonteco <vmonteco@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/11/13 05:03:29 by vmonteco          #+#    #+#              #
#    Updated: 2025/11/18 06:14:03 by vmonteco         ###   ########.fr        #
#                                                                              #
#******************************************************************************#

NAME						= ft_turing

# SRC
## JSON parser:
### Tests:
PARSER_TESTS_SUBDIR			= tests
PARSER_TESTS_FILENAMES		= package.lisp \
							  basic-test.lisp
PARSER_TESTS_SRC			= $(addprefix \
								$(PARSER_TESTS_SUBDIR)/, \
								$(PARSER_TESTS_FILENAMES) \
							  )
###
PARSER_SUBDIR				= json-parser
PARSER_FILENAMES			= package.lisp \
PARSER_SRC					= $(addprefix \
								$(PARSER_SUBDIR)/, \
								$(PARSER_TEST_SRC) \
								$(PARSER_FILENAMES) \
							  )
##

## Turing emulator:
### Tests:
EMULATOR_TESTS_SUBDIR		= tests
EMULATOR_TESTS_FILENAMES	= package.lisp \
							  basic-test.lisp
EMULATOR_TESTS_SRC			= $(addprefix \
								$(EMULATOR_TESTS_SUBDIR)/, \
								$(EMULATOR_TESTS_FILENAMES) \
							  )
###
EMULATOR_SUBDIR				= turing-emulator
-EMULATOR_FILENAMES			= package.lisp \
							  json-parser.lisp \
EMULATOR_SRC				= $(addprefix \
								$(EMULATOR_SUBDIR)/, \
								$(EMULATOR_FILENAMES) \
							  )
##
SRC_DIR						= src
SRC							= $(addprefix $(SRC_DIR)/, \
								$(PARSER_SRC) \
								$(EMULATOR_SRC) \
								ft_turing.asd \
								main.lisp \
								package.lisp \
							  ) \
#

all: $(NAME)

$(NAME): build.lisp $(SRC)
	./$<

test: test.lisp FORCE
	./$<

clean:

fclean: clean
	$(RM) $(NAME)

re: fclean all

FORCE:

.PHONY: all test clean fclean re FORCE
