#******************************************************************************#
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: vmonteco <vmonteco@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/11/13 05:03:29 by vmonteco          #+#    #+#              #
#    Updated: 2026/02/19 20:59:56 by vmonteco         ###   ########.fr        #
#                                                                              #
#******************************************************************************#

NAME								= ft_turing

# SRC
## ft_turing:
FT_TURING_SRC						= $(addprefix src/, \
										ft_turing.asd \
										main.lisp \
										package.lisp \
										tests/package.lisp \
										tests/tests.lisp \
									)

## libs:
### hardware:
HARDWARE_SRC						= $(addprefix hardware/, \
										hardware.asd \
										hardware.lisp \
										package.lisp \
										tests/package.lisp \
										tests/tests.lisp \
									)
### machine-description:
MACHINE_DESCRIPTION_SRC				= $(addprefix machine-description/, \
										conditions.lisp \
										format-functions.lisp \
										machine-description.asd \
										machine-description.lisp \
										process-functions.lisp \
										transition-result.lisp \
										package.lisp \
										transition-result.lisp \
										tests/package.lisp \
										tests/tests.lisp \
										tests/process-functions-tests.lisp \
										tests/transition-result-tests.lisp \
									)
### machine-maker:
MACHINE_MAKER_SRC					= $(addprefix machine-maker/, \
										machine-maker.asd \
										package.lisp \
										tests/package.lisp \
										tests/tests.lisp \
									)
### utils:
UTILS_SRC							= $(addprefix utils/, \
										alists-utils.lisp \
										flatten.lisp \
										macros.lisp \
										package.lisp \
										sets-utils.lisp \
										truncate-or-complete-list.lisp \
										utils.asd \
										tests/package.lisp \
										tests/tests.lisp \
										tests/truncate-or-complete-list-tests.lisp \
										tests/sets-utils-tests.lisp \
										tests/utils-alist-tests.lisp \
									)

LIBS_SRC							= $(addprefix libs/, \
										$(MHARDWARE_SRC) \
										$(MACHINE_DESCRIPTION_SRC) \
										$(MACHINE_MAKER_SRC) \
										$(UTILS_SRC) \
									)

LINK_FARM							= $(addprefix link-farm/, \
										ft_turing.asd \
										hardware.asd \
										machine-description.asd \
										machine-maker.asd \
										utils.asd \
									)

SRC									= Makefile \
									  settings.lisp \
									  $(FT_TURING_SRC) \
									  $(LIBS_SRC) \
									  $(LINK_FARM)

all: $(NAME)

$(NAME): build.lisp $(SRC)
	./$<

test: test.lisp $(SRC) FORCE
	./$<

clean:

fclean: clean
	$(RM) $(NAME)

re: fclean all

FORCE:

.PHONY: all test clean fclean re FORCE
