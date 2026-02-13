#!/usr/bin/env sh

FILENAME=json-files/01-file-without-permission.json

touch $FILENAME
chmod a-r $FILENAME
./ft_turing $FILENAME 111-11=
rm $FILENAME

