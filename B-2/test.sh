#!/bin/bash

GREEN="\e[0;32m"
RED="\e[1;31m"
END="\e[0m"

for i in tests/*.dat; do
    echo -en "${i%.dat}\t"
    ./main.d.byte <$i >output 2>/dev/null
    { diff -b ${i/.dat/.ans} output >/dev/null 2>&1 && echo -e "${GREEN}OK${END}"; } || echo -e  "${RED}WA${END}"
done
rm output
