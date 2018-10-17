#!/bin/bash

#objdumps .text section of an object in C-style escaped values.

E_WRONG_ARGS=3
num_expected_args=1

if [ $# -ne $num_expected_args ]
then
		echo "Usage: `basename $0` object_file"
		exit $E_WRONG_ARGS
fi

for i in $(objdump -d $@ |grep "^ " |cut -f2);
do echo -n '\x'$i;
done;
echo 
