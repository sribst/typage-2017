#!/bin/bash
declare -a arr=(
    # "basic"
    # "simpl_rec"
    # "big_rec"
    "fact"
    "X"
    "Y"
    "Y_prime"
    "Z"
    "omega"
    "omega_prime"
)

for i in "${arr[@]}"
do
    echo -e "\n\n$i : "
    ./main.native -S example/"$i".lang false
done
