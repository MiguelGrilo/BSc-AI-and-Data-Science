#!/bin/bash

#Compilar o programa
# -I corresponde ao diretório /usr/include/     -L corresponde à biblioteca
if mpicc -o kmeans_paralelo kmeans_paralelo.c -I/usr/include/hdf5/serial -L/usr/lib/x86_64-linux-gnu/hdf5/serial -lhdf5 -I/usr/include -L/usr/lib/x86_64-linux-gnu -ljansson -lm; then 
    echo "- - - - - - - - - - Programa compilado. - - - - - - - - - -"
else
    echo "Erro ao compilar o programa."
    exit 1
fi

#Correr o programa
datafile="points_20_2.csv"
K="2"
out_clusters="outputClustersParalelo.csv"
out_centroids="outputCentroidsParalelo.csv"
N="1"
#Formato
#mpirun -np $number_of_processes ./kmeans $input_data $K $out_clusters $out_centroids $N
#Debugger
#time mpirun -np 1 gdb ./kmeans_paralelo "$datafile" 2 outputClustersParalelo outputCentroidsParalelo 1
time mpirun -np "$N" ./kmeans_paralelo "$datafile" "$K" "$out_clusters" "$out_centroids" "$N"

echo "- - - - - - - - - - Programa executado. - - - - - - - - - -"