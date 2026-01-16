#!/bin/bash

echo "- - - - - - - - - - Preparação do ambiente: - - - - - - - - - - "

#Sistema
if sudo apt-get update; then
    echo "- - - - - - - - - - Lista de pacotes atualizada. - - - - - - - - - -"
else
    echo "Erro ao atualizar a lista de pacotes."
    exit 1
fi

if sudo apt-get upgrade -y; then
    echo "- - - - - - - - - - Pacotes atualizados. - - - - - - - - - -"
else
    echo "Erro ao atualizar os pacotes."
    exit 1
fi


#Pacote libopenmpi-dev
if dpkg -l | grep -qw "libopenmpi-dev"; then
    echo "- - - - - - - - - - Pacote libopenmpi-dev já instalado previamente. - - - - - - - - - -"
else
    if sudo apt-get install -y libopenmpi-dev; then
        echo "- - - - - - - - - - libopenmpi-dev instalado. - - - - - - - - - -"
    else
        echo "Erro ao instalar libopenmpi-dev."
        exit 1
    fi
fi


#Pacote openmpi-bin
if dpkg -l | grep -qw "openmpi-bin"; then
    echo "- - - - - - - - - - Pacote openmpi-bin já instalado previamente. - - - - - - - - - -"
else
    if sudo apt-get install -y openmpi-bin; then
        echo "- - - - - - - - - - openmpi-bin instalado. - - - - - - - - - -"
    else
        echo "Erro ao instalar openmpi-bin."
        exit 1
    fi
fi


#Pacote libhdf5-dev
if dpkg -l | grep -qw "libhdf5-dev"; then
    echo "- - - - - - - - - - Pacote libhdf5-dev já instalado previamente. - - - - - - - - - -"
else
    if sudo apt-get install -y libhdf5-dev; then
        echo "- - - - - - - - - - libhdf5-dev instalado. - - - - - - - - - -"
    else
        echo "Erro ao instalar libhdf5-dev."
        exit 1
    fi
fi


#Pacote libjansson-dev
if dpkg -l | grep -qw "libjansson-dev"; then
    echo "- - - - - - - - - - Pacote libjansson-dev já instalado previamente. - - - - - - - - - -"
else
    if sudo apt-get install -y libjansson-dev; then
        echo "- - - - - - - - - - libjansson-dev instalado. - - - - - - - - - -"
    else
        echo "Erro ao instalar libjansson-dev."
        exit 1
    fi
fi


#Pacote build-essential
if dpkg -l | grep -qw "build-essential"; then
    echo "- - - - - - - - - - Pacote build-essential já instalado previamente. - - - - - - - - - -"
else
    if sudo apt-get install -y build-essential; then
        echo "- - - - - - - - - - build-essential instalado. - - - - - - - - - -"
    else
        echo "Erro ao instalar build-essential."
        exit 1
    fi
fi

echo "Ambiente preparado."