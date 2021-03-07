#!/bin/bash

set -e
EV=3.1.0
BR=$(pwd)

sudo apt-get update && sudo apt-get -y install 64tass wget unzip build-essential vice gcc-multilib g++-multilib wine
rm -rf exomizer && \
	mkdir exomizer && \
	cd exomizer && \
	wget https://bitbucket.org/magli143/exomizer/wiki/downloads/exomizer-$EV.zip && \
	unzip exomizer-$EV.zip && \
	cd src && \
	make && \
	sudo cp exomizer /usr/local/bin
cd $BR/sources && make clean && make
