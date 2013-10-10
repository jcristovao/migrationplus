#!/bin/bash

# Install the environment necessary to test
# the migration-plus package
# ATTN: requires cabal 1.18

set -e

cabalversion=`cabal --version`

if [[ "$cabalversion" != *1.18* ]]
then
	echo "You need to install cabal version 1.18"
	exit -1
fi

if [ "$1" != "clean" -a "$1" != "mysql" -a "$1" != "postgresql" -a "$1" != "sqlite" -a "$1" != "" ];
then
	echo ""
	echo "Fetches and compiles all dependencies for persistent-migrationplus"
	echo ""
	echo -e "\tsetup.sh [mysql|postgresql|sqlite|clean]"
	echo ""
	exit -1
fi

if [ "$1" == "clean" ];
then
	rm -rf mysql mysql-simple persistent hssqlppp migrationplus persistent-migrationplus
	exit 0
fi

for i in `seq 1 5`; do echo "" ; done
echo -e "\t\t\tThis will take a while... "
if [ "$1" == "" ];
then
	echo ""
	echo -e "\t\t\tYou are compiling all supported backends."
	echo -e "\t\t\tFor a specific backend, type ./setup.sh backend"
	echo -e "\t\t\tWhere backend is one of [mysql|postgresql|sqlite]"
fi
for i in `seq 1 5`; do echo "" ; done

if [ "$1" == "mysql" -o "$1" == "" ];
then
	# Current MySql from https://github.com/bos/mysql
	# is not compiling in 1.18
	git clone https://github.com/jcristovao/mysql
	git clone https://github.com/bos/mysql-simple
fi

# Modified persistent
git clone -b extraPGSQL git@github.com:jcristovao/persistent.git

# Latest hssqlppp is not in hackage
git clone https://github.com/JakeWheat/hssqlppp

# and finnally, Migration Plus
cabal sandbox init
cabal sandbox add-source persistent/persistent
cabal sandbox add-source persistent/persistent-template
if [ "$1" == "mysql" -o "$1" == "" ];
then
	cabal sandbox add-source mysql
	cabal sandbox add-source mysql-simple
	cabal sandbox add-source persistent/persistent-mysql
fi
if [ "$1" == "postgresql" -o "$1" == "" ];
then
	cabal sandbox add-source persistent/persistent-postgresql
fi
if [ "$1" == "sqlite" -o "$1" == "" ];
then
	cabal sandbox add-source persistent/persistent-sqlite
fi
cabal sandbox add-source hssqlppp/hssqlppp
if [ "$1" == "postgresql" -o "$1" == "" ];
then
	cabal install --only-dependencies --enable-tests -f postgresql
	cabal configure --enable-tests -f postgresql
elif [ "$1" == "mysql" ]; 
then
	cabal install --only-dependencies --enable-tests -f mysql
	cabal configure --enable-tests -f mysql
elif [ "$1" == "sqlite" ]; 
then
	cabal install --only-dependencies --enable-tests -f sqlite
	cabal configure --enable-tests -f sqlite
fi
cabal build
