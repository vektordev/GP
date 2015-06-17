#!/bin/sh
#BSUB -J haskell-gp
#BSUB -o ~/GP/gp-$(date -d "today" +"%Y%m%d%H%M").out
#BSUB -q deflt 

#usage: bsub < thisfile.sh

#add user code here:
cd ~/GP/

mkdir -p $LSB_BATCH_JID

cd $LSB_BATCH_JID

env | tee -a myLogfile.log

hostname | tee -a myLogfile.log

module load gcc haskell

cd ~/GDI3/
~/GP/GRPCore +RTS -N4 | tee -a myTest -i
