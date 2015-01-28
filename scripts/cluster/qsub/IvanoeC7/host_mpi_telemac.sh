#!/bin/bash
srun hostname | sort > mpid.conf
for line in $(echo | cat mpid.conf)
do
  echo -e "$line 1"
done > mpitasks.conf
echo $1 > mpitasks | cat mpitasks mpitasks.conf > mpi_telemac.conf
#
rm -f mpitasks mpitasks.conf mpid.conf
