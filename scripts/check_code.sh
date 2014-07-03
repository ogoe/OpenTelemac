#!/bin/bash

#Returns incorrect lines
if [[ $# -lt 1 ]]; then
  echo "incorrect number of argument"
  echo "usage: check_code.sh path_to_code"
  exit 1
fi
#
echo '*****************'
echo 'indentation error'
echo '*****************'
grep -ER -n $2 '^(\ ){9}[^\ ]|^(\ ){7}[^\ ]|^(\ ){15}[^\ ]' $1 --include=*.[fF] > indent.log
echo '*****************'
echo 'Comments error'
echo '*****************'
grep -ER -n $2 '^[^!\n0-9#\ ]' $1 --include=*.[fF] > comments.log 
echo '*****************'
echo 'Continuation line error'
echo '*****************'
grep -ER -n $2 '^(\ ){5}[^\&\ ]' $1 --include=*.[fF] > continuation.log 
echo '*****************'
echo 'Lowercase error'
echo '*****************'
grep -ER -n $2 '^[^!#\"'\'']*[azertyuiopqsdfghjklmnbvcxw]' $1 --include=*.[fF] > lowercas.log  
