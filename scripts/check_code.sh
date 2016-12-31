#!/bin/bash
#
# Author: Y. Audouin
# Date: 16/07/2014
#
#Returns incorrect lines
if [[ $# -lt 1 ]]; then
  echo "incorrect number of argument"
  echo "usage: check_code.sh path_to_code"
  exit 1
fi
if [[ $1 -eq "-h" ]]; then
  echo "Script checking some points of the coding convention "
  echo "for all the .f and .F in the folder given in parameter"
  echo "It will generate 5 files:"
  echo "-- indent.log : contains the line where the indentation is not a 6 + x*2"
  echo "-- comments.log : checks that the character used for comments is '!'"
  echo "-- continuation.log : checks that the character for continuation is '&'"
  echo "-- lowercase.log : checks that there are no lowercase code"
  echo "-- linetoolong.log : checks that there are no line wider than 72 character (expect comments)"
  echo "-- invalidchar.log : checks that there are no tabs or CLRF (Windows \\n)"
  exit 1
fi
#
echo '*****************'
echo 'indentation error'
echo '*****************'
grep -ER -n $2 '^(\ ){9}[^\ ]|^(\ ){7}[^\ ]|^(\ ){15}[^\ ]' $1 --include=*.[fF] > indent.log
wc -l indent.log
echo '*****************'
echo 'Comments error'
echo '*****************'
grep -ER -n $2 '^[^!\n0-9#\ ]' $1 --include=*.[fF] > comments.log 
wc -l comments.log
echo '*****************'
echo 'Continuation line error'
echo '*****************'
grep -ER -n $2 '^(\ ){5}[^\&\ ]' $1 --include=*.[fF] > continuation.log 
wc -l continuation.log
echo '*****************'
echo 'Lowercase error'
echo '*****************'
grep -ER -n $2 '^[^!#\"'\'']*[azertyuiopqsdfghjklmnbvcxw]' $1 --include=*.[fF] > lowercas.log  
wc -l lowercas.log
echo '*****************'
echo 'Line too long error'
echo '*****************'
grep -ER -n $2 '^[^!]{73}' $1 --include=*.[fF] > linetoolong.log  
wc -l linetoolong.log
echo '*****************'
echo 'Invalid character error'
echo '*****************'
grep -PR -n $2 '\t|\r' $1 --include=*.[fF] > invalidchar.log  
wc -l invalidchar.log
