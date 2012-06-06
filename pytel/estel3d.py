#!/usr/bin/env python
import sys,os
if __name__ == "__main__": os.system("python "+os.path.join(os.path.dirname(sys.argv[0]),"runcode.py")+" "+os.path.splitext(os.path.basename(sys.argv[0]))[0]+" " + ' '.join(sys.argv[1:]))

"""@author Fabien Decung, inspired by other similar scripts in pytel
"""
"""@note ... this work is based on a collaborative effort between
  .________.                                                          ,--.
  |        |                                                      .  (  (
  |,-.    /   HR Wallingford                EDF - LNHE           / \_ \_/ .--.
  /   \  /    Howbery Park,                 6, quai Watier       \   )   /_   )
   ,.  `'     Wallingford, Oxfordshire      78401 Cedex           `-'_  __ `--
  /  \   /    OX10 8BA, United Kingdom      Chatou, France        __/ \ \ `.
 /    `-'|    www.hrwallingford.com         innovation.edf.com   |    )  )  )
!________!                                                        `--'   `--
"""
