#!/usr/bin/env python
#
# This script generates the NOAA r.d. moments for dates 1788 - 2211.
#

import os
import sys
#import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import GregorianCalendar

gc = GregorianCalendar()

data = []
month = 3

with open(os.path.join(PWD, 'VernalEquinox-1788-2211.dates'), 'r') as f:
    for line in f:
        #print(line.strip())
        y, d, h, m = line.strip().split(' ')
        data.append([int(y), month, int(d), int(h), int(m)])


for item in data:
    Y, M, D, h, m = item
    D += h / 24 + m / 60
    print(gc.fixed_from_gregorian((Y, M, D)))

#pprint.pprint(data)
