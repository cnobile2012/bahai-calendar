#!/usr/bin/env python

import os
import sys
import math
import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from bahai_calendar import BahaiCalendar


AL = (4.721964, 5.937458, 1.115589, 5.781616, 5.5474, 1.512, 4.1897, 1.163,
      5.415, 4.315, 4.553, 5.198, 5.989, 2.911, 1.423, 0.061, 2.317, 3.193,
      2.828, 0.52, 4.65, 4.35, 2.75, 4.5, 3.23, 1.22, 0.14, 3.44, 4.37, 1.14,
      2.84, 5.96, 5.09, 1.72, 2.56, 1.92, 0.09, 5.98, 4.03, 4.27, 0.79, 4.24,
      2.01, 2.65, 4.98, 0.93, 2.21, 3.59, 1.5, 2.55)

BL = (1.621043, 6283.348067, 6283.821524, 62829.634302, 125660.5691,
      125660.9845, 62832.4766, 0.813, 125659.31, 57533.85, -33.931,
      777137.715, 78604.191, 5.412, 39302.098, -34.861, 115067.698,
      15774.337, 5296.67, 58849.27, 5296.11, -3980.7, 52237.69, 55076.47,
      261.08, 15773.85, 188491.03, -7756.55, 264.89, 117906.27, 55075.75,
      -7961.39, 188489.81, 2132.19, 109771.03, 54868.56, 25443.93, -55731.43,
      60697.74, 2132.79, 109771.63, -7752.82, 188491.91, 207.81, 29424.63,
      -7.99, 46941.14, -68.29, 21463.25, 157208.4)

bc = BahaiCalendar()

print(len(bc._ADDENDS))
print(len(bc._MULTIPLIERS))
print(len(AL))
print(len(BL))

ADDENDS_AL = []
MULTIPLIERS_BL = []

for i in range(50):
    ADDENDS_AL.append(math.degrees(AL[i])) #, bc._ADDENDS[i]))
    MULTIPLIERS_BL.append(math.degrees(BL[i]/100)) #, bc._MULTIPLIERS[i]))


pprint.pprint(ADDENDS_AL)
pprint.pprint(MULTIPLIERS_BL)
