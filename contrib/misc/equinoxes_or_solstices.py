#!/usr/bin/env python
# -*- coding: utf-8 -*-
__docformat__ = "restructuredtext en"

import os
import sys
import math

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)


def sigma(lists:tuple, func:object) -> float:
    assert len(set(len(lst) for lst in lists)) == 1, (
        "Lists must have the same length")
    return sum(func(*e) for e in zip(*lists))


def equinoxes_or_solstices(year=1962):
    y = (year - 2000) / 1000

    jde = (2451716.56767 + 365241.62603 * y + 0.00325 * y**2 +
           0.00888 * y**3 - 0.00030 * y**4)

    t = (jde - 2451545.0) / 36525
    w = 35999.373 * t - 2.47
    dl = 1 + 0.0334 * math.cos(math.radians(w + 0.0007)) * math.cos(
        math.radians(2 * w))
    print(f"  y: {y}\njde: {jde}\n  t: {t}\n  w: {w}\n dl: {dl}")

    a0 = (485, 203, 199, 182, 156, 136, 77, 74, 70, 58, 52, 50,
          45, 44, 29, 18, 17, 16, 14, 12, 12, 12, 9, 8)
    b0 = (324.96, 337.23, 342.08, 27.85, 73.14, 171.52, 222.54, 296.72,
          243.58, 119.81, 297.17, 21.02, 247.54, 325.15, 60.93, 155.12,
          288.79, 198.04, 199.76, 95.39, 287.11, 320.81, 227.73, 15.45)
    c0 = (1934.136, 32964.467, 20.186, 445267.112, 45036.886, 22518.443,
          65928.934, 3034.906, 9037.513, 33718.147, 150.678, 2281.226,
          29929.562, 31555.956, 4443.417, 67555.328, 4562.452, 62894.029,
          31436.921, 14577.848, 31931.756, 34777.259, 1222.114, 16859.074)
    s = sigma((a0, b0, c0), lambda a, b, c: a * math.cos(
        math.radians(b + c * t)))

    return jde + (0.00001 * s) / dl


if __name__ == "__main__":
    print(equinoxes_or_solstices())
