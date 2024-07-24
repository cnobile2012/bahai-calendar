#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import sys
import math

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime.base_calendar import BaseCalendar


class ApproxLHA(BaseCalendar):
    """
    Test the _approx_local_hour_angle method in the BaseCalendar class.
    We need to determine what the jd and latitude would be to cause an
    h0 value that is < -1 or > 1.
    """
    SUN_OFFSET = 0.8333333333333334
    STARS_PLANET_OFFSET = 0.5666666666666667

    def analize(self):
        data = []

        for jd in range(2394646, 2467330):
            tc = self.julian_centuries(jd)

            for lat in range(-180, 181):
                h0 = self.__approx_local_hour_angle(tc, lat)

                if h0 < -1 or h0 > 1:
                    cos_h0 = h0 - math.floor(h0)
                else:
                    cos_h0 = h0

                deg = math.degrees(math.acos(cos_h0))

                if h0 < -1 or h0 > 1:
                    data.append((deg, h0, cos_h0, jd, lat))

        return data


    def __approx_local_hour_angle(self, tc:float, lat:float,
                                  offset:float=SUN_OFFSET) -> float:
        """

        """
        delta = self._sun_apparent_declination(tc)
        cos_h0 = ((self._sin_deg(-offset) - self._sin_deg(lat) *
                   self._sin_deg(delta)) / (self._cos_deg(lat) *
                                            self._cos_deg(delta)))
        return cos_h0


if __name__ == "__main__":
    #import argparse

    alha = ApproxLHA()
    [print(deg, h0, cos_h0, jd, alt)
     for deg, h0, cos_h0, jd, alt in alha.analize()]
