#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Test the Meeus version and the full IAU version.
# The Meeus version is printed first.
#

import os
import sys
import math
import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar


class TestMeeusNutation(BahaiCalendar):
    # Periodic terms for the nutation in longitude and obliquity.
    # Nutation in Longitude and Obliquity referred to mean ecliptic of date.
    # Epoch J2000.0 (JD 2451 545.0 TDB) T in Julian Centuries
    #        Arguments      Longitude           Obliquity
    #    D LS  LM  F  OM    psi_sin     t*sin   eps_cos   t*cos
    NUTATIONS = (
        (0, 0, 0, 0, 1, -171996, -174.2, 92025, 8.9),
        (-2, 0, 0, 2, 2, -13187, -1.6, 5736, -3.1),
        (0, 0, 0, 2, 2, -2274, -0.2, 977, -0.5),
        (0, 0, 0, 0, 2, 2062, 0.2, -895, 0.5),
        (0, 1, 0, 0, 0, 1426, -3.4, 54, -0.1),
        (0, 0, 1, 0, 0, 712, 0.1, -7, 0),
        (-2, 1, 0, 2, 2, -517, 1.2, 224, -0.6),
        (0, 0, 0, 2, 1, -386, -0.4, 200, 0),
        (0, 0, 1, 2, 2, -301, 0, 129, -0.1),
        (-2, -1, 0, 2, 2, 217, -0.5, -95, 0.3),
        (-2, 0, 1, 0, 0, -158, 0, 0, 0),
        (-2, 0, 0, 2, 1, 129, 0.1, -70, 0),
        (0, 0, -1, 2, 2, 123, 0, -53, 0),
        (2, 0, 0, 0, 0, 63, 0, 0, 0),
        (0, 0, 1, 0, 1, 63, 0.1, -33, 0),
        (2, 0, -1, 2, 2, -59, 0, 26, 0),
        (0, 0, -1, 0, 1, -58, -0.1, 32, 0),
        (0, 0, 1, 2, 1, -51, 0, 27, 0),
        (-2, 0, 2, 0, 0, 48, 0, 0, 0),
        (0, 0, -2, 2, 1, 46, 0, -24, 0),
        (2, 0, 0, 2, 2, -38, 0, 16, 0),
        (0, 0, 2, 2, 2, -31, 0, 13, 0),
        (0, 0, 2, 0, 0, 29, 0, 0, 0),
        (-2, 0, 1, 2, 2, 29, 0, -12, 0),
        (0, 0, 0, 2, 0, 26, 0, 0, 0),
        (-2, 0, 0, 2, 0, -22, 0, 0, 0),
        (0, 0, -1, 2, 1, 21, 0, -10, 0),
        (0, 2, 0, 0, 0, 17, -0.1, 0, 0),
        (2, 0, -1, 0, 1, 16, 0, -8, 0),
        (-2, 2, 0, 2, 2, -16, 0.1, 7, 0),
        (0, 1, 0, 0, 1, -15, 0, 9, 0),
        (-2, 0, 1, 0, 1, -13, 0, 7, 0),
        (0, -1, 0, 0, 1, -12, 0, 6, 0),
        (0, 0, 2, -2, 0, 11, 0, 0, 0),
        (2, 0, -1, 2, 1, -10, 0, 5, 0),
        (2, 0, 1, 2, 2, -8, 0, 3, 0),
        (0, 1, 0, 2, 2, 7, 0, -3, 0),
        (-2, 1, 1, 0, 0, -7, 0, 0, 0),
        (0, -1, 0, 2, 2, -7, 0, 3, 0),
        (2, 0, 0, 2, 1, -7, 0, 3, 0),
        (2, 0, 1, 0, 0, 6, 0, 0, 0),
        (-2, 0, 2, 2, 2, 6, 0, -3, 0),
        (-2, 0, 1, 2, 1, 6, 0, -3, 0),
        (2, 0, -2, 0, 1, -6, 0, 3, 0),
        (2, 0, 0, 0, 1, -6, 0, 3, 0),
        (0, -1, 1, 0, 0, 5, 0, 0, 0),
        (-2, -1, 0, 2, 1, -5, 0, 3, 0),
        (-2, 0, 0, 0, 1, -5, 0, 3, 0),
        (0, 0, 2, 2, 1, -5, 0, 3, 0),
        (-2, 0, 2, 0, 1, 4, 0, 0, 0),
        (-2, 1, 0, 2, 1, 4, 0, 0, 0),
        (0, 0, 1, -2, 0, 4, 0, 0, 0),
        (-1, 0, 1, 0, 0, -4, 0, 0, 0),
        (-2, 1, 0, 0, 0, -4, 0, 0, 0),
        (1, 0, 0, 0, 0, -4, 0, 0, 0),
        (0, 0, 1, 2, 0, 3, 0, 0, 0),
        (0, 0, -2, 2, 2, -3, 0, 0, 0),
        (-1, -1, 1, 0, 0, -3, 0, 0, 0),
        (0, 1, 1, 0, 0, -3, 0, 0, 0),
        (0, -1, 1, 2, 2, -3, 0, 0, 0),
        (2, -1, -1, 2, 2, -3, 0, 0, 0),
        (0, 0, 3, 2, 2, 3, 0, 0, 0),
        (2, -1, 0, 2, 2, -3, 0, 0, 0),
        )

    def nutation_and_obliquity(self, t:float, degrees:bool=False) -> float:
        """
        Nutation of the Earth's axis around it's 'mean' position.

        See:
     https://articles.adsabs.harvard.edu/full/seri/CeMec/0027//0000079.000.html
        """
        lm = self.coterminal_angle(self._poly(
            t, (134.96298, 477198.867398, 0.0086972, 1 / 56250)))
        ls = self.coterminal_angle(self._poly(
            t, (357.52772, 35999.05034, -0.0001603, -1 / 300000)))
        ff = self.coterminal_angle(self._poly(
            t, (93.27191, 483202.017538, -0.0036825, 1 / 327270)))
        dd = self.coterminal_angle(self._poly(
            t, (297.85036, 445267.111480, -0.0019142, 1 / 189474)))
        om = self.coterminal_angle(self._poly(
            t, (125.04452, -1934.136261, 0.0020708, 1 / 450000)))

        # W = LM*lm + LS*ls + F*ff + D*dd + OM*om
        # Where LM, LS, F, D, and OM are from the NUT periodic terms.
        # The nutation in longitude is a sum of terms of the form
        # (psi_sin + sin * T) * sin(W).
        # Where psi_sin and t_sin are from the NUT periodic terms and T is
        # the Julian time from J2000.
        # The obliquity in latitude is a sum of terms of the form
        # (eps_cos + cos * T) * cos(W).
        # Where eps_cos and t_cos are from the NUT periodic terms and T is
        # the Julian time from J2000.
        nut_sum = 0
        obl_sum = 0

        for D, LS, LM, F, OM, psi_sin, sin, eps_cos, cos in self.NUTATIONS:
            w = LM*lm + LS*ls + F*ff + D*dd + OM*om
            nut_sum += (psi_sin + sin * t) * self.sin_degrees(w)
            obl_sum += (eps_cos + cos * t) * self.cos_degrees(w)

        nut_sum /= 36000000
        obl_sum /= 36000000

        if degrees:
            nut_sum = self.coterminal_angle(math.degrees(nut_sum))
            obl_sum = self.coterminal_angle(math.degrees(obl_sum))

        return nut_sum, obl_sum


if __name__ == "__main__":
    tmn = TestMeeusNutation()
    jde = tmn.jd_from_moment(673221)
    t = tmn.julian_centuries(jde)
    nut_la, nut_oa = tmn.nutation_and_obliquity(t)
    nut_lb, nut_ob = tmn.nutation_and_obliquity(t, degrees=True)
    print(f"Radians: {nut_la}, {nut_oa}, Degrees: {nut_lb}, {nut_ob}")
    nut_la, nut_oa = tmn._nutation_and_obliquity(t)
    nut_lb, nut_ob = tmn._nutation_and_obliquity(t, degrees=True)
    print(f"Radians: {nut_la}, {nut_oa}, Degrees: {nut_lb}, {nut_ob}")
