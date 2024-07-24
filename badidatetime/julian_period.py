# -*- coding: utf-8 -*-
#
# badidatetime/julian_period.py
#
__docformat__ = "restructuredtext en"

import math


class JulianPeriod:
    """
    Implementation of the Julian Period.

    This Julian period is said to be named after Julius Cæsar Scaliger,
    the father of Josephus Justus Scaliger (Born: August 5, 1540, Agen,
    France), who invented the concept. The Julian Period is related to
    the Julian Calendar introduced by Julius Caesar, but it is not the
    same.

    See https://quasar.as.utexas.edu/BillInfo/JulianDatesG.html
        https://core2.gsfc.nasa.gov/time/julian.html
        https://www.tondering.dk/claus/cal/julperiod.php
    """
    # January 1, 2000, at 12:00 TT (Terrestrial Time)
    # TAI = TT - 32.184 seconds
    # January 1, 2000, 11:59:27.816 TAI (International Atomic Time)
    # January 1, 2000, 11:58:55.816 UTC (Coordinated Universal Time)
    # See: https://aa.usno.navy.mil/faq/sun_approx
    JULIAN_EPOCH = 0 # This is the actual Julian Epoch
    J2000 = 2451545.0 # TDT
    # Modified Julian day
    MJD = 2400000.5

    # 28 (solar cycle) × 19 (lunar cycle) × 15 (indiction cycle) = 7980 years
    JULIAN_PERIOD = 7980
    JULIAN_YEAR = 365.25

    JD_EPOCH = -1721424.5
    MJD_EPOCH = 678576

    JANUARY = 1
    FEBRUARY = 2
    MARCH = 3
    APRIL = 4
    MAY = 5
    JUNE = 6
    JULY = 7
    AUGUEST = 8
    SEPTEMBER = 9
    OCTOBER = 10
    NOVEMBER = 11
    DECEMBER = 12
    JULIAN_MONTHS = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    def julian_centuries(self, jde):
        """
        Calculate the Julian centuries from the Julian day.
        """
        return (jde - self.J2000) / 36525

    def julian_millennia(self, jde:float) -> float:
        """
        Calculate the Julian millennia from the Julian day.
        """
        return (jde - self.J2000) / 365250
