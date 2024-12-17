# -*- coding: utf-8 -*-
#
# badidatetime/julian_period.py
#
__docformat__ = "restructuredtext en"


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
    # 28 (solar cycle) × 19 (lunar cycle) × 15 (indiction cycle) = 7980 years
    #JULIAN_PERIOD = 7980
    JULIAN_YEAR = 365.25
    JULIAN_LEAP_YEAR = lambda self, year: year % 4 == 0

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
