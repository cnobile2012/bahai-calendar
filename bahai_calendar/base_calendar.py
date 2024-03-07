# -*- coding: utf-8 -*-
#
# bahai_calendar/base_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from functools import reduce
from operator import mul

from bahai_calendar.julian_period import JulianPeriod
from bahai_calendar.calendar_tables import CalendarTables


class BaseCalendar(CalendarTables, JulianPeriod):
    """
    Basic functionality used with all calenders.

    R.D. = Fixed date—elapsed days since the onset of Monday, January 1, 1
           (Gregorian)
    U.T. = Mean solar time at Greenwich, England (0◦ meridian), reckoned
           from midnight; sometimes g.m.t., Greenwich Mean Time
    Moment = We call an r.d. that has a fractional part giving the time of
             day a “moment.”

    Transformations between Time Systems:
    https://gssc.esa.int/navipedia/index.php/Transformations_between_Time_Systems
    """
    #(defun hr (x)
    #  ;; TYPE real -> duration
    #  ;; x hours.
    #  (/ x 24))
    HR = lambda self, x: x / 24

    #(defun mn (x)
    #  ;; TYPE real -> duration
    #  ;; x minutes.
    #  (/ x 24 60))
    MN = lambda self, x: x / 24 / 60

    #(defun sec (x)
    #  ;; TYPE real -> duration
    #  ;; x seconds.
    #  (/ x 24 60 60))
    SEC = lambda self, x: x / 24 / 60 / 60

    #(defun mins (x)
    #  ;; TYPE real -> angle
    #  ;; x arcminutes
    #  (/ x 60))
    MINS = lambda self, x: x / 60

    #(defun secs (x)
    #  ;; TYPE real -> angle
    #  ;; x arcseconds
    #  (/ x 3600))
    SECS = lambda self, x: x / 3600

    #(defun angle (d m s)
    #  ;; TYPE (integer integer real) -> angle
    #  ;; d degrees, m arcminutes, s arcseconds.
    #  (+ d (/ (+ m (/ s 60)) 60)))
    ANGLE = lambda self, d, m, s: d + (m + s / 60) / 60 # 0 - 360

    #(defun amod (x y)
    #  ;; TYPE (integer nonzero-integer) -> integer
    #  ;; The value of ($x$ mod $y$) with $y$ instead of 0.
    #  (+ y (mod x (- y))))
    AMOD = lambda self, x, y: y + x % -y

    #(defun mod3 (x a b)
    #  ;; TYPE (real real real) -> real
    #  ;; The value of x shifted into the range [a..b). Returns x if a=b.
    #  (if (= a b)
    #      x
    #    (+ a (mod (- x a) (- b a)))))
    MOD3 = lambda self, x, a, b : x if a == b else (
        a + math.fmod((x - a), (b - a)))

    #(defconstant mean-synodic-month
    #  ;; TYPE duration
    #  29.530588861L0)
    MEAN_SYNODIC_MONTH = 29.530588861

    #(defun time-from-moment (tee)
    #  ;; TYPE moment -> time
    #  ;; Time from moment tee.
    #  (mod tee 1))
    TIME_FROM_MOMENT = lambda self, tee: math.fmod(tee, 1)

    # (defun rd (tee)
    #  ;; TYPE moment -> moment
    #  ;; Identity function for fixed dates/moments. If internal timekeeping
    #  ;; is shifted, change epoch to be RD date of origin of internal count.
    #  ;; epoch should be an integer.
    #  (let* ((epoch 0))
    #    (- tee epoch)))
    # Unless the epoch is changed to something other than 0 there is no need
    # to implement this function.
    #RD = tee - 0

    PARTIAL_DAY_TO_HOURS = lambda self, x: (x % 1) * 24
    PARTIAL_HOUR_TO_MINUTE = lambda self, x: (x % 1) * 60
    PARTIAL_MINUTE_TO_SECOND = PARTIAL_HOUR_TO_MINUTE

    MEAN_TROPICAL_YEAR = 365.242189
    MEAN_SIDEREAL_YEAR = 365.25636

    MORNING = True
    EVENING = False
    # Seasons set to degrees.
    SPRING = 0
    SUMMER = 90
    AUTUMN = 180
    WINTER = 270
    SUN_OFFSET = 50/60

    def __init__(self):
        self._time = None
        self._nutation = (0, 0, False)
        self._obliquity = (0, 0, False)

    def parse_datetime(self, dt:datetime.datetime) -> None:
        self.time_representation = (dt.hour, dt.minute, dt.second)

    @property
    def time_representation(self):
        return self._time

    @time_representation.setter
    def time_representation(self, representation):
        self._time = representation

    #
    # Meeus Astronimical Algorithms
    #

    def mean_sidereal_time_greenwich(self, jde:float,
                                     reduce:bool=False) -> float:
        """
        Mean sidereal time at Greenwich.

        Meeus--AA p.88
        """
        t = self.julian_centuries(jde)
        deg = (280.46061837 + 360.98564736629 * (jde - self.J2000) +
               0.000387933 * t**2 - t**3 / 38710000)
        return self.coterminal_angle(deg) if reduce else deg

    def apparent_sidereal_time_greenwich(self, jde:float) -> float:
        """
        The apparent sidereal time, or the Greenwich hour angle of the
        true vernal equinox.

        Meeus--AA ch12 p88
        """
        t0 = self.mean_sidereal_time_greenwich(jde)
        eps = self.true_obliquity_of_ecliptic(jde)
        d_psi = self.astronomical_nutation(jde)
        return self.coterminal_angle(t0 + d_psi * self.cos_degrees(eps))

    def true_obliquity_of_ecliptic(self, jde:float) -> float:
        """
        The obliquity of the ecliptic, or inclination of the Earth’s axis
        of rotation, is the angle between the equator and the ecliptic.

        Convert lots of things:
        https://www.xconvert.com/unit-converter/arcseconds-to-degrees
        """
        t = self.julian_centuries(jde)
        u = t / 100
        # We convert arcseconds to degrees then do the polonomial.
        mean_ob = self._poly(u, (84381.448 / 3600, -4680.93 / 3600,
                                 -1.55 / 3600, 1999.25 / 3600, -51.38 / 3600,
                                 -249.67 / 3600, -39.05 / 3600, 7.12 / 3600,
                                 27.87 / 3600, 5.79 / 3600, 2.45 / 3600))
        return mean_ob + self.astronomical_obliquity(t)

    def approx_local_hour_angle(self, jde:float, lat:float,
                                offset:float=SUN_OFFSET) -> float:
        """
        Approximate local hour angle, measured westwards from the South
        in degrees.

        :param jde: Julian day.
        :type jde: float
        :param lat: Latitude
        :type lat: float
        :param offset: A constant “standard” altitude, i.e., the geometric
                       altitude of the center of the body at the time of
                       apparent rising or setting, namely,
                       h0 = -0°34’ = -0°5667 for stars and planets;
                       h0 = -0°50' = -0°8333 for the Sun.
        :return: The approximat local hour angle.
        :rtype: float

        .. note::

           1. Meeus--AA p.92, 101, 102
           2. If result of equation is negative then add 360°
              (6.283185307179586 radians). If result is greater than
              360° then subtract 360° (6.283185307179586 radians).
              https://www.quora.com/How-do-I-calculate-the-hour-angle
        """
        delta = self.apparent_declination(jde)
        cos_h0 = ((self.sin_degrees(-offset) - self.sin_degrees(lat) *
                   self.sin_degrees(delta)) / (self.cos_degrees(lat) *
                                               self.cos_degrees(delta)))

        if cos_h0 < -1:
            cos_h0 -= 6.283185307179586
        elif cosH0 > 1:
            cos_h0 += 6.283185307179586
        else:
            cos_h0 = math.acos(cos_h0)

        return math.degrees(cos_h0)

    def right_ascension(self, ):
        """
        Right ascension is measured (from 0 to 24 hours, sometimes from 0°
        to 360°) from the vernal equinox, positive to the east, along the
        celestial equator.

        Meeus--AA p.93 13.3
        """
        return

    def apparent_declination(self, t:float) -> float:
        """
        Declination is measured (from 0° to +90°) from the equator, positive
        to the north, negative to the south.

        :param t: Julian centuries.
        :type t: float
        :return: The apparent declination of the sun.
        :rtype: float

        .. note::

           Meeus--AA p.93 13.4
        """
        om = self.ascenting_node_longitude(t)
        eps = self.obliquity_of_ecliptic(t) + 0.00256 - self.cos_degrees(om)
        lam = self.apparent_longitude(t)
        return math.degrees(math.asin(self.sin_degrees(eps) *
                                      self.sin_degrees(lam)))

    def ascenting_node_longitude(self, t):
        """
        """
        return

    def azimuth(self, ):
        """
        Azimuth, measured westward from the Souh in degrees.

        Meeus--AA p.93 13.5
        """
        return

    def altitude(self, ):
        """
        Altitude, positive above the horizon, negative below in degrees.

        Meeus--AA p.93 13.6
        """
        return

    def _heliocentric_ecliptical_longitude(self, t:float) -> float:
        """
        Find the heliocentric ecliptical longitude.

        :param t: The moment in time referenced to J2000.
        :type t: float
        :return: Longitude in radians.
        :rtype: float
        """
        func = lambda a, b, c: a * math.cos(b + c * t)
        l0 = self._sigma((self.L0_A, self.L0_B, self.L0_C), func)
        l1 = self._sigma((self.L1_A, self.L1_B, self.L1_C), func)
        l2 = self._sigma((self.L2_A, self.L2_B, self.L2_C), func)
        l3 = self._sigma((self.L3_A, self.L3_B, self.L3_C), func)
        l4 = self._sigma((self.L4_A, self.L4_B, self.L4_C), func)
        l5 = self._sigma((self.L5_A, self.L5_B, self.L5_C), func)
        return self._poly(t, (l0, l1, l2, l3, l4, l5)) / 10**8

    def _heliocentric_ecliptical_latitude(self, t:float) -> float:
        """
        Find the heliocentric ecliptical latitude.

        :param t: The moment in time referenced to J2000.
        :type t: float
        :return: Latitude in radians.
        :rtype: float
        """
        func = lambda a, b, c: a * math.cos(b + c * t)
        b0 = self._sigma((self.B0_A, self.B0_B, self.B0_C), func)
        b1 = self._sigma((self.B1_A, self.B1_B, self.B1_C), func)
        return self._poly(t, (b0, b1)) / 10**8

    def _radius_vector(self, t:float) -> float:
        """
        Find the radius vector.

        :param t: The moment in time referenced to J2000.
        :type t: float
        :return: Radius vector in radians.
        :rtype: float
        """
        func = lambda a, b, c: a * math.cos(b + c * t)
        r0 = self._sigma((self.R0_A, self.R0_B, self.R0_C), func)
        r1 = self._sigma((self.R1_A, self.R1_B, self.R1_C), func)
        r2 = self._sigma((self.R2_A, self.R2_B, self.R2_C), func)
        r3 = self._sigma((self.R3_A, self.R3_B, self.R3_C), func)
        r4 = self._sigma((self.R4_A, self.R4_B, self.R4_C), func)
        return self._poly(t, (r0, r1, r2, r3, r4)) / 10**8

    def solar_longitude(self, moment):
        jde = self.jd_from_moment(moment)
        t = (jde - self.J2000) / 365250
        l = self._heliocentric_ecliptical_longitude(t)
        # Convert to FK5 notation
        t1 = t * 10
        l1 = l - math.radians(1.397) * t1 - math.radians(0.00031) * t1**2
        b = self._heliocentric_ecliptical_latitude(t)
        dl = -0.09033 + 0.03916 * (math.cos(l1) + math.sin(l1)) * math.tan(b)
        db = 0.03916 * (math.cos(l1) - math.sin(l1))

        return dl, db

    def astronomical_nutation(self, jde:float, *, degrees:bool=True) -> float:
        """
        Nutation of the Earth's axis around it's 'mean' position.
        """
        if jde != self._nutation[0] or degrees is not self._nutation[2]:
            t = self.julian_centuries(jde)
            nut_sum, obl_sum = self._nutation_and_obliquity(t, )
            self._nutation = (jde, nut_sum, degrees)
            self._obliquity = (jde, obl_sum, degrees)

        return self._nutation[1]

    def astronomical_obliquity(self, jde:float, *, degrees:bool=True) -> float:
        """
        """
        if jde != self._obliquity[0] or degrees is not self._obliquity[2]:
            t = self.julian_centuries(jde)
            nut_sum, obl_sum = self._nutation_and_obliquity(t, degrees=degrees)
            self._nutation = (jde, nut_sum)
            self._obliquity = (jde, obl_sum)

        return self._obliquity[1]

    def _nutation_and_obliquity(self, t:float, degrees:bool=False) -> float:
        """
        Nutation of the Earth's axis around it's 'mean' position.

        See:
     https://articles.adsabs.harvard.edu/full/seri/CeMec/0027//0000079.000.html
        """
        #l0 = self.coterminal_angle(self._poly(
        #    t, (280.46646, 36000.76983, 0.0003032)))
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

        for LM, LS, F, D, OM, day, psi_sin, sin, eps_cos, cos in self.NUT:
            w = LM*lm + LS*ls + F*ff + D*dd + OM*om
            nut_sum += (psi_sin + sin * t) * self.sin_degrees(w)
            obl_sum += (eps_cos + cos * t) * self.cos_degrees(w)

        nut_sum /= 36000000
        obl_sum /= 36000000

        if degrees:
            nut_sum = self.coterminal_angle(math.degrees(nut_sum))
            obl_sum = self.coterminal_angle(math.degrees(obl_sum))

        return nut_sum, obl_sum

    #
    # Time and Astronomy (Time)
    #

    #greenwich
    #acre
    #direction
    #arctan

    def zone_from_longitude(self, phi):
        """
        used

        (defun zone-from-longitude (phi)
          ;; TYPE circle -> duration
          ;; Difference between UT and local mean time at longitude
          ;; phi as a fraction of a day.
          (/ phi (deg 360)))
        """
        return phi / 360

    def universal_from_local(self, tee_ell):
        """
        used

        (defun universal-from-local (tee_ell location)
          ;; TYPE (moment location) -> moment
          ;; Universal time from local tee_ell at location.
          (- tee_ell (zone-from-longitude (longitude location))))
        """
        return tee_ell - self.zone_from_longitude(self.longitude)

    def local_from_universal(self, tee_rom_u):
        """
        (defun local-from-universal (tee_rom-u location)
          ;; TYPE (moment location) -> moment
          ;; Local time from universal tee_rom-u at location.
          (+ tee_rom-u (zone-from-longitude (longitude location))))
        """
        return tee_rom_u + self.zone_from_longitude(self.longitude)

    def standard_from_universal(self, tee_rom_u):
        """
        (Fixed bug in LISP code)

        (defun standard-from-universal (tee_rom-u location)
          ;; TYPE (moment location) -> moment
          ;; Standard time from tee_rom-u in universal time at location.
          (+ tee_rom-u (zone location)))
        """
        return tee_rom_u + self.HR(self.zone)

    def universal_from_standard(self, tee_rom_s):
        """
        used (Fixed bug in LISP code)

        (defun universal-from-standard (tee_rom-s location)
          ;; TYPE (moment location) -> moment
          ;; Universal time from tee_rom-s in standard time at location.
          (- tee_rom-s (zone location)))
        """
        return tee_rom_s - self.HR(self.zone)

    def standard_from_local(self, tee_ell):
        """
        used

        (defun standard-from-local (tee_ell location)
          ;; TYPE (moment location) -> moment
          ;; Standard time from local tee_ell at location.
          (standard-from-universal (universal-from-local tee_ell location)
          location))
        """
        return self.standard_from_universal(self.universal_from_local(tee_ell))

    def local_from_standard(self, tee_rom_s):
        """
        (defun local-from-standard (tee_rom-s location)
          ;; TYPE (moment location) -> moment
          ;; Local time from standard tee_rom-s at location.
          (local-from-universal
           (universal-from-standard tee_rom-s location)
           location))
        """
        return self.local_from_universal(self.universal_from_standard(
            tee_rom_s))

    def ephemeris_correction(self, tee):
        '''
        used

        (defun ephemeris-correction (tee)
          ;; TYPE moment -> fraction-of-day
          ;; Dynamical Time minus Universal Time (in days) for moment tee.
          ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
          ;; Willmann-Bell (1991) for years 1600-1986 and from polynomials
          ;; on the NASA Eclipse web site for other years.
          (let* ((year (gregorian-year-from-fixed (floor tee)))
                 (c (/ (gregorian-date-difference
                        (gregorian-date 1900 january 1)
                        (gregorian-date year july 1))
                       36525))
                 (c2051 (* 1/86400
                           (+ -20 (* 32 (expt (/ (- year 1820) 100) 2))
                              (* 0.5628L0 (- 2150 year)))))
                 (y2000 (- year 2000))
                 (c2006 (* 1/86400
                           (poly y2000
                               (list 62.92L0 0.32217L0 0.005589L0))))
                 (c1987 (* 1/86400
                           (poly y2000
                                 (list 63.86L0 0.3345L0 -0.060374L0
                                 0.0017275L0 0.000651814L0 0.00002373599L0))))
                 (c1900 (poly c
                              (list -0.00002L0 0.000297L0 0.025184L0
                              -0.181133L0 0.553040L0 -0.861938L0 0.677066L0
                              -0.212591L0)))
                 (c1800 (poly c
                              (list -0.000009L0 0.003844L0 0.083563L0
                              0.865736L0 4.867575L0 15.845535L0 31.332267L0
                              38.291999L0 28.316289L0 11.636204L0 2.043794L0)))
                 (y1700 (- year 1700))
                 (c1700 (* 1/86400
                           (poly y1700
                                 (list 8.118780842L0 -0.005092142L0
                                 0.003336121L0 -0.0000266484L0))))
                 (y1600 (- year 1600))
                 (c1600 (* 1/86400
                           (poly y1600
                                 (list 120 -0.9808L0 -0.01532L0
                                 0.000140272128L0))))
                 (y1000 (/ (- year 1000) 100L0))
                 (c500 (* 1/86400
                          (poly y1000
                                (list 1574.2L0 -556.01L0 71.23472L0 0.319781L0
                                -0.8503463L0 -0.005050998L0 0.0083572073L0))))
                 (y0 (/ year 100L0))
                 (c0 (* 1/86400
                        (poly y0
                              (list 10583.6L0 -1014.41L0 33.78311L0
                              -5.952053L0 -0.1798452L0 0.022174192L0
                              0.0090316521L0))))
                 (y1820 (/ (- year 1820) 100L0))
                 (other (* 1/86400
                           (poly y1820 (list -20 0 32)))))
            (cond ((<= 2051 year 2150) c2051)
                  ((<= 2006 year 2050) c2006)
                  ((<= 1987 year 2005) c1987)
                  ((<= 1900 year 1986) c1900)
                  ((<= 1800 year 1899) c1800)
                  ((<= 1700 year 1799) c1700)
                  ((<= 1600 year 1699) c1600)
                  ((<= 500 year 1599) c500)
                  ((< -500 year 500) c0)
                  (t other))))
        '''
        from .gregorian_calendar import GregorianCalendar
        gc = GregorianCalendar()

        year = gc.gregorian_year_from_fixed(math.floor(tee))
        y2000 = year - 2000
        c = gc.gregorian_date_difference(
            (1900, gc.JANUARY, 1), (year, gc.JULY, 1)) / 36525

        if 2051 <= year <= 2150:
            result = 1/86400 * (-20 + (32 * (((year - 1820) / 100) ** 2))
                                + 0.5628 * (2150 - year))
        elif 2006 <= year <= 2050:
            result = 1/86400 * self._poly(y2000, (62.92, 0.32217, 0.005589))
        elif 1987 <= year <= 2005:
            result = 1/86400 * self._poly(y2000,
                                          (63.86, 0.3345, -0.060374, 0.0017275,
                                           0.000651814, 0.00002373599))
        elif 1900 <= year <= 1986:
            result = self._poly(c, (-0.00002, 0.000297, 0.025184, -0.181133,
                                    0.553040, -0.861938, 0.677066, -0.212591))
        elif 1800 <= year <= 1899:
            result = self._poly(c, (-0.000009, 0.003844, 0.083563, 0.865736,
                                    4.867575, 15.845535, 31.332267, 38.291999,
                                    28.316289, 11.636204, 2.043794))
        elif 1700 <= year <= 1799:
            y1700 = year - 1700
            result = 1/86400 * self._poly(
                y1700, (8.118780842, -0.005092142, 0.003336121, -0.0000266484))
        elif 1600 <= year <= 1699:
            y1600 = year - 1600
            result = 1/86400 * self._poly(
               y1600, (120, -0.9808, -0.01532, 0.000140272128))
        elif 500 <= year <= 1599:
            y1000 = (year - 1000) / 100
            result = 1/86400 * self._poly(
                y1000, (1574.2, -556.01, 71.23472, 0.319781, -0.8503463,
                        -0.005050998, 0.0083572073))
        elif -500 < year < 500:
            y0 = year / 100
            result = 1/86400 * self._poly(
                y0, (10583.6, -1014.41, 33.78311, -5.952053, -0.1798452,
                     0.022174192, 0.0090316521))
        else:
            y1820 = (year - 1820) / 100
            result = 1/86400 * self._poly(y1820, (-20, 0, 32))

        return result

    def dynamical_from_universal(self, tee_rom_u):
        """
        used

        (defun dynamical-from-universal (tee_rom-u)
          ;; TYPE moment -> moment
          ;; Dynamical time at Universal moment tee_rom-u.
          (+ tee_rom-u (ephemeris-correction tee_rom-u)))
        """
        return tee_rom_u + self.ephemeris_correction(tee_rom_u)

    def universal_from_dynamical(self, tee):
        """
        (defun universal-from-dynamical (tee)
          ;; TYPE moment -> moment
          ;; Universal moment from Dynamical time tee.
          (- tee (ephemeris-correction tee)))
        """
        return tee - self.ephemeris_correction(tee)

    def equation_of_time(self, tee):
        '''
        (defun equation-of-time (tee)
          ;; TYPE moment -> fraction-of-day
          ;; Equation of time (as fraction of day) for moment tee.
          ;; Adapted from "Astronomical Algorithms" by Jean Meeus,
          ;; Willmann-Bell, 2nd edn., 1998, p. 185.
          (let* ((c (julian-centuries tee))
                 (lambda
                   (poly c
                         (deg (list 280.46645L0 36000.76983L0 0.0003032L0))))
                 (anomaly
                  (poly c
                        (deg (list 357.52910L0 35999.05030L0 -0.0001559L0
                                   -0.00000048L0))))
                 (eccentricity
                  (poly c
                        (list 0.016708617L0 -0.000042037L0 -0.0000001236L0)))
                 (varepsilon (obliquity tee))
                 (y (expt (tan-degrees (/ varepsilon 2)) 2))
                 (equation
                  (* (/ 1 2 pi)
                     (+ (* y (sin-degrees (* 2 lambda)))
                        (* -2 eccentricity (sin-degrees anomaly))
                        (* 4 eccentricity y (sin-degrees anomaly)
                           (cos-degrees (* 2 lambda)))
                        (* -0.5L0 y y (sin-degrees (* 4 lambda)))
                        (* -1.25L0 eccentricity eccentricity
                           (sin-degrees (* 2 anomaly)))))))
            (* (sign equation) (min (abs equation) (hr 12L0)))))
        '''
        c = self.julian_centuries_in_rd(tee)
        lam = self._poly(c, (280.46645, 36000.76983, 0.0003032))
        anomaly = self._poly(c, (357.52910, 35999.05030, -0.0001559,
                                 -0.00000048))
        eccentricity = self._poly(c, (0.016708617, -0.000042037,
                                      -0.0000001236))
        varepsilon = self.obliquity(tee)
        y = self.tan_degrees(varepsilon / 2) ** 2
        equation = ((0.5 / math.pi) *
                    ((y * self.sin_degrees(2 * lam)) +
                     (-2 * eccentricity * self.sin_degrees(anomaly)) +
                     (4 * eccentricity * y * self.sin_degrees(anomaly) *
                      self.cos_degrees(2 * lam)) +
                     (-0.5 * y * y * self.sin_degrees(4 * lam)) +
                     (-1.25 * eccentricity * eccentricity *
                      self.sin_degrees(2 * anomaly))))
        return math.sin(equation) * min(abs(equation), self.HR(12))

    def apparent_from_local(self, tee_ell):
        """
        Apparent time is the time measured by devices such as a sundial.

        (defun apparent-from-local (tee_ell location)
          ;; TYPE (moment location) -> moment
          ;; Sundial time from local time tee_ell at location.
          (+ tee_ell (equation-of-time
                      (universal-from-local tee_ell location))))
        """
        return tee_ell + self.equation_of_time(
            self.universal_from_local(tee_ell))

    def local_from_apparent(self, tee):
        """
        used

        (defun local-from-apparent (tee location)
          ;; TYPE (moment location) -> moment
          ;; Local time from sundial time tee at location.
          (- tee (equation-of-time (universal-from-local tee location))))
        """
        return tee - self.equation_of_time(self.universal_from_local(tee))

    #apparent-from-universal
    #universal-from-apparent
    #midnight
    #midday
    #sidereal-from-moment

    #
    # Time and Astronomy (The Year)
    #

    def declination(self, tee, lat, lon):
        """
        (defun declination (tee beta lambda)
          ;; TYPE (moment half-circle circle) -> angle
          ;; Declination at moment UT tee of object at
          ;; latitude beta and longitude lambda.
          (let* ((varepsilon (obliquity tee)))
            (arcsin-degrees (+ (* (sin-degrees beta)
                                  (cos-degrees varepsilon))
                               (* (cos-degrees beta)
                                  (sin-degrees varepsilon)
                                  (sin-degrees lambda))))))
        """
        varepsilon = self.obliquity(tee)
        return ((self.sin_degrees(lat) * self.cos_degrees(varepsilon)) +
                (self.cos_degrees(lat) * self.sin_degrees(varepsilon) *
                 self.sin_degrees(lon)))

    #mean-tropical-year
    #mean-sidereal-year

    def approx_julian_day_for_equinoxes_or_solstices(self, g_year:int,
                                                     lam:int=SPRING) -> float:
        """
        Find the approximate Julian day for the equinoxes or solstices.

        See: Astronomical Algorithms, 1998, by Jean Meeus Ch 27 p.177
        """
        if g_year <= 1000:
            y = g_year / 1000

            if lam == self.SPRING:
                jde = self._poly(y, (1721139.29189, 365242.13740, 0.06134,
                                     0.00111, -0.00071))
            elif lam == self.SUMMER:
                jde = self._poly(y, (1721233.25401, 365241.72562, -0.05323,
                                     0.00907, 0.00025))
            elif lam == self.AUTUMN:
                jde = self._poly(y, (1721325.70455, 365242.49558, -0.11677,
                                     -0.00297, 0.00074))
            else: # lam == self.WINTER:
                jde = self._poly(y, (1721414.39987, 365242.88257, -0.00769,
                                     -0.00933, -0.00006))
        else:
            y = (g_year - 2000) / 1000

            if lam == self.SPRING:
                jde = self._poly(y, (2451623.80984, 365242.37404, 0.05169,
                                     -0.00411, -0.00057))
            elif lam == self.SUMMER:
                jde = self._poly(y, (2451716.56767, 365241.62603, 0.00325,
                                     0.00888, -0.00030))
            elif lam == self.AUTUMN:
                jde = self._poly(y, (2451810.21715, 365242.01767, -0.11575,
                                     0.00337, 0.00078))
            else: # lam == self.WINTER:
                jde = self._poly(y, (2451900.05952, 365242.74049, -0.06223,
                                     -0.00823, 0.00032))

        return jde

    def alt_solar_longitude(self, tee):
        """
        Find the solar longitude in degrees (0 - 360).

        See the following links:
        https://aa.usno.navy.mil/faq/sun_approx
        https://squarewidget.com/solar-coordinates/
        """
        jd = self.jd_from_moment(tee)
        d = jd - self.jd_from_moment(self.RD_J2000)
        g = self.coterminal_angle(357.529 + 0.98560028 * d)
        q = self.coterminal_angle(280.459 + 0.98564736 * d)
        return q + 1.915 * math.sin(g) + 0.02 * math.sin(2 * g)

    def nutation(self, tee):
        """
        used

        (defun nutation (tee)
          ;; TYPE moment -> circle
          ;; Longitudinal nutation at moment tee.
          (let* ((c       ; moment in Julian centuries
                  (julian-centuries tee))
                 (cap-A (poly c (deg (list 124.90L0 -1934.134L0 0.002063L0))))
                 (cap-B (poly c (deg (list 201.11L0 72001.5377L0 0.00057L0)))))
             (+ (* (deg -0.004778L0) (sin-degrees cap-A))
                (* (deg -0.0003667L0) (sin-degrees cap-B)))))
        """
        c = self.julian_centuries_in_rd(tee)
        cap_a = self._poly(c, (124.90, -1934.134, 0.002063))
        cap_b = self._poly(c, (201.11, 72001.5377, 0.00057))
        return (-0.004778 * self.sin_degrees(cap_a) +
                -0.0003667 * self.sin_degrees(cap_b))

    def obliquity(self, tee):
        """
        Obliquity is the inclination of the Earth’s equator. The value
        of this inclination, called the obliquity, varies in a
        100000-year cycle, ranging from 24.2◦ 10000 years ago to 22.6◦
        in another 10000 years.

        (defun obliquity (tee)
          ;; TYPE moment -> angle
          ;; Obliquity of ecliptic at moment tee.
          (let* ((c (julian-centuries tee)))
            (+ (angle 23 26 21.448L0)
               (poly c (list 0L0
                             (angle 0 0 -46.8150L0)
                             (angle 0 0 -0.00059L0)
                             (angle 0 0 0.001813L0))))))
        """
        c = self.julian_centuries_in_rd(tee)
        angles = (0, self.ANGLE(0, 0, -46.815), self.ANGLE(0, 0, -0.00059),
                  self.ANGLE(0, 0, 0.001813))
        return self.ANGLE(23, 26, 21.448) + self._poly(c, angles)

    def aberration(self, tee):
        """
        used

        (defun aberration (tee)
          ;; TYPE moment -> circle
          ;; Aberration at moment tee.
          (let* ((c       ; moment in Julian centuries
                  (julian-centuries tee)))
            (- (* (deg 0.0000974L0)
                  (cos-degrees (+ (deg 177.63L0) (* (deg 35999.01848L0) c))))
               (deg 0.005575L0))))
        """
        c = self.julian_centuries_in_rd(tee)
        return (0.0000974 * self.cos_degrees(177.63 + 35999.01848 * c)
                - 0.005575)

    def astronomical_aberration(self, tee):
        """
        """

        return

    #solar-longitude-after
    #season-in-gregorian
    #urbana-winter
    #precession
    #sideread-solar-longitude
    #solar-altitude

    # Here α is the sun’s right ascension, δ is its declination,
    # and H is the local sidereal hour angle.
    # Greek letters phi (φ) for latitude and lambda (λ) for longitude.

    ################################
    # Astronomical Solar Calandars #
    ################################

    def estimate_prior_solar_longitude(self, lam:int, tee:int) -> float:
        """
        used

        https://farside.ph.utexas.edu/books/Syntaxis/Almagest/node34.html

        (defun estimate-prior-solar-longitude (lambda tee)
          ;; TYPE (season moment) -> moment
          ;; Approximate moment at or before tee when solar
          ;; longitude just exceeded lambda degrees.
          (let* ((rate            ; Mean change of one degree.
                  (/ mean-tropical-year (deg 360)))
                 (tau             ; First approximation.
                  (- tee
                     (* rate (mod (- (solar-longitude tee)
                                     lambda)
                              360))))
                 (cap-Delta       ; Difference in longitude.
                  (mod3 (- (solar-longitude tau) lambda)
                        -180 180)))
            (min tee (- tau (* rate cap-Delta)))))
        """
        rate = self.MEAN_TROPICAL_YEAR / 360 # always = 1.01456163611111111111
        tau = tee - rate * (math.fmod(
            (self.alt_solar_longitude(tee) - lam), 360))
        cap_delta = self.MOD3(self.alt_solar_longitude(tau) - lam, -180, 180)
        return min(tee, tau - rate * cap_delta)

    def find_moment_of_equinoxes_or_solstices(self, tee:int,
                                              lam:int=SPRING) -> float:
        """
        With the rd moment and time of year find an equinoxe or solstice.
        """
        from .gregorian_calendar import GregorianCalendar
        gc = GregorianCalendar()
        year = gc.gregorian_year_from_fixed(tee)
        jde = self.approx_julian_day_for_equinoxes_or_solstices(year, lam)
        t = (jde - self.J2000) / 36525
        w = 35999.373 * t - 2.47
        dl = 1 + 0.0334 * self.cos_degrees(w + 0.0007) * self.cos_degrees(2*w)
        a0 = (485, 203, 199, 182, 156, 136, 77, 74, 70, 58, 52, 50,
              45, 44, 29, 18, 17, 16, 14, 12, 12, 12, 9, 8)
        b0 = (324.96, 337.23, 342.08, 27.85, 73.14, 171.52, 222.54, 296.72,
              243.58, 119.81, 297.17, 21.02, 247.54, 325.15, 60.93, 155.12,
              288.79, 198.04, 199.76, 95.39, 287.11, 320.81, 227.73, 15.45)
        c0 = (1934.136, 32964.467, 20.186, 445267.112, 45036.886, 22518.443,
              65928.934, 3034.906, 9037.513, 33718.147, 150.678, 2281.226,
              29929.562, 31555.956, 4443.417, 67555.328, 4562.452, 62894.029,
              31436.921, 14577.848, 31931.756, 34777.259, 1222.114, 16859.074)
        s = self._sigma(
            (a0, b0, c0), lambda a, b, c: a * self.cos_degrees(b + c * t))
        return self.moment_from_jd(jde + (0.00001 * s) / dl)

    #
    # Time and Astronomy (The Month)
    #

    #mean-synodic-month
    #nth-new-moon
    #new-moon-before
    #new-moon-at-or-after
    #lunar-longitude
    #mean-lunar-longitude
    #lunar-elongation
    #solar-anomaly
    #lunar-anomaly
    #moon-node
    #lunar-node
    #sidereal-lunar-longitude
    #lunar-phase
    #lunar-phase-at-or-before
    #lunar-phase-at-or-after
    #new
    #first-quarter
    #full
    #last-quarter
    #lunar-latitude
    #lunar-altitude
    #lunar-distance
    #lunar-parallax
    #topocentric-lunar-altitude

    #
    # Time and Astronomy (Rising and Setting of the Sun and Moon)
    #

    def approx_moment_of_depression(self, tee:float, alpha:float,
                                    early:bool) -> float:
        """
        used

        (defun approx-moment-of-depression (tee location alpha early?)
          ;; TYPE (moment location half-circle boolean) - moment
          ;; Moment in local time near tee when depression angle of sun
          ;; is alpha (negative if above horizon) at location; early? is
          ;; true when morning event is sought and false for evening.
          ;; Returns bogus if depression angle is not reached.
          (let* ((try (sine-offset tee location alpha))
                 (date (fixed-from-moment tee))
                 (alt (if (>= alpha 0)
                          (if early? date (1+ date))
                        (+ date (hr 12))))
                 (value (if (> (abs try) 1)
                            (sine-offset alt location alpha) try)))
           (if (<= (abs value) 1)          ; Event occurs
               (let* ((offset (mod3 (/ (arcsin-degrees value) (deg 360))
                                    (hr -12) (hr 12))))
                 (local-from-apparent
                  (+ date
                     (if early?
                         (- (hr 6) offset)
                       (+ (hr 18) offset)))
                  location))
             bogus)))
        """
        result = None
        try_ = self.sine_offset(tee, alpha)
        date = self.fixed_from_moment(tee)

        if alpha >= 0:
            alt = date if early else date + 1
        else:
            alt = date + self.HR(12)

        value = self.sine_offset(alt, alpha) if abs(try_) > 1 else try_

        if abs(value) <= 1:
            offset = self.MOD3(self.arcsin_degrees(value) / 360,
                               self.HR(-12), self.HR(12))
            date += self.HR(6) - offset if early else self.HR(18) + offset
            result = self.local_from_apparent(date)

        return result

    def sine_offset(self, tee, alpha):
        """
        (defun sine-offset (tee location alpha)
          ;; TYPE (moment location half-circle) -> real
          ;; Sine of angle between position of sun at local time tee and
          ;; when its depression is alpha at location.
          ;; Out of range when it does not occur.
          (let* ((phi (latitude location))
                 (tee-prime (universal-from-local tee location))
                 (delta                 ; Declination of sun.
                  (declination tee-prime (deg 0L0)
                               (solar-longitude tee-prime))))
            (+ (* (tan-degrees phi)
                  (tan-degrees delta))
               (/ (sin-degrees alpha)
                  (* (cos-degrees delta)
                     (cos-degrees phi))))))
        """
        phi = self.latitude
        tee_prime = self.universal_from_local(tee)
        delta = self.declination(tee_prime, 0,
                                 self.alt_solar_longitude(tee_prime))
        return (self.tan_degrees(phi) * self.tan_degrees(delta) +
                self.sin_degrees(alpha) /
                (self.cos_degrees(delta) * self.cos_degrees(phi)))

    def moment_of_depression(self, approx, alpha, early=EVENING):
        """
        used

        (defun moment-of-depression (approx location alpha early?)
          ;; TYPE (moment location half-circle boolean) - moment
          ;; Moment in local time near approx when depression angle of sun
          ;; is alpha (negative if above horizon) at location; early? is
          ;; true when morning event is sought, and false for evening.
          ;; Returns bogus if depression angle is not reached.
          (let* ((tee (approx-moment-of-depression
                       approx location alpha early?)))
            (if (equal tee bogus)
                bogus
              (if (< (abs (- approx tee))
                     (sec 30))
                  tee
                (moment-of-depression tee location alpha early?)))))
        """
        tee = self.approx_moment_of_depression(approx, alpha, early=early)

        if tee is not None:
            if abs(approx - tee) < self.SEC(30):
                result = tee
            else:
                result = self.moment_of_depression(tee, alpha, early=early)
        else:
            result = None

        return result

    def dawn(self, date:float, alpha:float) -> float:
        """
        (defun dawn (date location alpha)
          ;; TYPE (fixed-date location half-circle) -> moment
          ;; Standard time in morning on fixed date at
          ;; location when depression angle of sun is alpha.
          ;; Returns bogus if there is no dawn on date.
          (let* ((result (moment-of-depression
                          (+ date (hr 6)) location alpha morning)))
            (if (equal result bogus)
                bogus
              (standard-from-local result location))))
        """
        result = self.moment_of_depression(date + self.HR(6), alpha,
                                           self.MORNING)
        return self.standard_from_local(result) if result else None

    def dusk(self, date:float, alpha:float) -> float:
        """
        used

        (defun dusk (date location alpha)
          ;; TYPE (fixed-date location half-circle) -> moment
          ;; Standard time in evening on fixed date at
          ;; location when depression angle of sun is alpha.
          ;; Returns bogus if there is no dusk on date.
          (let* ((result (moment-of-depression
                          (+ date (hr 18)) location alpha evening)))
            (if (equal result bogus)
                bogus
              (standard-from-local result location))))
        """
        result = self.moment_of_depression(date + self.HR(18), alpha,
                                           self.EVENING)
        return self.standard_from_local(result) if result else None

    def refraction(self) -> float:
        """
        used

        Refraction is the bending of the sun’s light by the Earth’s
        atmosphere.

        (defun refraction (tee location)
          ;; TYPE (moment location) -> half-circle
          ;; Refraction angle at moment tee at location.
          ;; The moment is not used.
          (let* ((h (max (mt 0) (elevation location)))
                 (cap-R (mt 6.372d6)) ; Radius of Earth.
                 (dip                 ; Depression of visible horizon.
                  (arccos-degrees (/ cap-R (+ cap-R h)))))
            (+ (mins 34) dip (* (secs 19) (sqrt h)))))
        """
        h = max(0, self.elevation)
        cap_r = 6.372e6 # Wikipedia average is 6371 Km
        dip = self.arccos_degrees(cap_r / (cap_r + h))
        return self.MINS(34) + dip + self.SECS(19) * math.sqrt(h)

    def sunrise(self, date):
        """
        (defun sunrise (date location)
          ;; TYPE (fixed-date location) -> moment
          ;; Standard time of sunrise on fixed date at location.
          (let* ((alpha (+ (refraction (+ date (hr 6)) location) (mins 16))))
            (dawn date location alpha)))
        """
        alpha = self.refraction() + self.MINS(16)
        return self.dawn(date, alpha)

    def sunset(self, day):
        """
        used
        This is based on  standard time which itself is based on location.

        (defun sunset (date location)
          ;; TYPE (fixed-date location) -> moment
          ;; Standard time of sunset on fixed date at location.
          (let* ((alpha (+ (refraction (+ date (hr 18)) location) (mins 16))))
            (dusk date location alpha)))
        """
        alpha = self.refraction() + self.MINS(16)
        return self.dusk(day, alpha)

    #urbana-sunset
    #cfs-alent
    #jewish-sabbath-ends
    #jewish-dusk
    #observed-lunar-altitude
    #moonrise
    #moonset

    #
    # Time and Astronomy (Times of Day)
    #

    #padua
    #local-zero-hour
    #italian-from-local
    #daytime-temporal-hour
    #nighttime-temporal-hour
    #standard-from-sundial
    #jewish-morning-end
    #alt-asr

    # We bypass all the Lunar Crescent Visability

    #
    # Calendar Basics (Mathematical Notation)
    #
    # Both of the below function have equivelent Python builtin functions.
    #
    #    (defun radians-from-degrees (theta)
    #      ;; TYPE real -> radian
    #      ;; Convert angle theta from degrees to radians.
    #      (* (mod theta 360) pi 1/180))
    #
    #    (defun degrees-from-radians (theta)
    #      ;; TYPE radian -> angle
    #      ;; Convert angle theta from radians to degrees.
    #      (mod (/ theta pi 1/180) 360))

    def sin_degrees(self, theta:float) -> float:
        """
        (defun sin-degrees (theta)
          ;; TYPE angle -> amplitude
          ;; Sine of theta (given in degrees).
          (sin (radians-from-degrees theta)))
        """
        return math.sin(math.radians(theta))

    def cos_degrees(self, theta:float) -> float:
        """
        (defun cos-degrees (theta)
          ;; TYPE angle -> amplitude
          ;; Cosine of theta (given in degrees).
          (cos (radians-from-degrees theta)))
        """
        return math.cos(math.radians(theta))

    def tan_degrees(self, theta:float) -> float:
        """
        (defun tan-degrees (theta)
          ;; TYPE angle -> real
          ;; Tangent of theta (given in degrees).
          (tan (radians-from-degrees theta)))
        """
        return math.tan(math.radians(theta))

    def arctan_degrees(self, y, x):
        """
        (defun arctan-degrees (y x)
          ;; TYPE (real real) -> angle
          ;; Arctangent of y/x in degrees.
          ;; Returns bogus if x and y are both 0.
          (if (and (= x y 0))
              bogus
            (mod
             (if (= x 0)
                 (* (sign y) (deg 90L0))
               (let* ((alpha (degrees-from-radians
                              (atan (/ y x)))))
                 (if (>= x 0)
                      alpha
                   (+ alpha (deg 180L0)))))
              360)))
        """
        assert not (x == 0 == y), (
            f"The value of x '{x}' and y '{y}' must not be x == 0 == y.")

        if x == 0:
            result = math.sin(y) * 90
        else:
            alpha = math.degrees(math.atan2(y, x))
            result = alpha if x >= 0 else alpha + 180

        return math.fmod(result, 360)

    def arcsin_degrees(self, x):
        """
        used

        (defun arcsin-degrees (x)
          ;; TYPE amplitude -> angle
          ;; Arcsine of x in degrees.
          (degrees-from-radians (asin x)))
        """
        assert -1 <= x <= 1, f"The value of x '{x}' must be >= -1 and <= 1."
        return math.degrees(math.asin(x))

    def arccos_degrees(self, x):
        """
        (defun arccos-degrees (x)
          ;; TYPE amplitude -> angle
          ;; Arccosine of x in degrees.
          (degrees-from-radians (acos x)))
        """
        assert -1 <= x <= 1, f"The value of x '{x}' must be >= -1 and <= 1."
        return math.degrees(math.acos(x))

    def fixed_from_moment(self, tee:float) -> float:
        """
        (defun fixed-from-moment (tee)
          ;; TYPE moment -> fixed-date
          ;; Fixed-date from moment tee.
          (floor tee))
        """
        return math.floor(tee)

    def _sigma(self, lists:tuple, func:object) -> float:
        """
        used

        (defmacro sigma (list body)
          ;; TYPE (list-of-pairs (list-of-reals->real))
          ;; TYPE -> real
          ;; list is of the form ((i1 l1)...(in ln)).
          ;; Sum of body for indices i1...in
          ;; running simultaneously thru lists l1...ln.
          `(apply `+ (mapcar (function (lambda
                                         ,(mapcar `car list)
                                         ,body))
                             ,@(mapcar `cadr list))))
        """
        # Ensure all lists have the same length
        assert len(set(len(lst) for lst in lists)) == 1, (
            "Lists must have the same length")
        return sum(func(*e) for e in zip(*lists))

    def _poly(self, x:float, a:list) -> float:
        """
        This is the Horner method used to eliminate the use of powers.
        Insted of:
        y = A + B * x + C * x^2 + D * x^3 + E * x^4
        do this:
        y = A + x * (B + x * (C + x * (D + x * E)))

        used

        (defun poly (x a)
          ;; TYPE (real list-of-reals) -> real
          ;; Sum powers of x with coefficients (from order 0 up) in list a.
          (if (equal a nil)
              0
            (+ (first a) (* x (poly x (rest a))))))
        """
        return 0 if not a else a[0] + (x * self._poly(x, a[1:]))

    def _next(self, initial:int, func:object) -> int:
        """
        used

        (defmacro next (index initial condition)
          ;; TYPE (* integer (integer->boolean)) -> integer
          ;; First integer greater or equal to initial such that
          ;; condition holds.
          ‘(loop for ,index from ,initial
                 when ,condition
                 return ,index))
        """
        while not func(initial):
            initial += 1

        return initial

    def _final(self, initial:int, func:object) -> int:
        """
        (defmacro final (index initial condition)
          ;; TYPE (* integer (integer->boolean)) -> integer
          ;; Last integer greater or equal to initial such that
          ;; condition holds.
          `(loop for ,index from ,initial
                 when (not ,condition)
                 return (1- ,index)))
        """
        return (initial - 1 if not func(initial)
                else self._final(initial + 1, func))

    def _to_radix(self, x:int, b:tuple, c:tuple=()):
        """
        (defun to-radix (x b &optional c)
          ;; TYPE (real list-of-rationals list-of-rationals)
          ;; TYPE -> list-of-reals
          ;; The radix notation corresponding to x
          ;; with base b for whole part and c for fraction.
          (if (null c)
              (if (null b)
                  (list x)
            (append (to-radix (quotient x (nth (1- (length b)) b))
                              (butlast b) nil)
                    (list (mod x (nth (1- (length b)) b)))))
           (to-radix (* x (apply ’* c)) (append b c))))
        """
        if not c:
            if not b:
                result = (x,)
            else:
                result = self._to_radix(self.QUOTIENT(x, b[-1]),
                                        b[:-1]) + (math.fmod(x, b[-1]),)
        else:
            result = self._to_radix(x * reduce(mul, c), b + c)

        return result

    #
    # Additional methods
    #

    def decimal_from_dms(self, degrees:int, minutes:int, seconds:float,
                         direction:str) -> float:
        '''
        Coordinantes in degrees, minutes, and seconds.
        The Shrine of Baha’u’llah: 32°56’36.86″N, 35° 5’30.38″E
        The Shrine of The Bab: 32°48’52.49″N, 34°59’13.91″E
        The Guardian’s Resting Place (not 3D): 51°37’21.85″N, 0°08’35.57″W

        ****

        Convert degrees, minutes, and seconds to a decimal.

        Degrees, minutes, and seconds to a decimal coordinant:

        1. Add the degrees to the minutes divided by 60
        2. Add the seconds divided by (60 x 60), which is 3600

        Example: To convert 35° 20′ 35", the answer is
                 35 + (20/60) + (35/3600) = 35.34306 degrees.

        :param degrees: The degree part of the coordinats.
        :type degrees: int
        :param minutes: The minute part of the coordinate.
        :type minutes: int
        :param seconds: The second part of the coordinate.
        :param direction: The direction part of the coordinate which can be
                          any of the following N, S, E, W in upper or lower
                          case.
        :type direction: str
        :return: latitude and longitude
        :rtype: tuple
        '''
        dirs = ('N', 'S', 'E', 'W')
        assert direction.upper() in dirs, (
            f"The 'direction' argument must be one of {dirs}")
        # Remove the minus sign iof it exists.
        degrees = -degrees if degrees < 0 else degrees
        decimal = degrees + (minutes / 60) + (seconds / 3600)
        # Adjust the sign based on the direction.
        return -decimal if direction.upper() in ('S', 'W') else decimal

    def dms_from_decimal(self, coord:float, direction:str) -> tuple:
        """
        Convert a decimal degree into degrees, minutes, and seconds.

        :param coord: The decimal coordinant.
        :type coord: float
        :param direction: The direction part of the coordinate which can be
                          any of the following N, S, E, W in upper or lower
                          case.
        :type direction: str
        :return: The degree, minute, second, and direction for of the
                 coordinate.
        :rtype: tuple
        """
        dirs = ('LATITUDE', 'LONGITUDE')
        direction = direction.upper()
        size = len(direction)
        assert size >= 3 and any([d.startswith(direction) for d in dirs]), (
            f"The direction argument must be one of {dirs}")
        # degrees
        degrees = int(abs(coord))
        # minutes
        minutes = int((abs(coord) - degrees) * 60)
        # seconds
        seconds = (abs(coord) - degrees - (minutes / 60)) * 3600

        if coord < 0:
            direc = 'S' if direction == 'LAT' else 'W'
        else:
            direc = 'N' if direction == 'LAT' else 'E'

        return degrees, minutes, seconds, direc

    def degrees_from_hms(self, h, m, s):
        """
        Find the degrees from the hours, minutes, and seconds of 360 degrees.
        Where as time zones are 15 degrees apart so 24 time zones times 15
        degrees is 360 degrees.

        15*h+15*m/60+15*s/3600
        """
        return 15 * h + 15 * m / 60 + 15 * s / 3600

    def hms_from_degrees(self, deg):
        """
        Find the hours, minutes, and seconds from degrees.
        """
        h = int(deg / 15)
        m = int((deg / 15 - h) * 60)
        s = (deg / 15 - h - m / 60) * 3600
        return h, m, s

    def coterminal_angle(self, value:float) -> float:
        """
        Find a Coterminal Angle.
        """
        value = math.fmod(value, 360)
        return value + 360 if value < 0 else value
