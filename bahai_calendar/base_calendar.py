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
from bahai_calendar.astronomical_terms import AstronomicalTerms


class BaseCalendar(AstronomicalTerms, JulianPeriod):
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
    SUN_OFFSET = 0.8333333333333334
    STARS_PLANET_OFFSET = 0.5666666666666667

    def __init__(self):
        self._time = None
        self._nutation = {'lon': (0, 0, False), 'obl': (0, 0, False)}
        self._sun_tss = {'trn': (), 'rsn': (), 'stn': ()}

    def parse_datetime(self, dt:datetime.datetime) -> None:
        self.time_representation = (dt.hour, dt.minute, dt.second)

    @property
    def time_representation(self):
        return self._time

    @time_representation.setter
    def time_representation(self, representation):
        self._time = representation

    #
    # Meeus Astronomical Algorithms
    #

    def delta_t(self, jd:float) -> float:
        """
        Calculate the value of ΔT=TT−UT.
        Only the year and month are considered, days, hoursm minutes, and
        seconds are ignored.

        :param jd: Julian day.
        :type jd: float
        :return: The delta t.
        :rtype: float

        .. note::

           see: http://eclipse.gsfc.nasa.gov/SEcat5/deltatpoly.html
        """
        from .gregorian_calendar import GregorianCalendar
        gc = GregorianCalendar()
        g_date = gc.gregorian_date_from_jd(jd)
        year = g_date[0] + (g_date[1] - 0.5) / 12
        func = lambda year: -20 + 32 * ((year - 1820) / 100)**2

        if year < -500:
            dt = func(year)
        elif year < 500:
            u = year / 100
            dt = self._poly(u, (10583.6, -1014.41, 33.78311, -5.952053,
                                -0.1798452, 0.022174192, 0.0090316521))
        elif year < 1600:
            u = (year - 1000) / 100
            dt = self._poly(u, (1574.2, -556.01, 71.23472, 0.319781,
                                -0.8503463, -0.005050998, 0.0083572073))
        elif year < 1700:
            u = year - 1600
            dt = self._poly(u, (120, -0.9808, -0.01532, 1 / 7129))
        elif year < 1800:
            u = year - 1700
            dt = self._poly(u, (8.83, 0.1603, -0.0059285, 0.00013336,
                                -1 / 1174000))
        elif year < 1860:
            u = year - 1800
            dt = self._poly(u, (13.72, -0.332447, 0.0068612, 0.0041116,
                                -0.00037436, 0.0000121272, -0.0000001699,
                                0.000000000875))
        elif year < 1900:
            u = year - 1860
            dt = self._poly(u, (7.62, 0.5737, -0.251754, 0.01680668,
                                -0.0004473624, 1 / 233174))
        elif year < 1920:
            u = year - 1900
            dt = self._poly(u, (-2.79, 1.494119, -0.0598939, 0.0061966,
                                -0.000197))
        elif year < 1941:
            u = year - 1920
            dt = self._poly(u, (21.20, 0.84493, -0.0761, 0.0020936))
        elif year < 1961:
            u = year - 1950
            dt = self._poly(u, (29.07, 0.407, -1 / 233, 1 / 2547))
        elif year < 1986:
            u = year - 1975
            dt = self._poly(u, (45.45, 1.067, -1 / 260, -1 / 718))
        elif year < 2005:
            u = year - 2000
            dt = self._poly(u, (63.86, 0.3345, -0.060374, 0.0017275,
                                0.000651814, 0.00002373599))
        elif year < 2050:
            u = year - 2000
            dt = self._poly(u, (62.92, 0.32217, 0.005589))
        elif year < 2150:
            dt = -20 + 32 * ((year - 1820) / 100)**2 - 0.5628 * (2150 - year)
        else: # 2150 >= year
            dt = func(year)

        return dt

    def _mean_sidereal_time_greenwich(self, tc:float) -> float:
        """
        Mean sidereal time at Greenwich. (GMST) If the hour angle is
        measured with respect to the mean equinox, mean sidereal time is
        being measured.

        :param tc: Julian century.
        :type tc: float
        :return: The mean sidereal time at Greenwich .
        :rtype: float

        .. note::

           Meeus--AA ch.12 p.88 Eq.12.3
        """
        return self.coterminal_angle(280.46061837 + 360.98564736629 *
                                     (tc * 36525) + 0.000387933 *
                                     tc**2 - tc**3 / 38710000)

    def _apparent_sidereal_time_greenwich(self, tc:float) -> float:
        """
        The apparent sidereal time, or the Greenwich. (GAST) If the hour
        angle is measured with respect to the true equinox, apparent
        sidereal time is being measured.

        :param tc: Julian century.
        :type tc: float
        :return: The apparent sidereal time at Greenwich.
        :rtype: float

        .. note::

           Meeus--AA ch.12 p.88
        """
        t0 = self._mean_sidereal_time_greenwich(tc)
        eps = self._true_obliquity_of_ecliptic(tc)
        d_psi = self._nutation_longitude(tc)
        return self.coterminal_angle(t0 + d_psi * self.cos_deg(eps))

    ## def ecliptic_longitude(self, tc:float) -> float:
    ##     """
    ##     Celestial longitude, or ecliptical longitude, often called simply
    ##     longitude, is measured (from 0° to 360°) from the vernal equinox,
    ##     positive to the east, along the ecliptic.

    ##     Meeus--AA ch.13 p.93 Eq.13.1
    ##     """
    ##     alpha = self._sun_apparent_right_ascension(tc)
    ##     delta = self._sun_apparent_declination(tc)
    ##     epsilon = self._true_obliquity_of_ecliptic(tc)
    ##     lam = math.degrees(
    ##         math.atan2(self.sin_deg(alpha) * self.cos_deg(epsilon) +
    ##                    math.tan(delta) * self.cos_deg(epsilon),
    ##                    self.sin_deg(alpha)))
    ##     print(alpha, delta, epsilon, lam)
    ##     return self.coterminal_angle(lam)

    ## def ecliptic_latitude(self, tc:float) -> float:
    ##     """
    ##     Celestial latitude, or ecliptical latitude, or simply latitude,
    ##     is measured (from 0° to +90° or to —90°) from the ecliptic,
    ##     positive to the north, negative to the south.

    ##     Meeus--AA ch.13 p.93 Eq.13.2
    ##     """
    ##     alpha = self._sun_apparent_right_ascension(tc)
    ##     delta = self._sun_apparent_declination(tc)
    ##     epsilon = self._true_obliquity_of_ecliptic(tc)
    ##     beta = math.degrees((self.sin_deg(delta) * self.cos_deg(epsilon) -
    ##                          self.cos_deg(delta) * self.sin_deg(epsilon) *
    ##                          cos_deg(alpha)))
    ##     return self.coterminal_angle(beta)

    ## def equatorial_right_ascension(self, jde:float) -> float:
    ##     """
    ##     Right ascension is measured (from 0 to 24 hours, sometimes from 0°
    ##     to 360°) from the vernal equinox, positive to the east, along the
    ##     celestial equator.

    ##     Meeus--AA ch.13 p.93 Eq.13.3
    ##     """
    ##     lam = self.ecliptic_longitude(jde)
    ##     beta = self.ecliptic_latitude(jde)
    ##     epsilon = self.true_obliquity_of_ecliptic(jde)
    ##     alpha = math.degrees(
    ##         math.atan2(self.sin_deg(lam) * self.cos_deg(epsilon) -
    ##                    self.tan_deg(beta) * self.sin_deg(epsilon),
    ##                    self.cos_deg(lam)))
    ##     return self.coterminal_angle(alpha)

    ## def equatorial_declination(self, jde:float) -> float:
    ##     """
    ##     Declination is measured (from 0° to +90°) from the equator, positive
    ##     to the north, negative to the south.

    ##     Meeus--AA ch.13 p.93 Eq.13.4
    ##     """
    ##     beta = self.ecliptic_latitude(jde)
    ##     lam = self.ecliptic_longitude(jde)
    ##     epsilon = self.true_obliquity_of_ecliptic(jde)
    ##     delta = math.degrees(
    ##         self.sin_deg(beta) * self.cos_deg(epsilon) +
    ##         self.cos_deg(beta) * self.sin_deg(epsilon) *
    ##         self.sin_deg())
    ##     return self.coterminal_angle(delta)

    def _altitude(self, delta:float, lat:float, h:float) -> float:
        """
        Altitude, positive above the horizon, negative below in degrees.

        :param delta: Declination in sidereal time.
        :type delta: float
        :param lat: Geographic latitude.
        :type lat: float
        :param h: Local hour angle.
        :type h: float
        :return: 
        :rtype: float

        .. note::

           Meeus--AA p.93 Eq.13.6
        """
        return math.degrees(math.asin(
            self.sin_deg(lat) * self.sin_deg(delta) + self.cos_deg(lat) *
            self.cos_deg(delta) * self.cos_deg(h)))

    def _approx_local_hour_angle(self, tc:float, lat:float,
                                 offset:float=SUN_OFFSET) -> float:
        """
        Approximate local hour angle, measured westwards from the South
        in degrees.

        Hour angle, in astronomy, the angle between an observer’s meridian
        (a great circle passing over his head and through the celestial
        poles) and the hour circle (any other great circle passing through
        the poles) on which some celestial body lies. This angle, when
        expressed in hours and minutes, is the time elapsed since the
        celestial body’s last transit of the observer’s meridian.

        :param tc: Julian century.
        :type tc: float
        :param lat: Latitude in decimal
        :type lat: float
        :param offset: A constant “standard” altitude, i.e., the geometric
                       altitude of the center of the body at the time of
                       apparent rising or setting, namely,
                       h0 = -0°34’ = -0°5667 for stars and planets;
                       h0 = -0°50' = -0°8333 for the Sun.
                       Default is SUN_OFFSET, STARS_PLANET_OFFSET can also
                       be used.
        :type offset: float
        :return: The approximat local hour angle in degrees.
        :rtype: float

        .. note::

           1. Meeus--AA p.101,102
           2. If the result of the equation is negative then add 360°
              (6.283185307179586 radians). If result is greater than
              360° then subtract 360° (6.283185307179586 radians).
              https://www.quora.com/How-do-I-calculate-the-hour-angle
        """
        delta = self._sun_apparent_declination(tc)
        cos_h0 = ((self.sin_deg(-offset) - self.sin_deg(lat) *
                   self.sin_deg(delta)) / (self.cos_deg(lat) *
                                           self.cos_deg(delta)))

        if cos_h0 < -1:
            cos_h0 -= 6.283185307179586
        elif cos_h0 > 1:
            cos_h0 += 6.283185307179586

        return math.degrees(math.acos(cos_h0))

    def _sun_transit(self, jd:float, lon:float, zone:float=0.0,
                     exact:bool=False) -> float:
        """
        The transit is when the body crosses the local maridian at upper
        culmination.

        :param jd: Julian day in UT.
        :type jd: float
        :param lon: Geographic longitude positive east negative west.
        :type lon: float
        :param zone: This is the political timezone.
        :type zone: float
        :param exact: Derive the timezone from the longitude. The 'zone'
                      parameter is not used if this is True, Default is
                      False.
        :type exact: bool
        :return:
        :rtype: float

        .. note::

           Meeus-AA ch.15 p. 102, 103 Eq.15.1, 15.2
        """
        assert (-180 <= zone <= 180 and not exact) or (zone == 0 and exact), (
            f"If exact is True then zone must be 0, found zone: {zone} "
            f"and exact: {exact}.")
        zone = zone if not exact else lon / 15
        func0 = lambda m: m + 1 if m <= 0 else m - 1 if m >= 1 else m
        tc = self.julian_centuries(jd)
        ast = self._apparent_sidereal_time_greenwich(tc)
        dt = self.delta_t(jd)
        tc_td = tc - (dt / (3600 * 24 * 36525))  # Convert to TD time.
        alpha = self._sun_apparent_right_ascension(tc_td)
        m = func0((alpha - lon - ast) / 360)
        md = self._transit_correction(tc, ast, dt, lon, m)
        m += md + self.decimal_from_hms(zone, 0, 0)
        return m

    def _transit_correction(self, tc, ast, dt, lon, m):
        """
        """
        srt = ast + 360.98564736629 * m
        n = m + dt / 86400
        ra0 = self._sun_apparent_right_ascension(tc - (1 / 36525))
        ra1 = self._sun_apparent_right_ascension(tc)
        ra2 = self._sun_apparent_right_ascension(tc + (1 / 36525))
        alpha = self.interpolation_from_three(ra0, ra1, ra2, n, True)
        h = self._local_hour_angle(srt, lon, alpha)
        return -h / 360

    def _sun_rising(self, jd:float, lat:float, lon:float, zone:float=0,
                    exact:bool=False, offset:float=SUN_OFFSET) -> float:
        """

        :param jd: Julian day in UT.
        :type jd: float
        :param lat: Geographic latitude positive north negative south.
        :type lat: float
        :param lon: Geographic longitude positive east negative west.
        :type lon: float
        :return:
        :rtype: float

        .. note::

           Meeus-AA ch.15 p. 102, 103 Eq.15.1, 15.2
        """
        return self._rising_setting(jd, lat, lon, zone, sr_ss='RISE')

    def _sun_setting(self, jd:float, lat:float, lon:float, zone:float=0,
                     exact:bool=False, offset:float=SUN_OFFSET) -> float:
        """

        :param jd: Julian day in UT.
        :type jd: float
        :param lat: Geographic latitude positive north negative south.
        :type lat: float
        :param lon: Geographic longitude positive east negative west.
        :type lon: float
        :return:
        :rtype: float

        .. note::

           Meeus-AA ch.15 p. 102, 103 Eq.15.1, 15.2
        """
        return self._rising_setting(jd, lat, lon, zone, sr_ss='SET')

    def _rising_setting(self, jd:float, lat:float, lon:float, zone:float=0,
                        exact:bool=False, offset:float=SUN_OFFSET,
                        sr_ss:str='RISE') -> float:
        """

        :param jd: Julian day in UT.
        :type jd: float
        :param lat: Geographic latitude positive north negative south.
        :type lat: float
        :param lon: Geographic longitude positive east negative west.
        :type lon: float
        :param zone: This is the political timezone.
        :type zone: float
        :param exact: Derive the timezone from the longitude. The 'zone'
                      parameter is not used if this is True, Default is
                      False.
        :type exact: bool
        :param offset: A constant “standard” altitude, i.e., the geometric
                       altitude of the center of the body at the time of
                       apparent rising or setting, namely,
                       h0 = -0°34’ = -0°5667 for stars and planets;
                       h0 = -0°50' = -0°8333 for the Sun.
                       Default is SUN_OFFSET, STARS_PLANET_OFFSET can also
                       be used.
        :type offset: float
        :param sr_ss: If 'RISE' return the sunrise else return sunset.
        :type sr_ss: str
        :return:
        :rtype: float

        .. note::

           Meeus-AA ch.15 p. 102, 103 Eq.15.1, 15.2
        """
        assert (-180 <= zone <= 180 and not exact) or (zone == 0 and exact), (
            f"If exact is True then zone must be 0, found zone: {zone} "
            f"and exact: {exact}.")
        zone = zone if not exact else lon / 15
        flags = ('RISE', 'SET')
        sr_ss = sr_ss.upper()
        assert sr_ss in flags, (
            f"Invalid value, should be one of '{flag}' found '{sr_ss}'.")
        func0 = lambda m: m + 1 if m <= 0 else m - 1 if m >= 1 else m
        tc = self.julian_centuries(jd)
        ast = self._apparent_sidereal_time_greenwich(tc)
        dt = self.delta_t(jd)
        tc_td = tc - (dt / (3600 * 24 * 36525))  # Convert to TD time.
        alpha = self._sun_apparent_right_ascension(tc_td)
        delta = self._sun_apparent_declination(tc_td)
        h0 = self._approx_local_hour_angle(tc, lat, offset=offset)
        m0 = func0((alpha - lon - ast) / 360)
        m = m0 - h0 / 360 if sr_ss == 'RISE' else m0 + h0 / 360
        dm = 1
        #print(m0, m, sr_ss)

        for i in range(5):
            if abs(dm) > 0.0001: break
            dm = self._rise_set_correction(tc, ast, dt, lat, lon, m, offset)
            #print('before-m', m, 'dm', dm, 'sr_ss', sr_ss)
            m += dm

        #print('jd', jd, 'tc', tc, 'dt', dt, 'tc_td', tc_td)
        #print('ast', ast, 'alpha', alpha, 'delta', delta, 'h0', h0, 'm0', m0)
        #print('after-m', m, 'i', i)
        m += self.decimal_from_hms(zone, 0, 0)
        return m

    def _rise_set_correction(self, tc, ast, dt, lat, lon, m, offset):
        """
        """
        srt = ast + 360.98564736629 * m
        n = m + dt / 86400
        ra0 = self._sun_apparent_right_ascension(tc - (1 / 36525))
        ra1 = self._sun_apparent_right_ascension(tc)
        ra2 = self._sun_apparent_right_ascension(tc + (1 / 36525))
        de0 = self._sun_apparent_declination(tc - (1 / 36525))
        de1 = self._sun_apparent_declination(tc)
        de2 = self._sun_apparent_declination(tc + (1 / 36525))
        alpha = self.interpolation_from_three(ra0, ra1, ra2, n, True)
        delta = self.interpolation_from_three(de0, de1, de2, n)
        #print('srt', srt, 'n', n)
        #print('Alpha', ra0, ra1, ra2, alpha)
        #print('Delta', de0, de1, de2, delta)
        h = self._local_hour_angle(srt, lon, alpha)
        alt = self._altitude(delta, lat, h)
        #print('h', h, 'alt', alt)
        return (alt + offset) / (360 * self.cos_deg(delta) *
                                 self.cos_deg(lat) * self.sin_deg(h))

    def _local_hour_angle(self, srt, lon, alpha):
        """
        Meeus-AA p.103
        """
        h = self.coterminal_angle(srt + lon - alpha)
        return h - 360 if h > 180 else h

    def _nutation_longitude(self, tc:float, *, degrees:bool=False) -> float:
        """
        Nutation longitude of the Earth's axis around it's 'mean' position.
        """
        tc_day, lon_sum, deg = self._nutation['lon']

        if tc != tc_day or degrees != deg:
            lon_sum, obl_sum = self._nutation_obliquity_longitude(
                tc, degrees=degrees)
            self._nutation['lon'] = (tc, lon_sum, degrees)
            self._nutation['obl'] = (tc, obl_sum, degrees)

        return self._nutation['lon'][1]

    def _nutation_obliquity(self, tc:float, *, degrees:bool=False) -> float:
        """
        Nutation obliquity of the Earth's equator around it's 'mean' position.
        """
        tc_day , obj_sum, deg = self._nutation['obl']

        if tc != tc_day or degrees != deg:
            lon_sum, obl_sum = self._nutation_obliquity_longitude(
                tc, degrees=degrees)
            self._nutation['lon'] = (tc, lon_sum, degrees)
            self._nutation['obl'] = (tc, obl_sum, degrees)

        return self._nutation['obl'][1]

    def _nutation_obliquity_longitude(self, tc:float,
                                      degrees:bool=False) -> float:
        """
        Nutation of the Earth's axis around it's 'mean' position.

        :param tc: Time in Julian centuries.
        :type tc: float
        :param degrees: If True units of degrees is returned, if False
                        units of radians is returned.
        :type degrees: float
        :return: Moon latitude.
        :rtype: float

        .. note::

           See:
      https://articles.adsabs.harvard.edu/full/seri/CeMec/0027/0000079.000.html
        """
        lm = self._moon_mean_anomaly(tc)
        ls = self._sun_earth_mean_anomaly(tc)
        ff = self._moon_latitude(tc)
        dd = self._mean_moon_elongation(tc)
        om = self._moon_ascending_node_longitude(tc)

        # W = LM*lm + LS*ls + F*ff + D*dd + OM*om
        # T = tc
        # Where LM, LS, F, D, and OM are from the nutation periodic terms.
        # The nutation in longitude is a sum of terms of the form
        # (psi_sin + sin * T) * sin(W).
        # Where psi_sin and t_sin are from the nutation periodic terms
        # and T is the Julian time from J2000.
        # The obliquity in latitude is a sum of terms of the form
        # (eps_cos + cos * T) * cos(W).
        # Where eps_cos and t_cos are from the nutation periodic terms
        # and T is the Julian time from J2000.
        lon_sum = 0
        obl_sum = 0

        for LM, LS, F, D, OM, day, psi_sin, sin, eps_cos, cos in self.NUT:
            w = LM*lm + LS*ls + F*ff + D*dd + OM*om
            lon_sum += (psi_sin + sin * tc) * self.sin_deg(w)
            obl_sum += (eps_cos + cos * tc) * self.cos_deg(w)

        lon_sum /= 36000000
        obl_sum /= 36000000

        if degrees:
            lon_sum = self.coterminal_angle(math.degrees(lon_sum))
            obl_sum = self.coterminal_angle(math.degrees(obl_sum))

        return lon_sum, obl_sum

    def _moon_mean_anomaly(self, tc:float) -> float:
        """

        :param tc: Time in Julian centuries.
        :type tc: float
        :return: Moon mean anomaly.
        :rtype: float

        .. note::

           Meeus--AA ch.22 p.144
           Referenced by lm (M').
        """
        return self.coterminal_angle(self._poly(
            tc, (134.96298, 477198.867398, 0.0086972, 1 / 56250)))

    def _sun_earth_mean_anomaly(self, tc:float) -> float:
        """

        :param tc: Time in Julian centuries.
        :type tc: float
        :return: Sun and earth mean anomaly.
        :rtype: float

        .. note::

           Meeus--AA ch.22 p.144
           Referenced by ls (M).
        """
        return self.coterminal_angle(self._poly(
            tc, (357.52772, 35999.05034, -0.0001603, -1 / 300000)))

    def _moon_latitude(self, tc:float) -> float:
        """

        :param tc: Time in Julian centuries.
        :type tc: float
        :return: Moon latitude.
        :rtype: float

        .. note::

           Meeus--AA ch.22 p.144
           Referenced by ff (F).
        """
        return self.coterminal_angle(self._poly(
            tc, (93.27191, 483202.017538, -0.0036825, 1 / 327270)))

    def _mean_moon_elongation(self, tc:float) -> float:
        """

        :param tc: Time in Julian centuries.
        :type tc: float
        :return: Mean moon elongation.
        :rtype: float

        .. note::

           Meeus--AA ch22 p144
           Referenced by dd (D).
        """
        return self.coterminal_angle(self._poly(
            tc, (297.85036, 445267.11148, -0.0019142, 1 / 189474)))

    def _moon_ascending_node_longitude(self, tc:float) -> float:
        """
        Longitude of the ascending node of the Moon’s mean orbit on the
        ecliptic, measured from the mean equinox of the date:

        :param tc: Time in Julian centuries.
        :type tc: float
        :return: Moon ascending node longitude.
        :rtype: float

        .. note::

           Meeus--AA ch22 p144
           Referenced by om (omega).
        """
        return self.coterminal_angle(self._poly(
            tc, (125.04452, -1934.136261, 0.0020708, 1 / 450000)))

    def _true_obliquity_of_ecliptic(self, tc:float) -> float:
        """
        The obliquity of the ecliptic, or inclination of the Earth’s axis
        of rotation, is the angle between the equator and the ecliptic.

        :param jde: Julian century.
        :type jde: float
        :return: The obliquity of the ecliptic in degrees as a decimal.
        :rtype: float

        .. note::

           Meeus--AA ch.22 p.147 Eq.22.3
           Convert lots of things:
           https://www.xconvert.com/unit-converter/arcseconds-to-degrees
        """
        u = tc / 100
        mean_ob = self._poly(u, (23.43929111111111, -1.3002583333333335,
                                 -0.00043055555555555555, 0.5553472222222222,
                                 -0.014272222222222223, -0.06935277777777778,
                                 -0.010847222222222222, 0.001977777777777778,
                                 0.007741666666666667, 0.0016083333333333334,
                                 0.0006805555555555556))
        return mean_ob + self._nutation_obliquity(tc)

    def _sun_mean_longitude(self, tc:float) -> float:
        """
        The geometric mean longitude of the Sun, referred to the mean
        equinox of the date.

        :param tc: Time in Julian centuries.
        :type tc: float
        :return: Mean longitude of the sun in degrees.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.163 Eq.25.2
           References by L0
        """
        return self.coterminal_angle(self._poly(
            tc, (280.46646, 36000.76983, 0.0003032)))

    def _eccentricity_earth_orbit(self, tc:float) -> float:
        """
        The eccentricity of the earth's orbit.

        Meeus--AA ch.25 p.163 Eq.25.4
        """
        return self._poly(tc, (0.016708634, -0.000042037, -0.0000001267))

    def _sun_equation_of_center(self, tc:float) -> float:
        """
        Meeus--AA ch.25 p.164
        """
        m = self._sun_earth_mean_anomaly(tc)
        return ((1.914602 - 0.004817 * tc - 0.000014 * tc**2) *
                self.sin_deg(m) + (0.019993 - 0.000101 * tc) *
                self.sin_deg(2 * m) + 0.000290 * self.sin_deg(3 * m))

    def _sun_true_longitude(self, tc:float) -> float:
        """
        The true geometric longitude referred to the mean equinox of the
        date. This longitude is the quantity required for instance in the
        calculation of geocentric planetary positions.

        Meeus--AA p.164
        """
        l0 = self._sun_mean_longitude(tc)
        cen = self._sun_equation_of_center(tc)
        return l0 + cen

    def _sun_apparent_longitude(self, tc:float) -> float:
        """
        Meeus--AA p.164
        This has a less accurate result.
        apparent_solar_longitude() should be more acurate.
        """
        sol = self._sun_true_longitude(tc)
        om = self._moon_ascending_node_longitude(tc)
        return sol - 0.00569 - 0.00478 * self.sin_deg(om)

    def _sun_apparent_right_ascension(self, tc:float) -> float:
        """
        Right ascension is measured (from 0 to 24 hours, sometimes from 0°
        to 360°) from the vernal equinox, positive to the east, along the
        celestial equator.

        :param tc: Julian century.
        :type tc: float
        :return: The apparent declination of the sun in radians.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.165 Eq.25.6
        """
        om = self._moon_ascending_node_longitude(tc)
        eps = self._true_obliquity_of_ecliptic(tc) + 0.00256 * self.cos_deg(om)
        lam = self._sun_apparent_longitude(tc)
        alpha = math.degrees(math.atan2(self.cos_deg(eps) * self.sin_deg(lam),
                                        self.cos_deg(lam)))
        return self.coterminal_angle(alpha)

    def _sun_apparent_declination(self, tc:float) -> float:
        """
        Declination is measured (from 0° to +90°) from the equator, positive
        to the north, negative to the south.

        :param tc: Julian century.
        :type tc: float
        :param app: If True the apparent declination is returned, if
        :type, app: bool
        :return: The apparent declination of the sun in radians.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p165 Eq25.7
        """
        om = self._moon_ascending_node_longitude(tc)
        eps = self._true_obliquity_of_ecliptic(tc) + 0.00256 * self.cos_deg(om)
        lam = self._sun_apparent_longitude(tc)
        return math.degrees(math.asin(self.sin_deg(eps) * self.sin_deg(lam)))

    def _heliocentric_ecliptical_longitude(self, tm:float,
                                           degrees:bool=False) -> float:
        """
        Find the heliocentric ecliptical longitude.

        :param tm: The moment in time referenced to J2000 millennia.
        :type tm: float
        :param degrees: The results if False are radians, else True
                        are degrees. Default is False.
        :type degrees: bool
        :return: Longitude in degrees or radians.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.166
           Referenced by L
        """
        func = lambda a, b, c: a * math.cos(b + c * tm)
        l0 = self._sigma((self.L0_A, self.L0_B, self.L0_C), func)
        l1 = self._sigma((self.L1_A, self.L1_B, self.L1_C), func)
        l2 = self._sigma((self.L2_A, self.L2_B, self.L2_C), func)
        l3 = self._sigma((self.L3_A, self.L3_B, self.L3_C), func)
        l4 = self._sigma((self.L4_A, self.L4_B, self.L4_C), func)
        l5 = self._sigma((self.L5_A, self.L5_B, self.L5_C), func)
        l = self._poly(tm, (l0, l1, l2, l3, l4, l5)) / 10**8
        return self.coterminal_angle(math.degrees(l)) if degrees else l

    def _heliocentric_ecliptical_latitude(self, tm:float,
                                          degrees:bool=False) -> float:
        """
        Find the heliocentric ecliptical latitude.

        :param tm: The moment in time referenced to J2000 millennia.
        :type tm: float
        :param degrees: The results if False are radians, else True
                        are degrees. Default is False.
        :type degrees: bool
        :return: Latitude in degrees or radians.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.166
           Referenced by B
        """
        func = lambda a, b, c: a * math.cos(b + c * tm)
        b0 = self._sigma((self.B0_A, self.B0_B, self.B0_C), func)
        b1 = self._sigma((self.B1_A, self.B1_B, self.B1_C), func)
        b = self._poly(tm, (b0, b1)) / 10**8
        return self.coterminal_angle(math.degrees(b)) if degrees else b

    def _radius_vector(self, tm:float, degrees:bool=False) -> float:
        """
        Find the distance of earth to the sun.

        :param tm: The moment in time referenced to J2000 millennia.
        :type tm: float
        :param degrees: The results if False are radians, else True
                        are degrees. Default is False.
        :type degrees: bool
        :return: Radius vector in degrees or radians.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.166
           Referenced by R
        """
        func = lambda a, b, c: a * math.cos(b + c * tm)
        r0 = self._sigma((self.R0_A, self.R0_B, self.R0_C), func)
        r1 = self._sigma((self.R1_A, self.R1_B, self.R1_C), func)
        r2 = self._sigma((self.R2_A, self.R2_B, self.R2_C), func)
        r3 = self._sigma((self.R3_A, self.R3_B, self.R3_C), func)
        r4 = self._sigma((self.R4_A, self.R4_B, self.R4_C), func)
        r = self._poly(tm, (r0, r1, r2, r3, r4)) / 10**8
        return self.coterminal_angle(math.degrees(r)) if degrees else r

    def apparent_solar_longitude(self, jde:float, degrees:bool=True) -> float:
        """
        Find the apparent solar longitude.

        Meeus--AA ch.25 p.166
        """
        tm = self.julian_millennia(jde)
        l = self._heliocentric_ecliptical_longitude(tm, degrees=False)
        l += math.pi
        # Convert to FK5 notation
        l -= math.radians(2.5091666666666666e-05) # -0".09033
        # Convert to centuries
        l += self._nutation_longitude(tm * 10, degrees=False)
        # eq 25.11
        l += self._aberration(tm)

        if degrees:
           l = self.coterminal_angle(math.degrees(l))

        return l

    def apparent_solar_latitude(self, jde:float, degrees:bool=True) -> float:
        """
        Find the apparent solar latitude.

        Meeus--AA ch.25 p.166
        """
        tm = self.julian_millennia(jde)
        tc = tm * 10 # Convert millenna to centuries
        b = self._heliocentric_ecliptical_latitude(tm)
        b *= -1 # Invert the result
        # Convert to FK5 notation
        l = self.apparent_solar_longitude(jde, degrees=False)
        b1 = self._poly(tc, (l, -1.397, -0.00031))
        bd = (math.radians(0.000010877777777777778) *
              (math.cos(b1) - math.sin(b1)))
        b += bd

        if degrees:
            b = self.coterminal_angle(math.degrees(b))

        return b

    def _aberration(self, tm:float, fixed:bool=True) -> float:
        """
        Find the aberration of a date with respect to a fixed reference
        frame or to the mean equinox.

        :param tc: The moment in time referenced to J2000 millennia.
        :type tm: float
        :param fixed: If True the results is to a fixed reference frame, if
                      False the results the reference is to the mean equinox.
                      Default is True.
        :type fixed: bool
        :return: The aberration of the date in degrees.
        :rtype: float

        .. note::

           AA p.167 Eq.25.11, p.168
        """
        if fixed:
            aberration = 3548.193
        else:
            aberration = 3548.33

        for a, b, c in self.ABER_A:
            aberration += math.radians(a) * self.sin_deg(b + c * tm)

        for a, b, c in self.ABER_B:
            aberration += math.radians(a) * tm * self.sin_deg(b + c * tm)

        for a, b, c in self.ABER_C:
            aberration += math.radians(a) * tm**2 * self.sin_deg(b + c * tm)

        r = self._radius_vector(tm, degrees=False)
        return self.decimal_from_dms(0, 0, -0.005775518 * r * aberration)

    def approx_julian_day_for_equinoxes_or_solstices(self, g_year:int,
                                                     lam:int=SPRING) -> float:
        """
        Find the approximate Julian day for the equinoxes or solstices.

        See: A ch.27 p.177
        """
        if g_year <= 1000:
            y = g_year / 1000

            if lam == self.SPRING:
                jde = self._poly(y, (1721139.29189, 365242.1374, 0.06134,
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
                                     0.00888, -0.0003))
            elif lam == self.AUTUMN:
                jde = self._poly(y, (2451810.21715, 365242.01767, -0.11575,
                                     0.00337, 0.00078))
            else: # lam == self.WINTER:
                jde = self._poly(y, (2451900.05952, 365242.74049, -0.06223,
                                     -0.00823, 0.00032))

        #print(f"year: {g_year}, Y: {y}")
        return jde

    def find_moment_of_equinoxes_or_solstices(self, jd:float,
                                              lam:int=SPRING) -> float:
        """
        With the jd and time of year find an equinoxe or solstice.

        :param jd: Julian day.
        :type jd: float
        :param lam: Lambda is the season as in (SPRING, SUMMER, AUTUMN, WINTER)
        :type lam: int
        :return: The Julian day of the equinoxe or solstice.
        :rtype: float

        .. note::

           Meeus--AA ch.27 p.177
        """
        from .gregorian_calendar import GregorianCalendar
        gc = GregorianCalendar()
        year = gc.gregorian_year_from_jd(jd)
        jde = self.approx_julian_day_for_equinoxes_or_solstices(year, lam)
        tc = self.julian_centuries(jde)
        w = 35999.373 * tc - 2.47
        dl = 1 + 0.0334 * self.cos_deg(w + 0.0007) * self.cos_deg(2*w)
        s = self._sigma(
            (self.EQ_SO_A, self.EQ_SO_B, self.EQ_SO_C),
            lambda a, b, c: a * self.cos_deg(b + c * tc))
        #print(f"jde0: {jde0}, T: {tc}, dl: {dl}, S: {s} ")
        return jde + (0.00001 * s) / dl

    #
    # Time and Astronomy (Time)
    #

    #greenwich
    #acre
    #direction
    #arctan

    def zone_from_longitude(self, phi:float) -> float:
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
        y = self.tan_deg(varepsilon / 2) ** 2
        equation = ((0.5 / math.pi) *
                    ((y * self.sin_deg(2 * lam)) +
                     (-2 * eccentricity * self.sin_deg(anomaly)) +
                     (4 * eccentricity * y * self.sin_deg(anomaly) *
                      self.cos_deg(2 * lam)) +
                     (-0.5 * y * y * self.sin_deg(4 * lam)) +
                     (-1.25 * eccentricity * eccentricity *
                      self.sin_deg(2 * anomaly))))
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
        return ((self.sin_deg(lat) * self.cos_deg(varepsilon)) +
                (self.cos_deg(lat) * self.sin_deg(varepsilon) *
                 self.sin_deg(lon)))

    #mean-tropical-year
    #mean-sidereal-year

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
        return (-0.004778 * self.sin_deg(cap_a) +
                -0.0003667 * self.sin_deg(cap_b))

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
        return (0.0000974 * self.cos_deg(177.63 + 35999.01848 * c)
                - 0.005575)

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
        return (self.tan_deg(phi) * self.tan_deg(delta) +
                self.sin_deg(alpha) /
                (self.cos_deg(delta) * self.cos_deg(phi)))

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

    def sin_deg(self, theta:float) -> float:
        """
        (defun sin-degrees (theta)
          ;; TYPE angle -> amplitude
          ;; Sine of theta (given in degrees).
          (sin (radians-from-degrees theta)))
        """
        return math.sin(math.radians(theta))

    def cos_deg(self, theta:float) -> float:
        """
        (defun cos-degrees (theta)
          ;; TYPE angle -> amplitude
          ;; Cosine of theta (given in degrees).
          (cos (radians-from-degrees theta)))
        """
        return math.cos(math.radians(theta))

    def tan_deg(self, theta:float) -> float:
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
                         direction:str='N') -> float:
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
                          either of the following 'LAT' or 'LON' in upper
                          or lower case.
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
        degrees = math.floor(abs(coord))
        # minutes
        minutes = math.floor((abs(coord) - degrees) * 60)
        # seconds
        seconds = (abs(coord) - degrees - (minutes / 60)) * 3600

        if coord < 0:
            direc = 'S' if direction == 'LAT' else 'W'
        else:
            direc = 'N' if direction == 'LAT' else 'E'

        return degrees, minutes, seconds, direc

    def degrees_from_hms(self, h:int, m:int, s:float) -> float:
        """
        Find the degrees from the hours, minutes, and seconds of 360 degrees.
        Where as time zones are 15 degrees apart so 24 time zones times 15
        degrees is 360 degrees.

        The angle may be expressed as negative east of the meridian plane
        and positive west of the meridian plane, or as positive westward
        from 0° to 360°. The angle may be measured in degrees or in time,
        with 24h = 360° exactly. So one hour is equal to (360/24)°=15°.
        """
        return 15 * h + 15 * m / 60 + 15 * s / 3600

    def hms_from_degrees(self, deg:float) -> tuple:
        """
        Find the hours, minutes, and seconds from 0 - 360 degrees. Where
        as time zones are 15 degrees apart so 24 time zones times 15
        degrees is 360 degrees.

        The angle may be expressed as negative east of the meridian plane
        and positive west of the meridian plane, or as positive westward
        from 0° to 360°. The angle may be measured in degrees or in time,
        with 24h = 360° exactly. So one hour is equal to (360/24)°=15°.
        """
        h = math.floor(deg / 15)
        m = math.floor((deg / 15 - h) * 60)
        s = (deg / 15 - h - m / 60) * 3600
        return h, m, s

    def seconds_from_hms(self, hours:int, minutes:int, seconds:float) -> float:
        """
        Convert hours, minutes, and seconds to seconds.
        """
        return hours * 3600 + minutes * 60 + seconds

    def hms_from_seconds(self, seconds:float) -> tuple:
        """
        Convert seconds into hours, minutes, and seconds.
        """
        hours = math.floor(seconds / 3600)
        m = (seconds - hours * 3600) / 60
        minutes = math.floor(m)
        return hours, minutes, m - minutes

    def decimal_from_hms(self, hours, minutes, seconds):
        """
        """
        return self.seconds_from_hms(hours, minutes, seconds) / 86400

    def hms_from_decimal(self, dec):
        """
        """
        return self.hms_from_seconds(dec * 86400)

    def coterminal_angle(self, value:float) -> float:
        """
        Find a Coterminal Angle.
        """
        value = math.fmod(value, 360)
        return value + 360 if value < 0 else value

    def interpolation_from_three(self, y1, y2, y3, n, normalize=False):
        """
        Interpolate from three terms with a factor.
        """
        a = y2 - y1
        b = y3 - y2

        if normalize:
            a += 360 if a < 0 else 0
            b += 360 if b < 0 else 0

        c = b - a
        return y2 + (n / 2) * (a + b + n * c)
