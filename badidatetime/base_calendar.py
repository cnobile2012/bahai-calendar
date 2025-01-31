# -*- coding: utf-8 -*-
#
# badidatetime/base_calendar.py
#
__docformat__ = "restructuredtext en"

import math

from badidatetime.julian_period import JulianPeriod
from badidatetime._astronomical_terms import AstronomicalTerms


class BaseCalendar(AstronomicalTerms, JulianPeriod):
    """
    Basic functionality used with all calenders.

    U.T. = Mean solar time at Greenwich, England (0◦ meridian), reckoned from
           midnight; sometimes GMT, Greenwich Mean Time

    Transformations between Time Systems:
    | https://gssc.esa.int/navipedia/index.php/Transformations_between_Time_Systems
    """
    _HR = lambda self, x: x / 24
    _MN = lambda self, x: x / 24 / 60
    _SEC = lambda self, x: x / 24 / 60 / 60
    _MINS = lambda self, x: x / 60
    _SECS = lambda self, x: x / 3600
    # Convert microseconds to a partial second.
    _US = lambda self, x: x / 1000000
    _ANGLE = lambda self, d, m, s: d + (m + s / 60) / 60  # 0 - 360
    _AMOD = lambda self, x, y: y + x % -y
    _MOD3 = lambda self, x, a, b: x if a == b else (
        a + math.fmod((x - a), (b - a)))
    _QUOTIENT = lambda self, m, n: math.floor(m / n)

    # The inline functions below will assume that 0 is midnight, if
    # converting from a Julian Period day add 0.5 to the value before
    # calling the function.
    _PARTIAL_DAY_TO_HOURS = lambda self, x: round(
        (x % 1) * 24, self._ROUNDING_PLACES)
    _PARTIAL_HOUR_TO_MINUTE = lambda self, x: round(
        (x % 1) * 60, self._ROUNDING_PLACES)
    _PARTIAL_MINUTE_TO_SECOND = _PARTIAL_HOUR_TO_MINUTE
    _PARTIAL_SECOND_TO_MICROSECOND = lambda self, x: int(
        round(x % 1, self._ROUNDING_PLACES) * 1e6)

    _MEAN_TROPICAL_YEAR = 365.2421897
    # MEAN_SIDEREAL_YEAR = 365.256363004

    _MORNING = True
    _EVENING = False
    # Seasons set to degrees.
    _SPRING = 0
    _SUMMER = 90
    _AUTUMN = 180
    _WINTER = 270
    _SUN_OFFSET = 0.8333333333333334
    _STARS_PLANET_OFFSET = 0.5666666666666667
    _ROUNDING_PLACES = 6
    _POSIX_EPOCH = 2440585.5  # This is using the more exact algorithm.
    _JULIAN_CAL_EPOCH = 1721423.5

    def __init__(self, *args, **kwargs):
        self._time = None
        self._nutation = {'lon': (0, 0, False), 'obl': (0, 0, False)}
        self._sun_tss = {'trn': (), 'rsn': (), 'stn': ()}

    #
    # Meeus Astronomical Algorithms
    #

    def _delta_t(self, jd: float, *, seconds: bool=False) -> float:
        """
        Calculate the value of ΔT = TD − UT, for TD = ΔT + UT, and for
        UT = ΔT - TD. Only the year and month are considered, days, hours,
        minutes, and seconds are ignored.

        :param float jd: Julian day.
        :param bool seconds: If True leave as seconds in a minute else convert
                             to seconds of a day.
        :return: The delta t.
        :rtype: float

        .. note::

           See: http://eclipse.gsfc.nasa.gov/SEcat5/deltatpoly.html
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
        else:  # 2150 >= year
            dt = func(year)

        # Convert to seconds of a day where 66.9 dt == 2010
        # Seconds of a day are 0.0007743055555555556
        return dt if seconds else dt / 86400

    def _mean_sidereal_time_greenwich(self, tc: float) -> float:
        """
        Mean sidereal time at Greenwich. (GMST) If the hour angle is
        measured with respect to the mean equinox, mean sidereal time is
        being measured.

        :param float tc: Julian century.
        :return: The mean sidereal time at Greenwich .
        :rtype: float

        .. note::

           Meeus--AA ch.12 p.88 Eq.12.3
        """
        return self._coterminal_angle(280.46061837 + 360.98564736629 *
                                      (tc * 36525) + 0.000387933 *
                                      tc**2 - tc**3 / 38710000)

    def _apparent_sidereal_time_greenwich(self, tc: float) -> float:
        """
        The apparent sidereal time, or the Greenwich. (GAST) If the hour
        angle is measured with respect to the true equinox, apparent
        sidereal time is being measured.

        :param float tc: Julian century.
        :return: The apparent sidereal time at Greenwich.
        :rtype: float

        .. note::

           Meeus--AA ch.12 p.88
        """
        t0 = self._mean_sidereal_time_greenwich(tc)
        eps = self._true_obliquity_of_ecliptic(tc)
        d_psi = self._nutation_longitude(tc)
        return self._coterminal_angle(t0 + d_psi * self._cos_deg(eps))

    def _altitude(self, delta: float, lat: float, h: float) -> float:
        """
        Altitude in degrees, positive above the horizon, negative below.

        :param float delta: Declination in sidereal time.
        :param float lat: Geographic latitude.
        :param float h: Local hour angle.
        :return: Altitude in degrees.
        :rtype: float

        .. note::

           Meeus--AA p.93 Eq.13.6
        """
        return math.degrees(math.asin(
            self._sin_deg(lat) * self._sin_deg(delta) + self._cos_deg(lat) *
            self._cos_deg(delta) * self._cos_deg(h)))

    def _approx_local_hour_angle(self, tc: float, lat: float,
                                 offset: float=_SUN_OFFSET) -> float:
        """
        Approximate local hour angle, measured westwards from the south
        in degrees.

        Hour angle, in astronomy, the angle between an observer’s meridian
        (a great circle passing over his head and through the celestial
        poles) and the hour circle (any other great circle passing through
        the poles) on which some celestial body lies. This angle, when
        expressed in hours and minutes, is the time elapsed since the
        celestial body’s last transit of the observer’s meridian.

        :param float tc: Julian century.
        :param float lat: Latitude in decimal
        :param float offset: A constant “standard” altitude, i.e., the
                             geometric altitude of the center of the body at
                             the time of apparent rising or setting, namely,
                             h0 = -0°34’ = -0°5667 for stars and planets;
                             h0 = -0°50' = -0°8333 for the Sun.
                             Default is _SUN_OFFSET, _STARS_PLANET_OFFSET can
                             also be used.
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
        cos_h0 = ((self._sin_deg(-offset) - self._sin_deg(lat) *
                   self._sin_deg(delta)) / (self._cos_deg(lat) *
                                            self._cos_deg(delta)))

        if cos_h0 < -1 or cos_h0 > 1:
            cos_h0 -= math.floor(cos_h0)

        return math.degrees(math.acos(cos_h0))

    def _sun_transit(self, jd: float, lon: float, zone: float=0.0,
                     exact_tz: bool=False) -> float:
        """
        The transit is when the body crosses the local maridian at upper
        culmination.

        :param float jd: Julian day in UT.
        :param float lon: Geographic longitude positive east negative west.
        :param float zone: This is the political timezone.
        :param bool exact_tz: Derive the timezone from the longitude. The
                              'zone' parameter is not used if this is True,
                              Default is False.
        :return:
        :rtype: float

        .. note::

           Meeus-AA ch.15 p. 102, 103 Eq.15.1, 15.2
        """
        assert ((-180 <= zone <= 180 and not exact_tz) or
                (zone == 0 and exact_tz)), (
            f"If exact_tz is True then zone must be 0, found zone: {zone} "
            f"and exact_tz: {exact_tz}.")
        zone = zone if not exact_tz else lon / 15
        func0 = lambda m: m + 1 if m <= 0 else m - 1 if m >= 1 else m
        tc = self._julian_centuries(jd)
        dt = self._delta_t(jd)
        tc_td = dt / 36525 + tc  # Compensate for the Julian Century
        alpha = self._sun_apparent_right_ascension(tc_td)
        ast = self._apparent_sidereal_time_greenwich(tc)
        m = func0((alpha - lon - ast) / 360)
        md = self._transit_correction(tc, ast, dt, lon, m)
        m += md + self._tz_decimal_from_dhms(0, zone, 0, 0)
        return m

    def _transit_correction(self, tc: float, ast: float, dt: float, lon: float,
                            m: float) -> float:
        """
        Find the correction to the transit.

        :param float tc: The Julian Period century.
        :param float ast: The apparent sidereal time at greenwich.
        :param float dt: The delta T of the JD.
        :param float lon: The longitude.
        :param float m: Times on day, expressed as fractions.
        :return: The correction to the transit.
        :rtype: float
        """
        srt = ast + 360.98564736629 * m
        n = m + dt / 86400
        ra0 = self._sun_apparent_right_ascension(tc - (1 / 36525))
        ra1 = self._sun_apparent_right_ascension(tc)
        ra2 = self._sun_apparent_right_ascension(tc + (1 / 36525))
        alpha = self._interpolation_from_three(ra0, ra1, ra2, n, True)
        h = self._local_hour_angle(srt, lon, alpha)
        return -h / 360

    def _sun_rising(self, jd: float, lat: float, lon: float, zone: float=0, *,
                    exact_tz: bool=False, offset: float=_SUN_OFFSET) -> float:
        """
        Find the jd for sunrise of the given jd.

        :param float jd: Julian day in UT.
        :param float lat: Geographic latitude positive north negative south.
        :param float lon: Geographic longitude positive east negative west.
        :param float zone: The time zone, defaults to the zero zone in
                           Greenwich UK.
        :param bool exact_tz: The political time zones or the exact time zone
                              derived from the longitude
                              (15 degrees = 360 / 24).
                              Default is False or the political time zone.
        :param bool offset: A constant “standard” altitude, i.e., the geometric
                            altitude of the center of the body at the time of
                            apparent rising or setting, namely,
                            h0 = -0°34’ = -0°5667 for stars and planets;
                            h0 = -0°50' = -0°8333 for the Sun.
                            Default is _SUN_OFFSET, _STARS_PLANET_OFFSET can
                            also be used.
        :return: The jd moments of the sunrise.
        :rtype: float

        .. note::

           Meeus-AA ch.15 p. 102, 103 Eq.15.1, 15.2
        """
        jd += self._rising_setting(jd, lat, lon, zone, exact_tz=exact_tz,
                                   offset=offset, sr_ss='RISE')
        return round(jd, self._ROUNDING_PLACES)

    def _sun_setting(self, jd: float, lat: float, lon: float, zone: float=0, *,
                     exact_tz: bool=False, offset: float=_SUN_OFFSET) -> float:
        """
        Find the jd for sunset of the given jd.

        :param float jd: Julian day in UT.
        :param float lat: Geographic latitude positive north negative south.
        :param float lon: Geographic longitude positive east negative west.
        :param float zone: The time zone, defaults to the zero zone in
                           Greenwich UK.
        :param bool exact_tz: The political time zones (default) or the exact
                              time zone. The exact time zone is derived from
                              the longitude (15 degrees = 360 / 24).
        :param bool offset: A constant “standard” altitude, i.e., the geometric
                            altitude of the center of the body at the time of
                            apparent rising or setting, namely,
                            h0 = -0°34’ = -0°5667 for stars and planets;
                            h0 = -0°50' = -0°8333 for the Sun.
                            Default is _SUN_OFFSET, _STARS_PLANET_OFFSET can
                            also be used.
        :return: The jd moments of the sunset.
        :rtype: float

        .. note::

           Meeus-AA ch.15 p. 102, 103 Eq.15.1, 15.2
        """
        jd += self._rising_setting(jd, lat, lon, zone, exact_tz=exact_tz,
                                   offset=offset, sr_ss='SET')
        return round(jd, self._ROUNDING_PLACES)

    def _rising_setting(self, jd: float, lat: float, lon: float, zone: float=0,
                        *, exact_tz: bool=False, offset: float=_SUN_OFFSET,
                        sr_ss: str='RISE') -> float:
        """
        Find the jd difference for sunrise or sunset of the given jd.

        :param float jd: Julian day in UT.
        :param float lat: Geographic latitude positive north negative south.
        :param float lon: Geographic longitude positive east negative west.
        :param float zone: The time zone, defaults to the zero zone in
                           Greenwich UK.
        :param bool exact_tz: The political time zones or the exact time zone
                              derived from the longitude
                              (15 degrees = 360 / 24).
                              Default is False or the political time zone.
        :param float offset: A constant “standard” altitude, i.e., the
                             geometric altitude of the center of the body at
                             the time of apparent rising or setting, namely,
                             h0 = -0°34’ = -0°5667 for stars and planets;
                             h0 = -0°50' = -0°8333 for the Sun.
                             Default is _SUN_OFFSET, _STARS_PLANET_OFFSET can
                             also be used.
        :param str sr_ss: If 'RISE' return the sunrise else return sunset.
        :return: The offset that would be added to the currect date.
        :rtype: float

        .. note::

           Meeus-AA ch.15 p. 102, 103 Eq.15.1, 15.2
        """
        assert ((-180 <= zone <= 180 and not exact_tz) or
                (zone == 0 and exact_tz)), (
            f"If exact_tz is True then zone must be 0, found zone: {zone} "
            f"and exact_tz: {exact_tz}.")
        zone = zone if not exact_tz else lon / 15
        flags = ('RISE', 'SET')
        sr_ss = sr_ss.upper()
        assert sr_ss in flags, (
            f"Invalid value, should be one of '{flags}' found '{sr_ss}'.")
        func0 = lambda m: m + 1 if m <= 0 else m - 1 if m >= 1 else m
        tc = self._julian_centuries(jd)
        dt = self._delta_t(jd)
        tc_td = dt / 36525 + tc  # Compensate for the Julian Century
        alpha = self._sun_apparent_right_ascension(tc_td)
        ast = self._apparent_sidereal_time_greenwich(tc)
        h0 = self._approx_local_hour_angle(tc, lat, offset=offset)
        m0 = func0((alpha - lon - ast) / 360)
        m = m0 - h0 / 360 if sr_ss == 'RISE' else m0 + h0 / 360
        dm = 1

        for i in range(5):
            dm = self._rise_set_correction(tc, ast, dt, lat, lon, m, offset)
            m += dm
            if abs(dm) < 0.0001: break

        m += self._tz_decimal_from_dhms(0, zone, 0, 0)
        return m % 1

    def _rise_set_correction(self, tc: float, ast: float, dt: float,
                             lat: float, lon: float, m: float, offset: float
                             ) -> float:
        """
        Find the correction to the sunrise and sunset.

        :param float tc: The Julian Period century.
        :param float ast: The apparent sidereal time at greenwich.
        :param float dt: The delta T of the JD.
        :param float lat: The latitude.
        :param float lon: The longitude.
        :param float m: Times on day, expressed as fractions.
        :param float offset: Either `_SUN_OFFSET` or `_STARS_PLANET_OFFSET`.
        :return: The correction to the sunrise or sunset.
        :rtype: float
        """
        srt = ast + 360.98564736629 * m
        n = m + dt / 86400
        ra0 = self._sun_apparent_right_ascension(tc - (1 / 36525))
        ra1 = self._sun_apparent_right_ascension(tc)
        ra2 = self._sun_apparent_right_ascension(tc + (1 / 36525))
        de0 = self._sun_apparent_declination(tc - (1 / 36525))
        de1 = self._sun_apparent_declination(tc)
        de2 = self._sun_apparent_declination(tc + (1 / 36525))
        alpha = self._interpolation_from_three(ra0, ra1, ra2, n, True)
        delta = self._interpolation_from_three(de0, de1, de2, n)
        h = self._local_hour_angle(srt, lon, alpha)
        alt = self._altitude(delta, lat, h)
        return (alt + offset) / (360 * self._cos_deg(delta) *
                                 self._cos_deg(lat) * self._sin_deg(h))

    def _local_hour_angle(self, srt: float, lon: float, alpha: float) -> float:
        """
        The LHA is the angle between the meridian of the observer and the
        meridian of the geographical position of the celestial body.

        :param float srt: Sidereal time.
        :param float lon: The Geographic longitude of the observer in degrees.
        :param float alpha: The apparent right ascensions.
        :return: The local hour angle.
        :rtype: float

        .. note::

           | Meeus-AA p.103
           | https://astronavigationdemystified.com/local-hour-angle-and-greenwich-hour-angle/

        """
        h = self._coterminal_angle(srt + lon - alpha)
        return h - 360 if h > 180 else h

    def _nutation_longitude(self, tc: float, *, degrees: bool=False) -> float:
        """
        Nutation longitude of the Earth's axis around it's 'mean' position.

        :param float tc: The Julian Period century.
        :param bool degrees: If `False` (default) return radians else if
                             `True` return degrees.
        :return: Either the radians or degrees depending on the `degrees`
                 argument.
        :rtype: float
        """
        tc_day, lon_sum, deg = self._nutation['lon']

        if tc != tc_day or degrees != deg:
            lon_sum, obl_sum = self._nutation_obliquity_longitude(
                tc, degrees=degrees)
            self._nutation['lon'] = (tc, lon_sum, degrees)
            self._nutation['obl'] = (tc, obl_sum, degrees)

        return self._nutation['lon'][1]

    def _nutation_obliquity(self, tc: float, *, degrees: bool=False) -> float:
        """
        Nutation obliquity of the Earth's equator around it's 'mean' position.

        :param float tc: The Julian Period century.
        :param bool degrees: If `False` (default) return radians else if
                             `True` return degrees.
        :return: The nutation of obliquity.
        :rtype: float
        """
        tc_day, obj_sum, deg = self._nutation['obl']

        if tc != tc_day or degrees != deg:
            lon_sum, obl_sum = self._nutation_obliquity_longitude(
                tc, degrees=degrees)
            self._nutation['lon'] = (tc, lon_sum, degrees)
            self._nutation['obl'] = (tc, obl_sum, degrees)

        return self._nutation['obl'][1]

    def _nutation_obliquity_longitude(self, tc: float, degrees: bool=False
                                      ) -> float:
        """
        Nutation of the Earth's axis around it's 'mean' position.

        :param float tc: Time in Julian centuries.
        :param float degrees: If True units of degrees is returned, if False
                              units of radians is returned.
        :return: Moon latitude.
        :rtype: float

        .. note::

           See: https://articles.adsabs.harvard.edu/full/seri/CeMec/0027/0000079.000.html
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

        for LM, LS, F, D, OM, day, psi_sin, sin, eps_cos, cos in self._NUT:
            w = LM*lm + LS*ls + F*ff + D*dd + OM*om
            lon_sum += (psi_sin + sin * tc) * self._sin_deg(w)
            obl_sum += (eps_cos + cos * tc) * self._cos_deg(w)

        lon_sum /= 36000000
        obl_sum /= 36000000

        if degrees:
            lon_sum = self._coterminal_angle(math.degrees(lon_sum))
            obl_sum = self._coterminal_angle(math.degrees(obl_sum))

        return lon_sum, obl_sum

    def _moon_mean_anomaly(self, tc: float) -> float:
        """
        The position of the moon in its orbit around the Earth.

        :param float tc: Time in Julian centuries.
        :return: Moon mean anomaly.
        :rtype: float

        .. note::

           | Meeus--AA ch.22 p.144
           | Referenced by lm (M').
        """
        return self._coterminal_angle(self._poly(
            tc, (134.96298, 477198.867398, 0.0086972, 1 / 56250)))

    def _sun_earth_mean_anomaly(self, tc: float) -> float:
        """
        The position of the Earth in its orbit around the Sun.

        :param float tc: Time in Julian centuries.
        :return: Sun and earth mean anomaly.
        :rtype: float

        .. note::

           | Meeus--AA ch.22 p.144
           | Referenced by ls (M).
        """
        return self._coterminal_angle(self._poly(
            tc, (357.52772, 35999.05034, -0.0001603, -1 / 300000)))

    def _moon_latitude(self, tc: float) -> float:
        """
        The angle between the Moon’s ecliptic longitude and its mean
        longitude.

        :param float tc: Time in Julian centuries.
        :return: Moon latitude.
        :rtype: float

        .. note::

           | Meeus--AA ch.22 p.144
           | Referenced by ff (F).
        """
        return self._coterminal_angle(self._poly(
            tc, (93.27191, 483202.017538, -0.0036825, 1 / 327270)))

    def _mean_moon_elongation(self, tc: float) -> float:
        """

        :param float tc: Time in Julian centuries.
        :return: Mean moon elongation.
        :rtype: float

        .. note::

           | Meeus--AA ch22 p144
           | Referenced by dd (D).
        """
        return self._coterminal_angle(self._poly(
            tc, (297.85036, 445267.11148, -0.0019142, 1 / 189474)))

    def _moon_ascending_node_longitude(self, tc: float) -> float:
        """
        Longitude of the ascending node of the Moon’s mean orbit on the
        ecliptic, measured from the mean equinox of the date:

        :param float tc: Time in Julian centuries.
        :return: Moon ascending node longitude.
        :rtype: float

        .. note::

           | Meeus--AA ch22 p144
           | Referenced by om (omega).
        """
        return self._coterminal_angle(self._poly(
            tc, (125.04452, -1934.136261, 0.0020708, 1 / 450000)))

    def _true_obliquity_of_ecliptic(self, tc: float) -> float:
        """
        The obliquity of the ecliptic, or inclination of the Earth’s axis
        of rotation, is the angle between the equator and the ecliptic.

        :param float jde: Julian century.
        :return: The obliquity of the ecliptic in degrees as a decimal.
        :rtype: float

        .. note::

           | Meeus--AA ch.22 p.147 Eq.22.3
           | Convert lots of things:
           | https://www.xconvert.com/unit-converter/arcseconds-to-degrees
        """
        u = tc / 100
        mean_ob = self._poly(u, (23.43929111111111, -1.3002583333333335,
                                 -0.00043055555555555555, 0.5553472222222222,
                                 -0.014272222222222223, -0.06935277777777778,
                                 -0.010847222222222222, 0.001977777777777778,
                                 0.007741666666666667, 0.0016083333333333334,
                                 0.0006805555555555556))
        return mean_ob + self._nutation_obliquity(tc)

    def _sun_mean_longitude(self, tc: float) -> float:
        """
        The geometric mean longitude of the Sun, referred to the mean
        equinox of the date.

        :param float tc: Time in Julian centuries.
        :return: Mean longitude of the sun in degrees.
        :rtype: float

        .. note::

           | Meeus--AA ch.25 p.163 Eq.25.2
           | References by L0
        """
        return self._coterminal_angle(self._poly(
            tc, (280.46646, 36000.76983, 0.0003032)))

    def _eccentricity_earth_orbit(self, tc: float) -> float:
        """
        The eccentricity of the earth's orbit.

        :param float tc: Time in Julian centuries.
        :return: The eccentricity of the earth's orbit.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.163 Eq.25.4
        """
        return self._poly(tc, (0.016708634, -0.000042037, -0.0000001267))

    def _sun_equation_of_center(self, tc: float) -> float:
        """
        Describes the difference between the true anomaly (the actual
        angular position of the Earth in its orbit around the Sun) and the
        mean anomaly (the position the Earth would have if its orbit were
        perfectly circular and uniform). This difference is caused by the
        elliptical shape of Earth’s orbit and the resulting non-uniform
        motion of the Earth around the Sun.

        :param float tc: Time in Julian centuries.
        :return: The Sun's equation of it's center.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.164
        """
        m = self._sun_earth_mean_anomaly(tc)
        return ((1.914602 - 0.004817 * tc - 0.000014 * tc**2) *
                self._sin_deg(m) + (0.019993 - 0.000101 * tc) *
                self._sin_deg(2 * m) + 0.000290 * self._sin_deg(3 * m))

    def _sun_true_longitude(self, tc: float) -> float:
        """
        The true geometric longitude referred to the mean equinox of the
        date. This longitude is the quantity required for instance in the
        calculation of geocentric planetary positions.

        :param float tc: Time in Julian centuries.
        :return: The true geometric longitude.
        :rtype: float

        .. note::

           Meeus--AA p.164
        """
        l0 = self._sun_mean_longitude(tc)
        cen = self._sun_equation_of_center(tc)
        return l0 + cen

    def _sun_apparent_longitude(self, tc: float) -> float:
        """
        The Sun’s apparent longitude is the angle measured from the vernal
        equinox to the Sun’s position on the ecliptic plane as seen from
        Earth.

        :param float tc: Time in Julian centuries.
        :return: The Sun’s apparent longitude.
        :rtype: float

        .. note::

           Meeus--AA p.164

           This has a less accurate result. apparent_solar_longitude()
           should be more acurate.
        """
        sol = self._sun_true_longitude(tc)
        om = self._moon_ascending_node_longitude(tc)
        return sol - 0.00569 - 0.00478 * self._sin_deg(om)

    def _sun_apparent_right_ascension(self, tc: float) -> float:
        """
        Right ascension is measured (from 0 to 24 hours, sometimes from 0°
        to 360°) from the vernal equinox, positive to the east, along the
        celestial equator.

        :param float tc: Julian century.
        :return: The apparent declination of the sun in radians.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.165 Eq.25.6
        """
        om = self._moon_ascending_node_longitude(tc)
        eps = (self._true_obliquity_of_ecliptic(tc) + 0.00256 *
               self._cos_deg(om))
        lam = self._sun_apparent_longitude(tc)
        alpha = math.degrees(math.atan2(self._cos_deg(eps) *
                                        self._sin_deg(lam),
                                        self._cos_deg(lam)))
        return self._coterminal_angle(alpha)

    def _sun_apparent_declination(self, tc: float) -> float:
        """
        Declination is measured (from 0° to +90°) from the equator, positive
        to the north, negative to the south.

        :param float tc: Julian century.
        :param bool app: If True the apparent declination is returned, if
        :return: The apparent declination of the sun in radians.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p165 Eq25.7
        """
        om = self._moon_ascending_node_longitude(tc)
        eps = (self._true_obliquity_of_ecliptic(tc) + 0.00256 *
               self._cos_deg(om))
        lam = self._sun_apparent_longitude(tc)
        return math.degrees(math.asin(self._sin_deg(eps) * self._sin_deg(lam)))

    def _heliocentric_ecliptical_longitude(self, tm: float,
                                           degrees: bool=False) -> float:
        """
        Find the heliocentric ecliptical longitude.

        :param float tm: The moment in time referenced to J2000 millennia.
        :param bool degrees: The results if False are radians, else True
                             are degrees. Default is False.
        :return: Longitude in degrees or radians.
        :rtype: float

        .. note::

           | Meeus--AA ch.25 p.166
           | Referenced by L
        """
        func = lambda a, b, c: a * math.cos(b + c * tm)
        l0 = self._sigma((self._L0_A, self._L0_B, self._L0_C), func)
        l1 = self._sigma((self._L1_A, self._L1_B, self._L1_C), func)
        l2 = self._sigma((self._L2_A, self._L2_B, self._L2_C), func)
        l3 = self._sigma((self._L3_A, self._L3_B, self._L3_C), func)
        l4 = self._sigma((self._L4_A, self._L4_B, self._L4_C), func)
        l5 = self._sigma((self._L5_A, self._L5_B, self._L5_C), func)
        l = self._poly(tm, (l0, l1, l2, l3, l4, l5)) / 10**8
        return round(self._coterminal_angle(math.degrees(l)) if degrees else l,
                     self._ROUNDING_PLACES)

    def _heliocentric_ecliptical_latitude(self, tm: float,
                                          degrees: bool=False) -> float:
        """
        Find the heliocentric ecliptical latitude.

        :param float tm: The moment in time referenced to J2000 millennia.
        :param bool degrees: The results if False are radians, else True
                             are degrees. Default is False.
        :return: Latitude in degrees or radians.
        :rtype: float

        .. note::

           | Meeus--AA ch.25 p.166
           | Referenced by B
        """
        func = lambda a, b, c: a * math.cos(b + c * tm)
        b0 = self._sigma((self._B0_A, self._B0_B, self._B0_C), func)
        b1 = self._sigma((self._B1_A, self._B1_B, self._B1_C), func)
        b = self._poly(tm, (b0, b1)) / 10**8
        return round(self._coterminal_angle(math.degrees(b)) if degrees else b,
                     self._ROUNDING_PLACES)

    def _radius_vector(self, tm: float, degrees: bool=False) -> float:
        """
        Find the distance of earth to the sun.

        :param float tm: The moment in time referenced to J2000 millennia.
        :param bool degrees: The results if False are radians, else True
                             are degrees. Default is False.
        :return: Radius vector in degrees or radians.
        :rtype: float

        .. note::

           | Meeus--AA ch.25 p.166
           | Referenced by R
        """
        func = lambda a, b, c: a * math.cos(b + c * tm)
        r0 = self._sigma((self._R0_A, self._R0_B, self._R0_C), func)
        r1 = self._sigma((self._R1_A, self._R1_B, self._R1_C), func)
        r2 = self._sigma((self._R2_A, self._R2_B, self._R2_C), func)
        r3 = self._sigma((self._R3_A, self._R3_B, self._R3_C), func)
        r4 = self._sigma((self._R4_A, self._R4_B, self._R4_C), func)
        r = self._poly(tm, (r0, r1, r2, r3, r4)) / 10**8
        return round(self._coterminal_angle(math.degrees(r)) if degrees else r,
                     self._ROUNDING_PLACES)

    def _apparent_solar_longitude(self, jde: float,
                                  degrees: bool=True) -> float:
        """
        Find the apparent solar longitude.

        :param float jde: The Julian Period day.
        :param bool degrees: The results if False are radians, else True
                             are degrees. Default is False.
        :return: The apparent solar longitude.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.166
        """
        tm = self._julian_millennia(jde)
        l = self._heliocentric_ecliptical_longitude(tm, degrees=False)
        l += math.pi
        # Convert to FK5 notation
        l -= math.radians(2.5091666666666666e-05)  # -0".09033
        # Convert to centuries
        l += self._nutation_longitude(tm * 10, degrees=False)
        # eq 25.11
        l += self._aberration(tm)

        if degrees:
            l = self._coterminal_angle(math.degrees(l))

        return round(l, self._ROUNDING_PLACES)

    def _apparent_solar_latitude(self, jde: float,
                                 degrees: bool=True) -> float:
        """
        Find the apparent solar latitude.

        :param float jde: The Julian Period day.
        :param bool degrees: The results if False are radians, else True
                             are degrees. Default is False.
        :return: The apparent solar latitude.
        :rtype: float

        .. note::

           Meeus--AA ch.25 p.166
        """
        tm = self._julian_millennia(jde)
        tc = tm * 10  # Convert millenna to centuries
        b = self._heliocentric_ecliptical_latitude(tm)
        b *= -1  # Invert the result
        # Convert to FK5 notation
        l = self._apparent_solar_longitude(jde, degrees=False)
        b1 = self._poly(tc, (l, -1.397, -0.00031))
        bd = (math.radians(0.000010877777777777778) *
              (math.cos(b1) - math.sin(b1)))
        b += bd

        if degrees:
            b = self._coterminal_angle(math.degrees(b))

        return round(b, self._ROUNDING_PLACES)

    def _aberration(self, tm: float, fixed: bool=True) -> float:
        """
        Find the aberration of a date with respect to a fixed reference
        frame or to the mean equinox.

        :param float tc: The moment in time referenced to J2000 millennia.
        :param bool fixed: If `True` (default) the results is to a fixed
                           reference frame, if `False` the result is
                           referenced to the mean equinox.
        :return: The aberration of the date in degrees.
        :rtype: float

        .. note::

           AA p.167 Eq.25.11, p.168
        """
        if fixed:
            aberration = 3548.193
        else:
            aberration = 3548.33

        for a, b, c in self._ABER_A:
            aberration += math.radians(a) * self._sin_deg(b + c * tm)

        for a, b, c in self._ABER_B:
            aberration += math.radians(a) * tm * self._sin_deg(b + c * tm)

        for a, b, c in self._ABER_C:
            aberration += math.radians(a) * tm**2 * self._sin_deg(b + c * tm)

        r = self._radius_vector(tm, degrees=False)
        return round(self._decimal_from_dms(
            0, 0, -0.005775518 * r * aberration), self._ROUNDING_PLACES)

    def _approx_julian_day_for_equinoxes_or_solstices(self, g_year: int,
                                                      lam: int=_SPRING
                                                      ) -> float:
        """
        Find the approximate Julian day for the equinoxes or solstices.

        :param int g_year: The Gregorian year.
        :param int lam: The lamda, either `_SPRING` (default), `_SUMMER`,
                        `_AUTUMN`, or `_WINTER`.
        :return: The approximate Julian day for the equinoxes or solstices.
        :rtype: float

        .. note::

           See: Meeus AA ch.27 p.177
        """
        if g_year <= 1000:
            y = g_year / 1000

            if lam == self._SPRING:
                jde = self._poly(y, (1721139.29189, 365242.1374, 0.06134,
                                     0.00111, -0.00071))
            elif lam == self._SUMMER:
                jde = self._poly(y, (1721233.25401, 365241.72562, -0.05323,
                                     0.00907, 0.00025))
            elif lam == self._AUTUMN:
                jde = self._poly(y, (1721325.70455, 365242.49558, -0.11677,
                                     -0.00297, 0.00074))
            else:  # lam == self._WINTER:
                jde = self._poly(y, (1721414.39987, 365242.88257, -0.00769,
                                     -0.00933, -0.00006))
        else:
            y = (g_year - 2000) / 1000

            if lam == self._SPRING:
                jde = self._poly(y, (2451623.80984, 365242.37404, 0.05169,
                                     -0.00411, -0.00057))
            elif lam == self._SUMMER:
                jde = self._poly(y, (2451716.56767, 365241.62603, 0.00325,
                                     0.00888, -0.0003))
            elif lam == self._AUTUMN:
                jde = self._poly(y, (2451810.21715, 365242.01767, -0.11575,
                                     0.00337, 0.00078))
            else:  # lam == self._WINTER:
                jde = self._poly(y, (2451900.05952, 365242.74049, -0.06223,
                                     -0.00823, 0.00032))

        # JD♈(Y) = JD0+78.814+365.24236 ΔY+5.004*10^−8 ΔY^2−2.87*10^−12
        # ΔY^3−4.5*10^−16 ΔY^4

        return jde

    def _find_moment_of_equinoxes_or_solstices(self, jd: float,
                                               lam: int=_SPRING,
                                               zone: float=0) -> float:
        """
        With the jd and time of year find an equinoxe or solstice at
        Greenwich.

        :param float jd: Meeus algorithm Julian day.
        :param int lam: The lamda, either `_SPRING` (default), `_SUMMER`,
                        `_AUTUMN`, or `_WINTER`.
        :param float zone: The time zone.
        :return: The Julian day of the equinox or solstice.
        :rtype: float

        .. note::

           Meeus--AA ch.27 p.177
        """
        from .gregorian_calendar import GregorianCalendar
        gc = GregorianCalendar()
        year = gc.gregorian_year_from_jd(jd)
        jde = self._approx_julian_day_for_equinoxes_or_solstices(year, lam)
        tc = self._julian_centuries(jde)
        w = 35999.373 * tc - 2.47
        dl = 1 + 0.0334 * self._cos_deg(w + 0.0007) * self._cos_deg(2*w)
        s = self._sigma((self._EQ_SO_A, self._EQ_SO_B, self._EQ_SO_C),
                        lambda a, b, c: a * self._cos_deg(b + c * tc))
        jde += (0.00001 * s) / dl + self._HR(zone)
        return round(jde, self._ROUNDING_PLACES)

    def _decimal_from_dms(self, degrees: int, minutes: int, seconds: float,
                          direction: str='N') -> float:
        '''
        Coordinantes in degrees, minutes, and seconds.

        | The Shrine of Baha’u’llah: 32°56’36.86″N, 35° 5’30.38″E
        | The Shrine of The Bab: 32°48’52.49″N, 34°59’13.91″E
        | The Guardian’s Resting Place (not 3D): 51°37’21.85″N, 0°08’35.57″W


        .. rubric:: Process

        Convert degrees, minutes, and seconds to a decimal.

        Degrees, minutes, and seconds to a decimal coordinant:

        1. Add the degrees to the minutes divided by 60
        2. Add the seconds divided by (60 x 60), which is 3600

        .. rubric:: Example

        To convert 35° 20′ 35", the answer is
        35 + (20/60) + (35/3600) = 35.34306 degrees.

        :param int degrees: The degree part of the coordinats.
        :param int minutes: The minute part of the coordinate.
        :param float seconds: The second part of the coordinate.
        :param str direction: The direction part of the coordinate which can
                              be any of the following N, S, E, W in upper or
                              lower case.
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

    def _dms_from_decimal(self, coord: float, direction: str) -> tuple:
        """
        Convert a decimal degree into degrees, minutes, and seconds.

        :param float coord: The decimal coordinant.
        :param str direction: The direction part of the coordinate which can
                              be any of the following N, S, E, W in upper or
                              lower case.
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

    def _degrees_from_hms(self, h: int, m: int, s: float) -> float:
        """
        Find the degrees from the hours, minutes, and seconds of 360 degrees.
        Where as time zones are 15 degrees apart so 24 time zones times 15
        degrees is 360 degrees.

        The angle may be expressed as negative east of the meridian plane
        and positive west of the meridian plane, or as positive westward
        from 0° to 360°. The angle may be measured in degrees or in time,
        with 24h = 360° exactly. So one hour is equal to (360/24)° = 15°.

        :param int h: The hour.
        :param int m: The minute.
        :param float s: The second.
        :return: The degrees from the hours, minutes, and seconds of
                 360 degrees.
        :rtype: float
        """
        return 15 * h + 15 * m / 60 + 15 * s / 3600

    def _hms_from_degrees(self, deg: float) -> tuple:
        """
        Find the hours, minutes, and seconds from 0 - 360 degrees. Where
        as time zones are 15 degrees apart so 24 time zones times 15
        degrees is 360 degrees.

        The angle may be expressed as negative east of the meridian plane
        and positive west of the meridian plane, or as positive westward
        from 0° to 360°. The angle may be measured in degrees or in time,
        with 24h = 360° exactly. So one hour is equal to (360/24)° = 15°.

        :param float deg: The degrees of the 360 degree curcomference of
                          the earth.
        :return: The hours, minutes, and seconds.
        :rtype: tuple
        """
        h = math.floor(deg / 15)
        m = math.floor((deg / 15 - h) * 60)
        s = (deg / 15 - h - m / 60) * 3600
        return h, m, s

    def _seconds_from_dhms(self, days: int, hours: int, minutes: int,
                           seconds: float, zone: float=0) -> float:
        """
        Convert days, hours, minutes, and seconds to seconds.

        :param int days: Number of days.
        :param int hours: Number of hours.
        :param int minutes: The number of minutes.
        :param float seconds: The number of seconds with possible fraction.
        :param float zone: The timezone in degrees, defaults to 0 or GMT.
        :return: The number of seconds.
        :rtype: float
        """
        return (days * 86400+ (hours + zone) * 3600 + minutes * 60
                + seconds + zone * 3600)

    def _dhms_from_seconds(self, seconds: float, zone: float=0) -> tuple:
        """
        Convert seconds into days, hours, minutes, and seconds. Depending
        on the timezone there could be an additional day added.

        :param float seconds: The number of seconds with possible fraction.
        :param float zone: The timezone in degrees, defaults to 0 or GMT.
        :return: The days, hours, minutes, and seconds.
        :rtype: tuple

        .. note::

           See: https://www.timeanddate.com/time/map/

           Timezones can be from -11 to +14 based on the political timeszones
           as of 2024-08-09.
        """
        seconds += zone * 3600
        hours = math.floor(seconds / 3600)
        m = (seconds - hours * 3600) / 60
        minutes = math.floor(m)

        if hours < 24:
            day = 0
        else:
            hours -= 24
            day = 1
            seconds -= 86400

        return day, hours, minutes, (seconds - hours * 3600) - minutes * 60

    def _tz_decimal_from_dhms(self, days: int, hours: int, minutes: int,
                              seconds: int) -> int:
        """
        Convert days, hours, minutes, and seconds to a decimal number
        representing percentage of one revolution around the Earth. Where
        the number 1 indicates one revolution.

        :param int days: Number of days.
        :param int hours: Number of hours.
        :param int minutes: The number of minutes.
        :param float seconds: The number of seconds with possible fraction.
        :return: A decimal number.
        :rtype: int

        .. note::

           This method is used in determining time zones.
        """
        return self._seconds_from_dhms(days, hours, minutes, seconds) / 86400

    def _tz_dhms_from_decimal(self, dec: float) -> tuple:
        """
        Convert a decimal number into days, hours, minutes, and seconds
        of a time zone. The decimal number represents the percentage of
        one revolution around the Earth. Where the number 1 indicates
        one revolution.

        :param float dec: A decimal number.
        :return: The days, hours, minutes, and seconds of a time zone.
        :rtype: tuple

        .. note::

           This method is used in determining time zones.
         """
        return self._dhms_from_seconds(dec * 86400)

    def _hms_from_decimal_day(self, dec: float, *, us: bool=False) -> tuple:
        """
        Convert a decimal day to hours, minutes, and seconds. If this
        method is used for a Julian Period day, 0.5 must be added to the
        value before being passed in.

        :param float dec: A decimal number.
        :param bool us: If False (default) no seperate field for microseconds
                        is returned else return microseconds.
        :return: A tuple representing the hour, minute, and seconds.
        :rtype: tuple

        .. note::

           If a whole number as in 10.5 is passed in, the value to the left
           of the decimal will be stripped off before calculations are done.
        """
        h = self._PARTIAL_DAY_TO_HOURS(dec)
        hour = math.floor(h)
        m = self._PARTIAL_HOUR_TO_MINUTE(h)
        minute = math.floor(m)
        second = self._PARTIAL_MINUTE_TO_SECOND(m)

        if us:
            microsec = (self._PARTIAL_SECOND_TO_MICROSECOND(second),)
            second = math.floor(second)
        else:
            microsec = ()

        return (hour, minute, second) + microsec

    def _decimal_day_from_hms(self, h: int, m: int, s: float) -> float:
        """
        Convert hours, minutes, and seconds to a decimal day.

        :param ine h: The hour.
        :param int m: The minute.
        :param float s: The second.
        :return: A decimal value representing the day with a partial that
                 indicates the hours, minutes, and seconds.
        :rtype: float
        """
        return (h * 60 * 60 + m * 60 + s) / 86400

    def _sec_microsec_from_seconds(self, second: float) -> tuple:
        """
        Split the second and microseconds.

        :param float second: The second with a partial indicating the
                             microseconds.
        :return: The second split between the second and microseconds.
        :rtype: tuple
        """
        p = second % 1
        s = abs(second) - p
        s *= -1 if second < 0 else 1
        us = self._PARTIAL_SECOND_TO_MICROSECOND(second)
        return math.floor(s), us

    def _sin_deg(self, theta: float) -> float:
        """
        Convert a value to sine in degrees.

        :param float thete: The value to convert to degrees.
        :return: The degrees representing the value provided.
        :rtype: float

        .. rubric:: LISP code from  Reingold &  Dershowitz CC TUE p. 513
        .. code:: lisp

           (defun sin-degrees (theta)
             ;; TYPE angle -> amplitude
             ;; Sine of theta (given in degrees).
             (sin (radians-from-degrees theta)))
        """
        return math.sin(math.radians(theta))

    def _cos_deg(self, theta: float) -> float:
        """
        Convert a value to the cosine in degrees.

        :param float thete: The value to convert to degrees.
        :return: The degrees representing the value provided.
        :rtype: float

        .. rubric:: LISP code from  Reingold &  Dershowitz CC TUE p. 513
        .. code:: lisp

           (defun cos-degrees (theta)
             ;; TYPE angle -> amplitude
             ;; Cosine of theta (given in degrees).
             (cos (radians-from-degrees theta)))
        """
        return math.cos(math.radians(theta))

    def _sigma(self, lists: tuple, func: object) -> float:
        """
        This gives a summation of a list based on the criteria in the
        provided function.

        :param tuple lists: The list of values to sum.
        :param object func: The function that determins the summation
                            parameters.
        :return: The summation.
        :rtype: float

        .. rubric:: LISP code from  Reingold &  Dershowitz CC TUE p. 473
        .. code:: lisp

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

    def _poly(self, x: float, a: list) -> float:
        """
        This is the Horner method of polynomial used to eliminate the use
        of powers.

        .. note::

           | Instead of:
           | y = A + B * x + C * x^2 + D * x^3 + E * x^4
           | do this
           | y = A + x * (B + x * (C + x * (D + x * E)))

        :param float x: Power of number.
        :param list a: The list of numbers in polynomial.
        :return: The polynomial result.
        :rtype: float

        .. rubric:: LISP code from  Reingold &  Dershowitz CC TUE p. 473
        .. code:: lisp

           (defun poly (x a)
             ;; TYPE (real list-of-reals) -> real
             ;; Sum powers of x with coefficients (from order 0 up) in list a.
             (if (equal a nil)
                 0
               (+ (first a) (* x (poly x (rest a))))))
        """
        return 0 if not a else a[0] + (x * self._poly(x, a[1:]))

    def _days_in_years(self, y: int, *, alt: bool=False) -> int:
        """
        Find the number of days up to the provided year.

        :param int y: The year to count to.
        :param bool alt: If True use the 4|128 rule else if False use the
                         4|100|400 rule. The default is False.
        :return: The count of days including year one to the given year.
        :rtype: int

        .. note::

           This method starts the count from year 1 of the Julian Calendar,
           however, it uses one of the two leap rules described above instead
           of the usual Julian Calendar leap year rule of every 4 year.
        """
        n_4 = y // 4

        if alt:
            n_128 = y // 128
            n_leap_years = n_4 - n_128
        else:
            n_100 = y // 100
            n_400 = y // 400
            n_leap_years = n_4 - n_100 + n_400

        a = y - n_leap_years  # Non-leap years
        b = y - a  # Leap years
        return a * 365 + b * 366

    def _meeus_from_exact(self, jd: float) -> int:
        """
        The returned difference value to convert an exact algorithm jd to
        a Meeus algorithm jd. This is added to the exact jd.

        :param float jd: Exact Julian Period day.
        :return: The difference to subtract from an exact algorithm jd.
        :rtype: int
        """
        jd_diff = (
            (1757641.5, 0), (1794165.5, 1), (1830689.5, 2), (1903738.5, 3),
            (1940262.5, 4), (1976786.5, 5), (2049835.5, 6), (2086359.5, 7),
            (2122883.5, 8), (2195932.5, 9), (2232456.5, 10), (2268980.5, 11),
            (2299158.5, 12),
            )
        diff = 2

        for j, df in jd_diff:
            if jd < j:
                diff = df
                break

        return diff

    def _exact_from_meeus(self, jd: float) -> int:
        """
        The returned difference value to convert a Meeus algorithm jd to
        an exact algorithm jd. This is subtracted from the meeus jd.

        :param float jd: Meeus Julian Period day.
        :return: The difference to subtract from a Meeus algorithm jd.
        :rtype: int
         """
        jd_diff = (
            (1757642.5, 0), (1794167.5, 1), (1830692.5, 2), (1903742.5, 3),
            (1940267.5, 4), (1976792.5, 5), (2049842.5, 6), (2086367.5, 7),
            (2122892.5, 8), (2195942.5, 9), (2232467.5, 10), (2268992.5, 11),
            (2299160.5, 12),
            )
        diff = 2

        for j, df in jd_diff:
            if jd < j:
                diff = df
                break

        return diff

    def _coterminal_angle(self, value: float) -> float:
        """
        Find the Coterminal Angle from a value that is either more than
        360 or less than 0.

        :param float value: The value that is more than 360 or less than 0.
        :return: The adjusted angle to be between 0 and 360.
        :rtype: float
        """
        value = math.fmod(value, 360)
        return value + 360 if value < 0 else value

    def _interpolation_from_three(self, y1: float, y2: float, y3: float,
                                  n: float, normalize: bool=False) -> float:
        """
        Interpolate from three terms with a factor.

        :param float y1: 1st of the three parameters.
        :param float y2: 2nd of the three parameters.
        :param float y3: 3rd of the three parameters.
        :param float n: The factor.
        :param bool mormalize: If `False' (default) no normalization is done
                               else if `True` normalize.
        :return: The three factor interpolation.
        :rtype: float
        """
        a = y2 - y1
        b = y3 - y2

        if normalize:
            a += 360 if a < 0 else 0
            b += 360 if b < 0 else 0

        c = b - a
        return y2 + (n / 2) * (a + b + n * c)

    def _truncate_decimal(self, n: int, places: int) -> int:
        """
        Trucate a decimal to a number of places.

        .. note::

           This is somewhat like rounding, but is used in places where
           rounding gives an invalid results.

        :param int n: Number to truncate.
        :param int places: The number of places to truncate to.
        :return: The truncated number.
        :rtype: int
        """
        p = int('1' + '0' * places)
        return math.floor(n * p) / p

    def _xor_boolean(self, booleans: tuple) -> bool:
        """
        Test that any number of booleans can all be False or only one True.

        :param tuple booleans: A tuple of booleans that cannot be used
                               together.
        :return: True if only one of the booleans are True or if None are True
                 else False.
        :rtype: bool
        """
        count = sum(booleans)
        return (count % 2 == 1) or (count == 0)

    def _ordinal_from_jd(self, jd: float, *, _exact: bool=True) -> int:
        """
        Convert a Julian Period day to an ordinal number.

        :param float jd: The Julian Period day.
        :param bool _exact: If True (default) the incoming JD is the more
                            astronomically exact Julin Period day else it
                            is the historically correct (Meeus) JD.
        :return: The ordinal number relating to the Julian Period day.
        :rtype: int
        """
        jd -= 0 if _exact else self._exact_from_meeus(jd)
        # We add 1 because ordinal date representations satrt at 1 not 0.
        return math.floor(jd - self._JULIAN_CAL_EPOCH) + 1

    def _jd_from_ordinal(self, ordinal: int, *, exact: bool=True) -> float:
        """
        Convert an ordinal number to a Julian Period day.

        :param int ordinal: The ordinal number of days starting with one on the
                            first day od the Julian Calendar.
        :param bool exact: If True (default) the outgoing Julian Period day is
                           the more astronomically exact Julin Period day else
                           if False it is the historically correct (Meeus) JD.
        :return: The Julian Period day relating to the ordinal number.
        :rtype: float
        """
        # We subtract 1 because ordinal date representations satrt at 1 not 0.
        jd = self._JULIAN_CAL_EPOCH + (ordinal - 1)
        return jd + (0 if exact else self._meeus_from_exact(jd))
