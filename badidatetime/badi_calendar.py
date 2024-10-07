# -*- coding: utf-8 -*-
#
# badidatetime/badi_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from badidatetime.base_calendar import BaseCalendar
from badidatetime.gregorian_calendar import GregorianCalendar


class BahaiCalendar(BaseCalendar):
    """
    Implementation of the Baha'i (Badi) Calendar.
    """
    # WGS84:          35.689252, 51.3896 1595m 3.5
    # WGS84--https://coordinates-converter.com/
    # https://whatismyelevation.com/location/35.63735,51.72569/Tehran--Iran-
#https://en-us.topographic-map.com/map-g9q1h/Tehran/?center=35.69244%2C51.19492
#https://www.google.com/maps/place/Tehran,+Tehran+Province,+Iran/@35.9098957,51.51371,9.49z/data=!4m6!3m5!1s0x3f8e02c69b919039:0x17c26479772c5928!8m2!3d35.6891975!4d51.3889736!16s%2Fm%2F025zk75?entry=ttu
    # https://gml.noaa.gov/grad/solcalc/ Sunset data
    # Nur Mazandaran Province, Iran (City Center)
    #BAHAI_LOCATION = (36.569336, 52.0050234, 3.5, 0)
    BAHAI_LOCATION = (35.681117, 51.4016521, 3.5, 0) # Moniriyeh Square Tehran
    GMT_LOCATION = (51.477928, -0.001545, 0, 0)
    BADI_EPOCH = 2394644.261791 # 2394646.261791 using Meeus' algorithm
    BADI_MONTH_NAMES = (
        (1, 'Bahá'), (2, 'Jalál'), (3, 'Jamál'), (4, "'Aẓamat"), (5, 'Núr'),
        (6, 'Raḥmat'), (7, 'Kalimát'), (8, 'Kamál'), (9, "Asmá'"),
        (10, "'Izzat"), (11, 'Mashíyyat'), (12, "'Ilm"), (13, 'Qudrat'),
        (14, 'Qawl'), (15, 'Masá’il'), (16, 'Sharaf'), (17, 'Sulṭán'),
        (18, 'Mulk'), (0, 'Ayyám-i-Há'), (19, "'Alá'")
        )
    KULL_I_SHAY_MIN = -5
    KULL_I_SHAY_MAX = 4
    MINYEAR = -1842
    MAXYEAR = 1161

    def __init__(self, *args, **kwargs):
        super().__init__()
        # kull_i_shay: 361-year (19^2) vahid (integer)
        # vahid: (integer) 19-year vahid
        # year: (integer) 1 - 19
        # month: (integer) 1 - 19 plus 0 for Ayyām-i-Hā
        # day: (integer) 1 - 19
        # Baha'i date: [kull_i_shay, vahid, year, month, day]
        self._bahai_date = None
        self._gc = GregorianCalendar()

    def parse_gregorian_datetime(self, dt:datetime.datetime, lat:float=None,
                                 lon:float=None, zone:float=None, *,
                                 short=False) -> None:
        """
        Parse a Gregorian date to a long form Badi date.
        """
        self._gc.parse_datetime(dt)
        jd = self._gc.jd_from_gregorian_date(self._gc.date_representation,
                                             exact=True)
        self.date_representation = self.badi_date_from_jd(
            jd, lat=lat, lon=lon, zone=zone, short=short)

    #def parse_badi_datetime(self, dt:badi_datetime) -> None:
    #    """
    #    Parse a Badi date to a long form Badi date.
    #    """
    #    self._gc.parse_datetime(dt)
    #    jd = self._bc.jd_from_badi_date(self._gc.date_representation)
    #    self.date_representation = self.badi_date_from_jd(jd)

    @property
    def date_representation(self) -> tuple:
        return self._bahai_date

    @date_representation.setter
    def date_representation(self, representation:tuple=None):
        self._bahai_date = representation

    def utc_sunset(self, date:tuple, lat:float=None, lon:float=None,
                   zone:float=None) -> tuple:
        """
        Return the time of sunset in UTC time for the given Badi Day.

        :param date: A Badi date.
        :type date: tuple
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :return: The hour, minute, and second of sunset based on the
                 provided coordinates.
        :rtype: tuple
        """
        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        jd = self.jd_from_badi_date(date[:3], lat, lon, zone)
        return self.hms_from_decimal_day(jd + 0.5)

    def naw_ruz_g_date(self, year:int, lat:float=None, lon:float=None,
                       zone:float=None, *, hms:bool=False) -> tuple:
        """
        Return the Badi date for Naw-Ruz from the given Badi year.

        :param year: A Badi year.
        :type year: int
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :param hms: If True the output returns the hours, minutes, and seconds
                    as seperate fields. If False the day has a decimal value
                    indicating the hours, minutes, and seconds.
        :type hms: bool
        :return: A Gregorian date.
        :rtype: tuple
        """
        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        jd = self.jd_from_badi_date((year, 1, 1), lat, lon, zone)
        date = self._gc.gregorian_date_from_jd(jd, exact=True)
        return self._gc.ymdhms_from_date(date) if hms else date

    def first_day_of_ridvan_g_date(self, year:int, lat:float=None,
                                   lon:float=None, zone:float=None, *,
                                   hms:bool=False) -> tuple:
        """
        Find the first day of Riḍván in either with or without hours,
        minutes, and seconds.
        If the latitude, longitude, and time zone are not given Riḍván time
        of day is determined for the city of Nur in Iran.

        :param year: A Badi year.
        :type year: int
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :param hms: If True the output returns the hours, minutes, and second
                    as seperate fields. If False the day has a decimal value
                    indicating the hours, minutes, and seconds.
        :type hms: bool
        :return: A Gregorian date.
        :rtype: tuple
        """
        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        jd = self.jd_from_badi_date((year, 2, 13), lat, lon, zone)
        date = self._gc.gregorian_date_from_jd(jd, exact=True)
        return self._gc.ymdhms_from_date(date) if hms else date

    def jd_from_badi_date(self, b_date:tuple, lat:float=None, lon:float=None,
                          zone:float=None, _chk_on=True) -> float:
        """
        Convert a Badi short form date to Julian period day.

        :param b_date: A short form Badi date.
        :type b_date: tuple
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The Julian Period day.
        :rtype: float
        """
        year, month, day = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date, trim=True, _chk_on=_chk_on),
            short=True, _chk_on=_chk_on)

        if month == 0: # Ayyam-i-Ha
            m = 18 * 19
        elif month < 19: # month 1 - 18
            m = (month - 1) * 19
        else: # month == 19:
            m = 18 * 19 + 4 + self._is_leap_year(year)

        td = self._days_in_years(year-1)
        jd = td + math.floor(self.BADI_EPOCH) + m + day

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        # The diff value converts my jd to the Meeus algorithm for
        # determining the sunset jd.
        diff = self._meeus_algorithm_jd_compensation(jd)
        ss_a = self._sun_setting(jd + diff, lat, lon, zone) % 1
        return round(jd + ss_a + self._get_coff(year), self.ROUNDING_PLACES)

    def _get_coff(self, year):
        def process_segment(y, coff1, coff2, onoff):
            func = lambda y: 1 < y < 100 and y % 4 in onoff
            coff = 0

            if coff1 and y in (1, 34, 67, 100):
                coff = coff1
            elif coff2 and func(y):
                coff = coff2

            return coff

        # General ranges are determined with:
        # ./contrib/misc/badi_jd_tests.py -p -S start_year -E end_year
        # Where -S is the 1st year and -E is the nth year + 1 that needs to
        # be process. Use the following command to test the results.
        # ./contrib/misc/badi_jd_tests.py -qX -S start_year -E end_year
        # The if or elif statements may not have the same ranges as are
        # passed into the process_segment method because we may need to skip
        # over already good results.
        if year < -1819: # -1842 to -1820 (range -S-1920 -E-1821)
            coff = process_segment(-1821 - year, -1, -1, (1, 2))
        elif year < -1796: # -1819 to -1797 (range -S-1819 -E-1800)
            coff = process_segment(-1800 - year, -1, -1, (1, 2, 3))
        elif year < -1747: # -1796 to -1748 (range -S-1792  -E-1782)
            coff = process_segment(-1782 - year, 0, 1, (2,))
        elif year < -1715: # -1747 to -1716 (range -S-1747 -E-1717)
            coff = process_segment(-1717 - year, 0, -1, (2,))
        elif year < -1697: # -1715 to -1698 (range -S-1715 -E-1701)
            coff = process_segment(-1701 - year, -1, -1, (1, 2))
        elif year < -1684: # -1697 to -1685 (range -S-1697 -E-1687)
            coff = process_segment(-1687 - year, 1, 1, (1, 2))
        elif year < -1615: # -1684 to -1616 (range -S-1684 -E-1655)
            coff = process_segment(-1655 - year, 1, 1, (1,))
        elif year < -1583: # -1615 to -1584 (range -S-1615 -E-1586)
            coff = process_segment(-1586 - year, -1, -1, (1,))
        elif year < -1551: # -1585 to -1552 (range -S-1585 -E-1553)
            coff = process_segment(-1553 - year, -1, -1, (1, 2))
        elif year < -1519: # -1551 to -1520 (range -S-1551 -E-1520)
            coff = process_segment(-1520 - year, -1, -1, (1, 2, 3))
        elif year < -1479: # -1519 to -1480 (range -S-1519 -E-1499)
            coff = process_segment(-1499 - year, -1, -1, (0, 1, 2, 3))
        elif year < -1447: # -1779 to -1448 (range -S-1479 -E-1450)
            coff = process_segment(-1450 - year, -1, -1, (1,))
        elif year < -1415: # -1447 to -1416 (range -S-1447 -E-1417)
            coff = process_segment(-1417 - year, -1, -1, (1, 2))
        elif year < -1396: # -1415 to -1397 (range -S-1415 -E-1400)
            coff = process_segment(-1400 - year, -1, -1, (1, 2, 3))
        elif year < -1347: # -1396 to -1348 (range -S-1396 -E-1387)
            coff = process_segment(-1387 - year, 1, 1, (1,))
        elif year < -1315: # -1347 to -1316 (range -S-1347 -E-1318)
            coff = process_segment(-1318 - year, -1, -1, (1,))
        elif year < -1297: # -1315 to -1298 (range -S-1315 -E-1301)
            coff = process_segment(-1301 - year, -1, -1, (1, 2,))
        elif year < -1280: # -1296 to -1281 (range -S-1297 -E-1283)
            coff = process_segment(-1283 - year, 1, 1, (1, 2))
        elif year < -1215: # -1280 to -1216 (range -S-1280 -E-1251)
            coff = process_segment(-1251 - year, 1, 1, (1,))
        elif year < -1183: # -1215 to -1184 (range -S-1215 -E-1186)
            coff = process_segment(-1186 - year, -1, -1, (1,))
        elif year < -1151: # -1183 to -1152 (range -S-1183 -E-1153)
            coff = process_segment(-1153 - year, -1, -1, (1, 2))
        elif year < -1119: # -1151 to -1120 (range -S-1151 -E-1120)
            coff = process_segment(-1120 - year, -1, -1, (1, 2, 3,))
        elif year < -1079: # -1119 to -1080 (range -S-1119 -E-1099)
            coff = process_segment(-1099 - year, -1, -1, (0, 1, 2, 3))
        elif year < -1051: # -1079 to -1052 (range -S-1079 -E-1054)
            coff = process_segment(-1054 - year, -1, -1, (1,))
        elif year < -1019: # -1051 to -1020 (range -S-1051 -E-1021)
            coff = process_segment(-1021 - year, -1, -1, (1, 2))
        elif year < -996: # -1019 to -997 (range -S-1019 -E-1000)
            coff = process_segment(-1000 - year, -1, -1, (1, 2, 3))
        elif year < -947: # -996 to -948 (range -S-996 -E-987)
            coff = process_segment(-987 - year, 1, 1, (1,))
        elif year < -915: # -947 to -916 (range -S-947 -E-918)
            coff = process_segment(-918 - year, -1, -1, (1,))
        elif year < -897: # -915 to -898 (range -S-915 -E-901
            coff = process_segment(-901 - year, -1, -1, (1, 2))
        elif year < -880: # -897 to -881 (range -S-897 -E-883)
            coff = process_segment(-883 - year, 1, 1, (1, 2))
        elif year < -815: # -880 to -816 (range -S-880 -E-851)
            coff = process_segment(-851 - year, 1, 1, (1,))
        elif year < -783: # -815 to -784 (range -S-815 -E-786)
            coff = process_segment(-786 - year, -1, -1, (1,))
        elif year < -751: # -783 to -752 (range -S-783 -E-753
            coff = process_segment(-753 - year, -1, -1, (1, 2,))
        elif year < -719: # -751 to -720 (range -S-751 -E-720)
            coff = process_segment(-720 - year, -1, -1, (1, 2, 3))
        elif year < -683: # -719 to -684 (range -S-719 -E-699)
            coff = process_segment(-699 - year, -1, -1, (0, 1, 2, 3))
        elif year < -651: # -683 to -652 (range -S-683 -E-654)
            coff = process_segment(-654 - year, -1, -1, (1,))
        elif year < -619: # -651 to -620 (range -S-651 -E-621)
            coff = process_segment(-621 - year, -1, -1, (1, 2,))
        elif year < -596: # -619 to -597 (range -S-619 -E-600)
            coff = process_segment(-600 - year, -1, -1, (1, 2, 3))
        elif year < -547: # -596 to -548 (range -S-596 -E-587)
            coff = process_segment(-587 - year, 1, 1, (1,))
        elif year < -515: # -547 to -516 (range -S-547 -E-518)
            coff = process_segment(-518 - year, -1, -1, (1,))
        elif year < -497: # -515 to -498 (range -S-515 -E-501)
            coff = process_segment(-501 - year, -1, -1, (1, 2,))
        elif year < -480: # -497 to -481 (range -S-497 -E-483)
            coff = process_segment(-483 - year, 1, 1, (1, 2))
        elif year < -415: # -480 to -416 (range -S-480 -E-451)
            coff = process_segment(-451 - year, 1, 1, (1,))
        elif year < -383: # -415 to -384 (range -S-415 -E-386)
            coff = process_segment(-386 - year, -1, -1, (1,))
        elif year < -351: # -383 to -352 (range -S-383 -E-353)
            coff = process_segment(-353 - year, -1, -1, (1, 2))
        elif year < -319: # -351 to -320 (range -S-351 -E-320)
            coff = process_segment(-320 - year, -1, -1, (1, 2, 3,))
        elif year < -279: # -319 to -280 (range -S-319 -E-299)
            coff = process_segment(-299 - year, -1, -1, (0, 1, 2, 3))
        elif year < -247: # -279 to -248 (range -S-279 -E-250)
            coff = process_segment(-250 - year, -1, -1, (1,))
        elif year < -215: # -247 to -216 (range -S-247 -E-217)
            coff = process_segment(-217 - year, -1, -1, (1, 2))
        elif year < -196: # -215 to -197 (range -S-215 -E-200)
            coff = process_segment(-200 - year, -1, -1, (1, 2, 3))
        elif year < -147: # -196 to -148 (range -S-196 -E-183)
            coff = process_segment(-183 - year, 1, 1, (1,))
        elif year < -115: # -147 to -116 (range -S-147 -E-118)
            coff = process_segment(-118 - year, -1, -1, (1,))
        elif year < -99: # -115 to -100 (range -S-115 -E-101)
            coff = process_segment(-101 - year, -1, -1, (1, 2,))
        elif year < -80: # -99 to -81 (range -S-97 -E-83)
            coff = process_segment(-83 - year, 1, 1, (1, 2))
        elif year < -15: # -80 to -16 (range -S-80 -E-51)
            coff = process_segment(-51 - year, 1, 1, (1,))
        elif year < 17: # -15 to 16 (range -S-15 -E14)
            coff = process_segment(14 - year, -1, -1, (1,))
        elif year < 49: # 17 to 48 (range -S17 -E47)
            coff = process_segment(47 - year, -1, -1, (1, 2))
        elif year < 81: # 49 to 80 (range -S49 -E80)
            coff = process_segment(80 - year, -1, -1, (1, 2, 3))
        elif year < 117: # 81 to 116 (range -S81 -E101)
            coff = process_segment(101 - year, -1, -1, (0, 1, 2 , 3))
        elif year < 153: # 117 to 129 (range -S117 -E150)
            coff = process_segment(150 - year, -1, -1, (1,))
        elif year < 185: # 153 to 184 (range -S153 -E183)
            coff = process_segment(183 - year, -1, -1, (1, 2))
        elif year < 204: # 185 to 203 (range -S185 -E200)
            coff = process_segment(200 - year, -1, -1, (1, 2, 3))
        elif year < 253: # 204 to 252 (range -S204 -E213)
            coff = process_segment(213 - year, 1, 1, (1,))
        elif year < 285: # 253 to 284 (range -S253 -E282)
            coff = process_segment(282 - year, -1, -1, (1,))
        elif year < 303: # 285 to 302 (range -S285 -E299)
            coff = process_segment(299 - year, -1, -1, (1, 2))
        elif year < 320: # 303 to 319 (range -S303 -E317)
            coff = process_segment(317 - year, 1, 1, (1, 2))
        elif year < 385: # 320 to 384 (range -S320 -E349)
            coff = process_segment(349 - year, 1, 1, (1,))
        elif year < 417: # 385 to 416 (range -S385 -E414)
            coff = process_segment(414 - year, -1, -1, (1,))
        elif year < 449: # 417 to 448 (range -S417 -E447)
            coff = process_segment(447 - year, -1, -1, (1, 2))
        elif year < 481: # 449 to 480 (range -S449 -E480)
            coff = process_segment(480 - year, -1, -1, (1, 2, 3))
        elif year < 521: # 481 to 520 (range -S481 -E501)
            coff = process_segment(501 - year, -1, -1, (0, 1, 2, 3))
        elif year < 553: # 521 to 552 (range -S521 -E550)
            coff = process_segment(550 - year, -1, -1, (1,))
        elif year < 585: # 553 to 584 (range -S553 -E583)
            coff = process_segment(583 - year, -1, -1, (1, 2))
        elif year < 604: # 585 to 603 (range -S585 -E600)
            coff = process_segment(600 - year, -1, -1, (1, 2 ,3))
        elif year < 653: # 604 to 652 (range -S604 -E613)
            coff = process_segment(613 - year, 1, 1, (1,))
        elif year < 685: # 653 to 684 (range -S653 -E682)
            coff = process_segment(682 - year, -1, -1, (1,))
        elif year < 703: # 685 to 702 (range -S685 -E699)
            coff = process_segment(699 - year, -1, -1, (1, 2))
        elif year < 720: # 703 to 719 (range -S703 -E717)
            coff = process_segment(717 - year, 1, 1, (1, 2))
        elif year < 785: # 720 to 784 (range -S720 -E749)
            coff = process_segment(749 - year, 1, 1, (1,))
        elif year < 817: # 785 to 816 (range -S785 -E814)
            coff = process_segment(814 - year, -1, -1, (1,))
        elif year < 849: # 817 to 848 (range -S817 -E847)
            coff = process_segment(847 - year, -1, -1, (1, 2))
        elif year < 881: # 849 to 880 (range -S849 -E880)
            coff = process_segment(880 - year, -1, -1, (1, 2, 3))
        elif year < 917: # 881 to 916 (range -S881 -E901)
            coff = process_segment(901 - year, -1, -1, (0, 1, 2, 3))
        elif year < 949: # 917 to 948 (range -S917 -E946)
            coff = process_segment(946 - year, -1, -1, (1,))
        elif year < 981: # 849 to 980 (range -S949 -E979)
            coff = process_segment(979 - year, -1, -1, (1, 2))
        elif year < 1004: # 981 to 1003 (range -S981 -E1000)
            coff = process_segment(1000 - year, -1, -1, (1, 2, 3))
        elif year < 1049: # 1004 to 1048 (range -S1004 -E1013)
            coff = process_segment(1013 - year, 1, 1, (1,))
        elif year < 1081: # 1049 to 1080 (range -S1049 -E1078)
            coff = process_segment(1078 - year, -1, -1, (1,))
        elif year < 1103: # 1081 to 1102 (range -S1081 -E1099)
            coff = process_segment(1099 - year, -1, -1, (1, 2))
        elif year < 1116: # 1103 to 1115 (range -S1103 -E1113)
            coff = process_segment(1113 - year, 1, 1, (1, 2))
        elif year < 1162: # 1116 to 1161 (range -S1116 -E1145)
            coff = process_segment(1145 - year, 1, 1, (1,))
        else:
            coff = 0

        return coff

    def badi_date_from_jd(self, jd:float, lat:float=None, lon:float=None,
                          zone:float=None, *, short:bool=False,
                          fraction:bool=False, rtd:bool=False) -> tuple:
        """
        Convert a Julian Period day to a Badi date.

        :param jd: Julian Period day.
        :type jd: float
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The standard time zone.
        :type zone: float
        :param short: If True then parse for a short date else if False
                      parse for a long date.
        :type short: bool
        :param fraction: This will return a short date with a possible
                         fraction on the day.
        :type fraction: bool
        :param rtd: Round to day.
        :type rtd: bool
        :return: The Badi date from a Julian Period day.
        :rtype: tuple
        """
        def get_leap_year_info(year):
            leap = self._is_leap_year(year)
            yds = 366 if leap else 365
            ld = 4 + leap
            return leap, yds, ld

        def check_and_fix_day(cjd, y, lat=None, lon=None, zone=None):
            fjdy = self.jd_from_badi_date((y, 1, 1), lat, lon, zone)
            return y-1 if (math.floor(fjdy) - math.floor(cjd)) > 0 else y

        md = jd - (self.BADI_EPOCH - 1)
        year = math.floor(md / self.MEAN_TROPICAL_YEAR) + 1
        leap, yds, ld = get_leap_year_info(year)

        if (y := check_and_fix_day(jd, year, lat, lon, zone)):
            year = y
            leap, yds, ld = get_leap_year_info(year)

        fjdy = self.jd_from_badi_date((year, 1, 1), lat, lon, zone)
        days = math.floor(jd) - math.floor(fjdy) + 1

        if days <= 342: # Month 1 - 18
            m_days = days % 19
            day = 19 if m_days == 0 else m_days
        elif (342 + ld) < days <= yds: # Month 19
            day = days - (342 + ld)
        else: # Ayyam-i-Ha
            day = days % 342

        month_days = [(n, 19) for n, v in self.BADI_MONTH_NAMES]
        month_days[18] = (0, ld)

        for month, ds in month_days:
            if days <= ds: break
            days -= ds

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        day = self._adjust_day_for_24_hours(
            jd, lat, lon, zone, day=day, rtd=rtd)

        if fraction:
            b_date = (year, month, day)
        else:
            date = self.long_date_from_short_date((year, month, day),
                                                  trim=True)
            b_date = self.kvymdhms_from_b_date(date, short=short, trim=True)

        return b_date

    def short_date_from_long_date(self, b_date:tuple, *, trim:bool=False,
                                  _chk_on:bool=True) -> tuple:
        """
        Convert a long date (kvymdhms) to a short date (ymdhms). In either
        case microseconds could also be provided.

        :param b_date: A long form date with or without microseconds.
        :type b_date: tuple
        :param trim: Trim the ms, ss, mm, and hh in that order.
        :type trim: bool
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The short form Badi date.
        :rtype: tuple
        """
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hh, mm, ss, ms = self._get_hms(b_date)
        y = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year
        hmsms = self._trim_hms((hh, mm, ss, ms)) if trim else (hh, mm, ss, ms)
        date = (y, month, day) + hmsms
        _chk_on and self._check_valid_badi_date(b_date, short_in=True)
        return date

    def long_date_from_short_date(self, date:tuple, *, trim:bool=False,
                                  _chk_on:bool=True) -> tuple:
        """
        Convert a date to a short date (ymdhms) to a long date (kvymdhms).

        :param b_date: A short form date with or without microseconds.
        :type b_date: tuple
        :param trim: Trim the ms, ss, mm, and hh in that order.
        :type trim: bool
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The long form Badi date.
        :rtype: tuple
        """
        y, month, day = date[:3]
        hh, mm, ss, ms = self._get_hms(date, short_in=True)
        k = y / 361
        kull_i_shay = 0 if y == 0 else math.ceil(k)
        k0 = self._truncate_decimal(k % 1, self.ROUNDING_PLACES)
        #k0 = round(k % 1, self.ROUNDING_PLACES)
        v = k0 / 19 * 361

        if v == 0: # If there is no fraction in v
            vahid = 19
            year = 19
        else:
            vahid = math.ceil(v)
            year = math.ceil(v % 1 * 19)

        hmsms = self._trim_hms((hh, mm, ss, ms)) if trim else (hh, mm, ss, ms)
        b_date = (kull_i_shay, vahid, year, month, day) + hmsms
        _chk_on and self._check_valid_badi_date(b_date)
        return b_date

    def date_from_kvymdhms(self, b_date:tuple, *, short:bool=False,
                           _chk_on:bool=True) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second, ms)
        into a (Kull-i-Shay, Váḥid, year, month, day.partial) date.

        :param b_date: The Badi date in long form.
        :type b_date: tuple
        :param short: If True the short form Badi date is returned else the
                      long form is returned.
        :type short: bool
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The long or short form Badi date with hours, minutes,
                 seconds, and microseconds if set.
        :rtype: tuple
        """
        _chk_on and self._check_valid_badi_date(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second, ms = self._get_hms(b_date)
        day += round(self.HR(hour) + self.MN(minute) + self.SEC(second) +
                     self.MS(ms), self.ROUNDING_PLACES)
        date = (kull_i_shay, vahid, year, month, day)
        return (self.short_date_from_long_date(date, trim=True)
                if short else date)

    def kvymdhms_from_b_date(self, b_date:tuple, *, ms:bool=False,
                             short:bool=False, trim:bool=False,
                             _chk_on:bool=True) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day.partial) into
        (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second) or if
        short is True (year, month, day, hour, minute, second). If ms is
        True the seconds are split to second and microsecond.

        :param b_date: The Badi date in long form.
        :type b_date: tuple
        :param ms: If True the seconds are split to seconds amd microseconds
                   else if False the seconds has a partial day as a decimal.
        :type ms: bool
        :param short: If True the short form Badi date is returned else the
                      long form is returned.
        :type short: bool
        :param trim: Trim the ms, ss, mm, and hh in that order.
        :type trim: bool
        :param _chk_on: If True (default) all date check are enforced else
                        if False they are turned off. This is only used
                        internally. Don't use unless you know what you are
                        doing.
        :type _chk_on: bool
        :return: The long or short form Badi date with hours, minutes,
                 seconds, and microseconds if set.
        :rtype: tuple
        """
        _chk_on and self._check_valid_badi_date(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        dlen = len(b_date)

        if dlen == 5:
            hd = self.PARTIAL_DAY_TO_HOURS(day)
            hour = math.floor(hd)
            md = self.PARTIAL_HOUR_TO_MINUTE(hd)
            minute = math.floor(md)
            second = round(self.PARTIAL_MINUTE_TO_SECOND(md),
                           self.ROUNDING_PLACES)
        else:
            hour = b_date[5] if dlen > 5 else 0
            minute = b_date[6] if dlen > 6 else 0
            second = b_date[7] if dlen > 7 else 0

        date = (kull_i_shay, vahid, year, month, math.floor(day))

        if ms:
            hmsms = (hour, minute, *self._sec_microsec_from_seconds(second))
        else:
            hmsms = (hour, minute, second)

        date += self._trim_hms(hmsms) if trim else hmsms
        return (self.short_date_from_long_date(date, trim=trim)
                if short else date)

    def badi_date_from_gregorian_date(self, g_date:tuple, *, short:bool=False,
                                      _exact:bool=True,
                                      rtd:bool=False) -> tuple:
        """
        Get the Badi date from the Gregorian date.

        :param g_date: A Gregorian date.
        :type g_date: tuple
        :param short: If True then parse for a short date else if False
                      parse for a long date.
        :type short: bool
        :param _exact: Use the more exact Julian Period algorithm. Default
                       is True. This should generally be set to True, a
                       False value will give inaccurate results and is used
                       for testing only.
        :type _exact: bool
        :param rtd: Round to day.
        :type rtd: bool
        :return: A Badi date long or short form.
        :rtype: tuple
        """
        jd = self._gc.jd_from_gregorian_date(g_date, exact=_exact)
        return self.badi_date_from_jd(jd, short=short, rtd=rtd)

    def gregorian_date_from_badi_date(self, b_date:tuple, lat:float=None,
                                      lon:float=None, zone:float=None, *,
                                      _exact=True) -> tuple:
        """
        Get the Gregorian date from the Badi date.

        :param b_date: A Badi date short form.
        :type b_date: tuple
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The standard time zone.
        :type zone: float
        :param _exact: Use the more exact Julian Period algorithm. Default
                       is True. This should generally be set to True, a
                       False value, in this method will give inaccurate
                       results and is used for testing only.
        :type _exact: bool
        :return: The Gregorian date.
        :rtype: tuple
        """
        jd = self.jd_from_badi_date(b_date, lat, lon, zone)
        return self._gc.gregorian_date_from_jd(jd, exact=_exact)

    def posix_timestamp(self, t:float, lat=None, lon=None, zone=None, *,
                        short=False) -> tuple:
        """
        Get the Badi date from a POSIX timestamp.

        :param t: Timestamp
        :type t: float
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The time zone.
        :type zone: float
        :param short: If True then parse for a short date else if False
                      parse for a long date.
        :type short: bool
        :return: A Badi date long or short form.
        :rtype: tuple
        """
        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        days = math.floor(t / 86400)
        jd = days + self.POSIX_EPOCH
        jd += t % 86400 / 86400
        return self.badi_date_from_jd(jd, lat, lon, zone, short=short)

    def midday(self, date:tuple, *, hms=False) -> tuple:
        """
        Find the midday time in hours with fraction.

        :param date: Badi date short or long.
        :type date: tuple
        :param hms: If True return the hours, minutes, and seconds else
                    if False return the decimal value.
        :type hms: bool
        :return: Midday in hours, minutes, and seconds.
        :rtype: tuple
        """
        if len(date) == 5:
            b_date = self.short_date_from_long_date(date, trim=True)
        else:
            b_date = date

        jd = self.jd_from_badi_date(b_date)
        diff0 = self._meeus_algorithm_jd_compensation(jd)
        ss0 = self._sun_setting(jd + diff0, *self.GMT_LOCATION[:3])
        diff1 = self._meeus_algorithm_jd_compensation(jd + 1)
        ss1 = self._sun_setting(jd + 1 + diff1, *self.GMT_LOCATION[:3])
        mid = (ss1 - ss0) / 2
        return self.hms_from_decimal_day(mid) if hms else mid

    def _trim_hms(self, hms:tuple) -> tuple:
        """
        Trim the hours, minutes, seconds or microseconds off if zero unless
        a lower value was not zero.

        :param hms: An hour, minute, and second object.
        :type hms: tuple
        :return: An object with the lower order parts stripped off if
                 they have a zero value.
        :rtype: tuple
        """
        items = []
        has = False

        for v in reversed(hms):
            if v == 0 and not has:
                continue
            else:
                items.append(v)
                has = True

        return tuple(reversed(items))

    def _check_valid_badi_date(self, b_date:tuple, short_in=False) -> None:
        """
        Check that the Kull-i-Shay, Váḥids, year, month, day, hour, minute,
        and second values are valid.

        :param b_date: A long form Badi date.
        :type b_date: tuple
        :param short_in: If True then parse for a short date else if False
                         parse for a long date. This is for incoming dates
                         not outgoing dates as in most other uses of 'short'.
        :type short_in: bool
        :return: Nothing
        :rtype: None
        :raises AssertionError: When a date Váḥid, year, month, day, hour,
                                minute, or second are out of range.
        """
        cycle = 20

        if not short_in: # Long Badi date
            kull_i_shay, vahid, year, month, day = b_date[:5]
            hour, minute, second, ms = self._get_hms(b_date)
            assert (self.KULL_I_SHAY_MIN <= kull_i_shay
                    <= self.KULL_I_SHAY_MAX), (
                f"Invalid kull-i-shay {kull_i_shay}, it must be in the range "
                f"of [{self.KULL_I_SHAY_MIN}, {self.KULL_I_SHAY_MAX}].")
            assert 1 <= vahid < cycle, (
                f"Invalid Váḥids '{vahid}' in a Kull-i-Shay’, it must be in "
                "the range of [1, 19].")
            assert 1 <= year < cycle, (
                f"Invalid year '{year}' in a Váḥid, it must be in the "
                "range of [1, 19].")
            ly = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year
        else: # Short Badi date
            year, month, day = b_date[:3]
            hour, minute, second, ms = self._get_hms(b_date, short_in=True)
            assert self.MINYEAR <= year <= self.MAXYEAR, (
                f"Invalid year '{year}' it must be in the range of ["
                f"{self.MINYEAR}, {self.MAXYEAR}].")
            ly = year

        assert 0 <= month < cycle, (
            f"Invalid month '{month}', it must be in the range of [0, 19].")
        # This is Ayyām-i-Hā and could be 4 or 5 days depending on leap year.
        cycle = (5 + self._is_leap_year(ly)) if month == 0 else cycle
        assert 1 <= day < (cycle), (
            f"Invalid day '{day}' for month '{month}', it must be in the "
            f"range of [1, {cycle-1}].")
        assert 0 <= hour < 25, (
            f"Invalid hour '{hour}', it must be in the range of [0, 24].")
        assert 0 <= minute < 60, (
            f"Invalid minute '{minute}', it must be in the range of [0, 59].")
        assert 0 <= second < 60, (
            f"Invalid second '{second}', it must be in the range of [0, 59].")
        assert 0 <= ms < 1000000, (
            f"Invalid microseconds '{ms}', it must be in the range of "
            "[0, 999999].")

        # Check if there are any fractionals that invalidate other values.
        if any((hour, minute, second)):
            assert not day % 1, (
                "If there is a part day then there can be no hours, minutes, "
                "or seconds.")

        if any((minute, second)):
            assert not hour % 1, ("If there is a part hour then there can "
                                  "be no minutes or seconds.")

        if second:
            assert not minute % 1, (
                "If there is a part minute then there can be no seconds.")

    def _is_leap_year(self, year:tuple) -> bool:
        """
        Return a Boolean True if a Badi leap year, False if not.

        :param date: This value must be a Badi short form year.
        :type year: int
        :return: A Boolean indicating if a leap year or not.
        :rtype: bool
        """
        return self._days_in_year(year) == 366

    def _days_in_year(self, year:int) -> int:
        """
        Determine the number of days in the provided Badi year.

        :param year: The Badi year to process.
        :type year: int
        :return: The number of days.
        :rtype: int
        """
        jd_n0 = self.jd_from_badi_date((year, 1, 1))
        jd_n1 = self.jd_from_badi_date((year + 1, 1, 1))
        return int(math.floor(jd_n1) - math.floor(jd_n0))

    def _get_hms(self, date:tuple, *, short_in:bool=False) -> tuple:
        """
        Parse the hours, minutes, seconds, and microseconds, if they exist
        for either the short or long form Badi date.

        :param date: A long or short form Badi date.
        :type date: tuple
        :param short_in: If True then parse for a short date else if False
                         parse for a long date. This is for incoming dates
                         not outgoing dates as in most other uses of 'short'.
        :type short_in: bool
        :return: The relevant hours, minutes, and seconds.
        :rtype: tuple
        """
        t_len = len(date)
        s = 3 if short_in else 5
        hour = date[s] if t_len > s and date[s] is not None else 0
        minute = date[s+1] if t_len > s+1 and date[s+1] is not None else 0
        second = date[s+2] if t_len > s+2 and date[s+2] is not None else 0
        ms = date[s+3] if t_len > s+3 and date[s+3] is not None else 0
        return hour, minute, second, ms

    def _meeus_algorithm_jd_compensation(self, jd:float) -> int:
        """
        The returned diff value converts my jd to the Meeus algorithm jd
        for determining the sunset jd.

        :param jd: My Julian Period day.
        :type jd: float
        :return: The difference to subtract from my jd algorithm to arrive
                 at Meeus' jd algorithm so that all his algorithms can be
                 use accurately.
        :rtype: int
        """
        jd_diff = (
            (1757642.5, 0), (1794165.5, 1), (1830689.5, 2), (1903738.5, 3),
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

    def _adjust_day_for_24_hours(self, jd:float, lat:float, lon:float,
                                 zone:float, *, day:float=None,
                                 hms:bool=False, rtd=False) -> float|tuple:
        """
        We have to deal with days that are either more or less than 24 hours.
        This method does two things. It corrects the Badi time which starts
        at sunset when given a Julian Period day which starts at noon. It
        also corrects the day and the fraction of the day when the Badi day
        is more or less than 24 hours.

        :param jd: The Julian Period day possibly with a fraction.
        :type jd: float
        :param lat: The latitude.
        :type lat: float
        :param lon: The longitude.
        :type lon: float
        :param zone: The standard time zone.
        :type zone: float
        :param day: The day that gets modified. This parameter is optional.
        :type day: float
        :param hms: If True the returned value is a tuple in the form of
                    (hh, mm, ss) indicating the length of the day else if
                    False one of the other two values are returned.
        :type hms: bool
        :param rtd: Round to closest day only when the day is used. Has no
                    effect if hms is True.
        :type rtd: bool
        :return: See the below note.
        :rtype: float | tuple

        .. note::

           1. There are three different outputs that can be returned.
              a. The adjusted day.
              b. The hour, minute, and seconds of the days offset either
                 less than or more than 24 hours.
        """
        if day is not None and hms:
            raise ValueError(
                "Cannot use the day and hms arguments at the same time.")

        if day is None and not hms:
            raise ValueError("Must provide a day or hms must be True.")

        jd0 = math.floor(jd)
        jd1 = jd0 + 1
        # The diff values converts my jd to the Meeus algorithm jd so
        # that the sunset can be determined properly. The fractional day
        # of either algorithm would be the same.
        diff1 = self._meeus_algorithm_jd_compensation(jd1)
        mjd1 = jd1 + diff1
        ss1 = self._sun_setting(mjd1, lat, lon, zone)

        if hms:
            diff0 = self._meeus_algorithm_jd_compensation(jd0)
            mjd0 = jd0 + diff0
            ss0 = self._sun_setting(mjd0, lat, lon, zone)
            value = self.hms_from_decimal_day(ss1 - ss0)
        else:
            fraction = round(jd % 1 - ss1 % 1, self.ROUNDING_PLACES)
            v = 1 if (day + fraction) < 1 else day + fraction
            value = round(v) if rtd else v

        return value
