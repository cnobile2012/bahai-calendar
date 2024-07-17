# -*- coding: utf-8 -*-
#
# bahai_calendar/badi_calendar.py
#
__docformat__ = "restructuredtext en"

import math
import datetime

from bahai_calendar.base_calendar import BaseCalendar
from bahai_calendar.gregorian_calendar import GregorianCalendar


class BahaiCalendar(BaseCalendar):
    """
    Implementation of the Baha'i (Badi) Calendar.
    """
    # WGS84:          35.689252, 51.3896 1595m 3.5
    # WGS84--https://coordinates-converter.com/
    # https://whatismyelevation.com/location/35.63735,51.72569/Tehran--Iran-
# https://en-us.topographic-map.com/map-g9q1h/Tehran/?center=35.69244%2C51.19492
    #BAHAI_LOCATION = (35.696111, 51.423056, 3.5, 0)
    # Nur Mazandaran Province, Iran (City Center)
# https://www.google.com/maps/place/Nur,+Mazandaran+Province,+Iran/@36.569336,52.0050234,15z/data=!3m1!4b1!4m6!3m5!1s0x3f8efdf2a3fc7385:0x1f76f83486da57be!8m2!3d36.5763485!4d52.0133073!16zL20vMGJ6cjl6?entry=ttu
    BAHAI_LOCATION = (36.569336, 52.0050234, 3.5, 0)
    BADI_EPOCH = 2394644.258361 # 2394646.259722 using Meeus' algorinthm
    BADI_MONTH_NAMES = (
        (1, 'Bahá'), (2, 'Jalál'), (3, 'Jamál'), (4, "'Aẓamat"), (5, 'Núr'),
        (6, 'Raḥmat'), (7, 'Kalimát'), (8, 'Kamál'), (9, "Asmá'"),
        (10, "'Izzat"), (11, 'Mashíyyat'), (12, "'Ilm"), (13, 'Qudrat'),
        (14, 'Qawl'), (15, 'Masá’il'), (16, 'Sharaf'), (17, 'Sulṭán'),
        (18, 'Mulk'), (0, 'Ayyám-i-Há'), (19, "'Alá'")
        )

    def __init__(self):
        super().__init__()
        # kull_i_shay: 361-year (19^2) vahid (integer)
        # vahid: (integer) 19-year vahid
        # year: (integer) 1 - 19
        # month: (integer) 1 - 19 plus 0 for Ayyām-i-Hā
        # day: (integer) 1 - 19
        # Baha'i date: [kull_i_shay, vahid, year, month, day]
        self._bahai_date = None
        self._gc = GregorianCalendar()

    def parse_gregorian_datetime(self, dt:datetime.datetime) -> None:
        """
        Parse a Gregorian date to a long form Badi date.
        """
        self._gc.parse_datetime(dt)
        jd = self._gc.jd_from_gregorian_date(self._gc.date_representation)
        self.date_representation = self.badi_date_from_jd(jd)

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

    def sunset(self, date:tuple, lat:float=None, lon:float=None,
               zone:float=None, short:bool=False) -> float:
        """
        Return the sunset for the given Badi Day in either the long or
        short form date.

        :param date: Short form Badi date.
        :type date: tuple

        """
        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        jd = self.jd_from_badi_date(date, lat=lat, lon=lon, zone=zone)
        ss = self._sun_setting(jd, lat, lon, zone)
        #print(date, jd, ss)
        return self.badi_date_from_jd(ss, short=short)

    def naw_ruz(self, year:int, short:bool=False) -> tuple:
        """
        Return the Badi date for Naw-Ruz from the given Badi year.

        """
        jd = self.jd_from_badi_date((year, 1, 1))
        ve = self.find_moment_of_equinoxes_or_solstices(jd)
        ss = self._sun_setting(ve, *self.BAHAI_LOCATION[:3])
        return self.badi_date_from_jd(ss, short)

    def first_day_of_ridvan(self, year:int, lat:float=0, lon:float=0,
                            zone:float=0, short:bool=False) -> tuple:
        """
        Find the first day of Riḍván in either the long or short form date.
        If the latitude, longitude, and time zone are not given Riḍván time
        of day is determined for the city of Nur in Iran.
        """
        naw_ruz = self.naw_ruz(year, short=True)

        if lat == 0 and lon == 0 and zone == 0:
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        year, month, day = naw_ruz[:3]
        ss_date = self.sunset((year, 2, 13), lat, lon, zone)
        kull_i_shay, vahid, year, month, day = ss_date[:5]
        hour, minute, second = self._get_hms(ss_date)
        b_date = (kull_i_shay, vahid, year, month, day, hour, minute, second)
        #print(ss_date, b_date)
        return self.short_date_from_long_date(b_date) if short else b_date

    def jd_from_badi_date(self, b_date:tuple, lat:float=None, lon:float=None,
                          zone:float=None) -> float:
        """
        Convert a Badi short form date to Julian period day.

        :param b_date: A short form Badi date.
        :type b_date: tuple
        :return: The Julian Period day.
        :rtype: float
        """
        year, month, day = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date), short=True)

        if month == 0: # Ayyam-i-Ha
            m = 18 * 19
        elif month < 19:
            m = (month - 1) * 19
        else: # month == 19:
            m = 18 * 19 + (5 if self._is_leap_year(year) else 4)

        td = self._days_in_years(year-1)
        jd = td + (math.floor(self.BADI_EPOCH) - 1) + m + (day - 1) + day % 1

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        jds = math.floor(jd)
        ss_a = self._sun_setting(jds, lat, lon, zone)
        p = round(day % 1, self.ROUNDING_PLACES)
        diff = p + ss_a % 1 + 2
        return round(jd + diff + self._get_coff(year), self.ROUNDING_PLACES)

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
        # The if or elif statments may not have the same ranges as are
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
                          zone:float=None, *, short:bool=False) -> tuple:
        """
        Convert a Julian period day to a Badi date.
        """
        def get_leap_year_info(y):
            leap = self._is_leap_year(year)
            yds = 366 if leap else 365
            ld = 5 if leap else 4
            return leap, yds, ld

        def check_and_fix_day(cjd, y, lat=None, lon=None, zone=None):
            fjdy = self.jd_from_badi_date((y, 1, 1), lat, lon, zone)
            return y-1 if (fjdy - cjd) > 0 else y

        md = jd - (self.BADI_EPOCH - 1)
        year = math.floor(md / self.MEAN_TROPICAL_YEAR) + 1
        #year = math.floor(abs(md / self.MEAN_TROPICAL_YEAR))
        #year *= -1 if md < (self.BADI_EPOCH - 1) else 1

        leap, yds, ld = get_leap_year_info(year)

        if (y := check_and_fix_day(jd, year, lat, lon, zone)):
            year = y
            leap, yds, ld = get_leap_year_info(year)

        fjdy = self.jd_from_badi_date((year, 1, 1), lat, lon, zone)
        days = math.floor(jd) - math.floor(fjdy) + 1

        if days <= 342:
            m_days = days % 19
            day = 19 if m_days == 0 else m_days
        elif (342 + ld) < days <= yds:
            day = days - (342 + ld)
        else: # Ayyam-i-Ha
            day = days % 342

        month_days = [(n, 19) for n, v in self.BADI_MONTH_NAMES]
        month_days[18] = (0, ld)

        for month, ds in month_days:
            if days > ds:
                days -= ds
            else:
                break

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        #diff = jd % 1 - self._sun_setting(jd, lat, lon, zone) % 1
        #day += jd % 1 + 0.5

        #print('jd:', jd, 'md:', md, #'td:', td,
        #      'days:', days,
        #      'diff', diff,
        #      'leap', self._is_leap_year(year), 'date:', (year, month, day))

        date = self.long_date_from_short_date((year, month, day))
        return self.kvymdhms_from_b_date(date, short)

    def short_date_from_long_date(self, b_date:tuple) -> tuple:
        """
        Convert a long date (kvymdhms) to a short date (ymdhms)
        """
        self._check_valid_badi_month_day(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second = self._get_hms(b_date)
        y = (kull_i_shay - 1) * 361 + (vahid - 1) * 19 + year
        return (y, month, day) + self._trim_hms((hour, minute, second))

    def long_date_from_short_date(self, date:tuple) -> tuple:
        """
        Convert a date to a short date (ymdhms) to a long date (kvymdhms).
        """
        year, month, day = date[:3]
        hour, minute, second = self._get_hms(date, True)
        k = year / 361
        k0 = self._truncate_decimal(k % 1, 6)
        v = k0 / 19 * 361
        kull_i_shay = math.floor(k)

        if v == 0: # If there is no fraction in v
            vahid = 19
            y = 19
        else:
            kull_i_shay += 1
            vahid = math.ceil(v)
            y = math.ceil(self._truncate_decimal(v % 1, 6) * 19)

        hms = self._trim_hms((hour, minute, second))
        b_date = (kull_i_shay, vahid, y, month, day) + hms
        self._check_valid_badi_month_day(b_date)
        return b_date

    def date_from_kvymdhms(self, b_date:tuple, short:bool=False) -> tuple:
        """
        Convert (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second)
        into a (Kull-i-Shay, Váḥid, year, month, day.partial) date.
        """
        self._check_valid_badi_month_day(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second = self._get_hms(b_date)
        day += round(self.HR(hour) + self.MN(minute) + self.SEC(second),
                     self.ROUNDING_PLACES)
        date = (kull_i_shay, vahid, year, month, day)
        return self.short_date_from_long_date(date) if short else date

    def kvymdhms_from_b_date(self, b_date:tuple, short:bool=False) -> tuple:
        """
        Convert ((Kull-i-Shay, Váḥid, year, month, day.partial) into
        (Kull-i-Shay, Váḥid, year, month, day, hour, minute, second).
        """
        self._check_valid_badi_month_day(b_date)
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hd = self.PARTIAL_DAY_TO_HOURS(day)
        hour = math.floor(hd)
        md = self.PARTIAL_HOUR_TO_MINUTE(hd)
        minute = math.floor(md)
        second = round(self.PARTIAL_MINUTE_TO_SECOND(md), self.ROUNDING_PLACES)
        hms = self._trim_hms((hour, minute, second))
        date = (kull_i_shay, vahid, year, month, math.floor(day)) + hms
        return self.short_date_from_long_date(date) if short else date

    def _trim_hms(self, hms:tuple) -> tuple:
        """
        Trim the hours, minutes, or seconds off if zero unless a lower
        value was not zero.
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

    def _check_valid_badi_month_day(self, b_date:tuple) -> bool:
        """
        Check that the month and day values are valid.
        """
        cycle = 20
        kull_i_shay, vahid, year, month, day = b_date[:5]
        hour, minute, second = self._get_hms(b_date)
        assert 1 <= vahid < cycle, (
            f"The number of Váḥids in a Kull-i-Shay’ should be >= 1 or <= 19, "
            f"found {vahid}")
        assert 1 <= year < cycle, (
            f"The number of years in a Váḥid should be >= 1 or <= 19, "
            f"found {year}")
        assert 0 <= month < cycle, (
            f"Invalid month '{month}', should be 0 - 19.")

        # This is Ayyām-i-Hā and could be 4 or 5 days depending on leap year.
        if month == 0:
            cycle = 6 if self._is_leap_year(b_date) else 5

        assert 1 <= day < (cycle), (
            f"Invalid day '{day}' for month '{month}' and year '{year}' "
            f"should be from 1 to <= {cycle-1}.")
        assert 0 <= hour < 24, (f"Invalid hour '{hour}' it must be "
                                f"0 <= {hour} < 24")
        assert 0 <= minute < 60, (f"Invalid minute '{minute}' should be "
                                  f"0 <= {minute} < 60.")

        if any((hour, minute, second)):
            assert not day % 1, ("If there is a part day then there can be no "
                                 "hours, minutes, or seconds.")

        if any((minute, second)):
            assert not hour % 1, (
                "If there is a part hour then there can be no minutes or "
                "seconds.")

        if second:
            assert not minute % 1, (
                "If there is a part minute then there can be no seconds.")

    def _is_leap_year(self, date:tuple) -> bool:
        """
        Return a boolean True if a Badi leap year, False if not.

        :param date: This value can be either the Badi year or a long form
                     date.
        :type date: int or tuple
        :return:
        :rtype: bool
        """
        if isinstance(date, tuple):
            assert len(date) >= 3, ("If tuple it must be the Kull-i-Shay', "
                                    f"Váḥid, and year, found {date}")
            year = (date[0] - 1) * 361 + (date[1] - 1) * 19 + date[2]
        else:
            year = date

        return True if self._days_in_year(year) == 366 else False

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

    def _get_hms(self, date:tuple, short:bool=False) -> tuple:
        """
        Parse the hours, minutes, and seconds correctly for either the
        short or long form Badi date.

        :param date: A short or long form Badi date.
        :type date: tuple
        :param short: If True then parse for a short date else if False
                      parse for a long date.
        :type short: bool
        :return: The relevant hours, minutes, and seconds.
        :rtype: tuple
        """
        t_len = len(date)
        s = 3 if short else 5
        hour = date[s] if t_len > s and date[s] is not None else 0
        minute = date[s+1] if t_len > s+1 and date[s+1] is not None else 0
        second = date[s+2] if t_len > s+2 and date[s+2] is not None else 0
        return hour, minute, second

    def badi_date_from_gregorian_date(self, g_date:tuple,
                                      short:bool=False) -> tuple:
        """
        Get the Badi date from the Gregorian date.
        """
        jd = self._gc.jd_from_gregorian_date(g_date)
        return self.badi_date_from_jd(jd, short=short)

    def gregorian_date_from_badi_date(self, b_date:tuple, lat:float=0,
                                      lon:float=0, zone:float=0,) -> tuple:
        """
        Get the Gregorian date from the Badi date.
        """
        jd = self.jd_from_badi_date(b_date, lat=lat, lon=lon, zone=zone)
        return self._gc.gregorian_date_from_jd(jd, exact=True)
