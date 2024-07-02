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

    BADI_EPOCH = 2394646.5 # 2394646.258361 # 2394646.5

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
        print(date, jd, ss)
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

    def jd_from_badi_date(self, b_date:tuple, lat:float=None, lon:float=None,
                          zone:float=None) -> float:
        """
        Convert a Badi short form date to Julian period day.

        :param b_date: A short form Badi date.
        :type b_date: tuple
        :return: The Julian Period day.
        :rtype: float
        """
        date = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date), short=True)
        year, month, day = date

        if month == 0: # Ayyam-i-Ha
            m = 18 * 19
        elif month < 19:
            m = (month - 1) * 19
        else: # month == 19:
            # Because we have to use 4 days without knowing if the leap year
            # is 4 or 5 days it's necessary to fix the day count by using
            # coefficients below.
            m = 18 * 19 + 4

        td = self._days_in_years(year-1)
        adj = 3 # Adjustment to Badi Epoch
        jd = td + (self.BADI_EPOCH - adj) + m + (day - 1) + day % 1
        diff = 0

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        jds = math.floor(jd)
        ss_a = self._sun_setting(jds, lat, lon, zone)
        p = round(day % 1, self.ROUNDING_PLACES)
        diff = p + ss_a % 1 + 0.5
        return round(jd + diff + self._get_coff(year), self.ROUNDING_PLACES)

    def _get_coff(self, year):
        def process_century(y, coff1, coff2, onoff):
            func = lambda y: 1 < y < 100 and y % 4 in onoff
            coff = 0

            if coff1 and y in (1, 34, 67, 100):
                coff = coff1
            elif coff2 and func(y):
                coff = coff2

            return coff

        # General ranges are determined with:
        # ./contrib/misc/full_partial.py -p -S start_year -E end_year
        # Where -S is the 1st year and -E is the nth year + 1 that needs to
        # be process. Use the following command to test the results.
        # ./contrib/misc/full_partial.py -qX -S start_year -E end_year
        # The if or elif statments may not have the same ranges as are
        # passed into the process_century method because we may need to skip
        # over already good results.
        if year < -1820: # -1920 to -1821 (range -S-1920 -E-1821)
            coff = process_century(-1821 - year, -1, -1, (1, 2))
        elif year < -1791: # -1821 to -1792 (range -S-1821 -E-1792)
            coff = process_century(-1792 - year, -1, -1, (1, 2, 3))
        elif year < -1747: # -1792 to -1748 (range -S-1792  -E-1782)
            coff = process_century(-1782 - year, 1, 1, (2,))
        elif year < -1715: # -1747 to -1716 (range -S-1747 -E-1717)
            coff = process_century(-1717 - year, 0, -1, (2,))
        elif year < -1687: # -1715 to -1687 (range -S-1715 -E-1689)
            coff = process_century(-1689 - year, -1, -1, (1, 2))
        elif year < -1660: # -1686 to -1661 (range -S-1687 -E-1664)
            coff = process_century(-1664 - year, -1, -1, (1, 2, 3))
        elif year < -1615: # -1660 to -1616 (range -S-1660 -E-1654)
            coff = process_century(-1654 - year, 0, 1, (2,))
        elif year < -1583: # -1615 to -1584 (range -S-1615 -E-1584)
            coff = process_century(-1584 - year, -1, -1, (3,))
        elif year < -1551: # -1583 to -1552 (range -S-1583 -E-1553)
            coff = process_century(-1553 - year, -1, -1, (1, 2))
        elif year < -1532: # -1551 to -1533 (range -S-1551 -E-1536)
            coff = process_century(-1536 - year, -1, -1, (1, 2, 3))
        elif year < -1479: # -1532 to -1480 (range -S-1532 -E-1518)
            coff = process_century(-1518 - year, 0, 1, (2,))
        elif year < -1447: # -1779 to -1448 (range -S-1479 -E-1450)
            coff = process_century(-1450 - year, -1, -1, (1,))
        elif year < -1415: # -1447 to -1416 (range -S-1447 -E-1417)
            coff = process_century(-1417 - year, -1, -1, (1, 2))
        elif year < -1404: # -1415 to -1405 (range -S-1415 -E-1408)
            coff = process_century(-1408 - year, -1, -1, (1, 2, 3))
        elif year < -1347: # -1404 to -1348 (range -S-1404 -E-1387)
            coff = process_century(-1387 - year, 1, 1, (1,))
        elif year < -1315: # -1347 to -1316 (range -S-1347 -E-1318)
            coff = process_century(-1318 - year, -1, -1, (1,))
        elif year < -1276: # -1315 to -1277 (range -S-1315 -E-1280)
            coff = process_century(-1280 - year, -1, -1, (2, 3))
        elif year < -1215: # -1276 to -1216 (range -S-1276 -E-1251)
            coff = process_century(-1251 - year, 1, 1, (1,))
        elif year < -1183: # -1215 to -1184 (range -S-1215 -E-1186)
            coff = process_century(-1186 - year, -1, -1, (1,))
        elif year < -1148: # -1183 to -1047 (range -S-1183 -E-1153)
            coff = process_century(-1153 - year, -1, -1, (1, 2))
        elif year < -1079: # -1148 to -1080 (range -S-1148 -E-1119)
            coff = process_century(-1119 - year, 1, 1, (1,))
        elif year < -1052: # -1079 to -1053 (range -S-1079 -E-1052)
            coff = process_century(-1052 - year, 0, -1, (3,))
        elif year < -1021: # -1052 to -1022 (range -S-1051 -E-1025)
            coff = process_century(-1025 - year, -1, -1, (1, 2))
        elif year < -947: # -1021 to -948 (range -S-1021 -E-987)
            coff = process_century(-987 - year, 1, 1, (1,))
        elif year < -915: # -947 to -916 (range -S-947 -E-918)
            coff = process_century(-918 - year, -1, -1, (1,))
        elif year < -893: # -915 to -894 (range -S-915 -E-897)
            coff = process_century(-897 - year, -1, -1, (1, 2))
        elif year < -880: # -893 to -881 (range -S-893 -E-883)
            coff = process_century(-883 - year, 1, 1, (1, 2))
        elif year < -815: # -880 to -816 (range -S-880 -E-851)
            coff = process_century(-851 - year, 1, 1, (1,))
        elif year < -783: # -815 to -784 (range -S-815 -E-786
            coff = process_century(-786 - year, -1, -1, (1,))
        elif year < -765: # -783 to -766 (range -S-783 -E-769)
            coff = process_century(-769 - year, -1, -1, (1, 2))
        elif year < -748: # -765 to -749 (range -S-765 -E-751)
            coff = process_century(-751 - year, 1, 1, (1, 2))
        elif year < -683: # -748 to -684 (range -S-748 -E-719)
            coff = process_century(-719 - year, 1, 1, (1,))
        elif year < -651: # -683 to -652 (range -S-683 -E-653
            coff = process_century(-653 - year, 0, -1, (2,))
        elif year < -637: # -651 to -638 (range -S-651 -E-641)
            coff = process_century(-641 - year, -1, -1, (1, 2))
        elif year < -616: # -637 to -617 (range -S-637 -E-619)
            coff = process_century(-619 - year, 1, 1, (1, 2))
        elif year < -547: # -616 to -548 (range -S-616 -E-587)
            coff = process_century(-587 - year, 1, 1, (1,))
        elif year < -509: # -547 to -510 (range -S-547 -E-513)
            coff = process_century(-513 - year, -1, -1, (2,))
        elif year < -480: # -509 to -481 (range -S-509 -E-483)
            coff = process_century(-483 - year, 1, 1, (1, 2))
        elif year < -415: # -480 to -416 (range -S-480 -E-451)
            coff = process_century(-451 - year, 1, 1, (1,))
        elif year < -381: # -415 to -382 (range -S-415 -E-386)
            coff = process_century(-386 - year, -1, -1, (1,))
        elif year < -348: # -381 to -349 (range -S-381 -E-351)
            coff = process_century(-351 - year, 1, 1, (1, 2,))
        elif year < -279: # -348 to -280 (range -S-348 -E-319)
            coff = process_century(-319 - year, 1, 1, (1,))
        elif year < -254: # -279 to -255 (range -S-279 -E-258)
            coff = process_century(-258 - year, -1, -1, (1,))
        elif year < -245: # -254 to -246 (range -S-254 -E-247)
            coff = process_century(-247 - year, 1, 1, (1, 2, 3))
        elif year < -212: # -245 to -213 (range -S-245 -E-215)
            coff = process_century(-215 - year, 1, 1, (1, 2))
        elif year < -147: # -212 to -148 (range -S-212 -E-181)
            coff = process_century(-181 - year, 0, 1, (3,))
        elif year < -126: # -147 to -127 (range -S-143 -E-126)
            coff = process_century(-130 - year, -1, -1, (1,))
        elif year < -113: # -126 to -114 (range -S-126 -E-115)
            coff = process_century(-115 - year, 1, 1, (1, 2, 3))
        elif year < -80: # -114 to -81 (range -S-114 -E-83)
            coff = process_century(-83 - year, 1, 1, (1, 2))
        elif year < -15: # -80 to -16 (range -S-80 -E-51)
            coff = process_century(-51 - year, 1, 1, (1,))
        elif year < 2: # -79 to 1 (range -S-15 -E-1)
            coff = process_century(-1 - year, 0, -1, (2,))
        elif year < 19: # 2 to 18 (range -S2 -E17)
            coff = process_century(17 - year, 1, 1, (1, 2, 3))
        elif year < 52: # 19 to 51 (range -S19 -E49)
            coff = process_century(49 - year, 1, 1, (1, 2,))
        elif year < 117: # 52 to 116 (range -S52 -E81)
            coff = process_century(81 - year, 1, 1, (1,))
        elif year < 130: # 117 to 129 (range -S117 -E126)
            coff = process_century(126 - year, -1, -1, (1,))
        elif year < 155: # 130 to 154 (range -S130 -E153)
            coff = process_century(153 - year, 1, 1, (1, 2, 3))
        elif year < 188: # 155 to 187 (range -S155 -E186)
            coff = process_century(186 - year, 0, 1, (2, 3))
        elif year < 253: # 188 to 252 (range -S188 -E213)
            coff = process_century(213 - year, 1, 1, (1,))
        elif year < 258: # 253 to 258 (range -S253 -E254)
            coff = process_century(254 - year, -1, 0, ())
        elif year < 287: # 258 to 286 (range -S258 -E285)
            coff = process_century(285 - year, 1, 1, (1, 2, 3))
        elif year < 320: # 287 to 319 (range -S287 -E318)
            coff = process_century(318 - year, 0, 1, (2, 3))
        elif year < 386: # 220 to 386 (range -S320 -E349)
            coff = process_century(349 - year, 1, 1, (1,))
        elif year < 419: # 387 to 418 (range -S386 -E417)
            coff = process_century(417 - year, 1, 1, (1, 2, 3))
        elif year < 452: # 419 to 451 (range -S419 -E449)
            coff = process_century(449 - year, 1, 1, (1, 2))
        elif year < 513: # 452 to 512 (range -S452 -E481)
            coff = process_century(481 - year, 1, 1, (1,))
        elif year < 518: # 513 to 517 (range -S513 -E518)
            coff = process_century(518 - year, 1, 1, (0, 1, 2, 3))
        elif year < 555: # 518 to 554 (range -S518 -E553)
            coff = process_century(553 - year, 1, 1, (1, 2, 3))
        elif year < 588: # 555 to 587 (range -S555 -E585)
            coff = process_century(585 - year, 1, 1, (1, 2))
        elif year < 641: # 588 to 640 (range -S588 -E613)
            coff = process_century(613 - year, 1, 1, (1,))
        elif year < 654: # 641 to 653 (range -S641 -E653)
            coff = process_century(653 - year, 1, 1, (0, 1, 2, 3))
        elif year < 687: # 654 to 686 (range -S654 -E685)
            coff = process_century(685 - year, 1, 1, (1, 2, 3))
        elif year < 720: # 687 to 719 (range -S687 -E717)
            coff = process_century(717 - year, 1, 1, (1, 2))
        elif year < 769: # 720 to 768 (range -S720 -E749)
            coff = process_century(749 - year, 1, 1, (1,))
        elif year < 786: # 769 to 785 (range -S769 -E785)
            coff = process_century(785 - year, 1, 1, (0, 1, 2, 3))
        elif year < 819: # 786 to 818 (range -S786 -E817)
            coff = process_century(817 - year, 1, 1, (1, 2, 3))
        elif year < 852: # 819 to 851 (range -S819 -E849)
            coff = process_century(849 - year, 1, 1, (1, 2))
        elif year < 897: # 852 to 896 (range -S819 -E881)
            coff = process_century(881 - year, 1, 1, (1,))
        elif year < 917: # 897 to 916 (range -S987 -E917)
            coff = process_century(917 - year, 1, 1, (0, 1, 2, 3))
        elif year < 949: # 917 to 850 (range -S918 -E949)
            coff = process_century(949 - year, 1, 1, (1, 2, 3))
        elif year < 984: # 851 to 983 (range -S951 -E981)
            coff = process_century(981 - year, 1, 1, (1, 2))
        elif year < 1025: # 984 to 1024 (range -S984 -E1013)
            coff = process_century(1013 - year, 1, 1, (1,))
        elif year < 1049: # 1025 to 1050 (range -S1025 -E1049)
            coff = process_century(1049 - year, 1, 1, (0, 1, 2, 3))
        elif year < 1083: # 1051 to 1082 (range -S1051 -E1081)
            coff = process_century(1081 - year, 1, 1, (1, 2, 3))
        elif year < 1116: # 1083 to 1115 (range -S1083 -E1113)
            coff = process_century(1113 - year, 1, 1, (1, 2))
        elif year < 1153: # 1116 to 1152 (range -S1116 -E1145)
            coff = process_century(1145 - year, 1, 1, (1,))
        elif year < 1162: # 1153 to 1161 (range -S1153 -E1162)
            coff = process_century(1162 - year, 1, 1, (0, 1, 2, 3))
        else:
            coff = 0

        return coff

    def badi_date_from_jd(self, jd:float, short:bool=False) -> tuple:
        """
        Convert a Julian period day to a Badi date.
        """
        a = jd - (self.BADI_EPOCH - 1)
        y = a / self.MEAN_TROPICAL_YEAR
        year = math.floor(y) + 1
        m = (y % 1) * self.MEAN_TROPICAL_YEAR
        m1 = math.floor(m / 19)
        month = m1 + 1

        if month > 18:
            ay_years = {365: 4, 366: 5}
            ytd = 18 * 19
            days_in_year = self._days_in_year(year)
            ay_days = ay_years.get(days_in_year)
            assert days_in_year in ay_years.keys(), (
                "Programming error, incorrect number of days in any "
                f"year, found {days}.")
            d = ytd + ay_days
            dsf = math.ceil(m)

            if ytd < dsf <= d:
                month = 0
                day = dsf - ytd
            else:
                month -= 1
                day = days_in_year - d
        else:
            day = math.floor(m) - m1 * 19 + 1

        day += round(a, self.ROUNDING_PLACES) % 1
        date = self.long_date_from_short_date((year, month, day))
        return self.kvymdhms_from_b_date(date, short)

    def _days_in_year(self, year:int) -> int:
        """
        Determine the number of days in the current year.
        """
        jd_n0 = self.jd_from_badi_date((year, 1, 1))
        jd_n1 = self.jd_from_badi_date((year + 1, 1, 1))
        return int(jd_n1 - jd_n0)

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
            f"should be 1 - < {cycle-1}.")
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
