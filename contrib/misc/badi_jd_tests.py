#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/misc/badi_jd_tests.py
#

import os
import sys
import math
import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar, datetime


class DateTests(BahaiCalendar):
    TRAN_COFF = 1843
    # Equinox and Solstices, Perihelion, and Aphelion
    # https://www.timeanddate.com/sun/@112931?month=3&year=1844
    # The site below is where I've gotten the Vernal Equinox data it uses
    # the 4, 100, and 400 algorithm, so we must also. The 4 and 128 algorithm
    # is more accurate, but I've not found Vernal Equinox data that uses it.
    # https://data.giss.nasa.gov/modelE/ar5plots/srvernal.html
    # https://aa.usno.navy.mil/data/Earth_Seasons
    # https://www.astropixels.com/ephemeris/soleq2001.html
    # https://stellafane.org/misc/equinox.html
    #------------------------------------------------------------------------
    # Julian Period day into:
    # https://aa.usno.navy.mil/data/JulianDate
    # -----------------------------------------------------------------------
    # Sun rise and set info:
    # https://gml.noaa.gov/grad/solcalc/
    # https://aa.usno.navy.mil/data/RS_OneYear
    # https://www.sunrisesunset.com/England/GreaterLondon/Greenwich.asp
    # Tehran: 35.682376, 51.285817 USED
    ########################################################
    # Nur Mazandaran Province, Iran (City Center) NOT USED #
    # Nur: 36.569336, 52.0050234 NOT USED                  #
    ########################################################
    # I use coordinates and the sunset in the city of Tehran to determine the
    # yearly Badi epochs. Below are the Gregorian dates of the Vernal Equinox.
    INJECT = (
        ((178, 0, 5), (2022, 3, 1, 17, 59)),
        ((178, 19, 1), (2022, 3, 2, 18)),
        ((178, 19, 2), (2022, 3, 3, 18, 1)),
        ((181, 1, 2), (2024, 3, 21, 18, 17)),
        ((181, 1, 5), (2024, 3, 24, 18, 20)),
        ((181, 2, 13), (2024, 4, 20, 18, 42)),
        # Sunset Tehran -> 15:02:
        ((181, 3, 18, 20), (2024, 5, 15, 15, 3, 36.0864)),
        ((181, 3, 19, 20), (2024, 5, 16, 15, 4, 23.4336)),
        ((181, 4, 1, 17), (2024, 5, 17, 12, 4, 2.6112)),
        ((181, 4, 1, 20), (2024, 5, 17, 15, 5, 10.3488)),
        ((181, 18, 1), (2025, 2, 6, 17, 31, 3.7056)),
        ((181, 18, 19), (2025, 2, 24, 17, 56, 46.7232)),
        ((181, 0, 1), (2025, 2, 25, 17, 57, 42.4512)),
        # (181, 0, 4) -> (2025, 3, 0.750321) is an error in
        # GregorianCalendar._check_valid_gregorian_month_day
        ((181, 0, 4), (2025, 2, 28, 17, 53, 36.0384)),
        ((181, 19, 1), (2025, 3, 1, 18, 1, 22.1664)),
        ((181, 19, 19), (2025, 3, 19, 18, 16, 7.7664)),
        ((182, 18, 19), (2026, 2, 24, 17, 56, 33.2448)),
        ((182, 0, 1), (2026, 2, 25, 17, 57, 29.0592)),
        ((182, 0, 5), (2026, 3, 1, 18, 1, 8.9472)),
        )
    MONTHS = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              12, 13, 14, 15, 16, 17, 18, 0, 19)

    def __init__(self):
        super().__init__()
        # https://qr.ae/psZONa
        # https://www.someweekendreading.blog/leap-year-revised/
        # 365 + 1/4 − 1/128 = 365.2421875 or 365 + 31/128
        # 365.2421897
        #self.MEAN_TROPICAL_YEAR = 365.2421897
        self._MONTHNAMES = {num: name for num, name in self.BADI_MONTH_NAMES}
        self.gc = GregorianCalendar()

    def analyze_date_error(self, options):
        """
        Finds the the Julian Period day (yearly epoch) that corresponds to
        the day of the Vernal Equinox.

        -a, optional -C, -G, and -X
        Also if -S and -E are used they must be used together.
        """
        return self._date_range(options)

    def check_long_date_from_short_date(self, data):
        """
        -c or --ck-dates
        """
        items = []

        for item in data:
            b_date, date = item
            bd = self.long_date_from_short_date(date)

        if bd != (b_date + (0, 0, 0)):
            items.append((item, bd))

        return items

    def find_leap_years(self, options):
        """
        -e and -S and -E

        Find all the leap years between -1842 and 1161.
        """
        data = []
        start = options.start
        end = options.end
        assert -1843 < start < 1162, (
            f"Start '{start}' must be from -1842 to 1161.")
        assert -1842 < end < 1163, (
            f"End '{end}' must be from -1841 to 1162")

        for year in range(start, end):
            is_leap = self._is_leap_year(year)
            days = 365 + is_leap
            total = 0

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + self._is_leap_year(year)

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    g_date = self.gregorian_date_from_badi_date(date)
                    # weekday, wd_name, m_name
                    weektuple = self._day_of_week(*date)
                    total += 1
                    data.append((date, g_date, *weektuple,
                                 is_leap, days, total))

        return data

    def find_gregorian_dates(self, options):
        """
        -g or --g-dates and -S and -E

        Converts Badi to Gregorian dates for the given range.
        """
        data = []

        for item in self._date_range(options):
            b_date, bjd, g_date, gjd, diff, offby = item
            g_date = self._gregorian_date_from_badi_date(
                b_date, options, *self.BAHAI_LOCATION[:3])
            gjd = self.gc.jd_from_gregorian_date(
                g_date, exact=options.exact, alt=options.alt_leap)
            diff = round(bjd - gjd, 6)
            offby = math.floor(bjd) - math.floor(gjd)
            data.append((b_date, bjd, g_date, gjd, diff, offby))

        return data

    def create_date_lists(self, options):
        """
        -l or --list
        Also -S and -E must both used together.
        """
        data = []

        for k in reversed(range(options.start, options.end)):
            for v in reversed(range(1, 20)):
                for y in reversed(range(1, 20)):
                    for m in reversed(range(0, 20)):
                        if m == 0:
                            self._calc_kvymd(5, k, v, y, m, data)
                        else:
                            self._calc_kvymd(20, k, v, y, m, data)

        return data

    def find_coefficents_precursor(self, options):
        """
        -p or --precursor

        This determines which coefficient group should be used for the
        years provided. The years provided are on the Badi Calendar - or +
        the epoch.

        Arguments to the process_segment() function.
        --------------------------------------------
        1. First run `badi_jd_tests.py -aGX > filename.txt`
           This file will be long so use `less filename.txt` to look at it.
           The last column will usually be -1.0, 0.0, or 1.0. The 0.0 values
           are already correct, the other two values means there is a
           difference in the between the Gregorian and Badi Julian Period
           days. These are the ones than need the coefficients which fixes them.
        2. The first argument is the current Badi year being processed
           subtracted from the end year argument.
        3. The second argument is the 1st coefficient corresponding to the
           (1, 34, 67, 100) numbers in the output from this method.
        4. The third argument is the 2nd coefficient which fixes the 1 values
           that were not included in the 1st coefficient and the 2 and 3 values.

        If an error JD falls on a 0 (zero) value then you need to change
        the start and end years so that no error JDs fall on a 0 value. The
        average number of years fixed in a group is 99, but this is not a
        hard and fast rule. Obvious break points are where a sequence
        changes. For example where there are two consecutive already good
        values where the values you need to fix had one.

        Note: Zero values never get processes.
        """
        data = []

        for y in range(options.start, options.end):
            year = options.end - y

            if year in (1, 34, 67, 100):
                a = year
            else:
                a = ''

            data.append((y + self.TRAN_COFF, y, year % 4, a))

        return data

    def find_coefficents(self, options):
        """
        -q or --coeff and -S and -E

        If -X is used the more exact mode is used. This should be the
        normal usage.
        """
        data = self._date_range(options)
        cp = {by: (n, a)
              for gy, by, n, a in self.find_coefficents_precursor(options)}
        items = []

        for item in data:
            b_year, month, day = item[0][:3]
            h, m, s = dt._get_hms(item[0], short_in=True)
            bjd = item[1]
            msg = (f"{b_year:> 5}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                   f"{s:<02} {bjd:<14} ")
            g_year, month, day = item[2][:3]
            h, m, s = dt._get_hms(item[2], short_in=True)
            gjd = item[3]
            msg += (f"{g_year:> 5}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                    f"{s:<02} {gjd:<9} ")
            diff = item[4]
            offby = item[5]
            msg += f"{diff:< 9} {offby:> 2} "
            j, k = cp.get(b_year)
            msg += f"{j} {k:<3}"
            items.append(msg)

        return items

    def get_range(self, end):
        """
        -r or --range
        """
        seq = {-159: -259, -64: -159, 35: -64, 134: 35, 233: 134, 332: 233,
               386: 332, 617: 517, 716: 617, 815: 716, 914: 815, 1013: 914,
               1112: 1013, 1211: 1112}
        valid_dates = list(seq.keys())
        start = seq.get(end)
        assert start is not None, (f"You must use valid dates, found {end}, "
                                   f"Valid dates are {valid_dates}.")
        data = []

        for y in range(start, end):
            yj = end - y
            jump = yj if yj in (1, 34, 67, 100) else 0 # jump values
            data.append((y, (end - y) % 4, jump))      # mod 4 values

        return data

    def twenty_four_hours(self, options):
        """
        Dump the hours, minutes, and seconds of the day.
        -t and -S and -E
        """
        data = []
        start = options.start
        end = options.end
        assert -1843 < start < 1162, (
            f"Start '{start}' must be from -1842 to 1161.")
        assert -1842 < end < 1163, (
            f"End '{end}' must be from -1841 to 1162")
        lat, lon, zone = self.BAHAI_LOCATION[:3]

        for year in range(start, end):
            is_leap = self._is_leap_year(year)
            days = 365 + is_leap
            total = 0

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + self._is_leap_year(year)

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    jd = self.jd_from_badi_date(date, lat, lon, zone)
                    g_date = self.gc.gregorian_date_from_jd(jd, exact=True)
                    jd0 = math.floor(jd)
                    jd1 = jd0 + 1
                    diff0 = self._meeus_algorithm_jd_compensation(jd0)
                    diff1 = self._meeus_algorithm_jd_compensation(jd1)
                    mjd0 = jd0 + diff0
                    mjd1 = jd1 + diff1
                    ss0 = self._sun_setting(mjd0, lat, lon, zone)
                    ss1 = self._sun_setting(mjd1, lat, lon, zone)
                    b_date = self.badi_date_from_jd(jd, short=True,
                                                    fraction=True)
                    ss_diff = ss1 - ss0
                    hms = self.hms_from_decimal_day(ss_diff)
                    data.append((b_date, g_date, round(ss0 % 1, 6),
                                 round(ss1 % 1, 6), round(ss_diff, 6), hms))

        return data

    def find_day_of_week(self, options):
        """
        Dump both the Badi and Gregorian weekdays.
        -w and -S and -E
        Optional -R to change the referance day from Jalál to whatever.
        """
        data = []
        _daynames = ['Jalál', 'Jamál', 'Kamál', 'Fiḍāl',
                     '`Idāl', 'Istijlāl', 'Istiqlāl']
        _g_daynames = ['Saturday', 'Sunday', 'Monday', 'Tuesday',
                       'Wednesday', 'Thursday', 'Friday']
        start = options.start
        end = options.end
        assert -1843 < start < 1162, (
            f"Start '{start}' must be from -1842 to 1161.")
        assert -1842 < end < 1163, (
            f"End '{end}' must be from -1841 to 1162")
        ref_day = options.ref_day

        if ref_day != 'Jalál':
            if ref_day in _daynames:
                r_idx = _daynames.index(ref_day)
                _daynames = _daynames[r_idx:] + _daynames[:r_idx]
                _g_daynames = _g_daynames[r_idx:] + _g_daynames[:r_idx]
            else:
                raise ValueError(f"Invalid reference_day {options.ref_day}. "
                                 "Must be a valid day of the week.")

        for year in range(start, end):
            is_leap = self._is_leap_year(year)
            days = 365 + is_leap
            total = 0

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + self._is_leap_year(year)

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    idx = (datetime._ymd2ord(
                        self, year, month, day) % 7 + 7) % 7
                    badi_weekday = _daynames[idx]
                    greg_weekday = _g_daynames[idx]
                    data.append((date, ref_day, idx,
                                 badi_weekday, greg_weekday))

        return data

    #
    # Supporting methods
    #
    def _jd_from_badi_date(self, b_date, lat=None, lon=None, zone=None, *,
                           options=None):
        year, month, day = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date), short=True)

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
        #print(f"{str(b_date):<15} {day:<9} {jd:<14} {ss_a:<20}")

        if options:
            coff = 0 if options.coff else self._get_coff(year)
        else:
            coff = self._get_coff(year)

        return round(jd + ss_a + coff, 6)

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
            coff = process_segment(-1821 - year, 1, 1, (1, 2))
        ## elif year < -1796: # -1819 to -1797 (range -S-1819 -E-1800)
        ##     coff = process_segment(-1800 - year, -1, -1, (1, 2, 3))
        ## elif year < -1747: # -1796 to -1748 (range -S-1792  -E-1782)
        ##     coff = process_segment(-1782 - year, 0, 1, (2,))
        ## elif year < -1715: # -1747 to -1716 (range -S-1747 -E-1717)
        ##     coff = process_segment(-1717 - year, 0, -1, (2,))
        ## elif year < -1697: # -1715 to -1698 (range -S-1715 -E-1701)
        ##     coff = process_segment(-1701 - year, -1, -1, (1, 2))
        ## elif year < -1684: # -1697 to -1685 (range -S-1697 -E-1687)
        ##     coff = process_segment(-1687 - year, 1, 1, (1, 2))
        ## elif year < -1615: # -1684 to -1616 (range -S-1684 -E-1655)
        ##     coff = process_segment(-1655 - year, 1, 1, (1,))
        ## elif year < -1583: # -1615 to -1584 (range -S-1615 -E-1586)
        ##     coff = process_segment(-1586 - year, -1, -1, (1,))
        ## elif year < -1551: # -1583 to -1552 (range -S-1585 -E-1553)
        ##     coff = process_segment(-1553 - year, -1, -1, (1, 2))
        ## elif year < -1519: # -1551 to -1520 (range -S-1551 -E-1520)
        ##     coff = process_segment(-1520 - year, -1, -1, (1, 2, 3))
        ## elif year < -1479: # -1519 to -1480 (range -S-1519 -E-1499)
        ##     coff = process_segment(-1499 - year, -1, -1, (0, 1, 2, 3))
        ## elif year < -1447: # -1779 to -1448 (range -S-1479 -E-1450)
        ##     coff = process_segment(-1450 - year, -1, -1, (1,))
        ## elif year < -1415: # -1447 to -1416 (range -S-1447 -E-1417)
        ##     coff = process_segment(-1417 - year, -1, -1, (1, 2))
        ## elif year < -1396: # -1415 to -1397 (range -S-1415 -E-1400)
        ##     coff = process_segment(-1400 - year, -1, -1, (1, 2, 3))
        ## elif year < -1347: # -1396 to -1348 (range -S-1396 -E-1387)
        ##     coff = process_segment(-1387 - year, 1, 1, (1,))
        ## elif year < -1315: # -1347 to -1316 (range -S-1347 -E-1318)
        ##     coff = process_segment(-1318 - year, -1, -1, (1,))
        ## elif year < -1297: # -1315 to -1298 (range -S-1315 -E-1301)
        ##     coff = process_segment(-1301 - year, -1, -1, (1, 2,))
        ## elif year < -1280: # -1296 to -1281 (range -S-1297 -E-1283)
        ##     coff = process_segment(-1283 - year, 1, 1, (1, 2))
        ## elif year < -1215: # -1280 to -1216 (range -S-1280 -E-1251)
        ##     coff = process_segment(-1251 - year, 1, 1, (1,))
        ## elif year < -1183: # -1215 to -1184 (range -S-1215 -E-1186)
        ##     coff = process_segment(-1186 - year, -1, -1, (1,))
        ## elif year < -1151: # -1183 to -1152 (range -S-1183 -E-1153)
        ##     coff = process_segment(-1153 - year, -1, -1, (1, 2))
        ## elif year < -1119: # -1151 to -1120 (range -S-1151 -E-1120)
        ##     coff = process_segment(-1120 - year, -1, -1, (1, 2, 3,))
        ## elif year < -1079: # -1119 to -1080 (range -S-1119 -E-1099)
        ##     coff = process_segment(-1099 - year, -1, -1, (0, 1, 2, 3))
        ## elif year < -1051: # -1079 to -1052 (range -S-1079 -E-1054)
        ##     coff = process_segment(-1054 - year, -1, -1, (1,))
        ## elif year < -1019: # -1051 to -1020 (range -S-1051 -E-1021)
        ##     coff = process_segment(-1021 - year, -1, -1, (1, 2))
        ## elif year < -996: # -1019 to -997 (range -S-1019 -E-1000)
        ##     coff = process_segment(-1000 - year, -1, -1, (1, 2, 3))
        ## elif year < -947: # -996 to -948 (range -S-996 -E-987)
        ##     coff = process_segment(-987 - year, 1, 1, (1,))
        ## elif year < -915: # -947 to -916 (range -S-947 -E-918)
        ##     coff = process_segment(-918 - year, -1, -1, (1,))
        ## elif year < -897: # -915 to -898 (range -S-915 -E-901
        ##     coff = process_segment(-901 - year, -1, -1, (1, 2))
        ## elif year < -880: # -897 to -881 (range -S-897 -E-883)
        ##     coff = process_segment(-883 - year, 1, 1, (1, 2))
        ## elif year < -815: # -880 to -816 (range -S-880 -E-851)
        ##     coff = process_segment(-851 - year, 1, 1, (1,))
        ## elif year < -783: # -815 to -784 (range -S-815 -E-786)
        ##     coff = process_segment(-786 - year, -1, -1, (1,))
        ## elif year < -751: # -783 to -752 (range -S-783 -E-753
        ##     coff = process_segment(-753 - year, -1, -1, (1, 2,))
        ## elif year < -719: # -751 to -720 (range -S-751 -E-720)
        ##     coff = process_segment(-720 - year, -1, -1, (1, 2, 3))
        ## elif year < -683: # -719 to -684 (range -S-719 -E-699)
        ##     coff = process_segment(-699 - year, -1, -1, (0, 1, 2, 3))
        ## elif year < -651: # -683 to -652 (range -S-683 -E-654)
        ##     coff = process_segment(-654 - year, -1, -1, (1,))
        ## elif year < -619: # -651 to -620 (range -S-651 -E-621)
        ##     coff = process_segment(-621 - year, -1, -1, (1, 2,))
        ## elif year < -596: # -619 to -597 (range -S-619 -E-600)
        ##     coff = process_segment(-600 - year, -1, -1, (1, 2, 3))
        ## elif year < -547: # -596 to -548 (range -S-596 -E-587)
        ##     coff = process_segment(-587 - year, 1, 1, (1,))
        ## elif year < -515: # -547 to -516 (range -S-547 -E-518)
        ##     coff = process_segment(-518 - year, -1, -1, (1,))
        ## elif year < -497: # -515 to -498 (range -S-515 -E-501)
        ##     coff = process_segment(-501 - year, -1, -1, (1, 2,))
        ## elif year < -480: # -497 to -481 (range -S-497 -E-483)
        ##     coff = process_segment(-483 - year, 1, 1, (1, 2))
        ## elif year < -415: # -480 to -416 (range -S-480 -E-451)
        ##     coff = process_segment(-451 - year, 1, 1, (1,))
        ## elif year < -383: # -415 to -384 (range -S-415 -E-386)
        ##     coff = process_segment(-386 - year, -1, -1, (1,))
        ## elif year < -351: # -383 to -352 (range -S-383 -E-353)
        ##     coff = process_segment(-353 - year, -1, -1, (1, 2))
        ## elif year < -319: # -351 to -320 (range -S-351 -E-320)
        ##     coff = process_segment(-320 - year, -1, -1, (1, 2, 3,))
        ## elif year < -279: # -319 to -280 (range -S-319 -E-299)
        ##     coff = process_segment(-299 - year, -1, -1, (0, 1, 2, 3))
        ## elif year < -247: # -279 to -248 (range -S-279 -E-250)
        ##     coff = process_segment(-250 - year, -1, -1, (1,))
        ## elif year < -215: # -247 to -216 (range -S-247 -E-217)
        ##     coff = process_segment(-217 - year, -1, -1, (1, 2))
        ## elif year < -196: # -215 to -197 (range -S-215 -E-200)
        ##     coff = process_segment(-200 - year, -1, -1, (1, 2, 3))
        ## elif year < -147: # -196 to -148 (range -S-196 -E-183)
        ##     coff = process_segment(-183 - year, 1, 1, (1,))
        ## elif year < -115: # -147 to -116 (range -S-147 -E-118)
        ##     coff = process_segment(-118 - year, -1, -1, (1,))
        ## elif year < -99: # -115 to -100 (range -S-115 -E-101)
        ##     coff = process_segment(-101 - year, -1, -1, (1, 2,))
        ## elif year < -80: # -99 to -81 (range -S-97 -E-83)
        ##     coff = process_segment(-83 - year, 1, 1, (1, 2))
        ## elif year < -15: # -80 to -16 (range -S-80 -E-51)
        ##     coff = process_segment(-51 - year, 1, 1, (1,))
        ## elif year < 17: # -15 to 16 (range -S-15 -E14)
        ##     coff = process_segment(14 - year, -1, -1, (1,))
        ## elif year < 49: # 17 to 48 (range -S17 -E47)
        ##     coff = process_segment(47 - year, -1, -1, (1, 2))
        ## elif year < 81: # 49 to 80 (range -S49 -E80)
        ##     coff = process_segment(80 - year, -1, -1, (1, 2, 3))
        ## elif year < 117: # 81 to 116 (range -S81 -E101)
        ##     coff = process_segment(101 - year, -1, -1, (0, 1, 2 , 3))
        ## elif year < 153: # 117 to 129 (range -S117 -E150)
        ##     coff = process_segment(150 - year, -1, -1, (1,))
        ## elif year < 185: # 153 to 184 (range -S153 -E183)
        ##     coff = process_segment(183 - year, -1, -1, (1, 2))
        ## elif year < 204: # 185 to 203 (range -S185 -E200)
        ##     coff = process_segment(200 - year, -1, -1, (1, 2, 3))
        ## elif year < 253: # 204 to 252 (range -S204 -E213)
        ##     coff = process_segment(213 - year, 1, 1, (1,))
        ## elif year < 285: # 253 to 284 (range -S253 -E282)
        ##     coff = process_segment(282 - year, -1, -1, (1,))
        ## elif year < 303: # 285 to 302 (range -S285 -E299)
        ##     coff = process_segment(299 - year, -1, -1, (1, 2))
        ## elif year < 320: # 303 to 319 (range -S303 -E317)
        ##     coff = process_segment(317 - year, 1, 1, (1, 2))
        ## elif year < 385: # 320 to 384 (range -S320 -E349)
        ##     coff = process_segment(349 - year, 1, 1, (1,))
        ## elif year < 417: # 385 to 416 (range -S385 -E414)
        ##     coff = process_segment(414 - year, -1, -1, (1,))
        ## elif year < 449: # 417 to 448 (range -S417 -E447)
        ##     coff = process_segment(447 - year, -1, -1, (1, 2))
        ## elif year < 481: # 449 to 480 (range -S449 -E480)
        ##     coff = process_segment(480 - year, -1, -1, (1, 2, 3))
        ## elif year < 521: # 481 to 520 (range -S481 -E501)
        ##     coff = process_segment(501 - year, -1, -1, (0, 1, 2, 3))
        ## elif year < 553: # 521 to 552 (range -S521 -E550)
        ##     coff = process_segment(550 - year, -1, -1, (1,))
        ## elif year < 585: # 553 to 584 (range -S553 -E583)
        ##     coff = process_segment(583 - year, -1, -1, (1, 2))
        ## elif year < 604: # 585 to 603 (range -S585 -E600)
        ##     coff = process_segment(600 - year, -1, -1, (1, 2 ,3))
        ## elif year < 653: # 604 to 652 (range -S604 -E613)
        ##     coff = process_segment(613 - year, 1, 1, (1,))
        ## elif year < 685: # 653 to 684 (range -S653 -E682)
        ##     coff = process_segment(682 - year, -1, -1, (1,))
        ## elif year < 703: # 685 to 702 (range -S685 -E699)
        ##     coff = process_segment(699 - year, -1, -1, (1, 2))
        ## elif year < 720: # 703 to 719 (range -S703 -E717)
        ##     coff = process_segment(717 - year, 1, 1, (1, 2))
        ## elif year < 785: # 720 to 784 (range -S720 -E749)
        ##     coff = process_segment(749 - year, 1, 1, (1,))
        ## elif year < 817: # 785 to 816 (range -S785 -E814)
        ##     coff = process_segment(814 - year, -1, -1, (1,))
        ## elif year < 849: # 817 to 848 (range -S817 -E847)
        ##     coff = process_segment(847 - year, -1, -1, (1, 2))
        ## elif year < 881: # 849 to 880 (range -S849 -E880)
        ##     coff = process_segment(880 - year, -1, -1, (1, 2, 3))
        ## elif year < 917: # 881 to 916 (range -S881 -E901)
        ##     coff = process_segment(901 - year, -1, -1, (0, 1, 2, 3))
        ## elif year < 949: # 917 to 948 (range -S917 -E946)
        ##     coff = process_segment(946 - year, -1, -1, (1,))
        ## elif year < 981: # 849 to 980 (range -S949 -E979)
        ##     coff = process_segment(979 - year, -1, -1, (1, 2))
        ## elif year < 1004: # 981 to 1003 (range -S981 -E1000)
        ##     coff = process_segment(1000 - year, -1, -1, (1, 2, 3))
        ## elif year < 1049: # 1004 to 1048 (range -S1004 -E1013)
        ##     coff = process_segment(1013 - year, 1, 1, (1,))
        ## elif year < 1081: # 1049 to 1080 (range -S1049 -E1078)
        ##     coff = process_segment(1078 - year, -1, -1, (1,))
        ## elif year < 1103: # 1081 to 1102 (range -S1081 -E1099)
        ##     coff = process_segment(1099 - year, -1, -1, (1, 2))
        ## elif year < 1116: # 1103 to 1115 (range -S1103 -E1113)
        ##     coff = process_segment(1113 - year, 1, 1, (1, 2))
        ## elif year < 1162: # 1116 to 1161 (range -S1116 -E1145)
        ##     coff = process_segment(1145 - year, 1, 1, (1,))
        else:
            coff = 0

        return coff

    def _badi_date_from_jd_alt(self, jd:float, lat:float=None, lon:float=None,
                               zone:float=None) -> tuple:
        """
        Convert a Julian period day to a Badi date.
        """
        def get_leap_year_info(y):
            leap = self._is_leap_year(year)
            yds = 366 if leap else 365
            ld = 4 + leap
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
            if days > ds:
                days -= ds
            else:
                break

        if any([True if l is None else False for l in (lat, lon, zone)]):
            lat, lon, zone = self.BAHAI_LOCATION[:3]

        #diff = jd % 1 - self._sun_setting(jd, lat, lon, zone) % 1
        #day += jd % 1 + 0.5

        ## print('jd:', jd, 'md:', md, #'td', td,
        ##       'days:', days,
        ##       'fjdy', fjdy,
        ##       #'d', d,
        ##       #'diff', diff,
        ##       'ld', ld, 'date:', (year, month, day),
        ##       file=sys.stderr)

        return year, month, day

    def _date_range(self, options):
        data = []
        last_year = 0
        inject = [(b_date[0], (b_date, g_date))
                  for b_date, g_date in self.INJECT]

        for g_year in range(options.start, options.end):
            g_date = (g_year, 3, 1)
            jd = self.gc.jd_from_gregorian_date(g_date) # Julian Period day
            ve_jd = self.find_moment_of_equinoxes_or_solstices(jd, zone=3.5)
            ss_jd = self._sun_setting(ve_jd, *self.BAHAI_LOCATION[:3])

            # It is allowed to have a Vernal Equinox to be up to one minute
            # before sunset and still use that sunset as the beginning of
            # the year. If a day == 1 then 1 minute is 0.0006944444444444444
            if ve_jd >= (ss_jd - 0.0006944444444444444):
                jd_boyear = ss_jd
            else:
                jd_boyear = self._sun_setting(ve_jd-1, *self.BAHAI_LOCATION[:3])

            g_ss = self.gc.ymdhms_from_date(self.gc.gregorian_date_from_jd(
                jd_boyear))
            # Get the Badi date for the beginning of the year.
            b_date = (g_year - self.TRAN_COFF, 1, 1)
            self._calculate_b_date(b_date, g_ss, data, options)

            #for dates in self._find_dates(b_date[0], inject):
            #    self._calculate_b_date(*dates, data, options)

        return data

    def _calculate_b_date(self, b_date, g_date, data, options):
        try:
            gjd = self.gc.jd_from_gregorian_date(
                g_date, exact=options.exact, alt=options.alt_leap)
            bjd = self._jd_from_badi_date(b_date, options=options)
            diff = round(bjd - gjd, 6)
            offby = math.floor(bjd) - math.floor(gjd)
            data.append((b_date, bjd, g_date, gjd, diff, offby))
        except Exception as e:
            msg = f"Badi date {b_date} and Gregorian date {g_date}, {e}"
            print(msg, file=sys.stderr)

    def _find_dates(self, year, inject):
        items = []

        for y, item in inject:
            if y == year:
                items.append(item)

        return items

    def _gregorian_date_from_badi_date(self, b_date:tuple, options, lat=0,
                                       lon=0, zone=0) -> tuple:
        """
        Get the Gregorian date from the Badi date.
        """
        jd = self._jd_from_badi_date(b_date, lat=lat, lon=lon, zone=zone,
                                     options=options)
        gd = self.gc.gregorian_date_from_jd(jd, exact=options.exact)
        g_date = self.gc.ymdhms_from_date(gd)
        return g_date

    def _calc_kvymd(self, days, k, v, y, m, data):
        year = (k - 1) * 361 + (v - 1) * 19 + y

        for d in reversed(range(1, days)):
            data.append(((k, v, y, m, d), (year, m, d)))

    def _day_of_week(self, year, month, day):
        weekday = (datetime._ymd2ord(self, year, month, day) % 7 + 7) % 7
        wd_name = datetime.DAYNAMES[weekday]
        m_name = self._MONTHNAMES[month]
        return weekday, wd_name, m_name


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test Badi date ranges."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze',
        help="Analyze Badi date errors when converting to jd.")
    parser.add_argument(
        '-c', '--ck-dates', action='store_true', default=False, dest='ck_dates',
        help="Check that long_date_from_short_date() works correctly.")
    parser.add_argument(
        '-e', '--leap-years', action='store_true', default=False,
        dest='leap_years', help="Convert Badi to Gregorian dates.")
    parser.add_argument(
        '-g', '--g-dates', action='store_true', default=False, dest='g_dates',
        help="Convert Badi to Gregorian dates.")
    parser.add_argument(
        '-l', '--list', action='store_true', default=False, dest='list',
        help="Generate a list of Badi dates both long and short versions.")
    parser.add_argument(
        '-p', '--precursor', action='store_true', default=False,
        dest='precursor',
        help="Dump data for determining the precursors to the coefficients.")
    parser.add_argument(
        '-q', '--coeff', action='store_true', default=False, dest='coeff',
        help="Dump data for determining coefficients.")
    parser.add_argument(
        '-r', '--range', type=int, default=0, dest='range',
        help="Dump an analysis of date ranges. Takes an integer value.")
    parser.add_argument(
        '-w', '--weekday', action='store_true', default=False,
        dest='weekday', help="Dump consecutive Badi and Gregorian weekdays.")
    parser.add_argument(
        '-t', '--twenty-four', action='store_true', default=False,
        dest='twenty_four', help="Find day length.")
    parser.add_argument(
        '-A', '--alt-leap', action='store_true', default=False,
        dest='alt_leap', help="Use alternative leap year method.")
    parser.add_argument(
        '-C', '--coff', action='store_true', default=False, dest='coff',
        help="Turn off all coefficients during an analysis.")
    parser.add_argument(
        '-D', '--dates', action='store_true', default=False, dest='dates',
        help="Test for the consecutive dates from JDs.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End Badi year of sequence.")
    parser.add_argument(
        '-G', '--graph', action='store_true', default=False, dest='graph',
        help=("Turn off all coefficients and dump output appropriate for "
              "graphing."))
    parser.add_argument(
        '-H', '--hours', action='store_true', default=False, dest='hours',
        help="Test for the consecutive hours.")
    parser.add_argument(
        '-J', '--jd', action='store_true', default=False, dest='jd',
        help=("Test for consecutive Julian Period days between start and "
              "end Badi years."))
    parser.add_argument(
        '-R', '--ref-day', type=str, default='Jalál', dest='ref_day',
        help="Change the referance day. Default is Jalál.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start Badi year of sequence.")
    parser.add_argument(
        '-X', '--exact', action='store_true', default=False, dest='exact',
        help=("Use the 4|100|400 or the 4|128 rules from Julian Calendar "
              "day one."))
    parser.add_argument(
        '-Y', '--year', action='store_true', default=False, dest='year',
        help="Test for the consecutive defined years 1 - 3004.")
    options = parser.parse_args()
    exclusive_error = (options.list, options.ck_dates, options.analyze,
                       options.consecutive, options.range != 0)
    dt = DateTests()
    ret = 0

    if options.analyze:
        if options.start is None or options.end is None:
            # Set default Gregorian years.
            options.start = 1  # Julian year 1
            options.end = 3005 # Gregorian year 3005

        if options.graph:
            options.coff = True
            data = dt.analyze_date_error(options)
            items = []

            for item in data:
                year, month, day = item[0][:3]
                h, m, s, ms = dt._get_hms(item[0], short_in=True)
                bjd = item[1]
                msg = (f"{year}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                       f"{s:>02} {bjd} ")
                year, month, day = item[2][:3]
                h, m, s, ms = dt._get_hms(item[2], short_in=True)
                gjd = item[3]
                msg += (f"{year}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                        f"{s:>02} {gjd} ")
                diff = item[4]
                offby = item[5]
                msg += f"{diff} {offby}"
                items.append(msg)

            [print(item) for item in items]
        else:
            data = dt.analyze_date_error(options)
            print("Badi Date     Badi JD        Gregorian Date                "
                  " Gregorian JD   Diff      Off By")

            for b_date, bjd, g_date, gjd, diff, offby in data:
                dn = '-' if diff < 0 else ' '
                on = '-' if offby < 0 else ' '
                print(f"{str(b_date):13} "
                      f"{bjd:<14} "
                      f"{str(g_date):30} "
                      f"{gjd:<14} "
                      f"{dn}{abs(diff):<8} "
                      f"{on}{abs(offby)}")

            diffs = []
            p = 0
            n = 0

            for item in data:
                if item[-1] != 0:
                    diffs.append(item[-1])

                if item[-1] > 0:
                    p += 1
                elif item[-1] < 0:
                    n += 1

            print(f"Total: {len(data)}\nPositive Errors: {p}\n"
                  f"Negative Errors: {n}\n   Total Errors: {len(diffs)}")

            if options.coff:
                coff = sum(diffs) / len(diffs)
                print(f"Average Coefficient: {coff}")
    elif options.ck_dates:
        if options.start is None or options.end is None:
            print("If option -c is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = dt.create_date_lists(options)
            bad_items = dt.check_long_date_from_short_date(data)
            bad_items = bad_items if bad_items else "All dates match."
            pprint.pprint(bad_items)
    elif options.list:
        if options.start is None or options.end is None:
            print("If option -l is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = dt.create_date_lists(options)
            pprint.pprint(data)
    elif options.range != 0:
        data = dt.get_range(options.range)
        [print(item) for item in data]
        print(f"Total years: {len(data)}")
    elif options.precursor:
        if options.start is None or options.end is None:
            print("If option -p is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = dt.find_coefficents_precursor(options)
            [print(f"{gy:> 5} {by:> 5}, {n:<1} {a:>2}")
             for gy, by, n, a in data]
            print(f"Total years: {len(data)}")
    elif options.coeff:
        if options.start is None or options.end is None:
            print("If option -q is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            [print(item) for item in dt.find_coefficents(options)]
    elif options.g_dates:
        if options.start is None or options.end is None:
            print("If option -g is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            print("b_date           "
                  "bjd            "
                  "g_date                         "
                  "gjd            "
                  "diff "
                  "offby")
            [print(f"{str(b_date):<16} "
                   f"{bjd:<14} "
                   f"{str(g_date):<30} "
                   f"{gjd:<14} "
                   f"{diff:<4} "
                   f"{offby}"
                   )
             for (b_date, bjd, g_date,
                  gjd, diff, offby) in dt.find_gregorian_dates(options)]
    elif options.leap_years:
        if options.start is None or options.end is None:
            print("If option -e is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            [print(f"{str(date):<14} "
                   f"{str(g_date):<21} "
                   f"{weekday} "
                   f"{wd_name:<8} "
                   f"{m_name:<10} "
                   f"{str(leap):5s} "
                   f"{days:<3} "
                   f"{total:>3}"
                   ) for (date, g_date, weekday, wd_name, m_name,
                          leap, days, total) in dt.find_leap_years(options)]
    elif options.weekday:
        if options.start is None or options.end is None:
            print("If option -w is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            [print(f"{str(date):15} "
                   f"{r_day:8} "
                   f"{idx} "
                   f"{bwd:8} "
                   f"{gwd:9}"
                   ) for (date, r_day, idx,
                       bwd, gwd) in dt.find_day_of_week(options)]
    elif options.twenty_four:
        if options.start is None or options.end is None:
            print("If option -t is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            print("Badi Date         Gregorian Date        "
                  "SS1 Frac SS2 Frac SS2-SS1  HMS Diff")
            [print(f"{str(b_date):<17} "
                   f"{str(g_date):<21} "
                   f"{fss0:<8} "
                   f"{fss1:<8} "
                   f"{ss_diff:<8} "
                   f"{str(hms):19}"
                   ) for (b_date, g_date, fss0, fss1,
                          ss_diff, hms) in dt.twenty_four_hours(options)]
    else:
        parser.print_help()

    sys.exit(ret)
