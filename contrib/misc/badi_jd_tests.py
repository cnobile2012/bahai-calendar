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
        -S and -E must be Gregorian dates

        This determines which coefficient group should be used for the
        years provided. The years provided are on the Badi Calendar - or +
        the epoch.

        Arguments to the process_segment() function.
        --------------------------------------------
        1. First run `badi_jd_tests.py -aX > filename.txt`
           This file will be long so use `less filename.txt` to look at it.
           The last column will usually be -1, 0, or 1. The 0 values are
           already correct, the other two values means there is a difference
           between the Gregorian and Badi Julian Period days. These are the
           ones than need the coefficients to fix them.
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
        start = options.start
        end = options.end
        options.start = options.start + self.TRAN_COFF
        options.end = options.end + self.TRAN_COFF
        data = self._date_range(options)
        options.start = start
        options.end = end
        cp = {by: (n, a)
              for gy, by, n, a in self.find_coefficents_precursor(options)}
        items = []

        for item in data:
            b_year, month, day = item[0][:3]
            h, m, s, ms = self._get_hms(item[0], short_in=True)
            bjd = item[1]
            msg = (f"{b_year:> 5}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                   f"{s:<02} {bjd:<14} ")
            g_year, month, day = item[2][:3]
            h, m, s, ms = self._get_hms(item[2], short_in=True)
            gjd = item[3]
            msg += (f"{g_year:> 5}-{month:>02}-{day:>02}T{h:>02}:{m:>02}:"
                    f"{s:<07} {gjd:<14} ")
            diff = item[4]
            offby = item[5]
            msg += f"{diff:< 9} {offby:< 1} "
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
        def process_segment(y, pc, c=0, onoff=(), *, c0=0, c1=0, c2=0, c3=0):
            func = lambda y, onoff: 1 < y < 100 and y % 4 in onoff
            coff = 0

            if pc and y in (1, 34, 67, 100):
                coff = pc
            elif c and func(y, onoff): # Whatever is passed in onoff.
                coff = c
            elif c0 and func(y, (0,)):
                coff = c0
            elif c1 and func(y, (1,)):
                coff = c1
            elif c2 and func(y, (2,)):
                coff = c2
            elif c3 and func(y, (3,)):
                coff = c3

            return coff

        def process_segments(year, pn, pc, c=0, onoff=(), *,
                             c0=0, c1=0, c2=0, c3=0):
            coff = 0

            for start, end in pn:
                if year in range(start, end):
                    # start to end (range -S start -E end)
                    coff = process_segment(end - year, pc, c=c, onoff=onoff,
                                           c0=c0, c1=c1, c2=c2, c3=c3)

            return coff

        z0 = ((-1727, -1699), )
        n1 = ((-1842, -1819), (-1699, -1691), )
        n0111 = ((-1819, -1799), (-1691, -1659), )
        n1222 = ((-1799, -1787), )
        n1122 = ((-1787, -1755), )
        n1112 = ((-1755, -1743),)
        n0011 = ((-1659, -1643), )
        n0001 = ((-1743, -1727), )
        n2 = ()
        p1 = ((-1599, -1566), (-1599, -1566), (-1546, -1543), )
        p1100 = ((-1643, -1627), )
        p1110 = ((-1627, -1599), )
        p1112 = ((-1566, -1546), )
        p3222 = ((-1543, -1531), )
        p3322 = ((-1531, -1501), )
        z0_flag = False
        coff = 0

        # General ranges are determined with:
        # ./contrib/misc/badi_jd_tests.py -p -S start_year -E end_year
        # Where -S is the 1st year and -E is the nth year + 1 that needs to
        # be process. Use the following command to test the results of each
        # segment. ./contrib/misc/badi_jd_tests.py -qXS start_year -E end_year
        # Full range is -1842 to 1161
        for start, end in z0:
            if year in range(start, end):
                z0_flag = True

        if not z0_flag:
            if not coff:
                coff = process_segments(year, n1, 1, 1, (0, 1, 2, 3))

            if not coff:
                coff = process_segments(year, n0111, 1, 1, (1, 2, 3))

            if not coff:
                coff = process_segments(year, n1222, 2, 2, (1, 2, 3), c0=1)

            if not coff:
                coff = process_segments(year, n1122, 2, 1, (0, 3,), c1=2, c2=2)

            if not coff:
                coff = process_segments(year, n1112, 2, 1, (0, 2, 3), c1=2)

            if not coff:
                coff = process_segments(year, n2, 2, 2, (0, 1, 2, 3))

            if not coff:
                coff = process_segments(year, n0011, 1, 1, (1, 2))

            if not coff:
                coff = process_segments(year, n0001, 1, c1=1)

            if not coff:
                coff = process_segments(year, p1, -1, -1, (0, 1, 2, 3))

            if not coff:
                coff = process_segments(year, p1100, 0, -1, (0, 3))

            if not coff:
                coff = process_segments(year, p1110, 0, -1, (0, 2, 3))

            if not coff:
                coff = process_segments(year, p1112, -2, -1, (0, 2, 3), c1=-2)

            if not coff:
                coff = process_segments(year, p3222, -2, -2, (1, 2, 3), c0=-3)

            if not coff:
                coff = process_segments(year, p3322, -3,
                                        c0=-2, c1=-3, c2=-3, c3=-2)

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
            # We must use the Meeus algorithm not mine when finding the
            # equinox and sunset. So don't use exact=options.exact here.
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
    dt = DateTests()
    ret = 0

    if options.analyze: # -a
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
    elif options.ck_dates: # -c
        if options.start is None or options.end is None:
            print("If option -c is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = dt.create_date_lists(options)
            bad_items = dt.check_long_date_from_short_date(data)
            bad_items = bad_items if bad_items else "All dates match."
            pprint.pprint(bad_items)
    elif options.leap_years: # -e
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
    elif options.list: # -l
        if options.start is None or options.end is None:
            print("If option -l is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = dt.create_date_lists(options)
            pprint.pprint(data)
    elif options.precursor: # -p
        if options.start is None or options.end is None:
            print("If option -p is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            data = dt.find_coefficents_precursor(options)
            [print(f"{gy:> 5} {by:> 5}, {n:<1} {a:>2}")
             for gy, by, n, a in data]
            print(f"Total years: {len(data)}")
    elif options.coeff: # -q
        if options.start is None or options.end is None:
            print("If option -q is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            [print(item) for item in dt.find_coefficents(options)]
    elif options.range != 0: # -r
        data = dt.get_range(options.range)
        [print(item) for item in data]
        print(f"Total years: {len(data)}")
    elif options.g_dates: # -g
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
    elif options.twenty_four: # -t
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
    elif options.weekday: # -w
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
    else:
        parser.print_help()

    sys.exit(ret)
