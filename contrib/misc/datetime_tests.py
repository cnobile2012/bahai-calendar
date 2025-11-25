#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/misc/datetime_tests.py
#

import os
import sys
import datetime as dtime

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from _timestamp import TimestampUtils
from badidatetime import (BahaiCalendar, GregorianCalendar, datetime, timezone,
                          timedelta)


class DatetimeTests(BahaiCalendar, TimestampUtils):
    LOCAL_COORDS = (35.5894, -78.7792, -5.0)
    BADI_COORDS = BahaiCalendar._BAHAI_LOCATION[:3]
    MONTHS = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
              12, 13, 14, 15, 16, 17, 18, 0, 19)
    GREG_BADI_DATES = (
        ((1, 3, 19, 18, 14, 32, 553600), (-1842, 1, 1)),
        ((43, 3, 20, 18, 14, 26, 246400), (-1800, 1, 1)),
        ((143, 3, 20, 18, 15, 5, 558400), (-1700, 1, 1)),
        ((243, 3, 20, 18, 16, 31, 8000), (-1600, 1, 1)),
        ((343, 3, 20, 18, 17, 9, 715200), (-1500, 1, 1)),
        ((443, 3, 20, 18, 17, 3, 235200), (-1400, 1, 1)),
        ((543, 3, 20, 18, 17, 42, 115200), (-1300, 1, 1)),
        ((643, 3, 20, 18, 18, 20, 131200), (-1200, 1, 1)),
        ((743, 3, 20, 18, 19, 45, 667200), (-1100, 1, 1)),
        ((843, 3, 20, 18, 19, 37, 27200), (-1000, 1, 1)),
        ((943, 3, 20, 18, 20, 14, 870400), (-900, 1, 1)),
        ((1043, 3, 20, 18, 20, 53, 318400), (-800, 1, 1)),
        ((1143, 3, 20, 18, 22, 17, 817600), (-700, 1, 1)),
        ((1243, 3, 20, 18, 22, 8, 40000), (-600, 1, 1)),
        ((1343, 3, 20, 18, 22, 45, 984000), (-500, 1, 1)),
        ((1443, 3, 20, 18, 23, 22, 790400), (-400, 1, 1)),
        ((1543, 3, 20, 18, 24, 48, 844800), (-300, 1, 1)),
        ((1582, 3, 20, 18, 24, 27, 158400), (-261, 1, 1)),
        ((1582, 4, 7), (-261, 1, 19)),
        ((1582, 4, 26), (-261, 2, 19)),
        ((1582, 5, 15), (-261, 3, 19)),
        ((1582, 6, 3), (-261, 4, 19)),
        ((1582, 6, 22), (-261, 5, 19)),
        ((1582, 7, 11), (-261, 6, 19)),
        ((1582, 7, 30), (-261, 7, 19)),
        ((1582, 8, 18), (-261, 8, 19)),
        ((1582, 9, 6), (-261, 9, 19)),
        ((1582, 9, 25), (-261, 10, 19)),
        ((1582, 10, 4), (-261, 11, 9)),
        ((1582, 10, 4, 17, 41, 27, 686400), (-261, 11, 9)),
        ((1582, 10, 14), (-261, 11, 19)),
        ((1643, 3, 20, 18, 16, 28, 675200), (-200, 1, 1)),
        ((1743, 3, 20, 18, 16, 13, 641600), (-100, 1, 1)),
        ((1752, 9, 2), (-91, 9, 16)),
        ((1752, 9, 3), (-91, 9, 17)),
        ((1752, 9, 13), (-91, 10, 8)),
        ((1752, 9, 14), (-91, 10, 9)),
        ((1843, 3, 20, 18, 16, 48, 806400), (0, 1, 1)),
        ((1844, 3, 19, 18, 16, 36, 710400), (1, 1, 1)),
        ((1943, 3, 20, 18, 16, 32, 563200), (100, 1, 1)),
        ((2024, 3, 19, 18, 15, 57, 312000), (181, 1, 1)),
        ((2024, 4, 6), (181, 1, 19)),
        ((2024, 4, 25), (181, 2, 19)),
        ((2024, 5, 14), (181, 3, 19)),
        ((2024, 6, 2), (181, 4, 19)),
        ((2024, 6, 21), (181, 5, 19)),
        ((2043, 3, 20, 18, 16, 16, 406400), (200, 1, 1)),
        ((2143, 3, 20, 18, 15, 59, 731200), (300, 1, 1)),
        ((2243, 3, 20, 18, 15, 41, 587200), (400, 1, 1)),
        ((2343, 3, 20, 18, 16, 15, 283200), (500, 1, 1)),
        ((2443, 3, 20, 18, 15, 56, 707200), (600, 1, 1)),
        ((2543, 3, 20, 18, 15, 37, 353600), (700, 1, 1)),
        ((2643, 3, 20, 18, 15, 18, 345600), (800, 1, 1)),
        ((2743, 3, 20, 18, 15, 50, 54400), (900, 1, 1)),
        ((2843, 3, 20, 18, 15, 29, 664000), (1000, 1, 1)),
        ((2943, 3, 20, 18, 15, 9, 100800), (1100, 1, 1)),
        ((3004, 3, 20, 18, 15, 14, 630400), (1161, 1, 1)),
        )
    BADI_TIMESTAMP_DATES = (
        ((0, 16, 2, 7, 58, 59.9988), -3976214400),
        ((1, 16, 2, 7, 58, 59.9988), -3944592000),
        ((2, 16, 2, 7, 58, 0.0012), -3913056000),
        ((3, 16, 2, 7, 58, 0.0012), -3881520000),
        ((4, 16, 2, 7, 58, 59.9988), -3849984000),
        ((5, 16, 2, 7, 58, 59.9988), -3818361600),
        ((6, 16, 2, 7, 58, 0.0012), -3786825600),
        ((7, 16, 2, 7, 58, 0.0012), -3755289600),
        ((8, 16, 2, 7, 58, 59.9988), -3723753600),
        ((9, 16, 2, 7, 58, 59.9988), -3692131200),
        ((10, 16, 2, 7, 58, 0.0012), -3660595200),
        ((11, 16, 2, 7, 58, 0.0012), -3629059200),
        ((12, 16, 2, 7, 58, 59.9988), -3597523200),
        ((13, 16, 2, 7, 58, 59.9988), -3565900800),
        ((14, 16, 2, 7, 58, 0.0012), -3534364800),
        ((15, 16, 2, 7, 58, 0.0012), -3502828800),
        ((16, 16, 2, 7, 58, 59.9988), -3471292800),
        ((17, 16, 2, 7, 58, 59.9988), -3439670400),
        ((18, 16, 2, 7, 58, 0.0012), -3408134400),
        ((19, 16, 2, 7, 58, 0.0012), -3376598400),
        ((20, 16, 2, 7, 58, 59.9988), -3345062400),
        ((21, 16, 2, 7, 58, 59.9988), -3313440000),
        ((22, 16, 2, 7, 58, 59.9988), -3281904000),
        ((23, 16, 2, 7, 58, 0.0012), -3250368000),
        ((24, 16, 2, 7, 58, 59.9988), -3218832000),
        ((25, 16, 2, 7, 58, 59.9988), -3187209600),
        ((26, 16, 2, 7, 58, 59.9988), -3155673600),
        ((27, 16, 2, 7, 58, 0.0012), -3124137600),
        ((28, 16, 2, 7, 58, 59.9988), -3092601600),
        ((29, 16, 2, 7, 58, 59.9988), -3060979200),
        ((30, 16, 2, 7, 58, 59.9988), -3029443200),
        ((31, 16, 2, 7, 58, 0.0012), -2997907200),
        ((32, 16, 2, 7, 58, 59.9988), -2966371200),
        ((33, 16, 2, 7, 58, 59.9988), -2934748800),
        ((34, 16, 2, 7, 58, 59.9988), -2903212800),
        ((35, 16, 2, 7, 58, 0.0012), -2871676800),
        ((36, 16, 2, 7, 58, 59.9988), -2840140800),
        ((37, 16, 2, 7, 58, 59.9988), -2808518400),
        ((38, 16, 2, 7, 58, 59.9988), -2776982400),
        ((39, 16, 2, 7, 58, 0.0012), -2745446400),
        ((40, 16, 2, 7, 58, 0.0012), -2713910400),
        ((41, 16, 2, 7, 58, 59.9988), -2682288000),
        ((42, 16, 2, 7, 58, 59.9988), -2650752000),
        ((43, 16, 2, 7, 58, 0.0012), -2619216000),
        ((44, 16, 2, 7, 58, 0.0012), -2587680000),
        ((45, 16, 2, 7, 58, 59.9988), -2556057600),
        ((46, 16, 2, 7, 58, 59.9988), -2524521600),
        ((47, 16, 2, 7, 58, 0.0012), -2492985600),
        ((48, 16, 2, 7, 58, 0.0012), -2461449600),
        ((49, 16, 2, 7, 58, 59.9988), -2429827200),
        ((50, 16, 2, 7, 58, 59.9988), -2398291200),
        ((51, 16, 2, 7, 58, 0.0012), -2366755200),
        ((52, 16, 2, 7, 58, 0.0012), -2335219200),
        ((53, 16, 2, 7, 58, 59.9988), -2303596800),
        ((54, 16, 2, 7, 58, 59.9988), -2272060800),
        ((55, 16, 2, 7, 58, 59.9988), -2240524800),
        ((56, 16, 2, 7, 58, 0.0012), -2208988800),
        ((57, 16, 2, 7, 58, 59.9988), -2177452800),
        ((58, 16, 2, 7, 58, 59.9988), -2145916800),
        ((59, 16, 2, 7, 58, 59.9988), -2114380800),
        ((60, 16, 2, 7, 58, 0.0012), -2082844800),
        ((61, 16, 2, 7, 58, 59.9988), -2051222400),
        ((62, 16, 2, 7, 58, 59.9988), -2019686400),
        ((63, 16, 2, 7, 58, 59.9988), -1988150400),
        ((64, 16, 2, 7, 58, 0.0012), -1956614400),
        ((65, 16, 2, 7, 58, 59.9988), -1924992000),
        ((66, 16, 2, 7, 58, 59.9988), -1893456000),
        ((67, 16, 2, 7, 58, 59.9988), -1861920000),
        ((68, 16, 2, 7, 58, 0.0012), -1830384000),
        ((69, 16, 2, 7, 58, 59.9988), -1798761600),
        ((70, 16, 2, 7, 58, 59.9988), -1767225600),
        ((71, 16, 2, 7, 58, 59.9988), -1735689600),
        ((72, 16, 2, 7, 58, 0.0012), -1704153600),
        ((73, 16, 2, 7, 58, 59.9988), -1672531200),
        ((74, 16, 2, 7, 58, 59.9988), -1640995200),
        ((75, 16, 2, 7, 58, 59.9988), -1609459200),
        ((76, 16, 2, 7, 58, 0.0012), -1577923200),

        ((126, 16, 2, 7, 57, 27.7), 0),
        ((127, 16, 2, 7, 57, 43.2), 31536000),
        ((128, 16, 2, 7, 58, 59.9988), 63072000),
        ((129, 16, 2, 7, 58, 59.9988), 94694400),
        ((130, 16, 2, 7, 58, 0.0012), 126230400),
        ((131, 16, 2, 7, 58, 59.9988), 157766400),
        ((132, 16, 2, 7, 58, 59.9988), 189302400),
        ((133, 16, 2, 7, 58, 59.9988), 220924800),
        ((134, 16, 2, 7, 58, 0.0012), 252460800),
        ((135, 16, 2, 7, 58, 59.9988), 283996800),
        ((136, 16, 2, 7, 58, 59.9988), 315532800),
        ((137, 16, 2, 7, 58, 59.9988), 347155200),
        ((138, 16, 2, 7, 58, 0.0012), 378691200),
        ((139, 16, 2, 7, 58, 59.9988), 410227200),
        ((140, 16, 2, 7, 58, 59.9988), 441763200),
        ((141, 16, 2, 7, 58, 59.9988), 473385600),
        ((142, 16, 2, 7, 58, 0.0012), 504921600),
        ((143, 16, 2, 7, 58, 59.9988), 536457600),
        ((144, 16, 2, 7, 58, 59.9988), 567993600),
        ((145, 16, 2, 7, 58, 59.9988), 599616000),
        ((146, 16, 2, 7, 58, 0.0012), 631152000),
        ((147, 16, 2, 7, 58, 59.9988), 662688000),
        ((148, 16, 2, 7, 58, 59.9988), 694224000),
        ((149, 16, 2, 7, 58, 59.9988), 725846400),
        ((150, 16, 2, 7, 58, 59.9988), 757382400),
        ((151, 16, 2, 7, 58, 59.9988), 788918400),
        ((152, 16, 2, 7, 58, 59.9988), 820454400),
        ((153, 16, 2, 7, 58, 59.9988), 852076800),
        ((154, 16, 2, 7, 58, 59.9988), 883612800),
        ((155, 16, 2, 7, 58, 59.9988), 915148800),
        ((156, 16, 2, 7, 58, 59.9988), 946684800),
        ((157, 16, 2, 7, 58, 59.9988), 978307200),
        ((158, 16, 2, 7, 58, 59.9988), 1009843200),
        ((159, 16, 2, 7, 58, 59.9988), 1041379200),
        ((160, 16, 2, 7, 58, 59.9988), 1072915200),
        ((161, 16, 2, 7, 58, 59.9988), 1104537600),
        ((162, 16, 2, 7, 58, 59.9988), 1136073600),
        ((163, 16, 2, 7, 58, 59.9988), 1167609600),
        ((164, 16, 2, 7, 58, 59.9988), 1199145600),
        ((165, 16, 2, 7, 58, 59.9988), 1230768000),
        ((166, 16, 2, 7, 58, 59.9988), 1262304000),
        ((167, 16, 2, 7, 58, 59.9988), 1293840000),
        ((168, 16, 2, 7, 58, 59.9988), 1325376000),
        ((169, 16, 2, 7, 58, 59.9988), 1356998400),
        ((170, 16, 2, 7, 58, 59.9988), 1388534400),
        ((171, 16, 2, 7, 58, 0.0012), 1420070400),
        ((172, 16, 2, 7, 58, 59.9988), 1451606400),
        ((173, 16, 2, 7, 58, 59.9988), 1483228800),
        ((174, 16, 2, 7, 58, 59.9988), 1514764800),
        ((175, 16, 2, 7, 58, 0.0012), 1546300800),
        ((176, 16, 2, 7, 58, 59.9988), 1577836800),
        ((177, 16, 2, 7, 58, 59.9988), 1609459200),
        ((178, 16, 2, 7, 58, 59.9988), 1640995200),
        ((179, 16, 2, 7, 58, 0.0012), 1672531200),
        ((180, 16, 2, 7, 58, 59.9988), 1704067200),
        ((181, 16, 2, 7, 58, 59.9988), 1735689600),
        ((182, 16, 2, 7, 58, 59.9988), 1767225600),
        ((183, 16, 2, 7, 58, 0.0012), 1798761600),
        ((184, 16, 2, 7, 58, 59.9988), 1830297600),
        ((185, 16, 2, 7, 58, 59.9988), 1861920000),
        ((186, 16, 2, 7, 58, 59.9988), 1893456000),
        ((187, 16, 2, 7, 58, 59.9988), 1924992000),
        ((188, 16, 2, 7, 58, 59.9988), 1956528000),
        ((189, 16, 2, 7, 58, 59.9988), 1988150400),
        ((190, 16, 2, 7, 58, 59.9988), 2019686400),
        ((191, 16, 2, 7, 58, 59.9988), 2051222400),
        ((192, 16, 2, 7, 58, 59.9988), 2082758400),
        ((193, 16, 2, 7, 58, 59.9988), 2114380800),
        ((194, 16, 2, 7, 58, 59.9988), 2145916800),
        ((195, 16, 2, 7, 58, 59.9988), 2177452800),
        ((196, 16, 2, 7, 58, 59.9988), 2208988800),
        ((197, 16, 2, 7, 58, 59.9988), 2240611200),
        ((198, 16, 2, 7, 58, 59.9988), 2272147200),
        ((199, 16, 2, 7, 58, 59.9988), 2303683200),
        ((200, 16, 2, 7, 58, 59.9988), 2335219200),
        ((201, 16, 2, 7, 58, 59.9988), 2366841600),
        ((202, 16, 2, 7, 58, 59.9988), 2398377600),
        ((203, 16, 2, 7, 58, 59.9988), 2429913600),
        ((204, 16, 2, 7, 58, 59.9988), 2461449600),
        ((205, 16, 2, 7, 58, 59.9988), 2493072000),
        ((206, 16, 2, 7, 58, 59.9988), 2524608000),
        ((207, 16, 2, 7, 58, 59.9988), 2556144000),
        ((208, 16, 2, 7, 58, 0.0012), 2587680000),
        ((209, 16, 2, 7, 58, 59.9988), 2619302400),
        ((210, 16, 2, 7, 58, 59.9988), 2650838400),
        ((211, 16, 2, 7, 58, 59.9988), 2682374400),
        ((212, 16, 2, 7, 58, 0.0012), 2713910400),
        ((213, 16, 2, 7, 58, 59.9988), 2745532800),
        ((214, 16, 2, 7, 58, 59.9988), 2777068800),
        ((215, 16, 2, 7, 58, 59.9988), 2808604800),
        ((216, 16, 2, 8, 0, 0.0), 2840140800),
        ((217, 16, 2, 7, 58, 59.9988), 2871763200),
        ((218, 16, 2, 7, 58, 59.9988), 2903299200),
        ((219, 16, 2, 7, 58, 59.9988), 2934835200),
        ((220, 16, 2, 7, 58, 59.9988), 2966371200),
        ((221, 16, 2, 7, 58, 59.9988), 2997993600),
        )

    def __init__(self):
        super().__init__()
        self.gc = GregorianCalendar()

    def analyze_ordinal_error_list(self, options):
        """
        Find the errors between the Badi datetime ordinals and the Python
        date time ordinals.

        -a
        """
        data = []

        for g_date, b_date in self.GREG_BADI_DATES:
            g_dt = dtime.datetime(*g_date)
            g_ord = g_dt.toordinal()
            b_dt = datetime(*b_date)
            b_ord = b_dt.toordinal()
            same = g_ord == b_ord
            diff = g_ord - b_ord
            g_jd = self.gc.jd_from_gregorian_date(g_date, exact=True)
            b_jd = self.jd_from_badi_date(b_date, *self.LOCAL_COORDS)
            jd_diff = round(g_jd - b_jd, 6)
            date = self.badi_date_from_jd(b_jd, *self.LOCAL_COORDS,
                                          short=True, trim=True, rtd=True)
            d_diff = self._subtract_tuples(b_date, date)
            leap = self._is_leap_year(b_date[0])
            data.append((g_date, g_ord, b_date, b_ord, same, diff,
                         g_jd, jd_diff, date, d_diff, leap))

        return data

    def analyze_ordinal_error_create(self, options):
        """
        Find the errors between conversion between badi dates to JD and
        vice versa. This tests the BahaiCalendar._adjust_date method.

        -b
        Also if -S and -E are used they must be used together and refer
        to Badi years.
        """
        data = []
        start = options.start
        end = options.end

        for year in range(start, end):
            is_leap = self._is_leap_year(year)

            for month in self.MONTHS:
                dm = 19 if month != 0 else 4 + is_leap

                for day in range(1, dm + 1):
                    date = (year, month, day)
                    #print(date, file=sys.stderr)
                    # We default to the Epoch Coordinates.
                    jd = self.jd_from_badi_date(date)
                    # Get the Badi date from the Julian Period day.
                    b_date = self.badi_date_from_jd(
                        jd, short=True, trim=True, rtd=True)
                    # Difference of the date converted to a JD then back to a
                    # date again then subtract the converted date from the
                    # original date. They should be the same.
                    diff0 = self._subtract_tuples(b_date, date)
                    # The ordinal date for visual comparison.
                    ordinal = self._ordinal_from_jd(jd)
                    o = datetime.fromordinal(ordinal, short=True)
                    o_date = (o.year, o.month, o.day)
                    # Difference of the Badi datetime derived from an ordinal
                    # then subtract the derived date original date. They
                    # should be the same.
                    diff1 = self._subtract_tuples(o_date, date)
                    # Get the Gregorian date.
                    g_date = self.gc.gregorian_date_from_jd(jd, exact=True)
                    data.append((g_date, jd, date, b_date, o_date,
                                 diff0, diff1))

        return data

    def analyze_timestamp_errors(self, options):
        """
        Find the errors in timestamp conversions to Badi dates. This test
        trys to ensures that the datetime.date.today method changes date on
        sunset based on local time.
        https://www.unixtimestamp.com

        -c
        """
        data = []
        lat = options.latitude
        lon = options.longitude
        zone = options.zone
        tz = dtime.timezone(dtime.timedelta(hours=zone))

        for b_date, ts in self.BADI_TIMESTAMP_DATES:
            # Get the Gregorian timestamp for reference.
            g_date = self.gregorian_date_from_badi_date(b_date, lat, lon, zone)
            g_ts_ss = dtime.datetime(*g_date, tzinfo=tz).timestamp()
            #g_ts_ss = self.timestamp_at_sunset(g_date, lat, lon)
            # Timestamp only methods, 1st get UTC timestamp for sunset.
            ts_b_date = self.posix_timestamp(g_ts_ss, lat, lon, zone,
                                             short=True, trim=True, rtd=False)
            # Julian Period day methods.
            year, month, day, hour, minute, second = b_date
            jd_ts_ss = datetime(year, month, day, hour=hour, minute=minute,
                                second=second).timestamp()
            jd_b_date = self.posix_timestamp(jd_ts_ss, lat, lon, zone,
                                             short=True, trim=True, rtd=False,
                                             _chk_on=False)
            ts_diff = g_ts_ss - ts
            jd_diff = jd_ts_ss - ts
            data.append((b_date, ts, g_date, g_ts_ss, ts_diff, ts_b_date,
                         jd_ts_ss, jd_diff, jd_b_date))

        return data

    def analyze_timestamp_for_today(self, options):
        """
        See if we get close to the correct time after sunset so we can prove
        that sunset can be found with a timestamp.

        -t -S and -E in Gregorian years
        """
        data = []
        start = options.start
        end = options.end
        #lat = options.latitude
        lon = options.longitude

        if lon:
            zone = lon / 15
        else:
            zone = options.zone

        tz0 = dtime.timezone(dtime.timedelta(hours=zone))
        tz1 = timezone(timedelta(hours=zone))

        for year in range(start, end):
            g_date = (year, 1, 1)
            g_ts = dtime.datetime(*g_date, tzinfo=tz0).timestamp()
            bd = datetime.fromtimestamp(g_ts, tz1)
            b_date = bd.b_date
            b_ts = bd.timestamp()
            diff = b_ts - g_ts
            data.append((g_date, g_ts, b_date, b_ts, diff))

        return data

    def _subtract_tuples(self, t0, t1):
        return t0[0] - t1[0], t0[1] - t1[1], t0[2] - t1[2]


def fmt_float(value, left=4, right=4):
    """
    Format one float so that it is visually centered on the decimal point.

    Parameters
    ----------
    value : float | int | str
        The number to format.
    left : int
        Width to reserve on the left of the decimal (including any minus sign).
    right : int
        Number of digits to show after the decimal.
    """
    s = f"{value:.{right}f}"
    left_part, right_part = s.split(".")
    return f"{left_part.rjust(left)}.{right_part.ljust(right)}"


if __name__ == "__main__":
    import time
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test datetime date ranges."))
    parser.add_argument(
        '-a', '--analyze', action='store_true', default=False, dest='analyze',
        help="Analyze ordinal dates from list.")
    parser.add_argument(
        '-b', '--analyze1', action='store_true', default=False,
        dest='analyze1', help="Analyze Badi and Gregorian dates.")
    parser.add_argument(
        '-c', '--analyze2', action='store_true', default=False,
        dest='analyze2', help="Analyze timestamps relative to sunset.")
    parser.add_argument(
        '-t', '--analyze3', action='store_true', default=False,
        dest='analyze3', help="Analyze timestamps relative to sunset.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End of year of sequence.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start of year of sequence.")
    parser.add_argument(
        '-T', '--timezone', type=float, default=0.0, dest='timezone',
        help="Timezone offset floating point value.")
    parser.add_argument(
        '-A', '--latitude', type=float, default=None, dest='latitude',
        help="Latitude")
    parser.add_argument(
        '-O', '--logitude', type=float, default=None, dest='longitude',
        help="Longitude")
    parser.add_argument(
        '-Z', '--zone', type=float, default=None, dest='zone',
        help="Time zone.")

    options = parser.parse_args()
    dt = DatetimeTests()
    ret = 0
    basename = os.path.basename(__file__)

    if options.analyze:  # -a
        print(f"./contrib/misc/{basename} -a")
        print("Gregorian Date                   Greg ord Badi Date     "
              "Badi ord Same  Df | JD             JD Diff    Badi Date     "
              "Date Diff Leap")
        print('-'*131)
        [print(f"{str(g_date):33} "
               f"{g_ord:>7} "
               f"{str(b_date):14} "
               f"{b_ord:>7} "
               f"{str(same):<5} "
               f"{diff:02} | "
               f"{g_jd:<14} "
               f"{jd_diff:+09} "
               f"{str(date):14} "
               f"{str(d_diff)} "
               f"{str(leap)}"
               ) for (g_date, g_ord, b_date, b_ord, same, diff,
                      g_jd, jd_diff, date, d_diff, leap)
         in dt.analyze_ordinal_error_list(options)]
    elif options.analyze1:  # -b
        if options.start is None or options.end is None:
            # Set default Gregorian years.
            options.start = -1842  # Julian year 1
            options.end = 1162     # Gregorian year 3005

        start_time = time.time()
        print(f"./contrib/misc/{basename} -bS {options.start} "
              f"-E {options.end}")
        print(" "*78, "Orig - Badi   Orig - Ord")
        print("Greg Date             JD             Orig Date     Badi Date"
              "     Ordinal Date  B Date Diff   O Date Diff   HMS from JD")
        print('-'*123)
        data = dt.analyze_ordinal_error_create(options)
        total_diff0 = total_diff1 = 0
        items = []
        #print(data, file=sys.stderr)

        for g_date, jd, date, b_date, o_date, diff0, diff1 in data:
            if diff0 != (0, 0, 0):
                total_diff0 += 1

            if diff1 != (0, 0, 0):
                total_diff1 += 1

        [print(f"{str(g_date):21} "
               f"{jd:<14} "
               f"{str(date):13} "
               f"{str(b_date):13} "
               f"{str(o_date):13} "
               f"{str(diff0):13} "
               f"{str(diff1):13} "
               f"{dt._hms_from_decimal_day(jd)}"
               )
         for g_date, jd, b_date, o_date, date, diff0, diff1 in data]
        total_errors = total_diff0 + total_diff1
        print(f"Analyzing year {options.start} to year {options.end-1}.")
        print(f"Ordinal Errors: {total_diff1}")
        print(f"   Badi Errors: {total_diff0}")
        print(f"  Total Errors: {total_errors}")
        end_time = time.time()
        days, hours, minutes, seconds = dt._dhms_from_seconds(
            end_time - start_time)
        print(f"  Elapsed time: {hours:02} hours, {minutes:02} minutes, "
              f"{round(seconds, 6):02.6} seconds.")
    elif options.analyze2:  # -c
        start_time = time.time()
        underline_length = 212
        print(f"./contrib/misc/{basename} -cA {options.latitude} "
              f"-O {options.longitude} -Z {options.zone}")
        print("Criteria Badí' Date    and    timestamp     Gregorian Date",
              " " * 19, "Gregorian TS       Greg TS Diff    "
              "Badí' Date (TS)", " " * 14, "Sunset TS (JD)   Badi TS Diff "
              "Badí' Date (JD)", " " * 13, "TSD JDD")
        print('-' * underline_length)
        data = dt.analyze_timestamp_errors(options)
        items = []

        # Adjust for Badi calendar
        for (b_date, ts, g_date, g_ts_ss, ts_diff,
             ts_b_date, jd_ts_ss, jd_diff, jd_b_date) in data:
            ts_badi_err = b_date[2] - ts_b_date[2]
            jd_badi_err = b_date[2] - jd_b_date[2]
            items.append((b_date, fmt_float(ts, 11, 1), g_date,
                          fmt_float(g_ts_ss, 11, 6), fmt_float(ts_diff, 7, 6),
                          ts_b_date, fmt_float(jd_ts_ss, 11, 4),
                          fmt_float(jd_diff, 5, 6), jd_b_date,
                          ts_badi_err, jd_badi_err))

        [print(f"{str(b_date):29} "
               f"{ts:<10} "
               f"{str(g_date):34} "
               f"{g_ts_ss:<18} "
               f"{ts_diff:<13}  "
               f"{str(ts_b_date):30} "
               f"{jd_ts_ss:<15} "
               f"{jd_diff:<12} "
               f"{str(jd_b_date):30} "
               f"{ts_badi_err:>2}  "
               f"{jd_badi_err:>2}"
               )
         for (b_date, ts, g_date, g_ts_ss, ts_diff, ts_b_date, jd_ts_ss,
              jd_diff, jd_b_date, ts_badi_err, jd_badi_err) in items]
        print('-' * underline_length)
        ss_ts_diff_ts = ss_ts_diff_jd = 0

        for item in items:
            #print(item[-2], item[-1], file=sys.stderr)
            if item[-2] != 0:
                ss_ts_diff_ts += 1

            if item[-1] != 0:
                ss_ts_diff_jd += 1

        print("Analyzing year")
        print(f"                   Total days: {len(items):>4}")
        print(f"Sunset derived from TS errors: {ss_ts_diff_ts:>4}")
        print(f"Sunset derived from JD errors: {ss_ts_diff_jd:>4}")
        print("=" * 35)
        print("                 Total errors: "
              f"{ss_ts_diff_ts + ss_ts_diff_jd:>4}")
        end_time = time.time()
        days, hours, minutes, seconds = dt._dhms_from_seconds(
            end_time - start_time)
        print(f"  Elapsed time: {hours:02} hours, {minutes:02} minutes, "
              f"{round(seconds, 6):02.6} seconds.")
    elif options.analyze3:  # -t
        if options.start is None or options.end is None:
            # Set default Gregorian years.
            options.start = 2  # Year 1 would be before that oldest Badi date.
            options.end = 3005

        start_time = time.time()
        underline_length = 76
        lon = options.longitude
        zone = lon / 14 if options.zone is None else options.zone
        lon_text = "" if lon is None else f" -O {lon}"
        print(f"./contrib/misc/{basename} -tS {options.start} "
              f"-E {options.end}{lon_text} -Z {zone}")
        print("Gregorian DT Greg TS        Badi Date      Badi Timestamp      "
              "Diff")
        print('-' * underline_length)
        data = dt.analyze_timestamp_for_today(options)
        [print(f"{str(g_date):12} "
               f"{fmt_float(g_ts, 12, 1)} "
               f"{str(b_date):14} "
               f"{fmt_float(b_ts, 12, 6)} "
               f"{fmt_float(diff, 6, 6)}"
               )
         for g_date, g_ts, b_date, b_ts, diff in data]
        print('-' * underline_length)
        deviation = {'n': 0, 'p': 0, 'max_n': 0, 'max_p': 0, 'total': 0}

        for items in data:
            diff = items[-1]

            if diff < 0:
                deviation['n'] += 1
            elif diff > 0:
                deviation['p'] += 1

            if diff < deviation['max_n']:
                deviation['max_n'] = diff
            elif diff > deviation['max_p']:
                deviation['max_p'] = diff

            deviation['total'] += diff

        total_years = options.end - options.start
        mean_deviation = deviation['total'] / total_years
        print(f"Analyzing year {options.start} to year {options.end} "
              "Gregorian.")
        print(f"Total years:                          {total_years}")
        print(f"Total Timestamp negative deviations:  {deviation['n']}")
        print(f"Maximum negative deviation (seconds): {deviation['max_n']}")
        print(f"Total Timestamp positive deviations:  {deviation['p']}")
        print(f"Maximum positive deviation (seconds): {deviation['max_p']}")
        print(f"Mean deviation (seconds):             {mean_deviation}")
        end_time = time.time()
        days, hours, minutes, seconds = dt._dhms_from_seconds(
            end_time - start_time)
        print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
              f"{round(seconds, 6):02.6} seconds.")
    else:
        parser.print_help()

    sys.exit(ret)
