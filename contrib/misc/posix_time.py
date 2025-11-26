#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# contrib/misc/posix_time.py
#

#import importlib
import os
import sys
import importlib

import datetime as dtime
from unittest.mock import patch
from contextlib import redirect_stdout

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import (BahaiCalendar, GregorianCalendar, datetime, timezone,
                          timedelta)
badidt = importlib.import_module('badidatetime.datetime')


class PosixTests(BahaiCalendar):
    """
    | POSIX converter: https://www.unixtimestamp.com/
    | Julian Period Converter: https://aa.usno.navy.mil/data/JulianDate
    | Sunset: https://gml.noaa.gov/grad/solcalc/

    The Python datetime package seems to always give local time from
    timestamps not UTC time. For Example:

    | In [18]: dtime.datetime.fromtimestamp(18000) This -5 hours from UTC time.
    | Out[19]: datetime.datetime(1970, 1, 1, 0, 0)
    """
    #BADI_COORD = (35.682376, 51.285817, 3.5)
    #FQ_COORD = (35.5894, -78.7792, -5.0)
    GMT_COORD = (51.477928, -0.001545, 0)
    #EPOCH = datetime(126, 16, 2, None, None, 7, 59, 31, 497600,
    #              tzinfo=timezone.utc)
    EPOCH = datetime(126, 16, 2, None, None, 7, 57, 27, 700000,
                  tzinfo=timezone.utc)
    #EPOCH = datetime(126, 16, 2, None, None, 7, 56, 30, 0,
    #              tzinfo=timezone.utc)

    def __init__(self):
        super().__init__()
        self.LOCAL_COORD = None
        self.gc = GregorianCalendar()

    #@patch.object(datetime, 'LOCAL_COORD', datetime.GMT_COORD)
    def mktime(self, options) -> list:
        """
        Test that the _mktime method produces the correct local timestamp.

        -m or --mktime with -S, and -E (Gregorian Dates) -A latitude
                            -O longitude -Z zone

        Total derived range -1842 through 1161 with Gregorian years 2 - 3006.
        """
        start = options.start
        end = options.end
        coords = (options.latitude, options.longitude, options.zone)
        tz = dtime.timezone(dtime.timedelta(hours=coords[-1]))
        data = []

        # The datetime.fromtimestamp(g_ts) method below internally needs the
        # local time, however, every run of this method could be with a
        # different set of coordinents, so we need to patch
        # badidatetime.datetime.LOCAL_COORD.
        with patch.object(badidt, 'LOCAL_COORD', coords):
            for year in range(start, end):
                g_date = (year, 1, 1)
                g_ts = dtime.datetime(*g_date, tzinfo=tz).timestamp()

                b_date = self.badi_date_from_gregorian_date(
                    g_date, *coords, short=True, trim=True)
                bd = datetime(*b_date[:3], None, None, *b_date[3:])
                # *** TODO *** Check for off-by-one day error in method below
                # bd = datetime.fromtimestamp(g_ts)
                # b_date = bd.b_date
                # hms = self._get_badi_hms(b_date, coords)[-1]
                # b_date += hms
                # bd = bd.replace(hour=hms[0], minute=hms[1], second=hms[2],
                #                 microsecond=hms[3])
                #print(hms, bd, file=sys.stderr)
                b_ts = self._mktime(bd, coords)
                g_leap = self.gc._GREGORIAN_LEAP_YEAR(year)
                b_leap = self._is_leap_year(b_date[0])
                diff = round(b_ts - g_ts)
                data.append((g_date, g_leap, g_ts, b_date, b_leap, b_ts, diff))

        return data

    def sunset(self, options) -> list:
        """
        Find the sunset for given years. This tests the self._get_badi_hms
        method below.

        -s or --sunset with -S and -E (Gregorian Dates) -A latitude
                            -O longitude -Z zone
        """
        start = options.start
        end = options.end
        coords = (options.latitude, options.longitude, options.zone)
        tz = dtime.timezone(dtime.timedelta(hours=coords[-1]))
        data = []

        with patch.object(badidt, 'LOCAL_COORD', coords):
            for year in range(start, end):
                g_date = (year, 1, 1)
                g_ts = dtime.datetime(*g_date, tzinfo=tz).timestamp()
                bd = datetime.fromtimestamp(g_ts)
                b_date = bd.b_date
                ss, utc_hms, badi_hms = self._get_badi_hms(b_date, coords)
                data.append((g_date, b_date, ss, utc_hms, badi_hms))

        return data

    def all_timezones(self, options):
        """
        Dump analysis files for all defined timezones.

        -t or --timezones with -S and -E (Gregorian Dates) -A latitude
                               -O longitude -Z zone
        """
        #start = options.start
        #end = options.end
        zones = _epoch_for_timezone()
        save_path = options.path

        if save_path.startswith(os.sep):
            path = save_path
        else:
            path = os.path.join(BASE_DIR, save_path)

        if not os.path.exists(path):
            os.mkdir(path)

        for name, (lat, lon, zone) in zones.items():
            start_time = time.time()
            #print(name, lat, lon, zone, file=sys.stderr)
            zone_txt = f"{zone}" if zone < 0 else f"+{zone}"
            filename = f"posix-TS{zone_txt}-{name}.txt"
            fullpath = os.path.join(path, filename)

            with open(fullpath, mode='w') as f:
                options.latitude = lat
                options.longitude = lon
                options.zone = zone

                with redirect_stdout(f):
                    _m_and_t_options(options, start_time, self.mktime(options))

    # Supporting methods

    def _mktime(self, dt: datetime, coords: tuple) -> float:
        """
        This is the inverse function of localtime(). Its argument is the
        struct_time or full 9-tuple (since the dst flag is needed; use -1 as
        the dst flag if it is unknown) which expresses the time in local time,
        not UTC. It returns a floating-point number, for compatibility with
        time(). If the input value cannot be represented as a valid time,
        either OverflowError or ValueError will be raised (which depends on
        whether the invalid value is caught by Python or the underlying C
        libraries).
        https://en.wikipedia.org/wiki/Time_zone

        Derived Badi time on the epoch 1970-01-01
        ------------------------------------------------
        GMT 0.0 time:     0126-16-02T07:58:31.4976+00:00
        EST -5.0 time:    0126-16-02T06:47:57.12-05:00
        Tehran 3.5 time:  0126-16-02T06:57:58.5504+03:30
        Beijing 8.0 time: 0126-16-01T07:00:59.1264+08:00
        """
        def sunset(date, coords):
            jd = self.jd_from_badi_date(date, *coords)
            mjd = self._meeus_from_exact(jd)
            ss = self._sun_setting(mjd-1, *coords)
            return ss % 1 * 86400  # fraction * seconds in a day

        # Find the floor of the timestamp of the naive epoch datetime
        # subtracted from the local datetime.
        tz = timezone(timedelta(hours=coords[-1]))
        dt = dt.replace(tzinfo=tz)
        t = (dt - self.EPOCH) // timedelta(0, 1)
        # Compensate for off-by-one days on the 1st day of the POSIX epoch
        # per year.
        greg_epoch = (1970, 1, 1)
        badi_epoch = self.badi_date_from_gregorian_date(greg_epoch, *coords,
                                                        short=True)
        if badi_epoch[2] != dt.day:
            dt = dt.replace(day=badi_epoch[2])

        # Compensate for different sunsets.
        # 1. Get date at GMT
        # 2. Get the number of seconds after noon for sunset for the date
        # 3. Subtract the GMT seconds from the local seconds
        # 4. Add the resultant value to t
        b_date = dt.b_date + dt.b_time
        loc_ss_diff = sunset(b_date, coords)
        gmt_ss_diff = sunset(b_date, self.GMT_COORD)
        t1 = t + loc_ss_diff - gmt_ss_diff
        #print(dt, loc_ss_diff, gmt_ss_diff, t, t1, file=sys.stderr)
        return t1

    def _get_badi_hms(self, b_date, coords: tuple) -> tuple:
        """
        Find the correct Badi hour, minute, and second after sunset equal
        to UTC midnight based on the coordinents.

        Correct Badi time on the epoch 1970-01-01   Sunset  Approx b_time
                                                    before
                                                    epoch
        ---------------------------------------------------------------------
        GMT 0.0 time:     0126-16-02T08:00:00+00:00 (16:00 hrs) 0.33333333
        EST -5.0 time:    0126-16-02T06:48:00-05:00 (17:11 hrs) 0.2833101875
        Tehran 3.5 time:  0126-16-02T06:59:00+03:30 (17:01 hrs) 0.29097220
        Beijing 8.0 time: 0126-16-02T07:01:00+08:00 (16:59 hrs) 0.29236110416

        Use the last value returned, the first two are only for testing.
        """
        jd = self.jd_from_badi_date(b_date, *coords)
        jd = self._meeus_from_exact(jd)
        ss = self._sun_setting(jd, *coords)
        partial_day = ss % 1
        utc_hms = self._hms_from_decimal_day(partial_day + 0.5, us=True)
        b_time = 0.5 - partial_day
        badi_hms = self._hms_from_decimal_day(b_time, us=True)
        # print("Badi date:", b_date, "UTC Midnight:", 0.5,
        #       "UTC sunset time:", partial_day,
        #       "Badi time at UTC midnight:", b_time, file=sys.stderr)
        return ss, utc_hms, badi_hms


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
    value = round(value, right)
    s = f"{value:.{right}f}"
    left_part, right_part = s.split(".")
    return f"{left_part.rjust(left)}.{right_part.ljust(right)}"


def _group_sequences(lst: list) -> list:
    """
    This function combines all years into a tuple. If there is a spread of
    years they the first and last year is placed in a tuple which is then
    placed in the outer tupple.

    Thanks to ChatGPT for deriving this algorithm.
    """
    grouped = []
    start = lst[0]  # Start of a potential sequence
    prev = lst[0]   # Previous number in sequence

    for i in range(1, len(lst)):
        # Check if the sequence continues
        if lst[i] == prev + 1:
            prev = lst[i]
        else:
            # If the sequence is 3 or more, add as a tuple (first, last)
            if prev - start >= 2:
                grouped.append((start, prev))
            else:
                # Otherwise, add numbers individually
                grouped.extend(range(start, prev + 1))

            # Reset start and prev for new sequence
            start = lst[i]
            prev = lst[i]

    # Handle the last sequence or number
    if prev - start >= 2:
        grouped.append((start, prev))
    else:
        grouped.extend(range(start, prev + 1))

    return grouped


def _epoch_for_timezone() -> tuple:
    """
    https://timezonedb.com/
    https://www.timeanddate.com/time/map/
    https://latitudelongitude.org/
    """
    return {
        'Kanton': (-2.81056, -171.67556, -12.0),         # 1 -43200
        'Nome': (64.501114, -165.406387, -11.0),         # 2 -39600
        'Honolulu': (21.306944, -157.858333, -10.0),     # 3 -36000
        'Anchorage': (61.21806, -149.90028, -9.0),       # 4 -32400
        'Boise': (43.618881, -116.215019, -8.0),         # 5 -28800
        'Bahia_Banderas': (20.80426, -105.30913, -7.0),  # 6 -25200
        'Belize': (17.189877, -88.49765, -6.0),          # 7 -21600
        'New_York': (40.71427, -74.00597, -5.0),         # 8 -18000
        'Aruba': (12.52111, -69.968338, -4.5),           # 9 -16200
        'Barbados': (13.193887, -59.543198, -4.0),       # 10 -14400
        'Goose_Bay': (53.3016826, -60.3260842, -3.5),    # 11 -12600
        'Montevideo': (-34.9033, -56.1882, -3.0),        # 12 -10800
        'Recife': (-8.047562, -34.876964, -2.0),         # 13 -7200
        'Azores': (37.741249, -25.675594, -1.0),         # 14 -3600
        'GMT': (51.477928, -0.001545, 0.0),              # 15 0
        'Paris': (48.856614, 2.3522219, 1.0),            # 16 3600
        'Latvia': (56.946, 24.10589, 2.0),               # 17 7200
        'Saratov': (51.54056, 46.00861, 3.0),            # 18 10800
        'Tehran': (35.682376, 51.285817, 3.5),           # 19 12600
        'Yerevan': (40.183333, 44.516667, 4.0),          # 20 14400
        'Balkh': (36.75635, 66.8972, 4.5),               # 21 16200
        'Yekaterinburg': (56.8519, 60.6122, 5.0),        # 22 18000
        'Colombo': (6.93194, 79.84778, 5.5),             # 23 19800
        'Kathmandu': (27.70169, 85.3206, 5.75),          # 24 20700
        'Omsk': (54.99244, 73.36859, 6.0),               # 25 21600
        'Yangon': (16.866069, 96.195132, 6.5),           # 26 23400
        'Christmas': (-10.5, 105.6667, 7.0),             # 27 25200
        'Perth': (-31.95224, 115.8614, 8.0),             # 28 28800
        'Eucla': (-31.6772316, 128.8897862, 8.75),       # 29 31500
        'Yokohama': (35.44778, 139.6425, 9.0),           # 30 32400
        'Adelaide': (-34.92866, 138.59863, 9.5),         # 31 34200
        'Brisbane': (-27.46794, 153.02809, 10.0),        # 32 36000
        'Lord_Howe': (-31.55, 159.08333, 10.5),          # 33 37800
        'Kosrae': (5.31086478, 162.9761931, 11.0),       # 34 39600
        'Kwajalein': (9.083333, 167.333333, 12.0),       # 35 43200
        'Chatham_Islands': (-44.00575230, -176.54006740, 12.75),  # 36 45900
        'Fakaofo': (-9.380255, -171.218836, 13.0),       # 37 46800
        'Kiritimati': (1.94, -157.475, 14.0),            # 38 50400
        }


def _m_and_t_options(options, start_time, data):
    k = '-K ' if options.kill_coeff else ''
    underline_length = 106
    print(f"./contrib/misc/{basename} -mA {options.latitude} "
          f"-O {options.longitude} -Z {options.zone} {k}"
          f"-S {options.start} -E {options.end}")
    print("Gregorian DT Leap  Greg Timestamp Badi Date Equal to UTC "
          "Midnight  Leap  Badi Timestamp      Diff")
    print('-' * underline_length)
    [print(f"{str(g_date):>12} "
           f"{str(g_leap):5} "
           f"{fmt_float(g_ts, 12, 1)} "
           f"{str(b_date):32} "
           f"{str(g_leap):5} "
           f"{fmt_float(b_ts, 12, 6)} "
           f"{fmt_float(diff, 6, 6)}"
           )
     for (g_date, g_leap, g_ts, b_date, b_leap, b_ts, diff) in data]
    print('-' * underline_length)
    diffs = []  # [(diff, year)...]
    dt = p = n = 0

    for item in data:
        diff = item[6]
        dt += diff

        if diff != 0:
            year = item[0][0]
            diffs.append((diff, year))

        if diff > 0:
            p += 1
        elif diff < 0:
            n += 1

    total_years = options.end - options.start
    print(f"Total Years Analyzed:       {total_years:>4}")
    print(f"Positive Errors:            {p:>4}")
    print(f"Negative Errors:            {n:>4}")
    print("-" * 32)
    print(f"Total Errors:               {len(diffs):>4}\n")
    set_items = set([diff for diff, year in diffs])
    items = []

    if set_items:
        min_max = set(set_items)
        print(f"Maximum deviation (seconds):  {fmt_float(max(min_max), 4, 1)}")
        print(f"Minimum deviation (seconds):  {fmt_float(min(min_max), 4, 1)}")
        print("Difference Average (seconds): "
              f"{fmt_float(dt/total_years, 4, 15)}")

    for diff in sorted(set_items):
        years = [y for d, y in diffs if d == diff]
        years = _group_sequences(years)
        items.append((diff, len(years), years))

    print(f"\nThere is/are {len(set_items)} sequence(s) in items "
          f"within the year {options.start} to {options.end-1} range:")
    pprint.pp(items, width=70, compact=True)
    end_time = time.time()
    days, hours, minutes, seconds = pt._dhms_from_seconds(
        end_time - start_time)
    print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
          f"{round(seconds, 6):02.6} seconds.")


if __name__ == "__main__":
    import time
    import argparse
    import pprint

    parser = argparse.ArgumentParser(
        description=("Test POSIX dates and times."))
    parser.add_argument(
        '-m', '--mktime', action='store_true', default=False, dest='mktime',
        help="Test the _mktime() method for accuracy.")
    parser.add_argument(
        '-s', '--sunset', action='store_true', default=False, dest='sunset',
        help="Find the sunset for given years.")
    parser.add_argument(
        '-t', '--timezones', action='store_true', default=False,
        dest='timezones',
        help="Dump analysis files for all defined timezones.")
    parser.add_argument(
        '-D', '--debug', action='store_true', default=False, dest='debug',
        help="Run in debug mode.")
    parser.add_argument(
        '-E', '--end', type=int, default=None, dest='end',
        help="End Badí' year of sequence.")
    parser.add_argument(
        '-K', '--kill-coeff', action='store_true', default=False,
        dest='kill_coeff',
        help="Turn off all coefficients during an analysis.")
    parser.add_argument(
        '-S', '--start', type=int, default=None, dest='start',
        help="Start Badí' year of sequence.")
    parser.add_argument(
        '-A', '--latitude', type=float, default=None, dest='latitude',
        help="Latitude")
    parser.add_argument(
        '-O', '--logitude', type=float, default=None, dest='longitude',
        help="Longitude")
    parser.add_argument(
        '-Z', '--zone', type=float, default=None, dest='zone',
        help="Time zone.")
    parser.add_argument(
        '-P', '--path', type=str, default='txt', dest='path',
        help=("Path to timezone files. If it starts with a / then the path "
              "is absolute."))
    options = parser.parse_args()
    pt = PosixTests()
    ret = 0
    basename = os.path.basename(__file__)

    if options.debug:
        sys.stderr.write("DEBUG--options: {}\n".format(options))

    if options.mktime:  # -m
        if options.start is None or options.end is None:
            print("If option -m is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            start_time = time.time()
            _m_and_t_options(options, start_time, pt.mktime(options))
    elif options.sunset:  # -s
        if options.start is None or options.end is None:
            print("If option -s is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            start_time = time.time()
            print(f"./contrib/misc/{basename} -sA {options.latitude} "
                  f"-O {options.longitude} -Z {options.zone} "
                  f"-S {options.start} -E {options.end}")
            print("Gregorian DT Badi Date      UTC Sunset JD  UTC Sunset HMS",
                  "      Badí' HMS equal")
            print(" " * 62, " to UTC midnight")
            underline_length = 83
            print('-' * underline_length)
            data = pt.sunset(options)
            [print(f"{str(g_date):>12} "
                   f"{str(b_date):>14} "
                   f"{ss:14.6f} "
                   f"{str(utc_hms):20} "
                   f"{str(badi_hms):20}"
                   ) for g_date, b_date, ss, utc_hms, badi_hms in data]
            print('-' * underline_length)
            end_time = time.time()
            days, hours, minutes, seconds = pt._dhms_from_seconds(
                end_time - start_time)
            print(f"\nElapsed time: {hours:02} hours, {minutes:02} minutes, "
                f"{round(seconds, 6):02.6} seconds.")
    elif options.timezones:  # -t
        if options.start is None or options.end is None:
            print("If option -t is used, -S and -E must also be used.",
                  file=sys.stderr)
            ret = 1
        else:
            pt.all_timezones(options)
    else:
        parser.print_help()

    sys.exit(ret)
