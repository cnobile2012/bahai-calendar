#!/usr/bin/env python

import os
import sys
import math
import pprint
import argparse

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from badidatetime import BahaiCalendar, GregorianCalendar


class FindLatLon(BahaiCalendar):
    WC_DATA = { # WC Naw-Ruz
        172: ((2015, 3, 21), False),
        173: ((2016, 3, 20), False),
        174: ((2017, 3, 20), True),
        175: ((2018, 3, 21), False),
        176: ((2019, 3, 21), False),
        177: ((2020, 3, 20), False),
        178: ((2021, 3, 20), True),
        179: ((2022, 3, 21), False),
        180: ((2023, 3, 21), False),
        181: ((2024, 3, 20), False),
        182: ((2025, 3, 20), True),
        183: ((2026, 3, 21), False),
        184: ((2027, 3, 21), False),
        185: ((2028, 3, 20), False),
        186: ((2029, 3, 20), False),
        187: ((2030, 3, 20), True),
        188: ((2031, 3, 21), False),
        189: ((2032, 3, 20), False),
        190: ((2033, 3, 20), False),
        191: ((2034, 3, 20), True),
        192: ((2035, 3, 21), False),
        193: ((2036, 3, 20), False),
        194: ((2037, 3, 20), False),
        195: ((2038, 3, 20), True),
        196: ((2039, 3, 21), False),
        197: ((2040, 3, 20), False),
        198: ((2041, 3, 20), False),
        199: ((2042, 3, 20), True),
        200: ((2043, 3, 21), False),
        201: ((2044, 3, 20), False),
        202: ((2045, 3, 20), False),
        203: ((2046, 3, 20), True),
        204: ((2047, 3, 21), False),
        205: ((2048, 3, 20), False),
        206: ((2049, 3, 20), False),
        207: ((2050, 3, 20), True),
        208: ((2051, 3, 21), False),
        209: ((2052, 3, 20), False),
        210: ((2053, 3, 20), False),
        211: ((2054, 3, 20), True),
        212: ((2055, 3, 21), False),
        213: ((2056, 3, 20), False),
        214: ((2057, 3, 20), False),
        215: ((2058, 3, 20), False),
        216: ((2059, 3, 20), True),
        217: ((2060, 3, 20), False),
        218: ((2061, 3, 20), False),
        219: ((2062, 3, 20), False),
        220: ((2063, 3, 20), True),
        221: ((2064, 3, 20), False),
        }

    def __init__(self):
        self.gc = GregorianCalendar()

    def start(self):
        start_and_stop = self.find_first_and_last_day()
        data = []
        lats_lons = self.create_lat_lon_ranges()

        for lat, lon in lats_lons:
            # Modify the lat and lon in the BahaiCalendar class.
            location = list(self.BAHAI_LOCATION)
            location[0] = lat
            location[1] = lon
            self.BAHAI_LOCATION = location
            item = self.find_sunset_for_wc_dates()[::2]

            if len(data) < 1:
                data[:] = item
            else:
                items = [data, item]
                correct = [0, 0]

                for x, item in enumerate(items):
                    for y, (year, ve_day, ss_day, b) in enumerate(item):

                        # A count of 1 (one) is a fill day or 24 hours.
                        if ve_day < ss_day and (ss_day - ve_day) < 1:
                            correct[x] += 1

                    item.append((None, lat, lon, correct[0]))

                if correct[0] > correct[1]:
                    data[:] = items[0]
                elif correct[1] >= correct[0]:
                    data[:] = items[1]

        return data

    def find_first_and_last_day(self):
        data = []
        years = list(self.WC_DATA)
        years.sort()
        prev_bool = False

        for year in years:
            date = self.WC_DATA[year][0]
            boolean = self.WC_DATA[year][1]
            moment = self.gc.fixed_from_gregorian(date)

            if year != 172:
                data.append((year-1, moment-1, prev_bool))

            data.append((year, moment, boolean))
            prev_bool = boolean

        return data

    def create_lat_lon_ranges(self):
        data = []
        lat0, lat1 = (36, 37)
        lon0, lon1 = (51, 53)
        precision = 10000
        lat0 *= precision
        lat1 *= precision
        lon0 *= precision
        lon1 *= precision

        #for lat in range(lat0, lat1 + 1):
        #    lat_ = lat / precision
        lat_ = 36.176768

        for lon in range(lon0, lon1 + 1):
            lon_ = lon / precision
            data.append((lat_, lon_))

        return data

    def find_sunset_for_wc_dates(self):
        data = []

        for year, fixed, b in self.find_first_and_last_day():
            ss_day = self.sunset(fixed)
            ve_day = self.find_moment_of_equinoxes_or_solstices(fixed)
            data.append((year, ve_day, ss_day, b))

        return data


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=("Find latitude and longitude"))
    parser.add_argument(
        '-D', '--debug', action='store_true', dest='debug',
        help="Turn on DEBUG logging mode, can be very verbose.")
    parser.add_argument(
        '-l', '--lat-lon', action='store_true', default=False, dest='lat_lon',
        help="Find the latitude and longitude.")
    parser.add_argument(
        '-d', '--days', action='store_true', default=False, dest='days',
        help="Dump the first and last days of the World Center data.")
    parser.add_argument(
        '-r', '--ranges', action='store_true', default=False, dest='ranges',
        help="Dump all the current lat and lon ranges.")
    parser.add_argument(
        '-s', '--sunset', action='store_true', default=False, dest='sunset',
        help="Find the sunset.")
    options = parser.parse_args()
    fll = FindLatLon()

    if options.lat_lon:
        data = fll.start()
    elif options.days:
        data = fll.find_first_and_last_day()
    elif options.ranges:
        data = fll.create_lat_lon_ranges()
    elif options.sunset:
        items = fll.find_sunset_for_wc_dates()
        data = [f"{year}, {str(fll.gc.gregorian_from_fixed(ve_day)):<29}, "
                f"{str(fll.gc.gregorian_from_fixed(ss_day)):<29}, {b}"
                for year, ve_day, ss_day, b in items]
    else:
        data = []

    pprint.pprint(data)
