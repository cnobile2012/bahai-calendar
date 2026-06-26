# -*- coding: utf-8 -*-
#
# badidatetime/test/test___init__.py
#
__docformat__ = "restructuredtext en"

import importlib
import unittest

from badidatetime import (_local_timezone_info, _get_local_coordinates,
                          _locale_config, set_local_coordinates)


class Test__init__(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    #@unittest.skip("Temporarily skipped")
    def test__local_timezone_info(self):
        """
        Test that the _local_timezone_info function returns the timezone
        offset in seconds, dst (0 or 1), and the IANA key.

        .. note::

           The _local_timezone_info cannot be tested, because any test requires
           knowing the exact local timezone, and would break if not run in the
           same timezone that the test we written for. Soooo, we just test that
           data is returned.
        """
        offset, dst, iana = _local_timezone_info()
        self.assertTrue(isinstance(offset, float),
                        f"The offset {offset} was not an float.")
        self.assertTrue(dst in (0, 1), "The dst was not a 0 or 1")
        self.assertTrue(isinstance(iana, str),
                        f"The IANA {iana} was not a string.")

    #@unittest.skip("Temporarily skipped")
    def test__get_local_coordinates(self):
        """
        Test that the _get_local_coordinates function returns the local
        coordinates and timezone offset.

        .. note::

           This test requires internet access. We only test for the existance
           of values because every locale would give different information.
        """
        set_local_coordinates(lat=35.7796, lon=-78.6382)
        lat, lon, zone = _get_local_coordinates()
        self.assertTrue(isinstance(lat, float),
                        f"The lat {lat} was not a float, found {type(lat)}.")
        self.assertTrue(isinstance(lon, float),
                        f"The lon {lon} was not a float, found {type(lon)}.")
        self.assertTrue(
            isinstance(zone, float),
            f"The zone {zone} was not a float, found {type(zone)}.")

    #@unittest.skip("Temporarily skipped")
    def test_set_local_coordinates(self):
        """
        Test that the set_local_coordinates function correctly sets up the
        locale.
        """
        data = (
            (35.7796, -78.6382, '', (35.7796, -78.6382)),
            (None, None, 'New York', (40.7127281, -74.0060152)),
            (None, None, '', (35.69435, 51.288701)),
            )
        msg = "Expected {}, found {}."

        for lat, lon, locale, expected in data:
            set_local_coordinates(lat, lon, locale=locale)
            from badidatetime import LOCAL_COORD, BADI_COORD
            self.assertEqual(expected, LOCAL_COORD[:2])
