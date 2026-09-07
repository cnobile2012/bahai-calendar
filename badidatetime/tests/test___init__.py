# -*- coding: utf-8 -*-
#
# badidatetime/test/test___init__.py
#
__docformat__ = "restructuredtext en"

import unittest

from badidatetime import (
    _local_timezone_info, _get_local_coordinates, set_local_coordinates,
    CoordinateManager, has_local_coords, BADI_COORD, LOCAL_COORD)


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
        data = (
            (35.7796, -78.6382, '', (35.7796, -78.6382)),
            (None, None, 'New York', (40.7127281, -74.0060152)),
            (None, None, '', (None, None)),
            )
        msg = "Expected {}, found {}."

        for lat, lon, locale, expected in data:
            set_local_coordinates(lat=lat, lon=lon, locale=locale)
            result = _get_local_coordinates()
            self.assertEqual(expected, result[:2], msg.format(
                expected, result[:2]))

            if result[-1] is not None:
                self.assertIsInstance(result[-1], float)

    #@unittest.skip("Temporarily skipped")
    def test_set_local_coordinates(self):
        """
        Test that the set_local_coordinates function correctly sets the locale.
        """
        data = (
            (35.7796, -78.6382, '', (35.7796, -78.6382)),
            ('35.7796', '-78.6382', '', (35.7796, -78.6382)),
            (None, None, 'New York', (40.7127281, -74.0060152)),
            (None, None, '', (35.69435, 51.113642)),  # Defaults to Tehran
            )
        msg = "Expected {}, found {}."

        for lat, lon, locale, expected in data:
            set_local_coordinates(lat, lon, locale=locale)
            self.assertEqual(expected, LOCAL_COORD[:2], msg.format(
                expected, LOCAL_COORD[:2]))

    #@unittest.skip("Temporarily skipped")
    def test_has_local_coords(self):
        """
        Test that the has_local_coords function returns True if the local
        is set or False if not set.
        """
        data = (
            (LOCAL_COORD, BADI_COORD, False, False),
            (LOCAL_COORD, BADI_COORD, True, True),
            )
        msg = "Expected {}, found {}."

        for local, badi, update, expected in data:
            if update:
                set_local_coordinates(35.7796, -78.6382)
            else:
                set_local_coordinates(*BADI_COORD[:2])

            result = has_local_coords()
            self.assertEqual(expected, result, msg.format(expected, result))


class TestCoordinateManager(unittest.TestCase):

    def __init__(self, name):
        super().__init__(name)

    def setUp(self):
        self.COORDS = CoordinateManager()

    def tearDown(self):
        self.COORDS = None

    #@unittest.skip("Temporarily skipped")
    def test_use_as_tuple(self):
        """
        Test that the object created by the CoordinateManager class can
        be used as a tuple.
        """
        coords = (35.7796, -78.6382, -5)
        msg = "Expected {}, found {}."
        self.COORDS.set(coords)
        self.assertEqual(coords, tuple(self.COORDS), msg.format(
            coords, tuple(self.COORDS)))
        self.assertEqual(coords[1], self.COORDS[1])

    #@unittest.skip("Temporarily skipped")
    def test___len__(self):
        """
        Test that the __len__ correctly returns the length of the
        CoordinateManager.
        """
        coords = (35.7796, -78.6382, -5)
        msg = "Expected {}, found {}."
        self.COORDS.set(coords)
        self.assertEqual(len(coords), len(self.COORDS), msg.format(
            len(coords), len(self.COORDS)))

    #@unittest.skip("Temporarily skipped")
    def test___repr__(self):
        """
        Test that the __repr__ returns the correct representation of the
        current CoordinateManager class object.
        """
        coords = (35.7796, -78.6382, -5)
        expected = "CoordinateManager(35.7796, -78.6382, -5)"
        msg = "Expected {}, found {}."
        self.COORDS.set(coords)
        result = repr(self.COORDS)
        self.assertEqual(expected, result, msg.format(expected, result))
