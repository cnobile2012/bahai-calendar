# -*- coding: utf-8 -*-
#
# badidatetime/test/test___init__.py
#
__docformat__ = "restructuredtext en"

import importlib
import unittest

from badidatetime import (_local_timezone_info, _get_local_coordinates,
                          enable_geocoder)


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
           of values because every locat would give different information.
        """
        lat, lon, zone = _get_local_coordinates()
        self.assertTrue(isinstance(lat, float),
                        f"The lat {lat} was not a float.")
        self.assertTrue(isinstance(lon, float),
                        f"The lon {lon} was not a float.")
        self.assertTrue(isinstance(zone, float),
                        f"The zone {zone} was not a float.")

    #@unittest.skip("Temporarily skipped")
    def test_enable_geocoder(self):
        """
        Test that the enable_geocoder function enables the geocoder.

        .. note::

           This test requires internet access.
        """
        badidt = importlib.import_module('badidatetime.datetime')
        self.assertEqual(badidt.LOCAL_COORD, badidt.BADI_COORD,
                         f"The LOCAL_COORD should be {badidt.BADI_COORD}, "
                         f"found {badidt.LOCAL_COORD}")
        self.assertEqual(badidt.LOCAL, badidt.BADI,
                         f"The LOCAL should be {badidt.BADI}, "
                         f"found {badidt.LOCAL}")
        enable_geocoder()
        self.assertNotEqual(badidt.LOCAL_COORD, badidt.BADI_COORD,
                            "The LOCAL_COORD should not be "
                            f"{badidt.BADI_COORD}, found {badidt.LOCAL_COORD}")
        self.assertNotEqual(badidt.LOCAL, badidt.BADI,
                            f"The LOCAL should not be {badidt.BADI}, "
                            f"found {badidt.LOCAL}")
