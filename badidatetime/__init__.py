# -*- coding: utf-8 -*-
#
# badidatetime/__init__.py
#
__docformat__ = "restructuredtext en"

import sys
from tzlocal import get_localzone
from datetime import datetime as _dtime

from badidatetime.badi_calendar import BahaiCalendar
from badidatetime.gregorian_calendar import GregorianCalendar

dt_objects = ('date', 'datetime', 'time', 'timezone', 'timedelta', 'tzinfo',
              'TZWithCoords', 'MINYEAR', 'MAXYEAR', 'BADI_IANA', 'BADI_COORD',
              'GMT_COORD', 'UTC', 'BADI', 'LOCAL_COORD', 'LOCAL', 'MONTHNAMES',
              'MONTHNAMES_ABV', 'DAYNAMES', 'DAYNAMES_ABV')


__version__ = "1.1.0"


def _local_timezone_info():
    """
    Returns the offset in seconds, dst, IANA timezone key.

    :returns: Offset in seconds, True or False for dst, and the IANA key.
    :rtype: tuple

    .. note::

       Currently this must use the Python built in datetime, because the
       tzlocal package does not work completely with the Badí' datetime
       package.
    """
    localzone = get_localzone()
    dt = _dtime.now(localzone)
    offset = dt.utcoffset().total_seconds()
    dst = dt.dst().total_seconds() != 0
    return offset, dst, localzone.key


def _get_local_coordinates() -> tuple:
    """
    Get the locales coordinates and timezone offset for generating the
    Rata Die.

    :returns: The latitude, longitude, and the offset in hours.
    :rtype: tuple
    """
    import geocoder
    offset, dst, key = _local_timezone_info()
    # Get latitude and longitude
    g = geocoder.ip('me')
    latitude = g.lat
    longitude = g.lng
    return latitude, longitude, offset / 3600


def enable_geocoder(enable: bool=True) -> None:
    """
    Enable or disable the geocode query to find the local latitude, longitude,
    and time zone. If this function is never run then the defaults for
    `datetime.LOCAL_COORD` and `datetime.LOCAL` will be the Tehran Iran locale.

    :param bool enable: If True (default) geocoder is run else if False it is
                        not run.
    """
    import importlib
    badidt = importlib.import_module('badidatetime.datetime')

    if enable:
        badidt.LOCAL_COORD = _get_local_coordinates()
    else:
        badidt.LOCAL_COORD = badidt.BADI_COORD

    badidt.LOCAL = badidt.timezone.local = badidt.timezone._create(
        badidt.timedelta(hours=badidt.LOCAL_COORD[2]))

    for obj in dt_objects:
        globals()[obj] = getattr(sys.modules["badidatetime.datetime"], obj)


def init_leap_cache():
    """
    Initialize variables used to find Rata Die.
    """
    bc = BahaiCalendar
    bc._YEAR_START = BahaiCalendar()._build_badi_year_start()
    bc._RD_END = bc._YEAR_START[bc.MAXYEAR]


if BahaiCalendar._YEAR_START is None:
    init_leap_cache()


enable_geocoder(False)
__all__ = ('BahaiCalendar', 'GregorianCalendar', 'enable_geocoder',
           'init_leap_cache', '__version__') + dt_objects
