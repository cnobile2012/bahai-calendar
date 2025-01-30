# -*- coding: utf-8 -*-
#
# badidatetime/__init__.py
#
__docformat__ = "restructuredtext en"

import importlib
import geocoder
from tzlocal import get_localzone
from datetime import datetime as _dtime

from badidatetime.badi_calendar import BahaiCalendar
from badidatetime.gregorian_calendar import GregorianCalendar

dt_objects = ('date', 'datetime', 'time', 'timezone', 'timedelta', 'tzinfo',
              'MINYEAR', 'MAXYEAR', 'BADI_IANA', 'BADI_COORD', 'GMT_COORD',
              'UTC', 'BADI', 'LOCAL_COORD', 'LOCAL')


def _local_timezone_info():
    """
    Returns the offset in seconds, dst, IANA timezone key.

    :returns: Offset in seconds, True or False for dst, and the IANA key.
    :rtype: tuple

    .. note::

       Currently this must use the Python built in datetime, because the
       tzlocal package does not work completely with the badi datetime
       package.
    """
    localzone = get_localzone()
    dt = _dtime.now(localzone)
    offset = dt.utcoffset().total_seconds()
    dst = dt.dst().total_seconds() != 0
    return offset, dst, localzone.key


def _get_local_coordinates():
    """
    Get the locales coordinates and timezone offset.
    """
    offset, dst, key = _local_timezone_info()

    # Get latitude and longitude
    g = geocoder.ip('me')
    latitude = g.lat
    longitude = g.lng
    return latitude, longitude, offset / 3600


def enable_geocoder(enable=True):
    """
    Enable or disable the geocode query to find the local latitude,
    longitude, and zone. If this function is never run then the geocoder
    is nor run the the defaults for datetime.LOCAL_COORD and datetime.LOCAL
    as set to be local in Tehran Iran.

    :param bool enable: If True (default) geocoder is run else if False it
                        is not run.
    """
    badidt = importlib.import_module('badidatetime.datetime')

    if enable:
        badidt.LOCAL_COORD = _get_local_coordinates()
    else:
        badidt.LOCAL_COORD = badidt.BADI_COORD

    badidt.LOCAL = badidt.timezone.local = badidt.timezone._create(
        badidt.timedelta(hours=badidt.LOCAL_COORD[2]))
    badidt = importlib.reload(badidt)

    for obj in dt_objects:
        globals()[obj] = getattr(badidt, obj)


enable_geocoder(False)
__all__ = ('BahaiCalendar', 'GregorianCalendar', 'enable_geocoder')+dt_objects
