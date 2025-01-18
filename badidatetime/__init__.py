# -*- coding: utf-8 -*-
#
# badidatetime/__init__.py
#
__docformat__ = "restructuredtext en"

import os
import geocoder
from tzlocal import get_localzone
from badidatetime.badi_calendar import BahaiCalendar
from badidatetime.gregorian_calendar import GregorianCalendar
from datetime import datetime as _dtime

if os.getenv('DEBUG', False):
    from badidatetime import datetime
    __all__ = ('disable_geocoder',)
else:
    from badidatetime.datetime import *

    __all__ = ('date', 'datetime', 'time', 'timedelta', 'timezone', 'tzinfo',
               'MINYEAR', 'MAXYEAR', 'BADI_IANA', 'BADI_COORD', 'GMT_COORD',
               'LOCAL_COORD', 'UTC', 'BADI', 'LOCAL', 'BahaiCalendar',
               'GregorianCalendar', 'disable_geocoder')

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

def disable_geocoder(off=False):
    """
    Turn off the geocode query to find the local lattitude, longitude, and
    zone. This method MUST be imported and executed before the badidatetime
    is imported.

    :param bool off: If False (default) geocoder is run else if True it is not.
    """
    from badidatetime import datetime

    if not off:
        datetime.LOCAL_COORD = _get_local_coordinates()

    datetime.LOCAL = datetime.timezone.local = datetime.timezone._create(
        datetime.timedelta(hours=datetime.LOCAL_COORD[2]))
    del datetime

disable_geocoder()
