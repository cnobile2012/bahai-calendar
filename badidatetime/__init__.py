# -*- coding: utf-8 -*-
#
# badidatetime/__init__.py
#
__docformat__ = "restructuredtext en"

import sys
from datetime import datetime as _dtime

from tzlocal import get_localzone
from geopy.geocoders import Nominatim

from badidatetime.badi_calendar import BahaiCalendar
from badidatetime.gregorian_calendar import GregorianCalendar

dt_objects = ('date', 'datetime', 'time', 'timezone', 'timedelta', 'tzinfo',
              'TZWithCoords', 'MINYEAR', 'MAXYEAR', 'BADI_IANA', 'BADI_COORD',
              'GMT_COORD', 'UTC', 'BADI', 'LOCAL_COORD', 'LOCAL', 'MONTHNAMES',
              'MONTHNAMES_ABV', 'DAYNAMES', 'DAYNAMES_ABV')


__version__ = "1.2.0"
_LOCAL_COORDS = ()


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


def _get_local_coordinates() -> tuple | None:
    """
    Get the locales coordinates and timezone offset for generating the
    Rata Die.

    :returns: The latitude, longitude, and the offset in hours.
    :rtype: tuple or None
    """
    offset, dst, key = _local_timezone_info()

    if (_LOCAL_COORDS and isinstance(_LOCAL_COORDS[0], float)
        and isinstance(_LOCAL_COORDS[1], float)):
        lat = _LOCAL_COORDS[0]
        lon = _LOCAL_COORDS[1]
    elif _LOCAL_COORDS and _LOCAL_COORDS[2]:
        geolocator = Nominatim(user_agent='nc-bookkeeper')
        location = geolocator.geocode(_LOCAL_COORDS[2])
        assert location, ("Could not find the latitude and longitude with "
                          f"locale {_LOCAL_COORDS[2]}.")
        lat = location.latitude
        lon = location.longitude
    else:
        lat = lon = None

    return lat, lon, offset / 3600


def _locale_config() -> None:
    """
    This function sets the `datetime.LOCAL_COORD` and `datetime.LOCAL`
    variables.
    """
    import importlib
    badidt = importlib.import_module('badidatetime.datetime')
    coords = _get_local_coordinates()

    if None in coords:
        badidt.LOCAL_COORD = badidt.BADI_COORD
    else:
        badidt.LOCAL_COORD = coords

    badidt.LOCAL = badidt.timezone.local = badidt.timezone._create(
        badidt.timedelta(hours=badidt.LOCAL_COORD[2]))

    for obj in dt_objects:
        globals()[obj] = getattr(sys.modules["badidatetime.datetime"], obj)


def set_local_coordinates(lat: float=None, lon: float=None, *,
                          locale: str | int='') -> None:
    """
    Either supply the latitude and longitude or the locale. If you supply the
    the latitude and longitude not network call is needed, the locale will
    cause a network call. If nothing is supplied the coordinents for Tehran
    will be used.

    :param float lat: the latitude of your locale.
    :param float lon: the longitude of your locale.
    :param str or int locale: This is your city, street address, or zip code.
    """
    global _LOCAL_COORDS
    _LOCAL_COORDS = (lat, lon, locale)
    _locale_config()


def init_leap_cache():
    """
    Initialize variables used to find Rata Die.
    """
    bc = BahaiCalendar
    bc._YEAR_START = BahaiCalendar()._build_badi_year_start()
    bc._RD_END = bc._YEAR_START[bc.MAXYEAR]


if BahaiCalendar._YEAR_START is None:
    init_leap_cache()


__all__ = ('BahaiCalendar', 'GregorianCalendar', 'init_leap_cache',
           'set_local_coordinates', '__version__') + dt_objects
