.. -*-coding: utf-8-*-

=====
Usage
=====

Because of the nature of the Badí' Calendar things will work a little
differently compared to the standard Python datetime package. Most computers
know what timezone they are in, but do not know the latitude and longitude of
your location. However, the Badí' Calendar needs this information to get the
correct sunset for any location.

----------
Initialize
----------

To start off you must decide if you will have internet access while using the
API. If not then the latitude and longitude cannot be obtained and some
functionality will not be accurate in the date (fromtimestamp) and datetime
(fromtimestamp, atimezone, and the LOCAL timezone object) classes. This
functionality defaults to the latitude, longitude, and zone of Tehran thus
unless you happen to live in Tehran any local information will be wrong.

The default setting is to not do a WEB request for the latitude and longitude.
If you want to do this request then follow the processed outlined below. Be
sure to always import the packages you want after enabling the geocoder.

.. code-block:: python

   from badidatetime import enable_geocoder

   enable_geocoder()

   from badidatetime import date, time, datetime, ...

The ``enable_geocoder()`` function takes one argument which can be set either
``True`` (default) or ``False``.

---------------
datetime module
---------------

++++++++++++++++
Provides Classes
++++++++++++++++

| timedelta
| date
| tzinfo
| time
| datetime
| timezone

This module has most of the features that is in the standard datetime module
that comes with Python. There are exceptions in two categories.

1. The badidatetime module conforms to the Badí' Calendar not the Gregorian
   Calendar. Where a Gregorian date is always represented as yy-mm-dd (ISO
   notation), the Badí' Calendar has two formats which are called the long and
   short form dates.

   a. Short form dates are represented the same as the Gregorian as yy-mm-dd
      and the ISO standard would work the same also.

   b. Long form dates are the Kull-i-Shay’, Váḥid, Year, Month, and Day. I have
      extended the ISO notation as kk-vv-yy-mm-dd. Remember that the year in
      this form is not the same as the year in the short form. The formula for
      converting to the short form is year = (kk-1) * 361 + (vv-1) * 19 + yy,
      the month and day are the same as the short form month and day.

2. Other than the **UTC** timezone object the badidatetime module also provides
   a few other constants.

   a. The **BADI** timezone object.

   b. The **LOCAL** timezone object. The **LOCAL** object will be the same as
      the **BADI** object if the geocoder is left disabled, see above.

   c. The **BADI_IANA** which gives the IANA text value for the Tehran, Iran.

   d. The **BADI_COORD** is a tuple containing the latitude, longitude, and
      offset from GMT.

   e. The **GMT_COORD** is a tuple containing the latitude, longitude, and
      offset of GMT.

   f. The **LOCAL_COORD** is a tuple containing the latitude, longitude, and
      offset of the current locale or will be the same as **BADI_COORD** if the
      geocoder is left disabled, see above.


--------------------
badi_calendar module
--------------------

++++++++++++++
Provides Class
++++++++++++++

BahaiCalendar

This module provides the functionality needed for finding the Julian Period day
and the long or short form Badí' dates along with conversion methods between
Badí' and Gregorian dates. There is also a conversion method to convert a
``POSIX`` timestamp to a Badí' date. The **BahaiCalendar** class supers both
the **BaseCalendar** and **JulianPeriod** classes. Internally fractions of days
are a fraction on the day. These fractions can generally be converted to hours,
minutes, seconds, and microseconds.

-------------------------
gregorian_calendar module
-------------------------

++++++++++++++
Provides Class
++++++++++++++

GregorianCalendar

This module provides the functionality needed for finding the Julian Period day
and the Gregorian dates. Internally fractions of days are a fraction on the
day. These fractions can generally be converted to hours, minutes, seconds, and
microseconds.

--------------------
base_clanedar module
--------------------

++++++++++++++
Provides Class
++++++++++++++

BaseCalendar


This module provides all the heavy lifting for the astronomical calculations.
All the methods are protected, so unless you are fixing a bug in this API you
will probably never need to use any methods in the BaseCalendar class.

--------------------
julian_period module
--------------------

++++++++++++++
Provides Class
++++++++++++++

JulianPeriod

This module provides functionality to convert a Julian Period day to a century
and millennia. All the methods are protected, so unless you are fixing a bug in
this API you will probably never need to use any methods in the JulianCalendar
class.
