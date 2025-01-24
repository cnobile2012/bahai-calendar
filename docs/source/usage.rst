.. -*-coding: utf-8-*-

Usage
=====

Because of the nature of the Badí' Calendar things will work a little
differently than with the standard Python datetime package. Most computers know
what timezone they are in, but do not know the latitude and longitude of your
location. However, the Badí' Calendar needs this information to get the correct
sunset for any location.

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

