.. -*-coding: utf-8-*-

***************************************
Introduction to the Badí' Calendar API
***************************************

.. image:: https://img.shields.io/badge/license-MIT-green
   :target: https://en.wikipedia.org/wiki/MIT_License
   :alt: License

.. image:: https://img.shields.io/pypi/v/badidatetime.svg
   :target: https://pypi.python.org/pypi/badidatetime
   :alt: PyPi Version

.. image:: http://img.shields.io/pypi/wheel/badidatetime.svg
   :target: https://pypi.python.org/pypi/badidatetime
   :alt: PyPI Wheel

.. image:: https://github.com/cnobile2012/bahai-calendar/actions/workflows/main.yml/badge.svg?branch=main
   :target: https://github.com/cnobile2012/bahai-calendar/actions/workflows/main.yml
   :alt: Build Status

.. image:: https://coveralls.io/repos/github/cnobile2012/bahai-calendar/badge.svg?branch=main
   :target: https://coveralls.io/github/cnobile2012/bahai-calendar?branch=main
   :alt: Coverage


The objective of this API (Application Programming Interface) is to implement
a library that provides date and time functionality similar to that of the
standard Python datetime package, however for the Badí' Calendar this API is
much more extensive than the standard package as it includes all the
astronomical calculations needed to find the Vernal Equinox, sunset, and many
other astronomical events that are needed to support the Badí' Calendar.

The Badí' Calendar is an Astronomical Self-correcting Solar Calendar. Since
astronomical calculations are needed it is obviously an astronomical calendar.
It is self-correcting because the leap years are determined by calculating the
number of days from the sunset immediately preceding the Vernal Equinox to the
sunset immediately preceding the next Vernal Equinox. Counting the days it will
either be 365 on ordinary years or 366 on leap years. The Vernal Equinox is
when the sun is directly above the equator and the length of the day and night
are nearly equal making it a solar calendar.

The Badí' leap years are irregular and cannot be guess with a simple formula as
is done with Gregorian Calendar. Quite in contrast the Gregorian Calendar is
only a solar calendar. It uses an approximation formula to guess when the leap
years will be. This formula I call a 4/100/400 formula. In other words a leap
year is every 4 years unless the year is divisible by 100 then it is not a leap
year unless it is also dividable by 400 then it is a leap year. This means that
the Gregorian leap years are on a 400 year cycle and can be predicted by the
above formula thus making leap years fairly regular. 

------------
Attributions
------------

Much of the astronomical code is derived from Astronomical Algorithms by Jean
Meeus and there are some code snippets from Calendrical Calculations -- The
Ultimate Edition 4th Edition by Edward M. Reingold and Nachum Dershowitz.

Feel free to contact me at: carl dot nobile at gmail dot com

Complete Documentation can be found on
`Read the Docs <https://readthedocs.org/>`_ at:
`A Badí' Date and Time API <http://badidatetime.readthedocs.io/en/latest/>`_
