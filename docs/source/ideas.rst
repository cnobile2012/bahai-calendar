.. -*-coding: utf-8-*-

*************************
Some Ideas for the Future
*************************

**The following are future speculations.**

====================
Geographic Longitude
====================

Jean Meeus in his book Astronomical Algorithms the 1998 edition in chapter
13 p. 93 in a section named "Note on the geographic longitudes" complained that
the International Astronomical Union set the geographic longitude negative
westward and positive eastward of the meridian of Greenwich, in direct
contradiction to the standards used before and currently on all other planetary
bodies.

Since the Badí' Calendar relies heavily on where the latitude and longitude is,
to give a correct location for the sunset and Vernal Equinox, whether the
longitude is negative or positive has serious implications. Meeus choose to not
follow the IAU resolution, and keep the original definition the same as the
other planetary bodies. My work keeps with the IAU resolution as most software
engineers using my code would not know why nothing was working correctly.

As such in the future the Bahá'í World Center may choose to influence changes
to the standard making everything the same. I could also envision that the
meridian in Greenwich change to the Badí' origin location as explained below.

====================================================
Location of the Origin for Astronomical Observations
====================================================

In 2014 the Bahá'í World Center published documents defining Tehran, Iran to be
that origin. As explained in the :ref:`rst Letter from The Universal House of
Justice`, the `Universal House of Justice` may change the origin point in the
future.

The effects on this API would be mostly in the unittest code, as most
developers know many things are hard-coded in unittest code, thus a change to
the latitude and longitude and the time zone will break many of the tests. The
unittests provide a baseline for the validity of the entire API and must be
updated if the origin point should ever change.

However, most of the API code, with the one caveats below, will not be affected
and its accuracy should also not be affected.

In the class :ref:`rst Class` the methods that may break are
``jd_from_badi_date`` which then calls the ``_get_coff``. These two methods
together determine the Julian Period day for any date. This is done by finding
the exact date and time for the first day, Bahá 1 at sunset at the origin point
for each supported year. This is needed to determine many other things
including which years are a leap years. So once this method is fixed for a
change in origin point and the new latitude and longitude are set in the
:ref:`rst Class` everything else should work as expected. To change the two
methods mentioned above read the comments in the :ref:`rst badi_jd_tests` these
methods are duplicated in this script, so they can be experimented with without
disturbing the actual code. Once a new algorithm is found the code must be
copied to the actual API, but **DO NOT** copy everything verbatim, there are
differences in how they work. The script should be run as shown below.

.. code:: bash

   ./contrib/misc/badi_jd_tests.py -aS-1842 -E1162 > SunsetFrom-1842to1161.txt

====================
Political Time Zones
====================

The `IANA <https://www.iana.org/time-zones>`_ time zones that most developers
are familiar with are essentially political in nature. In other words, they
often have artificial boundaries some are 15 minute and 30 minute off of the
Greenwich standard. The real time zones are defined very simply. If you
consider the Earth from any point on the Equator all the way around and back to
that point you have 360° then divide it by 24 hours you get 15° per hour. As it
stands now we need to know the IANA timezone to know what the offset is from
Greenwich time. However, if you just use the simple formula below we just need
to know the longitude to determine what the time offset is. 

.. math::

   offset = longitude / 15°

So if we use this formula for New York City during standard time where the
longitude is -73.935242, then the actual offset is -4.929016 hours not -5 hours
which is the political time zone. It is close, but not as exact. The longitude
that I use for Tehran Iran is 51.285817, if this is put into the formula above
we get 3.419055 hours, the political offset is 3.5 hours. A thirds example is
in India, the eastern side has a longitude of 81.1875° giving it an offset of
5.41 hours and the west side longitude is 71.5429687° giving it an offset of
4.769 hours. The political time zone for the whole country is 5.5 hours which
is more than any place in the country.

This is yet another issue that can be address in the future.

=====================
Daylight Savings Time
=====================

DST is very arbitrary, it creates a fold in time when going from DST to ST
(Standard Time) which needs to be disambiguated and a gap in time when going in
the other direction. I see no value to it at all within the Badí' Calendar,
however, I have kept it in the code. In the future I may start to remove
it. This should make things work much cleaner and with less confusion.

==================
Julian Period Days
==================

The most commonly used algorithm to convert to-and-from different Calendars is
the JD (Julian period Day). You take your date from some calendar feed it into
a function then out pops a JD. Then take that day and feed it into the  reverse
function for a different calendar and out pops the date that is equivalent to
the date of the first calendar. Essentially the JD is a continuous count of
days from the Gregorian/Julian calendar year -4712:01:01 at noon. Jean Meeus in
his book "Astronomical Algorithms" has a formula for doing this
conversion. This formula is historically correct meaning that all historical
Astronomical calculations for-the-most-part use it. The USNO has an `online
calculator <https://aa.usno.navy.mil/data/JulianDate>`_ for this formula.

Now I said "for-the-most-part" above because the formula mentioned above is
astronomically inaccurate for all dates before Gregorian year 1582 October 15
the date that Pope Gregory introduced the Gregorian calendar. The results are
that the Vernal Equinox gets earlier and earlier in the year the farther you go
back in time because the leap years were wrong. There is another problem with
this formula in that Pope Gregory only compensated for 10 days when in reality
he should have compensated for 12 days so even this formula is still off by two
days to this day.

Since the API that I have written is for the Badí' calendar the Vernal Equinox
calculations needed to be correct. This presented a few problems. I needed to
find an algorithm that was Astronomically correct or write one myself. I know
that both NASA and the USNO have an Astronomically correct formula, because
they both have tables of the correct Vernal Eqinox, but I was not able to find
it and if I did find it, it would only be for the Gregorian calendar. So as
such I had to write my own formulas for both the Gregorian and Badí' calendars.
This required both to-and-from formulas or four formulas in total.

As mentioned elsewhere the Gregorian calendar uses a guestimation formula to
determine the leap years so figuring out the leap years is rather easy. It is
both consistant and regular. The Badí' calendar uses the sunset before the
Vernal Equinox to determine the start of the year. There is no regularity in
this at all, so determining leap years was a much bigger task, therefore the
formula for the Badí' calendar needed coefficients to get it in line with a
proper representation of the JD when checking against the Gregorian calendar.
In my code I refer the Astronomically formula with the `exact` keyword being
either `True` or `False`.

================
POSIX Timestamps
================

The POSIX timestamp had some of the same issues as the Julian Period day
discussed above. It also needed to factor in coefficients to get inline with
the Gregorian timestamps. However, because the timestamps are different
depending on the timezone there is a minimum and maximum deviation between the
Gregorian and Badí' timestamps. This deviation from a few time zones that were
tested was between -241 and 121 seconds or about minus four minutes to plus two
minutes.
