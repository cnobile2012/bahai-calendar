.. -*-coding: utf-8-*-

*************************
Some Ideas for the Future
*************************

The following are purely speculations into the future.

====================
Geographic Longitude
====================

Jean Meeus in his book Astronomical Algorithms the 1998 edition in chapter
13 p. 93 in a section named "Note on the geographic longitudes" complained that
the International Astronomical Union set the geographic longitude negative
westward and positive eastward of the meridian of Greenwich, in direct
contradiction to the standards used before and currently on all other planetary
bodies.

Since the Badí' Calendar relies heavily on where the latitude ans longitude is,
to give a correct location for the sunset and Vernal Equinox, whether the
longitude is negative or positive has serious implications. Meeus choose to not
follow the IAU resolution, and keep the original definition the same as the
other planetary bodies. My work keeps with the IAU resolution as most software
engineers using my code would not know why nothing was working correctly.

As such in the future the Bahá'í World Center may choose to influence changes
to the standard making everything the same.

====================================================
Location of the Origin for Astronomical Observations
====================================================

In 2014 the Bahá'í World Center published documents defining Tehran, Iran to be
that origin. As explained in :ref:`rst Letter from The Universal House of
Justice`, the `Universal House of Justice` may change the origin point in the
future.

The effects on this API would be mostly in the unittest code, as most
developers know many things are hard-coded in unittest code, thus a change to
the latitude and longitude and the time zone will break many of the tests. The
unittests provide a baseline for the validity of the entire API and must be
fixed if the origin point should ever change.

However, most of the API code, with the one caveats below, will not be affected
and its accuracy should also not be affected.

In the class :ref:`rst Class` the methods that may break are
`jd_from_badi_date` which then calls the `_get_coff`. These two method
determine the Julian Period day for any date. This is done by finding the exact
date and time for the first day of each year, Bahá 1 of each year
supported. This is needed to determine many other things including which year
is a leap year. So once this method is fixed for a change in origin point and
the new latitude and longitude are set in the :ref:`rst Class` everything else
should work as expected.
