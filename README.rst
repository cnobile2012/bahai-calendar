***************************************
Introduction to the Badí' Calendar API
***************************************

The objective of this API (Application Programming Interface) is to implement
a library that provides date and time functionality similar to the standard
Python datetime package, however for the Badí' Calendar This API is much more
extensive than that standard package as it includes all the astronomical
calculations to fine the Vernal Equinox and sunset and many other calculation
that are needed to support how the Badí' Calendar works.

The Badí' Calendar is an Astronomical Self-correcting Solar Calendar. What that
means is that the determination of leap years is based on the number of days
from the sunset immediately preceding the Vernal Equinox to the sunset
immediately preceding the next Vernal Equinox. Counting the days it will either
be 365 an ordinary year or 366 a leap year. Since it uses the time of the
Vernal Equinox and the only way to calculate this is by doing lots of
astronomical calculations it's an Astronomical Calendar. It's self-correcting
because there is no guess work since we know exactly when the Vernal Equinox
occurs. It is solar because the Vernal Equinox has a direct relation to the
Sun. The Badí' leap years are irregular and cannot be guess with a simple
formula.

In contrast the Gregorian Calendar is only a solar calendar. It uses an
approximation formula--simple as it is--to essentially guess when the leap
years will be. This formula I call a 4/100/400 formula. In other words a leap
year is every 4 years unless the year is divisible by 100 then it isn't a leap
year unless it is also dividable by 400 then it is a leap year. So Gregorian
leap years are on a 400 year cycle and can be predicted by the above formula
thus making leap years fairly regular.

Much of the astronomical code is derived from Astronomical Algorithms by Jean
Meeus and there are some code snippits from Calendrical Calculations -- The
Ultimate Edition 4th Edition by Edward M. Reingold and Nachum Dershowitz.

Feel free to contact me at: carl dot nobile at gmail dot com
