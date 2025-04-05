.. -*-coding: utf-8-*-

************************
Additional Documentation
************************


==================
Julian Period Days
==================

The most commonly used algorithm to convert to-and-from different Calendars is
the JD (Julian period Day). You take your date from some calendar feed it into
a function then out pops a JD. Then take that JD and feed it into the  reverse
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
them. Even if I had found at least one of them, it would only have been for the
Gregorian calendar. So as such I had to write my own formulas for both the
Gregorian and Badí' calendars. This required both to-and-from formulas or four
formulas in total.

As mentioned elsewhere the Gregorian calendar uses a guestimation formula to
determine the leap years so figuring out the leap years is rather easy. It is
both consistant and regular. The Badí' calendar, on the other hand, uses the
sunset before the Vernal Equinox to determine the start of the year. There is
little regularity in this, so determining leap years was a much bigger task,
therefore the formula for the Badí' calendar needed coefficients to get it in
line with a proper representation of the JD when checking against the Gregorian
calendar. In my code the Astronomically formula is used by passing either
`True` or `False` to the keyword `exact`.

================
POSIX Timestamps
================

The POSIX timestamp had some of the same issues as the Julian Period day
discussed above. It also needed to factor in coefficients to get inline with
the Gregorian timestamps. However, because the timestamps are different
depending on the timezone there is a minimum and maximum deviation between the
Gregorian and Badí' timestamps. The deviation from a few time zones were tested
and shown to be between -241 and 121 seconds or about minus four minutes to
plus two minutes.

=============================
Strftime and Strptime Methods
=============================

This API supports many more conversion formats than the standard Python
library. Partially because The Badí' Calendar has calendar designators that
don't exist in the Gregorian Calendar and there are a lot of formats in general
that the standard library don't support.

.. code::

   %a  Abbreviated weekday name                     Jal
   %A  Full weekday name                            Jalál
   %b  Abbreviated month name                       Bah
   %B  Full month name                              Bahá
   %c  Locale specific date and time. Equivilent
       to "%a %b %d %H:%M:%S %Y"                    Jal Bah 01 22:30:45 181
   %C  Century as a space-padded decimal number.
       (The year divided by 100 then truncated
       to an integer.)                              15 or ' 1'
   %d  Day of month as a zero-padded decimal
       number.                                      09
   %-d Day of month as a decimal number.            9
   %D  Date where year is without century.
       Equivilent to a localized %m/%d/%y.          19/01/81
   %e  Day of the month as a space-padded decimal
       number.                                      15 or ' 1'
   %f  Microseconds as a decimal number.            000000 - 999999
   %G  ISO 8601 year with century as a zero-padded
       decimal number.                              0181
   %h  Abbreviated month name (Same as %b).         Bah
   %H  Hour (24-hour clock) as a zero-padded
       decimal number.                              00, 01, ..., 23
   %-H Hour (24-hour clock) as a decimal number.    0, 1, ..., 23
   %I  Hour (12-hour clock) as a zero-padded
       decimal number.                              01, 02, ..., 12
   %j  Day of the year as a zero-padded decimal
       number.                                      001, 002, ..., 366
   %-j Day of the year as a decimal number.         1, 2, ..., 366
   %k  Hour (24-hour clock) as a space-padded
       decimal number.                              0 to 23
   %:K Kull-i-Shay as a negative or positive
       decimal number.                              -5 to 4
   %l  Hour (12-hour clock) as a space-padded
       decimal number.                              ' 1' to 12
   %-l Hour (12-hour clock) as a decimal number.    1 to 12
   %m  Month as a zero-padded decimal number.       01 - 19
   %-m Month as a decimal number.                   1 - 19
   %M  Minute as a zero-padded decimal number.      00, 01, ..., 59
   %-M Minute as a decimal number.                  0, 1, ..., 59
   %n  Newline character.                           \n
   %p  Locale defined AM and PM.                    am or pm
   %r  Locale defined 12-hour clock time (am/pm).   3:30:45 PM
   %S  Second as a zero-padded decimal number.      00, 01, ..., 59
   %-S Second as a decimal number.                  0, 1, ..., 59
   %T  Locale defined 24-hour clock time.           15:30:45
   %u  Weekday as a decimal number. [1(Jalál), 7]   1 - 7
   %U  Week number of the year (Jalál as the first
       day of the week) as a zero-padded decimal
       number. All days in a new year preceding
       the first `Idāl are considered to be in
       week 0.                                      00, 01, ..., 52
   %V  ISO 8601 week as a zero-padded decimal
       number with Jalál as the first day of the
       week. Week 01 is the week containing the
       4th of Bahá.                                 01, 02, ..., 52
   %:V Váḥid as a zero-padded decimal number.       01 - 19
   %w  Weekday as a decimal number. [0(Jalál), 6]   0 - 6
   %W  Week number of the year (Jalál as the first
       day of the week) as a zero-padded decimal
       number. All days in a new year preceding
       the first Jalál are considered to be in
       week 0. It make no sense to start a week
       on different day in the Badi Calendar. So
       this format is the same as %U.               00, 01, ..., 52
   %x  Locale defined date representation.          08/16/81 (None);
                                                    08/16/181 (en_US);
                                                    16.08.181 (de_DE)
   %X  Locale defined time representation.          21:30:00 (en_US);
                                                    21:30:00 (de_DE)
   %y  Year without century as a zero-padded
       decimal number.                              00, 01, ..., 99
   %-y Year without century as a decimal number.    0, 1, ..., 99
   %Y  Year with century as a zero-padded decimal
       number.                                      0001, 0002, ..., 1161
   %z  UTC offset in the form ±HHMM[SS[.ffffff]]
       (empty string if the object is naive).       (empty), +0000, -0400,
                                                    +1030, +063415,
                                                    -030712.345216
   %:z UTC offset in the form ±HH:MM[:SS[.ffffff]]  (empty string if the
                                                     object is naive).
   %Z  Time zone name (empty string if the object
       is naive).                                   (empty), UTC, GMT
   %%  A literal '%' character.                     %
