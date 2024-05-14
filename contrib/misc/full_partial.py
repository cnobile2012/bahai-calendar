#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import sys
import math
import pprint

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(os.path.dirname(PWD))
sys.path.append(BASE_DIR)

from bahai_calendar import BahaiCalendar, GregorianCalendar


class DateTests(BahaiCalendar):
    START_K = -5
    END_K = 5

    # The following three must be updated in unison.
    # This must be the first Gregorian date in TMP_ANS_DATES below.
    START_G = 1494
    END_G = 3004
    TRAN_COFF = 1843
    # https://www.timeanddate.com/sun/@112931?month=3&year=1844
    # https://data.giss.nasa.gov/modelE/ar5plots/srvernal.html
    # https://aa.usno.navy.mil/data/RS_OneYear
    # https://www.sunrisesunset.com/England/GreaterLondon/Greenwich.asp
    # https://gml.noaa.gov/grad/solcalc/
    # Tehran: 35.696111 (35, 41, 45.9996), 51.423056 (51, 25, 23.0016)
    TMP_ANS_DATES = (
        ## (   1, 3, 23), (   2, 3, 23), (   3, 3, 23), (   4, 3, 23),
        ## (   5, 3, 23), (   6, 3, 23), (   7, 3, 23), (   8, 3, 23),
        ## (   9, 3, 23), (  10, 3, 23), (  11, 3, 23), (  12, 3, 23),
        ## (  13, 3, 23), (  14, 3, 23), (  15, 3, 23), (  16, 3, 23),
        ## (  17, 3, 23), (  18, 3, 23), (  19, 3, 23), (  20, 3, 23),
        ## (  21, 3, 23), (  22, 3, 23), (  23, 3, 23), (  24, 3, 22),
        ## (  25, 3, 23), (  26, 3, 23), (  27, 3, 23), (  28, 3, 22),
        ## (  29, 3, 23), (  30, 3, 23), (  31, 3, 23), (  32, 3, 22),
        ## (  33, 3, 23), (  34, 3, 23), (  35, 3, 23), (  36, 3, 22),
        ## (  37, 3, 23), (  38, 3, 23), (  39, 3, 23), (  40, 3, 22),

        ## (1491, 3, 21), (1492, 3, 20), (1493, 3, 20), (1494, 3, 20),
        ## (1495, 3, 21), (1496, 3, 20), (1497, 3, 20), (1498, 3, 21),
        ## (1499, 3, 21), (1500, 3, 21), (1501, 3, 21), (1502, 3, 21),
        ## (1503, 3, 22), (1504, 3, 21), (1505, 3, 21), (1506, 3, 21),
        ## (1507, 3, 22), (1508, 3, 21), (1509, 3, 21), (1510, 3, 21),
        ## (1511, 3, 22), (1512, 3, 21), (1513, 3, 21), (1514, 3, 21),
        ## (1515, 3, 22), (1516, 3, 21), (1517, 3, 21), (1518, 3, 21),
        ## (1519, 3, 22), (1520, 3, 21), (1521, 3, 21), (1522, 3, 21),
        ## (1523, 3, 21), (1524, 3, 21), (1525, 3, 21), (1526, 3, 21),
        ## (1527, 3, 21), (1528, 3, 21), (1529, 3, 21), (1530, 3, 21),
        ## (1531, 3, 21), (1532, 3, 21), (1533, 3, 21), (1534, 3, 21),
        ## (1535, 3, 21), (1536, 3, 21), (1537, 3, 21), (1538, 3, 21),
        ## (1539, 3, 21), (1540, 3, 21), (1541, 3, 21), (1542, 3, 21),
        ## (1543, 3, 21), (1544, 3, 21), (1545, 3, 21), (1546, 3, 21),
        ## (1547, 3, 21), (1548, 3, 21), (1549, 3, 21), (1550, 3, 21),
        ## (1551, 3, 21), (1552, 3, 21), (1553, 3, 21), (1554, 3, 21),
        ## (1555, 3, 21), (1556, 3, 21), (1557, 3, 21), (1558, 3, 21),
        ## (1559, 3, 21), (1560, 3, 21), (1561, 3, 21), (1562, 3, 21),
        ## (1563, 3, 21), (1564, 3, 20), (1565, 3, 21), (1566, 3, 21),
        ## (1567, 3, 21), (1568, 3, 20), (1569, 3, 21), (1570, 3, 21),
        ## (1571, 3, 21), (1572, 3, 20), (1573, 3, 21), (1574, 3, 21),
        ## (1575, 3, 21), (1576, 3, 20), (1577, 3, 21), (1578, 3, 21),
        ## (1579, 3, 21), (1580, 3, 20), (1581, 3, 21), (1582, 3, 21),
        # All dates before 1582-10-15 will be wrong.
        (1583, 3, 21), (1584, 3, 20), (1585, 3, 21), (1586, 3, 21),
        (1587, 3, 21), (1588, 3, 20), (1589, 3, 21), (1590, 3, 21),
        (1591, 3, 21), (1592, 3, 20), (1593, 3, 21), (1594, 3, 21),
        (1595, 3, 21), (1596, 3, 20), (1597, 3, 20), (1598, 3, 21),
        (1599, 3, 21), (1600, 3, 20), (1601, 3, 20), (1602, 3, 21),
        (1603, 3, 21), (1604, 3, 20), (1605, 3, 20), (1606, 3, 21),
        (1607, 3, 21), (1608, 3, 20), (1609, 3, 20), (1610, 3, 21),
        (1611, 3, 21), (1612, 3, 20), (1613, 3, 20), (1614, 3, 21),
        (1615, 3, 21), (1616, 3, 20), (1617, 3, 20), (1618, 3, 21),
        (1619, 3, 21), (1620, 3, 20), (1621, 3, 20), (1622, 3, 21),
        (1623, 3, 21), (1624, 3, 20), (1625, 3, 20), (1626, 3, 21),
        (1627, 3, 21), (1628, 3, 20), (1629, 3, 20), (1630, 3, 20),
        (1631, 3, 21), (1632, 3, 20), (1633, 3, 20), (1634, 3, 20),
        (1635, 3, 21), (1636, 3, 20), (1637, 3, 20), (1638, 3, 20),
        (1639, 3, 21), (1640, 3, 20), (1641, 3, 20), (1642, 3, 20),
        (1643, 3, 21), (1644, 3, 20), (1645, 3, 20), (1646, 3, 20),
        (1647, 3, 21), (1648, 3, 20), (1649, 3, 20), (1650, 3, 20),
        (1651, 3, 21), (1652, 3, 20), (1653, 3, 20), (1654, 3, 20),
        (1655, 3, 21), (1656, 3, 20), (1657, 3, 20), (1658, 3, 20),
        (1659, 3, 21), (1660, 3, 20), (1661, 3, 20), (1662, 3, 20),
        (1663, 3, 20), (1664, 3, 20), (1665, 3, 20), (1666, 3, 20),
        (1667, 3, 20), (1668, 3, 20), (1669, 3, 20), (1670, 3, 20),
        (1671, 3, 20), (1672, 3, 20), (1673, 3, 20), (1674, 3, 20),
        (1675, 3, 20), (1676, 3, 20), (1677, 3, 20), (1678, 3, 20),
        (1679, 3, 20), (1680, 3, 20), (1681, 3, 20), (1682, 3, 20),
        (1683, 3, 20), (1684, 3, 20), (1685, 3, 20), (1686, 3, 20),
        (1687, 3, 20), (1688, 3, 20), (1689, 3, 20), (1690, 3, 20),
        (1691, 3, 20), (1692, 3, 20), (1693, 3, 20), (1694, 3, 20),
        (1695, 3, 20), (1696, 3, 19), (1697, 3, 20), (1698, 3, 20),
        (1699, 3, 20), (1700, 3, 20), (1701, 3, 21), (1702, 3, 21),
        (1703, 3, 21), (1704, 3, 20), (1705, 3, 21), (1706, 3, 21),
        (1707, 3, 21), (1708, 3, 20), (1709, 3, 21), (1710, 3, 21),
        (1711, 3, 21), (1712, 3, 20), (1713, 3, 21), (1714, 3, 21),
        (1715, 3, 21), (1716, 3, 20), (1717, 3, 21), (1718, 3, 21),
        (1719, 3, 21), (1720, 3, 20), (1721, 3, 21), (1722, 3, 21),
        (1723, 3, 21), (1724, 3, 20), (1725, 3, 21), (1726, 3, 21),
        (1727, 3, 21), (1728, 3, 20), (1729, 3, 20), (1730, 3, 21),
        (1731, 3, 21), (1732, 3, 20), (1733, 3, 20), (1734, 3, 21),
        (1735, 3, 21), (1736, 3, 20), (1737, 3, 20), (1738, 3, 21),
        (1739, 3, 21), (1740, 3, 20), (1741, 3, 20), (1742, 3, 21),
        (1743, 3, 21), (1744, 3, 20), (1745, 3, 20), (1746, 3, 21),
        (1747, 3, 21), (1748, 3, 20), (1749, 3, 20), (1750, 3, 21),
        (1751, 3, 21), (1752, 3, 20), (1753, 3, 20), (1754, 3, 20),
        (1755, 3, 21), (1756, 3, 20), (1757, 3, 20), (1758, 3, 21),
        (1759, 3, 21), (1760, 3, 20), (1761, 3, 20), (1762, 3, 20),
        (1763, 3, 21), (1764, 3, 20), (1765, 3, 20), (1766, 3, 20),
        (1767, 3, 21), (1768, 3, 20), (1769, 3, 20), (1770, 3, 20),
        (1771, 3, 21), (1772, 3, 20), (1773, 3, 20), (1774, 3, 20),
        (1775, 3, 21), (1776, 3, 20), (1777, 3, 20), (1778, 3, 20),
        (1779, 3, 21), (1780, 3, 20), (1781, 3, 20), (1782, 3, 20),
        (1783, 3, 21), (1784, 3, 20), (1785, 3, 20), (1786, 3, 20),
        (1787, 3, 21), (1788, 3, 20), (1789, 3, 20), (1790, 3, 20),
        (1791, 3, 21), (1792, 3, 20), (1793, 3, 20), (1794, 3, 20),
        (1795, 3, 20), (1796, 3, 20), (1797, 3, 20), (1798, 3, 20),
        (1799, 3, 20), (1800, 3, 21), (1801, 3, 21), (1802, 3, 21),
        (1803, 3, 21), (1804, 3, 21), (1805, 3, 21), (1806, 3, 21),
        (1807, 3, 21), (1808, 3, 21), (1809, 3, 21), (1810, 3, 21),
        (1811, 3, 21), (1812, 3, 21), (1813, 3, 21), (1814, 3, 21),
        (1815, 3, 21), (1816, 3, 21), (1817, 3, 21), (1818, 3, 21),
        (1819, 3, 21), (1820, 3, 21), (1821, 3, 21), (1822, 3, 21),
        (1823, 3, 21), (1824, 3, 21), (1825, 3, 21), (1826, 3, 21),
        (1827, 3, 21), (1828, 3, 20), (1829, 3, 21), (1830, 3, 21),
        (1831, 3, 21), (1832, 3, 20), (1833, 3, 21), (1834, 3, 21),
        (1835, 3, 21), (1836, 3, 20), (1837, 3, 21), (1838, 3, 21),
        (1839, 3, 21), (1840, 3, 20), (1841, 3, 21), (1842, 3, 21),
        (1843, 3, 21),
        # Badi epoch are the first two below.
        (1844, 3, 20), (1844, 3, 20, 18, 16), (1845, 3, 21), (1846, 3, 21),
        (1847, 3, 21), (1848, 3, 20), (1849, 3, 21), (1850, 3, 21),
        (1851, 3, 21), (1852, 3, 20), (1853, 3, 21), (1854, 3, 21),
        (1855, 3, 21), (1856, 3, 20), (1857, 3, 21), (1858, 3, 21),
        (1859, 3, 21), (1860, 3, 20), (1861, 3, 20), (1862, 3, 21),
        (1863, 3, 21), (1864, 3, 20), (1865, 3, 20), (1866, 3, 21),
        (1867, 3, 21), (1868, 3, 20), (1869, 3, 20), (1870, 3, 21),
        (1871, 3, 21), (1872, 3, 20), (1873, 3, 20), (1874, 3, 21),
        (1875, 3, 21), (1876, 3, 20), (1877, 3, 20), (1878, 3, 21),
        (1879, 3, 21), (1880, 3, 20), (1881, 3, 20), (1882, 3, 21),
        (1883, 3, 21), (1884, 3, 20), (1885, 3, 20), (1886, 3, 21),
        (1887, 3, 21), (1888, 3, 20), (1889, 3, 20), (1890, 3, 21),
        (1891, 3, 21), (1892, 3, 20), (1893, 3, 20), (1894, 3, 21),
        (1895, 3, 21), (1896, 3, 20), (1897, 3, 20), (1898, 3, 20),
        (1899, 3, 21), (1900, 3, 21), (1901, 3, 21), (1902, 3, 21),
        (1903, 3, 22), (1904, 3, 21), (1905, 3, 21), (1906, 3, 21),
        (1907, 3, 22), (1908, 3, 21), (1909, 3, 21), (1910, 3, 21),
        (1911, 3, 22), (1912, 3, 21), (1913, 3, 21), (1914, 3, 21),
        (1915, 3, 22), (1916, 3, 21), (1917, 3, 21), (1918, 3, 21),
        (1919, 3, 22), (1920, 3, 21), (1921, 3, 21), (1922, 3, 21),
        (1923, 3, 22), (1924, 3, 21), (1925, 3, 21), (1926, 3, 21),
        (1927, 3, 22), (1928, 3, 21), (1929, 3, 21), (1930, 3, 21),
        (1931, 3, 21), (1932, 3, 21), (1933, 3, 21), (1934, 3, 21),
        (1935, 3, 21), (1936, 3, 21), (1937, 3, 21), (1938, 3, 21),
        (1939, 3, 21), (1940, 3, 21), (1941, 3, 21), (1942, 3, 21),
        (1943, 3, 21), (1944, 3, 21), (1945, 3, 21), (1946, 3, 21),
        (1947, 3, 21), (1948, 3, 21), (1949, 3, 21), (1950, 3, 21),
        (1951, 3, 21), (1952, 3, 21), (1953, 3, 21), (1954, 3, 21),
        (1955, 3, 21), (1956, 3, 21), (1957, 3, 21), (1958, 3, 21),
        (1959, 3, 21), (1960, 3, 20), (1961, 3, 21), (1962, 3, 21),
        (1963, 3, 21), (1964, 3, 21), (1965, 3, 21), (1966, 3, 21),
        (1967, 3, 21), (1968, 3, 20), (1969, 3, 21), (1970, 3, 21),
        (1971, 3, 21), (1972, 3, 20), (1973, 3, 21), (1974, 3, 21),
        (1975, 3, 21), (1976, 3, 20), (1977, 3, 21), (1978, 3, 21),
        (1979, 3, 21), (1980, 3, 20), (1981, 3, 21), (1982, 3, 21),
        (1983, 3, 21), (1984, 3, 20), (1985, 3, 21), (1986, 3, 21),
        (1987, 3, 21), (1988, 3, 20), (1989, 3, 21), (1990, 3, 21),
        (1991, 3, 21), (1992, 3, 20), (1993, 3, 21), (1994, 3, 21),
        (1995, 3, 21), (1996, 3, 20), (1997, 3, 20), (1998, 3, 21),
        (1999, 3, 21), (2000, 3, 20), (2001, 3, 20), (2002, 3, 21),
        (2003, 3, 21), (2004, 3, 20), (2005, 3, 20), (2006, 3, 21),
        (2007, 3, 21), (2008, 3, 20), (2009, 3, 20), (2010, 3, 21),
        (2011, 3, 21), (2012, 3, 20), (2013, 3, 20), (2014, 3, 21),
        # Start World Center dates at 2015
        (2015, 3, 21), (2016, 3, 20), (2017, 3, 20), (2018, 3, 21),
        (2019, 3, 21), (2020, 3, 20), (2021, 3, 20), (2022, 3, 21),
        (2023, 3, 21), (2024, 3, 20), (2025, 3, 20), (2026, 3, 21),
        (2027, 3, 21), (2028, 3, 20), (2029, 3, 20), (2030, 3, 20),
        (2031, 3, 21), (2032, 3, 20), (2033, 3, 20), (2034, 3, 20),
        (2035, 3, 21), (2036, 3, 20), (2037, 3, 20), (2038, 3, 20),
        (2039, 3, 21), (2040, 3, 20), (2041, 3, 20), (2042, 3, 20),
        (2043, 3, 21), (2044, 3, 20), (2045, 3, 20), (2046, 3, 20),
        (2047, 3, 21), (2048, 3, 20), (2049, 3, 20), (2050, 3, 20),
        (2051, 3, 21), (2052, 3, 20), (2053, 3, 20), (2054, 3, 20),
        (2055, 3, 21), (2056, 3, 20), (2057, 3, 20), (2058, 3, 20),
        (2059, 3, 20), (2060, 3, 20), (2061, 3, 20), (2062, 3, 20),
        #              End World Center dates at 2064
        (2063, 3, 20), (2064, 3, 20), (2065, 3, 20), (2066, 3, 20),
        (2067, 3, 20), (2068, 3, 20), (2069, 3, 20), (2070, 3, 20),
        (2071, 3, 20), (2072, 3, 20), (2073, 3, 20), (2074, 3, 20),
        (2075, 3, 20), (2076, 3, 20), (2077, 3, 20), (2078, 3, 20),
        (2079, 3, 20), (2080, 3, 20), (2081, 3, 20), (2082, 3, 20),
        (2083, 3, 20), (2084, 3, 20), (2085, 3, 20), (2086, 3, 20),
        (2087, 3, 20), (2088, 3, 20), (2089, 3, 20), (2090, 3, 20),
        (2091, 3, 20), (2092, 3, 20), (2093, 3, 20), (2094, 3, 20),
        (2095, 3, 20), (2096, 3, 19), (2097, 3, 20), (2098, 3, 20),
        (2099, 3, 20), (2100, 3, 20), (2101, 3, 21), (2102, 3, 21),
        (2103, 3, 21), (2104, 3, 20), (2105, 3, 21), (2106, 3, 21),
        (2107, 3, 21), (2108, 3, 20), (2109, 3, 21), (2110, 3, 21),
        (2111, 3, 21), (2112, 3, 20), (2113, 3, 21), (2114, 3, 21),
        (2115, 3, 21), (2116, 3, 20), (2117, 3, 21), (2118, 3, 21),
        (2119, 3, 21), (2120, 3, 20), (2121, 3, 21), (2122, 3, 21),
        (2123, 3, 21), (2124, 3, 20), (2125, 3, 21), (2126, 3, 21),
        (2127, 3, 21), (2128, 3, 20), (2129, 3, 21), (2130, 3, 21),
        (2131, 3, 21), (2132, 3, 20), (2133, 3, 20), (2134, 3, 21),
        (2135, 3, 21), (2136, 3, 20), (2137, 3, 20), (2138, 3, 21),
        (2139, 3, 21), (2140, 3, 20), (2141, 3, 20), (2142, 3, 21),
        (2143, 3, 21), (2144, 3, 20), (2145, 3, 20), (2146, 3, 21),
        (2147, 3, 21), (2148, 3, 20), (2149, 3, 20), (2150, 3, 21),
        (2151, 3, 21), (2152, 3, 20), (2153, 3, 20), (2154, 3, 21),
        (2155, 3, 21), (2156, 3, 20), (2157, 3, 20), (2158, 3, 21),
        (2159, 3, 21), (2160, 3, 20), (2161, 3, 20), (2162, 3, 20),
        (2163, 3, 21), (2164, 3, 20), (2165, 3, 20), (2166, 3, 20),
        (2167, 3, 21), (2168, 3, 20), (2169, 3, 20), (2170, 3, 20),
        (2171, 3, 21), (2172, 3, 20), (2173, 3, 20), (2174, 3, 20),
        (2175, 3, 21), (2176, 3, 20), (2177, 3, 20), (2178, 3, 20),
        (2179, 3, 21), (2180, 3, 20), (2181, 3, 20), (2182, 3, 20),
        (2183, 3, 21), (2184, 3, 20), (2185, 3, 20), (2186, 3, 20),
        (2187, 3, 21), (2188, 3, 20), (2189, 3, 20), (2190, 3, 20),
        (2191, 3, 21), (2192, 3, 20), (2193, 3, 20), (2194, 3, 20),
        (2195, 3, 20), (2196, 3, 20), (2197, 3, 20), (2198, 3, 20),
        (2199, 3, 20), (2200, 3, 21), (2201, 3, 21), (2202, 3, 21),
        (2203, 3, 21), (2204, 3, 21), (2205, 3, 21), (2206, 3, 21),
        (2207, 3, 21), (2208, 3, 21), (2209, 3, 21), (2210, 3, 21),
        (2211, 3, 21), (2212, 3, 21), (2213, 3, 21), (2214, 3, 21),
        (2215, 3, 21), (2216, 3, 21), (2217, 3, 21), (2218, 3, 21),
        (2219, 3, 21), (2220, 3, 21), (2221, 3, 21), (2222, 3, 21),
        (2223, 3, 21), (2224, 3, 21), (2225, 3, 21), (2226, 3, 21),
        (2227, 3, 21), (2228, 3, 21), (2229, 3, 21), (2230, 3, 21),
        (2231, 3, 21), (2232, 3, 20), (2233, 3, 21), (2234, 3, 21),
        (2235, 3, 21), (2236, 3, 20), (2237, 3, 21), (2238, 3, 21),
        (2239, 3, 21), (2240, 3, 20), (2241, 3, 21), (2242, 3, 21),
        (2243, 3, 21), (2244, 3, 20), (2245, 3, 21), (2246, 3, 21),
        (2247, 3, 21), (2248, 3, 20), (2249, 3, 21), (2250, 3, 21),
        (2251, 3, 21), (2252, 3, 20), (2253, 3, 21), (2254, 3, 21),
        (2255, 3, 21), (2256, 3, 20), (2257, 3, 21), (2258, 3, 21),
        (2259, 3, 21), (2260, 3, 20), (2261, 3, 20), (2262, 3, 21),
        (2263, 3, 21), (2264, 3, 20), (2265, 3, 20), (2266, 3, 21),
        (2267, 3, 21), (2268, 3, 20), (2269, 3, 20), (2270, 3, 21),
        (2271, 3, 21), (2272, 3, 20), (2273, 3, 20), (2274, 3, 21),
        (2275, 3, 21), (2276, 3, 20), (2277, 3, 20), (2278, 3, 21),
        (2279, 3, 21), (2280, 3, 20), (2281, 3, 20), (2282, 3, 21),
        (2283, 3, 21), (2284, 3, 20), (2285, 3, 20), (2286, 3, 21),
        (2287, 3, 21), (2288, 3, 20), (2289, 3, 20), (2290, 3, 21),
        (2291, 3, 21), (2292, 3, 20), (2293, 3, 20), (2294, 3, 20),
        (2295, 3, 21), (2296, 3, 20), (2297, 3, 20), (2298, 3, 20),
        (2299, 3, 21), (2300, 3, 21), (2301, 3, 21), (2302, 3, 21),
        (2303, 3, 22), (2304, 3, 21), (2305, 3, 21), (2306, 3, 21),
        (2307, 3, 22), (2308, 3, 21), (2309, 3, 21), (2310, 3, 21),
        (2311, 3, 22), (2312, 3, 21), (2313, 3, 21), (2314, 3, 21),
        (2315, 3, 22), (2316, 3, 21), (2317, 3, 21), (2318, 3, 21),
        (2319, 3, 22), (2320, 3, 21), (2321, 3, 21), (2322, 3, 21),
        (2323, 3, 22), (2324, 3, 21),

        (2334, 3, 21),
        (2344, 3, 21),
        (2354, 3, 21),
        (2364, 3, 20),
        (2374, 3, 21),
        (2384, 3, 20),

        (2394, 3, 21),
        (2395, 3, 21), (2396, 3, 20), (2397, 3, 20), (2398, 3, 21),
        (2399, 3, 21), (2400, 3, 20), (2401, 3, 20), (2402, 3, 21),
        (2403, 3, 21),

        (2404, 3, 20),
        (2414, 3, 20),
        (2424, 3, 20),
        (2434, 3, 20),
        (2444, 3, 20),
        (2454, 3, 20),
        (2464, 3, 20),
        (2474, 3, 20),
        (2484, 3, 20),

        (2494, 3, 20),
        (2495, 3, 20), (2496, 3, 19), (2497, 3, 20), (2498, 3, 20),
        (2499, 3, 20), (2500, 3, 20), (2501, 3, 21), (2502, 3, 21),
        (2503, 3, 21), (2504, 3, 20),

        (2514, 3, 21),
        (2524, 3, 20),
        (2534, 3, 21),
        (2544, 3, 20),
        (2554, 3, 21),
        (2564, 3, 20),
        (2574, 3, 20),
        (2584, 3, 20),

        (2594, 3, 20),
        (2595, 3, 20), (2596, 3, 20), (2597, 3, 20), (2598, 3, 20),
        (2599, 3, 20), (2600, 3, 21), (2601, 3, 21), (2602, 3, 21),
        (2603, 3, 21), (2604, 3, 21),

        (2614, 3, 21),
        (2624, 3, 21),
        (2634, 3, 21),
        (2644, 3, 20),
        (2654, 3, 21),
        (2664, 3, 20),
        (2674, 3, 21),
        (2684, 3, 20),

        (2694, 3, 20),
        (2695, 3, 21), (2696, 3, 20), (2697, 3, 20), (2698, 3, 20),
        (2699, 3, 21), (2700, 3, 21), (2701, 3, 21), (2702, 3, 21),
        (2703, 3, 22), (2704, 3, 21),

        (2714, 3, 21),
        (2724, 3, 21),
        (2734, 3, 21),
        (2744, 3, 21),
        (2754, 3, 21),
        (2764, 3, 20),
        (2774, 3, 21),
        (2784, 3, 20),

        (2794, 3, 21),
        (2795, 3, 21), (2796, 3, 20), (2797, 3, 20), (2798, 3, 21),
        (2799, 3, 21), (2800, 3, 20), (2801, 3, 20), (2802, 3, 21),
        (2803, 3, 21), (2804, 3, 20),

        (2814, 3, 21),
        (2824, 3, 20),
        (2834, 3, 20),
        (2844, 3, 20),
        (2854, 3, 20),
        (2864, 3, 20),
        (2874, 3, 20),
        (2884, 3, 20),

        (2894, 3, 20),
        (2895, 3, 20), (2896, 3, 19), (2897, 3, 20), (2898, 3, 20),
        (2899, 3, 20), (2900, 3, 20), (2901, 3, 21), (2902, 3, 21),
        (2903, 3, 21), (2904, 3, 20),

        (2914, 3, 21),
        (2924, 3, 20),
        (2934, 3, 21),
        (2944, 3, 20),
        (2954, 3, 21),
        (2964, 3, 20),
        (2974, 3, 20),
        (2984, 3, 20),

        (2994, 3, 20),
        (2995, 3, 20), (2996, 3, 20), (2997, 3, 20), (2998, 3, 20),
        (2999, 3, 20), (3000, 3, 21), (3001, 3, 21), (3002, 3, 21),
        (3003, 3, 21), (3004, 3, 21),
        )

    def __init__(self):
        self.gc = GregorianCalendar()

    def _calc_kvymd(self, days, k, v, y, m, data):
        year = (k - 1) * 361 + (v - 1) * 19 + y

        for d in reversed(range(1, days)):
            data.append(((k, v, y, m, d), (year, m, d)))

    def create_date_lists(self):
        data = []

        for k in reversed(range(self.START_K, self.END_K)):
            for v in reversed(range(1, 20)):
                for y in reversed(range(1, 20)):
                    for m in reversed(range(0, 20)):
                        if m == 0:
                            self._calc_kvymd(5, k, v, y, m, data)
                        else:
                            self._calc_kvymd(20, k, v, y, m, data)

        return data

    def check_long_date_from_short_date(self, data):
        items = []

        for item in data:
            b_date, date = item
            bd = self.long_date_from_short_date(date)

        if bd != (b_date + (0, 0, 0)):
            items.append((item, bd))

        return items

    def _create_gregorian_date_range(self, md=(3, 20)):
        return [(year,) + md for year in range(self.START_G, self.END_G, 10)]

    def _create_jd_for_gulian_date(self, data):
        return [self.gc.jd_from_gregorian_date(date) for date in data]

    def analize_date_error(self, options):
        #g_data = self._create_gregorian_date_range()
        #jds = self._create_jd_for_gulian_date(g_data)
        #z = zip(jds, g_data)
        #pprint.pprint([d for d in z])
        data = []
        diff = 0

        for g_date in self.TMP_ANS_DATES:
            b_date = (g_date[0] - self.TRAN_COFF, 1, 1) + (
                g_date[3:6] if len(g_date) > 3 else ())
            gjd = round(self.gc.jd_from_gregorian_date(g_date), 6)
            bjd, jey_y_m_o, coff, floor_jey = self._jd_from_badi_date(b_date,
                                                                      options)
            diff = bjd - gjd
            #f = abs(jey_y_m_o) % 1 if diff else 0
            data.append((b_date, bjd, g_date, gjd, jey_y_m_o,
                         floor_jey, jey_y_m_o+coff, diff))

        return data

    def _jd_from_badi_date(self, b_date, options):
        date = self.date_from_kvymdhms(
            self.long_date_from_short_date(b_date), short=True)
        year, month, day = date[:3]

        if month == 0: # Ayyam-i-Ha
            d = 18 * 19 + day
        elif month < 19:
            d = (month - 1) * 19 + day
        else: # month == 19:
            d = 18 * 19 + 4 + day

        # BADI_EPOCH = 2394645.5 # 2394646.257639
        badi_epoch_m_o = self.BADI_EPOCH - 1
        # Tropical Year: 365.242189 (self.MEAN_TROPICAL_YEAR)
        # Sidereal Year: 365.25636
        # Anomalistic Year: 365.25964
        jey_y_m_o = self.MEAN_TROPICAL_YEAR * (year - 1)
        coff = 0

        if not options.coff:
            coff = -0.0779

            if year == -246:
                coff -= 0.9
            elif year < -89:
                coff -= 0.093655
            elif year < -85:
                coff -= 0.1251
            elif year < 51:
                coff -= 0.0626024
            elif year < 121:
                coff -= 0.017
            elif year > 1031:
                coff += 0.25
            elif year > 702:
                coff += 0.19
            elif year == 571:
                coff -= 0.06
            elif year > 384:
                coff += 0.077325
            elif year > 281:
                coff += 0.06
            elif year == 216:
                coff -= 0.9921
            elif year >= 120:
                coff += 0.01523

        floor_jey = math.floor(jey_y_m_o + coff)
        return round(badi_epoch_m_o + floor_jey + d, 6
                     ), jey_y_m_o, coff, floor_jey


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description=("Test Badi date ranges."))
    parser.add_argument(
        '-l', '--list', action='store_true', default=False, dest='list',
        help="Generate a list of Badi dates both ling and short versions.")
    parser.add_argument(
        '-c', '--ck-dates', action='store_true', default=False, dest='ck_dates',
        help="Check that long_date_from_short_date() works correctly.")
    parser.add_argument(
        '-a', '--analize', action='store_true', default=False, dest='analize',
        help="Analize Badi date errors when converting to jd.")
    parser.add_argument(
        '-C', '--coff', action='store_true', default=False, dest='coff',
        help="Turn off all coefficents during an analysis.")
    options = parser.parse_args()
    exclusive_error = (options.list, options.ck_dates, options.analize)
    assert exclusive_error.count(True) <= 1, (
        "Options -l, -c, and -a are exclusive.")

    dt = DateTests()

    if options.list:
        data = dt.create_date_lists()
        pprint.pprint(data)

    if options.ck_dates:
        data = dt.create_date_lists()
        bad_items = dt.check_long_date_from_short_date(data)
        bad_items = bad_items if bad_items else "All dates match."
        pprint.pprint(bad_items)

    if options.analize:
        data = dt.analize_date_error(options)
        [print(item) for item in data]
        diffs = []

        for item in data:
            if item[-1] != 0.0:
                diffs.append(item[-1])

        print(len(data), len(diffs))

        if options.coff:
            coff = sum(diffs) / len(diffs)
            print(coff)

    sys.exit(0)
