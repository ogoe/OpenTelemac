#!/usr/bin/env python
"""@author Tobias Bieniek
      Tobias.Bieniek@gmx.de
      https://github.com/Turbo87/utm
"""
"""@note ... this work has been copied from Tobias's code and
      redesigned based on a collaborative effort between
  .________.                                                          ,--.
  |        |                                                      .  (  (
  |,-.    /   HR Wallingford                EDF - LNHE           / \_ \_/ .--.
  /   \  /    Howbery Park,                 6, quai Watier       \   )   /_   )
   ,.  `'     Wallingford, Oxfordshire      78401 Cedex           `-'_  __ `--
  /  \   /    OX10 8BA, United Kingdom      Chatou, France        __/ \ \ `.
 /    `-'|    www.hrwallingford.com         innovation.edf.com   |    )  )  )
!________!                                                        `--'   `--
"""
"""@brief
      Bidirectional UTM-WGS84 converter for python
"""
"""@details
> Ellipsoid name,    Equatorial Radius,  square of eccentricity
	"Airy",                    R =  6377563, E = 0.00667054,
	"Australian National",     R =  6378160, E = 0.006694542,
	"Bessel 1841",             R =  6377397, E = 0.006674372,
	"Bessel 1841 (Nambia] ",   R =  6377484, E = 0.006674372,
	"Clarke 1866",             R =  6378206, E = 0.006768658,
	"Clarke 1880",             R =  6378249, E = 0.006803511,
	"Everest",                 R =  6377276, E = 0.006637847,
	"Fischer 1960 (Mercury] ", R =  6378166, E = 0.006693422,
	"Fischer 1968",            R =  6378150, E = 0.006693422,
	"GRS 1967",                R =  6378160, E = 0.006694605,
	"GRS 1980",                R =  6378137, E = 0.00669438,
	"Helmert 1906",            R =  6378200, E = 0.006693422,
	"Hough",                   R =  6378270, E = 0.00672267,
	"International",           R =  6378388, E = 0.00672267,
	"Krassovsky",              R =  6378245, E = 0.006693422,
	"Modified Airy",           R =  6377340, E = 0.00667054,
	"Modified Everest",        R =  6377304, E = 0.006637847,
	"Modified Fischer 1960",   R =  6378155, E = 0.006693422,
	"South American 1969",     R =  6378160, E = 0.006694542,
	"WGS 60",                  R =  6378165, E = 0.006693422,
	"WGS 66",                  R =  6378145, E = 0.006694542,
	"WGS-72",                  R =  6378135, E = 0.006694318,
	"WGS-84",                  R =  6378137, E = 0.00669438
"""
"""@history 27/11/2013 -- Sebastien E. Bourban:
      Re-interpretation of Tobias's code for the purpose of the
         TELEMAC-MASCARET system -- Thank you Tobias !
      Extension to numpy arrays
      Removal of the requirement for a Zone Letter
"""

# _____          ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import sys
import numpy as np
import math

# _____                 ____________________________________________
# ____/ Primary Access /___________________________________________/
#
__all__ = ['toLatLong', 'fromLatLong']

# _____                   __________________________________________
# ____/ Global Variables /_________________________________________/
#

K0 = 0.9996
E = 0.00669438   # "WGS-84"
R = 6378137      # "WGS-84"

E2 = E * E
E3 = E2 * E
E_P2 = E / (1.0 - E)

SQRT_E = math.sqrt(1 - E)
_E = (1 - SQRT_E) / (1 + SQRT_E)
_E3 = _E * _E * _E
_E4 = _E3 * _E

M1 = (1 - E / 4 - 3 * E2 / 64 - 5 * E3 / 256)
M2 = (3 * E / 8 + 3 * E2 / 32 + 45 * E3 / 1024)
M3 = (15 * E2 / 256 + 45 * E3 / 1024)
M4 = (35 * E3 / 3072)

P2 = (3 * _E / 2 - 27 * _E3 / 32)
P3 = (21 * _E3 / 16 - 55 * _E4 / 32)
P4 = (151 * _E3 / 96)

# _____                     ________________________________________
# ____/ Conversion LatLong /_______________________________________/
#

def toLatLong( easting,northing, zone ):

   # ~~> easting
   easting = easting - 500000
   mineast = np.min(easting)
   maxeast = np.max(easting)
   if mineast < -1000000 or maxeast > 1000000:
      print '... Easting out of range (must be between 100 km and 1000 km)'
      sys.exit(1)

   maxnord = np.max(northing)
   if maxnord < 0: northing = northing - 10000000   # South and North
   minnord = np.min(northing)
   maxnord = np.max(northing)
   if minnord < 0 or maxnord > 10000000:
      print '... Northing out of range (must be between 0 m and 10.000.000 m)'
      sys.exit(1)

   # ~~> zoning
   if zone < 1 or zone > 60:
      print '... Zone number out of range (must be between 1 and 60)'
      sys.exit(1)


   #if zone_letter < 'N': northing -= 10000000

   m = northing / K0
   mu = m / ( R*M1 )

   p_rad = ( mu + P2*np.sin( 2*mu ) + P3*np.sin( 4*mu ) + P4*np.sin( 6*mu) )

   p_sin = np.sin( p_rad )
   p_sin2 = np.power( p_sin,2 )

   p_cos = np.cos( p_rad )

   p_tan = np.divide( p_sin,p_cos )
   p_tan2 = np.power( p_tan,2 )
   p_tan4 = np.power( p_tan,4 )

   ep_sin = 1 - E * p_sin2
   ep_sin_sqrt = np.power( ( 1 - E * p_sin2 ),-0.5 )

   n = np.power( ( R * ep_sin_sqrt * K0 ) ,-1 )
   r = ( ep_sin )/( 1-E )

   c = _E * np.power( p_cos,2 )
   c2 = np.power( c,2 )

   d = np.multiply( easting,n )
   d2 = np.power( d,2 )
   d3 = np.power( d,3 )
   d4 = np.power( d,4 )
   d5 = np.power( d,5 )
   d6 = np.power( d,6 )

   latitude = ( p_rad - np.multiply( np.multiply( p_tan,r ), \
      ( d2/2 - d4/24 * ( 5+3*p_tan2+10*c-4*c2-9*E_P2 ) ) ) + \
      d6/720 * ( 61+90*p_tan2+298*c+45*p_tan4-252*E_P2-3*c2 ) )
   latitude = np.degrees(latitude)

   longitude = np.divide( ( d - np.multiply( d3, ( 1+2*p_tan2+c ) )/6 + \
      np.multiply( d5,( 5-2*c+28*p_tan2-3*c2+8*E_P2+24*p_tan4 ) )/120 ), p_cos )
   longitude = np.degrees(longitude) + ( (zone-1)*6 - 180 + 3 )

   return longitude,latitude


def fromLatLong(longitude,latitude,zone):

   # ~~> latitude
   minlat = np.min(latitude)
   maxlat = np.max(latitude)
   if minlat < -84 or maxlat > 84:
      print '... Latitude out of range (must be between 84 deg S and 84 deg N)'
      sys.exit(1)
   lat_rad = np.radians(latitude)
   lat_sin = np.sin(lat_rad)
   lat_cos = np.cos(lat_rad)
   lat_tan = np.divide( lat_sin,lat_cos )
   lat_tan2 = np.power( lat_tan,2 )
   lat_tan4 = np.power( lat_tan,4 )

   # ~~> longitude
   minlon = np.min(longitude)
   maxlon = np.max(longitude)
   if minlon < -180 or maxlon > 180:
      print '... Longitude out of range (must be between 180 deg W and 180 deg E)'
      sys.exit(1)
   lon_rad = np.radians(longitude)

   # ~~> zone number for the mid point

   midlat = ( maxlat+minlat) / 2.0
   if zone == 0:
      midlon = ( maxlon+minlon) / 2.0
      if 56 <= midlat <= 64 and 3 <= midlon <= 12: zone = 32
      elif 72 <= midlat <= 84 and midlon >= 0:
         if midlon <= 9: zone = 31
         elif midlon <= 21: zone = 33
         elif midlon <= 33: zone = 35
         elif midlon <= 42: zone = 37
      else: zone = int( ( midlon+180 ) / 6 ) + 1

   # ~~> central longitude
   centre = ( zone - 1 ) * 6 - 180 + 3
   centre = math.radians(centre)

   n = R * np.power( ( 1 - E * np.power( lat_sin,2 ) ), -0.5 )
   c = E_P2 * np.power( lat_cos,2 )

   a = np.multiply( lat_cos,( lon_rad-centre ) )
   a2 = np.power( a,2 )
   a3 = np.power( a,3 )
   a4 = np.power( a,4 )
   a5 = np.power( a,5 )
   a6 = np.power( a,6 )

   m = R*( M1*lat_rad - M2*np.sin( 2*lat_rad ) + M3*np.sin( 4*lat_rad ) - M4*np.sin( 6*lat_rad ) )

   easting = 500000 + K0 * \
      np.multiply( n, ( a + a3/6 * ( 1-lat_tan2+c ) + a5/120 * ( 5-18*lat_tan2+lat_tan4+72*c-58*E_P2 ) ) )

   northing = K0 * ( m + np.multiply( np.multiply( n, lat_tan ), ( \
      a2/2 + a4/24 * ( 5-lat_tan2+9*c+4*np.power(c,2) ) + a6/720*( 61-58*lat_tan2+lat_tan4+600*c-330*E_P2 ) ) ) )

   if midlat < 0: northing = northing + 10000000

   return easting, northing, zone

