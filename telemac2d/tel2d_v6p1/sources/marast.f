C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TIDAL FORCE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, COSLAT, FU1, FV1, GRAV, MARDAT, MARTIM, NPOIN, PHI0, SINLAT, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AHL, AHS, AL, ARL, ARS, AS, DAY, DL, DS, F0L, F0S, FXL, FXS, FYL, FYS, H2, HOUR, I, K2, LONG, LONG0, MIN, MLT, MONTH, MST, RT, SEC, TLOC, TLOC1, YEAR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MARAST
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ASTRO(), TSLOC()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PROSOU()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> 01/03/1994
!> </td><td> E. DAVID (LHF) 76 33 42 08; F LEPEINTRE (LNH) 30 87 78 54; J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>COSLAT
!></td><td>--></td><td>COS DE LA LATITUDE EN COORD SPHERIQUE
!>    </td></tr>
!>          <tr><td>FU1,FV1
!></td><td><--</td><td>FORCES GENERATRICES CALCULEES
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>PESANTEUR
!>    </td></tr>
!>          <tr><td>MARDAT
!></td><td>--></td><td>TABLEAU CONTENANT LES INFOS DE DATE
!>    </td></tr>
!>          <tr><td>MARTIM
!></td><td>--></td><td>TABLEAU CONTENANT LES INFOS DE TEMPS (TU)
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>PHI0
!></td><td>--></td><td>LONGITUDE DU POINT ORIGINE
!>    </td></tr>
!>          <tr><td>SINLAT
!></td><td>--></td><td>SIN DE LA LATITUDE EN COORD SPHERIQUE
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>ABSCISSES DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MARAST
     &(MARDAT,MARTIM,PHI0,NPOIN,AT,FU1,FV1,X,SINLAT,COSLAT,GRAV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| COSLAT         |-->| COS DE LA LATITUDE EN COORD SPHERIQUE
C| FU1,FV1        |<--| FORCES GENERATRICES CALCULEES
C| GRAV           |-->| PESANTEUR
C| MARDAT         |-->| TABLEAU CONTENANT LES INFOS DE DATE
C| MARTIM         |-->| TABLEAU CONTENANT LES INFOS DE TEMPS (TU)
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| PHI0           |-->| LONGITUDE DU POINT ORIGINE
C| SINLAT         |-->| SIN DE LA LATITUDE EN COORD SPHERIQUE
C| X             |-->| ABSCISSES DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_TELEMAC2D, EX_MARAST => MARAST
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: MARDAT(3),MARTIM(3),NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: FU1(NPOIN),FV1(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: COSLAT(NPOIN),SINLAT(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),GRAV,AT,PHI0
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER YEAR,MONTH,DAY,HOUR,MIN,SEC,I
C
      DOUBLE PRECISION ARL,ARS,DL,DS,AL,AS
      DOUBLE PRECISION RT,LONG,AHL,AHS,MLT,MST,LONG0
      DOUBLE PRECISION F0L,F0S,FXL,FYL,FXS,FYS
      DOUBLE PRECISION TLOC,TSLOC,TLOC1
      DOUBLE PRECISION K2,H2
C
      EXTERNAL TSLOC
C
      INTRINSIC ACOS,COS,SIN
C
C-----------------------------------------------------------------------
C
C     SPLITS ARRAYS MARDAT AND MARTIM
C
      YEAR  = MARDAT(1)
      MONTH = MARDAT(2)
      DAY   = MARDAT(3)
      HOUR  = MARTIM(1)
      MIN   = MARTIM(2)
      SEC   = MARTIM(3)
C
C REMINDER : HOUR IN UNIVERSAL TIME |
C GENERAL REMARK : T=TERRE, L=LUNE , S=SOLEIL
C
C LONG0: REFERENCE LONGITUDE IN RADIAN (0,2PI)
C
      LONG0=PHI0*ACOS(-1.D0)/180.D0
C
C CALLS THE MAIN FUNCTION COMPUTING THE LUNAR AND SOLAR ANGLES
C
      CALL ASTRO(YEAR,MONTH,DAY,HOUR,MIN,SEC,AT,ARL,ARS,DL,DS,AL,AS)
C
C RT: EARTH RADIUS IN M
C
      RT   = 6378000.D0
C
C MASS RATIO MOON/EARTH
C
      MLT  = 1.D0 / 81.53D0
C
C MASS RATIO SUN/EARTH
C
      MST  = 331954.D0
C
C AMPLITUDE OF THE FORCE INDUCED BY :
C
C     - THE MOON
C
      F0L  = GRAV * MLT * ARL**2
C
C     - THE SUN
C
      F0S  = GRAV * MST * ARS**2
C
C SIDEREAL TIME
C
      TLOC1 = TSLOC(YEAR,MONTH,DAY,HOUR,MIN,SEC,AT)
C
      DO 10 I=1,NPOIN
C
C LONGITUDE OF THE CONSIDERED NODE
C
        LONG = X(I)/RT+LONG0
C
C LOCAL SIDEREAL TIME
C
        TLOC = TLOC1 + LONG
C
C TIME ANGLE OF THE MOON
C
        AHL  = TLOC - AL
C
C TIME ANGLE OF THE SUN
C
        AHS  = TLOC - AS
C
C FORCE INDUCED BY THE ASTRONOMICAL POTENTIAL ONLY
C
C    FXL : FORCE ALONG X FOR THE MOON
C     Y  : ALONG Y
C     S  : SAME THING FOR THE SUN
C
        FXL  = F0L * COS(DL) * SIN(AHL) *
     &         ( ( 1.D0-2*ARL*(SINLAT(I)*SIN(DL)+
     &           COSLAT(I)*COS(DL)*COS(AHL))+ARL*ARL )**(-1.5D0) -1.D0 )
C
        FXS  = F0S * COS(DS) * SIN(AHS) *
     &         ( ( 1.D0-2*ARS*(SINLAT(I)*SIN(DS)+
     &           COSLAT(I)*COS(DS)*COS(AHS))+ARS*ARS )**(-1.5D0) -1.D0 )
C
        FYL  = F0L*(COSLAT(I)*SIN(DL)-SINLAT(I)*COS(DL)*COS(AHL))*
     &         ( ( 1.D0-2*ARL*(SINLAT(I)*SIN(DL)+
     &           COSLAT(I)*COS(DL)*COS(AHL))+ARL*ARL )**(-1.5D0) -1.D0 )
C
        FYS  = F0S*( COSLAT(I)*SIN(DS)-SINLAT(I)*COS(DS)*COS(AHS))*
     &         ( ( 1.D0-2*ARS*(SINLAT(I)*SIN(DS)+
     &           COSLAT(I)*COS(DS)*COS(AHS))+ARS*ARS )**(-1.5D0) -1.D0 )

C
C TAKES INTO ACCOUNT :
C
C    - THE TERRESTRIAL TIDE (LOVE NUMBER H2)
C
        H2=0.61D0
C
C    - THE STATIC PERTUBATIONS (LOVE NUMBER K2)
C
        K2=0.30D0
C
C MISSES :
C
C    - DYNAMIC PERTUBATION OF AUTO-ATTRACTION
C    - DYNAMIC PERTUBATION OF THE EFFECTS OF LOADS
C
C FINAL FORCE
C
        FU1(I)=FU1(I)+(1.D0+K2-H2)*(FXL+FXS)
        FV1(I)=FV1(I)+(1.D0+K2-H2)*(FYL+FYS)
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C