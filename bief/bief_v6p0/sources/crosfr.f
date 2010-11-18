C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WANT TO INTERPOLATE THE BOTTOM ELEVATION FOR A POINT
!>                WITH COORDINATES X AND Y. A POINT (XR,YR) IS USED
!>                IN THIS INTERPOLATION.
!><br>            CHECKS HERE THAT THIS POINT IS NOT OUTSIDE OF THE
!>                DOMAIN, I.E. CHECKS THAT THE SEGMENT LINKING (X,Y)
!>                AND (XR,YR) DOES NOT INTERSECT WITH THE DOMAIN
!>                BOUNDARY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  JMH : DOES NOT WORK IN PARALLEL MODE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DM, KP1BOR, NBOR, NPMAX, NPTFR, OK, X, XMAIL, XR, Y, YMAIL, YR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALFA, BETA, DET, DISTA2, DISTB2, DM2, EPS, KA, XA, XB, YA, YB
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FASP()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 20/03/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DM
!></td><td>--></td><td>DISTANCE MINIMALE A LA FRONTIERE
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION DES ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>--></td><td>NOMBRE MAX DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTRF
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>OK
!></td><td>---</td><td>
!>                  .FALSE: SINON
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU POINT OU L'ON VEUT INTERPOLER
!>    </td></tr>
!>          <tr><td>XMAIL,YMAIL
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XR,YR
!></td><td>--></td><td>COORDONNEES DU POINT SITUE DANS LE CADRAN R
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CROSFR
     &(X,Y,XR,YR,XMAIL,YMAIL,NPMAX,NBOR,KP1BOR,NPTFR,DM,OK)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DM             |-->| DISTANCE MINIMALE A LA FRONTIERE
C| KP1BOR         |---| 
C| NBOR           |-->| NUMEROTATION DES ELEMENTS DE BORD
C| NPMAX          |-->| NOMBRE MAX DE POINTS DU MAILLAGE
C| NPTFR          |---| 
C| NPTRF          |-->| NOMBRE DE POINTS FRONTIERE
C| OK             |---| 
C|                |   | .FALSE: SINON
C| X,Y            |-->| COORDONNEES DU POINT OU L'ON VEUT INTERPOLER
C| XMAIL,YMAIL    |-->| COORDONNEES DES POINTS DU MAILLAGE
C| XR,YR          |-->| COORDONNEES DU POINT SITUE DANS LE CADRAN R
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN) :: X,Y,XR,YR,DM
      INTEGER, INTENT(IN)          :: NPTFR,NPMAX
      DOUBLE PRECISION, INTENT(IN) :: XMAIL(NPMAX),YMAIL(NPMAX)
      INTEGER, INTENT(IN)          :: NBOR(NPTFR),KP1BOR(NPTFR)
      LOGICAL, INTENT(INOUT)       :: OK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER KA
C
      DOUBLE PRECISION DM2,XA,YA,XB,YB,DET,ALFA,BETA,EPS,DISTA2,DISTB2
C
C-----------------------------------------------------------------------
C
C     DOES NOT CONSIDER POINTS TOO CLOSE TO THE BOUNDARY
C     DM     : MINIMUM DISTANCE
      DM2 = DM**2
C
      DO 10 KA=1,NPTFR
C
C INTERSECTION OF A BOUNDARY SEGMENT AND THE SEGMENT
C FORMED BY THE POINTS (X,Y) AND (XR,YR)
C
        XA = XMAIL(NBOR(KA))
        YA = YMAIL(NBOR(KA))
        XB = XMAIL(NBOR(KP1BOR(KA)))
        YB = YMAIL(NBOR(KP1BOR(KA)))
C
        DET = (XR-X)*(YA-YB) - (YR-Y)*(XA-XB)
C
        IF(ABS(DET).LT.1.D-6) GO TO 10
C
        ALFA = ( (XA-X)*(YA-YB) - (YA-Y)*(XA-XB) ) / DET
        BETA = ( (XR-X)*(YA-Y ) - (YR-Y)*(XA-X ) ) / DET
C
        EPS=0.05D0
        IF(ALFA.GE.EPS.AND.ALFA.LE.1.D0-EPS.AND.
     &     BETA.GE.EPS.AND.BETA.LE.1.D0-EPS) THEN
          OK = .FALSE.
          GO TO 1000
        ENDIF
C
C ALSO ELIMINATES THE POINTS TOO CLOSE TO THE BOUNDARY
C
        DISTA2 = (XR-XA)**2 + (YR-YA)**2
        DISTB2 = (XR-XB)**2 + (YR-YB)**2
        IF(DISTA2.LT.DM2.OR.DISTB2.LT.DM2) THEN
          OK = .FALSE.
          GO TO 1000
        ENDIF
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
1000  RETURN
      END
C
C#######################################################################
C