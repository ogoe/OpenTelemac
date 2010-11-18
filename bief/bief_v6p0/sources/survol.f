C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE AREA (VOLUME) OF THE ELEMENTS OF A MESH.
!>  @code
!>  MEANING OF IELM:<br>
!>  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE<br>
!>  11 : TRIANGLE P1            3                       YES
!>  21 : QUADRILATERAL Q1       4                       YES
!>  41 : TELEMAC-3D PRISMS      6                       NO
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM, NELEM, NELMAX, SURFAC, XEL, YEL, ZEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SURVOL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), SURV11()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GEOELT()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 05/02/91
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td><--</td><td>SURFACE OU VOLUME DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS PAR ELEMENT.
!>    </td></tr>
!>          <tr><td>ZEL
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SURVOL
     &(SURFAC, XEL,YEL,ZEL,NELEM,NELMAX,IELM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |-->| TYPE D'ELEMENT
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SURFAC         |<--| SURFACE OU VOLUME DES ELEMENTS.
C| XEL,YEL,       |-->| COORDONNEES DES POINTS PAR ELEMENT.
C| ZEL            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SURVOL => SURVOL
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: IELM,NELEM,NELMAX
C
      DOUBLE PRECISION, INTENT(INOUT) :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN) :: YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN) :: ZEL(NELMAX,*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      IF(IELM.EQ.11) THEN
C
        CALL SURV11(SURFAC, XEL,YEL,NELEM,NELMAX)
C
C     ELSEIF(IELM.EQ.21) THEN
C
C       CALL SURV21(SURFAC, XEL,YEL,NELEM,NELMAX)
C
C     ELSEIF(IELM.EQ.41) THEN
C
C       CALL SURV41(SURFAC, XEL,YEL,ZEL,NELEM,NELMAX)
C
C  VALUE FOR IELM NOT PERMITTED : ERROR
C
      ELSE
         IF (LNG.EQ.1) WRITE(LU,100) IELM
         IF (LNG.EQ.2) WRITE(LU,101) IELM
100      FORMAT(1X,'SURVOL (BIEF) : IELM = ',1I6,' ELEMENT NON PREVU')
101      FORMAT(1X,
     &   'SURVOL (BIEF) : IELM = ',1I6,' ELEMENT NOT AVAILABLE')
         CALL PLANTE(1)
         STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C