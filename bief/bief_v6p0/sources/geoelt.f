C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES DETERMINANTS AND SOME OTHER VALUES FOR
!>                ISOPARAMETRIC COORDINATES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM, NELEM, NELMAX, SURDET, SURFAC, XEL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET, IELEM, T12, T13, T22, T23, XSOM, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_GEOELT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), SURVOL()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 10/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
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
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td><--</td><td>INVERSE DU DETERMINANT DE LA TRANSFORMEE
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td><--</td><td>SURFACES DES ELEMENTS
!>    </td></tr>
!>          <tr><td>TAILLE
!></td><td><--</td><td>TAILLE DES ELEMENTS (LONGUEUR CARACTERISTIQUE)
!>    </td></tr>
!>          <tr><td>XEL,YEL
!></td><td>--></td><td>COORDONNEES DES NOEUDS PAR ELEMENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GEOELT
     &(SURDET,SURFAC,XEL,YEL,NELEM,NELMAX,IELM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |-->| TYPE D'ELEMENT
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |---| 
C| SURDET         |<--| INVERSE DU DETERMINANT DE LA TRANSFORMEE
C| SURFAC         |<--| SURFACES DES ELEMENTS
C| TAILLE         |<--| TAILLE DES ELEMENTS (LONGUEUR CARACTERISTIQUE)
C| XEL,YEL        |-->| COORDONNEES DES NOEUDS PAR ELEMENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_GEOELT => GEOELT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: IELM,NELEM,NELMAX
      DOUBLE PRECISION, INTENT(OUT) :: SURDET(NELEM),SURFAC(NELEM)
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*),YEL(NELMAX,*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
C
      DOUBLE PRECISION XSOM(4,2)
C
      DOUBLE PRECISION T12,T13,T22,T23,DET,Z(1)
C
C-----------------------------------------------------------------------
C
      CALL SURVOL(SURFAC, XEL,YEL,Z,NELEM,NELMAX,IELM)
C
      IF(IELM.EQ.11) THEN
C
         DO 400 IELEM = 1 , NELEM
C
         XSOM(1,1) = XEL(IELEM,1)
         XSOM(2,1) = XEL(IELEM,2)
         XSOM(3,1) = XEL(IELEM,3)
         XSOM(1,2) = YEL(IELEM,1)
         XSOM(2,2) = YEL(IELEM,2)
         XSOM(3,2) = YEL(IELEM,3)
C
         T12 = - XSOM(1,1) + XSOM(2,1)
         T13 = - XSOM(1,1) + XSOM(3,1)
         T22 = - XSOM(1,2) + XSOM(2,2)
         T23 = - XSOM(1,2) + XSOM(3,2)
C
         DET = T12*T23 - T22*T13
C
         IF(DET.LT.1.D-20) THEN
           IF(LNG.EQ.1) WRITE(LU,98) IELEM
           IF(LNG.EQ.2) WRITE(LU,99) IELEM
98         FORMAT(1X,'GEOELT: ELEMENT ',1I6,' : DETERMINANT NEGATIF')
99         FORMAT(1X,'GEOELT: ELEMENT ',1I6,' : NEGATIVE DETERMINANT')
           STOP
         ENDIF
C
         SURDET(IELEM) = 1.D0/DET
C
400      CONTINUE
C
      ELSE
            IF(LNG.EQ.1) WRITE(LU,10) IELM
            IF(LNG.EQ.2) WRITE(LU,11) IELM
10          FORMAT(1X,'GEOELT: TYPE D''ELEMENT INCONNU :',1I6)
11          FORMAT(1X,'GEOELT: UNKNOWN TYPE OF ELEMENT :',1I6)
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