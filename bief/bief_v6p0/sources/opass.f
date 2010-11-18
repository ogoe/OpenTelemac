C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FRONTAL MATRIX-VECTOR PRODUCT FOR ELEMENT 11-11.
!><br>            OMITS THE DIAGONAL TERMS HERE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IW, IY, LIMVOI, MXPTVS, NPMAX, OP, W, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MW0303()

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
!>          <tr><td>IKLE
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATION LOCALE-GLOBALE
!>    </td></tr>
!>          <tr><td>IW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMVOI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MXPTVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>PREMIERE DIMENSION DE IKLE ET W.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>--></td><td>TABLEAUX DE TRAVAIL CONTENANT LE VECTEUR SOUS
!>                  FORME NON ASSEMBLEE
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>VECTEUR ASSEMBLE
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE OPASS
     &(OP,X,W,IW,Y,IY,LIMVOI,MXPTVS,NPMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| CORRESPONDANCE NUMEROTATION LOCALE-GLOBALE
C| IW             |---| 
C| IY             |---| 
C| LIMVOI         |---| 
C| MXPTVS         |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPMAX          |---| 
C| OP             |---| 
C| W             |-->| TABLEAUX DE TRAVAIL CONTENANT LE VECTEUR SOUS
C|                |   | FORME NON ASSEMBLEE
C| X             |<->| VECTEUR ASSEMBLE
C| Y             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPMAX,MXPTVS
      INTEGER, INTENT(IN) :: IW(NPMAX,*),IY(NPMAX,*),LIMVOI(MXPTVS,2)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN)    :: W(*),Y(*)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
C-----------------------------------------------------------------------
C
      IF(MXPTVS.GT.11) THEN
       IF(LNG.EQ.1) WRITE(LU,777)
       IF(LNG.EQ.2) WRITE(LU,778)
777    FORMAT(1X,'OPASS (BIEF) : PROGRAMME JUSQU''A 11 VOISINS',/,
     &        1X,'CHOISIR STOCKAGE DES MATRICES : 1')
778    FORMAT(1X,'OPASS (BIEF): IMPLEMENTED UP TO 11 NEIGHBOURS ONLY',/,
     &        1X,'CHOOSE STORAGE OF MATRICES : 1')
       CALL PLANTE(1)
       STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(OP(1:8).EQ.'X=WY    ') THEN
C
      IF(LIMVOI(1,1).GT.0) THEN
C       THIS CASE IS NOT POSSIBLE IN THEORY
        DO I = LIMVOI(1,1) , LIMVOI(1,2)
           X(I) = W(IW(I,1))*Y(IY(I,1))
        ENDDO
      ENDIF
C
      IF(LIMVOI(2,1).GT.0) THEN
C       THIS CASE ONLY EXISTS IF THERE ARE OVERSTRESSED TRIANGLES
        DO I = LIMVOI(2,1) , LIMVOI(2,2)
           X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
        ENDDO
      ENDIF
C
      IF(MXPTVS.GE.3.AND.LIMVOI(3,1).GT.0) THEN
      DO I = LIMVOI(3,1) , LIMVOI(3,2)
         X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
     &        + W(IW(I,3))*Y(IY(I,3))
      ENDDO
      ENDIF
C
      IF(MXPTVS.GE.4.AND.LIMVOI(4,1).GT.0) THEN
      DO I = LIMVOI(4,1) , LIMVOI(4,2)
         X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
     &        + W(IW(I,3))*Y(IY(I,3)) + W(IW(I,4))*Y(IY(I,4))
      ENDDO
      ENDIF
C
      IF(MXPTVS.GE.5.AND.LIMVOI(5,1).GT.0) THEN
      DO I = LIMVOI(5,1) , LIMVOI(5,2)
         X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
     &        + W(IW(I,3))*Y(IY(I,3)) + W(IW(I,4))*Y(IY(I,4))
     &        + W(IW(I,5))*Y(IY(I,5))
      ENDDO
      ENDIF
C
      IF(MXPTVS.GE.6.AND.LIMVOI(6,1).GT.0) THEN
      DO I = LIMVOI(6,1) , LIMVOI(6,2)
         X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
     &        + W(IW(I,3))*Y(IY(I,3)) + W(IW(I,4))*Y(IY(I,4))
     &        + W(IW(I,5))*Y(IY(I,5)) + W(IW(I,6))*Y(IY(I,6))
      ENDDO
      ENDIF
C
      IF(MXPTVS.GE.7.AND.LIMVOI(7,1).GT.0) THEN
      DO I = LIMVOI(7,1) , LIMVOI(7,2)
         X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
     &        + W(IW(I,3))*Y(IY(I,3)) + W(IW(I,4))*Y(IY(I,4))
     &        + W(IW(I,5))*Y(IY(I,5)) + W(IW(I,6))*Y(IY(I,6))
     &        + W(IW(I,7))*Y(IY(I,7))
      ENDDO
      ENDIF
C
      IF(MXPTVS.GE.8.AND.LIMVOI(8,1).GT.0) THEN
      DO I = LIMVOI(8,1) , LIMVOI(8,2)
         X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
     &        + W(IW(I,3))*Y(IY(I,3)) + W(IW(I,4))*Y(IY(I,4))
     &        + W(IW(I,5))*Y(IY(I,5)) + W(IW(I,6))*Y(IY(I,6))
     &        + W(IW(I,7))*Y(IY(I,7)) + W(IW(I,8))*Y(IY(I,8))
      ENDDO
      ENDIF
C
      IF(MXPTVS.GE.9.AND.LIMVOI(9,1).GT.0) THEN
      DO I = LIMVOI(9,1) , LIMVOI(9,2)
         X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
     &        + W(IW(I,3))*Y(IY(I,3)) + W(IW(I,4))*Y(IY(I,4))
     &        + W(IW(I,5))*Y(IY(I,5)) + W(IW(I,6))*Y(IY(I,6))
     &        + W(IW(I,7))*Y(IY(I,7)) + W(IW(I,8))*Y(IY(I,8))
     &        + W(IW(I,9))*Y(IY(I,9))
      ENDDO
      ENDIF
C
      IF(MXPTVS.GE.10.AND.LIMVOI(10,1).GT.0) THEN
      DO I = LIMVOI(10,1) , LIMVOI(10,2)
         X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
     &        + W(IW(I,3))*Y(IY(I,3)) + W(IW(I,4))*Y(IY(I,4))
     &        + W(IW(I,5))*Y(IY(I,5)) + W(IW(I,6))*Y(IY(I,6))
     &        + W(IW(I,7))*Y(IY(I,7)) + W(IW(I,8))*Y(IY(I,8))
     &        + W(IW(I,9))*Y(IY(I,9)) + W(IW(I,10))*Y(IY(I,10))
      ENDDO
      ENDIF
C
      IF(MXPTVS.GE.11.AND.LIMVOI(11,1).GT.0) THEN
      DO I = LIMVOI(11,1) , LIMVOI(11,2)
         X(I) = W(IW(I,1))*Y(IY(I,1)) + W(IW(I,2))*Y(IY(I,2))
     &        + W(IW(I,3))*Y(IY(I,3)) + W(IW(I,4))*Y(IY(I,4))
     &        + W(IW(I,5))*Y(IY(I,5)) + W(IW(I,6))*Y(IY(I,6))
     &        + W(IW(I,7))*Y(IY(I,7)) + W(IW(I,8))*Y(IY(I,8))
     &        + W(IW(I,9))*Y(IY(I,9)) + W(IW(I,10))*Y(IY(I,10))
     &        + W(IW(I,11))*Y(IY(I,11))
      ENDDO
      ENDIF
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,3000) OP
        IF (LNG.EQ.2) WRITE(LU,3001) OP
3000    FORMAT(1X,'OPASS (BIEF) : OPERATION INCONNUE : ',A8)
3001    FORMAT(1X,'OPASS (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C