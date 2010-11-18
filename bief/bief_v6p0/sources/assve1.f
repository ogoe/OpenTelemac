C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ASSEMBLY LOOP FOR A VECTOR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, LV, MASKEL, MSK, NELEM, NELMAX, W, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IB, IELEM
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ASSVEC()

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
!> </td><td> 18/08/94
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
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>PREMIERE DIMENSION DE IKLE ET W.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NNNNN
!></td><td>--></td><td>ANCIENNE DIMENSION DU TABLEAU X
!>                  (NE SERT PLUS, CONSERVE POUR COMPATIBILITE
!>                  AVEC LES ANCIENNES VERSIONS).
!>    </td></tr>
!>          <tr><td>W
!></td><td>--></td><td>TABLEAUX DE TRAVAIL CONTENANT LE VECTEUR SOUS
!>                  FORME NON ASSEMBLEE
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>VECTEUR ASSEMBLE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ASSVE1
     &(X, IKLE,W, NELEM,NELMAX,LV,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| CORRESPONDANCE NUMEROTATION LOCALE-GLOBALE
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NNNNN          |-->| ANCIENNE DIMENSION DU TABLEAU X
C|                |   | (NE SERT PLUS, CONSERVE POUR COMPATIBILITE
C|                |   | AVEC LES ANCIENNES VERSIONS).
C| W             |-->| TABLEAUX DE TRAVAIL CONTENANT LE VECTEUR SOUS
C|                |   | FORME NON ASSEMBLEE
C| X             |<->| VECTEUR ASSEMBLE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,LV
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX),MASKEL(NELMAX)
      LOGICAL         , INTENT(IN)    :: MSK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IB
C
      INTRINSIC MIN
C
C-----------------------------------------------------------------------
C LOOP IN SCALAR MODE (LV=1) OR WITH FORCED VECTORISATION
C-----------------------------------------------------------------------
C
C  WITH MASKING
C
      IF(MSK) THEN
C
      IF(LV.EQ.1) THEN
C
C  SCALAR MODE
C
      DO 10 IELEM = 1 , NELEM
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM) * MASKEL(IELEM)
10    CONTINUE
C
      ELSE
C
C  VECTOR MODE
C
      DO 20 IB = 1,(NELEM+LV-1)/LV
CVOCL LOOP,NOVREC
CDIR$ IVDEP
      DO 30 IELEM = 1+(IB-1)*LV , MIN(NELEM,IB*LV)
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM) * MASKEL(IELEM)
30    CONTINUE
20    CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  WITHOUT MASKING
C
      ELSE
C
      IF(LV.EQ.1) THEN
C
C  SCALAR MODE
C
      DO 40 IELEM = 1 , NELEM
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM)
40    CONTINUE
C
      ELSE
C
C  VECTOR MODE
C
      DO 60 IB = 1,(NELEM+LV-1)/LV
CVOCL LOOP,NOVREC
CDIR$ IVDEP
      DO 50 IELEM = 1+(IB-1)*LV , MIN(NELEM,IB*LV)
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM)
50    CONTINUE
60    CONTINUE
C
      ENDIF
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