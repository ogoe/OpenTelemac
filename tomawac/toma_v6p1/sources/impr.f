C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES OUT TO THE LISTING.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, ICOD, ISITS, LISPRD, LT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IMP, LTT, TEXIMP
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 08/06/2010
!> </td><td> JMH
!> </td><td> PRINT REPLACED BY WRITE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 01/02/95
!> </td><td> F.MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS DU CALCUL
!>    </td></tr>
!>          <tr><td>ICOD
!></td><td>--></td><td>CODE POUR LES SORTIES
!>    </td></tr>
!>          <tr><td>ISITS
!></td><td>--></td><td>NOMBRE DE SOUS ITERATION DES TERMES SOURCE
!>    </td></tr>
!>          <tr><td>LISPRD
!></td><td>--></td><td>PERIODE DE SORTIE LISTING
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE IMPR
     &(LISPRD,LT,AT,ISITS,ICOD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS DU CALCUL
C| ICOD           |-->| CODE POUR LES SORTIES
C| ISITS          |-->| NOMBRE DE SOUS ITERATION DES TERMES SOURCE
C| LISPRD         |-->| PERIODE DE SORTIE LISTING
C| LT             |-->| NUMERO DU PAS DE TEMPS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER LT,ICOD,LISPRD,LTT,ISITS
C
      DOUBLE PRECISION AT
C
      LOGICAL IMP
      CHARACTER*45 TEXIMP(9)
C
      DATA TEXIMP / 'CALCUL DU CHAMP CONVECTEUR ET REMONTEE DES  ' ,
     &              '    CARACTERISTIQUES                        ' ,
     &              'SAUVEGARDE DE L''ETAT FINAL A T=            ' ,
     &              'INTERPOLATION AUX PIEDS DES CARACTERISTIQUES' ,
     &              'TEMPS :                                     ' ,
     &              ' SECONDES                                   ' ,
     &              'IEME  ITERATION                             ' ,
     &              ' SOUS-ITERATION(S)                          ' ,
     &              'PRISE EN COMPTE DES TERMES SOURCES EN       ' /
C
C-----------------------------------------------------------------------
C
      IMP=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF(LT.EQ.LTT) IMP=.TRUE.
C
      IF (.NOT.IMP) RETURN
C
      IF (ICOD.EQ.1) THEN
        WRITE(LU,101) TEXIMP(1)
      ENDIF
C
      IF (ICOD.EQ.2) THEN
        WRITE(LU,102) TEXIMP(2)
      ENDIF
C
      IF (ICOD.EQ.3) THEN
        WRITE(LU,103) TEXIMP(5),AT,TEXIMP(6),LT,TEXIMP(7)
      ENDIF
C
      IF (ICOD.EQ.4) THEN
        WRITE(LU,104) TEXIMP(9),ISITS,TEXIMP(8)
      ENDIF
C
      IF (ICOD.EQ.5) THEN
        WRITE(LU,105) TEXIMP(4)
      ENDIF
C
      IF (ICOD.EQ.6) THEN
        WRITE(LU,106) TEXIMP(3),AT,TEXIMP(6)
      ENDIF
C
C-----------------------------------------------------------------------
C
101   FORMAT(80('-'),/,7X,A44)
102   FORMAT(7X,A44)
103   FORMAT(/,80('='),/,7X,A8,F12.4,A10,23X,I5,A17,/,80('-'))
104   FORMAT(7X,A37,I3,A18)
105   FORMAT(7X,A44,/,80('-'))
106   FORMAT(80('-'),/,7X,A32,F12.4,A10,/,/,80('='))
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C