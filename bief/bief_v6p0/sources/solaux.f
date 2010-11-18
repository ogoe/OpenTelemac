C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TB IS A BLOCK OF VECTORS AND TBB A BLOCK OF BLOCKS.
!>                SOLAUX PREPARES A TBB BLOCK BY FILLING IT IN WITH
!>                MAX(1,S) VECTORS FROM TB.
!><br>            THE OUTPUT ADDRESS IS AN ADDRESS RELATIVE TO TBB.
!><br>            IN PRACTICE TB OBJECTS ARE VECTORS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IPT, ITB, ITBB, S, TB, TBB
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IAD, K, MAXTB, MAXTBB
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SOLAUX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ADDBLO(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SOLVE()

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
!> </td><td> 01/02/95
!> </td><td> J.M. HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IPT
!></td><td><--</td><td>POINTEUR DE TBB VERS L'OBJET RENDU
!>    </td></tr>
!>          <tr><td>ITB
!></td><td>--></td><td>PREMIER VECTEUR LIBRE DE TB
!>    </td></tr>
!>          <tr><td>ITBB
!></td><td>--></td><td>PREMIER BLOC LIBRE DE TBB
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TB
!></td><td>--></td><td>BLOC DE TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TBB
!></td><td>--></td><td>BLOC DE BLOCS DE TRAVAIL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SOLAUX
     &(IPT, TB,TBB,ITB,ITBB,S)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IPT            |<--| POINTEUR DE TBB VERS L'OBJET RENDU
C| ITB            |-->| PREMIER VECTEUR LIBRE DE TB
C| ITBB           |-->| PREMIER BLOC LIBRE DE TBB
C| S             |---| 
C| TB             |-->| BLOC DE TABLEAUX DE TRAVAIL
C| TBB            |-->| BLOC DE BLOCS DE TRAVAIL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SOLAUX => SOLAUX
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: S
      INTEGER, INTENT(INOUT) :: ITB,IPT,ITBB
C
C-----------------------------------------------------------------------
C
C  STRUCTURES OF BLOCKS OF WORKING ARRAYS
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TB,TBB
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,IAD,MAXTB,MAXTBB
C
C-----------------------------------------------------------------------
C
      MAXTBB = TBB%N
      IF(ITBB.GT.MAXTBB) THEN
        IF(LNG.EQ.1) WRITE(LU,30) ITBB
        IF(LNG.EQ.2) WRITE(LU,31) ITBB
        CALL PLANTE(1)
        STOP
      ENDIF
      IPT = ITBB
      MAXTB = TB%N
      ITBB = ITBB + 1
C     REINITIALISES THE BLOCK
      TBB%ADR(IPT)%P%N=0
      DO 20 K = 1 , MAX(S,1)
        IF(ITB.GT.MAXTB) THEN
          IF(LNG.EQ.1) WRITE(LU,10) ITB + MAX(S,1) - K + 1
          IF(LNG.EQ.2) WRITE(LU,11) ITB + MAX(S,1) - K + 1
          CALL PLANTE(1)
          STOP
        ENDIF
        IAD=ITB
        CALL ADDBLO(TBB%ADR(IPT)%P,TB%ADR(IAD)%P)
        ITB = ITB + 1
20    CONTINUE
C
C-----------------------------------------------------------------------
C
10    FORMAT(1X,'SOLAUX (BIEF) : NOMBRE DE TABLEAUX DE TRAVAIL',/,
     &       1X,'INSUFFISANT DANS LE BLOC TB. MINIMUM NECESSAIRE :',1I4)
11    FORMAT(1X,'SOLAUX (BIEF): INSUFFICIENT NUMBER OF ARRAYS',/,
     &       1X,'IN BLOCK TB. MINIMUM REQUIRED:',1I4)
30    FORMAT(1X,'SOLAUX (BIEF) : NOMBRE DE BLOCS DE TRAVAIL',/,
     &       1X,'INSUFFISANT DANS LE BLOC TBB. MINIMUM NECESSAIRE:',1I4)
31    FORMAT(1X,'SOLAUX (BIEF): INSUFFICIENT NUMBER OF BLOCKS',/,
     &       1X,'IN BLOCK TBB. MINIMUM REQUIRED:',1I4)
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C