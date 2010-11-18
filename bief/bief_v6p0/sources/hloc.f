C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE LOCAL SPACE STEP [ |CI|/SUM(LIJ) ].

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRS, DTHAUT, NBOR, NPOIN, NPTFR, NSEG, NUBO, VNOCL, XNEBOR, YNEBOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, K, NSG, NUBO1, NUBO2, VNL, VNX, VNY
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
!>      <td><center> 5.4                                       </center>
!> </td><td>
!> </td><td> INRIA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AIRS
!></td><td>--></td><td>AIRES DES CELLULES DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>DTHAUT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE D'ARETES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NUBO
!></td><td>--></td><td>NUMEROS GLOBAUX DES EXTREMITES D'UNE ARETE
!>    </td></tr>
!>          <tr><td>VNOCL
!></td><td>--></td><td>NORMALE A L'INTERFACE
!>                  (2 PREMIERES COMPOSANTES) ET
!>                  LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>NORMALE AUX POINTS FRONTIERE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE HLOC
     &(NPOIN,NSEG,NPTFR,NUBO,NBOR,VNOCL,XNEBOR,YNEBOR,AIRS,DTHAUT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRS           |-->| AIRES DES CELLULES DU MAILLAGE.
C| DTHAUT         |---| 
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS FRONTIERE
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
C| NUBO           |-->| NUMEROS GLOBAUX DES EXTREMITES D'UNE ARETE
C| VNOCL          |-->| NORMALE A L'INTERFACE
C|                |   | (2 PREMIERES COMPOSANTES) ET
C|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
C| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NSEG,NPOIN,NPTFR,NUBO(2,*)
      INTEGER, INTENT(IN)           :: NBOR(*)
      DOUBLE PRECISION, INTENT(IN)  :: VNOCL(3,*),XNEBOR(*),YNEBOR(*)
      DOUBLE PRECISION, INTENT(IN)  :: AIRS(NPOIN)
      DOUBLE PRECISION, INTENT(OUT) :: DTHAUT(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,K,NSG,NUBO1,NUBO2
      DOUBLE PRECISION VNX,VNY,VNL
C
C-----------------------------------------------------------------------
C
C     INITIALISES
C
      DO I=1,NPOIN
        DTHAUT(I) = 0.D0
      ENDDO
C
      DO NSG=1,NSEG
C
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
C
         DTHAUT(NUBO1) = DTHAUT(NUBO1) + VNOCL(3,NSG)
         DTHAUT(NUBO2) = DTHAUT(NUBO2) + VNOCL(3,NSG)
C
      ENDDO
C
      DO K=1,NPTFR
       I=NBOR(K)
       VNX=XNEBOR(K+NPTFR)
       VNY=YNEBOR(K+NPTFR)
       VNL=SQRT(VNX**2+VNY**2)
       DTHAUT(I) = DTHAUT(I) + VNL
C
      ENDDO
C
      DO I=1,NPOIN
         DTHAUT(I) = AIRS(I)/ DTHAUT(I)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C