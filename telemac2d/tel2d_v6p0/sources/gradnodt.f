C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE GRADIENTS BY TRIANGLES AND NODE
!>                AND THE DIFFUSION TERM FOR TRACER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRS, AIRT, CE, CVIST, DIFT, DJX, DJY, DPX, DPY, DTT, DX, DY, H, NS, NT, NU, T
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AIRJ, AIS, AUX, HTT, IS, JT, NUBO1, NUBO2, NUBO3, UA1, UA2, UA3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MAJTRAC()

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
!></td><td>--></td><td>AIRES DES CELLULES
!>    </td></tr>
!>          <tr><td>AIRT
!></td><td>--></td><td>AIRES DES TRIANGLES
!>    </td></tr>
!>          <tr><td>CE
!></td><td><--</td><td>TERME DE DIFFUSION
!>    </td></tr>
!>          <tr><td>CVIST
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION DU TRACEUR
!>    </td></tr>
!>          <tr><td>DIFT
!></td><td>--></td><td>LOGIQUE INDIQUANT S'IL Y A DIFFUSION TRACEUR
!>    </td></tr>
!>          <tr><td>DJX,DJY
!></td><td><--</td><td>GRADIENTS PAR TRIANGLES
!>    </td></tr>
!>          <tr><td>DPX, DPY
!></td><td>--></td><td>GRADIENTS DES FONCTIONS DE BASE
!>    </td></tr>
!>          <tr><td>DTT
!></td><td>--></td><td>PAS DE TEMPS TRACEUR
!>    </td></tr>
!>          <tr><td>DX,DY
!></td><td><--</td><td>GRADIENTS PAR NOEUDS
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEURS D'EAU
!>    </td></tr>
!>          <tr><td>NS
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NT
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NU
!></td><td>--></td><td>NUMEROS DES NOEUDS PAR TRIANGLE
!>    </td></tr>
!>          <tr><td>T
!></td><td>--></td><td>TRACEURS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GRADNODT
     &(NS,NT,NU,AIRT,AIRS,H,T,DPX,DPY,DJX,DJY,
     & DX,DY,DIFT,CVIST,CE,DTT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRS           |-->| AIRES DES CELLULES
C| AIRT           |-->| AIRES DES TRIANGLES
C| CE             |<--| TERME DE DIFFUSION
C| CVIST          |-->| COEFFICIENT DE DIFFUSION DU TRACEUR
C| DIFT           |-->| LOGIQUE INDIQUANT S'IL Y A DIFFUSION TRACEUR
C| DJX,DJY        |<--| GRADIENTS PAR TRIANGLES
C| DPX, DPY       |-->| GRADIENTS DES FONCTIONS DE BASE
C| DTT            |-->| PAS DE TEMPS TRACEUR
C| DX,DY          |<--| GRADIENTS PAR NOEUDS
C| H             |-->| HAUTEURS D'EAU
C| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
C| NT             |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NU             |-->| NUMEROS DES NOEUDS PAR TRIANGLE
C| T             |-->| TRACEURS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NS,NT
      INTEGER, INTENT(IN)             :: NU(NT,3)
      LOGICAL, INTENT(IN)             :: DIFT
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(IN)    :: AIRT(NT),AIRS(NS),H(NS),T(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(NT),DJY(NT),DX(NS),DY(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS)
      DOUBLE PRECISION, INTENT(IN)    :: DTT,CVIST
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS,JT,NUBO1,NUBO2,NUBO3
      DOUBLE PRECISION AIRJ,UA1,UA2,UA3,AIS,HTT,AUX
C
C-----------------------------------------------------------------------
C
C     INITIALISES THE HERMITIAN NODAL GRADIENTS
C
      DO IS=1,NS
        DX(IS)  = 0.D0
        DY(IS)  = 0.D0
      ENDDO
C
C     LOOP ON GLOBAL LIST OF TRIANGLES
C
      DO JT=1,NT
C
         NUBO1 = NU(JT,1)
         NUBO2 = NU(JT,2)
         NUBO3 = NU(JT,3)
C
         AIRJ=   AIRT(JT)
         HTT = H(NUBO1)+H(NUBO2)+H(NUBO3)
         AUX =  CVIST*DTT*AIRJ*HTT/3.
C
C        COMPUTES THE P1-GRADIENTS
C
           UA1=T(NUBO1)
           UA2=T(NUBO2)
           UA3=T(NUBO3)
C
C  GRADIENTS BY TRIANGLES
C
            DJX(JT)      = UA1*DPX(1,JT) +
     &               UA2*DPX(2,JT) + UA3*DPX(3,JT)
            DJY(JT)      = UA1*DPY(1,JT) +
     &               UA2*DPY(2,JT) + UA3*DPY(3,JT)
C
C  GRADIENTS BY NODES
C
         DX(NUBO1)    = DX(NUBO1) + AIRJ*DJX(JT)
         DX(NUBO2)    = DX(NUBO2) + AIRJ*DJX(JT)
         DX(NUBO3)    = DX(NUBO3) + AIRJ*DJX(JT)
C
         DY(NUBO1)    = DY(NUBO1) + AIRJ*DJY(JT)
         DY(NUBO2)    = DY(NUBO2) + AIRJ*DJY(JT)
         DY(NUBO3)    = DY(NUBO3) + AIRJ*DJY(JT)
C
C   DIFFUSION TERM
C
       IF(DIFT.AND.CVIST.NE.0.) THEN
         CE(NUBO1)       = CE(NUBO1) -AUX*
     &  (DJX(JT)*DPX(1,JT)+DJY(JT)*DPY(1,JT))
         CE(NUBO2)       = CE(NUBO2) -AUX*
     &  (DJX(JT)*DPX(2,JT)+DJY(JT)*DPY(2,JT))
         CE(NUBO3)       = CE(NUBO3) -AUX*
     &  (DJX(JT)*DPX(3,JT)+DJY(JT)*DPY(3,JT))
C
        ENDIF
C
       ENDDO
C
C     COMPLETES THE COMPUTATION OF THE NODAL GRADIENTS
C
      DO IS=1,NS
         AIS = 1.D0/(3.D0*AIRS(IS))
         DX(IS)       = DX(IS)*AIS
         DY(IS)       = DY(IS)*AIS
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C