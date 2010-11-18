C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE GRADIENTS BY TRIANGLES AND NODE
!>                AND THE DIFFUSION TERMS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRS, AIRT, CE, CVIS, DJX, DJY, DPX, DPY, DX, DY, IVIS, NS, NT, NU, UA, ZF
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AIRJ, AIS, AUX, HTT, IS, IVAR, JT, NUBO1, NUBO2, NUBO3, UA1, UA2, UA3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUHYD()

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
!></td><td><--</td><td>TERMES DE DIFFUSION
!>    </td></tr>
!>          <tr><td>CVIS
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION DES VITESSES
!>    </td></tr>
!>          <tr><td>DJX,DJY
!></td><td><--</td><td>GRADIENTS PAR TRIANGLES
!>    </td></tr>
!>          <tr><td>DPX, DPY
!></td><td>--></td><td>GRADIENTS DES FONCTIONS DE BASE
!>    </td></tr>
!>          <tr><td>DX,DY
!></td><td><--</td><td>GRADIENTS PAR NOEUDS
!>    </td></tr>
!>          <tr><td>IVIS
!></td><td>--></td><td>OPTION DIFFUSION DES VITESSES
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
!>          <tr><td>UA
!></td><td>--></td><td>UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTES DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GRADNOD
     &(NS,NT,NU,AIRT,AIRS,UA,DPX,DPY,DJX,DJY,DX,DY,IVIS,CVIS,CE,ZF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRS           |-->| AIRES DES CELLULES
C| AIRT           |-->| AIRES DES TRIANGLES
C| CE             |<--| TERMES DE DIFFUSION
C| CVIS           |-->| COEFFICIENT DE DIFFUSION DES VITESSES
C| DJX,DJY        |<--| GRADIENTS PAR TRIANGLES
C| DPX, DPY       |-->| GRADIENTS DES FONCTIONS DE BASE
C| DX,DY          |<--| GRADIENTS PAR NOEUDS
C| IVIS           |-->| OPTION DIFFUSION DES VITESSES
C| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
C| NT             |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NU             |-->| NUMEROS DES NOEUDS PAR TRIANGLE
C| UA             |-->| UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
C| ZF             |-->| COTES DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NS,NT,IVIS
      INTEGER, INTENT(IN)             :: NU(NT,3)
      DOUBLE PRECISION, INTENT(IN)    :: AIRT(NT),AIRS(NS),CVIS
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,NT),DJY(3,NT)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NS),DY(3,NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(3,NS)
      DOUBLE PRECISION, INTENT(IN)    :: UA(3,NS),ZF(NS)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NT),DPY(3,NT)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS,JT,NUBO1,NUBO2,NUBO3,IVAR
      DOUBLE PRECISION AIRJ,UA1,UA2,UA3,AIS,HTT,AUX
C
C-----------------------------------------------------------------------
C
C     INITIALISES THE HERMITIAN NODAL GRADIENTS
C
      DO IS=1,NS
        DO IVAR=1,3
          DX(IVAR,IS) = 0.D0
          DY(IVAR,IS) = 0.D0
        ENDDO
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
         HTT = UA(1,NUBO1)+UA(1,NUBO2)+UA(1,NUBO3)
         AUX = CVIS*AIRJ*HTT/3.D0
C
C        COMPUTES THE P1-GRADIENTS
C
C
C   COMPUTES THE H+Z GRADIENT
C
       IVAR=1
           UA1=UA(IVAR,NUBO1) + ZF(NUBO1)
           UA2=UA(IVAR,NUBO2) + ZF(NUBO2)
           UA3=UA(IVAR,NUBO3) + ZF(NUBO3)
C
            DJX(IVAR,JT)      = UA1*DPX(1,JT) +
     &               UA2*DPX(2,JT) + UA3*DPX(3,JT)
            DJY(IVAR,JT)      = UA1*DPY(1,JT) +
     &               UA2*DPY(2,JT) + UA3*DPY(3,JT)
C
         DX(IVAR,NUBO1)    = DX(IVAR,NUBO1) + AIRJ*DJX(IVAR,JT)
         DX(IVAR,NUBO2)    = DX(IVAR,NUBO2) + AIRJ*DJX(IVAR,JT)
         DX(IVAR,NUBO3)    = DX(IVAR,NUBO3) + AIRJ*DJX(IVAR,JT)
C
         DY(IVAR,NUBO1)    = DY(IVAR,NUBO1) + AIRJ*DJY(IVAR,JT)
         DY(IVAR,NUBO2)    = DY(IVAR,NUBO2) + AIRJ*DJY(IVAR,JT)
         DY(IVAR,NUBO3)    = DY(IVAR,NUBO3) + AIRJ*DJY(IVAR,JT)
C
C    COMPUTES THE VELOCITY GRADIENTS
C
         DO IVAR=2,3
C
           UA1=UA(IVAR,NUBO1)
           UA2=UA(IVAR,NUBO2)
           UA3=UA(IVAR,NUBO3)
C
            DJX(IVAR,JT)      = UA1*DPX(1,JT) +
     &               UA2*DPX(2,JT) + UA3*DPX(3,JT)
            DJY(IVAR,JT)      = UA1*DPY(1,JT) +
     &               UA2*DPY(2,JT) + UA3*DPY(3,JT)
C
         DX(IVAR,NUBO1)    = DX(IVAR,NUBO1) + AIRJ*DJX(IVAR,JT)
         DX(IVAR,NUBO2)    = DX(IVAR,NUBO2) + AIRJ*DJX(IVAR,JT)
         DX(IVAR,NUBO3)    = DX(IVAR,NUBO3) + AIRJ*DJX(IVAR,JT)
C
         DY(IVAR,NUBO1)    = DY(IVAR,NUBO1) + AIRJ*DJY(IVAR,JT)
         DY(IVAR,NUBO2)    = DY(IVAR,NUBO2) + AIRJ*DJY(IVAR,JT)
         DY(IVAR,NUBO3)    = DY(IVAR,NUBO3) + AIRJ*DJY(IVAR,JT)
         ENDDO
C
C  COMPUTES THE VELOCITY DIFFUSION TERMS
C
      IF(IVIS.EQ.0.OR.CVIS.EQ.0.) GOTO 10
         CE(2,NUBO1)       = CE(2,NUBO1) -AUX*
     &  (DJX(2,JT)*DPX(1,JT)+DJY(2,JT)*DPY(1,JT))
         CE(2,NUBO2)       = CE(2,NUBO2) -AUX*
     &  (DJX(2,JT)*DPX(2,JT)+DJY(2,JT)*DPY(2,JT))
         CE(2,NUBO3)       = CE(2,NUBO3) -AUX*
     &  (DJX(2,JT)*DPX(3,JT)+DJY(2,JT)*DPY(3,JT))
C
         CE(3,NUBO1)       = CE(3,NUBO1) -AUX*
     &  (DJX(3,JT)*DPX(1,JT)+DJY(3,JT)*DPY(1,JT))
         CE(3,NUBO2)       = CE(3,NUBO2) -AUX*
     &  (DJX(3,JT)*DPX(2,JT)+DJY(3,JT)*DPY(2,JT))
         CE(3,NUBO3)       = CE(3,NUBO3) -AUX*
     &  (DJX(3,JT)*DPX(3,JT)+DJY(3,JT)*DPY(3,JT))
 10       CONTINUE
         ENDDO
C
C     COMPLETES THE COMPUTATION OF THE NODAL GRADIENTS
C
      DO IS=1,NS
C
         AIS = 1.D0/(3.D0*AIRS(IS))
C
         DO IVAR=1,3
           DX(IVAR,IS) = DX(IVAR,IS)*AIS
           DY(IVAR,IS) = DY(IVAR,IS)*AIS
         ENDDO
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C