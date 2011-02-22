C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SAINT VENANT-KINETIC.
!><br>            COMPUTES THE Z VARIATIONS (2ND ORDER).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRS, AIRST, AIRT, BETA, CMI, CORR, DPX, DPY, DSM, DSP, DSZ, DXIZ, DYIZ, JV, NS, NSEG, NT, NU, NUBO, X, Y, ZF
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AIRJ, AIX, AIY, AJX, AJY, AMDS, DSH, DXTZ, DYTZ, GRADI, GRADJ, GRIJ, GRJI, I1, I2, I3, ILIM, IS, J, JT, NSG, NUBO1, NUBO2, ZF1, ZF2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> EXLIM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RESOLU()

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
!>          <tr><td>AIRST
!></td><td>--></td><td>AIRES DES SOUS-TRIANGLES DANS CELLULES
!>    </td></tr>
!>          <tr><td>AIRT
!></td><td>--></td><td>AIRES DES TRIANGLES
!>    </td></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>COEFFICIENT EXTRAPOLATION
!>    </td></tr>
!>          <tr><td>CMI
!></td><td>--></td><td>COORDONNEES DES POINTS MILIEUX D'INTERFACE
!>    </td></tr>
!>          <tr><td>DPX,DPY
!></td><td>--></td><td>GRADIENT DES FONCTIONS DE BASE P1
!>                  PAR TRIANGLE
!>    </td></tr>
!>          <tr><td>DSM,CORR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DSZ
!></td><td><--</td><td>VARIATION DE Z POUR ORDRE 2
!>    </td></tr>
!>          <tr><td>DXIZ,DYIZ,DSP
!></td><td>---</td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>JV
!></td><td>--></td><td>NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
!>                  POINT MILIEU D'UNE INTERFACE
!>    </td></tr>
!>          <tr><td>NS
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE D'ARETES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NT
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NU
!></td><td>--></td><td>NUMEROS DES NOEUDS PAR TRIANGLE
!>    </td></tr>
!>          <tr><td>NUBO
!></td><td>--></td><td>NUMEROS DES DEUX SOMMETS D'UNE ARETE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTES DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GRADZ
     &(NS,NT,NSEG,NU,NUBO,X,Y,AIRT,AIRS,CMI,JV,
     & ZF,DPX,DPY,DSZ,BETA,AIRST,DXIZ,DYIZ,DSP,DSM,CORR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRS           |-->| AIRES DES CELLULES
C| AIRST          |-->| AIRES DES SOUS-TRIANGLES DANS CELLULES
C| AIRT           |-->| AIRES DES TRIANGLES
C| BETA           |---| COEFFICIENT EXTRAPOLATION
C| CMI            |-->| COORDONNEES DES POINTS MILIEUX D'INTERFACE
C| DPX,DPY        |-->| GRADIENT DES FONCTIONS DE BASE P1
C|                |   | PAR TRIANGLE
C| DSM,CORR       |---| 
C| DSZ            |<--| VARIATION DE Z POUR ORDRE 2
C| DXIZ,DYIZ,DSP  |---| TABLEAUX DE TRAVAIL
C| JV             |-->| NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
C|                |   | POINT MILIEU D'UNE INTERFACE
C| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
C| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
C| NT             |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NU             |-->| NUMEROS DES NOEUDS PAR TRIANGLE
C| NUBO           |-->| NUMEROS DES DEUX SOMMETS D'UNE ARETE
C| X,Y            |-->| COORDONNEES DES NOEUDS DU MAILLAGE
C| ZF             |-->| COTES DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NS,NT,NSEG
      INTEGER, INTENT(IN)             :: NU(NT,3),NUBO(2,NSEG),JV(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS),AIRT(NT),AIRS(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DXIZ(NS),DYIZ(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DSP(NS),DSM(NS),CORR(NS),BETA
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(IN)    :: CMI(2,*),AIRST(2,*),ZF(NS)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS,I1,I2,I3,JT,J,NSG,NUBO1,NUBO2,ILIM
      DOUBLE PRECISION AIRJ,DXTZ,DYTZ,AIX,AIY,AJX,AJY
      DOUBLE PRECISION ZF1,ZF2,GRADI,GRADJ,GRIJ,GRJI,AMDS,DSH
C
      DOUBLE PRECISION EXLIM
      EXTERNAL         EXLIM
C
C-----------------------------------------------------------------------
C
      DO IS=1,NS
         DXIZ(IS) = 0.D0
         DYIZ(IS) = 0.D0
      ENDDO
C
      DO JT=1,NT
C
         I1 = NU(JT,1)
         I2 = NU(JT,2)
         I3 = NU(JT,3)
C
         AIRJ =   AIRT(JT)
         DXTZ =ZF(I1)*DPX(1,JT) +ZF(I2)*DPX(2,JT) + ZF(I3)*DPX(3,JT)
         DYTZ =ZF(I1)*DPY(1,JT) +ZF(I2)*DPY(2,JT) + ZF(I3)*DPY(3,JT)
C
         DXIZ(I1) = DXIZ(I1) + AIRJ*DXTZ
         DXIZ(I2) = DXIZ(I2) + AIRJ*DXTZ
         DXIZ(I3) = DXIZ(I3) + AIRJ*DXTZ
C
         DYIZ(I1) = DYIZ(I1) + AIRJ*DYTZ
         DYIZ(I2) = DYIZ(I2) + AIRJ*DYTZ
         DYIZ(I3) = DYIZ(I3) + AIRJ*DYTZ
      ENDDO
C
      DO IS=1,NS
         DXIZ(IS) = DXIZ(IS)/(3.D0*AIRS(IS))
         DYIZ(IS) = DYIZ(IS)/(3.D0*AIRS(IS))
         DSP(IS)  = 0.D0
         DSM(IS)  = 0.D0
      ENDDO
C
C    REBUILDS BY INTERFACE
C
      DO NSG=1,NSEG
C
         J         = JV(NSG)
C
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
C
         ZF1   =    ZF(NUBO1)
         ZF2   =    ZF(NUBO2)
C
         AIX       = CMI(1,NSG)-X(NUBO1)
         AIY       = CMI(2,NSG)-Y(NUBO1)
         AJX       = CMI(1,NSG)-X(NUBO2)
         AJY       = CMI(2,NSG)-Y(NUBO2)
C
C        NODE GRADIENTS
C
         GRADI  = AIX*DXIZ(NUBO1) + AIY*DYIZ(NUBO1)
C
         GRADJ  = AJX*DXIZ(NUBO2) + AJY*DYIZ(NUBO2)
C
C
         I1 = NU(J,1)
         I2 = NU(J,2)
         I3 = NU(J,3)
C
C        GRADIENT BY TRIANGLE
C
         DXTZ =ZF(I1)*DPX(1,J) +ZF(I2)*DPX(2,J) + ZF(I3)*DPX(3,J)
         DYTZ =ZF(I1)*DPY(1,J) +ZF(I2)*DPY(2,J) + ZF(I3)*DPY(3,J)
C
         GRIJ  = AIX*DXTZ + AIY*DYTZ
C
         GRJI  = AJX*DXTZ + AJY*DYTZ
C
C    EXTRAPOLATES AND CAPS
C
       ILIM=1
       BETA=1.D0
         DSZ(1,NSG)  =  EXLIM(ILIM,BETA,GRADI,GRIJ )
         DSZ(2,NSG)  =  EXLIM (ILIM,BETA,GRADJ,GRJI )
C
         IF(DSZ(1,NSG).GE.0.D0) THEN
         DSP(NUBO1) = DSP(NUBO1) + AIRST(1,NSG)*DSZ(1,NSG)
         ELSE
         DSM(NUBO1) = DSM(NUBO1) - AIRST(1,NSG)*DSZ(1,NSG)
         ENDIF
         IF(DSZ(2,NSG).GE.0.) THEN
         DSP(NUBO2) = DSP(NUBO2) + AIRST(2,NSG)*DSZ(2,NSG)
         ELSE
         DSM(NUBO2) = DSM(NUBO2) - AIRST(2,NSG)*DSZ(2,NSG)
         ENDIF
C
       ENDDO
C
C  COMPUTES THE CORRECTIONS NECESSARY TO HAVE CONSERVATION
C
      DO IS=1,NS
       CORR(IS) =  DSM(IS) - DSP(IS)
       AMDS =MAX(DSP(IS),DSM(IS))
        IF(AMDS.GT.0.D0) THEN
        CORR(IS) = CORR(IS)/AMDS
        ENDIF
      ENDDO
C
      DO NSG=1,NSEG
C
         NUBO1 = NUBO(1,NSG)
         NUBO2 = NUBO(2,NSG)
C
         DSH =  DSZ(1,NSG)
         DSZ(1,NSG) =   DSH +
     & MIN(0.D0,CORR(NUBO1))*MAX(0.D0,DSH)+
     & MAX(0.D0,CORR(NUBO1))*MAX(0.D0,-DSH)
C
         DSH     =  DSZ(2,NSG)
         DSZ(2,NSG) =   DSH +
     & MIN(0.D0,CORR(NUBO2))*MAX(0.D0,DSH)+
     & MAX(0.D0,CORR(NUBO2))*MAX(0.D0,-DSH)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C