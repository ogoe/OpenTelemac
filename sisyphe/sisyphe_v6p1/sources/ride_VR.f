C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DIMENSIONS OF EQUILIBRIUM RIPPLES.
!>                VAN RIJN (2007) (CURRENT ONLY).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ACLADM, GRAV, HN, KS, KSR, NPOIN, UNORM, XMVE, XMVS
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AI, DGRAVEL, DSAND, DSILT, FES, FFS, I, KSCD, KSCMR, KSCR, MOB, UC, ZERO
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TOB_SISYPHE()

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
!> </td><td>
!> </td><td> C. VILLARET (LNHE); AG DAVIES (UCW)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ACLAD50
!></td><td>--></td><td>DIAMTRE MOYEN DU SEDIMENT
!>    </td></tr>
!>          <tr><td>ACLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>KS
!></td><td><--</td><td>COEFFICIENT DE RUGOSITE TOTALE
!>    </td></tr>
!>          <tr><td>KSR
!></td><td><--</td><td>COEFFICIENT DE RUGOSITE DE PEAU
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS
!>    </td></tr>
!>          <tr><td>UNORM
!></td><td>--></td><td>INTENSITE DU COURANT
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>--></td><td>MASSE VOLUMIQUE DE L'EAU
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>--></td><td>MASSE VOLUMIQUE DU SEDIMENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       SUBROUTINE RIDE_VR
     & (KSR,KS,UNORM,HN,GRAV,XMVE,XMVS,NPOIN,ACLADM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACLAD50        |-->| DIAMTRE MOYEN DU SEDIMENT
C| ACLADM         |---| 
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| HN             |-->| HAUTEUR D'EAU
C| KS             |<--| COEFFICIENT DE RUGOSITE TOTALE
C| KSR            |<--| COEFFICIENT DE RUGOSITE DE PEAU
C| NPOIN          |-->| NOMBRE DE POINTS
C| UNORM          |-->| INTENSITE DU COURANT
C| XMVE           |-->| MASSE VOLUMIQUE DE L'EAU
C| XMVS           |-->| MASSE VOLUMIQUE DU SEDIMENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER I,NPOIN
C
      DOUBLE PRECISION, INTENT(INOUT)  :: KSR(NPOIN),KS(NPOIN)
      DOUBLE PRECISION, INTENT(IN)     :: GRAV,XMVE,XMVS
      DOUBLE PRECISION, INTENT(IN)     :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)     :: ACLADM(NPOIN),UNORM(NPOIN)
C
C LOCAL VARIABLES
C
      DOUBLE PRECISION UC,AI,ZERO,KSCR,KSCD,KSCMR,MOB,FES,FFS
      DOUBLE PRECISION DSAND,DGRAVEL,DSILT
C
C---------------------------------------------------------------------
C
      ZERO=1.D-6
      DSILT=0.000032D0
      DGRAVEL=0.002D0
      DSAND=0.000062D0
C
C COMPUTES CURRENT-DOMINATED ROUGHNESS USING VAN RIJN (2007)
C
      DO I=1,NPOIN
C
C MOBILITY NUMBER FOR CURRENT ONLY
C
         AI  = ACLADM(I)*GRAV*(XMVS-XMVE)/XMVE
         MOB = UNORM(I)**2/AI
C
C RIPPLE ROUGHNESS
C
         IF(ACLADM(I).LE.0.25D0*DGRAVEL)THEN
           FES=1.D0
         ELSE
           FES=(0.25D0*DGRAVEL/ACLADM(I))**1.5D0
         ENDIF
C
         IF(ACLADM(I).LT.DSILT)THEN
           KSCR=20.D0*DSILT
         ELSE
           AI= TANH(0.015D0*(MOB-150.D0))
           KSCR=FES*ACLADM(I)*(85.D0-65.D0*AI)
         ENDIF
C
C MEGARIPPLE ROUGHNESS
C
         IF(ACLADM(I).GE.(1.5D0*DSAND))THEN
           FFS=1.D0
         ELSE
           FFS=ACLADM(I)/1.5D0/DSAND
         ENDIF
         IF(ACLADM(I).LE.DSILT)THEN
           KSCMR=0.D0
         ELSE
           KSCMR=0.00002D0*FFS*HN(I)*(1.D0-EXP(-0.05D0*MOB))
           IF(MOB.GT.550.D0.AND.ACLADM(I).GE.1.5D0*DSAND)THEN
             KSCMR=0.02D0
           ELSEIF(MOB.GT.550D0.AND.ACLADM(I).LT.1.5D0*DSAND)THEN
             KSCMR=200.D0*ACLADM(I)
           ENDIF
         ENDIF
C
C DUNE ROUGHNESS
C
         IF(ACLADM(I).LT.DSILT) THEN
           KSCD=0.D0
         ELSE
           AI=(1.D0-EXP(-0.02D0*MOB))*(600.D0-MOB)
           KSCD=0.00008D0*FFS*HN(I)* AI
         ENDIF
         IF(MOB.GT.600.D0) KSCD=0.D0
         IF(KSCD.GT.1.D0) KSCD=1.D0
C
C ***RIPPLE BED ROUGHNESS FOR SEDIMENT COMPUTATIONS IN SISYPHE ***
C
         KSR(I)=KSCR
C
C *** TOTAL ROUGHNESS FOR COMPUTATIONS IN TELEMAC2D **
C
         KS(I)=SQRT(KSCR**2+KSCMR**2+KSCD**2)
C
      ENDDO
C
CAGD****************************************************
C
C---------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C