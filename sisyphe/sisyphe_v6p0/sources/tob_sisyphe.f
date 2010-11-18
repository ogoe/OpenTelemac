C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TOTAL STRESS AT THE BOTTOM DEPENDING
!>                ON WHETHER SISYPHE IS COUPLED OR NOT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ACLADM, CF, CF_TEL, CHESTR, CODE, FW, GRAV, HMIN, HN, HOULE, ICR, KARMAN, KFROT, KS, KSP, KSPRATIO, KSR, MU, NPOIN, TOB, TOBW, TW, UETCAR, UNORM, UW, VCE, XMVE, XMVS, ZERO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, B, C, HCLIP, I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_TOB_SISYPHE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> COEFRO_SISYPHE(), OS(), OV(), RIDE(), RIDE_VR(), TOBW_SISYPHE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!> </td><td> 29/11/06
!> </td><td> C. VILLARET (LNHE) 01 30 87 83 28
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/04/05
!> </td><td> CV
!> </td><td> CORRECTION WHEN SISYPHE IS RUN ALONE: DO NOT MODIFY
!>           CHESTR EXCEPT IF KFROT = 0 OR 1
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ACLADM
!></td><td>--></td><td>DIAMETRE MOYEN  DU SEDIMENT
!>    </td></tr>
!>          <tr><td>CF
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT QUADRATIQUE DU COURANT
!>    </td></tr>
!>          <tr><td>CF_TEL
!></td><td>--></td><td>COEFFICIENT DE FROTTMT CF      SI COUPL. T2D
!>    </td></tr>
!>          <tr><td>CHESTR
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT (MOT CLE)
!>    </td></tr>
!>          <tr><td>CODE
!></td><td>--></td><td>CALLING PROGRAM IN COUPLING
!>    </td></tr>
!>          <tr><td>FW
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT QUADRATIQUE DE LA HOULE
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITE
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU AU TEMPS N
!>    </td></tr>
!>          <tr><td>HOULE
!></td><td>--></td><td>PRISE EN COMPTE DE LA HOULE
!>    </td></tr>
!>          <tr><td>ICR
!></td><td>--></td><td>PREDICTEUR DE RIDE POUR LE FROTTEMENT DE PEAU
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI     DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>KS
!></td><td><--</td><td>RUGOSITE TOTALE
!>    </td></tr>
!>          <tr><td>KSP
!></td><td><--</td><td>RUGOSITE DE PEAU
!>    </td></tr>
!>          <tr><td>KSPRATIO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSR
!></td><td><--</td><td>RUGOSITE DE RIDE
!>    </td></tr>
!>          <tr><td>MU
!></td><td><--</td><td>RAPPORT ENTRE LA CONTRAINTE DE FROTTEMENT DE PEAU ET
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>Q
!></td><td>--></td><td>DEBIT LIQUIDE
!>    </td></tr>
!>          <tr><td>TOB
!></td><td><--</td><td>CONTRAINTE DE FROTTEMENT TOTAL EN COURANT SEUL
!>    </td></tr>
!>          <tr><td>TOBW
!></td><td><--</td><td>CONTRAINTE DE FROTTEMENT  EN HOULE SEULE
!>    </td></tr>
!>          <tr><td>TW,UW
!></td><td>--></td><td>PERIODE DE LA HOULE ET VITESSE ORBITALE
!>    </td></tr>
!>          <tr><td>UETCAR
!></td><td>--></td><td>VITESSE DE FROTTEMENT AU CARRE SI COUPL. T3D
!>    </td></tr>
!>          <tr><td>UNORM
!></td><td>--></td><td>INTENSITE DU COURANT
!>    </td></tr>
!>          <tr><td>VCE
!></td><td>--></td><td>VISCOSITE DE L'EAU
!>    </td></tr>
!>          <tr><td>XMVE,XMVS
!></td><td>--></td><td>MASSE VOLUMIQUE DE L'EAU, DU SEDIMENT
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TOB_SISYPHE
     & (TOB, TOBW, MU, KS,KSP, KSR,CF,FW,CHESTR,UETCAR,CF_TEL,CODE,
     &  KFROT,ICR, KSPRATIO, HOULE,GRAV,XMVE,XMVS, VCE, KARMAN,
     &  ZERO,HMIN,HN, ACLADM, UNORM,UW, TW, NPOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACLADM         |-->| DIAMETRE MOYEN  DU SEDIMENT
C| CF             |<--| COEFFICIENT DE FROTTEMENT QUADRATIQUE DU COURANT
C| CF_TEL         |-->| COEFFICIENT DE FROTTMT CF      SI COUPL. T2D
C| CHESTR         |-->| COEFFICIENT DE FROTTEMENT (MOT CLE)
C| CODE           |-->| CALLING PROGRAM IN COUPLING
C| FW             |<--| COEFFICIENT DE FROTTEMENT QUADRATIQUE DE LA HOULE
C| GRAV           |-->| GRAVITE
C| HMIN           |---| 
C| HN             |-->| HAUTEUR D'EAU AU TEMPS N
C| HOULE          |-->| PRISE EN COMPTE DE LA HOULE
C| ICR            |-->| PREDICTEUR DE RIDE POUR LE FROTTEMENT DE PEAU
C| KARMAN         |---| 
C| KFROT          |-->| LOI     DE FROTTEMENT
C| KS             |<--| RUGOSITE TOTALE
C| KSP            |<--| RUGOSITE DE PEAU
C| KSPRATIO       |---| 
C| KSR            |<--| RUGOSITE DE RIDE
C| MU             |<--| RAPPORT ENTRE LA CONTRAINTE DE FROTTEMENT DE PEAU ET
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| Q             |-->| DEBIT LIQUIDE
C| TOB            |<--| CONTRAINTE DE FROTTEMENT TOTAL EN COURANT SEUL
C| TOBW           |<--| CONTRAINTE DE FROTTEMENT  EN HOULE SEULE
C| TW,UW          |-->| PERIODE DE LA HOULE ET VITESSE ORBITALE
C| UETCAR         |-->| VITESSE DE FROTTEMENT AU CARRE SI COUPL. T3D
C| UNORM          |-->| INTENSITE DU COURANT
C| VCE            |-->| VISCOSITE DE L'EAU
C| XMVE,XMVS      |-->| MASSE VOLUMIQUE DE L'EAU, DU SEDIMENT
C| ZERO           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_SISYPHE, EX_TOB_SISYPHE=>TOB_SISYPHE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,            INTENT(IN)  :: NPOIN,KFROT,ICR
C      LOGICAL,            INTENT(IN) :: LCONDIS
      LOGICAL,            INTENT(IN)  :: HOULE
      CHARACTER(LEN=24),  INTENT(IN)  :: CODE
      DOUBLE PRECISION,   INTENT(IN)  :: XMVE,XMVS, VCE,GRAV,KARMAN
      DOUBLE PRECISION,   INTENT(IN)  :: ZERO,HMIN,KSPRATIO
      TYPE(BIEF_OBJ), INTENT(IN)      :: UETCAR
      TYPE(BIEF_OBJ), INTENT(IN)      :: HN,UNORM
      TYPE(BIEF_OBJ), INTENT(IN)      :: TW,UW
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: KS,KSP,KSR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: CHESTR,MU
      TYPE(BIEF_OBJ), INTENT(IN)      :: ACLADM
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: CF,TOB
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FW,TOBW
      TYPE(BIEF_OBJ), INTENT(IN)      :: CF_TEL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER                     :: I
      DOUBLE PRECISION            :: A,B,C, HCLIP
C
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  BED ROUGHNESS PREDICTOR
C                         SKIN   : KSP
C                         TOTAL  : KS
C                         RIPPLES : KSR
C                         KS PUT IN CHESTR IF NO COUPLING, RE-COMPUTED OTHERWISE
C  NOTE: IT IS RECOMMENDED TO USE FRICTION LAW NO 3 WHEN COUPLING TO
C        AVOID UNNECESSARY COMPUTATION
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C SKIN BED ROUGHNESS --> KSP
C
        CALL OS('X=CY    ', X=KSP, Y=ACLADM, C=KSPRATIO)
C
C RIPPLED BED ROUGHNESS --> KSR =KSP
C
        CALL OS('X=CY    ', X=KSR, Y=ACLADM, C=KSPRATIO)
C
C TOTAL BED ROUGHNESS --> KS
C        KFROT= 0: FLAT BED     KS=KSP
C        KFROT = 1: RIPPLED BED KS= KSP + KSR +KSMR
C
         IF(KFROT.EQ.0) THEN
           CALL OS('X=Y     ', X=KS, Y=KSP)
          ENDIF
C
          IF(KFROT.EQ.1.OR.ICR.EQ.2) THEN
C
            IF(HOULE) THEN
C WIBERG AND HARRIS: KSR (RIPPLES)
C                    KS (RIPPLES + SKIN)
              CALL RIDE(KSR%R, TW%R, UW%R, UNORM%R, GRAV, XMVE,
     &                XMVS, VCE, NPOIN, KSPRATIO, ACLADM%R)
              CALL OS('X=Y+Z   ', X=KS, Y=KSP, Z=KSR)
            ELSE
C VR PREDICTOR : KSR (RIPPLES)
C                KS (RIPPLES+DUNES+MEGA-RIPPLES) + SKIN
              CALL RIDE_VR(KSR%R,KS%R,UNORM%R,HN%R,GRAV,XMVE,
     &                     XMVS,NPOIN,ACLADM%R)
              CALL OS('X=X+Y   ', X=KS, Y=KSP)
            ENDIF
C
          ENDIF
C
C SISYPHE ALONE: CHESTR IS CHANGED ONLY IF KFROT =1 OR 0
C
          IF(KFROT.EQ.1.OR.KFROT.EQ.0)
     &      CALL OS('X=Y     ', X=CHESTR, Y=KS)
C

C
C ----------------------------------------------------------------------------------------------
C TOTAL HYDRODYNAMIC FRICTION :  --> TOB
C  QUADRATIC COEFICIENT       :  ---> CF
C
C-----------------------------------------------------------------------
C
C     INTERNAL COUPLING WITH TELEMAC2D
C     UETCAR IS CF IN TELEMAC-2D
C
      IF(CODE(1:9).EQ.'TELEMAC2D') THEN
         CALL OV('X=Y     ',CF%R,CF_TEL%R,CF_TEL%R,0.D0,CF%DIM1)
         DO I=1,NPOIN
           TOB%R(I) = XMVE*0.5D0*CF%R(I)*UNORM%R(I)**2
         ENDDO
C
C     INTERNAL COUPLING WITH TELEMAC3D
C     UETCAR CORRESPONDS TO THE FRICTION VELOCITY SQUARED
C
      ELSEIF(CODE(1:9).EQ.'TELEMAC3D') THEN
        CALL OS( 'X=CY     ',X=TOB,Y=UETCAR,C=XMVE)
        CALL OV('X=Y     ',CF%R,CF_TEL%R,CF_TEL%R,0.D0,CF%DIM1)
C
C   NO COUPLING : USE KFROT AND CHESTR
C
      ELSE
C
        CALL COEFRO_SISYPHE(CF,HN,KFROT,CHESTR,GRAV,NPOIN,HMIN,KARMAN)
        DO I=1,NPOIN
          TOB%R(I) = XMVE*0.5D0*CF%R(I)*UNORM%R(I)**2
        ENDDO
C
      ENDIF
C
C ---------------------------------------------------------------------
C TOTAL BED ROUGHNESS COMPUTED AS A FUNCTION OF QUADRATIC BED FRICTION
C UNNECESSARY IF (KFROT =0, 1 OR 5) AND (NO COUPLING)
C              ---->   KS
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF (CODE(1:8).EQ.'TELEMAC'.OR.KFROT.GE.2.OR.KFROT.LE.4) THEN
         DO I=1,NPOIN
           A = KARMAN*SQRT(2.D0/MAX(CF%R(I),ZERO))
           KS%R(I)=12.D0*HN%R(I)/EXP(A)
         ENDDO
      ENDIF
C ------------------------------------------------------------------------
C SKIN FRICTION CORRECTOR
C                ---> MU = TOP/TOB
C ICR=0:    MU=1
C ICR=1     : SKIN FRICTION CORRECTION USE KSP
C ICR= 2    : RIPPLE ROUGHNESS USE KSR, KSR
C COUPLED WITH TELEMAC: MU>1 IS ACCEPTABLE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      IF(ICR.EQ.0) THEN
          CALL OS('X=C     ', X=MU, C=1.D0)
      ELSE IF(ICR.EQ.1) THEN
         DO I= 1, NPOIN
          IF((CF%R(I) > ZERO).AND.(HN%R(I).GT.KSP%R(I))) THEN
            HCLIP=MAX(HN%R(I),KSP%R(I))
            A = 2.5D0*LOG(12.D0*HCLIP/ KSP%R(I))
            C =2.D0/A**2
            MU%R(I) = C/CF%R(I)
          ELSE
             MU%R(I) = 0.D0
          ENDIF
        ENDDO
      ELSE IF(ICR.EQ.2) THEN
        DO I= 1, NPOIN
           IF(HN%R(I).GT.MAX(KSR%R(I),KSP%R(I)).AND.
     &        CF%R(I).GT.ZERO)THEN
                A = LOG(12.D0*HN%R(I)/ KSP%R(I))
                B = LOG(12.D0*HN%R(I)/ KSR%R(I))
                C =0.32D0/CF%R(I)
                MU%R(I) = C/SQRT(B*A**3)
           ELSE
             MU%R(I) = 0.D0
           ENDIF
        ENDDO
      ENDIF
C
C -----WAVE-INDUCED FRICTION -----------------------------
C  --> TOBW
C
      IF(HOULE) THEN
        CALL TOBW_SISYPHE
     &          (TOBW%R,CF%R,FW%R,UW%R,TW%R,HN%R,NPOIN,XMVE)
      ENDIF
C
C------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C