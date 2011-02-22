C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES AUBOR, COEFFICIENT FOR THE LOG LAW
!>                AT THE BOTTOM.

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
!> </td><td> 21/06/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/01/2001
!> </td><td> V. BOYER UMIST
!> </td><td> REICHARD LAW
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE TFOND
     &(AUBOR,CF,U2D,V2D,U3D,V3D,W3D,KARMAN,LISRUG,PROPNU,Z,NPOIN,KFROT,
     & RUGOF,UETCAR,NONHYD,OPTBAN,HN,GRAV,IPBOT,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AUBOR          |<--| COEFFICIENT DE FROTTEMENT SUR LES PAROIS
C| CF             |-->| COEFFICIENT DE FROTTEMENT POUR K-EPSILON
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| HN             |-->| HAUTEUR D'EAU AU TEMPS N
C| IPBOT          |---| 
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KFROT          |-->| LAW OF BOTTOM FRICTION
C| LISRUG         |-->| REGIME DE TURBULENCE 1: LISSE 2: RUGUEUX
C| NONHYD         |---| 
C| NPLAN          |---| 
C| NPOIN          |---| 
C| OPTBAN         |---| 
C| PROPNU         |-->| COEFFICIENT DE DIFFUSION MOLECULAIRE
C| RUGOF          |-->| FRICTION COEFFICIENT
C| U2D            |---| 
C| U3D            |---| 
C| UETCAR         |<--| (FRICTION VELOCITY)**2
C| UN , VN        |-->| COMPOSANTES DE LA VITESSE AU TEMPS N
C| V2D            |---| 
C| V3D            |---| 
C| W3D            |---| 
C| Z              |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: LISRUG,NPOIN,KFROT,OPTBAN,NPLAN
      INTEGER, INTENT(IN) :: IPBOT(NPOIN)
C
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,PROPNU,GRAV
      DOUBLE PRECISION, INTENT(INOUT) :: AUBOR(NPOIN),UETCAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN)
C
C     VECTORS
C
      DOUBLE PRECISION, INTENT(IN) :: CF(*),U2D(*),V2D(*)
      DOUBLE PRECISION, INTENT(IN) :: RUGOF(*),U3D(*),V3D(*),W3D(*),Z(*)
C
      LOGICAL, INTENT(IN) :: NONHYD
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,IT,I1
C
      DOUBLE PRECISION UTANG,DIST,UETUTA,YPLUS,VNORM
C
      INTRINSIC SQRT,LOG
C
C-----------------------------------------------------------------------
C
C
C     COMPUTES UETOIL ** 2 FOR THE SOLID BOUNDARIES
C     ----------------------------------------
C
C     SMOOTH FRICTION REGIME :
C
C     ********************
      IF(LISRUG.EQ.1) THEN
C     ********************
C
        DO N=1,NPOIN
C
C         APPLIES LOG LAW AT THE FIRST PLANE TO DETERMINE U*/UTANG
C         WHICH IS CALLED UETUTA
C
          UTANG=SQRT(U3D(N+NPOIN)**2+V3D(N+NPOIN)**2)
C
          DIST  = Z(NPOIN+N)-Z(N)
C
C         ITERATIONS TO GET UETUTA
          UETUTA = 6.D-2
          DO IT=1,10
C           REICHARD LAW
C           YPLUS>30 IN THE LOGARITHMIC LAYER, OTHERWISE POSSIBLE
C           DIVISION BY ZERO, IF YPLUS=0
            YPLUS = MAX(DIST*UETUTA*UTANG/PROPNU,30.D0)
            UETUTA = 1.D0/(LOG(1.D0+KARMAN*YPLUS)/KARMAN+7.8D0*
     &      (1.D0-EXP(-YPLUS/11.D0)-YPLUS/11.D0*EXP(-0.33D0*YPLUS)))
          ENDDO
C
          UETCAR(N) = (UETUTA*UTANG)**2
C
        ENDDO
C
C     ***************************************
      ELSEIF(LISRUG.EQ.2.OR.LISRUG.EQ.3) THEN
C     ***************************************
C
C       ROUGH FRICTION REGIME
C
        IF( KFROT.EQ.0.OR.KFROT.EQ.2.OR.
     &      KFROT.EQ.3.OR.KFROT.EQ.4     ) THEN
C
C         NEW AUBOR TO GET THE SAME CALIBRATION THAN IN 2D
C         LAWS BASED ON STRICKLER OR CHEZY
          DO N=1,NPOIN
            UETCAR(N)=(U2D(N)**2+V2D(N)**2)*0.5D0*CF(N)
          ENDDO
C
        ELSEIF(KFROT.EQ.5) THEN
C
C         APPLIES LOG LAW AT THE FIRST PLANE TO DETERMINE U*
C         THE BOTTOM
C
          DO N=1,NPOIN
C           TAKES INTO ACCOUNT CRUSHED PLANES
C           I1 FIRST POINT WITH WATER ABOVE (EXCEPT CASE OF TIDAL FLATS
C           WHERE THE LEVEL UNDER THE FREE SURFACE IS TAKEN)
            I1=(MIN(NPLAN-1,IPBOT(N)+1)-1)*NPOIN+N
C           1.D-6 TO AVOID LOG(0) ON TIDAL FLATS
            DIST  = MAX((Z(NPOIN+I1)-Z(I1))+RUGOF(N)/30.D0,1.D-6)
            UETCAR(N)=(KARMAN/LOG(30.D0*DIST/RUGOF(N)))**2
     *               * (U3D(N+NPOIN)**2+V3D(N+NPOIN)**2)
          ENDDO
C
        ELSE
C
          IF(LNG.EQ.1) WRITE(LU,402) KFROT
          IF(LNG.EQ.2) WRITE(LU,403) KFROT
402       FORMAT(1X,'TFOND : LOI DE FROTTEMENT INCONNUE : ',1I6)
403       FORMAT(1X,'TFOND : UNKNOWN LAW OF FRICTION : ',1I6)
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C     ****
      ELSE
C     ****
C
        IF(LNG.EQ.1) WRITE(LU,400) LISRUG
        IF(LNG.EQ.2) WRITE(LU,401) LISRUG
400     FORMAT(1X,'TFOND : REGIME DE TURBULENCE INCONNU : ',1I6)
401     FORMAT(1X,'TFOND : UNKNOWN TURBULENCE MODEL : ',1I6)
        CALL PLANTE(1)
        STOP
C
C     *****
      ENDIF
C     *****
C
C
C-----------------------------------------------------------------------
C
C     FINAL COMPUTATION OF AUBOR = - (U*)**2 / U(BOTTOM)
C
C-----------------------------------------------------------------------
C
C     ON TIDAL FLATS A MINIMUM VELOCITY IS IMPOSED (U*)**2=GH
C     TO OPPOSE SOME FRICTION TO FREE SURFACE GRADIENTS
C
      IF(OPTBAN.EQ.1) THEN
        DO N=1,NPOIN
          IF(HN(N).LT.1.D-3) THEN
            UETCAR(N)=MAX(GRAV*MAX(HN(N),1.D-7),UETCAR(N))
          ENDIF
        ENDDO
      ENDIF
C
      IF(NONHYD) THEN
C
C       NOTE: ON TIDAL FLATS THE UPPER LAYER IS TAKEN FOR VNORM
C             MAYBE NOT USEFUL IF ALL VALUES EQUAL UP TO IPBOT+1
C
        DO N=1,NPOIN
          I1=IPBOT(N)*NPOIN+N
          VNORM=SQRT(U3D(I1)**2+V3D(I1)**2+W3D(I1)**2)
          AUBOR(N)= - UETCAR(N)/MAX(1.D-4,VNORM)
        ENDDO
C
      ELSE
C
        DO N=1,NPOIN
          I1=IPBOT(N)*NPOIN+N
          VNORM=SQRT(U3D(I1)**2+V3D(I1)**2)
          AUBOR(N)= - UETCAR(N)/MAX(1.D-4,VNORM)
        ENDDO
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
