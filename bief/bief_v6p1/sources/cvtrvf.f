!                    *****************
                     SUBROUTINE CVTRVF
!                    *****************
!
     &(F,FN,FSCEXP,DIFT,CONV,H,HN,HPROP,UCONV,VCONV,DM1,ZCONV,SOLSYS,
     & VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,FBOR,MASKTR,MESH,
     & T1,T2,T3,T4,T5,T6,T7,T8,HNT,HT,AGGLOH,TE1,DT,ENTET,BILAN,
     & OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,LIMTRA,KDIR,KDDL,NPTFR,FLBOR,
     & YAFLBOR,V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    FINITE VOLUMES, UPWIND, EXPLICIT ADVECTOR.
!
!warning  AFBOR AND BFBOR MUST BE 0 FOR THE BOUNDARY ELEMENTS
!+            WITH NO FRICTION
!warning  DISCRETISATION OF VISC
!
!history  CHI-TUAN PHAM  (LNHE)    ?? ??
!+        09/10/09
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AGGLOH         |-->| MASS-LUMPING UTILISE DANS L'EQUATION DE CONTINUITE
!| BILAN          |-->| LOGIQUE INDIQUANT SI ON DOIT FAIRE UN BILAN
!|                |   | DE MASSE. DANS CE CAS IL FAUT RETOURNER LA
!|                |   | VALEUR DE L'APPORT DES TERMES SOURCES.
!| CONV           |-->| LOGIQUE INDIQUANT S'IL Y A CONVECTION DE F
!| DIFT           |-->| LOGIQUE INDIQUANT S'IL Y A DIFFUSION DE F
!| DM1            |---|
!| DT             |-->| PAS DE TEMPS
!| ENTET          |-->| LOGIQUE INDIQUANT SI ON IMPRIME DES INFOS
!|                |   | SUR LE BILAN DE MASSE DE TRACEUR
!| F              |<--| VALEURS A L' ETAPE N+1.
!| FBOR           |-->| CONDITIONS DE DIRICHLET SUR F.
!| FLBOR          |---|
!| FLBORTRA       |---|
!| FN             |-->| VALEURS A L' ETAPE N.
!| FSCEXP         |-->| PARTIE EXPLICITE DU TERME SOURCE
!|                |   | EGALE A ZERO PARTOUT SAUF POUR LES POINTS
!|                |   | SOURCES OU IL Y A FSCE - (1-TETAT) FN
!|                |   | VOIR DIFSOU
!| HNT,HT         |<--| TABLEAUX DE TRAVAIL (HAUTEURS MODIFIEES POUR
!|                |   | TENIR COMPTE DU MASS-LUMPING)
!| HPROP          |-->| HAUTEUR DE PROPAGATION (FAITE DANS CVDFTR).
!| IOPT           |---| OPTIONS DE CALCUL
!|                |   | CHIFFRE DES DIZAINES (IOPT2):
!|                |   | 0 : UCONV RESPECTE L'EQUATION DE CONTINUITE
!|                |   | 1 : UCONV NE RESPECTE PAS LA CONTINUITE
!|                |   | CHIFFRE DES UNITES (IOPT1):
!|                |   | 0 : CONSTANTE PAR ELEMENT NULLE
!|                |   | 1 : CONSTANTE DE CHI-TUAN PHAM
!|                |   | 2 : SCHEMA N
!|                |   | 3 : SCHEMA PSI
!| KDDL           |-->| CONVENTION POUR LES DEGRES DE LIBERTE
!| KDIR           |---|
!| LIMTRA         |---|
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
!| MASKPT         |---|
!| MASSOU         |-->| MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
!|                |   | VOIR DIFSOU
!| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE.
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NPTFR          |---|
!| OPDTRA         |-->| MOT-CLE : OPTION POUR LA DIFFUSION DU TRACEUR
!| OPTSOU         |-->| OPTION DE TRAITEMENT DES TERMES SOURCES.
!|                |   | 1 : NORMAL
!|                |   | 2 : DIRAC
!| S              |-->| STRUCTURE BIDON
!| SM             |-->| TERMES SOURCES .
!| SMH            |-->| TERME SOURCE DE L'EQUATION DE CONTINUITE
!| SMI            |---|
!| SOLSYS         |---|
!| T5,T6,T7       |<->| TABLEAUX DE TRAVAIL
!| T8             |---|
!| TE1            |<->| TABLEAU DE TRAVAIL SUR LES ELEMENTS
!| UCONV,VCONV    |-->| TABLEAUX DE TRAVAIL.
!| UNSV2D         |---|
!| V2DPAR         |---|
!| VISC           |-->| COEFFICIENTS DE VISCOSITE SUIVANT X,Y ET Z .
!|                |   | SI P0 : VISCOSITE DONNEE PAR ELEMENT
!|                |   | SINON : VISCOSITE DONNEE PAR POINT
!| VISC_S         |---|
!| YAFLBOR        |---|
!| YASMH          |-->| LOGIQUE INDIQUANT DE PRENDRE EN COMPTE SMH
!| YASMI          |---|
!| ZCONV          |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CVTRVF => CVTRVF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: OPDTRA,OPTSOU,KDIR,NPTFR,SOLSYS
      INTEGER, INTENT(IN)             :: LIMTRA(NPTFR),KDDL,IOPT
      DOUBLE PRECISION, INTENT(IN)    :: DT,AGGLOH
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      LOGICAL, INTENT(IN)             :: BILAN,CONV,YASMH,YAFLBOR
      LOGICAL, INTENT(IN)             :: DIFT,MSK,ENTET,YASMI
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,H,HN,DM1,ZCONV,MASKPT
      TYPE(BIEF_OBJ), INTENT(IN)      :: V2DPAR,UNSV2D,HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: F,SM,HNT,HT
      TYPE(BIEF_OBJ), INTENT(IN)      :: FBOR,UCONV,VCONV,FN,SMI,SMH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TE1,FLBORTRA
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ), INTENT(IN)      :: FSCEXP,S,MASKTR,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: VISC_S,VISC
      TYPE(BIEF_MESH) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMF,I,IOPT1,IOPT2
      LOGICAL YACSTE
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION MASSET,MASSETN,TSOU,DTMAX,DT_REMAIN,DDT,TDT
      DOUBLE PRECISION FXT2
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVE_HT,SAVE_HNT
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: FXMAT,FXMATPAR
!
      DOUBLE PRECISION P_DMIN,P_DSUM
      EXTERNAL         P_DMIN,P_DSUM
!
      INTEGER NITMAX,NIT
      DATA NITMAX/200/
!
!-----------------------------------------------------------------------
!
      SAVE_HT =>HT%R
      SAVE_HNT=>HNT%R
      FXMAT=>MESH%MSEG%X%R(1:MESH%NSEG)
!     IN PARALLEL MODE, ASSEMBLED AND NON ASSEMBLED VERSIONS ARE DIFFERENT
      IF(NCSIZE.GT.1) THEN
        FXMATPAR=>MESH%MSEG%X%R(MESH%NSEG+1:2*MESH%NSEG)
      ELSE
        FXMATPAR=>MESH%MSEG%X%R(1:MESH%NSEG)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXTRACTS THE OPTIONS
!
      IOPT2=IOPT/10
      IOPT1=IOPT-10*IOPT2
!
!-----------------------------------------------------------------------
!
!     IELMF = F%ELM
!     FORCED TO LINEAR
      IELMF=11
!
!     TAKES MASS-LUMPING INTO ACCOUNT IN THE CONTINUITY EQUATION
!
      IF(ABS(1.D0-AGGLOH).GT.1.D-8) THEN
        CALL VECTOR(HT ,'=','MASVEC          ',IELMF,
     &              1.D0-AGGLOH,H ,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(HNT,'=','MASVEC          ',IELMF,
     &              1.D0-AGGLOH,HN,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(HT ,2,MESH)
          CALL PARCOM(HNT,2,MESH)
        ENDIF
        CALL OS('X=YZ    ',X=HT ,Y=HT ,Z=UNSV2D)
        CALL OS('X=YZ    ',X=HNT,Y=HNT,Z=UNSV2D)
        CALL OS('X=X+CY  ',X=HT ,Y=H  ,C=AGGLOH)
        CALL OS('X=X+CY  ',X=HNT,Y=HN ,C=AGGLOH)
      ELSE
!       CALL OS('X=Y     ',X=HT ,Y=H )
!       CALL OS('X=Y     ',X=HNT,Y=HN)
        HT%R =>H%R
        HNT%R=>HN%R
      ENDIF
!
!     INITIALISES THE TRACER FLUX AT THE BOUNDARY
!
      DO I=1,MESH%NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
!         FLBOR IS NOT ASSEMBLED IN PARALLEL MODE
          FLBORTRA%R(I)=FLBOR%R(I)*FBOR%R(I)
        ELSE
!         FOR KDDL, WILL BE DONE IN TVF
          FLBORTRA%R(I)=0.D0
        ENDIF
      ENDDO
!
!     COMPUTES THE FLUXES PHIIJ = FXMAT
!
      FORMUL='HUGRADP         '
      IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
      CALL VECTOR(T2,'=',FORMUL,IELMF,-1.D0,
     &            HPROP,DM1,ZCONV,UCONV,VCONV,VCONV,MESH,MSK,MASKEL)
!                 T2 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
!                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
      NIT=0
      DT_REMAIN=DT
      TDT=0.D0
      CALL CPSTVC(HN,T7)
      CALL CPSTVC(H ,T5)
      CALL CPSTVC(H ,T4)
      CALL CPSTVC(F,T8)
!
!     T4 WILL BE F PROGRESSIVELY UPDATED
!     T5 WILL BE THE DEPTH AT THE END OF THE SUB-TIMESTEP
!     (INITIALISED HERE TO CALL CFLVF)
!
      DO I=1,HN%DIM1
        T4%R(I)=FN%R(I)
        T5%R(I)=HNT%R(I)
      ENDDO
!
!     T1 WILL BE THE DEPTH ACCORDING TO THE CONTINUITY EQUATION
!
      IF(IOPT2.EQ.1) THEN
        DO I=1,HN%DIM1
          T1%R(I)=HNT%R(I)
        ENDDO
      ENDIF
!
      IF(.NOT.YAFLBOR) THEN
!       MASK=8 FOR LIQUID BOUNDARIES
        CALL VECTOR(T3,'=','FLUBDF          ',1,1.D0,HPROP,HPROP,HPROP,
     &              UCONV,VCONV,VCONV,MESH,.TRUE.,MASKTR%ADR(8)%P)
      ENDIF
!
100   CONTINUE
      NIT=NIT+1
!
!----------------------------------------
! VARIOUS OPTIONS TO COMPUTE THE FLUXES
!----------------------------------------
!
      IF(NIT.EQ.1.OR.IOPT1.EQ.3) THEN
        CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,IOPT1,T4)
!       CANCELS FLUXES TO AND FROM MASKED POINTS
        IF(MSK) THEN
          CALL FLUX_MASK(FXMAT,MESH%NSEG,
     &                   MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MASKPT%R)
        ENDIF
!       ASSEMBLES THE FLUXES AT INTERFACES IN PARALLEL MODE, THIS
!       IS FOR UPWINDING (STORED IN SECOND DIMENSION OF MESH%MSEG)
        IF(NCSIZE.GT.1) THEN
          CALL OV('X=Y     ',FXMATPAR,FXMAT,FXMAT,0.D0,MESH%NSEG)
          CALL PARCOM2_SEG(FXMATPAR,FXMATPAR,FXMATPAR,
     &                     MESH%NSEG,1,2,1,MESH,1)
        ENDIF
      ENDIF
!
!---------------------------------------------
! DETERMINES THE LARGEST ADMISSIBLE TIMESTEP
!---------------------------------------------
!
!     THIS COULD BE PUT OUTSIDE THE LOOP, BUT T7 USED LATER IN THE LOOP...
!
!     IN CFLVF, T7 WILL BE FLBOR WITH A DIMENSION NPOIN
      CALL OS('X=0     ',X=T7)
      IF(YAFLBOR) THEN
        CALL OSDB('X=Y     ',T7,FLBOR,FLBOR,0.D0,MESH)
      ELSE
        CALL OSDB('X=Y     ',T7,T3,T3,0.D0,MESH)
      ENDIF
      IF(NCSIZE.GT.1) CALL PARCOM(T7,2,MESH)
!
!     MASKS FLBOR IF(MSK)
!
      IF(MSK) CALL OS('X=XY    ',X=T7,Y=MASKPT)
!
!     COMPUTES THE MAXIMUM TIMESTEP ENSURING MONOTONICITY
!
      CALL CFLVF(DTMAX,T5%R,HT%R,FXMAT,FXMATPAR,
!                                   FLBOR%R(NPOIN)
     &           V2DPAR%R,DT_REMAIN,T7%R   ,SMH%R,
     &           YASMH,T8,MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &           MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MESH,MSK,MASKPT)
      IF(NCSIZE.GT.1) DTMAX=P_DMIN(DTMAX)
!
      DDT=MIN(DT_REMAIN,DTMAX)
      TDT=TDT+DDT
!
!     T5 WILL TAKE THE SUCCESSIVE VALUES OF H
!     AT THE END OF THE SUB-TIMESTEP
!
      DO I=1,HN%DIM1
        T5%R(I)=HNT%R(I)+TDT*(HT%R(I)-HNT%R(I))/DT
      ENDDO
!
!     IN TVF FACTOR HT/HLIN MAY TRIGGER DIVERGENCE FOR DRY POINTS
!
      IF(MSK) THEN
        DO I=1,HN%DIM1
          IF(MASKPT%R(I).LT.0.5D0) T5%R(I)=HT%R(I)
        ENDDO
      ENDIF
!
!-----------------
! FINAL RESOLUTION
!-----------------
!
      IF(YAFLBOR) THEN
        CALL TRACVF(F,FN,FSCEXP,HT,HNT,FXMAT,FXMATPAR,V2DPAR,UNSV2D,
     &              DDT,FLBOR,FBOR,SMH,YASMH,T1,T2,T4,T5,T6,T7,T8,
     &              MESH,LIMTRA,KDIR,KDDL,OPTSOU,IOPT2,FLBORTRA,MSK,
     &              NIT,DT,TDT)
      ELSE
        CALL TRACVF(F,FN,FSCEXP,HT,HNT,FXMAT,FXMATPAR,V2DPAR,UNSV2D,
     &              DDT,T3,FBOR,SMH,YASMH,T1,T2,T4,T5,T6,T7,T8,MESH,
     &              LIMTRA,KDIR,KDDL,OPTSOU,IOPT2,FLBORTRA,MSK,
     &              NIT,DT,TDT)
      ENDIF
!
      DO I=1,HN%DIM1
        T4%R(I)=F%R(I)
      ENDDO
      IF(IOPT2.EQ.1) THEN
        DO I=1,HN%DIM1
          T1%R(I)=T2%R(I)
        ENDDO
      ENDIF
!
      DT_REMAIN=DT_REMAIN-DDT
!
      IF(DT_REMAIN.NE.0.D0.AND.NIT.LT.NITMAX) GO TO 100
!
      IF(NIT.GE.NITMAX) THEN
        IF(LNG.EQ.1) WRITE(LU,900) NIT
        IF(LNG.EQ.2) WRITE(LU,901) NIT
900     FORMAT(1X,'CVTRVF : ',1I6,' SOUS-ITERATIONS DEMANDEES POUR LE'
     &   ,/,1X,   '         SCHEMA VF. DIMINUER LE PAS DE TEMPS')
901     FORMAT(1X,'CVTRVF: ',1I6,' SUB-ITERATIONS REQUIRED FOR THE'
     &   ,/,1X,   '         VF SCHEME. DECREASE THE TIME-STEP')
        CALL PLANTE(1)
        STOP
      ELSEIF(ENTET) THEN
        IF(LNG.EQ.1) WRITE(LU,902) NIT
        IF(LNG.EQ.2) WRITE(LU,903) NIT
902     FORMAT(1X,'CVTRVF (BIEF) : ',1I6,' SOUS-ITERATIONS')
903     FORMAT(1X,'CVTRVF (BIEF): ',1I6,' SUB-ITERATIONS')
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERM
!
      DO I = 1,MESH%NPOIN
        F%R(I) = F%R(I)+DT*SM%R(I)
      ENDDO
!
!     IMPLICIT SOURCE TERM
!
      IF(YASMI) THEN
        DO I = 1,MESH%NPOIN
          F%R(I) = F%R(I)/(1.D0-DT*SMI%R(I)/MAX(H%R(I),1.D-4))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     LOCAL MASS BALANCE (FOR CHECKING PURPOSES)
!
!     CALL OS('X=Y-Z   ',X=T7,Y=T5,Z=HT)
!     PRINT*,'DIFFERENCE ENTRE H RECALCULE ET H : ',DOTS(T7,T7)
!     CHECKS THE TRACER EQUATION
!     CALL CPSTVC(FBOR,T4)
!     T4 : F AT THE BOUNDARIES AS TAKEN FOR THE BOUNDARY FLUXES
!     DO I=1,NPTFR
!       IF(LIMTRA(I).EQ.KDIR) THEN
!         T4%R(I)=FBOR%R(I)
!       ELSE
!         T4%R(I)=FN%R(MESH%NBOR%I(I))
!       ENDIF
!     ENDDO
!     CALL OS('X=YZ    ',X=T6,Y=FN,Z=HNT)
!     CALL OS('X=YZ    ',X=T7,Y=F ,Z=HT )
!     MASSETN=P_DOTS(V2DPAR,T6,MESH)
!     MASSET =P_DOTS(V2DPAR,T7,MESH)
!     FXT2   =P_DOTS(FLBOR,T4,MESH)
!     PRINT*,'MASSE INIT: ',MASSETN,' MASSE FINALE: ',MASSET
!     PRINT*,'FLUX: ',FXT2
!     MASSETN = MASSETN - FXT2*DT
!     TSOU=0.D0
!     IF(YASMH) THEN
!       IF(OPTSOU.EQ.1) THEN
!         DO I=1,MESH%NPOIN
!           MASSETN=MASSETN
!    *             +DT*V2DPAR%R(I)*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
!           TSOU=TSOU+DT*V2DPAR%R(I)*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
!         ENDDO
!       ELSEIF(OPTSOU.EQ.2) THEN
!         DO I=1,MESH%NPOIN
!           MASSETN=MASSETN
!    *             +DT*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
!           TSOU=TSOU+DT*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
!         ENDDO
!       ENDIF
!     ENDIF
!     PRINT*,'CREATION PAR SOURCE : ',TSOU
!     PRINT*,'ERREUR DE MASSE DE TRACEUR VF : ',MASSETN-MASSET
!     CHECKS THE CONTINUITY EQUATION
!     DO I = 1,MESH%NPOIN
!       T5%R(I)=V2DPAR%R(I)*(HT%R(I)-HNT%R(I))
!     ENDDO
!     DO I = 1,MESH%NSEG
!       T5%R(MESH%GLOSEG%I(I)) =
!    *  T5%R(MESH%GLOSEG%I(I)) + DT*MESH%MSEG%X%R(I)
!       T5%R(MESH%GLOSEG%I(I+MESH%NSEG)) =
!    *  T5%R(MESH%GLOSEG%I(I+MESH%NSEG)) - DT*MESH%MSEG%X%R(I)
!     ENDDO
!     DO I = 1,MESH%NPTFR
!       T5%R(MESH%NBOR%I(I))=T5%R(MESH%NBOR%I(I))+DT*FLBOR%R(I)
!     ENDDO
!     IF(YASMH) THEN
!       IF(OPTSOU.EQ.1) THEN
!         DO I = 1,MESH%NPOIN
!           T5%R(I)=T5%R(I)-DT*V2DPAR%R(I)*SMH%R(I)
!         ENDDO
!       ELSEIF(OPTSOU.EQ.2) THEN
!         DO I = 1,MESH%NPOIN
!           T5%R(I)=T5%R(I)-DT*SMH%R(I)
!         ENDDO
!       ENDIF
!     ENDIF
!     MASSET=0.D0
!     MASSETN = 0.D0
!     DO I = 1,MESH%NPOIN
!       MASSET=MASSET+T5%R(I)
!       MASSETN=MAX(MASSETN,ABS(T5%R(I)))
!     ENDDO
!     PRINT*,'ERREUR DE MASSE GLOBALE : ',MASSET,' LOCALE : ',MASSETN
!
!-----------------------------------------------------------------------
!
!     RETURNS POINTERS HT AND HNT
!
      HT%R =>SAVE_HT
      HNT%R=>SAVE_HNT
!
!-----------------------------------------------------------------------
!
      RETURN
      END