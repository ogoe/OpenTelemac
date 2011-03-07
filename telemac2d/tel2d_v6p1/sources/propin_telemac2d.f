!                    ***************************
                     SUBROUTINE PROPIN_TELEMAC2D
!                    ***************************
!
     &(LIMPRO,LIMDIM,MASK,LIUBOR,LIVBOR,LIHBOR,KP1BOR,NBOR,NPTFR,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,KNEU,KDIR,KDDL,KOND,
     & CLH,CLU,CLV,IELMU,U,V,GRAV,H,LT,NPOIN,NELBOR,NELMAX,MSK,MASKEL,
     & NFRLIQ,THOMFR,DEBLIQ,FINLIQ,FRTYPE,XNEBOR,YNEBOR,ENTET,MESH)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    1) CHECKS THE COMPATIBILITY OF BOUNDARY CONDITIONS.
!+
!+            2) FILLS ARRAYS LIMPRO AND MASK.
!
!history  J-M HERVOUET (LNHE)
!+        27/06/2008
!+        V5P9
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
!| CLH,CLU,CLV    |<->| TYPES DE CONDITIONS AUX LIMITES SUR H,U,V
!|                |   | RECOPIES DE LIHBOR,LIUBOR,LIVBOR
!| DEBLIQ         |-->| NUMERO DU PREMIER POINT DE LA FRONTIERE LIQUID
!| ENTET          |-->| SI OUI : MESSAGES IMPRIMES
!|                |   | SAUF MESSAGES D'ERREURS QUI TOUJOURS IMPRIMES
!| FINLIQ         |-->| NUMERO DU DERNIER POINT DE LA FRONTIERE LIQUID
!| FRTYPE         |-->| TYPE DE TRAITEMENT POUR LES FRONTIERES LIQUIDE
!| GRAV           |-->| PESANTEUR
!| IELMU          |---| 
!| KADH           |-->| INDICATEUR DE POINT DIRICHLET
!| KDDL           |-->| INDICATEUR DE DEGRE DE LIBERTE AU BORD
!| KDIR           |-->| INDICATEUR DE POINT DE DIRICHLET
!| KENT           |-->| INDICATEUR DE POINT D'ENTREE FLUIDE
!| KENTU          |-->| INDICATEUR DE VITESSE IMPOSEE.
!| KINC           |-->| INDICATEUR D'ONDE INCIDENTE
!|                |   | CONDITIONS AUX LIMITES TECHNIQUES:
!| KLOG           |-->| INDICATEUR DE PAROI SOLIDE
!| KNEU           |-->| INDICATEUR DE POINT DE NEUMANN
!| KOND           |-->| INDICATEUR D'ONDE INCIDENTE
!| KP1BOR         |-->| POINT SUIVANT SUR LA FRONTIERE.
!| KSORT          |-->| INDICATEUR DE POINT DE SORTIE FLUIDE
!| LIHBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR H
!| LIMDIM         |---| 
!| LIMPRO         |<--| TYPES DE CONDITIONS AUX LIMITES POUR LA
!|                |   | PROPAGATION
!|                |   | PAR POINTS   :    .1:H  .2:U  .3:V
!|                |   | PAR SEGMENTS :    .4:H  .5:U  .6:V
!| LIUBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR U
!| LIVBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR V
!| LT             |-->| NUMERO DE L'ITERATION COURANTE.
!| MASK           |<--| MASQUES POUR LES SEGMENTS
!|                |   | MASK(NPTFR,1) : 1. SI KDIR SUR U 0. SINON
!|                |   | MASK(NPTFR,2) : 1. SI KDIR SUR V 0. SINON
!|                |   | MASK(NPTFR,3) : 1. SI KDDL SUR U 0. SINON
!|                |   | MASK(NPTFR,4) : 1. SI KDDL SUR V 0. SINON
!|                |   | MASK(NPTFR,5) : 1. SI KNEU SUR U 0. SINON
!|                |   | MASK(NPTFR,6) : 1. SI KNEU SUR V 0. SINON
!|                |   | MASK(NPTFR,7) : 1. SI KOND 0. SINON
!|                |   | MASK(NPTFR,8) : 1. - MASK( ,5)
!|                |   | MASK(NPTFR,9) : 1. SI H DIRICHLET
!|                |   | MASK(NPTFR,10): 1. SI H NEUMANN
!|                |   | MASK(NPTFR,11): 1. SI H DEGRE DE LIBERTE
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
!| MESH           |---| 
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NBOR           |-->| CORRESPONDANCE ENTRE NUMEROTATION DES
!|                |   | POINTS FRONTIERES ET NUMEROTATION GLOBALE
!| NELBOR         |-->| NUMEROS DES ELEMENTS ADJACENTS AUX BORDS.
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS.
!| NFRLIQ         |-->| NOMBRE DE FRONTIERES LIQUIDES
!| NPOIN          |-->| NOMBRE DE NOEUD DU MAILLAGE
!| NPTFR          |-->| DIMENSION DES TABLEAUX.
!|                |   | CONDITIONS AUX LIMITES PHYSIQUES:
!| THOMFR         |-->| TRAITEMENT PAR CARACTERISTIQUES DES FRONTIERES
!|                |   | LIQUIDES
!| XNEBOR         |---| 
!| YNEBOR         |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NELMAX,NPTFR,KOND,KENTU,LT,NFRLIQ
      INTEGER, INTENT(IN) :: KENT,KSORT,KADH,KLOG,KINC,KNEU,KDIR,KDDL
      INTEGER, INTENT(IN) :: LIMDIM,IELMU
      INTEGER, INTENT(IN) :: NELBOR(NPTFR),LIVBOR(NPTFR),LIHBOR(NPTFR)
      INTEGER, INTENT(IN) :: LIUBOR(NPTFR),FRTYPE(*)
      INTEGER, INTENT(IN) :: DEBLIQ(NFRLIQ),FINLIQ(NFRLIQ)
      INTEGER, INTENT(INOUT) :: LIMPRO(LIMDIM,6)
      INTEGER, INTENT(IN) :: KP1BOR(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: CLH(NPTFR),CLU(NPTFR),CLV(NPTFR)
      LOGICAL, INTENT(IN) :: MSK,THOMFR,ENTET
      DOUBLE PRECISION, INTENT(IN)   :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)   :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)   :: GRAV,MASKEL(NELMAX)
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: MASK
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,N,IFRLIQ,IELEM,KP1
!
      INTEGER, PARAMETER :: UDIR    =  1
      INTEGER, PARAMETER :: VDIR    =  2
      INTEGER, PARAMETER :: UDDL    =  3
      INTEGER, PARAMETER :: VDDL    =  4
      INTEGER, PARAMETER :: UNEU    =  5
      INTEGER, PARAMETER :: VNEU    =  6
      INTEGER, PARAMETER :: HOND    =  7
      INTEGER, PARAMETER :: UNONNEU =  8
      INTEGER, PARAMETER :: HDIR    =  9
      INTEGER, PARAMETER :: HNEU    = 10
      INTEGER, PARAMETER :: HDDL    = 11
!
      DOUBLE PRECISION YY,F2,F3
!
      LOGICAL DEP,ALERTE1,ALERTE2
!
      INTEGER IGUILT1,IGUILT2
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
      DO 1 K=1,NPTFR
        CLH(K) = LIHBOR(K)
        CLU(K) = LIUBOR(K)
        CLV(K) = LIVBOR(K)
1     CONTINUE
!
! SHORTCUT FOR THOMPSON TREATMENT
!
      IF(NFRLIQ.NE.0.AND.THOMFR) THEN
!
      DO 2 IFRLIQ = 1 , NFRLIQ
        DEP=.FALSE.
        K = DEBLIQ(IFRLIQ)
15      IF(FRTYPE(IFRLIQ).EQ.2.AND.H(NBOR(K)).GT.1.D-3) THEN
          CLH(K) = KENT
          CLU(K) = KENTU
          CLV(K) = KENTU
        ENDIF
        IF(K.EQ.FINLIQ(IFRLIQ).AND.DEP) THEN
          GO TO 16
        ELSE
          DEP=.TRUE.
!         DOES NOT WORK IN PARALLEL; THEN AGAIN NEITHER DOES THOMPSON
          K = KP1BOR(K)
          GO TO 15
        ENDIF
16      CONTINUE
2     CONTINUE
!
      ENDIF
!
!  CHECKS AND MODIFIES THE CONDITIONS (IF REQUIRED) TO AVOID NON
!  PHYSICAL CASES : COMPLETELY FREE EXIT IN RIVER FLOW
!                   INCIDENT WAVE IN SUPERCRITICAL OUTGOING FLOW
!                   INCIDENT WAVE IN SUPERCRITICAL INCOMING FLOW
!
      ALERTE1=.FALSE.
      ALERTE2=.FALSE.
!
      DO 3 K=1,NPTFR
!
        N = NBOR(K)
        F2 = (U(N)**2+V(N)**2) / GRAV / MAX(H(N),1.D-8)
!
!       INCIDENT WAVE IN SUPERCRITICAL OUTGOING FLOW
!       INCIDENT WAVE IN SUPERCRITICAL INCOMING FLOW
!
        IF(CLU(K).EQ.KINC.AND.
     &     CLV(K).EQ.KINC.AND.
     &     F2.GE.1.D0) THEN
          CLU(K) = KSORT
          CLV(K) = KSORT
        ENDIF
!
!       COMPLETELY FREE EXIT IN RIVER FLOW
!
        IF(CLH(K).EQ.KSORT.AND.
     &     CLU(K).EQ.KSORT.AND.
     &     CLV(K).EQ.KSORT.AND.
     &     F2.LE.1.D0) THEN
          CLU(K) = KINC
          CLV(K) = KINC
        ENDIF
!
!       INCOMING FREE VELOCITY
!
        IF(CLU(K).EQ.KSORT.AND.CLV(K).EQ.KSORT) THEN
          F3 = U(N)*XNEBOR(K)+V(N)*YNEBOR(K)
          IF(F3.LE.-1.D-2) THEN
            ALERTE1=.TRUE.
            IGUILT1=K
          ENDIF
        ENDIF
!
!       SUPERCRITICAL INFLOW WITH FREE ELEVATION
!
        IF(CLH(K).EQ.KSORT.AND.F2.GE.1.D0) THEN
          F3 = U(N)*XNEBOR(K)+V(N)*YNEBOR(K)
          IF(F3.LE.-1.D-2) THEN
            ALERTE2=.TRUE.
            IGUILT2=K
          ENDIF
        ENDIF
!
3     CONTINUE
!
      IF(ALERTE1.AND.ENTET) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'PROBLEME MAL POSE, VITESSE LIBRE ENTRANTE'
          WRITE(LU,*) 'PAR EXEMPLE AU POINT DE BORD NUMERO ',IGUILT1
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'ILL-POSED PROBLEM, ENTERING FREE VELOCITY'
          WRITE(LU,*) 'FOR EXAMPLE AT BOUNDARY POINT NUMBER ',IGUILT1
        ENDIF
      ENDIF
!
      IF(ALERTE2.AND.ENTET) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'PROBLEME MAL POSE, HAUTEUR LIBRE'
          WRITE(LU,*) 'SUR FRONTIERE AVEC VITESSE ENTRANTE'
          WRITE(LU,*) 'ET ECOULEMENT TORRENTIEL'
          WRITE(LU,*) 'PAR EXEMPLE AU POINT DE BORD NUMERO ',IGUILT2
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'ILL-POSED PROBLEM, FREE DEPTH'
          WRITE(LU,*) 'ON BOUNDARY WITH ENTERING VELOCITY'
          WRITE(LU,*) 'AND SUPERCRITICAL FLOW'
          WRITE(LU,*) 'FOR EXAMPLE AT BOUNDARY POINT NUMBER ',IGUILT2
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISES THE BOUNDARY CONDITIONS FOR PROPAGATION:
!
!     INITIALISES ALL THE VECTORS OF THE BLOCK TO 0
!
      CALL OS('X=0     ',X=MASK)
!
      DO 4 K=1,NPTFR
!
!     IF THE NODE FOLLOWING K IS NOT IN THE SUB-DOMAIN IN PARALLEL MODE
!     WILL HAVE KP1=K
      KP1=KP1BOR(K)
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITIONS ON ELEVATION
!
      IF(CLH(K).EQ.KENT) THEN
        LIMPRO(K,1) = KDIR
        IF(KP1.NE.K) THEN
          IF(CLH(KP1).EQ.KENT) THEN
            MASK%ADR(HDIR)%P%R(K)=1.D0
          ELSEIF(CLH(KP1).EQ.KLOG) THEN
            MASK%ADR(HNEU)%P%R(K)=1.D0
          ELSEIF(CLH(KP1).EQ.KSORT) THEN
            MASK%ADR(HDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,10) K
            IF(LNG.EQ.2) WRITE(LU,11) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLH(K).EQ.KSORT) THEN
        LIMPRO(K,1) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLH(KP1).EQ.KSORT) THEN
            MASK%ADR(HDDL)%P%R(K)=1.D0
          ELSEIF(CLH(KP1).EQ.KLOG) THEN
            MASK%ADR(HNEU)%P%R(K)=1.D0
          ELSEIF(CLH(KP1).EQ.KENT) THEN
            MASK%ADR(HDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,10) K
            IF(LNG.EQ.2) WRITE(LU,11) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLH(K).EQ.KLOG ) THEN
        LIMPRO(K,1) = KNEU
        IF(KP1.NE.K) MASK%ADR(HNEU)%P%R(K)=1.D0
      ELSE
        IF(LNG.EQ.1) WRITE(LU,10) K
        IF(LNG.EQ.2) WRITE(LU,11) K
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   BOUNDARY CONDITIONS ON U
!
      IF(CLU(K).EQ.KENT.OR.CLU(K).EQ.KENTU) THEN
        LIMPRO(K,2) = KDIR
        IF(KP1.NE.K) THEN
          IF(CLU(KP1).EQ.KENT.OR.CLU(KP1).EQ.KENTU) THEN
            MASK%ADR(UDIR)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KADH) THEN
            MASK%ADR(UDIR)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KLOG) THEN
            MASK%ADR(UNEU)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KSORT) THEN
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,20) K
            IF(LNG.EQ.2) WRITE(LU,21) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLU(K).EQ.KADH) THEN
        LIMPRO(K,2) = KDIR
        IF(KP1.NE.K) MASK%ADR(UNEU)%P%R(K)=1.D0
      ELSEIF(CLU(K).EQ.KSORT) THEN
        LIMPRO(K,2) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLU(KP1).EQ.KSORT) THEN
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KLOG) THEN
            MASK%ADR(UNEU)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KENT) THEN
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KENTU) THEN
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KADH) THEN
            MASK%ADR(UNEU)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,20) K
            IF(LNG.EQ.2) WRITE(LU,21) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLU(K).EQ.KLOG) THEN
        LIMPRO(K,2) = KDDL
        IF(KP1.NE.K) MASK%ADR(UNEU)%P%R(K)=1.D0
      ELSEIF(CLU(K).EQ.KINC ) THEN
        LIMPRO(K,2) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLU(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KLOG) THEN
            MASK%ADR(UNEU)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KSORT) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KENT) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KENTU) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KADH) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,20) K
            IF(LNG.EQ.2) WRITE(LU,21) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,20) K
        IF(LNG.EQ.2) WRITE(LU,21) K
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   BOUNDARY CONDITIONS ON V
!
      IF(CLV(K).EQ.KENT.OR.CLV(K).EQ.KENTU) THEN
        LIMPRO(K,3) = KDIR
        IF(KP1.NE.K) THEN
          IF(CLV(KP1).EQ.KENT.OR.CLV(KP1).EQ.KENTU) THEN
            MASK%ADR(VDIR)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KADH) THEN
            MASK%ADR(VDIR)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KLOG) THEN
            MASK%ADR(VNEU)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KSORT) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,30) K
            IF(LNG.EQ.2) WRITE(LU,31) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLV(K).EQ.KADH) THEN
        LIMPRO(K,3) = KDIR
        IF(KP1.NE.K) MASK%ADR(VNEU)%P%R(K)=1.D0
      ELSEIF(CLV(K).EQ.KSORT) THEN
        LIMPRO(K,3) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLV(KP1).EQ.KSORT) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KLOG) THEN
            MASK%ADR(VNEU)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KENT) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KENTU) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KADH) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,30) K
            IF(LNG.EQ.2) WRITE(LU,31) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLV(K).EQ.KLOG ) THEN
        LIMPRO(K,3) = KDDL
        IF(KP1.NE.K) MASK%ADR(VNEU)%P%R(K)=1.D0
      ELSEIF(CLV(K).EQ.KINC ) THEN
        LIMPRO(K,3) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLV(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KLOG) THEN
            MASK%ADR(VNEU)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KSORT) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KENT) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KENTU) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KADH) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,30) K
            IF(LNG.EQ.2) WRITE(LU,31) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,30) K
        IF(LNG.EQ.2) WRITE(LU,31) K
        CALL PLANTE(1)
        STOP
      ENDIF
!
4     CONTINUE
!
!-----------------------------------------------------------------------
!
!     LIQUID BOUNDARIES MASK
!
      DO K=1,NPTFR
        KP1=KP1BOR(K)
        IF(KP1.NE.K) MASK%ADR(UNONNEU)%P%R(K)=1.D0-MASK%ADR(UNEU)%P%R(K)
      ENDDO
!
!     DEDUCES ARRAYS LIMPRO (. , 4 5 AND 6) FROM THE MASKS
!
      DO K=1,NPTFR
        IF(MASK%ADR(HDIR)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,4)=KDIR
        ELSEIF(MASK%ADR(HDDL)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,4)=KDDL
        ELSEIF(MASK%ADR(HNEU)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,4)=KNEU
        ELSEIF(MASK%ADR(HOND)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,4)=KOND
        ELSE
          IF(NCSIZE.GT.1) THEN
            LIMPRO(K,4)=0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,10) K
            IF(LNG.EQ.2) WRITE(LU,11) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(MASK%ADR(UDIR)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,5)=KDIR
        ELSEIF(MASK%ADR(UDDL)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,5)=KDDL
        ELSEIF(MASK%ADR(UNEU)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,5)=KNEU
        ELSEIF(MASK%ADR(HOND)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,5)=KOND
        ELSE
          IF(NCSIZE.GT.1) THEN
            LIMPRO(K,5)=0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,20) K
            IF(LNG.EQ.2) WRITE(LU,21) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(MASK%ADR(VDIR)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,6)=KDIR
        ELSEIF(MASK%ADR(VDDL)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,6)=KDDL
        ELSEIF(MASK%ADR(VNEU)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,6)=KNEU
        ELSEIF(MASK%ADR(HOND)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,6)=KOND
        ELSE
          IF(NCSIZE.GT.1) THEN
            LIMPRO(K,6)=0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,30) K
            IF(LNG.EQ.2) WRITE(LU,31) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDDO
!
!     SUPPLEMENT FOR QUADRATIC SPEEDS
!     THE POINT IN THE MIDDLE OF A SEGMENT HAS THE SAME CONDITION AS THE SEGMENT
!
      IF(IELMU.EQ.13) THEN
        DO K=1,NPTFR
          LIMPRO(K+NPTFR,2)=LIMPRO(K,5)
          LIMPRO(K+NPTFR,3)=LIMPRO(K,6)
        ENDDO
      ENDIF
!
!     MASKS USING THE MASK OF THE ELEMENTS
!
      IF(MSK) THEN
        DO K=1,NPTFR
          IELEM=NELBOR(K)
          IF(IELEM.GT.0) THEN
            YY = MASKEL(IELEM)
            MASK%ADR(UDIR   )%P%R(K) = MASK%ADR(UDIR   )%P%R(K) * YY
            MASK%ADR(VDIR   )%P%R(K) = MASK%ADR(VDIR   )%P%R(K) * YY
            MASK%ADR(UDDL   )%P%R(K) = MASK%ADR(UDDL   )%P%R(K) * YY
            MASK%ADR(VDDL   )%P%R(K) = MASK%ADR(VDDL   )%P%R(K) * YY
            MASK%ADR(UNEU   )%P%R(K) = MASK%ADR(UNEU   )%P%R(K) * YY
            MASK%ADR(VNEU   )%P%R(K) = MASK%ADR(VNEU   )%P%R(K) * YY
            MASK%ADR(HOND   )%P%R(K) = MASK%ADR(HOND   )%P%R(K) * YY
            MASK%ADR(HDIR   )%P%R(K) = MASK%ADR(HDIR   )%P%R(K) * YY
            MASK%ADR(HNEU   )%P%R(K) = MASK%ADR(HNEU   )%P%R(K) * YY
            MASK%ADR(HDDL   )%P%R(K) = MASK%ADR(HDDL   )%P%R(K) * YY
            MASK%ADR(UNONNEU)%P%R(K) = MASK%ADR(UNONNEU)%P%R(K) * YY
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
10    FORMAT(1X,'PROPIN : POINT DE BORD',1I5,' CAS NON PREVU SUR H')
11    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR H')
20    FORMAT(1X,'PROPIN : POINT DE BORD',1I5,' CAS NON PREVU SUR U')
21    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR U')
30    FORMAT(1X,'PROPIN : POINT DE BORD',1I5,' CAS NON PREVU SUR V')
31    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR V')
!
!-----------------------------------------------------------------------
!
      RETURN
      END