C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       1) CHECKS THE COMPATIBILITY OF BOUNDARY CONDITIONS.
!><br>            2) FILLS ARRAYS LIMPRO AND MASK.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CLH, CLU, CLV, DEBLIQ, ENTET, FINLIQ, FRTYPE, GRAV, H, IELMU, KADH, KDDL, KDIR, KENT, KENTU, KINC, KLOG, KNEU, KOND, KP1BOR, KSORT, LIHBOR, LIMDIM, LIMPRO, LIUBOR, LIVBOR, LT, MASK, MASKEL, MESH, MSK, NBOR, NELBOR, NELMAX, NFRLIQ, NPOIN, NPTFR, THOMFR, U, V, XNEBOR, YNEBOR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALERTE1, ALERTE2, DEP, F2, F3, HDDL, HDIR, HNEU, HOND, IELEM, IFRLIQ, IGUILT1, IGUILT2, K, KP1, N, UDDL, UDIR, UNEU, UNONNEU, VDDL, VDIR, VNEU, YY
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 27/06/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CLH,CLU,CLV
!></td><td><-></td><td>TYPES DE CONDITIONS AUX LIMITES SUR H,U,V
!>                  RECOPIES DE LIHBOR,LIUBOR,LIVBOR
!>    </td></tr>
!>          <tr><td>DEBLIQ
!></td><td>--></td><td>NUMERO DU PREMIER POINT DE LA FRONTIERE LIQUID
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>--></td><td>SI OUI : MESSAGES IMPRIMES
!>                  SAUF MESSAGES D'ERREURS QUI TOUJOURS IMPRIMES
!>    </td></tr>
!>          <tr><td>FINLIQ
!></td><td>--></td><td>NUMERO DU DERNIER POINT DE LA FRONTIERE LIQUID
!>    </td></tr>
!>          <tr><td>FRTYPE
!></td><td>--></td><td>TYPE DE TRAITEMENT POUR LES FRONTIERES LIQUIDE
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>PESANTEUR
!>    </td></tr>
!>          <tr><td>IELMU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>INDICATEUR DE POINT DIRICHLET
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>--></td><td>INDICATEUR DE DEGRE DE LIBERTE AU BORD
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>INDICATEUR DE POINT DE DIRICHLET
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>INDICATEUR DE POINT D'ENTREE FLUIDE
!>    </td></tr>
!>          <tr><td>KENTU
!></td><td>--></td><td>INDICATEUR DE VITESSE IMPOSEE.
!>    </td></tr>
!>          <tr><td>KINC
!></td><td>--></td><td>INDICATEUR D'ONDE INCIDENTE
!>                  CONDITIONS AUX LIMITES TECHNIQUES:
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>INDICATEUR DE PAROI SOLIDE
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>INDICATEUR DE POINT DE NEUMANN
!>    </td></tr>
!>          <tr><td>KOND
!></td><td>--></td><td>INDICATEUR D'ONDE INCIDENTE
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>POINT SUIVANT SUR LA FRONTIERE.
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>INDICATEUR DE POINT DE SORTIE FLUIDE
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES SUR H
!>    </td></tr>
!>          <tr><td>LIMDIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td><--</td><td>TYPES DE CONDITIONS AUX LIMITES POUR LA
!>                  PROPAGATION
!>                  PAR POINTS   :    .1:H  .2:U  .3:V
!>                  PAR SEGMENTS :    .4:H  .5:U  .6:V
!>    </td></tr>
!>          <tr><td>LIUBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES SUR U
!>    </td></tr>
!>          <tr><td>LIVBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES SUR V
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DE L'ITERATION COURANTE.
!>    </td></tr>
!>          <tr><td>MASK
!></td><td><--</td><td>MASQUES POUR LES SEGMENTS
!>                  MASK(NPTFR,1) : 1. SI KDIR SUR U 0. SINON
!>                  MASK(NPTFR,2) : 1. SI KDIR SUR V 0. SINON
!>                  MASK(NPTFR,3) : 1. SI KDDL SUR U 0. SINON
!>                  MASK(NPTFR,4) : 1. SI KDDL SUR V 0. SINON
!>                  MASK(NPTFR,5) : 1. SI KNEU SUR U 0. SINON
!>                  MASK(NPTFR,6) : 1. SI KNEU SUR V 0. SINON
!>                  MASK(NPTFR,7) : 1. SI KOND 0. SINON
!>                  MASK(NPTFR,8) : 1. - MASK( ,5)
!>                  MASK(NPTFR,9) : 1. SI H DIRICHLET
!>                  MASK(NPTFR,10): 1. SI H NEUMANN
!>                  MASK(NPTFR,11): 1. SI H DEGRE DE LIBERTE
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>CORRESPONDANCE ENTRE NUMEROTATION DES
!>                  POINTS FRONTIERES ET NUMEROTATION GLOBALE
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS ADJACENTS AUX BORDS.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>--></td><td>NOMBRE DE FRONTIERES LIQUIDES
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE NOEUD DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>DIMENSION DES TABLEAUX.
!>                  CONDITIONS AUX LIMITES PHYSIQUES:
!>    </td></tr>
!>          <tr><td>THOMFR
!></td><td>--></td><td>TRAITEMENT PAR CARACTERISTIQUES DES FRONTIERES
!>                  LIQUIDES
!>    </td></tr>
!>          <tr><td>U,V, ,H
!></td><td>--></td><td>VALEURS DE U,V   ET H AU TEMPS T
!>    </td></tr>
!>          <tr><td>XNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PROPIN_TELEMAC2D
     &(LIMPRO,LIMDIM,MASK,LIUBOR,LIVBOR,LIHBOR,KP1BOR,NBOR,NPTFR,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,KNEU,KDIR,KDDL,KOND,
     & CLH,CLU,CLV,IELMU,U,V,GRAV,H,LT,NPOIN,NELBOR,NELMAX,MSK,MASKEL,
     & NFRLIQ,THOMFR,DEBLIQ,FINLIQ,FRTYPE,XNEBOR,YNEBOR,ENTET,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CLH,CLU,CLV    |<->| TYPES DE CONDITIONS AUX LIMITES SUR H,U,V
C|                |   | RECOPIES DE LIHBOR,LIUBOR,LIVBOR
C| DEBLIQ         |-->| NUMERO DU PREMIER POINT DE LA FRONTIERE LIQUID
C| ENTET          |-->| SI OUI : MESSAGES IMPRIMES
C|                |   | SAUF MESSAGES D'ERREURS QUI TOUJOURS IMPRIMES
C| FINLIQ         |-->| NUMERO DU DERNIER POINT DE LA FRONTIERE LIQUID
C| FRTYPE         |-->| TYPE DE TRAITEMENT POUR LES FRONTIERES LIQUIDE
C| GRAV           |-->| PESANTEUR
C| IELMU          |---| 
C| KADH           |-->| INDICATEUR DE POINT DIRICHLET
C| KDDL           |-->| INDICATEUR DE DEGRE DE LIBERTE AU BORD
C| KDIR           |-->| INDICATEUR DE POINT DE DIRICHLET
C| KENT           |-->| INDICATEUR DE POINT D'ENTREE FLUIDE
C| KENTU          |-->| INDICATEUR DE VITESSE IMPOSEE.
C| KINC           |-->| INDICATEUR D'ONDE INCIDENTE
C|                |   | CONDITIONS AUX LIMITES TECHNIQUES:
C| KLOG           |-->| INDICATEUR DE PAROI SOLIDE
C| KNEU           |-->| INDICATEUR DE POINT DE NEUMANN
C| KOND           |-->| INDICATEUR D'ONDE INCIDENTE
C| KP1BOR         |-->| POINT SUIVANT SUR LA FRONTIERE.
C| KSORT          |-->| INDICATEUR DE POINT DE SORTIE FLUIDE
C| LIHBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR H
C| LIMDIM         |---| 
C| LIMPRO         |<--| TYPES DE CONDITIONS AUX LIMITES POUR LA
C|                |   | PROPAGATION
C|                |   | PAR POINTS   :    .1:H  .2:U  .3:V
C|                |   | PAR SEGMENTS :    .4:H  .5:U  .6:V
C| LIUBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR U
C| LIVBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR V
C| LT             |-->| NUMERO DE L'ITERATION COURANTE.
C| MASK           |<--| MASQUES POUR LES SEGMENTS
C|                |   | MASK(NPTFR,1) : 1. SI KDIR SUR U 0. SINON
C|                |   | MASK(NPTFR,2) : 1. SI KDIR SUR V 0. SINON
C|                |   | MASK(NPTFR,3) : 1. SI KDDL SUR U 0. SINON
C|                |   | MASK(NPTFR,4) : 1. SI KDDL SUR V 0. SINON
C|                |   | MASK(NPTFR,5) : 1. SI KNEU SUR U 0. SINON
C|                |   | MASK(NPTFR,6) : 1. SI KNEU SUR V 0. SINON
C|                |   | MASK(NPTFR,7) : 1. SI KOND 0. SINON
C|                |   | MASK(NPTFR,8) : 1. - MASK( ,5)
C|                |   | MASK(NPTFR,9) : 1. SI H DIRICHLET
C|                |   | MASK(NPTFR,10): 1. SI H NEUMANN
C|                |   | MASK(NPTFR,11): 1. SI H DEGRE DE LIBERTE
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NBOR           |-->| CORRESPONDANCE ENTRE NUMEROTATION DES
C|                |   | POINTS FRONTIERES ET NUMEROTATION GLOBALE
C| NELBOR         |-->| NUMEROS DES ELEMENTS ADJACENTS AUX BORDS.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS.
C| NFRLIQ         |-->| NOMBRE DE FRONTIERES LIQUIDES
C| NPOIN          |-->| NOMBRE DE NOEUD DU MAILLAGE
C| NPTFR          |-->| DIMENSION DES TABLEAUX.
C|                |   | CONDITIONS AUX LIMITES PHYSIQUES:
C| THOMFR         |-->| TRAITEMENT PAR CARACTERISTIQUES DES FRONTIERES
C|                |   | LIQUIDES
C| U,V, ,H        |-->| VALEURS DE U,V   ET H AU TEMPS T
C| XNEBOR         |---| 
C| YNEBOR         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,N,IFRLIQ,IELEM,KP1
C
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
C
      DOUBLE PRECISION YY,F2,F3
C
      LOGICAL DEP,ALERTE1,ALERTE2
C
      INTEGER IGUILT1,IGUILT2
C
      INTRINSIC MAX
C
C-----------------------------------------------------------------------
C
      DO 1 K=1,NPTFR
        CLH(K) = LIHBOR(K)
        CLU(K) = LIUBOR(K)
        CLV(K) = LIVBOR(K)
1     CONTINUE
C
C SHORTCUT FOR THOMPSON TREATMENT
C
      IF(NFRLIQ.NE.0.AND.THOMFR) THEN
C
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
C         DOES NOT WORK IN PARALLEL; THEN AGAIN NEITHER DOES THOMPSON
          K = KP1BOR(K)
          GO TO 15
        ENDIF
16      CONTINUE
2     CONTINUE
C
      ENDIF
C
C  CHECKS AND MODIFIES THE CONDITIONS (IF REQUIRED) TO AVOID NON
C  PHYSICAL CASES : COMPLETELY FREE EXIT IN RIVER FLOW
C                   INCIDENT WAVE IN SUPERCRITICAL OUTGOING FLOW
C                   INCIDENT WAVE IN SUPERCRITICAL INCOMING FLOW
C
      ALERTE1=.FALSE.
      ALERTE2=.FALSE.
C
      DO 3 K=1,NPTFR
C
        N = NBOR(K)
        F2 = (U(N)**2+V(N)**2) / GRAV / MAX(H(N),1.D-8)
C
C       INCIDENT WAVE IN SUPERCRITICAL OUTGOING FLOW
C       INCIDENT WAVE IN SUPERCRITICAL INCOMING FLOW
C
        IF(CLU(K).EQ.KINC.AND.
     &     CLV(K).EQ.KINC.AND.
     &     F2.GE.1.D0) THEN
          CLU(K) = KSORT
          CLV(K) = KSORT
        ENDIF
C
C       COMPLETELY FREE EXIT IN RIVER FLOW
C
        IF(CLH(K).EQ.KSORT.AND.
     &     CLU(K).EQ.KSORT.AND.
     &     CLV(K).EQ.KSORT.AND.
     &     F2.LE.1.D0) THEN
          CLU(K) = KINC
          CLV(K) = KINC
        ENDIF
C
C       INCOMING FREE VELOCITY
C
        IF(CLU(K).EQ.KSORT.AND.CLV(K).EQ.KSORT) THEN
          F3 = U(N)*XNEBOR(K)+V(N)*YNEBOR(K)
          IF(F3.LE.-1.D-2) THEN
            ALERTE1=.TRUE.
            IGUILT1=K
          ENDIF
        ENDIF
C
C       SUPERCRITICAL INFLOW WITH FREE ELEVATION
C
        IF(CLH(K).EQ.KSORT.AND.F2.GE.1.D0) THEN
          F3 = U(N)*XNEBOR(K)+V(N)*YNEBOR(K)
          IF(F3.LE.-1.D-2) THEN
            ALERTE2=.TRUE.
            IGUILT2=K
          ENDIF
        ENDIF
C
3     CONTINUE
C
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
C
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
C
C-----------------------------------------------------------------------
C
C INITIALISES THE BOUNDARY CONDITIONS FOR PROPAGATION:
C
C     INITIALISES ALL THE VECTORS OF THE BLOCK TO 0
C
      CALL OS('X=0     ',X=MASK)
C
      DO 4 K=1,NPTFR
C
C     IF THE NODE FOLLOWING K IS NOT IN THE SUB-DOMAIN IN PARALLEL MODE
C     WILL HAVE KP1=K
      KP1=KP1BOR(K)
C
C-----------------------------------------------------------------------
C
C     BOUNDARY CONDITIONS ON ELEVATION
C
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
C
C   BOUNDARY CONDITIONS ON U
C
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
C
C   BOUNDARY CONDITIONS ON V
C
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
C
4     CONTINUE
C
C-----------------------------------------------------------------------
C
C     LIQUID BOUNDARIES MASK
C
      DO K=1,NPTFR
        KP1=KP1BOR(K)
        IF(KP1.NE.K) MASK%ADR(UNONNEU)%P%R(K)=1.D0-MASK%ADR(UNEU)%P%R(K)
      ENDDO
C
C     DEDUCES ARRAYS LIMPRO (. , 4 5 AND 6) FROM THE MASKS
C
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
C
C     SUPPLEMENT FOR QUADRATIC SPEEDS
C     THE POINT IN THE MIDDLE OF A SEGMENT HAS THE SAME CONDITION AS THE SEGMENT
C
      IF(IELMU.EQ.13) THEN
        DO K=1,NPTFR
          LIMPRO(K+NPTFR,2)=LIMPRO(K,5)
          LIMPRO(K+NPTFR,3)=LIMPRO(K,6)
        ENDDO
      ENDIF
C
C     MASKS USING THE MASK OF THE ELEMENTS
C
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
C
C-----------------------------------------------------------------------
C
10    FORMAT(1X,'PROPIN : POINT DE BORD',1I5,' CAS NON PREVU SUR H')
11    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR H')
20    FORMAT(1X,'PROPIN : POINT DE BORD',1I5,' CAS NON PREVU SUR U')
21    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR U')
30    FORMAT(1X,'PROPIN : POINT DE BORD',1I5,' CAS NON PREVU SUR V')
31    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR V')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C