!                    *****************
                     SUBROUTINE RESOLU
!                    *****************
!
     & (W,FLUSCE,NUBO,VNOIN,WINF,AT,DT,LT,NIT,
     &  NELEM,NSEG,NPTFR,FLUX,AIRS,AIRE,
     &  X,Y,IKLE,ZF,CF,NPOIN,HN,H,U,V,QU,QV,G,LISTIN,XNEBOR,
     &  YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL,
     &  HBOR,UBOR,VBOR,FLUSORT,FLUENT,CFLWTD,DTVARI,NELMAX,KFROT,
     &  NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,YASMH,SMH,MASSES,
     &  NTRAC,DIMT,T,HTN,TN,DLIMT,LIMTRA,
     &  TBOR,MASSOU,FLUTENT,FLUTSOR,DTHAUT,DPX,DPY,DJX,DJY,CMI,JMI,
     &  SMTR,DXT,DYT,DJXT,DJYT,
     &  DIFVIT,ITURB,PROPNU,DIFT,DIFNU,
     &  DX,DY,OPTVF,FLUSORTN,FLUENTN,
     &  DSZ,AIRST,HSTOK,HCSTOK,FLUXT,FLUHBOR,FLBOR,
     &  LOGFR,LTT,DTN,FLUXTEMP,FLUHBTEMP,
     &  HC,TMAX,DTT,T1,T2,T3,T4,T5)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    1. SOLVES THE PROBLEM BY A METHOD OF TYPE ROE FOR
!+               INTERIOR FLUXES AND OF TYPE STEGER AND WARMING FOR I/O;
!+               OR BY A KINETIC SCHEME (ORDER 1 OR 2).
!+
!+            2. SOLVES IN TIME USING AN EULER TYPE SCHEME.
!
!history  N.GOUTAL; INRIA
!+        22/03/1998
!+
!+   ROE SCHEME (NG); KINETIC SCHEMES (INRIA)
!
!history  J-M HERVOUET (LNHE)
!+        05/09/2007
!+
!+   MULTIPLE TRACERS
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
!| AIRE           |-->| AIRES DES ELEMENTS
!| AIRS           |-->| AIRES DES CELLULES DU MAILLAGE
!| AIRST          |-->| AIRES DES SOUS-TRIANGLES DANS CELLULES
!| AT,DT,LT       |-->| TEMPS, PAS DE TEMPS, NUMERO DU PAS
!| CF             |-->| COEFFICIENT DE FROTTEMENT
!| CFLWTD         |-->| NOMBRE DE CFL
!| CMI            |-->| COORDONNEES DES POINTS MILIEUX D'INTERFACE
!| DIFNU          |-->| COEFFICIENT DE DIFFUSION DU TRACEUR
!| DIFT           |-->| LOGIQUE INDIQUANT S'IL Y A DIFFUSION TRACEUR
!| DIFVIT         |-->| INDIQUE S'IL FAUT FAIRE LA DIFFUSION DE U,V
!| DIMT           |-->| DIMENSION DU TRACEUR
!| DJXT,DJYT      |---| TABLEAUX DE TRAVAIL POUR TRACEUR
!| DLIMT          |-->| DIMENSION DU TRACEUR AU BORD
!| DSZ            |<->| VARIATION DE Z POUR ORDRE 2
!| DTHAUT         |-->| UTILISE POUR CONDITION CFL
!| DTN            |<->| PAS DE TEMPS   DE TN+1 A TN+2
!| DTT            |<->| PAS DE TEMPS TRACEUR
!| DTVARI         |---|
!| DX,DY          |---| TABLEAUX DE TRAVAIL
!| DXT,DYT        |---| TABLEAUX DE TRAVAIL POUR TRACEUR
!| FLUENT,FLUSORT |<--| FLUX MASSE ENTREE ET SORTIE DE TN A TN+1
!| FLUHBTEMP      |<->| FLUX BORD POUR TRACEUR
!| FLUSCE         |---|
!| FLUSORTN,FLUENT|<->| FLUX MASSE ENTREE ET SORTIE DE TN+1 A TN+2
!| FLUTENT,FLUTSOR|<--| FLUX TRACEUR ENTREE ET SORTIE
!| FLUX           |---| TABLEAU DE TRAVAIL
!| FLUXT,FLUHBOR  |<->| FLUX, FLUX BORD ACCUMULES POUR TRACEUR
!| FLUXTEMP       |<->| FLUX POUR TRACEUR
!| G              |-->| CONSTANTE DE GRAVITE
!| H              |<--| HAUTEURS D'EAU AU TEMPS N+1
!| HBOR           |-->| VALEURS IMPOSEES DE H
!| HC             |<->| H RECONSTRUIT ORDRE 2   CORRIGE
!| HN             |-->| HAUTEURS D'EAU AU TEMPS N
!| HSTOK,HCSTOK   |<->| H, H CORRIGE  A STOCKER POUR TRACEUR
!| HTN,TN         |-->| HT, T  AU TEMPS N
!| IKLE           |-->| NUMEROS DES NOEUDS PAR TRIANGLE
!| ISCE           |-->| POINTS SOURCES
!| ITURB          |-->| MODELE DE TURBULENCE  1 : LAMINAIRE
!| JMI            |-->| NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
!|                |   | POINT MILIEU DE L'INTERFACE
!| KDDL           |-->| CONVENTION POUR LES POINTS LIBRES
!| KDIR           |-->| CONVENTION POUR LES POINTS DIRICHLET
!| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND
!| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN
!| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
!| LIMTRA         |-->| TYPES DE CONDITIONS AUX LIMITES SUR TRACEUR
!| LISTIN         |-->| SI OUI, MESSAGES IMPRIMES SUR LISTING.
!| LOGFR          |<->| REFERENCE DES NOEUDS FRONTIERE
!| LTT            |<->| NOMBRE DE PAS DE TEMPS TRACEUR
!| MASSES         |<--| MASSE AJOUTEE PAR TERME SOURCE
!| MASSOU         |<--| MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
!| MAXSCE         |---|
!| MAXTRA         |---|
!| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
!| NELEM          |-->| NOMBRE D'ELEMENTS
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
!| NIT            |-->| NOMBRE TOTAL DE PAS DE TEMPS
!| NPOIN          |-->| NOMBRE DE NOEUDS DU MAILLAGE
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
!| NREJET         |-->| NOMBRE DE SOURCES/PUITS
!| NSEG           |-->| NOMBRE D'ARETES
!| NTRAC          |---|
!| NUBO           |-->| NUMEROS GLOBAUX DES EXTREMITES DES ARETES
!| OPTVF          |-->| OPTION SCHEMA
!|                |   | 0:ROE, 1:CINETIQUE ORDRE 1,2:CINETIQUE ORDRE 2
!| PROPNU         |-->| COEFFICIENT DE DIFFUSION MOLECULAIRE
!| QU,QV          |<->| COMPOSANTES DU DEBIT AU TEMPS N PUIS  N+1
!| SMH            |-->| TERMES SOURCES DE L'EQUATION DE CONTINUITE
!| SMTR           |---| TERMES SOURCES DU TRACEUR
!| T              |<--| TRACEUR MIS A JOUR
!| T1,T2,T3,T4,T5 |---| TABLEAUX DE TRAVAIL
!| TBOR           |-->| CONDITIONS AUX LIMITES SUR T
!| TMAX           |-->| TEMPS DE FIN DU CALCUL
!| TSCE2          |---|
!| U,V            |<--| COMPOSANTES DE LA VITESSE AU TEMPS N+1
!| UBOR           |-->| VALEURS IMPOSEES DE U
!| VBOR           |-->| VALEURS IMPOSEES DE V
!| VNOIN          |-->| NORMALE A L'INTERFACE
!|                |   | (2 PREMIERES COMPOSANTES) ET
!|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!| W              |<->| TABLEAU DE TRAVAIL
!| WINF           |---|
!| X,Y            |-->| COORDONNEES DES NOEUDS DU MAILLAGE
!| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
!| YASMH          |-->| INDIQUE SI ON PREND EN COMPTE SMH
!| ZF             |-->| COTES DU FOND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_RESOLU => RESOLU
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN,NSEG,NPTFR,LT,NIT,NREJET,DIMT
      INTEGER, INTENT(IN) :: MAXSCE,MAXTRA
      INTEGER, INTENT(IN) :: DLIMT,OPTVF,JMI(*)
      INTEGER, INTENT(IN) :: KDIR,KNEU,KDDL,ITURB,NELMAX,KFROT,NTRAC
      INTEGER, INTENT(IN) :: NUBO(2,*),LIMPRO(NPTFR,6),NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3),ISCE(NREJET),LIMTRA(DLIMT)
      INTEGER, INTENT(INOUT) :: LTT,LOGFR(*)
!
      LOGICAL, INTENT(IN) :: LISTIN,DTVARI,YASMH,DIFVIT,DIFT
      DOUBLE PRECISION, INTENT(INOUT) :: T1(*),T2(*),T3(*),T4(*),T5(*)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(IN)    :: AT,VNOIN(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: TSCE2(MAXSCE,MAXTRA)
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN),FLUSORTN,FLUENTN
      DOUBLE PRECISION, INTENT(IN)    :: AIRE(NPOIN),DTHAUT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),UBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: VBOR(NPTFR),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SMH(NPOIN),ZF(NPOIN),CF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NELMAX),DPY(3,NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: WINF(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),AIRS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSCE(3,NPOIN),FLUX(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSORT,FLUENT,MASSES
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFLWTD,AIRST(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: HSTOK(*),HCSTOK(2,*),DTT
      DOUBLE PRECISION, INTENT(INOUT) :: CMI(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: PROPNU,DIFNU,TMAX
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,NELMAX),DJY(3,NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NPOIN),DY(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(NELMAX),DJYT(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: DXT(NPOIN),DYT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: HC(2,NSEG),DTN
!
      TYPE(BIEF_OBJ) , INTENT(IN)     :: TBOR,TN
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: T,HTN,SMTR,FLUHBOR,FLUHBTEMP
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUXTEMP,FLUXT,FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IS,K,ICIN,IVIS,NORDRE,ITRAC,FCHOICE
!
      DOUBLE PRECISION XNC,W1,EPS,DMIN,BETA,TEST
!
!-----------------------------------------------------------------------
!
      IF(OPTVF.EQ.0) THEN
        ICIN = 0
        NORDRE = 1
      ELSEIF(OPTVF.EQ.1) THEN
        ICIN = 1
        NORDRE = 1
      ELSEIF(OPTVF.EQ.2) THEN
        ICIN = 1
        NORDRE = 2
      ELSEIF(OPTVF.EQ.3) THEN
        ICIN =2
        NORDRE=1
      ELSEIF(OPTVF.EQ.4) THEN
        ICIN =3
        NORDRE=1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'SCHEMA INCONNU : ',OPTVF
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN SCHEME: ',OPTVF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     VALEUR LIMITE POUR LE CLIPPING
!
      EPS =  1.D-6
! IF NOT CFL VARIABLE WE IMPOSE CFL=0.5
!      IF(.NOT.DTVARI)CFLWTD=0.5D0
!
!     RECOPIE DES CONDITIONS AUX LIMITES
!
!------
! 1. CALCUL DE L'ETAT LIMITE
!------
!
!  * WINF CONTIENT WINF INITIALES CALCULEES DANS BORD
!
      DO 110 K=1,NPTFR
        IF(LIMPRO(K,1).EQ.KDIR) THEN
          WINF(1,K) =  HBOR(K)
          WINF(2,K) =  HBOR(K)*UBOR(K)
          WINF(3,K) =  HBOR(K)*VBOR(K)
        ELSE
          WINF(1,K) =  H(NBOR(K))
          WINF(2,K) =  H(NBOR(K))*UBOR(K)
          WINF(3,K) =  H(NBOR(K))*VBOR(K)
        ENDIF
110   CONTINUE
!
       IF(ICIN .EQ.0) THEN
!-----------------------------------------------------------------------
!        SCHEMA DE ROE
!-----------------------------------------------------------------------
!
      IF(LT.EQ.1) THEN
       WRITE(LU,*) ' '
       WRITE(LU,*) '          ***************** '
       WRITE(LU,*) '          * SCHEMA DE ROE * '
       WRITE(LU,*) '          ***************** '
       WRITE(LU,*) ' '
           ENDIF
!
!     RECOPIE DES VARIABLES DANS W
!
      DO 111 I=1,NPOIN
        W(1,I)= HN(I)
        W(2,I)= QU(I)
        W(3,I)= QV(I)
111   CONTINUE
!
! CALCUL DU DT QUI SATISFAIT CFL
!      CALL VFCFL(NUBO,VNOIN,NSEG,NPOIN,X,Y,G,H,EPS,QU,QV,DT,CFLWTD,
!     *           DTVARI,LISTIN)
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,CFLWTD,ICIN,DTVARI,LISTIN)
      DT = MIN(DT,TMAX-AT)
!
!  * WINF CONTIENT WINF INITIALES CALCULEES DANS BORD
!     WINF CONTIENT LES VALEURS AU BORD APRES UTILISATION
!       DES INVARIANTS DE RIEMAMM
      CALL FLUSEW(WINF,UBOR,VBOR,NPOIN,EPS,G,W,
     &            XNEBOR,YNEBOR,NPTFR,LIMPRO,NBOR,KDIR,KNEU,KDDL)
!
!
      CALL FLUROE(W,FLUSCE,NUBO,VNOIN,
     &            WINF,FLUX,FLUSORT,FLUENT,NELEM,NSEG,NPTFR,
     &            NPOIN,X,Y,AIRS,ZF,EPS,DMIN,G,
     &            XNEBOR,YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL,FLBOR)
!
! ---> INTEGRATION EN TEMPS
!      --------------------
!
      CALL INTEMP(W,FLUX,AIRS,DT,NPOIN,ZF,CF,EPS,DMIN,KFROT,SMH)
!
!  CALCUL DU VOLUME AJOUTE PAR LES SOURCES
!
      IF(YASMH) THEN
        MASSES=0.D0
        DO I=1,NPOIN
          MASSES = MASSES + SMH(I)
        ENDDO
        MASSES = DT * MASSES
      ENDIF
!
      DO  I=1,NPOIN
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
!
!       CALCUL DE U,V AJOUTE PAR JMH
!
        IF (H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I)
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
      ENDDO
!
! BEGIN RA
!
        DO K=1,NPTFR
!               IF(LIMPRO(K,1).EQ.KDIR) THEN
!                H(NBOR(K)) =  HBOR(K)
!               ENDIF
                U(NBOR(K)) =  UBOR(K)
          	V(NBOR(K)) =  VBOR(K)
        ENDDO
!
!END RA
!
      XNC = 0.D0
      DO I=1,NPOIN
         IF(H(I).GT.EPS) THEN
          W1=SQRT((QU(I)/H(I))**2+(QV(I)/H(I))**2)+SQRT(G*H(I))
          IF(W1.GE.XNC) XNC = W1
          IF(W1.GE.50.D0) THEN
            QU(I) = 0.D0
            QV(I) = 0.D0
          ENDIF
         ENDIF
      ENDDO
!
      ELSE IF(ICIN.EQ.1) THEN
!    *****************************
!
!-----------------------------------------------------------------------
!             SCHEMA CINETIQUE
!-----------------------------------------------------------------------
!
      IVIS=0
      IF(DIFVIT.AND.ITURB.EQ.1) IVIS=1
!
      IF(LT.EQ.1) THEN
!
!             INITIALISATIONS AU 1ER PAS DE TEMPS
!             ***********************************
!
       WRITE(LU,*) ' '
       WRITE(LU,*) '          ******************** '
       WRITE(LU,*) '          * SCHEMA CINETIQUE * '
       WRITE(LU,*) '          ******************** '
       WRITE(LU,*) ' '
!
!    CALCUL DU GRADIENT DE ZF
!
       IF(NORDRE.EQ.2) THEN
      CALL GRADZ(NPOIN,NELMAX,NSEG,IKLE,NUBO,X,Y,AIRE,AIRS,CMI,JMI,
     &           ZF,DPX,DPY,DSZ,BETA,AIRST,T1,T2,T3,T4,T5)
       ENDIF
!
!    INITIALISATION POUR TRACEUR
!
       IF(NTRAC.GT.0) THEN
         DO ITRAC=1,NTRAC
           MASSOU(ITRAC) = 0.D0
           FLUTENT(ITRAC)= 0.D0
           FLUTSOR(ITRAC)= 0.D0
           DO IS=1,NPOIN
             HTN%ADR(ITRAC)%P%R(IS) = HN(IS) * TN%ADR(ITRAC)%P%R(IS)
           ENDDO
           CALL OS('X=Y     ',X=T%ADR(ITRAC)%P,Y=TN%ADR(ITRAC)%P)
         ENDDO
       ENDIF
!
!   DEFINITION D'UNE REFERENCE POUR DISTINGUER NOEUDS INTERIEURS
!      ET NOEUDS FRONTIERE ( POUR TRACEUR ORDRE 2)
!
       DO IS=1,NPOIN
         LOGFR(IS)=0
       ENDDO
!
      DO K=1,NPTFR
!
        IS=NBOR(K)
        IF(LIMPRO(K,2).EQ.KDIR) LOGFR(IS)=1
        IF(LIMPRO(K,1).EQ.KDIR) LOGFR(IS)=3
        IF(LIMPRO(K,1).EQ.KNEU) LOGFR(IS)=2
!
       ENDDO
!
       ENDIF
!-----------------------------------------------------------------------
!
      IF(LT.EQ.1.OR.NTRAC.EQ.0) THEN
!
!     RECOPIE DES VARIABLES DANS W
!
      DO I=1,NPOIN
          W(1,I)= HN(I)
        IF (HN(I).GT.EPS) THEN
          W(2,I) = QU(I) / HN(I)
          W(3,I) = QV(I) / HN(I)
        ELSE
          W(2,I) = 0.D0
          W(3,I) = 0.D0
        ENDIF
      ENDDO
!
!  CALCUL DU PAS DE TEMPS SATISFAISANT LA CONDITION CFL (ORDRE 1)
!
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DTN,CFLWTD,ICIN,DTVARI,LISTIN)
!
!  CALCUL DES FLUX HYDRO
!
      CALL FLUHYD(NPOIN,NELMAX,NSEG,NPTFR,NUBO,G,DTN,X,Y,AIRS,IKLE,AIRE,
     &            W,ZF,VNOIN,FLUX,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &            KDDL, HBOR,UBOR,VBOR,FLUENTN,FLUSORTN,NORDRE,CMI,JMI,
     &            DJX,DJY,DX,DY,DTHAUT,CFLWTD,FLBOR,
     &            DPX,DPY,IVIS,PROPNU,FLUHBTEMP,BETA,DSZ,AIRST,HC,
     &            FLUXTEMP,NTRAC)
!
      IF(NTRAC.GT.0) THEN
!       INITIALISATION POUR TRACEUR
        CALL REINIT(NPOIN,NSEG,NPTFR,HN,
     &              SMTR,HSTOK,HC,HCSTOK,FLUXT,FLUHBOR,DTT,NTRAC)
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!           SI TRACEUR, FIN D'INITIALISATION
!
!-----------------------------------------------------------------------
!
!                     MISE A JOUR HYDRO
!-----------------------------------------------------------------------
!
      DT = MIN(DTN,TMAX-AT)
!
      FLUENT =FLUENTN
      FLUSORT =FLUSORTN
      DO ITRAC=1,NTRAC
        FLUTENT(ITRAC)=0.D0
        FLUTSOR(ITRAC)=0.D0
        MASSOU(ITRAC) =0.D0
      ENDDO
!
      CALL MAJ(NPOIN,NSEG,NPTFR,G,DT,AIRS,
     &         HN,QU,QV,W,FLUX,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,
     &         SMH,KFROT,CF )
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
!
!         ON INCREMENTE LES FLUX DE MASSE ET LES SOURCES POUR TRACEUR
          CALL FLUTRAC(NSEG,NPTFR,DT,FLUXT%ADR(ITRAC)%P%R,
     &                               FLUHBOR%ADR(ITRAC)%P%R,
     &                               FLUXTEMP%ADR(ITRAC)%P%R,
     &                               FLUHBTEMP%ADR(ITRAC)%P%R,DTT)
!
!         CALCUL DU SECOND MEMBRE POUR TRACEUR
          CALL SMTRAC(NPOIN,DIMT,AT,DT,SMTR%ADR(ITRAC)%P%R,
     &                SMH,NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,ITRAC)
!
        ENDDO
!
      ENDIF
!
!  CALCUL DU VOLUME AJOUTE PAR LES SOURCES
!
      IF(YASMH) THEN
      MASSES=0.D0
      DO  I=1,NPOIN
      MASSES = MASSES + SMH(I)
      ENDDO
       MASSES = DT * MASSES
      ENDIF
!
      DO 115 I=1,NPOIN
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
!
!       CALCUL DE U,V AJOUTE PAR JMH
!
        IF (H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I)
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
115   CONTINUE
!
! BEGIN RA
!
!	DO K=1,NPTFR
!                U(NBOR(K)) =  UBOR(K)
!          	V(NBOR(K)) =  VBOR(K)
!        ENDDO
!
!END RA
!
      IF(NTRAC.EQ.0)  RETURN
!
!-----------------------------------------------------------------------
!
!  SI FIN DU CALCUL, ON MET A JOUR LE TRACEUR
!
      IF(AT+DT.GE.TMAX) GOTO 200
!
!-----------------------------------------------------------------------
!     SI TRACEUR, ON CALCULE PAR ANTICIPATION LES FLUX
!     DU PAS HYDRO SUIVANT
!-----------------------------------------------------------------------
!
!  ON MET DANS W LES VARIABLES PRIMITIVES
!
      DO I=1,NPOIN
        W(1,I) = H(I)
        W(2,I) = U(I)
        W(3,I) = V(I)
      ENDDO
!
!  CALCUL DU PAS DE TEMPS SATISFAISANT LA CONDITION CFL (ORDRE 1)
!
      CALL CALDT(NPOIN,G,H,U,V,DTHAUT,DTN,CFLWTD,ICIN,DTVARI,LISTIN)
!
!  CALCUL DES FLUX HYDRO DU PAS DE TEMPS SUIVANT
!
      CALL FLUHYD(NPOIN,NELMAX,NSEG,NPTFR,NUBO,G,DTN,X,Y,AIRS,IKLE,AIRE,
     &            W,ZF,VNOIN,FLUX,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &            KDDL,HBOR,UBOR,VBOR,FLUENTN,FLUSORTN,NORDRE,CMI,JMI,
     &            DJX,DJY,DX,DY,DTHAUT,CFLWTD,FLBOR,
     &            DPX,DPY,IVIS,PROPNU,FLUHBTEMP,BETA,DSZ,AIRST,HC,
     &            FLUXTEMP,NTRAC)
!
!  TEST DES FLUX TRACEURS POUR POSITIVITE
!
!     NE SERT A RIEN MAIS EVITE WARNING DE COMPILATEUR
      TEST=-1.D0
!
      CALL TESTEUR(NPOIN,NSEG,NPTFR,NUBO,DTN,NBOR,
     &             NORDRE,AIRS,AIRST,HSTOK,HCSTOK,
     &             FLUXT,FLUXTEMP,FLUHBOR,FLUHBTEMP,LOGFR,TEST,NTRAC)
!
      IF(TEST.GE.0.D0) RETURN
 200  CONTINUE
!
!  MISE A JOUR DU TRACEUR (POUR ETRE MOINS DIFFUSIF, ON NE TRAITE LE TRACEUR
!                          QUE QUAND LA MONOTONIE EST MENACEE)
!
      LTT=LTT+1
!
      DO ITRAC=1,NTRAC
!
      CALL MAJTRAC(NPOIN,NELMAX,DIMT,DLIMT,NSEG,NPTFR,NUBO,
     &             X,Y,AIRS,IKLE,AIRE,T%ADR(ITRAC)%P%R,
     &             HTN%ADR(ITRAC)%P%R,TN%ADR(ITRAC)%P%R,ZF,NBOR,
     &             TBOR%ADR(ITRAC)%P%R,FLUTENT(ITRAC),FLUTSOR(ITRAC),
     &             SMTR%ADR(ITRAC)%P%R,NORDRE,CMI,JMI,
     &             DJXT,DJYT,DXT,DYT,
     &             DPX,DPY,DIFT,DIFNU,BETA,DSZ,AIRST,HSTOK,
     &             HCSTOK,FLUXT%ADR(ITRAC)%P%R,FLUHBOR%ADR(ITRAC)%P%R,
     &             MASSOU(ITRAC),DTT)
!
!   HT EST DANS T A LA SORTIE DE MAJTRAC
!
      DO I=1,NPOIN
        HTN%ADR(ITRAC)%P%R(I) = T%ADR(ITRAC)%P%R(I)
        IF(H(I).GT.EPS) THEN
          T%ADR(ITRAC)%P%R(I) = T%ADR(ITRAC)%P%R(I) / H(I)
        ELSE
          T%ADR(ITRAC)%P%R(I) = 0.D0
        ENDIF
      ENDDO
!
      ENDDO
!
! INITIALISATION POUR TRACEUR
!
      CALL REINIT(NPOIN,NSEG,NPTFR,H,
     &            SMTR,HSTOK,HC,HCSTOK,FLUXT,FLUHBOR,DTT,NTRAC)
!
      ELSE IF(ICIN.EQ.2) THEN
!    *****************************
!
!-----------------------------------------------------------------------
!             SCHEMA DE ZOKAGOA
!-----------------------------------------------------------------------
!
      IVIS=0
      IF(DIFVIT.AND.ITURB.EQ.1) IVIS=1
!
      IF(LT.EQ.1) THEN
!
!             INITIALISATIONS AU 1ER PAS DE TEMPS
!             ***********************************
!
       WRITE(LU,*) ' '
       WRITE(LU,*) '          *********************** '
       WRITE(LU,*) '          * SCHEMA DE ZAKAGOA   * '
       WRITE(LU,*) '          *********************** '
       WRITE(LU,*) ' '
!
       ENDIF
!-----------------------------------------------------------------------
!
!
!     RECOPIE DES VARIABLES DANS W
!
      DO I=1,NPOIN
          W(1,I)= HN(I)
          W(2,I)= QU(I)
          W(3,I)= QV(I)
      ENDDO
!
!  TIME STEP UNDER CFL CONDITION
!
! CALCUL DU DT QUI SATISFAIT CFL
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,CFLWTD,ICIN,DTVARI,LISTIN)
!
      DT = MIN(DT,TMAX-AT)
!
! INFLOW AND OUTFLOWS
!
      CALL FLUSEW(WINF,UBOR,VBOR,NPOIN,EPS,G,W,
     &            XNEBOR,YNEBOR,NPTFR,LIMPRO,NBOR,KDIR,KNEU,KDDL)
!
!-----------------------------------------------------------------------
!  FLUX COMPUTATION
      CALL FLUXZZ(NPOIN,NSEG,NUBO,G,X,Y,W,ZF,VNOIN,FLUX,AIRS)
! BOUNDARY CONDITIONS
       CALL CDLZZ(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &             KDDL,G,HBOR,UBOR,VBOR,W,FLUX,FLUENT,FLUSORT,
     &             FLBOR,DTHAUT,
     &             DT,CFLWTD,EPS,ZF,WINF)
!
!-----------------------------------------------------------------------
!
!  TIME INTEGRATION
!
      CALL MAJZZ(W,FLUX,AIRS,DT,NPOIN,ZF,CF,EPS,DMIN,KFROT,SMH,
     &          HN,QU,QV)
!
!-----------------------------------------------------------------------
!
!  CALCUL DU VOLUME AJOUTE PAR LES SOURCES
!
      IF(YASMH) THEN
        MASSES=0.D0
        DO  I=1,NPOIN
          MASSES = MASSES + SMH(I)
        ENDDO
        MASSES = DT * MASSES
      ENDIF
!
      DO 715 I=1,NPOIN
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
!
!       CALCUL DE U,V
!
        IF (H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I)
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
715   CONTINUE
!
! BEGIN RA
!
        DO K=1,NPTFR
                U(NBOR(K)) =  UBOR(K)
          	V(NBOR(K)) =  VBOR(K)
        ENDDO
!
!END RA
!-----------------------------------------------------------------------
!
!
      ELSE IF(ICIN.EQ.3) THEN
!    *****************************
!
!-----------------------------------------------------------------------
!     TCHAMEN SCHEME
!-----------------------------------------------------------------------
!
      IVIS=0
      IF(DIFVIT.AND.ITURB.EQ.1) IVIS=1
!
      IF(LT.EQ.1) THEN
!
!     INITIALISATIONS AT 1ST TIME STEP
!     ********************************
!
       WRITE(LU,*) ' '
       WRITE(LU,*) '          *********************** '
       WRITE(LU,*) '          * SCHEMA DE TCHAMEN   * '
       WRITE(LU,*) '          *********************** '
       WRITE(LU,*) ' '
!
      ENDIF
!-----------------------------------------------------------------------
!
!     SAVING VARIABLES INTO W
!
      DO I=1,NPOIN
          W(1,I)= HN(I)
          W(2,I)= QU(I)
          W(3,I)= QV(I)
      ENDDO
!
!  TIME STEP UNDER CFL CONDITION
!
! CALCUL DU DT QUI SATISFAIT CFL
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DT,CFLWTD,ICIN,DTVARI,LISTIN)
!
      DT = MIN(DT,TMAX-AT)
!
! INFLOW AND OUTFLOWS
!
      CALL FLUSEW(WINF,UBOR,VBOR,NPOIN,EPS,G,W,
     &            XNEBOR,YNEBOR,NPTFR,LIMPRO,NBOR,KDIR,KNEU,KDDL)
!
!-----------------------------------------------------------------------
!  FLUX COMPUTATION
      CALL FLUX_TCH(NPOIN,NSEG,NUBO,G,X,Y,W,ZF,VNOIN,FLUX,AIRS)
! BOUNDARY CONDITIONS
       CALL CDL_TCH(NPOIN,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &             KDDL,G,HBOR,UBOR,VBOR,W,FLUX,FLUENT,FLUSORT,
     &             FLBOR,DTHAUT,
     &             DT,CFLWTD,EPS,ZF,WINF)
!
!-----------------------------------------------------------------------
!
!  TIME INTEGRATION
!
      CALL MAJZZ(W,FLUX,AIRS,DT,NPOIN,ZF,CF,EPS,DMIN,KFROT,SMH,
     &           HN,QU,QV)
!
!
!-----------------------------------------------------------------------
!
!  CALCUL DU VOLUME AJOUTE PAR LES SOURCES
!
      IF(YASMH) THEN
      MASSES=0.D0
      DO  I=1,NPOIN
      MASSES = MASSES + SMH(I)
      ENDDO
       MASSES = DT * MASSES
      ENDIF
!
      DO 815 I=1,NPOIN
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
!
!       CALCUL DE U,V
!
        IF (H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I)
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
815   CONTINUE
!
! BEGIN RA
!
        DO K=1,NPTFR
                U(NBOR(K)) =  UBOR(K)
          	V(NBOR(K)) =  VBOR(K)
        ENDDO
!
!END RA
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END