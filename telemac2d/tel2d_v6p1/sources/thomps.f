!                    *****************
                     SUBROUTINE THOMPS
!                    *****************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,T,ZF,X,Y,NBOR,FRTYPE,UNA,C,
     & UCONV,VCONV,T6,FU,FV,LIHBOR,LIUBOR,LIVBOR,LITBOR,LISPFR,T8,W1,
     & ITRAV2,
     & W1R,W2R,W3R,W4R,HBTIL,UBTIL,VBTIL,TBTIL,ZBTIL,SURDET,IKLE,
     & CF,SMH,IFABOR,NULONE,NELEM,MESH,
     & KP1BOR,XNEBOR,YNEBOR,NPOIN,NPTFR,LT,NIT,TEMPS,DT,GRAV,
     & DEBLIQ,FINLIQ,NTRAC,NFRLIQ,KSORT,LV,MSK,MASKEL,MASKPT,
     & NELBOR,NELMAX,IELM,NORD,FAIR,WINDX,WINDY,
     & VENT,HWIND,CORIOL,FCOR,SPHERI,
     & OPTPRO,MAREE,MARDAT,MARTIM,PHI0,OPTSOU,ISCE,DSCE,USCE,VSCE,T5,
     & COUROU,NPTH,VARCL,NVARCL,VARCLA,NUMLIQ,SHP,UNSV2D,HFROT,
     & FXWAVE,FYWAVE)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    TREATS LIQUID BOUNDARIES USING THOMPSON METHOD
!+                BASED ON CHARACTERISTICS.
!
!history  J-M HERVOUET (LNHE)
!+        01/09/2008
!+
!+   POINTS GROUPED REGARDLESS OF THEIR BOUNDARY NUMBER.
!
!history  E DAVID (LHF)
!+        05/09/2008
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
!| C              |-->| TABLEAU DE TRAVAIL : CELERITE DES ONDES
!| CF             |---|
!| CORIOL         |---|
!| COUROU         |---|
!| DEBLIQ         |-->| TABLEAU D'INDICES DE DEBUT DE FRONTIERE LIQ.
!| DSCE           |---|
!| DT             |-->| PAS DE TEMPS
!| FAIR           |---|
!| FCOR           |---|
!| FINLIQ         |-->| TABLEAU D'INDICES DE FIN DE FRONTIERE LIQUI.
!| FRTYPE         |-->| TYPE DE FRONTIERES LIQUIDES
!| FU,FV          |-->| TABLEAU DE TRAVAIL : TERMES SOURCES
!| GRAV           |-->| GRAVITE
!| H              |-->| HAUTEUR AU TEMPS N
!| HBOR           |<--| HAUTEUR IMPOSEE.
!| HBTIL          |---|
!| HFROT          |---|
!| HWIND          |---|
!| IELM           |---|
!| IFABOR         |---|
!| IKLE           |---|
!| ISCE           |---|
!| ITRAV2         |---|
!| KP1BOR         |-->| NUMERO DU POINT FRONTIERE SUIVANT
!| LIHBOR         |-->| CONDITIONS AUX LIMITES SUR H
!| LISPFR         |-->| LISTE DES POINTS FRONTIERES CONTIGUS TRAITES
!|                |   | ENSEMBLES PAR LES CARACTERISTIQUES
!|                |   | CARACTERISTIQUES
!| LITBOR         |-->| CONDITIONS AUX LIMITES SUR LE TRACEUR
!| LIUBOR,LIVBOR  |-->| CONDITIONS AUX LIMITES SUR U ET V
!| LT             |-->| NUMERO DE L'ITERATION EN COURS
!| LV             |---|
!| MARDAT         |---|
!| MAREE          |---|
!| MARTIM         |---|
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
!| MASKPT         |---|
!| MESH           |---|
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NBOR           |-->| ADRESSES DES POINTS DE BORD
!| NELBOR         |-->| NUMEROS DES ELEMENTS ADJACENTS AUX BORDS
!| NELEM          |---|
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
!| NFRLIQ         |-->| NOMBRE DE FRONTIERES LIQUIDES
!| NIT            |---|
!| NORD           |---|
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE.
!| NPTH           |---|
!| NTRAC          |---|
!| NULONE         |---|
!| NUMLIQ         |---|
!| NVARCL         |---|
!| OPTPRO         |---|
!| OPTSOU         |---|
!| PHI0           |---|
!| SHP            |---|
!| SMH            |---|
!| SPHERI         |---|
!| SURDET         |---|
!| T              |-->| TRACEUR AU TEMPS N
!| T5             |---|
!| T6             |---|
!| T8             |---|
!| TBOR           |<--| TRACEUR IMPOSE AU BORD
!| TBTIL          |---|
!| TEMPS          |-->| TEMPS
!| U,V            |-->| COMPOSANTES DE LA VITESSE AU TEMPS N
!| UBOR           |<--| VITESSE U IMPOSEE.
!| UBTIL          |---|
!| UCONV,VCONV    |-->| TABLEAU DE TRAVAIL : CHAMPS DE VITESSE
!|                |   | CONVECTEUR DES INVARIANTS DE RIEMANN
!| UNA            |-->| TABLEAU DE TRAVAIL
!| UNSV2D         |---|
!| USCE           |---|
!| VARCL          |---|
!| VARCLA         |---|
!| VBOR           |<--| VITESSE V IMPOSEE.
!| VBTIL          |---|
!| VENT           |---|
!| VSCE           |---|
!| W1             |---|
!| W1R            |---|
!| W2R            |---|
!| W3R            |---|
!| W4R            |---|
!| WINDX          |---|
!| WINDY          |---|
!| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
!| XNEBOR,YNEBOR  |-->| NORMALES EXTERIEURES AUX POINTS.
!| ZBTIL          |---|
!| ZF             |-->| FOND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_THOMPS => THOMPS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR,LT,NIT,NPOIN,NELEM,NELMAX,NFRLIQ,LV
      INTEGER, INTENT(IN) :: NVARCL,NPTH,KSORT,IELM,NTRAC,HFROT
      INTEGER, INTENT(IN) :: OPTPRO,MARDAT(3),MARTIM(3),OPTSOU,ISCE(*)
      INTEGER, INTENT(IN) :: DEBLIQ(NFRLIQ),FINLIQ(NFRLIQ)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),KP1BOR(NPTFR,2),NELBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(*),IFABOR(*),NULONE(*)
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER, INTENT(IN) :: FRTYPE(NFRLIQ),NUMLIQ(NFRLIQ)
      INTEGER, INTENT(INOUT) :: LISPFR(NPTFR)
!     ITRAV2 : OF DIMENSION NPOIN
      INTEGER, INTENT(INOUT) :: ITRAV2(*)
      LOGICAL, INTENT(IN) :: VENT,MAREE,CORIOL,SPHERI,MSK,COUROU
      DOUBLE PRECISION, INTENT(IN) :: HWIND
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(*),DSCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: USCE(*),VSCE(*)
      DOUBLE PRECISION, INTENT(IN)  :: TEMPS,GRAV,DT,FAIR,FCOR,NORD,PHI0
      DOUBLE PRECISION, INTENT(INOUT) :: W1R(NPTFR),W2R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: W3R(NPTFR),W4R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: HBTIL(NPTFR),UBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: VBTIL(NPTFR),ZBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: T5(NPOIN),SHP(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: WINDX,WINDY,MASKEL,MASKPT
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: W1,VARCL,FXWAVE,FYWAVE
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FU,FV,T8,UNA,UCONV,VCONV,C,U,V
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,T,SMH,TBOR,TBTIL,T6
      TYPE(BIEF_OBJ), INTENT(IN)      :: ZF,CF,LITBOR,UNSV2D
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      CHARACTER(LEN=32), INTENT(IN)   :: VARCLA(NVARCL)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,NDEB,NFIN,IFRLIQ,NPT,KP,J,ITRAC,N
!
      DOUBLE PRECISION EPSIL,HMIN,HHBOR
!
      LOGICAL TSI
!
      DATA EPSIL /1.D-5/
      DATA TSI   /.FALSE./
      DATA HMIN  /2.D-2/
!
      INTRINSIC ABS
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'THOMPSON NE MARCHE PAS EN PARALLELE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'THOMPSON NOT YET IMPLEMENTED IN PARALLEL'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     ONLY IF THERE ARE LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
!
!
! COMPUTES THE FRICTION TERM IN UCONV
! "C,C" ADDED BY JMH ON 08/08/2000 (VERTICAL STRUCTURES)
! BUT NOT TAKEN INTO ACCOUNT HERE (LAST ARGUMENT SET TO FALSE)
!
        CALL FRICTI(FU,FV,C,C,U,V,H,CF,MESH,T8,T6,.FALSE.,UNSV2D,
     &              MSK,MASKEL,HFROT)
!
! COMPUTES DT*UCONV*U
!
        CALL OS('X=CYZ   ', UCONV , FU , U , DT )
        CALL OS('X=CYZ   ', VCONV , FV , V , DT )
!
! COMPUTES THE SOURCE TERMS IN FU
!
        CALL PROSOU(FU,FV,SMH,U,V,H,GRAV,NORD,
     &              FAIR,WINDX,WINDY,VENT,HWIND,CORIOL,FCOR,
     &              SPHERI,TSI,MESH%COSLAT,MESH%SINLAT,
     &              TEMPS,LT,0,0,DSCE,ISCE,UNA,MESH,MSK,MASKEL,
     &              MAREE,MARDAT,MARTIM,PHI0,OPTSOU,COUROU,NPTH,
     &              VARCL,NVARCL,VARCLA,UNSV2D,FXWAVE,FYWAVE)
!
! GOES IN FU
!
        CALL OS('X=Y+CZ  ', FU , UCONV , FU , DT )
        CALL OS('X=Y+CZ  ', FV , VCONV , FV , DT )
!
! COMPUTES THE CELERITY
!
        CALL OS('X=CY    ' , C , H , H , GRAV )
        CALL CLIP(C,0.D0,.TRUE.,1.D6,.FALSE.,0)
        CALL OS('X=SQR(Y)',X=C,Y=C )
!
! CORRECTS FOR TIDAL FLATS
!
        DO 9 K=1,NPOIN
          IF(H%R(K).LT.HMIN) THEN
            FU%R(K)=0.D0
            FV%R(K)=0.D0
          ENDIF
9       CONTINUE
!
! TEMPORAL MARCHING (SPLITTING DU/DT=FU)
!
        CALL OS('X=X+Y   ',X=U,Y=FU)
        CALL OS('X=X+Y   ',X=V,Y=FV)
!
! REGROUPS THE POINTS WITH THE SAME NORMAL
! TO THE ACCURACY 'EPSIL'
! NPT : NUMBER OF SEQUENTIAL POINTS
! LISPFR : LIST OF THOSE POINTS (BOUNDARY NODE NUMBERS)
!
      NDEB=0
!
19    CONTINUE
      K=NDEB
20    CONTINUE
!
      K=K+1
      IF(K.GT.NPTFR) GO TO 1000
      IF(NUMLIQ(K).EQ.0) GO TO 20
      IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
!       FIRST THOMPSON POINT IN THE LIST
        NPT=1
        LISPFR(NPT)=K
      ELSE
        GO TO 20
      ENDIF
      NDEB = K
      KP   = K
30    CONTINUE
      KP=KP+1
      IF(KP.GT.NPTFR) GO TO 999
      IF(NUMLIQ(KP).EQ.0) GO TO 999
      IF(FRTYPE(NUMLIQ(KP)).EQ.2.AND.
     &   ABS(XNEBOR(KP)-XNEBOR(K)).LT.EPSIL.AND.
     &   ABS(YNEBOR(KP)-YNEBOR(K)).LT.EPSIL     ) THEN
        NPT=NPT+1
        LISPFR(NPT)=KP
        GO TO 30
      ENDIF
999   CONTINUE
      NDEB=LISPFR(NPT)
!
! UPDATES THE BOUNDARY VALUES IF FREE EXIT BOUNDARY
!
      DO J=1,NPT
        K=LISPFR(J)
        N=NBOR(K)
        IF(LIHBOR(K).EQ.KSORT) THEN
          HBOR(K)=H%R(N)
        ENDIF
        IF(LIUBOR(K).EQ.KSORT) THEN
          UBOR(K)=U%R(N)
        ENDIF
        IF(LIVBOR(K).EQ.KSORT) THEN
          VBOR(K)=V%R(N)
        ENDIF
      ENDDO
      IF(NTRAC.GT.0) THEN
        DO J=1,NPT
          K=LISPFR(J)
          N=NBOR(K)
          DO ITRAC=1,NTRAC
            IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KSORT) THEN
              TBOR%ADR(ITRAC)%P%R(K)=T%ADR(ITRAC)%P%R(N)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
! COMPUTES THE ADVECTION FIELD U ACCORDING TO THE NORMAL DIRECTION
! AT THE BOUNDARY
!
      CALL OS('X=CY    ',UNA  ,U  ,U  ,XNEBOR(LISPFR(1)))
      CALL OS('X=X+CY  ',UNA  ,V  ,V  ,YNEBOR(LISPFR(1)))
      CALL OS('X=CY    ',UCONV,UNA,UNA,XNEBOR(LISPFR(1)))
      CALL OS('X=CY    ',VCONV,UNA,UNA,YNEBOR(LISPFR(1)))
!
! CHARACTERISTICS FOR THE GROUP OF POINTS, ADVECTION FIELD U
!
      CALL GTSH11(UCONV%R,VCONV%R,X,Y,SHP,ITRAV2,
!                      INDIC  NLOC   (NOT USED ANYMORE)
     &            IKLE,ITRAV2,ITRAV2,NPOIN,NELEM,NELMAX,1,MSK,MASKEL%R)
      CALL CARAFR
     & ( U%R,V%R,H%R,T,UCONV%R,VCONV%R,X,Y,SHP,
     &   SURDET , DT , IKLE , IFABOR , ITRAV2 ,
     &   NBOR , NELBOR , NULONE , IELM , NELEM , NELMAX ,
     &   NPOIN , 3 , NPTFR ,
     &   MSK , MASKEL%R , MASKPT%R ,  NPT , LISPFR , NTRAC ,
     &   HBTIL , UBTIL , VBTIL , TBTIL , ZBTIL , ZF%R,T5)
!
! COMPUTES THE RIEMANN INVARIANTS W1 AND W4 (SECOND DIMENSION OF TBOR)
! CARRIED BY THIS FIELD
!
      DO J=1,NPT
       K=LISPFR(J)
       IF(UNA%R(NBOR(K)).GE.0.D0) THEN
        W1R(K)=-HBTIL(K)*(XNEBOR(LISPFR(1))*(VBTIL(K)-V%R(NBOR(K)))-
     &                    YNEBOR(LISPFR(1))*(UBTIL(K)-U%R(NBOR(K))))
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            TBOR%ADR(ITRAC)%P%R(K+NPTFR)=HBTIL(K)*
     &      (TBTIL%ADR(ITRAC)%P%R(K)-T%ADR(ITRAC)%P%R(NBOR(K)))
          ENDDO
        ENDIF
       ELSE
        W1R(K)=-HBOR(K)*(XNEBOR(LISPFR(1))*(VBOR(K)-V%R(NBOR(K)))-
     &                   YNEBOR(LISPFR(1))*(UBOR(K)-U%R(NBOR(K))) )
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            TBOR%ADR(ITRAC)%P%R(K+NPTFR)=
     &      HBOR(K)*(TBOR%ADR(ITRAC)%P%R(K)-T%ADR(ITRAC)%P%R(NBOR(K)))
          ENDDO
        ENDIF
       ENDIF
      ENDDO
!
! COMPUTES THE ADVECTION FIELD U + C ACCORDING TO THE NORMAL DIRECTION
! AT THE BOUNDARY
!
      CALL OS('X=X+CY  ',UNA  , C   , C   ,             1.D0 )
      CALL OS('X=CY    ',UCONV, UNA , UNA , XNEBOR(LISPFR(1)) )
      CALL OS('X=CY    ',VCONV, UNA , UNA , YNEBOR(LISPFR(1)) )
!
! CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
!
      CALL GTSH11(UCONV%R,VCONV%R,X,Y,SHP,ITRAV2,
!                      INDIC  NLOC   (NOT USED ANYMORE)
     &            IKLE,ITRAV2,ITRAV2,NPOIN,NELEM,NELMAX,1,MSK,MASKEL%R)
      CALL CARAFR
     & ( U%R,V%R,H%R,T,UCONV%R,VCONV%R,X,Y,SHP,
     &   SURDET,DT,IKLE,IFABOR,ITRAV2,
     &   NBOR,NELBOR,NULONE,IELM,NELEM,NELMAX,
     &   NPOIN,3,NPTFR,
     &   MSK,MASKEL%R,MASKPT%R,NPT,LISPFR,NTRAC,
     &   HBTIL,UBTIL,VBTIL,TBTIL,ZBTIL,ZF%R,T5)
!
! COMPUTES THE RIEMANN INVARIANTS W2 CARRIED BY THIS ADVECTION FIELD
!
      DO 50 J=1,NPT
       K=LISPFR(J)
       IF (UNA%R(NBOR(K)).GE.0.D0) THEN
        W2R(K)=(-ZF%R(NBOR(K))+HBTIL(K)+ZBTIL(K))*C%R(NBOR(K))+
     &         HBTIL(K)*(XNEBOR(LISPFR(1))*(UBTIL(K)-U%R(NBOR(K)))+
     &                   YNEBOR(LISPFR(1))*(VBTIL(K)-V%R(NBOR(K))) )
       ELSE
        W2R(K)=HBOR(K)*(C%R(NBOR(K))+
     &                 (XNEBOR(LISPFR(1))*(UBOR(K)-U%R(NBOR(K)))+
     &                  YNEBOR(LISPFR(1))*(VBOR(K)-V%R(NBOR(K)))) )
       ENDIF
50    CONTINUE
!
! COMPUTES THE ADVECTION FIELD U-C ACCORDING TO THE NORMAL DIRECTION
! AT THE BOUNDARY
!
      CALL OS('X=X+CY  ',X=UNA, Y=C , C=-2.D0 )
      CALL OS('X=CY    ',X=UCONV,Y=UNA , C=XNEBOR(LISPFR(1)) )
      CALL OS('X=CY    ',X=VCONV,Y=UNA , C=YNEBOR(LISPFR(1)) )
!
! CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
!
      CALL GTSH11(UCONV%R,VCONV%R,X,Y,SHP,ITRAV2,
!                      INDIC  NLOC   (NOT USED ANYMORE)
     &            IKLE,ITRAV2,ITRAV2,NPOIN,NELEM,NELMAX,1,MSK,MASKEL%R)
      CALL CARAFR
     & ( U%R,V%R,H%R,T,UCONV%R,VCONV%R,X,Y,SHP,
     &   SURDET,DT,IKLE,IFABOR,ITRAV2,
     &   NBOR,NELBOR,NULONE,IELM,NELEM,NELMAX,NPOIN,3,NPTFR,
     &   MSK,MASKEL%R,MASKPT%R,NPT,LISPFR,NTRAC,
     &   HBTIL,UBTIL,VBTIL,TBTIL,ZBTIL,ZF%R,T5)
!
! COMPUTES THE RIEMANN INVARIANTS W3 CARRIED BY THIS ADVECTION FIELD
!
      DO 60 J=1,NPT
       K=LISPFR(J)
       IF(UNA%R(NBOR(K)).GE.0.D0) THEN
        W3R(K)=(-ZF%R(NBOR(K))+HBTIL(K)+ZBTIL(K))*C%R(NBOR(K))-
     &         HBTIL(K)*(XNEBOR(LISPFR(1))*(UBTIL(K)-U%R(NBOR(K)))+
     &                   YNEBOR(LISPFR(1))*(VBTIL(K)-V%R(NBOR(K))) )
       ELSE
        W3R(K)=HBOR(K)*(C%R(NBOR(K))-
     &                 (XNEBOR(LISPFR(1))*(UBOR(K)-U%R(NBOR(K)))+
     &                  YNEBOR(LISPFR(1))*(VBOR(K)-V%R(NBOR(K)))) )
       ENDIF
60    CONTINUE
!
! RE-BUILDS THE TELEMAC-2D VARIABLES
!
! FOR TIDAL FLATS (H HERE
! THE PREVIOUS RELEASE HAD
!
      DO 70 J=1,NPT
!
        K=LISPFR(J)
        IF(C%R(NBOR(K))**2.GT.GRAV*HMIN) THEN
          HBOR(K)=(W2R(K)+W3R(K))/(2*C%R(NBOR(K)))
          IF(HBOR(K).GT.HMIN) THEN
!           BEWARE TIDAL FLATS, AND HIDDEN PARAMETER 0.1
            HHBOR=MAX(0.1D0,HBOR(K))
            UBOR(K)=(YNEBOR(LISPFR(1))*W1R(K)+XNEBOR(LISPFR(1))*W2R(K)-
     &HBOR(K)*C%R(NBOR(K))*XNEBOR(LISPFR(1)))/HHBOR+U%R(NBOR(K))
            VBOR(K)=(YNEBOR(LISPFR(1))*W2R(K)-XNEBOR(LISPFR(1))*W1R(K)-
     &HBOR(K)*C%R(NBOR(K))*YNEBOR(LISPFR(1)))/HHBOR+V%R(NBOR(K))
            IF(NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                TBOR%ADR(ITRAC)%P%R(K)=
     &          TBOR%ADR(ITRAC)%P%R(K+NPTFR)/HHBOR+
     &          T%ADR(ITRAC)%P%R(NBOR(K))
              ENDDO
            ENDIF
          ELSE
!           BECOMES DRY
            HBOR(K)=MAX(0.D0,HBOR(K))
            UBOR(K)=0.D0
            VBOR(K)=0.D0
          ENDIF
        ELSE
!         WAS DRY, H IS GIVEN BY BORD
          UBOR(K)=0.D0
          VBOR(K)=0.D0
        ENDIF
!
70    CONTINUE
!
      IF(NDEB.LE.NPTFR) GO TO 19
!
!     TEST IF(NFRLIQ.GT.0)...
      ENDIF
!
!
1000  CONTINUE
!
!
! RECOVERS OF THE EXACT VALUES FOR U AND V
! REQUIRED FOR THE SUB-ITERATIONS
!
      CALL OS('X=X-Y   ' , X=U , Y=FU )
      CALL OS('X=X-Y   ' , X=V , Y=FV )
!
!-----------------------------------------------------------------------
!
      RETURN
      END