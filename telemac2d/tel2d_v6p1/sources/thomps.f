!                    *****************
                     SUBROUTINE THOMPS
!                    *****************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,T,ZF,X,Y,NBOR,FRTYPE,UNA,C,
     & UCONV,VCONV,T6,T7,XCONV,YCONV,FU,FV,LIHBOR,LIUBOR,LIVBOR,
     & LITBOR,LISPFR,T8,ITRAV2,
     & W1R,W2R,W3R,HBTIL,UBTIL,VBTIL,TBTIL,ZBTIL,SURDET,IKLE,
     & CF,SMH,IFABOR,NELEM,MESH,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,LT,TEMPS,DT,GRAV,
     & NTRAC,NFRLIQ,KSORT,LV,MSK,MASKEL,
     & NELMAX,IELM,NORD,FAIR,WINDX,WINDY,
     & VENT,HWIND,CORIOL,FCOR,SPHERI,
     & MAREE,MARDAT,MARTIM,PHI0,OPTSOU,ISCE,DSCE,SHPP,
     & COUROU,NPTH,VARCL,NVARCL,VARCLA,NUMLIQ,SHP,UNSV2D,HFROT,
     & FXWAVE,FYWAVE,DX_T,DY_T,DZ_T,ELT_T,IT3,IT4)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
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
!| DSCE           |---|
!| DT             |-->| PAS DE TEMPS
!| FAIR           |---|
!| FCOR           |---|
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
!| MESH           |---|
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NBOR           |-->| ADRESSES DES POINTS DE BORD
!| NELEM          |---|
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
!| NFRLIQ         |-->| NOMBRE DE FRONTIERES LIQUIDES
!| NORD           |---|
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE.
!| NPTH           |---|
!| NTRAC          |---|
!| NUMLIQ         |---|
!| NVARCL         |---|
!| OPTSOU         |---|
!| PHI0           |---|
!| SHP            |---|
!| SHPP           |---|
!| SMH            |---|
!| SPHERI         |---|
!| SURDET         |---|
!| T              |-->| TRACEUR AU TEMPS N
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
!| VARCL          |---|
!| VARCLA         |---|
!| VBOR           |<--| VITESSE V IMPOSEE.
!| VBTIL          |---|
!| VENT           |---|
!| W1R            |---|
!| W2R            |---|
!| W3R            |---|
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
      USE STREAMLINE, ONLY : SCARACT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR,LT,NPOIN,NELEM,NELMAX,NFRLIQ,LV
      INTEGER, INTENT(IN) :: NVARCL,NPTH,KSORT,IELM,NTRAC,HFROT
      INTEGER, INTENT(IN) :: MARDAT(3),MARTIM(3),OPTSOU,ISCE(*)
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(*),IFABOR(*)
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER, INTENT(IN) :: FRTYPE(NFRLIQ),NUMLIQ(NPTFR)
      INTEGER, INTENT(INOUT) :: LISPFR(NPTFR),ELT_T(NPTFR)
!     ITRAV2 : OF DIMENSION NPOIN
      INTEGER, INTENT(INOUT) :: ITRAV2(*)
      LOGICAL, INTENT(IN) :: VENT,MAREE,CORIOL,SPHERI,MSK,COUROU
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(*),DSCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: TEMPS,GRAV,DT,FAIR,FCOR,NORD
      DOUBLE PRECISION, INTENT(IN)    :: HWIND,PHI0
      DOUBLE PRECISION, INTENT(INOUT) :: W1R(NPTFR),W2R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: W3R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPP(3,NPTFR),SHP(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX_T(NPTFR),DY_T(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: DZ_T(NPTFR)
      TYPE(BIEF_OBJ), INTENT(IN)      :: WINDX,WINDY,MASKEL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: HBTIL,UBTIL,VBTIL,ZBTIL,T7
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: VARCL,FXWAVE,FYWAVE,XCONV,YCONV
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FU,FV,T8,UNA,UCONV,VCONV,C,U,V
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,T,SMH,TBOR,TBTIL,T6,IT3,IT4
      TYPE(BIEF_OBJ), INTENT(IN)      :: ZF,CF,LITBOR,UNSV2D
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      CHARACTER(LEN=32), INTENT(IN)   :: VARCLA(NVARCL)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,NDEB,NPT,KP,J,ITRAC,N,NOMB               
      INTEGER P_IMIN
      EXTERNAL P_IMIN
      DOUBLE PRECISION EPSIL,HMIN,HHBOR
      DOUBLE PRECISION ZSTAR(1),ZCONV(1,1),SHZ(1),Z(1,1)
      INTEGER ETA(1)
      LOGICAL TSI,QUAD
      INTEGER NDP,NPLAN,IELMU,I,ISTOP
!
      DATA EPSIL /1.D-5/
      DATA TSI   /.FALSE./
      DATA HMIN  /2.D-2/
!      
      LOGICAL :: INIT
      DATA INIT /.TRUE./
      TYPE(BIEF_OBJ) :: FNCAR1,FTILD1
      SAVE
!
!-----------------------------------------------------------------------
!     
      NPT=0
!
!-----------------------------------------------------------------------
!
!     ALLOCATIONS PROVISOIRES
!
         IF(INIT.EQV..TRUE.) THEN
            CALL ALLBLO(FNCAR1,'FNCAR1')
            CALL ALLBLO(FTILD1,'FTILD1')
            CALL ADDBLO(FNCAR1,U)
            CALL ADDBLO(FNCAR1,V)
            CALL ADDBLO(FNCAR1,H)
            CALL ADDBLO(FNCAR1,ZF)
            CALL ADDBLO(FTILD1,UBTIL)
            CALL ADDBLO(FTILD1,VBTIL)
            CALL ADDBLO(FTILD1,HBTIL)
            CALL ADDBLO(FTILD1,ZBTIL)                       
            IF(NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                CALL ADDBLO(FNCAR1,T%ADR(ITRAC)%P)
                CALL ADDBLO(FTILD1,TBTIL%ADR(ITRAC)%P)
              ENDDO
            ENDIF
            INIT=.FALSE. 
         ENDIF
!
      CALL CPSTVC(HBTIL,ZBTIL)
!
         ETA(1)=1  
         NDP=3
         NPLAN=1
         IELMU=IELM 
         ISTOP=1
         NOMB=4+NTRAC
!
!-----------------------------------------------------------------------
!
      QUAD=.FALSE.     
!
!     ONLY IF THERE ARE LIQUID BOUNDARIES
! 
      IF(NFRLIQ.NE.0) THEN
!
!       COMPUTES THE FRICTION TERM IN UCONV
!       "C,C" ADDED BY JMH ON 08/08/2000 (VERTICAL STRUCTURES)
!       BUT NOT TAKEN INTO ACCOUNT HERE (ARGUMENT SET TO FALSE)
!
        CALL FRICTI(FU,FV,C,C,U,V,H,CF,MESH,T8,T6,.FALSE.,UNSV2D,
     *              MSK,MASKEL,HFROT)
!     
!       COMPUTING TERM DT*UCONV*U
!     
        CALL OS('X=CYZ   ', UCONV , FU , U , DT )
        CALL OS('X=CYZ   ', VCONV , FV , V , DT )
!
!       COMPUTES THE SOURCE TERMS IN FU
!
        CALL PROSOU(FU,FV,SMH,U,V,H,GRAV,NORD,
     &              FAIR,WINDX,WINDY,VENT,HWIND,CORIOL,FCOR,
     &              SPHERI,TSI,MESH%COSLAT,MESH%SINLAT,
     &              TEMPS,LT,0,0,DSCE,ISCE,UNA,MESH,MSK,MASKEL,
     &              MAREE,MARDAT,MARTIM,PHI0,OPTSOU,COUROU,NPTH,
     &              VARCL,NVARCL,VARCLA,UNSV2D,FXWAVE,FYWAVE)
!
!       GOES IN FU
!
        CALL OS('X=Y+CZ  ', FU , UCONV , FU , DT )
        CALL OS('X=Y+CZ  ', FV , VCONV , FV , DT )
!     
!       COMPUTES THE CELERITY
!     
        CALL OS('X=CY    ' , C , H , H , GRAV )
        CALL CLIP(C,0.D0,.TRUE.,1.D6,.FALSE.,0)
        CALL OS('X=SQR(Y)',X=C,Y=C)
!
!       CORRECTS FOR TIDAL FLATS
!
        DO K=1,NPOIN
          IF(H%R(K).LT.HMIN) THEN
            FU%R(K)=0.D0
            FV%R(K)=0.D0
          ENDIF
        ENDDO
!
!       TEMPORAL MARCHING (SPLITTING DU/DT=FU)
!
        CALL OS('X=X+Y   ',X=U,Y=FU)
        CALL OS('X=X+Y   ',X=V,Y=FV)
!
!       REGROUPS THE POINTS WITH THE SAME NORMAL TO THE ACCURACY 'EPSIL'
!       NPT : NUMBER OF SEQUENTIAL POINTS
!       LISPFR : LIST OF THOSE POINTS (BOUNDARY NODE NUMBERS)
!
        ISTOP=0
!       INIT OF NDEB TO 0
        NDEB=0
!       THIS LOOP 19 ENDS WHEN THE GLOBAL DETECTION IS ACHIEVED
19      CONTINUE          
        K=NDEB          
20      CONTINUE
        NPT=0      
!
        K=K+1
!     
        IF(K.GT.NPTFR) GO TO 999
!       ALL BOUNDARY POINT HAS BEEN CHECKED BY THE LOCAL PROCESSOR GOTO 999 
        IF(NUMLIQ(K).EQ.0) GO TO 20
        IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
!         FIRST THOMPSON POINT IN THE LIST
          NPT=1
          LISPFR(NPT)=K             
        ELSE
          GO TO 20
        ENDIF
!
        NDEB = K
        KP   = K
 30     CONTINUE
        KP=KP+1
        IF(KP.GT.NPTFR) GO TO 999
        IF(NUMLIQ(KP).EQ.0) GO TO 999
        IF(FRTYPE(NUMLIQ(KP)).EQ.2.AND.
     *    ABS(XNEBOR(KP)-XNEBOR(K)).LT.EPSIL.AND.
     *    ABS(YNEBOR(KP)-YNEBOR(K)).LT.EPSIL     ) THEN
          NPT=NPT+1
          LISPFR(NPT)=KP
          GO TO 30
        ENDIF
!             
999     CONTINUE
!
!       IF THERE IS NO LOCAL THOMPSON POINT ISTOP=1 ELSE ISTOP=0 
        IF(NPT.EQ.0) ISTOP=1
!
!       UPDATES THE BOUNDARY VALUES IF FREE EXIT BOUNDARY
! 
        IF(NPT.GT.0) THEN    
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
!         COMPUTES THE ADVECTION FIELD U ACCORDING TO THE NORMAL 
!         DIRECTION AT THE BOUNDARY
!     
          CALL OS('X=CY    ',UNA  ,U  ,U  ,XNEBOR(LISPFR(1)))
          CALL OS('X=X+CY  ',UNA  ,V  ,V  ,YNEBOR(LISPFR(1)))
          CALL OS('X=CY    ',UCONV,UNA,UNA,XNEBOR(LISPFR(1)))
          CALL OS('X=CY    ',VCONV,UNA,UNA,YNEBOR(LISPFR(1)))
!
        ENDIF
!
!--------------------------------------------------------------------     
!       CHARACTERISTICS FOR THE GROUP OF POINTS, ADVECTION FIELD U
!       MAY BE CALLED WITH NPT=0 IF OTHER PROCESSORS STILL AT WORK
!--------------------------------------------------------------------
!
        CALL GTSH11(SHP,ITRAV2,IKLE,NPOIN,NELEM,NELMAX,MSK,MASKEL%R)
!
        CALL PRE_SCARACT_THOMPSON(NPOIN,NPTFR,NPT,NDP,NELEM,NELMAX,
     *                            IKLE,MSK,MASKEL,NBOR,LISPFR,X,Y,
     *                            ITRAV2,SHP,XCONV%R,YCONV%R,SHPP,
     *                            ELT_T)
!
        CALL OS('X=0     ',X=UBTIL)
        CALL OS('X=0     ',X=VBTIL)
        CALL OS('X=0     ',X=HBTIL)
        CALL OS('X=0     ',X=ZBTIL)
!
        CALL SCARACT(FNCAR1,FTILD1,UCONV%R,VCONV%R,VCONV%R,X,Y,
     *               ZSTAR,XCONV%R,YCONV%R,ZCONV,DX_T,DY_T,DZ_T,Z,
     *               SHPP,SHZ,SURDET,DT,IKLE,IFABOR,ELT_T,
     *               ETA,IT3%I,IT4%I,IELM,IELMU,NELEM,NELMAX,
     *               NOMB,NPOIN,NPOIN,NDP,NPLAN,LV,MSK,MASKEL%R,
     *               MESH,MESH%FAC%R,T7%R,T7,.FALSE.,QUAD,NPT,
     *               .FALSE.,.FALSE.)
!
!       COMPUTES THE RIEMANN INVARIANTS W1 AND W4 (SECOND DIMENSION OF TBOR)
!       CARRIED BY THIS FIELD
! 
        IF(NPT.GT.0) THEN 
        DO J=1,NPT
          K=LISPFR(J)
          N=NBOR(K)     
          IF(UNA%R(N).GE.0.D0) THEN
            W1R(K)=-HBTIL%R(J)*(XNEBOR(LISPFR(1))*(VBTIL%R(J)-V%R(N))-
     *                          YNEBOR(LISPFR(1))*(UBTIL%R(J)-U%R(N)))
            IF(NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                TBOR%ADR(ITRAC)%P%R(K+NPTFR)=HBTIL%R(J)*
     *          (TBTIL%ADR(ITRAC)%P%R(K)-T%ADR(ITRAC)%P%R(N))
              ENDDO
            ENDIF
          ELSE
            W1R(K)=-HBOR(K)*(XNEBOR(LISPFR(1))*(VBOR(K)-V%R(N))-
     *                       YNEBOR(LISPFR(1))*(UBOR(K)-U%R(N)) )
            IF(NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                TBOR%ADR(ITRAC)%P%R(K+NPTFR)=
     *          HBOR(K)*(TBOR%ADR(ITRAC)%P%R(K)-T%ADR(ITRAC)%P%R(N))
              ENDDO
            ENDIF
          ENDIF
        ENDDO
        ENDIF
!
!----------------------------------------------------------
!       COMPUTES THE ADVECTION FIELD U + C ACCORDING TO THE 
!       NORMAL DIRECTION AT THE BOUNDARY
!----------------------------------------------------------
!
        IF(NPT.GT.0) THEN
          CALL OS('X=X+CY  ',UNA  , C   , C   ,             1.D0 )
          CALL OS('X=CY    ',UCONV, UNA , UNA , XNEBOR(LISPFR(1)))
          CALL OS('X=CY    ',VCONV, UNA , UNA , YNEBOR(LISPFR(1)))
        ENDIF
!
!----------------------------------------------------------------------
!       CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
!       MAY BE CALLED WITH NPT=0 IF OTHER PROCESSORS STILL AT WORK
!----------------------------------------------------------------------
!
        CALL PRE_SCARACT_THOMPSON(NPOIN,NPTFR,NPT,NDP,NELEM,NELMAX,
     *                          IKLE,MSK,MASKEL,NBOR,LISPFR,X,Y,ITRAV2,
     *                          SHP,XCONV%R,YCONV%R,SHPP,ELT_T)
!
        CALL OS('X=0     ',X=UBTIL)
        CALL OS('X=0     ',X=VBTIL)
        CALL OS('X=0     ',X=HBTIL)
        CALL OS('X=0     ',X=ZBTIL)
!
        CALL SCARACT(FNCAR1,FTILD1,UCONV%R,VCONV%R,VCONV%R,X,Y,
     *               ZSTAR,XCONV%R,YCONV%R,ZCONV,DX_T,DY_T,DZ_T,Z,
     *               SHPP,SHZ,SURDET,DT,IKLE,IFABOR,ELT_T,ETA,
     *               IT3%I,IT4%I,IELM,IELMU,NELEM,NELMAX,NOMB,NPOIN,
     *               NPOIN,NDP,NPLAN,LV,MSK,MASKEL%R,MESH,MESH%FAC%R,
     *               T7%R,T7,.FALSE.,QUAD,NPT,.FALSE.,.FALSE.)
!
!       COMPUTES THE RIEMANN INVARIANTS W2 CARRIED BY THIS ADVECTION FIELD
!
        IF(NPT.GT.0) THEN
        DO J=1,NPT
          K=LISPFR(J)
          N=NBOR(K)
          IF(UNA%R(N).GE.0.D0) THEN
            W2R(K)=(-ZF%R(N)+HBTIL%R(J)+ZBTIL%R(J))*C%R(N)+
     *           HBTIL%R(J)*(XNEBOR(LISPFR(1))*(UBTIL%R(J)-U%R(N))+
     *                     YNEBOR(LISPFR(1))*(VBTIL%R(J)-V%R(N)) )
          ELSE
            W2R(K)=HBOR(K)*(C%R(N)+
     *           (XNEBOR(LISPFR(1))*(UBOR(K)-U%R(N))+
     *            YNEBOR(LISPFR(1))*(VBOR(K)-V%R(N))) )
          ENDIF             
        ENDDO
        ENDIF           
!
!       COMPUTES THE ADVECTION FIELD U-C ACCORDING TO THE
!       NORMAL DIRECTION AT THE BOUNDARY
!    
        IF(NPT.GT.0) THEN
          CALL OS('X=X+CY  ',X=UNA, Y=C , C=-2.D0 )
          CALL OS('X=CY    ',X=UCONV,Y=UNA , C=XNEBOR(LISPFR(1)) )
          CALL OS('X=CY    ',X=VCONV,Y=UNA , C=YNEBOR(LISPFR(1)) )
        ENDIF
!
!       CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
!     
        CALL PRE_SCARACT_THOMPSON(NPOIN,NPTFR,NPT,NDP,NELEM,NELMAX,
     *                            IKLE,MSK,MASKEL,NBOR,LISPFR,X,Y,
     *                            ITRAV2,SHP,XCONV%R,YCONV%R,SHPP,ELT_T)
!
        CALL OS('X=0     ',X=UBTIL)
        CALL OS('X=0     ',X=VBTIL)
        CALL OS('X=0     ',X=HBTIL)
        CALL OS('X=0     ',X=ZBTIL)
!
        CALL SCARACT(FNCAR1,FTILD1,UCONV%R,VCONV%R,VCONV%R,X,Y,ZSTAR,
     *               XCONV%R,YCONV%R,ZCONV,DX_T,DY_T,DZ_T,Z,SHPP,SHZ,
     *               SURDET,DT,IKLE,IFABOR,ELT_T,ETA,IT3%I,IT4%I,IELM,
     *               IELMU,NELEM,NELMAX,NOMB,NPOIN,NPOIN,NDP,NPLAN, 
     *               LV,MSK,MASKEL%R,MESH,MESH%FAC%R,T7%R,T7,
     *               .FALSE.,QUAD,NPT,.FALSE.,.FALSE.)
!
! COMPUTES THE RIEMANN INVARIANTS W3 CARRIED BY THIS ADVECTION FIELD
!
        IF(NPT.GT.0) THEN
        DO J=1,NPT
          K=LISPFR(J)
          N=NBOR(K)
          IF(UNA%R(N).GE.0.D0) THEN
            W3R(K)=(-ZF%R(N)+HBTIL%R(J)+ZBTIL%R(J))*C%R(N)-
     *         HBTIL%R(J)*(XNEBOR(LISPFR(1))*(UBTIL%R(J)-U%R(N))+
     *                   YNEBOR(LISPFR(1))*(VBTIL%R(J)-V%R(N)) )
          ELSE
            W3R(K)=HBOR(K)*(C%R(N)-
     *                 (XNEBOR(LISPFR(1))*(UBOR(K)-U%R(N))+
     *                  YNEBOR(LISPFR(1))*(VBOR(K)-V%R(N))) )
          ENDIF
        ENDDO
        ENDIF
!
!       RE-BUILDS THE TELEMAC-2D VARIABLES
!
        IF(NPT.GT.0) THEN     
        DO J=1,NPT    
          K=LISPFR(J)
          N=NBOR(K)       
          IF(C%R(N)**2.GT.GRAV*HMIN) THEN
            HBOR(K)=(W2R(K)+W3R(K))/(2*C%R(N))                      
            IF(HBOR(K).GT.HMIN) THEN
!             BEWARE TIDAL FLATS, AND HIDDEN PARAMETER 0.1
              HHBOR=MAX(0.1D0,HBOR(K))
             UBOR(K)=(YNEBOR(LISPFR(1))*W1R(K)+XNEBOR(LISPFR(1))*W2R(K)-
     *       HBOR(K)*C%R(N)*XNEBOR(LISPFR(1)))/HHBOR+U%R(N)              
             VBOR(K)=(YNEBOR(LISPFR(1))*W2R(K)-XNEBOR(LISPFR(1))*W1R(K)-
     *       HBOR(K)*C%R(N)*YNEBOR(LISPFR(1)))/HHBOR+V%R(N)
              IF(NTRAC.GT.0) THEN
                DO ITRAC=1,NTRAC
                  TBOR%ADR(ITRAC)%P%R(K)=
     *            TBOR%ADR(ITRAC)%P%R(K+NPTFR)/HHBOR+T%ADR(ITRAC)%P%R(N)
                ENDDO
              ENDIF
            ELSE
!             BECOMES DRY
              HBOR(K)=MAX(0.D0,HBOR(K))
              UBOR(K)=0.D0
              VBOR(K)=0.D0
            ENDIF
          ELSE
!           WAS DRY, H IS GIVEN BY BORD
            UBOR(K)=0.D0
            VBOR(K)=0.D0
          ENDIF
        ENDDO
        ENDIF
!
        IF(NPT.EQ.0) THEN 
!         LOCAL NON CONVERGENCE
          ISTOP=1
        ENDIF
!       DETECTIING GLOBAL CONVERGENCE
        IF(NCSIZE.GT.1) ISTOP=P_IMIN(ISTOP)        
        IF(ISTOP.EQ.0) GO TO 20
!
!       TEST IF(NFRLIQ.GT.0)...
      ENDIF
   
1000  CONTINUE
!
! RECOVERS OF THE EXACT VALUES FOR U AND V, REQUIRED FOR SUB-ITERATIONS
!
      CALL OS('X=X-Y   ' , X=U , Y=FU )
      CALL OS('X=X-Y   ' , X=V , Y=FV )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
