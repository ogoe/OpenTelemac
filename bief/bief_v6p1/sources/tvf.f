!                    **************
                     SUBROUTINE TVF
!                    **************
!
     &(F,FN,FC,H,FXMAT,FXMATPAR,
     & UNSV2D,DT,FXBOR,FXBORPAR,T7,FBOR,SMH,YASMH,FSCEXP,
     & NSEG,NPOIN,NPTFR,GLOSEG,SIZGLO,NBOR,LIMTRA,KDIR,KDDL,OPTSOU,HLIN,
     & IOPT2,FLBORTRA,SURNIT,MESH,SF)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE TRACER FOR FINITE VOLUME SCHEME.
!+                TO COMPLETE.
!
!history  C-T PHAM (LNHE)
!+        27/02/09
!+        V5P9
!+   JMH : DISTINGUISHES BETWEEN FXBOR AND FXBORTRA 
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
!| DT             |-->| PAS DE TEMPS.
!| F              |<--| VALEURS DU TRACEUR A L'ETAPE N+1
!|                |   | DE LA SOUS-ITERATION
!| FBOR           |-->| VALEURS DU TRACEUR SUR LE BORD.
!| FC             |-->| VALEURS DU TRACEUR A L'ETAPE N
!|                |   | DE LA SOUS-ITERATION
!| FLBORTRA       |---| 
!| FN             |-->| VALEURS DU TRACEUR A L'ETAPE N
!| FSCEXP         |-->| FSCE-(1-TETAT)*FN, SEE DIFSOU
!|                |   | SO HERE FSCE-FN, THIS IS NOT VERY CONVENIENT
!|                |   | AS WE NEED HERE FSCE-FC (LOOK UNDER IF(YASMH))
!| FXBOR          |-->| FLUX SUR LE BORD (DEFINI SUR LE BORD)
!|                |   | NON ASSEMBLE
!| FXBORPAR       |-->| FLUX SUR LE BORD (DEFINI SUR TOUT LE DOMAINE
!|                |   | ET ASSEMBLE EN PARALLELE)
!| FXMAT          |-->| MATRICE DE STOCKAGE DES FLUX.
!| FXMATPAR       |-->| IDEM, ASSEMBLE EN PARALLELE.
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT.
!| H              |-->| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
!|                |   | EN SUPPOSANT LA CONTINUITE RESOLUE
!| HLIN           |-->| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
!|                |   | AVEC INTERPOLATION LINEAIRE EN TEMPS
!|                |   | ENTRE HN ET H
!| IOPT2          |---| 
!| KDDL           |---| 
!| KDIR           |---| 
!| LIMTRA         |---| 
!| MESH           |---| 
!| NBOR           |-->| TABLEAU D'INDICES DE NOEUDS SUR LE BORD.
!| NPOIN          |-->| NOMBRE DE NOEUDS DANS LE MAILLAGE.
!| NPTFR          |-->| NOMBRE DE NOEUDS SUR LA FRONTIERE.
!| NSEG           |-->| NOMBRE DE SEGMENTS DANS LE MAILLAGE.
!| OPTSOU         |---| 
!| SF             |---| 
!| SIZGLO         |---| 
!| SMH            |-->| TERME SOURCE DE L'EQUATION DE CONTINUITE.
!| SURNIT         |---| 
!| T7             |---| 
!| UNSV2D         |---| 
!| YASMH          |-->| IF YES, SOURCE TERMS IN SMH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_TVF => TVF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,KDIR,KDDL
      INTEGER, INTENT(IN)             :: SIZGLO,OPTSOU,IOPT2
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMTRA(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,SURNIT
      DOUBLE PRECISION, INTENT(INOUT) :: FLBORTRA(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: FC(NPOIN),H(NPOIN),HLIN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SMH(NPOIN),UNSV2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FSCEXP(NPOIN),FN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FBOR(NPTFR),FXBORPAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG),FXMATPAR(NSEG)
      LOGICAL, INTENT(IN)             :: YASMH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T7,SF
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N
!
!-----------------------------------------------------------------------
!
      IF(IOPT2.EQ.0) THEN
!       CONSERVATIVE ADVECTION FIELD
        DO I = 1,NPOIN
          F(I) = FC(I)
        ENDDO
      ELSEIF(IOPT2.EQ.1) THEN
!       NON CONSERVATIVE ADVECTION FIELD
        DO I = 1,NPOIN
          F(I) = FC(I)*MAX(H(I),1.D-8)/MAX(HLIN(I),1.D-8)
        ENDDO
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TVF : OPTION INCONNUE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TVF: UNKNOWN OPTION'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
!       THE CONTRIBUTION OF FLUXES IS BUILT APART FOR
!       PRELIMINARY PARALLEL ASSEMBLING BEFORE ADDING ON F
        DO I = 1,NPOIN
          T7%R(I) = 0.D0
        ENDDO
        DO I = 1,NSEG
          IF(FXMATPAR(I).LT.0.D0) THEN
            T7%R(GLOSEG(I,1)) = T7%R(GLOSEG(I,1))
     &      - DT/HLIN(GLOSEG(I,1))*UNSV2D(GLOSEG(I,1))
     &      *FXMAT(I)*(FC(GLOSEG(I,2))-FC(GLOSEG(I,1)))
          ELSEIF(FXMATPAR(I).GT.0.D0) THEN
            T7%R(GLOSEG(I,2)) = T7%R(GLOSEG(I,2))
     &      + DT/HLIN(GLOSEG(I,2))*UNSV2D(GLOSEG(I,2))
     &      *FXMAT(I)*(FC(GLOSEG(I,1))-FC(GLOSEG(I,2)))
          ENDIF
        ENDDO
        CALL PARCOM(T7,2,MESH)
        DO I = 1,NPOIN
          F(I) = F(I)+T7%R(I)
        ENDDO
      ELSE
        DO I = 1,NSEG
          IF(FXMATPAR(I).LT.0.D0) THEN
            F(GLOSEG(I,1)) = F(GLOSEG(I,1))
     &      - DT/HLIN(GLOSEG(I,1))*UNSV2D(GLOSEG(I,1))
     &      *FXMAT(I)*(FC(GLOSEG(I,2))-FC(GLOSEG(I,1)))
          ELSEIF(FXMATPAR(I).GT.0.D0) THEN
            F(GLOSEG(I,2)) = F(GLOSEG(I,2))
     &      + DT/HLIN(GLOSEG(I,2))*UNSV2D(GLOSEG(I,2))
     &      *FXMAT(I)*(FC(GLOSEG(I,1))-FC(GLOSEG(I,2)))
          ENDIF
        ENDDO
      ENDIF
!
!     SOURCE TERMS
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            F(I)=F(I)+DT/HLIN(I)*SMH(I)*(FSCEXP(I)+FN(I)-FC(I))
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
         F(I)=F(I)+DT/HLIN(I)*UNSV2D(I)*SMH(I)*(FSCEXP(I)+FN(I)-FC(I))
          ENDDO
        ENDIF
      ENDIF
!
! ON THE DIRICHLET BOUNDARIES, FLUX TERMS TAKEN INTO ACCOUNT
! ON OTHERS, FBOR IS TAKEN AS FN, SO NO CONTRIBUTION
!
      DO I=1,NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
          N=NBOR(I)
          F(N)=F(N)-DT/HLIN(N)*UNSV2D(N)*FXBORPAR(N)*(FBOR(I)-FC(N))
        ELSEIF(LIMTRA(I).EQ.KDDL) THEN
          N=NBOR(I)
          FLBORTRA(I)=FLBORTRA(I)+FXBOR(I)*FC(N)*SURNIT
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END