!                    *************
                     SUBROUTINE OM
!                    *************
!
     &( OP , M , N , D , C , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES.
!code
!+   D: DIAGONAL MATRIX
!+   C: CONSTANT
!+
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON MATRICES M AND N, D AND C.
!+
!+   THE RESULT IS MATRIX M.
!+
!+      OP = 'M=N     '  : COPIES N IN M
!+      OP = 'M=CN    '  : MULTIPLIES N BY C
!+      OP = 'M=M+CN  '  : ADDS CN TO M
!+      OP = 'M=MD    '  : M X D
!+      OP = 'M=DM    '  : D X M
!+      OP = 'M=DMD   '  : D X M X D
!+      OP = 'M=0     '  : SETS M TO 0 (TO BE CHECKED)
!+      OP = 'M=X(M)  '  : NOT SYMMETRICAL FORM OF M
!+      OP = 'M=TN    '  : COPIES TRANSPOSE OF N IN M
!+      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!
!warning  BE CAREFUL IF A NEW OPERATION IS ADDED TO THE LIST
!warning  IF OP CONTAINS N, IT THEN MEANS THAT MATRIX N IS USED
!
!history  ALGIANE FROEHLY
!+        13/02/2008
!+
!+   ADDED OM1113 AND OM1311
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        05/02/2010
!+        V6P0
!+   CALL TO OMSEGBOR MODIFIED, OMSEGPAR SUPPRESSED
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
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| D              |-->| A DIAGONAL MATRIX
!| M              |<->| RESULTING MATRIX
!| MESH           |-->| MESH STRUCTURE
!| N              |-->| MATRIX USED IN FORMULA OP
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_OM => OM
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: C
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: M
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: N,D
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELM1,IELM2,IELN1,IELN2,NELEM,NELMAX
      INTEGER NDIAGM,NDIAGN,NDIAGX,NSEG1,NSEG2
      INTEGER STOM,STON,NPOIN,NPTFR,NPTFX,MDIAGX
      INTEGER SIZXN,SZMXN,NETAGE
!
      CHARACTER*1 TYPDIM,TYPEXM,TYPDIN,TYPEXN
!
      INTEGER, DIMENSION(:), POINTER :: IKLE
!
!-----------------------------------------------------------------------
!
!  CASE WHERE THE STRUCTURE OF M BECOMES THAT OF N
!
      IF(OP(3:8).EQ.'N     '.OR.OP(3:8).EQ.'CN    ') THEN
        CALL CPSTMT(N,M)
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
        CALL CPSTMT(N,M,TRANS=.TRUE.)
      ENDIF
!
!  EXTRACTS THE CHARACTERISTICS OF MATRIX M
!
      TYPDIM = M%TYPDIA
      TYPEXM = M%TYPEXT
      STOM = M%STO
      NDIAGM = M%D%DIM1
      MDIAGX = M%D%MAXDIM1
      IELM1 = M%ELMLIN
      IELM2 = M%ELMCOL
!
      IF(OP(3:8).EQ.'X(M)  ') THEN
        IF(M%ELMLIN.NE.M%ELMCOL) THEN
          IF(LNG.EQ.1) WRITE(LU,900) M%NAME
          IF(LNG.EQ.2) WRITE(LU,901) M%NAME
900       FORMAT(1X,'OM (BIEF) : M (NOM REEL : ',A6,') NON CARREE')
901       FORMAT(1X,'OM (BIEF) : M (REAL NAME: ',A6,') NOT SQUARE')
          IF(LNG.EQ.1) WRITE(LU,700)
          IF(LNG.EQ.2) WRITE(LU,701)
700       FORMAT(1X,'            EST DEJA NON SYMETRIQUE')
701       FORMAT(1X,'            IS ALREADY NON SYMMETRICAL')
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(M%X%MAXDIM2*M%X%MAXDIM1.LT.
     &      BIEF_DIM1_EXT(IELM1,IELM2,STOM,'Q',MESH)*
     &      BIEF_DIM2_EXT(IELM1,IELM2,STOM,'Q',MESH)    ) THEN
            IF(LNG.EQ.1) WRITE(LU,400) M%NAME
            IF(LNG.EQ.2) WRITE(LU,401) M%NAME
            IF(LNG.EQ.1) WRITE(LU,800)
            IF(LNG.EQ.2) WRITE(LU,801)
800         FORMAT(1X,'            POUR DEVENIR NON SYMETRIQUE')
801         FORMAT(1X,'            TO BECOME NON SYMMETRICAL')
            CALL PLANTE(1)
            STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!  EXTRACTS THE CHARACTERISTICS OF MATRIX N (OPTIONAL)
      IF(INCLUS(OP,'N')) THEN
        TYPDIN = N%TYPDIA
        TYPEXN = N%TYPEXT
        STON   = N%STO
        NDIAGN = N%D%DIM1
        NDIAGX = N%D%MAXDIM1
!
        SIZXN = N%X%DIM1
        SZMXN = N%X%MAXDIM1
!
! 07/02/03 : DIVISION BY NDIAGN=0 AVOIDED (SUBDOMAIN WITHOUT BOUNDARY POINTS
!            IN PARALLEL). COURTESY OLIVER GOETHEL (HANNOVER UNIVERSITY)
!
        IF(NDIAGN.GT.0) THEN
          NETAGE = NDIAGM/NDIAGN - 1
        ELSE
          NETAGE = 0
        ENDIF
!
        IELN1 = N%ELMLIN
        IELN2 = N%ELMCOL
        IF(NDIAGN.GT.MDIAGX) THEN
         IF(LNG.EQ.1) WRITE(LU,400) M%NAME
         IF(LNG.EQ.2) WRITE(LU,401) M%NAME
400      FORMAT(1X,'OM (BIEF) : M (NOM REEL : ',A6,') TROP PETITE')
401      FORMAT(1X,'OM (BIEF) : M (REAL NAME: ',A6,') TOO SMALL')
         STOP
        ENDIF
      ELSE
        IELN1 = IELM1
        IELN2 = IELM2
        STON = STOM
      ENDIF
!
!-----------------------------------------------------------------------
!
!  DEPLOYMENT OF THE MESH STRUCTURE
!
!     STANDARD MATRIX
      IF(DIMENS(IELM1).EQ.MESH%DIM) THEN
        IKLE=>MESH%IKLE%I
        NELEM = MESH%NELEM
        NELMAX= MESH%NELMAX
      ELSE
!     BOUNDARY MATRIX
        IKLE=>MESH%IKLBOR%I
        NELEM  = MESH%NELEB
        NELMAX = MESH%NELEBX
      ENDIF
!
      NPOIN= MESH%NPOIN
      NPTFR= MESH%NPTFR
      NPTFX= MESH%NPTFRX
!
!-----------------------------------------------------------------------
!
!  TRADITIONAL EBE STORAGE:
!
      IF(STOM.EQ.1.AND.STON.EQ.1) THEN
!
      IF(IELM1.EQ.1.AND.IELM2.EQ.1) THEN
!
!     ELEMENTS WITH 2 POINTS
!
      CALL OM0101(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                 N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                 IKLE,NELEM,NELMAX,NDIAGM)
!
!
!     ELEMENTS WITH 3 POINTS
!
      ELSEIF( (IELM1.EQ.2 .AND.IELM2.EQ.2 ) .OR.
     &        (IELM1.EQ.11.AND.IELM2.EQ.11) .OR.
     &        (IELM1.EQ.61.AND.IELM2.EQ.61) .OR.
     &        (IELM1.EQ.81.AND.IELM2.EQ.81)       ) THEN
!
        IF( (IELN1.EQ.2 .AND.IELN2.EQ.2 ) .OR.
     &      (IELN1.EQ.11.AND.IELN2.EQ.11) .OR.
     &      (IELN1.EQ.61.AND.IELN2.EQ.61) .OR.
     &      (IELN1.EQ.81.AND.IELN2.EQ.81)         ) THEN
!
          CALL OM1111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN,D%R,C,
     &                     IKLE,NELEM,NELMAX,NDIAGM)
!
        ELSEIF(IELN1.EQ.1.AND.IELN2.EQ.1) THEN
!
          CALL OM1101(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                     MESH%NULONE%I,MESH%NELBOR%I,
     &                     MESH%NBOR%I,
     &                     NELMAX,NDIAGM,NDIAGN,NDIAGX)
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     ELEMENTS WITH 4 POINTS
!
      ELSEIF( (IELM1.EQ.21.AND.IELM2.EQ.21) .OR.
     &        (IELM1.EQ.71.AND.IELM2.EQ.71) .OR.
     &        (IELM1.EQ.31.AND.IELM2.EQ.31) .OR.
     &        (IELM1.EQ.51.AND.IELM2.EQ.51) .OR.
     &        (IELM1.EQ.12.AND.IELM2.EQ.12)      ) THEN
!
        IF(   (IELN1.EQ.21.AND.IELN2.EQ.21) .OR.
     &        (IELN1.EQ.71.AND.IELN2.EQ.71) .OR.
     &        (IELN1.EQ.31.AND.IELN2.EQ.31) .OR.
     &        (IELN1.EQ.51.AND.IELN2.EQ.51) .OR.
     &        (IELN1.EQ.12.AND.IELN2.EQ.12)      ) THEN
!
          CALL OM2121(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                IKLE,NELEM,NELMAX,NDIAGM)
        ELSEIF(  (IELM1.EQ.12.AND.IELM2.EQ.12) .AND.
     &           (IELN1.EQ.1 .AND.IELN2.EQ.1 )   ) THEN
!
          CALL OM1201(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                     MESH%NULONE%I,MESH%NELBOR%I,
     &                     MESH%NBOR%I,
     &                     NELMAX,NDIAGM,NDIAGN,NDIAGX)
        ELSEIF(( (IELM1.EQ.51.AND.IELM2.EQ.51) .AND.
     &           (IELN1.EQ.61.AND.IELN2.EQ.61))   ) THEN
!         PRISMS SPLIT IN TETRAHEDRONS M INTERIOR MATRIX
!                                      N LATERAL BOUNDARY MATRIX
          CALL OM5161(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN,C,
     &                MESH%NULONE%I,MESH%NELBOR%I,MESH%NBOR%I,
     &                NELMAX,NDIAGN,SIZXN,SZMXN)
!
        ELSEIF(( (IELM1.EQ.31.AND.IELM2.EQ.31) .AND.
     &           (IELN1.EQ.81.AND.IELN2.EQ.81))   ) THEN
!         NOT STRUCTURED TETRAHEDRONS    M INTERIOR MATRIX
!                                        N LATERAL BOUNDARY MATRIX
          CALL OM3181(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN,C,
     &                MESH%NULONE%I,MESH%NELBOR%I,MESH%NBOR%I,
     &                NELMAX,NDIAGN,MESH%NELEB,SZMXN)
!
        ELSEIF(  (IELM1.EQ.51.AND.IELM2.EQ.51) .AND.
     &           (IELN1.EQ.11.AND.IELN2.EQ.11)   ) THEN
!         PRISMS SPLIT IN TETRAHEDRONS M INTERIOR MATRIX
!                                      N BOTTOM (NB) OR SURFACE (NS) BOUNDARY MATRIX
!                                      OPERATIONS M+NB AND M+NS
          CALL OM5111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                NDIAGN,NDIAGX,SIZXN,SZMXN,NETAGE,NELMAX)
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     RECTANGULAR MATRICES WITH 3 AND 4 POINTS
!
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.12) THEN
!
        IF((IELN1.EQ.11.AND.IELN2.EQ.12).OR.
     &     (IELN1.EQ.12.AND.IELN2.EQ.11.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1112(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                     IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     RECTANGULAR MATRICES WITH 4 AND 3 POINTS
!
      ELSEIF(IELM1.EQ.12.AND.IELM2.EQ.11) THEN
!
        IF((IELN1.EQ.12.AND.IELN2.EQ.11).OR.
     &     (IELN1.EQ.11.AND.IELN2.EQ.12.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1211(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     ELEMENTS WITH 6 POINTS
!
      ELSEIF( (IELM1.EQ.41.AND.IELM2.EQ.41).OR.
     &        (IELM1.EQ.13.AND.IELM2.EQ.13)      ) THEN
!
        IF( (IELN1.EQ.41.AND.IELN2.EQ.41).OR.
     &      (IELN1.EQ.13.AND.IELN2.EQ.13)        ) THEN
!
          CALL OM4141(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                IKLE,NELEM,NELMAX,NDIAGM)
!
        ELSEIF(IELN1.EQ.71.AND.IELN2.EQ.71) THEN
!
          CALL OM4121(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                   N%D%R,TYPDIN,N%X%R,TYPEXN,C,
     &                   MESH%NULONE%I,MESH%NELBOR%I,MESH%NBOR%I,
     &                   NELMAX,NDIAGN,SIZXN,SZMXN)
!
        ELSEIF(IELN1.EQ.11.AND.IELN2.EQ.11) THEN
!
          CALL OM4111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                     NDIAGN,NDIAGX,SIZXN,SZMXN,NETAGE,NELMAX)
!
        ELSEIF(  (IELM1.EQ.13.AND.IELM2.EQ.13) .AND.
     &           (IELN1.EQ.2 .AND.IELN2.EQ.2 )   ) THEN
!
          CALL OM1302(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                     MESH%NULONE%I,MESH%NELBOR%I,
     &                     MESH%NBOR%I,
     &                     NELMAX,NDIAGM,NPTFR,NPTFX)
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     RECTANGULAR MATRICES WITH 3 AND 6 POINTS
!
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
!
        IF((IELN1.EQ.11.AND.IELN2.EQ.13).OR.
     &     (IELN1.EQ.13.AND.IELN2.EQ.11.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1113(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                     IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     RECTANGULAR MATRICES WITH 6 AND 3 POINTS
!
      ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
!
        IF((IELN1.EQ.13.AND.IELN2.EQ.11).OR.
     &     (IELN1.EQ.11.AND.IELN2.EQ.13.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1311(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!  COMBINATION OF IELM1 AND IELM2 NOT IMPLEMENTED: ERROR
!
      ELSE
         IF (LNG.EQ.1) WRITE(LU,100) M%NAME
         IF (LNG.EQ.2) WRITE(LU,101) M%NAME
         IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
         IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
         IF (LNG.EQ.1) WRITE(LU,410) STOM,STON
         IF (LNG.EQ.2) WRITE(LU,411) STOM,STON
         IF (LNG.EQ.1) WRITE(LU,300)
         IF (LNG.EQ.2) WRITE(LU,301)
         CALL PLANTE(1)
         STOP
      ENDIF
!
      ELSEIF(STOM.EQ.3.AND.STON.EQ.3) THEN
!
!  STORAGE BY SEGMENT
!
        IF(M%ELMCOL.NE.N%ELMCOL.OR.M%ELMLIN.NE.N%ELMLIN) THEN
          WRITE(LU,*) 'M ET N DE STRUCTURES DIFFERENTES'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        NSEG1 = BIEF_NBSEG(M%ELMLIN,MESH)
        NSEG2 = BIEF_NBSEG(M%ELMCOL,MESH)
!
!       IN LINEAR-QUADRATIC RECTANGULAR MATRICES, PURELY QUADRATIC
!       SEGMENTS ARE NOT CONSIDERED (NUMBER 13,14 AND 15, SO 3 PER ELEMENT)
!
        IF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
          NSEG2=NSEG2-3*NELEM
        ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
          NSEG1=NSEG1-3*NELEM
        ENDIF
!
!       IN LINEAR-QUADRATIC RECTANGULAR MATRICES, PURELY QUADRATIC
!       SEGMENTS ARE NOT CONSIDERED (NUMBER 13,14 AND 15, SO 3 PER ELEMENT)
!
        CALL OMSEG(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                  N%D%R,TYPDIN,N%X%R,TYPEXN,D%R,C,
     &                  NDIAGM,NSEG1,NSEG2,MESH%GLOSEG%I,
     &                  MESH%GLOSEG%MAXDIM1)
!
      ELSEIF(STOM.EQ.3.AND.STON.EQ.1) THEN
!
!       EDGE-BASED STORAGE FOR M AND EBE FOR N
!       THIS CAN HAPPEN ONLY WHEN N IS A BOUNDARY MATRIX
!
        IF(  (M%ELMLIN.EQ.11.AND.M%ELMCOL.EQ.11.AND.
     &        N%ELMLIN.EQ.1 .AND.N%ELMCOL.EQ.1) .OR.
     &       (M%ELMLIN.EQ.12.AND.M%ELMCOL.EQ.12.AND.
     &        N%ELMLIN.EQ.1 .AND.N%ELMCOL.EQ.1) .OR.
     &       (M%ELMLIN.EQ.13.AND.M%ELMCOL.EQ.13.AND.
     &        N%ELMLIN.EQ.1 .AND.N%ELMCOL.EQ.1) .OR.
     &       (M%ELMLIN.EQ.13.AND.M%ELMCOL.EQ.13.AND.
     &        N%ELMLIN.EQ.2 .AND.N%ELMCOL.EQ.2)      ) THEN
!
          NSEG1 = BIEF_NBSEG(M%ELMLIN,MESH)
          NSEG2 = BIEF_NBSEG(M%ELMCOL,MESH)
!
          CALL OMSEGBOR(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                       N%D%R,TYPDIN,N%X%R,TYPEXN,D%R,C,
     &                       NDIAGM,NSEG1,NSEG2,MESH%NBOR%I,
     &                       MESH%KP1BOR%I,NPTFR,
     &                       M%ELMLIN,N%ELMLIN,
     &                       BIEF_NBSEG(11,MESH))
!
        ELSE
          WRITE(LU,*) 'OM : UNEXPECTED CASE IN SEGMENT STORAGE'
          WRITE(LU,*) '     M%ELMLIN=',M%ELMLIN
          WRITE(LU,*) '     M%ELMCOL=',M%ELMCOL
          WRITE(LU,*) '     M%NAME=',M%NAME
          WRITE(LU,*) '     N%ELMLIN=',N%ELMLIN
          WRITE(LU,*) '     N%ELMCOL=',N%ELMCOL
          WRITE(LU,*) '     N%NAME=',N%NAME
          WRITE(LU,*) '     IMPLEMENTATION MISSING'
          CALL PLANTE(1)
          STOP
        ENDIF
!
!  COMBINATION OF IELM1 AND IELM2 NOT IMPLEMENTED: ERROR
!
!     ELSE
!        IF (LNG.EQ.1) WRITE(LU,100) M%NAME
!        IF (LNG.EQ.2) WRITE(LU,101) M%NAME
!        IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
!        IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
!        IF (LNG.EQ.1) WRITE(LU,410) STOM,STON
!        IF (LNG.EQ.2) WRITE(LU,411) STOM,STON
!        IF (LNG.EQ.1) WRITE(LU,300)
!        IF (LNG.EQ.2) WRITE(LU,301)
!        CALL PLANTE(1)
!        STOP
!     ENDIF
!
      ELSE
!
!  STORAGE COMBINATION NOT IMPLEMENTED
!
         IF (LNG.EQ.1) WRITE(LU,100) M%NAME
         IF (LNG.EQ.2) WRITE(LU,101) M%NAME
         IF (LNG.EQ.1) WRITE(LU,410) STOM,STON
         IF (LNG.EQ.2) WRITE(LU,411) STOM,STON
         IF (LNG.EQ.1) WRITE(LU,300)
         IF (LNG.EQ.2) WRITE(LU,301)
         CALL PLANTE(1)
         STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  RE-ENCODES THE NEW TYPE
!
      M%TYPDIA = TYPDIM
      M%TYPEXT = TYPEXM
      IF(OP(3:8).EQ.'X(M)  ') THEN
        M%X%DIM1=BIEF_DIM1_EXT(IELM1,IELM2,STOM,'Q',MESH)
        M%X%DIM2=BIEF_DIM2_EXT(IELM1,IELM2,STOM,'Q',MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
100      FORMAT(1X,'OM (BIEF) : MATRICE M (NOM REEL : ',A6,')')
150      FORMAT(1X,'OM (BIEF) : MATRICE N (NOM REEL : ',A6,')')
200      FORMAT(1X,'            IELM1 = ',1I6,' IELM2 = ',1I6)
250      FORMAT(1X,'            IELN1 = ',1I6,' IELN2 = ',1I6)
300      FORMAT(1X,'            CAS NON PREVU')
410      FORMAT(1X,'ET STOCKAGES   M  : ',1I6,'    N  : ',1I6)
!
101      FORMAT(1X,'OM (BIEF) : MATRIX  M (REAL NAME:',A6,')')
151      FORMAT(1X,'OM (BIEF) : MATRIX  N (REAL NAME:',A6,')')
201      FORMAT(1X,'            IELM1 = ',1I6,' IELM2 = ',1I6)
251      FORMAT(1X,'            IELN1 = ',1I6,' IELN2 = ',1I6)
301      FORMAT(1X,'            THIS CASE IS NOT IMPLEMENTED')
411      FORMAT(1X,'AND STORAGES   M  : ',1I6,' STON  : ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
