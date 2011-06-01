!                    *****************
                     SUBROUTINE INBIEF
!                    *****************
!
     &(LIHBOR,KLOG,IT1,IT2,IT3,LVMAC,IELMX,
     & LAMBD0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE DATA STRUCTURE FOR BIEF.
!+                THE INTEGER AND REAL ARRAYS DESCRIBING THE MESH
!+                ARE BUILT AND STORED IN MESH.
!
!history
!+        22/01/2008
!+
!+   DYNAMIC ALLOCATION OF IKLESTR DELETED
!
!history
!+        29/02/2008
!+
!+   NORMAB MODIFIED; XSEG, YSEG NO LONGER USED
!
!history
!+        20/03/2008
!+
!+   NBOR, IKLBOR ADAPTED FOR QUADRATIC TRIANGLES
!
!history
!+        14/08/2008
!+
!+   PARINI MODIFIED
!
!history  J-M HERVOUET (LNHE)     ; REGINA NEBAUER; LAM MINH PHUONG; EMILE RAZAFINDRAKOTO
!+        05/02/2010
!+        V6P0
!+   COMP_SEG UPDATED FOR QUADRATIC TRIANGLES
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
!| EQUA           |-->| IDENTIFICATION OF PROGRAM OR EQUATIONS SOLVED
!| IELMX          |-->| THE MORE COMPLEX ELEMENT USED (FOR MEMORY)
!| IT1            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT2            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT3            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LAMBD0         |-->| LATITUDE OF ORIGIN POINT (SPHERICAL COORDINATES)
!| LIHBOR         |-->| TYPES OF BOUNDARY CONDITIONS ON DEPTH
!| LVMAC          |-->| VECTOR LENGTH (IF VECTOR MACHINE)
!| MESH           |-->| MESH STRUCTURE
!| OPTASS         |-->| OPTION FOR MATRIX STORAGE.
!| PRODUC         |-->| OPTION FOR MATRIX x VECTOR PRODUCT.
!| SPHERI         |-->| LOGICAL, IF YES : SPHERICAL COORDINATES.
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_INBIEF => INBIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: IELMX,OPTASS,PRODUC,KLOG,LVMAC
      INTEGER, INTENT(IN)            :: LIHBOR(*)
      DOUBLE PRECISION, INTENT(IN)   :: LAMBD0
      LOGICAL, INTENT(IN)            :: SPHERI
      CHARACTER(LEN=20)              :: EQUA
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: T1,T2,IT1,IT2,IT3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IELEM,NELEM,NELMAX,NPTFR,NPOIN,IELM
      INTEGER MXPTVS,MXELVS,NPLAN
      INTEGER LV,NDP,IDP,I1,I2,I3,NPOIN2
      INTEGER NPTFR2,NELEM2,NELMAX2,NELEB2,NELEB
!
      DOUBLE PRECISION C,Z(1),X2,X3,Y2,Y3
!
!-----------------------------------------------------------------------
!     FOR CALL TO VOISIN31
      INTEGER IKLESTR(1,3)
!
!     DEPLOYMENT OF THE DATA STRUCTURE
!
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
      NPOIN = MESH%NPOIN
      IELM  = MESH%X%ELM
      NDP   = BIEF_NBPEL(IELM,MESH)
      NPTFR = MESH%NPTFR
      NELEB = MESH%NELEB
!
!     WITH PRISMS, DIFFERENT FROM 2D VALUES, OTHERWISE
!
      IF(IELM.EQ.41.OR.IELM.EQ.51) THEN
        NPOIN2  =BIEF_NBPTS(11,MESH)
        NELEM2  =BIEF_NBPTS(10,MESH)
        NELMAX2 =BIEF_NBMPTS(10,MESH)
        NPTFR2  =BIEF_NBPTS(1,MESH)
        NPLAN   =NPOIN/NPOIN2
      ELSEIF(IELM.EQ.11.OR.IELM.EQ.31) THEN
        NPOIN2  =NPOIN
        NELEM2  =NELEM
        NELMAX2 =NELMAX
        NPTFR2  =NPTFR
        NELEB2  =NELEB
        NPLAN   =1
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  PARALLEL MODE : 1) BUILDS KNOLG AND CONVERTS TO SUB-DOMAIN
!                     GLOBAL NUMBERS
!                  2) INITIALISES THE ARRAYS NHP,NHM
!                     INDPU,FAC, ETC.
!
!
      IF(NCSIZE.GT.1) THEN
!       1)
        CALL PARAGL(MESH%KNOGL%I,MESH%KNOGL%DIM1,MESH%KNOLG%I,
     &              MESH%NBOR%I ,MESH%NACHB%I,NPTFR2,NPOIN2)
!
!       2)
        CALL PARINI(MESH%NHP%I,MESH%NHM%I,MESH%INDPU%I,MESH%FAC,
     &              NPOIN2,MESH%NACHB%I,NPLAN,MESH,
     &              MESH%NB_NEIGHB,MESH%NB_NEIGHB_SEG,
     &              NELEM2,MESH%IFAPAR%I)
!
!       PRISMS: COMPLEMENTS FAC
        IF(IELM.EQ.41.OR.IELM.EQ.51) THEN
          DO I = 2,NPLAN
            CALL OV_2('X=Y     ',MESH%FAC%R,I,MESH%FAC%R,1,
     &                           MESH%FAC%R,1,0.D0,NPOIN2,NPOIN2)
          ENDDO
        ENDIF
!
      ELSE
!       THESE STUCTURES ARE ALLOCATED IN PARINI
        CALL BIEF_ALLVEC(2,MESH%NB_NEIGHB_PT,'NBNGPT',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%LIST_SEND   ,'LSSEND',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%NH_COM      ,'NH_COM',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%NB_NEIGHB_PT_SEG,'NBNGSG',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%LIST_SEND_SEG,'LSSESG',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%NH_COM_SEG  ,'NH_CSG',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,MESH%BUF_SEND    ,'BUSEND',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,MESH%BUF_RECV    ,'BURECV',0,1,0,MESH)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE NEIGHBOURS OF THE BOUNDARY FACES (TRIANGULAR MESH)
!
!     NOTE: SEE CPIKLE2 AND CPIKLE3 IN 3D. IKLE CAN HERE BE 3D BECAUSE
!           THE BEGINNING OF IKLE IN 3D IS THE SAME AS THAT IN 2D (THE
!           FIRST 3 POINTS OF THE PRISMS OR TETRAHEDRONS CORRESPOND
!           TO THE 3 POINTS OF THE BOTTOM TRIANGLES)
!
!
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
        CALL VOISIN(MESH%IFABOR%I,NELEM2,NELMAX2,IELM,MESH%IKLE%I,
     &              MESH%IKLE%DIM1,
     &              NPOIN2,MESH%NACHB%I,MESH%NBOR%I,NPTFR2,IT1%I,IT2%I)
!
      ELSEIF (IELM.EQ.31) THEN
        CALL VOISIN31(MESH%IFABOR%I,NELEM2,NELMAX2,IELM,MESH%IKLE%I,
     &                MESH%IKLE%DIM1,
     &                NPOIN2,MESH%NACHB%I,MESH%NBOR%I,NPTFR2,
     &                LIHBOR,KLOG,IKLESTR,1,NELEB2)
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!
      MXPTVS = MESH%MXPTVS
      MXELVS = MESH%MXELVS
      CALL ELEBD(MESH%NELBOR%I,MESH%NULONE%I,MESH%KP1BOR%I,
     &           MESH%IFABOR%I,MESH%NBOR%I,MESH%IKLE%I,MESH%IKLE%DIM1,
     &           MESH%IKLBOR%I,NELEM2,NELMAX2,
     &           NPOIN2,NPTFR2,IELM,
     &           LIHBOR,KLOG,MESH%IFANUM%I,
     &           OPTASS,MESH%ISEG%I,
     &           IT1%I,IT2%I,IT3%I)
!
      ELSEIF(IELM.EQ.31) THEN
      CALL ELEBD31(MESH%NELBOR%I,MESH%NULONE%I,MESH%IKLBOR%I,
     &             MESH%IFABOR%I,MESH%NBOR%I,MESH%IKLE%I,
     &             NELEM2,NELEB2,NELMAX2,NPOIN2,NPTFR2,IELM)
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COMPLETES ARRAYS FOR PRISMS
!
      IF(IELM.EQ.41) THEN
        CALL ELEB3D(MESH%IKLE%I,MESH%NBOR%I,MESH%KP1BOR%I,
     &              MESH%NELBOR%I,MESH%IKLBOR%I,
     &              MESH%NULONE%I,NELEM2,NPOIN2,NPLAN,NPLAN-1,NPTFR2)
!
!     COMPLETES ARRAYS FOR TETRAHEDRONS
!
      ELSEIF(IELM.EQ.51) THEN
        CALL ELEB3DT(MESH%IKLE%I,MESH%NBOR%I,MESH%KP1BOR%I,
     &               MESH%NELBOR%I,MESH%IKLBOR%I,
     &               MESH%NULONE%I,NELEM2,NELMAX2,
     &               NPOIN2,NPLAN,NPLAN-1,NPTFR2)
      ELSEIF(IELM.NE.11.AND.IELM.NE.31) THEN
        WRITE(LU,*) 'INBIEF UNEXPECTED ELEMENT: ',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
! LOOKS FOR VECTORISATION POSSIBILITIES
!
      IF(IELM.EQ.11) THEN
!
      IF(LVMAC.NE.1) THEN
        IF(LNG.EQ.1) WRITE(LU,200) LVMAC
        IF(LNG.EQ.2) WRITE(LU,201) LVMAC
200     FORMAT(1X,'INBIEF (BIEF) : MACHINE VECTORIELLE',/,1X,
     &  'AVEC LONGUEUR DE VECTEUR :',1I6,
     &  ' (SELON VOS DONNEES OU DANS LE DICTIONNAIRE DES MOTS-CLES)')
201     FORMAT(1X,'INBIEF (BIEF): VECTOR MACHINE',/,1X,
     &  'WITH VECTOR LENGTH :',1I6,
     &  ' (ACCORDING TO YOUR DATA OR IN THE DICTIONNARY OF KEY-WORDS)')
        CALL VECLEN(LV,NDP,MESH%IKLE%I,NELEM,NELMAX,NPOIN,T1%R)
        IF(LV.LT.LVMAC) THEN
          IF(LNG.EQ.1) WRITE(LU,300) LV
          IF(LNG.EQ.2) WRITE(LU,301) LV
300       FORMAT(1X,'LONGUEUR LIMITEE A ',1I4,' PAR LA NUMEROTATION DES
     &ELEMENTS (VOIR LA DOCUMENTATION DE STBTEL)')
301       FORMAT(1X,'THIS LENGTH IS REDUCED TO ',1I4,' BY THE NUMBERING
     &OF THE ELEMENTS (SEE STBTEL DOCUMENTATION)')
        ENDIF
      ELSE
        LV = 1
        IF(LNG.EQ.1) WRITE(LU,400)
        IF(LNG.EQ.2) WRITE(LU,401)
400     FORMAT(1X,'INBIEF (BIEF) : MACHINE NON VECTORIELLE',
     &                                           ' (SELON VOS DONNEES)')
401     FORMAT(1X,'INBIEF (BIEF): NOT A VECTOR MACHINE',
     &                                      ' (ACCORDING TO YOUR DATA)')
      ENDIF
!
      MESH%LV = LV
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(SPHERI.AND.IELM.EQ.11) THEN
!
        CALL LATITU(MESH%COSLAT%R,MESH%SINLAT%R,LAMBD0,MESH%Y%R,NPOIN)
        CALL CORLAT
!
! RELEASE 5.5
        CALL CPSTVC(MESH%X,T1)
        CALL CPSTVC(MESH%Y,T2)
        DO I=1,NPOIN
          T1%R(I)=MESH%X%R(I)*MESH%COSLAT%R(I)
          T2%R(I)=MESH%Y%R(I)*MESH%COSLAT%R(I)
        ENDDO
!
! RELEASE 5.4 (IN PARAMETER ESTIMATION COSLAT MAY HAVE BEEN CHANGED
!              INTO QUASI-BUBBLE BY A PREVIOUS RUN, AND OS STOPS)
!       CALL OS( 'X=YZ    ' , T1 , MESH%X , MESH%COSLAT , C )
!       CALL OS( 'X=YZ    ' , T2 , MESH%Y , MESH%COSLAT , C )
!
      ELSE
!
        CALL OS( 'X=Y     ' , X=T1 , Y=MESH%X )
        CALL OS( 'X=Y     ' , X=T2 , Y=MESH%Y )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CONVERTS TO COORDINATES BY ELEMENTS (ONLY IN TRIANGLES)
!
      IF(IELM.EQ.11) THEN
!
        CALL PTTOEL(MESH%XEL,T1,MESH)
        CALL PTTOEL(MESH%YEL,T2,MESH)
!
!  CONVERTS TO A LOCAL SYSTEM IN X AND Y
!
        DO 10 IDP=2,NDP
        CALL OV_2('X=X-Y   ',MESH%XEL%R,IDP,
     &                       MESH%XEL%R,1  ,
     &                       MESH%XEL%R,1  , C , NELMAX , NELEM )
        CALL OV_2('X=X-Y   ',MESH%YEL%R,IDP,
     &                       MESH%YEL%R,1  ,
     &                       MESH%YEL%R,1  , C , NELMAX , NELEM )
10      CONTINUE
!
        CALL OV('X=C     ', MESH%XEL%R , Z , Z , 0.D0 , NELEM )
        CALL OV('X=C     ', MESH%YEL%R , Z , Z , 0.D0 , NELEM )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES THE GEOMETRICAL COEFFICIENTS FOR EACH ELEMENT
!
      IF(IELM.EQ.11) THEN
!
        CALL GEOELT(MESH%SURDET%R,MESH%SURFAC%R,
     &              MESH%XEL%R   ,MESH%YEL%R   ,NELEM,NELMAX,IELM)
!
! FOR THE TIME BEING, SURDET IS ONLY USED BY CARACT, WHICH DOES NOT
! WORK ON THE MESH IN SPHERICAL COORDINATES.
! ERASES SURDET COMPUTED BY GEOELE FROM XEL AND YEL
!
        IF(SPHERI) THEN
!
         DO IELEM = 1 , NELEM
!
         I1 = MESH%IKLE%I(IELEM)
         I2 = MESH%IKLE%I(IELEM+NELMAX)
         I3 = MESH%IKLE%I(IELEM+2*NELMAX)
         X2 = - MESH%X%R(I1) + MESH%X%R(I2)
         X3 = - MESH%X%R(I1) + MESH%X%R(I3)
         Y2 = - MESH%Y%R(I1) + MESH%Y%R(I2)
         Y3 = - MESH%Y%R(I1) + MESH%Y%R(I3)
!
         MESH%SURDET%R(IELEM) = 1.D0 / (X2*Y3 - X3*Y2)
!
         ENDDO
!
        ENDIF
!
      ELSEIF(IELM.EQ.41.OR.IELM.EQ.51.OR.IELM.EQ.31) THEN
!
!        FOR PRISMS, SURFAC IS THE SURFACE OF THE TRIANGLES
!
         DO IELEM = 1 , NELEM
!
         I1 = MESH%IKLE%I(IELEM)
         I2 = MESH%IKLE%I(IELEM+NELMAX)
         I3 = MESH%IKLE%I(IELEM+2*NELMAX)
         X2 = - MESH%X%R(I1) + MESH%X%R(I2)
         X3 = - MESH%X%R(I1) + MESH%X%R(I3)
         Y2 = - MESH%Y%R(I1) + MESH%Y%R(I2)
         Y3 = - MESH%Y%R(I1) + MESH%Y%R(I3)
!
         MESH%SURFAC%R(IELEM) = 0.5D0 * (X2*Y3 - X3*Y2)
!
         ENDDO
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
      ENDIF
!
!-----------------------------------------------------------------------
!
! DEFINES THE OUTGOING NORMALS AT THE BOUNDARIES
!         AND THE DISTANCES TO THE BOUNDARY
!
      IF(IELM.EQ.11) THEN
!
      CALL NORMAB(MESH%XNEBOR%R,MESH%YNEBOR%R,
     &            MESH%XSGBOR%R,MESH%YSGBOR%R,
     &            MESH%DISBOR%R,MESH%SURFAC%R,NELMAX,
     &            MESH%NBOR%I,MESH%KP1BOR%I,MESH%NELBOR%I,
     &            MESH%LGSEG%R,NPTFR,
     &            MESH%X%R,MESH%Y%R,MESH,T1)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  DATA STRUCTURE FOR EDGE-BASED STORAGE (FROM 5.9 ON ALWAYS DONE IN 2D)
!  SEE CALL TO COMP_SEG BELOW TO COMPLETE THE STRUCTURE
!
      IF(IELM.EQ.11) THEN
!
      CALL STOSEG(MESH%IFABOR%I,NELEM,NELMAX,NELMAX,IELMX,MESH%IKLE%I,
     &            MESH%NBOR%I,NPTFR,
     &            MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,
     &            MESH%ELTSEG%I,MESH%ORISEG%I,MESH%NSEG,
     &            MESH%KP1BOR%I,MESH%NELBOR%I,MESH%NULONE%I,
     &            MESH%KNOLG%I)
!
      ELSEIF(IELM.EQ.41.AND.OPTASS.EQ.3) THEN
!
      CALL STOSEG41(MESH%IFABOR%I,NELEM,NELMAX,IELMX,MESH%IKLE%I,
     &              MESH%NBOR%I,NPTFR,
     &              MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,
     &              MESH%ELTSEG%I,MESH%ORISEG%I,MESH%NSEG,
     &              MESH%KP1BOR%I,MESH%NELBOR%I,MESH%NULONE%I,
     &              NELMAX2,NELEM2,NPTFR2,NPOIN2,NPLAN,MESH%KNOLG%I,
     &              BIEF_NBSEG(11,MESH))
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1.AND.IELM.EQ.11) THEN
!
!       COMPLETES NH_COM_SEG WITH SEGMENT NUMBERS ONCE ELTSEG IS KNOWN
!
        CALL COMP_NH_COM_SEG(MESH%ELTSEG%I,NELEM,MESH%NH_COM_SEG%I,
     &                       MESH%NH_COM_SEG%DIM1,MESH%NB_NEIGHB_SEG,
     &                       MESH%NB_NEIGHB_PT_SEG%I,
     &                       MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &                       MESH%KNOLG%I,NPOIN)
!
!       COMPLETES FAC ONCE IFABOR AND ELTSEG ARE KNOWN
!
        IF(IELM.EQ.11.AND.IELMX.EQ.13) THEN
          CALL COMP_FAC(MESH%ELTSEG%I,MESH%IFABOR%I,NELEM,
     &                  NPOIN,MESH%FAC)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  DATA STRUCTURE FOR EDGE-BASED STORAGE
!
      IF(IELM.EQ.11.AND.PRODUC.EQ.2) THEN
!
      CALL FROPRO(MESH%NBOR%I,MESH%IKLE%I,
     &            NELEM,NELMAX,NPOIN,MESH%NPMAX,NPTFR,IELM,
     &            MESH%IKLEM1%I,MESH%LIMVOI%I,OPTASS,PRODUC,MXPTVS,
     &            IT1%I,MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MESH%NSEG)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  COMPLEMENTS IKLE BEYOND LINEAR ELEMENTS
!
      IF(IELM.EQ.11.AND.IELM.NE.IELMX) THEN
        IF(MESH%IKLE%DIM2.NE.BIEF_NBPEL(IELMX,MESH)) THEN
          IF(LNG.EQ.1) WRITE(LU,100) IELMX
          IF(LNG.EQ.2) WRITE(LU,101) IELMX
100       FORMAT(1X,'INBIEF (BIEF) : IKLE MAL DIMENSIONNE',/,1X,
     &              'POUR UN ELEMENT DE TYPE :',1I6)
101       FORMAT(1X,'INBIEF (BIEF): WRONG DIMENSION OF IKLE',/,1X,
     &              'FOR AN ELEMENT WITH TYPE :',1I6)
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL COMP_IKLE(MESH%IKLE%I,MESH%IKLBOR%I,
     &                 MESH%ELTSEG%I,MESH%NBOR%I,
     &                 IELMX,NELEM,NELMAX,NPOIN,NPTFR)
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPLEMENTS THE SEGMENT STRUCTURE BEYOND THE LINEAR ELEMENTS
!
      IF(IELM.NE.IELMX) THEN
        CALL COMP_SEG(NELEM,NELMAX,IELMX,MESH%IKLE%I,MESH%GLOSEG%I,
     &                MESH%GLOSEG%MAXDIM1,MESH%ELTSEG%I,MESH%ORISEG%I,
     &                MESH%NSEG)
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPLEMENTS THE DATA STRUCTURE FOR FINITE VOLUMES
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
!
          CALL INFCEL(MESH%X%R,MESH%Y%R,MESH%IKLE%I,
     &                MESH%NUBO%I,MESH%VNOIN%R,
     &                NPOIN,MXPTVS,NELEM,NELMAX,MESH%NSEG,MESH%CMI%R,
     &                MESH%JMI%I,MESH%AIRST%R)
!
!         COMPUTES THE SURFACE OF THE CELLS
!
          CALL VECTOR(T1,'=','MASBAS          ',11,
     &                1.D0,T2,T2,T2,T2,T2,T2,MESH,.FALSE.,T2)
!
!         COMPUTES THE LOCAL SPACE STEP PER CELL
!
          CALL HLOC(NPOIN,MESH%NSEG,MESH%NPTFR,MESH%NUBO%I,
     &              MESH%NBOR%I,MESH%VNOIN%R,
     &              MESH%XNEBOR%R,MESH%YNEBOR%R,T1%R,MESH%DTHAUT%R)
!
!         COMPUTES THE GRADIENTS OF THE BASE FUNCTIONS
!
          CALL GRADP(NPOIN,MESH%NELMAX,MESH%IKLE%I,MESH%SURFAC%R,
     &               MESH%X%R,MESH%Y%R,MESH%DPX%R,MESH%DPY%R)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
