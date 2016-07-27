      PROGRAM API_DUMMY_EXE_T2D
        USE API_INTERFACE_T2D
        USE DECLARATIONS_PARALLEL
        USE DECLARATIONS_PARTEL, ONLY : CODE
        IMPLICIT NONE
        INTEGER ::  I, K, IERR, ID, IDUM
        INTEGER :: NPOIN,NPTFR,NELEM,NTIME_STEPS,NELMAX
        CHARACTER(LEN=144) :: CAS_FILE, DICO_FILE
        CHARACTER(LEN=250) ::  RES_FILE,GEO_FILE, CLI_FILE, RES_FILE2
        CHARACTER(LEN=250) :: NAMESEC, NAMEZONE
        CHARACTER(LEN=8) :: FFORMAT
        CHARACTER(LEN=T2D_VAR_LEN) :: VARNAME
        INTEGER LU,LNG
        INTEGER IPID,NCSIZE,PMETHOD,VAR_SIZE
        INTEGER SPLIT_COMM,NCSIZE2,ipid2,color
        INTEGER NPLAN
!       PARAMETER FOR TELEMAC2D/MASCARET COUPLING
        ! TYPE FOR MASCARET BOUNDARY CONDITION

        ! OUTPUT FOR WRITING
        LU=6
        ! 1 FOR FRENCH 2 FOR ENGLISH
        LNG=2
        ID = 0
        NPLAN = 0
        CAS_FILE = 't2d_gouttedo.cas'
        DICO_FILE = '/home/B61570/opentelemac/'
     &     //'git/trunk/sources/telemac2d/'
     &     //'telemac2d.dico'
        GEO_FILE='geo_gouttedo.slf'
        CLI_FILE='geo_gouttedo.cli'
        FFORMAT = 'SERAFIN '
        NAMESEC = REPEAT(' ',250)
        NAMEZONE = REPEAT(' ',250)
        ! Partitioning method to use 1: metis
        PMETHOD=1
!
        ! Initialising mpi
        CALL MPI_INIT(IERR)
        ! Getting rank
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,IPID,IERR)
        ! Getting the number of process
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NCSIZE,IERR)
!
        color = ipid/2
        CALL MPI_COMM_SPLIT(MPI_COMM_WORLD,color,IPID,SPLIT_COMM,IERR)
        print*,'mpi_comm_split',ierr
        CALL MPI_COMM_SIZE(SPLIT_COMM,NCSIZE2,IERR)
        print *,ierr
        CALL MPI_COMM_RANK(SPLIT_COMM,IPID2,IERR)
        print *,ierr
        print *,'ipid/ncsize',IPID,'/',ncsize,
     &          'ipid2/ncsize2',ipid2,'/',ncsize2

        ! The partitioning is done sequentially

        IF(IPID.EQ.0) THEN
          ! PARITIONING THE GEOMETRY FILE
          CODE='T2D'
          CALL PARTEL(GEO_FILE,CLI_FILE,NCSIZE2,PMETHOD,
     &                FFORMAT,NAMESEC,NAMEZONE)
        ENDIF
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)

!
        CALL RUN_SET_CONFIG_T2D(ID,666+COLOR,LNG,SPLIT_COMM,IERR)
        PRINT *, 'IERR',IERR
        PRINT *, 'ID',ID

        CALL RUN_READ_CASE_T2D(ID,CAS_FILE,DICO_FILE,IERR)
        PRINT *, 'IERR',IERR

        ! Changing the name of the result file
        VARNAME = 'MODEL.RESULTFILE'
        CALL GET_VAR_SIZE_T2D(ID,VARNAME,VAR_SIZE,IDUM,IDUM,IERR)
        PRINT *, 'RANK:',IPID,'VAR_SIZE:',VAR_SIZE
        RES_FILE='toto_'//CHAR(74+COLOR)//'.srf'
        CALL SET_STRING_T2D(ID,VARNAME,RES_FILE,VAR_SIZE,IERR)
        PRINT *, 'IERR',IERR

        CALL RUN_ALLOCATION_T2D(ID,IERR)
        PRINT *, 'IERR',IERR

        CALL RUN_INIT_T2D(ID,IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NPOIN'
        CALL GET_INTEGER_T2D(ID, VARNAME, NPOIN, 0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NPTFR'
        CALL GET_INTEGER_T2D(ID, VARNAME, NPTFR, 0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NELEM'
        CALL GET_INTEGER_T2D(ID, VARNAME, NELEM, 0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NELMAX'
        CALL GET_INTEGER_T2D(ID, VARNAME, NELMAX, 0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        VARNAME = 'MODEL.NTIMESTEPS'
        CALL GET_INTEGER_T2D(ID, VARNAME, NTIME_STEPS,
     &                       0, 0, 0, IERR)
        PRINT *, 'IERR',IERR

        PRINT *, NPOIN, NPTFR, NELEM, NTIME_STEPS
!
        DO I=1,NTIME_STEPS
          CALL RUN_TIMESTEP_T2D(ID,IERR)
        ENDDO
!
        CALL RUN_FINALIZE_T2D(ID,IERR)
        ! Mergin step
        call MPI_BARRIER(MPI_COMM_WORLD,IERR)
        IF(IPID2.EQ.0) THEN
          print *,'Gretel step for ',trim(res_file)
          CALL GRETEL_AUTOP(GEO_FILE,FFORMAT,RES_FILE,FFORMAT,
     &                      NCSIZE2,NPLAN)
        ENDIF
        CALL MPI_FINALIZE(IERR)

        END PROGRAM
!                    *****************
                     SUBROUTINE VC19AA
!                    *****************
!
     &(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     & XEL,YEL,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,W1,W2,W3,FORMUL)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /          DPSII      DPSII
!+      V  =  XMUL   /   F * ( U -----  + V ----- )   D(OMEGA)
!+       I          /OMEGA        DX         DY
!+
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!+
!+    F, U AND V ARE VECTORS
!+                                                  ->
!+    BEWARE: IF FORMUL='HUGRADP ' THE VELOCITY IS: U
!+                                                  ->      --->
!+            IF FORMUL='HUGRADP2' THE VELOCITY IS: U + G * GRAD(H)
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!warning  U AND V ARE QUASI-BUBBLE; TREATED AS IF LINEAR!!!!!!!!!!!!
!
!history  C-T PHAM (LNHE)
!+        09/01/08
!+        V5P8
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/05/2014
!+        V7P0
!+   Discontinuous elements better treated: new types 15, 16 and 17 for
!+   discontinuous linear, quasi-bubble, and quadratic, rather than
!+   using component DIMDISC=11, 12 or 13.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| FORMUL         |-->| STRING WITH THE FORMULA DESCRIBING THE VECTOR
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| H              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SH             |-->| BIEF_OBJ STRUCTURE OF H
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SURFAC         |-->| AREA OF TRIANGLES
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC19AA => VC19AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*),W3(*)
!
!     STRUCTURES OF H, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: SF,SH,SG,SU,SV
      DOUBLE PRECISION, INTENT(IN)  :: F(*),G(*),H(*),U(*),V(*)
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMU,IELMV,IELMF,IELMH,IELMG,I1,I2,I3
!
      DOUBLE PRECISION X2,Y2,X3,Y3,Z1,Z2,Z3,ZX,ZY,DET
      DOUBLE PRECISION H1,H2,H3,U1,U2,U3,V1,V2,V3
      DOUBLE PRECISION H123,U123,V123,HU123,HV123
      DOUBLE PRECISION XSUR24
!
!-----------------------------------------------------------------------
!
      XSUR24 = XMUL / 24.D0
!
!-----------------------------------------------------------------------
!
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMF=SF%ELM
      IELMG=SG%ELM
      IELMH=SH%ELM
!
!-----------------------------------------------------------------------
!
      IF(FORMUL(1:8).EQ.'HUGRADP ') THEN
!
!     F LINEAR, U AND V ARE LINEAR OR QUASI-BIBBLE
!               BUT THE FOURTH POINT DISCARDED
!
      IF(        IELMF.EQ.11
     &     .AND.(IELMU.EQ.11.OR.IELMU.EQ.12)
     &     .AND.(IELMV.EQ.11.OR.IELMV.EQ.12)  ) THEN
!
        DO IELEM = 1 , NELEM
!
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          H1 = F(I1)
          H2 = F(I2)
          H3 = F(I3)
          U1 = U(I1)
          U2 = U(I2)
          U3 = U(I3)
          V1 = V(I1)
          V2 = V(I2)
          V3 = V(I3)
!
          H123 = H1+H2+H3
          U123 = U1+U2+U3
          V123 = V1+V2+V3
!
          HU123 = H1*U1+H2*U2+H3*U3
          HV123 = H1*V1+H2*V2+H3*V3
!
          W1(IELEM) = ( (Y2-Y3)*(H123*U123+HU123)
     &                 +(X3-X2)*(H123*V123+HV123) )*XSUR24
          W2(IELEM) = (     Y3 *(H123*U123+HU123)
     &                     -X3 *(H123*V123+HV123) )*XSUR24
          W3(IELEM) = (    -Y2 *(H123*U123+HU123)
     &                     +X2 *(H123*V123+HV123) )*XSUR24
!
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
        IF (LNG.EQ.1) WRITE(LU,400) IELMU,SU%NAME
        IF (LNG.EQ.1) WRITE(LU,500) IELMV,SV%NAME
        IF (LNG.EQ.1) WRITE(LU,600)
        IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
        IF (LNG.EQ.2) WRITE(LU,401) IELMU,SU%NAME
        IF (LNG.EQ.2) WRITE(LU,501) IELMV,SV%NAME
        IF (LNG.EQ.2) WRITE(LU,601)
100     FORMAT(1X,'VC19AA (BIEF) :',/,
     &         1X,'DISCRETISATION DE F : ',1I6,
     &         1X,'NOM REEL : ',A6)
200     FORMAT(1X,'DISCRETISATION DE G : ',1I6,
     &         1X,'NOM REEL : ',A6)
300     FORMAT(1X,'DISCRETISATION DE H : ',1I6,
     &         1X,'NOM REEL : ',A6)
400     FORMAT(1X,'DISCRETISATION DE U : ',1I6,
     &         1X,'NOM REEL : ',A6)
500     FORMAT(1X,'DISCRETISATION DE V : ',1I6,
     &         1X,'NOM REEL : ',A6)
600     FORMAT(1X,'CAS NON PREVU')
101     FORMAT(1X,'VC19AA (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &         1X,'REAL NAME: ',A6)
301     FORMAT(1X,'DISCRETIZATION OF H:',1I6,
     &         1X,'REAL NAME: ',A6)
401     FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
501     FORMAT(1X,'DISCRETIZATION OF V:',1I6,
     &         1X,'REAL NAME: ',A6)
601     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(1:8).EQ.'HUGRADP2') THEN
!
!-----------------------------------------------------------------------
!
!
!     F AND G ARE LINEAR, U, V ARE LINEAR OR QUASI-BUBBLE
!     H IS PIECEWISE LINEAR
!
      IF(       IELMF.EQ.11
     &     .AND.IELMG.EQ.11
     &     .AND.IELMH.EQ.15
     &     .AND.(IELMU.EQ.11.OR.IELMU.EQ.12)
     &     .AND.(IELMV.EQ.11.OR.IELMV.EQ.12)  ) THEN
!
        DO IELEM = 1 , NELEM
!
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          H1 = F(I1)
          H2 = F(I2)
          H3 = F(I3)
          DET=X2*Y3-X3*Y2
          Z1=H(IELEM)
          Z2=H(IELEM+  NELMAX)-Z1
          Z3=H(IELEM+2*NELMAX)-Z1
          ZX=(Z2*Y3-Z3*Y2)/DET
          ZY=(X2*Z3-X3*Z2)/DET
          U1 = U(I1) + G(I1)*ZX
          U2 = U(I2) + G(I2)*ZX
          U3 = U(I3) + G(I3)*ZX
          V1 = V(I1) + G(I1)*ZY
          V2 = V(I2) + G(I2)*ZY
          V3 = V(I3) + G(I3)*ZY
!
          H123 = H1+H2+H3
          U123 = U1+U2+U3
          V123 = V1+V2+V3
!
          HU123 = H1*U1+H2*U2+H3*U3
          HV123 = H1*V1+H2*V2+H3*V3
!
          W1(IELEM) = ( (Y2-Y3)*(H123*U123+HU123)
     &                 +(X3-X2)*(H123*V123+HV123) )*XSUR24
          W2(IELEM) = (     Y3 *(H123*U123+HU123)
     &                     -X3 *(H123*V123+HV123) )*XSUR24
          W3(IELEM) = (    -Y2 *(H123*U123+HU123)
     &                     +X2 *(H123*V123+HV123) )*XSUR24
!
        ENDDO
!
!     F, G AND H ARE LINEAR, U AND V ARE LINEAR OR QUASI-BUBBLE
!
      ELSEIF(       IELMF.EQ.11
     &         .AND.IELMG.EQ.11
     &         .AND.IELMH.EQ.11
     &         .AND.(IELMU.EQ.11.OR.IELMU.EQ.12)
     &         .AND.(IELMV.EQ.11.OR.IELMV.EQ.12)  ) THEN
!
        DO IELEM = 1 , NELEM
!
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          H1 = F(I1)
          H2 = F(I2)
          H3 = F(I3)
          DET=X2*Y3-X3*Y2
          Z1=H(I1)
          Z2=H(I2)-Z1
          Z3=H(I3)-Z1
          ZX=(Z2*Y3-Z3*Y2)/DET
          ZY=(X2*Z3-X3*Z2)/DET
          U1 = U(I1) + G(I1)*ZX
          U2 = U(I2) + G(I2)*ZX
          U3 = U(I3) + G(I3)*ZX
          V1 = V(I1) + G(I1)*ZY
          V2 = V(I2) + G(I2)*ZY
          V3 = V(I3) + G(I3)*ZY
!
          H123 = H1+H2+H3
          U123 = U1+U2+U3
          V123 = V1+V2+V3
!
          HU123 = H1*U1+H2*U2+H3*U3
          HV123 = H1*V1+H2*V2+H3*V3
!
          W1(IELEM) = ( (Y2-Y3)*(H123*U123+HU123)
     &                 +(X3-X2)*(H123*V123+HV123) )*XSUR24
          W2(IELEM) = (     Y3 *(H123*U123+HU123)
     &                     -X3 *(H123*V123+HV123) )*XSUR24
          W3(IELEM) = (    -Y2 *(H123*U123+HU123)
     &                     +X2 *(H123*V123+HV123) )*XSUR24
!
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
        IF (LNG.EQ.1) WRITE(LU,200) IELMG,SG%NAME
        IF (LNG.EQ.1) WRITE(LU,300) IELMH,SH%NAME
        IF (LNG.EQ.1) WRITE(LU,400) IELMU,SU%NAME
        IF (LNG.EQ.1) WRITE(LU,500) IELMV,SV%NAME
        IF (LNG.EQ.1) WRITE(LU,600)
        IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
        IF (LNG.EQ.2) WRITE(LU,201) IELMG,SG%NAME
        IF (LNG.EQ.2) WRITE(LU,301) IELMH,SH%NAME
        IF (LNG.EQ.2) WRITE(LU,401) IELMU,SU%NAME
        IF (LNG.EQ.2) WRITE(LU,401) IELMV,SV%NAME
        IF (LNG.EQ.2) WRITE(LU,601)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(1:8).EQ.'HUGRADP3') THEN
!
!-----------------------------------------------------------------------
!
!
!     F AND G ARE LINEAR, H IS PIECEWISE LINEAR
!
      IF(IELMF.EQ.11.AND.IELMG.EQ.11.AND.IELMH.EQ.15) THEN
!
        DO IELEM = 1 , NELEM
!
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          H1 = F(I1)
          H2 = F(I2)
          H3 = F(I3)
          DET=X2*Y3-X3*Y2
          Z1=H(IELEM)
          Z2=H(IELEM+  NELMAX)-Z1
          Z3=H(IELEM+2*NELMAX)-Z1
          ZX=(Z2*Y3-Z3*Y2)/DET
          ZY=(X2*Z3-X3*Z2)/DET
          U1 = G(I1)*ZX
          U2 = G(I2)*ZX
          U3 = G(I3)*ZX
          V1 = G(I1)*ZY
          V2 = G(I2)*ZY
          V3 = G(I3)*ZY
!
          H123 = H1+H2+H3
          U123 = U1+U2+U3
          V123 = V1+V2+V3
!
          HU123 = H1*U1+H2*U2+H3*U3
          HV123 = H1*V1+H2*V2+H3*V3
!
          W1(IELEM) = ( (Y2-Y3)*(H123*U123+HU123)
     &                 +(X3-X2)*(H123*V123+HV123) )*XSUR24
          W2(IELEM) = (     Y3 *(H123*U123+HU123)
     &                     -X3 *(H123*V123+HV123) )*XSUR24
          W3(IELEM) = (    -Y2 *(H123*U123+HU123)
     &                     +X2 *(H123*V123+HV123) )*XSUR24
!
        ENDDO
!
      ELSEIF(       IELMF.EQ.11
     &         .AND.IELMG.EQ.11
     &         .AND.IELMH.EQ.11  ) THEN
!
!       F, G AND H ARE LINEAR
!
        DO IELEM = 1 , NELEM
!
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          H1 = F(I1)
          H2 = F(I2)
          H3 = F(I3)
          DET=X2*Y3-X3*Y2
          Z1=H(I1)
          Z2=H(I2)-Z1
          Z3=H(I3)-Z1
          ZX=(Z2*Y3-Z3*Y2)/DET
          ZY=(X2*Z3-X3*Z2)/DET
          U1 = G(I1)*ZX
          U2 = G(I2)*ZX
          U3 = G(I3)*ZX
          V1 = G(I1)*ZY
          V2 = G(I2)*ZY
          V3 = G(I3)*ZY
!
          H123 = H1+H2+H3
          U123 = U1+U2+U3
          V123 = V1+V2+V3
!
          HU123 = H1*U1+H2*U2+H3*U3
          HV123 = H1*V1+H2*V2+H3*V3
!
          W1(IELEM) = ( (Y2-Y3)*(H123*U123+HU123)
     &                 +(X3-X2)*(H123*V123+HV123) )*XSUR24
          W2(IELEM) = (     Y3 *(H123*U123+HU123)
     &                     -X3 *(H123*V123+HV123) )*XSUR24
          W3(IELEM) = (    -Y2 *(H123*U123+HU123)
     &                     +X2 *(H123*V123+HV123) )*XSUR24
!
        ENDDO
!
      ELSE
        IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
        IF (LNG.EQ.1) WRITE(LU,200) IELMG,SG%NAME
        IF (LNG.EQ.1) WRITE(LU,300) IELMH,SH%NAME
        IF (LNG.EQ.1) WRITE(LU,600)
        IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
        IF (LNG.EQ.2) WRITE(LU,201) IELMG,SG%NAME
        IF (LNG.EQ.2) WRITE(LU,301) IELMH,SH%NAME
        IF (LNG.EQ.2) WRITE(LU,601)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
        IF (LNG.EQ.2) WRITE(LU,2000) FORMUL
1000    FORMAT(1X,'VC19AA (BIEF) :',/,
     &         1X,'FORMULE : ',A16,' NON PREVUE')
2000    FORMAT(1X,'VC19AA (BIEF):',/,
     &         1X,'FORMULA: ',A16,' UNEXPECTED')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC-2D VERSION 5.9         19/08/98  J-M HERVOUET TEL: 30 87 80 18
!
!***********************************************************************
!
!     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |                | -- |
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,ITRAC
!
      DOUBLE PRECISION EIKON
!
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TEMPS
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VITESSES : VITESSES NULLES
!
      CALL OS( 'X=0     ' , X=U )
      CALL OS( 'X=0     ' , X=V )
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DE H , LA HAUTEUR D'EAU
!
      IF(CDTINI(1:10).EQ.'COTE NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES') THEN
      DO IPOIN=1,NPOIN
        EIKON=( (X(IPOIN)-10.05D0)**2 + (Y(IPOIN)-10.05D0)**2 ) / 4.D0
        H%R(IPOIN) = 2.4D0 * ( 1.D0 + EXP(-EIKON) )
      ENDDO
      ELSE
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TRACEUR 1
!
      IF(NTRAC.GT.0) THEN
        CALL OS( 'X=0     ' , X=T%ADR(1)%P )
        DO IPOIN=1,NPOIN
          IF((X(IPOIN)-10.05D0)**2+(Y(IPOIN)-10.05D0)**2.LT.4.D0**2)
     &    THEN
            T%ADR(1)%P%R(IPOIN) = 1.D0
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISATION DE LA VISCOSITE
!
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       ***************
                        SUBROUTINE HREF
!                       ***************
!
!***********************************************************************
! PROGICIEL : BIEF 5.0         01/03/90    J-M HERVOUET
!***********************************************************************
!
!  FONCTION  : CALCUL DE LA HAUTEUR DE REFERENCE POUR LES EQUATIONS
!              DE BOUSSINESQ
!
!              PAR DEFAUT ON PREND LA HAUTEUR INITIALE
!
!              CE SOUS-PROGRAMME PEUT ETRE MODIFIE
!
!              ON PEUT METTRE PAR EXEMPLE LA HAUTEUR DE LINEARISATION
!
!              SI ON VEUT RETOMBER SUR SAINT-VENANT, ON PEUT METTRE
!              H0 = 0
!
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      H0        |<-- | HAUTEUR DE REFERENCE
! |      H         | -->| HAUTEUR INITIALE
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      ZF        | -->| FOND A MODIFIER.
! |      HAULIN    | -->| PROFONDEUR DE LINEARISATION
! |      COTINI    | -->| COTE INITIALE
! |      MESH      | -->| MAILLAGE
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!     CALL OS( 'X=Y     ' , H0 , H , H , C )
!
      CALL OS( 'X=C     ' , H0 , H , H , 2.4D0 )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
