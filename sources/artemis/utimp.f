!                    ****************
                     SUBROUTINE UTIMP
!                    ****************
!
     &(PHIR,PHII,C,CG,K,X,Y,ZF,H,
     & HHO,U0,V0,PHAS,S,TRA01,TRA02,TRA03,TRA04,INCI,
     & GRAV,PER,OMEGA,IKLE,NBOR,KP1BOR,
     & NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN,PRIVE)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ALMOST ALL THE COMPUTATION VARIABLES ARE AVAILABLE
!+             HERE TO WRITE OUT SPECIFIC OUTPUT, COMPUTE ANALYTICAL
!+             SOLUTIONS...
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!code
!+     EXAMPLE : U1 AND V1
!+              (HORIZONTAL VELOCITIES AT T/4)
!+               ARE TRANSFERRED TO PRIVE(I,1) AND PRIVE(I,2)
!+
!+      CALL VECTOR(TRA02, '=' , 'GRADF          X' , IELM ,
!+     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!+     *            MESH , MESH , MSK , MASKEL )
!+
!+      CALL VECTOR(TRA03 , '=' , 'GRADF          Y' , IELM ,
!+     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!+     *            MESH , MESH , MSK , MASKEL )
!+     *            MESH , XMESH ,
!+
!+      CALL VECTOR(TRA01 , '=' , 'MASBAS          ' , IELM ,
!+     *            1.D0 , BID , BID , BID , BID , BID , BID ,
!+     *            MESH , MESH , MSK , MASKEL )
!+     *            MESH , XMESH ,
!+
!+      CALL OS( 'X=Y/Z   ' , TRA02 , TRA02 , TRA01 , BID )
!+      CALL OS( 'X=Y/Z   ' , TRA03 , TRA03 , TRA01 , BID )
!+
!+      DO I = 1,NPOIN
!+         PRIVE%ADR(1)%P%R(1) = TRA02(I)
!+         PRIVE%ADR(1)%P%R(2) = TRA03(I)
!+      ENDDO
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        04/06/1999
!+        V5P1
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
!| C,CG           |-->| PHASE AND GROUP VELOCITIES
!| GRAV           |-->| GRAVITY
!| H              |-->| WATER HEIGHT
!| HHO            |-->| WAVE HEIGHT
!| IELM           |-->| ELEMENT TYPE
!| IELMB          |-->| BOUNDARY ELEMENT TYPE
!| IKLE           |-->| CONNECTIVITY TABLE
!| INCI           |-->| WAVE INCIDENCE
!| K              |-->| WAVE NUMBER
!| KP1BOR         |-->| BOUNDARY NUMBER OF THE NEXT POINT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OMEGA          |-->| WAVE PULSATION
!| PER            |-->| WAVE PERIOD
!| PHAS           |-->| WAVE PHASE
!| PHIR,PHII      |-->| POTENTIAL (REAL & IMAGINARY)
!| PRIVE          |<->| USER PRIVATE TABLE
!| S              |-->| FREE SURFACE ELEVATION
!| TRA02          |<->| WORK ARRAY
!| TRA03          |<->| WORK ARRAY
!| TRA04          |<->| WORK ARRAY
!| U0,V0          |-->| VELOCITIES AT FREE SURFACE (AT T=0)
!| X,Y            |-->| MESH COORDINATES
!| ZF             |-->| BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_UTIMP=> UTIMP
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN
      INTEGER IKLE(NELMAX,3),NBOR(NPTFR),KP1BOR(NPTFR)
!
      DOUBLE PRECISION PHIR(NPOIN),PHII(NPOIN)
      DOUBLE PRECISION C(NPOIN),CG(NPOIN),K(NPOIN),X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION ZF(NPOIN),H(NPOIN),HHO(NPOIN),U0(NPOIN),V0(NPOIN)
      DOUBLE PRECISION INCI(NPOIN)
      DOUBLE PRECISION PHAS(NPOIN),S(NPOIN)
      DOUBLE PRECISION TRA01(NPOIN),TRA02(NPOIN)
      DOUBLE PRECISION TRA03(NPOIN),TRA04(NPOIN)
!
      TYPE(BIEF_OBJ) :: PRIVE
!
      DOUBLE PRECISION GRAV,PER,OMEGA
!
!------------------------------------------------------------------
!     EXAMPLE : U1 AND V1
!              (HORIZONTAL VELOCITIES AT T/4)
!               ARE TRANSFERRED TO PRIVE(I,1) AND PRIVE(I,2)
!------------------------------------------------------------------
!
!      CALL VECTOR(TRA02, '=' , 'GRADF          X' , IELM ,
!     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!     *            MESH , MESH , MSK , MASKEL )
!
!      CALL VECTOR(TRA03 , '=' , 'GRADF          Y' , IELM ,
!     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!     *            MESH , MESH , MSK , MASKEL )
!     *            MESH , XMESH ,
!
!      CALL VECTOR(TRA01 , '=' , 'MASBAS          ' , IELM ,
!     *            1.D0 , BID , BID , BID , BID , BID , BID ,
!     *            MESH , MESH , MSK , MASKEL )
!     *            MESH , XMESH ,
!
!      CALL OS( 'X=Y/Z   ' , TRA02 , TRA02 , TRA01 , BID )
!      CALL OS( 'X=Y/Z   ' , TRA03 , TRA03 , TRA01 , BID )
!
!      DO I = 1,NPOIN
!        PRIVE%ADR(1)%P%R(1) = OMEGAM(I)
!        PRIVE%ADR(1)%P%R(2) = 2D0*3.1415D0/PERPIC
!      ENDDO
!
      RETURN
      END
