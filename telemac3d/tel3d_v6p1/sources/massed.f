!                    *****************
                     SUBROUTINE MASSED
!                    *****************
!
     &(MASSE,EPAI,CONC,HDEP,TRA02,NPOIN2,  
     & NPFMAX,NCOUCH,NPF,TASSE,GIBSON,RHOS,CFDEP,VOLU2D)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PERFORMS INITIAL RELATIVE MASS BALANCE FOR
!+                THE SEDIMENT.
!
!history  C.LE NORMANT(LNH)
!+        26/08/92
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (LNHE)
!+        17/03/2011
!+        V6P1
!+   Rewritten (formula changed, parallelism,...)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CFDEP          |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| CONC           |-->| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| EPAI           |-->| THICKNESS OF SOLID BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ TOTAL BED THICKNESS)
!| GIBSON         |-->| LOGICAL FOR GIBSON MODEL
!| HDEP           |-->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| MASSE          |-->| MASS OF SUSPENDED SEDIMENT
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NPF            |-->| NUMBER OF POINTS OF THE BOTTOM ON ONE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES THAT
!|                |   | DISCRETISE MUD BED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| RHOS           |-->| DENSITY OF SEDIMENT
!| TASSE          |-->| MULTILAYER SETTLING MODEL LOGICAL
!| TRA02          |<->| WORK ARRAY
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPFMAX,NPOIN2,NCOUCH
      INTEGER, INTENT(IN)             :: NPF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: MASSE
      DOUBLE PRECISION, INTENT(IN)    :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: VOLU2D(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: HDEP(NPOIN2),CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: RHOS,CFDEP
      LOGICAL, INTENT(IN)             :: TASSE,GIBSON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,IPF
      DOUBLE PRECISION MASSE1,MASSE6
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!=======================================================================
!
! MASS OF MUDDY DEPOSITS ON THE RIGID BED (MASSE6)
!
!=======================================================================
!
      IF(TASSE) THEN
        DO IPOIN=1,NPOIN2
          TRA02(IPOIN)=0.D0
          DO IPF=1,NCOUCH
            TRA02(IPOIN)=TRA02(IPOIN)+CONC(IPF)*EPAI(IPF,IPOIN)
          ENDDO
        ENDDO
      ELSEIF(GIBSON) THEN
        DO IPOIN=1,NPOIN2
          TRA02(IPOIN)=CFDEP*HDEP(IPOIN)
          DO IPF=1,NPF(IPOIN)-1
            TRA02(IPOIN)=TRA02(IPOIN)+RHOS*EPAI(IPF,IPOIN)
          ENDDO
        ENDDO
      ELSE
        DO IPOIN=1,NPOIN2
          TRA02(IPOIN)=CFDEP*HDEP(IPOIN)
        ENDDO      
      ENDIF
!
      MASSE6=0.D0
      DO IPOIN=1,NPOIN2
        MASSE6=MASSE6+VOLU2D(IPOIN)*TRA02(IPOIN)
      ENDDO
      IF(NCSIZE.GT.1) MASSE6=P_DSUM(MASSE6)
!
!=======================================================================
!
! TOTAL MASS OF SEDIMENT IN THE DOMAIN (MASSE1)
!
!=======================================================================
!
      MASSE1=MASSE+MASSE6
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'MASSE TOTALE DE SEDIMENTS            : ',MASSE1
        WRITE(LU,*) 'MASSE DES DEPOTS VASEUX              : ',MASSE6
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'TOTAL MASS OF SEDIMENTS              : ',MASSE1
        WRITE(LU,*) 'MASS OF MUD DEPOSIT                  : ',MASSE6
      ENDIF
!
!=======================================================================
!
      RETURN
      END
