!                    *****************
                     SUBROUTINE GESTDP
!                    *****************
!
     &        ( IVIDE  , EPAI   , HDEP    ,
     &          NPOIN2 , NPFMAX , NPF     ,
     &          EPAI0  , CFDEP  , RHOS    )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MANAGES THE DEPOSITED QUANTITY.
!
!history  C LE NORMANT
!+
!+
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  N. DURAND (CHC-NRC)
!+        18/07/06
!+        V5P7
!+   MODIFIED
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
!| CFDEP          |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| EPAI           |<->| THICKNESS OF SOLID BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ TOTAL BED THICKNESS)
!| EPAI0          |<->| REFERENCE THICKNESS CREATE NEW ELEMENTS
!| HDEP           |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| IVIDE          |<->| VOID INDEX OF BOTTOM POINTS
!| NPF            |-->| NUMBER OF POINTS OF THE BOTTOM ON ONE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES THAT
!|                |   | DISCRETISE MUD BED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| RHOS           |-->| SEDIMENT DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN2 , NPFMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI0
      DOUBLE PRECISION, INTENT(IN)    :: CFDEP , RHOS
!
      INTEGER, INTENT(INOUT) :: NPF(NPOIN2)
!
      DOUBLE PRECISION EDEPOT
      INTEGER IPOIN , NCOUCH , NAJOUT
      INTEGER MANQUE , IPF , ICOUCH
!#####> NOE-CHANGES
      DOUBLE PRECISION EPAIX
!#####< NOE-CHANGES
      INTRINSIC MAX,INT
!
!=======================================================================
!
!     -----VOIDS INDEX FOR FRESH DEPOSITS-----
!
      EDEPOT=RHOS/CFDEP-1.D0
!
!     -----LOOP ON THE BOTTOM POINTS-----
!
        DO IPOIN=1,NPOIN2
!
!     -----IF DEPOSIT OCCURS ON NOT ERODABLE BOTTOM----
          IF(NPF(IPOIN).EQ.0) THEN
            IVIDE(1,IPOIN)=EDEPOT
            NPF(IPOIN)=1
          ENDIF
!
!     -----NUMBER OF THE LAST LAYER OF THE MUDDY BED-----
          NCOUCH=MAX(NPF(IPOIN)-1,0)
!
!     -----COMPUTES THE NUMBER OF LAYERS TO CREATE-----
!
!#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! EPAI0 SHOULD NOT BE CHANGED - PARTICULARLY, IT CANNOT VARY WITH IPOIN
!          EPAI0=EPAI0*(INT(HDEP(IPOIN)/((NPFMAX-1)*EPAI0))+1)
!
! IF YOU NEED TO HAVE THICKER LAYERS BECAUSE NPFMAX IS NOT LARGE ENOUGH
!  TO SPLIT HDEP, YOU OUGHT TO INTRODUCE EPAIX
          EPAIX=EPAI0*(INT(HDEP(IPOIN)/((NPFMAX-1)*EPAI0))+1)
!
! SEE ADDITIONAL CHANGES BELOW WHERE EPAI0 HAS BEEN REPLACED WITH EPAIX
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< NOE-CHANGES
          NAJOUT=INT(HDEP(IPOIN)/EPAIX)           !>NOE: EPAIX <= EPAI0
          MANQUE=NPF(IPOIN)-NPFMAX+NAJOUT         !>NOE: EPAIX <= EPAI0
          HDEP(IPOIN)=HDEP(IPOIN)-EPAIX*NAJOUT    !>NOE: EPAIX <= EPAI0
!
!      DEPENDING ON THE QUANTITY OF MUD DEPOSITS
          IF (MANQUE.LE.0) THEN
!      SIMPLY ADDS NEW LAYERS TO THE MUDDY BED
!
             DO ICOUCH=1+NCOUCH,NAJOUT+NCOUCH
!              --THEIR THICKNESS IS:              !>NOE: EPAIX <= EPAI0
                 EPAI(ICOUCH,IPOIN)=EPAIX/(1.+EDEPOT)
!              --THE VOIDS INDEX OF THE 'UPPER BOUNDARY' POINT IS:
                 IVIDE(ICOUCH+1,IPOIN)=EDEPOT
             END DO
!            ----THE NUMBER OF POINTS DISCRETISING THE BED IS:
                 NPF(IPOIN)=NCOUCH+1+NAJOUT
          ELSE
!      OR MODIFIES THE DISCRETISATION OF THE MUDDY BED BEFOREHAND
!
             DO IPF=2,MANQUE+1
!#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! EPAI BECOMES ARTIFICIALLY LARGE BY DOING SO
! HENCE EPAI IS NO LONGER REPRESENTATIVE OF THE 2 LAYERS
!               IVIDE(IPF,IPOIN)=IVIDE(2*IPF-1,IPOIN)
!               EPAI(IPF-1,IPOIN)=
!     &                    EPAI(2*(IPF-1)-1,IPOIN)+EPAI(2*(IPF-1),IPOIN)
               EPAI(IPF-1,IPOIN)=
     &        ( EPAI(2*(IPF-1)-1,IPOIN)*
     &           (2.+IVIDE(2*(IPF-1)-1,IPOIN)+IVIDE(2*(IPF-1),IPOIN))/2.
     &        + EPAI(2*(IPF-1),IPOIN)*
     &        (2.+IVIDE(2*(IPF-1),IPOIN)+IVIDE(2*(IPF-1)+1,IPOIN))/2.)/
     &       (1.+(IVIDE(2*(IPF-1)-1,IPOIN)+IVIDE(2*(IPF-1)+1,IPOIN))/2.)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< NOE-CHANGES
             ENDDO
             DO IPF=MANQUE+2,NPF(IPOIN)-MANQUE
                 IVIDE(IPF,IPOIN)=IVIDE(IPF+MANQUE,IPOIN)
                 EPAI(IPF-1,IPOIN)=EPAI(IPF-1+MANQUE,IPOIN)
             ENDDO
             DO IPF=NPF(IPOIN)-MANQUE+1,NPF(IPOIN)+NAJOUT-MANQUE
                 IVIDE(IPF,IPOIN)=EDEPOT          !>NOE: EPAIX
                 EPAI(IPF-1,IPOIN)=EPAIX/(1.+EDEPOT)
             ENDDO
!
!            ----NUMBER OF POINTS DISCRETISING THE BED:
                 NPF(IPOIN)=NPF(IPOIN)+NAJOUT-MANQUE
          ENDIF
!
!     IF THE DEPOSITED QUANTITY IS NOT SUFFICIENT TO
!     CREATE A NEW LAYER, STORES IT IN A BUFFER LAYER
!     AND WAITS THE NEXT ITERATION!
!
       ENDDO
!
      RETURN
      END
