!                    **************
                     SUBROUTINE PARTITIONER_PARA
!                    **************
     &    (PMETHOD, NELEM, NPOIN, NDP, NPARTS, IKLES, EPART, NPART)
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    call to the partionning software
!
!history   R. KOPMANN (BAW)
!+
!+
!+         created
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| PMETHOD        |<--| 1: FOR METIS 2: FOR SCOTCH
!| NELEM          |<--| THE NUMBER OF ELEMENTS
!| NDP            |<--| THE NUMBE OF POINT PER ELEMENT
!| NPARTS         |<--| NUMBER OF PARTITIONS
!| IKLES          |<--| CONNECTIVITY TABLE
!| EPART          |-->| PARTITION NUMBER OF AN ELEMENT
!| MYPART         |-->| PARTITION NUMBER OF A POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
#if defined (HAVE_SCOTCH)
      USE ISO_C_BINDING
#endif
      IMPLICIT NONE
!
!     PTSCOTCH'S HEADER (FORTRAN)
#if defined (HAVE_PTSCOTCH)
      INCLUDE "ptscotchf.h"
#endif
!     
      INTEGER, INTENT(IN)  :: PMETHOD
      INTEGER, INTENT(IN)  :: NELEM
      INTEGER, INTENT(IN)  :: NPOIN
      INTEGER, INTENT(IN)  :: NDP
      INTEGER, INTENT(IN)  :: IKLES(NELEM*NDP)
      INTEGER, INTENT(IN)  :: NPARTS
      INTEGER, INTENT(OUT) :: EPART(NELEM)
!TODO: Remove ??
      INTEGER, INTENT(OUT) :: NPART(NELEM)
!
!-----------------------------------------------------------------------
!
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER :: ERR, I, J, K
!
!     FOR PARMETIS
!      
      INTEGER :: NCOMMONNODES
      INTEGER :: EDGECUT
      INTEGER, ALLOCATABLE :: EPTR(:), EIND(:)
      INTEGER, ALLOCATABLE :: NULLTABLE(:)
#if defined (HAVE_PTSCTOCH) .OR. defined(HAVE_PARMETIS)
      INTEGER, ALLOCATABLE :: EPART_LOC(:)
      INTEGER :: NUMFLAG
      INTEGER :: T_P,N_P
      INTEGER :: NCON
      INTEGER :: NELEM_GLOBAL, NELEM_LOC
      INTEGER :: OPTIONS(0:2)
      INTEGER, ALLOCATABLE :: RECVBUF(:)

#endif
#if defined (HAVE_PTSCOTCH)
!
      REAL*8, DIMENSION(SCOTCH_DGRAPHDIM) :: PTSCOTCHGRAPH
      REAL*8, DIMENSION(SCOTCH_STRATDIM) :: PTSCOTCHSTRAT
      TYPE(C_PTR), POINTER :: PTXADJ(:), PTADJNCY(:)
      INTEGER, POINTER :: XADJ2(:), ADJNCY2(:)
      REAL*4, ALLOCATABLE :: TPWGTS(:), UBVEC(:)
      INTEGER :: WGTFLAG
      DOUBLE PRECISION, ALLOCATABLE :: VWGT(:)
!
#endif
        IF(PMETHOD.EQ.1) THEN
#if defined(HAVE_PARMETIS)
          WRITE(LU,*) 'BEGIN PARTITIONING WITH PARMETIS'
          ! Calling parmetis metis parallel partitionner        
          ALLOCATE (ELMDIST(NPARTS+1),STAT=ERR)
          CALL CHECK_ALLOCATE (ERR, 'ELMDIST')
!          
!         THE NUMBER OF COMMON POINT NEEDED BETWEEN 2 ELEMENTS TO MAKE AN
!         EDGE
          IF (NDP==3.OR.NDP==6) THEN 
             NCOMMONNODES = 2 ! FOR TRIANGLE OR RECTANGLE
          ELSE
             WRITE(LU,*) 'PARMETIS: IMPLEMENTED FOR ',
     &                   'TRIANGLES OR PRISMS ONLY'
             CALL PLANTE(-1)
             STOP
          ENDIF 
!         
!         ELMDIST: THIS ARRAY DESCRIBES HOW THE ELEMENTS OF THE MESH ARE DISTRIBUTED AMONG THE PROCESSORS. 
!              IT IS ANALOGOUS TO THE VTXDIST ARRAY. ITS CONTENTS ARE IDENTICAL FOR EVERY PROCESSOR. 
          ELMDIST(1)=1
          DO I=1,NPARTS-1
            ELMDIST(I+1) = ELMDIST(I) + NELEM/NPARTS
          ENDDO
          ELMDIST(NPARTS+1) = NELEM + 1
!          
          NELEM_LOC = ELMDIST(ID+2) - ELMDIST(ID+1)
!         
          ALLOCATE (EPTR(NELEM_LOC+1),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPTR')
          ALLOCATE (EIND(NELEM_LOC*NDP),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EIND')
          ALLOCATE (EPART_LOC(NELEM_LOC),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPART_LOC')
!         
!         EPTR, EIND: THESE ARRAYS SPECIFIES THE ELEMENTS THAT ARE STORED LOCALLY AT EACH PROCESSOR. 
!         
          DO I=1,NELEM_LOC+1
            EPTR(I) = (I-1)*NDP + 1
          ENDDO
!       
          K=1
          DO I=ELMDIST(ID+1),ELMDIST(ID+2)-1
            DO J=1,NDP
              EIND(K) = IKLES((I-1)*NDP+J)
              K = K + 1
            ENDDO   
          ENDDO    
!         
!         REPRESENT THE NUMBER OF PARAMETER FOR BALANCING THE PARTIRION
          NCON = 1
!         
          ALLOCATE (TPWGTS(NCON*NPARTS),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'TPWGTS')
          ALLOCATE (UBVEC(NCON),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'UBVEC')
!         
          TPWGTS(:) = 1.0/FLOAT(NPARTS)
          OPTIONS(:)=0
          UBVEC(:) = 1.05
          WGTFLAG = 0
          NUMFLAG = 1
!         
          CALL PARMETIS_V3_PARTMESHKWAY(ELMDIST, EPTR, EIND, VWGT,
     &                              WGTFLAG, NUMFLAG,
     &                              NCON, NCOMMONNODES, NPARTS, TPWGTS,
     &                              UBVEC, OPTIONS, EDGECUT, EPART_LOC, 
     &                              MPI_COMM_WORLD)
          
          ALLOCATE(RECVBUF(NPARTS),STAT=ERR)
          IF (ERR.NE.0) CALL PARTEL_PARA_ALLOER(LU,'RECVBUF')
          DO I=1,NPARTS
            RECVBUF(I) = ELMDIST(I+1) - ELMDIST(I)
          ENDDO
          ELMDIST = ELMDIST - 1
          CALL P_ALLGATHERV_I(EPART_LOC,NELEM_LOC,MPI_INTEGER,EPART,
     &                      RECVBUF,ELMDIST,MPI_INTEGER,MPI_COMM_WORLD,
     &                      ERR)
!          
          DEALLOCATE(ELMDIST)
          DEALLOCATE(EPART_LOC)
          DEALLOCATE(RECVBUF)
          DEALLOCATE(EIND)
          DEALLOCATE(EPTR)
#endif
        ELSE IF(PMETHOD.EQ.2) THEN
#if defined (HAVE_PTSCOTCH)             
          WRITE(LU,*) 'BEGIN PARTITIONING WITH PTSCOTCH'
!          
          ALLOCATE (ELMDIST(N_P+1),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'ELMDIST')
!         
!!        The number of common point needed between 2 elements to make an
!         edge
          IF (NDP==3.OR.NDP==6) THEN 
             NCOMMONNODES = 2 ! FOR TRIANGLE OR RECTANGLE
          ELSE
             WRITE(LU,*) 'PTSCOTCH: IMPLEMENTED FOR ',
     &                   'TRIANGLES OR PRISMS ONLY'
             CALL PLANTE(-1)
             STOP
          ENDIF 
!         
!    ELM  DIST: THIS ARRAY DESCRIBES HOW THE ELEMENTS OF THE MESH ARE DISTRIBUTED AMONG THE PROCESSORS. 
!!             IT IS ANALOGOUS TO THE VTXDIST ARRAY. ITS CONTENTS ARE IDENTICAL FOR EVERY PROCESSOR. 
!              SAME PRINCIPAL AS FOR XADJ AND ADJCNY
          ELMDIST(1)=1
          DO I=1,N_P-1
             ELMDIST(I+1) = ELMDIST(I) + NELEM_GLOBAL/N_P
          ENDDO
          ELMDIST(N_P+1) = NELEM_GLOBAL + 1
!          
          NELEM_LOC = ELMDIST(T_P+1) - ELMDIST(T_P)
!         
          ALLOCATE (EPTR(NELEM_LOC+1),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPTR')
          ALLOCATE (EIND(NELEM_LOC*NDP),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EIND')
          ALLOCATE (EPART_LOC(NELEM_LOC),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPART_LOC')
!         
!         EPTR, EIND: THESE ARRAYS SPECIFIES THE ELEMENTS THAT ARE STORED LOCALLY AT EACH PROCESSOR. 
!                  (SEE DISCUSSION IN SECTION 4.3).
!         
          DO I=1,NELEM_LOC+1
             EPTR(I) = (I-1)*NDP + 1
          ENDDO
!         
          K=1
          DO I=ELMDIST(T_P),ELMDIST(T_P+1)-1
             DO J=1,NDP
                EIND(K) = IKLES((I-1)*NDP+J)
                K = K + 1
             ENDDO   
          ENDDO    
!         
!!!! REP! RESENT THE NUMBER OF PARAMETER FOR BALANCING THE PARTIRION
          NCON = 1
!         
          ALLOCATE(PTXADJ(1))
          ALLOCATE(PTADJNCY(1))
          CALL PARMETIS_V3_MESH2DUAL(ELMDIST, EPTR, EIND, 
     &                             NUMFLAG, NCOMMONNODES,
     &                             PTXADJ,PTADJNCY,
     &                             MPI_COMM_WORLD)
!         
          DEALLOCATE(EPTR,EIND)
          ALLOCATE(RECVBUF(1))
          RECVBUF(1) = NELEM_LOC+1
          CALL C_F_POINTER(PTXADJ(1),XADJ2,RECVBUF)
!          
          RECVBUF(1) = XADJ2(NELEM_LOC+1)-1
          CALL C_F_POINTER(PTADJNCY(1),ADJNCY2,RECVBUF)
          DEALLOCATE(RECVBUF)
!         
          ! BEGINNING PT-SCOTCH PARTIONNING
!          
          CALL SCOTCHFSTRATINIT(PTSCOTCHSTRAT,ERR)
          IF (ERR.EQ.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: CANNOT INITIALIZE STRAT'
            CALL PLANTE(-1)
          ENDIF   
!         
          CALL SCOTCHFDGRAPHINIT(PTSCOTCHGRAPH,MPI_COMM_WORLD,ERR)
          IF (ERR.EQ.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: CANNOT INITIALIZE GRAPH'
            CALL PLANTE(-1)
          ENDIF
!         
          CALL SCOTCHFDGRAPHBUILD ( PTSCOTCHGRAPH,     ! GRAFDAT
     &                              NUMFLAG,     ! BASEVAL
     &                              NELEM_LOC,  ! VERTLOCNBR
     &                              NELEM_LOC,  ! VERTLOCMAX=VERLOCNBR (NO HOLES IN GLOBAL
                                       ! NUMBERING)
     &                              XADJ2,  ! VERTLOCTAB
     &                              XADJ2(2:NELEM_LOC+1),  ! VENDLOCTAB = NULL
     &                              XADJ2,  ! VELOLOCTAB = NULL
     &                              XADJ2,  ! VLBLLOCTAB = NULL
     &                              XADJ2(NELEM_LOC+1)-1,  ! EDGELOCNBR
     &                              XADJ2(NELEM_LOC+1)-1,  ! EDGELOCSIZ
     &                              ADJNCY2,  ! EDGELOCATAB
     &                              ADJNCY2,  ! EDGEGSTTAB = NULL
     &                              ADJNCY2,  ! EDLOLOCTAB = NULL
     &                              ERR)
          IF (ERR.EQ.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: CANNOT BUILD GRAPH'
            CALL PLANTE(-1)
          ENDIF
!         
          CALL SCOTCHFDGRAPHCHECK(PTSCOTCHGRAPH,ERR)
          IF (ERR.EQ.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: GRAPH NOT CONSISTANT'
            CALL PLANTE(-1)
          ENDIF
!          
          CALL SCOTCHFDGRAPHPART ( PTSCOTCHGRAPH, 
     &                            NPARTS, 
     &                            PTSCOTCHSTRAT,
     &                            EPART_LOC, 
     &                            ERR )
!         
          IF (ERR.EQ.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: CANNOT PARTITION GRAPH'
            CALL PLANTE(-1)
          END IF
!!!! CHA! NGING EPART NUMBERING TO 1-NPART
          DO I=1,NELEM_LOC
             EPART_LOC(I) = EPART_LOC(I) + 1
          ENDDO
!           
          CALL SCOTCHFGRAPHEXIT(PTSCOTCHGRAPH)
!         
          CALL SCOTCHFSTRATEXIT(PTSCOTCHSTRAT)
!          DEALLOCATE(XADJ2)
!          DEALLOCATE(ADJNCY2)
          DEALLOCATE(PTXADJ)
          DEALLOCATE(PTADJNCY)
          DEALLOCATE(ELMDIST)
          DEALLOCATE(EPART_LOC)
          DEALLOCATE(RECVBUF)
#else
          WRITE(LU,*) "TRYING TO PARTITIONNE WITH PTSCOTCH WHEN",
     &                "PTSCOTCH IS NOT INSTALLED" 
          CALL PLANTE(-1)
#endif        
        ELSEIF(PMETHOD.EQ.3) THEN
          WRITE(LU,*) 'BEGIN PARTITIONING WITH METIS'
          ALLOCATE (EPTR(NELEM+1),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPTR')
          ALLOCATE (EIND(NELEM*NDP),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EIND')     
!         
          DO I=1,NELEM+1
             EPTR(I) = (I-1)*NDP + 1
          ENDDO
!         
          K=1
          DO I=1,NELEM
             DO J=1,NDP
                EIND(K) = IKLES((I-1)*NDP+J)
                K = K + 1
             ENDDO   
          ENDDO    
!         
!         SWITCH TO C NUMBERING
          EIND = EIND -1
          EPTR = EPTR -1
!         
!         METIS REQUIRES THE NUMBER OF COMMON POINT NEEDED BETWEEN 2 ELEMENTS TO MAKE AN EDGE
!         NCOMMONNODES = 2   FOR TRIANGLE OR RECTANGLE
!         NCOMMONNODES = 3   FOR TETRAHEDRE
!         NCOMMONNODES = 4   FOR HEXAHEDRE
          
!         
          IF (NDP==3.OR.NDP==6) THEN 
             NCOMMONNODES = 2 ! FOR TRIANGLE OR RECTANGLE
          ELSE
           WRITE(LU,*) 'METIS: IMPLEMENTED FOR TRIANGLES OR PRISMS ONLY'
             CALL PLANTE(-1)
             STOP
          ENDIF 
          
!         WE ONLY USE METIS_PARTMESHDUAL AS ONLY THE FINITE ELEMENTS PARTITION
!         IS RELEVANT HERE.  
!         
#if defined(HAVE_MPI)
            CALL METIS_PARTMESHDUAL 
     &          (NELEM, NPOIN, EPTR, EIND, NULLTABLE,
     &           NULLTABLE, NCOMMONNODES, NPARTS, NULLTABLE, 
     &           NULLTABLE, EDGECUT, EPART, NPART)
#else
        WRITE(LU,*) 'ERROR: TRY TO RUN PARTEL_PARA WITH A '//
     &              'SERIAL CONFIGURATION'
        CALL PLANTE(-1)
#endif
!         
!         
!         DEALLOCATING TEMPORARY ARRAYS FOR METIS
!         SWITCHING EPART TO FORTRAN NUMBERING (1...N)      
          EPART = EPART+1
          DEALLOCATE(EPTR)
          DEALLOCATE(EIND)
!
        ENDIF
      END SUBROUTINE PARTITIONER_PARA

