C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       - READS SECTIONS INPUT FILE IN SCALAR AND PARALLEL MODES
!><br>            - DEFINES THE CONTROL SECTIONS, OR...
!><br>            - ...RE-DEFINES THE ONES DECLARED PREVIOUSLY IN THE STEERING FILE
!><br>            - SECTIONS ARE DEFINED BY GLOBAL NODE NUMBERS OR,
!>                BY END POINT COORDINATES (THEN NEAREST NODE FOUND)
!><br>            - IN PARALLEL MODE, TWO OPTIONS:
!><br>                 -> TAKES THE "SCALAR" FILE (AS "PREVIOUSLY")
!><br>                 -> TAKES A PARTITIONED FILE - COMPUTING FLUXES THROUGH SECTIONS
!>                     - CROSSING NUMEROUS MESH PARTITIONS IS POSSIBLE
!><br>            - MODIFIES CTRLSC AND NCP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::CHAIN CHAIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CTRLSC CTRLSC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NCP NCP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2DSEC T2DSEC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2D_FILES T2D_FILES@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DISTB, DISTE, DMINB, DMINE, ERR, I, IHOWSEC, INP, N, NSEC, XA, YA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>POINT_TELEMAC2D()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 15/02/2010
!> </td><td> JAJ PINXIT, JACEK.JANKOWSKI@BAW.DE
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                  SUBROUTINE READ_SECTIONS_TELEMAC2D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, ONLY: NCSIZE
      USE DECLARATIONS_TELEMAC2D, ONLY: MESH, CHAIN, NCP, CTRLSC,
     &                                  T2D_FILES, T2DSEC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER :: NSEC, IHOWSEC, I, N, ERR, INP
      DOUBLE PRECISION :: XA, YA, DISTB, DISTE, DMINB, DMINE
!
!-----------------------------------------------------------------------
!
C      WRITE(LU,*) '-> ENTERING READ_SECTIONS_TELEMAC2D'
      INP=T2D_FILES(T2DSEC)%LU
      READ (INP,*) ! THE MANDATORY COMMENT LINE
      READ (INP,*) NSEC, IHOWSEC
      IF (.NOT.ALLOCATED(CHAIN)) THEN
        ALLOCATE (CHAIN(NSEC), STAT=ERR)
        IF (ERR/=0) THEN
          WRITE(LU,*)
     &      'READ_SECTIONS: ERROR BY REALLOCATING CHAIN:',ERR
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF

      SELECT CASE (IHOWSEC)
      CASE (:-1) ! SECTION TERMINAL POINTS PROVIDED AS GLOBAL NODES
        DO N=1,NSEC
          READ (INP,*) CHAIN(N)%DESCR
          READ (INP,*) CHAIN(N)%NPAIR(:)
          IF (NCSIZE>1) THEN
            CHAIN(N)%XYBEG(:)=0.0D0
            CHAIN(N)%XYEND(:)=0.0D0
          ELSE
            CHAIN(N)%XYBEG(:)= (/MESH%X%R(CHAIN(N)%NPAIR(1)),
     &                           MESH%Y%R(CHAIN(N)%NPAIR(1))/)
            CHAIN(N)%XYEND(:)= (/MESH%X%R(CHAIN(N)%NPAIR(2)),
     &                           MESH%Y%R(CHAIN(N)%NPAIR(2))/)
          ENDIF
          CHAIN(N)%NSEG=-1
          NULLIFY(CHAIN(N)%LISTE)
        END DO
C        WRITE(LU,'(A)') ' -> SECTION, TERMINAL COORDINATES:'
C        DO N=1,NSEC
C          WRITE(LU,'(I9,4(1X,1PG13.6))') N,
!     &          CHAIN(N)%XYBEG, CHAIN(N)%XYEND
C        END DO
      CASE (0) ! SECTION TERMINAL POINTS PROVIDED BY COORDINATES
        DO N=1,NSEC
          READ (INP,*) CHAIN(N)%DESCR
          READ (INP,*) CHAIN(N)%XYBEG(:), CHAIN(N)%XYEND(:)
          CHAIN(N)%NPAIR(:)=0
          CHAIN(N)%NSEG=-1
          NULLIFY(CHAIN(N)%LISTE)
        END DO
        DO N=1,NSEC         ! FINDS NEAREST NODES
          XA=MESH%X%R(1)
          YA=MESH%Y%R(1)
          DMINB = SQRT( (CHAIN(N)%XYBEG(1)-XA)**2
     &                + (CHAIN(N)%XYBEG(2)-YA)**2 )
          DMINE = SQRT( (CHAIN(N)%XYEND(1)-XA)**2
     &                + (CHAIN(N)%XYEND(2)-YA)**2 )
          CHAIN(N)%NPAIR(1)=1
          CHAIN(N)%NPAIR(2)=1
          DO I=2,MESH%NPOIN ! COMPUTATIONALLY INTENSIVE
            XA=MESH%X%R(I)
            YA=MESH%Y%R(I)
            DISTB = SQRT( (CHAIN(N)%XYBEG(1)-XA)**2
     &                  + (CHAIN(N)%XYBEG(2)-YA)**2 )
            DISTE = SQRT( (CHAIN(N)%XYEND(1)-XA)**2
     &                 + (CHAIN(N)%XYEND(2)-YA)**2 )
            IF ( DISTB < DMINB ) THEN
              CHAIN(N)%NPAIR(1)=I
              DMINB=DISTB
            ENDIF
            IF ( DISTE < DMINE ) THEN
              CHAIN(N)%NPAIR(2)=I
              DMINE=DISTE
            ENDIF
          END DO
C          WRITE(LU,'(A,3(1X,I9))')
!     &          ' -> SECTION, TERMINAL NODES: ', N, CHAIN(N)%NPAIR(:)
        END DO
      CASE (1:) ! PARTITIONED, INSTEAD OF TERMINAL POINTS, PROVIDED AS CHAINS
        DO N=1,NSEC
          READ (INP,*) CHAIN(N)%DESCR
          READ (INP,*) CHAIN(N)%NSEG
          IF (CHAIN(N)%NSEG>0) THEN
            ALLOCATE (CHAIN(N)%LISTE(CHAIN(N)%NSEG,2), STAT=ERR)
            IF (ERR/=0) THEN
              WRITE(LU,*) 'READ_SECTIONS_TELEMAC2D: ',
     &         ' ERROR BY REALLOCATING CHAIN(N)%LISTE, N, ERR:',N,ERR
              CALL PLANTE(1)
              STOP
            ENDIF
            DO I=1,CHAIN(N)%NSEG
              READ(INP,*) CHAIN(N)%LISTE(I,:)
              CHAIN(N)%NPAIR=-1 ! HM...
              CHAIN(N)%XYBEG=0.0D0
              CHAIN(N)%XYEND=0.0D0
            END DO
          ELSE
            NULLIFY(CHAIN(N)%LISTE)
          ENDIF
        END DO
      END SELECT
!
!-----------------------------------------------------------------------
!
C      WRITE(LU,*) 'SECTIONS SUMMARY:'
C      WRITE(LU,*) 'NSEC,IHOWSEC: ',NSEC,IHOWSEC
C      SELECT CASE (IHOWSEC)
C      CASE(:0) ! SERIAL CASE, OR "CLASSICAL CASE" IN PARALLEL (DEVEL)
C        DO N=1,NSEC
C          WRITE(LU,*) CHAIN(N)%DESCR
C          WRITE(LU,*) CHAIN(N)%XYBEG(:), CHAIN(N)%XYEND(:)
C          WRITE(LU,*) CHAIN(N)%NPAIR(:)
C        END DO
C      CASE (1:) ! PARTITIONED, READY SEGMENT CHAINS GIVEN
C        DO N=1,NSEC
C          WRITE(LU,*) 'NAME: ', CHAIN(N)%DESCR
C          WRITE(LU,*) 'NSEG: ', CHAIN(N)%NSEG
C          DO I=1,CHAIN(N)%NSEG
C            WRITE(LU,*) CHAIN(N)%LISTE(I,:)
C          END DO
C        END DO
C      END SELECT
!
!-----------------------------------------------------------------------
C TRANSFER TO THE GLOBAL TELEMAC OR SISYPHE VARIABLES
C NCP IS 2 * NUMBER OF SECTIONS
C CTRLSC IS THE LIST OF THE SECTION TERMINAL NODES
C CTRLSC HAS TO BE RE-ALLOCATED CAREFULLY
!
C      WRITE (LU,*) 'ARRANGING SECTIONS FOR TELEMAC'
C      WRITE (LU,*) 'TELEMAC NCP WAS: ',NCP
      NCP = 2*NSEC
      IF (ALLOCATED(CTRLSC)) THEN
        DEALLOCATE(CTRLSC, STAT=ERR)
        IF (ERR/=0) THEN
          WRITE(LU,*)
     &    'READ_SECTIONS_TELEMAC2D: ERROR BY DEALLOCATING CTRLSC:',ERR
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
      ALLOCATE (CTRLSC(NCP), STAT=ERR)
      IF (ERR/=0) THEN
        WRITE(LU,*)
     &  'READ_SECTIONS_TELEMAC2D: ERROR BY REALLOCATING CTRLSC:',ERR
        CALL PLANTE(1)
        STOP
      ENDIF
      I=1
      DO N=1,NSEC
        CTRLSC(I)   = CHAIN(N)%NPAIR(1)
        CTRLSC(I+1) = CHAIN(N)%NPAIR(2)
        I=I+2
      END DO
C      WRITE (LU,*) 'NCP@TELEMAC: ',NCP
C      WRITE (LU,*) 'CTRLSC@TELEMAC: ',CTRLSC
!
!-----------------------------------------------------------------------
!
C      WRITE(LU,*) '-> LEAVING READ_SECTIONS_TELEMAC2D'
      RETURN
      END SUBROUTINE READ_SECTIONS_TELEMAC2D
!-----------------------------------------------------------------------
C
C#######################################################################
C