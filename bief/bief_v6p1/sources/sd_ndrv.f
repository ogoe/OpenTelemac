!                    ******************
                     SUBROUTINE SD_NDRV
!                    ******************
!
     &(N,R,C,IC,IA,JA,A,B,Z,NSP,ISP,RSP,ESP,PATH,FLAG)
!
!***********************************************************************
! BIEF   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    DRIVER FOR SUBROUTINES TO SOLVE SPARSE NONSYMMETRICAL
!+                SYSTEMS OF LINEAR EQUATIONS (UNCOMPRESSED POINTER STORAGE).
!code
!+    PARAMETERS
!+    CLASS ABBREVIATIONS ARE --
!+       N - INTEGER VARIABLE
!+       F - REAL VARIABLE
!+       V - SUPPLIES A VALUE TO THE DRIVER
!+       R - RETURNS A RESULT FROM THE DRIVER
!+       I - USED INTERNALLY BY THE DRIVER
!+       A - ARRAY
!+
!+ CLASS \ PARAMETER
!+ ------+----------
!+       \
!+         THE NONZERO ENTRIES OF THE COEFFICIENT MATRIX M ARE STORED
!+    ROW-BY-ROW IN THE ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO
!+    ENTRIES IN EACH ROW, WE NEED TO KNOW IN WHICH COLUMN EACH ENTRY
!+    LIES.  THE COLUMN INDICES WHICH CORRESPOND TO THE NONZERO ENTRIES
!+    OF M ARE STORED IN THE ARRAY JA;  I.E., IF  A(K) = M(I,J),  THEN
!+    JA(K) = J.  IN ADDITION, WE NEED TO KNOW WHERE EACH ROW STARTS AND
!+    HOW LONG IT IS.  THE INDEX POSITIONS IN JA AND A WHERE THE ROWS OF
!+    M BEGIN ARE STORED IN THE ARRAY IA;  I.E., IF M(I,J) IS THE FIRST
!+    NONZERO ENTRY (STORED) IN THE I-TH ROW AND A(K) = M(I,J),  THEN
!+    IA(I) = K.  MOREOVER, THE INDEX IN JA AND A OF THE FIRST LOCATION
!+    FOLLOWING THE LAST ELEMENT IN THE LAST ROW IS STORED IN IA(N+1).
!+    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS GIVEN BY
!+    IA(I+1) - IA(I),  THE NONZERO ENTRIES OF THE I-TH ROW ARE STORED
!+    CONSECUTIVELY IN
!+            A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1),
!+    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN
!+            JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1).
!+    FOR EXAMPLE, THE 5 BY 5 MATRIX
!+                ( 1. 0. 2. 0. 0.)
!+                ( 0. 3. 0. 0. 0.)
!+            M = ( 0. 4. 5. 6. 0.)
!+                ( 0. 0. 0. 7. 0.)
!+                ( 0. 0. 0. 8. 9.)
!+    WOULD BE STORED AS
!+               \ 1  2  3  4  5  6  7  8  9
!+            ---+--------------------------
!+            IA \ 1  3  4  7  8 10
!+            JA \ 1  3  2  2  3  4  4  4  5
!+             A \ 1. 2. 3. 4. 5. 6. 7. 8. 9.         .
!+
!+ NV    \ N     - NUMBER OF VARIABLES/EQUATIONS.
!+ FVA   \ A     - NONZERO ENTRIES OF THE COEFFICIENT MATRIX M, STORED
!+       \           BY ROWS.
!+       \           SIZE = NUMBER OF NONZERO ENTRIES IN M.
!+ NVA   \ IA    - POINTERS TO DELIMIT THE ROWS IN A.
!+       \           SIZE = N+1.
!+ NVA   \ JA    - COLUMN NUMBERS CORRESPONDING TO THE ELEMENTS OF A.
!+       \           SIZE = SIZE OF A.
!+ FVA   \ B     - RIGHT-HAND SIDE B;  B AND Z CAN THE SAME ARRAY.
!+       \           SIZE = N.
!+ FRA   \ Z     - SOLUTION X;  B AND Z CAN BE THE SAME ARRAY.
!+       \           SIZE = N.
!+
!+         THE ROWS AND COLUMNS OF THE ORIGINAL MATRIX M CAN BE
!+    REORDERED (E.G., TO REDUCE FILLIN OR ENSURE NUMERICAL STABILITY)
!+    BEFORE CALLING THE DRIVER.  IF NO REORDERING IS DONE, THEN SET
!+    R(I) = C(I) = IC(I) = I  FOR I=1,...,N.  THE SOLUTION Z IS RETURNED
!+    IN THE ORIGINAL ORDER.
!+
!+ NVA   \ R     - ORDERING OF THE ROWS OF M.
!+       \           SIZE = N.
!+ NVA   \ C     - ORDERING OF THE COLUMNS OF M.
!+       \           SIZE = N.
!+ NVA   \ IC    - INVERSE OF THE ORDERING OF THE COLUMNS OF M;  I.E.,
!+       \           IC(C(I)) = I  FOR I=1,...,N.
!+       \           SIZE = N.
!+
!+         THE SOLUTION OF THE SYSTEM OF LINEAR EQUATIONS IS DIVIDED INTO
!+    THREE STAGES --
!+      NSF -- THE MATRIX M IS PROCESSED SYMBOLICALLY TO DETERMINE WHERE
!+              FILLIN WILL OCCUR DURING THE NUMERIC FACTORIZATION.
!+      NNF -- THE MATRIX M IS FACTORED NUMERICALLY INTO THE PRODUCT LDU
!+              OF A UNIT LOWER TRIANGULAR MATRIX L, A DIAGONAL MATRIX D,
!+              AND A UNIT UPPER TRIANGULAR MATRIX U, AND THE SYSTEM
!+              MX = B  IS SOLVED.
!+      NNS -- THE LINEAR SYSTEM  MX = B  IS SOLVED USING THE LDU
!+  OR          FACTORIZATION FROM NNF.
!+      NNT -- THE TRANSPOSED LINEAR SYSTEM  MT X = B  IS SOLVED USING
!+              THE LDU FACTORIZATION FROM NNF.
!+    FOR SEVERAL SYSTEMS WHOSE COEFFICIENT MATRICES HAVE THE SAME
!+    NONZERO STRUCTURE, NSF NEED BE DONE ONLY ONCE (FOR THE FIRST
!+    SYSTEM);  THEN NNF IS DONE ONCE FOR EACH ADDITIONAL SYSTEM.  FOR
!+    SEVERAL SYSTEMS WITH THE SAME COEFFICIENT MATRIX, NSF AND NNF NEED
!+    BE DONE ONLY ONCE (FOR THE FIRST SYSTEM);  THEN NNS OR NNT IS DONE
!+    ONCE FOR EACH ADDITIONAL RIGHT-HAND SIDE.
!+
!+ NV    \ PATH  - PATH SPECIFICATION;  VALUES AND THEIR MEANINGS ARE --
!+       \           1  PERFORM NSF AND NNF.
!+       \           2  PERFORM NNF ONLY  (NSF IS ASSUMED TO HAVE BEEN
!+       \               DONE IN A MANNER COMPATIBLE WITH THE STORAGE
!+       \               ALLOCATION USED IN THE DRIVER).
!+       \           3  PERFORM NNS ONLY  (NSF AND NNF ARE ASSUMED TO
!+       \               HAVE BEEN DONE IN A MANNER COMPATIBLE WITH THE
!+       \               STORAGE ALLOCATION USED IN THE DRIVER).
!+       \           4  PERFORM NNT ONLY  (NSF AND NNF ARE ASSUMED TO
!+       \               HAVE BEEN DONE IN A MANNER COMPATIBLE WITH THE
!+       \               STORAGE ALLOCATION USED IN THE DRIVER).
!+       \           5  PERFORM NSF ONLY.
!+
!+         VARIOUS ERRORS ARE DETECTED BY THE DRIVER AND THE INDIVIDUAL
!+    SUBROUTINES.
!+
!+ NR    \ FLAG  - ERROR FLAG;  VALUES AND THEIR MEANINGS ARE --
!+       \             0     NO ERRORS DETECTED
!+       \             N+K   NULL ROW IN A  --  ROW = K
!+       \            2N+K   DUPLICATE ENTRY IN A  --  ROW = K
!+       \            3N+K   INSUFFICIENT STORAGE IN NSF  --  ROW = K
!+       \            4N+1   INSUFFICIENT STORAGE IN NNF
!+       \            5N+K   NULL PIVOT  --  ROW = K
!+       \            6N+K   INSUFFICIENT STORAGE IN NSF  --  ROW = K
!+       \            7N+1   INSUFFICIENT STORAGE IN NNF
!+       \            8N+K   ZERO PIVOT  --  ROW = K
!+       \           10N+1   INSUFFICIENT STORAGE IN NDRV
!+       \           11N+1   ILLEGAL PATH SPECIFICATION
!+
!+         WORKING STORAGE IS NEEDED FOR THE FACTORED FORM OF THE MATRIX
!+    M PLUS VARIOUS TEMPORARY VECTORS.  THE ARRAYS ISP AND RSP SHOULD BE
!+    EQUIVALENCED;  INTEGER STORAGE IS ALLOCATED FROM THE BEGINNING OF
!+    ISP AND REAL STORAGE FROM THE END OF RSP.
!+
!+ NV    \ NSP   - DECLARED DIMENSION OF RSP;  NSP GENERALLY MUST
!+       \           BE LARGER THAN  5N+3 + 2K  (WHERE  K = (NUMBER OF
!+       \           NONZERO ENTRIES IN M)).
!+ NVIRA \ ISP   - INTEGER WORKING STORAGE DIVIDED UP INTO VARIOUS ARRAYS
!+       \           NEEDED BY THE SUBROUTINES;  ISP AND RSP SHOULD BE
!+       \           EQUIVALENCED.
!+       \           SIZE = LRATIO*NSP
!+ FVIRA \ RSP   - REAL WORKING STORAGE DIVIDED UP INTO VARIOUS ARRAYS
!+       \           NEEDED BY THE SUBROUTINES;  ISP AND RSP SHOULD BE
!+       \           EQUIVALENCED.
!+       \           SIZE = NSP.
!+ NR    \ ESP   - IF SUFFICIENT STORAGE WAS AVAILABLE TO PERFORM THE
!+       \           SYMBOLIC FACTORIZATION (NSF), THEN ESP IS SET TO THE
!+       \           AMOUNT OF EXCESS STORAGE PROVIDED (NEGATIVE IF
!+       \           INSUFFICIENT STORAGE WAS AVAILABLE TO PERFORM THE
!+       \           NUMERIC FACTORIZATION (NNF)).
!
!note     IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!note
!+
!+     TO CONVERT THESE ROUTINES FOR DOUBLE PRECISION ARRAYS, SIMPLY USE
!+
!+     THE DOUBLE PRECISION DECLARATIONS IN PLACE OF THE REAL DECLARATIONS
!+
!+     IN EACH SUBPROGRAM;  IN ADDITION, THE DATA VALUE OF THE INTEGER
!+
!+     VARIABLE LRATIO MUST BE SET AS INDICATED IN SUBROUTINE NDRV.
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        18/02/08
!+        V5P9
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
!| A              |-->|NONZERO ENTRIES OF THE COEFFICIENT MATRIX M, 
!|                |   |STORED BY ROWS
!| B              |-->|RIGHT-HAND SIDE B ; 
!| C              |-->|ORDERING OF THE COLUMNS OF MATRIX
!| ESP            |---|INTEGER DIMENSION STORAGE : IF SUFFICIENT STORAGE  
!|                |   |WAS AVAILABLE TO PERFORM THE SYMBOLIC  
!|                |   |FACTORIZATION (NSF), THEN ESP IS SET TO THE AMOUNT 
!|                |   | OF EXCESS STORAGE PROVIDED (NEGATIVE IF  
!|                |   |INSUFFICIENT STORAGE WAS AVAILABLE TO PERFORM 
!|                |   |THE NUMERIC FACTORIZATION (NNF)).
!| FLAG           |<--|ERROR FLAG;  VALUES AND THEIR MEANINGS ARE :
!|                |   |0     NO ERRORS DETECTED
!|                |   |N+K   NULL ROW IN A  --  ROW = K
!|                |   |2N+K   DUPLICATE ENTRY IN A  --  ROW = K
!|                |   |3N+K   INSUFFICIENT STORAGE IN NSF  --  ROW = K
!|                |   |4N+1   INSUFFICIENT STORAGE IN NNF
!|                |   |5N+K   NULL PIVOT  --  ROW = K
!|                |   |6N+K   INSUFFICIENT STORAGE IN NSF  --  ROW = K
!|                |   |7N+1   INSUFFICIENT STORAGE IN NNF
!|                |   |8N+K   ZERO PIVOT  --  ROW = K
!|                |   |10N+1   INSUFFICIENT STORAGE IN NDRV
!|                |   |11N+1   ILLEGAL PATH SPECIFICATION
!| IA             |-->|POINTERS TO DELIMIT THE ROWS IN A ; SIZE = N+1
!| IC             |-->|INVERSE OF THE ORDERING OF THE COLUMNS OF MATRIX
!| ISP            |-->|INTEGER WORKING STORAGE
!| JA             |-->|COLUMN NUMBERS CORRESPONDING TO THE ELEMENTS OF A
!| N              |-->|NUMBER OF VARIABLES/EQUATIONS
!| NSP            |-->|DECLARED DIMENSION OF RSP : GENERALLY MUST
!|                |   |BE LARGER THAN  5N+3 + 2K  (WHERE  K = (NUMBER OF
!|                |   |NONZERO ENTRIES IN THE MATRIX M).
!| PATH           |-->|PATH SPECIFICATION;  VALUES AND THEIR MEANINGS ARE:
!|                |   |1  PERFORM NSF AND NNF.
!|                |   |2  PERFORM NNF ONLY  (NSF IS ASSUMED TO HAVE BEEN
!|                |   |   DONE IN A MANNER COMPATIBLE WITH THE STORAGE
!|                |   |   ALLOCATION USED IN THE DRIVER).
!|                |   |3  PERFORM NNS ONLY  (NSF AND NNF ARE ASSUMED TO
!|                |   |   HAVE BEEN DONE IN A MANNER COMPATIBLE WITH THE
!|                |   |   STORAGE ALLOCATION USED IN THE DRIVER).
!|                |   |4  PERFORM NNT ONLY  (NSF AND NNF ARE ASSUMED TO
!|                |   |   HAVE BEEN DONE IN A MANNER COMPATIBLE WITH THE
!|                |   |   STORAGE ALLOCATION USED IN THE DRIVER).
!|                |   |5  PERFORM NSF ONLY.
!| R              |-->|ORDERING OF THE ROWS OF MATRIX
!| RSP            |-->|REAL WORKING STORAGE (SIZE = NSP)
!| Z              |<--|SOLUTION X
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,NSP
      INTEGER R(*),C(*),IC(*),IA(*),JA(*),ISP(*)
      INTEGER ESP,PATH,FLAG,Q,IM,D,U,ROW,TMP,UMAX,VMAX
      DOUBLE PRECISION A(*),B(*),Z(*),RSP(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IL,JL,IU,JU,JLMAX,JUTMP,JUMAX,J,L,LMAX
!
!  SET LRATIO EQUAL TO THE RATIO BETWEEN THE LENGTH OF FLOATING POINT
!  AND INTEGER ARRAY DATA;  E. G., LRATIO = 1 FOR (REAL, INTEGER),
!  LRATIO = 2 FOR (DOUBLE PRECISION, INTEGER)
!
      INTEGER LRATIO
      DATA LRATIO/2/
!
!-----------------------------------------------------------------------
!
      IF(PATH.LT.1.OR.5.LT.PATH) GO TO 111
!  ******  INITIALISES AND DIVIDES UP TEMPORARY STORAGE  ***************
      IL = 1
      IU = IL + N+1
      JL = IU + N+1
!
!  ******  CALLS NSF IF FLAG IS SET  ***********************************
      IF ((PATH-1) * (PATH-5) .NE. 0)  GO TO 2
      VMAX = (LRATIO*NSP + 1 - JL) - (N+1) - N
      JLMAX = VMAX/2
      Q = JL + JLMAX
      IM = Q + (N+1)
      JUTMP = IM  +   N
      JUMAX = LRATIO*NSP + 1 - JUTMP
      ESP = VMAX/LRATIO
      IF (JLMAX.LE.0 .OR. JUMAX.LE.0)  GO TO 110
      CALL SD_NSF(N,R,IC,IA,JA,
     &        ISP(IL),ISP(JL),JLMAX,ISP(IU),ISP(JUTMP),JUMAX,
     &        ISP(Q),ISP(IM),FLAG)
      IF (FLAG.NE.0)  GO TO 100
!  ******  MOVES JU NEXT TO JL  ****************************************
      JLMAX = ISP(IL+N)-1
      JU    = JL + JLMAX
      JUMAX = ISP(IU+N)-1
      IF(JUMAX.GE.1) THEN
        DO J=1,JUMAX
          ISP(JU+J-1) = ISP(JUTMP+J-1)
        ENDDO
      ENDIF
!
!  ******  CALLS REMAINING SUBROUTINES  ********************************
2     CONTINUE
      JLMAX = ISP(IL+N)-1
      JU    = JL  + JLMAX
      JUMAX = ISP(IU+N)-1
      L     = (JU + JUMAX - 2 + LRATIO)  /  LRATIO    +    1
      LMAX  = JLMAX
      D     = L   + LMAX
      U     = D   + N
      ROW   = NSP + 1 - N
      TMP   = ROW - N
      UMAX  = TMP - U
      ESP = UMAX - JUMAX
!
      IF((PATH-1)*(PATH-2).NE.0) GO TO 3
        IF(UMAX.LE.0) GO TO 110
          CALL SD_NNF(N,R,C,IC,IA,JA,A,Z,B,
     &                ISP(IL),ISP(JL),RSP(L),LMAX,RSP(D),
     &                ISP(IU),ISP(JU),RSP(U),UMAX,
     &                RSP(ROW),RSP(TMP),FLAG)
          IF(FLAG.NE.0) GO TO 100
          RETURN
!
   3    IF ((PATH-3) .NE. 0)  GO TO 4
          CALL SD_NNS
     &       (N,  R, C,
     &        ISP(IL), ISP(JL), RSP(L),  RSP(D),
     &          ISP(IU), ISP(JU), RSP(U),
     &        Z,  B,  RSP(TMP))
!
4     IF((PATH-4).NE.0) GO TO 5
          CALL SD_NNT(N,R,C,ISP(IL),ISP(JL),RSP(L),RSP(D),
     &                ISP(IU),ISP(JU),RSP(U),Z,B,RSP(TMP))
5     RETURN
!
! ** ERROR:  ERROR DETECTED IN NSF, NNF, NNS, OR NNT
100   RETURN
! ** ERROR:  INSUFFICIENT STORAGE
110   FLAG = 10*N + 1
      RETURN
! ** ERROR:  ILLEGAL PATH SPECIFICATION
111   FLAG = 11*N + 1
      RETURN
      END
