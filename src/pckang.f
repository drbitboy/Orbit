C$Procedure      PCKANG_N ( PCK, evaluate type 03 for RA, DEC, W )
 
      SUBROUTINE PCKANG_N ( BODY, ET, FRAME, ANGLES, FOUND )
 
C$ Abstract
C
C     For a specified body and TDB epoch obtain the native reference 
C     frame and values of RA, DEC, W, and their rates from a loaded PCK 
C     file.
C
C     This routine was developed for the NEAR project at the request of 
C     Jim Miller at JPL. It is not a SPICE subroutine, but is provided 
C     as a convenience to the NEAR project..
C
C$ Required_Reading
C
C     PCK
C
C$ Keywords
C
C    None
C
C$ Declarations
 
      INTEGER               BODY
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      ANGLES   ( 6 )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BODY       I   SPICE ID for the body of interest.
C     ET         I   Target epoch.
C     FRAME      O   The frame relative to which the ANGLES are given.
C     ANGLES     O   RA, DEC, W, and rates at epoch ET.
C     FOUND      O   A flag indicating data was found for the epoch ET.
C
C$ Detailed_Input
C
C     BODY        is the SPICE ID code for the body whose orientation 
C                 angles are desired.
C
C     ET          is a target epoch, at which RA, DEC, W, and their 
C                 rates are to eb computed.
C
C$ Detailed_Output
C
C     FRAME       is the name of the inertial reference frame relative 
C                 to which RA, DEC, W, and their rates are given. For 
C                 the NEAR mission this will always be J2000, but we 
C                 include it just in case there is a change of plans.
C
C                 If there is not enough room in the character string
C                 FRAME to hold the entire reference frame name, the 
C                 name will be truncated on the right.
C
C     ANGLES      RA, DEC, W, and rates at epoch ET.
C
C                 ANGLE(1) = RA
C                 ANGLE(2) = DEC
C                 ANGLE(3) = W
C                 ANGLE(4) = dRA
C                 ANGLE(5) = dDEC
C                 ANGLE(6) = dW
C
C     FOUND       is a logical flag indicating whether data was found 
C                 for the specified body and epoch. 
C
C                 A value or .TRUE. indicates that RA, DEC, W, and 
C                 their rates have been successfully coumputed.
C
C                 A value of .FALSE. indicates that data was not 
C                 available for either the body or the epoch.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the type of the PCK segment is not 03, then ther error
C        NEAR(BADSEGMENTTYPE) will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of type 03 segments are described 
C     in the PCK Required Reading file.
C
C     A type 03 segment contains six sets of Chebyshev coefficients,
C     one set each for RA, DEC, and W and one set each for their
C     derivatives.  PCKANG_N calls the routine CHBVAL to evalute each 
C     polynomial, to arrive at a complete set of angles and rates.
C
C     This subroutine was written at the request of Jim Miller of JPL
C     for use by the NEAR project. It is intended only for use by this
C     project and shoule not be considered to be a part of the NAIF 
C     toolkit ot spicelib. 
C
C$ Examples
C
C     C
C     C     Load an orientation file for Eros.
C     C
C           CALL PCKLOF ( 'eros.bpc', HANDLE )
C     C
C     C     Obtain RA, DEC, W, and their rates.
C     C
C
C           CALL PCKANG_N ( EROSID, ET, FRAME, ANGLES, FOUND )
C     C
C     C     Do something with the angles.
C     C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     PCK Required Reading
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    Version 1.0.0, 29-MAR-1995 (KRG)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
C     Local paramaters
C
C     This is the maximum record size that this subroutine can handle.
C     It allows up to a 19th degree Chebyshev polynomial to be returned.
C     NEAR uses a 10th degree Chebyshev polynomial, but better safe than 
C     sorry.
C
      INTEGER               RECSIZ
      PARAMETER           ( RECSIZ = 123 )
C
C     ND, NI, and NC values for a PCK file.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 5 )

      INTEGER               NC
      PARAMETER           ( NC = 8 * (ND + (NI+1)/2) )
C
C     The size of a PCK segment descriptor.
C
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = ND + (NI+1)/2 )
C
C     Local variables
C
      CHARACTER*(NC)        MYSGID

      DOUBLE PRECISION      DESCR (DSCSIZ)
      DOUBLE PRECISION      DUMMY2
      DOUBLE PRECISION      DUMMY3
      DOUBLE PRECISION      RECORD(RECSIZ)

      INTEGER               COFLOC
      INTEGER               DEGREE
      INTEGER               DUMMY1
      INTEGER               DUMMY4
      INTEGER               DUMMY5
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IFRAME
      INTEGER               MYTYPE
      INTEGER               NCOEFF

      LOGICAL               MYFND
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKANG_N' )
      END IF
C
C     First we search through the loaded PCK files for the body and 
C     epoch of interest.
C
      CALL PCKSFS ( BODY, ET, HANDLE, DESCR, MYSGID, MYFND )
C
C     If we found data for the body and epoch of interest, then read
C     and evaluate the appropriate record from the file.
C
      IF ( MYFND ) THEN
C
C        Unpack the descriptor.
C
         CALL PCKUDS ( DESCR,  DUMMY1, IFRAME, MYTYPE, 
     .                 DUMMY2, DUMMY3, DUMMY4, DUMMY5  )
C
C        Check the data type. If it's not 03, then we signal an error.
C
         IF ( MYTYPE .NE. 03 ) THEN
            CALL SETMSG ( 'The PCK data type found was #. Only PCK'
     .      //            ' type 3 may be used with this subroutine.' )
            CALL ERRINT ( '#', MYTYPE                                 )
            CALL SIGERR ( 'NEAR(BADSEGMENTTYPE)'                      )
            CALL CHKOUT ( 'PCKANG_N'                                  )
            RETURN
         END IF
C
C        Get the name of the reference frame.
C
         CALL IRFNAM ( IFRAME, FRAME )
C
C        Read the appropriate record.
C
         CALL PCKR03 ( HANDLE, DESCR, ET, RECORD )
C
C        The first number in the record is the number of Chebyshev 
C        Polynomial coefficients used to represent each component of the 
C        state vector.  Following it are two numbers that will be used 
C        later, then the six sets of coefficients.
C
         NCOEFF = INT( RECORD( 1 ) )
C
C        The degree of each polynomial is one less than the number of
C        coefficients.
C
         DEGREE = NCOEFF - 1
C
C        Call CHBVAL once for each quantity to evaluate the position
C        and velocity values.
C
         DO I = 1, 6
C
C           The coefficients for each variable are located contiguously,
C           following the first three words in the record.
C
            COFLOC = NCOEFF * ( I - 1 ) + 4
C
C           CHBVAL needs as input the coefficients, the degree of the
C           polynomial, the epoch, and also two variable transformation
C           parameters, which are located, in our case, in the second 
C           and third slots of the record.
C
            CALL CHBVAL ( RECORD( COFLOC ), DEGREE, RECORD( 2 ), ET,
     .                    ANGLES(I)                                  )

         END DO

      END IF
C
C     Set the value of FOUND, check out and return.
C
      FOUND = MYFND
 
      CALL CHKOUT ( 'PCKANG_N' )
      RETURN
      END
